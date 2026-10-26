;;;; wpg2.lisp -- WPG version 2 records (WordPerfect 6 and later).
;;;;
;;;; Records carry a class byte and an extension count as well as a type.
;;;; Coordinates are 16-bit integers, or 16.16 fixed point when the Start
;;;; WPG record selects double precision; the unit is set per file. Drawing
;;;; objects begin with an "object characterization" (flags plus an optional
;;;; transform), and colors are given directly as RGBA.

(in-package #:wordperfect-graphics)

(defun double-precision-p (state)
  (wpg-image-double-precision (parse-state-image state)))

(defun read-coord (reader state)
  (if (double-precision-p state)
      (let ((v (/ (read-s32 reader) 65536)))
        (if (integerp v) v (float v 1d0)))
      (read-s16 reader)))

(defun read-points-2 (reader state count)
  (let ((v (make-array count)))
    (dotimes (i count v)
      (let* ((x (read-coord reader state)) (y (read-coord reader state)))
        (setf (aref v i) (cons x y))))))

(defun read-rgba-8 (reader)
  (let* ((r (read-u8 reader)) (g (read-u8 reader)) (b (read-u8 reader))
         (transparency (read-u8 reader)))
    (make-color r g b (- 255 transparency))))

(defun read-rgba-16 (reader)
  (flet ((c () (ash (read-u16 reader) -8)))
    (let* ((r (c)) (g (c)) (b (c)) (transparency (c)))
      (make-color r g b (- 255 transparency)))))

(defun fixed (integer fraction)
  (let ((v (+ integer (/ fraction 65536))))
    (if (integerp v) v (float v 1d0))))

(defun read-characterization (reader)
  "The object characterization that starts every WPG2 drawing object.
Returns a plist. :MATRIX is (a b c d e f), mapping (x, y) to
(ax + cy + e, bx + dy + f)."
  (let* ((flags (read-u16 reader))
         (taper (logbitp 0 flags)) (translate (logbitp 1 flags))
         (skew (logbitp 2 flags)) (scale (logbitp 3 flags))
         (rotate (logbitp 4 flags)) (has-id (logbitp 5 flags))
         (edit-lock (logbitp 7 flags))
         (a 1) (b 0) (c 0) (d 1) (e 0) (f 0)
         (fields '()))
    (when edit-lock
      (setf (getf fields :lock-flags) (read-u32 reader)))
    (when has-id
      ;; 15 bits, or 31 when the top bit of the first word is set.
      (let ((id (read-u16 reader)))
        (setf (getf fields :object-id)
              (if (logbitp 15 id)
                  (logior (ash (logand id #x7fff) 16) (read-u16 reader))
                  id))))
    (when rotate
      (setf (getf fields :rotation) (fixed 0 (read-u32 reader))))
    (when (or rotate scale)
      (setf a (/ (read-s32 reader) 65536)
            d (/ (read-s32 reader) 65536)))
    (when (or rotate skew)
      (setf c (/ (read-s32 reader) 65536)
            b (/ (read-s32 reader) 65536)))
    (when translate
      (let* ((xf (read-u16 reader)) (xi (read-s32 reader))
             (yf (read-u16 reader)) (yi (read-s32 reader)))
        (setf e (fixed xi xf) f (fixed yi yf))))
    (when taper
      (setf (getf fields :taper) (list (/ (read-s32 reader) 65536)
                                       (/ (read-s32 reader) 65536))))
    (list* :flags flags
           :winding-rule (logbitp 12 flags)
           :filled (logbitp 13 flags)
           :closed (logbitp 14 flags)
           :framed (logbitp 15 flags)
           :matrix (mapcar (lambda (v) (if (integerp v) v (float v 1d0)))
                           (list a b c d e f))
           fields)))

(define-record (2 #x01 :start-wpg) (r s)
  (let* ((x-units (read-u16 r))
         (y-units (read-u16 r))
         (precision (read-u8 r))
         (image (parse-state-image s)))
    (unless (member precision '(0 1))
      (decode-error "unknown coordinate precision ~D" precision))
    (setf (wpg-image-double-precision image) (= precision 1))
    (when (or (zerop x-units) (zerop y-units))
      (note-warning s "Start WPG gives a zero unit size; assuming 1200 per inch")
      (setf x-units 1200 y-units 1200))
    (let* ((viewport (loop repeat 4 collect (read-coord r s)))
           (bounds (loop repeat 4 collect (read-coord r s)))
           ;; Some files written by WordPerfect 8 have leftover bytes where
           ;; the image bounds belong (unordered, far off the page); the
           ;; viewport is the page there.
           (page (destructuring-bind (x1 y1 x2 y2) bounds
                   (if (and (< x1 x2) (< y1 y2)) bounds viewport))))
      (unless (eq page bounds)
        (note-warning s "Start WPG image bounds ~S are not a rectangle; using the viewport" bounds))
      (when (eq page viewport)
        (destructuring-bind (x1 y1 x2 y2) viewport
          (unless (and (/= x1 x2) (/= y1 y2))
            (note-warning s "Start WPG viewport ~S is empty" viewport))))
      (destructuring-bind (x1 y1 x2 y2) page
        (setf (wpg-image-x-units image) x-units
              (wpg-image-y-units image) y-units
              (wpg-image-x-offset image) (min x1 x2)
              (wpg-image-y-offset image) (min y1 y2)
              (wpg-image-width image) (abs (- x2 x1))
              (wpg-image-height image) (abs (- y2 y1))))
      (list :x-units x-units :y-units y-units :precision precision
            :viewport viewport :bounds bounds))))

(define-record (2 #x02 :end-wpg) (r s)
  nil)

(define-record (2 #x0c :color-palette) (r s)
  (let* ((start (read-u16 r))
         (count (read-u16 r))
         (entries (read-palette-entries r count #'read-rgba-8)))
    (store-palette s start entries)
    (list :start-index start :entries entries)))

(define-record (2 #x0d :dp-color-palette) (r s)
  (let* ((start (read-u16 r))
         (count (read-u16 r))
         (entries (read-palette-entries r count #'read-rgba-16)))
    (store-palette s start entries)
    (list :start-index start :entries entries)))

(defun wpg2-pixel-format (color-format)
  "Returns (values depth format) for a WPG2 bitmap color format code."
  (case color-format
    (1 (values 1 :indexed))
    (2 (values 2 :indexed))
    (3 (values 4 :indexed))
    (4 (values 8 :indexed))
    (5 (values 16 :gray16))
    (6 (values 16 :rgb555))
    (8 (values 24 :rgb))
    (9 (values 32 :rgba))
    (12 (values 8 :gray))
    (t (decode-error "unsupported WPG2 bitmap color format ~D" color-format))))

(defun read-uncompressed-rows (reader row-bytes rows)
  (let* ((out (make-octets (* row-bytes rows)))
         (n (min (length out) (reader-remaining reader))))
    (replace out (read-octets reader n))
    (values out n)))

(define-record (2 #x0e :bitmap-data) (r s)
  (let* ((width (read-u16 r))
         (height (read-u16 r))
         (color-format (read-u8 r))
         (compression (read-u8 r))
         (row-bytes 0)
         (placement (shiftf (parse-state-pending-bitmap s) nil)))
    (when (or (zerop width) (zerop height))
      (decode-error "empty bitmap (~Dx~D)" width height))
    (multiple-value-bind (depth format) (wpg2-pixel-format color-format)
    (setf row-bytes (row-bytes-for width depth))
    (check-bitmap-size row-bytes height)
    (multiple-value-bind (data produced)
        (case compression
          (0 (read-uncompressed-rows r row-bytes height))
          (1 (decode-wpg2-rle r row-bytes height))
          (t (decode-error "unknown WPG2 bitmap compression ~D" compression)))
      (let ((bitmap (make-wpg-bitmap
                     :width width :height height :depth depth :format format
                     :palette (and (eq format :indexed) (bitmap-palette s depth 2))
                     :data data :row-bytes row-bytes
                     :compression (if (= compression 0) :none :rle)
                     :complete-p (= produced (length data)))))
        (when placement
          (destructuring-bind (&key x1 y1 x2 y2 h-resolution v-resolution
                               &allow-other-keys)
              placement
            (setf (wpg-bitmap-x1 bitmap) x1 (wpg-bitmap-y1 bitmap) y1
                  (wpg-bitmap-x2 bitmap) x2 (wpg-bitmap-y2 bitmap) y2)
            (when (plusp h-resolution)
              (setf (wpg-bitmap-h-resolution bitmap) h-resolution))
            (when (plusp v-resolution)
              (setf (wpg-bitmap-v-resolution bitmap) v-resolution))))
        (add-bitmap s bitmap)
        (list :width width :height height :color-format color-format
              :depth depth :compression compression :bitmap bitmap))))))

(define-record (2 #x15 :polyline) (r s)
  (let ((ch (read-characterization r)))
    (list :characterization ch :points (read-points-2 r s (read-u16 r)))))

(define-record (2 #x16 :polyspline) (r s)
  (let ((ch (read-characterization r)))
    (list :characterization ch :points (read-points-2 r s (read-u16 r)))))

(define-record (2 #x17 :polycurve) (r s)
  ;; Each node is an incoming control point, the anchor, and an outgoing
  ;; control point.
  (let* ((ch (read-characterization r))
         (n (read-u16 r))
         (nodes (make-array n)))
    (dotimes (i n)
      (setf (aref nodes i)
            (loop repeat 3
                  collect (let* ((x (read-coord r s)) (y (read-coord r s)))
                            (cons x y)))))
    (list :characterization ch :nodes nodes)))

(define-record (2 #x18 :rectangle) (r s)
  (let ((ch (read-characterization r)))
    (list :characterization ch
          :x1 (read-coord r s) :y1 (read-coord r s)
          :x2 (read-coord r s) :y2 (read-coord r s)
          :rx (read-coord r s) :ry (read-coord r s))))

(define-record (2 #x19 :arc) (r s)
  ;; A full ellipse when the start point (ix, iy) equals the end (ex, ey).
  (let ((ch (read-characterization r)))
    (list :characterization ch
          :cx (read-coord r s) :cy (read-coord r s)
          :rx (read-coord r s) :ry (read-coord r s)
          :ix (read-coord r s) :iy (read-coord r s)
          :ex (read-coord r s) :ey (read-coord r s))))

(defun read-resolution (reader state)
  (if (double-precision-p state) (read-s32 reader) (read-s16 reader)))

(define-record (2 #x1a :compound-polygon) (r s)
  ;; The polylines and curves among its EXTENSION child records form one
  ;; path, drawn with this record's flags and transform.
  (list :characterization (read-characterization r)))

(define-record (2 #x20 :group) (r s)
  ;; Its EXTENSION records are its members, drawn through its transform.
  ;; The box is the members' extent, in their own coordinates.
  (let ((ch (read-characterization r)))
    (list* :characterization ch
           (when (>= (reader-remaining r) (if (double-precision-p s) 16 8))
             (list :x1 (read-coord r s) :y1 (read-coord r s)
                   :x2 (read-coord r s) :y2 (read-coord r s))))))

(define-record (2 #x3c :texture-definition) (r s)
  ;; A brush texture: its ID, an average color, the bitmap's original file
  ;; name, and (as child records) the bitmap itself and its palette.
  (let* ((id (read-u16 r))
         (color (read-rgb r))
         (flags (read-u8 r)))
    (list :texture-id id :color color :flags flags
          :file (octets-to-string (read-octets r (reader-remaining r))))))

(define-record (2 #x3d :brush-texture) (r s)
  (list :texture-id (read-u16 r)))

(define-record (2 #x1b :bitmap) (r s)
  (let* ((ch (read-characterization r))
         (fields (list :x1 (read-coord r s) :y1 (read-coord r s)
                       :x2 (read-coord r s) :y2 (read-coord r s)
                       :h-resolution (read-resolution r s)
                       :v-resolution (read-resolution r s))))
    (setf (parse-state-pending-bitmap s) fields)
    (list* :characterization ch fields)))

(define-record (2 #x08 :pen-style-definition) (r s)
  ;; Dash and gap lengths, in units of 218/3.6 of the pen width (libwpg).
  (let* ((style (read-u16 r))
         (n (read-u16 r)))
    (list :style style
          :segments (loop repeat n
                          while (>= (reader-remaining r) (if (double-precision-p s) 8 4))
                          collect (if (double-precision-p s)
                                      (list (/ (read-u32 r) 65536) (/ (read-u32 r) 65536))
                                      (list (read-u16 r) (read-u16 r)))))))

(define-record (2 #x1c :text-line) (r s)
  ;; A line of text anchored at (X, Y). Alignment: horizontal 0 left,
  ;; 1 center, 2 right. ANGLE is the baseline angle in degrees. The text
  ;; is in the Text Data record that follows.
  (let ((ch (read-characterization r)))
    (list :characterization ch :flags (read-u16 r)
          :x (read-coord r s) :y (read-coord r s)
          :h-align (read-u8 r) :v-align (read-u8 r)
          :angle (fixed 0 (read-u32 r)))))

(define-record (2 #x1d :text-block) (r s)
  ;; Text flowed into a box; the text is in the Text Data record that follows.
  (let ((ch (read-characterization r)))
    (list :characterization ch
          :x1 (read-coord r s) :y1 (read-coord r s)
          :x2 (read-coord r s) :y2 (read-coord r s))))

(define-record (2 #x0f :text-data) (r s)
  (list :runs (decode-wp6-text (read-octets r (reader-remaining r)))))

(define-record (2 #x25 :pen-fore-color) (r s) (list :color (read-rgba-8 r)))
(define-record (2 #x26 :dp-pen-fore-color) (r s) (list :color (read-rgba-16 r)))
(define-record (2 #x27 :pen-back-color) (r s) (list :color (read-rgba-8 r)))
(define-record (2 #x28 :dp-pen-back-color) (r s) (list :color (read-rgba-16 r)))

(define-record (2 #x29 :pen-style) (r s)
  (list :style (read-u16 r)))

(define-record (2 #x2b :pen-size) (r s)
  (list :width (read-u16 r) :height (read-u16 r)))

(define-record (2 #x2c :dp-pen-size) (r s)
  (list :width (/ (read-u32 r) 65536) :height (/ (read-u32 r) 65536)))

(define-record (2 #x2d :line-cap) (r s)
  (list :cap (read-u8 r)))

(define-record (2 #x2e :line-join) (r s)
  (list :join (read-u8 r)))

(defun read-brush-color (reader read-entry)
  (let ((gradient-type (read-u8 reader)))
    (if (zerop gradient-type)
        (list :gradient-type 0 :color (funcall read-entry reader))
        (let* ((n (read-u16 reader))
               (colors (read-palette-entries reader n read-entry))
               ;; N-1 stop positions follow, as 16-bit fractions.
               (positions (loop repeat (max 0 (1- n))
                                while (>= (reader-remaining reader) 2)
                                collect (/ (read-u16 reader) 65536))))
          (list :gradient-type gradient-type :colors colors
                :positions (mapcar (lambda (p) (float p 1d0)) positions))))))

(define-record (2 #x31 :brush-fore-color) (r s) (read-brush-color r #'read-rgba-8))
(define-record (2 #x32 :dp-brush-fore-color) (r s) (read-brush-color r #'read-rgba-16))
(define-record (2 #x33 :brush-back-color) (r s) (list :color (read-rgba-8 r)))
(define-record (2 #x34 :dp-brush-back-color) (r s) (list :color (read-rgba-16 r)))

(defun read-brush-gradient (reader)
  ;; The angle is in degrees, 16.16. The reference point, in 1/65536ths
  ;; of the object's extent, is #xFFFF when unset.
  (let* ((fraction (read-u16 reader))
         (integer (read-u16 reader))
         (x-ref (read-u16 reader))
         (y-ref (read-u16 reader))
         ;; One flags byte in the files seen (libwpg reads two words here,
         ;; past the end of the record).
         (flags (if (>= (reader-remaining reader) 2) (read-u16 reader) (read-u8 reader))))
    (list :angle (fixed integer fraction) :x-ref x-ref :y-ref y-ref :flags flags)))

(define-record (2 #x2f :brush-gradient) (r s) (read-brush-gradient r))
(define-record (2 #x30 :dp-brush-gradient) (r s) (read-brush-gradient r))

(define-record (2 #x35 :brush-pattern) (r s)
  (list :pattern (read-u16 r)))

(define-record-names 2
  (#x03 :form-settings)
  (#x04 :ruler-settings)
  (#x05 :grid-settings)
  (#x06 :layer)
  (#x09 :pattern-definition)
  (#x0a :comment)
  (#x0b :color-transfer)
  (#x10 :chart-style)
  (#x11 :chart-data)
  (#x12 :object-image)
  (#x1e :text-path)
  (#x1f :chart)
  (#x21 :object-capsule)
  (#x22 :font-settings)
  (#x2a :pen-pattern)
  (#x36 :horizontal-line)
  (#x37 :vertical-line)
  (#x38 :poster-settings)
  (#x39 :image-state)
  (#x3a :envelope-definition)
  (#x3b :envelope)
  (#x3e :texture-alignment)
  (#x3f :pen-texture))
