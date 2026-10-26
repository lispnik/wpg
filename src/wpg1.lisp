;;;; wpg1.lisp -- WPG version 1 records (WordPerfect 5.x).
;;;;
;;;; Coordinates are signed 16-bit WordPerfect units (1200 per inch) with the
;;;; origin at the lower left. Colors are indices into the current palette.

(in-package #:wordperfect-graphics)

(defun read-points-1 (reader)
  "A 16-bit count followed by that many (x, y) pairs, as a vector of conses."
  (let* ((n (read-u16 reader))
         (v (make-array n)))
    (dotimes (i n v)
      (let* ((x (read-s16 reader)) (y (read-s16 reader)))
        (setf (aref v i) (cons x y))))))

(defun read-rgb (reader)
  (let* ((r (read-u8 reader)) (g (read-u8 reader)) (b (read-u8 reader)))
    (make-color r g b)))

(define-record (1 #x01 :fill-attributes) (r s)
  (list :style (read-u8 r) :color (read-u8 r)))

(define-record (1 #x02 :line-attributes) (r s)
  (list :style (read-u8 r) :color (read-u8 r) :width (read-u16 r)))

(define-record (1 #x03 :marker-attributes) (r s)
  (list :style (read-u8 r) :color (read-u8 r) :height (read-u16 r)))

(define-record (1 #x04 :polymarker) (r s)
  (list :points (read-points-1 r)))

(define-record (1 #x05 :line) (r s)
  (list :x1 (read-s16 r) :y1 (read-s16 r) :x2 (read-s16 r) :y2 (read-s16 r)))

(define-record (1 #x06 :polyline) (r s)
  (list :points (read-points-1 r)))

(define-record (1 #x07 :rectangle) (r s)
  (list :x (read-s16 r) :y (read-s16 r) :width (read-s16 r) :height (read-s16 r)))

(define-record (1 #x08 :polygon) (r s)
  (list :points (read-points-1 r)))

(define-record (1 #x09 :ellipse) (r s)
  (list :cx (read-s16 r) :cy (read-s16 r) :rx (read-s16 r) :ry (read-s16 r)
        :rotation (read-s16 r) :start-angle (read-s16 r) :end-angle (read-s16 r)
        :flags (read-u16 r)))

(defun decode-wpg1-bitmap (reader state width height depth hres vres)
  (unless (member depth '(1 2 4 8))
    (decode-error "unsupported WPG1 bitmap depth ~D" depth))
  (when (or (zerop width) (zerop height))
    (decode-error "empty bitmap (~Dx~D)" width height))
  (let ((row-bytes (row-bytes-for width depth)))
    (check-bitmap-size row-bytes height)
    (multiple-value-bind (data produced) (decode-wpg1-rle reader row-bytes height)
      (make-wpg-bitmap :width width :height height :depth depth
                       ;; A missing resolution means 72 dpi, as in libwpg.
                       :h-resolution (if (plusp hres) hres 72)
                       :v-resolution (if (plusp vres) vres 72)
                       :palette (bitmap-palette state depth 1)
                       :data data :row-bytes row-bytes
                       :compression :rle
                       :complete-p (= produced (length data))))))

(define-record (1 #x0b :bitmap-type-1) (r s)
  (let* ((width (read-u16 r)) (height (read-u16 r)) (depth (read-u16 r))
         (hres (read-u16 r)) (vres (read-u16 r))
         (bitmap (decode-wpg1-bitmap r s width height depth hres vres)))
    (add-bitmap s bitmap)
    (list :width width :height height :depth depth
          :h-resolution hres :v-resolution vres :bitmap bitmap)))

(define-record (1 #x0c :graphics-text) (r s)
  (let* ((length (read-u16 r)) (x (read-s16 r)) (y (read-s16 r)))
    (list :x x :y y
          :text (octets-to-string (read-octets r (min length (reader-remaining r)))))))

(define-record (1 #x0d :graphics-text-attributes) (r s)
  ;; Sizes are in WordPerfect units: HEIGHT is the character cell height
  ;; (the font size), WIDTH the character cell width. ANGLE is in degrees,
  ;; counterclockwise. Alignment: horizontal 0 left, 1 center, 2 right.
  (let* ((width (read-u16 r))
         (height (read-u16 r)))
    (reader-skip r 10)
    (let* ((font (read-u16 r)))
      (reader-skip r 1)
      (list :width width :height height :font font
            :h-align (read-u8 r) :v-align (read-u8 r)
            :color (read-u8 r) :angle (read-s16 r)))))

(define-record (1 #x0e :color-map) (r s)
  (let* ((start (read-u16 r))
         (count (read-u16 r))
         (entries (read-palette-entries r count #'read-rgb)))
    (store-palette s start entries)
    (list :start-index start :entries entries)))

(define-record (1 #x0f :start-wpg) (r s)
  (let* ((version (read-u8 r)) (flags (read-u8 r))
         (width (read-u16 r)) (height (read-u16 r))
         (image (parse-state-image s)))
    (setf (wpg-image-width image) width
          (wpg-image-height image) height)
    (list :version version :flags flags :width width :height height)))

(define-record (1 #x10 :end-wpg) (r s)
  nil)

(define-record (1 #x11 :postscript-type-1) (r s)
  ;; The bounding box is in points (1/72 inch), not WordPerfect units.
  (list :x1 (read-s16 r) :y1 (read-s16 r) :x2 (read-s16 r) :y2 (read-s16 r)
        :postscript-length (reader-remaining r)))

(define-record (1 #x13 :curved-polyline) (r s)
  (let ((reserved (read-u32 r)))
    (list :reserved reserved :points (read-points-1 r))))

(define-record (1 #x1f :curved-polygon) (r s)
  ;; Not in any published description: written by WordPerfect 8. Every one
  ;; seen is a closed cubic Bezier path -- a count of 3n+1 points, the last
  ;; the same as the first -- drawn filled, like a polygon.
  (list :points (read-points-1 r)))

(define-record (1 #x14 :bitmap-type-2) (r s)
  (let* ((rotation (read-u16 r))
         (x1 (read-s16 r)) (y1 (read-s16 r)) (x2 (read-s16 r)) (y2 (read-s16 r))
         (width (read-u16 r)) (height (read-u16 r)) (depth (read-u16 r))
         (hres (read-u16 r)) (vres (read-u16 r))
         (bitmap (decode-wpg1-bitmap r s width height depth hres vres)))
    ;; Bit 15 mirrors left-right, bit 13 top-bottom; the low 12 bits are
    ;; a rotation in degrees.
    (setf (wpg-bitmap-x1 bitmap) x1 (wpg-bitmap-y1 bitmap) y1
          (wpg-bitmap-x2 bitmap) x2 (wpg-bitmap-y2 bitmap) y2
          (wpg-bitmap-rotation bitmap) (logand rotation #x0fff)
          (wpg-bitmap-mirror-x bitmap) (logbitp 15 rotation)
          (wpg-bitmap-mirror-y bitmap) (logbitp 13 rotation))
    (add-bitmap s bitmap)
    (list :rotation rotation :x1 x1 :y1 y1 :x2 x2 :y2 y2
          :width width :height height :depth depth
          :h-resolution hres :v-resolution vres :bitmap bitmap)))

(define-record (1 #x1b :postscript-type-2) (r s)
  (let* ((data-length (read-u32 r))
         (rotation (read-s16 r)))
    (reader-skip r 6)
    (list :data-length data-length :rotation rotation
          :x1 (read-s16 r) :y1 (read-s16 r) :x2 (read-s16 r) :y2 (read-s16 r))))

(define-record (1 #x18 :graphics-text-type-2) (r s)
  ;; A bounding box and a WordPerfect 5 text stream. Files written by
  ;; DrawPerfect follow each one with the same text as Graphics Text
  ;; Attributes and Graphics Text (Type 1) records, for older readers.
  (let* ((size (read-u32 r))
         (rotation (read-s16 r))
         (unknown (read-u16 r))
         (x1 (read-s16 r)) (y1 (read-s16 r)) (x2 (read-s16 r)) (y2 (read-s16 r))
         (sx (read-s16 r)) (sy (read-s16 r))
         (type (read-u8 r)))
    (list :size size :rotation rotation :unknown unknown :x1 x1 :y1 y1 :x2 x2 :y2 y2
          :scale-x sx :scale-y sy :text-type type
          :text (decode-wp5-text (read-octets r (reader-remaining r))))))

(define-record-names 1
  (#x0a :reserved)
  (#x12 :output-attributes)
  (#x15 :start-figure)
  (#x16 :start-chart)
  (#x17 :planperfect-data)
  (#x19 :start-wpg-type-2)
  (#x1a :graphics-text-type-3))

(defun find-postscript-start (body from)
  "Where the PostScript in BODY begins: the first \"%!\" or DOS EPS binary
header (C5 D0 D3 C6) at or after FROM."
  (let ((text (search #(37 33) body :start2 from))
        (eps (search #(#xc5 #xd0 #xd3 #xc6) body :start2 from)))
    (if (and text eps) (min text eps) (or text eps))))

(defun record-postscript (record)
  "The embedded PostScript of a WPG1 PostScript record, as octets, or NIL.
The data follows a fixed header (8 bytes in type 1; libwpg and ImageMagick
disagree about type 2), so it is located by its signature, falling back to
the end of the header."
  (let* ((body (wpg-record-body record))
         (header (case (wpg-record-name record)
                   (:postscript-type-1 8)
                   (:postscript-type-2 #x3c))))
    (when (and header (> (length body) header))
      (subseq body (or (find-postscript-start body (min header 20)) header)))))
