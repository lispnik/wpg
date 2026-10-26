;;;; svg.lisp -- render a parsed WPG-IMAGE as SVG.
;;;;
;;;; Records are replayed in file order, tracking the current pen, brush and
;;;; palette. All geometry is written in file units inside one group whose
;;;; transform flips WPG's y-up space into SVG's y-down space; text and
;;;; bitmaps are flipped back locally so they come out upright.
;;;;
;;;; WPG2 rendering follows libwpg (what LibreOffice and Inkscape use):
;;;; records nest, compound polygons join their children into one path, and
;;;; pen and brush records inside a compound polygon or the Start WPG page
;;;; attributes are ignored.
;;;;
;;;; Not rendered: fill patterns (drawn solid), gradients with more than two
;;;; colors (drawn with their first color), text paths and object images.
;;;; Typefaces are approximated by generic families.

(in-package #:wordperfect-graphics)

(defun svg-num (v)
  "V as a compact SVG number."
  (cond ((integerp v) (format nil "~D" v))
        (t (let* ((s (format nil "~,3F" (float v 1d0)))
                  (s (string-right-trim "0" s)))
             (string-right-trim "." (if (string= s "-0.") "0" s))))))

(defun xml-escape (string)
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (t (if (or (char= ch #\Tab) (char= ch #\Newline)
                          (>= (char-code ch) 32))
                      (write-char ch out)
                      (write-char #\? out)))))))

(defstruct (render-state (:conc-name rs-))
  stream
  image
  palette
  bitmap-href
  ;; Pen
  (stroke "#000000")              ; color string, or NIL for none
  (stroke-opacity 1)
  (stroke-width 0)                ; 0 = hairline
  ;; Brush
  (fill nil)
  (fill-opacity 1)
  ;; WPG1 markers and text
  (marker-color "#000000")
  (marker-size 60)
  ;; WPG2: the characterization of the Bitmap record awaiting its data
  (pending-bitmap-ch nil)
  ;; WPG2: the brush gradient, a plist, or NIL for a solid brush
  (gradient nil)
  (gradient-angle 0)
  (gradient-ref nil)
  (gradient-count 0)
  ;; WPG2: record -> its child records
  (children-of (constantly nil))
  ;; Dashes: a list of dash/gap lengths in pen widths, or NIL for solid
  (dash nil)
  ;; WPG2: pen style number -> dash list, from Pen Style Definitions
  (pen-styles (make-hash-table))
  ;; WPG1: the current Graphics Text Attributes fields
  (text-attributes nil)
  ;; WPG1: draw Graphics Text (Type 2) only in files without Type 1 text
  (draw-text-2 nil)
  ;; WPG2: the Text Line or Text Block awaiting its Text Data
  (pending-text nil))

(defun palette-hex (rs index)
  (let ((palette (rs-palette rs)))
    (if (< index (length palette))
        (color-hex (aref palette index))
        "#000000")))

(defun opacity-of (color)
  (/ (color-alpha color) 255))

(defun gradient-paint (rs)
  "Write a <linearGradient> for the current two-color brush gradient and
return a paint reference to it. Called while a shape's attributes are being
built, so the definition lands just before the shape."
  (destructuring-bind (&key colors &allow-other-keys) (rs-gradient rs)
    (let* ((id (format nil "g~D" (incf (rs-gradient-count rs))))
           (angle (* pi (/ (rs-gradient-angle rs) 180)))
           ;; libwpg: the far color starts the ramp, the near one sits at a
           ;; reference point and, when a reference is given, the ramp
           ;; mirrors back to the far color.
           (far (aref colors 1)) (near (aref colors 0))
           (ref (rs-gradient-ref rs))
           (stops (if ref
                      (list (cons 0 far) (cons (first ref) near) (cons 1 far))
                      (list (cons 0 far) (cons 1 near))))
           (dx (/ (cos angle) 2)) (dy (/ (sin angle) 2)))
      ;; Object-bounding-box units, in the file's y-up space.
      (emit rs "<linearGradient id=\"~A\" x1=\"~A\" y1=\"~A\" x2=\"~A\" y2=\"~A\">~{~A~}</linearGradient>"
            id (svg-num (- 0.5 dx)) (svg-num (- 0.5 dy)) (svg-num (+ 0.5 dx)) (svg-num (+ 0.5 dy))
            (loop for (offset . c) in stops
                  collect (format nil "<stop offset=\"~A\" stop-color=\"~A\"~:[ stop-opacity=\"~A\"~;~*~]/>"
                                  (svg-num offset) (color-hex c)
                                  (= 255 (color-alpha c)) (svg-num (opacity-of c)))))
      (format nil "url(#~A)" id))))

;;; Dashes. Lengths follow libwpg: a table value V is V * 3.6/218 pen
;;; widths in its units, which comes to about 1.19 * V * pen width. A
;;; hairline pen counts as 1/96 inch wide.

(defparameter +default-dash-styles+
  #(()                                  ; 0: solid
    (218 73) (145 73) (73 73) (36 36) (18 18) (18 55)
    (18 55 18 55 18 127) (164 55 18 55) (145 36 18 36 18 36)
    (91 55 91 55 18 55) (91 36 91 36 18 36 18 36) (182 73 73 73)
    (182 36 55 36 55 36) (255 73 266 73 73 73) (273 36 273 36 55 36 55 36))
  "WordPerfect's built-in pen styles, as dash/gap pairs (libwpg's table).")

(defun dash-for-style (rs style)
  (let ((dash (or (gethash style (rs-pen-styles rs))
                  (and (< style (length +default-dash-styles+))
                       (aref +default-dash-styles+ style)))))
    (and dash (every #'plusp dash) (copy-list dash))))

(defun dash-lengths (rs)
  (let ((width (if (zerop (rs-stroke-width rs))
                   (/ (wpg-image-x-units (rs-image rs)) 96)
                   (rs-stroke-width rs))))
    (mapcar (lambda (v) (svg-num (* v 36/2180 72 width))) (rs-dash rs))))

(defun style-attributes (rs &key (fill t) (stroke t) fill-rule)
  "The paint attributes for a shape, from the current pen and brush."
  (with-output-to-string (s)
    (when (and fill fill-rule)
      (format s " fill-rule=\"~(~A~)\"" fill-rule))
    (if (and fill (rs-fill rs))
        (progn (format s " fill=\"~A\"" (if (rs-gradient rs) (gradient-paint rs) (rs-fill rs)))
               (unless (= 1 (rs-fill-opacity rs))
                 (format s " fill-opacity=\"~A\"" (svg-num (rs-fill-opacity rs)))))
        (format s " fill=\"none\""))
    (if (and stroke (rs-stroke rs))
        (progn (format s " stroke=\"~A\"" (rs-stroke rs))
               (unless (= 1 (rs-stroke-opacity rs))
                 (format s " stroke-opacity=\"~A\"" (svg-num (rs-stroke-opacity rs))))
               (if (zerop (rs-stroke-width rs))
                   (format s " stroke-width=\"1\" vector-effect=\"non-scaling-stroke\"")
                   (format s " stroke-width=\"~A\"" (svg-num (rs-stroke-width rs))))
               (when (rs-dash rs)
                 (format s " stroke-dasharray=\"~{~A~^ ~}\"" (dash-lengths rs))))
        (format s " stroke=\"none\""))))

(defun points-attribute (points)
  (format nil "~{~A~^ ~}"
          (map 'list (lambda (p) (format nil "~A,~A" (svg-num (car p)) (svg-num (cdr p))))
               points)))

(defun emit (rs control &rest args)
  (apply #'format (rs-stream rs) control args)
  (terpri (rs-stream rs)))

;;; Bitmaps

(defun emit-bitmap (rs bitmap)
  (let* ((image (rs-image rs))
         (href (and (rs-bitmap-href rs) (funcall (rs-bitmap-href rs) bitmap))))
    (if (null href)
        (emit rs "<!-- bitmap ~Dx~D omitted -->"
              (wpg-bitmap-width bitmap) (wpg-bitmap-height bitmap))
        (multiple-value-bind (x1 y1 x2 y2)
            (if (wpg-bitmap-x1 bitmap)
                (values (min (wpg-bitmap-x1 bitmap) (wpg-bitmap-x2 bitmap))
                        (min (wpg-bitmap-y1 bitmap) (wpg-bitmap-y2 bitmap))
                        (max (wpg-bitmap-x1 bitmap) (wpg-bitmap-x2 bitmap))
                        (max (wpg-bitmap-y1 bitmap) (wpg-bitmap-y2 bitmap)))
                ;; No placement: natural size, at the top-left corner.
                (let* ((w (* (wpg-bitmap-width bitmap)
                             (/ (wpg-image-x-units image)
                                (max 1 (wpg-bitmap-h-resolution bitmap)))))
                       (h (* (wpg-bitmap-height bitmap)
                             (/ (wpg-image-y-units image)
                                (max 1 (wpg-bitmap-v-resolution bitmap)))))
                       (left (wpg-image-x-offset image))
                       (top (+ (wpg-image-y-offset image) (wpg-image-height image))))
                  (values left (- top h) (+ left w) top)))
          (let* ((w (- x2 x1)) (h (- y2 y1))
                 (extra (with-output-to-string (s)
                          (when (wpg-bitmap-mirror-x bitmap)
                            (format s " translate(~A 0) scale(-1 1)" (svg-num w)))
                          (when (wpg-bitmap-mirror-y bitmap)
                            (format s " translate(0 ~A) scale(1 -1)" (svg-num h)))
                          (unless (zerop (wpg-bitmap-rotation bitmap))
                            (format s " rotate(~A ~A ~A)"
                                    (- (wpg-bitmap-rotation bitmap))
                                    (svg-num (/ w 2)) (svg-num (/ h 2)))))))
            (emit rs "<image x=\"0\" y=\"0\" width=\"~A\" height=\"~A\" preserveAspectRatio=\"none\" transform=\"translate(~A ~A) scale(1 -1)~A\" href=\"~A\"/>"
                  (svg-num w) (svg-num h) (svg-num x1) (svg-num y2) extra
                  (xml-escape href)))))))

;;; WPG1

(defun wpg1-arc-path (cx cy rx ry start end)
  (flet ((pt (deg)
           (let ((a (* pi (/ deg 180))))
             (values (+ cx (* rx (cos a))) (+ cy (* ry (sin a)))))))
    (multiple-value-bind (sx sy) (pt start)
      (multiple-value-bind (ex ey) (pt end)
        (format nil "M ~A ~A A ~A ~A 0 ~D 1 ~A ~A"
                (svg-num sx) (svg-num sy) (svg-num rx) (svg-num ry)
                (if (> (mod (- end start) 360) 180) 1 0)
                (svg-num ex) (svg-num ey))))))

(defun render-wpg1-record (rs record)
  (let ((f (wpg-record-fields record)))
    (flet ((field (k) (getf f k)))
      (case (wpg-record-name record)
        (:color-map
         (loop for i from (field :start-index)
               for c across (field :entries)
               when (< i (length (rs-palette rs)))
                 do (setf (aref (rs-palette rs) i) c)))
        (:line-attributes
         ;; Style 0 is no line, 1 solid; the others are taken to be the
         ;; built-in pen styles in order (2 = pen style 1, ...).
         (setf (rs-stroke rs) (if (zerop (field :style)) nil (palette-hex rs (field :color)))
               (rs-stroke-width rs) (field :width)
               (rs-dash rs) (and (> (field :style) 1) (dash-for-style rs (1- (field :style))))))
        (:graphics-text-attributes
         (setf (rs-text-attributes rs) f))
        (:fill-attributes
         (setf (rs-fill rs) (if (zerop (field :style)) nil (palette-hex rs (field :color)))))
        (:marker-attributes
         (setf (rs-marker-color rs) (palette-hex rs (field :color))
               (rs-marker-size rs) (max 1 (field :height))))
        (:line
         (emit rs "<line x1=\"~D\" y1=\"~D\" x2=\"~D\" y2=\"~D\"~A/>"
               (field :x1) (field :y1) (field :x2) (field :y2)
               (style-attributes rs :fill nil)))
        (:polyline
         (emit rs "<polyline points=\"~A\"~A/>" (points-attribute (field :points))
               (style-attributes rs :fill nil)))
        (:polygon
         (emit rs "<polygon points=\"~A\"~A/>" (points-attribute (field :points))
               (style-attributes rs)))
        (:rectangle
         (let ((x (field :x)) (y (field :y)) (w (field :width)) (h (field :height)))
           (emit rs "<rect x=\"~D\" y=\"~D\" width=\"~D\" height=\"~D\"~A/>"
                 (min x (+ x w)) (min y (+ y h)) (abs w) (abs h)
                 (style-attributes rs))))
        (:ellipse
         (let* ((cx (field :cx)) (cy (field :cy)) (rx (field :rx)) (ry (field :ry))
                (start (field :start-angle)) (end (field :end-angle))
                (rotation (if (zerop (field :rotation)) ""
                              (format nil " transform=\"rotate(~D ~D ~D)\""
                                      (field :rotation) cx cy))))
           (if (zerop (mod (- end start) 360))
               (emit rs "<ellipse cx=\"~D\" cy=\"~D\" rx=\"~D\" ry=\"~D\"~A~A/>"
                     cx cy (abs rx) (abs ry) rotation (style-attributes rs))
               (emit rs "<path d=\"~A\"~A~A/>"
                     (wpg1-arc-path cx cy (abs rx) (abs ry) start end)
                     rotation (style-attributes rs :fill nil)))))
        (:curved-polyline
         (let ((pts (field :points)))
           (when (plusp (length pts))
             (emit rs "<path d=\"M ~A ~A~{ C ~A~}\"~A/>"
                   (car (aref pts 0)) (cdr (aref pts 0))
                   (loop for i from 1 below (- (length pts) 2) by 3
                         collect (points-attribute (subseq pts i (+ i 3))))
                   (style-attributes rs :fill nil)))))
        (:polymarker
         (loop for p across (field :points)
               do (emit rs "<circle cx=\"~D\" cy=\"~D\" r=\"~A\" fill=\"~A\"/>"
                        (car p) (cdr p) (svg-num (/ (rs-marker-size rs) 2))
                        (rs-marker-color rs))))
        (:graphics-text
         (let ((a (rs-text-attributes rs)))
           (emit-text rs (field :x) (field :y)
                      (list (list :text (field :text)))
                      :size (or (getf a :height) 200)
                      :char-width (getf a :width)
                      :angle (or (getf a :angle) 0)
                      :h-align (getf a :h-align 0)
                      :v-align (getf a :v-align 0)
                      :color (palette-hex rs (getf a :color 0)))))
        (:graphics-text-type-2
         (when (rs-draw-text-2 rs)
           ;; Only the box is known: fit the lines into it.
           (let* ((lines (max 1 (1+ (count #\Newline (field :text)))))
                  (size (* 0.6 (/ (abs (- (field :y2) (field :y1))) lines))))
             (emit-text rs (min (field :x1) (field :x2))
                        (- (max (field :y1) (field :y2)) size)
                        (list (list :text (field :text)))
                        :size size :angle (field :rotation)
                        :color (palette-hex rs 0)))))
        ((:bitmap-type-1 :bitmap-type-2)
         (emit-bitmap rs (field :bitmap)))))))

;;; Text

(defun font-family (name)
  "An SVG font-family list for a WordPerfect typeface NAME."
  (let ((n (string-downcase (or name ""))))
    (flet ((has (&rest parts) (some (lambda (p) (search p n)) parts)))
      (cond ((has "courier" "mono" "letter gothic" "prestige") "Courier, monospace")
            ((has "times" "roman" "dutch" "serif" "garamond" "century" "bookman" "palatino" "schoolbook")
             (if (has "sans") "Helvetica, Arial, sans-serif" "Times, serif"))
            ((plusp (length n))
             (format nil "~A, Helvetica, Arial, sans-serif" (xml-escape (substitute #\Space #\" name))))
            (t "Helvetica, Arial, sans-serif")))))

(defun split-runs-into-lines (runs)
  "RUNS (plists with :TEXT) as a list of lines, each a list of runs, split
at newlines."
  (let ((lines (list '())))
    (dolist (run runs)
      (loop with text = (getf run :text)
            for start = 0 then (1+ nl)
            for nl = (position #\Newline text :start start)
            do (let ((piece (subseq text start (or nl (length text)))))
                 (when (plusp (length piece))
                   (push (list* :text piece run) (first lines))))
               (if nl (push '() lines) (return))))
    (nreverse (mapcar #'reverse lines))))

(defun emit-text (rs x y runs &key size char-width (angle 0) (h-align 0) (v-align 0)
                                   (color "#000000") (size-of (constantly nil))
                                   font)
  "Draw RUNS as text with its first baseline at (X, Y) in file space, turned
ANGLE degrees counterclockwise. SIZE is the font size in file units, and
SIZE-OF maps a run to its own size. CHAR-WIDTH, if given, stretches each
line to that many units per character."
  (let ((lines (split-runs-into-lines runs)))
    (loop for line in lines
          for dy = 0 then (+ dy (* 1.2 line-size))
          for line-size = (reduce #'max line :key (lambda (r) (or (funcall size-of r) size))
                                             :initial-value size)
          when line
            do (let ((chars (reduce #'+ line :key (lambda (r) (length (getf r :text))))))
                 (emit rs "<text x=\"0\" y=\"~A\" transform=\"translate(~A ~A)~:[ rotate(~A)~;~*~] scale(1 -1)\" font-family=\"~A\" font-size=\"~A\" fill=\"~A\"~A~A~A>~{~A~}</text>"
                       (svg-num dy) (svg-num x) (svg-num y)
                       (zerop angle) (svg-num angle)
                       (font-family font) (svg-num size) color
                       (case h-align (1 " text-anchor=\"middle\"") (2 " text-anchor=\"end\"") (t ""))
                       (case v-align
                         (1 " dominant-baseline=\"text-before-edge\"")
                         (2 " dominant-baseline=\"middle\"")
                         (3 " dominant-baseline=\"text-after-edge\"")
                         (t ""))
                       (if (and char-width (plusp char-width))
                           (format nil " textLength=\"~A\" lengthAdjust=\"spacingAndGlyphs\""
                                   (svg-num (* char-width chars)))
                           "")
                       (loop for run in line
                             collect (let ((run-size (funcall size-of run))
                                           (run-color (getf run :color))
                                           (run-font (getf run :font)))
                                       (if (or run-size run-color run-font)
                                           (format nil "<tspan~@[ font-size=\"~A\"~]~@[ fill=\"~A\"~]~@[ font-family=\"~A\"~]>~A</tspan>"
                                                   (and run-size (svg-num run-size))
                                                   (and run-color (color-hex run-color))
                                                   (and run-font (font-family run-font))
                                                   (xml-escape (getf run :text)))
                                           (xml-escape (getf run :text))))))))))

;;; WPG2

(defun identity-matrix-p (m)
  (equal m '(1 0 0 1 0 0)))

(defun apply-matrix (m point)
  (destructuring-bind (a b c d e f) m
    (let ((x (car point)) (y (cdr point)))
      (cons (+ (* a x) (* c y) e) (+ (* b x) (* d y) f)))))

(defun fill-rule (ch)
  (if (getf ch :winding-rule) :nonzero :evenodd))

(defun call-with-characterization (rs ch thunk &key (transform t))
  "Run THUNK with the pen and brush adjusted for an object's FILLED and
FRAMED flags, inside a group for its transform if it has one."
  (let ((matrix (getf ch :matrix '(1 0 0 1 0 0)))
        (saved-fill (rs-fill rs))
        (saved-stroke (rs-stroke rs)))
    (unless (getf ch :filled) (setf (rs-fill rs) nil))
    (unless (getf ch :framed) (setf (rs-stroke rs) nil))
    (unwind-protect
         (if (or (not transform) (identity-matrix-p matrix))
             (funcall thunk)
             (progn
               (emit rs "<g transform=\"matrix(~{~A~^ ~})\">" (mapcar #'svg-num matrix))
               (funcall thunk)
               (emit rs "</g>")))
      (setf (rs-fill rs) saved-fill
            (rs-stroke rs) saved-stroke))))

(defmacro with-characterization ((rs ch &rest keys) &body body)
  `(call-with-characterization ,rs ,ch (lambda () ,@body) ,@keys))

(defun wpg2-arc-path (cx cy rx ry ix iy ex ey closed)
  (flet ((angle (x y)
           (atan (/ (- y cy) (if (zerop ry) 1 ry))
                 (/ (- x cx) (if (zerop rx) 1 rx)))))
    (let ((sweep (mod (- (angle ex ey) (angle ix iy)) (* 2 pi))))
      (format nil "M ~A ~A A ~A ~A 0 ~D 1 ~A ~A~A"
              (svg-num ix) (svg-num iy) (svg-num rx) (svg-num ry)
              (if (> sweep pi) 1 0) (svg-num ex) (svg-num ey)
              (if closed (format nil " L ~A ~A Z" (svg-num cx) (svg-num cy)) "")))))

(defun xy (p)
  (format nil "~A ~A" (svg-num (car p)) (svg-num (cdr p))))

(defun polyline-path (points matrix)
  (with-output-to-string (s)
    (loop for p across points
          for first = t then nil
          do (format s "~:[ L~;M~] ~A" first (xy (apply-matrix matrix p))))))

(defun polycurve-path (nodes closed &optional (matrix '(1 0 0 1 0 0)))
  ;; Node = (incoming-control anchor outgoing-control). As in libwpg, a
  ;; closed curve is closed with a straight segment.
  (flet ((pt (p) (xy (apply-matrix matrix p))))
    (with-output-to-string (s)
      (when (plusp (length nodes))
        (format s "M ~A" (pt (second (aref nodes 0))))
        (loop for i from 1 below (length nodes)
              for prev = (aref nodes (1- i))
              for node = (aref nodes i)
              do (format s " C ~A ~A ~A" (pt (third prev)) (pt (first node)) (pt (second node))))
        (when closed (format s " Z"))))))

(defun compose-matrices (outer inner)
  "The matrix applying INNER, then OUTER."
  (destructuring-bind (a1 b1 c1 d1 e1 f1) outer
    (destructuring-bind (a2 b2 c2 d2 e2 f2) inner
      (list (+ (* a1 a2) (* c1 b2)) (+ (* b1 a2) (* d1 b2))
            (+ (* a1 c2) (* c1 d2)) (+ (* b1 c2) (* d1 d2))
            (+ (* a1 e2) (* c1 f2) e1) (+ (* b1 e2) (* d1 f2) f1)))))

(defun compound-path (rs compound children)
  "The path data for a compound polygon: its polyline and polycurve
children joined. Other children are drawn on their own."
  (let ((outer (getf (record-field compound :characterization) :matrix '(1 0 0 1 0 0))))
    (with-output-to-string (s)
      (dolist (child children)
        (let* ((ch (record-field child :characterization))
               ;; libwpg applies the compound's transform, then the child's.
               (m (compose-matrices (getf ch :matrix '(1 0 0 1 0 0)) outer)))
          (case (and (not (wpg-record-error child)) (wpg-record-name child))
            ((:polyline :polyspline)
             (format s "~A " (polyline-path (record-field child :points) m)))
            (:polycurve
             (format s "~A " (polycurve-path (record-field child :nodes) (getf ch :closed) m)))
            ((nil))
            (t (render-wpg2-tree rs child :compound))))))))

(defun render-compound (rs record children)
  (let* ((ch (record-field record :characterization))
         (d (string-right-trim " " (compound-path rs record children))))
    (when (plusp (length d))
      (with-characterization (rs ch :transform nil)
        (emit rs "<path d=\"~A~:[~; Z~]\"~A/>" d (getf ch :closed)
              (style-attributes rs :fill-rule (fill-rule ch)))))))

(defun set-brush-color (rs f)
  (cond ((and (field-value f :colors) (= 2 (length (field-value f :colors))))
         (setf (rs-gradient rs) f))
        (t (let ((c (or (field-value f :color)
                        (let ((cs (field-value f :colors))) (and cs (plusp (length cs)) (aref cs 0))))))
             (when c
               (setf (rs-gradient rs) nil
                     (rs-fill rs) (color-hex c)
                     (rs-fill-opacity rs) (opacity-of c)))))))

(defun field-value (fields key) (getf fields key))

(defun render-wpg2-record (rs record context)
  "Draw one record. CONTEXT is :TOP, :PAGE (inside Start WPG) or :COMPOUND;
pen and brush records only count at the top level and inside groups."
  (let ((f (wpg-record-fields record))
        (styles (not (member context '(:page :compound)))))
    (flet ((field (k) (getf f k)))
      (let ((ch (field :characterization)))
        (case (wpg-record-name record)
          ((:pen-fore-color :dp-pen-fore-color)
           (when styles
             (setf (rs-stroke rs) (color-hex (field :color))
                   (rs-stroke-opacity rs) (opacity-of (field :color)))))
          ((:brush-fore-color :dp-brush-fore-color)
           (when styles (set-brush-color rs f)))
          ((:brush-gradient :dp-brush-gradient)
           (when styles
             (setf (rs-gradient-angle rs) (field :angle)
                   (rs-gradient-ref rs)
                   (and (/= #xffff (field :x-ref)) (/= #xffff (field :y-ref))
                        ;; libwpg's position of the near color along the ramp.
                        (let* ((x (/ (field :x-ref) 65536d0)) (y (/ (field :y-ref) 65536d0))
                               (tan (tan (* pi (/ (field :angle) 180)))))
                          (list (min 1d0 (max 0d0 (if (< -100 tan 100)
                                                      (/ (+ y (* x tan)) (+ 1 tan))
                                                      x)))))))))
          ((:pen-size :dp-pen-size)
           (when styles (setf (rs-stroke-width rs) (field :width))))
          (:pen-style
           (when styles (setf (rs-dash rs) (dash-for-style rs (field :style)))))
          (:pen-style-definition
           (setf (gethash (field :style) (rs-pen-styles rs))
                 (loop for (dash gap) in (field :segments) append (list dash gap))))
          ((:text-line :text-block)
           (setf (rs-pending-text rs) record))
          (:text-data
           (let ((anchor (shiftf (rs-pending-text rs) nil)))
             (when anchor (render-wpg2-text rs anchor (field :runs)))))
          ((:polyline :polyspline)
           (with-characterization (rs ch)
             (if (and (or (getf ch :filled) (getf ch :closed)) (> (length (field :points)) 2))
                 (emit rs "<polygon points=\"~A\"~A/>" (points-attribute (field :points))
                       (style-attributes rs :fill-rule (fill-rule ch)))
                 (emit rs "<polyline points=\"~A\"~A/>" (points-attribute (field :points))
                       (style-attributes rs :fill nil)))))
          (:polycurve
           (with-characterization (rs ch)
             (emit rs "<path d=\"~A\"~A/>"
                   (polycurve-path (field :nodes) (getf ch :closed))
                   (style-attributes rs :fill-rule (fill-rule ch)))))
          (:rectangle
           (with-characterization (rs ch)
             (let ((x1 (field :x1)) (y1 (field :y1)) (x2 (field :x2)) (y2 (field :y2)))
               (emit rs "<rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\"~A~A~A/>"
                     (svg-num (min x1 x2)) (svg-num (min y1 y2))
                     (svg-num (abs (- x2 x1))) (svg-num (abs (- y2 y1)))
                     (if (plusp (field :rx)) (format nil " rx=\"~A\"" (svg-num (field :rx))) "")
                     (if (plusp (field :ry)) (format nil " ry=\"~A\"" (svg-num (field :ry))) "")
                     (style-attributes rs)))))
          (:arc
           (with-characterization (rs ch)
             (let ((cx (field :cx)) (cy (field :cy)) (rx (abs (field :rx))) (ry (abs (field :ry)))
                   (ix (field :ix)) (iy (field :iy)) (ex (field :ex)) (ey (field :ey)))
               (if (and (= ix ex) (= iy ey))
                   (emit rs "<ellipse cx=\"~A\" cy=\"~A\" rx=\"~A\" ry=\"~A\"~A/>"
                         (svg-num cx) (svg-num cy) (svg-num rx) (svg-num ry)
                         (style-attributes rs))
                   (emit rs "<path d=\"~A\"~A/>"
                         (wpg2-arc-path cx cy rx ry ix iy ex ey (getf ch :closed))
                         (style-attributes rs :fill (getf ch :closed)))))))
          (:bitmap
           (setf (rs-pending-bitmap-ch rs) ch))
          (:bitmap-data
           (let ((ch (shiftf (rs-pending-bitmap-ch rs) nil)))
             (if (and ch (not (identity-matrix-p (getf ch :matrix))))
                 (with-characterization (rs ch) (emit-bitmap rs (field :bitmap)))
                 (emit-bitmap rs (field :bitmap))))))))))

(defun render-wpg2-text (rs anchor runs)
  "Draw the Text Data RUNS at the place the Text Line or Text Block ANCHOR
gives. Sizes are in points; 12 when the text never sets one."
  (let* ((f (wpg-record-fields anchor))
         (matrix (getf (getf f :characterization) :matrix '(1 0 0 1 0 0)))
         (units (wpg-image-y-units (rs-image rs)))
         (size-of (lambda (run) (and (getf run :size) (* units (/ (getf run :size) 72)))))
         (size (or (some size-of runs) (* units 1/6))))
    (if (eq (wpg-record-name anchor) :text-line)
        (let ((p (apply-matrix matrix (cons (getf f :x) (getf f :y)))))
          (emit-text rs (car p) (cdr p) runs :size size :size-of size-of
                     :angle (getf f :angle) :h-align (getf f :h-align)
                     :v-align (getf f :v-align)))
        ;; A block: lines run down from its top-left corner.
        (let ((p (apply-matrix matrix (cons (min (getf f :x1) (getf f :x2))
                                            (max (getf f :y1) (getf f :y2))))))
          (emit-text rs (car p) (- (cdr p) size) runs :size size :size-of size-of)))))

(defun render-wpg2-tree (rs record context)
  (let ((children (funcall (rs-children-of rs) record)))
    (cond ((wpg-record-error record)
           (dolist (c children) (render-wpg2-tree rs c context)))
          ((eq (wpg-record-name record) :compound-polygon)
           (render-compound rs record children))
          (t
           (render-wpg2-record rs record context)
           (let ((child-context (case (wpg-record-name record)
                                  (:start-wpg :page)
                                  (t context))))
             (dolist (c children)
               (render-wpg2-tree rs c child-context)))))))

(defun render-wpg2 (rs image)
  (let* ((records (wpg-image-records image))
         (children (make-hash-table)))
    (loop for r across records
          for parent = (wpg-record-parent r)
          when parent do (push r (gethash parent children)))
    (setf (rs-children-of rs)
          (lambda (r) (reverse (gethash (wpg-record-index r) children))))
    (loop for r across records
          unless (wpg-record-parent r)
            do (render-wpg2-tree rs r :top))))

;;; Entry points

(defun write-svg (image stream &key bitmap-href)
  "Write IMAGE (a WPG-IMAGE) to STREAM as an SVG document.
BITMAP-HREF, if given, is called with each WPG-BITMAP and should return a
URI for it (a file name, or a data: URI) or NIL to leave it out; without
it, bitmaps are left out."
  (let* ((w (max 1 (wpg-image-width image)))
         (h (max 1 (wpg-image-height image)))
         (rs (make-render-state :stream stream :image image
                                :palette (copy-palette
                                          (if (= (wpg-image-version image) 1)
                                              (default-wpg1-palette)
                                              (default-wpg2-palette)))
                                :bitmap-href bitmap-href)))
    (when (= (wpg-image-version image) 2)
      ;; WPG2 defaults, as in libwpg: black pen and black brush.
      (setf (rs-fill rs) "#000000"))
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format stream "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"~Ain\" height=\"~Ain\" viewBox=\"0 0 ~A ~A\">~%"
            (svg-num (/ w (max 1 (wpg-image-x-units image))))
            (svg-num (/ h (max 1 (wpg-image-y-units image))))
            (svg-num w) (svg-num h))
    ;; File space (y up, origin at the offset) to SVG space (y down).
    (format stream "<g transform=\"matrix(1 0 0 -1 ~A ~A)\">~%"
            (svg-num (- (wpg-image-x-offset image)))
            (svg-num (+ h (wpg-image-y-offset image))))
    (setf (rs-draw-text-2 rs) (null (find-records image :graphics-text)))
    (if (= (wpg-image-version image) 1)
        (loop for record across (wpg-image-records image)
              unless (wpg-record-error record)
                do (render-wpg1-record rs record))
        (render-wpg2 rs image))
    (format stream "</g>~%</svg>~%")
    image))

(defun svg-string (image &key bitmap-href)
  "IMAGE rendered as an SVG document string. See WRITE-SVG."
  (with-output-to-string (s)
    (write-svg image s :bitmap-href bitmap-href)))
