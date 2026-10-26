;;;; scene.lisp -- a WPG drawing as a list of resolved shapes.
;;;;
;;;; BUILD-SCENE replays a file's records in order, tracking the current pen,
;;;; brush, palette and text attributes, and produces a SCENE: the page size
;;;; and the shapes to draw, each with its style already worked out. Output
;;;; formats (SVG, PNG) only have to draw shapes.
;;;;
;;;; Coordinates are the file's: y points up, and the page runs from
;;;; (X-OFFSET, Y-OFFSET) to (X-OFFSET + WIDTH, Y-OFFSET + HEIGHT). A shape's
;;;; TRANSFORM, when not NIL, is a matrix (a b c d e f) mapping its own
;;;; coordinates (x, y) to (ax + cy + e, bx + dy + f) in file space.
;;;;
;;;; WPG2 follows libwpg (what LibreOffice and Inkscape use): records nest,
;;;; compound polygons join their children into one path, and pen and brush
;;;; records inside a compound polygon or the Start WPG page attributes are
;;;; ignored.
;;;;
;;;; Not drawn: fill patterns (drawn solid), gradients with more than two
;;;; colors (drawn with their first color), text paths and object images.

(in-package #:wordperfect-graphics)

;;; The model

(defstruct scene
  "A drawing ready to render. See BUILD-SCENE."
  (width 1) (height 1)
  (x-offset 0) (y-offset 0)
  (x-units 1200) (y-units 1200)
  (shapes '()))

(defstruct gradient
  "A linear gradient across a shape's bounding box. ANGLE is the file's
gradient angle in degrees: 0 runs the ramp from top to bottom, and larger
angles turn it clockwise (libwpg's reading). STOPS is a list of
(offset . color), offsets from 0 to 1, along the ramp."
  (angle 0) (stops '()))

(defun gradient-direction (gradient)
  "The unit vector the ramp runs along, in the file's y-up space."
  (let ((a (* pi (/ (gradient-angle gradient) 180))))
    (values (- (sin a)) (- (cos a)))))

(defun matrix-scale (m)
  "How much M scales lengths, roughly."
  (destructuring-bind (a b c d &rest rest) m
    (declare (ignore rest))
    (sqrt (abs (- (* a d) (* b c))))))

(defstruct texture
  "A bitmap tiled across the plane, each tile WIDTH by HEIGHT file units,
starting from the origin. COLOR is its average color, for output formats
that cannot tile."
  bitmap width height (color (make-color 128 128 128)))

(defstruct style
  "How a shape is painted. FILL is a COLOR, a GRADIENT, a TEXTURE, or NIL for none;
FILL-RULE is :NONZERO or :EVENODD (or NIL when unspecified). STROKE is a
COLOR or NIL. STROKE-WIDTH is in file units, 0 meaning a hairline. DASH is
a list of dash and gap lengths in file units, or NIL for a solid line.
Opacities run from 0 to 1."
  (fill nil) (fill-opacity 1) (fill-rule nil)
  (stroke nil) (stroke-opacity 1) (stroke-width 0) (dash nil))

(defstruct shape
  "The common part of every shape."
  (transform nil) (style nil))

(defstruct (line-shape (:include shape)) x1 y1 x2 y2)

(defstruct (poly-shape (:include shape))
  "A polyline, or a polygon when CLOSED. POINTS is a vector of (x . y)."
  points closed)

(defstruct (rect-shape (:include shape)) x y width height (rx 0) (ry 0))

(defstruct (ellipse-shape (:include shape)) cx cy rx ry)

(defstruct (path-shape (:include shape))
  "COMMANDS is a list of (:MOVE x y), (:LINE x y), (:CURVE x1 y1 x2 y2 x y)
for a cubic Bezier, (:ARC rx ry rotation large-arc sweep x y) as in SVG,
and (:CLOSE)."
  commands)

(defstruct (text-shape (:include shape))
  "Text whose first baseline starts at (X, Y), turned ANGLE degrees
counterclockwise. LINES is a list of lines, each a list of runs; a run is a
plist with :TEXT and optionally :SIZE (file units), :COLOR and :FONT. SIZE,
COLOR and FONT are the defaults for the runs. H-ALIGN is 0 left, 1 center,
2 right; V-ALIGN 0 baseline, 1 top, 2 middle, 3 bottom. CHAR-WIDTH, when
set, is the width each character should take."
  x y lines (size 200) char-width (angle 0) (h-align 0) (v-align 0)
  (color (make-color 0 0 0)) font)

(defstruct (image-shape (:include shape))
  "BITMAP drawn into the rectangle X1 Y1 X2 Y2 (file space, X1 < X2 and
Y1 < Y2), mirrored and turned as the bitmap says."
  bitmap x1 y1 x2 y2)

(defun text-line-advance (line default-size)
  "How far below the previous baseline LINE's baseline is."
  (* 1.2 (reduce #'max line :key (lambda (r) (or (getf r :size) default-size))
                            :initial-value default-size)))

;;; Building

(defstruct (builder (:conc-name rs-))
  image
  palette
  (shapes '())
  ;; Pen
  (stroke (make-color 0 0 0))     ; COLOR, or NIL for none
  (stroke-opacity 1)
  (stroke-width 0)                ; 0 = hairline
  ;; Brush
  (fill nil)
  (fill-opacity 1)
  ;; WPG1 markers
  (marker-color (make-color 0 0 0))
  (marker-size 60)
  ;; WPG2: the characterization of the Bitmap record awaiting its data
  (pending-bitmap-ch nil)
  ;; WPG2: the brush gradient's fields, or NIL for a solid brush
  (gradient nil)
  (gradient-angle 0)
  (gradient-ref nil)
  ;; WPG2: record -> its child records
  (children-of (constantly nil))
  ;; Dashes: a list of dash/gap lengths in pen-style units, or NIL
  (dash nil)
  ;; WPG2: pen style number -> dash list, from Pen Style Definitions
  (pen-styles (make-hash-table))
  ;; WPG1: the current Graphics Text Attributes fields
  (text-attributes nil)
  ;; WPG1: draw Graphics Text (Type 2) only in files without Type 1 text
  (draw-text-2 nil)
  ;; WPG2: the Text Line or Text Block awaiting its Text Data
  (pending-text nil)
  ;; WPG2: the transform for shapes being added, from the characterization
  (transform nil)
  ;; WPG2: the transform of the groups enclosing the records being built
  (group-matrix nil)
  ;; WPG2: texture ID -> TEXTURE, the current brush texture, and the one
  ;; the Start WPG page attributes fill the page with
  (textures (make-hash-table))
  (texture nil)
  (pending-texture-id nil))

(defun add-shape (rs shape)
  (when (and (rs-transform rs) (null (shape-transform shape)))
    (setf (shape-transform shape) (rs-transform rs)))
  (when (rs-group-matrix rs)
    (setf (shape-transform shape)
          (if (shape-transform shape)
              (compose-matrices (rs-group-matrix rs) (shape-transform shape))
              (rs-group-matrix rs))))
  (push shape (rs-shapes rs))
  shape)

(defun palette-color (rs index)
  (let ((palette (rs-palette rs)))
    (if (< index (length palette))
        (aref palette index)
        (make-color 0 0 0))))

(defun opacity-of (color)
  (/ (color-alpha color) 255))

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
    (mapcar (lambda (v) (* v 36/2180 72 width)) (rs-dash rs))))

(defun current-gradient (rs)
  (destructuring-bind (&key colors &allow-other-keys) (rs-gradient rs)
    ;; libwpg: the far color starts the ramp, the near one sits at a
    ;; reference point and, when a reference is given, the ramp mirrors
    ;; back to the far color.
    (let ((far (aref colors 1)) (near (aref colors 0))
          (ref (rs-gradient-ref rs)))
      (make-gradient :angle (rs-gradient-angle rs)
                     :stops (if ref
                                (list (cons 0 far) (cons (first ref) near) (cons 1 far))
                                (list (cons 0 far) (cons 1 near)))))))

(defun effective-stroke-width (rs)
  "The pen width, or 0 (a hairline) when it would come out thinner than
1/300 inch once the shape's transforms are applied; WordPerfect never drew
a line thinner than a device pixel."
  (let* ((width (rs-stroke-width rs))
         (scale (* (if (rs-transform rs) (matrix-scale (rs-transform rs)) 1)
                   (if (rs-group-matrix rs) (matrix-scale (rs-group-matrix rs)) 1))))
    (if (< (* width scale) (/ (wpg-image-x-units (rs-image rs)) 300))
        0
        width)))

(defun current-style (rs &key (fill t) (stroke t) fill-rule)
  "The style for a shape drawn now, from the current pen and brush."
  (make-style :fill (and fill (rs-fill rs)
                         (cond ((rs-texture rs))
                               ((rs-gradient rs) (current-gradient rs))
                               (t (rs-fill rs))))
              :fill-opacity (rs-fill-opacity rs)
              :fill-rule (and fill fill-rule)
              :stroke (and stroke (rs-stroke rs))
              :stroke-opacity (rs-stroke-opacity rs)
              :stroke-width (effective-stroke-width rs)
              :dash (and stroke (rs-stroke rs) (rs-dash rs) (dash-lengths rs))))

;;; Bitmaps

(defun add-bitmap-shape (rs bitmap)
  (let ((image (rs-image rs)))
    (multiple-value-bind (x1 y1 x2 y2)
        (if (wpg-bitmap-x1 bitmap)
            (values (min (wpg-bitmap-x1 bitmap) (wpg-bitmap-x2 bitmap))
                    (min (wpg-bitmap-y1 bitmap) (wpg-bitmap-y2 bitmap))
                    (max (wpg-bitmap-x1 bitmap) (wpg-bitmap-x2 bitmap))
                    (max (wpg-bitmap-y1 bitmap) (wpg-bitmap-y2 bitmap)))
            ;; No placement: natural size, at the top-left corner.
            (let* ((w (* (wpg-bitmap-width bitmap)
                         (/ (wpg-image-x-units image) (max 1 (wpg-bitmap-h-resolution bitmap)))))
                   (h (* (wpg-bitmap-height bitmap)
                         (/ (wpg-image-y-units image) (max 1 (wpg-bitmap-v-resolution bitmap)))))
                   (left (wpg-image-x-offset image))
                   (top (+ (wpg-image-y-offset image) (wpg-image-height image))))
              (values left (- top h) (+ left w) top)))
      (add-shape rs (make-image-shape :bitmap bitmap :x1 x1 :y1 y1 :x2 x2 :y2 y2)))))

;;; Text

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

(defun add-text (rs x y runs &rest keys &key &allow-other-keys)
  (add-shape rs (apply #'make-text-shape :x x :y y :lines (split-runs-into-lines runs) keys)))

;;; WPG1

(defun rotation-matrix (degrees cx cy)
  "Rotation by DEGREES counterclockwise about (CX, CY)."
  (let* ((a (* pi (/ degrees 180))) (c (cos a)) (s (sin a)))
    (list c s (- s) c (+ (- cx (* c cx)) (* s cy)) (- cy (* s cx) (* c cy)))))

(defun wpg1-arc-commands (cx cy rx ry start end)
  (flet ((pt (deg)
           (let ((a (* pi (/ deg 180))))
             (list (+ cx (* rx (cos a))) (+ cy (* ry (sin a)))))))
    `((:move ,@(pt start))
      (:arc ,rx ,ry 0 ,(if (> (mod (- end start) 360) 180) 1 0) 1 ,@(pt end)))))

(defun build-wpg1-record (rs record)
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
         (setf (rs-stroke rs) (if (zerop (field :style)) nil (palette-color rs (field :color)))
               (rs-stroke-width rs) (field :width)
               (rs-dash rs) (and (> (field :style) 1) (dash-for-style rs (1- (field :style))))))
        (:graphics-text-attributes
         (setf (rs-text-attributes rs) f))
        (:fill-attributes
         (setf (rs-fill rs) (if (zerop (field :style)) nil (palette-color rs (field :color)))))
        (:marker-attributes
         (setf (rs-marker-color rs) (palette-color rs (field :color))
               (rs-marker-size rs) (max 1 (field :height))))
        (:line
         (add-shape rs (make-line-shape :x1 (field :x1) :y1 (field :y1) :x2 (field :x2) :y2 (field :y2)
                                        :style (current-style rs :fill nil))))
        (:polyline
         (add-shape rs (make-poly-shape :points (field :points)
                                        :style (current-style rs :fill nil))))
        (:polygon
         (add-shape rs (make-poly-shape :points (field :points) :closed t
                                        :style (current-style rs))))
        (:rectangle
         (let ((x (field :x)) (y (field :y)) (w (field :width)) (h (field :height)))
           (add-shape rs (make-rect-shape :x (min x (+ x w)) :y (min y (+ y h))
                                          :width (abs w) :height (abs h)
                                          :style (current-style rs)))))
        (:ellipse
         (let* ((cx (field :cx)) (cy (field :cy)) (rx (abs (field :rx))) (ry (abs (field :ry)))
                (start (field :start-angle)) (end (field :end-angle))
                (transform (and (/= 0 (mod (field :rotation) 360))
                                (rotation-matrix (field :rotation) cx cy))))
           (add-shape rs (if (zerop (mod (- end start) 360))
                             (make-ellipse-shape :cx cx :cy cy :rx rx :ry ry :transform transform
                                                 :style (current-style rs))
                             (make-path-shape :commands (wpg1-arc-commands cx cy rx ry start end)
                                              :transform transform
                                              :style (current-style rs :fill nil))))))
        ((:curved-polyline :curved-polygon)
         (let ((pts (field :points))
               (closed (eq (wpg-record-name record) :curved-polygon)))
           (when (plusp (length pts))
             (add-shape rs (make-path-shape
                            :commands (append
                                       (list (list :move (car (aref pts 0)) (cdr (aref pts 0))))
                                       (loop for i from 1 below (- (length pts) 2) by 3
                                             collect (list :curve
                                                           (car (aref pts i)) (cdr (aref pts i))
                                                           (car (aref pts (+ i 1))) (cdr (aref pts (+ i 1)))
                                                           (car (aref pts (+ i 2))) (cdr (aref pts (+ i 2)))))
                                       (when closed (list (list :close))))
                            :style (current-style rs :fill closed))))))
        (:polymarker
         (loop for p across (field :points)
               do (add-shape rs (make-ellipse-shape
                                 :cx (car p) :cy (cdr p)
                                 :rx (/ (rs-marker-size rs) 2) :ry (/ (rs-marker-size rs) 2)
                                 :style (make-style :fill (rs-marker-color rs))))))
        (:graphics-text
         (let ((a (rs-text-attributes rs)))
           (add-text rs (field :x) (field :y)
                     (list (list :text (field :text)))
                     :size (or (getf a :height) 200)
                     :char-width (getf a :width)
                     :angle (or (getf a :angle) 0)
                     :h-align (getf a :h-align 0)
                     :v-align (getf a :v-align 0)
                     :color (palette-color rs (getf a :color 0)))))
        (:graphics-text-type-2
         (when (rs-draw-text-2 rs)
           ;; Only the box is known: fit the lines into it.
           (let* ((lines (max 1 (1+ (count #\Newline (field :text)))))
                  (size (* 0.6 (/ (abs (- (field :y2) (field :y1))) lines))))
             (add-text rs (min (field :x1) (field :x2))
                       (- (max (field :y1) (field :y2)) size)
                       (list (list :text (field :text)))
                       :size size :angle (field :rotation)
                       :color (palette-color rs 0)))))
        ((:bitmap-type-1 :bitmap-type-2)
         (add-bitmap-shape rs (field :bitmap)))))))

;;; WPG2

(defun identity-matrix-p (m)
  (or (null m) (equal m '(1 0 0 1 0 0))))

(defun apply-matrix (m point)
  (destructuring-bind (a b c d e f) m
    (let ((x (car point)) (y (cdr point)))
      (cons (+ (* a x) (* c y) e) (+ (* b x) (* d y) f)))))

(defun compose-matrices (outer inner)
  "The matrix applying INNER, then OUTER."
  (destructuring-bind (a1 b1 c1 d1 e1 f1) outer
    (destructuring-bind (a2 b2 c2 d2 e2 f2) inner
      (list (+ (* a1 a2) (* c1 b2)) (+ (* b1 a2) (* d1 b2))
            (+ (* a1 c2) (* c1 d2)) (+ (* b1 c2) (* d1 d2))
            (+ (* a1 e2) (* c1 f2) e1) (+ (* b1 e2) (* d1 f2) f1)))))

(defun fill-rule (ch)
  (if (getf ch :winding-rule) :nonzero :evenodd))

(defun call-with-characterization (rs ch thunk &key (transform t))
  "Run THUNK with the pen and brush adjusted for an object's FILLED and
FRAMED flags, and with its transform applied to the shapes it adds."
  (let ((matrix (getf ch :matrix))
        (saved-fill (rs-fill rs))
        (saved-stroke (rs-stroke rs))
        (saved-transform (rs-transform rs)))
    (unless (getf ch :filled) (setf (rs-fill rs) nil))
    (unless (getf ch :framed) (setf (rs-stroke rs) nil))
    (when (and transform (not (identity-matrix-p matrix)))
      (setf (rs-transform rs) matrix))
    (unwind-protect (funcall thunk)
      (setf (rs-fill rs) saved-fill
            (rs-stroke rs) saved-stroke
            (rs-transform rs) saved-transform))))

(defmacro with-characterization ((rs ch &rest keys) &body body)
  `(call-with-characterization ,rs ,ch (lambda () ,@body) ,@keys))

(defun wpg2-arc-commands (cx cy rx ry ix iy ex ey closed)
  (flet ((angle (x y)
           (atan (/ (- y cy) (if (zerop ry) 1 ry))
                 (/ (- x cx) (if (zerop rx) 1 rx)))))
    (let ((sweep (mod (- (angle ex ey) (angle ix iy)) (* 2 pi))))
      `((:move ,ix ,iy)
        (:arc ,rx ,ry 0 ,(if (> sweep pi) 1 0) 1 ,ex ,ey)
        ,@(when closed `((:line ,cx ,cy) (:close)))))))

(defun polyline-commands (points matrix)
  (loop for p across points
        for first = t then nil
        collect (let ((q (apply-matrix matrix p)))
                  (list (if first :move :line) (car q) (cdr q)))))

(defun polycurve-commands (nodes closed &optional (matrix '(1 0 0 1 0 0)))
  ;; Node = (incoming-control anchor outgoing-control). As in libwpg, a
  ;; closed curve is closed with a straight segment.
  (flet ((pt (p) (let ((q (apply-matrix matrix p))) (list (car q) (cdr q)))))
    (when (plusp (length nodes))
      (append (list (cons :move (pt (second (aref nodes 0)))))
              (loop for i from 1 below (length nodes)
                    for prev = (aref nodes (1- i))
                    for node = (aref nodes i)
                    collect `(:curve ,@(pt (third prev)) ,@(pt (first node)) ,@(pt (second node))))
              (when closed (list (list :close)))))))

(defun compound-commands (rs compound children)
  "The path commands for a compound polygon: its polyline and polycurve
children joined. Other children are drawn on their own."
  (let ((outer (getf (record-field compound :characterization) :matrix '(1 0 0 1 0 0))))
    (loop for child in children
          append (let* ((ch (record-field child :characterization))
                        ;; libwpg applies the compound's transform, then the child's.
                        (m (compose-matrices (getf ch :matrix '(1 0 0 1 0 0)) outer)))
                   (case (and (not (wpg-record-error child)) (wpg-record-name child))
                     ((:polyline :polyspline) (polyline-commands (record-field child :points) m))
                     (:polycurve (polycurve-commands (record-field child :nodes) (getf ch :closed) m))
                     ((nil) nil)
                     (t (build-wpg2-tree rs child :compound) nil))))))

(defun build-compound (rs record children)
  (let* ((ch (record-field record :characterization))
         (commands (compound-commands rs record children)))
    (when commands
      (with-characterization (rs ch :transform nil)
        (add-shape rs (make-path-shape :commands (if (getf ch :closed)
                                                     (append commands (list (list :close)))
                                                     commands)
                                       :style (current-style rs :fill-rule (fill-rule ch))))))))

(defun set-brush-color (rs f)
  (let ((colors (getf f :colors)))
    (if (and colors (= 2 (length colors)))
        (setf (rs-gradient rs) f)
        (let ((c (or (getf f :color) (and colors (plusp (length colors)) (aref colors 0)))))
          (when c
            (setf (rs-gradient rs) nil
                  (rs-texture rs) nil
                  (rs-fill rs) c
                  (rs-fill-opacity rs) (opacity-of c)))))))

(defun build-wpg2-text (rs anchor runs)
  "Add the Text Data RUNS at the place the Text Line or Text Block ANCHOR
gives. Sizes are in points; 12 when the text never sets one."
  (let* ((f (wpg-record-fields anchor))
         (matrix (getf (getf f :characterization) :matrix '(1 0 0 1 0 0)))
         (units (wpg-image-y-units (rs-image rs)))
         (runs (mapcar (lambda (run)
                         (let ((pt (getf run :size)))
                           (list* :size (and pt (* units (/ pt 72)))
                                  (loop for (k v) on run by #'cddr
                                        unless (eq k :size) append (list k v)))))
                       runs))
         (size (or (some (lambda (r) (getf r :size)) runs) (* units 1/6))))
    (if (eq (wpg-record-name anchor) :text-line)
        (let ((p (apply-matrix matrix (cons (getf f :x) (getf f :y)))))
          (add-text rs (car p) (cdr p) runs :size size
                    :angle (getf f :angle) :h-align (getf f :h-align)
                    :v-align (getf f :v-align)))
        ;; A block: lines run down from its top-left corner.
        (let ((p (apply-matrix matrix (cons (min (getf f :x1) (getf f :x2))
                                            (max (getf f :y1) (getf f :y2))))))
          (add-text rs (car p) (- (cdr p) size) runs :size size)))))

(defun build-wpg2-record (rs record context)
  "Add one record. CONTEXT is :TOP, :PAGE (inside Start WPG), :COMPOUND or
:TEXTURE (inside a Texture Definition). Pen and brush records are ignored
inside a compound polygon; inside Start WPG they describe the page."
  (let ((f (wpg-record-fields record))
        (styles (not (eq context :compound))))
    (flet ((field (k) (getf f k)))
      (let ((ch (field :characterization)))
        (case (wpg-record-name record)
          ((:pen-fore-color :dp-pen-fore-color)
           (when styles
             (setf (rs-stroke rs) (field :color)
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
             (when anchor (build-wpg2-text rs anchor (field :runs)))))
          ((:polyline :polyspline)
           (with-characterization (rs ch)
             (if (and (or (getf ch :filled) (getf ch :closed)) (> (length (field :points)) 2))
                 (add-shape rs (make-poly-shape :points (field :points) :closed t
                                                :style (current-style rs :fill-rule (fill-rule ch))))
                 (add-shape rs (make-poly-shape :points (field :points)
                                                :style (current-style rs :fill nil))))))
          (:polycurve
           (with-characterization (rs ch)
             (add-shape rs (make-path-shape :commands (polycurve-commands (field :nodes) (getf ch :closed))
                                            :style (current-style rs :fill-rule (fill-rule ch))))))
          (:rectangle
           (with-characterization (rs ch)
             (let ((x1 (field :x1)) (y1 (field :y1)) (x2 (field :x2)) (y2 (field :y2)))
               (add-shape rs (make-rect-shape :x (min x1 x2) :y (min y1 y2)
                                              :width (abs (- x2 x1)) :height (abs (- y2 y1))
                                              :rx (max 0 (field :rx)) :ry (max 0 (field :ry))
                                              :style (current-style rs))))))
          (:arc
           (with-characterization (rs ch)
             (let ((cx (field :cx)) (cy (field :cy)) (rx (abs (field :rx))) (ry (abs (field :ry)))
                   (ix (field :ix)) (iy (field :iy)) (ex (field :ex)) (ey (field :ey)))
               (add-shape rs (if (and (= ix ex) (= iy ey))
                                 (make-ellipse-shape :cx cx :cy cy :rx rx :ry ry
                                                     :style (current-style rs))
                                 (make-path-shape :commands (wpg2-arc-commands cx cy rx ry ix iy ex ey
                                                                               (getf ch :closed))
                                                  :style (current-style rs :fill (getf ch :closed))))))))
          (:bitmap
           (setf (rs-pending-bitmap-ch rs) ch))
          (:bitmap-data
           (if (eq context :texture)
               (register-texture rs (field :bitmap))
               (let ((ch (shiftf (rs-pending-bitmap-ch rs) nil)))
                 (if (and ch (not (identity-matrix-p (getf ch :matrix))))
                     (with-characterization (rs ch) (add-bitmap-shape rs (field :bitmap)))
                     (add-bitmap-shape rs (field :bitmap))))))
          (:texture-definition
           (setf (rs-pending-texture-id rs) (field :texture-id))
           (setf (gethash (field :texture-id) (rs-textures rs))
                 (make-texture :color (field :color))))
          (:brush-texture
           (let ((texture (gethash (field :texture-id) (rs-textures rs))))
             (when (and texture (texture-bitmap texture) styles)
               (setf (rs-texture rs) texture)))))))))

(defun register-texture (rs bitmap)
  "BITMAP is the image of the texture being defined. Tiles are taken to be
at 96 pixels per inch."
  (let* ((image (rs-image rs))
         (texture (gethash (rs-pending-texture-id rs) (rs-textures rs))))
    (when texture
      (setf (texture-bitmap texture) bitmap
            (texture-width texture) (* (wpg-bitmap-width bitmap) (/ (wpg-image-x-units image) 96))
            (texture-height texture) (* (wpg-bitmap-height bitmap) (/ (wpg-image-y-units image) 96))))))

(defun build-wpg2-tree (rs record context)
  (let ((children (funcall (rs-children-of rs) record)))
    (cond ((wpg-record-error record)
           (dolist (c children) (build-wpg2-tree rs c context)))
          ((eq (wpg-record-name record) :compound-polygon)
           (build-compound rs record children))
          ((eq (wpg-record-name record) :group)
           ;; Members are drawn through the group's transform.
           (let ((saved (rs-group-matrix rs))
                 (matrix (getf (record-field record :characterization) :matrix)))
             (unless (identity-matrix-p matrix)
               (setf (rs-group-matrix rs) (if saved (compose-matrices saved matrix) matrix)))
             (unwind-protect
                  (dolist (c children) (build-wpg2-tree rs c context))
               (setf (rs-group-matrix rs) saved))))
          ((eq (wpg-record-name record) :start-wpg)
           (build-wpg2-record rs record context)
           (build-page rs children))
          (t
           (build-wpg2-record rs record context)
           (let ((child-context (case (wpg-record-name record)
                                  (:texture-definition :texture)
                                  (t context))))
             (dolist (c children)
               (build-wpg2-tree rs c child-context)))))))

(defun build-page (rs children)
  "Apply Start WPG's page attributes. Their brush paints the page when it is
a gradient or a texture (WordPerfect 8's backgrounds); a plain brush color
leaves the page clear. Either way, drawing then starts from libwpg's
defaults: black pen, black brush."
  (dolist (c children) (build-wpg2-tree rs c :page))
  (let ((paint (cond ((rs-texture rs))
                     ((rs-gradient rs) (current-gradient rs))))
        (image (rs-image rs)))
    (when paint
      (add-shape rs (make-rect-shape :x (wpg-image-x-offset image) :y (wpg-image-y-offset image)
                                     :width (wpg-image-width image) :height (wpg-image-height image)
                                     :style (make-style :fill paint))))
    (setf (rs-stroke rs) (make-color 0 0 0) (rs-stroke-opacity rs) 1 (rs-stroke-width rs) 0
          (rs-dash rs) nil
          (rs-fill rs) (make-color 0 0 0) (rs-fill-opacity rs) 1
          (rs-gradient rs) nil (rs-gradient-angle rs) 0 (rs-gradient-ref rs) nil
          (rs-texture rs) nil)))

(defun build-wpg2 (rs image)
  (let* ((records (wpg-image-records image))
         (children (make-hash-table)))
    (loop for r across records
          for parent = (wpg-record-parent r)
          when parent do (push r (gethash parent children)))
    (setf (rs-children-of rs)
          (lambda (r) (reverse (gethash (wpg-record-index r) children))))
    (loop for r across records
          unless (wpg-record-parent r)
            do (build-wpg2-tree rs r :top))))

(defun build-scene (image)
  "Replay the records of IMAGE (a WPG-IMAGE) into a SCENE: its page and
the shapes to draw, in order, each with its style resolved."
  (let ((rs (make-builder :image image
                          :palette (if (= (wpg-image-version image) 1)
                                       (default-wpg1-palette)
                                       (default-wpg2-palette)))))
    (if (= (wpg-image-version image) 1)
        (progn
          (setf (rs-draw-text-2 rs) (null (find-records image :graphics-text)))
          (loop for record across (wpg-image-records image)
                unless (wpg-record-error record)
                  do (build-wpg1-record rs record)))
        (progn
          ;; WPG2 defaults, as in libwpg: black pen and black brush.
          (setf (rs-fill rs) (make-color 0 0 0))
          (build-wpg2 rs image)))
    (make-scene :width (max 1 (wpg-image-width image))
                :height (max 1 (wpg-image-height image))
                :x-offset (wpg-image-x-offset image)
                :y-offset (wpg-image-y-offset image)
                :x-units (max 1 (wpg-image-x-units image))
                :y-units (max 1 (wpg-image-y-units image))
                :shapes (nreverse (rs-shapes rs)))))
