;;;; raster.lisp -- draw a SCENE into pixels (system wordperfect-graphics/raster).
;;;;
;;;; Shapes are built as cl-vectors paths in their own coordinates, flattened,
;;;; mapped through their transform into pixel space, and filled or stroked
;;;; with cl-aa's anti-aliasing. Text is drawn from TrueType outlines (zpb-ttf),
;;;; using the system's fonts; see *FONT-FILES*. Pixels are composited in a
;;;; premultiplied float buffer and returned as straight RGBA octets.

(in-package #:wordperfect-graphics)

(defparameter *font-files*
  '(:sans ("/System/Library/Fonts/Supplemental/Arial.ttf" "/Library/Fonts/Arial.ttf"
           "/System/Library/Fonts/Helvetica.ttc"
           "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" "/usr/share/fonts/TTF/DejaVuSans.ttf"
           "/usr/share/fonts/dejavu/DejaVuSans.ttf"
           "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
           "C:/Windows/Fonts/arial.ttf")
    :serif ("/System/Library/Fonts/Supplemental/Times New Roman.ttf" "/System/Library/Fonts/Times.ttc"
            "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf" "/usr/share/fonts/TTF/DejaVuSerif.ttf"
            "/usr/share/fonts/truetype/liberation/LiberationSerif-Regular.ttf"
            "C:/Windows/Fonts/times.ttf")
    :mono ("/System/Library/Fonts/Supplemental/Courier New.ttf" "/System/Library/Fonts/Courier.ttc"
           "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf" "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
           "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf"
           "C:/Windows/Fonts/cour.ttf"))
  "Candidate TrueType files for each kind of typeface, first found wins.
A kind with no file falls back to :SANS; with no font at all, text is not
drawn.")

;;; The canvas: premultiplied RGBA floats, row-major, top row first.

(defstruct (canvas (:constructor %make-canvas))
  width height
  (pixels nil :type (simple-array single-float (*))))

(defun make-canvas (width height background)
  (let ((pixels (make-array (* 4 width height) :element-type 'single-float :initial-element 0.0)))
    (when background
      (let ((a (/ (color-alpha background) 255.0)))
        (loop for i from 0 below (length pixels) by 4
              do (setf (aref pixels i) (* a (/ (color-red background) 255.0))
                       (aref pixels (+ i 1)) (* a (/ (color-green background) 255.0))
                       (aref pixels (+ i 2)) (* a (/ (color-blue background) 255.0))
                       (aref pixels (+ i 3)) a))))
    (%make-canvas :width width :height height :pixels pixels)))

(defun blend (canvas x y r g b a)
  "Composite straight-alpha color R G B at opacity A (0..1) over pixel X, Y."
  (when (and (< -1 x (canvas-width canvas)) (< -1 y (canvas-height canvas)) (plusp a))
    (let* ((p (canvas-pixels canvas))
           (r (float r 1.0)) (g (float g 1.0)) (b (float b 1.0)) (a (float a 1.0))
           (i (* 4 (+ x (* y (canvas-width canvas)))))
           (keep (- 1.0 a)))
      (setf (aref p i) (+ (* r a) (* (aref p i) keep))
            (aref p (+ i 1)) (+ (* g a) (* (aref p (+ i 1)) keep))
            (aref p (+ i 2)) (+ (* b a) (* (aref p (+ i 2)) keep))
            (aref p (+ i 3)) (+ a (* (aref p (+ i 3)) keep))))))

(defun canvas-octets (canvas)
  (let* ((p (canvas-pixels canvas))
         (out (make-octets (length p))))
    (loop for i from 0 below (length p) by 4
          do (let ((a (aref p (+ i 3))))
               (when (plusp a)
                 (flet ((c (k) (min 255 (max 0 (round (* 255 (/ (aref p (+ i k)) a)))))))
                   (setf (aref out i) (c 0) (aref out (+ i 1)) (c 1) (aref out (+ i 2)) (c 2))))
               (setf (aref out (+ i 3)) (min 255 (max 0 (round (* 255 a)))))))
    out))

;;; Geometry

(defun invert-matrix (m)
  (destructuring-bind (a b c d e f) m
    (let ((det (- (* a d) (* b c))))
      (if (< (abs det) 1d-12)
          nil
          (list (/ d det) (/ (- b) det) (/ (- c) det) (/ a det)
                (/ (- (* c f) (* d e)) det) (/ (- (* b e) (* a f)) det))))))

(defun pixel-matrix (scene sx sy)
  "File space to pixel space."
  (list sx 0 0 (- sy)
        (- (* sx (scene-x-offset scene)))
        (* sy (+ (scene-y-offset scene) (scene-height scene)))))

(defun point (x y) (paths:make-point (float x 1d0) (float y 1d0)))

(defun command-paths (commands)
  "cl-paths paths for path COMMANDS, one per subpath."
  (let ((paths '()) (path nil) (start nil) (here nil))
    (labels ((finish (closed)
               (when path
                 (push (cons path closed) paths)
                 (setf path nil)))
             (ensure-path ()
               ;; Drawing on after a close starts a new subpath where the
               ;; closed one began.
               (unless path
                 (setf path (paths:create-path :open-polyline))
                 (paths:path-reset path (or start (point 0 0))))))
      (dolist (c commands)
        (destructuring-bind (op &rest a) c
          (ecase op
            (:move (finish nil)
             (setf path (paths:create-path :open-polyline)
                   start (point (first a) (second a)) here start)
             (paths:path-reset path start))
            (:line (ensure-path)
             (setf here (point (first a) (second a)))
             (paths:path-extend path (paths:make-straight-line) here))
            (:curve (ensure-path)
             (destructuring-bind (x1 y1 x2 y2 x y) a
                      (setf here (point x y))
                      (paths:path-extend path (paths:make-bezier-curve (list (point x1 y1) (point x2 y2)))
                                         here)))
            (:arc (ensure-path)
             (destructuring-bind (rx ry rot large sweep x y) a
                    (setf here (point x y))
                    (paths:path-extend path (paths:make-arc (float rx 1d0) (float ry 1d0)
                                                            :x-axis-rotation (* pi (/ rot 180))
                                                            :large-arc-flag (= large 1)
                                                            :sweep-flag (= sweep 1))
                                       here)))
            (:close (finish t)))))
      (finish nil))
    (nreverse paths)))

(defun shape-local-paths (shape)
  "The shape's outline as a list of (path . closed-p), in its own coordinates."
  (etypecase shape
    (line-shape
     (list (cons (paths:make-simple-path (list (point (line-shape-x1 shape) (line-shape-y1 shape))
                                               (point (line-shape-x2 shape) (line-shape-y2 shape))))
                 nil)))
    (poly-shape
     (let ((pts (map 'list (lambda (p) (point (car p) (cdr p))) (poly-shape-points shape))))
       (when pts
         (list (cons (paths:make-simple-path pts (if (poly-shape-closed shape) :closed-polyline :open-polyline))
                     (poly-shape-closed shape))))))
    (rect-shape
     (let ((x (rect-shape-x shape)) (y (rect-shape-y shape)))
       (list (cons (paths:make-rectangle-path
                    x y (+ x (rect-shape-width shape)) (+ y (rect-shape-height shape))
                    :round-x (and (plusp (rect-shape-rx shape)) (float (rect-shape-rx shape) 1d0))
                    :round-y (and (plusp (rect-shape-ry shape))
                                  (float (rect-shape-ry shape) 1d0)))
                   t))))
    (ellipse-shape
     (list (cons (paths:make-circle-path (float (ellipse-shape-cx shape) 1d0)
                                         (float (ellipse-shape-cy shape) 1d0)
                                         (float (ellipse-shape-rx shape) 1d0)
                                         (float (ellipse-shape-ry shape) 1d0))
                 t)))
    (path-shape (command-paths (path-shape-commands shape)))))

(defun transform-path (path closed matrix)
  "PATH flattened and mapped through MATRIX, as a polyline path."
  (let ((points '())
        (iterator (paths:path-iterator-segmented path)))
    (loop (multiple-value-bind (interpolation knot end-p) (paths:path-iterator-next iterator)
            (declare (ignore interpolation))
            (when knot
              (let ((p (apply-matrix matrix (cons (paths:point-x knot) (paths:point-y knot)))))
                (push (paths:make-point (car p) (cdr p)) points)))
            (when (or end-p (null knot)) (return))))
    (setf points (nreverse points))
    ;; An open path's iterator does not repeat its first point; a closed
    ;; one comes back round to it.
    (paths:make-simple-path points (if closed :closed-polyline :open-polyline))))

;;; Painting

(defun nonzero-alpha (alpha) (min 256 (abs alpha)))

(defun even-odd-alpha (alpha)
  (let ((v (mod (abs alpha) 512)))
    (min 256 (if (<= v 256) v (- 512 v)))))

(defun paths-bounds (paths)
  (let ((x0 most-positive-double-float) (y0 most-positive-double-float)
        (x1 most-negative-double-float) (y1 most-negative-double-float))
    (dolist (path paths)
      (let ((iterator (paths:path-iterator-segmented path)))
        (loop (multiple-value-bind (interpolation knot end-p) (paths:path-iterator-next iterator)
                (declare (ignore interpolation))
                (when knot
                  (setf x0 (min x0 (paths:point-x knot)) x1 (max x1 (paths:point-x knot))
                        y0 (min y0 (paths:point-y knot)) y1 (max y1 (paths:point-y knot))))
                (when (or end-p (null knot)) (return))))))
    (values x0 y0 x1 y1)))

(defun texture-function (texture opacity matrix)
  "Pixel (x, y) -> r g b a for TEXTURE, tiled in the space MATRIX maps to pixels."
  (let* ((bitmap (texture-bitmap texture))
         (bw (wpg-bitmap-width bitmap)) (bh (wpg-bitmap-height bitmap))
         (rgba (bitmap-rgba-octets bitmap))
         (tw (texture-width texture)) (th (texture-height texture))
         (from-pixels (invert-matrix matrix)))
    (lambda (x y)
      (if (null from-pixels)
          (values 0 0 0 0)
          (let* ((p (apply-matrix from-pixels (cons (+ x 0.5) (+ y 0.5))))
                 ;; Tiles run right and down from the origin; y is up.
                 (col (min (1- bw) (floor (* bw (/ (mod (car p) tw) tw)))))
                 (row (min (1- bh) (floor (* bh (/ (mod (- (cdr p)) th) th)))))
                 (i (* 4 (+ col (* row bw)))))
            (values (/ (aref rgba i) 255.0) (/ (aref rgba (+ i 1)) 255.0)
                    (/ (aref rgba (+ i 2)) 255.0)
                    (* opacity (/ (aref rgba (+ i 3)) 255.0))))))))

(defun paint-function (paint opacity paths &optional matrix)
  "A function of pixel (x, y) returning r g b a (0..1) for PAINT, a COLOR,
a GRADIENT laid across the pixel bounds of PATHS, or a TEXTURE tiled in the
space MATRIX maps to pixels."
  (cond
    ((texture-p paint) (texture-function paint opacity matrix))
    ((color-p paint)
      (let ((r (/ (color-red paint) 255.0)) (g (/ (color-green paint) 255.0))
            (b (/ (color-blue paint) 255.0))
            (a (float (* opacity (/ (color-alpha paint) 255)) 1.0)))
        (lambda (x y) (declare (ignore x y)) (values r g b a))))
    (t
     (multiple-value-bind (x0 y0 x1 y1) (paths-bounds paths)
       (let ((ramp (gradient-ramp paint x0 y0 x1 y1))
             (stops (gradient-stops paint)))
         (lambda (x y)
           (let ((tt (min 1 (max 0 (funcall ramp (+ x 0.5d0) (+ y 0.5d0))))))
             (multiple-value-bind (c0 c1 f) (gradient-span stops tt)
               (flet ((mix (k) (/ (+ (* (- 1 f) (funcall k c0)) (* f (funcall k c1))) 255.0)))
                 (values (mix #'color-red) (mix #'color-green) (mix #'color-blue)
                         (float (* opacity (/ (+ (* (- 1 f) (color-alpha c0)) (* f (color-alpha c1))) 255))
                                1.0)))))))))))

(defun gradient-ramp (gradient x0 y0 x1 y1)
  "A function of a pixel position giving the GRADIENT's offset there, for
a shape whose pixel bounds are X0 Y0 X1 Y1."
  (let* ((w (max 1d-9 (- x1 x0))) (h (max 1d-9 (- y1 y0))))
    (ecase (gradient-kind gradient)
      (:linear
       ;; As in the SVG: from (0.5,0.5)-d to (0.5,0.5)+d in the bounding
       ;; box, v measured upward.
       (multiple-value-bind (ux uy) (gradient-direction gradient)
         (let* ((dx (/ ux 2)) (dy (/ uy 2))
                (len2 (max 1d-12 (* 4 (+ (* dx dx) (* dy dy))))))
           (lambda (x y)
             (let ((u (/ (- x x0) w)) (v (/ (- y1 y) h)))
               (/ (+ (* (- u (- 0.5 dx)) (* 2 dx)) (* (- v (- 0.5 dy)) (* 2 dy))) len2))))))
      ((:radial :square)
       (let* ((center (gradient-center gradient))
              (cu (car center)) (cv (cdr center))
              (cx (+ x0 (* w cu))) (cy (+ y0 (* h cv)))
              (a (* pi (/ (gradient-angle gradient) 180)))
              (cos (cos a)) (sin (sin a))
              (isotropic (gradient-isotropic gradient))
              (radial (eq (gradient-kind gradient) :radial)))
         (labels ((offset (dx dy)
                    ;; Distance from the center before scaling to the corners.
                    (multiple-value-bind (dx dy)
                        ;; Stretch to the box unless isotropic, then turn by
                        ;; the angle.
                        (let ((ex (if isotropic dx (/ dx w))) (ey (if isotropic dy (/ dy h))))
                          (values (+ (* cos ex) (* sin ey)) (- (* cos ey) (* sin ex))))
                      (if radial
                          (sqrt (+ (* dx dx) (* dy dy)))
                          (max (abs dx) (abs dy))))))
           (let ((reach (max 1d-12
                             (loop for (px . py) in (list (cons x0 y0) (cons x1 y0) (cons x0 y1) (cons x1 y1))
                                   maximize (offset (- px cx) (- py cy))))))
             (lambda (x y) (/ (offset (- x cx) (- y cy)) reach)))))))))

(defun gradient-span (stops tt)
  "The two stop colors around TT and how far TT is between them."
  (loop for (s0 s1) on stops
        while s1
        when (<= tt (car s1))
          do (return (values (cdr s0) (cdr s1)
                             (if (= (car s1) (car s0)) 0 (/ (- tt (car s0)) (- (car s1) (car s0))))))
        finally (let ((last (car (last stops))))
                  (return (values (cdr last) (cdr last) 0)))))

(defun fill-paths (canvas paths paint opacity &key (rule :nonzero) matrix)
  (when paths
    (let ((state (aa:make-state))
          (paint (paint-function paint opacity paths matrix))
          (alpha-of (if (eq rule :evenodd) #'even-odd-alpha #'nonzero-alpha)))
      (vectors:update-state state paths)
      (aa:cells-sweep state
                      (lambda (x y alpha)
                        (let ((coverage (/ (funcall alpha-of alpha) 256.0)))
                          (when (plusp coverage)
                            (multiple-value-bind (r g b a) (funcall paint x y)
                              (blend canvas x y r g b (* a coverage))))))))))

(defun draw-outline (canvas local-paths matrix style)
  "Fill and stroke LOCAL-PATHS, a list of (path . closed-p) in the shape's
coordinates, mapped to pixels by MATRIX."
  (let ((pixel-paths (loop for (path . closed) in local-paths
                           collect (transform-path path closed matrix)))
        (scale (matrix-scale matrix)))
    (when (style-fill style)
      (fill-paths canvas pixel-paths (style-fill style) (style-fill-opacity style)
                  :rule (or (style-fill-rule style) :nonzero) :matrix matrix))
    (when (style-stroke style)
      (let* ((width (if (zerop (style-stroke-width style)) 1d0
                        (max 1d0 (* scale (style-stroke-width style)))))
             (outline (if (style-dash style)
                          (paths:dash-path pixel-paths
                                           (map 'vector (lambda (d) (max 0.5d0 (* scale d)))
                                                (style-dash style)))
                          pixel-paths)))
        (fill-paths canvas (paths:stroke-path outline width :joint :round :caps :butt)
                    (style-stroke style) (style-stroke-opacity style))))))

;;; Text

(defvar *font-loaders*)

(defun font-kind (name)
  (let ((n (string-downcase (or name ""))))
    (flet ((has (&rest parts) (some (lambda (p) (search p n)) parts)))
      (cond ((has "courier" "mono" "letter gothic" "prestige") :mono)
            ((and (has "times" "roman" "dutch" "serif" "garamond" "century" "bookman" "palatino" "schoolbook")
                  (not (has "sans")))
             :serif)
            (t :sans)))))

(defun font-loader (kind fonts)
  "A zpb-ttf loader for KIND, or NIL if no font file is available."
  (multiple-value-bind (loader found) (gethash kind *font-loaders*)
    (if found
        loader
        (setf (gethash kind *font-loaders*)
              (or (loop for file in (getf fonts kind)
                        when (probe-file file)
                          do (handler-case (return (zpb-ttf:open-font-loader file))
                               (error () nil)))
                  (and (not (eq kind :sans)) (font-loader :sans fonts)))))))

(defun line-glyphs (line default-size default-font fonts)
  "The glyphs of LINE as (glyph loader x size run), x from the line start,
and the line's natural width."
  (let ((x 0) (glyphs '()))
    (dolist (run line)
      (let* ((size (or (getf run :size) default-size))
             (loader (font-loader (font-kind (or (getf run :font) default-font)) fonts)))
        (when loader
          (let ((scale (/ size (zpb-ttf:units/em loader))))
            (loop for ch across (getf run :text)
                  do (let ((glyph (zpb-ttf:find-glyph ch loader)))
                       (push (list glyph loader x size run) glyphs)
                       (incf x (* scale (zpb-ttf:advance-width glyph)))))))))
    (values (nreverse glyphs) x)))

(defun draw-text (canvas shape pixel-matrix fonts)
  (let* ((size (text-shape-size shape))
         (angle (* pi (/ (text-shape-angle shape) 180)))
         (cos (cos angle)) (sin (sin angle))
         (char-width (text-shape-char-width shape)))
    (loop for line in (text-shape-lines shape)
          for dy = 0 then (+ dy (text-line-advance line size))
          when line
            do (multiple-value-bind (glyphs natural) (line-glyphs line size (text-shape-font shape) fonts)
                 (let* ((chars (reduce #'+ line :key (lambda (r) (length (getf r :text)))))
                        (stretch (if (and char-width (plusp char-width) (plusp natural))
                                     (/ (* char-width chars) natural)
                                     1))
                        (width (* stretch natural))
                        (x0 (case (text-shape-h-align shape) (1 (- (/ width 2))) (2 (- width)) (t 0)))
                        (y0 (- (case (text-shape-v-align shape)
                                 (1 (* -0.8 size)) (2 (* -0.3 size)) (3 (* 0.2 size)) (t 0))
                               dy))
                        ;; Text space (x along the baseline, y up) to file space.
                        (to-file (list (* stretch cos) (* stretch sin) (- sin) cos
                                       (+ (text-shape-x shape) (- (* cos x0) (* sin y0)))
                                       (+ (text-shape-y shape) (* sin x0) (* cos y0))))
                        (matrix (compose-matrices pixel-matrix to-file)))
                   (loop for (glyph loader gx gsize run) in glyphs
                         do (let* ((scale (/ gsize (zpb-ttf:units/em loader)))
                                   (outline (paths-ttf:paths-from-glyph
                                             glyph :offset (paths:make-point gx 0)
                                                   :scale-x scale :scale-y scale)))
                              (when outline
                                (fill-paths canvas
                                            (mapcar (lambda (p) (transform-path p t matrix)) outline)
                                            (or (getf run :color) (text-shape-color shape))
                                            1)))))))))

;;; Bitmaps

(defun draw-image (canvas shape matrix)
  "Draw an image shape; MATRIX maps file space to pixels."
  (let* ((bitmap (image-shape-bitmap shape))
         (bw (wpg-bitmap-width bitmap)) (bh (wpg-bitmap-height bitmap))
         (rgba (bitmap-rgba-octets bitmap))
         (x1 (image-shape-x1 shape)) (y1 (image-shape-y1 shape))
         (x2 (image-shape-x2 shape)) (y2 (image-shape-y2 shape))
         (w (- x2 x1)) (h (- y2 y1))
         (to-pixels (if (identity-matrix-p (shape-transform shape))
                        matrix
                        (compose-matrices matrix (shape-transform shape))))
         (from-pixels (invert-matrix to-pixels))
         (turn (* pi (/ (wpg-bitmap-rotation bitmap) 180))))
    (when (and from-pixels (plusp w) (plusp h))
      (let ((corners (mapcar (lambda (p) (apply-matrix to-pixels p))
                             (list (cons x1 y1) (cons x2 y1) (cons x1 y2) (cons x2 y2)))))
        (loop for py from (max 0 (floor (reduce #'min corners :key #'cdr)))
                below (min (canvas-height canvas) (ceiling (reduce #'max corners :key #'cdr)))
              do (loop for px from (max 0 (floor (reduce #'min corners :key #'car)))
                         below (min (canvas-width canvas) (ceiling (reduce #'max corners :key #'car)))
                       do (let* ((f (apply-matrix from-pixels (cons (+ px 0.5) (+ py 0.5))))
                                 ;; Into the image's own space: x right, y down.
                                 (qx (- (car f) x1)) (qy (- y2 (cdr f))))
                            (when (wpg-bitmap-mirror-x bitmap) (setf qx (- w qx)))
                            (when (wpg-bitmap-mirror-y bitmap) (setf qy (- h qy)))
                            (unless (zerop turn)
                              (let ((cx (- qx (/ w 2))) (cy (- qy (/ h 2))))
                                (setf qx (+ (/ w 2) (- (* (cos turn) cx) (* (sin turn) cy)))
                                      qy (+ (/ h 2) (* (sin turn) cx) (* (cos turn) cy)))))
                            (let ((col (floor (* bw (/ qx w)))) (row (floor (* bh (/ qy h)))))
                              (when (and (< -1 col bw) (< -1 row bh))
                                (let ((i (* 4 (+ col (* row bw)))))
                                  (blend canvas px py
                                         (/ (aref rgba i) 255.0) (/ (aref rgba (+ i 1)) 255.0)
                                         (/ (aref rgba (+ i 2)) 255.0)
                                         (/ (aref rgba (+ i 3)) 255.0))))))))))))

;;; Entry points

(defvar *max-pixels* 100000000
  "The largest image RENDER-SCENE will make, in pixels.")

(defun render-scene (scene &key width height (dpi 96) (background (make-color 255 255 255))
                                (fonts *font-files*))
  "Draw SCENE into pixels. Returns (values rgba-octets width height).
The size is WIDTH and/or HEIGHT in pixels (keeping the aspect ratio when
only one is given), else the page size at DPI. BACKGROUND is a COLOR, or
NIL for transparent. FONTS is a plist like *FONT-FILES*."
  (let* ((pw (scene-width scene)) (ph (scene-height scene))
         (aspect (/ (/ ph (scene-y-units scene)) (/ pw (scene-x-units scene))))
         (w (max 1 (round (or width (and height (/ height aspect))
                              (* dpi (/ pw (scene-x-units scene)))))))
         (h (max 1 (round (or height (* w aspect)))))
         (canvas (if (> (* w h) *max-pixels*)
                     (error 'wpg-decode-error
                            :format-control "a ~Dx~D image is larger than *MAX-PIXELS* (~:D)"
                            :format-arguments (list w h *max-pixels*))
                     (make-canvas w h background)))
         (pixel-matrix (pixel-matrix scene (/ w pw) (/ h ph)))
         (*font-loaders* (make-hash-table)))
    (unwind-protect
         (dolist (shape (scene-shapes scene))
           (let ((matrix (if (identity-matrix-p (shape-transform shape))
                             pixel-matrix
                             (compose-matrices pixel-matrix (shape-transform shape)))))
             (typecase shape
               (text-shape (draw-text canvas shape matrix fonts))
               (image-shape (draw-image canvas shape pixel-matrix))
               (t (draw-outline canvas (shape-local-paths shape) matrix (shape-style shape))))))
      (loop for loader being the hash-values of *font-loaders*
            when loader do (ignore-errors (zpb-ttf:close-font-loader loader))))
    (values (canvas-octets canvas) w h)))

(defun render-png (image destination &rest keys &key width height dpi background fonts)
  "Render IMAGE (a WPG-IMAGE) and write it to DESTINATION, a pathname or a
binary output stream, as a PNG. Keywords as for RENDER-SCENE."
  (declare (ignore width height dpi background fonts))
  (multiple-value-bind (octets w h) (apply #'render-scene (build-scene image) keys)
    (let ((png (make-instance 'zpng:png :width w :height h :color-type :truecolor-alpha
                                        :image-data octets)))
      (if (streamp destination)
          (zpng:write-png-stream png destination)
          (zpng:write-png png destination :if-exists :supersede))
      destination)))
