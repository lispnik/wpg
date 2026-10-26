;;;; scene-tests.lisp -- OLE containers, the drawing model, and rasterizing.

(in-package #:wordperfect-graphics/tests)

(def-suite scene :in wordperfect-graphics)
(in-suite scene)

(test ole-container
  (dolist (filler '(0 5000))           ; a mini-stream stream, then a regular one
    (let* ((wpg (bytes (header :version 2) (start2) (rec2 #x0a (make-list filler :initial-element 7))
                       (rec2 #x02)))
           (image (wpg:read-wpg (ole-file "PerfectOffice_MAIN" wpg))))
      (is (eq :ole (wpg:wpg-image-container image)))
      (is (= 2400 (wpg:wpg-image-width image)))
      (is (= 3 (length (wpg:wpg-image-records image))))))
  (signals wpg:wpg-format-error (wpg:read-wpg (ole-file "Other" (wpg2)))))

(test start-wpg-bad-bounds
  ;; Unordered image bounds: the viewport is the page.
  (let ((image (wpg:read-wpg (wpg2 (rec2 #x01 (u16 1200) (u16 1200) 0
                                         (mapcar #'s16 '(600 600 12600 9600))
                                         (mapcar #'s16 '(-2433 21344 30828 -2156)))))))
    (is (= 12000 (wpg:wpg-image-width image)))
    (is (= 600 (wpg:wpg-image-y-offset image)))
    (is (= 1 (length (wpg:wpg-image-warnings image))))))

(test scene-shapes
  (let ((scene (wpg:build-scene
                (wpg:read-wpg (wpg1 (start1 1200 1200)
                                    (rec1 #x01 1 4)
                                    (rec1 #x07 (s16 10) (s16 20) (s16 30) (s16 40))
                                    (rec1 #x09 (s16 500) (s16 500) (s16 100) (s16 50)
                                          (s16 30) (s16 0) (s16 360) (u16 0)))))))
    (is (= 1200 (wpg:scene-width scene)))
    (destructuring-bind (rect ellipse) (wpg:scene-shapes scene)
      (is (wpg:rect-shape-p rect))
      (is (equal '(170 0 0) (rgb (wpg:style-fill (wpg:shape-style rect)))))
      (is (wpg:ellipse-shape-p ellipse))
      ;; Rotated 30 degrees about its center.
      (is (< (abs (- (first (wpg:shape-transform ellipse)) (cos (/ pi 6)))) 1d-9)))))

(test groups-compose
  ;; A group scaled by 2 holding a group translated by (10, 0) holding a
  ;; polyline: the polyline is drawn through both.
  (let* ((scale (list (u16 8) (s32 (* 2 65536)) (s32 (* 2 65536))))
         (translate (list (u16 2) (u16 0) (s32 10) (u16 0) (s32 0)))
         (scene (wpg:build-scene
                 (wpg:read-wpg
                  (wpg2 (start2)
                        (rec2-ext #x20 1 scale)
                        (rec2-ext #x20 1 translate)
                        (rec2 #x15 (u16 #x8000) (u16 2) (mapcar #'s16 '(0 0 5 5)))))))
         (line (first (wpg:scene-shapes scene))))
    (is (equal '(2 0 0 2 20 0) (wpg:shape-transform line)))))

(test hairlines
  ;; One unit at 1200 per inch is thinner than 1/300 inch: a hairline.
  (flet ((width-of (pen)
           (wpg:style-stroke-width
            (wpg:shape-style (first (wpg:scene-shapes
                                     (wpg:build-scene
                                      (wpg:read-wpg (wpg2 (start2) (rec2 #x2b (u16 pen) (u16 pen))
                                                          (rec2 #x15 (u16 #x8000) (u16 2)
                                                                (mapcar #'s16 '(0 0 9 9))))))))))))
    (is (= 0 (width-of 1)))
    (is (= 24 (width-of 24)))))

(defun texture-file ()
  ;; Page attributes: a 2x1 texture (red, blue) and a brush using it.
  (wpg2 (rec2-ext #x01 4 (u16 1200) (u16 1200) 0 (mapcar #'s16 '(0 0 1200 1200))
                  (mapcar #'s16 '(0 0 1200 1200)))
        (rec2-ext #x3c 2 (u16 #x8001) 128 0 128 1 "TILE.BMP" 0)
        (rec2 #x0c (u16 0) (u16 2) 255 0 0 0 0 0 255 0)
        (rec2 #x0e (u16 2) (u16 1) 4 0 0 1)
        (rec2 #x3d (u16 #x8001) 0 0 0 0 0)))

(test page-texture
  (let* ((image (wpg:read-wpg (texture-file)))
         (shapes (wpg:scene-shapes (wpg:build-scene image)))
         (fill (wpg:style-fill (wpg:shape-style (first shapes)))))
    (is (= 1 (length shapes)))
    (is (wpg:texture-p fill))
    ;; 2 pixels at 96 per inch, in 1200ths.
    (is (= 25 (wpg:texture-width fill)))
    (is (has "<pattern id=\"t1\"" (wpg:svg-string image :bitmap-href (constantly "x.png"))))
    (is (has "fill=\"#800080\"" (wpg:svg-string image)))))

(test gradient-direction
  (flet ((dir (angle)
           (multiple-value-list (wpg:gradient-direction (wpg::make-gradient :angle angle)))))
    (is (every (lambda (a b) (< (abs (- a b)) 1d-9)) '(0 -1) (dir 0)))
    (is (every (lambda (a b) (< (abs (- a b)) 1d-9)) '(-1 0) (dir 90)))))

;;; Rasterizing

(defun pixel (octets width x y)
  (let ((i (* 4 (+ x (* y width)))))
    (coerce (subseq octets i (+ i 4)) 'list)))

(test raster-fill-and-holes
  ;; A 1-inch page, a red square with a square hole (even-odd), at 20 dpi.
  (multiple-value-bind (octets w h)
      (wpg:render-scene
       (wpg:build-scene
        (wpg:read-wpg
         (wpg2 (start2 :bounds '(0 0 1200 1200))
               (rec2 #x31 0 255 0 0 0)
               (rec2-ext #x1a 2 (u16 (logior (ash 1 13) (ash 1 14))))
               (rec2 #x15 (u16 0) (u16 4) (mapcar #'s16 '(0 0 1200 0 1200 1200 0 1200)))
               (rec2 #x15 (u16 0) (u16 4) (mapcar #'s16 '(300 300 900 300 900 900 300 900))))))
       :dpi 20)
    (is (= 20 w))
    (is (= 20 h))
    (is (equal '(255 0 0 255) (pixel octets w 2 2)))
    (is (equal '(255 255 255 255) (pixel octets w 10 10)))))

(test raster-transparent-and-texture
  (multiple-value-bind (octets w) (wpg:render-scene (wpg:build-scene (wpg:read-wpg (texture-file)))
                                                     :dpi 96 :background nil)
    ;; Tiles of red then blue, one pixel each at 96 dpi.
    (is (equal '(255 0 0 255) (pixel octets w 0 0)))
    (is (equal '(0 0 255 255) (pixel octets w 1 0)))
    (is (equal '(255 0 0 255) (pixel octets w 2 5))))
  (multiple-value-bind (octets w) (wpg:render-scene (wpg:build-scene (wpg:read-wpg (wpg2 (start2))))
                                                     :width 10 :background nil)
    (is (equal '(0 0 0 0) (pixel octets w 5 2)))))

(test raster-text
  ;; Only meaningful with a system font; otherwise text is skipped.
  (when (some #'probe-file (getf wpg:*font-files* :sans))
    (multiple-value-bind (octets w h)
        (wpg:render-scene (wpg:build-scene
                           (wpg:read-wpg (wpg1 (start1 1200 600)
                                               (text-attributes :height 400)
                                               (rec1 #x0c (u16 4) (s16 50) (s16 200) "HHHH"))))
                          :dpi 100)
      (is (find-if (lambda (i) (< (aref octets i) 100))
                   (loop for i from 0 below (* w h 4) by 4 collect i))))))

(test render-png-file
  (let ((path (uiop:with-temporary-file (:pathname p :keep t :type "png") p)))
    (unwind-protect
         (progn (wpg:render-png (wpg:read-wpg (fixture "libreoffice-wpg1.wpg")) path :width 64)
                (is (equalp #(137 80 78 71) (subseq (wpg::source-octets path) 0 4))))
      (uiop:delete-file-if-exists path))))

(test wpg1-curved-polygon
  ;; Record #x1F (WordPerfect 8): a closed Bezier path, filled.
  (let* ((image (wpg:read-wpg (wpg1 (start1 1200 1200) (rec1 #x01 1 4)
                                    (rec1 #x1f (u16 4) (mapcar #'s16 '(0 0 100 0 100 100 0 0))))))
         (shape (first (wpg:scene-shapes (wpg:build-scene image)))))
    (is (eq :curved-polygon (wpg:wpg-record-name (aref (wpg:wpg-image-records image) 2))))
    (is (equal '((:move 0 0) (:curve 100 0 100 100 0 0) (:close)) (wpg:path-shape-commands shape)))
    (is (wpg:style-fill (wpg:shape-style shape)))))

(test page-brush
  (flet ((first-shape (&rest page-attributes)
           (first (wpg:scene-shapes
                   (wpg:build-scene
                    (wpg:read-wpg
                     (wpg2 (rec2-ext #x01 (length page-attributes) (u16 1200) (u16 1200) 0
                                     (mapcar #'s16 '(0 0 1200 1200)) (mapcar #'s16 '(0 0 1200 1200)))
                           page-attributes
                           (rec2 #x15 (u16 (ash 1 13)) (u16 3) (mapcar #'s16 '(0 0 9 0 9 9))))))))))
    ;; A gradient page brush fills the page...
    (let ((page (first-shape (rec2 #x2f (u16 0) (u16 0) (u16 #xffff) (u16 #xffff) 0)
                             (rec2 #x31 1 (u16 2) 0 0 0 0 255 255 255 0 (u16 0)))))
      (is (wpg:rect-shape-p page))
      (is (wpg:gradient-p (wpg:style-fill (wpg:shape-style page)))))
    ;; ...a plain one does not, and drawing starts with the black brush.
    (let ((shape (first-shape (rec2 #x31 0 255 255 255 0))))
      (is (wpg:poly-shape-p shape))
      (is (equal '(0 0 0) (rgb (wpg:style-fill (wpg:shape-style shape))))))))

(defun gradient-square-file (type &key (x-ref 32767) (y-ref 32767) (colors '((255 0 0) (0 0 255))))
  "A 1-inch page filled by a square polygon with a gradient brush of TYPE."
  (wpg2 (start2 :bounds '(0 0 1200 1200))
        (rec2 #x2f (u16 0) (u16 0) (u16 x-ref) (u16 y-ref) 0)
        (rec2 #x31 type (u16 (length colors))
              (loop for c in colors collect (list c 0))
              (loop repeat (1- (length colors)) collect (u16 0)))
        (rec2 #x15 (u16 (logior (ash 1 13) (ash 1 14))) (u16 4)
              (mapcar #'s16 '(0 0 1200 0 1200 1200 0 1200)))))

(test gradient-kinds
  (flet ((fill-of (type)
           (wpg:style-fill (wpg:shape-style (first (wpg:scene-shapes
                                                    (wpg:build-scene
                                                     (wpg:read-wpg (gradient-square-file type)))))))))
    (is (eq :linear (wpg:gradient-kind (fill-of 1))))
    (is (eq :radial (wpg:gradient-kind (fill-of 4))))
    (is (wpg:gradient-isotropic (fill-of 4)))
    (is (not (wpg:gradient-isotropic (fill-of 6))))
    (is (eq :square (wpg:gradient-kind (fill-of 10))))
    ;; Radial: the first color at the center.
    (is (equal '(255 0 0) (rgb (cdr (first (wpg:gradient-stops (fill-of 4)))))))))

(test radial-gradient-pixels
  ;; Red at the center, blue at the corners.
  (multiple-value-bind (octets w)
      (wpg:render-scene (wpg:build-scene (wpg:read-wpg (gradient-square-file 4))) :dpi 21)
    (destructuring-bind (r g b a) (pixel octets w 10 10)
      (declare (ignore g a))
      (is (> r 240)) (is (< b 15)))
    (destructuring-bind (r g b a) (pixel octets w 0 0)
      (declare (ignore g a))
      (is (< r 25)) (is (> b 230))))
  ;; Off-center, toward the top-left: that corner is reddest.
  (multiple-value-bind (octets w)
      (wpg:render-scene (wpg:build-scene (wpg:read-wpg (gradient-square-file 10 :x-ref 16383 :y-ref 16383)))
                        :dpi 21)
    (is (> (first (pixel octets w 5 5)) 200))
    (is (> (first (pixel octets w 1 1)) (first (pixel octets w 19 19))))))

(test radial-gradient-svg
  (let ((svg (wpg:svg-string (wpg:read-wpg (gradient-square-file 4)))))
    (is (has "<radialGradient id=\"g1\" gradientUnits=\"userSpaceOnUse\" cx=\"599.991\" cy=\"600.009\"" svg))
    (is (has "<stop offset=\"0\" stop-color=\"#ff0000\"/>" svg))))

(test many-color-gradient
  ;; Three colors, evenly spaced, last first as two-color ramps run.
  (let ((stops (wpg:gradient-stops
                (wpg:style-fill (wpg:shape-style
                                 (first (wpg:scene-shapes
                                         (wpg:build-scene
                                          (wpg:read-wpg
                                           (gradient-square-file
                                            1 :colors '((255 0 0) (0 255 0) (0 0 255))))))))))))
    (is (equal '(0 1/2 1) (mapcar #'car stops)))
    (is (equal '((0 0 255) (0 255 0) (255 0 0)) (mapcar (lambda (s) (rgb (cdr s))) stops)))))
