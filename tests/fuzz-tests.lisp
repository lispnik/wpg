;;;; fuzz-tests.lisp -- inputs the fuzzer found, and a short fuzz run.
;;;;
;;;; tests/data/fuzz/ holds damaged files that once made the library fail
;;;; with something other than a WPG-ERROR (or hang); each must now pass
;;;; CHECK-OCTETS. `make fuzz' runs the fuzzer at length.

(in-package #:wordperfect-graphics/tests)

(def-suite fuzz-tests :in wordperfect-graphics)
(in-suite fuzz-tests)

(defun fuzz-cases ()
  (directory (merge-pathnames (make-pathname :name :wild :type "wpg")
                              (asdf:system-relative-pathname :wordperfect-graphics/tests
                                                             "tests/data/fuzz/"))))

(test fuzz-regressions
  (let ((files (fuzz-cases)))
    (is (plusp (length files)))
    (dolist (file files)
      (let ((failure (wordperfect-graphics/fuzz:check-octets
                      (wpg::source-octets file) :timeout 20)))
        (is (null failure) "~A: ~A" (file-namestring file)
            (and failure (wordperfect-graphics/fuzz:failure-signature failure)))))))

(test short-fuzz-run
  (let ((failures (wordperfect-graphics/fuzz:fuzz
                   :seeds (list (asdf:system-relative-pathname :wordperfect-graphics/tests
                                                               "tests/data/"))
                   :extra-seeds (list (ole-file "PerfectOffice_MAIN"
                                                (wpg2 (start2) (rec2 #x0e (u16 0)))))
                   :iterations 1000 :seed 20261027
                   :output nil :minimize nil :verbose nil :timeout 20)))
    (is (null failures) "~{~A~^; ~}" (mapcar #'first failures))))

;;; The same bugs, rebuilt from scratch. Each must finish well inside the
;;; deadline; before the fixes they ran for minutes or exhausted the heap.

(defmacro within-seconds ((seconds) &body body)
  `(handler-case (sb-ext:with-timeout ,seconds ,@body)
     (sb-ext:timeout () (fail "took more than ~D seconds" ,seconds) nil)))

(defun render-one (shape &key (size 100))
  "Render SHAPE alone on a SIZE-unit square page, 64 pixels wide; returns
the RGBA octets."
  (wpg:render-scene (wpg::make-scene :width size :height size :shapes (list shape))
                    :width 64 :background nil))

(defun alpha-at (octets x y &optional (width 64))
  (aref octets (+ 3 (* 4 (+ x (* y width))))))

(defun solid (&rest keys)
  (apply #'wpg::make-style :fill (wpg:make-color 0 0 0) keys))

(test raster-shape-far-bigger-than-canvas
  ;; A polygon millions of pixels across: only the canvas is swept.
  (within-seconds (10)
    (let ((px (render-one (wpg::make-poly-shape
                           :style (solid) :closed t
                           :points #((-9000000 . -9000000) (9000000 . -9000000)
                                     (9000000 . 9000000) (-9000000 . 9000000))))))
      (is (= 255 (alpha-at px 32 32))))))

(test raster-huge-ellipse
  ;; Arcs are flattened at pixel scale, capped by the canvas size.
  (within-seconds (10)
    (let ((px (render-one (wpg::make-ellipse-shape :style (solid) :cx 50 :cy 50
                                                   :rx 30000000 :ry 30000000))))
      (is (= 255 (alpha-at px 32 32))))))

(test raster-arc-with-scaled-up-radius
  ;; Radii too small to span the endpoints are scaled up by SVG rules; here
  ;; to hundreds of millions of units.
  (within-seconds (10)
    (render-one (wpg::make-path-shape
                 :style (solid :stroke (wpg:make-color 0 0 0) :stroke-width 10)
                 :commands '((:move 0 12547) (:arc 32768 2 0 0 1 -256 -32768) (:close))))
    (pass)))

(test raster-huge-stroke
  ;; A stroke millions of pixels wide, with round joins at the corners, is
  ;; made at a smaller scale.
  (within-seconds (10)
    (let ((px (render-one (wpg::make-rect-shape
                           :style (wpg::make-style :stroke (wpg:make-color 0 0 0)
                                                   :stroke-width 5000000)
                           :x 10 :y 10 :width 80 :height 80 :rx 0 :ry 0))))
      (is (= 255 (alpha-at px 32 32))))))

(test raster-dashes-on-a-huge-line
  ;; Too many dashes to draw: drawn solid.
  (within-seconds (10)
    (let ((px (render-one (wpg::make-line-shape
                           :style (wpg::make-style :stroke (wpg:make-color 0 0 0)
                                                   :stroke-width 10 :dash '(1 1))
                           :x1 -90000000 :y1 50 :x2 90000000 :y2 50))))
      (is (= 255 (alpha-at px 10 32))))))

(test ole-stream-longer-than-its-sectors
  ;; The directory claims a stream far longer than the data in the file.
  (let ((file (ole-file "PerfectOffice_MAIN" (wpg2 (start2)))))
    ;; Sector 1 is the directory; its second entry is the stream.
    (replace file (u32 100000) :start1 (+ 512 512 128 #x78))
    (is (null (wordperfect-graphics/fuzz:check-octets file)))))

(test wp6-text-ending-inside-a-group
  ;; Variable-length groups cut off after one, two and five bytes.
  (dolist (octets '(#(#x41 #xd4) #(#x41 #xd4 #x1a) #(#x41 #xd4 #x1a #x40 #x00 #x80)))
    (let ((runs (wpg:decode-wp6-text (coerce octets '(vector (unsigned-byte 8))))))
      (is (eql 0 (search "A" (getf (first runs) :text))) "~S" octets))))

(test bitmap-mostly-missing
  ;; A 2053x45569 16-bit bitmap with four bytes of data.
  (is (null (wordperfect-graphics/fuzz:check-octets
             (wpg2 (start2) (rec2 #x0e (u16 2053) (u16 45569) 5 0 1 2 3 4))))))

(test raster-huge-bitmap-small-render
  ;; 100 megapixels drawn 64 pixels wide: only the pixels sampled are read.
  (let ((bitmap (wpg::make-wpg-bitmap
                 :width 10000 :height 10000 :depth 1 :format :indexed :row-bytes 1250
                 :palette (vector (wpg:make-color 0 0 0) (wpg:make-color 255 255 255))
                 :data (wpg::make-octets (* 1250 10000)))))
    ;; Converting the whole bitmap first took about ten seconds.
    (within-seconds (2)
      (let ((px (render-one (wpg::make-image-shape :bitmap bitmap :x1 0 :y1 0 :x2 100 :y2 100))))
        (is (= 255 (alpha-at px 32 32)))))))
