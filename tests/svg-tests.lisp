;;;; svg-tests.lisp -- SVG and image output.

(in-package #:wordperfect-graphics/tests)

(def-suite output :in wordperfect-graphics)
(in-suite output)

(defun has (needle haystack)
  (search needle haystack))

(test svg-wpg1-shapes
  (let ((svg (wpg:svg-string
              (wpg:read-wpg
               (wpg1 (start1 2400 1200)
                     (rec1 #x02 1 4 (u16 12))
                     (rec1 #x01 1 2)
                     (rec1 #x05 (s16 0) (s16 0) (s16 100) (s16 100))
                     (rec1 #x07 (s16 10) (s16 20) (s16 30) (s16 40))
                     (rec1 #x09 (s16 500) (s16 500) (s16 100) (s16 50)
                           (s16 0) (s16 0) (s16 360) (u16 0))
                     (rec1 #x09 (s16 500) (s16 500) (s16 100) (s16 100)
                           (s16 0) (s16 0) (s16 90) (u16 0))
                     (rec1 #x0c (u16 7) (s16 1) (s16 2) "a<b & c"))))))
    (is (has "<svg " svg))
    (is (has "width=\"2in\" height=\"1in\" viewBox=\"0 0 2400 1200\"" svg))
    (is (has "matrix(1 0 0 -1 0 1200)" svg))
    (is (has "<line x1=\"0\" y1=\"0\" x2=\"100\" y2=\"100\" fill=\"none\" stroke=\"#aa0000\" stroke-width=\"12\"/>" svg))
    (is (has "<rect x=\"10\" y=\"20\" width=\"30\" height=\"40\" fill=\"#00aa00\"" svg))
    (is (has "<ellipse cx=\"500\" cy=\"500\" rx=\"100\" ry=\"50\"" svg))
    (is (has "<path d=\"M 600 500 A 100 100 0 0 1 500 600\"" svg))
    (is (has "a&lt;b &amp; c" svg))
    (is (has "</svg>" svg))))

(test svg-bitmaps
  (let* ((image (wpg:read-wpg (wpg1 (start1 1200 1200) (bitmap1 8 1 1 '(#xff)))))
         (seen '())
         (svg (wpg:svg-string image :bitmap-href (lambda (b) (push b seen) "x.png"))))
    (is (= 1 (length seen)))
    (is (has "href=\"x.png\"" svg))
    (is (has "bitmap 8x1 omitted" (wpg:svg-string image)))))

(test svg-wpg2-shapes
  (let ((svg (wpg:svg-string
              (wpg:read-wpg
               (wpg2 (start2 :bounds '(0 0 1000 500))
                     (rec2 #x25 255 0 0 0)
                     (rec2 #x31 0 0 0 255 0)
                     ;; framed, filled, closed polyline -> polygon
                     (rec2 #x15 (u16 #xe000) (u16 3) (mapcar #'s16 '(0 0 10 0 10 10)))
                     ;; framed only rectangle, scaled 2x
                     (rec2 #x18 (u16 (logior #x8000 8)) (s32 (* 2 65536)) (s32 (* 2 65536))
                           (mapcar #'s16 '(1 2 3 4 0 0)))
                     (rec2 #x19 (u16 #x8000) (mapcar #'s16 '(50 50 10 10 60 50 60 50))))))))
    (is (has "<polygon points=\"0,0 10,0 10,10\" fill-rule=\"evenodd\" fill=\"#0000ff\" stroke=\"#ff0000\"" svg))
    (is (has "<g transform=\"matrix(2 0 0 2 0 0)\">" svg))
    (is (has "<rect x=\"1\" y=\"2\" width=\"2\" height=\"2\" fill=\"none\" stroke=\"#ff0000\"" svg))
    (is (has "<ellipse cx=\"50\" cy=\"50\" rx=\"10\" ry=\"10\" fill=\"none\"" svg))))

(test png-output
  (let* ((image (wpg:read-wpg (wpg1 (bitmap1 3 2 8 '(1 2 3 4 5 6)))))
         (png (wpg:bitmap-png-octets (first (wpg:wpg-image-bitmaps image)))))
    (is (equalp #(137 80 78 71 13 10 26 10) (subseq png 0 8)))))

(test ppm-output
  (let ((image (wpg:read-wpg (wpg1 (bitmap1 2 1 8 '(15 0))))))
    (let ((path (uiop:with-temporary-file (:pathname p :keep t :type "ppm") p)))
      (unwind-protect
           (progn
             (wpg:write-bitmap-ppm (first (wpg:wpg-image-bitmaps image)) path)
             (let ((data (wpg::source-octets path)))
               (is (equal (format nil "P6~%2 1~%255~%") (map 'string #'code-char (subseq data 0 11))))
               (is (equalp #(255 255 255 0 0 0) (subseq data 11)))))
        (uiop:delete-file-if-exists path)))))

(defun rec2-ext (type extension &rest body)
  (let ((b (bytes body)))
    (list #x0f type (varlen extension) (varlen (length b)) b)))

(test svg-compound-polygon
  ;; Two child polylines become one even-odd path, filled with the brush
  ;; set before the compound and placed by the compound's translation.
  ;; The brush record inside the compound is ignored, as in libwpg.
  (let ((svg (wpg:svg-string
              (wpg:read-wpg
               (wpg2 (start2 :bounds '(0 0 1000 1000))
                     (rec2 #x31 0 255 0 0 0)
                     (rec2-ext #x1a 3 (u16 (logior 2 (ash 1 13) (ash 1 14)))
                               (u16 0) (s32 100) (u16 0) (s32 0))
                     (rec2 #x31 0 0 255 0 0)
                     (rec2 #x15 (u16 0) (u16 3) (mapcar #'s16 '(0 0 10 0 10 10)))
                     (rec2 #x15 (u16 0) (u16 2) (mapcar #'s16 '(20 20 30 30))))))))
    (is (has "<path d=\"M 100 0 L 110 0 L 110 10 M 120 20 L 130 30 Z\" fill-rule=\"evenodd\" fill=\"#ff0000\" stroke=\"none\"/>" svg))
    (is (not (has "#00ff00" svg)))))

(test svg-wpg2-default-brush-is-black
  (let ((svg (wpg:svg-string
              (wpg:read-wpg (wpg2 (rec2 #x15 (u16 (ash 1 13)) (u16 3) (mapcar #'s16 '(0 0 1 0 1 1))))))))
    (is (has "fill=\"#000000\" stroke=\"none\"" svg))))

(test svg-two-color-gradient
  ;; libwpg's angles: 0 runs top to bottom, 90 right to left.
  (let ((svg (wpg:svg-string
              (wpg:read-wpg
               (wpg2 (rec2 #x2f (u16 0) (u16 90) (u16 #xffff) (u16 #xffff) 0)
                     (rec2 #x31 1 (u16 2) 255 0 0 0 0 0 255 0 (u16 0))
                     (rec2 #x15 (u16 (ash 1 13)) (u16 3) (mapcar #'s16 '(0 0 1 0 1 1))))))))
    (is (has "<linearGradient id=\"g1\" x1=\"1\" y1=\"0.5\" x2=\"0\" y2=\"0.5\"><stop offset=\"0\" stop-color=\"#0000ff\"/><stop offset=\"1\" stop-color=\"#ff0000\"/></linearGradient>" svg))
    (is (has "fill=\"url(#g1)\"" svg))))

(test svg-dashes
  ;; WPG2: built-in pen style 8, and a style the file defines.
  (let ((svg (wpg:svg-string
              (wpg:read-wpg
               (wpg2 (rec2 #x08 (u16 20) (u16 1) (u16 109) (u16 218))
                     (rec2 #x2b (u16 100) (u16 100))
                     (rec2 #x29 (u16 8))
                     (rec2 #x15 (u16 #x8000) (u16 2) (mapcar #'s16 '(0 0 10 0)))
                     (rec2 #x29 (u16 20))
                     (rec2 #x15 (u16 #x8000) (u16 2) (mapcar #'s16 '(0 0 10 0)))
                     (rec2 #x29 (u16 0))
                     (rec2 #x15 (u16 #x8000) (u16 2) (mapcar #'s16 '(0 0 10 0))))))))
    ;; Lengths are in file units, whatever the pen width.
    (is (has "stroke-dasharray=\"164 55 18 55\"" svg))
    (is (has "stroke-dasharray=\"109 218\"" svg))
    (is (= 2 (count-matches "dasharray" svg))))
  ;; WPG1 line style 2 is taken as pen style 1.
  (is (has "stroke-dasharray=\"218 73\""
           (wpg:svg-string (wpg:read-wpg (wpg1 (rec1 #x02 2 0 (u16 100))
                                               (rec1 #x05 (s16 0) (s16 0) (s16 9) (s16 9))))))))

(test oriented-bitmaps
  ;; A 2x1 bitmap, pixels 1 and 2, mirrored left-right then turned 90.
  (let ((bm (first (wpg:wpg-image-bitmaps
                    (wpg:read-wpg (wpg1 (bitmap1 2 1 8 '(1 2))))))))
    (flet ((pixels (&rest keys)
             (multiple-value-bind (o w h) (apply #'wpg:bitmap-rgb-octets bm keys)
               (list w h (loop for i below (length o) by 3 collect (aref o (+ i 2)))))))
      (is (equal '(2 1 (170 0)) (pixels)))
      (setf (wpg:wpg-bitmap-mirror-x bm) t)
      (is (equal '(2 1 (0 170)) (pixels :oriented t)))
      (setf (wpg:wpg-bitmap-mirror-x bm) nil
            (wpg:wpg-bitmap-rotation bm) 90)
      ;; Counterclockwise: the right pixel ends up on top.
      (is (equal '(1 2 (0 170)) (pixels :oriented t)))
      (is (equal '(2 1 (170 0)) (pixels))))))
