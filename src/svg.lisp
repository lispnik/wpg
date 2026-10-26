;;;; svg.lisp -- write a SCENE as SVG.
;;;;
;;;; All geometry is written in file units inside one group whose transform
;;;; flips the file's y-up space into SVG's y-down space; text and bitmaps
;;;; are flipped back locally so they come out upright.

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

(defstruct (svg-writer (:conc-name sw-))
  stream
  bitmap-href
  (gradient-count 0))

(defun emit (sw control &rest args)
  (apply #'format (sw-stream sw) control args)
  (terpri (sw-stream sw)))

(defun gradient-paint (sw gradient)
  "Write a <linearGradient> for GRADIENT and return a paint reference to it.
Called while a shape's attributes are being built, so the definition lands
just before the shape."
  (multiple-value-bind (ux uy) (gradient-direction gradient)
  (let* ((id (format nil "g~D" (incf (sw-gradient-count sw))))
         (dx (/ ux 2)) (dy (/ uy 2)))
    ;; Object-bounding-box units, in the file's y-up space.
    (emit sw "<linearGradient id=\"~A\" x1=\"~A\" y1=\"~A\" x2=\"~A\" y2=\"~A\">~{~A~}</linearGradient>"
          id (svg-num (- 0.5 dx)) (svg-num (- 0.5 dy)) (svg-num (+ 0.5 dx)) (svg-num (+ 0.5 dy))
          (loop for (offset . c) in (gradient-stops gradient)
                collect (format nil "<stop offset=\"~A\" stop-color=\"~A\"~:[ stop-opacity=\"~A\"~;~*~]/>"
                                (svg-num offset) (color-hex c)
                                (= 255 (color-alpha c)) (svg-num (opacity-of c)))))
    (format nil "url(#~A)" id))))

(defun texture-paint (sw texture)
  "Write a <pattern> tiling TEXTURE and return a paint reference to it, or
the texture's average color when bitmaps are being left out."
  (let ((href (and (sw-bitmap-href sw) (funcall (sw-bitmap-href sw) (texture-bitmap texture)))))
    (if (null href)
        (color-hex (texture-color texture))
        (let ((id (format nil "t~D" (incf (sw-gradient-count sw))))
              (w (svg-num (texture-width texture))) (h (svg-num (texture-height texture))))
          ;; The pattern lives in the file's y-up space, so flip the tile.
          (emit sw "<pattern id=\"~A\" patternUnits=\"userSpaceOnUse\" width=\"~A\" height=\"~A\"><image width=\"~A\" height=\"~A\" preserveAspectRatio=\"none\" transform=\"translate(0 ~A) scale(1 -1)\" href=\"~A\"/></pattern>"
                id w h w h h (xml-escape href))
          (format nil "url(#~A)" id)))))

(defun style-attributes (sw style)
  (with-output-to-string (s)
    (when (style-fill-rule style)
      (format s " fill-rule=\"~(~A~)\"" (style-fill-rule style)))
    (let ((fill (style-fill style)))
      (if fill
          (progn (format s " fill=\"~A\"" (cond ((gradient-p fill) (gradient-paint sw fill))
                                                   ((texture-p fill) (texture-paint sw fill))
                                                   (t (color-hex fill))))
                 (unless (= 1 (style-fill-opacity style))
                   (format s " fill-opacity=\"~A\"" (svg-num (style-fill-opacity style)))))
          (format s " fill=\"none\"")))
    (if (style-stroke style)
        (progn (format s " stroke=\"~A\"" (color-hex (style-stroke style)))
               (unless (= 1 (style-stroke-opacity style))
                 (format s " stroke-opacity=\"~A\"" (svg-num (style-stroke-opacity style))))
               (if (zerop (style-stroke-width style))
                   (format s " stroke-width=\"1\" vector-effect=\"non-scaling-stroke\"")
                   (format s " stroke-width=\"~A\"" (svg-num (style-stroke-width style))))
               (when (style-dash style)
                 (format s " stroke-dasharray=\"~{~A~^ ~}\"" (mapcar #'svg-num (style-dash style)))))
        (format s " stroke=\"none\""))))

(defun points-attribute (points)
  (format nil "~{~A~^ ~}"
          (map 'list (lambda (p) (format nil "~A,~A" (svg-num (car p)) (svg-num (cdr p))))
               points)))

(defun path-data (commands)
  (format nil "~{~A~^ ~}"
          (mapcar (lambda (c)
                    (destructuring-bind (op &rest args) c
                      (format nil "~A~{ ~A~}"
                              (ecase op (:move "M") (:line "L") (:curve "C") (:arc "A") (:close "Z"))
                              (mapcar #'svg-num args))))
                  commands)))

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

(defun write-text-shape (sw shape)
  (let ((size (text-shape-size shape))
        (angle (text-shape-angle shape))
        (char-width (text-shape-char-width shape)))
    (loop for line in (text-shape-lines shape)
          for dy = 0 then (+ dy (text-line-advance line size))
          when line
            do (let ((chars (reduce #'+ line :key (lambda (r) (length (getf r :text))))))
                 (emit sw "<text x=\"0\" y=\"~A\" transform=\"translate(~A ~A)~:[ rotate(~A)~;~*~] scale(1 -1)\" font-family=\"~A\" font-size=\"~A\" fill=\"~A\"~A~A~A>~{~A~}</text>"
                       (svg-num dy) (svg-num (text-shape-x shape)) (svg-num (text-shape-y shape))
                       (zerop angle) (svg-num angle)
                       (font-family (text-shape-font shape)) (svg-num size)
                       (color-hex (text-shape-color shape))
                       (case (text-shape-h-align shape)
                         (1 " text-anchor=\"middle\"") (2 " text-anchor=\"end\"") (t ""))
                       (case (text-shape-v-align shape)
                         (1 " dominant-baseline=\"text-before-edge\"")
                         (2 " dominant-baseline=\"middle\"")
                         (3 " dominant-baseline=\"text-after-edge\"")
                         (t ""))
                       (if (and char-width (plusp char-width))
                           (format nil " textLength=\"~A\" lengthAdjust=\"spacingAndGlyphs\""
                                   (svg-num (* char-width chars)))
                           "")
                       (loop for run in line
                             collect (let ((run-size (getf run :size))
                                           (run-color (getf run :color))
                                           (run-font (getf run :font)))
                                       (if (or run-size run-color run-font)
                                           (format nil "<tspan~@[ font-size=\"~A\"~]~@[ fill=\"~A\"~]~@[ font-family=\"~A\"~]>~A</tspan>"
                                                   (and run-size (svg-num run-size))
                                                   (and run-color (color-hex run-color))
                                                   (and run-font (font-family run-font))
                                                   (xml-escape (getf run :text)))
                                           (xml-escape (getf run :text))))))))))

(defun write-image-shape (sw shape)
  (let* ((bitmap (image-shape-bitmap shape))
         (href (and (sw-bitmap-href sw) (funcall (sw-bitmap-href sw) bitmap))))
    (if (null href)
        (emit sw "<!-- bitmap ~Dx~D omitted -->"
              (wpg-bitmap-width bitmap) (wpg-bitmap-height bitmap))
        (let* ((x1 (image-shape-x1 shape)) (y2 (image-shape-y2 shape))
               (w (- (image-shape-x2 shape) x1)) (h (- y2 (image-shape-y1 shape)))
               (extra (with-output-to-string (s)
                        (when (wpg-bitmap-mirror-x bitmap)
                          (format s " translate(~A 0) scale(-1 1)" (svg-num w)))
                        (when (wpg-bitmap-mirror-y bitmap)
                          (format s " translate(0 ~A) scale(1 -1)" (svg-num h)))
                        (unless (zerop (wpg-bitmap-rotation bitmap))
                          (format s " rotate(~A ~A ~A)"
                                  (- (wpg-bitmap-rotation bitmap))
                                  (svg-num (/ w 2)) (svg-num (/ h 2)))))))
          (emit sw "<image x=\"0\" y=\"0\" width=\"~A\" height=\"~A\" preserveAspectRatio=\"none\" transform=\"translate(~A ~A) scale(1 -1)~A\" href=\"~A\"/>"
                (svg-num w) (svg-num h) (svg-num x1) (svg-num y2) extra
                (xml-escape href))))))

(defun write-shape (sw shape)
  (let ((transform (shape-transform shape)))
    (unless (identity-matrix-p transform)
      (emit sw "<g transform=\"matrix(~{~A~^ ~})\">" (mapcar #'svg-num transform)))
    (etypecase shape
      (line-shape
       (emit sw "<line x1=\"~A\" y1=\"~A\" x2=\"~A\" y2=\"~A\"~A/>"
             (svg-num (line-shape-x1 shape)) (svg-num (line-shape-y1 shape))
             (svg-num (line-shape-x2 shape)) (svg-num (line-shape-y2 shape))
             (style-attributes sw (shape-style shape))))
      (poly-shape
       (emit sw "<~:[polyline~;polygon~] points=\"~A\"~A/>"
             (poly-shape-closed shape) (points-attribute (poly-shape-points shape))
             (style-attributes sw (shape-style shape))))
      (rect-shape
       (emit sw "<rect x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\"~A~A~A/>"
             (svg-num (rect-shape-x shape)) (svg-num (rect-shape-y shape))
             (svg-num (rect-shape-width shape)) (svg-num (rect-shape-height shape))
             (if (plusp (rect-shape-rx shape)) (format nil " rx=\"~A\"" (svg-num (rect-shape-rx shape))) "")
             (if (plusp (rect-shape-ry shape)) (format nil " ry=\"~A\"" (svg-num (rect-shape-ry shape))) "")
             (style-attributes sw (shape-style shape))))
      (ellipse-shape
       (emit sw "<ellipse cx=\"~A\" cy=\"~A\" rx=\"~A\" ry=\"~A\"~A/>"
             (svg-num (ellipse-shape-cx shape)) (svg-num (ellipse-shape-cy shape))
             (svg-num (ellipse-shape-rx shape)) (svg-num (ellipse-shape-ry shape))
             (style-attributes sw (shape-style shape))))
      (path-shape
       (emit sw "<path d=\"~A\"~A/>" (path-data (path-shape-commands shape))
             (style-attributes sw (shape-style shape))))
      (text-shape (write-text-shape sw shape))
      (image-shape (write-image-shape sw shape)))
    (unless (identity-matrix-p transform)
      (emit sw "</g>"))))

(defun write-scene-svg (scene stream &key bitmap-href)
  "Write SCENE to STREAM as an SVG document. See WRITE-SVG."
  (let ((w (scene-width scene)) (h (scene-height scene))
        (sw (make-svg-writer :stream stream :bitmap-href bitmap-href)))
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format stream "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"~Ain\" height=\"~Ain\" viewBox=\"0 0 ~A ~A\">~%"
            (svg-num (/ w (scene-x-units scene))) (svg-num (/ h (scene-y-units scene)))
            (svg-num w) (svg-num h))
    ;; File space (y up, origin at the offset) to SVG space (y down).
    (format stream "<g transform=\"matrix(1 0 0 -1 ~A ~A)\">~%"
            (svg-num (- (scene-x-offset scene)))
            (svg-num (+ h (scene-y-offset scene))))
    (dolist (shape (scene-shapes scene))
      (write-shape sw shape))
    (format stream "</g>~%</svg>~%")
    scene))

(defun write-svg (image stream &key bitmap-href)
  "Write IMAGE (a WPG-IMAGE) to STREAM as an SVG document.
BITMAP-HREF, if given, is called with each WPG-BITMAP and should return a
URI for it (a file name, or a data: URI) or NIL to leave it out; without
it, bitmaps are left out."
  (write-scene-svg (build-scene image) stream :bitmap-href bitmap-href)
  image)

(defun svg-string (image &key bitmap-href)
  "IMAGE rendered as an SVG document string. See WRITE-SVG."
  (with-output-to-string (s)
    (write-svg image s :bitmap-href bitmap-href)))
