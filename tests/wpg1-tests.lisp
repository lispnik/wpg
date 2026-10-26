;;;; wpg1-tests.lisp -- WPG1 records, palettes and bitmaps.

(in-package #:wordperfect-graphics/tests)

(def-suite wpg1 :in wordperfect-graphics)
(in-suite wpg1)

(defun rec (image index)
  (aref (wpg:wpg-image-records image) index))

(defun rgb (color)
  (list (wpg:color-red color) (wpg:color-green color) (wpg:color-blue color)))

(test default-palette
  (let ((p (wpg:default-wpg1-palette)))
    (is (= 256 (length p)))
    (is (equal '(0 0 0) (rgb (aref p 0))))
    (is (equal '(170 85 0) (rgb (aref p 6))))
    (is (equal '(255 255 255) (rgb (aref p 15))))
    ;; From here on, these are libwpg's values.
    (is (equal '(0 0 0) (rgb (aref p 16))))
    (is (equal '(60 60 65) (rgb (aref p 224))))
    (is (equal '(69 69 69) (rgb (aref p 21))))
    (is (equal '(255 255 255) (rgb (aref p 31))))
    (is (equal '(0 0 255) (rgb (aref p 32))))
    (is (equal '(65 0 255) (rgb (aref p 33))))
    (is (equal '(125 125 255) (rgb (aref p 56))))
    (is (equal '(0 0 0) (rgb (aref p 255))))))

(test start-and-end
  (let ((image (wpg:read-wpg (wpg1 (start1 2400 1800)))))
    (is (= 1 (wpg:wpg-image-version image)))
    (is (= 2400 (wpg:wpg-image-width image)))
    (is (= 1800 (wpg:wpg-image-height image)))
    (is (= 2 (wpg:wpg-image-width-inches image)))
    (is (eq :start-wpg (wpg:wpg-record-name (rec image 0))))
    (is (eq :end-wpg (wpg:wpg-record-name (rec image 1))))
    (is (null (wpg:wpg-image-warnings image)))))

(test drawing-primitives
  (let ((image (wpg:read-wpg
                (wpg1 (start1 1000 1000)
                      (rec1 #x02 1 4 (u16 12))
                      (rec1 #x05 (s16 10) (s16 -20) (s16 300) (s16 400))
                      (rec1 #x06 (u16 2) (s16 1) (s16 2) (s16 3) (s16 4))
                      (rec1 #x07 (s16 5) (s16 6) (s16 70) (s16 80))
                      (rec1 #x09 (s16 500) (s16 500) (s16 100) (s16 50)
                            (s16 0) (s16 0) (s16 360) (u16 0))))))
    (is (equal '(:style 1 :color 4 :width 12) (wpg:wpg-record-fields (rec image 1))))
    (is (equal '(:x1 10 :y1 -20 :x2 300 :y2 400) (wpg:wpg-record-fields (rec image 2))))
    (is (equalp #((1 . 2) (3 . 4)) (wpg:record-field (rec image 3) :points)))
    (is (= 80 (wpg:record-field (rec image 4) :height)))
    (is (= 360 (wpg:record-field (rec image 5) :end-angle)))
    (is (= 1 (length (wpg:find-records image :polyline))))
    (is (= 1 (length (wpg:find-records image #x05))))))

(test graphics-text
  (let ((image (wpg:read-wpg (wpg1 (rec1 #x0c (u16 5) (s16 100) (s16 200) "Hello")))))
    (is (equal "Hello" (wpg:record-field (rec image 0) :text)))
    (is (= 200 (wpg:record-field (rec image 0) :y)))))

(test color-map
  (let* ((image (wpg:read-wpg (wpg1 (rec1 #x0e (u16 2) (u16 2) 10 20 30 40 50 60))))
         (p (wpg:wpg-image-palette image)))
    (is (equal '(10 20 30) (rgb (aref p 2))))
    (is (equal '(40 50 60) (rgb (aref p 3))))
    (is (equal '(0 0 0) (rgb (aref p 0))))
    (is (= 2 (length (wpg:record-field (rec image 0) :entries))))))

(defun bitmap1 (width height depth data &key (hres 75) (vres 75))
  (rec1 #x0b (u16 width) (u16 height) (u16 depth) (u16 hres) (u16 vres)
        (literal-rle1 data)))

(test monochrome-bitmap
  ;; 10 pixels wide = 2 bytes per row; 1 is white by default.
  (let* ((image (wpg:read-wpg (wpg1 (bitmap1 10 2 1 '(#b10100000 #b01000000
                                                     #b00000000 #b11000000)))))
         (bm (first (wpg:wpg-image-bitmaps image))))
    (is (= 1 (length (wpg:wpg-image-bitmaps image))))
    (is (= 10 (wpg:wpg-bitmap-width bm)))
    (is (= 2 (wpg:wpg-bitmap-row-bytes bm)))
    (is (wpg:wpg-bitmap-complete-p bm))
    (is (equal '(1 0 1 0) (loop for x below 4 collect (wpg:bitmap-color-index bm x 0))))
    (is (= 1 (wpg:bitmap-color-index bm 9 0)))
    (is (= 1 (wpg:bitmap-color-index bm 8 1)))
    (is (equal '(255 255 255) (rgb (wpg:bitmap-pixel bm 0 0))))
    (is (equal '(0 0 0) (rgb (wpg:bitmap-pixel bm 1 0))))
    (is (eq bm (wpg:record-field (rec image 0) :bitmap)))
    (is (= 0 (wpg:wpg-bitmap-record-index bm)))
    ;; No Start WPG: sized from the bitmap at 75 dpi.
    (is (= 160 (wpg:wpg-image-width image)))))

(test four-and-eight-bit-bitmaps
  (let* ((image (wpg:read-wpg (wpg1 (bitmap1 3 1 4 '(#x1f #x20))
                                    (bitmap1 2 1 8 '(32 200)))))
         (b4 (first (wpg:wpg-image-bitmaps image)))
         (b8 (second (wpg:wpg-image-bitmaps image))))
    (is (equal '(1 15 2) (loop for x below 3 collect (wpg:bitmap-color-index b4 x 0))))
    (is (equal '(0 0 170) (rgb (wpg:bitmap-pixel b4 0 0))))
    (is (equal '(0 0 255) (rgb (wpg:bitmap-pixel b8 0 0))))
    (is (equalp #(0 0 255 255) (subseq (wpg:bitmap-rgba-octets b8) 0 4)))
    (is (= 6 (length (wpg:bitmap-rgb-octets b8))))))

(test color-map-applies-to-later-bitmaps
  (let* ((image (wpg:read-wpg (wpg1 (bitmap1 1 1 8 '(5))
                                    (rec1 #x0e (u16 5) (u16 1) 1 2 3)
                                    (bitmap1 1 1 8 '(5)))))
         (bitmaps (wpg:wpg-image-bitmaps image)))
    (is (equal '(170 0 170) (rgb (wpg:bitmap-pixel (first bitmaps) 0 0))))
    (is (equal '(1 2 3) (rgb (wpg:bitmap-pixel (second bitmaps) 0 0))))))

(test all-black-monochrome-palette-repaired
  (let* ((image (wpg:read-wpg (wpg1 (rec1 #x0e (u16 0) (u16 2) 0 0 0 0 0 0)
                                    (bitmap1 8 1 1 '(#x80)))))
         (bm (first (wpg:wpg-image-bitmaps image))))
    (is (equal '(255 255 255) (rgb (wpg:bitmap-pixel bm 0 0))))
    (is (equal '(0 0 0) (rgb (wpg:bitmap-pixel bm 1 0))))))

(test bitmap-type-2-placement
  (let* ((image (wpg:read-wpg
                 (wpg1 (rec1 #x14 (u16 #xa000) (s16 100) (s16 200) (s16 1300) (s16 1400)
                             (u16 2) (u16 2) (u16 8) (u16 300) (u16 300)
                             #x84 7))))
         (bm (first (wpg:wpg-image-bitmaps image))))
    (is (equal '(100 200 1300 1400) (list (wpg:wpg-bitmap-x1 bm) (wpg:wpg-bitmap-y1 bm)
                                          (wpg:wpg-bitmap-x2 bm) (wpg:wpg-bitmap-y2 bm))))
    (is (wpg:wpg-bitmap-mirror-x bm))
    (is (wpg:wpg-bitmap-mirror-y bm))
    (is (= 0 (wpg:wpg-bitmap-rotation bm)))
    (is (= 300 (wpg:wpg-bitmap-h-resolution bm)))
    (is (= 7 (wpg:bitmap-color-index bm 1 1)))))

(test short-bitmap-is-kept-with-warning
  (let* ((image (wpg:read-wpg (wpg1 (rec1 #x0b (u16 4) (u16 4) (u16 8) (u16 75) (u16 75)
                                          #x84 1))))
         (bm (first (wpg:wpg-image-bitmaps image))))
    (is (not (wpg:wpg-bitmap-complete-p bm)))
    (is (= 1 (length (wpg:wpg-image-warnings image))))))

(test bad-record-is-kept-and-parsing-continues
  (let ((image (wpg:read-wpg (wpg1 (rec1 #x0b (u16 1) (u16 1) (u16 3) (u16 75) (u16 75))
                                   (start1 10 10)))))
    (is (typep (wpg:wpg-record-error (rec image 0)) 'wpg:wpg-decode-error))
    (is (null (wpg:wpg-image-bitmaps image)))
    (is (= 10 (wpg:wpg-image-width image)))
    (is (= 1 (length (wpg:wpg-image-warnings image))))
    (signals wpg:wpg-decode-error
      (wpg:read-wpg (wpg1 (rec1 #x0b (u16 1) (u16 1) (u16 3) (u16 75) (u16 75)))
                    :strict t))))

(test short-record-body
  ;; A polyline claiming 20 bytes with only 6 left in the file.
  (let ((image (wpg:read-wpg (bytes (header) #x06 20 (u16 1) (s16 1) (s16 2)))))
    (is (= 1 (length (wpg:wpg-image-records image))))
    (is (= 6 (length (wpg:wpg-record-body (rec image 0)))))
    (is (find-if (lambda (w) (search "claims 20 bytes" w)) (wpg:wpg-image-warnings image)))
    (is (find "no End WPG record" (wpg:wpg-image-warnings image) :test #'string=))
    (signals wpg:wpg-truncated-error
      (wpg:read-wpg (bytes (header) #x06 20 (u16 1)) :strict t))))

(test long-record-lengths
  ;; 75 points = a 302-byte body, which needs the three-byte length form.
  (let ((image (wpg:read-wpg (wpg1 (rec1 #x06 (u16 75) (make-list 300 :initial-element 0))))))
    (is (= 302 (wpg:wpg-record-length (rec image 0))))
    (is (= 75 (length (wpg:record-field (rec image 0) :points))))
    (is (eq :polyline (wpg:wpg-record-name (rec image 0))))))

(test unknown-and-raw-records
  (let ((image (wpg:read-wpg (wpg1 (rec1 #x7a 1 2 3) (rec1 #x0d 9 9)))))
    (is (null (wpg:wpg-record-name (rec image 0))))
    (is (equalp #(1 2 3) (wpg:wpg-record-body (rec image 0))))
    (is (eq :graphics-text-attributes (wpg:wpg-record-name (rec image 1))))
    (is (null (wpg:wpg-record-fields (rec image 1))))))

(test postscript-payload
  (let* ((image (wpg:read-wpg (wpg1 (rec1 #x11 (make-list 8 :initial-element 0) "%!PS"))))
         (ps (wpg:record-postscript (rec image 0))))
    (is (equal "%!PS" (map 'string #'code-char ps)))
    (is (= 4 (wpg:record-field (rec image 0) :postscript-length)))
    (is (null (wpg:record-postscript (rec image 1))))))

(test postscript-type-2
  (let* ((ps-header (list (u32 4) (s16 0) (u32 0) (s16 0)
                          (s16 1) (s16 2) (s16 300) (s16 400)
                          (make-list 48 :initial-element 0)))
         (image (wpg:read-wpg (wpg1 (rec1 #x1b ps-header "%!PS-Adobe"))))
         (r (rec image 0)))
    (is (= 400 (wpg:record-field r :y2)))
    (is (equal "%!PS-Adobe" (map 'string #'code-char (wpg:record-postscript r))))))

(test graphics-text-type-2
  (let* ((image (wpg:read-wpg
                 (wpg1 (rec1 #x18 (u32 0) (s16 90) (u16 2) (s16 1) (s16 2) (s16 3) (s16 4)
                             (s16 100) (s16 100) 0 "Hi"))))
         (r (rec image 0)))
    (is (equal "Hi" (wpg:record-field r :text)))
    (is (= 90 (wpg:record-field r :rotation)))))

(test zero-type-ends-wpg1-data
  (let ((image (wpg:read-wpg (bytes (header) (start1 10 10) 0 0 0 0))))
    (is (= 1 (length (wpg:wpg-image-records image))))))

(test zero-resolution-means-72-dpi
  (let ((bm (first (wpg:wpg-image-bitmaps
                    (wpg:read-wpg (wpg1 (bitmap1 8 1 1 '(0) :hres 0 :vres 0)))))))
    (is (= 72 (wpg:wpg-bitmap-h-resolution bm)))
    (is (= 72 (wpg:wpg-bitmap-v-resolution bm)))))
