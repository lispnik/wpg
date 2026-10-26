;;;; fixture-tests.lisp -- real WPG files from other projects' test suites.
;;;; See tests/data/NOTICE for where they come from.

(in-package #:wordperfect-graphics/tests)

(def-suite fixtures :in wordperfect-graphics)
(in-suite fixtures)

(defun fixture (name)
  (asdf:system-relative-pathname :wordperfect-graphics/tests
                                 (concatenate 'string "tests/data/" name)))

(test imagemagick-wpg2
  ;; A 32x32 icon: a 1-bit AND mask, then the 24-bit image.
  (let* ((image (wpg:read-wpg (fixture "imagemagick-input.wpg") :strict t))
         (bitmaps (wpg:wpg-image-bitmaps image)))
    (is (= 2 (wpg:wpg-image-version image)))
    (is (= 18 (length (wpg:wpg-image-records image))))
    (is (null (wpg:wpg-image-warnings image)))
    (is (= 6728 (wpg:wpg-image-width image)))
    (is (= 2 (length bitmaps)))
    (destructuring-bind (mask icon) bitmaps
      (is (equal '(32 32 1 :rle) (list (wpg:wpg-bitmap-width mask) (wpg:wpg-bitmap-height mask)
                                       (wpg:wpg-bitmap-depth mask) (wpg:wpg-bitmap-compression mask))))
      (is (equal '(24 :rgb :none) (list (wpg:wpg-bitmap-depth icon) (wpg:wpg-bitmap-format icon)
                                        (wpg:wpg-bitmap-compression icon))))
      (is (wpg:wpg-bitmap-complete-p mask))
      (is (= 114 (wpg:wpg-bitmap-h-resolution icon)))
      (is (= 506 (wpg:wpg-bitmap-x2 icon)))
      (is (= 2352 (count 0 (wpg:bitmap-rgb-octets mask))))
      (is (= 398287 (reduce #'+ (wpg:bitmap-rgb-octets icon))))
      (loop for (x y m c) in '((0 0 "#000000" "#7d017f") (16 16 "#000000" "#bfbf00")
                               (5 25 "#000000" "#ffffff") (30 0 "#ffffff" "#000000"))
            do (is (equal m (wpg:color-hex (wpg:bitmap-pixel mask x y))))
               (is (equal c (wpg:color-hex (wpg:bitmap-pixel icon x y)))))
      ;; Where the mask is transparent the icon is black -- all but one
      ;; pixel, which is how the file is.
      (is (= 1 (loop for y below 32
                     sum (loop for x below 32
                               count (and (= 1 (wpg:bitmap-color-index mask x y))
                                          (not (equal "#000000"
                                                      (wpg:color-hex (wpg:bitmap-pixel icon x y))))))))))))

(test libreoffice-wpg1
  (let* ((image (wpg:read-wpg (fixture "libreoffice-wpg1.wpg") :strict t))
         (records (wpg:wpg-image-records image)))
    (is (= 1 (wpg:wpg-image-version image)))
    (is (equal '(6275 7910) (list (wpg:wpg-image-width image) (wpg:wpg-image-height image))))
    (is (equal '(:start-wpg :start-wpg-type-2 :fill-attributes :line-attributes :polygon
                 :fill-attributes :line-attributes :end-wpg)
               (map 'list #'wpg:wpg-record-name records)))
    (is (equalp #((232 . 3955) (3136 . 7677) (6042 . 3949) (3146 . 232))
                (wpg:record-field (aref records 4) :points)))
    (is (search "<polygon points=\"232,3955 3136,7677 6042,3949 3146,232\""
                (wpg:svg-string image)))))

(test not-a-wpg
  ;; From an ImageMagick hardening test: an MVG script named .wpg.
  (signals wpg:wpg-format-error
    (wpg:read-wpg (bytes "push graphic-context" 10 "viewbox 0 0 640 480"))))
