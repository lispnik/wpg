;;;; rle-tests.lisp -- the WPG1 and WPG2 run-length decoders.

(in-package #:wordperfect-graphics/tests)

(def-suite rle :in wordperfect-graphics)
(in-suite rle)

(defun rle1 (row-bytes rows &rest stream)
  (multiple-value-bind (out n)
      (wpg:decode-wpg1-rle (wpg::make-octet-reader (bytes stream)) row-bytes rows)
    (values (coerce out 'list) n)))

(defun rle2 (row-bytes rows &rest stream)
  (multiple-value-bind (out n)
      (wpg:decode-wpg2-rle (wpg::make-octet-reader (bytes stream)) row-bytes rows)
    (values (coerce out 'list) n)))

(test wpg1-runs
  (is (equal '(7 7 7 7) (rle1 4 1 #x84 7)))
  (is (equal '(255 255 255) (rle1 3 1 #x80 3)))
  (is (equal '(1 2 3) (rle1 3 1 3 1 2 3)))
  (is (equal '(1 2 9 9) (rle1 4 1 2 1 2 #x82 9))))

(test wpg1-row-repeat
  (is (equal '(1 2 1 2 1 2) (rle1 2 3 2 1 2 0 2)))
  ;; A repeat count past the end is clipped.
  (is (equal '(5 6 5 6) (rle1 2 2 2 5 6 0 9)))
  ;; Mid-row, the last ROW-BYTES bytes are copied, as libwpg does.
  (is (equal '(1 2 3 2 3 0) (rle1 2 3 3 1 2 3 0 1))))

(test wpg1-short-data
  (multiple-value-bind (out n) (rle1 4 2 #x83 1)
    (is (= 3 n))
    (is (equal '(1 1 1 0 0 0 0 0) out))))

(test wpg2-literals-and-repeats
  ;; Counts mean count+1.
  (is (equal '(1 2 3) (rle2 3 1 2 1 2 3)))
  (is (equal '(9 9 9) (rle2 3 1 #x82 9)))
  (is (equal '(0 0 0 0) (rle2 4 1 #x7f 3)))
  (is (equal '(255 255) (rle2 2 1 #xff 1)))
  ;; #xFD repeats the last repeated sample.
  (is (equal '(4 4 4 4) (rle2 4 1 #x81 4 #xfd 1))))

(test wpg2-sample-size
  (is (equal '(1 2 3 1 2 3) (rle2 6 1 #x7d 3 #x81 1 2 3)))
  (is (equal '(0 0 0 0 0 0) (rle2 6 1 #x7d 3 #x7f 1)))
  (is (equal '(5 6 7 8 9 10) (rle2 6 1 #x7d 2 2 5 6 7 8 9 10)))
  (signals wpg:wpg-decode-error (rle2 2 1 #x7d 9)))

(test wpg2-row-repeat
  (is (equal '(1 2 1 2 1 2) (rle2 2 3 1 1 2 #xfe 1)))
  (is (equal '(1 2 3 2 3 0) (rle2 2 3 2 1 2 3 #xfe 0))))

(test wpg2-xor
  ;; The raster after #x7E is XORed with the one before it.
  (is (equal '(1 2 2 1) (rle2 2 2 1 1 2 #x7e 1 3 3)))
  (is (equal '(1 2 2 1 5 5) (rle2 2 3 1 1 2 #x7e 1 3 3 1 5 5))))
