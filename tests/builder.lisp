;;;; builder.lisp -- assemble WPG files byte by byte for the tests.

(in-package #:wordperfect-graphics/tests)

(defun bytes (&rest items)
  "Flatten ITEMS (bytes, lists and vectors of bytes, nested) into an octet vector."
  (let ((out '()))
    (labels ((walk (x)
               (etypecase x
                 (integer (assert (<= 0 x 255)) (push x out))
                 (string (map nil (lambda (c) (push (char-code c) out)) x))
                 (sequence (map nil #'walk x)))))
      (walk items))
    (coerce (nreverse out) '(simple-array (unsigned-byte 8) (*)))))

(defun u16 (n) (list (ldb (byte 8 0) n) (ldb (byte 8 8) n)))
(defun s16 (n) (u16 (ldb (byte 16 0) n)))
(defun u32 (n) (list (ldb (byte 8 0) n) (ldb (byte 8 8) n)
                     (ldb (byte 8 16) n) (ldb (byte 8 24) n)))
(defun s32 (n) (u32 (ldb (byte 32 0) n)))

(defun varlen (n)
  (cond ((< n #xff) (list n))
        ((< n #x8000) (list #xff (u16 n)))
        (t (list #xff (u16 (logior #x8000 (ash n -16))) (u16 (logand n #xffff))))))

(defun header (&key (version 1) (offset 16) (file-type #x16) (key 0))
  (list #xff "WPC" (u32 offset) 1 file-type version 0 (u16 key) (u16 0)))

(defun rec1 (type &rest body)
  (let ((b (bytes body)))
    (list type (varlen (length b)) b)))

(defun rec2 (type &rest body)
  (let ((b (bytes body)))
    (list #x0f type (varlen 0) (varlen (length b)) b)))

(defun wpg1 (&rest records)
  "A WPG1 file: header, the given records, End WPG."
  (bytes (header) records (rec1 #x10)))

(defun wpg2 (&rest records)
  (bytes (header :version 2) records (rec2 #x02)))

(defun start1 (width height)
  (rec1 #x0f 1 0 (u16 width) (u16 height)))

(defun start2 (&key (units 1200) (precision 0) (bounds '(0 0 2400 1200)))
  (let ((coord (if (= precision 1)
                   (lambda (v) (s32 (* v 65536)))
                   #'s16)))
    (rec2 #x01 (u16 units) (u16 units) precision
          (mapcar coord bounds) (mapcar coord bounds))))

(defun literal-rle1 (data)
  "DATA encoded as WPG1 RLE using only literal runs."
  (loop for start from 0 below (length data) by 127
        collect (let ((chunk (subseq data start (min (length data) (+ start 127)))))
                  (list (length chunk) (coerce chunk 'list)))))

(defun read-bytes (&rest items)
  (wpg:read-wpg (bytes items)))
