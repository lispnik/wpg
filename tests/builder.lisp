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

(defun ole-file (stream-name data)
  "A minimal OLE2 compound document (512-byte sectors) holding DATA in a
stream named STREAM-NAME. Streams under 4096 bytes go in the mini stream,
as real files do."
  (let* ((data (bytes data))
         (small (< (length data) 4096))
         (sectors (make-array 0 :adjustable t :fill-pointer 0))
         (fat (make-array 128 :initial-element #xffffffff)))
    (labels ((sector (octets)
               (let ((s (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
                 (replace s octets)
                 (vector-push-extend s sectors)
                 (1- (length sectors))))
             (chain (octets)
               ;; Store OCTETS in consecutive sectors chained in the FAT.
               (let ((first nil) (prev nil))
                 (loop for start from 0 below (max 1 (length octets)) by 512
                       do (let ((s (sector (subseq octets start (min (length octets) (+ start 512))))))
                            (if prev (setf (aref fat prev) s) (setf first s))
                            (setf prev s)))
                 (setf (aref fat prev) #xfffffffe)
                 first))
             (u32s (&rest values) (apply #'bytes (mapcar #'u32 values)))
             (entry (name type start size)
               (let ((e (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0))
                     (name16 (bytes (loop for ch across name collect (list (char-code ch) 0)) 0 0)))
                 (replace e name16)
                 (replace e (u16 (length name16)) :start1 #x40)
                 (setf (aref e #x42) type)
                 (replace e (u32s #xffffffff #xffffffff (if (= type 5) 1 #xffffffff)) :start1 #x44)
                 (replace e (u32 start) :start1 #x74)
                 (replace e (u32 size) :start1 #x78)
                 e)))
      (sector #())                       ; sector 0: the FAT, filled in last
      (setf (aref fat 0) #xfffffffd)
      (let* ((dir-sector (sector #()))
             (minifat-start #xfffffffe) (root-start #xfffffffe) (root-size 0) (stream-start 0))
        (setf (aref fat dir-sector) #xfffffffe)
        (if small
            (let* ((mini-count (ceiling (max 1 (length data)) 64))
                   (minifat (apply #'u32s (append (loop for i from 1 below mini-count collect i)
                                                  (list #xfffffffe)))))
              (setf minifat-start (chain minifat)
                    root-start (chain data)
                    root-size (* 64 mini-count)
                    stream-start 0))
            (setf stream-start (chain data)))
        (replace (aref sectors dir-sector)
                 (bytes (entry "Root Entry" 5 root-start root-size)
                        (entry stream-name 2 stream-start (length data))))
        (replace (aref sectors 0) (apply #'u32s (coerce fat 'list)))
        (bytes #xd0 #xcf #x11 #xe0 #xa1 #xb1 #x1a #xe1 (make-list 16 :initial-element 0)
               (u16 #x3e) (u16 3) (u16 #xfffe) (u16 9) (u16 6) (make-list 10 :initial-element 0)
               (u32 1) (u32 dir-sector) (u32 0) (u32 4096)
               (u32 minifat-start) (u32 (if small 1 0)) (u32 #xfffffffe) (u32 0)
               (u32 0) (make-list (* 4 108) :initial-element #xff)
               (coerce sectors 'list))))))
