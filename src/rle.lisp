;;;; rle.lisp -- the two run-length encodings WPG bitmaps use.
;;;;
;;;; Both decoders fill a buffer of ROWS rows of ROW-BYTES bytes and return
;;;; (values buffer bytes-produced). A short stream leaves the remainder
;;;; zeroed rather than failing, since partial images are still useful;
;;;; callers compare bytes-produced against the buffer length.

(in-package #:wordperfect-graphics)

(defun repeat-previous-row (out pos row-bytes count total)
  "Append COUNT copies of the ROW-BYTES bytes just before POS. Returns the
new POS. Normally POS is at a row boundary, so this repeats the previous
row; libwpg copies the last ROW-BYTES bytes even when it is not, and so
does this. With less than a row decoded, the repeats are left as zeros."
  (loop repeat count
        while (< pos total)
        do (let ((end (min total (+ pos row-bytes))))
             (when (>= pos row-bytes)
               ;; Byte by byte: source and destination may not overlap
               ;; cleanly when POS is mid-row.
               (loop for i from pos below end
                     do (setf (aref out i) (aref out (- i row-bytes)))))
             (setf pos end)))
  pos)

(defun decode-wpg1-rle (reader row-bytes rows)
  "Decode WPG1 bitmap data. Each code byte B is one of:
  #x80 | n, n>0   the next byte, repeated n times
  #x80, then n    #xFF repeated n times
  n, n>0          n literal bytes follow
  #x00, then n    the previous row, repeated n times"
  (let* ((total (* row-bytes rows))
         (out (make-octets total))
         (pos 0))
    (flet ((emit (byte count)
             (let ((end (min total (+ pos count))))
               (fill out byte :start pos :end end)
               (setf pos end))))
      (loop while (and (< pos total) (not (reader-at-end-p reader)))
            do (let* ((code (read-u8 reader))
                      (count (logand code #x7f)))
                 (cond ((logbitp 7 code)
                        (if (plusp count)
                            (emit (read-u8 reader) count)
                            (emit #xff (read-u8 reader))))
                       ((plusp count)
                        (loop repeat count
                              do (let ((b (read-u8 reader)))
                                   (when (< pos total)
                                     (setf (aref out pos) b)
                                     (incf pos)))))
                       (t
                        (setf pos (repeat-previous-row out pos row-bytes
                                                       (read-u8 reader) total)))))))
    (values out pos)))

(defun decode-wpg2-rle (reader row-bytes rows)
  "Decode WPG2 bitmap data. Runs work in samples of SAMPLE-SIZE bytes
(default 1). Every count N below means N+1. Codes:
  #x7D s      set the sample size to s (1..8)
  #x7E        XOR the next ROW-BYTES bytes with the row before them
  #x7F n      zero samples
  #xFD n      the last repeated sample again
  #xFE n      the previous row
  #xFF n      #xFF-filled samples
  #x80 | n    one sample follows, repeated
  n (< #x80)  n+1 literal samples follow"
  (let* ((total (* row-bytes rows))
         (out (make-octets total))
         (pos 0)
         (sample-size 1)
         (sample (make-octets 8))
         (xor-start nil))
    (labels ((put (b)
               (when (< pos total)
                 (setf (aref out pos) b)
                 (incf pos)))
             (fill-run (byte n)
               (loop repeat (* sample-size n) do (put byte)))
             (sample-run (n)
               (loop repeat n
                     do (dotimes (i sample-size) (put (aref sample i))))))
      (loop while (and (< pos total) (not (reader-at-end-p reader)))
            do (let ((code (read-u8 reader)))
                 (case code
                   (#x7d (let ((s (read-u8 reader)))
                           (unless (<= 1 s 8)
                             (decode-error "invalid WPG2 sample size ~D" s))
                           (setf sample-size s)))
                   (#x7e (setf xor-start pos))
                   (#x7f (fill-run 0 (1+ (read-u8 reader))))
                   (#xfd (sample-run (1+ (read-u8 reader))))
                   (#xfe (setf pos (repeat-previous-row out pos row-bytes
                                                        (1+ (read-u8 reader)) total)))
                   (#xff (fill-run #xff (1+ (read-u8 reader))))
                   (t (let ((n (1+ (logand code #x7f))))
                        (cond ((logbitp 7 code)
                               (dotimes (i sample-size)
                                 (setf (aref sample i) (read-u8 reader)))
                               (sample-run n))
                              (t
                               (loop repeat (* n sample-size)
                                     do (put (read-u8 reader)))))))))
               ;; Once the XORed raster is complete, combine it with the
               ;; one before it.
               (when (and xor-start (>= pos (min total (+ xor-start row-bytes))))
                 (when (>= xor-start row-bytes)
                   (loop for i from xor-start below (min total (+ xor-start row-bytes))
                         do (setf (aref out i)
                                  (logxor (aref out i) (aref out (- i row-bytes))))))
                 (setf xor-start nil))))
    (values out pos)))
