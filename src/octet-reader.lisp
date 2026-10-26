;;;; octet-reader.lisp -- bounds-checked little-endian reads over an octet vector.
;;;;
;;;; WPG files are small, so the whole file is read into memory and parsed
;;;; from there. Every read is checked; running off the end signals
;;;; WPG-TRUNCATED-ERROR rather than returning garbage.

(in-package #:wordperfect-graphics)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun make-octets (n)
  (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun coerce-octets (sequence)
  (if (typep sequence 'octets)
      sequence
      (let ((v (make-octets (length sequence))))
        (replace v sequence)
        v)))

(defstruct (octet-reader (:constructor %make-octet-reader (octets position end))
                         (:conc-name reader-))
  (octets (make-octets 0) :type octets)
  (position 0 :type fixnum)
  (end 0 :type fixnum))

(defun make-octet-reader (octets &key (start 0) (end (length octets)))
  (%make-octet-reader (coerce-octets octets) start end))

(declaim (inline reader-remaining reader-at-end-p))
(defun reader-remaining (r)
  (- (reader-end r) (reader-position r)))

(defun reader-at-end-p (r)
  (>= (reader-position r) (reader-end r)))

(defun reader-need (r n)
  (when (> n (reader-remaining r))
    (error 'wpg-truncated-error :position (reader-position r)
                                :wanted n
                                :available (max 0 (reader-remaining r)))))

(defun read-u8 (r)
  (reader-need r 1)
  (prog1 (aref (reader-octets r) (reader-position r))
    (incf (reader-position r))))

(defun read-u16 (r)
  (reader-need r 2)
  (let* ((v (reader-octets r)) (p (reader-position r)))
    (setf (reader-position r) (+ p 2))
    (logior (aref v p) (ash (aref v (1+ p)) 8))))

(defun read-u32 (r)
  (reader-need r 4)
  (let* ((v (reader-octets r)) (p (reader-position r)))
    (setf (reader-position r) (+ p 4))
    (logior (aref v p)
            (ash (aref v (+ p 1)) 8)
            (ash (aref v (+ p 2)) 16)
            (ash (aref v (+ p 3)) 24))))

(defun signed (value bits)
  (if (logbitp (1- bits) value)
      (- value (ash 1 bits))
      value))

(defun read-s16 (r) (signed (read-u16 r) 16))
(defun read-s32 (r) (signed (read-u32 r) 32))

(defun read-octets (r n)
  (reader-need r n)
  (let ((p (reader-position r)))
    (setf (reader-position r) (+ p n))
    (subseq (reader-octets r) p (+ p n))))

(defun reader-skip (r n)
  (reader-need r n)
  (incf (reader-position r) n))

(defun read-varlen (r)
  "Read WPG's variable-length unsigned integer, used for record lengths.
One byte below #xFF is the value. #xFF introduces a 16-bit value; if that
has its top bit set, its low 15 bits are the high half of a 31-bit value
whose low half follows."
  (let ((b (read-u8 r)))
    (if (< b #xff)
        b
        (let ((w (read-u16 r)))
          (if (logbitp 15 w)
              (logior (ash (logand w #x7fff) 16) (read-u16 r))
              w)))))

(defun octets-to-string (octets)
  "Decode a byte string as Latin-1, stopping at the first NUL.
WPG text is in WordPerfect's own character set, which agrees with ASCII for
the printable range; that is as far as this goes."
  (let ((end (or (position 0 octets) (length octets))))
    (map 'string #'code-char (subseq octets 0 end))))
