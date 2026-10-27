;;;; bitmap.lisp -- pixel access for decoded bitmaps.

(in-package #:wordperfect-graphics)

(defun row-bytes-for (width depth)
  (ceiling (* width depth) 8))

(defvar *max-bitmap-bytes* (* 256 1024 1024)
  "The largest decoded bitmap the reader will allocate. A damaged header
can claim a bitmap of gigabytes; past this size the record fails to decode
instead.")

(defun check-bitmap-size (row-bytes height)
  (when (> (* row-bytes height) *max-bitmap-bytes*)
    (decode-error "bitmap of ~:D bytes is larger than *MAX-BITMAP-BYTES* (~:D)"
                  (* row-bytes height) *max-bitmap-bytes*)))

(defun bitmap-color-index (bitmap x y)
  "The palette index of pixel (X, Y), counting from the top-left corner.
Only meaningful for paletted bitmaps (depth 8 or less)."
  (let* ((depth (wpg-bitmap-depth bitmap))
         (data (wpg-bitmap-data bitmap))
         (row (* y (wpg-bitmap-row-bytes bitmap))))
    (ecase depth
      (8 (aref data (+ row x)))
      ((1 2 4)
       (let* ((bit (* x depth))
              (byte (aref data (+ row (floor bit 8))))
              (shift (- 8 depth (mod bit 8))))
         (ldb (byte depth shift) byte))))))

(defun bitmap-pixel (bitmap x y)
  "The COLOR of pixel (X, Y), counting from the top-left corner."
  (let* ((data (wpg-bitmap-data bitmap))
         (bytes (max 1 (floor (wpg-bitmap-depth bitmap) 8)))
         (i (+ (* y (wpg-bitmap-row-bytes bitmap)) (* bytes x))))
    (flet ((at (k) (aref data (+ i k))))
      (ecase (wpg-bitmap-format bitmap)
        (:indexed (aref (wpg-bitmap-palette bitmap) (bitmap-color-index bitmap x y)))
        (:rgb (make-color (at 0) (at 1) (at 2)))
        (:rgba (make-color (at 0) (at 1) (at 2) (at 3)))
        (:gray (make-color (at 0) (at 0) (at 0)))
        ;; 16-bit gray: little-endian, so the high byte is second.
        (:gray16 (make-color (at 1) (at 1) (at 1)))
        (:rgb555 (let ((w (logior (at 0) (ash (at 1) 8))))
                   (flet ((c (shift) (round (* 255 (ldb (byte 5 shift) w)) 31)))
                     (make-color (c 10) (c 5) (c 0)))))))))

(defun %bitmap-octets (bitmap channels)
  (let* ((w (wpg-bitmap-width bitmap))
         (h (wpg-bitmap-height bitmap))
         (out (make-octets (* w h channels)))
         (i 0))
    (declare (type (simple-array (unsigned-byte 8) (*)) out) (type fixnum i))
    (if (eq (wpg-bitmap-format bitmap) :indexed)
        ;; Paletted: look the palette up once per entry, not per pixel.
        (let* ((palette (wpg-bitmap-palette bitmap))
               (table (make-octets (* 4 (length palette)))))
          (declare (type (simple-array (unsigned-byte 8) (*)) table))
          (loop for c across palette
                for k from 0 by 4
                do (setf (aref table k) (color-red c) (aref table (+ k 1)) (color-green c)
                         (aref table (+ k 2)) (color-blue c) (aref table (+ k 3)) (color-alpha c)))
          (dotimes (y h out)
            (dotimes (x w)
              (let ((k (* 4 (bitmap-color-index bitmap x y))))
                (dotimes (j channels)
                  (setf (aref out (+ i j)) (aref table (+ k j))))
                (incf i channels)))))
        (dotimes (y h out)
          (dotimes (x w)
            (let ((c (bitmap-pixel bitmap x y)))
              (setf (aref out i) (color-red c)
                    (aref out (+ i 1)) (color-green c)
                    (aref out (+ i 2)) (color-blue c))
              (when (= channels 4)
                (setf (aref out (+ i 3)) (color-alpha c)))
              (incf i channels)))))))

(defun orient-octets (octets width height channels bitmap)
  "Apply BITMAP's mirroring and rotation to its pixels OCTETS. Returns
(values octets width height). Rotation is counterclockwise, and only right
angles are applied; other angles are left to the caller."
  (let ((mx (wpg-bitmap-mirror-x bitmap))
        (my (wpg-bitmap-mirror-y bitmap))
        (quarter-turns (let ((r (mod (wpg-bitmap-rotation bitmap) 360)))
                         (if (zerop (mod r 90)) (/ r 90) 0))))
    (if (and (not mx) (not my) (zerop quarter-turns))
        (values octets width height)
        (let* ((swap (oddp quarter-turns))
               (w2 (if swap height width))
               (h2 (if swap width height))
               (out (make-octets (length octets))))
          (dotimes (y height)
            (dotimes (x width)
              (let* ((sx (if mx (- width 1 x) x))
                     (sy (if my (- height 1 y) y))
                     ;; (x, y) in the result for source pixel (sx, sy).
                     (dx x) (dy y))
                (ecase quarter-turns
                  (0 (setf dx x dy y))
                  (1 (setf dx y dy (- width 1 x)))
                  (2 (setf dx (- width 1 x) dy (- height 1 y)))
                  (3 (setf dx (- height 1 y) dy x)))
                (replace out octets
                         :start1 (* channels (+ dx (* dy w2)))
                         :start2 (* channels (+ sx (* sy width)))
                         :end2 (* channels (+ sx (* sy width) 1))))))
          (values out w2 h2)))))

(defun bitmap-rgba-octets (bitmap &key oriented)
  "The pixels of BITMAP as a flat RGBA octet vector, top row first.
With ORIENTED, the file's mirroring and right-angle rotation are applied;
the second and third values are then the width and height."
  (let ((octets (%bitmap-octets bitmap 4)))
    (if oriented
        (orient-octets octets (wpg-bitmap-width bitmap) (wpg-bitmap-height bitmap) 4 bitmap)
        (values octets (wpg-bitmap-width bitmap) (wpg-bitmap-height bitmap)))))

(defun bitmap-rgb-octets (bitmap &key oriented)
  "The pixels of BITMAP as a flat RGB octet vector. See BITMAP-RGBA-OCTETS."
  (let ((octets (%bitmap-octets bitmap 3)))
    (if oriented
        (orient-octets octets (wpg-bitmap-width bitmap) (wpg-bitmap-height bitmap) 3 bitmap)
        (values octets (wpg-bitmap-width bitmap) (wpg-bitmap-height bitmap)))))

(defun write-bitmap-ppm (bitmap destination &key oriented)
  "Write BITMAP as a binary PPM (P6) to DESTINATION, a pathname or a
binary output stream. Alpha is dropped. ORIENTED: see BITMAP-RGBA-OCTETS."
  (flet ((write-to (out)
           (multiple-value-bind (octets w h) (bitmap-rgb-octets bitmap :oriented oriented)
             (write-sequence (map 'vector #'char-code (format nil "P6~%~D ~D~%255~%" w h)) out)
             (write-sequence octets out))))
    (if (streamp destination)
        (write-to destination)
        (with-open-file (out destination :direction :output :if-exists :supersede
                                         :element-type '(unsigned-byte 8))
          (write-to out)))
    destination))
