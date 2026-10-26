;;;; wpg2-tests.lisp -- WPG2 records.

(in-package #:wordperfect-graphics/tests)

(def-suite wpg2 :in wordperfect-graphics)
(in-suite wpg2)

(test start-wpg-2
  (let ((image (wpg:read-wpg (wpg2 (start2 :units 1000 :bounds '(100 -50 2100 950))))))
    (is (= 2 (wpg:wpg-image-version image)))
    (is (= 1000 (wpg:wpg-image-x-units image)))
    (is (= 2000 (wpg:wpg-image-width image)))
    (is (= 1000 (wpg:wpg-image-height image)))
    (is (= 100 (wpg:wpg-image-x-offset image)))
    (is (= -50 (wpg:wpg-image-y-offset image)))
    (is (not (wpg:wpg-image-double-precision image)))
    (let ((r (aref (wpg:wpg-image-records image) 0)))
      (is (= #x0f (wpg:wpg-record-class r)))
      (is (= 0 (wpg:wpg-record-extension r))))))

(test double-precision
  (let* ((image (wpg:read-wpg
                 (wpg2 (start2 :precision 1 :bounds '(0 0 1200 600))
                       (rec2 #x15 (u16 0) (u16 1) (s32 (* 3/2 65536)) (s32 (* 10 65536))))))
         (line (aref (wpg:wpg-image-records image) 1)))
    (is (wpg:wpg-image-double-precision image))
    (is (= 1200 (wpg:wpg-image-width image)))
    (is (equalp #((1.5d0 . 10)) (wpg:record-field line :points)))))

(test palette-with-transparency
  (let* ((image (wpg:read-wpg (wpg2 (rec2 #x0c (u16 1) (u16 2) 10 20 30 0 40 50 60 255))))
         (p (wpg:wpg-image-palette image)))
    (is (equal '(10 20 30) (rgb (aref p 1))))
    (is (= 255 (wpg:color-alpha (aref p 1))))
    (is (= 0 (wpg:color-alpha (aref p 2))))))

(test dp-palette
  (let ((p (wpg:wpg-image-palette
            (wpg:read-wpg (wpg2 (rec2 #x0d (u16 0) (u16 1)
                                      (u16 #xff00) (u16 #x8000) (u16 #x0100) (u16 0)))))))
    (is (equal '(255 128 1) (rgb (aref p 0))))))

(defun bitmap-object (x1 y1 x2 y2 &key (res 300))
  (rec2 #x1b (u16 0) (s16 x1) (s16 y1) (s16 x2) (s16 y2) (u16 res) (u16 res)))

(test uncompressed-24-bit-bitmap
  (let* ((image (wpg:read-wpg
                 (wpg2 (start2)
                       (bitmap-object 0 0 600 300)
                       (rec2 #x0e (u16 2) (u16 1) 8 0  1 2 3 4 5 6))))
         (bm (first (wpg:wpg-image-bitmaps image))))
    (is (= 24 (wpg:wpg-bitmap-depth bm)))
    (is (eq :none (wpg:wpg-bitmap-compression bm)))
    (is (equal '(4 5 6) (rgb (wpg:bitmap-pixel bm 1 0))))
    (is (= 600 (wpg:wpg-bitmap-x2 bm)))
    (is (= 300 (wpg:wpg-bitmap-h-resolution bm)))
    (is (equalp #(1 2 3 255 4 5 6 255) (wpg:bitmap-rgba-octets bm)))))

(test rle-8-bit-bitmap-with-palette
  (let* ((image (wpg:read-wpg
                 (wpg2 (rec2 #x0c (u16 7) (u16 1) 200 100 50 0)
                       (rec2 #x0e (u16 3) (u16 2) 4 1  #x82 7 #xfe 0))))
         (bm (first (wpg:wpg-image-bitmaps image))))
    (is (= 8 (wpg:wpg-bitmap-depth bm)))
    (is (wpg:wpg-bitmap-complete-p bm))
    (is (equal '(200 100 50) (rgb (wpg:bitmap-pixel bm 2 1))))
    ;; Without a placement record, the bitmap has none.
    (is (null (wpg:wpg-bitmap-x1 bm)))))

(test default-wpg2-palette
  (let* ((image (wpg:read-wpg (wpg2 (rec2 #x0e (u16 4) (u16 1) 2 0 #b00011011))))
         (bm (first (wpg:wpg-image-bitmaps image))))
    (is (= 2 (wpg:wpg-bitmap-depth bm)))
    (is (equal '((0 0 0) (255 255 255) (127 127 127) (191 191 191))
               (loop for x below 4 collect (rgb (wpg:bitmap-pixel bm x 0)))))))

(test bad-bitmap-formats
  (let ((image (wpg:read-wpg (wpg2 (rec2 #x0e (u16 1) (u16 1) 10 0 0)
                                   (rec2 #x0e (u16 1) (u16 1) 4 7 0)))))
    (is (every #'wpg:wpg-record-error (subseq (wpg:wpg-image-records image) 0 2)))
    (is (null (wpg:wpg-image-bitmaps image)))))

(test characterization
  ;; Flags: translate (bit 1), filled (13), closed (14), framed (15).
  (let* ((flags (logior #b10 (ash 1 13) (ash 1 14) (ash 1 15)))
         (image (wpg:read-wpg
                 (wpg2 (rec2 #x15 (u16 flags)
                             (u16 #x8000) (s32 10) (u16 0) (s32 -20)
                             (u16 2) (s16 0) (s16 0) (s16 5) (s16 5)))))
         (ch (wpg:record-field (aref (wpg:wpg-image-records image) 0) :characterization)))
    (is (getf ch :filled))
    (is (getf ch :closed))
    (is (getf ch :framed))
    (is (equal '(1 0 0 1 10.5d0 -20) (getf ch :matrix)))))

(test characterization-scale-and-id
  ;; Scale (bit 3) and object id (bit 5).
  (let* ((image (wpg:read-wpg
                 (wpg2 (rec2 #x18 (u16 (logior 8 32)) (u16 42)
                             (s32 (* 2 65536)) (s32 (* 3 65536))
                             (s16 0) (s16 0) (s16 10) (s16 10) (s16 0) (s16 0)))))
         (r (aref (wpg:wpg-image-records image) 0))
         (ch (wpg:record-field r :characterization)))
    (is (= 42 (getf ch :object-id)))
    (is (equal '(2 0 0 3 0 0) (getf ch :matrix)))
    (is (= 10 (wpg:record-field r :x2)))))

(test pen-and-brush
  (let* ((image (wpg:read-wpg (wpg2 (rec2 #x25 1 2 3 0)
                                    (rec2 #x31 0 9 8 7 128)
                                    (rec2 #x2b (u16 24) (u16 24)))))
         (records (wpg:wpg-image-records image)))
    (is (equal '(1 2 3) (rgb (wpg:record-field (aref records 0) :color))))
    (is (= 127 (wpg:color-alpha (wpg:record-field (aref records 1) :color))))
    (is (= 24 (wpg:record-field (aref records 2) :width)))))

(test polycurve-and-arc
  (let* ((image (wpg:read-wpg
                 (wpg2 (rec2 #x17 (u16 0) (u16 1) (s16 1) (s16 2) (s16 3) (s16 4) (s16 5) (s16 6))
                       (rec2 #x19 (u16 0) (mapcar #'s16 '(50 50 10 20 60 50 50 70))))))
         (records (wpg:wpg-image-records image)))
    (is (equalp #(((1 . 2) (3 . 4) (5 . 6))) (wpg:record-field (aref records 0) :nodes)))
    (is (= 70 (wpg:record-field (aref records 1) :ey)))))

(test record-names
  (is (eq :bitmap-data (wpg:record-type-name 2 #x0e)))
  (is (eq :color-map (wpg:record-type-name 1 #x0e)))
  (is (eq :text-line (wpg:record-type-name 2 #x1c)))
  (is (null (wpg:record-type-name 2 #xee))))

(test long-object-id
  (let* ((image (wpg:read-wpg (wpg2 (rec2 #x15 (u16 32) (u16 #x8001) (u16 2) (u16 0)))))
         (ch (wpg:record-field (aref (wpg:wpg-image-records image) 0) :characterization)))
    (is (= #x10002 (getf ch :object-id)))))

(test more-pixel-formats
  (let* ((image (wpg:read-wpg
                 (wpg2 (rec2 #x0e (u16 1) (u16 1) 9 0  10 20 30 128)  ; RGBA
                       (rec2 #x0e (u16 2) (u16 1) 12 0  0 200)        ; 8-bit gray
                       (rec2 #x0e (u16 1) (u16 1) 5 0  #x34 #x99)     ; 16-bit gray
                       (rec2 #x0e (u16 1) (u16 1) 6 0  (u16 #x7c1f))))) ; RGB555
         (bitmaps (wpg:wpg-image-bitmaps image)))
    (is (equal '(:rgba :gray :gray16 :rgb555) (mapcar #'wpg:wpg-bitmap-format bitmaps)))
    (is (= 128 (wpg:color-alpha (wpg:bitmap-pixel (first bitmaps) 0 0))))
    (is (equal '(200 200 200) (rgb (wpg:bitmap-pixel (second bitmaps) 1 0))))
    (is (equal '(#x99 #x99 #x99) (rgb (wpg:bitmap-pixel (third bitmaps) 0 0))))
    (is (equal '(255 0 255) (rgb (wpg:bitmap-pixel (fourth bitmaps) 0 0))))))

(test oversized-bitmap-refused
  (let* ((image (wpg:read-wpg (wpg2 (rec2 #x0e (u16 65535) (u16 65535) 9 1 0))))
         (r (aref (wpg:wpg-image-records image) 0)))
    (is (typep (wpg:wpg-record-error r) 'wpg:wpg-decode-error))
    (is (search "larger than" (princ-to-string (wpg:wpg-record-error r))))))

(defun rec2x (type extension &rest body)
  "A WPG2 record with EXTENSION child records following it."
  (let ((b (bytes body)))
    (list #x0f type (varlen extension) (varlen (length b)) b)))

(test record-nesting
  ;; Start WPG owns one child; a compound polygon owns two polylines; a
  ;; group owns a compound polygon (with one child) and a rectangle.
  (let* ((image (wpg:read-wpg
                 (wpg2 (rec2x #x01 1 (u16 1200) (u16 1200) 0 (make-list 16 :initial-element 0))
                       (rec2 #x03)
                       (rec2x #x1a 2 (u16 0))
                       (rec2 #x15 (u16 0) (u16 0))
                       (rec2 #x15 (u16 0) (u16 0))
                       (rec2x #x20 2)
                       (rec2x #x1a 1 (u16 0))
                       (rec2 #x15 (u16 0) (u16 0))
                       (rec2 #x18 (u16 0) (make-list 12 :initial-element 0)))))
         (records (wpg:wpg-image-records image)))
    (is (equal '(nil 0 nil 2 2 nil 5 6 5 nil)
               (map 'list #'wpg:wpg-record-parent records)))
    (is (equal '(0 1 0 1 1 0 1 2 1 0) (map 'list #'wpg:wpg-record-depth records)))
    (is (equal '(3 4) (mapcar #'wpg:wpg-record-index
                              (wpg:record-children image (aref records 2)))))))

(test brush-gradient-record
  ;; Nine bytes as Corel writes them: angle 55 degrees, references, flags.
  (let* ((image (wpg:read-wpg (wpg2 (rec2 #x2f (u16 0) (u16 55) (u16 0) (u16 #x7272) #x40))))
         (r (aref (wpg:wpg-image-records image) 0)))
    (is (null (wpg:wpg-record-error r)))
    (is (= 55 (wpg:record-field r :angle)))
    (is (= #x7272 (wpg:record-field r :y-ref)))
    (is (= #x40 (wpg:record-field r :flags)))))

(test gradient-brush-colors
  (let* ((image (wpg:read-wpg (wpg2 (rec2 #x31 1 (u16 2) 1 2 3 0 4 5 6 0 (u16 #x8000)))))
         (r (aref (wpg:wpg-image-records image) 0)))
    (is (= 2 (length (wpg:record-field r :colors))))
    (is (equal '(0.5d0) (wpg:record-field r :positions)))))
