;;;; ole.lisp -- WPG files wrapped in an OLE2 compound document.
;;;;
;;;; Corel's suites saved some graphics (the WordPerfect 8 backgrounds, for
;;;; one) as OLE2 "compound file binary" containers, with the WPG itself in
;;;; a stream named PerfectOffice_MAIN. libwpg reads them the same way. This
;;;; is just enough of the container format to read one stream by name:
;;;; sectors, the sector allocation table (FAT), the directory, and the
;;;; mini stream that holds small streams.

(in-package #:wordperfect-graphics)

(defparameter +ole-signature+ #(#xd0 #xcf #x11 #xe0 #xa1 #xb1 #x1a #xe1))

(defconstant +end-of-chain+ #xfffffffe)

(defun ole-file-p (octets)
  (and (>= (length octets) 512)
       (every #'= +ole-signature+ (subseq octets 0 8))))

(defun ole-u16 (octets i)
  (if (< (1+ i) (length octets))
      (logior (aref octets i) (ash (aref octets (1+ i)) 8))
      (format-error "OLE container truncated at offset ~D" i)))

(defun ole-u32 (octets i)
  (logior (ole-u16 octets i) (ash (ole-u16 octets (+ i 2)) 16)))

(defun ole-chain (table start limit)
  "The sector numbers of the chain starting at START in TABLE (a vector of
next-sector entries). LIMIT bounds its length, against loops."
  (loop for sector = start then (aref table sector)
        for count from 0
        while (and (< sector (length table)) (/= sector +end-of-chain+))
        when (> count limit)
          do (format-error "OLE container has a looping sector chain")
        collect sector))

(defun ole-read-chain (octets sectors sector-size)
  (let ((out (make-octets (* sector-size (length sectors)))))
    (loop for s in sectors
          for k from 0
          do (let ((start (* (1+ s) sector-size)))
               (replace out octets :start1 (* k sector-size)
                                   :start2 (min start (length octets))
                                   :end2 (min (+ start sector-size) (length octets)))))
    out))

(defun ole-stream (octets name)
  "The contents of the stream NAME (compared without regard to case) in
the OLE2 compound document OCTETS, or NIL if there is no such stream."
  (let* ((sector-size (ash 1 (ole-u16 octets #x1e)))
         (mini-size (ash 1 (ole-u16 octets #x20)))
         (fat-count (ole-u32 octets #x2c))
         (dir-start (ole-u32 octets #x30))
         (mini-cutoff (ole-u32 octets #x38))
         (minifat-start (ole-u32 octets #x3c))
         (difat-start (ole-u32 octets #x44))
         (max-sectors (1+ (floor (length octets) sector-size)))
         (fat-sectors '()))
    (unless (and (<= 512 sector-size 65536) (<= 32 mini-size sector-size))
      (format-error "OLE container has unusual sector sizes (~D, ~D)" sector-size mini-size))
    ;; The FAT's own sectors: 109 listed in the header, the rest in a chain
    ;; of DIFAT sectors, each ending with a pointer to the next.
    (loop for i below (min 109 fat-count)
          do (push (ole-u32 octets (+ #x4c (* 4 i))) fat-sectors))
    (loop with next = difat-start
          with per = (1- (/ sector-size 4))
          repeat max-sectors
          while (and (< (length fat-sectors) fat-count) (< next max-sectors))
          do (let ((base (* (1+ next) sector-size)))
               (loop for i below per
                     while (< (length fat-sectors) fat-count)
                     do (push (ole-u32 octets (+ base (* 4 i))) fat-sectors))
               (setf next (ole-u32 octets (+ base (* 4 per))))))
    (let* ((fat-bytes (ole-read-chain octets (nreverse fat-sectors) sector-size))
           (fat (let ((v (make-array (/ (length fat-bytes) 4))))
                  (dotimes (i (length v) v) (setf (aref v i) (ole-u32 fat-bytes (* 4 i))))))
           (directory (ole-read-chain octets (ole-chain fat dir-start max-sectors) sector-size)))
      (flet ((entry (i)
               ;; (values name type start size) for directory entry I.
               (let* ((base (* 128 i))
                      (name-bytes (min 64 (max 0 (- (ole-u16 directory (+ base #x40)) 2)))))
                 (values (map 'string (lambda (k) (code-char (ole-u16 directory (+ base (* 2 k)))))
                              (loop for k below (/ name-bytes 2) collect k))
                         (aref directory (+ base #x42))
                         (ole-u32 directory (+ base #x74))
                         (ole-u32 directory (+ base #x78))))))
        (loop for i below (floor (length directory) 128)
              do (multiple-value-bind (entry-name type start size) (entry i)
                   (when (and (= type 2) (string-equal entry-name name))
                     (return
                       (subseq
                        (if (< size mini-cutoff)
                            ;; Small streams live in the mini stream, which is
                            ;; the root entry's data, in 64-byte mini sectors.
                            (multiple-value-bind (root-name root-type root-start) (entry 0)
                              (declare (ignore root-name root-type))
                              (let* ((mini-stream (ole-read-chain
                                                   octets (ole-chain fat root-start max-sectors)
                                                   sector-size))
                                     (minifat-bytes (ole-read-chain
                                                     octets (ole-chain fat minifat-start max-sectors)
                                                     sector-size))
                                     (minifat (let ((v (make-array (/ (length minifat-bytes) 4))))
                                                (dotimes (k (length v) v)
                                                  (setf (aref v k) (ole-u32 minifat-bytes (* 4 k))))))
                                     (chain (ole-chain minifat start (length minifat)))
                                     (out (make-octets (* mini-size (length chain)))))
                                (loop for s in chain for k from 0
                                      do (replace out mini-stream
                                                  :start1 (* k mini-size)
                                                  :start2 (min (* s mini-size) (length mini-stream))
                                                  :end2 (min (* (1+ s) mini-size) (length mini-stream))))
                                out))
                            (ole-read-chain octets (ole-chain fat start max-sectors) sector-size))
                        0 size)))))))))
