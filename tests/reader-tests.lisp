;;;; reader-tests.lisp -- the octet reader, the header, and input sources.

(in-package #:wordperfect-graphics/tests)

(def-suite reader :in wordperfect-graphics)
(in-suite reader)

(defun reader-over (&rest items)
  (wpg::make-octet-reader (bytes items)))

(test little-endian-integers
  (let ((r (reader-over #x34 #x12 #xfe #xff #x78 #x56 #x34 #x12 #xff #xff #xff #xff)))
    (is (= #x1234 (wpg::read-u16 r)))
    (is (= -2 (wpg::read-s16 r)))
    (is (= #x12345678 (wpg::read-u32 r)))
    (is (= -1 (wpg::read-s32 r)))
    (is (wpg::reader-at-end-p r))))

(test variable-length-integers
  (is (= 0 (wpg::read-varlen (reader-over 0))))
  (is (= #xfe (wpg::read-varlen (reader-over #xfe))))
  (is (= #xff (wpg::read-varlen (reader-over #xff #xff #x00))))
  (is (= #x7fff (wpg::read-varlen (reader-over #xff #xff #x7f))))
  (is (= #x12345 (wpg::read-varlen (reader-over #xff #x01 #x80 #x45 #x23))))
  (dolist (n '(0 1 254 255 300 32767 32768 70000 #x7fffffff))
    (is (= n (wpg::read-varlen (reader-over (varlen n)))) "round trip ~D" n)))

(test truncated-read-signals
  (let ((r (reader-over 1 2 3)))
    (wpg::read-u16 r)
    (signals wpg:wpg-truncated-error (wpg::read-u16 r))
    (handler-case (wpg::read-u32 (reader-over 1))
      (wpg:wpg-truncated-error (e)
        (is (= 4 (wpg:wpg-truncated-error-wanted e)))
        (is (= 1 (wpg:wpg-truncated-error-available e)))))))

(test header-fields
  (let ((h (wpg:read-wpg-header (bytes (header :version 2 :offset 20)))))
    (is (= 20 (wpg:wpg-header-data-offset h)))
    (is (= 1 (wpg:wpg-header-product-type h)))
    (is (= #x16 (wpg:wpg-header-file-type h)))
    (is (= 2 (wpg:wpg-header-major-version h)))
    (is (= 0 (wpg:wpg-header-encryption-key h)))))

(test rejects-non-wpg
  (signals wpg:wpg-format-error (wpg:read-wpg (bytes "GIF89a" (make-list 20 :initial-element 0))))
  (signals wpg:wpg-format-error (wpg:read-wpg (bytes #xff "WPC" 0 0)))
  (signals wpg:wpg-format-error (wpg:read-wpg (bytes (header :file-type #x0a))))
  (signals wpg:wpg-format-error (wpg:read-wpg (bytes (header :version 3))))
  (signals wpg:wpg-format-error (wpg:read-wpg (bytes (header :key #x1234))))
  (signals wpg:wpg-format-error (wpg:read-wpg (bytes (header :offset 5000))))
  (is (not (wpg:wpg-file-p (bytes "hello"))))
  (is (wpg:wpg-file-p (wpg1))))

(test honours-data-offset
  ;; Four bytes of padding between the header and the first record.
  (let ((image (wpg:read-wpg (bytes (header :offset 20) 9 9 9 9 (start1 100 50) (rec1 #x10)))))
    (is (= 2 (length (wpg:wpg-image-records image))))
    (is (= 100 (wpg:wpg-image-width image)))
    (is (= 20 (wpg:wpg-record-offset (aref (wpg:wpg-image-records image) 0))))))

(test reads-pathnames-and-streams
  (let ((data (wpg1 (start1 1200 600)))
        (path (uiop:with-temporary-file (:pathname p :keep t :type "wpg") p)))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
             (write-sequence data out))
           (is (= 1200 (wpg:wpg-image-width (wpg:read-wpg path))))
           (is (= 600 (wpg:wpg-image-height (wpg:read-wpg (namestring path)))))
           (with-open-file (in path :element-type '(unsigned-byte 8))
             (is (= 2 (length (wpg:wpg-image-records (wpg:read-wpg in))))))
           (is (= 1 (wpg:wpg-image-width-inches (wpg:read-wpg path)))))
      (uiop:delete-file-if-exists path))))

(test filenames-are-native
  ;; [ and * are ordinary characters in a filename string.
  (let ((name (concatenate 'string (uiop:native-namestring (uiop:temporary-directory))
                           (format nil "wpg-test-~D [name]*.wpg" (random 1000000 (make-random-state t))))))
    (unwind-protect
         (progn
           (with-open-file (out (uiop:parse-native-namestring name) :direction :output
                                :if-exists :supersede :element-type '(unsigned-byte 8))
             (write-sequence (wpg1 (start1 10 20)) out))
           (is (= 20 (wpg:wpg-image-height (wpg:read-wpg name)))))
      (uiop:delete-file-if-exists (uiop:parse-native-namestring name)))))
