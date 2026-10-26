;;;; read.lisp -- the entry points: header, record stream, whole file.

(in-package #:wordperfect-graphics)

(defparameter +wpg-magic+ #(#xff #x57 #x50 #x43)) ; #xFF "WPC"

(defun source-octets (source)
  "SOURCE as an octet vector: a pathname or filename string is read from
disk, a binary stream is read to its end, and a vector of octets is used as
is. A string is taken as a native filename, so characters such as [ and *
in it are not wildcards."
  (etypecase source
    (string (source-octets (uiop:parse-native-namestring source)))
    (pathname
     (with-open-file (in source :element-type '(unsigned-byte 8))
       (let ((v (make-octets (file-length in))))
         (read-sequence v in)
         v)))
    (stream
     (let ((chunks '()) (total 0))
       (loop (let* ((buf (make-octets 65536))
                    (n (read-sequence buf source)))
               (when (zerop n) (return))
               (push (subseq buf 0 n) chunks)
               (incf total n)))
       (let ((v (make-octets total)) (p 0))
         (dolist (c (nreverse chunks) v)
           (replace v c :start1 p)
           (incf p (length c))))))
    (vector (coerce-octets source))))

(defun parse-header (octets)
  (when (< (length octets) 16)
    (format-error "only ~D byte~:P long; a WPG header needs 16" (length octets)))
  (unless (every #'= +wpg-magic+ (subseq octets 0 4))
    (format-error "missing the WordPerfect #xFF \"WPC\" signature"))
  (let* ((r (make-octet-reader octets :start 4))
         (header (make-wpg-header :data-offset (read-u32 r)
                                  :product-type (read-u8 r)
                                  :file-type (read-u8 r)
                                  :major-version (read-u8 r)
                                  :minor-version (read-u8 r)
                                  :encryption-key (read-u16 r)
                                  :reserved (read-u16 r))))
    (unless (= (wpg-header-file-type header) #x16)
      (format-error "a WordPerfect file, but of type ~D, not a graphic (22)"
                    (wpg-header-file-type header)))
    header))

(defun read-wpg-header (source)
  "Read and check just the 16-byte header of SOURCE."
  (parse-header (source-octets source)))

(defun wpg-file-p (source)
  "True if SOURCE starts with a WPG header."
  (handler-case (progn (read-wpg-header source) t)
    (error () nil)))

(defun check-header (header octets)
  (let ((version (wpg-header-major-version header)))
    (unless (member version '(1 2))
      (format-error "unknown WPG version ~D" version))
    (unless (zerop (wpg-header-encryption-key header))
      (format-error "the file is password-protected (key #x~4,'0X)"
                    (wpg-header-encryption-key header)))
    (when (> (wpg-header-data-offset header) (length octets))
      (format-error "data offset ~D is past the end of the ~D-byte file"
                    (wpg-header-data-offset header) (length octets)))
    version))

(defun read-record-header (reader version)
  "Returns (values class type extension length)."
  (if (= version 1)
      (let ((type (read-u8 reader)))
        (values nil type nil (read-varlen reader)))
      (let* ((class (read-u8 reader))
             (type (read-u8 reader))
             (extension (read-varlen reader)))
        (values class type extension (read-varlen reader)))))

(defun decode-record (record version state strict)
  (let ((decoder (record-decoder version (wpg-record-type record))))
    (when decoder
      (setf (parse-state-record-index state) (wpg-record-index record))
      (flet ((decode ()
               (setf (wpg-record-fields record)
                     (funcall decoder (make-octet-reader (wpg-record-body record))
                              state))))
        (if strict
            (decode)
            (handler-case (decode)
              (error (e)
                (setf (wpg-record-error record) e)
                (note-warning state "record ~D (~(~A~)) at offset ~D: ~A"
                              (wpg-record-index record) (wpg-record-name record)
                              (wpg-record-offset record) e))))))))

(defun end-record-p (version type)
  (= type (if (= version 1) #x10 #x02)))

(defun parse-records (octets header state strict)
  (let* ((version (wpg-header-major-version header))
         (reader (make-octet-reader octets :start (wpg-header-data-offset header)))
         (records (make-array 16 :adjustable t :fill-pointer 0))
         ;; WPG2 nesting: (index . children-left) for each open parent.
         ;; Every record counts against the innermost open parent only,
         ;; as in libwpg.
         (open-parents '())
         (ended nil))
    (loop until (reader-at-end-p reader)
          do (let ((start (reader-position reader)))
               ;; libwpg treats a zero type byte in WPG1 as the end of data;
               ;; files padded with zeros after their last record exist.
               (when (and (= version 1)
                          (zerop (aref (reader-octets reader) (reader-position reader))))
                 (return))
               (multiple-value-bind (class type extension length)
                   (handler-case (read-record-header reader version)
                     (wpg-truncated-error (e)
                       (when strict (error e))
                       (note-warning state "truncated record header at offset ~D" start)
                       (return)))
                 (let* ((body-start (reader-position reader))
                        (available (reader-remaining reader))
                        (short (> length available)))
                   (when short
                     (when strict
                       (error 'wpg-truncated-error :position body-start
                                                   :wanted length :available available))
                     (note-warning state "record at offset ~D claims ~D byte~:P but only ~D remain"
                                   start length available))
                   (let ((record (make-wpg-record
                                  :index (fill-pointer records)
                                  :offset start :body-offset body-start
                                  :class class :type type :extension extension
                                  :length length
                                  :body (read-octets reader (min length available))
                                  :name (record-type-name version type))))
                     (when open-parents
                       (setf (wpg-record-parent record) (car (first open-parents))
                             (wpg-record-depth record) (length open-parents))
                       (when (zerop (decf (cdr (first open-parents))))
                         (pop open-parents)))
                     (when (and extension (plusp extension))
                       (push (cons (wpg-record-index record) extension) open-parents))
                     (vector-push-extend record records)
                     (decode-record record version state strict))
                   (when (end-record-p version type)
                     (setf ended t)
                     (return))
                   (when short (return))))))
    (unless ended
      (note-warning state "no End WPG record"))
    (coerce records 'simple-vector)))

(defun infer-size-from-bitmaps (image)
  "A WPG1 file with no Start WPG record: size the drawing to its bitmaps."
  (dolist (b (wpg-image-bitmaps image))
    (let ((w (round (* 1200 (wpg-bitmap-width b)) (max 1 (wpg-bitmap-h-resolution b))))
          (h (round (* 1200 (wpg-bitmap-height b)) (max 1 (wpg-bitmap-v-resolution b)))))
      (setf (wpg-image-width image) (max w (wpg-image-width image))
            (wpg-image-height image) (max h (wpg-image-height image))))))

(defun read-wpg (source &key strict)
  "Read the WPG file SOURCE (a pathname, namestring, binary stream or octet
vector) and return a WPG-IMAGE.

Damage after the header is tolerated by default: a record that fails to
decode keeps its raw body and gets an ERROR, and problems are collected in
WPG-IMAGE-WARNINGS. With STRICT true, the first problem is signalled
instead."
  (let* ((octets (source-octets source))
         (header (parse-header octets))
         (version (check-header header octets))
         (image (make-wpg-image :header header :version version))
         (state (make-parse-state :image image
                                  :palette (if (= version 1)
                                               (default-wpg1-palette)
                                               (default-wpg2-palette)))))
    (setf (wpg-image-records image) (parse-records octets header state strict))
    (setf (wpg-image-bitmaps image) (nreverse (wpg-image-bitmaps image))
          (wpg-image-warnings image) (nreverse (wpg-image-warnings image))
          (wpg-image-palette image) (parse-state-palette state))
    (when (and (= version 1) (zerop (wpg-image-width image)))
      (infer-size-from-bitmaps image))
    image))
