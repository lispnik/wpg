;;;; main.lisp -- the `wpg' command-line program.
;;;;
;;;;   wpg info FILE...                 summarize each file
;;;;   wpg records FILE [--fields] [--hex]
;;;;                                    list the records
;;;;   wpg extract FILE [-o DIR] [-f png|ppm] [--postscript]
;;;;                                    write out the bitmaps
;;;;   wpg svg FILE [-o OUT] [--no-bitmaps]
;;;;                                    convert to SVG

(defpackage #:wordperfect-graphics.cli
  (:use #:cl)
  (:local-nicknames (#:wpg #:wordperfect-graphics))
  (:export #:main #:top-level-command))

(in-package #:wordperfect-graphics.cli)

(defparameter *version* "0.1.0")

;;; Errors

(define-condition usage-error (error)
  ((message :initarg :message :reader usage-error-message))
  (:report (lambda (c s) (write-string (usage-error-message c) s))))

(defun usage-error (control &rest args)
  (error 'usage-error :message (apply #'format nil control args)))

(defun stdout-closed-p (condition)
  "True when CONDITION is a write to a standard output nobody is reading
any more, as in `wpg records x.wpg | head'."
  (and (typep condition 'stream-error)
       (let ((s (stream-error-stream condition)))
         (and (typep s 'sb-sys:fd-stream)
              (eql 1 (sb-sys:fd-stream-fd s))))))

(defun call-reporting-errors (thunk)
  "Run THUNK, turning the errors a user can cause into a message and exit 1."
  (handler-case (funcall thunk)
    (stream-error (e)
      (if (stdout-closed-p e)
          (uiop:quit 0 nil)
          (progn (format *error-output* "wpg: ~A~%" e)
                 (uiop:quit 1))))
    ((or wpg:wpg-error file-error usage-error) (e)
      (format *error-output* "wpg: ~A~%" e)
      (finish-output *error-output*)
      (uiop:quit 1))))

(defmacro with-errors-reported (() &body body)
  `(call-reporting-errors (lambda () ,@body)))

(defun require-arguments (cmd min &optional max)
  (let ((args (clingon:command-arguments cmd)))
    (when (or (< (length args) min) (and max (> (length args) max)))
      (clingon:print-usage cmd *error-output*)
      (usage-error "expected ~:[at least ~D~;~D~] file argument~:P"
                   (eql min max) min))
    args))

(defun native (filename &key directory)
  "FILENAME, as typed on the command line, as a pathname. Native parsing,
so [ ] * and ? in names are ordinary characters, not wildcards."
  ;; SBCL's parser directly: UIOP's :ENSURE-DIRECTORY re-reads the last
  ;; component as a Lisp namestring, which escapes the brackets.
  (merge-pathnames (sb-ext:parse-native-namestring filename nil *default-pathname-defaults*
                                                   :as-directory directory)))

(defun load-image (filename)
  (let ((path (native filename)))
    (unless (probe-file path)
      (usage-error "~A: no such file" filename))
    (handler-case (wpg:read-wpg path)
      (wpg:wpg-error (e) (usage-error "~A: ~A" filename e)))))

;;; Formatting values

(defun fmt-number (v)
  (if (integerp v) (format nil "~D" v) (wpg::svg-num v)))

(defun fmt-color (c)
  (if (= 255 (wpg:color-alpha c))
      (wpg:color-hex c)
      (format nil "~A/~D" (wpg:color-hex c) (wpg:color-alpha c))))

(defun fmt-value (v &key verbose)
  (labels ((fmt (v)
             (typecase v
               (wpg:color (fmt-color v))
               (wpg:wpg-bitmap (format nil "<bitmap ~Dx~D ~D-bit>"
                                       (wpg:wpg-bitmap-width v) (wpg:wpg-bitmap-height v)
                                       (wpg:wpg-bitmap-depth v)))
               (string (format nil "~S" v))
               (number (fmt-number v))
               ((eql t) "yes")
               (null "no")
               (cons (cond ((and (atom (car v)) (atom (cdr v)) (cdr v))
                            (format nil "(~A,~A)" (fmt (car v)) (fmt (cdr v))))
                           ((keywordp (car v))   ; a nested field plist
                            (format nil "{~A}" (fmt-fields v :verbose verbose)))
                           (t (format nil "(~{~A~^ ~})" (mapcar #'fmt v)))))
               (vector (if (or verbose (<= (length v) 4))
                           (format nil "[~{~A~^ ~}]" (map 'list #'fmt v))
                           (format nil "[~D items]" (length v))))
               (t (princ-to-string v)))))
    (fmt v)))

(defun fmt-fields (fields &key verbose)
  (format nil "~{~A~^ ~}"
          (loop for (k v) on fields by #'cddr
                collect (format nil "~(~A~)=~A" k (fmt-value v :verbose verbose)))))

(defun hex-dump (octets stream &key (indent 6))
  (loop for start from 0 below (length octets) by 16
        for end = (min (length octets) (+ start 16))
        do (format stream "~vT~6,'0X  ~{~2,'0X~^ ~}~vT~{~A~}~%"
                   indent start
                   (coerce (subseq octets start end) 'list)
                   (+ indent 58)
                   (map 'list (lambda (b) (if (<= 32 b 126) (code-char b) #\.))
                        (subseq octets start end)))))

;;; info

(defun describe-bitmap (i b stream)
  (format stream "    #~D  ~Dx~D, ~D-bit, ~(~A~), ~Dx~D dpi~@[, at ~{(~A,~A)-(~A,~A)~}~]~:[, INCOMPLETE~;~]~%"
          i (wpg:wpg-bitmap-width b) (wpg:wpg-bitmap-height b) (wpg:wpg-bitmap-depth b)
          (wpg:wpg-bitmap-compression b)
          (wpg:wpg-bitmap-h-resolution b) (wpg:wpg-bitmap-v-resolution b)
          (and (wpg:wpg-bitmap-x1 b)
               (mapcar #'fmt-number (list (wpg:wpg-bitmap-x1 b) (wpg:wpg-bitmap-y1 b)
                                          (wpg:wpg-bitmap-x2 b) (wpg:wpg-bitmap-y2 b))))
          (wpg:wpg-bitmap-complete-p b)))

(defun print-info (path image stream)
  (let* ((header (wpg:wpg-image-header image))
         (records (wpg:wpg-image-records image))
         (unknown (count nil records :key #'wpg:wpg-record-name))
         (failed (count-if #'wpg:wpg-record-error records))
         (bitmaps (wpg:wpg-image-bitmaps image)))
    (format stream "~A: WPG version ~D.~D~%" path
            (wpg:wpg-header-major-version header) (wpg:wpg-header-minor-version header))
    (format stream "  size:     ~A x ~A units (~,2F x ~,2F in at ~D/in)~:[~; double precision~]~%"
            (fmt-number (wpg:wpg-image-width image)) (fmt-number (wpg:wpg-image-height image))
            (float (wpg:wpg-image-width-inches image))
            (float (wpg:wpg-image-height-inches image))
            (wpg:wpg-image-x-units image)
            (wpg:wpg-image-double-precision image))
    (format stream "  records:  ~D (~D of unknown type, ~D failed to decode)~%"
            (length records) unknown failed)
    (let ((types (make-hash-table)))
      (loop for r across records
            do (incf (gethash (or (wpg:wpg-record-name r) :unknown) types 0)))
      (format stream "  kinds:    ~{~A~^, ~}~%"
              (sort (loop for k being the hash-keys of types using (hash-value n)
                          collect (format nil "~(~A~)~[~;~:;×~:*~D~]" k n))
                    #'string<)))
    (format stream "  bitmaps:  ~D~%" (length bitmaps))
    (loop for b in bitmaps for i from 0 do (describe-bitmap i b stream))
    (let ((ps (count-if #'wpg:record-postscript records)))
      (when (plusp ps)
        (format stream "  postscript: ~D embedded block~:P~%" ps)))
    (when (wpg:wpg-image-warnings image)
      (format stream "  warnings:~%~{    ~A~%~}" (wpg:wpg-image-warnings image)))))

(defun info-handler (cmd)
  (with-errors-reported ()
    (let ((paths (require-arguments cmd 1))
          (status 0))
      (dolist (path paths)
        (handler-case (print-info path (load-image path) *standard-output*)
          (usage-error (e)
            (setf status 1)
            (format *error-output* "wpg: ~A~%" e))
          ((or wpg:wpg-error file-error) (e)
            (setf status 1)
            (format *error-output* "wpg: ~A: ~A~%" path e))))
      (unless (zerop status) (uiop:quit status)))))

(defun info-command ()
  (clingon:make-command
   :name "info"
   :description "summarize WPG files: version, size, records, bitmaps"
   :usage "FILE..."
   :handler #'info-handler))

;;; records

(defun records-handler (cmd)
  (with-errors-reported ()
    (let* ((path (first (require-arguments cmd 1 1)))
           (image (load-image path))
           (verbose (clingon:getopt cmd :fields))
           (hex (clingon:getopt cmd :hex))
           (v2 (= 2 (wpg:wpg-image-version image))))
      (format t "~4@A ~8@A ~:[~;cls ~]type ~22A ~7@A  ~A~%"
              "#" "offset" v2 "name" "length" "fields")
      (loop for r across (wpg:wpg-image-records image)
            do (format t "~4D ~8D ~:[~*~;~3D ~]0x~2,'0X ~22A ~7D  ~A~%"
                       (wpg:wpg-record-index r) (wpg:wpg-record-offset r)
                       v2 (wpg:wpg-record-class r)
                       (wpg:wpg-record-type r)
                       (string-downcase (or (wpg:wpg-record-name r) "?"))
                       (wpg:wpg-record-length r)
                       (if (wpg:wpg-record-error r)
                           (format nil "ERROR: ~A" (wpg:wpg-record-error r))
                           (fmt-fields (wpg:wpg-record-fields r) :verbose verbose)))
               (when hex (hex-dump (wpg:wpg-record-body r) *standard-output*)))
      (dolist (w (wpg:wpg-image-warnings image))
        (format *error-output* "warning: ~A~%" w)))))

(defun records-command ()
  (clingon:make-command
   :name "records"
   :description "list every record in a WPG file with its decoded fields"
   :usage "[--fields] [--hex] FILE"
   :options (list (clingon:make-option :flag :long-name "fields" :short-name #\v :key :fields
                                       :description "show every element of point lists and palettes")
                  (clingon:make-option :flag :long-name "hex" :short-name #\x :key :hex
                                       :description "hex-dump each record's body"))
   :handler #'records-handler))

;;; extract

(defun output-directory (dir)
  (let ((d (if dir (native dir :directory t) (uiop:getcwd))))
    (ensure-directories-exist d)
    d))

(defun extract-handler (cmd)
  (with-errors-reported ()
    (let* ((path (first (require-arguments cmd 1 1)))
           (image (load-image path))
           (dir (output-directory (clingon:getopt cmd :output)))
           (format (clingon:getopt cmd :format))
           (base (pathname-name (native path)))
           (written 0))
      (loop for b in (wpg:wpg-image-bitmaps image)
            for i from 0
            do (let ((out (make-pathname :name (format nil "~A-~D" base i)
                                         :type (string-downcase format) :defaults dir)))
                 (let ((oriented (not (clingon:getopt cmd :raw))))
                   (ecase format
                     (:png (wpg:write-bitmap-png b out :oriented oriented))
                     (:ppm (wpg:write-bitmap-ppm b out :oriented oriented))))
                 (incf written)
                 (format t "~A (~Dx~D, ~D-bit)~%" (uiop:native-namestring out)
                         (wpg:wpg-bitmap-width b) (wpg:wpg-bitmap-height b)
                         (wpg:wpg-bitmap-depth b))))
      (when (clingon:getopt cmd :postscript)
        (loop for r across (wpg:wpg-image-records image)
              for ps = (wpg:record-postscript r)
              with i = 0
              when ps
                do (let ((out (make-pathname :name (format nil "~A-ps-~D" base i)
                                             :type "eps" :defaults dir)))
                     (with-open-file (s out :direction :output :if-exists :supersede
                                            :element-type '(unsigned-byte 8))
                       (write-sequence ps s))
                     (incf i) (incf written)
                     (format t "~A (~D bytes of PostScript)~%"
                             (uiop:native-namestring out) (length ps)))))
      (when (zerop written)
        (format *error-output* "wpg: ~A: nothing to extract~%" path)))))

(defun extract-command ()
  (clingon:make-command
   :name "extract"
   :description "write each bitmap in a WPG file to its own image file"
   :usage "[-o DIR] [-f png|ppm] [--postscript] [--raw] FILE"
   :options (list (clingon:make-option :string :long-name "output" :short-name #\o :key :output
                                       :description "directory to write into (default: current)")
                  (clingon:make-option :enum :long-name "format" :short-name #\f :key :format
                                       :items '(("png" . :png) ("ppm" . :ppm))
                                       :initial-value "png"
                                       :description "image format: png or ppm")
                  (clingon:make-option :flag :long-name "postscript" :short-name #\p :key :postscript
                                       :description "also write embedded PostScript as .eps")
                  (clingon:make-option :flag :long-name "raw" :key :raw
                                       :description "keep bitmaps as stored, without the file's mirroring and rotation"))
   :handler #'extract-handler))

;;; svg

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun base64 (octets)
  (with-output-to-string (s)
    (loop for i from 0 below (length octets) by 3
          do (let* ((n (min 3 (- (length octets) i)))
                    (w (logior (ash (aref octets i) 16)
                               (if (> n 1) (ash (aref octets (+ i 1)) 8) 0)
                               (if (> n 2) (aref octets (+ i 2)) 0))))
               (dotimes (j 4)
                 (write-char (if (<= j n)
                                 (char +base64-alphabet+ (ldb (byte 6 (- 18 (* 6 j))) w))
                                 #\=)
                             s))))))

(defun png-data-uri (bitmap)
  (concatenate 'string "data:image/png;base64," (base64 (wpg:bitmap-png-octets bitmap))))

(defun svg-handler (cmd)
  (with-errors-reported ()
    (let* ((path (first (require-arguments cmd 1 1)))
           (image (load-image path))
           (output (clingon:getopt cmd :output))
           (out (cond ((equal output "-") "-")
                      (output (native output))
                      (t (make-pathname :type "svg" :defaults (native path)))))
           (href (unless (clingon:getopt cmd :no-bitmaps) #'png-data-uri)))
      (if (equal out "-")
          (wpg:write-svg image *standard-output* :bitmap-href href)
          (progn
            (with-open-file (s out :direction :output :if-exists :supersede
                                   :external-format :utf-8)
              (wpg:write-svg image s :bitmap-href href))
            (format t "~A~%" (uiop:native-namestring out))))
      (dolist (w (wpg:wpg-image-warnings image))
        (format *error-output* "warning: ~A~%" w)))))

(defun svg-command ()
  (clingon:make-command
   :name "svg"
   :description "convert a WPG file to SVG (bitmaps embedded as PNG)"
   :usage "[-o OUT] [--no-bitmaps] FILE"
   :options (list (clingon:make-option :string :long-name "output" :short-name #\o :key :output
                                       :description "output file, or - for stdout (default: FILE.svg)")
                  (clingon:make-option :flag :long-name "no-bitmaps" :key :no-bitmaps
                                       :description "leave bitmaps out instead of embedding them"))
   :handler #'svg-handler))

;;; Top level

(defun top-level-command ()
  (clingon:make-command
   :name "wpg"
   :version *version*
   :description "inspect and convert WordPerfect Graphics (.wpg) files"
   :usage "COMMAND [OPTIONS] FILE..."
   :handler (lambda (cmd)
              (let ((args (clingon:command-arguments cmd)))
                (when args
                  (format *error-output* "wpg: unknown command ~S~%~%" (first args))
                  (clingon:print-usage cmd *error-output*)
                  (uiop:quit 64))
                (clingon:print-usage-and-exit cmd t)))
   :sub-commands (list (info-command) (records-command) (extract-command) (svg-command))))

(defun main ()
  (handler-case
      (clingon:run (top-level-command))
    (sb-sys:interactive-interrupt ()
      (uiop:quit 130))
    (error (e)
      (format *error-output* "wpg: internal error: ~A~%" e)
      (uiop:quit 2))))
