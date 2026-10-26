;;;; main.lisp -- the `wpg' command-line program.
;;;;
;;;;   wpg info FILE|DIR... [--json] [-r]
;;;;                                    summarize each file
;;;;   wpg records FILE [--fields] [--hex] [--json]
;;;;                                    list the records
;;;;   wpg extract FILE|DIR... [-o DIR] [-f png|ppm] [--postscript] [--raw] [-r]
;;;;                                    write out the bitmaps
;;;;   wpg svg FILE|DIR... [-o OUT] [--no-bitmaps] [-r]
;;;;                                    convert to SVG
;;;;   wpg png FILE|DIR... [-o OUT] [-w PX] [--dpi N] [--transparent] [-r]
;;;;                                    render to PNG

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

;;; Inputs and outputs
;;;
;;; Commands that take FILE... also take directories: their *.wpg files,
;;; and with -r those of every subdirectory. Each input remembers the
;;; subdirectory it was found in, so output written to -o DIR mirrors the
;;; input tree and same-named files cannot collide.

(defstruct input
  path            ; the file
  name            ; how to refer to it in messages
  (subdirs '()))  ; directories between the argument and the file

(defun wpg-pathname-p (p)
  (and (pathname-name p) (pathname-type p) (string-equal (pathname-type p) "wpg")))

(defun expand-argument (arg recursive)
  (let* ((file (native arg))
         (dir (native arg :directory t)))
    (cond ((and (pathname-name file) (probe-file file)
                (not (uiop:directory-exists-p dir)))
           (list (make-input :path file :name arg)))
          ((uiop:directory-exists-p dir)
           (labels ((walk (d subdirs)
                      (append
                       (loop for f in (sort (uiop:directory-files d) #'string< :key #'namestring)
                             when (wpg-pathname-p f)
                               collect (make-input :path f :name (uiop:native-namestring f)
                                                   :subdirs subdirs))
                       (when recursive
                         (loop for sub in (sort (uiop:subdirectories d) #'string< :key #'namestring)
                               append (walk sub (append subdirs
                                                        (last (pathname-directory sub)))))))))
             (walk dir '())))
          (t (usage-error "~A: no such file or directory" arg)))))

(defun command-inputs (cmd)
  (let ((args (require-arguments cmd 1)))
    (loop for arg in args
          append (expand-argument arg (clingon:getopt cmd :recursive)))))

(defun output-path (input type output &key multiple)
  "Where to write INPUT converted to TYPE. OUTPUT is the -o value: NIL
(next to the input), a directory, or (for a single input) a file."
  (let ((name (pathname-name (input-path input))))
    (cond ((null output) (make-pathname :type type :defaults (input-path input)))
          ((or multiple (uiop:directory-exists-p (native output :directory t))
               (let ((s (string-right-trim " " output)))
                 (and (plusp (length s)) (char= #\/ (char s (1- (length s)))))))
           (let ((dir (merge-pathnames
                       (make-pathname :directory (cons :relative (input-subdirs input)))
                       (native output :directory t))))
             (ensure-directories-exist dir)
             (make-pathname :name name :type type :defaults dir)))
          (t (native output)))))

(defun for-each-input (cmd function)
  "Call FUNCTION on each input of CMD. A failure is reported and the rest
carry on; the exit status is 1 if anything failed."
  (let ((failed 0) (inputs (command-inputs cmd)))
    (dolist (input inputs)
      (handler-case (funcall function input (wpg:read-wpg (input-path input)))
        ((or wpg:wpg-error file-error usage-error) (e)
          (incf failed)
          (format *error-output* "wpg: ~A: ~A~%" (input-name input) e))))
    (when (and (> (length inputs) 1) (plusp failed))
      (format *error-output* "wpg: ~D of ~D file~:P failed~%" failed (length inputs)))
    (unless (zerop failed) (uiop:quit 1))))

(defun recursive-option ()
  (clingon:make-option :flag :long-name "recursive" :short-name #\r :key :recursive
                       :description "with a directory, also convert the files in its subdirectories"))

(defun warn-about (input image)
  (dolist (w (wpg:wpg-image-warnings image))
    (format *error-output* "warning: ~A: ~A~%" (input-name input) w)))

;;; JSON (written by hand: the CLI needs very little of it)

(defun json-key (keyword)
  (substitute #\_ #\- (string-downcase (string keyword))))

(defun write-json-string (string stream)
  (write-char #\" stream)
  (loop for ch across string
        do (case ch
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Tab (write-string "\\t" stream))
             (t (if (< (char-code ch) 32)
                    (format stream "\\u~4,'0X" (char-code ch))
                    (write-char ch stream)))))
  (write-char #\" stream))

(defun plist-p (v)
  (and (consp v) (keywordp (car v)) (evenp (length v))
       (loop for k in v by #'cddr always (keywordp k))))

(defun write-json (v stream)
  "Write V as JSON: plists become objects, other lists and vectors arrays,
(x . y) points two-element arrays, colors \"#rrggbb\" (or \"#rrggbbaa\"),
bitmaps a summary object, T true, :FALSE false and NIL null."
  (typecase v
    (null (write-string "null" stream))
    ((eql t) (write-string "true" stream))
    ((eql :false) (write-string "false" stream))
    (keyword (write-json-string (string-downcase (string v)) stream))
    (string (write-json-string v stream))
    (integer (format stream "~D" v))
    (real (write-string (wpg::svg-num v) stream))
    (wpg:color (write-json-string
                (if (= 255 (wpg:color-alpha v))
                    (wpg:color-hex v)
                    (format nil "~A~(~2,'0x~)" (wpg:color-hex v) (wpg:color-alpha v)))
                stream))
    (wpg:wpg-bitmap (write-json (bitmap-summary v) stream))
    (cons (cond ((plist-p v)
                 (write-char #\{ stream)
                 (loop for (k val) on v by #'cddr
                       for first = t then nil
                       do (unless first (write-char #\, stream))
                          (write-json-string (json-key k) stream)
                          (write-char #\: stream)
                          (write-json val stream))
                 (write-char #\} stream))
                ((and (realp (car v)) (realp (cdr v)))
                 (write-json (list (car v) (cdr v)) stream))
                (t (write-json (coerce v 'vector) stream))))
    (vector (write-char #\[ stream)
     (loop for x across v
           for first = t then nil
           do (unless first (write-char #\, stream))
              (write-json x stream))
     (write-char #\] stream))
    (t (write-json-string (princ-to-string v) stream))))

(defun bitmap-summary (b)
  (list :width (wpg:wpg-bitmap-width b) :height (wpg:wpg-bitmap-height b)
        :depth (wpg:wpg-bitmap-depth b)
        :format (wpg:wpg-bitmap-format b) :compression (wpg:wpg-bitmap-compression b)
        :h-resolution (wpg:wpg-bitmap-h-resolution b) :v-resolution (wpg:wpg-bitmap-v-resolution b)
        :placement (and (wpg:wpg-bitmap-x1 b)
                        (list (wpg:wpg-bitmap-x1 b) (wpg:wpg-bitmap-y1 b)
                              (wpg:wpg-bitmap-x2 b) (wpg:wpg-bitmap-y2 b)))
        :complete (if (wpg:wpg-bitmap-complete-p b) t :false)))

;;; info

(defun record-kinds (image)
  (let ((types (make-hash-table)))
    (loop for r across (wpg:wpg-image-records image)
          do (incf (gethash (or (wpg:wpg-record-name r) :unknown) types 0)))
    (sort (loop for k being the hash-keys of types using (hash-value n) collect (cons k n))
          #'string< :key (lambda (c) (string (car c))))))

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
    (format stream "~A: WPG version ~D.~D~@[ (inside an ~A container)~]~%" path
            (wpg:wpg-header-major-version header) (wpg:wpg-header-minor-version header)
            (and (wpg:wpg-image-container image)
                 (string-upcase (wpg:wpg-image-container image))))
    (format stream "  size:     ~A x ~A units (~,2F x ~,2F in at ~D/in)~:[~; double precision~]~%"
            (fmt-number (wpg:wpg-image-width image)) (fmt-number (wpg:wpg-image-height image))
            (float (wpg:wpg-image-width-inches image))
            (float (wpg:wpg-image-height-inches image))
            (wpg:wpg-image-x-units image)
            (wpg:wpg-image-double-precision image))
    (format stream "  records:  ~D (~D of unknown type, ~D failed to decode)~%"
            (length records) unknown failed)
    (format stream "  kinds:    ~{~A~^, ~}~%"
            (loop for (k . n) in (record-kinds image)
                  collect (format nil "~(~A~)~[~;~:;×~:*~D~]" k n)))
    (format stream "  bitmaps:  ~D~%" (length bitmaps))
    (loop for b in bitmaps for i from 0 do (describe-bitmap i b stream))
    (let ((ps (count-if #'wpg:record-postscript records)))
      (when (plusp ps)
        (format stream "  postscript: ~D embedded block~:P~%" ps)))
    (when (wpg:wpg-image-warnings image)
      (format stream "  warnings:~%~{    ~A~%~}" (wpg:wpg-image-warnings image)))))

(defun info-json (path image)
  (let* ((header (wpg:wpg-image-header image))
         (records (wpg:wpg-image-records image)))
    (list :file path
          :version (wpg:wpg-header-major-version header)
          :minor-version (wpg:wpg-header-minor-version header)
          :container (wpg:wpg-image-container image)
          :width (wpg:wpg-image-width image) :height (wpg:wpg-image-height image)
          :x-units (wpg:wpg-image-x-units image) :y-units (wpg:wpg-image-y-units image)
          :width-inches (float (wpg:wpg-image-width-inches image))
          :height-inches (float (wpg:wpg-image-height-inches image))
          :records (length records)
          :unknown-records (count nil records :key #'wpg:wpg-record-name)
          :failed-records (count-if #'wpg:wpg-record-error records)
          :kinds (loop for (k . n) in (record-kinds image) append (list k n))
          :bitmaps (coerce (wpg:wpg-image-bitmaps image) 'vector)
          :postscript-blocks (count-if #'wpg:record-postscript records)
          :warnings (coerce (wpg:wpg-image-warnings image) 'vector))))

(defun info-handler (cmd)
  (with-errors-reported ()
    (let ((json (clingon:getopt cmd :json)))
      (for-each-input cmd (lambda (input image)
                            (if json
                                (progn (write-json (info-json (input-name input) image) *standard-output*)
                                       (terpri))
                                (print-info (input-name input) image *standard-output*)))))))

(defun info-command ()
  (clingon:make-command
   :name "info"
   :description "summarize WPG files: version, size, records, bitmaps"
   :usage "[--json] [-r] FILE|DIR..."
   :options (list (clingon:make-option :flag :long-name "json" :key :json
                                       :description "one JSON object per file, one per line")
                  (recursive-option))
   :handler #'info-handler))

;;; records

(defun record-json (r)
  (list :index (wpg:wpg-record-index r) :offset (wpg:wpg-record-offset r)
        :class (wpg:wpg-record-class r) :type (wpg:wpg-record-type r)
        :name (wpg:wpg-record-name r) :length (wpg:wpg-record-length r)
        :parent (wpg:wpg-record-parent r) :depth (wpg:wpg-record-depth r)
        :fields (or (wpg:wpg-record-fields r) (vector))
        :error (and (wpg:wpg-record-error r) (princ-to-string (wpg:wpg-record-error r)))))

(defun records-handler (cmd)
  (with-errors-reported ()
    (let* ((path (first (require-arguments cmd 1 1)))
           (image (load-image path))
           (verbose (clingon:getopt cmd :fields))
           (hex (clingon:getopt cmd :hex))
           (v2 (= 2 (wpg:wpg-image-version image))))
      (if (clingon:getopt cmd :json)
          (progn
            (write-json (list :file path :version (wpg:wpg-image-version image)
                              :records (map 'vector #'record-json (wpg:wpg-image-records image))
                              :warnings (coerce (wpg:wpg-image-warnings image) 'vector))
                        *standard-output*)
            (terpri))
          (progn
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
              (format *error-output* "warning: ~A~%" w)))))))

(defun records-command ()
  (clingon:make-command
   :name "records"
   :description "list every record in a WPG file with its decoded fields"
   :usage "[--fields] [--hex] [--json] FILE"
   :options (list (clingon:make-option :flag :long-name "fields" :short-name #\v :key :fields
                                       :description "show every element of point lists and palettes")
                  (clingon:make-option :flag :long-name "hex" :short-name #\x :key :hex
                                       :description "hex-dump each record's body")
                  (clingon:make-option :flag :long-name "json" :key :json
                                       :description "write the records as one JSON document"))
   :handler #'records-handler))

;;; extract

(defun extract-handler (cmd)
  (with-errors-reported ()
    (let* ((format (clingon:getopt cmd :format))
           (oriented (not (clingon:getopt cmd :raw)))
           (output (or (clingon:getopt cmd :output) ".")))
      (for-each-input
       cmd
       (lambda (input image)
         (let ((base (pathname-name (input-path input)))
               (written 0))
           (flet ((out (suffix type)
                    (let ((p (output-path input type output :multiple t)))
                      (make-pathname :name (format nil "~A-~A" base suffix) :defaults p))))
             (loop for b in (wpg:wpg-image-bitmaps image)
                   for i from 0
                   do (let ((path (out i (string-downcase format))))
                        (ecase format
                          (:png (wpg:write-bitmap-png b path :oriented oriented))
                          (:ppm (wpg:write-bitmap-ppm b path :oriented oriented)))
                        (incf written)
                        (format t "~A (~Dx~D, ~D-bit)~%" (uiop:native-namestring path)
                                (wpg:wpg-bitmap-width b) (wpg:wpg-bitmap-height b)
                                (wpg:wpg-bitmap-depth b))))
             (when (clingon:getopt cmd :postscript)
               (loop for r across (wpg:wpg-image-records image)
                     for ps = (wpg:record-postscript r)
                     with i = 0
                     when ps
                       do (let ((path (out (format nil "ps-~D" i) "eps")))
                            (with-open-file (s path :direction :output :if-exists :supersede
                                                    :element-type '(unsigned-byte 8))
                              (write-sequence ps s))
                            (incf i) (incf written)
                            (format t "~A (~D bytes of PostScript)~%"
                                    (uiop:native-namestring path) (length ps))))))
           (when (zerop written)
             (format *error-output* "wpg: ~A: nothing to extract~%" (input-name input)))))))))

(defun extract-command ()
  (clingon:make-command
   :name "extract"
   :description "write each bitmap in WPG files to its own image file"
   :usage "[-o DIR] [-f png|ppm] [--postscript] [--raw] [-r] FILE|DIR..."
   :options (list (clingon:make-option :string :long-name "output" :short-name #\o :key :output
                                       :description "directory to write into (default: current)")
                  (clingon:make-option :enum :long-name "format" :short-name #\f :key :format
                                       :items '(("png" . :png) ("ppm" . :ppm))
                                       :initial-value "png"
                                       :description "image format: png or ppm")
                  (clingon:make-option :flag :long-name "postscript" :short-name #\p :key :postscript
                                       :description "also write embedded PostScript as .eps")
                  (clingon:make-option :flag :long-name "raw" :key :raw
                                       :description "keep bitmaps as stored, without the file's mirroring and rotation")
                  (recursive-option))
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

(defun single-output-p (cmd)
  "True if -o names one output file for one input."
  (let ((output (clingon:getopt cmd :output)))
    (and output (not (uiop:directory-exists-p (native output :directory t))))))

(defun svg-handler (cmd)
  (with-errors-reported ()
    (let* ((output (clingon:getopt cmd :output))
           (href (unless (clingon:getopt cmd :no-bitmaps) #'png-data-uri))
           (inputs (command-inputs cmd)))
      (when (and (equal output "-") (> (length inputs) 1))
        (usage-error "-o - (standard output) takes a single input file"))
      (for-each-input
       cmd
       (lambda (input image)
         (if (equal output "-")
             (wpg:write-svg image *standard-output* :bitmap-href href)
             (let ((out (output-path input "svg" output :multiple (> (length inputs) 1))))
               (with-open-file (s out :direction :output :if-exists :supersede
                                      :external-format :utf-8)
                 (wpg:write-svg image s :bitmap-href href))
               (format t "~A~%" (uiop:native-namestring out))))
         (warn-about input image))))))

(defun svg-command ()
  (clingon:make-command
   :name "svg"
   :description "convert WPG files to SVG (bitmaps embedded as PNG)"
   :usage "[-o OUT] [--no-bitmaps] [-r] FILE|DIR..."
   :options (list (clingon:make-option :string :long-name "output" :short-name #\o :key :output
                                       :description "output file, directory, or - for stdout (default: next to each input)")
                  (clingon:make-option :flag :long-name "no-bitmaps" :key :no-bitmaps
                                       :description "leave bitmaps out instead of embedding them")
                  (recursive-option))
   :handler #'svg-handler))

;;; png

(defun png-handler (cmd)
  (with-errors-reported ()
    (let* ((output (clingon:getopt cmd :output))
           (width (clingon:getopt cmd :width))
           (height (clingon:getopt cmd :height))
           (dpi (clingon:getopt cmd :dpi))
           (inputs (command-inputs cmd)))
      (for-each-input
       cmd
       (lambda (input image)
         (let ((out (output-path input "png" output :multiple (> (length inputs) 1))))
           (wpg:render-png image out :width width :height height :dpi dpi
                                     :background (unless (clingon:getopt cmd :transparent)
                                                   (wpg:make-color 255 255 255)))
           (format t "~A~%" (uiop:native-namestring out)))
         (warn-about input image))))))

(defun png-command ()
  (clingon:make-command
   :name "png"
   :description "render WPG files to PNG images"
   :usage "[-o OUT] [-w PIXELS] [--height PIXELS] [--dpi N] [--transparent] [-r] FILE|DIR..."
   :options (list (clingon:make-option :string :long-name "output" :short-name #\o :key :output
                                       :description "output file or directory (default: next to each input)")
                  (clingon:make-option :integer :long-name "width" :short-name #\w :key :width
                                       :description "image width in pixels")
                  (clingon:make-option :integer :long-name "height" :key :height
                                       :description "image height in pixels")
                  (clingon:make-option :integer :long-name "dpi" :key :dpi :initial-value 96
                                       :description "pixels per inch, when no size is given (default 96)")
                  (clingon:make-option :flag :long-name "transparent" :key :transparent
                                       :description "leave the background transparent instead of white")
                  (recursive-option))
   :handler #'png-handler))

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
   :sub-commands (list (info-command) (records-command) (extract-command) (svg-command) (png-command))))

(defun main ()
  (handler-case
      (clingon:run (top-level-command))
    (sb-sys:interactive-interrupt ()
      (uiop:quit 130))
    (error (e)
      (format *error-output* "wpg: internal error: ~A~%" e)
      (uiop:quit 2))))
