;;;; records.lisp -- the record-type registry and the state a parse carries.

(in-package #:wordperfect-graphics)

(defvar *record-names* (make-hash-table :test 'equal)
  "(version . type) -> keyword name.")

(defvar *record-decoders* (make-hash-table :test 'equal)
  "(version . type) -> function of (reader state) returning a field plist.")

(defstruct parse-state
  image
  palette
  (palette-loaded-p nil)
  (record-index 0)
  ;; WPG2: the placement read from a Bitmap object record, waiting for the
  ;; Bitmap Data record that follows it.
  (pending-bitmap nil))

(defun record-type-name (version type)
  "The keyword naming record TYPE in a WPG file of VERSION (1 or 2), or NIL."
  (values (gethash (cons version type) *record-names*)))

(defmacro define-record-names (version &body pairs)
  "Name record types whose bodies are kept raw."
  `(progn
     ,@(loop for (type name) in pairs
             collect `(setf (gethash (cons ,version ,type) *record-names*) ,name))))

(defmacro define-record ((version type name) (reader state) &body body)
  "Define how to decode the body of record TYPE in WPG VERSION. BODY runs
with READER over the record's data and STATE the parse state, and returns
the record's field plist."
  `(progn
     (setf (gethash (cons ,version ,type) *record-names*) ,name)
     (setf (gethash (cons ,version ,type) *record-decoders*)
           (lambda (,reader ,state)
             (declare (ignorable ,reader ,state))
             ,@body))
     ,name))

(defun record-decoder (version type)
  (values (gethash (cons version type) *record-decoders*)))

(defun note-warning (state control &rest args)
  (push (apply #'format nil control args)
        (wpg-image-warnings (parse-state-image state))))

(defun bitmap-palette (state depth version)
  "The palette a new bitmap of DEPTH should use, given what the file has
said so far."
  (cond ((> depth 8) nil)
        ((parse-state-palette-loaded-p state)
         (let ((p (copy-palette (parse-state-palette state))))
           ;; A monochrome bitmap whose two colors are both black is a
           ;; broken palette that real files contain; make 1 white.
           (when (and (= depth 1)
                      (color= (aref p 0) (make-color 0 0 0))
                      (color= (aref p 1) (make-color 0 0 0)))
             (setf (aref p 1) (make-color 255 255 255)))
           p))
        ;; WPG1 monochrome without a palette is black and white.
        ((and (= version 1) (= depth 1))
         (grayscale-palette 1))
        (t (copy-palette (parse-state-palette state)))))

(defun add-bitmap (state bitmap)
  (setf (wpg-bitmap-record-index bitmap) (parse-state-record-index state))
  (push bitmap (wpg-image-bitmaps (parse-state-image state)))
  (unless (wpg-bitmap-complete-p bitmap)
    (note-warning state "record ~D: bitmap data ends before the last row"
                  (parse-state-record-index state)))
  bitmap)

(defun read-palette-entries (reader count read-entry)
  (let ((v (make-array count)))
    (dotimes (i count v)
      (setf (aref v i) (funcall read-entry reader)))))

(defun store-palette (state start entries)
  (let ((palette (parse-state-palette state)))
    (loop for i from start
          for c across entries
          when (< i (length palette))
            do (setf (aref palette i) c))
    (setf (parse-state-palette-loaded-p state) t)))
