;;;; conditions.lisp -- everything the reader signals.

(in-package #:wordperfect-graphics)

(define-condition wpg-error (error)
  ()
  (:documentation "Superclass of every error signalled by this library."))

(define-condition wpg-format-error (wpg-error simple-condition)
  ()
  (:documentation "The input is not a WPG file, or is one this library cannot read.")
  (:report (lambda (c s)
             (format s "Not a readable WPG file: ~?"
                     (simple-condition-format-control c)
                     (simple-condition-format-arguments c)))))

(define-condition wpg-truncated-error (wpg-error)
  ((position  :initarg :position  :reader wpg-truncated-error-position)
   (wanted    :initarg :wanted    :reader wpg-truncated-error-wanted)
   (available :initarg :available :reader wpg-truncated-error-available))
  (:documentation "A read ran past the end of the data it was given.")
  (:report (lambda (c s)
             (format s "Unexpected end of WPG data at offset ~D: wanted ~D byte~:P, ~D available"
                     (wpg-truncated-error-position c)
                     (wpg-truncated-error-wanted c)
                     (wpg-truncated-error-available c)))))

(define-condition wpg-decode-error (wpg-error simple-condition)
  ()
  (:documentation "A record's contents are malformed or use an unsupported feature.")
  (:report (lambda (c s)
             (format s "WPG decode error: ~?"
                     (simple-condition-format-control c)
                     (simple-condition-format-arguments c)))))

(define-condition wpg-warning (warning simple-condition)
  ()
  (:documentation "Something recoverable was wrong with the file."))

(defun format-error (control &rest args)
  (error 'wpg-format-error :format-control control :format-arguments args))

(defun decode-error (control &rest args)
  (error 'wpg-decode-error :format-control control :format-arguments args))
