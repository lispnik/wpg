;;;; wptext.lisp -- text embedded in WPG files as WordPerfect document streams.
;;;;
;;;; WPG1 Graphics Text (Type 2) records hold WordPerfect 5 text, and WPG2
;;;; Text Data records hold WordPerfect 6 text. Both are a stream of
;;;; characters mixed with formatting codes. This extracts the characters,
;;;; and for WP6 the font size, color and typeface changes, following
;;;; libwpd. Everything else (tabs, tables, margins...) is skipped.

(in-package #:wordperfect-graphics)

(defparameter +wp5-fixed-group-sizes+
  #(4 9 11 3 3 5 6 7 4 5 6 6 8 10 10 12)
  "Total sizes of the WP5 fixed-length codes #xC0-#xCF.")

(defparameter +wp6-fixed-group-sizes+
  #(4 5 3 3 3 3 4 4 4 5 5 6 6 8 8)
  "Total sizes of the WP6 fixed-length codes #xF0-#xFE.")

(defun octet-u16 (octets i)
  (if (< (1+ i) (length octets))
      (logior (aref octets i) (ash (aref octets (1+ i)) 8))
      0))

(defun decode-wp5-text (octets)
  "The characters of a WordPerfect 5 text stream, as a string. Hard returns
become newlines."
  (let ((octets (coerce-octets octets))
        (i 0))
    (with-output-to-string (out)
      (loop while (< i (length octets))
            do (let ((b (aref octets i)))
                 (cond ((or (= b 0) (= b #x7f) (= b #xff)) (incf i))
                       ((< b #x20)
                        (case b
                          (#x0a (write-char #\Newline out))
                          (#x0d (write-char #\Space out)))
                        (incf i))
                       ((< b #x7f) (write-char (code-char b) out) (incf i))
                       ((< b #xc0)
                        (case b
                          ((#x8c #x90 #x99) (write-char #\Newline out))
                          ((#x93 #x94 #x95 #xa0) (write-char #\Space out))
                          ((#xa9 #xaa #xab) (write-char #\- out)))
                        (incf i))
                       ((< b #xd0)
                        (when (and (= b #xc0) (< (+ i 2) (length octets)))
                          (write-char (wp-character (aref octets (+ i 2)) (aref octets (+ i 1))
                                                    :version 5)
                                      out))
                        (incf i (aref +wp5-fixed-group-sizes+ (- b #xc0))))
                       (t
                        ;; code, subgroup, length, LENGTH bytes ending in code.
                        (incf i (+ 4 (octet-u16 octets (+ i 2)))))))))))

(defun utf16-name (octets start end)
  "The first run of three or more printable UTF-16LE characters between
START and END: how a font descriptor packet stores the typeface name."
  (loop for i from start below (1- end)
        do (let ((n (loop for j from i below (1- end) by 2
                          while (and (<= #x20 (aref octets j) #x7e) (zerop (aref octets (1+ j))))
                          count t)))
             (when (>= n 3)
               (return (map 'string (lambda (k) (code-char (aref octets (+ i (* 2 k)))))
                            (loop for k below n collect k)))))))

(defun decode-wp6-text (octets)
  "The text of a WordPerfect 6 stream as a list of runs, each a plist:
  :TEXT   the characters; hard returns are newlines
  :SIZE   font size in points, or NIL if the stream never sets one
  :COLOR  a COLOR, or NIL
  :FONT   the typeface name, or NIL
A new run starts wherever the size, color or typeface changes."
  (let ((octets (coerce-octets octets))
        (i 0)
        (runs '())
        (text (make-string-output-stream))
        (size nil) (color nil) (font nil))
    (labels ((flush ()
               (let ((s (get-output-stream-string text)))
                 (when (plusp (length s))
                   (push (list :text s :size size :color color :font font) runs))))
             (change (&key (new-size size) (new-color color) (new-font font))
               (unless (and (eql new-size size) (equalp new-color color) (equal new-font font))
                 (flush)
                 (setf size new-size color new-color font new-font)))
             (emit (ch) (write-char ch text))
             (variable-group (start code)
               ;; code, subgroup, size (whole group), flags, [prefix ids],
               ;; non-deletable size, data ..., size, code
               (let* ((subgroup (aref octets (+ start 1)))
                      (total (octet-u16 octets (+ start 2)))
                      (flags (if (< (+ start 4) (length octets)) (aref octets (+ start 4)) 0))
                      (data (+ start 5))
                      (end (min (length octets) (+ start (max total 1)))))
                 (when (logbitp 7 flags)
                   (incf data (+ 1 (* 2 (if (< data (length octets)) (aref octets data) 0)))))
                 (incf data 2)          ; non-deletable size
                 (when (and (= code #xd4) (< data end))
                   (case subgroup
                     (#x18 (when (< (+ data 2) end)
                             (change :new-color (make-color (aref octets data)
                                                            (aref octets (+ data 1))
                                                            (aref octets (+ data 2))))))
                     ;; Face: old size, hash, font index, matched size (1/50 pt).
                     (#x1a (let ((matched (octet-u16 octets (+ data 6))))
                             (change :new-size (if (plusp matched) (/ matched 50) size)
                                     :new-font (or (utf16-name octets (+ data 8) end) font))))
                     (#x1b (let ((desired (octet-u16 octets data)))
                             (when (plusp desired)
                               (change :new-size (/ desired 50)))))))
                 (max total 1))))
      (loop while (< i (length octets))
            do (let ((b (aref octets i)))
                 (cond ((= b 0) (incf i))
                       ((<= b #x20) (emit (code-char (aref +wp6-low-characters+ (1- b)))) (incf i))
                       ((<= b #x7f) (emit (code-char b)) (incf i))
                       ((<= b #xcf)
                        (case b
                          ((#x80 #x81 #xcd #xce #xcf) (emit #\Space))
                          (#x84 (emit #\-))
                          ((#x87 #xb7 #xb8 #xb9 #xca #xcb #xcc
                            #xb4 #xb5 #xb6 #xc7 #xc8 #xc9) (emit #\Newline)))
                        (incf i))
                       ((<= b #xef) (incf i (variable-group i b)))
                       ((<= b #xfe)
                        (when (and (= b #xf0) (< (+ i 2) (length octets)))
                          (emit (wp-character (aref octets (+ i 2)) (aref octets (+ i 1)))))
                        (incf i (aref +wp6-fixed-group-sizes+ (- b #xf0))))
                       (t (incf i)))))
      (flush)
      (nreverse runs))))
