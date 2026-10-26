;;;; text-tests.lisp -- WordPerfect text streams, text records and their SVG.

(in-package #:wordperfect-graphics/tests)

(def-suite text :in wordperfect-graphics)
(in-suite text)

(test character-sets
  (is (char= #\A (wpg::wp-character 0 #x41)))
  (is (char= (code-char #xe9) (wpg::wp-character 1 41)))      ; é
  (is (char= (code-char #x2022) (wpg::wp-character 4 3)))     ; bullet
  (is (char= #\? (wpg::wp-character 12 1))))

(test wp5-text
  (is (equal "Hi" (wpg:decode-wp5-text (bytes "Hi"))))
  ;; Extended character (C0 index set C0), hard return, hard space.
  (is (equal (format nil "caf~C~%a b" (code-char #xe9))
             (wpg:decode-wp5-text (bytes "caf" #xc0 41 1 #xc0 #x0a "a" #xa0 "b"))))
  ;; A variable-length group is skipped: code, subgroup, length, body.
  (is (equal "xy" (wpg:decode-wp5-text (bytes "x" #xd1 #x02 (u16 6) 0 0 6 0 2 #xd1 "y")))))

(defun wp6-group (code subgroup data &key prefix)
  "A WP6 variable-length group around DATA (the non-deletable part)."
  (let* ((d (bytes data))
         (head (bytes (if prefix (list #x80 1 (u16 prefix)) 0) (u16 (length d))))
         (total (+ 1 1 2 (length head) (length d) 2 1)))
    (bytes code subgroup (u16 total) head d (u16 total) code)))

(test wp6-text
  (let ((runs (wpg:decode-wp6-text
               (bytes (wp6-group #xd4 #x1b (u16 600) :prefix 4)   ; 12 pt
                      "Big" #xcc
                      (wp6-group #xd4 #x18 '(255 0 0))            ; red
                      "red" 1 #x80 #xf0 41 1 #xf0))))
    (is (= 2 (length runs)))
    (is (equal (format nil "Big~%") (getf (first runs) :text)))
    (is (= 12 (getf (first runs) :size)))
    (is (null (getf (first runs) :color)))
    (is (equal (format nil "red~C ~C" (code-char #xe5) (code-char #xe9))
               (getf (second runs) :text)))
    (is (equal '(255 0 0) (rgb (getf (second runs) :color))))))

(test wp6-text-from-file
  (let* ((image (wpg:read-wpg (fixture "imagemagick-input.wpg")))
         (runs (wpg:record-field (first (wpg:find-records image :text-data)) :runs)))
    (is (equal '((:text "City.gif" :size 6 :font "Helve-WP"))
               (mapcar (lambda (r) (list :text (getf r :text) :size (getf r :size)
                                         :font (getf r :font)))
                       runs)))
    (is (has "City.gif</tspan></text>" (wpg:svg-string image)))))

(defun text-attributes (&key (width 100) (height 240) (h-align 0) (color 0) (angle 0))
  (rec1 #x0d (u16 width) (u16 height) (make-list 10 :initial-element 0) (u16 #x1150)
        0 h-align 0 color (s16 angle)))

(test wpg1-text-attributes
  (let* ((image (wpg:read-wpg (wpg1 (text-attributes :h-align 1 :color 4 :angle 30)
                                    (rec1 #x0c (u16 2) (s16 100) (s16 200) "Hi"))))
         (a (wpg:wpg-record-fields (aref (wpg:wpg-image-records image) 0)))
         (svg (wpg:svg-string image)))
    (is (equal '(:width 100 :height 240 :font #x1150 :h-align 1 :v-align 0 :color 4 :angle 30) a))
    (is (has "transform=\"translate(100 200) rotate(30) scale(1 -1)\"" svg))
    (is (has "font-size=\"240\" fill=\"#aa0000\" text-anchor=\"middle\" textLength=\"200\"" svg))))

(test wpg1-type-2-text
  (let* ((type-2 (rec1 #x18 (u32 10) (s16 0) (u16 0) (s16 0) (s16 0) (s16 1000) (s16 300)
                       (s16 100) (s16 100) 1 #xd1 #x02 (u16 2) 0 #xd1 "Hello"))
         (alone (wpg:read-wpg (wpg1 type-2)))
         (paired (wpg:read-wpg (wpg1 type-2 (rec1 #x0c (u16 5) (s16 0) (s16 50) "Hello")))))
    (is (equal "Hello" (wpg:record-field (aref (wpg:wpg-image-records alone) 0) :text)))
    ;; Drawn only when there is no type 1 copy of the text.
    (is (= 1 (count-matches ">Hello<" (wpg:svg-string alone))))
    (is (= 1 (count-matches ">Hello<" (wpg:svg-string paired))))))

(defun count-matches (needle haystack)
  (loop for start = 0 then (1+ pos)
        for pos = (search needle haystack :start2 start)
        while pos count t))

(test wpg2-text-line
  (let* ((image (wpg:read-wpg
                 (wpg2 (start2 :bounds '(0 0 1200 1200))
                       (rec2 #x1c (u16 0) (u16 0) (s16 600) (s16 300) 2 0 (u32 (* 90 65536)))
                       (rec2 #x0f "Text" #xcc "two"))))
         (svg (wpg:svg-string image))
         (line (aref (wpg:wpg-image-records image) 1)))
    (is (= 90 (wpg:record-field line :angle)))
    (is (= 2 (wpg:record-field line :h-align)))
    ;; 12 point default = 200 units at 1200 per inch; second line 1.2 lower.
    (is (has "<text x=\"0\" y=\"0\" transform=\"translate(600 300) rotate(90) scale(1 -1)\"" svg))
    (is (has "<text x=\"0\" y=\"240\"" svg))
    (is (has "font-size=\"200\"" svg))
    (is (has "text-anchor=\"end\"" svg))))
