;;;; fuzz.lisp -- mutation fuzzing for the reader, scene builder and renderers.
;;;;
;;;; Take some real WPG files, damage copies of them at random, and push each
;;;; copy through everything the library does with a file: READ-WPG (lenient
;;;; and strict), BUILD-SCENE, SVG output with its PNG bitmaps, and the
;;;; rasterizer. Damaged input may be refused, but only with a WPG-ERROR.
;;;; Anything else counts as a bug: another error, a timeout, running out of
;;;; memory or stack, or a lenient read that hides a non-WPG error in a
;;;; record.
;;;;
;;;; Most mutations are aimed at record bodies, found by reading the seed
;;;; first, since damage to the header or the record framing is mostly
;;;; refused before anything interesting runs.

(defpackage #:wordperfect-graphics/fuzz
  (:use #:cl)
  (:local-nicknames (#:wpg #:wordperfect-graphics))
  (:export #:check-octets #:mutate #:minimize #:fuzz #:main
           #:failure #:failure-stage #:failure-condition #:failure-signature))

(in-package #:wordperfect-graphics/fuzz)

;;; Checking one input

(defstruct failure
  stage          ; :read, :read-strict, :decode, :scene, :svg, :raster
  condition      ; the condition, or a keyword for :timeout
  backtrace)     ; a list of frames, innermost first

(defun failure-signature (failure)
  "A short string that groups failures with the same likely cause: the
stage, the condition type, and the innermost frame in this library."
  (let ((c (failure-condition failure)))
    (format nil "~(~A~) ~(~S~)~@[ in ~A~]"
            (failure-stage failure)
            (if (typep c 'condition) (type-of c) c)
            (or (find-if #'library-frame-p (failure-backtrace failure))
                (first (failure-backtrace failure))))))

(defun library-frame-p (frame)
  "True if FRAME names a function of the library (not of this fuzzer):
WORDPERFECT-GRAPHICS:FOO, or a local function inside one."
  (let ((name (string-left-trim "(" frame)))
    (dolist (prefix '("FLET " "LABELS "))
      (when (eql 0 (search prefix name))
        (setf name (subseq name (length prefix)))))
    (eql 0 (search "WORDPERFECT-GRAPHICS:" name))))

(defun frame-names (frames)
  (loop for frame in frames
        for name = (ignore-errors
                    (let ((*print-length* 3) (*print-level* 2))
                      (prin1-to-string (first frame))))
        when name collect name))

(defvar *timeout* 10
  "Seconds one input may take, for all stages together.")

(defvar *render-width* 64
  "Pixel width of the raster stage; small, since it only has to run.")

(defun run-stage (stage thunk &key allow)
  "Call THUNK. Return its value, or a FAILURE if it signalled an error that
is not of type ALLOW, or ran out of memory or stack."
  (block nil
    (handler-case
        (handler-bind
            ((serious-condition
               (lambda (c)
                 (unless (or (and allow (typep c allow))
                             (typep c 'sb-sys:interactive-interrupt))
                   (return (make-failure
                            :stage stage :condition c
                            :backtrace (frame-names
                                        (ignore-errors
                                         (sb-debug:list-backtrace :count 40)))))))))
          (funcall thunk))
      ;; Only allowed errors get this far.
      (error () nil))))

(defun check-octets (octets &key (timeout *timeout*))
  "Run OCTETS through the whole library. Returns NIL if every stage either
worked or refused the input with a WPG-ERROR, otherwise a FAILURE."
  (handler-case
      ;; A smaller bitmap limit than the default, which allows a bounded but
      ;; slow 256 MB: the fuzzer is after work that is not bounded at all.
      (let ((wpg:*max-bitmap-bytes* (* 16 1024 1024)))
        (sb-ext:with-timeout timeout
          (check-stages octets)))
    (sb-ext:timeout ()
      (make-failure :stage :timeout :condition :timeout))))

(defun check-stages (octets)
  (block check
    (flet ((stage (name thunk &optional (allow 'wpg:wpg-error))
             ;; Run one stage; leave CHECK with the failure if it failed.
             (let ((v (run-stage name thunk :allow allow)))
               (when (failure-p v) (return-from check v))
               v)))
      (stage :read-strict (lambda () (wpg:read-wpg octets :strict t)))
      (let ((image (stage :read (lambda () (wpg:read-wpg octets)))))
        (unless image                   ; refused: the header was bad
          (return-from check nil))
        ;; The lenient reader catches everything a record decoder signals;
        ;; anything but a WPG-ERROR there is a bug it hid.
        (loop for r across (wpg:wpg-image-records image)
              for e = (wpg:wpg-record-error r)
              when (and e (not (typep e 'wpg:wpg-error)))
                do (return-from check
                     (make-failure :stage :decode :condition e
                                   :backtrace (list (format nil "record ~(~A~)"
                                                            (wpg:wpg-record-name r))))))
        (let ((scene (stage :scene (lambda () (wpg:build-scene image)) nil)))
          (stage :svg (lambda ()
                        (wpg:svg-string image :bitmap-href
                                        (lambda (b) (length (wpg:bitmap-png-octets b)) "x"))))
          (stage :raster (lambda ()
                           (let ((wpg:*max-pixels* 1000000))
                             (wpg:render-scene scene :width *render-width*))))
          nil)))))

;;; Mutation

(defparameter +interesting-8+ #(0 1 2 #x7f #x80 #xfe #xff))
(defparameter +interesting-16+ #(0 1 #x7fff #x8000 #xffff #x0100 #x00ff))
(defparameter +interesting-32+ #(0 1 #x7fffffff #x80000000 #xffffffff #x00010000))

(defun record-spans (octets)
  "(start body-start end) for each record the lenient reader finds."
  (let ((image (ignore-errors (wpg:read-wpg octets))))
    (when image
      (map 'vector (lambda (r)
                     (list (wpg:wpg-record-offset r) (wpg:wpg-record-body-offset r)
                           (+ (wpg:wpg-record-body-offset r)
                              (length (wpg:wpg-record-body r)))))
           (wpg:wpg-image-records image)))))

(defun random-elt (seq) (elt seq (random (length seq))))

(defun put-le (v i value bytes)
  (dotimes (k bytes)
    (when (< (+ i k) (length v))
      (setf (aref v (+ i k)) (ldb (byte 8 (* 8 k)) value)))))

(defun concat (&rest parts)
  (let ((out (make-array (reduce #'+ parts :key #'length)
                         :element-type '(unsigned-byte 8)))
        (p 0))
    (dolist (part parts out)
      (replace out part :start1 p)
      (incf p (length part)))))

(defun octets (seq) (coerce seq '(simple-array (unsigned-byte 8) (*))))

(defun pick-position (v spans)
  "A position to damage: usually inside a record body, sometimes anywhere
after the signature."
  (let ((bodies (and spans (remove-if (lambda (s) (= (second s) (third s))) spans))))
    (if (and bodies (plusp (length bodies)) (< (random 10) 8))
        (let ((s (random-elt bodies)))
          (+ (second s) (random (- (third s) (second s)))))
        (+ 4 (random (max 1 (- (length v) 4)))))))

(defun mutate-once (v spans donors)
  "One random change to V (which may be modified), returning the new vector."
  (let ((n (length v)))
    (if (< n 8)
        v
        (ecase (random 12)
          (0 (let ((i (pick-position v spans)))  ; flip a bit
               (when (< i n) (setf (aref v i) (logxor (aref v i) (ash 1 (random 8)))))
               v))
          (1 (let ((i (pick-position v spans)))  ; random byte
               (when (< i n) (setf (aref v i) (random 256)))
               v))
          ((2 3) (let ((i (pick-position v spans))) ; interesting value
                   (case (random 3)
                     (0 (put-le v i (random-elt +interesting-8+) 1))
                     (1 (put-le v i (random-elt +interesting-16+) 2))
                     (2 (put-le v i (random-elt +interesting-32+) 4)))
                   v))
          (4 (let ((i (pick-position v spans)))  ; nudge a value
               (put-le v i (ldb (byte 16 0) (+ (logior (aref v (min i (1- n)))
                                                       (ash (aref v (min (1+ i) (1- n))) 8))
                                               (- (random 33) 16)))
                       2)
               v))
          (5 (if (> n 17)                              ; truncate
                 (subseq v 0 (+ 16 (random (- n 16))))
                 v))
          (6 (let* ((i (pick-position v spans))           ; delete a range
                    (k (min (- n i) (1+ (random 16)))))
               (concat (subseq v 0 i) (subseq v (+ i k)))))
          (7 (let ((i (pick-position v spans)))           ; insert random bytes
               (concat (subseq v 0 (min i n))
                       (octets (loop repeat (1+ (random 8)) collect (random 256)))
                       (subseq v (min i n)))))
          ((8 9) (if (and spans (> (length spans) 1)) ; whole records
                     (let* ((a (random-elt spans)) (b (random-elt spans))
                            (rec (subseq v (first a) (min n (third a)))))
                       (case (random 3)
                         (0 (concat (subseq v 0 (first b)) rec (subseq v (first b))))
                         (1 (concat (subseq v 0 (first a)) (subseq v (min n (third a)))))
                         (2 (let ((i (+ (first a) (random (max 1 (- (second a) (first a)))))))
                              ;; damage the record header itself
                              (when (< i n) (setf (aref v i) (random-elt +interesting-8+)))
                              v))))
                     v))
          ((10 11) (if donors                  ; splice in a record from another file
                       (destructuring-bind (dv . dspans) (random-elt donors)
                         (if (and dspans (plusp (length dspans)) spans (plusp (length spans)))
                             (let* ((d (random-elt dspans))
                                    (at (first (random-elt spans))))
                               (concat (subseq v 0 at)
                                       (subseq dv (first d) (min (length dv) (third d)))
                                       (subseq v at)))
                             v))
                       v))))))

(defun mutate (octets &key spans donors (count (1+ (random 4))))
  "A damaged copy of OCTETS, after COUNT random changes. SPANS are its
record spans (from RECORD-SPANS) and DONORS a list of (octets . spans) to
splice records from."
  (let ((v (copy-seq octets)))
    (dotimes (i count v)
      (let ((before (length v)))
        (setf v (mutate-once v spans donors))
        ;; Records have moved: find them again.
        (unless (= before (length v))
          (setf spans (record-spans v)))))))

;;; Shrinking a failing input

(defun timeout-p (failure)
  (let ((c (failure-condition failure)))
    (or (eq c :timeout) (typep c 'sb-ext:timeout))))

(defun minimize (octets failure &key (timeout *timeout*)
                                     (max-tries (if (timeout-p failure) 20 1000)))
  "A smaller input that fails with the same signature as FAILURE: removes
ever smaller chunks while the failure persists. Timeouts get few tries,
since each one waits out the timeout."
  (let ((signature (failure-signature failure))
        (best octets)
        (tries 0))
    (flet ((same-p (candidate)
             (incf tries)
             (let ((f (check-octets candidate :timeout timeout)))
               (and f (string= (failure-signature f) signature)))))
      (loop with chunk = (max 1 (floor (length best) 4))
            while (and (>= chunk 1) (< tries max-tries))
            do (let ((progress nil))
                 (loop with i = 16
                       while (and (< i (length best)) (< tries max-tries))
                       do (let ((candidate (concat (subseq best 0 i)
                                                   (subseq best (min (length best) (+ i chunk))))))
                            (if (same-p candidate)
                                (setf best candidate progress t)
                                (incf i chunk))))
                 (unless progress
                   (setf chunk (floor chunk 2)))))
      best)))

;;; The driver

(defun seed-files (paths max-bytes)
  "(pathname . octets) for each WPG file in PATHS (files, or directories
searched recursively) no bigger than MAX-BYTES."
  (let ((files '()))
    (dolist (p paths)
      (let ((p (if (stringp p) (uiop:parse-native-namestring p) p)))
        (cond ((uiop:directory-exists-p p)
               (uiop:collect-sub*directories
                (uiop:ensure-directory-pathname p) t t
                (lambda (dir) (setf files (append (uiop:directory-files dir) files)))))
              ((uiop:file-exists-p p) (push p files)))))
    (loop for f in (sort (remove-duplicates files :test #'equal) #'string< :key #'namestring)
          for octets = (ignore-errors (wpg::source-octets f))
          when (and octets (<= (length octets) max-bytes)
                    (or (wpg::ole-file-p octets)
                        (and (> (length octets) 4) (= (aref octets 0) #xff)
                             (= (aref octets 1) #x57))))
            collect (cons f octets))))

(defun write-octets (octets path)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
    (write-sequence octets out)))

(defun save-failure (dir octets failure)
  (ensure-directories-exist dir)
  (let* ((hash (subseq (format nil "~(~{~2,'0X~}~)" (coerce (sb-md5:md5sum-sequence octets) 'list))
                       0 12))
         (base (merge-pathnames (format nil "crash-~A" hash) dir)))
    (with-open-file (out (make-pathname :type "wpg" :defaults base)
                         :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
      (write-sequence octets out))
    (with-open-file (out (make-pathname :type "txt" :defaults base)
                         :direction :output :if-exists :supersede)
      (format out "~A~%~%~A~%~%~{~A~%~}" (failure-signature failure)
              (ignore-errors (princ-to-string (failure-condition failure)))
              (failure-backtrace failure)))
    (make-pathname :type "wpg" :defaults base)))

(defun fuzz (&key (seeds '("tests/data/")) (iterations 1000) (seed 1)
                  (output "fuzz-crashes/") (max-seed-bytes 262144)
                  (timeout *timeout*) (minimize t) (verbose t) extra-seeds)
  "Fuzz for ITERATIONS inputs mutated from the files in SEEDS (files or
directories, searched recursively) plus EXTRA-SEEDS (octet vectors).
SEED makes the run repeatable. Each distinct failure is shrunk (with
MINIMIZE) and saved under OUTPUT, if OUTPUT is not NIL. Returns a list of
(signature octets failure), one per distinct failure."
  (let* ((*random-state* (sb-ext:seed-random-state seed))
         (pool (coerce (mapcar (lambda (o) (cons o (record-spans o)))
                               (append (mapcar #'cdr (seed-files seeds max-seed-bytes))
                                       extra-seeds))
                       'vector))
         (donors (coerce pool 'list))
         (found (make-hash-table :test #'equal))
         (refused 0)
         (start (get-internal-real-time)))
    (when (zerop (length pool))
      (error "No seed files found in ~S" seeds))
    (when verbose
      (format t "~&Fuzzing ~:D input~:P from ~D seed~:P (random seed ~D)~%"
              iterations (length pool) seed))
    (dotimes (i iterations)
      (destructuring-bind (octets . spans) (aref pool (random (length pool)))
        (let* ((input (mutate octets :spans spans :donors donors))
               (failure (progn
                          ;; Leave the input on disk while it runs, in case
                          ;; it hangs in a way the timeout cannot stop.
                          (when output (write-octets input (merge-pathnames "current.wpg" output)))
                          (check-octets input :timeout timeout))))
          (cond
            ((null failure)
             (unless (ignore-errors (wpg:read-wpg input :strict t)) (incf refused)))
            ((not (gethash (failure-signature failure) found))
             (let* ((small (if minimize (minimize input failure :timeout timeout) input))
                    (path (and output (save-failure output small failure))))
               (setf (gethash (failure-signature failure) found)
                     (list (failure-signature failure) small failure))
               (when verbose
                 (format t "~&[~D] ~A~%    ~A~@[~%    saved ~A (~:D bytes)~]~%"
                         i (failure-signature failure)
                         (ignore-errors (princ-to-string (failure-condition failure)))
                         (and path (namestring path)) (length small)))))))
        (when (and verbose (plusp i) (zerop (mod i 1000)))
          (format t "~&  ~:D done, ~D distinct failure~:P~%" i (hash-table-count found)))))
    (when verbose
      (format t "~&Done: ~:D inputs in ~,1Fs, ~:D refused in strict mode, ~D distinct failure~:P~%"
              iterations (/ (- (get-internal-real-time) start) internal-time-units-per-second)
              refused (hash-table-count found)))
    (loop for v being the hash-values of found collect v)))

(defun main ()
  "Entry point for `make fuzz': settings from the environment."
  (flet ((env (name default)
           (let ((v (uiop:getenv name)))
             (if (and v (plusp (length v))) v default))))
    (let ((failures (fuzz :seeds (uiop:split-string (env "FUZZ_CORPUS" "tests/data/") :separator ":")
                          :iterations (parse-integer (env "FUZZ_ITERATIONS" "5000"))
                          :seed (parse-integer (env "FUZZ_SEED" "1"))
                          :output (env "FUZZ_OUTPUT" "fuzz-crashes/"))))
      (uiop:quit (if failures 1 0)))))
