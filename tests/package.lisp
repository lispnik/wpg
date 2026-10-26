;;;; package.lisp -- the test package and top-level suite.

(defpackage #:wordperfect-graphics/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:wpg #:wordperfect-graphics))
  (:export #:run-tests))

(in-package #:wordperfect-graphics/tests)

(def-suite wordperfect-graphics
  :description "All wordperfect-graphics tests.")

(defun run-tests ()
  "Run every suite; true if all passed."
  (run! 'wordperfect-graphics))
