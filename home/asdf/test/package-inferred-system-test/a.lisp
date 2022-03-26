(defpackage package-inferred-system-test/a (:use cl))

(in-package :package-inferred-system-test/a)

(eval-when (:compile-toplevel)
  (format t "This is compile-time and the *read-base* is ~D~%" *read-base*))
(eval-when (:load-toplevel)
  (format t "This is load-time and the *read-base* is ~D~%" *read-base*))
(eval-when (:execute)
  (format t "This is execute-time and *read-base* is ~D~%" *read-base*))

(defun tst (x)
  (1+ x))

(defun add10 (x)
  (+ x 10))

(format t "(add10 0) is (in decimal) ~D~%" (add10 0))
