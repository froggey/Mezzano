(defpackage :hello
  (:use :cl :uiop)
  (:export #:main #:entry-point))

(in-package :hello)

(defun main (&rest arguments)
  (format t "hello, world~%")
  (when arguments
    (format t "You passed ~D arguments:~%~{  ~S~%~}" (length arguments) arguments))
  ;; Return success!
  t)

(defun entry-point ()
  (apply 'main *command-line-arguments*))
