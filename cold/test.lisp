(in-package #:sys.int)

(defun initialize-lisp ()
  (setf (io-port/8 #xE9) (char-code #\O))
  (setf (io-port/8 #xE9) (char-code #\K))
  (loop))
