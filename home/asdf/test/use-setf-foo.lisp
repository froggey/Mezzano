(in-package :asdf-test/deferred-warnings)

(defclass <bar> () ())

(defun use-setf-foo (v x)
  (setf (foo x) v))

(defmethod gf ((x <bar>) (y (eql (+ 1 1))))
  (setf (foo (make-instance '<bar>)) #(1 2 3 4)))


(defmethod (setf gf) (value (x <bar>) (y (eql (+ 1 1))))
  (setf (foo x) (+ y value)))



#+clisp (eval-when (:compile-toplevel :load-toplevel :execute)
          (format t "~&~S~%" `(sys::*unknown-functions* ,sys::*unknown-functions*)))
