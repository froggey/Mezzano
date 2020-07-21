;;; Generate a Bochs-compatible map file containing every function in the current image.

(in-package :mezzano.internals)

(defun generate-bochs-map-file (path)
  (let ((total-functions 0)
        (areas '(:wired-function :function)))
    (dolist (area areas)
      (walk-area area
                 (lambda (object address size)
                   (declare (ignore address size))
                   (when (or (%object-of-type-p object +object-tag-function+)
                             (function-reference-p object))
                     (incf total-functions)))))
    (setf total-functions (* total-functions 2))
    (let ((functions (make-array total-functions :fill-pointer 0)))
      (dolist (area areas)
        (walk-area area
                   (lambda (object address size)
                   (declare (ignore address size))
                     (when (or (%object-of-type-p object +object-tag-function+)
                               (function-reference-p object))
                       (vector-push object functions)))))
      (setf functions (sort functions #'< :key #'lisp-object-address))
      (with-open-file (s path :direction :output)
        (loop for fn across functions do
             (if (function-reference-p fn)
                 (format s "~12,'0X {Fref ~S}~%"
                         (%function-reference-code-location fn) (function-reference-name fn))
                 (format s "~12,'0X ~S~%" (%object-ref-signed-byte-64 fn 0) (function-name fn))))))))
