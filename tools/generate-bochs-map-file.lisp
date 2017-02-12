;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Generate a Bochs-compatible map file containing every function in the current image.

(in-package :sys.int)

(defun generate-bochs-map-file (path)
  (let ((total-functions 0))
    (dolist (area '(:wired :pinned))
      (walk-area area
                 (lambda (object address size)
                   (declare (ignore address size))
                   (when (and (functionp object)
                              (eql (%object-tag object) +object-tag-function+))
                     (incf total-functions)))))
    (setf total-functions (* total-functions 2))
    (let ((functions (make-array total-functions :fill-pointer 0)))
      (dolist (area '(:wired :pinned))
        (walk-area area
                   (lambda (object address size)
                   (declare (ignore address size))
                     (when (and (functionp object)
                                (eql (%object-tag object) +object-tag-function+))
                       (vector-push object functions)))))
      (setf functions (sort functions #'< :key #'lisp-object-address))
      (with-open-file (s path :direction :output)
        (loop for fn across functions do
             (format s "~12,'0X ~S~%" (%object-ref-signed-byte-64 fn 0) (function-name fn)))))))
