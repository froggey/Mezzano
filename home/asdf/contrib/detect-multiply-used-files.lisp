(uiop:define-package :detect-multiply-used-files
  (:nicknames :asdf/contrib/detect-multiply-used-files)
  (:use :asdf/component :asdf/system-registry :uiop :common-lisp)
  (:export #:find-fishy-components #:register-component-files #:*file-components*))

(in-package :detect-multiply-used-files)

(defparameter *file-components* (make-hash-table :test 'equal))

(defun register-component-files (component)
  (let ((c (find-component () component)))
    (if-let (p (component-pathname c))
      (pushnew (component-find-path c) (gethash (namestring p) *file-components*)))
    (when (typep c 'parent-component)
      (dolist (cc (component-children c))
        (register-component-files cc)))))

(defun table-keys (table)
  (loop :for s :being :the :hash-keys :of table :collect s))

(defun find-fishy-components ()
  (clrhash *file-components*)
  (map () 'register-component-files (table-keys *registered-systems*))
  (loop :for p :in (sort (table-keys *file-components*) 'string<)
        :for l = (gethash p *file-components*)
        :when (and (file-pathname-p p) (not (length=n-p l 1)))
        :do (format t "~&~S =>~{ ~S~}~%" p l)))

#| ;; Use it like that:
(asdf:load-systems system1 system2 ...)
(detect-multiply-used-files:find-fishy-components)
|#
