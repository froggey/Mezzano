(in-package :asdf-tools)

;;; ASDF directory
(defun asdf-dir ()
  (ensure-pathname (system-relative-pathname :asdf/defsystem ())
                   :want-physical t :want-absolute t
                   :want-existing t :truename t))

(defun pn (&rest x)
  (subpathname (asdf-dir) (and x (uiop:resolve-relative-location x))))
(defun nn (&rest x)
  (native-namestring (apply 'pn x)))

(defun uiop-dir () (pn "uiop/"))
(defun build-dir () (pn "build/"))

(defun call-with-asdf-dir (thunk &rest subs)
  (with-current-directory ((apply 'pn subs))
    (funcall thunk)))

(defmacro with-asdf-dir ((&rest subs) &body body)
  `(call-with-asdf-dir (lambda () ,@body) ,@subs))

