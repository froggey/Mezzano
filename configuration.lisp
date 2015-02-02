#+sbcl (setf sb-impl::*default-external-format* :utf-8)

(defvar *image-name*          "mezzano")
(defvar *this-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults #.(or *compile-file-pathname* *load-pathname*)))
(defvar *quicklisp-directory*
  (merge-pathnames #P"quicklisp/" (user-homedir-pathname) nil))


(load (merge-pathnames #P"setup.lisp" *quicklisp-directory* nil))
(push (merge-pathnames #P"./file-server/" *this-directory* nil) asdf:*central-registry*)
(push (merge-pathnames #P"./"             *this-directory* nil) asdf:*central-registry*)

;;;; THE END ;;;;
