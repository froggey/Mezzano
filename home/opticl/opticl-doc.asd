
(eval-when (:load-toplevel :compile-toplevel :execute)
  (map nil #'asdf:load-system '(:alexandria :cl-markdown)))

(cl:defpackage :opticl-doc
  (:use #:cl #:asdf))

(cl:in-package :opticl-doc)

(defclass markdown-file (source-file) ())

(defmethod source-file-type ((c markdown-file) (s module)) "md")

(defmethod output-files ((op compile-op) (c markdown-file))
  (values (list (merge-pathnames (make-pathname :type "html")
                                 (compile-file-pathname (component-pathname c))))
          t))

(defmethod perform ((op compile-op) (c markdown-file))
  (let* ((md-file (namestring (component-pathname c)))
         (html-file (namestring (car (output-files op c)))))
    (with-open-file (html-stream html-file
                                 :direction :output
                                 :if-exists :supersede)
      (let ((md (alexandria:read-file-into-string md-file)))
        (cl-markdown:markdown md :stream html-stream)))))

(defmethod perform ((op load-op) (c markdown-file)))

(asdf:defsystem :opticl-doc
  :name "opticl-doc"
  :description "Documentation for opticl"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :default-component-class markdown-file
  :depends-on (opticl)
  :pathname #.(make-pathname :directory '(:relative "doc"))
  :components ((:file "pixel-access")))

