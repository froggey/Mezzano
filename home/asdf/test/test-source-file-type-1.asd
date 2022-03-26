;; Should work with both ASDF 1 and ASDF 2.
(defclass cl-source-file-1 (cl-source-file)
  ())

(defmethod asdf::source-file-type ((f cl-source-file-1) (s system))
  (declare (ignorable f s))
  (format t "Hello, world!~%")
  "cl")

(defsystem test-source-file-type-1
  :default-component-class cl-source-file-1
  :serial t
  :components ((:cl-source-file "file1") ; for the package
               (:file "test-tmp")))
