;; Works only in ASDF 2
(defclass cl-source-file-2 (cl-source-file)
  ((type :initform "cl")))

(defsystem test-source-file-type-2
  :default-component-class cl-source-file-2
  :serial t
  :components ((:file "file1" :type "lisp") ; for package
               (:file "test-tmp")))
