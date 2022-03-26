
(defsystem #:persistent
  :components
  ((:file "binseq-package")
   (:file "binseq" :depends-on ("binseq-package"))
   (:file "obinseq" :depends-on ("binseq-package" "binseq"))
   (:file "binseq2" :depends-on ("binseq-package" "obinseq" "binseq"))))
