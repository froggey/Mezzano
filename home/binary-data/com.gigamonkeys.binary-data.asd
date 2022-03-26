;;
;; Copyright (c) 2005-2011, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.binary-data
  :name "binary-data"
  :description "Library for reading and writing binary data."
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :components
  ((:file "packages")
   (:file "binary-data" :depends-on ("packages"))
   (:file "common-datatypes" :depends-on ("packages" "binary-data")))
  :depends-on (alexandria))
