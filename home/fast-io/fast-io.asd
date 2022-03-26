(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :fast-io *features*))

#+(or sbcl ccl cmucl ecl lispworks allegro)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :fast-io-sv *features*))

(defsystem :fast-io
  :description "Alternative I/O mechanism to a stream or vector"
  :author "Ryan Pavlik"
  :license "NewBSD"
  :version "1.0"

  :depends-on (:alexandria :trivial-gray-streams
               #+fast-io-sv
               :static-vectors)

  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "types")
   (:file "io")
   (:file "gray")))
