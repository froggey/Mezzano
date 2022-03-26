(defsystem "hellow"
  :components ((:file "hello"))
  :class program-system
  :build-operation program-op
  :build-pathname "hellow"
  :prologue-code "printf(\"Good morning sunshine!\\n\");fflush(stdout);"
  :epilogue-code (progn
                   (format t "~%Good bye sunshine.~%")
                   (ext:quit 0))
  :no-uiop t
  :extra-build-args (:ld-flags #.(list (namestring (compile-file-pathname "hello_aux.c" :type :object)))))
