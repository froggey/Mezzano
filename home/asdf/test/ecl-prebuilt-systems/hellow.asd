(defsystem "hellow"
  :depends-on ("asdf")
  :components ((:file "hello"))
  :class program-system
  :build-operation program-op
  :build-pathname "hellow"
  :prologue-code "printf(\"Good morning sunshine!\");fflush(stdout);"
  :epilogue-code (progn (format t "~%Good bye sunshine.~%") (ext:quit 0)))
