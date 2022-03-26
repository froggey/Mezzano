(in-package #:cl-user)

(ffi::clines "extern const char *hello_string;")

(ffi::def-foreign-var ("hello_string" +hello-string+) (* :char) nil)

(princ (ffi:convert-from-foreign-string +hello-string+))
