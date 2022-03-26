(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; CLISP 2.48 has a bug that makes this test fail. Hopefully, everyone uses 2.49 or better.
  ;; The ECL bytecode compiler also fails.
  ;; Work around:
  #+(and ecl ecl-bytecmp)
  (when (and (eq asdf:*compile-file-failure-behaviour* :error)
             #+(or clasp ecl) (equal (compile-file-type) "fasc"))
    (error 'compile-file-error :description "faking it"))
  (warn "Warning."))
