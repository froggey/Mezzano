(in-package :asdf-tools)

;;; BONUS: install asdf as module for your favorite Lisp implementation.
(deftestcmd install-asdf (lisp)
  "install asdf as a module on specified Lisp"
  (flet ((doit ()
           (with-asdf-dir ()
             (run-test-lisp
              (format nil "installing ASDF to be provided as a module on ~(~A~)" lisp)
              '((load "tools/install-asdf.lisp")(uiop:quit))
              :lisp lisp))))
    (case lisp
      ((:allegro :allegromodern :ccl :clisp :cmucl :ecl :ecl_bytecodes :lispworks :mkcl :sbcl :scl :xcl)
       (doit))
      ((:abcl)
       (format t "Upgrading the implementation-provided ASDF on ~(~A~) isn't supported (yet).
Happily, that implementation is known to keep ASDF reasonably up to date.~%" lisp))
      ((:cormancl :gcl :genera :mcl :mocl)
       (format t "Installing ASDF so it is provided by ~(~A~) isn't supported.
If you care, go hack the implementation.~%" lisp))
      (otherwise
       (if (string-prefix-p "allegro" (string-downcase lisp))
           (doit)
           (error "Unknown implementation ~(~A~)" lisp))))))
