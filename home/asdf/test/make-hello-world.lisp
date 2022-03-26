;;; -*- Lisp -*-
#+lispworks (lispworks:load-all-patches)
(load (make-pathname :name "script-support" :defaults *load-pathname*))
(load-asdf)
#+(or clasp ecl mkcl) (require :cmp)

(asdf-test::register-directory asdf-test::*asdf-directory*) ;; we need UIOP, and ECL can dump.
(asdf-test::register-directory asdf-test::*uiop-directory*)
(asdf:upgrade-asdf) ;; may recompile and rename away package asdf?

(asdf-test::frob-packages)

(println "This is make-hello-world, testing its standard-output.") ; *standard-output*
(println "This is make-hello-world, testing its error-output." *error-output*)
(println "This is make-hello-world, testing its stdout." *stdout*)
(println "This is make-hello-world, testing its stderr." *stderr*)

#+mkcl
(defun add-mkcl-dll (pathname)
  ;; make sure mkcl-X.X.X.dll is the same directory as the executable
  (let* ((dll-orig (subpathname (si::self-truename)
                                (strcat #-os-windows "../lib/"
                                        "mkcl_" (si:mkcl-version)
                                        "." (asdf/bundle:bundle-pathname-type :shared-library))))
         (dll-dest (subpathname pathname (strcat #-os-windows "../lib/" (file-namestring dll-orig)))))
    (ensure-directories-exist dll-dest)
    (copy-file dll-orig dll-dest)))


(defun make-hello-bundle (operation)
  (operate operation :hello-world-example)
  #+mkcl (add-mkcl-dll (asdf::output-file operation :hello-world-example))
  (quit 0))

(defun make-hello-image ()
  (make-hello-bundle 'image-op))

(defun make-hello-program ()
  (make-hello-bundle 'program-op))
