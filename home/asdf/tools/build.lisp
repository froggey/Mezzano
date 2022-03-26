(in-package :asdf-tools)

(deftestcmd build-asdf ()
  "make sure asdf.lisp is built"
  (load-system :asdf)
  (success))

(deftestcmd build-asdf-tools ()
  "build a binary for asdf-tools"
  (asdf:operate :program-op :asdf-tools)
  (success))

;;; Documentation
(deftestcmd doc ()
  "build documentation in doc/ directory"
  (with-failure-context (:name "in doc/ directory")
    (run* '(make) :directory (pn "doc/"))))

(deftestcmd website ()
  "publish documentation onto the public website"
  (with-failure-context (:name "in doc/ directory")
    (run* '(make website) :directory (pn "doc/"))))


;;; Line counting
(deftestcmd wc ()
  "count lines of lisp code in asdf and uiop"
  (with-asdf-dir ()
    (flet ((lisp-only (x) (remove "lisp" x :test-not 'equal :key 'pathname-type)))
      (let ((uiop-files (mapcar #'(lambda (x) (subpathname "uiop/" x)) (lisp-only (uiop-files))))
            (defsystem-files (remove "build/asdf.lisp"
                                     (lisp-only (asdf-defsystem-files)) :test 'equal)))
        (run* `(pipe (wc ,@uiop-files) (sort -n)))
        (terpri)
        (run* `(pipe (wc ,@defsystem-files) (sort -n)))
        (terpri)
        (run* `(pipe (wc ,@uiop-files ,@defsystem-files) (tail -n 1)))))))


;;; debug the source registry that is being used to execute these tools.
(deftestcmd list-source-registry ()
  "Display the source-registry cache"
  (writeln (sort (alexandria:hash-table-alist asdf::*source-registry*)
                 'string< :key 'car)))
