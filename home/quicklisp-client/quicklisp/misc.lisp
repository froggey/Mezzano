;;;; misc.lisp

(in-package #:quicklisp-client)

;;;
;;; This stuff will probably end up somewhere else.
;;;

(defun use-only-quicklisp-systems ()
  (asdf:initialize-source-registry
   '(:source-registry :ignore-inherited-configuration))
  (asdf:map-systems 'asdf:clear-system)
  t)

(defun who-depends-on (system-name)
  "Return a list of names of systems that depend on SYSTEM-NAME."
  (loop for system in (provided-systems t)
        when (member system-name (required-systems system) :test 'string=)
        collect (name system)))
