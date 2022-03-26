(in-package :cl-user)

(asdf:load-system :atdoc)
(asdf:load-system :trivial-garbage)

(let* ((base (asdf:component-pathname (asdf:find-system :trivial-garbage)))
       (output-directory (merge-pathnames "doc/" base))
       (css-file (merge-pathnames "doc/index.css" base)))
  (ensure-directories-exist output-directory)
  (atdoc:generate-html-documentation '(:trivial-garbage)
                                     output-directory
                                     :single-page-p t
                                     :heading "Trivial Garbage"
                                     :index-title "Trivial Garbage"
                                     :css css-file))
