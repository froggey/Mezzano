
(defsystem #:clim-lisp
  :serial t
  :depends-on (#:alexandria
               #:trivial-gray-streams
               #:closer-mop
               #+(or) #:log4cl)
  :components (;; First possible patches
               (:file "patch")
               (:module "Lisp-Dep"
                        :components
                        (#+(or excl clisp)
                           (:file   #+excl      "fix-acl"
                                    #+clisp     "fix-clisp")))
               (:file "package")))
