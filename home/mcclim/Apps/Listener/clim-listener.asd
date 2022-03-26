
(defsystem #:clim-listener
  :license "LGPL-2.1+"
  :description "CLIM Lisp listener."
  :long-description "CLIM Lisp listener

The McCLIM Listener provides an interactive toplevel with full access
to the graphical capabilities of CLIM and a set of built-in commands
intended to be useful for lisp development and
experimentation. Present features include:

- Reading/evaluation of lisp expressions
- Ability to run external programs, through the 'Run' command or #! macro
- Commands for inspecting CLOS classes (superclasses/subclasses, slots, etc.)
- Navigation of the filesystem, including a directory stack
- Launching of external programs sensitive to file type (determined by mailcap
  and mime.types files)"
  :depends-on (#:mcclim #:clim-debugger #:uiop #:cl-fad #+sbcl #:sb-posix)
  :serial t
  :build-operation asdf:program-op
  :build-pathname "clim-listener"
  :entry-point "clim-listener:run-listener"
  :components ((:file "package")
               (:file "appearance")
               (:file "util")
               (:file "icons")
               (:file "file-types")
               #+(or) ; ASDF commands seems slightly broken
               (:file "asdf")
               (:file "dev-commands")
               (:file "wholine")
               (:file "listener")
               (:file "cmu-hacks" :if-feature :cmu)))
