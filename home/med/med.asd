(in-package :cl-user)

(defpackage :med-asd
  (:use :cl :asdf))

(in-package :med-asd)

(defsystem :med
  :version "0.1"
  :description "med - Mezzano EDitor"
  :serial t
  :components ((:file "package")
               (:file "line")
               (:file "mark")
               (:file "editor")
               (:file "save-excursion")
               (:file "buffer")
               (:file "buffer-stream")
               (:file "point")
               (:file "minibuffer")
               (:file "redisplay")
               (:file "keybindings")
               (:file "main")
               (:file "commands/commands")
               (:file "commands/display")
               (:file "commands/buffer")
               (:file "commands/sexp")
               (:file "commands/file")
               (:file "commands/eval")
               (:file "commands/repl")
               (:file "commands/grep")
               (:file "commands/find-definition")
               (:file "commands/isearch")))

(defsystem :med-web
  :version "0.1"
  :description "Web browser for med"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :licence "MIT"
  :serial t
  :depends-on (:cl-html5-parser :med)
  :components ((:file "commands/web")))
