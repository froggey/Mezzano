(defsystem "drei-mcclim"
  :description "Drei Replaces EINE's Inheritor – McCLIM editor substrate"
  :depends-on ((:version "flexichain" "1.5.1")
               "esa-mcclim" "clim-core" #-clim-without-swank "swank"
               "automaton" "persistent"
               "mcclim-fonts")
  :components
  ((:file "packages")
   (:file "buffer" :depends-on ("packages"))
   (:file "delegating-buffer" :depends-on ("packages" "buffer"))
   (:file "motion" :depends-on ("packages" "buffer" "syntax"))
   (:file "editing" :depends-on ("packages" "buffer" "syntax" "motion" "kill-ring"))
   (:file "base" :depends-on ("packages" "buffer" "Persistent/persistent-buffer" "kill-ring"
                                         "delegating-buffer"))
   (:file "syntax" :depends-on ("packages" "buffer" "base"))
   (:file "modes" :depends-on ("packages" "syntax"))
   (:file "views" :depends-on ("packages" "buffer" "base" "syntax" "Persistent/persistent-undo"
                                          "Persistent/persistent-buffer" "undo" "abbrev"
                                          "delegating-buffer" "modes"))
   (:file "drei" :depends-on ("packages" "views" "motion" "editing"))
   (:file "drei-clim" :depends-on ("drei"))
   (:file "drei-redisplay" :depends-on ("drei-clim"))
   (:file "drawing-options" :depends-on ("drei-redisplay"))
   (:file "input-editor" :depends-on ("drei-redisplay" "lisp-syntax" "core"))
   (:file "abbrev" :depends-on ("packages"))
   (:file "kill-ring" :depends-on ("packages"))
   (:file "undo" :depends-on ("packages"))
   (:file "basic-commands" :depends-on ("drei-clim" "motion" "editing"))
   (:file "core" :depends-on ("drei"))
   (:file "fundamental-syntax" :depends-on ("packages" "drei-redisplay" "core"))
   (:file "buffer-streams" :depends-on ("core"))
   (:file "rectangle" :depends-on ("core"))
   (:file "targets" :depends-on ("core"))
   (:file "core-commands" :depends-on ("core" "rectangle" "drei-clim"))
   (:file "Persistent/persistent-buffer" :depends-on ("packages"))
   (:file "Persistent/persistent-undo"
          :depends-on ("packages" "buffer" "Persistent/persistent-buffer" "undo"))
   (:file "misc-commands" :depends-on ("basic-commands"))
   (:file "search-commands" :depends-on ("core" "targets" "drei-clim"))
   (:file "lr-syntax" :depends-on ("fundamental-syntax" "core" "drawing-options"))
   (:file "lisp-syntax" :depends-on ("lr-syntax" "motion" "core"))
   (:file "lisp-syntax-swine" :depends-on ("lisp-syntax"))
   (:file "lisp-syntax-commands" :depends-on ("lisp-syntax-swine" "misc-commands"))
   (:file "lisp-syntax-swank"
          :if-feature (:not :clim-without-swank)
          :depends-on ("lisp-syntax")))
  :in-order-to ((test-op (test-op "drei-mcclim/test"))))

(defsystem "drei-mcclim/test"
  :depends-on ("drei-mcclim" "fiveam" "automaton")
  :components
  ((:module "Tests"
    :components
    ((:file "packages")
     (:file "testing" :depends-on ("packages"))
     (:file "buffer-tests" :depends-on ("testing"))
     (:file "base-tests" :depends-on ("testing"))
     (:file "kill-ring-tests" :depends-on ("testing"))
     (:file "motion-tests" :depends-on ("testing"))
     (:file "editing-tests" :depends-on ("testing"))
     (:file "core-tests" :depends-on ("testing"))
     (:file "buffer-streams-tests" :depends-on ("testing"))
     (:file "rectangle-tests" :depends-on ("testing"))
     (:file "undo-tests" :depends-on ("testing"))
     (:file "lisp-syntax-tests" :depends-on ("testing" "motion-tests"))
     (:file "lisp-syntax-swine-tests" :depends-on ("lisp-syntax-tests")))))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:drei-tests '#:run-tests)))
