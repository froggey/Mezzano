;;;
;;; Copyright (c) 2005, Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

(defsystem #:automaton
  :depends-on ()
  :components
  ((:file "automaton-package")
   (:file "eqv-hash" :depends-on ("automaton-package"))
   (:file "state-and-transition" :depends-on ("eqv-hash"))
   (:file "automaton" :depends-on ("state-and-transition" "eqv-hash"))
   (:file "regexp" :depends-on ("automaton"))))
