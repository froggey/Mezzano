;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

;;; This is a complete rewrite which does not share any code or
;;; architecture with the old inspector also called "clouseau". It
;;; keeps the system and package name since it should be (and in order
;;; to make it) usable as a drop-in replacement.
(defsystem "clouseau"
  :description "A graphical inspector for arbitrary Common Lisp objects."
  :license     "LGPL-2.1+"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  ;; Version and dependencies
  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("closer-mop"
                "mcclim")
  ;; Files
  :components  ((:module     "base"
                 :pathname   "src"
                 :serial     t
                 :components ((:file       "package")
                              ;; Special variables
                              (:file       "variables")
                              ;; Formatting utilities
                              (:file       "formatting")
                              ;; Generic functions
                              (:file       "protocol")
                              ;; Places and place formatting
                              (:file       "place")
                              (:file       "place-formatting")
                              ;; Presentations for places and values
                              (:file       "presentations")
                              ;; Commands applicable to all objects
                              (:file       "commands")
                              ;; Inspector state, pane mixin and pane
                              (:file       "state")
                              (:file       "pane")))
                ;; Display functions and commands
                (:module     "objects"
                 :pathname   "src/objects"
                 :depends-on ("base")
                 :serial     t
                 :components ((:file       "generic")
                              (:file       "documentation")
                              ;; Standard object kinds
                              (:file       "number")
                              (:file       "character")
                              (:file       "sequence")
                              (:file       "list")
                              (:file       "array")
                              (:file       "instance")
                              (:file       "symbol")
                              (:file       "pathname")
                              (:file       "hash-table")
                              (:file       "function")
                              (:file       "class")
                              (:file       "extended-sequence")
                              ;; Generic disassembly display
                              (:file       "disassembly")))
                ;; History of inspected objects
                (:module     "navigation"
                 :pathname   "src/navigation"
                 :depends-on ("base")
                 :components ((:file       "navigation")))
                ;; Application frame, command table user-face
                ;; functions
                (:module     "application"
                 :pathname   "src"
                 :depends-on ("base" "navigation")
                 :components ((:file       "application"))))

  :in-order-to ((test-op (test-op "clouseau/test"))))

(defsystem "clouseau/test"
  :depends-on ("clouseau"
               "fiveam")

  :components ((:module     "test"
                :components ((:file       "package")))

               (:module     "objects"
                :pathname   "test/objects"
                :depends-on ("test")
                :serial     t
                :components ((:file        "util")
                             (:file        "sequence")
                             (:file        "list"))))
  :perform    (test-op (operation component)
                (uiop:symbol-call '#:clouseau.test '#:run-tests)))
