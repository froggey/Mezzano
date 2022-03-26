(in-package :cl-user)

(defpackage :command-and-command-table-utilities
  (:use :clim :clim-extensions :clim-lisp)
  (:export :command-of-command-table
           :command-or-commands-of-command-table
           :expand-command-tables))

(defpackage :creating-assoc
  (:use :cl)
  (:export :creating-assoc))

(defpackage :conditional-commands
  (:use :clim :clim-extensions :clim-lisp
        :command-and-command-table-utilities :creating-assoc)
  (:export :define-conditional-application-frame
           :define-conditional-command
           :add-entity-enabledness-change
           :remove-entity-enabledness-change
           :change-entity-enabledness))
