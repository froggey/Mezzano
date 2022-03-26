(in-package :cl-user)

(defpackage :conditional-commands-example
  (:use :clim :clim-extensions :clim-lisp
        :conditional-commands)
  (:export :run))


(in-package :conditional-commands-example)

(defvar *incarnation-number* 0)

(define-conditional-application-frame conditional-commands-example ()
  (:disable-commands (com-bar com-baz)
   :disable-sheets (thud grunt)
   :evaluate-this (incf *incarnation-number*))
  ()
  (:command-table (conditional-commands-example
                   :inherit-from (cruft-command-table)
                   :menu (("Cruft" :menu cruft-command-table))))
  (:menu-bar t)
  (:panes
   (incarnation (make-pane 'clim-stream-pane :height 25
                           :display-function
                           (lambda (frame pane)
                             (declare (ignore frame))
                             (format pane "Incarnation #~d" *incarnation-number*))))
   (screen :application :display-time nil
           :width 200 :height 100)
   (thud :push-button :label "thud")
   (grunt :push-button :label "grunt"))
  (:layouts
   (defaults (vertically ()
               incarnation
               screen
               thud
               grunt))))

(defun run ()
  (run-frame-top-level (make-application-frame 'conditional-commands-example)))

(define-command-table cruft-command-table)

(define-conditional-command (com-foo :name t :menu t
                                     :command-table cruft-command-table)
    (conditional-commands-example :enable-commands  (com-bar)
                                  :disable-commands (com-baz)
                                  :disable-sheets   (thud grunt))
    ()
    (format t "~&foo called")
    (finish-output))

(define-conditional-command (com-bar :name t :menu t
                                     :command-table cruft-command-table)
    (conditional-commands-example :enable-commands  (com-baz)
                                  :disable-commands (com-bar)
                                  :enable-sheets    (thud)
                                  :disable-sheets   (grunt))
    ()
    (format t "~%bar called"))


;;; It might be less confusing to define all entity-enabledness-changes
;;; in one place, therefore it is also possible to define the
;;; entity-enabledness-change separately:

(define-conditional-command (com-baz :name t :menu t
                                     :command-table cruft-command-table)
    ()
    ()
    (format t "~%baz called"))

(add-entity-enabledness-change 'com-baz 'conditional-commands-example
                               :enable-commands  '(com-bar)
                               :disable-commands '(com-baz)
                               :enable-sheets   '(grunt)
                               :disable-sheets   '(thud))
