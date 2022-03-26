;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2007-2008 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.
;;;
;;; This file contains the implementation of the infrastructure for
;;; Drei "modes", loosely equivalent to Emacs minor modes. They modify
;;; aspects of the behavior of a view or syntax.

(in-package :drei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The general mode protocol and macros.

(defvar *global-modes* '()
  "A list of the names of modes globally available to Drei
instances. Do not use this list to retrieve modes, use the
function `available-modes' instead. The modes on this list are
available to all Drei variants.")

(defun applicable-modes (drei)
  "Return a list of the names of all modes applicable for
`drei'."
  (remove-if-not #'(lambda (mode)
                     (mode-applicable-p (view drei) mode))
                 (available-modes drei)))

(defclass view-mode (mode)
  ()
  (:documentation "The superclass for all view modes."))

(defclass syntax-mode (mode)
  ()
  (:documentation "The superclass for all syntax modes."))

(defmacro define-mode (name (&rest superclasses)
                       (&rest slot-specs)
                       &rest options)
  "Define a toggable Drei mode. It is essentially a class, with
the provided `name', `superclasses', `slot-specs' and
`options'. It will automatically be a subclass of `mode'. Apart
from the normal class options, `options' can also have a
`:global' option, which when true signifies that the mode is
globally available to all Drei instances. This option is true by
default. Note that modes created via this macro are not
applicable to anything."
  (let ((global t)
        (actual-options '()))
    (dolist (option options)
      (case (first option)
        (:global (setf global (second option)))
        (t (push option actual-options))))
   `(progn
      (defclass ,name (,@superclasses mode)
        (,@slot-specs)
        ,@actual-options)
      ,(when global `(push ',name *global-modes*)))))

(defmacro define-view-mode (name (&rest superclasses)
                            (&rest slot-specs)
                            &rest options)
  "Define a mode (as `define-mode') that is applicable to
views. Apart from taking the same options as `define-mode', it
also takes an `:applicable-views' option (nil by default) that is
a list of views the mode should be applicable to. Multiple uses
of this option are cumulative."
  (let ((applicable-views '())
        (actual-options '()))
    (dolist (option options)
      (case (first option)
        (:applicable-views (setf applicable-views
                                 (append applicable-views
                                         (rest option))))
        (t (push option actual-options))))
   `(progn
      (define-mode ,name (,@superclasses view-mode)
        (,@slot-specs)
        ,@actual-options)
      ,@(loop for view in applicable-views
           collecting `(defmethod mode-directly-applicable-p or
                           ((view ,view) (mode-name (eql ',name)))
                         t)))))

(defmacro define-syntax-mode (name (&rest superclasses)
                              (&rest slot-specs)
                              &rest options)
  "Define a mode (as `define-mode') that is applicable to
syntaxes. Apart from taking the same options as `define-mode', it
also takes an `:applicable-syntaxes' option (nil by default) that
is a list of syntaxes the mode should be applicable to. Multiple
uses of this option are cumulative."
  (let ((applicable-syntaxes '())
        (actual-options '()))
    (dolist (option options)
      (case (first option)
        (:applicable-syntaxes (setf applicable-syntaxes
                                    (append applicable-syntaxes
                                            (rest option))))
        (t (push option actual-options))))
    `(progn
       (define-mode ,name (,@superclasses syntax-mode)
         (,@slot-specs)
         ,@actual-options)
       ,@(loop for syntax in applicable-syntaxes
            collecting `(defmethod mode-directly-applicable-p or
                            ((syntax ,syntax) (mode-name (eql ',name)))
                          t)))))

(defmacro define-mode-toggle-commands (command-name
                                       (mode-name &optional (string-form (capitalize (string mode-name))))
                                       &key (name t) command-table)
  "Define a simple command (named `command-name') for toggling
the mode named by `mode-name' on and off. `String-form' is the
name of the mode that will be put in the docstring, `name' and
`command-table' work as in `define-command'."
  (check-type command-name symbol)
  (check-type mode-name symbol)
  (check-type string-form string)
  `(define-command (,command-name :name ,name :command-table ,command-table)
       ()
     ,(concatenate 'string "Toggle " string-form " mode.")
     (if (mode-enabled-p (drei-instance) ',mode-name)
         (disable-mode (drei-instance) ',mode-name)
         (enable-mode (drei-instance) ',mode-name))))
