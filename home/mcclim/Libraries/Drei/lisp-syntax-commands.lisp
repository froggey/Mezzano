;;; -*- Mode: Lisp; Package: DREI-LISP-SYNTAX; -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;
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

;;; Commands specific to the Lisp syntax for Drei.

(in-package :drei-lisp-syntax)

;;; This command table is used when Drei runs as a pane.
(make-command-table 'pane-lisp-table
                    :errorp nil)

(defmethod additional-command-tables append ((drei drei-pane) (command-table lisp-table))
  '(pane-lisp-table))

;; Movement commands.
(drei-commands:define-motion-commands expression lisp-table)
(drei-commands:define-motion-commands definition lisp-table)
(drei-commands:define-motion-commands up lisp-table
  :noun "nesting level up"
  :plural "levels")
(drei-commands:define-motion-commands down lisp-table
  :noun "nesting level down"
  :plural "levels")
(drei-commands:define-motion-commands list lisp-table)

(drei-commands:define-editing-commands expression lisp-table)
(drei-commands:define-deletion-commands expression lisp-table)

(define-command (com-fill-paragraph :name t :command-table lisp-table) 
    ()
  "Fill paragraph at point. Will have no effect unless there is a
string at point."
  (let* ((token (form-around (current-syntax) (offset (point))))
         (fill-column (auto-fill-column (current-view))))
    (when (form-string-p token)
      (with-accessors ((offset1 start-offset) 
                       (offset2 end-offset)) token
        (fill-region (make-buffer-mark (current-buffer) offset1 :right)
                     (make-buffer-mark (current-buffer) offset2 :right)
                     #'(lambda (mark)
                         (proper-line-indentation (current-view) mark))
                     fill-column
                     (tab-space-count (current-view))
                     (current-syntax)
                     t)))))

(define-command (com-indent-expression :name t :command-table lisp-table)
    ((count 'integer :prompt "Number of expressions" :default 1))
  (let ((mark (clone-mark (point))))
    (if (plusp count)
        (loop repeat count do (forward-expression mark (current-syntax)))
        (loop repeat (- count) do (backward-expression mark (current-syntax))))
    (indent-region (current-view) (point) mark)))

(define-command (com-lookup-arglist-for-this-symbol :command-table lisp-table)
    ()
  "Show argument list for symbol at point."
  (let* ((token (this-form (current-syntax) (point))))
    (if (and token (form-token-p token))
        (com-lookup-arglist (form-to-object (current-syntax) token))
        (display-message "Could not find symbol at point."))))

(define-command (com-lookup-arglist :name t :command-table lisp-table)
    ((symbol 'symbol :prompt "Symbol"))
  "Show argument list for a given symbol."
  (show-arglist (current-syntax) symbol))

(define-command (com-self-insert-then-arglist :command-table lisp-table)
    ()
  "Insert the gesture used to invoke this command and display
argument hints in the minibuffer."
  (insert-character *current-gesture*)
  (show-arglist-for-form-at-mark (point) (current-syntax))
  (clear-completions))

(define-command (com-newline-indent-then-arglist :command-table lisp-table) ()
  "Inserts a newline, indents the new line, then displays
argument hints in the minibuffer."
  (insert-object (point) #\Newline)
  (indent-current-line (current-view) (point))
  (show-arglist-for-form-at-mark (point) (current-syntax)))

(define-command (com-complete-symbol :name t :command-table lisp-table)
    ()
  "Attempt to complete the symbol at mark. If successful, move point
to end of symbol.  

If more than one completion is available, a list of possible
completions will be displayed. If there is no symbol at mark, all
relevant symbols accessible in the current package will be
displayed."
  (complete-symbol-at-mark (current-syntax) (point)))

(define-command (com-fuzzily-complete-symbol :name t :command-table lisp-table)
    ()
  "Attempt to fuzzily complete the abbreviation at mark.

Fuzzy completion tries to guess which symbol is abbreviated. If
the abbreviation is ambiguous, a list of possible completions
will be displayed. If there is no symbol at mark, all relevant
symbols accessible in the current package will be displayed."
  (fuzzily-complete-symbol-at-mark (current-syntax) (point)))

(define-command (com-indent-line-and-complete-symbol :name t :command-table lisp-table) ()
  "Indents the current line and performs symbol completion.
First indents the line.  If the line was already indented,
completes the symbol.  If there's no symbol at the point, shows
the arglist for the most recently enclosed operator."
  (let ((old-offset (offset (point))))
    (indent-current-line (current-view) (point))
    (when (= old-offset
             (offset (point)))
      (or (complete-symbol-at-mark (current-syntax) (point) nil)
          (show-arglist-for-form-at-mark (point) (current-syntax))))))

(define-presentation-to-command-translator lookup-symbol-arglist
    (symbol com-lookup-arglist lisp-table
            :gesture :describe
            :documentation "Lookup arglist")
    (object)
  (list object))

(define-command (com-eval-region :name t :command-table pane-lisp-table)
    ()
  "Evaluate the current region."
  (let ((mark (mark))
        (point (point)))
    (when (mark> mark point)
      (rotatef mark point))
    (eval-region mark point (current-syntax))))

(define-command (com-eval-last-expression :name t :command-table pane-lisp-table)
    ((insertp 'boolean :prompt "Insert?" :default nil))
  "Evaluate the expression before point in the local Lisp image."
  (let ((token (form-before (current-syntax) (offset (point)))))
    (if token
        (with-syntax-package ((current-syntax) (point))
          (let ((*read-base* (base (current-syntax))))
            (drei-commands::com-eval-expression
             (form-to-object (current-syntax) token :read t)
             insertp)))
        (display-message "Nothing to evaluate."))))

(define-command (com-eval-defun :name t :command-table pane-lisp-table) ()
  (eval-defun (point) (current-syntax)))

(define-command (com-remove-definition :name t :command-table lisp-table)
    ()
  "Remove the definition point is in.

The operator of the definition form will be used to determine
what kind of definition it is. The user will be asked for
confirmation before anything is actually done."
  (let ((definition-form (definition-at-mark (current-syntax) (point))))
    (if (or (null definition-form)
            (mark> (point) (end-offset definition-form))
            (mark< (point) (start-offset definition-form)))
        (display-message "No definition found at point.")
        (handler-case
            (let* ((definition-type (form-to-object (current-syntax)
                                                    (form-operator definition-form)))
                   (undefiner (get-undefiner definition-type)))
              (if (null undefiner)
                  (display-message "Doesn't know how to undefine ~S." definition-type)
                  (handler-case
                      (when (accept 'boolean
                             :prompt (format nil "Undefine the ~A ~S?"
                                             (undefiner-type undefiner)
                                             (definition-name undefiner (current-syntax) definition-form))
                             :default t :insert-default t)
                        (undefine undefiner (current-syntax) definition-form))
                    (form-conversion-error (e)
                      (display-message "Could not undefine ~S form: ~A" definition-type (problem e))))))
          (form-conversion-error (e)
            (display-message "Couldn't turn \"~A\" into valid operator: ~A"
                             (form-string (current-syntax) (form e)) (problem e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gesture bindings

(set-key 'com-fill-paragraph
         'lisp-table
         '((#\q :meta)))

(set-key `(com-indent-expression ,*numeric-argument-marker*)
         'lisp-table
         '((#\q :meta :control)))

(set-key `(com-backward-up ,*numeric-argument-marker*)
         'lisp-table
         '((#\u :control :meta)))

(set-key `(com-forward-down ,*numeric-argument-marker*)
         'lisp-table
         '((#\d :control :meta)))

(set-key `(com-backward-expression ,*numeric-argument-marker*)
         'lisp-table
         '((#\b :control :meta)))

(set-key `(com-forward-expression ,*numeric-argument-marker*)
         'lisp-table
         '((#\f :control :meta)))

(set-key `(com-backward-definition ,*numeric-argument-marker*)
         'lisp-table
         '((#\a :control :meta)))

(set-key `(com-forward-definition ,*numeric-argument-marker*)
         'lisp-table
         '((#\e :control :meta)))

(set-key `(com-forward-list ,*numeric-argument-marker*)
         'lisp-table
         '((#\n :control :meta)))

(set-key `(com-backward-list ,*numeric-argument-marker*)
         'lisp-table
         '((#\p :control :meta)))

(set-key `(com-kill-expression ,*numeric-argument-marker*)
         'lisp-table
         '((#\k :control :meta)))

(set-key 'com-lookup-arglist-for-this-symbol
         'lisp-table
         '((#\c :control) (#\d :control) (#\a)))

(set-key 'com-self-insert-then-arglist
         'lisp-table
         '((#\Space)))

(set-key 'com-self-insert-then-arglist
         'lisp-table
         '((#\))))

(set-key 'com-complete-symbol
         'lisp-table
         '((#\Tab :meta)))

(set-key 'com-fuzzily-complete-symbol
         'lisp-table
         '((#\c :control) (#\i :meta)))

(set-key 'com-indent-line-and-complete-symbol
         'lisp-table
         '((#\Tab)))

(set-key 'com-newline-indent-then-arglist
         'lisp-table
         '(#\Newline))

(set-key 'com-eval-region
         'pane-lisp-table
         '((#\c :control) (#\r :control)))

(set-key `(com-eval-last-expression ,*numeric-argument-marker*)
         'pane-lisp-table
         '((#\c :control) (#\e :control)))

(set-key `(com-backward-kill-expression ,*numeric-argument-marker*)
	 'lisp-table
	 '((#\Backspace :control :meta)))

(set-key `(com-kill-expression ,*numeric-argument-marker*)
	 'lisp-table
	 '((#\Rubout :control :meta)))

(set-key 'com-remove-definition
         'lisp-table
         '((#\c :control) (#\u :control)))
