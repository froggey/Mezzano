;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)

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

(in-package :clim-internals)

;;; Commands and presentation translators that live in the
;;; global-command-table.

;;; Global help command
(define-command (com-null-command :command-table global-command-table :name nil)
    ()
  nil)

(define-command (com-help :command-table global-command-table :name "Help")
    ((kind '(completion (("Keyboard" keyboard) ("Commands" commands))
             :value-key cadr)
             :prompt "with"
             :default 'keyboard
             :display-default nil))
  (if (eq kind 'keyboard)
      (format *query-io* "Input editor commands are like Emacs.~%")
      (let ((command-table (frame-command-table *application-frame*))
            (command-names nil))
        (map-over-command-table-names #'(lambda (name command)
                                          (push (cons name command)
                                                command-names))
                                      command-table)
        (setf command-names (remove-duplicates command-names :key #'cdr))
        (setf command-names (sort command-names #'(lambda (a b)
                                                    (string-lessp (car a)
                                                                  (car b)))))
        (formatting-item-list (*query-io*)
          (loop
             for (nil . command) in command-names
             do (formatting-cell (*query-io*)
                 (present command
                          `(command-name :command-table ,command-table)
                          :stream *query-io*)))))))


;;; Describe command.  I don't know if this should go in the global command
;;; table, but we don't exactly have a surplus of commands yet...
(define-command
    (com-describe :command-table global-command-table :name "Describe")
    ((obj 'expression
          :prompt "object"
          :gesture :describe))
  (describe obj *query-io*))

;;; Another somewhat gratuitous command...
(define-gesture-name :describe-presentation :pointer-button-press
  (:left :super))

;;; The argument obj is not really the presentation object but the
;;; presentation itself as supplied by the translator.
(define-command (com-describe-presentation :command-table global-command-table)
    ((obj t))
  (com-describe obj))

(define-presentation-to-command-translator com-describe-presentation-translator
    (t com-describe-presentation global-command-table
     :gesture :describe-presentation
     :tester ((object presentation)
              (declare (ignore object))
              (not (eq presentation *null-presentation*)))
     :documentation "Describe Presentation"
     :pointer-documentation "Describe Presentation"
     :menu presentation-debugging)
    (object presentation)
  (list presentation))

;;; Default presentation translator; translates an object to itself.

(define-presentation-translator default-translator
    (t nil global-command-table
     :gesture :select
     :tester ((object presentation context-type)
              ;; see the comments around DEFUN PRESENTATION-SUBTYPEP
              ;; for some of the logic behind this.  Only when
              ;; PRESENTATION-SUBTYPEP is unsure do we test the object
              ;; itself for PRESENTATION-TYPEP.
              (declare (ignore object))
              (multiple-value-bind (yp sp)
                  (presentation-subtypep (presentation-type presentation)
                                         context-type)
                (or yp (not sp))))
     :tester-definitive nil
     :menu nil
     :documentation
     ((object presentation context-type frame event window x y stream)
      (let* ((type (presentation-type presentation))
             (options (decode-options type))
             (description (getf options :description)))
        (if description
            (if (stringp description)
                (princ description stream)
                (funcall description object
                         :presentation presentation
                         :context-type context-type
                         :frame frame
                         :event event
                         :window window
                         :x x :y y
                         :stream stream))
            (present object (presentation-type presentation)
                     :stream stream
                     :sensitive nil)))))
  (object presentation)
  ;; returning (PRESENTATION-TYPE PRESENTATION) as the ptype is
  ;; formally undefined, as this means that the translator returns a
  ;; presentation type which is not PRESENTATION-SUBTYPEP the
  ;; translator's TO-TYPE.
  (values object (presentation-type presentation)))

(define-presentation-action presentation-menu
    (t nil global-command-table
       :documentation "Menu"
       :menu nil
       :gesture :menu
       :tester ((object presentation frame window x y event)
                (declare (ignore object))
                (find-applicable-translators presentation
                                             *input-context* ; XXX ?
                                             frame window x y
                                             :event event ; XXX ?
                                             :for-menu t
                                             :fastp t)))
    (object presentation frame window x y)
  (declare (ignore object))
  (call-presentation-menu presentation *input-context*
                          frame window x y
                          :for-menu t
                          :label (format nil "Operation on ~A"
                                         (presentation-type presentation))))

;;; Action for possibilities menu of complete-input
;;;
;;; XXX The context type needs to change to COMPLETER or something so that this
;;; isn't applicable all over the place.
(define-presentation-action possibilities-menu
    (blank-area nil global-command-table
     :documentation "Possibilities menu for completion"
     :pointer-documentation "Possibilities"
     :menu nil
     :gesture :menu
     :tester ((object)
              (declare (ignore object))
              *completion-possibilities-continuation*))
    (object)
  (declare (ignore object))
  (funcall *completion-possibilities-continuation*))

;;; Turn symbols and lists into forms
(define-gesture-name :literal-expression :pointer-button-press
  (:left :meta))

(defun document-form-translator (object &key stream &allow-other-keys)
  (unless (constantp object)
    (write-char #\' stream))
  (present object 'form :stream stream))

(macrolet ((%frob-exp (type-name)
             (let ((expression-translator-name (symbol-concat
                                                type-name
                                                '-to-expression)))
               `(define-presentation-translator ,expression-translator-name
                    (,type-name expression global-command-table
                                :gesture :select
                                :menu nil)
                  (object)
                  object)))
           (%frob-constant-form (type-name)
             (let ((form-translator-name (symbol-concat type-name '-to-form)))
              `(define-presentation-translator ,form-translator-name
                   (,type-name form global-command-table
                               :gesture :select
                               :menu nil
                               :documentation document-form-translator)
                 (object)
                 object)))
           (%frob-form (type-name)
             (let ((form-translator-name (symbol-concat type-name '-to-form)))
               `(define-presentation-translator ,form-translator-name
                    (,type-name form global-command-table
                                :gesture :select
                                :menu nil
                                :documentation document-form-translator)
                  (object)
                  (if (constantp object)
                      object
                      `',object))))

           (frob (type-name)
             `(progn
                (%frob-exp ,type-name)
                (%frob-constant-form ,type-name)))
           (frob-form (type-name)
             `(progn
                (%frob-exp ,type-name)
                (%frob-form ,type-name))))
  (frob null)
  (frob boolean)
  (frob keyword)
  (frob number)
  (frob character)
  (frob string)
  (frob pathname)
  (frob-form symbol)
  (frob-form sequence)
  (frob standard-object))

(define-presentation-translator expression-to-form
    (expression form global-command-table
     :gesture :select
     :menu nil
     :priority 11)
  (object)
  (if (or (consp object) (and (symbolp object) (not (null object))))
      `',object
      object))

;;; I changed :menu nil to :menu t because :literal-expression is hard for me
;;; to type on my Mac :) I'm not sure why I excluded this from the menu
;;; originally.
(define-presentation-translator expression-as-form
    (expression form global-command-table
     :gesture :literal-expression
     :menu t
     :documentation "expression as literal"
     :tester ((object)
              (or (symbolp object) (consp object)))
     :tester-definitive t)
  (object)
  (values object 'form))

;;; Support for accepting subforms of a form.

;;; Used to signal a read that ends a list
(define-presentation-type list-terminator ()
  :inherit-from 'form)

(defvar *sys-read* #'read)
(defvar *sys-read-preserving-whitespace* #'read-preserving-whitespace)

;;; Arguments for read
(defvar *eof-error-p* t)
(defvar *eof-value* nil)
(defvar *recursivep* nil)

;;; For passing arguments to the call to %read-list-expression.
;;; Gross, but not as gross as using presentation type options.
;;;
;;; XXX But I am using a presentation type option to choice the
;;; subform reader; what's the difference? Granted the presentation
;;;type specifier is constant.... -- moore

(defvar *dot-ok*)
(defvar *termch*)

(defun whitespacep (char)
  (or (char= char #\Space)
      (char= char #\Newline)
      (char= char #\Return)
      (char= char #\Tab)))

#+openmcl
(defvar *sys-%read-list-expression* #'ccl::%read-list-expression)

#+openmcl
(with-system-redefinition-allowed
(defun ccl::%read-list-expression (stream *dot-ok* &optional (*termch* #\)))
  (if (typep stream 'input-editing-stream)
      (progn
        ;; Eat "whitespace" so it is not deleted by presentation-replace-input
        (let ((gesture (read-gesture :stream stream :timeout 0 :peek-p t)))
          (when (and gesture
                     (or (activation-gesture-p gesture)
                         (delimiter-gesture-p gesture)
                         (and (characterp gesture)
                              (whitespacep gesture))))
            (read-gesture :stream stream)))
        (multiple-value-bind (object type)
            (accept '((expression) :subform-read t) :stream stream :prompt nil)
          (values object (if (presentation-subtypep type 'list-terminator)
                             nil
                             t))))
      (funcall *sys-%read-list-expression* stream *dot-ok* *termch*)))
)                                       ; with-system-redefinition-allowed

(define-presentation-method accept ((type expression) stream (view textual-view)
                                    &key)
  (let* ((object nil)
         (ptype nil))
    #.(funcall (if #+openmcl t #-openmcl nil #'identity #'fourth)
               `(if subform-read
                    (multiple-value-bind (val valid)
                        (funcall *sys-%read-list-expression* stream *dot-ok* *termch*)
                      (if valid
                          (setq object val)
                          (return-from accept (values nil 'list-terminator))))
                    ;; We don't want activation gestures like :return causing an eof
                    ;; while reading a form. Also, we don't want spaces within forms or
                    ;; strings causing a premature return either!
                    ;; XXX This loses when rescanning (possibly in other contexts too) an
                    ;; activated input buffer (e.g., reading an expression from the accept
                    ;; method for OR where the previous readers have already given
                    ;; up). We should call *sys-read-preserving-whitespace* and handle the
                    ;; munching of whitespace ourselves according to the
                    ;; PRESERVE-WHITESPACE parameter. Fix after .9.2.2.
                    (with-delimiter-gestures (nil :override t)
                      (with-activation-gestures (nil :override t)
                        (setq object (funcall
                                      (if preserve-whitespace
                                          *sys-read-preserving-whitespace*
                                          *sys-read*)
                                      stream
                                      *eof-error-p* *eof-value* *recursivep*))))))
    (setq ptype (presentation-type-of object))
    (unless (presentation-subtypep ptype 'expression)
      (setq ptype 'expression))
    (if (or subform-read auto-activate)
        (values object ptype)
        (loop
           for c = (read-char stream)
           until (or (activation-gesture-p c) (delimiter-gesture-p c))
           finally
             (when (delimiter-gesture-p c)
               (unread-char c stream))
             (return (values object ptype))))))

(define-presentation-method accept ((type expression)
                                    (stream input-editing-stream)
                                    (view textual-view)
                                    &key)
  ;; This method is specialized to
  ;; input-editing-streams and has thus been
  ;; made slightly more tolerant of input
  ;; errors. It is slightly hacky, but seems
  ;; to work fine.
  (let* ((object nil)
         (ptype nil))
    #.(funcall (if #+openmcl t #-openmcl nil #'identity #'fourth)
               `(if (and #-openmcl nil subform-read)
                    (multiple-value-bind (val valid)
                        (funcall *sys-%read-list-expression* stream *dot-ok* *termch*)
                      (if valid
                          (setq object val)
                          (return-from accept (values nil 'list-terminator))))
                    ;; We don't want activation gestures like :return causing an
                    ;; eof while reading a form. Also, we don't want spaces within
                    ;; forms or strings causing a premature return either!
                    (with-delimiter-gestures (nil :override t)
                      (with-activation-gestures (nil :override t)
                        (setq object
                              ;; We loop in our accept of user input, if a reader
                              ;; error is signalled, we merely ignore it and ask
                              ;; for more input. This is so a single malplaced #\(
                              ;; or #\, won't throw up a debugger with a
                              ;; READER-ERROR and remove whatever the user wrote
                              ;; to the stream.
                              (loop for potential-object =
                                   (handler-case (funcall
                                                  (if preserve-whitespace
                                                      *sys-read-preserving-whitespace*
                                                      *sys-read*)
                                                  stream
                                                  *eof-error-p*
                                                  *eof-value*
                                                  *recursivep*)
                                     ((and reader-error) (e)
                                       (declare (ignore e))
                                       nil))
                                   unless (null potential-object)
                                   return potential-object))))))
    (setq ptype (presentation-type-of object))
    (unless (presentation-subtypep ptype 'expression)
      (setq ptype 'expression))
    (if (or subform-read auto-activate)
        (values object ptype)
        (loop
           for c = (read-char stream)
           until (or (activation-gesture-p c) (delimiter-gesture-p c))
           finally
           (when (delimiter-gesture-p c)
             (unread-char c stream))
           (return (values object ptype))))))


(with-system-redefinition-allowed
    (defun read (&optional (stream *standard-input*)
                   (eof-error-p t)
                   (eof-value nil)
                   (recursivep nil))
      (if (typep stream 'input-editing-stream)
          (let ((*eof-error-p* eof-error-p)
                (*eof-value* eof-value)
                (*recursivep* recursivep))
            (accept '((expression) :auto-activate t :preserve-whitespace nil)
                    :stream stream :prompt nil))
          (funcall *sys-read* stream eof-error-p eof-value recursivep)))

  (defun read-preserving-whitespace (&optional (stream *standard-input*)
                                       (eof-error-p t)
                                       (eof-value nil)
                                       (recursivep nil))
    (if (typep stream 'input-editing-stream)
        (let ((*eof-error-p* eof-error-p)
              (*eof-value* eof-value)
              (*recursivep* recursivep))
          (accept '((expression) :auto-activate t :preserve-whitespace t)
                  :stream stream :prompt nil))
        (funcall *sys-read-preserving-whitespace*
                 stream eof-error-p eof-value recursivep)))
) ; with-system-redefinition-allowed
