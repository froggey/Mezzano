;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006-2007 by
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
;;; Drei is an editing substrate designed for use in CLIM, and used to
;;; implement text-editor gadgets and input-editing-streams in
;;; McCLIM. It is also used as the editor engine in Climacs, from
;;; which Drei was originally extracted. At it's base, Drei has a
;;; `drei' class that contains the buffer and some marks into the
;;; buffer - from this, we derive concrete Drei implementations that
;;; implement usage-dependent semantics for redisplay and input
;;; handling. The essense of Drei is that a set of protocols can be
;;; used to define editing commands and functionality that can be used
;;; in all Drei derivatives, from Climacs to the tiniest of
;;; input-fields, and hence make it as easy for the user (and hacker)
;;; to customize every text-editing task in the CLIM environment, as
;;; it is to customize Emacs.
;;;
;;; In this file, we wrap all the various bits and parts together and
;;; build the basic Drei primitives, such as the buffer and the
;;; abstract `drei' class.
;;;
;;; Officially, Drei stands for "Drei Replaces EINE's Inheritor", but
;;; there are alternatives:
;;;
;;; * Drei Reimplements Emacs Intelligently
;;;
;;; * Drei Reimplements Emacs' Internals
;;;
;;; * Drei Raises Engineer's Interest
;;;
;;; * Drei Revives Eremites' Interest
;;;
;;; * Drei Recursively Expands Itself
;;;
;;; * Drei Erhbar Emacs Ist

(in-package :drei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience stuff.

(defgeneric drei-instance-of (object)
  (:documentation "Return the Drei instance of `object'. For an
editor frame, this would be the active editor instance. If
`object' itself is a Drei instance, this function should just
return `object'."))

(defun drei-instance (&optional (object *esa-instance*))
  "Return the Drei instance of `object'. If `object' is not
provided, the currently running Drei instance will be returned."
  (drei-instance-of object))

(defun (setf drei-instance) (new-instance &optional (object *esa-instance*))
  (setf (drei-instance-of object) new-instance))

(defun current-view (&optional (object (drei-instance)))
  "Return the view of the provided object. If no object is
provided, the currently running Drei instance will be used."
  (view object))

(defun (setf current-view) (new-view &optional (object (drei-instance)))
  (setf (view object) new-view))

(defun point (&optional (object (current-view)))
  "Return the point of the provided object. If no object is
provided, the current view will be used."
  (point-of object))

(defun (setf point) (new-point object)
  (setf (point-of object) new-point))

(defgeneric point-of (object)
  (:documentation "Return the mark object that is the point of
`object'. Some objects have their own points, for example Drei
buffer-views and buffers."))

(defun mark (&optional (object (current-view)))
  "Return the mark of the provided object. If no object is
provided, the current view will be used."
  (mark-of object))

(defun (setf mark) (new-mark object)
  (setf (mark-of object) new-mark))

(defgeneric mark-of (object)
  (:documentation "Return the mark object that is the mark of
`object'. Some objects have their own points, for example Drei
instances."))

(defun current-syntax ()
  "Return the syntax of the current buffer."
  (syntax (current-view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Isearch

(defclass isearch-state ()
  ((search-string :initarg :search-string :accessor search-string)
   (search-mark :initarg :search-mark :accessor search-mark)
   (search-forward-p :initarg :search-forward-p :accessor search-forward-p)
   (search-success-p :initarg :search-success-p :accessor search-success-p)
   (targets :initarg :targets :accessor targets )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Query replace

(defclass query-replace-state ()
  ((string1 :initarg :string1 :accessor string1)
   (string2 :initarg :string2 :accessor string2)
   (targets :initarg :targets :accessor targets)
   (occurences :initform 0 :accessor occurrences)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drei command tables.

;;; Commenting.
(make-command-table 'comment-table :errorp nil)
;;; Deleting.
(make-command-table 'deletion-table :errorp nil)
;;; Editing - making changes to a buffer.
(make-command-table 'editing-table :errorp nil)
;;; Filling.
(make-command-table 'fill-table :errorp nil)
;;; Dealing with charcase.
(make-command-table 'case-table :errorp nil)
;;; Indentation.
(make-command-table 'indent-table :errorp nil)
;;; Marking things.
(make-command-table 'marking-table :errorp nil)
;;; Moving around.
(make-command-table 'movement-table :errorp nil)
;;; Searching.
(make-command-table 'search-table :errorp nil)
;;; Information about buffer contents.
(make-command-table 'info-table :errorp nil)
;;; Self-insertion.
(make-command-table 'self-insert-table :errorp nil)
;;; View stuff (scrolling, etc)
(make-command-table 'view-table :errorp nil)

;;; Command table for concrete editor stuff.
(define-syntax-command-table editor-table
    :errorp nil
    :inherit-from '(comment-table
                    deletion-table
                    editing-table
                    case-table
                    fill-table
                    indent-table
                    marking-table
                    movement-table
                    search-table
                    info-table
                    self-insert-table
                    keyboard-macro-table)
    :menu '(("Commenting" :menu comment-table)))

;; Command table for commands that are only available when Drei is a
;; gadget. There is no pane-exclusive table because the Drei pane is
;; not meant to be used as-is, but is meant to be subclassed, so we do
;; not want to force users to work around too much default behavior.
(make-command-table 'exclusive-gadget-table :errorp nil)

;; Command table for input-editor-only commands.
(make-command-table 'exclusive-input-editor-table :errorp nil)

(define-command (com-drei-extended-command :command-table exclusive-gadget-table)
    ()
  "Prompt for a command name and arguments, then run it."
  (require-minibuffer)
  (let ((item (handler-case
                  (accept
                   `(command :command-table ,(esa-command-table (drei-instance)))
                   ;; this gets erased immediately anyway
                   :prompt "" :prompt-mode :raw)
                ((or command-not-accessible command-not-present) ()
                  (beep)
                  (display-message "No such command")
                  (return-from com-drei-extended-command nil)))))
    (execute-drei-command (drei-instance) item)))

(set-key 'com-drei-extended-command
         'exclusive-gadget-table
         '((#\x :meta)))

(defclass drei-command-table (standard-command-table)
  ()
  (:documentation "This class is used to provide the kind of
indirection we need to support syntax-specific command tables in
Drei. Commands should *NOT* be added to it."))

(defmethod additional-command-tables append ((frame application-frame)
                                             (command-table syntax-command-table))
  "This method allows users of Drei to extend syntaxes with new,
app-specific commands, as long as they inherit from a Drei class
and specialise a method for it."
  (additional-command-tables (drei-instance) command-table))

(defmethod command-table-inherit-from ((table drei-command-table))
  (append (view-command-tables (current-view))
          (additional-command-tables (drei-instance) table)
          (when (use-editor-commands-p (current-view))
            '(editor-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The basic Drei class.

(defclass drei ()
  ((%view :initarg :view
          :accessor view
          :documentation "The CLIM view that will be used
whenever this Drei is being displayed. During redisplay, the
`stream-default-view' of the output stream will be temporarily
bound to this value.")
   (%previous-command :initform nil
                      :accessor previous-command
                      :documentation "The previous CLIM command
executed by this Drei instance. May be NIL if no command has been
executed.")
   (%editor-pane :reader editor-pane
                 :initarg :editor-pane
                 :type (or null clim-stream-pane)
                 :initform nil
                 :documentation "The stream or pane that the Drei
instance will perform output to.")
   (%minibuffer :initform nil
                :accessor minibuffer
                :initarg :minibuffer
                :type (or minibuffer-pane pointer-documentation-pane null)
                :documentation "The minibuffer pane (or null)
associated with the Drei instance. This may be NIL.")
   (%command-table :initform (make-instance 'drei-command-table
                                            :name 'drei-dispatching-table)
                   :reader esa-command-table
                   :initarg :command-table
                   :type standard-command-table
                   :documentation "The command table used for
looking up commands for the Drei instance. Has a sensible
default, don't override it unless you know what you are doing.")
   (%cursors :accessor cursors
             :initform '()
             :documentation "A list of which cursors are
associated with the Drei instance. During redisplay,
`display-drei-view-cursor' is called on each element of this
list.")
   (%point-cursor :accessor point-cursor
                  :documentation "The cursor object that is
considered the primary user-oriented cursor, most probably the
cursor for the editor point. Note that this cursor is also in the
cursors-list.")
   (%cursors-visible :accessor cursors-visible
                     :initform t
                     :initarg :cursors-visible
                     :documentation "If true, the cursors of this
Drei instance will be visible. If false, they will not.")
   (%isearch-mode :initform nil :accessor isearch-mode)
   (%isearch-states :initform '() :accessor isearch-states)
   (%isearch-previous-string :initform nil :accessor isearch-previous-string)
   (%query-replace-mode :initform nil :accessor query-replace-mode)
   (%query-replace-state :initform nil :accessor query-replace-state))
  (:metaclass modual-class)
  (:default-initargs :active t :editable-p t)
  (:documentation "The abstract Drei class that maintains
standard Drei editor state. It should not be directly
instantiated, a subclass implementing specific behavior (a Drei
variant) should be used instead."))

(defmethod active ((drei drei))
  "Return true if `drei' is active. A drei instance is active if
its view is active."
  (active (view drei)))

(defmethod (setf active) (new-val (drei drei))
  (setf (active (view drei)) new-val))

(defmethod (setf cursors-visible) :after (new-val (drei drei))
  (dolist (cursor (cursors drei))
    (setf (enabled cursor) new-val)))

(defmethod available-modes append ((modual drei))
  (available-modes (view modual)))

(defmethod mode-applicable-p or ((modual drei) mode-name)
  (mode-applicable-p (view modual) mode-name))

(defmethod mode-enabled-p or ((modual drei) mode-name)
  (mode-enabled-p (view modual) mode-name))

(defmethod enable-mode ((modual drei) mode-name &rest initargs)
  (if (mode-applicable-p (view modual) mode-name)
      (apply #'enable-mode (view modual) mode-name initargs)
      (call-next-method)))

(defmethod disable-mode ((modual drei) mode-name)
  (if (mode-applicable-p (view modual) mode-name)
      (disable-mode (view modual) mode-name)
      (call-next-method)))

(defun add-view-cursors (drei)
  "Add the cursors desired by the Drei view to the editor-pane of
the Drei instance."
  (setf (cursors drei) (nreverse (create-view-cursors (editor-pane drei) (view drei))))
  (dolist (cursor (cursors drei))
    (setf (enabled cursor) (cursors-visible drei)))
  ;; We define the point cursor to be the first point-cursor object
  ;; in the list of cursors.
  (setf (point-cursor drei)
        (find-if #'(lambda (cursor)
                     (typep cursor 'point-cursor))
                 (cursors drei))))

(defmethod initialize-instance :after ((drei drei) &rest args &key
                                       view active single-line (editable-p t)
                                       no-cursors initial-contents)
  (declare (ignore args))
  (unless view             ; Unless a view object has been provided...
    ;; Create it with the provided initargs.
    (setf (view drei) (make-instance 'textual-drei-syntax-view
                       :active active
                       :single-line single-line
                       :read-only (not editable-p)
                       :no-cursors no-cursors
                       :initial-contents initial-contents)))
  (add-view-cursors drei))

(defmethod (setf view) :after (new-val (drei drei))
  ;; Delete the old cursors, then add the new ones, provided the
  ;; setting of the view is successful.
  (dolist (cursor (cursors drei))
    (when (output-record-parent cursor)
      (delete-output-record cursor (output-record-parent cursor) nil)))
  (add-view-cursors drei)
  ;; Finally make sure it doesn't remember anything from a potential
  ;; traumatic past.
  (clear-redisplay-information new-val))

(defmethod esa-current-buffer ((drei drei))
  (buffer (view drei)))

(defmethod esa-current-window ((drei drei))
  drei)

(defmethod drei-instance-of ((object drei))
  object)

(defmethod print-object ((object drei) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (type-of (view object)))))

(defmethod command-for-unbound-gestures ((drei drei) gestures)
  (command-for-unbound-gestures (view drei) gestures))

;; Main redisplay entry point.
(defgeneric display-drei (drei &key redisplay-minibuffer)
  (:documentation "`Drei' must be an object of type `drei' and
`frame' must be a CLIM frame containing the editor pane of
`drei'. If you define a new subclass of `drei', you must define a
method for this generic function. In most cases, methods defined
on this function will merely be a trampoline to a function
specific to the given Drei variant.

If `redisplay-minibuffer' is true, also redisplay `*minibuffer*'
if it is non-NIL."))

(define-condition no-available-minibuffer (user-condition-mixin error)
  ((%drei :reader drei
          :initarg :drei
          :initform (error "A drei instance must be provided")
          :documentation "The Drei instance that does not have an
available minibuffer."))
  (:documentation "This error is signalled when a command wants
to use the minibuffer, but none is available."))

(defun no-available-minibuffer (drei-instance)
  "Signal an `no-available-minibuffer' error for
`drei-instance'."
  (error 'no-available-minibuffer :drei drei-instance))

(defun require-minibuffer (&optional (drei-instance (drei-instance)))
  "Check that the provided Drei instance (defaulting to the one
currently running) has an available minibuffer. If not, signal an
error of type `no-available-minibuffer'."
  (unless *minibuffer*
    (no-available-minibuffer drei-instance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some standard building block machinery.

(defgeneric handle-drei-condition (drei condition)
  (:documentation "When an unhandled condition that is a subtype
of `user-condition-mixin' (and some other hardcoded condition
types) is signalled during execution of a Drei command, this
generic function will be called with the Drei instance as the
first argument, and the condition as the second argument."))

(defmethod handle-drei-condition (drei (condition offset-before-beginning))
  (beep) (display-message "Beginning of buffer"))

(defmethod handle-drei-condition (drei (condition offset-after-end))
  (beep) (display-message "End of buffer"))

(defmethod handle-drei-condition (drei (condition motion-before-beginning))
  (beep) (display-message "Beginning of buffer"))

(defmethod handle-drei-condition (drei (condition motion-after-end))
  (beep) (display-message "End of buffer"))

(defmethod handle-drei-condition (drei (condition no-such-operation))
  (beep) (display-message "Operation unavailable for syntax"))

(defmethod handle-drei-condition (drei (condition buffer-read-only))
  (beep) (display-message "Buffer is read only"))

(defmethod handle-drei-condition (drei (condition user-condition-mixin))
  (beep) (with-minibuffer-stream (minibuffer)
           (let ((*print-escape* nil))
             (princ condition minibuffer))))

(defmacro handling-drei-conditions (&body body)
  "Evaluate `body' while handling Drei user notification
signals. The handling consists of displaying their meaning to the
user in the minibuffer. This is the macro that ensures conditions
such as `motion-before-end' does not land the user in the
debugger."
  ;; Perhaps a DREI-CONDITION class should be added so we could more
  ;; easily catch all these. `User-condition-mixin' isn't available
  ;; at, for example, the buffer level, after all.
  `(handler-case (progn ,@body)
     (user-condition-mixin (c)
       (handle-drei-condition (drei-instance) c))
     (offset-before-beginning (c)
       (handle-drei-condition (drei-instance) c))
     (offset-after-end (c)
       (handle-drei-condition (drei-instance) c))
     (motion-before-beginning (c)
       (handle-drei-condition (drei-instance) c))
     (motion-after-end (c)
       (handle-drei-condition (drei-instance) c))
     (no-such-operation (c)
       (handle-drei-condition (drei-instance) c))))

(defun find-available-minibuffer (drei-instance)
  "Find a pane usable as the minibuffer for `drei-instance'. The
default will be to use the minibuffer specified for
`drei-instance' (if there is one), secondarily the value of
`*minibuffer*' will be used. Thirdly, the value of
`*pointer-documentation-output*' will be used. If the found panes
are not available (for example, if they are the editor-panes of
`drei-instance'), it is possible for this function to return
NIL."
  (flet ((available-minibuffer-p (pane)
           (and (or (typep pane 'minibuffer-pane)
                    (typep pane 'pointer-documentation-pane))
                (not (eq pane (editor-pane drei-instance))))))
    (find-if #'available-minibuffer-p
             (list (minibuffer drei-instance)
                   *minibuffer*
                   *pointer-documentation-output*))))

(defmacro with-bound-drei-special-variables ((drei-instance &key
                                                            (kill-ring nil kill-ring-p)
                                                            (minibuffer nil minibuffer-p)
                                                            (command-parser nil command-parser-p)
                                                            (partial-command-parser nil partial-command-parser-p)
                                                            (previous-command nil previous-command-p)
                                                            (prompt nil prompt-p))
                                             &body body)
  "Evaluate `body' with a set of Drei special
variables (`(drei-instance)', `*kill-ring*', `*minibuffer*',
`*command-parser*', `*partial-command-parser*',
`*previous-command*', `*extended-command-prompt*') bound to their
proper values, taken from `drei-instance'. The keyword arguments
can be used to provide forms that will be used to obtain values
for the respective special variables, instead of finding their
value in `drei-instance' or their existing binding. This macro
binds all of the usual Drei special variables, but also some CLIM
special variables needed for ESA-style command parsing."
  `(let* ((*esa-instance* ,drei-instance)
          (*kill-ring* ,(if kill-ring-p kill-ring
                            '*kill-ring*))
          (*minibuffer* ,(if minibuffer-p minibuffer
                             `(find-available-minibuffer (drei-instance))))
          (*command-parser* ,(if command-parser-p command-parser
                                 ''esa-command-parser))
          (*partial-command-parser* ,(if partial-command-parser-p partial-command-parser
                                         ''esa-partial-command-parser))
          (*previous-command* ,(if previous-command-p previous-command
                                   `(previous-command (drei-instance))))
          (*extended-command-prompt* ,(if prompt-p prompt
                                          "Extended command: "))
          (*standard-input* (or *minibuffer* *standard-input*)))
     ,@body))

(defgeneric invoke-performing-drei-operations (drei continuation &key with-undo redisplay)
  (:documentation "Invoke `continuation', setting up and
performing the operations specified by the keyword arguments for
the given Drei instance."))

(defmethod invoke-performing-drei-operations ((drei drei) (continuation function)
                                              &key with-undo (redisplay t))
  (with-accessors ((buffer buffer)) (view drei)
    (with-undo ((when with-undo (list buffer)))
      (funcall continuation))
    (unless with-undo
      (clear-undo-history buffer))
    (when redisplay
      (etypecase drei
        (pane
         (redisplay-frame-pane *application-frame* drei))
        (t
         (display-drei drei :redisplay-minibuffer t))))))

(defmacro performing-drei-operations ((drei &rest args &key with-undo
                                            (redisplay t))
                                      &body body)
  "Provide various Drei maintenance services around the
evaluation of `body'. This macro provides a convenient way to
perform some operations on a Drei, and make sure that they are
properly reflected in the undo tree, that the Drei is
redisplayed, the syntax updated, etc. Exactly what is done can be
controlled via the keyword arguments. Note that if `with-undo' is
false, the *entire* undo history will be cleared after `body' has
been evaluated. This macro expands into a call to
`invoke-performing-drei-operations'."
  (declare (ignore with-undo redisplay))
  `(invoke-performing-drei-operations ,drei (lambda ()
                                              ,@body)
                                      ,@args))

(defmacro with-drei-options ((drei &key
                                   (syntax nil syntax-provided-p)
                                   keep-syntax)
                             &body body)
  "Evaluate `body' with the Drei instance `drei' changed to
reflect the given options. The Drei instance will revert to the
original options after `body' has been evaluated."
  ;; Build a list consisting of lists of three elements, the first
  ;; element being how to save the old value, the second element being
  ;; how to set the new value, the third element being how to restore
  ;; the old value.
  (once-only (drei syntax)
    (let (triple-list)
      (when syntax-provided-p
        (push (list (unless keep-syntax
                      `(old-syntax (syntax (view ,drei))))
                    `(progn (setf (syntax (view ,drei))
                                  (etypecase ,syntax
                                    (string (make-syntax-for-view (view ,drei)
                                             (or (syntax-from-name ,syntax)
                                                 (error "No such syntax: ~A" ,syntax))))
                                    (symbol (make-syntax-for-view (view ,drei) ,syntax))
                                    (syntax ,syntax))))
                    (unless keep-syntax
                      `(progn (setf (syntax (view ,drei)) old-syntax))))
              triple-list))
      `(progn
         (check-type ,drei drei)
         (let ,(remove-if #'null (mapcar #'first triple-list))
           ,@(remove-if #'null (mapcar #'second triple-list))
           (unwind-protect (progn ,@body)
             ,@(remove-if #'null (mapcar #'third triple-list))))))))

(defgeneric invoke-accepting-from-user (drei continuation)
  (:documentation "Set up `drei' and the environment so that
calls to `accept' will behave properly. Then call
`continuation'."))

(defmethod invoke-accepting-from-user ((drei drei) (continuation function))
  ;; By default, everything should work.
  (funcall continuation))

(defmacro accepting-from-user ((drei) &body body)
  "Modidfy `drei' and the environment so that calls to `accept'
can be done to arbitrary streams from within `body'. Or, at
least, make sure the Drei instance will not be a problem. When
Drei calls a command, it will be wrapped in this macro, so it
should be safe to use `accept' within Drei commands. This macro
expands into a call to `invoke-accepting-from-user'."
  `(invoke-accepting-from-user ,drei #'(lambda () ,@body)))

;;; Plain `execute-frame-command' is not good enough for us. Our
;;; event-handler method uses this function to invoke commands.
(defgeneric execute-drei-command (drei-instance command)
  (:documentation "Execute `command' for `drei'. This is the
standard function for executing Drei commands - it will take care
of reporting to the user if a condition is signalled, updating
the syntax, setting the `previous-command' of `drei' and
recording the operations performed by `command' for undo."))

(defmethod execute-drei-command ((drei drei) command)
  (performing-drei-operations (drei :redisplay nil
                                    :with-undo t)
    (handling-drei-conditions
      (apply (command-name command) (command-arguments command)))
    (setf (previous-command drei) command)))
