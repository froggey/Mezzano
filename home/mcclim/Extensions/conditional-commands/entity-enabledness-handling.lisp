(in-package :conditional-commands)

;;; General note:
;;;    The commands in the slots ENABLED-COMMANDS and DISABLE-COMMANDS
;;;    can not only be names of commands but also names of whole
;;;    command-tables.

;;; Todo:
;;;   - Handle more than one conditional-application-frame
;;;        (As we want to re-establish the state of a previous
;;;         application frame, we cannot simply replace the three
;;;         global variables by slots in the application-frame.)
;;;     Done. (The three global variables are now association lists).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; state variables

(defvar *entity-enabledness-changes* nil
  "holds all defined entity-enabledness-changes")

(defvar *enabled-conditional-commands* nil)
(defvar *enabled-conditional-sheets* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class definition + print-object method

(defclass entity-enabledness-change ()
  ((command                        :initarg :command          :reader entity-enabledness-change-command)
   (enable-commands  :initform nil :initarg :enable-commands  :reader enable-commands)
   (disable-commands :initform nil :initarg :disable-commands :reader disable-commands)
   (enable-sheets    :initform nil :initarg :enable-sheets    :reader enable-sheets)
   (disable-sheets   :initform nil :initarg :disable-sheets   :reader disable-sheets)
   (evaluate-this    :initform nil :initarg :evaluate-this    :reader evaluate-this)
   (change-status    :initform t   :initarg :change-status    :reader change-status)))

(defun slot-value-or-something (object &key (slot 'name) (something "without name"))
  (if (slot-boundp object slot)
      (slot-value object slot)
      something))

(defmethod print-object ((entity-enabledness-change entity-enabledness-change) stream)
  (print-unreadable-object (entity-enabledness-change stream :type t)
    (princ (slot-value-or-something entity-enabledness-change :slot 'command) stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interface functions for an entitiy-enabledness-change
;;; equal-p, remove, add, and find methods

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function 'entity-enabledness-change-equal-p) #'equal))

(defun remove-entity-enabledness-change (command application-frame)
  (let ((changes (assoc application-frame *entity-enabledness-changes*)))
    (when changes
      (setf (cdr changes)
            (delete command (cdr changes)
                    :key #'entity-enabledness-change-command
                    :test #'entity-enabledness-change-equal-p)))))

;;; (setf (symbol-function (intern 'add-entity-enabledness-change)) #'make-instance)
(defun add-entity-enabledness-change (command application-frame &rest arguments-for-make-instance)
  (let ((entity-enabledness-change
         (apply #'make-instance 'entity-enabledness-change :command command arguments-for-make-instance))
        (changes (creating-assoc application-frame *entity-enabledness-changes*)))
    (remove-entity-enabledness-change (entity-enabledness-change-command entity-enabledness-change)
                                      application-frame)
    (push entity-enabledness-change (cdr changes))
    entity-enabledness-change))

(defun find-entity-enabledness-change (command application-frame)
  (find command
        (cdr (assoc application-frame *entity-enabledness-changes*))
        :key #'entity-enabledness-change-command
        :test #'entity-enabledness-change-equal-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; main function: actually "do" an entitiy-enabledness-change

(defun change-entity-enabledness (command &key (debug-output nil))
  (let* ((application-frame (slot-value *application-frame* 'climi::name))
         (entity-enabledness-change (find-entity-enabledness-change command application-frame)))
    (unless entity-enabledness-change
      (error "~&There is no entity-enabledness-change for command ~a" command))

    (with-slots (enable-commands disable-commands
                 enable-sheets   disable-sheets
                 evaluate-this
                 change-status)
        entity-enabledness-change

      (when debug-output
        (format debug-output "~&entity-enabledness-change for command ~a" command)
        (format debug-output "~&entity-enabledness-change: enable  commands: ~a" enable-commands)
        (format debug-output "~&entity-enabledness-change: disable commands: ~a" disable-commands)
        (format debug-output "~&entity-enabledness-change: enable  sheets: ~a" enable-sheets)
        (format debug-output "~&entity-enabledness-change: disable sheets: ~a" disable-sheets)
        (format debug-output "~&entity-enabledness-change: evaluate-this: ~a" evaluate-this)
        (format debug-output "~&entity-enabledness-change: change-status: ~a" change-status)
        (format debug-output "~&entity-enabledness-change application-frame: ~a" *application-frame*)
        (force-output))
      
      (when *application-frame*

        (let ((enabled-conditional-commands (creating-assoc application-frame *enabled-conditional-commands*))
              (enabled-conditional-sheets (creating-assoc application-frame *enabled-conditional-sheets*)))

          ;; commands
          (when change-status
            (dolist (command enable-commands )
              (pushnew command (cdr enabled-conditional-commands)))
            (dolist (command disable-commands)
              (setf (cdr enabled-conditional-commands)
                    (delete command (cdr enabled-conditional-commands)))))
          (dolist (command (expand-command-tables enable-commands))
            (setf (command-enabled command *application-frame*) t))
          (dolist (command (expand-command-tables disable-commands))
            (setf (command-enabled command *application-frame*) nil))
          ;; sheets
          (dolist (sheet enable-sheets)
            (setf (sheet-enabled-p (find-pane-named *application-frame* sheet)) t)
            (when change-status
              (pushnew sheet (cdr enabled-conditional-sheets))))
          (dolist (sheet disable-sheets)
            (setf (sheet-enabled-p (find-pane-named *application-frame* sheet)) nil)
            (when change-status
              (setf (cdr enabled-conditional-sheets)
                    (delete sheet (cdr enabled-conditional-sheets)))))
          ;; re-establish previous if the status should not have been touched
          (unless change-status
            (dolist (command (expand-command-tables (cdr enabled-conditional-commands)))
              (setf (command-enabled command *application-frame*) t))
            (dolist (sheet (cdr enabled-conditional-sheets))
              (setf (sheet-enabled-p (find-pane-named *application-frame* sheet)) t)))
          ;; evaluate-this
          (eval evaluate-this))))
    
    entity-enabledness-change))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFINE-COMMAND and DEFINE-APPLICATION-FRAME wrappers

(defmacro define-conditional-command (name-and-options entity-enabledness-change args &body body)
  `(progn
    (when ',entity-enabledness-change
      (apply #'add-entity-enabledness-change ',(first name-and-options)
                                             ',(first entity-enabledness-change)
                                             ',(cdr entity-enabledness-change)))
    (define-command ,name-and-options
        ,args
      (prog1
        (progn
          ,@body)
        (change-entity-enabledness ',(car name-and-options))))))

(defmacro define-conditional-application-frame (name superclasses entity-enabledness-change
                                                slots &rest options)
  (let ((change-name (read-from-string (format nil "~a-start" name))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (when ',entity-enabledness-change
      (apply #'add-entity-enabledness-change ',change-name ',name
             ;; The following is allowed even if :CHANGE-STATUS is already in
             ;; ENTITY-ENABLEDNESS-CHANGE; see CLHS 3.4.1.4: "If more than one
             ;; such argument pair matches, the leftmost argument pair is used."
                                             ',(append entity-enabledness-change
                                                       '(:change-status nil))))
    
    (define-application-frame ,name
        ,superclasses
      ,slots
      ,@options)

    (defmethod run-frame-top-level :before ((frame ,name) &key)
      (change-entity-enabledness ',change-name #+nil ',name)))))

