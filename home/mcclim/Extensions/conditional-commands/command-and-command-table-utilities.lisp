(in-package :command-and-command-table-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command/command-table helper functions

(defun commands-of-command-table (command-table)
  (loop for i being the hash-key of (climi::commands command-table)
                 collect i))

(defun command-or-commands-of-command-table (command-or-command-table-name)
  (let ((command-table (ignore-errors (find-command-table command-or-command-table-name))))
    (if command-table
        (commands-of-command-table command-table)
        (list command-or-command-table-name))))

(defun expand-command-tables (commands)
  (apply #'append
         (mapcar #'command-or-commands-of-command-table commands)))

;;; (defun set-enable-state-of-commands-in-a-command-table (command-table &optional value)
;;;   (dolist (command (expand-command-tables (list command-table)))
;;;     (setf (command-enabled command *application-frame*) value)))

;;; (defun enable-command-table (command-table)
;;;   (set-enable-state-of-commands-in-a-command-table command-table t))

;;; (defun disable-command-table (command-table)
;;;   (set-enable-state-of-commands-in-a-command-table command-table nil))
