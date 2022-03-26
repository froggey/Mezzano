;;; -*- Mode: Lisp; Package: DREI-MOTION; -*-

;;;  (c) copyright 2006 by
;;;           Taylor R. Campbell (campbell@mumble.net)
;;;  (c) copyright 2006 by
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

;;; Drei Motion

;;; A basic motion function is a function named FORWARD-ONE-<unit>' or
;;; BACKWARD-ONE-<unit> of the signature (<mark> <syntax>) that
;;; returns true if any motion happened or false if a limit was
;;; reached.
;;;
;;; A general motion function is a function named FORWARD-<unit> or
;;; BACKWARD-<unit> of the signature (<mark> <syntax> &OPTIONAL
;;; (<count> 1) (<limit-action> #'ERROR-LIMIT-ACTION)) that returns
;;; true if it could move forward or backward over the requested
;;; number of units, <count>, which may be positive or negative; and
;;; calls the limit action if it could not, or returns nil if the
;;; limit action is nil.
;;;
;;; A limit action is a function usually named <mumble>-LIMIT-ACTION
;;; of the signature (<mark> <original-offset> <remaining-units>
;;; <unit> <syntax>) that is called whenever a general motion function
;;; cannot complete the motion.  <Mark> is the mark the object in
;;; motion; <original-offset> is the original offset of the mark,
;;; before any motion; <remaining-units> is the number of units left
;;; until the motion would be complete; <unit> is a string naming the
;;; unit; and <syntax> is the syntax instance passed to the motion
;;; function.
;;;
;;; A motion command is a CLIM command named Forward <unit> or
;;; Backward <unit> which can take a numeric prefix argument and moves
;;; the point over the requested number, or 1, of units, by calling
;;; the general motion function FORWARD-<unit> or BACKWARD-<unit>.
;;;
;;; Given the basic motion functions FORWARD-ONE-<unit> and
;;; BACKWARD-ONE-<unit>,
;;;
;;;   (DEFINE-MOTION-FNS <unit>)
;;;
;;; defines the general motion functions FORWARD-<unit> and
;;; BACKWARD-<unit>.
;;;
;;; NOTE: FORWARD-OBJECT and BACKWARD-OBJECT, by virtue of their
;;; low-level status and placement in the buffer protocol (see
;;; buffer.lisp) do not obey this protocol, in that they have no
;;; syntax argument. Therefore, all <frob>-OBJECT functions and
;;; commands lack this argument as well (FIXME? We could shadow the
;;; definition from the buffer protocol and just ignore the syntax
;;; argument). There are no FORWARD-ONE-OBJECT or BACKWARD-ONE-OBJECT
;;; functions.


(in-package :drei-motion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Functions to move mark around based on the syntax:

(defun forward-to-word-boundary (mark syntax)
  "Move the mark forward to the beginning of the next word."
  (loop until (end-of-buffer-p mark)
	until (word-constituentp syntax (object-after mark))
	do (incf (offset mark))))

(defun backward-to-word-boundary (mark syntax)
  "Move the mark backward to the end of the previous word."
  (loop until (beginning-of-buffer-p mark)
	until (word-constituentp syntax (object-before mark))
	do (decf (offset mark))))

(defun beep-limit-action (mark original-offset remaining unit syntax)
  "This limit action will beep at the user."
  (declare (ignore mark original-offset remaining unit syntax))
  (clim:beep)
  nil)

(defun revert-limit-action (mark original-offset remaining unit syntax)
  "This limit action will try to restore the mark state from
before the attempted action. Note that this will not restore any
destructive actions that have been performed, it will only
restore the position of `mark'."
  (declare (ignore remaining unit syntax))
  (setf (offset mark) original-offset)
  nil)

(define-condition motion-limit-error (error)
  ((mark :initarg :mark)
   (original-offset :initarg :original-offset)
   (unit :initarg :unit)
   (remaining :initarg :remaining)
   (syntax :initarg :syntax))
  (:documentation
   "This error condition signifies that a motion cannot be performed.")
  (:report (lambda (condition stream)
             (format stream "Motion by ~A reached limit."
                     (slot-value condition 'UNIT)))))

(defun error-limit-action (mark original-offset remaining unit syntax)
  "This limit action will signal an error of type
`motion-limit-error'."
  (error 'MOTION-LIMIT-ERROR
         :mark mark
         :original-offset original-offset
         :remaining remaining
         :unit unit
         :syntax syntax))

(defmacro define-motion-fns (unit &key plural)
  (labels ((concat (&rest strings)
             (apply #'concatenate 'string (mapcar #'string strings)))
           (symbol (&rest strings)
             (intern (apply #'concat strings))))
    (let ((forward-one (symbol "FORWARD-ONE-" unit))
          (backward-one (symbol "BACKWARD-ONE-" unit))
          (forward (symbol "FORWARD-" unit))
          (backward (symbol "BACKWARD-" unit))
          (unit-name (string-downcase unit)))
      (let ((plural (or plural (concat unit-name "s"))))
        `(progn
           (defgeneric ,forward
               (mark syntax &optional count limit-action)
             (:documentation
              ,(concat "Move MARK forward by COUNT " plural ".")))
           (defgeneric ,backward
               (mark syntax &optional count limit-action)
             (:documentation
              ,(concat "Move MARK backward by COUNT " plural ".")))
           (defmethod ,forward (mark syntax &optional
                                (count 1)
                                (limit-action #'error-limit-action))
             (let ((offset (offset mark)))
               (dotimes (i count t)
                 (if (not (,forward-one mark syntax))
                     (return (and limit-action
                                  (funcall limit-action
                                           mark
                                           offset
                                           (- count i)
                                           ,unit-name
                                           syntax)))))))
           (defmethod ,backward (mark syntax &optional
                                 (count 1)
                                 (limit-action #'error-limit-action))
             (let ((offset (offset mark)))
               (dotimes (i count t)
                 (if (not (,backward-one mark syntax))
                     (return (and limit-action
                                  (funcall limit-action
                                           mark
                                           offset
                                           (- i count)
                                           ,unit-name
                                           syntax)))))))
           (defmethod ,forward :around (mark syntax &optional
                                        (count 1)
                                        (limit-action :error))
             (cond ((minusp count)
                    (,backward mark syntax (- count) limit-action))
                   ((plusp count)
                    (call-next-method))
                   (t t)))
           (defmethod ,backward :around (mark syntax &optional
                                         (count 1)
                                         (limit-action :error))
             (cond ((minusp count)
                    (,forward mark syntax (- count) limit-action))
                   ((plusp count)
                    (call-next-method))
                   (t t))))))))

(defun make-diligent-motor (motor fiddler)
  "Create and return a diligent motor with a default limit action
of `beep-limit-action'. `Motor' and `fiddler' will take turns
being called until either `motor' succeeds or `fiddler' fails."
  (labels ((make-limit-action (loser)
             (labels ((limit-action
                          (mark original-offset remaining unit syntax)
                        (declare (ignore original-offset unit))
                        (and (funcall fiddler mark syntax 1 loser)
                             (funcall motor mark syntax
                                      (if (minusp remaining)
                                          (- -1 remaining)
                                          (- remaining 1))
                                      #'limit-action))))
               #'limit-action))
           (move (mark syntax &optional
                  (count 1)
                  (loser #'beep-limit-action))
             (funcall motor mark syntax count
                      (make-limit-action loser))))
    #'move))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Line start motion

(defgeneric forward-one-line-start (mark syntax)
  (:documentation "Move MARK to the start of the next line."))

(defmethod forward-one-line-start (mark syntax)
  (when (forward-object mark)
    (loop until (beginning-of-line-p mark)
       do (forward-object mark)
       finally (return t))))

(defgeneric backward-one-line-start (mark syntax)
  (:documentation "Move MARK to the end of the next line."))

(defmethod backward-one-line-start (mark syntax)
  (when (backward-object mark)
    (loop until (beginning-of-line-p mark)
       do (backward-object mark)
       finally (return t))))

(define-motion-fns line-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Line end motion

(defgeneric forward-one-line-end (mark syntax)
  (:documentation "Move MARK to the end of the next line."))

(defmethod forward-one-line-end (mark syntax)
  (when (forward-object mark)
    (loop until (end-of-line-p mark)
       do (forward-object mark)
       finally (return t))))

(defgeneric backward-one-line-end (mark syntax)
  (:documentation "Move MARK to the end of the previous line."))

(defmethod backward-one-line-end (mark syntax)
  (when (backward-object mark)
    (loop until (end-of-line-p mark)
       do (backward-object mark)
       finally (return t))))

(define-motion-fns line-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Word motion

(defgeneric forward-one-word (mark syntax)
  (:documentation "Move MARK forward over the next word.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod forward-one-word (mark syntax)
  (unless (end-of-buffer-p mark)
    (forward-to-word-boundary mark syntax)
    (loop until (end-of-buffer-p mark)
       while (word-constituentp syntax (object-after mark))
       do (forward-object mark)
       finally (return t))))

(defgeneric backward-one-word (mark syntax)
  (:documentation "Move MARK backward over the previous word.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod backward-one-word (mark syntax)
  (unless (beginning-of-buffer-p mark)
    (backward-to-word-boundary mark syntax)
    (loop until (beginning-of-buffer-p mark)
       while (word-constituentp syntax (object-before mark))
       do (backward-object mark)
       finally (return t))))

(define-motion-fns word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Line motion

(defgeneric forward-one-line (mark syntax)
  (:documentation
   "Move MARK forward to the next line, preserving column.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod forward-one-line (mark syntax)
  (let ((column (column-number mark)))
    (end-of-line mark)
    (handler-case (cond ((forward-object mark)
                         (setf (column-number mark) column)
                         t)
                        (t nil))
      (motion-after-end ()))))

(defgeneric backward-one-line (mark syntax)
  (:documentation
   "Move MARK backward to the previous line, preserving column.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod backward-one-line (mark syntax)
  (let ((column (column-number mark)))
    (beginning-of-line mark)
    (handler-case (cond ((backward-object mark)
                         (setf (column-number mark) column)
                         t)
                        (t nil))
      (motion-before-beginning ()))))

(define-motion-fns line)

;; Faster version for special mark... I don't know whether it's ever
;; going to be used, but it was in the old motion code.
(defmethod backward-line ((mark p-line-mark-mixin) syntax
                          &optional (count 1)
                          (limit-action
                           #'error-limit-action))
  (let* ((column (column-number mark))
         (line (line-number mark))
	 (goto-line (- line count)))
    (handler-case
        (setf (offset mark)
              (+ column
                 (buffer-line-offset (buffer mark) goto-line)))
      (invalid-motion ()
        (funcall limit-action mark
                 (offset mark) (- count line)
                 "line" syntax)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Page motion

(defgeneric forward-one-page (mark syntax)
  (:documentation
   "Move MARK forward over the next page.
Return T if successful, or NIL if the buffer limit was
reached."))

(defmethod forward-one-page (mark syntax)
  (unless (end-of-buffer-p mark)
    (forward-object mark 1)
    (if (search-forward mark (coerce (page-delimiter syntax) 'vector))
        (progn (backward-object mark (length (page-delimiter syntax)))
               t)
        (progn (end-of-buffer mark)
               nil))))

(defgeneric backward-one-page (mark syntax)
  (:documentation
   "Move MARK backward to the previous page.
Return T if successful, or NIL if the buffer limit was
reached."))

(defmethod backward-one-page (mark syntax)
  (unless (beginning-of-buffer-p mark)
    (backward-object mark 1)
    (if (search-backward mark (coerce (page-delimiter syntax) 'vector))
        (progn (forward-object mark (length (page-delimiter syntax)))
               t)
        (progn (beginning-of-buffer mark)
               t))))

(define-motion-fns page)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Expression motion

(defgeneric forward-one-expression (mark syntax)
  (:documentation
   "Move MARK forward over the next expression.
Return T if successful, or NIL if the buffer limit or the end of the
  enclosing expression was reached."))

(defmethod forward-one-expression (mark syntax)
  (declare (ignore mark syntax))
  (error 'NO-SUCH-OPERATION))

(defgeneric backward-one-expression (mark syntax)
  (:documentation
   "Move MARK backward over the previous expression.
Return T if successful, or NIL if the buffer limit or the start of the
  enclosing expression was reached."))

(defmethod backward-one-expression (mark syntax)
  (declare (ignore mark syntax))
  (error 'NO-SUCH-OPERATION))

(defgeneric forward-one-definition (mark syntax)
  (:documentation
   "Move MARK forward over the next definition.
Return T if successful, or NIL if the buffer limit was
reached."))

(defmethod forward-one-definition (mark syntax)
  (declare (ignore mark syntax))
  (error 'NO-SUCH-OPERATION))

(defgeneric backward-one-definition (mark syntax)
  (:documentation
   "Move MARK backward over the previous definition.
Return T if successful, or NIL if the buffer limit was
reached."))

(defmethod backward-one-definition (mark syntax)
  (declare (ignore mark syntax))
  (error 'NO-SUCH-OPERATION))

(defgeneric forward-one-up (mark syntax)
  (:documentation
   "Move MARK forward by one nesting level up.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod forward-one-up (mark syntax)
  (declare (ignore mark syntax))
  (error 'NO-SUCH-OPERATION))

(defgeneric backward-one-up (mark syntax)
  (:documentation
   "Move MARK backward by one nesting level up.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod backward-one-up (mark syntax)
  (declare (ignore mark syntax))
  (error 'NO-SUCH-OPERATION))

(defgeneric forward-one-down (mark syntax)
  (:documentation
   "Move MARK forward by one nesting level down.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod forward-one-down (mark syntax)
  (declare (ignore mark syntax))
  (error 'NO-SUCH-OPERATION))

(defgeneric backward-one-down (mark syntax)
  (:documentation
   "Move MARK backward by one nesting level down.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod backward-one-down (mark syntax)
  (declare (ignore mark syntax))
  (error 'NO-SUCH-OPERATION))

(define-motion-fns expression)
(define-motion-fns definition)
(define-motion-fns up :plural "nesting levels up")
(define-motion-fns down :plural "nesting levels down")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Paragraph motion

(defgeneric backward-one-paragraph (mark syntax)
  (:documentation
   "Move MARK backward by one paragraph.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod backward-one-paragraph (mark syntax)
  (unless (beginning-of-buffer-p mark)
    (backward-object mark 1)
    (if (search-backward mark (coerce (paragraph-delimiter syntax) 'vector))
        (forward-object mark (length (paragraph-delimiter syntax)))
        (beginning-of-buffer mark))
    t))

(defgeneric forward-one-paragraph (mark syntax)
  (:documentation
   "Move MARK forward by one paragraph.
Return T if successful, or NIL if the buffer limit was reached."))

(defmethod forward-one-paragraph (mark syntax)
  (unless (end-of-buffer-p mark)
    (forward-object mark 1)
    (if (search-forward mark (coerce (paragraph-delimiter syntax) 'vector))
        (backward-object mark (length (paragraph-delimiter syntax)))
        (end-of-buffer mark))
    t))

(define-motion-fns paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; List motion

(defgeneric backward-one-list (mark syntax)
  (:documentation
   "Move MARK backward by one list.
Return T if successful, or NIL if the buffer limit was reached.")
  (:method (mark syntax)
    (error 'no-such-operation)))

(defgeneric forward-one-list (mark syntax)
  (:documentation
   "Move MARK forward by one list.
Return T if successful, or NIL if the buffer limit was reached.")
  (:method (mark syntax)
    (error 'no-such-operation)))

(define-motion-fns list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Sentence motion

(defgeneric backward-one-sentence (mark syntax)
  (:documentation
   "Move MARK backward by one sentence.
Return T if successful, or NIL if the buffer limit was reached.")
  (:method (mark syntax)
    (error 'no-such-operation)))

(defgeneric forward-one-sentence (mark syntax)
  (:documentation
   "Move MARK forward by one sentence.
Return T if successful, or NIL if the buffer limit was reached.")
  (:method (mark syntax)
    (error 'no-such-operation)))

(define-motion-fns sentence)

;;; Paredit-like motion operations: move forward or backward across
;;; expressions, until the limits of the enclosing expression are
;;; reached; then move up a level.

(declaim (ftype function
                forward-expression-or-up
                backward-expression-or-up))

(setf (fdefinition 'FORWARD-EXPRESSION-OR-UP)
      (make-diligent-motor #'forward-expression #'forward-up))

(setf (fdefinition 'BACKWARD-EXPRESSION-OR-UP)
      (make-diligent-motor #'backward-expression #'backward-up))


