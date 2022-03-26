;;; -*- Mode: Lisp; Package: DREI-KILL-RING -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004 by
;;;           Elliott Johnson (ejohnson@fasl.info)

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

;;; kill ring system

(in-package :drei-kill-ring)

(defgeneric kill-ring-chain (ring)
  (:documentation "Return the cursorchain associated with the
kill ring `ring'."))

(defgeneric kill-ring-cursor (ring)
  (:documentation "Return the flexicursor associated with the
kill ring."))

(defclass kill-ring ()
  ((max-size :type (integer 5 *) ;5 element minimum from flexichain protocol 
	     :initarg :max-size
             :documentation "The limitation placed upon the
number of elements held by the kill ring.  Once the maximum size
has been reached, older entries must first be removed before new
ones can be added. When altered, any surplus elements will be
silently dropped.")
   (cursorchain :type standard-cursorchain
		:accessor kill-ring-chain
		:initform (make-instance 'standard-cursorchain)
                :documentation "The cursorchain associated with
the kill ring.")
   (yankpoint   :type left-sticky-flexicursor
	        :accessor kill-ring-cursor
                :documentation "The flexicursor associated with
the kill ring.")
   (append-next-p :type boolean :initform nil
		  :accessor append-next-p))
  (:documentation "A class for all kill rings"))

(define-condition empty-kill-ring (simple-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "The kill ring is empty")))
  (:documentation "This condition is signaled whenever a yank
  operation is performed on an empty kill ring."))

(defmethod initialize-instance :after ((kr kill-ring) &rest args)
  "Adds in the yankpoint"
  (declare (ignore args))
  (with-slots (cursorchain yankpoint) kr
    (setf yankpoint (make-instance 'left-sticky-flexicursor :chain cursorchain))))

(defgeneric kill-ring-length (kr)
  (:documentation "Returns the current length of the kill-ring.
Note this is different than `kill-ring-max-size'."))

(defgeneric kill-ring-max-size (kr)
  (:documentation "Returns the value of the kill ring's maximum
size"))

(defgeneric (setf kill-ring-max-size) (kr size)
  (:documentation "Alters the maximum size of the kill ring, even
if it means dropping elements to do so."))

(defgeneric reset-yank-position (kr)
  (:documentation "Moves the current yank point back to the start
of of kill ring position"))

(defgeneric rotate-yank-position (kr &optional times)
  (:documentation "Moves the yank point associated with a
kill-ring one or times many positions away from the start of ring
position.  If times is greater than the current length then the
cursor will wrap to the start of ring position and continue
rotating."))

(defgeneric kill-ring-standard-push (kr vector)
  (:documentation "Pushes a vector of objects onto the kill ring
creating a new start of ring position.  This function is much
like an everyday Lisp push with size considerations.  If the
length of the kill ring is greater than the maximum size, then
\"older\" elements will be removed from the ring until the
maximum size is reached."))

(defgeneric kill-ring-concatenating-push (kr vector)
  (:documentation "Concatenates the contents of vector onto the
end of the current contents of the top of the kill ring.  If the
kill ring is empty the a new entry is pushed."))

(defgeneric kill-ring-reverse-concatenating-push (kr vector)
  (:documentation "Concatenates the contents of vector onto the front
of the current contents of the top of the kill ring. If the kill ring
is empty a new entry is pushed."))

(defgeneric kill-ring-yank (kr &optional reset)
  (:documentation "Returns the vector of objects currently
pointed to by the cursor.  If `reset' is T, a call to
`reset-yank-position' is called before the object is yanked.  The
default for reset is NIL.  If the kill ring is empty, a condition
of type `empty-kill-ring' is signalled."))

(defmethod kill-ring-length ((kr kill-ring))
  (nb-elements (kill-ring-chain kr)))

(defmethod kill-ring-max-size ((kr kill-ring))
  (with-slots (max-size) kr
     max-size))

(defmethod (setf kill-ring-max-size) (size (kr kill-ring))
  (unless (typep size 'integer)
    (error "Error, ~S, is not an integer value" size))
  (if (< size 5)
      (setf (slot-value kr 'max-size) 5)
      (setf (slot-value kr 'max-size) size))
  (let ((len (kill-ring-length kr)))
    (if (> len size)
	(loop for n from 1 to (- len size)
	      do (pop-end (kill-ring-chain kr))))))

(defmethod reset-yank-position ((kr kill-ring))
  (setf (cursor-pos (kill-ring-cursor kr)) 0)
  t) 

(defmethod rotate-yank-position ((kr kill-ring) &optional (times 1))
    (if (> (kill-ring-length kr) 0)
	(let* ((curs (kill-ring-cursor kr))
	       (pos (mod (+ times (cursor-pos curs))
			 (kill-ring-length kr))))
	  (setf (cursor-pos curs) pos))))

(defun push-start-and-copy (chain vector)
  (push-start chain vector)
  (when (every #'characterp vector)
    (alexandria:when-let ((pane (drei:editor-pane (drei:drei-instance))))
      (clime:publish-selection pane :clipboard (coerce vector 'string) 'string))))

(defmethod kill-ring-standard-push ((kr kill-ring) vector)
  (check-type vector vector)
  (cond ((append-next-p kr)
	 (kill-ring-concatenating-push kr vector)
	 (setf (append-next-p kr) nil))
	(t (let ((chain (kill-ring-chain kr)))
	     (if (>= (kill-ring-length kr)
		     (kill-ring-max-size kr))
	         (progn
		   (pop-end chain)
		   (push-start-and-copy chain vector))
	         (push-start-and-copy chain vector)))
	   (reset-yank-position kr))))

(defmethod kill-ring-concatenating-push ((kr kill-ring) vector)
  (check-type vector vector)
  (let ((chain (kill-ring-chain kr)))
    (if (zerop (kill-ring-length kr))
	(push-start-and-copy chain vector)
        (push-start-and-copy chain
		             (concatenate 'vector
				          (pop-start chain)
				          vector)))
    (reset-yank-position kr)))

(defmethod kill-ring-reverse-concatenating-push ((kr kill-ring) vector)
  (check-type vector vector)
  (let ((chain (kill-ring-chain kr)))
    (if (zerop (kill-ring-length kr))
	(push-start-and-copy chain vector)
	(push-start-and-copy chain
		             (concatenate 'vector
				          vector
				          (pop-start chain))))
    (reset-yank-position kr)))

(defmethod kill-ring-yank ((kr kill-ring) &optional (reset nil))
  (assert (plusp (kill-ring-length kr))
          ()
          (make-condition 'empty-kill-ring))
  (if reset (reset-yank-position kr))
  (element> (kill-ring-cursor kr)))

(defparameter *kill-ring* (make-instance 'kill-ring :max-size 7)
  "This special variable is bound to the kill ring of the running
application or Drei instance whenever a command is executed.")
