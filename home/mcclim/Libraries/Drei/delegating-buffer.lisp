;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
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

;;; Buffer class that allows for specifying buffer implementation at run time.

(in-package :drei-buffer)

(defclass delegating-buffer (buffer)
  ((%implementation :reader implementation
                    :initform (error "A delegating buffer must have an implementation")
                    :initarg :implementation))
  (:documentation "Buffer class that delegates the buffer
protocol functionality to a buffer implementation object stored
in the `implementation' slot."))

(defmethod size ((buffer delegating-buffer))
  (size (implementation buffer)))

(defmethod number-of-lines ((buffer delegating-buffer))
  (number-of-lines (implementation buffer)))

(defmethod insert-buffer-object ((buffer delegating-buffer) offset object)
  (insert-buffer-object (implementation buffer) offset object))

(defmethod insert-buffer-sequence ((buffer delegating-buffer) offset sequence)
  (insert-buffer-sequence (implementation buffer) offset sequence))

(defmethod delete-buffer-range ((buffer delegating-buffer) offset n)
  (delete-buffer-range (implementation buffer) offset n))

(defmethod buffer-object ((buffer delegating-buffer) offset)
  (buffer-object (implementation buffer) offset))

(defmethod (setf buffer-object) (object (buffer delegating-buffer) offset)
  (setf (buffer-object (implementation buffer) offset) object))

(defmethod buffer-sequence ((buffer delegating-buffer) offset1 offset2)
  (buffer-sequence (implementation buffer) offset1 offset2))

(defmethod buffer-line-number ((buffer delegating-buffer) offset)
  (buffer-line-number (implementation buffer) offset))

(defmethod buffer-column-number ((buffer delegating-buffer) offset)
  (buffer-column-number (implementation buffer) offset))

(defclass delegating-mark (mark-mixin)
  ((%implementation :reader implementation
                    :initform (error "A delegating mark must have an implementation")
                    :initarg :implementation))
  (:documentation "Superclass for classes suitable for use in a
`delegating-buffer'."))

(defmethod offset ((mark delegating-mark))
  (offset (implementation mark)))

(defmethod (setf offset) (new-value (mark delegating-mark))
  (setf (offset (implementation mark)) new-value))

(defclass delegating-left-sticky-mark (left-sticky-mark delegating-mark)
  ()
  (:documentation "A `left-sticky-mark' subclass suitable for use
in a `delegating-buffer'."))
 
(defclass delegating-right-sticky-mark (right-sticky-mark delegating-mark)
  ()
  (:documentation "A `right-sticky-mark' subclass suitable for
use in a `delegating-buffer'."))

(defmethod clone-mark ((mark delegating-left-sticky-mark) &optional stick-to)
  (cond ((or (null stick-to) (eq stick-to :left))
	 (make-instance 'delegating-left-sticky-mark
          :implementation (clone-mark (implementation mark) :left)
          :buffer (buffer mark)))
	((eq stick-to :right)
	 (make-instance 'delegating-right-sticky-mark
          :implementation (clone-mark (implementation mark) :right)
          :buffer (buffer mark)))
	(t (error "invalid value for stick-to"))))

(defmethod clone-mark ((mark delegating-right-sticky-mark) &optional stick-to)
  (cond ((or (null stick-to) (eq stick-to :right))
         (make-instance 'delegating-right-sticky-mark
          :implementation (clone-mark (implementation mark) :right)
          :buffer (buffer mark)))
	((eq stick-to :left)
	 (make-instance 'delegating-left-sticky-mark
          :implementation (clone-mark (implementation mark) :left)
          :buffer (buffer mark)))
	(t (error "invalid value for stick-to"))))

(defmethod make-buffer-mark ((buffer delegating-buffer)
                             &optional (offset 0) (stick-to :left))
  (make-instance (ecase stick-to
                   (:left 'delegating-left-sticky-mark)
                   (:right 'delegating-right-sticky-mark))
                 :implementation (make-buffer-mark (implementation buffer)
                                                   offset stick-to)
                 :buffer buffer))
