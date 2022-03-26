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

;;; Part of the Undo protocol that works with persistent buffers

(in-package :drei-undo)

(defclass p-undo-mixin ()
  ((tree :initform (make-instance 'standard-undo-tree) :reader undo-tree)
   (undo-accumulate :initform '() :accessor undo-accumulate)
   (performing-undo :initform nil :accessor performing-undo)))

(defclass p-undo-record (climacs-undo-record)
  ((contents :initarg :contents)))

(defun save-p-undo-record (buffer)
  (unless (performing-undo buffer)
    (push (make-instance
	   'p-undo-record
	   :buffer buffer
	   :contents (slot-value buffer 'drei-buffer::contents))
     (undo-accumulate buffer))))

(defmethod insert-buffer-object :before ((buffer p-undo-mixin) offset object)
  (declare (ignore offset object))
  (save-p-undo-record buffer))

(defmethod insert-buffer-sequence :before ((buffer p-undo-mixin) offset seq)
  (declare (ignore offset seq))
  (save-p-undo-record buffer))

(defmethod delete-buffer-range :before ((buffer p-undo-mixin) offset n)
  (declare (ignore offset n))
  (save-p-undo-record buffer))

(defmethod (setf buffer-object) :before (object (buffer p-undo-mixin) offset)
  (declare (ignore object offset))
  (save-p-undo-record buffer))

(defmethod flip-undo-record ((record p-undo-record))
  (with-slots (buffer contents) record
    (setf (slot-value buffer 'drei-buffer::contents) contents)
    (drei-buffer::filter-and-update
     (drei-buffer::cursors buffer)
     #'(lambda (c) (flexichain::weak-pointer-value c))
     #'(lambda (wpc)
	 (setf (cursor-pos wpc)
	       (max 0 (min (cursor-pos wpc) (1- (size buffer)))))))))
