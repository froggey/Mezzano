;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM-2, Chapter 32.2 Multi-processing
;;;            for CMU
;;;   Created: 2001-05-22
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann

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

(in-package :CLIM-INTERNALS)

(defconstant *multiprocessing-p* t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :clim-mp *features*))

(defun make-lock (&optional name)
  (mp:make-lock name))

(defun make-recursive-lock (&optional name)
  (mp:make-lock name :kind :recursive))

(defmacro with-lock-held ((place &optional state) &body body)
  `(mp:with-lock-held (,place ,state)
     ,@body))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  `(mp:with-lock-held (,place ,state)
     ,@body))

;;; condition variables, not actually part of CLIM-SYS but useful anyway

(defstruct condition-variable
  (lock (make-lock "condition variable"))
  (value nil)
  (process-queue nil))

(defun %release-lock (lock) ; copied from with-lock-held in multiproc.lisp
  #+i486 (kernel:%instance-set-conditional
	  lock 2 mp:*current-process* nil)
  #-i486 (when (eq (lock-process lock) mp:*current-process*)
	   (setf (lock-process lock) nil)))

(defun condition-wait (cv lock &optional timeout)
  (declare (ignore timeout))		;For now
  (loop
     (let ((cv-lock (condition-variable-lock cv)))
       (with-lock-held (cv-lock)
	 (when (condition-variable-value cv)
	   (setf (condition-variable-value cv) nil)
	   (return-from condition-wait t))
	 (setf (condition-variable-process-queue cv)
	       (nconc (condition-variable-process-queue cv)
		      (list mp:*current-process*)))
	 (%release-lock lock))
       (mp:process-add-arrest-reason mp:*current-process* cv)
       (let ((cv-val nil))
	 (with-lock-held (cv-lock)
	   (setq cv-val (condition-variable-value cv))
	   (when cv-val
	     (setf (condition-variable-value cv) nil)))
	 (when cv-val
	   (mp::lock-wait lock "waiting for condition variable lock")
	   (return-from condition-wait t))))))

(defun condition-notify (cv)
  (with-lock-held ((condition-variable-lock cv))
    (let ((proc (pop (condition-variable-process-queue cv))))
      ;; The waiting process may have released the CV lock but not
      ;; suspended itself yet
      (when proc
	(loop
	 for activep = (mp:process-active-p proc)
	 while activep
	 do (mp:process-yield))
	(setf (condition-variable-value cv) t)
	(mp:process-revoke-arrest-reason proc cv))))
  ;; Give the other process a chance
  (mp:process-yield))

;; all other are handled by import in defpack.lisp
