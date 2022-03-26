;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM-2, Chapter 32.2 Multi-processing
;;;            for single processing Lisps
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

(in-package :clim-internals)

;;; No multi-processing here

(defconstant *multiprocessing-p* nil)

(defun make-process (function &key name)
  (declare (ignore function name))
  (error "No ~S here." 'make-process))

(defun destroy-process (process)
  (declare (ignore process))
  (error "Huh?"))

(defun current-process ()
  nil)

(defun all-processes ()
  nil)

(defun processp (object)
  (declare (ignore object))
  nil)

(defun process-name (process)
  (declare (ignore process))
  nil)

(defun process-state (process)
  (declare (ignore process))
  nil)

(defun process-whostate (process)
  (declare (ignore process)))

(defun process-wait (reason predicate)
  (declare (ignore reason))
  (loop until (funcall predicate)))

(defun process-wait-with-timeout (reason timeout predicate)
  (declare (ignore reason))
  (let ((end-time (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop until (or (funcall predicate)
                    (> (get-internal-real-time) end-time)))))

(defun process-yield ()
  nil)

(defun process-interrupt (process function)
  (declare (ignore process))
  (funcall function))

(defun disable-process (process)
  (declare (ignore process))
  (error "Huh?!"))

(defun enable-process (process)
  (declare (ignore process))
  (error "Huh?!"))

(defun restart-process (process)
  (declare (ignore process))
  (error "Huh?!"))

(defmacro without-scheduling (&body body)
  `(progn ,@body))

(defmacro atomic-incf (place)
  `(incf (the fixnum ,place)))

(defmacro atomic-decf (place)
  `(decf (the fixnum ,place)))

;;; 32.3 Locks

(defun make-lock (&optional name)
  (declare (ignore name))
  (list nil))

(defmacro with-lock-held ((place &optional state) &body body)
  (declare (ignore place state))
  `(progn ,@body))

(defun make-recursive-lock (&optional name)
  (declare (ignore name))
  (list nil))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  (declare (ignore place state))
  `(progn ,@body))

;;; This is a bit dodgy; it depends on the condition notifier to be
;;; called from process-next-event. However, I don't feel obligated
;;; to put too much work into CLIM-SYS on non-multiprocessing platforms.

(defun make-condition-variable () (list nil))

(defun condition-wait (cv lock &optional timeout)
  (declare (ignore lock))
  (flet ((wait-func ()
           (loop for port in climi::*all-ports* ;; this is dubious
                 do (loop as this-event = (process-next-event port :timeout 0)
                     for got-events = this-event then (or got-events this-event)
                     while this-event
                     finally (unless got-events (process-next-event port))))
           (car cv)))
    (setf (car cv) nil)
    (if timeout
        (process-wait-with-timeout "Waiting for event" timeout #'wait-func)
        (process-wait "Waiting for event" #'wait-func))))

(defun condition-notify (cv)
  (setf (car cv) t))
