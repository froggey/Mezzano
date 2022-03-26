;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: clim-internals; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM-2, Chapter 32.2 Multi-processing
;;;            for the Scieneer Common Lisp
;;;   Created: 2006-03-12
;;;    Author: Scieneer Pty Ltd
;;;   Based on mp-acl, created 2001-05-22 by Gilbert Baumann
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2006 by Scieneer Pty Ltd

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

(defconstant *multiprocessing-p* t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :clim-mp *features*))

(defun make-process (function &key name)
  (mp:make-process function :name name))

(defun restart-process (process)
  (mp:restart-process process))

(defun destroy-process (process)
  (mp:destroy-process process))

(defun current-process ()
  (mp:current-process))

(defun all-processes ()
  (mp:all-processes))

(defun processp (object)
  (mp:processp object))

(defun process-name (process)
  (mp:process-name process))

(defun process-state (process)
  (mp:process-state process))

(defun process-whostate (process)
  (mp:process-whostate process))

(defun process-wait (reason predicate)
  (mp:process-wait reason predicate))

(defun process-wait-with-timeout (reason timeout predicate)
  (mp:process-wait-with-timeout reason timeout predicate))

(defun process-yield ()
  (mp:process-yield))

(defun process-interrupt (process function)
  (mp:process-interrupt process function))

(defun disable-process (process)
  (mp:disable-process process))

(defun enable-process (process)
  (mp:enable-process process))

(defmacro without-scheduling (&body body)
  `(mp:without-scheduling ,@body))

(defmacro atomic-incf (place)
  `(mp:atomic-incf ,place))

(defmacro atomic-decf (place) 
  `(mp:atomic-decf ,place))

;;; 32.3 Locks

(defun make-lock (&optional name)
  (mp:make-lock name :type :error-check))

(defmacro with-lock-held ((place &optional state) &body body)
  `(mp:with-lock-held (,place (or ,state "Lock Wait"))
    ,@body))

(defun make-recursive-lock (&optional name)
  (mp:make-lock name :type :recursive))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  `(mp:with-lock-held (,place (or ,state "Lock Wait"))
    ,@body))

(defun make-condition-variable ()
  (thread:make-cond-var))

(defun condition-wait (condition-variable lock &optional timeout)
  (cond (timeout
	 (thread:cond-var-timedwait condition-variable lock timeout))
	(t
	 (thread:cond-var-wait condition-variable lock)
	 t)))

(defun condition-notify (condition-variable)
  (thread:cond-var-broadcast condition-variable))


