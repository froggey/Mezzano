;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM-2, Chapter 32.2 Multi-processing
;;;            for ACL
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

(defun make-process (function &key name)
  (mp:process-run-function name () function))

(defun destroy-process (process)
  (mp:process-kill process))

(defun current-process ()
  mp:*current-process*)

(defun all-processes ()
  (mp:list-all-processes))

(defun processp (object)
  (mp:process-p object))

(defun process-name (process)
  (mp:process-name process))

(defun process-state (process)
  (declare (ignore process))
  ;; Hmm can we somehow gain useful information here?
  nil)

(defun process-whostate (process)
  (mp:process-whostate process))

(defun process-wait (reason predicate)
  (mp:process-wait reason predicate))

(defun process-wait-with-timeout (reason timeout predicate)
  (mp:process-wait-with-timeout reason timeout predicate))

(defun process-yield ()
  (mp:process-allow-scheduling))

(defun process-interrupt (process function)
  (mp:process-interrupt process function))

(defun disable-process (process)
  (mp:without-interrupts
   (setf (mp:process-arrest-reasons process)
         (pushnew 'suspend (mp:process-arrest-reasons process)))))

(defun enable-process (process)
  (mp:without-interrupts 
   (setf (mp:process-arrest-reasons process)
         (remove 'suspend (mp:process-arrest-reasons process)))))

(defun restart-process (process)
  (mp:process-reset process))

(defmacro without-scheduling (&body body)
  `(mp:without-preemption .,body))

(defmacro atomic-incf (place)
  (declare (optimize (speed 3) (safety 1)))
  `(mp:without-interrupts (incf (the fixnum ,place))))

(defmacro atomic-decf (place)
  (declare (optimize (speed 3) (safety 1)))
  `(mp:without-interrupts(decf (the fixnum ,place))))

;;; 32.3 Locks

(defun make-lock (&optional name)
  (mp:make-lock :name name))

(defmacro with-lock-held ((place &optional state) &body body)
  `(mp:with-lock (,place ,@(if state (list :whostate state) nil))
     .,body))

(defun make-recursive-lock (&optional name)
  (mp:make-lock :name name))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  `(mp:with-lock (,place ,@(if state (list :whostate state) nil))
     .,body))
