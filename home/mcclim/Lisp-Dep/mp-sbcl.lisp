;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM-2, Chapter 32.2 Multi-processing
;;;            for SBCL
;;;   Created: 2003-02-22
;;;    Author: Daniel Barlow <dan@metacircles.com>
;;;   Based on mp-acl, created 2001-05-22 by Gilbert Baumann
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann
;;;  (c) copyright 2002 by John Wiseman (jjwiseman@yahoo.com)
;;;  (c) copyright 2003 by Daniel Barlow <dan@metacircles.com>

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

(defstruct (process
	     (:constructor %make-process)
	     (:predicate processp))
  name
  state
  whostate
  function
  thread)

(defun make-current-process ()
  (%make-process
   :name (sb-thread:thread-name sb-thread:*current-thread*)
   :function nil
   :thread sb-thread:*current-thread*))

(defvar *current-process* (make-current-process))

(defvar *all-processes* (list *current-process*)
  "A list of processes created by McCLIM, plus the one that was
running when this file was loaded.")

(defun reinit-processes ()
  (setf *current-process* (make-current-process))
  (setf *all-processes* (list *current-process*)))
 
(push 'reinit-processes sb-ext:*init-hooks*)

(defvar *all-processes-lock*
  (sb-thread:make-mutex :name "Lock around *ALL-PROCESSES*"))

;; we implement disable-process by making the disablee attempt to lock
;; *permanent-queue*, which is already locked because we locked it
;; here.  enable-process just interrupts the lock attempt.

(defvar *permanent-queue*
  (sb-thread:make-mutex :name "Lock for disabled threads"))
(unless (sb-thread:mutex-value *permanent-queue*)
  (sb-sys:without-interrupts
    (sb-sys:allow-with-interrupts
      (sb-thread:grab-mutex *permanent-queue*))))

(defun make-process (function &key name)
  (let ((p (%make-process :name name :function function)))
    (restart-process p)))

(defun restart-process (p)
  (labels ((boing ()
             (let ((*current-process* p))
               (sb-thread:with-mutex (*all-processes-lock*)
                 (sb-sys:without-interrupts
                   (pushnew p *all-processes*)))
               (sb-sys:without-interrupts
                 (unwind-protect (sb-sys:with-local-interrupts
                                   (funcall (process-function p)))
                   (sb-thread:with-mutex (*all-processes-lock*)
                     (sb-sys:without-interrupts
                       (setf *all-processes* (delete p *all-processes*)))))))))
    (when (process-thread p) (sb-thread:terminate-thread p))
    (when (setf (process-thread p) (sb-thread:make-thread #'boing :name (process-name p)))
      p)))

(defun destroy-process (process)
  (sb-thread:terminate-thread (process-thread process)))

(defun current-process ()
  (if (eq (process-thread *current-process*) sb-thread:*current-thread*)
      *current-process*
      (setf *current-process*
            (or (find sb-thread:*current-thread* *all-processes*
                 :key #'process-thread)
                ;; Don't add this to *all-processes*, because we don't
                ;; control it.
                (%make-process
                 :name (sb-thread:thread-name sb-thread:*current-thread*)
                 :function nil
                 :thread sb-thread:*current-thread*)))))

(defun all-processes ()
  ;; we're calling DELETE on *ALL-PROCESSES*.  If we look up the value
  ;; while that delete is executing, we could end up with nonsense.
  ;; Better use a lock (or call REMOVE instead in DESTROY-PROCESS).
  (sb-thread:with-mutex (*all-processes-lock*)
    *all-processes*))

;;; people should be shot for using these, honestly.  Use a queue!
(declaim (inline yield))
(defun yield ()
  (declare (optimize speed (safety 0)))
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "sched_yield" (function sb-alien:int)))
  (values))

(defun process-wait (reason predicate)
  (let ((old-state (process-whostate *current-process*)))
    (sb-sys:without-interrupts
      (unwind-protect
           (progn
             (setf old-state (process-whostate *current-process*)
                   (process-whostate *current-process*) reason)
             (loop
                (let ((it (sb-sys:with-local-interrupts (funcall predicate))))
                  (when it (return it)))
                                        ;(sleep .01)
                (yield)))
        (setf (process-whostate *current-process*) old-state)))))

(defun process-wait-with-timeout (reason timeout predicate)
  (let ((old-state (process-whostate *current-process*))
        (end-time (+ (get-universal-time) timeout)))
    (sb-sys:without-interrupts
      (unwind-protect
           (progn
             (setf old-state (process-whostate *current-process*)
                   (process-whostate *current-process*) reason)
             (loop
                (let ((it (sb-sys:with-local-interrupts (funcall predicate))))
                  (when (or (> (get-universal-time) end-time) it)
                    (return it)))
                                        ;(sleep .01)))
                (yield)))
        (setf (process-whostate *current-process*) old-state)))))

(defun process-interrupt (process function)
  (sb-thread:interrupt-thread (process-thread process) function))

(defun disable-process (process)
  (sb-thread:interrupt-thread
   (process-thread process)
   (lambda ()
     (catch 'interrupted-wait (sb-thread:with-mutex (*permanent-queue*))))))

(defun enable-process (process)
  (sb-thread:interrupt-thread
   (process-thread process) (lambda () (throw 'interrupted-wait nil))))

(defun process-yield ()
  (sleep .1))

;;; FIXME but, of course, we can't.  Fix whoever wants to use it,
;;; instead
(defmacro without-scheduling (&body body)
  `(progn ,@body))

(defparameter *atomic-lock*
  (sb-thread:make-mutex :name "atomic incf/decf"))

(defmacro atomic-incf (place)
  `(sb-thread:with-mutex (*atomic-lock*)
     (sb-sys:without-interrupts
       (incf ,place))))

(defmacro atomic-decf (place)
  `(sb-thread:with-mutex (*atomic-lock*)
     (sb-sys:without-interrupts
       (decf ,place))))

;;; 32.3 Locks

(defun make-lock (&optional name)
  (sb-thread:make-mutex :name name))

(defmacro with-lock-held ((place &optional state) &body body)
  (let ((old-state (gensym "OLD-STATE")))
    `(let (,old-state)
       (sb-thread:with-mutex (,place)
         (sb-sys:without-interrupts
           (unwind-protect
                (progn
                  (when ,state
                    (setf ,old-state (process-state *current-process*))
                    (setf (process-state *current-process*) ,state))
                  (sb-sys:with-local-interrupts
                    ,@body))
             (setf (process-state *current-process*) ,old-state)))))))


(defun make-recursive-lock (&optional name)
  (sb-thread:make-mutex :name name))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  (let ((old-state (gensym "OLD-STATE")))
    `(sb-thread:with-recursive-lock (,place)
       (sb-sys:without-interrupts
         (let (,old-state)
           (unwind-protect
                (progn
                  (when ,state
                    (setf ,old-state (process-state *current-process*))
                    (setf (process-state *current-process*) ,state))
                  (sb-sys:with-local-interrupts
                    ,@body))
             (setf (process-state *current-process*) ,old-state)))))))

(defun make-condition-variable () (sb-thread:make-waitqueue))

(defun condition-wait (cv lock &optional timeout)
  (if timeout
      (handler-case 
	  (sb-ext:with-timeout timeout
	    (sb-thread:condition-wait cv lock)
	    t)
	(sb-ext:timeout (c)
	  (declare (ignore c))
	  nil))
      (progn (sb-thread:condition-wait cv lock) t)))

(defun condition-notify (cv)
  (sb-thread:condition-notify cv))
