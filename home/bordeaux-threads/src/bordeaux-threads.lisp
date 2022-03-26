;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

(defvar *supports-threads-p* nil
  "This should be set to T if the running instance has thread support.")

(defun mark-supported ()
  (setf *supports-threads-p* t)
  (pushnew :bordeaux-threads *features*))

(define-condition bordeaux-mp-condition (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream (message condition)))))

(defgeneric make-threading-support-error ()
  (:documentation "Creates a BORDEAUX-THREADS condition which specifies
  whether there is no BORDEAUX-THREADS support for the implementation, no
  threads enabled for the system, or no support for a particular
  function.")
  (:method ()
    (make-condition
     'bordeaux-mp-condition
     :message (if *supports-threads-p*
                  "There is no support for this method on this implementation."
                  "There is no thread support in this instance."))))

#-sbcl
(define-condition timeout (serious-condition)
  ((length :initform nil
             :initarg :length
             :reader timeout-length))
  (:report (lambda (c s)
             (if (timeout-length c)
                 (format s "A timeout set to ~A seconds occurred."
                         (timeout-length c))
                 (format s "A timeout occurred.")))))


;;; Thread Creation

;;; See default-implementations.lisp for MAKE-THREAD.

;; Forms are evaluated in the new thread or in the calling thread?
(defvar *default-special-bindings* nil
  "This variable holds an alist associating special variable symbols
  to forms to evaluate. Special variables named in this list will
  be locally bound in the new thread before it begins executing user code.

  This variable may be rebound around calls to MAKE-THREAD to
  add/alter default bindings. The effect of mutating this list is
  undefined, but earlier forms take precedence over later forms for
  the same symbol, so defaults may be overridden by consing to the
  head of the list.")

(defmacro defbindings (name docstring &body initforms)
  (check-type docstring string)
  `(defparameter ,name
     (list
      ,@(loop for (special form) in initforms
              collect `(cons ',special ',form)))
     ,docstring))

;; Forms are evaluated in the new thread or in the calling thread?
(defbindings *standard-io-bindings*
  "Standard bindings of printer/reader control variables as per CL:WITH-STANDARD-IO-SYNTAX."
  (*package*                   (find-package :common-lisp-user))
  (*print-array*               t)
  (*print-base*                10)
  (*print-case*                :upcase)
  (*print-circle*              nil)
  (*print-escape*              t)
  (*print-gensym*              t)
  (*print-length*              nil)
  (*print-level*               nil)
  (*print-lines*               nil)
  (*print-miser-width*         nil)
  (*print-pprint-dispatch*     (copy-pprint-dispatch nil))
  (*print-pretty*              nil)
  (*print-radix*               nil)
  (*print-readably*            t)
  (*print-right-margin*        nil)
  (*read-base*                 10)
  (*read-default-float-format* 'single-float)
  (*read-eval*                 t)
  (*read-suppress*             nil)
  (*readtable*                 (copy-readtable nil)))

(defun binding-default-specials (function special-bindings)
  "Return a closure that binds the symbols in SPECIAL-BINDINGS and calls
FUNCTION."
  (let ((specials (remove-duplicates special-bindings :from-end t :key #'car)))
    (lambda ()
      (progv (mapcar #'car specials)
          (loop for (nil . form) in specials collect (eval form))
        (funcall function)))))

;;; FIXME: This test won't work if CURRENT-THREAD
;;;        conses a new object each time
(defun signal-error-if-current-thread (thread)
  (when (eq thread (current-thread))
    (error 'bordeaux-mp-condition
           :message "Cannot destroy the current thread")))

(defparameter *no-condition-wait-timeout-message*
  "CONDITION-WAIT with :TIMEOUT is not available for this Lisp implementation.")

(defun signal-error-if-condition-wait-timeout (timeout)
  (when timeout
    (error 'bordeaux-mp-condition
           :message *no-condition-wait-timeout-message*)))

(defmacro define-condition-wait-compiler-macro ()
  `(define-compiler-macro condition-wait
       (&whole whole condition-variable lock &key timeout)
    (declare (ignore condition-variable lock))
    (when timeout
      (simple-style-warning *no-condition-wait-timeout-message*))
    whole))
