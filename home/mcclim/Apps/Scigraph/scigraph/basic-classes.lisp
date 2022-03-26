;;; -*- Syntax: Common-lisp; Package: TOOL -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :tool)

;;; NAMED-MIXIN

;;; Allows an object to have a name that effects the way it is printed or chosen from
;;; a menu. If a name is not provided one will be generated automatically like
;;; OBJECT-1, OBJECT-2 ....

(defclass named-mixin
	  ()
    ((name :initform nil :initarg :name :reader name))
  (:documentation
    "Allows each instance to have a name.  One is generated for it if not provided.
     The name is always a symbol."))

(defmethod initialize-instance :after ((self named-mixin) &key)
  "Generate a name if necessary."
  (let ((name (name self)))
    (when (or (not name) (not (symbolp (name self))))
      (setf (name self) (or name (make-name self))))))

(defmethod make-name ((self named-mixin))
  "Make a name for yourself if necessary."
  (let ((class-name (class-name (class-of self))))
    (intern
      (format nil "~a-~a" class-name
	      (setf (get class-name 'name-index)
		    (1+ (or (get class-name 'name-index) 0))))
      (symbol-package class-name))))

(defmethod (setf name) (new-name (self named-mixin))
  "This version make sure name is a symbol.  You may want something else."
  (setf (slot-value self 'name)
	(cond ((not new-name) (make-name self))
	      ((symbolp new-name) new-name)
	      ((stringp new-name) (intern new-name))
	      (t (intern (format nil "~a" new-name))))))

(defmethod name-string ((self t))
  "Returns name as a string."
  (let ((name (name self)))
    (cond ((stringp name) name)
	  ((symbolp name) (string-capitalize name))
	  (t (format nil "~a" name)))))

(defmethod print-object ((self named-mixin) stream)
  (if *print-escape*
      (dwim:printing-random-object (self stream :no-pointer)
	(format stream "~a ~a" (class-name (class-of self)) (name self)))
      (format stream "~a" (name self))))


;;; Since Objects do not have negative inheritance, it is difficult to get rid of
;;; behavior once it has been mixed in (append combinded methods for example).  Thus
;;; methods for the OBJECT protocols are sometimes broken out into separate mixins
;;; with the suffix -OB-MIXIN.  This gives you better control over pop-edit etc.

;;; Handy macro.  
;;; Make a completely unspecific version of the method.  If this
;;; gets called, it's a bug in the setup of the code.

(defmacro declare-required-method (METHOD-NAME METHOD-ARGS)
  (let* ((REAL-ARGS ())
	 (ARG-LIST (loop for THING in METHOD-ARGS
			 as REAL-ARG =
			    (cond ((listp THING)
				   (first THING))
				  ((and THING
					(not (char-equal (aref (symbol-name THING) 0) #\&)))
				   THING))
			 when REAL-ARG
			   do (push REAL-ARG REAL-ARGS)
			 collecting (or REAL-ARG THING))))
    `(defmethod ,METHOD-NAME ,ARG-LIST
		(declare (ignore ,@REAL-ARGS))
       (error "Undefined :required-method ~A" ',method-name))))
