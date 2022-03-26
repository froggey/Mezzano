;;; -*- Syntax: Common-lisp; Package: DWIM -*-
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

(in-package :dwim)

;;;*****************
;;; Lisp Extensions
;;;*****************

(defmacro with-rem-keywords ((new-list list keywords-to-remove) &body body)
  `(let ((,new-list (with-rem-keywords-internal ,list ,keywords-to-remove)))
    ,@body))

(defun with-rem-keywords-internal (keyword-list keywords-to-remove)
  ;; Remove leading keywords.
  (loop (unless (member (car keyword-list) keywords-to-remove)
	  (return))
	(setf keyword-list (cddr keyword-list)))
  (when keyword-list
    (do* ((kwl keyword-list cons2)	
	  (cons1 (cdr kwl) (cdr kwl))
	  (cons2 (cdr cons1) (cdr cons1)))
	 ((null cons2) keyword-list)
      (when (member (car cons2) keywords-to-remove)
	(setf (cdr cons1) (cddr cons2))
	(setf cons2 kwl)))))

(defun rem-keywords (list keywords-to-remove)
  (with-rem-keywords (new-list list keywords-to-remove)
    (copy-list new-list)))




;;; **************************
;;; UNIX Environmental Support
;;; **************************
  
						
(defun getenv (string)
  "Get the value of the environment variable named STRING."
  (assert (stringp string))
  #+lucid
  (lucid::environment-variable string)

  #+lispworks
  (lispworks:environment-variable name)

  #+allegro
  (system:getenv string)

  #+genera
  (let ((symbol (intern string :scl)))
    (and (boundp symbol) (symbol-value symbol)))

  #+openmcl
  (let ((symbol (intern string :scl)))
    (and (boundp symbol) (symbol-value symbol)))

  #+sbcl
  (sb-ext:posix-getenv string)

  #+scl
  (cdr (assoc string ext:*environment-list* :test #'string=)))

(defun process-run-function (name-or-keywords function &rest args)
  (let* ((new-args (copy-list args)) ; in case of stack-allocation
	 (predicate
	  (if args #'(lambda () (apply function new-args)) function)))
    (clim-sys:make-process predicate :name name-or-keywords)))

(defun type-specifier-p (object)
  "Determine if OBJECT is a valid type specifier"
  ;; A somewhat consful implementation, but entirely portable.
  (let ((test #'(lambda (x) (typep 't x))))
    (when (or (symbolp object) (listp object))
      (multiple-value-bind (v errorp) (ignore-errors (funcall test object))
	(declare (ignore v))
	(not errorp)))))

;;;************
;;; DUMPER
;;;************

;;; Here is the simplest possible dumper that worries about the EQ-ness of shared
;;; objects.  Objects that satisfy INSTANCEP are put into a hash table.  References
;;; to such objects are replaced with corresponding GETHASH forms:
;;;
;;;      `(progn (setq frog #<FROG 123>) (setq reptile #<FROG 123>))
;;;
;;;           becomes something like
;;;
;;;      (let ((*dump-table* (make-hash-table)))
;;;         (setf (gethash 0 *dump-table*) (make-instance 'frog))
;;;         (progn (setq frog (gethash 0 *dump-table*)))
;;;                (setq reptile (gethash 0 *dump-table*))))
;;;
;;; Thus the dumper augments the forms being dumped with appropriate calls to
;;; MAKE-HASH-TABLE and GETHASH.  Thus any Common Lisp reader is sufficient
;;; to parse this ASCII file.
;;;
;;; KRA has pointed out that when you load via (EVAL (READ)), the form being read
;;; can be as large as the object being recreated.  After the EVAL, the form is
;;; thrown away, which may be somewhat difficult for the GC to swallow if it is
;;; large.  A better dumper would be more considerate of the GC.
;;;
;;; Some day, this dumper should probably be extended to worry about structure-sharing
;;; of all structures, particularly lists and instances of STRUCTURE-OBJECT.  

(defvar *dump-table* (make-hash-table) "Hash table used by the dumper.")
(defvar *dump-index* 0 "Counter used by the dumper.")

(defmacro writing-dump-file ((stream-var file) &body body)
  `(let ((*dump-table* (make-hash-table))
	 (*dump-index* 0))
     (with-open-file (,stream-var ,file :direction :output :if-exists :supersede)
       ,@body)))

(defun enter-table (object)
  (setf (gethash object *dump-table*) t)
  (incf *dump-index*))

(defun finish-enter-table (object index)
  (setf (gethash object *dump-table*) (1- index)))

(defun dump-form-to-eval (form stream)
  "Dump a form directly to the stream."
  (print form stream))

(defun dump-instance (object stream)
  "Use MAKE-LOAD-FORM to dump this object to the stream."
  (multiple-value-bind (maker initializer) (make-load-form object)
    (let ((index (enter-table object))
	  (symbol (intern "*DUMP-TABLE*")))
      (dump-form-to-file `(setf (gethash ,(1- index) ,symbol) ,maker) stream)
      (finish-enter-table object index)
      (when initializer (dump-form-to-file initializer stream)))))

(defun dump-form-to-file (form stream)
  "Dump a form that may refer to instances."
  (labels ((tree-search (tree predicate)
	     (if (atom tree)
		 (if (funcall predicate tree)
		     (return-from tree-search t)
		   nil)
	       (dolist (element tree)
		 (if (tree-search element predicate)
		     (return-from tree-search t)))))
	   (need-full-dump (object)
	     (or (and (arrayp object) (not (stringp object)))
		 (hash-table-p object )))
	   (traverse (form)
	     (let (index)
	       (cond ((need-full-dump form)
		      (cond ((setq index (gethash form *dump-table*))
			     (if (not (numberp index))
				 (error "Circular dependency encountered.")
			       `(gethash ,index ,(intern "*DUMP-TABLE*"))))
			    (t (dump-instance form stream)
			       (traverse form))))
		     ((atom form) form)
		     ((not (eq (first form) 'quote))
		      ;; The normal case, an unquoted form.
		      (mapcar #'traverse form))
		     ((tree-search form #'need-full-dump)
		      ;; KRA has pointed out that LIST can take no more than 512
		      ;; arguments in Lucid.  That means the following will break
		      ;; if the list is a long one.
		      (if (atom (second form))
			  (traverse (second form)) ; quoted instance
			(traverse ; list contains instances
			 `(list ,@(mapcar #'(lambda (x) `(quote ,x))
					  (second form)))))) 
		     (t form)))))
    (dump-form-to-eval (traverse form) stream)))

(defun dump-objects-to-file (file objects
			     &optional (package #+ansi-cl :common-lisp-user
						#-ansi-cl :user))
  "Use the MAKE-LOAD-FORM protocol to dump a list of objects to the specified file."
  (or (and (symbolp package) (find-package package))
      (error "Package ~A does not exist" package))
  (setq file (namestring (pathname file)))
  (let ((*package* (find-package package)))
    (writing-dump-file (stream file)
      (format stream ";-*- Package: ~A; Syntax: Common-Lisp -*-" (package-name *package*))
      (format stream "~%(lisp::in-package ~S)" (package-name *package*))
      (format stream "~%(let ((~S (make-hash-table)))"
	      (intern "*DUMP-TABLE*"))
      (dolist (object objects)
	(dump-instance object stream))
      (format stream "~%)"))))



