;;; Some of the 7 extremely random interface commands
;;; got replaced by defpackage.

;;; Use uninterned symbols here so that:
;;; 1. there's no iterate symbol in package USER.
;;; 2. it may work in case-sensitive mode/packages.

(defpackage #:iterate
  (:use #:cl)
  (:nicknames #:ITER)
  (:export #:iterate #:iter #:display-iterate-clauses
	   #:defsynonym #:dsetq #:declare-variables
	   #:defmacro-clause #:defmacro-driver #:defclause-sequence
	   #:initially #:after-each #:finally #:finally-protected
	   #:else #:if-first-time #:first-iteration-p #:first-time-p
	   #:finish #:leave #:next-iteration #:next #:terminate
	   #:repeat #:for #:as #:generate #:generating #:in
	   #:sum #:summing #:multiply #:multiplying
	   #:maximize #:minimize #:maximizing #:minimizing #:counting
	   #:always #:never #:thereis #:finding #:collect #:collecting
	   #:with #:while #:until #:adjoining #:nconcing #:appending
	   #:nunioning #:unioning #:reducing #:accumulate #:accumulating))

(in-package #:iterate)

;;; work around sbcl's obnoxious standard compliance

(defmacro defconst (name value &optional doc)
   `(eval-when (:compile-toplevel :load-toplevel :execute)
      (unless (boundp ',name)
        ,(if doc
             `(defconstant ,name ,value ,doc)
           `(defconstant ,name ,value)))))

;;; arch-tag: "b8bb0bb6-313c-11d8-abb9-000c76244c24"
