;;; -*- Syntax: Common-lisp; Package: GRAPH -*-
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

(in-package :graph)

;;; This file contains functionality for editing instances in a popup-style window.
;;; You do not need to "mix" anything in to get the functionality, you just need to
;;; provide some methods to customize the behavior.
;;;
;;; (POPUP-ACCEPT instance stream), top level function call
;;;
;;; (POPUP-ACCEPTABLE instance), returns T if you can pop-edit the instance.  The
;;;   default is NIL.  
;;;
;;; (POP-ACCEPT-ITEMS instance), a PROGN-combined method that formats each of the 
;;;   items in the display.
;;; 
;;; (POP-ACCEPT-LABEL instance), the title for the editor pane.
;;;
;;; (POP-ACCEPT-UNSATISFIED-WARNINGS instance), an OR-combined method that provides
;;;   constraints that encourage the user to make sense.  Constraints are not rigidly
;;;   enforced until the user tries to quit, because users are willing to tolerate
;;;   temporary inconsistencies.
;;;
;;; ABORT is not handled by editing a copy of the object.  Rather, a snapshot of the
;;; object is taken beforehand, and an abort handler is provided to restore the
;;; object's state afterward if necessary, based on the saved snapshot.

;;;********************************************************************************
;;; 
;;;********************************************************************************

;;; Here we provide the default behavior, which is extremely simple and which works
;;; on any instance.

(defmethod pop-accept-label ((self t)) (present-to-string self))

(defmethod popup-acceptable ((self t)) nil)

(defmethod popup-accept-variables ((self standard-object))
    "Get list of instance variables."
    (let ((slots 
           (mapcar #'c2mop:slot-definition-name
                   (c2mop:class-slots (class-of self)))))
      (sort slots #'string-lessp)))

(defconstant *unbound* '%%unbound%%)

(defmethod restore-object-state ((self standard-object) snapshot)
  "Abort handler to restore original variable values."
  (loop for variable in (popup-accept-variables self)
	for value in snapshot
	do (if (eq value *unbound*)
	       (slot-makunbound self variable)
	       (setf (slot-value self variable) value))))

(defmethod snapshot-object-state ((self standard-object))
  "Record original variable values in case of abort."
  (loop for variable in (popup-accept-variables self)
	for VALUE = (if (slot-boundp self variable)
			(slot-value self variable)
			*unbound*)
	;;;NLC19NOV90 - SOME OF THE LISTS ARE EXPLICITLY BASHED DURING THE POPUP
	collect (if (consp VALUE) (copy-list VALUE) VALUE)))

(defmethod abort-protect ((self t) (continuation t))
  ;;(declare (values instance aborted-p))
  (let ((snapshot (snapshot-object-state self)))
    (let ((value (funcall continuation)))
      (when (eq value :abort)
	(restore-object-state self snapshot))
      value)))

(defmethod popup-accept ((SELF standard-object) STREAM)
  ;; Default way to edit an instance, works on any instance.
  ;; Collect up the instance variables and accept new values for them.
  (let ((TITLE (pop-accept-label SELF)))
    (accepting-values (stream :own-window t
			      :label title)
      (dolist (var (popup-accept-variables self))
	(if (slot-boundp self var)
	    (setf (slot-value self var)
		  (accept 'expression
			  :stream stream
			  :default (slot-value self var)
			  :prompt (string var)))
	    (format stream "~A: unbound" var))
	(terpri stream)))))

(defmethod popup-accept :around ((SELF t) STREAM)
  (let ((*print-level* 5)		  ; Avoid printing deeply nested
	(*print-circle* t)		  ; and circular 
	(*print-length* 25))		  ; and long structures.
    (abort-protect
      self
      #'(lambda () (call-next-method self stream)))))

(defmethod popup-accept-from-presentation ((self t) (stream t) (presentation t))
  "Like popup-accept except you get a hook onto the presentation."
  ;;(declare (values self aborted-p))
  (values self (eq (popup-accept self stream) :abort)))

(define-command (com-pop-edit :command-table :global)
    ((object 'invisible-object) (window 'sheet) (presentation 'invisible-object))
   (popup-accept-from-presentation object WINDOW PRESENTATION))

;;; Scigraph no longer uses this translator to edit graphs and graph-data.
;;; The documentation string is too generic and tends to confuse naive users.
;;; Elsewhere there are better translators specific to those ptypes.  This
;;; translator remains as a tool that others might want to use.
(define-presentation-to-command-translator com-pop-edit
   (t :command-name com-pop-edit
      :command-table :graph
      :tester ((object) (popup-acceptable object))
      :documentation "Edit (Pop Up Window)"
      :menu t
      :gesture :edit)
   (object &key presentation window)
  (list object window presentation))

(install-command 'clim::accept-values 'com-pop-edit)


;;;
;;; Syntactic sugar and constraint checking.
;;;

(defgeneric pop-accept-items (self MENU-STREAM GRAPH-WINDOW)
  (:method-combination progn :most-specific-last)) 

;;; +++ Until lucid gets :most-specific-last, pop-accept-items isn't going to work
;;; quite right for the case where display is conditioned on the values of other
;;; items.  Accepting-values doesn't make a full pass over all the items, which is a
;;; problem without :most-specific-last.  jpm 1 Mar 91.  +++

(defgeneric pop-accept-unsatisfied-warnings (self)
  (:method-combination or))

(defmethod pop-accept-unsatisfied-warnings or ((self t)) nil)


(defmacro popup-accept-forms-accept
	  (STREAM PROMPT-STRING ATYPE DEFAULT QUERY-ID &rest ACCEPT-ARGS)
  `(accept ,ATYPE
	   :stream ,STREAM
	   :prompt ,PROMPT-STRING
	   :default ,DEFAULT
	   :query-identifier ,QUERY-ID
	   ,@ACCEPT-ARGS))

;;; Added code to return a values list of the accepted value and
;;; a flag indicating that the value changed (Test EQL). Clim 1.0
;;; does something like this AGB

(defmacro popup-accept-forms-var (STREAM VAR-NAME PROMPT-STRING ATYPE &rest ACCEPT-ARGS)
  `(let ((%%old-value%% ,var-name))
    (setf ,VAR-NAME
     (popup-accept-forms-accept
      ,STREAM
      ,PROMPT-STRING
      ,ATYPE
      %%old-value%%
      ,VAR-NAME;;here was ',
      ,@ACCEPT-ARGS))
    (terpri ,stream)
    (values ,var-name (not (eql ,var-name %%old-value%%)))))

(defmacro popup-accept-forms-slot
	  (OBJECT STREAM SLOT-NAME PROMPT-STRING ATYPE &rest ACCEPT-ARGS)
  `(let ((%%old-value%% (slot-value ,OBJECT ,SLOT-NAME)))
    (setf (slot-value ,OBJECT ,SLOT-NAME)
     (popup-accept-forms-accept
      ,STREAM
      ,PROMPT-STRING
      ,ATYPE
      %%old-value%%
      ,SLOT-NAME;;here was ',
      ,@ACCEPT-ARGS))
    (terpri ,stream)
    (values (slot-value ,OBJECT ,SLOT-NAME) (not (eql (slot-value ,OBJECT ,SLOT-NAME) %%old-value%%)))))

(defmacro popup-accept-forms-string ((STREAM &optional (TERPRI? t)) STRING &rest ARGS)
  `(progn
     (redisplayable-format ,STREAM ,STRING ,@ARGS)
     ,(and TERPRI? `(terpri ,STREAM))))

(defmacro popup-accept-forms-warn ((STREAM &optional (TERPRI? t)) STRING &rest ARGS)
  `(with-character-face (:bold ,stream)
     (redisplayable-format
       ,STREAM
       "WARNING:<<~?>>"
       ,STRING ,ARGS)
     ,(and TERPRI? `(terpri ,STREAM))))

;;; Acceptable FORMS
;;;  (PA-SLOT  <Slot-Name> <Prompt-String> <Accept-Type> . <Accept-Args> )
;;;  (PA-VAR   <Variable-Name> <Prompt-String> <Accept-Type> . <Accept-Args> )
;;;  (PA-STRING  <String> &rest ARGS)  Followed by (Terpri)
;;;  (PA-STRING1 <String> &rest ARGS)  NOT Followed by (Terpri)
(defmacro popup-accept-forms ((STREAM &optional (OBJECT 'SELF)) &body BODY)
  `(macrolet ((pa-accept (PROMPT-STRING ATYPE DEFAULT QUERY-ID &rest ACCEPT-ARGS)
		`(popup-accept-forms-accept
		   ,',STREAM ,PROMPT-STRING ,ATYPE ,DEFAULT ,QUERY-ID ,@ACCEPT-ARGS))
	      
	      (pa-slot (SLOT-NAME PROMPT-STRING ATYPE &rest ACCEPT-ARGS)
		`(popup-accept-forms-slot
		   ,',OBJECT ,',STREAM ',SLOT-NAME ,PROMPT-STRING ,ATYPE ,@ACCEPT-ARGS))
	      
	      (pa-var (VAR-NAME PROMPT-STRING ATYPE &rest ACCEPT-ARGS)
		`(popup-accept-forms-var
		   ,',STREAM ,VAR-NAME,PROMPT-STRING ,ATYPE ,@ACCEPT-ARGS))

	      (pa-warn (STRING &rest ARGS)
		`(popup-accept-forms-warn (,',STREAM)  ,STRING ,@ARGS))
	      
	      (pa-string (STRING &rest ARGS)
		`(popup-accept-forms-string (,',STREAM)  ,STRING ,@ARGS))
	      
	      (pa-string1 (STRING &rest ARGS)
		`(popup-accept-forms-string (,',STREAM nil) ,STRING ,@ARGS))
	      )
     (progn
       ,@BODY)))

(defvar *avv-extra-redisplay* nil)

(defun popup-accept-standard-loop (self stream)
  "Used by most POPUP-ACCEPT methods."
  (let ((GRAPH-WINDOW STREAM)
	(MENU-MENU-STREAM STREAM)
	(TITLE (pop-accept-label SELF))
	(result self)
	(*avv-extra-redisplay* t)
	(own-window ;; Clim/Lucid may lose if you pass a list that the compiler
		    ;; may have assumed was a constant, so copy it at run
		    ;; time.  ("Segmentation Violation")
                    (copy-list '(:left 150 :bottom 150
                                 :right-margin 50
                                 :bottom-margin 200))))
    (loop for FIRST-TIME? = t then nil
	  do (setq result
		   (accepting-values (menu-menu-stream
				      :own-window own-window
				      :label title)
		     (unless FIRST-TIME?
		       (popup-accept-forms-string
			 (menu-menu-stream nil)
			 "To proceed, correct all condition marked with ")
		       (popup-accept-forms-warn (menu-menu-stream) ".")
		       (popup-accept-forms-string
			 (menu-menu-stream)
			 "Or click on \"Abort\" to cancel all changes.~%"))
		     (pop-accept-items SELF MENU-MENU-STREAM GRAPH-WINDOW)
		     ))
	  while (pop-accept-unsatisfied-warnings SELF)
	  do (progn (beep) (beep)))
    result))

