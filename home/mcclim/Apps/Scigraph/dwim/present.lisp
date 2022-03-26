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

;;; I guess this is as good of a place as any to put this.
(clim:define-command-table :global)

;;;
;;; Manipulating presentations
;;;

(progn
  (clim:define-gesture-name :left :pointer-button :left)
  (clim:define-gesture-name :middle :pointer-button :middle)
  (clim:define-gesture-name :right :pointer-button :right))

(defun presentation-under-pointer (stream)
  (multiple-value-bind (x y) (clim:stream-pointer-position stream)
    (clim::find-innermost-applicable-presentation '((t)) stream x y)))

(defun presentation-p (object)
  (typep object 'clim:presentation))

(defun presentation-object (presentation)
  (if (presentation-p presentation) (clim:presentation-object presentation)))

(defun presentation-subtypep (subtype supertype)
  (clim:presentation-subtypep subtype supertype))

(defun presentation-type-p (type)
  (clim:presentation-type-specifier-p type))

(defun describe-presentation-type (type &optional (stream *standard-output*))
  (clim:describe-presentation-type type stream))

(defun bounding-rectangle* (presentation)
  "Get the bounding edges of the presentation."
  (clim:with-bounding-rectangle*
      (left top right bottom) presentation
    (values left top right bottom)))

(defun redisplay (record stream)
  (clim:redisplay record stream :check-overlapping nil))

(defun redisplayable? (stream)
  (clim:redisplayable-stream-p stream))

(defun redisplayable-format (stream string &rest args)
  (if (eq stream 't) (setq stream *standard-output*))
  (if (redisplayable? stream)
      (let ((a (copy-list args)))
	(with-redisplayable-output (:stream stream
					    :unique-id string
					    :cache-value a
					    :cache-test #'equal)
	  (apply #'format stream string a)))
      (apply #'format stream string args)))

(defun accept-values (descriptions &key (prompt nil)
					(stream *query-io*)
					(own-window nil))
  (accepting-values
      (stream :own-window own-window :label prompt)
    (mapcar #'(lambda (description)
		(destructuring-bind (type &rest options)
		    description
		  (prog1 (apply #'accept type :stream stream
				:query-identifier
				(getf options :query-identifier (car description))
				options)
		    (terpri stream))))
	    descriptions)))

(defun menu-choose (choices
		    &key (prompt "Choose:")
		         default-choice)
  (clim:menu-choose choices
                    :associated-window *standard-input*
                    :label prompt :default-item default-choice))

;;;
;;; Presentation parser primitives
;;;

(defun read-char-for-accept (stream)
  (read-char stream nil nil))

(defun unread-char-for-accept (char stream)
  (if (clim:activation-gesture-p char)
      ;; unreading an activation character causes problems for stream-unread-gesture.
      (clim:with-activation-gestures (nil :override t) (unread-char char stream))
      (unread-char char stream)))

(defun peek-char-for-accept (stream &optional hang)
  (let ((ch (and (or hang
		     (not (interactive-stream-p stream))
		     (< (input-position stream) (insertion-pointer stream)))
		 (read-char-for-accept stream))))
    (when ch
      (unread-char-for-accept ch stream))
    ch))

(defun compare-char-for-accept (char-from-accept comparandum)
  (and char-from-accept
       (typecase char-from-accept
	 (character (char-equal comparandum char-from-accept))
	 (list
	   ;; this should only happen in DW.
	   (and (member (first char-from-accept) '(:activation :blip-character :accept))
		(characterp (second char-from-accept))
		(char-equal comparandum (second char-from-accept)))))))

(defun read-token (stream)
  (clim:read-token stream))

(defun input-position (stream)
  ;; This location identifies the current position of the parser in a (buffered)
  ;; input stream.  When a character gets read by read-char-for-accept, this pointer
  ;; gets incremented.  Upon failure, the parser backtracks by decrementing it.
  (if (clim:input-editing-stream-p stream)
      (clim:stream-scan-pointer stream)
      (file-position stream)))

(defmethod (setf input-position) (new stream)
  (if (clim:input-editing-stream-p stream)
      (setf (clim:stream-scan-pointer stream) new)
      (file-position stream new)))

(defun insertion-pointer (stream)
  (cond ((interactive-stream-p stream)
	 (clim:stream-insertion-pointer stream))))

(defvar *%%ready-to-catch%%* nil)

(defmacro catching-parser-failures (form &rest failure-actions)
  "Use this to catch a parser failure and do something about it."
  (let ((normal (gensym)))
    `(let ((*%%ready-to-catch%%* t))
       (catch ',normal
	 (catch 'catch-parser-failures
	   (handler-bind ((error #'(lambda (error)
				     (when (and (typep error 'clim:abort-gesture)
						(find-restart 'abort))
				       (invoke-restart 'abort))
				     (throw 'catch-parser-failures t))))
	     (throw ',normal ,form)))
	 ,@failure-actions))))

(defun input-not-of-required-type (stream object type)
  "Use this to signal a parser failure and cause backtracking."
  (declare (ignore stream))
  ;; Used by faes expression editor.  Don't use the one from clim or dw,
  ;; it's so fancy that it outsmarts itself.
  (when *%%ready-to-catch%%*
      (throw 'catch-parser-failures t))
  (if (eq object :failure)
      (clim:simple-parse-error "The input read was not of the required type.")
      (clim:input-not-of-required-type object type)))

(defun validate-object (object ptype)
  (let ((p (clim:expand-presentation-type-abbreviation ptype)))
    (clim::presentation-typep object p)))

(defmacro with-accept-activation-chars ((additional-characters &key override) &body body)
  `(clim:with-activation-gestures (,additional-characters :override ,override)
    ,@body))

(defun accept-activation-p (char &optional (achars))
  (declare (ignorable achars))
  (clim:activation-gesture-p char))

(defmacro with-accept-blip-chars ((additional-characters &key override) &body body)
  `(clim:with-delimiter-gestures (,additional-characters :override ,override) ,@body))

(defun accept-blip-p (char &optional (chars))
  (declare (ignorable chars))
  (clim:delimiter-gesture-p char))

(defmacro with-activation-characters ((additional-characters &key override) &body body)
  `(with-accept-activation-chars (,additional-characters :override ,override) ,@body))

(defmacro with-blip-characters ((additional-characters &key override) &body body)
  `(with-accept-blip-chars (,additional-characters :override ,override) ,@body))

(defmacro completing-from-suggestions
	  ((stream &key delimiters allow-any-input
		   (initially-display-possibilities nil idp)
		   (type nil typep))
	   &body body)
  (declare (ignore initially-display-possibilities type))
  (progn
    (if idp (format t "~% completing-from-suggestions :initially-display-possibilities not supported."))
    (if typep (format t "~% completing-from-suggestions :type not supported."))
    `(clim:completing-from-suggestions
	 (,stream :partial-completers ,delimiters :allow-any-input ,allow-any-input)
       ,@body)))

(defun complete-from-sequence (sequence stream &key type (name-key #'string))
  (declare (ignore type))
  (completing-from-suggestions (stream)
    (map nil #'(lambda (elt) (suggest (funcall name-key elt) elt)) sequence)))

;;; JPM.  This isn't really portable because it generates a
;;; DWish blip object.  Use with-input-context below, if you can.
(defmacro with-presentation-input-context
	  ((PRESENTATION-TYPE &rest OPTIONS)
	   (&optional (BLIP-VAR   '.BLIP.))
	   NON-BLIP-FORM
	   &body CLAUSES)
  `(clim:with-input-context (,PRESENTATION-TYPE :override ,(getf options :inherit))
			    (.object. .presentation-type. .gesture.)
	,NON-BLIP-FORM
	,@(mapcar #'(lambda (clause)
		      `(,(first clause)
			(let ((,blip-var (list .presentation-type.
					       .gesture.
					       .object.)))
			  ,blip-var
			  ,@(rest clause))))
		  CLAUSES)))

(defmacro with-input-context
	  ((PRESENTATION-TYPE &key OVERRIDE STREAM)
	   (&optional (OBJECT-VAR '.object.)
		      (PT-VAR     '.presentation-type.)
		      (GESTURE-VAR   '.gesture. gesture-p))
	   NON-BLIP-FORM
	   &body CLAUSES)
  (declare (ignore stream))
  `(clim:with-input-context (,PRESENTATION-TYPE :override ,OVERRIDE)
			    (,OBJECT-VAR ,PT-VAR ,@(if gesture-p GESTURE-VAR))
	,NON-BLIP-FORM
      ,@CLAUSES))

;;;
;;; Presentation types
;;;

(define-presentation-type sheet ()
   :parser ((stream)
	    (progn
	      (read-char stream)
	      (error "The SHEET presentation type is broken.  Sorry.")))
   :printer ((window stream)
	     (let ((*print-readably* nil))
               (format stream "~A" window)))
   :description "a window")

(clim:define-presentation-type-abbreviation alist-member
    (&key (alist (error "The :ALIST keyword argument is required"))
          (test 'eql))
  `(clim:member-alist ,alist :test ,test))

(defun menu-execute-no-side-effects (item)
  (cond ((atom item) item)
	((atom (cdr item)) (cdr item))
	((atom (cddr item)) (cadr item))
	((eq (second item) :value) (third item))))

(defun token-element-string (element)
  (typecase element
    (null (symbol-name element))
    (cons (string (first element)))
    (symbol (string-capitalize (symbol-name element)))
    (string element)
    (otherwise (present-to-string element))))

(defun make-accept-values-choices (&key query-identifier sequence select-action)
  (declare (ignorable sequence))
  (clim-internals::make-accept-values-multiple-choices
   :query-identifier query-identifier
   :select-action select-action))

(defun make-accept-values-choice (&key choices choice value documentation)
  (declare (ignore documentation))
  (declare (ignorable choice))
  (clim-internals::make-accept-values-multiple-choice
   :choices choices
   :value value))

(defun type-for-avv-choice ()
  'clim-internals::accept-values-one-of)

(defun accept-values-choose-from-sequence
    (stream sequence selected-value query-identifier
     &key
     drawer select-action
     n-columns n-rows
     (selection-test #'eq)
     (value-key #'menu-execute-no-side-effects)
     (name-key #'token-element-string)
     (choice-type (type-for-avv-choice))
     (make-choices #'make-accept-values-choices)
     (make-choice #'make-accept-values-choice))
  "Used for the ACCEPT-VALUES method of some presentation types."
  ;; This is how ALIST-MEMBER works.
  ;; DRAWER: how to draw an element in the sequence.
  ;; SELECT-ACTION: how to combine the selected choice with the default value.
  (when (not drawer)
    (setq drawer
      #'(lambda (str obj name selected)
	  (declare (ignore obj))
	  (clim:formatting-cell (str)
            (if selected
                (with-text-face (:bold str) (write-string name str))
                (write-string name str))))))
  (when (not select-action)
    (setq select-action
      #'(lambda (choice default-val)
	  (declare (ignore default-val))
	  choice)))
  (let ((choices (funcall make-choices
			  :query-identifier query-identifier
			  :sequence sequence
			  :select-action select-action))
	;;(width (- (stream-viewport-size stream) (stream-cursor-position* stream)))
	)
    (labels ((draw-one (item value pretty-name selected-p stream)
	       (with-output-as-presentation
		   (:type choice-type
			  :stream stream
			  :object (funcall make-choice
					   :choices choices
					   :choice item
					   :value value
					   :documentation pretty-name)
			  :single-box t)
		 (clim:with-room-for-graphics (stream)
                   (clim:formatting-cell (stream)
                     (funcall drawer stream value pretty-name selected-p)))))
	     (draw-all (sequence stream)
	       (dolist (item sequence)
		 (let* ((value (funcall value-key item))
			(pretty-name (funcall name-key item))
			(selected-p (funcall selection-test value selected-value)))
		   (draw-one item value pretty-name selected-p stream)))))
      (with-output-as-presentation (:stream stream :single-box t)
	(clim:formatting-item-list
            (stream :n-columns n-columns
                    :n-rows n-rows
                    :x-spacing '(2 :character))
	 (draw-all sequence stream))))))

(define-presentation-type alist-subset
    (&key (alist (error "The :ALIST keyword argument is required")))
  ;; Yes, I know clim 1.0 has one of these, but it doesn't work in avv mode!.
  :parser ((stream)
	   (accept `(sequence (alist-member :alist ,alist))
		   :stream stream :prompt nil))
  :printer ((object stream)
	    (do ((sequence object (cdr sequence)))
		((null sequence))
	      (let ((element (find (first sequence) alist
				   :key #'menu-execute-no-side-effects)))
		(write-string (token-element-string element) stream))
	      (unless (= (length sequence) 1)
		(write-string ", " stream))))
  :typep ((object)
	  (block testem
	    (dolist (element object)
	      (or (find element alist :key #'menu-execute-no-side-effects)
		  (return-from testem nil)))
	    t))
  :describer ((stream)
	      (write-string "any of " stream)
	      ;; -- CLIM doesn't have a general list formatter yet
	      (let (length)
		(do ((rest-of-elements alist (rest rest-of-elements)))
		    ((not rest-of-elements))
		  (setq length (length rest-of-elements))
		  (format stream "~A" (token-element-string (car rest-of-elements)))
		  (cond ((> length 2)
			 (write-string ", " stream))
			((= length 2)
			 (write-string " or " stream))))))
  :accept-values-displayer
  ((stream object query-identifier)
   ;; OBJECT is the currently chosen subset.
   ;; OBJECT is the currently chosen subset.
   (accept-values-choose-from-sequence
     stream alist object query-identifier
     :select-action
     #'(lambda (new list)
	 (cond ((not (listp list)) (list new))
	       ((member new list)  (remove new list))
	       (t (adjoin new list))))
     :selection-test #'member
     :drawer
     #'(lambda (stream object name selected-p)
	 (declare (ignore object))
	 (if selected-p
	     (with-character-face (:bold stream)
	       (format stream "~A" name))
	     (format stream "~A" name))))))

(defun all-characters (&optional (n (1+ (char-code #\rubout))))
  (let ((list nil))
    (dotimes (i n) (push (code-char i) list))
    list))

(defvar *all-characters* nil)

(defun readline-no-echo (stream)
  (clim:with-output-recording-options (stream :draw nil :record nil)
    (accept 'string :stream stream :prompt nil :default nil)))

;;; A hack so the user doesnt have to see some ugly commands get echoed.
;;; Also seems like a useful way to read a password.
(define-presentation-type invisible-object ()
  :parser ((stream)
	   (values (readline-no-echo stream) 'invisible-object))
  :printer ((object stream)
            (declare (ignore object))
	    (write-string "*" stream)))
