;;; -*- Mode: Lisp; Package: AUTOMATON -*-
;;;
;;;  (c) copyright 2005-2007 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;
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

;;; Derived from dk.brics.automaton v1.8.1, (c) 2001-2005 by Anders M/oller
;;; - Some comments have been copied verbatim from the original code.

;;; Regular expressions are built from the following abstract syntax:

;;; regexp	::=	unionexp
;;; unionexp	::=	interexp | unionexp	(union)
;;;                   |	interexp
;;; interexp	::=	concatexp & interexp	(intersection)	[OPTIONAL]
;;;                   |	concatexp
;;; concatexp	::=	repeatexp concatexp	(concatenation)
;;;                   |	repeatexp
;;; repeatexp	::=	repeatexp ?	(zero or one occurrence)
;;;                   |	repeatexp *	(zero or more occurrences)
;;;                   |	repeatexp +	(one or more occurrences)
;;;                   |	repeatexp {n}	(n occurrences)
;;;                   |	repeatexp {n,}	(n or more occurrences)
;;;                   |	repeatexp {n,m}	(n to m occurrences, including both)
;;;                   |	complexp
;;; complexp	::=	~ complexp	(complement)	[OPTIONAL]
;;;                   |	charclassexp
;;; charclassexp	::=	[ charclasses ]	(character class)
;;;                   |	[^ charclasses ]	(negated character class)
;;;                   |	simpleexp
;;; charclasses	::=	charclass charclasses
;;;                   |	charclass
;;; charclass	::=	charexp - charexp	(character range, including end-points)
;;;                   |	charexp
;;; simpleexp	::=	charexp
;;;                   |	.	(any single character)
;;;                   |	#	(the empty language)	[OPTIONAL]
;;;                   |	@	(any string)	[OPTIONAL]
;;;                   |	" <Unicode string without double-quotes> "	(a string)
;;;                   |	( )	(the empty string)
;;;                   |	( unionexp )	(precedence override)
;;;                   |	< <identifier> >	(named automaton)	[OPTIONAL]
;;;                   |	<n-m>	(numerical interval)	[OPTIONAL]
;;; charexp	::=	<Unicode character>	(a single non-reserved character)
;;;                   |	\ <Unicode character> 	(a single character)

;;; The productions marked [OPTIONAL] are only allowed if specified by
;;; the syntax flags passed to the string-regexp constructor. The
;;; reserved characters used in the (enabled) syntax must be escaped
;;; with backslash (\) or double-quotes ("..."). (In contrast to other
;;; regexp syntaxes, this is required also in character classes.) Be
;;; aware that dash (-) has a special meaning in charclass
;;; expressions. An identifier is a string not containing right angle
;;; bracket (>) or dash (-). Numerical intervals are specified by
;;; non-negative decimal integers and include both end points, and if
;;; n and m have the same number of digits, then the conforming
;;; strings must have that length (i.e. prefixed by 0's).

(in-package :automaton)

(deftype kind ()
  '(member nil :union :concatenation :intersection :optional :repeat
    :repeat-min :repeat-minmax :complement :char :char-range :anychar :empty
    :string :anystring :automaton :interval))

(defconstant +intersection+ #x0001) ; enables intersection (&)
(defconstant +complement+   #x0002) ; enables complement (~)
(defconstant +empty+        #x0004) ; enables empty language (#)
(defconstant +anystring+    #x0008) ; enables anystring (@)
(defconstant +automaton+    #x0010) ; enables named automaton (<id>)
(defconstant +interval+     #x0020) ; enables numerical intervals (n-m)
(defconstant +all+          #xffff) ; enables all optional syntax
(defconstant +none+         #x0000) ; enables no optional syntax

(deftype flags-type () `(integer ,+none+ ,+all+))

(defclass regexp ()
  ((kind :initform nil :initarg :kind :reader kind :type kind)
   (exp1 :initform nil :initarg :exp1 :reader exp1 :type (or null regexp))
   (exp2 :initform nil :initarg :exp2 :reader exp2 :type (or null regexp))
   (text :initform nil :initarg :text :reader text :type (or null string))
   (s :initform nil :initarg :s :reader s :type (or null string))
   (c :initform nil :initarg :c :reader c :type (or null character))
   (minr :initform nil :initarg :minr :reader minr :type (or null fixnum))
   (maxr :initform nil :initarg :maxr :reader maxr :type (or null fixnum))
   (digits :initform nil :initarg :digits :reader digits
	   :type (or null fixnum))
   (from :initform nil :initarg :from :reader from :type (or null character))
   (to :initform nil :initarg :to :reader to :type (or null character))
   (flags :initform +all+ :initarg :flags :reader flags :type flags-type)
   (pos :initform 0 :initarg :pos :accessor pos :type integer)))

(defun regexp-equal (r1 r2) ; for testing
  (or (eq r1 r2)
      (and (eq (kind r1) (kind r2))
	   (regexp-equal (exp1 r1) (exp1 r2))
	   (regexp-equal (exp2 r1) (exp2 r2))
	   (equal (s r1) (s r2))
	   (eql (c r1) (c r2))
	   (eql (minr r1) (minr r2))
	   (eql (maxr r1) (maxr r2))
	   (eql (digits r1) (digits r2))
	   (eql (from r1) (from r2))
	   (eql (to r1) (to r2))
	   (eql (flags r1) (flags r2)))))

(defun string-regexp (s &optional fs)
  "Returns a new regexp object corresponding to regular expression
string S. FS is a logior or optional syntax flags."
  (let* ((r (make-instance 'regexp :text s :flags (or fs +all+)))
	 (e (parse-union-exp r)))
    (when (more r)
      (error "end of string expected at position ~A" (pos r)))
    e))

(defun regexp-automaton (r &optional as)
  "Returns a new automaton object corresponding to regexp R. AS is a
hash table mapping from identifiers to auxiliary named automata. (An
error is signaled if R uses an identifier not in AS.) The constructed
automaton is deterministic and minimal, and has no transitions to dead
states."
  (let ((a (ecase (kind r)
	     (:union
	      (aunion (regexp-automaton (exp1 r) as)
		      (regexp-automaton (exp2 r) as)))
	     (:concatenation
	      (aconcatenate (regexp-automaton (exp1 r) as)
			    (regexp-automaton (exp2 r) as)))
	     (:intersection
	      (aintersection (regexp-automaton (exp1 r) as)
			     (regexp-automaton (exp2 r) as)))
	     (:optional
	      (optional (regexp-automaton (exp1 r) as)))
	     (:repeat
	      (repeat (regexp-automaton (exp1 r) as)))
	     (:repeat-min
	      (repeat-min (regexp-automaton (exp1 r) as) (minr r)))
	     (:repeat-minmax
	      (repeat-minmax (regexp-automaton (exp1 r) as)
			     (minr r) (maxr r)))
	     (:complement
	      (acomplement (regexp-automaton (exp1 r) as)))
	     (:char
	      (char-automaton (char-code (c r))))
	     (:char-range
	      (char-range-automaton (char-code (from r)) (char-code (to r))))
	     (:anychar
	      (any-char-automaton))
	     (:empty
	      (empty-automaton))
	     (:string
	      (string-automaton (s r)))
	     (:anystring
	      (any-string-automaton))
	     (:automaton
	      (let ((aa (gethash (s r) as)))
		(if aa
		    (clone aa)
		    (error "~A not found" (s r)))))
	     (:interval
	      (interval-automaton (minr r) (maxr r) (digits r))))))
    (minimize a)))

(defmethod print-object ((r regexp) s)
  (ecase (kind r)
    (:union
     (princ "(" s)
     (print-object (exp1 r) s)
     (princ "\|" s)
     (print-object (exp2 r) s)
     (princ ")" s))
    (:concatenation
     (print-object (exp1 r) s)
     (print-object (exp2 r) s))
    (:intersection
     (princ "(" s)
     (print-object (exp1 r) s)
     (princ "&" s)
     (print-object (exp2 r) s)
     (princ ")" s))
    (:optional
     (princ "(" s)
     (print-object (exp1 r) s)
     (princ ")?" s))
    (:repeat
     (princ "(" s)
     (print-object (exp1 r) s)
     (princ ")*" s))
    (:repeat-min
     (princ "(" s)
     (print-object (exp1 r) s)
     (princ "){" s)
     (princ (minr r) s)
     (princ ",}" s))
    (:repeat-minmax
     (princ "(" s)
     (print-object (exp1 r) s)
     (princ "){" s)
     (princ (minr r) s)
     (princ "," s)
     (princ (maxr r) s)
     (princ "}" s))
    (:complement
     (princ "~(" s)
     (print-object (exp1 r) s)
     (princ ")" s))
    (:char
     (princ "\\" s)
     (princ (c r) s))
    (:char-range
     (princ "[\\" s)
     (princ (from r) s)
     (princ "-\\" s)
     (princ (to r) s)
     (princ "]" s))
    (:anychar
     (princ "." s))
    (:empty
     (princ "#" s))
    (:string
     (princ "\"" s)
     (princ (s r) s)
     (princ "\"" s))
    (:anystring
     (princ "@" s))
    (:automaton
     (princ "<" s)
     (princ (s r) s)
     (princ ">" s))
    (:interval
     (princ "<" s)
     (format s "~V,'0D" (digits r) (minr r))
     (princ "-" s)
     (format s "~V,'0D" (digits r) (maxr r))
     (princ ">" s))))

(defun more (r)
  (< (pos r) (length (text r))))

(defun peek (r s)
  (and (more r) (position (aref (text r) (pos r)) s)))

(defun match (r c)
  (and (more r) (char= (aref (text r) (pos r)) c) (incf (pos r))))

(defun next (r)
  (if (more r)
      (prog1
	  (aref (text r) (pos r))
	(incf (pos r)))
      (error "unexpected end of string")))

(defun check (r flag)
  (not (= (logand (flags r) flag) 0)))

(defun make-regexp (kind &optional e1 e2)
  (make-instance 'regexp :kind kind :exp1 e1 :exp2 e2))

(defun parse-union-exp (r)
  (let ((e (parse-intersection-exp r)))
    (if (match r #\|)
	(make-regexp :union e (parse-union-exp r))
	e)))

(defun parse-intersection-exp (r)
  (let ((e (parse-concatenation-exp r)))
    (if (and (check r +intersection+) (match r #\&))
	(make-regexp :intersection e (parse-intersection-exp r))
	e)))

(defun parse-concatenation-exp (r)
  (let ((e (parse-repeat-exp r)))
    (if (and (more r) (not (peek r ")&\|")))
	(let* ((ee (parse-concatenation-exp r))
	       (ee1 (exp1 ee)))
	  (cond ; optimizations?
	    ((and (member (kind e) '(:string :char))
		  (eq (kind ee) :concatenation)
		  (member (kind ee1) '(:string :char)))
	     (let ((ss (format nil "~A~A"
			       (if (eq (kind e) :string) (s e) (c e))
			       (if (eq (kind ee1) :string) (s ee1) (c ee1)))))
	       (if (position #\" ss)
		   (make-regexp :concatenation
				(make-instance 'regexp :kind :string :s ss)
				(exp2 ee))
		   (make-regexp :concatenation e ee))))
	    ((and (member (kind e) '(:string :char))
		  (member (kind ee) '(:string :char)))
	     (let ((ss (format nil "~A~A"
			       (if (eq (kind e) :string) (s e) (c e))
			       (if (eq (kind ee) :string) (s ee) (c ee)))))
	       (if (position #\" ss)
		   (make-regexp :concatenation e ee)
		   (make-instance 'regexp :kind :string :s ss))))
	    (t (make-regexp :concatenation e ee))))
	e)))

(defun parse-repeat-exp (r)
  (let ((e (parse-complement-exp r)))
    (loop while (peek r "?*+{") do
	 (cond
	   ((match r #\?)
	    (setq e (make-regexp :optional e)))
	   ((match r #\*)
	    (setq e (make-regexp :repeat e)))
	   ((match r #\+)
	    (setq e (make-instance 'regexp :kind :repeat-min :exp1 e :minr 1)))
	   ((match r #\{)
	    (let ((start (pos r)))
	      (loop while (peek r "0123456789") do (next r))
	      (when (= start (pos r))
		(error "integer expected at position ~A" (pos r)))
	      (let ((n (parse-integer (text r) :start start :end (pos r)))
		    (m nil))
		(if (match r #\,)
		    (let ((start (pos r)))
		      (loop while (peek r "0123456789") do (next r))
		      (when (/= start (pos r))
			(setq m (parse-integer
				 (text r) :start start :end (pos r)))))
		    (setq m n))
		(unless (match r #\})
		  (error "expected '}' at positiion ~A" (pos r)))
		(return-from parse-repeat-exp
		  (if m
		      (make-instance
		       'regexp :kind :repeat-minmax :exp1 e :minr n :maxr m)
		      (make-instance
		       'regexp :kind :repeat-min :exp1 e :minr n)))))))
       finally (return e))))

(defun parse-complement-exp (r)
  (if (and (check r +complement+) (match r #\~))
      (make-regexp :complement (parse-complement-exp r))
      (parse-char-class-exp r)))

(defun parse-char-class-exp (r)
  (if (match r #\[)
      (let ((negate (match r #\^))
	    (e (parse-char-classes r)))
	(unless (match r #\])
	  (error "expected ']' at position ~A" (pos r)))
	(if negate
	    (make-regexp :intersection
			 (make-regexp :anychar)
			 (make-regexp :complement e))
	    e))
      (parse-simple-exp r)))

(defun parse-char-classes (r)
  (let ((e (parse-char-class r)))
    (loop while (and (more r) (not (peek r "]"))) do
	 (setq e (make-regexp :union e (parse-char-class r)))
       finally (return e))))

(defun parse-char-class (r)
  (let ((c (parse-char-exp r)))
    (if (match r #\-)
	(make-instance
	 'regexp :kind :char-range :from c :to (parse-char-exp r))
	(make-instance 'regexp :kind :char :c c))))

(defun parse-simple-exp (r)
  (cond
    ((match r #\.)
     (make-regexp :anychar))
    ((and (check r +empty+) (match r #\#))
     (make-regexp :empty))
    ((and (check r +anystring+) (match r #\@))
     (make-regexp :anystring))
    ((match r #\")
     (let ((start (pos r)))
       (loop while (and (more r) (not (peek r "\""))) do (next r))
       (unless (match r #\")
	 (error "expected '\"' at position ~A" (pos r)))
       (make-instance 'regexp :kind :string
		      :s (subseq (text r) start (1- (pos r))))))
    ((match r #\()
     (if (match r #\))
	 (make-instance 'regexp :kind :string :s "")
	 (let ((e (parse-union-exp r)))
	   (unless (match r #\))
	     (error "expected ')' at position ~A" (pos r)))
	   e)))
    ((and (or (check r +automaton+) (check r +interval+)) (match r #\<))
     (let ((start (pos r)))
       (loop while (and (more r) (not (peek r ">"))) do (next r))
       (unless (match r #\>)
	 (error "expected '>' at position ~A" (pos r)))
       (let* ((s (subseq (text r) start (1- (pos r))))
	      (i (position #\- s)))
	 (if i
	     (progn
	       (unless (check r +interval+)
		 (error "illegal identifier at position ~A" (1- (pos r))))
	       (handler-bind
		   ((error #'(lambda (c)
			       (error
				"interval syntax error at position ~A (~A)"
				(1- (pos r)) c))))
		 (when (or (= i 0) (= i (length s))
			   (/= i (position #\- s :from-end t)))
		   (error "number format exception"))
		 (let* ((smin (subseq s 0 i))
			(smax (subseq s (1+ i)))
			(imin (parse-integer smin))
			(imax (parse-integer smax))
			(digs (if (= (length smin) (length smax))
				  (length smin)
				  0)))
		   (when (> imin imax)
		     (rotatef imin imax))
		   (make-instance 'regexp :kind :interval
				  :minr imin :maxr imax :digits digs))))
	     (if (check r +automaton+)
		 (make-instance 'regexp :kind :automaton :s s)
		 (error "interval syntax error at position ~A"
			(1- (pos r))))))))
    (t (make-instance 'regexp :kind :char :c (parse-char-exp r)))))

(defun parse-char-exp (r)
  (match r #\\)
  (next r))