;;; -*- Mode: Lisp; Package: AUTOMATON -*-
;;;
;;;  (c) copyright 2005-2007 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
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

(in-package :automaton)

(defconstant +min-char-code+ 0)
(defconstant +max-char-code+ (1- char-code-limit))
;;; In Allegro (for one), defconstants aren't available as values at compile
;;; time.
(deftype char-code-type () `(integer 0 ,(1- char-code-limit)))

(defclass state ()
  ((accept :initform nil :accessor accept :type boolean)
   (transitions :accessor transitions :type generalized-hash-table)
   (num :initform 0 :accessor num :type fixnum)
   (id :accessor id :type fixnum)
   (next-id :allocation :class :initform -1 :accessor next-id :type fixnum)))

(declaim (special *state-ht*))
(defun state-equal (s1 s2) ; for testing, assuming minimization
  (multiple-value-bind (se se-p)
      (gethash (cons s1 s2) *state-ht*) ; TODO: consider (cons s2 s1), too
    (if se-p
	se
	(setf (gethash (cons s1 s2) *state-ht*) t ; bound recursion temporarily
	      (gethash (cons s1 s2) *state-ht*)
	      (and (eq (accept s1) (accept s2))
		   (transitions-equal (transitions s1) (transitions s2)))))))

(declaim (special *to-first*))
(defun transitions-equal (ts1 ts2) ; for testing, assuming minimization
  (let* ((*to-first* nil)
	 (tss1 (sort (with-ht-collect (t1 nil) ts1 t1) #'transition<))
	 (tss2 (sort (with-ht-collect (t2 nil) ts2 t2) #'transition<)))
    (flet ((%transition-equal (t1 t2)
	     (with-slots ((minc1 minc) (maxc1 maxc) (to1 to)) t1
	       (with-slots ((minc2 minc) (maxc2 maxc) (to2 to)) t2
		 (and
		  (= minc1 minc2) (= maxc1 maxc2) (state-equal to1 to2))))))
      (and (= (length tss1) (length tss2))
	   (loop for t1 in tss1 and t2 in tss2
	      always (%transition-equal t1 t2))))))

(defclass state-pair ()
  ((s :initarg :s :accessor s :type (or null state))
   (s1 :initarg :s1 :accessor s1 :type state)
   (s2 :initarg :s2 :accessor s2 :type state)))

(defclass transition ()
  ((minc :initarg :minc :accessor minc :type char-code-type)
   (maxc :initarg :maxc :accessor maxc :type char-code-type)
   (to :initarg :to :accessor to :type state)))

(defclass state-set ()
  ((ht :initform (make-hash-table) :initarg :ht :accessor ht :type hash-table)))

(defmethod initialize-instance :after ((s state) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (transitions id next-id) s
    (setf transitions (make-generalized-hash-table +equalp-key-situation+)
	  id (incf next-id))))

(defmethod initialize-instance :after ((tr transition) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (minc maxc to) tr
    (cond
      ((not minc)
       (assert maxc nil "MINC or MAXC required")
       (setf minc maxc))
      ((not maxc)
       (assert minc nil "MINC or MAXC required")
       (setf maxc minc))
      ((> minc maxc)
       (rotatef minc maxc)))
    (assert to nil "TO required")))

(defmethod eqv ((sp1 state-pair) (sp2 state-pair)
		(s (eql +equalp-key-situation+)))
  (and (eq (s1 sp1) (s1 sp2)) (eq (s2 sp1) (s2 sp2))))

(defmethod hash ((sp state-pair) (s (eql +equalp-key-situation+)))
  "Returns the hash code for state-pair SP."
  (the fixnum
    (mod (+ (sxhash (s1 sp)) (sxhash (s2 sp))) most-positive-fixnum)))

(defmethod eqv ((tr1 transition) (tr2 transition)
		(s (eql +equalp-key-situation+)))
  "Returns true if transitions TR1 and TR2 have equal interval and
same (eq) destination state."
  (with-slots ((minc1 minc) (maxc1 maxc) (to1 to)) tr1
    (with-slots ((minc2 minc) (maxc2 maxc) (to2 to)) tr2
      (and (= minc1 minc2) (= maxc1 maxc2) (eq to1 to2)))))

(defmethod hash ((tr transition) (s (eql +equalp-key-situation+)))
  "Returns the hash code for transition TR."
  (with-slots (minc maxc) tr
    (the fixnum (mod (+ (* 2 minc) (* 3 maxc)) most-positive-fixnum))))

(defgeneric clone (transition))

(defmethod clone ((tr transition))
  "Returns a clone of TR."
  (with-slots (minc maxc to) tr
    (make-instance 'transition :minc minc :maxc maxc :to to)))

(defmethod eqv ((ss1 state-set) (ss2 state-set)
		(s (eql +equalp-key-situation+)))
  "Returns true if state-set objects SS1 and SS2 contain the same (eql)
state objects."
  (and (= (hash-table-count (ht ss1)) (hash-table-count (ht ss2)))
       (loop for st being the hash-keys of (ht ss1)
	  always (gethash st (ht ss2)))))

(defmethod hash ((ss state-set) (s (eql +equalp-key-situation+)))
  "Returns the hash code for state-set SS."
  (the fixnum
    (mod (loop for st being the hash-keys of (ht ss)
	    sum (sxhash st))
	 most-positive-fixnum)))

(defvar *escape-unicode-chars* nil) ; true may be useful in Slime

(defun escaped-char (c)
  (if (or (not *escape-unicode-chars*)
	  (and (<= #x21 c #x7e) (/= c (char-code #\\))))
      (code-char c)
      (format nil "\\u~4,'0O" c)))

(defmethod print-object ((st state) s)
  (with-slots (accept transitions num) st
    (format s "~@<state ~A [~A]: ~2I~_~@<~{~W~^ ~_~}~:>~:>"
	    num
	    (if accept "accept" "reject")
	    (with-ht-collect (tr nil) transitions tr)))
  st)

(defmethod print-object ((tr transition) s)
  (with-slots (minc maxc to) tr
    (format s "~@<~A~:[~*~;-~A~] -> ~A~:>"
	    (escaped-char minc)
	    (/= minc maxc)
	    (escaped-char maxc)
	    (num to))
    tr))

(defun transition< (tr1 tr2)
  "Returns true if TR1 is strictly less than TR2. If *TO-FIRST*
special variable is bound to true, the values of the destination
states' NUM slots are compared first, followed by the intervals
comparison. The intervals comparison is done as follows: the lower
interval bounds are compared first, followed by reversed upper
interval bounds comparisons. If *TO-FIRST* is bound to nil, the
interval comparison is done first, followed by the NUM comparisons."
  (with-slots ((minc1 minc) (maxc1 maxc) (to1 to)) tr1
    (with-slots ((minc2 minc) (maxc2 maxc) (to2 to)) tr2
      (let ((to< (< (num to1) (num to2)))
	    (to= (= (num to1) (num to2)))
	    (min-rmax< (or (< minc1 minc2)
			   (and (= minc1 minc2) (> maxc1 maxc2))))
	    (min-rmax= (and (= minc1 minc2) (= maxc1 maxc2))))
	(if *to-first*
	    (or to< (and to= min-rmax<))
	    (or min-rmax< (and min-rmax= to<)))))))

(defun reset-transitions (s)
  (setf (transitions s) (make-generalized-hash-table +equalp-key-situation+)))

(defun sstep (s c)
  "Returns a state reachable from S, given the input character code
C."
  (with-ht (tr nil) (transitions s)
    (when (<= (minc tr) (char-code c) (maxc tr))
      (return-from sstep (to tr)))))

(defun add-epsilon (s to)
  "Adds transitions of state TO to state S. Also, if TO accepts, so
does S."
  (when (accept to)
    (setf (accept s) t))
  (let ((s-table (transitions s)))
    (with-ht (tr nil) (transitions to)
      (htadd s-table tr))))

(defun sorted-transition-vector (s *to-first*)
  "Returns a vector of all transitions of S, sorted using TRANSITION<
and *TO-FIRST*."
  (let ((v (make-array `(,(cnt (transitions s)))
		       :element-type '(or null transition)
                       :initial-element nil))
	(i -1))
    (sort
     (progn
       (with-ht (tr nil) (transitions s)
	 (setf (aref v (incf i)) tr))
       v)
     #'transition<)))

(defun sorted-transition-list (s *to-first*)
  "Returns a list of all transitions of S, sorted using TRANSITION<
and *TO-FIRST*."
  (sort
   (with-ht-collect (tr nil) (transitions s) tr)
   #'transition<))
