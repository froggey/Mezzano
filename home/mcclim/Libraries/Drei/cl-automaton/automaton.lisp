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
;;;
;;; Derived from dk.brics.automaton v1.8.1, (c) 2001-2005 by Anders M/oller
;;; - Functionality not used by the regular expression engine and not tested
;;;   has been omitted from this initial release.
;;; - Some comments have been copied verbatim from the original code.

(in-package :automaton)

(deftype minimization () '(member huffman brzozowski hopcroft))
(defvar *minimization* 'hopcroft)
(defvar *minimize-always* t)

;;; Class invariants:
;;; - An automaton is either represented explicitly (with state and
;;;   transition objects) or with a singleton string in case the
;;;   automaton accepts exactly one string.
;;; - Automata are always reduced (see areduce) and have no transitions
;;;   to dead states (see remove-dead-transitions).
;;; - If an automaton is nondeterministic, then deterministic returns nil
;;;   (but the converse is not required).
;;; Implicitly, all states and transitions of an automaton are reachable
;;; from its initial state.
;;; If the states or transitions are manipulated manually, the
;;; restore-invariant and (setf deterministic) methods should be used
;;; afterwards to restore certain representation invariants that are
;;; assumed by the built-in automata operations.
;;; If minimize-always is true, minimize will automatically be invoked
;;; after every operation that otherwise may produce a non-minimal automaton
;;; (usually, an intermediate result).
(defclass automaton ()
  ((minimization :initform *minimization* :accessor minimization
		 :type minimization)
   (initial :initform (make-instance 'state) :accessor initial :type state)
   (deterministic :initform t :accessor deterministic :type boolean)
   (info :initform nil :accessor info)
   (hash-code :initform 0 :accessor hash-code :type fixnum)
   (singleton :initform nil :accessor singleton :type (or null string))
   (minimize-always :initform *minimize-always* :accessor minimize-always
		    :type boolean)))

(defun restore-invariant (a)
  (remove-dead-transitions a)
  (setf (hash-code a) 0))

(declaim (special *state-ht*))
(defun automaton-equal (a1 a2) ; for testing, assumes minimization
  (and (eq (minimization a1) (minimization a2))
       (let ((*state-ht* (make-hash-table :test #'equal)))
	 (state-equal (initial a1) (initial a2)))
       (eq (deterministic a1) (deterministic a2))
       (eqv a1 a2 +equalp-key-situation+)
       (eq (minimize-always a1) (minimize-always a2))))

(defclass int-pair () ; TODO: replace with a simple cons
  ((n1 :initarg :n1 :reader n1 :type fixnum)
   (n2 :initarg :n2 :reader n2 :type fixnum)))

(defclass state-list-node ()
  ((q :initform nil :initarg :q :accessor q :type (or null state))
   (succ :initform nil :accessor succ :type (or null state-list-node))
   (pred :initform nil :accessor pred :type (or null state-list-node))
   (sl :initform nil :initarg :sl :accessor sl :type (or null state-list))))

(defclass state-list ()
  ((size :initform 0 :accessor size :type fixnum)
   (fst :initform nil :accessor fst :type (or null state-list-node))
   (lst :initform nil :accessor lst :type (or null state-list-node))))

(defun check-minimize-always (a)
  (if (minimize-always a)
      (minimize a)
      a))

(defun states (a)
  "Returns a hash table containing the set of states reachable from
the initial state of A."
  (expand-singleton a)
  (let ((visited (make-hash-table))
	(worklist nil))
    (setf (gethash (initial a) visited) t)
    (push (initial a) worklist)
    (loop for s = (first worklist)
       while worklist do
       (pop worklist)
       (with-ht (tr nil) (transitions s)
         (let ((s2 (to tr)))
           (unless (gethash s2 visited)
             (setf (gethash s2 visited) t)
             (push s2 worklist)))))
    visited))

(defun accepting-states (a)
  "Returns a hash table containing the set of accepting states
reachable from the initial state of A."
  (let ((accepting (make-hash-table)))
    (loop for s being the hash-keys of (states a)
       when (accept s) do
	 (setf (gethash s accepting) t))
    accepting))

(defun set-state-nums (states)
  "Renumerates, by assigning consecutive numbers to the NUM slot of
states being the keys of STATES hash table, and finally returns
STATES."
  (let ((i -1))
    (loop for s being the hash-keys of states do
	 (setf (num s) (incf i))))
  states)

(defun totalize (a)
  "Adds transitions to an explicit crash state, added to A, to ensure
that the transition function is total. Finally, returns A."
  (let* ((s (make-instance 'state))
	 (tr (make-instance
	      'transition :minc +min-char-code+ :maxc +max-char-code+ :to s)))
    (htadd (transitions s) tr)
    (loop for p being the hash-keys of (states a)
       and maxi = +min-char-code+ do
	 (loop for tr in (sorted-transition-list p nil) do
	      (with-slots (minc maxc) tr
		(when (> minc maxi)
		  (htadd (transitions p)
			 (make-instance
			  'transition :minc maxi :maxc (1- minc) :to s)))
		(when (> (1+ maxc) maxi)
		  (setq maxi (1+ maxc)))))
	 (when (<= maxi +max-char-code+)
	   (htadd (transitions p)
		  (make-instance
		   'transition :minc maxi :maxc +max-char-code+ :to s))))
    a))

(defun areduce (a)
  "Reduces automaton A by combining overlapping and adjacent edge
intervals with the same destination. Finally, returns A."
  (if (singleton a)
      a
      (let ((states (states a)))
	(set-state-nums states)
	(loop for s being the hash-keys of states do
	     (let ((st (sorted-transition-list s t)))
	       (reset-transitions s)
	       (let ((p nil)
		     (min -1)
		     (max -1))
		 (loop for tr in st
		    if (eq p (to tr)) do
		      (with-slots (minc maxc) tr
			(if (<= minc (1+ max))
			    (when (> maxc max)
			      (setq max maxc))
			    (progn
			      (when p
				(htadd
				 (transitions s)
				 (make-instance
				  'transition :minc min :maxc max :to p)))
			      (setq min minc
				    max maxc))))
		    else do
		      (with-slots (minc maxc to) tr
			(when p
			  (htadd (transitions s)
				 (make-instance
				  'transition :minc min :maxc max :to p)))
			(setq p to
			      min minc
			      max maxc)))
		 (when p
		   (htadd (transitions s)
			  (make-instance
			   'transition :minc min :maxc max :to p))))))
	a)))

(defun start-points (a)
  "Returns a sorted vector of all interval start points (character
codes)."
  (let ((pset (make-hash-table)))
    (loop for s being the hash-keys of (states a) do
	 (setf (gethash +min-char-code+ pset) t)
	 (with-ht (tr nil) (transitions s)
	   (with-slots (minc maxc) tr
	     (setf (gethash minc pset) t)
	     (when (< maxc +max-char-code+)
	       (setf (gethash (1+ maxc) pset) t)))))
    (let ((pa (make-array (hash-table-count pset)
			  :element-type 'char-code-type)))
      (loop for p being the hash-keys of pset and n from 0 do
	   (setf (aref pa n) p)
	 finally (return (sort pa #'<))))))

(defun live-states2 (a states)
  "Returns the set of live states of A that are in STATES hash
table. A state is live if an accepting state is reachable from it."
  (let ((map (make-hash-table)))
    (loop for s being the hash-keys of states do
	 (setf (gethash s map) (make-hash-table)))
    (loop for s being the hash-keys of states do
	 (with-ht (tr nil) (transitions s)
	   (setf (gethash s (gethash (to tr) map)) t)))
    (let* ((live (accepting-states a))
	   (worklist (loop for s being the hash-keys of live collect s)))
      (loop for s = (first worklist)
         while worklist do
         (pop worklist)
         (loop for p being the hash-keys of (gethash s map)
            unless (gethash p live) do
            (setf (gethash p live) t)
            (push p worklist)))
      live)))

(defun remove-dead-transitions (a)
  "Returns reduced A with transitions to dead states removed. A state
is dead if no accepting state is reachable from it."
  (if (singleton a)
      nil
      (let* ((states (states a))
	     (live (live-states2 a states)))
	(loop for s being the hash-keys of states do
	     (let ((st (transitions s)))
	       (reset-transitions s)
	       (with-ht (tr nil) st
		 (when (gethash (to tr) live)
		   (htadd (transitions s) tr)))))
	(areduce a))))

(defun sorted-transitions (states)
  "Renumerates each state in STATES hash table, and returns a vector
of sorted vectors of transitions for each state, ordered by the NUM
slot."
  (set-state-nums states)
  (let ((transitions (make-array (hash-table-count states))))
    (loop for s being the hash-keys of states do
	 (setf (aref transitions (num s)) (sorted-transition-vector s nil)))
    transitions))

(defun empty-automaton ()
  "Returns a new determinsitic automaton with the empty language."
  (let ((a (make-instance 'automaton))
	(s (make-instance 'state)))
    (setf (initial a) s
	  (deterministic a) t)
    a))

(defun empty-string-automaton ()
  "Returns a new deterministic automaton that accepts only the empty
string."
  (let ((a (make-instance 'automaton)))
    (setf (singleton a) ""
	  (deterministic a) t)
    a))

(defun any-string-automaton ()
  "Returns a new deterministic automaton that accepts any string."
  (let ((a (make-instance 'automaton))
	(s (make-instance 'state)))
    (setf (initial a) s
	  (accept s) t
	  (deterministic a) t)
    (htadd (transitions s)
	   (make-instance
	    'transition :minc +min-char-code+ :maxc +max-char-code+ :to s))
    a))

(defun any-char-automaton ()
  "Returns a new deterministic automaton that accepts any single
character."
  (char-range-automaton +min-char-code+ +max-char-code+))

(defun char-automaton (c)
  "Returns a new deterministic automaton that accepts a single
character whose code is C."
  (char-range-automaton c c))

(defun char-range-automaton (cmin cmax)
  "Returns a new deterministic automaton that accepts a single
character whose code is in closed interval [CMIN, CMAX]."
  (let ((a (make-instance 'automaton))
	(s1 (make-instance 'state))
	(s2 (make-instance 'state)))
    (setf (initial a) s1
	  (accept s2) t
	  (deterministic a) t)
    (when (<= cmin cmax)
      (htadd (transitions s1)
	     (make-instance 'transition :minc cmin :maxc cmax :to s2)))
    a))

(defun char-set-automaton (str)
  "Returns a new deterministic automaton that accepts a single
character in set STR."
  (let ((a (make-instance 'automaton))
	(s1 (make-instance 'state))
	(s2 (make-instance 'state)))
    (setf (initial a) s1
	  (accept s2) t
	  (deterministic a) t)
    (loop with t-table = (transitions s1)
       for c across str do
	 (htadd t-table (make-instance
			 'transition :minc (char-code c) :maxc (char-code c)
			 :to s2)))
    (areduce a)))

(defun any-of-right-length-subautomaton (str n)
  "Returns a new sub-automaton (root of a state graph) accepting
non-negative (decimal) integers of length of (subseq STR N)."
  (let ((s (make-instance 'state)))
    (if (= (length str) n)
	(setf (accept s) t)
	(htadd (transitions s)
	       (make-instance
		'transition :minc (char-code #\0) :maxc (char-code #\9)
		:to (any-of-right-length-subautomaton str (1+ n)))))
    s))

(defun at-least-subautomaton (str n initials zeros)
  "Returns a new sub-automaton (root of a state graph) accepting
non-negative (decimal) integers of value at least the one represented
by (subseq STR N), and of length of (subseq STR N)."
  (let ((s (make-instance 'state)))
    (if (= (length str) n)
	(setf (accept s) t)
	(let ((c (elt str n)))
	  (when zeros
	    (push s (car initials)))
	  (htadd (transitions s)
		 (make-instance
		  'transition :minc (char-code c) :maxc (char-code c)
		  :to (at-least-subautomaton
		       str (1+ n) initials (and zeros (char= c #\0)))))
	  (when (char< c #\9)
	    (htadd
	     (transitions s)
	     (make-instance
	      'transition :minc (1+ (char-code c)) :maxc (char-code #\9)
	      :to (any-of-right-length-subautomaton str (1+ n)))))))
    s))

(defun at-most-subautomaton (str n)
  "Returns a new sub-automaton (root of a state graph) accepting
non-negative (decimal) integers of value at most the one represented
by (subseq STR N), and of length of (subseq STR N)."
  (let ((s (make-instance 'state)))
    (if (= (length str) n)
	(setf (accept s) t)
	(let ((c (elt str n)))
	  (htadd (transitions s)
		 (make-instance
		  'transition :minc (char-code c) :maxc (char-code c)
		  :to (at-most-subautomaton str (1+ n))))
	  (when (char> c #\0)
	    (htadd (transitions s)
		   (make-instance
		    'transition :minc (char-code #\0) :maxc (1- (char-code c))
		    :to (any-of-right-length-subautomaton str (1+ n)))))))
    s))

(defun between-subautomaton (str1 str2 n initials zeros)
  "Returns a new sub-automaton (root of a state graph) accepting
non-negative (decimal) integers of value between the one represented
by (subseq STR1 N) and (subseq STR2 N), inclusive, and of length of
\(subseq STR1 N) = (subseq STR2 N)."
  (let ((s (make-instance 'state)))
    (if (= (length str1) n)
	(setf (accept s) t)
	(let ((c1 (elt str1 n))
	      (c2 (elt str2 n)))
	  (when zeros
	    (push s (car initials)))
	  (if (char= c1 c2)
	      (htadd (transitions s)
		     (make-instance
		      'transition :minc (char-code c1) :maxc (char-code c1)
		      :to (between-subautomaton str1 str2 (1+ n) initials
						(and zeros (char= c1 #\0)))))
	      (progn
		(htadd
		 (transitions s)
		 (make-instance
		  'transition :minc (char-code c1) :maxc (char-code c1)
		  :to (at-least-subautomaton str1 (1+ n) initials
					     (and zeros (char= c1 #\0)))))
		(htadd
		 (transitions s)
		 (make-instance
		  'transition :minc (char-code c2) :maxc (char-code c2)
		  :to (at-most-subautomaton str2 (1+ n))))
		(when (< (1+ (char-code c1)) (char-code c2))
		  (htadd
		   (transitions s)
		   (make-instance
		    'transition
		    :minc (1+ (char-code c1)) :maxc (1- (char-code c2))
		    :to (any-of-right-length-subautomaton str1 (1+ n)))))))))
    s))

(defun interval-automaton (min max digits)
  "Returns a new automaton that accepts strings representing
non-negative (decimal) integers in interval [MIN, MAX]. If DIGITS > 0,
uses the fixed number of digits (strings must be prefixed by 0s to
obtain the right length). Otherwise, the number of digits is not
fixed. If MIN > MAX or if the numbers in the interval cannot be
expressed with the given fixed number of digits, an error is
signaled."
  (flet ((%num-digits (n) (if (= n 0) 1 (1+ (floor (log n 10))))))
    (assert (and (<= 0 min max)
		 (or (<= digits 0) (<= (%num-digits max) digits)))
	    () "MIN and MAX not expressible with the same number of digits")
    (let* ((a (make-instance 'automaton))
	   (d (if (> digits 0) digits (%num-digits max)))
	   (str1 (format nil "~V,'0D" d min))
	   (str2 (format nil "~V,'0D" d max))
	   (initials (cons nil nil)))
      (setf (initial a)
	    (between-subautomaton str1 str2 0 initials (<= digits 0)))
      (if (<= digits 0)
	  (let ((pairs nil))
	    (loop for p in (car initials)
	       unless (eq (initial a) p) do
		 (push (make-instance 'state-pair :s1 (initial a) :s2 p)
		       pairs))
	    (add-epsilons a pairs)
	    (htadd (transitions (initial a))
		   (make-instance
		    'transition :minc (char-code #\0) :maxc (char-code #\0)
		    :to (initial a)))
	    (setf (deterministic a) nil))
	  (setf (deterministic a) t))
      (check-minimize-always a))))

(defun expand-singleton (a)
  "Expands the singleton representation of A into the regular
representation, and returns A."
  (with-slots ((st singleton)) a
    (when st
      (let ((p (make-instance 'state)))
	(setf (initial a) p)
	(loop for c across st do
	     (let ((q (make-instance 'state)))
	       (htadd (transitions p)
		      (make-instance
		       'transition :minc (char-code c) :maxc (char-code c)
		       :to q))
	       (setq p q)))
	(setf (accept p) t
	      (deterministic a) t
	      st nil))))
  a)

(defun string-automaton (str)
  "Returns a new deterministic automaton that accepts the single given
string STR."
  (let ((a (make-instance 'automaton)))
    (setf (singleton a) str
	  (deterministic a) t)
    a))

(defun aconcatenate (a1 a2)
  "Returns a new automaton that accepts the concatenation of the
languages of A1 and A2. Complexity: linear in the number of states."
  (if (and (singleton a1) (singleton a2))
      (string-automaton (concatenate 'string (singleton a1) (singleton a2)))
      (progn
	(setf a1 (clone-expanded a1)
	      a2 (clone-expanded a2))
	(loop for s being the hash-keys of (accepting-states a1) do
	     (setf (accept s) nil)
	     (add-epsilon s (initial a2)))
	(setf (deterministic a1) nil)
	(check-minimize-always a1))))

(defun aconcatenate-many (l)
  "Returns a new automaton that accepts the concatenation of the
languages of automata in list L, respecting the order. Complexity:
linear in the total number of states."
  (if l
      (let* ((a1 (clone-expanded (car l)))
	     (ac1 (accepting-states a1)))
	(loop for a2 in (cdr l) do
	     (let* ((a2 (clone-expanded a2))
		    (ac2 (accepting-states a2)))
	       (loop for s being the hash-keys of ac1 do
		    (setf (accept s) nil)
		    (add-epsilon s (initial a2))
		    (when (accept s)
		      (setf (gethash s ac2) t)))
	       (setq ac1 ac2)))
	(setf (deterministic a1) nil)
	(check-minimize-always a1))
      (empty-string-automaton)))

(defun optional (a)
  "Returns a new automaton that accepts the union of the empty string
and the language of A. Complexity: linear in the number of states."
  (let ((a (clone-expanded a))
	(s (make-instance 'state)))
    (add-epsilon s (initial a))
    (setf (accept s) t
	  (initial a) s
	  (deterministic a) nil)
    (check-minimize-always a)))

(defun repeat (a)
  "Returns a new automaton that accepts the Kleene star (zero or more
concatenated repetitions) of the language of A. Complexity: linear in
the number of states."
  (let ((a (clone-expanded a))
	(s (make-instance 'state)))
    (setf (accept s) t)
    (add-epsilon s (initial a))
    (loop for p being the hash-keys of (accepting-states a) do
	 (add-epsilon p s))
    (setf (initial a) s
	  (deterministic a) nil)
    (check-minimize-always a)))

(defun repeat-min (a min)
  "Returns a new automaton that accepts MIN or more concatenated
repetitions of the language of A."
  (let ((a2 (repeat a)))
    (loop while (> min 0) do
	 (setq a2 (aconcatenate a a2)
	       min (1- min)))
    a2))

(defun repeat-minmax (a min max)
  "Returns a new automaton that accepts a number, from [MIN, MAX], of
concatenated repetitions of the language of A. If MIN > MAX, the empty
automaton is returned."
  (expand-singleton a)
  (when (> min max)
    (return-from repeat-minmax (empty-automaton)))
  (decf max min)
  (let ((a2 (cond
	      ((= min 0) (empty-string-automaton))
	      ((= min 1) (clone a))
	      (t (loop with tmp = a
		    while (> (decf min) 0) do
		    (setq tmp (aconcatenate a tmp))
		    finally (return tmp))))))
    (when (= max 0)
      (return-from repeat-minmax a2))
    (let ((a3 (clone a)))
      (loop while (> (decf max) 0) do
	   (let ((a4 (clone a)))
	     (loop for p being the hash-keys of (accepting-states a4) do
		  (add-epsilon p (initial a3)))
	     (setq a3 a4)))
      (loop for p being the hash-keys of (accepting-states a2) do
	   (add-epsilon p (initial a3)))
      (setf (deterministic a2) nil)
      (check-minimize-always a2))))

(defun acomplement (a)
  "Returns a new deterministic"
  (let ((a (clone-expanded a)))
    (determinize a)
    (totalize a)
    (loop for p being the hash-keys of (states a) do
	 (setf (accept p) (not (accept p))))
    (remove-dead-transitions a)
    (check-minimize-always a)))

(defun aintersection (a1 a2)
  "Returns a new deterministic automaton that accepts the intersection
of the languages of A and A2. As a side-effect, both A1 and A2 are
determinized if not already deterministic. Complexity: quadratic in
the number of states (when deterministic)."
  (if (and (singleton a1) (singleton a2))
      (if (string= (singleton a1) (singleton a2))
	  (string-automaton (singleton a1))
	  (empty-automaton))
      (progn
	(determinize a1)
	(determinize a2)
	(let* ((trs1 (sorted-transitions (states a1)))
	       (trs2 (sorted-transitions (states a2)))
	       (a3 (make-instance 'automaton))
	       (worklist nil)
	       (newstates (make-generalized-hash-table +equalp-key-situation+))
	       (s (make-instance 'state))
	       (p (make-instance
		   'state-pair :s s :s1 (initial a1) :s2 (initial a2))))
	  (setf (initial a3) s)
	  (push p worklist)
	  (setf (htref newstates p) p)
	  (loop while worklist do
	       (setq p (pop worklist))
	       (setf (accept (s p)) (and (accept (s1 p)) (accept (s2 p))))
	       (let* ((t1 (aref trs1 (num (s1 p))))
		      (t2 (aref trs2 (num (s2 p))))
		      (t1l (length t1))
		      (t2l (length t2)))
		 (loop with n1 = 0 and n2 = 0
		    while (and (< n1 t1l) (< n2 t2l)) do
		      (cond
			((< (maxc (aref t1 n1)) (minc (aref t2 n2)))
			 (incf n1))
			((< (maxc (aref t2 n2)) (minc (aref t1 n1)))
			 (incf n2))
			(t (let* ((q (make-instance 'state-pair
						    :s1 (to (aref t1 n1))
						    :s2 (to (aref t2 n2))))
				  (r (htref newstates q))
				  (min (max (minc (aref t1 n1))
					    (minc (aref t2 n2))))
				  (max (min (maxc (aref t1 n1))
					    (maxc (aref t2 n2)))))
			     (unless r
			       (setf (s q) (make-instance 'state))
			       (push q worklist)
			       (setf (htref newstates q) q)
			       (setq r q))
			     (htadd (transitions (s p))
				    (make-instance
				     'transition
				     :minc min :maxc max :to (s r)))
			     (if (< (maxc (aref t1 n1)) (maxc (aref t2 n2)))
				 (incf n1)
				 (incf n2))))))))
	  (setf (deterministic a3) t)
	  (remove-dead-transitions a3)
	  (check-minimize-always a3)))))

(defun aunion (a1 a2)
  "Returns a new automaton that accepts the union of the languages of
A1 and A2. Complexity: linear in the number of states."
  (when (and (singleton a1) (singleton a2)
	     (string= (singleton a1) (singleton a2)))
    (return-from aunion (clone a1)))
  (let ((a2 (clone-expanded a2))
	(a3 (clone-expanded a1))
	(s (make-instance 'state)))
    (add-epsilon s (initial a2))
    (add-epsilon s (initial a3))
    (setf (initial a2) s
	  (deterministic a2) nil)
    (check-minimize-always a2)))

(defun aunion-many (l)
  "Returns a new automaton that accepts the union of the languages of
automata given in list L."
  (let ((s (make-instance 'state))
	(a (make-instance 'automaton)))
    (loop for b in l do
	 (add-epsilon s (initial (clone-expanded b))))
    (setf (initial a) s
	  (deterministic a) nil)
    (check-minimize-always a)))

(defun determinize (a)
  "Determinizes A and returns it."
  (if (or (deterministic a) (singleton a))
      a
      (let ((initialset (make-instance 'state-set)))
	(setf (gethash (initial a) (ht initialset)) t)
	(determinize2 a initialset))))

(defun determinize2 (a initialset)
  "Determinizes A using the set of initial states in INITIALSET
state-set."
  (let ((points (start-points a))
	(sets (make-generalized-hash-table +equalp-key-situation+))
	(worklist nil)
	(newstate (make-generalized-hash-table +equalp-key-situation+)))
    (setf (htref sets initialset) initialset)
    (push initialset worklist)
    (setf (initial a) (make-instance 'state))
    (setf (htref newstate initialset) (initial a))
    (loop while worklist do
	 (let* ((s (pop worklist))
		(r (htref newstate s)))
	   (loop for q being the hash-keys of (ht s)
	      when (accept q) do
		(setf (accept r) t)
		(return))
	   (loop with len = (length points)
	      for c across points
	      and n from 0 do
		(let ((p (make-instance 'state-set)))
		  (loop for q being the hash-keys of (ht s) do
		       (with-ht (tr nil) (transitions q)
			 (when (<= (minc tr) c (maxc tr))
			   (setf (gethash (to tr) (ht p)) t))))
		  (unless (htpresent sets p)
		    (setf (htref sets p) p)
		    (push p worklist)
		    (setf (htref newstate p) (make-instance 'state)))
		  (let ((q (htref newstate p))
			(min c)
			(max (if (< (1+ n) len)
				 (1- (aref points (1+ n)))
				 +max-char-code+)))
		    (htadd (transitions r)
			   (make-instance
			    'transition :minc min :maxc max :to q)))))))
    (setf (deterministic a) t)
    (remove-dead-transitions a)))

(defun minimize (a)
  "Minimizes, and determinizes if not already deterministic, A and
returns it."
  (with-slots (singleton minimization hash-code) a
    (unless singleton
      (ecase minimization
	(huffman (minimize-huffman a))
	(brzozowski (minimize-brzozowski a))
	(hopcroft (minimize-hopcroft a))))
    (setf hash-code (+ (* 3 (num-of-states a)) (* 2 (num-of-transitions a))))
    (when (= hash-code 0)
      (setf hash-code 1)))
  a)

(defun states-agree (trs mark n1 n2)
  (let ((t1 (aref trs n1))
	(t2 (aref trs n2)))
    (loop with k1 = 0 and k2 = 0
       and l1 = (length t1) and l2 = (length t2)
       while (and (< k1 l1) (< k2 l2)) do
	 (cond
	   ((< (maxc (aref t1 k1)) (minc (aref t2 k2)))
	    (incf k1))
	   ((< (maxc (aref t2 k2)) (minc (aref t1 k1)))
	    (incf k2))
	   (t (let ((m1 (num (to (aref t1 k1))))
		    (m2 (num (to (aref t2 k2)))))
		(when (> m1 m2)
		  (rotatef m1 m2))
		(when (aref mark m1 m2)
		  (return nil))
		(if (< (maxc (aref t1 k1)) (maxc (aref t2 k2)))
		    (incf k1)
		    (incf k2)))))
       finally (return t))))

(defun add-triggers (trs triggers n1 n2)
  (let ((t1 (aref trs n1))
	(t2 (aref trs n2)))
    (loop with k1 = 0 and k2 = 0
       while (and (< k1 (length t1)) (< k2 (length t2))) do
	 (cond
	   ((< (maxc (aref t1 k1)) (minc (aref t2 k2)))
	    (incf k1))
	   ((< (maxc (aref t2 k2)) (minc (aref t1 k1)))
	    (incf k2))
	   (t (unless (eq (to (aref t1 k1)) (to (aref t2 k2)))
		(let ((m1 (num (to (aref t1 k1))))
		      (m2 (num (to (aref t2 k2)))))
		  (when (> m1 m2)
		    (rotatef m1 m2))
		  (unless (aref triggers m1 m2)
		    (setf (aref triggers m1 m2) (make-hash-table)))
		  (setf (gethash (make-instance 'int-pair :n1 n1 :n2 n2)
				 (aref triggers m1 m2))
			t)))
	      (if (< (maxc (aref t1 k1)) (maxc (aref t2 k2)))
		  (incf k1)
		  (incf k2)))))))

(defun mark-pair (mark triggers n1 n2)
  (setf (aref mark n1 n2) t)
  (when (aref triggers n1 n2)
    (loop for p being the hash-keys of (aref triggers n1 n2) do
	 (let ((m1 (n1 p))
	       (m2 (n2 p)))
	   (when (> m1 m2)
	     (rotatef m1 m2))
	   (unless (aref mark m1 m2)
	     (mark-pair mark triggers m1 m2))))))

(defun ht-set-to-vector (ht)
  (loop with vec = (make-array (hash-table-count ht))
     for k being the hash-keys of ht
     and i from 0 do
       (setf (aref vec i) k)
     finally (return vec)))

(defun minimize-huffman (a)
  "Minimizes A using the standard textbook, Huffman's
algorithm. Complexity: O(N ^ 2), where N is the number of states."
  (determinize a)
  (totalize a)
  (let* ((ss (states a))
	 (ss-cnt (hash-table-count ss))
	 (trs (make-array ss-cnt))
	 (states (ht-set-to-vector ss))
	 (mark (make-array `(,ss-cnt ,ss-cnt) :element-type 'boolean
			   :initial-element nil))
	 (triggers (make-array `(,ss-cnt ,ss-cnt) :initial-element nil))
	 (numclasses 0))
    (loop for n1 below ss-cnt do
	 (setf (num (aref states n1)) n1
	       (aref trs n1) (sorted-transition-vector (aref states n1) nil))
	 (loop for n2 from (1+ n1) below ss-cnt
	    unless (eq (accept (aref states n1)) (accept (aref states n2))) do
	      (setf (aref mark n1 n2) t)))
    (loop for n1 below ss-cnt do
	 (loop for n2 from (1+ n1) below ss-cnt
	    unless (aref mark n1 n2) do
	      (if (states-agree trs mark n1 n2)
		  (add-triggers trs triggers n1 n2)
		  (mark-pair mark triggers n1 n2))))
    (loop for n below ss-cnt do
	 (setf (num (aref states n)) -1))
    (loop for n1 below ss-cnt
       when (= (num (aref states n1)) -1) do
	 (setf (num (aref states n1)) numclasses)
	 (loop for n2 from (1+ n1) below ss-cnt
	    unless (aref mark n1 n2) do
	      (setf (num (aref states n2)) numclasses))
	 (incf numclasses))
    (let ((newstates (make-array numclasses)))
      (loop for n below numclasses do
	   (setf (aref newstates n) (make-instance 'state)))
      (loop for n below ss-cnt do
	   (setf (num (aref newstates (num (aref states n)))) n)
	   (when (eq (aref states n) (initial a))
	     (setf (initial a) (aref newstates (num (aref states n))))))
      (loop for n below numclasses do
	   (let ((s (aref newstates n)))
	     (setf (accept s) (accept (aref states (num s))))
	     (with-ht (tr nil) (transitions (aref states (num s)))
	       (htadd (transitions s)
		      (make-instance
		       'transition :minc (minc tr) :maxc (maxc tr)
		       :to (aref newstates (num (to tr))))))))
      (remove-dead-transitions a))))

(defun minimize-brzozowski (a)
  "Minimizes A using Brzozowski's algorithm. Complexity: O(2 ^ N),
where N is the number of states, but works very well in practice (even
better than Hopcroft's)."
  (if (singleton a)
      nil
      (progn
	(determinize2 a (make-instance 'state-set :ht (areverse a)))
	(determinize2 a (make-instance 'state-set :ht (areverse a))))))

(defun minimize-hopcroft (a)
  "Minimizes A using Hopcroft's algorithm. Complexity: O(N log N),
regarded as one of the most generally efficient existing algorithms."
  (determinize a)
  (let ((trs (transitions (initial a))))
    (when (= (cnt trs) 1)
      (with-ht (tr nil) trs
	(when (and (eq (to tr) (initial a))
		   (= (minc tr) +min-char-code+)
		   (= (maxc tr) +max-char-code+))
	  (return-from minimize-hopcroft)))))
  (totalize a)
  (let* ((ss (states a))
	 (ss-cnt (hash-table-count ss))
	 (states (ht-set-to-vector ss)))
    (set-state-nums ss)
    (let* ((sigma (start-points a))
	   (sigma-cnt (length sigma))
	   (rvrs (make-array `(,ss-cnt ,sigma-cnt) :initial-element nil))
	   (rvrs-ne (make-array `(,ss-cnt ,sigma-cnt) :element-type 'boolean
				:initial-element nil))
	   (partition (make-array ss-cnt :initial-element nil))
	   (block (make-array ss-cnt :element-type 'fixnum))
	   (active (make-array `(,ss-cnt ,sigma-cnt)))
	   (active2 (make-array `(,ss-cnt ,sigma-cnt) :initial-element nil))
	   (pending nil)
	   (pending2 (make-array `(,sigma-cnt ,ss-cnt) :element-type 'boolean
				 :initial-element nil))
	   (split nil)
	   (split2 (make-array ss-cnt :element-type 'boolean
			       :initial-element nil))
	   (refine nil)
	   (refine2 (make-array ss-cnt :element-type 'boolean
				:initial-element nil))
	   (splitblock (make-array ss-cnt :initial-element nil))
	   (k 2))
      (loop for j below ss-cnt do
 	 (loop for i below sigma-cnt do
	      (setf (aref active j i) (make-instance 'state-list))))
      (loop for q below ss-cnt
	 for qq = (aref states q) do
	   (let ((j (if (accept qq) 0 1)))
	     (push qq (aref partition j))
	     (setf (aref block (num qq)) j)
	     (loop for i below sigma-cnt do
		  (let* ((aa (code-char (aref sigma i)))
			 (p (sstep qq aa)))
		    (push qq (aref rvrs (num p) i))
		    (setf (aref rvrs-ne (num p) i) t)))))
      (loop for j from 0 to 1 do
	   (loop for i below sigma-cnt do
		(loop for qq in (aref partition j)
		   when (aref rvrs-ne (num qq) i) do
		     (setf (aref active2 (num qq) i)
			   (slnadd (aref active j i) qq)))))
      (loop for i below sigma-cnt
	 for i0 = (size (aref active 0 i))
	 and i1 = (size (aref active 1 i)) do
	   (let ((j (if (<= i0 i1) 0 1)))
	     (push (make-instance 'int-pair :n1 j :n2 i) pending)
	     (setf (aref pending2 i j) t)))
      (loop for ip = (first pending)
            for p = (when pending (n1 ip)) and i = (when pending (n2 ip))
            while pending do
           (pop pending)
	   (setf (aref pending2 i p) nil)
	   (loop for m = (fst (aref active p i)) then (succ m)
	      while m do
		(loop for s in (aref rvrs (num (q m)) i)
		   unless (aref split2 (num s)) do
		     (setf (aref split2 (num s)) t)
		     (push s split)
		     (let ((j (aref block (num s))))
		       (push s (aref splitblock j))
		       (unless (aref refine2 j)
			 (setf (aref refine2 j) t)
			 (push j refine)))))
	   (loop for j in refine do
		(when (< (length (aref splitblock j))
			 (length (aref partition j)))
		  (loop for s in (aref splitblock j) do
		       (setf (aref partition j) (remove s (aref partition j)))
		       (push s (aref partition k))
		       (setf (aref block (num s)) k)
		       (loop for c below sigma-cnt
			  for sn = (aref active2 (num s) c)
			  when (and sn (eq (sl sn) (aref active j c))) do
			    (slnremove sn)
			    (setf (aref active2 (num s) c)
				  (slnadd (aref active k c) s))))
		  (loop for c below sigma-cnt
		     for ij = (size (aref active j c))
		     and ik = (size (aref active k c))
		     if (and (not (aref pending2 c j)) (< 0 ij) (<= ij ik)) do
		       (setf (aref pending2 c j) t)
		       (push (make-instance 'int-pair :n1 j :n2 c) pending)
		     else do
		       (setf (aref pending2 c k) t)
		       (push (make-instance 'int-pair :n1 k :n2 c) pending))
		  (incf k))
		(loop for s in (aref splitblock j) do
		     (setf (aref split2 (num s)) nil))
		(setf (aref refine2 j) nil)
		(setf (aref splitblock j) nil))
	   (setq split nil)
	   (setq refine nil))
      (let ((newstates (make-array k)))
	(loop for n below k
	   for s = (make-instance 'state) do
	     (setf (aref newstates n) s)
	     (loop for q in (aref partition n) do
		  (when (eq q (initial a))
		    (setf (initial a) s))
		  (setf (accept s) (accept q)
			(num s) (num q)
			(num q) n)))
	(loop for n below k
	   for s = (aref newstates n) do
	     (with-ht (tr nil) (transitions (aref states (num s)))
	       (setf (num s) n)
	       (htadd (transitions s)
		      (make-instance
		       'transition :minc (minc tr) :maxc (maxc tr)
		       :to (aref newstates (num (to tr)))))))
	(remove-dead-transitions a)))))

(defun areverse (a)
  "Reverses the language of non-singleton A. Returns a hash table of
new initial states."
  (let ((m (make-hash-table))
	(states (states a))
	(astates (accepting-states a)))
    (loop for r being the hash-keys of states do
	 (setf (gethash r m)
	       (make-generalized-hash-table +equalp-key-situation+)
	       (accept r) nil))
    (loop for r being the hash-keys of states do
	 (with-ht (tr nil) (transitions r)
	   (htadd (gethash (to tr) m)
		  (make-instance
		   'transition :minc (minc tr) :maxc (maxc tr) :to r))))
    (loop for r being the hash-keys of states do
	 (setf (transitions r) (gethash r m)))
    (setf (accept (initial a)) t
	  (initial a) (make-instance 'state))
    (loop for r being the hash-keys of astates do
	 (add-epsilon (initial a) r))
    (setf (deterministic a) nil)
    astates))

(defun add-epsilons (a pairs)
  "Adds epsilon transitions to A and returns it. This is done by
adding extra character interval transitions that are equivalent to the
given set of epsilon transitions. PAIRS is a list of state-pair
objects representing pairs of source-destination states where the
epsilon transitions should be added."
  (expand-singleton a)
  (let ((forward (make-hash-table))
	(back (make-hash-table)))
    (loop for p in pairs do
       (let ((tos (gethash (s1 p) forward))
	     (froms (gethash (s2 p) back)))
	 (unless tos
	   (setq tos (make-hash-table))
	   (setf (gethash (s1 p) forward) tos))
	 (setf (gethash (s2 p) tos) t)
	 (unless froms
	   (setq froms (make-hash-table))
	   (setf (gethash (s2 p) back) froms))
	 (setf (gethash (s1 p) froms) t)))
    (let ((worklist pairs)
	  (workset (make-generalized-hash-table +equalp-key-situation+)))
      (loop for p in pairs do (htadd workset p))
      (loop for p = (first worklist)
            while worklist do
           (pop worklist)
	   (htremove workset p)
	   (let ((tos (gethash (s2 p) forward))
		 (froms (gethash (s1 p) back)))
	     (when tos
	       (loop for s being the hash-keys of tos
		  for pp = (make-instance 'state-pair :s1 (s1 p) :s2 s)
		  unless (member pp pairs
				 :test #'(lambda (o1 o2)
					   (eqv o1 o2 +equalp-key-situation+)))
		  do
		    (push pp pairs)
		    (setf (gethash s (gethash (s1 p) forward)) t)
		    (setf (gethash (s1 p) (gethash s back)) t)
		    (push pp worklist)
		    (htadd workset pp)
		    (when froms
		      (loop for q being the hash-keys of froms
			 for qq = (make-instance 'state-pair :s1 q :s2 (s1 p))
			 unless (htpresent workset qq) do
			   (push qq worklist)
			   (htadd worklist qq)))))))
      (loop for p in pairs do
	   (add-epsilon (s1 p) (s2 p)))
      (setf (deterministic a) nil)
      (check-minimize-always a))))

(defun run (a str)
  "Returns true if STR is accepted by A. As a side-effect, A is
determinized if not already deterministic. Complexity: linear in the
length of STR (when A is deterministic)."
  (if (singleton a)
    (string= str (singleton a))
    (progn
      (determinize a)
      (let ((p (initial a)))
	(loop for i below (length str)
	   for q = (sstep p (elt str i))
	   unless q
	   return nil
	   do (setq p q)
	   finally (return (accept p)))))))

(defun run-to-first-match (a str &optional (start 0) (end (length str)))
  "Returns the end position of match if a substring of STR, optionally
between positions START and END, is found that is accepted by A;
otherwise, returns nil. Complexity: linear in the length of STR (when
A is deterministic)."
  (if (singleton a)
      (let ((from (search (singleton a) str :start2 start :end2 end)))
	(when from
	  (+ from (length str))))
      (progn
	(determinize a)
	(let ((p (initial a)))
	  (loop for i from start below end
	     for q = (sstep p (elt str i)) do
	       (unless q
		 (return nil))
	       (if (accept q)
		   (return (1+ i))
		   (setq p q))
	     finally (return nil))))))

(defun run-to-first-unmatch (a str &optional (start 0) (end (length str)))
  "Returns the end position of match if a substring of STR, optionally
between positions START and END, is found that is accepted by A;
otherwise, returns nil. A greedy approach is taken until the first
match failure or the end of STR (whatever happens first), trying to
extend the match length one character at a time. Complexity: linear in
the length of STR (when A is deterministic)."
  (if (singleton a)
      (let ((from (search (singleton a) str :start2 start :end2 end)))
	(when from
	  (+ from (length str))))
      (progn
	(determinize a)
	(let* ((p (initial a))
	       (matched (accept p)))
	  (loop for i from start below end
	     for q = (sstep p (elt str i))
	     if (not q)
	     return (if matched i nil)
	     else do
	       (if (accept q)
		   (setq matched t)
		   (when matched
		       (return i)))
	       (setq p q)
	     finally (return (if matched i nil)))))))

(defun num-of-states (a)
  "Returns the number of states of A."
  (if (singleton a)
      (1+ (length (singleton a)))
      (hash-table-count (states a))))

(defun num-of-transitions (a)
  "Returns the number of transitions of A."
  (if (singleton a)
    (length (singleton a))
    (loop for s being the hash-keys of (states a)
       sum (cnt (transitions s)))))

(defun empty-p (a)
  "Returns true if A accepts no strings."
  (if (singleton a)
      nil
      (and (not (accept (initial a))) (= (cnt (transitions (initial a))) 0))))

(defun subset-of (a a2)
  "Returns true if the language of A is a subset of the language of A2."
  (if (singleton a)
    (if (singleton a2)
	(string= (singleton a) (singleton a2))
	(run a2 (singleton a)))
    (empty-p (aintersection a (acomplement a2)))))

(defmethod eqv ((a1 automaton) (a2 automaton) (s (eql +equalp-key-situation+)))
  "Returns true if the language of A1 is equal to the language of A2."
  (or (and (singleton a1) (singleton a2)
	   (string= (singleton a1) (singleton a2)))
      (and (= (hash a1 s) (hash a2 s))
	   (subset-of a1 a2) (subset-of a2 a1))))

(defmethod hash ((a automaton) (s (eql +equalp-key-situation+)))
  "Returns the hash code for automaton A."
  (when (= (hash-code a) 0)
    (minimize a))
  (hash-code a))

(defvar *print-renumerate-states* nil)

(defmethod print-object ((a automaton) s)
  (let* ((a (if *print-renumerate-states* (clone a) a))
	 (states (states a)))
    (when *print-renumerate-states*
      (set-state-nums states))
    (format s "~@<initial state: ~A ~_~@<~{~W~^ ~_~}~:>~:>"
  	    (num (initial a))
	    (loop for st being the hash-keys of states collect st)))
  a)

(defun clone-expanded (a)
  "Returns a clone of A, expanded if singleton."
  (let ((a (clone a)))
    (expand-singleton a)
    a))

(defmethod clone ((a automaton))
  "Returns a clone of A."
  (let ((a2 (make-instance 'automaton)))
    (setf (minimization a2) (minimization a)
	  (deterministic a2) (deterministic a)
	  (info a2) (info a)
	  (hash-code a2) (hash-code a)
	  (minimize-always a2) (minimize-always a))
    (if (singleton a)
	(setf (singleton a2) (singleton a))
	(let ((map (make-hash-table))
	      (states (states a)))
	  (loop for s being the hash-keys of states do
	       (setf (gethash s map) (make-instance 'state)))
	  (loop for s being the hash-keys of states do
	       (let ((p (gethash s map)))
		 (setf (accept p) (accept s))
		 (when (eq s (initial a))
		   (setf (initial a2) p))
		 (let ((p-table (transitions p)))
		   (with-ht (tr nil) (transitions s)
		     (htadd p-table
			    (make-instance
			     'transition :minc (minc tr) :maxc (maxc tr)
			     :to (gethash (to tr) map)))))))))
    a2))

(defun slnadd (sl q)
  "Adds state Q to state-list SL and returns a new state-list-node
object."
  (make-instance 'state-list-node :q q :sl sl))

(defmethod initialize-instance :after ((sln state-list-node) &rest initargs)
  (declare (ignorable initargs))
  (if (= (incf (size (sl sln))) 1)
      (setf (fst (sl sln)) sln
	    (lst (sl sln)) sln)
      (setf (succ (lst (sl sln))) sln
	    (pred sln) (lst (sl sln))
	    (lst (sl sln)) sln)))

(defun slnremove (sln)
  "Removes state-list-node SLN from its state-list object."
  (decf (size (sl sln)))
  (if (eq sln (fst (sl sln)))
      (setf (fst (sl sln)) (succ sln))
      (setf (succ (pred sln)) (succ sln)))
  (if (eq sln (lst (sl sln)))
      (setf (lst (sl sln)) (pred sln))
      (setf (pred (succ sln)) (pred sln))))