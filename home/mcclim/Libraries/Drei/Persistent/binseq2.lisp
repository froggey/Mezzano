;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
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

;;; Adaptation of binary sequences by Robert Will (real-world-phenomena.org)
;;; Differs from binseq in:
;;;   nodes contain two counts: number of lines and number of objects
;;;   leafs contain obinseqs representing lines

(in-package :binseq)

(defun binseq2-p (s) ; NOTE: should use a 3-vector instead of the 3-list...
  (or (eq s 'empty)
      (and (consp s)
	   (or (and (eq (car s) 'leaf)
		    (obinseq-p (cdr s)))
	       (and (eq (car s) 'node)
		    (let ((nc (cadr s)))
		      (and (consp nc)
			   (integerp (car nc))
			   (integerp (cdr nc))))
		    (consp (cddr s))
		    (binseq2-p (caddr s))
		    (binseq2-p (cdddr s)))))))

(defun list-binseq2* (l) ; l is a list of obinseqs
  (flet ((%split (l n) ; TODO: use side-effects to avoid consing
	   (loop for b on l
	      and i from 0
	      if (< i n)
	      collect (car b) into a
	      else do (return (values a b))
	      finally (return (values l nil)))))
    (cond
      ((null l) 'empty)
      ((null (cdr l)) `(leaf . ,(car l)))
      (t (let ((len (length l)))
	   (multiple-value-bind (a b) (%split l (floor len 2))
	     (let* ((sa (list-binseq2* a))
		    (sb (list-binseq2* b))
		    (size (+ (binseq2-size sa) (binseq2-size sb))))
	       `(node . ((,len . ,size) . (,sa . ,sb))))))))))

(defun list-binseq2 (l) ; TODO: use side-effects to avoid consing
  (list-binseq2*
   (loop
      with curr = nil
      and ll = nil
      for e in l
      do
	(push e curr)
	(when (eql e #\Newline)
	  (push (list-obinseq (nreverse curr)) ll)
	  (setf curr nil))
      finally
	(when curr
	  (push (list-obinseq (nreverse curr)) ll))
	(return (nreverse ll)))))

(defun binseq2-list (s)
  (labels ((%to-list (s l)
	     (cond
	       ((eq s 'empty) l)
	       ((eq (car s) 'leaf) (nconc (obinseq-list (cdr s)) l))
	       (t (%to-list (caddr s) (%to-list (cdddr s) l))))))
    (%to-list s nil)))

(defun vector-binseq2 (v)
  (list-binseq2*
   (loop
      with len = (length v)
      for start = 0 then end
      while (< start len)
      for end = (1+ (or (position #\Newline v :start start) (1- len)))
      collect (vector-obinseq v start end))))

(defun binseq2-vector (s)
  (let ((v (make-array (binseq2-size s))))
    (labels ((%set2-v (s o)
	       (cond
		 ((eq s 'empty))
		 ((eq (car s) 'leaf) (%set-v (cdr s) o))
		 (t (let ((a (caddr s))
			  (b (cdddr s)))
		      (%set2-v a o)
		      (%set2-v b (+ o (binseq2-size a)))))))
	     (%set-v (s o)
	       (cond
		 ((null s))
		 ((atom s) (setf (aref v o) s))
		 (t (let ((a (cadr s))
			  (b (cddr s)))
		      (%set-v a o)
		      (%set-v b (+ o (obinseq-length a))))))))
      (%set2-v s 0))
    v))

(defun binseq2-empty (s)
  (eq s 'empty))

(defun binseq2-length (s)
  (cond
    ((eq s 'empty) 0)
    ((eq (car s) 'leaf) 1)
    (t (caadr s))))

(defun binseq2-size (s)
  (cond
    ((eq s 'empty) 0)
    ((eq (car s) 'leaf) (obinseq-length (cdr s)))
    (t (cdadr s))))

(defun binseq2-cons (e s)
  (binseq2-append `(leaf . ,e) s))

(defun binseq2-snoc (e s)
  (binseq2-append s `(leaf . ,e)))

(defun binseq2-possibly-append-end-lines (a b)
  "If the last line of A does not end with a newline, remove the first
line of B and append it to the last line of A; otherwise, do nothing."
  (let ((a-last-line (cdr (binseq2-back a 1))))
    (if (eql (obinseq-back a-last-line 1) #\Newline)
	(values a b)
	(values
	 (binseq2-set a (1- (binseq2-length a))
		      (obinseq-append a-last-line (cdr (binseq2-front b 1))))
	 (binseq2-back b (1- (binseq2-length b)))))))

;;;(defparameter *imbalance-bound* 3) ; must be >= 3

(defun binseq2-append (a b)
  (labels ((%not-much-longer (a b)
	     (<= (binseq2-length a) (* *imbalance-bound* (binseq2-length b))))
	   (%much-shorter (a b)
	     (not (%not-much-longer b a)))
	   (%similar-in-length (a b)
	     (and (%not-much-longer a b) (%not-much-longer b a)))
	   (%cond-single (la lb lc)
	     (and (<= lb (* *imbalance-bound* lc))
		  (<= (+ lb lc) (* *imbalance-bound* la))))
	   (%cond-double (la lb lc)
	     (<= (+ la lb) (* (+ 1 *imbalance-bound*) lc)))
	   (%cons (a b)
	     (let ((len (+ (binseq2-length a) (binseq2-length b)))
		   (size (+ (binseq2-size a) (binseq2-size b))))
	       (assert (>= len 2))
	       `(node . ((,len . ,size) . (,a . ,b)))))
	   (%rotate-right (s1 s2)
	     (cond
	       ((and (consp s1) (eq (car s1) 'node))
		(let* ((a (caddr s1))
		       (b (cdddr s1))
		       (la (binseq2-length a))
		       (lb (binseq2-length b))
		       (ls2 (binseq2-length s2)))
		  (cond
		    ((%cond-single la lb ls2)
		     (%cons a (%cons b s2)))
		    ((%cond-double la lb ls2)
		     (let ((s11 (caddr b))
			   (s12 (cdddr b)))
		       (%cons (%cons a s11) (%cons s12 s2))))
		    (t (%append a (%append b s2))))))
	       (t (%append a (%append b s2)))))
	   (%rotate-left (s1 s2)
	     (cond
	       ((and (consp s2) (eq (car s2) 'node))
		(let* ((a (cdddr s2))
		       (b (caddr s2))
		       (la (binseq2-length a))
		       (lb (binseq2-length b))
		       (ls1 (binseq2-length s1)))
		  (cond
		    ((%cond-single la lb ls1)
		     (%cons (%cons s1 b) a))
		    ((%cond-double la lb ls1)
		     (let ((s21 (cdddr b))
			   (s22 (caddr b)))
		       (%cons (%cons s1 s22) (%cons s21 a))))
		    (t (%append (%append s1 b) a)))))
	       (t (%append (%append s1 b) a))))
	   (%append (a b)
	     (cond
	       ((%similar-in-length a b)
		(%cons a b))
	       ((%much-shorter a b)
		(%rotate-left a b))
	       (t (%rotate-right a b)))))
    (cond
      ((eq a 'empty) b)
      ((eq b 'empty) a)
      (t (multiple-value-bind (a2 b2) (binseq2-possibly-append-end-lines a b)
	   (cond
	     ((eq a2 'empty) b2)
	     ((eq b2 'empty) a2)
	     (t (%append a2 b2))))))))

;;; Functions whose names end with '2' are passed objects and object offsets
;;; and return binseq2s that are possibly accordingly truncated

(defun binseq2-front (s i)
  (cond
    ((<= i 0) 'empty)
    ((<= (binseq2-length s) i) s)
    ((<= i (binseq2-length (caddr s))) (binseq2-front (caddr s) i))
    (t (binseq2-append
	(caddr s)
	(binseq2-front (cdddr s) (- i (binseq2-length (caddr s))))))))

(defun binseq2-offset (s i)
  (labels ((%offset (s i o)
	     (cond
	       ((or (eq s 'empty) (<= i 0) (eq (car s) 'leaf)) o)
	       ((< i (binseq2-length (caddr s))) (%offset (caddr s) i o))
	       (t (%offset (cdddr s) (- i (binseq2-length (caddr s)))
			   (+ o (binseq2-size (caddr s))))))))
    (%offset s i 0)))

(defun binseq2-front2 (s i)
  (cond
    ((<= i 0) 'empty)
    ((<= (binseq2-size s) i) s)
    ((eq (car s) 'leaf) `(leaf . ,(obinseq-front (cdr s) i)))
    ((<= i (binseq2-size (caddr s))) (binseq2-front2 (caddr s) i))
    (t (binseq2-append
	(caddr s)
	(binseq2-front2 (cdddr s) (- i (binseq2-size (caddr s))))))))

(defun binseq2-line2 (s i)
  (labels ((%line (s i o)
	     (cond
	       ((or (eq s 'empty) (<= i 0) (eq (car s) 'leaf)) o)
	       ((< i (binseq2-size (caddr s))) (%line (caddr s) i o))
	       (t (%line (cdddr s) (- i (binseq2-size (caddr s)))
			 (+ o (binseq2-length (caddr s))))))))
    (%line s i 0)))

(defun binseq2-back (s i)
  (cond
    ((<= i 0) 'empty)
    ((<= (binseq2-length s) i) s)
    ((<= i (binseq2-length (cdddr s))) (binseq2-back (cdddr s) i))
    (t (binseq2-append
	(binseq2-back (caddr s) (- i (binseq2-length (cdddr s))))
	(cdddr s)))))

(defun binseq2-back2 (s i)
  (cond
    ((<= i 0) 'empty)
    ((<= (binseq2-size s) i) s)
    ((eq (car s) 'leaf) `(leaf . ,(obinseq-back (cdr s) i)))
    ((<= i (binseq2-size (cdddr s))) (binseq2-back2 (cdddr s) i))
    (t (binseq2-append
	(binseq2-back2 (caddr s) (- i (binseq2-size (cdddr s))))
	(cdddr s)))))

(defun %has2-index (s i)
  (and (<= 0 i) (< i (binseq2-length s))))

(defun %has2-index2 (s i)
  (and (<= 0 i) (< i (binseq2-size s))))

(defun %has2-gap (s i)
  (and (<= 0 i) (<= i (binseq2-length s))))

(defun %has2-gap2 (s i)
  (and (<= 0 i) (<= i (binseq2-size s))))

(defun binseq2-get (s i)
  (assert (%has2-index s i) nil "Index out of bounds: ~S, ~S" s i)
  (cdr (binseq2-back (binseq2-front s (1+ i)) 1)))

(defun binseq2-get2 (s i)
  (assert (%has2-index2 s i) nil "Index out of bounds: ~S, ~S" s i)
  (cdr (binseq2-back2 (binseq2-front2 s (1+ i)) 1)))

(defun binseq2-set (s i e)
  (assert (%has2-index s i) nil "Index out of bounds: ~S, ~S" s i)
  (binseq2-append
   (binseq2-front s i)
   (binseq2-cons e (binseq2-back s (- (binseq2-length s) i 1)))))

(defun binseq2-set2 (s i e) ; an object is also a leaf obinseq!
  (assert (%has2-index2 s i) nil "Index out of bounds: ~S, ~S" s i)
  (assert (and e (atom e)))
  (binseq2-append
   (binseq2-front2 s i)
   (binseq2-cons e (binseq2-back2 s (- (binseq2-size s) i 1)))))

(defun binseq2-sub (s i n)
  (assert (and (>= n 0) (<= (+ i n) (binseq2-length s))) nil
	  "Invalid subsequence bounds: ~S, ~S, ~S" s i n)
  (binseq2-back (binseq2-front s (+ i n)) n))

(defun binseq2-sub2 (s i n)
  (assert (and (>= n 0) (<= (+ i n) (binseq2-size s))) nil
	  "Invalid subsequence bounds: ~S, ~S, ~S" s i n)
  (binseq2-back2 (binseq2-front2 s (+ i n)) n))

(defun binseq2-insert (s i e)
  (assert (%has2-gap s i) nil "Index out of bounds: ~S, ~S, ~S" s i e)
  (binseq2-append
   (binseq2-front s i)
   (binseq2-cons e (binseq2-back s (- (binseq2-length s) i)))))

(defun binseq2-insert2 (s i e) ; an object is also a leaf obinseq!
  (assert (%has2-gap2 s i) nil "Index out of bounds: ~S, ~S, ~S" s i e)
  (assert (and e (atom e)))
  (binseq2-append
   (binseq2-front2 s i)
   (binseq2-cons e (binseq2-back2 s (- (binseq2-size s) i)))))

(defun binseq2-insert* (s i s2)
  (assert (%has2-gap s i) nil "Index out of bounds: ~S, ~S, ~S" s i s2)
  (if (eq s2 'empty)
      s
      (binseq2-append
       (binseq2-front s i)
       (binseq2-append s2 (binseq2-back s (- (binseq2-length s) i))))))

(defun binseq2-insert*2 (s i s2)
  (assert (%has2-gap2 s i) nil "Index out of bounds: ~S, ~S, ~S" s i s2)
  (if (eq s2 'empty)
      s
      (binseq2-append
       (binseq2-front2 s i)
       (binseq2-append s2 (binseq2-back2 s (- (binseq2-size s) i))))))

(defun binseq2-remove (s i)
  (assert (%has2-index s i) nil "Index out of bounds: ~S, ~S" s i)
  (binseq2-append
   (binseq2-front s i)
   (binseq2-back s (- (binseq2-length s) i 1))))

(defun binseq2-remove2 (s i)
  (assert (%has2-index2 s i) nil "Index out of bounds: ~S, ~S" s i)
  (binseq2-append
   (binseq2-front2 s i)
   (binseq2-back2 s (- (binseq2-size s) i 1))))

(defun binseq2-remove* (s i n)
  (assert (%has2-index s i) nil "Start index out of bounds: ~S, ~S, ~S" s i n)
  (assert (and (>= n 0) (<= (+ i n) (binseq2-length s))) nil
	  "Count out of range: ~S, ~S, ~S" s i n)
  (if (zerop n)
      s
      (binseq2-append
       (binseq2-front s i)
       (binseq2-back s (- (binseq2-length s) i n)))))

(defun binseq2-remove*2 (s i n)
  (assert (%has2-index2 s i) nil "Start index out of bounds: ~S, ~S, ~S" s i n)
  (assert (and (>= n 0) (<= (+ i n) (binseq2-size s))) nil
	  "Count out of range: ~S, ~S, ~S" s i n)
  (if (zerop n)
      s
      (binseq2-append
       (binseq2-front2 s i)
       (binseq2-back2 s (- (binseq2-size s) i n)))))