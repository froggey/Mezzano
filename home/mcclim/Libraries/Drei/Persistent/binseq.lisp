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

(in-package :binseq)

(defun binseq-p (s) ; NOTE: should use a 3-vector instead of the 3-list...
  (or (eq s 'empty)
      (and (consp s)
	   (or (eq (car s) 'leaf)
	       (and (eq (car s) 'node)
		    (integerp (cadr s))
		    (consp (cddr s))
		    (binseq-p (caddr s))
		    (binseq-p (cdddr s)))))))

(defun list-binseq (l)
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
	     `(node . (,len . (,(list-binseq a) . ,(list-binseq b))))))))))

(defun binseq-list (s)
  (labels ((%to-list (s l)
	     (cond
	       ((eq s 'empty) l)
	       ((eq (car s) 'leaf) (cons (cdr s) l))
	       (t (%to-list (caddr s) (%to-list (cdddr s) l))))))
    (%to-list s nil)))

(defun vector-binseq (v &optional (start 0) (end (length v)))
  (cond
    ((= start end) 'empty)
    ((= (- end start) 1) `(leaf . ,(aref v start)))
    (t (let* ((len (- end start))
	      (mid (+ start (floor len 2))))
	 `(node . (,len . (,(vector-binseq v start mid) .
			    ,(vector-binseq v mid end))))))))

(defun binseq-vector (s)
  (let ((v (make-array (binseq-length s))))
    (labels ((%set-v (s o)
	       (cond
		 ((eq s 'empty))
		 ((eq (car s) 'leaf) (setf (aref v o) (cdr s)))
		 (t (let ((a (caddr s))
			  (b (cdddr s)))
		      (%set-v a o)
		      (%set-v b (+ o (binseq-length a))))))))
      (%set-v s 0)
      v)))

(defun binseq-empty (s)
  (eq s 'empty))

(defun binseq-length (s)
  (cond
    ((eq s 'empty) 0)
    ((eq (car s) 'leaf) 1)
    (t (cadr s))))

(defun binseq-cons (e s)
  (binseq-append `(leaf . ,e) s))

(defun binseq-snoc (e s)
  (binseq-append s `(leaf . ,e)))

(defparameter *imbalance-bound* 3) ; must be >= 3

(defun binseq-append (a b)
  (labels ((%not-much-longer (a b)
	     (<= (binseq-length a) (* *imbalance-bound* (binseq-length b))))
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
	     (let ((len (+ (binseq-length a) (binseq-length b))))
	       (assert (>= len 2))
	       `(node . (,len . (,a . ,b)))))
	   (%rotate-right (s1 s2)
	     (cond
	       ((and (consp s1) (eq (car s1) 'node))
		(let* ((a (caddr s1))
		       (b (cdddr s1))
		       (la (binseq-length a))
		       (lb (binseq-length b))
		       (ls2 (binseq-length s2)))
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
		       (la (binseq-length a))
		       (lb (binseq-length b))
		       (ls1 (binseq-length s1)))
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
      (t (%append a b)))))

(defun binseq-front (s i)
  (cond
    ((<= i 0) 'empty)
    ((<= (binseq-length s) i) s)
    ((<= i (binseq-length (caddr s))) (binseq-front (caddr s) i))
    (t (binseq-append
	(caddr s)
	(binseq-front (cdddr s) (- i (binseq-length (caddr s))))))))

(defun binseq-back (s i)
  (cond
    ((<= i 0) 'empty)
    ((<= (binseq-length s) i) s)
    ((<= i (binseq-length (cdddr s))) (binseq-back (cdddr s) i))
    (t (binseq-append
	(binseq-back (caddr s) (- i (binseq-length (cdddr s))))
	(cdddr s)))))

(defun %has-index (s i)
  (and (<= 0 i) (< i (binseq-length s))))

(defun %has-gap (s i)
  (and (<= 0 i) (<= i (binseq-length s))))

(defun binseq-get (s i)
  (assert (%has-index s i) nil "Index out of bounds: ~S, ~S" s i)
  (cdr (binseq-back (binseq-front s (1+ i)) 1)))

(defun binseq-set (s i e)
  (assert (%has-index s i) nil "Index out of bounds: ~S, ~S" s i)
  (binseq-append
   (binseq-front s i)
   (binseq-cons e (binseq-back s (- (binseq-length s) i 1)))))

(defun binseq-sub (s i n)
  (assert (and (>= n 0) (<= (+ i n) (binseq-length s))) nil
	  "Invalid subsequence bounds: ~S, ~S, ~S" s i n)
  (binseq-back (binseq-front s (+ i n)) n))

(defun binseq-insert (s i e)
  (assert (%has-gap s i) nil "Index out of bounds: ~S, ~S, ~S" s i e)
  (binseq-append
   (binseq-front s i)
   (binseq-cons e (binseq-back s (- (binseq-length s) i)))))

(defun binseq-insert* (s i s2)
  (assert (%has-gap s i) nil "Index out of bounds: ~S, ~S, ~S" s i s2)
  (if (eq s2 'empty)
      s
      (binseq-append
       (binseq-front s i)
       (binseq-append s2 (binseq-back s (- (binseq-length s) i))))))

(defun binseq-remove (s i)
  (assert (%has-index s i) nil "Index out of bounds: ~S, ~S" s i)
  (binseq-append
   (binseq-front s i)
   (binseq-back s (- (binseq-length s) i 1))))

(defun binseq-remove* (s i n)
  (assert (%has-index s i) nil "Start index out of bounds: ~S, ~S, ~S" s i n)
  (assert (and (>= n 0) (<= (+ i n) (binseq-length s))) nil
	  "Count out of range: ~S, ~S, ~S" s i n)
  (if (zerop n)
      s
      (binseq-append
       (binseq-front s i)
       (binseq-back s (- (binseq-length s) i n)))))