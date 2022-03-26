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
;;; Optimized version of binseq.lisp:
;;;   the contents of a leaf node must not be nil nor a cons.

(in-package :binseq)

(defun obinseq-p (s)
  (or (null s)
      (atom s)
      (and (consp s)
	   (and (integerp (car s)) ; might wanna check the value
		(consp (cdr s))
		(obinseq-p (cadr s))
		(obinseq-p (cddr s))))))

(defun list-obinseq (l)
  (flet ((%split (l n) ; TODO: use side-effects to avoid consing
	   (loop for b on l
	      and i from 0
	      if (< i n)
	      collect (car b) into a
	      else do (return (values a b))
	      finally (return (values l nil)))))
    (cond
      ((null l) nil)
      ((null (cdr l))
       (let ((e (car l)))
       (assert (and e (atom e)) nil
	       "Sequence element must be a non-nil atom: ~S" e)
       e))
      (t (let ((len (length l)))
	   (multiple-value-bind (a b) (%split l (floor len 2))
	     `(,len . (,(list-obinseq a) . ,(list-obinseq b)))))))))

(defun obinseq-list (s)
  (labels ((%to-list (s l)
	     (cond
	       ((null s) nil)
	       ((atom s) (cons s l))
	       (t (%to-list (cadr s) (%to-list (cddr s) l))))))
    (%to-list s nil)))

(defun vector-obinseq (v &optional (start 0) (end (length v)))
  (cond
    ((= start end) nil)
    ((= (- end start) 1)
     (let ((e (aref v start)))
       (assert (and e (atom e)) nil
	       "Sequence element must be a non-nil atom: ~S" e)
       e))
    (t (let* ((len (- end start))
	      (mid (+ start (floor len 2))))
	 `(,len . (,(vector-obinseq v start mid) .
		    ,(vector-obinseq v mid end)))))))

(defun obinseq-vector (s)
  (let ((v (make-array (obinseq-length s))))
    (labels ((%set-v (s o)
	       (cond
		 ((null s))
		 ((atom s) (setf (aref v o) s))
		 (t (let ((a (cadr s))
			  (b (cddr s)))
		      (%set-v a o)
		      (%set-v b (+ o (obinseq-length a))))))))
      (%set-v s 0)
      v)))

(defun obinseq-empty (s)
  (null s))

(defun obinseq-length (s)
  (cond
    ((null s) 0)
    ((atom s) 1)
    (t (car s))))

(defun obinseq-cons (e s)
  (obinseq-append e s))

(defun obinseq-snoc (e s)
  (obinseq-append s e))

(defun obinseq-append (a b)
  (labels ((%not-much-longer (a b)
	     (<= (obinseq-length a) (* *imbalance-bound* (obinseq-length b))))
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
	     (let ((len (+ (obinseq-length a) (obinseq-length b))))
	       (assert (>= len 2))
	       `(,len . (,a . ,b))))
	   (%rotate-right (s1 s2)
	     (cond
	       ((consp s1)
		(let* ((a (cadr s1))
		       (b (cddr s1))
		       (la (obinseq-length a))
		       (lb (obinseq-length b))
		       (ls2 (obinseq-length s2)))
		  (cond
		    ((%cond-single la lb ls2)
		     (%cons a (%cons b s2)))
		    ((%cond-double la lb ls2)
		     (let ((s11 (cadr b))
			   (s12 (cddr b)))
		       (%cons (%cons a s11) (%cons s12 s2))))
		    (t (%append a (%append b s2))))))
	       (t (%append a (%append b s2)))))
	   (%rotate-left (s1 s2)
	     (cond
	       ((consp s2)
		(let* ((a (cddr s2))
		       (b (cadr s2))
		       (la (obinseq-length a))
		       (lb (obinseq-length b))
		       (ls1 (obinseq-length s1)))
		  (cond
		    ((%cond-single la lb ls1)
		     (%cons (%cons s1 b) a))
		    ((%cond-double la lb ls1)
		     (let ((s21 (cddr b))
			   (s22 (cadr b)))
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
      ((null a) b)
      ((null b) a)
      (t (%append a b)))))

(defun obinseq-front (s i)
  (cond
    ((<= i 0) nil)
    ((<= (obinseq-length s) i) s)
    ((<= i (obinseq-length (cadr s))) (obinseq-front (cadr s) i))
    (t (obinseq-append
	(cadr s)
	(obinseq-front (cddr s) (- i (obinseq-length (cadr s))))))))

(defun obinseq-back (s i)
  (cond
    ((<= i 0) nil)
    ((<= (obinseq-length s) i) s)
    ((<= i (obinseq-length (cddr s))) (obinseq-back (cddr s) i))
    (t (obinseq-append
	(obinseq-back (cadr s) (- i (obinseq-length (cddr s))))
	(cddr s)))))

(defun %ohas-index (s i)
  (and (<= 0 i) (< i (obinseq-length s))))

(defun %ohas-gap (s i)
  (and (<= 0 i) (<= i (obinseq-length s))))

(defun obinseq-get (s i)
  (assert (%ohas-index s i) nil "Index out of bounds: ~S, ~S" s i)
  (obinseq-back (obinseq-front s (1+ i)) 1))

(defun obinseq-set (s i e)
  (assert (%ohas-index s i) nil "Index out of bounds: ~S, ~S, ~S" s i e)
  (obinseq-append
   (obinseq-front s i)
   (obinseq-cons e (obinseq-back s (- (obinseq-length s) i 1)))))

(defun obinseq-sub (s i n)
  (assert (and (>= n 0) (<= (+ i n) (obinseq-length s))) nil
	  "Invalid subsequence bounds: ~S, ~S, ~S" s i n)
  (obinseq-back (obinseq-front s (+ i n)) n))

(defun obinseq-insert (s i e)
  (assert (%ohas-gap s i) nil "Index out of bounds: ~S, ~S, ~S" s i e)
  (obinseq-append
   (obinseq-front s i)
   (obinseq-cons e (obinseq-back s (- (obinseq-length s) i)))))

(defun obinseq-insert* (s i s2)
  (assert (%ohas-gap s i) nil "Index out of bounds: ~S, ~S, ~S" s i s2)
  (if (null s2)
      s
      (obinseq-append
       (obinseq-front s i)
       (obinseq-append s2 (obinseq-back s (- (obinseq-length s) i))))))

(defun obinseq-remove (s i)
  (assert (%ohas-index s i) nil "Index out of bounds: ~S, ~S" s i)
  (obinseq-append
   (obinseq-front s i)
   (obinseq-back s (- (obinseq-length s) i 1))))

(defun obinseq-remove* (s i n)
  (assert (%ohas-index s i) nil "Start index out of bounds: ~S, ~S, ~S" s i n)
  (assert (and (>= n 0) (<= (+ i n) (obinseq-length s))) nil
	  "Count out of range: ~S, ~S, ~S" s i n)
  (if (zerop n)
      s
      (obinseq-append
       (obinseq-front s i)
       (obinseq-back s (- (obinseq-length s) i n)))))