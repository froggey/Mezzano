;;; Test cases for Iterate.

;;; Copyright (c) 2003 Andreas Fuchs <asf@boinkor.net>
;;; Copyright (c) 2004-2005 Joerg Hoehle <hoehle@users.sourceforge.net>

;;; License:
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Commentary:

;; Although growing, this testsuite does not yet cover every documented
;; feature of Iterate.

(cl:defpackage #:iterate.test
  (:use #:cl #:iterate
	#+sbcl #:sb-rt
	#-sbcl #:regression-test))

(cl:in-package #:iterate.test)

(rem-all-tests)

(deftest dsetq.1
    (let (x y)
      (dsetq (x y) (list* 4 5 6))
      (list x y))
  (4 5))

(deftest dsetq.2
    (let (x y)
      (dsetq (x nil y) (list* 4 5 6 7))
      (list x y))
  (4 6))

(deftest dsetq.3
    (let ((a '((1 2) 3)) b)
      (dsetq (a b) a)
      (values a b))
  (1 2) 3)

(deftest dsetq.destructuring.1
    (let (x y)
      (dsetq (x . y) (list* 4 5 6))
      (list x y))
  (4 (5 . 6)))

(deftest dsetq.destructuring.2
    (let (x y z)
      (dsetq (x nil (y . z)) (list* 4 5 '(6 7 . 8) 9))
      (list x y z))
  (4 6 (7 . 8)))

(deftest dsetq.values.1
    (let (x y)
      (dsetq (values x y) (values 1 'a))
      (list x y))
  (1 a))

(deftest dsetq.values.2
    (let (x y)
      (dsetq (values x nil y) (values 1 'a "c"))
      (list x y))
  (1 "c"))

(deftest dsetq.values.3
    (let (x)
      (dsetq (values nil (nil . x)) (values 1 '(a b . c)))
      x)
  (b . c))

(deftest dsetq.values.4
    (let (x y z) (dsetq (values nil x (y z))
			(values 1 'a '(b c))) (list x y z))
  (a b c))

(deftest repeat.1
    (iter (repeat 9) (count 1))
  9)

(deftest repeat.2
    (iter (repeat 2.5s0) (counting t))
  3)

(deftest repeat.3
    (iter (repeat -1.5f0) (counting t))
  0)

(deftest locally.1
    (iterate (for i in '(1 2 3)) (repeat 2)
	     (locally (collect i) (collect 0)))
  (1 0 2 0))

(deftest locally.2
    (iterate (for i in '(1 2 3)) (repeat 2)
	     (locally
	       (declare (optimize safety))
	       (declare (fixnum i)) (collect i)))
  (1 2))

(deftest always.1
    (iter (repeat 3) (always 2))
  2)

(deftest always.2
    (iter (repeat 0) (always 2))
  t)

(deftest always.3
    (iter (for i in '()) (always i))
  t)

(deftest always.never.1
    (iterate  (repeat 2)
	      (always 2)
	      (never nil))
  2)

(deftest always.never.2
    (iter (for x in '(b (2 . a)))
	  (if (consp x) (always (car x)) (always x))
	  (never nil))
  2)

(deftest thereis.finally.1
    (iter (repeat 3) (thereis nil) (finally (prin1 "hi")))
  nil)

(deftest thereis.finally.2
    (with-output-to-string (*standard-output*) 
      (iter (repeat 3) (thereis nil) (finally (princ "hi"))))
  "hi")

(deftest thereis.finally.3
    (iter (repeat 3) (thereis nil) (finally (return 2)))
  2)

(deftest thereis.finally-protected.1
    (iter (repeat 3) (thereis 4) (finally-protected (prin1 "hi")))
  4)

(deftest thereis.finally-protected.2
    (with-output-to-string (*standard-output*) 
      (iter (repeat 3) (thereis 4) (finally-protected (princ "hi"))))
  "hi")

(deftest finding.such-that.2
    (iter (for i in '(7 -4 2 -3 4))
	  (if (plusp i)
	      (finding    i  such-that (evenp i))
	      (finding (- i) such-that (oddp i))))
  2)

(deftest finding.such-that.nest.1
    (iter (for i in '(1 2 3))
	  (finding (1+ i)
		   such-that #'(lambda (x)
				 (declare (ignore x))
				 (collect (- i) into m))))
  2) ; not -1 as some old version did

(deftest finding.such-that.nest.2
    (iter (for i in '(1 2 3))
	  (finding (1+ i)
		   such-that #'(lambda (x)
				 (finding (- x)
					  such-that #'(lambda (x) x nil)
					  into n)
				 t)
		   into m)
	  (finally (return (values m n))))
  2 nil) ; not -2 nil as some old version did

(deftest finding.thereis.1
    (iterate (for x in '(a 7 (-4 -3)))
	     (thereis (consp x))
	     (finding x such-that (numberp x)))
  7)

(deftest finding.thereis.2
    (iterate (for x in '(a (-4 -3) 7))
	     (thereis (consp x))
	     (finding x such-that (numberp x)))
  t)

(deftest finding.thereis.3
    (iterate (for x in '(a #\b))
	     (thereis (consp x))
	     (finding x such-that (numberp x)))
  nil)

(deftest finding.always.1
    (iterate (for x in '(-4 -2 -3))
	     (always (numberp x))
	     (finding x such-that (plusp x) on-failure t))
  t)

(deftest finding.always.2
    (iterate (for x in '(-4 7 -2 -3))
	     (always (numberp x))
	     (finding x such-that (plusp x) on-failure t))
  7)

(deftest finding.always.3
    (iterate (for x in '(-4 c -3))
	     (always (numberp x))
	     (finding x such-that (plusp x) on-failure t))
  nil)

(defun setup-hash-table (hash)
  (dotimes (i (random 100)) (declare (ignorable i))
	(setf (gethash (random 10000) hash) (random 10000))
	(setf (gethash (gensym) hash) (gensym))))

(deftest in-hashtable.keys
    (let* ((hash (make-hash-table))
	   (all-entries (progn (setup-hash-table hash) '()))
	   (generated-entries
	    (iterate (for (key) in-hashtable hash)
		     (collect key))))
      (maphash (lambda (key value) value (push key all-entries)) hash)
      (= (length all-entries)
	 (length generated-entries)
	 (length (union all-entries generated-entries
			:test (hash-table-test hash)))))
  t)

(deftest in-hashtable.items.1
    (let ((items nil)
	  (hash (make-hash-table)))
      (setup-hash-table hash)
      (maphash (lambda (key item) key (push item items)) hash)
      (set-difference items
		      (iterate (for (key item) in-hashtable hash)
			       (declare (ignore key))
			       (collect item))))
  nil)

(deftest in-hashtable.items.2
    (let ((items nil)
	  (hash (make-hash-table)))
      (setup-hash-table hash)
      (maphash (lambda (key item) key (push item items)) hash)
      (set-difference items
		      (iterate (for (nil item) in-hashtable hash)
			       (collect item))))
  nil)

(deftest in-hashtable.1
    (let* ((hash (make-hash-table))
	   (all-entries (progn (setup-hash-table hash) '()))
	   (generated-entries
	    (iterate (as (key item) in-hashtable hash)
		     (collect (cons key item)))))
      (maphash #'(lambda (key value) (push (cons key value) all-entries)) hash)
      (= (length all-entries)
	 (length generated-entries)
	 (length (union all-entries generated-entries
			:key #'car :test (hash-table-test hash)))))
  t)

(deftest in-hashtable.destructuring.1
    (let ((hash (make-hash-table :test #'equal))
	  (entries '(((a . b) . (1 . 2)) (("c" . 3) . (6 . "7")))))
      (iterate (for (k . v) in entries)
	       (setf (gethash k hash) v))
      (sort
       (iterate (for ((nil . k2) (v1 . v2)) in-hashtable hash)
		(always (numberp v1))
		(while k2)
		(collect (cons v1 k2) into vals)
		(finally (return vals)))
       #'< :key #'car))
  ((1 . b) (6 . 3)))

(deftest in-package.internals
    (let ((syms nil)
	  (iter-syms (iterate (for sym in-package *package* :external-only nil)
			      (collect sym))))
      (do-symbols (sym *package* nil)
	(push sym syms))
      (list
       (set-difference syms iter-syms :test #'eq)
       (set-difference iter-syms syms)))
  (()()))

(deftest in-package.externals.1
    (let ((syms nil)
	  (iter-syms (iterate (for sym in-package '#:cl-user external-only t)
			      (collect sym))))
      (do-external-symbols (sym '#:cl-user nil)
	(push sym syms))
      (list
       (set-difference syms iter-syms :test #'eq)
       (set-difference iter-syms syms)))
  (()()))

(deftest in-package.externals.2
    (let ((sym-count 0))
      (do-external-symbols (sym '#:iterate) (declare (ignore sym))
	(incf sym-count))
      (= sym-count
	 (iterate (for () in-package '#:iterate external-only t)
		  (count 1))))
  t)

(deftest in-package.generator
    (let ((syms nil)
	  (iter-syms (iterate (generate sym in-package *package*)
			      (collect (next sym)))))
      (do-symbols (sym *package*)
	(push sym syms))
      (list
       (set-difference syms iter-syms :test #'eq)
       (set-difference iter-syms syms)))
  (()()))

(deftest in-packages.external
    (let ((syms nil)
	  (iter-syms (iterate (as (sym access package) in-packages '(#:cl-user)
				   :having-access (:external))
			      (collect sym))))
      (do-external-symbols (sym '#:cl-user nil)
	(push sym syms))
      (list
       (set-difference syms iter-syms :test #'eq)
       (set-difference iter-syms syms)))
  (()()))

(deftest in-packages.generator-access
    (let ((iter-syms (iterate (generate (sym access) in-packages (list (find-package "COMMON-LISP")))
			      (repeat 1)
			      (next sym)
			      (collect (list sym access)))))
      (equal (multiple-value-list
	      (find-symbol (symbol-name (caar iter-syms)) "COMMON-LISP"))
	     (car iter-syms)))
  t)

(deftest in-stream.1
    (iter (as x in-stream (make-string-input-stream "#xa()2"))
	  (collect x))
  (10 () 2))

(deftest in-stream.previous
    (iter (for x in-stream (make-string-input-stream "#xa()2"))
	  (as p previous x :initially 1)
	  (collect p))
  (1 10 ()))

(deftest in-stream.2
    ;; This fails in cmucl, sbcl and gcl, because string-input-streams
    ;; are always open, even after close.
    (let ((s (make-string-input-stream "(")))
      (ignore-errors
       (iter (for x in-stream s :using #'read)))
      (open-stream-p s))
  nil)

(deftest in-stream.3
    (iter (for c in-stream (make-string-input-stream "235")
	       :using #'read-char)
	  (accumulating (digit-char-p c) by #'+ initial-value 0))
  10)

(deftest in-stream.reducing
    (iter (with s = (make-string-input-stream "(+ 2)(+ 10)(- 5)(+ 6)"))
	  (for (op num) in-stream s)
	  (reducing num :by op :initial-value 0))
  13)

(deftest in-stream.accumulating
    (iter (with s = (make-string-input-stream "(+ 2)(+ 10)(- 5)(+ 6)"))
	  (for (op num) in-stream s)
	  (accumulating num :by op :initial-value 0))
  -1) ; (6 + (5 - (10 + (2 + 0))))

(deftest in-stream.generate
    (iter (with s = (make-string-input-stream "2 + 10 - 5 + 6"))
      (with start = (read s))
      (generate e in-stream s using #'read)
      (as op = (next e))
      (for arg = (next e))
      (reducing arg by op initial-value start))
  13)

(deftest reducing.0
    (iter (with expr = '(2 + 10 - 5 + 6))
	  (with start = (pop expr))
	  (for (op arg) on expr by #'cddr)
	  (reducing arg by op initial-value start))
  13)

(deftest until.1
    (iter (with rest = 235) (with digit = 0)
	  (multiple-value-setq (rest digit) (floor rest 10))
	  (sum digit into sum)
	  (multiplying digit into product)
	  (until (zerop rest))
	  (finally (return (values sum product))))
  10 30)
	  
(deftest until.2
    (iter (for i in-sequence '#(1 2 -3 6))
	  (until (zerop (sum i into x)))
	  (multiplying i))
  2)

(deftest while.1
    (iter (for i in-sequence '#(1 2 -3 6))
	  (while (< (length (collect i)) 2)))
  (1 2))

;;; tests for my examples:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *an-alist* '((a . 2) (b . 3) (zero . 10) (c . 4) (one . 20) (d . 5) (e . 99)))
  (defparameter *list-of-lists* (loop for i from 0 to 100
				      collect (loop for len from 0 to (random 20)
						    collect len)))
  (defun longest-list (list1 list2)
    (if (< (length list2) (length list1))
	list1
	list2)))

(deftest collect.1
    (iterate (as (key . item) in *an-alist*)
	     (collect key into keys)
	     (collect item into items)
	     (finally (return (values keys items))))
  #.(loop for (key . nil) in *an-alist*
        collect key)
  #.(loop for (key . item) in *an-alist*
        collect item))

(deftest generate.1
    (iterate (generate i from 0 to 6)
	     (for (key . value) in *an-alist*)
	     (when (>= value 10)
	       (collect (cons key (next i)))))
  #.(loop with counter = 0
        for (key . value) in *an-alist*
        when (>= value 10)
           collect (cons key (prog1 counter (incf counter)))))

(deftest find-longest-list.1
    (iterate (for elt in *list-of-lists*)
	     (finding elt maximizing (length elt)))
  #.(reduce #'longest-list *list-of-lists*))

(deftest find-longest-list.2
    (iterate (for elt in *list-of-lists*)
	     (finding elt maximizing (length elt) into (e m))
	     (finally (return m)))
  #.(reduce #'max *list-of-lists* :key #'length))

(deftest find-longest-list.3
    (iterate (for elt in *list-of-lists*)
	     (finding elt maximizing #'length))
  #.(reduce #'longest-list *list-of-lists*))

(deftest find-longest-list.4
    (iterate (for elt in *list-of-lists*)
	     (finding elt maximizing #'length into (e m))
	     (finally (return m)))
  #.(reduce #'max *list-of-lists* :key #'length))

(deftest maximize.1
    (iterate (for elt in *list-of-lists*)
	     (maximizing (length elt) into m)
	     (finally (return m)))
  #.(reduce #'max *list-of-lists* :key #'length))

(deftest maximize.2
    (iterate (for elt in *list-of-lists*)
	     (maximize (length elt)))
  #.(reduce #'max *list-of-lists* :key #'length))

(deftest finding.minimizing.1
    (iterate (for elt in *list-of-lists*)
	     (finding elt minimizing #'length into (e m))
	     (finally (return m)))
  #.(reduce #'min *list-of-lists* :key #'length))

(deftest minimize.1
    (iterate (for elt in *list-of-lists*)
	     (minimizing (length elt) into m)
	     (finally (return m)))
  #.(reduce #'min *list-of-lists* :key #'length))

(deftest minimize.2
    (iterate (for elt in *list-of-lists*)
	     (minimize (length elt)))
  #.(reduce #'min *list-of-lists* :key #'length))

(deftest subblocks.maximize.1
    (iter outer (for elt in *list-of-lists*)
	  (iterate running (for e in elt)
		   (in outer (maximize e)))
	  (maximizing (length elt)))
  #.(reduce #'max *list-of-lists* :key #'length))

(deftest subblocks.minimize.1
    (iter outer (for elt in *list-of-lists*)
	  (minimizing (length elt))
	  (iterate running (for e in elt)
		   (in outer (minimize e))))
  0)

(deftest maximize.3
    (iterate (for i in-vector '#(-3))
	     (maximize i))
  -3)

(deftest minimize.3
    (iterate (as i in-vector '#(3))
	     (minimize i))
  3)

(deftest maximize.multiple
    (iter (as i from 3 downto -3 by 2) (maximize i)
	  (for j from -1)            (maximizing j))
  3)

(deftest minimize.multiple
    (iter (as i from -3 to 3 by 2) (minimize i into x)
	  (for j downfrom -1)    (minimizing j into x)
	  (finally (return x)))
  -4)

(deftest accumulate.1
    (iter (for c in-string "235")
	  (declare (type character c))
	  (accumulate (digit-char-p c) by '* initial-value 1))
  30)

(deftest accumulate.2
    (iter (for c in-sequence "235")
	  (accumulating (digit-char-p c) by #'* initial-value 1))
  30)

(deftest accumulate.3
    (iter (for c in-sequence "235")
	  (accumulate (digit-char-p c) by 'cons initial-value 1))
  (5 3 2 . 1))

(deftest accumulate.4
    (iter (for c in-vector "235")
	  (accumulating (digit-char-p c) by #'cons))
  (5 3 2))

(deftest accumulate.5
    (iter (repeat 0)
	  (accumulating 1 by #'cons))
  nil)

(deftest accumulate.6
    (iter (repeat 0)
	  (accumulate 1 by #'cons initial-value 2))
  2)

(deftest in-string.downto.1
    (iter (for c in-string "235" downto 1)
	  (accumulate (digit-char-p c) by 'cons))
  (3 5))

(deftest in-sequence.downto.1
    (iter (for c in-sequence "235" downto 1)
	  (accumulate (digit-char-p c) by #'cons))
  (3 5))

(deftest reducing.1
    (iter (for c in-string "235")
	  (reducing (digit-char-p c) by 'list initial-value 1))
  (((1 2) 3) 5))

(deftest reducing.2
    (iter (as x index-of-string "235")
	  (reducing x :by #'list initial-value -1))
  (((-1 0) 1) 2))

(deftest reducing.3
    (iter (repeat 0)
	  (reducing 1 #:by 'cons initial-value -1))
  -1)

(deftest reducing.4
    (iter (for i from 3 to 5)
	  (reducing i by #'- :initial-value '0))
  -12)

(deftest reducing.5
    (iter (for x in-vector #(3))
	  (reducing (cons x x) by #'list))
  (3 . 3))

(deftest reducing.6
    (iter (for x in-vector (vector 3))
	  (reducing (cons x x) by #'list :initial-value nil))
  (nil (3 . 3)))


;; synonyms (e.g. GENERATING, COLLECTING) didn't work

(deftest generate.destructuring.1
    (iter (generate (key . item) in '((a . 1) (b . 2) (c .3)))
	  (collect (next key))
	  (collect (next item)))
  (a 2 c))

(deftest generating.destructuring.1
    (iter (generating (key . item) in '((a . 1) (b . 2) (c .3)))
	  (collect (next key))
	  (collect (next item)))
  (a 2 c))

(deftest for.generate-t.destructuring.1
    (iter (for (key . item) in '((a . 1) (b . 2) (c .3)) :generate t)
	  (collect (next key))
	  (collect (next item)))
  (a 2 c))

(deftest generate.next.1
    (iter (generate c in '(a b c d e f g h i j k l m n o p q))
	  (for s in '(1 1 2 3 1 0 1 0 2 1))
	  (collect (next c s)))
  (a b d g h h i i k l))

(deftest generate.previous.1
    (iter (generate c in '(a b c d e f g h i j k l m n o p q))
	  (for s in '(1 1 2 3 1 0 1 0 2 1))
	  (for x = (next c s))
	  (as y previous x)
	  (collect (list y x)))
  ((nil a) (a b) (b d) (d g) (g h) (h h) (h i) (i i) (i k) (k l)))

(deftest generate.next.2
    (with-output-to-string (*standard-output*)
      (iter (generate i in '(1 2 3 4 5)) (princ (next i 2))))
  "24")

(deftest if.1
    (iter (generate x in-vector '#(t nil nil t))
	  (as i from 0)
	  (if (next x) (collect i)))
  (0 3))

(deftest or.1
    (iter (generate x in '(a nil nil 1))
	  (generate y in-vector '#(2 #\c #\d))
	  (collect (or (next x) (next y))))
  (a 2 #\c 1))

(deftest or.2
    (iter (generate x in '(a nil nil 1 nil))
	  (generate y in-vector '#(2 nil #\c #\d))
	  (collect (or (next x) (next y) 3)))
  (a 2 3 1 #\c))

(deftest setf.1
    (iter (generate i from 0 to 3)
	  (with v = (vector 'a 'b 'c 'd))
	  (setf (aref v (next i)) i)
	  (finally (return v)))
  #(0 1 2 3))

(deftest setf.2
    ;; These setf tests fail in CormanLisp 2.0 because ccl does
    ;; not respect setf evaluation order rules.
    (iter (generate i from 0 to 3)
	  (with v = (vector 'a 'b 'c 'd))
	  (setf (aref v (next i)) (next i))
	  (finally (return v)))
  #(1 b 3 d))

(deftest setf.3
    (iter (generate i in '(0 1 2 3 4 5))
	  (with v = (vector 'a 'b 'c 'd))
	  (setf (aref v (next i)) (next i 2))
	  (finally (return v)))
  #(2 b c 5))

(deftest setf.4
    (iter (generate i from 0 to 3)
	  (with v = (vector 'a 'b 'c 'd))
	  (setf (apply #'aref v (list (next i))) (next i))
	  (finally (return v)))
  #(1 b 3 d))

(deftest after-each.1
    (iter (after-each (collecting 0))
	  (generate i in '(a b c))
	  (adjoining (next i)))
  (a 0 b 0 c 0))

(deftest after-each.2
    (iter (with i = 0)
	  (while (< i 4))
	  (after-each (incf i)) ; the C programmer's for (;;) loop
	  (collect i))
  (0 1 2 3))

(deftest after-each.3
    (iter (with i = 0)
	  (while (< i 4))
	  (collect i)
	  (after-each (incf i)))
  (0 1 2 3))

(deftest next-iteration.1
    (iter (for i below 10)
	  (when (oddp i) (next-iteration))
	  (count t))
  5)

(deftest next-iteration.2
    (iter (for thing in '(var &optional else &key (test #'eql)))
	  (collect
	   (cond ((consp thing) (first thing))
		 ((not (member thing lambda-list-keywords)) thing)
		 (t (next-iteration)))))
  (var else test))

;;;; tests from the documentation:

(deftest collect.2
    (iter (for i from 1 to 10)
	  (collect i))
  (1 2 3 4 5 6 7 8 9 10))

(deftest for-in.2
    (iter (for el in '(1 2 3 4 5 6 f 7 8 9 a 10))
	  (if (and (numberp el) (oddp el))
	      (collect el)))
  (1 3 5 7 9))

(deftest for.destructuring.1
    (iter (for (key . item) in '((a . 10) (b . 20) (c . 30)))
	  (for i from 0)
	  (declare (fixnum i))
	  (collect (cons i key)))
  ((0 . a) (1 . b) (2 . c)))

(deftest repeat.0
    (with-output-to-string (*standard-output*)
      (iter (repeat 100)
	    (print "I will not talk in class.")))
  #.(with-output-to-string (*standard-output*)
      (dotimes (i 100)
	(declare (ignorable i)) ; cmucl/sbcl complain about (ignore i)
	(print "I will not talk in class."))))

;;; for.next.1 and for.do-next.1 used to be broken in older versions;
;;; they didn't WALK their NEXT args.
(deftest for.next.1
    (iterate (initially (setq i 0))
	     (for i next (if (> i 10) (terminate) (1+ i)))
	     (finally (return i)))
  11)

;;; This gave STYLE-WARNINGS for undefined i in old versions.
(deftest for.do-next.1
    (iterate (initially (setq i 0))
	     (as i do-next (if (> i 10) (terminate) (incf i)))
	     (finally (return i)))
  11)

(deftest for.do-next.2
    ;; INITIALLY not needed because 0 is inferred from type declaration
    (iterate (for i do-next (if (> i 7) (terminate) (incf i)))
	     (declare (type fixnum i))
	     (finally (return i)))
  8)

(deftest for.do-next.3
    (iter (for a from 1 to 3)
	  (for b = (1+ (* a a)))
	  ;; (values ...) is supported, even though (x y) would do
	  (for (values x y) do-next (dsetq (values x y) (floor b a)))
	  (collect x) (collect y))
  (2 0 2 1 3 1))

(deftest for.next.walk
    (iter (repeat 2)
	  (for x next (progn (after-each (collect 1)) 2))
	  (collect x))
  (2 1 2 1))

(deftest for.do-next.walk
    (iter (repeat 2)
	  (for x do-next (progn (after-each (collect 1)) (dsetq x 2)))
	  (collect x))
  (2 1 2 1))

(deftest for.next.previous
    (iter (for i from 2 to 4)
	  (for x next (progn (after-each (collect i)) (- i)))
	  (for z previous x initially 0)
	  (nconcing (list z x)))
  (0 -2 2 -2 -3 3 -3 -4 4))

(deftest for.do-next.previous
    (iter (for i from 2 to 4)
	  (for x do-next (progn (setq x (- i)) (after-each (collect i))))
	  (for z previous x initially 0)
	  (appending (list z x)))
  (0 -2 2 -2 -3 3 -3 -4 4))

(deftest for-nongenerator.1
    (iter (for el in '(a b c d))
	  (generate i upfrom 1)
	  (if el (collect (cons el (next i)))))
  #.(iter (for el in '(a b c d))
	  (for i upfrom 1)
	  (if el (collect (cons el i)))))
  

(deftest for.previous.in
    (iter (for el in '(1 2 3 4))
	  (for pp-el previous el back 2 initially 0)
	  (collect pp-el))
  (0 0 1 2))

(deftest for.previous.type.1
    (iter (for el in '(1 2 3 4))
	  (declare (type integer el))
	  (for pp-el previous el back 2)
	  (collect pp-el))
  (0 0 1 2))

(deftest for.previous.index-of-string.1
    (iter (as x index-of-string "235")
	  (as p previous x :initially 9)
	  (collecting p))
  (9 0 1))

(deftest for.previous.in-string.with-index
    (iter (as x in-string "235" :with-index y)
	  (as p previous y :initially 9)
	  (collecting p))
  (9 0 1))

(deftest for.previous.index-of-vector
    (iter (as x index-of-vector '#(2 3 4 5))
	  (as p previous x :initially 9 back 2)
	  (collecting p))
  (9 9 0 1))

(deftest for.previous.in-vector.with-index
    (iter (as x in-vector '#(2 3 4 5) with-index y)
	  (as p previous y :initially 9 back 2)
	  (collecting p))
  (9 9 0 1))

(deftest for.first.1
    (iter (for num in '(20 19 18 17 16))
	  (for i first num then (1+ i))
	  (collect i))
  (20 21 22 23 24))

(deftest for.initially.1
    (iter (with (v z))
	  (for i initially (length v) then (1+ i))
	  (collect (cons i z))
	  (while (evenp i)))
  ((0) (1)))

(deftest sum.1
    (iter (for el in '(100 200 300))
	  (sum el into x)
	  (declare (fixnum x))
	  (finally (return x)))
  600)

(deftest collect.3
    (iter (for i from 1 to 5)
	  (collect i))
  (1 2 3 4 5))

(deftest collect.4
    (iter (for i from 1 to 5)
	  (collect i at beginning))
  (5 4 3 2 1))

(deftest collect.5
    (iter (for i from 1 to 4)
	  (collect i at :end))
  (1 2 3 4))

(deftest collect.6
    (iter (for i from 1 to 3)
	  (collect i :at start))
  (3 2 1))

(deftest collect-by.1
    (iter (for i downfrom 10 by 2) (repeat 3)
	  (collect i))
  (10 8 6))

(deftest in-vector.by.1
    (iter (for i in-vector '#(0 1 2 3 4) by 2)
	  (collect i))
  (0 2 4))

(deftest index-of-vector.by.1
    (iter (for i index-of-vector '#(0 1 2 3 4) by 2)
	  (collect i))
  (0 2 4))

(deftest in-vector.downto.1
    (iter (for i in-vector '#(0 1 2 3 4) downto 0)
	  (collect i))
  (4 3 2 1 0))

(deftest index-of-vector.downto.1
    (iter (for i index-of-vector #(0 1 2 3 4) downto 0)
	  (collect i))
  (4 3 2 1 0))

(deftest in-vector.downto.2
    (iter (for i in-vector '#(0 1 2 3 4) downto 0 by 2)
	  (collect i))
  (4 2 0)) ; erroneously got (3 1) in some past

(deftest index-of-vector.downto.2
    (iter (for i index-of-vector #(0 1 2 3 4) downto 0 by 2)
	  (collect i))
  (4 2 0))

(deftest generate.in-vector.downto.1
    (iter (generate i in-vector #(0 1 2 3 4) downto 0 by 2)
	  (collect (next i)))
  (4 2 0))

(deftest generate.index-of-vector.downto.1
    (iter (generate i index-of-vector '#(0 1 2 3 4) downto 0 by 2)
	  (collect (next i)))
  (4 2 0))

(deftest if-first-time.1
    (with-output-to-string (*standard-output*)
      (iter (for i from 200 to 203)
	    (if-first-time (format t "honka"))))
  "honka")

(deftest if-first-time.2
    (with-output-to-string (*standard-output*)
      (iter (for i from 200 to 204)
	    (if (oddp i) (if-first-time (princ "honka") (princ "tah")))))
  "honkatah")

(deftest if-first-time.3
    (iter (for i to 5)
	  (when (oddp i)
	    (if-first-time nil (collect -1))
	    (collect i)))
  (1 -1 3 -1 5))

(deftest first-time-p.1
    (iter (for i to 5)
          (if (first-time-p) (collect -1))
          (if (first-time-p) (collect -2))
	  (when (oddp i)
	    (if (first-time-p) nil (collect -1))
	    (collect i)))
  (-1 -2 1 -1 3 -1 5))

(deftest first-iteration-p.1
    (iter (for i to 5)
          (if (first-iteration-p) (collect -1))
          (if (first-iteration-p) (collect -2))
	  (when (oddp i)
	    (if (first-iteration-p) nil (collect -1))
	    (collect i)))
  (-1 -2 -1 1 -1 3 -1 5))

(deftest collect.multiple.1
    (iter (for i from 1 to 10)
	  (collect i into nums)
	  (collect (sqrt i) into nums)
	  (finally (return nums)))
  #.(loop for i from 1 to 10
	  collect i
	  collect (sqrt i)))

(deftest collect.type.string.1
    (locally (declare (optimize safety (debug 2) (speed 0) (space 1)))
      (iter (declare (iterate:declare-variables))
	    (for s in-vector '#(\a |b| |cD|))
	    (collect (char (symbol-name s) 0) :result-type string)))
  "abc")

(deftest collect.type.string.2
    (iter (for c in-stream (make-string-input-stream "aBc") :using #'read-char)
	  (when (digit-char-p c 16)
	    (collect c :result-type string)))
  "aBc")

(deftest collect.type.string.3
    (iter (for c in-string "235" downfrom 1)
	  (collect c into s result-type string)
	  (finally (return s)))
  "32")

(deftest collect.type.vector.1
    (locally (declare (optimize safety (debug 2) (speed 0) (space 1)))
      (iter (declare (iterate:declare-variables))
	    (for s in-vector '#(\a |b| |cD|))
	    (collect (char (symbol-name s) 0) :result-type vector)))
  #(#\a #\b #\c))

(deftest collect.type.vector.2
    (iter (for c in-vector "235" downfrom 1)
	  (collect (digit-char-p c) :into v :result-type vector)
	  (finally (return v)))
  #(3 2))

(deftest adjoin.1
    (iter (for i in '("ab" "aB" "abc" "AB"))
	  (adjoining i at :start :test #'string-equal))
  ("abc" "ab"))

(deftest adjoin.2
    (iter (for i in '("ab" "aB" "abc" "AB"))
	  (adjoining i at :start))
  ("AB" "abc" "aB" "ab"))

(deftest adjoin.3
    (iter (for i in '("ab" "aB" "abc" "AB"))
	  (adjoining i :at end #:test #'string-equal))
  ("ab" "abc"))

(deftest adjoin.4
    (iter (for i in '("ab" "aB" "abc" "AB"))
	  (adjoining i at :end))
  ("ab" "aB" "abc" "AB"))

(deftest adjoin.5
    (iter (for i in '("ab" "aB" "abc" "AB"))
	  (adjoining (string-downcase i) at :start :test #'string-equal))
  ("abc" "ab"))

(deftest adjoin.6
    (iter (for i in '("ab" "aB" "abc" "AB"))
	  (adjoining (string-upcase i) #:at :end test #'string=))
  ("AB" "ABC"))

(deftest append.1
    (iter (for l on '(1 2 3))
	  (appending l at :start))
  (3 2 3 1 2 3))

(deftest nconc.1
    (iter (for l on (list 1 2 3))
	  (nconcing (copy-list l) at :beginning))
  (3 2 3 1 2 3))

(deftest append.2
    (iter (for l on '(1 2 3))
	  (appending l :at #:end))
  (1 2 3 2 3 3))

(deftest nconc.2
    (iter (for l on (list 1 2 3))
	  (nconcing (copy-list l) at end))
  (1 2 3 2 3 3))

(deftest append.3
    (iter (for l on '(1 2 3))
	  (appending l into x) (finally (return x)))
  (1 2 3 2 3 3))

(deftest nconc.3
    (iter (for l on (list 1 2 3))
	  (nconcing (copy-list l) into x) (finally (return x)))
  (1 2 3 2 3 3))

(deftest append.4
    (iter (for l on '(1 2 3))
	  (appending l into x))
  nil)

(deftest nconc.4
    (iter (for l on (list 1 2 3))
	  (nconcing (copy-list l) into x))
  nil)

(deftest append.5
    (iter (for l on '(1 2 3))
	  (appending l :at #:end)
	  (collect (first l)))
  (1 2 3 1 2 3 2 3 3))

(deftest append.6
    (iter (for l on '(1 2 3))
	  (appending l :at :end)
	  (collect l))
  (1 2 3 (1 2 3) 2 3 (2 3) 3 (3)))

(deftest nconc.5
    (iter (for l on (list 1 2 3))
	  (collect (first l))
	  (nconcing (copy-list l) at end))
  (1 1 2 3 2 2 3 3 3))

(deftest union.1
    (iter (for l on '(a b c))
	  (unioning l)
	  (collect (first l)))
  (a b c a b c))

(deftest union.2
    (iter (for l on '(a b c))
	  (collecting (first l))
	  (unioning l :test #'eql))
  (a b c b c))

(deftest union.3
    (iter (for l in-vector '#("a" "A" "aB" "ab" "AB"))
	  (unioning (list l) :test #'string-equal))
  ("a" "aB"))

(deftest nunion.3
    (iter (for l in-vector '#("a" "A" "aB" "ab" "AB"))
	  (nunioning (list l) :test #'string-equal :at :start))
  ("aB" "a"))

(deftest value.minimize
    (iter (for i from 4 downto -3 by 3)
	  (collect (minimize (* i i) into foo)))
  (16 1 1))

(deftest value.maximize
    (iter (for i from 3 to 5)
	  (sum (maximize (- i 2) into foo)))
  6)

(deftest value.finding-maximizing.1
    (iter (for i from 3 to 6)
	  (adjoining (finding (* i i) maximizing #'integer-length
			      :into foo) :test #'=))
  (9 16 36))

(deftest value.finding-maximizing.2
    (iter (for i from 3 to 6)
	  (adjoining (finding (* i i) maximizing (integer-length i)
			      :into foo) :test #'=))
  (9 16))

(deftest walk.counting
    (iter (for i from 3 to 5)
	  (counting (if-first-time nil t)))
  2)

(deftest value.counting
    (iter (for x in-sequence '#(nil t nil t))
	  (collect (counting x into foo)))
  (0 1 1 2))

(deftest value.adjoining
    (iter (for i from 3 to 5)
	  (sum (length (adjoining i into foo))))
  6)

(deftest value.collecting
    (iter (for i from 3 to 5)
	  (collect (copy-list (collecting i :into foo at #:start))
		   :at end))
  ((3) (4 3) (5 4 3)))

(deftest value.accumulate
    (iter (for c in-string "245")
	  (collect (accumulate (digit-char-p c) by #'+
			       :initial-value 0 into s) into l)
	  (finally (return (cons s l))))
  (11 2 6 11))

(deftest value.always
    (iter (for i from -3 downto -6 by 2)
	  (summing (always i) into x)
	  (finally (return x)))
  -8)

(deftest walk.multiple-value-bind
    (string-upcase
     (iter (for name in-vector (vector 'iterate "FoOBaRzOt" '#:repeat))
	   (multiple-value-bind (sym access)
	       (find-symbol (string name) #.*package*)
	     (declare (type symbol sym))
	     (collect (if access (char (symbol-name sym) 0) #\-)
		      result-type string))))
  "I-R")

(deftest subblocks.1
    (iter fred
	  (for i from 1 to 10)
	  (iter barney
		(for j from i to 10)
		(if (> (* i j) 17)
		    (return-from fred j))))
  9)

(deftest subblocks.wrong.1
    (let ((ar #2a((1 2 3)
	      (4 5 6)
	      (7 8 9))))
      (iter (for i below (array-dimension ar 0))
	    (iter (for j below (array-dimension ar 1))
		  (collect (aref ar i j)))))
  nil)
  
(deftest subblocks.2
    (let ((ar #2a((1 2 3)
		  (4 5 6)
		  (7 8 9))))
      (iter outer (for i below (array-dimension ar 0))
	    (iter (for j below (array-dimension ar 1))
		  (in outer (collect (aref ar i j))))))
  (1 2 3 4 5 6 7 8 9))

(deftest destructuring.1
    (iter (for (values (a . b) c)
	       = (funcall #'(lambda () (values (cons 1 'b) 2))))
	  (leave (list a b c)))
  (1 b 2))

(deftest leave
    (iter (for x in '(1 2 3))
	  (if (evenp x) (leave x))
	  (finally (error "not found")))
  2)

(deftest lambda
    (iter (for i index-of-sequence "ab")
	  (collecting ((lambda(n) (cons 1 n)) i)))
  ((1 . 0) (1 . 1)))

(deftest type.1
    (iter (for el in '(1 2 3 4 5))
	  (declare (fixnum el))
	  (counting (oddp el)))
  3)

(deftest type.2
    (iter (for (the fixnum el) in '(1 2 3 4 5))
	  (counting (oddp el)))
  3)

(deftest type.3
    (iter (declare (iterate:declare-variables))
	  (for el in '(1 2 3 4 5))
	  (count (oddp el) into my-result)
	  (declare (integer my-result))
	  (finally (return my-result)))
  3)

(deftest type.4
    (iter (declare (iterate:declare-variables))
	  (for i from 1 to 10)
	  (collect i))
  (1 2 3 4 5 6 7 8 9 10))

(deftest type.5
    (iter (declare (iterate:declare-variables))
	  (repeat 0)
	  (minimize (the fixnum '1)))
  0)

(deftest type.6
    (iter (declare (iterate:declare-variables))
	  (repeat 0)
	  (maximize 1))
  0)

(deftest type.7
    (iter (declare (iterate:declare-variables))
	  (repeat 0)
	  (minimize (the double-float '1.0d0)))
  0.0d0)

(deftest static.error.1
    (values
     (ignore-errors ; Iterate complains multiple values make no sense here
      (macroexpand-1 '(iter (for (values a b) in '(1 2 3)))) t))
  nil)

(deftest code-movement.1
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((x 3))
			   (initially (setq x 4))
			   (return x))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  t)

(deftest code-movement.2
        (handler-case (macroexpand '
		       (iter (for i from 1 to 10)
			     (let ((x 3))
			       (collect i into x))))
	  (error () t)
	  (:no-error (f x) (declare (ignore f x)) nil))
  t)

(deftest code-movement.3
    (iter (with x = 3)
	  (for el in '(0 1 2 3))
	  (setq x 1)
	  (reducing el by #'+ initial-value x))
  9 ; not 7
  )

(deftest code-movement.else
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((x 3))
			   (else (return x)))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  t)

(deftest code-movement.after-each
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((y i))
			   (after-each (princ y)))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  t)

(deftest code-movement.declare.1
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((y i))
			   (declare (optimize safety))
			   (after-each (princ y)))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  t)

(deftest code-movement.declare.2
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((safety i))
			   (after-each
			     (let ()
			       (declare (optimize safety))
			       (princ i))))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  nil)

(deftest code-movement.locally.1
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((y i))
			   (else (locally (princ y))))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  t)

(deftest code-movement.locally.2
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((y i))
			   (else (locally (princ i))))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  nil)

(deftest code-movement.initially
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((y i))
			   (initially (princ y)))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  t)

(deftest code-movement.finally
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((y i))
			   (finally (return y)))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  t)

(deftest code-movement.finally-protected
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (let ((y i))
			   (finally-protected (return y)))))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  t)

(deftest static.conflict.1
    (handler-case (macroexpand '
		   (iter (for i from 1 to 10)
			 (collect i) (sum i)))
      (error () t)
      (:no-error (f x) (declare (ignore f x)) nil))
  t)

;;; 2005: I'm considering making this shadowing feature unspecified (and
;;; removing the test), because it takes away implementation freedom of
;;; choosing to reimplement Iterate's own clauses via macrolet or defmacro.
(deftest macro.shadow.clause
    (macrolet ((multiply (expr)
		 `(reducing ,expr :by #'+ :initial-value 0)))
      (iter (for el in '(1 2 3 4))
	    (multiply el)))
  10)

(deftest multiply.1
    (iter (for el in '(1 2 3 4))
	  (multiply el))
  24)

(defmacro sum-of-squares (expr)
  (let ((temp (gensym)))
    `(let ((,temp ,expr))
       (sum (* ,temp ,temp)))))

(deftest sum-of-squares.1
    (iter (for el in '(1 2 3))
	  (sum-of-squares el))
  14)

(deftest defmacro-clause
    (defmacro-clause (multiply.clause expr &optional INTO var)
	"from testsuite"
      `(reducing ,expr by #'* into ,var initial-value 1))
  ;; A better return value would be the exact list usable with remove-clause
  ;; The next version shall do that
  (multiply.clause expr &optional INTO var))

(deftest multiply.clause
    (iter (for el in '(1 2 3 4))
	  (multiply.clause el))
  24)

(deftest remove-clause.1
    (iter::remove-clause '(multiply.clause &optional INTO))
  t)

(deftest remove-clause.2
    (values
     (ignore-errors
      (iter::remove-clause '(multiply.clause &optional INTO))))
  nil)

(iter:defmacro-clause (for var IN-WHOLE-VECTOR.clause v)
  "All the elements of a vector (disregards fill-pointer)"
  (let ((vect (gensym "VECTOR"))
        (index (gensym "INDEX")))
    `(progn
       (with ,vect = ,v)
       (for ,index from 0 below (array-dimension ,vect 0))
       (for ,var = (aref ,vect ,index)))))

(deftest in-whole-vector.clause
    (iter (for i IN-WHOLE-VECTOR.clause (make-array 3 :fill-pointer 2
					 :initial-contents '(1 2 3)))
	  (collect i))
  (1 2 3))

(deftest in-vector.fill-pointer
    (iter (for i in-vector (make-array 3 :fill-pointer 2
			    :initial-contents '(1 2 3)))
	  (collect i))
  (1 2))

(iter:defmacro-driver (for var IN-WHOLE-VECTOR v)
  "All the elements of a vector (disregards fill-pointer)"
   (let ((vect (gensym "VECTOR"))
         (end (gensym "END"))
         (index (gensym "INDEX"))
         (kwd (if iter:generate 'generate 'for)))
     `(progn
        (with ,vect = ,v)
        (with ,end = (array-dimension ,vect 0))
        (with ,index = -1)
        (,kwd ,var next (progn (incf ,index)
                               (if (>= ,index ,end) (terminate))
                               (aref ,vect ,index))))))

(deftest in-whole-vector.driver
    (iter (for i IN-WHOLE-VECTOR (make-array '(3) :fill-pointer 2
					     :initial-contents '(1 2 3)))
	  (collect i))
  (1 2 3))

(deftest in-whole-vector.generate
    (iter (generating i IN-WHOLE-VECTOR (make-array '(3) :fill-pointer 2
						    :initial-contents '(1 2 3)))
	  (collect (next i)))
  (1 2 3))

(deftest defclause-sequence
    (progn
      (iter:defclause-sequence IN-WHOLE-VECTOR.seq INDEX-OF-WHOLE-VECTOR
	:access-fn 'aref
	:size-fn '#'(lambda (v) (array-dimension v 0))
	:sequence-type 'vector
	:element-type t
	:element-doc-string 
	  "Elements of a vector, disregarding fill-pointer"
	:index-doc-string 
	  "Indices of vector, disregarding fill-pointer")
      t)
  t)

(deftest in-whole-vector.seq
    (iter (for i IN-WHOLE-VECTOR.seq (make-array '(3) :fill-pointer 2
						 :initial-contents '(1 2 3)))
	  (collect i))
  (1 2 3))

(deftest in-whole-vector.seq.index
    (iter (for i INDEX-OF-WHOLE-VECTOR
	       (make-array 3 :fill-pointer 2 :initial-contents '(1 2 3)))
	  (for j previous i :initially 9)
	  (collect (list j i)))
  ((9 0)(0 1)(1 2)))

(deftest in-whole-vector.seq.with-index
    (iter (for e IN-WHOLE-VECTOR.seq
	       (make-array '(3) :fill-pointer 2 :initial-contents '(a b c))
	       :with-index i)
	  (for j previous i :initially 9)
	  (collect (list j i e)))
  ((9 0 a)(0 1 b)(1 2 c)))

(deftest in-whole-vector.seq.generate
    (iter (generate e IN-WHOLE-VECTOR.seq
	       (make-array 3 :fill-pointer 2 :initial-contents '(a b c))
	       :with-index i)
	  (collect (list (next e) e i)))
  ((a a 0) (b b 1) (c c 2)))

(deftest display.1
    (let ((*standard-output* (make-broadcast-stream)))
      (display-iterate-clauses) t)
  t)

(deftest display.2
    (let ((*standard-output* (make-broadcast-stream)))
      (display-iterate-clauses 'for) t)
  t)

(deftest multiple-value-prog1.1
    (iter (for x in '(a b c))
	  (collect (multiple-value-prog1 7)))
  (7 7 7))

(deftest ignore-errors.1
    (iter (for x in '(a b c))
	  (collect (ignore-errors x)))
  (a b c))

(deftest ignore-errors.2
    (iter (generate x in '(a b c))
	  (collect (ignore-errors (next x))))
  (a b c))

(deftest handler-bind.1
    (iter (for i from -1 to 2 by 2)
	  (handler-bind ((error (lambda(c) c nil)))
	    (collect i)))
  (-1 1))

(deftest destructuring-bind.1
    ;; One version of Iterate would enter endless loop in ACL 7 and SBCL
    ;; reported by Julian Stecklina in early 2005
    (null (macroexpand '(iter (for index in '((1 2)))
			 (collect (destructuring-bind (a b) index
				    (+ a b))))))
  nil)

(deftest destructuring-bind.2
    (iter (for index in '((1 2)))
	  (collect (destructuring-bind (a b) index
		     (+ a b))))
  (3))

(deftest symbol-macrolet
    (iter (for i from -1 downto -3)
	  (symbol-macrolet ((x (* i i)))
	    (declare (optimize debug))
	    (sum x)))
  14)

(defclass polar ()
  ((rho   :initarg :mag)
   (theta :initform 0 :accessor angle)))

(deftest with-slots
    (iter (with v = (vector (make-instance 'polar :mag 2)))
	  (for x in-sequence v)
	  (with-slots (rho) x
	    (multiplying rho)))
  2)

(deftest with-accessors
    (iter (with v = (vector (make-instance 'polar :mag 1)))
	  (for x in-sequence v)
	  (with-accessors ((alpha angle)) x
	    (incf alpha 2)
	    (summing alpha)))
  2)

;;; Tests for bugs. 
;; when these start failing, I have done something right (-:

;; The walker ignores function bindings,
;; therefore shadowing is not handled correctly.
(deftest bug/walk.1
    (macrolet ((over(x) `(collect ,x)))
      (iter (for i in '(1 2 3))
	    (flet ((over(x)(declare (ignore x)) (collect 1)))
	      (over i)))) ; would yield (1 1 1) if correct
  (1 2 3))

(deftest bug/macrolet.2
    (progn
      (format *error-output*
	      "~&Note: These tests generate warnings ~
  involving MACROLET within Iterate~%")
      (values
       (ignore-errors ; would yield 1 if correct
	 (iterate (repeat 10)
		  (macrolet ((foo () 1))
		    (multiplying (foo)))))))
  nil)

(deftest macrolet.3
    (iterate (repeat 2)
	     (multiplying (macrolet ((foo () 1))
			    (foo))))
  1)

;; Hashtable iterators are specified to be defined as macrolets.
;; But we handle these by special-casing with-hash-table/package-iterator
(deftest nested-hashtable.1
    (let ((ht1 (make-hash-table))
          (ht2 (make-hash-table)))
      (setup-hash-table ht2)
      (setf (gethash 'a ht1) ht2)
      (= (hash-table-count ht2)
	 (length
	  (iter outer (for (k1 v1) in-hashtable ht1)
		(iter (for (k2 v2) in-hashtable ht2)
		      (in outer (collect k2)))))))
  t)

(deftest nested.in-hashtable.2
    ;; Here the inner macrolet code does not affect the outer iteration
    (let ((ht1 (make-hash-table))
          (ht2 (make-hash-table)))
      (setup-hash-table ht2)
      (setf (gethash 'a ht1) ht2)
      (iter (for (k1 v1) in-hashtable ht1)
	    (counting
	     (iter (for (k2 nil) in-hashtable ht2)
		   (count k2)))))
  1)

(deftest nested.in-hashtable.3
    (let ((ht1 (make-hash-table))
          (ht2 (make-hash-table)))
      (setup-hash-table ht2)
      (setf (gethash 'a ht1) ht2)
      (iter (for (k1 v1) in-hashtable ht1)
	    (progn
	     (iter (for (nil v2) in-hashtable v1)
		   (count v2))
	     (collect k1))))
  (a))

(deftest nested.in-package
  (< 6
     (print
      (iter (for scl in-package '#:iterate :external-only t)
	    (count ; Iterate exports ~50 symbols
	     (iter (for si in-package #.*package*)
		   (thereis (eq si scl))))))
     80)
  t)

(deftest macrolet.loop-finish
    (iter (for l on *an-alist*)
	  (loop for e in l
		when (equal (car e) 'zero)
		do (loop-finish)))
  nil)

;;; arch-tag: "b8b1db2d-313c-11d8-abb9-000c76244c24"
