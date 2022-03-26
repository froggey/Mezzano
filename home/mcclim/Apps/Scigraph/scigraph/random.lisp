;;; -*- Syntax: Common-lisp; Package: STATISTICS -*-
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

(in-package :statistics)

;;; PORTABLE UNIFORM AND GAUSSIAN RANDOM NUMBERS

#||
Should work in most Common LISP's.  See description of UNIFORM-BASIC.
This was written before CL to provide a portable random number generator.  If
you want to use the portable uniform-basic function then do (PUSHNEW *FEATURES*
SYSTEM-RANDOM), and you will use the portable uniform random number generator
provided below.

Linus Schrage, A More Portable Fortran Random Number Generator,
  ACM Trans. Math. Soft., V5, No. 2, p 132-138, 1979.

S.K. Park and K.W. Miller, Random number generators:  good ones are hard to find,
  CACM, v31, 10, Oct. 1988, p. 1192 - 1201.

Pierre L'Ecuyer, Efficient and Portable Combined Random Number Generators,
CACM, June 1988, Vol 31, Number 6, p. 742-774.
||#

;;; Better numbers, see Ecuyer, 1988.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *uniform-a* 40692)
  (defconstant *uniform-m* 2147483399))

(defun random-seed ()
  "Return a random seed."
  (make-random-state))		; structure

(defun uniform-basic (previous-fixnum)
  "Returns a random fixnum between 1 and (1- *uniform-m*)."
  #||
  Repeated calls will generate fixnums in the range
  1 -> *uniform-m*
  
  The basic ideas is that a is a primitive root of m, a large prime,
  2 <= a <= m-1.
  f(z) = a*z mod m, 
  The sequence zn = f(n-1), with initial seed, z1 , 1 <= z1 <= m-1 is:

  zn = a^n z1 mod m.

  a^m-1 mod m = 1.  (Fermats' Theorem)

  a is a primitive root iff a^n mod m != 1 for 1 <= n <= m-2.

  If we start with z1 = 1, zn = a^n, zp = 1 for some p, which is the period
  of the random number sequence.  When a is a primitive root of m, p = m-1,
  so p is as large as possible.  This is a good random number generator
  but is not be the fastest!  On most COMMON LISP's it will require bignums
  since (* *uniform-a* previous-fixnum) can have 46 bits in it. 
  ||#
  (mod (* #.*uniform-a* previous-fixnum) #.*uniform-m*))

(defmacro uniform-internal (seed m a)
  "This version does not cons, but fixnums must be >= m, and (< (expt a 2) m)."
  (multiple-value-bind (q r) (truncate m a)
    `(multiple-value-bind (sq sr) 
	 (truncate (the fixnum ,seed) ,q)
       (declare (fixnum sq sr))
       (let ((random (- (the fixnum (* ,a sr)) (the fixnum (* sq ,r)))))
	 (declare (fixnum random))
	 (if (> random 0) random (+ random ,m))))))

(defun combined-uniform (seed-1 seed-2)
  "Returns a uniform random number between 0 and 1.
   It is computed from the difference between 2 uniform random numbers s1 and s2,
   mod (1- m1).  S1, and s2 are linear congruential generators, with the constants
   m1, a1, m2, and a2, provided below.  With these choices the period of the
   generator is (/ (* (1- m1) (1- m2))) or 2.305842648436452E18."
  (setq seed-1 (uniform-internal seed-1 2147483563 40014)
	seed-2 (uniform-internal seed-2 2147483399 40692))
  (let ((random (- seed-1 seed-2)))
    (values (* (if (< random 1) (+ random (1- 2147483563)) random)
	       (/ (coerce (- 2147483563 1) 'single-float)))
	    seed-1 seed-2)))
	
(defun make-uniform-stream (seed-1 seed-2)
  #'(lambda ()
      (let (r)
	(multiple-value-setq (r seed-1 seed-2)
	  (combined-uniform seed-1 seed-2))
	r)))

(defvar *uniform-seed* 63400018)		; 0 < fixnum < (- *uniform-m* 1)

(defmacro with-seed (s-newseed &body body)
  "evaluates body with the seed of the random numbers set to S-NEWSEED.
   the value of S-NEWSEED is updated.  Thus this is a way of
   Keeping several sequences of random numbers with their own seeds."
  `(let ((*random-state* ,s-newseed))
    (prog1 (progn ,@body)
      (setf ,s-newseed *random-state*))))

(defun uniform ()
  "Returns the next uniform random number in the sequence
   To have your own sequence, use the macro WITH-SEED."
  (random *uniform-m* *random-state*))

(defun make-uniform-1-stream (seed)
  ;; Stream of uniform random numbers between 0 and 1
  #'(lambda ()
      (setq seed (uniform-internal seed #.*uniform-m* #.*uniform-a*))
      (* seed (/ (float *uniform-m*)))))

(defun uniform-0-1 ()
  "A uniform random number greater than 0 and less than 1."
  (* (uniform) (/ (float *uniform-m*))))

;;; For speed, you probably want an inline version of uniform-between for your application.
(defun uniform-between (low-num high-num)
  "Returns a uniform random  number, X, LOW-NUM < X < HIGH-NUM
   If low-num and/or high-num are fixnums, a fixnum is returned."
  (if (= low-num high-num)
      low-num
    (+ low-num (random (- high-num low-num)))  ))

(defun gaussian-random-1 ()
  "Returns a gaussian random variable with mean 0.0 and standard deviation 1.0.
   Good tails."
  (* (sqrt (* -2.0 (log (random 1.0))))
     (sin (* #.(* 2.0 (coerce pi 'single-float)) (random 1.0)))))

(defun gaussian-random (mean standard-deviation)
  "Returns a Gaussian random number with mean MEAN and standard deviation
   standard-deviation."
  (+ mean (* (gaussian-random-1) standard-deviation)))

(defun gaussian (x)
  (/ (exp (- (/ (expt x 2) 2.0))) 
     (sqrt #.(* 2.0 (coerce pi 'single-float)))))
     
(defun random-yes-no (fraction-yes)
  "Returns t randomly FRACTION-YES of the time."
  (<= (uniform-0-1) fraction-yes))

(defmacro HORNER (x &rest coefs)
  "Expand polynomials by Horner's rule."
  (check-type x symbol)
  (cond ((null coefs)
	 (error "There must be at least one coefficient."))
	((null (cdr coefs))
	 (first coefs))
	((null (cddr coefs))
	 (let ((multiple (second coefs)))
	   (if (= (abs multiple) 1)
	       (if (minusp multiple)
		   `(- ,(first coefs) ,x)
		 `(+ ,(first coefs) ,x))
	     `(+ ,(first coefs) (* ,x ,multiple)))))
	(t
	 `(+ ,(first coefs) (* ,x (horner ,x . ,(cdr coefs)))))))

(defun erf (x &aux y)
  "(ERF x) => (* (/ 2 (sqrt pi)) (integral from 0 to x of (exp (- (expt z 2)))) dz))
   Approximation good to (abs error) <= 1.5e-7.  Equation 7.1.26 of M. Abramowitz and
   I.A. Segun,\"Handbook of Mathematical Function\", 7th Edition, Dover Publications Inc.,
   New York, 1968."
  (setq x (float x))
  (cond  ((< x 0) (- (erf (- x))))
	 ((> x 9) 1.0)				; Too big for exp.
	 (t (setq y (/ 1.0 (+ 1.0 (*  0.3275911 x))))
	    (coerce (- 1.0 (* (exp (- (expt x 2)))
		      (* y (horner y 0.2548296d0 -0.28449672d0 1.4214138d0
				      -1.4531521d0 1.0614054d0))))
		    'single-float))))

;;;
;;; Numerical Recipes in LISP
;;;


(defmacro memoize-last (args &body body)
  (let ((result (cons nil nil))
	(old-args (make-list (1+ (length args)))))
    (setf (car (last old-args)) result)
  `(let ((.old-args. ',old-args))
     (let ((.p. .old-args.))
       (if (and ,@(map 'list #'(lambda (a) `(equal ,a (pop .p.))) args)
		(not (eq (car .old-args.) (first .p.))))	; Initialized?
	 (first .p.)
	 (let ((.p. .old-args.))
	   ,@(map 'list #'(lambda (a)
			    `(progn (setf (first .p.) ,a) (setq .p. (cdr .p.)))) args)
	   (setf (first .p.) (progn ,@body))))))))

(defmacro memoize (args &body body)
  (let ((body `(multiple-value-bind (.entry. .found.) (gethash .args. .table.)
		 (if .found. (values-list .entry.)
		     (progn (setf (gethash .args. .table.)
				  (setq .entry. (multiple-value-list ,@body)))
			    (values-list .entry.))))))

  `(let ((.table. (load-time-value (make-hash-table :test #'equal))))
     ,(if (cdr args)
	  `(with-stack-list (.args. ,@args) ,body)
	  `(let ((.args. ,(first args))) ,body)))))



;;;
;;; Gamma Distribution p. 206.
;;;

(defun gamma-random (a random)
  "Returns a deviate distributed as a gamma distribution of interger
  order A, i.e. a waiting time to the A'th even in a Poisson process
  of unit mean, using RANDOM as the source of uniform deviates.
  A must be >= 1.  For example, when A = 1, it is just the exponential
  distribution, the waithing time to the first event."
  (if (< a 6)				; Add waiting times directly.
      (let ((x 1.0))
	(loop repeat a doing (setq x (* x (funcall random))))
	(- (log x)))
      (loop
       (let ((v1 (- (* 2.0 (funcall random)) 1.0)) ; Rejection method.
	     (v2 (- (* 2.0 (funcall random)) 1.0)))
	 (if (<= (+ (* v1 v1) (* v2 v2)) 1.0)
	     (let* ((y (/ v2 v1))	; y = tan(pi + random).
		    (am (- a 1.0))
		    (s (sqrt (+ (* 2.0 am) 1.0)))
		    (x (+ (* s y) am)))
	       (if (> x 0.0)
		   (let ((e (* (+ 1.0 (* y y))
			       (exp (- (* am (log (/ x am))) (* s y))))))
		     (if (<= (funcall random) e)
			 (return x))))))))))



;;;
;;; Poisson Deviates p. 207.
;;;

(defun poisson-random (m random)
  "Returns a integer that is a random deviate drawn from a Poisson
   distribution of mean m using RANDOM as the source of uniform random deviates."
  (if (< m 12)
      (let ((em -1)
	    (temp 1.0))		; Multiplying uniform deviates is the
	(loop (setf em (1+ em)	; same as adding expoential ones.
		    temp (* temp (funcall random)))
	      (unless (> temp			; Compare exponential rather than
			 (memoize (m) (exp (- m))))	; computing the log.
		(return)))			
	em)
      (progn				; Use rejection method.
	(multiple-value-bind (sq log-m g)
	    (memoize (m) 
	      (let ((log-m (log m)))
		(values (sqrt (* 2.0 m)) log-m (- (* m log-m (gammln (+ m 1.0)))))))
	  (let (y 
		em)
	    (loop
	      ;; Y is a deviate from a Lorentzian comparison function.
	      (setf y  (tan (* pi (funcall random)))	
		    ;; EM is Y shifted and scaled.
			em (+ (* sq y) m))	
	      (if (>= em 0.0)			; Keep?
		  ;; The trick for integer-valued distributions.
		  (progn
		    (setq em (values (truncate em)))
		    ;; The ratio of the desired distribution to the comparison function.
		    ;; Reject by comparing it to another uniform deviate.
		    ;; The factor 0.9 makes in never exceed 1.0.
		    (if (<= (funcall random) (* 0.9 (+ 1.0 (* y y)) 
					(exp (- (* em log-m) (gammln (+ em 1.0)) g))))
			(return em))))))))))

;;;
;;; 6.1 Gamma Function, Beta Function, Factorials, Binomial Coefficients.
;;; Page 157.


(defun gammln (x)
  "Returns the value of (log (gamma x)) for (> x 0).  Full accuracy is obtained 
   when (> x 1).  For (< 0 X 1), the reflextion formula (6.1.4) can be used first."
  ;; Omit double precision if five figure accuracy is good enough.
  (let* ((x (- x 1.0d0))
	 (tmp (let ((tmp (+ x 5.5d0)))
		(- (* (+ x 0.5d0) (log tmp)) tmp)))
	 (ser 1.0d0))
    (incf ser (/  76.18009173d0   (incf x 1.0d0))) 
    (incf ser (/ -86.50532033d0   (incf x 1.0d0))) 
    (incf ser (/  24.01409822d0   (incf x 1.0d0))) 
    (incf ser (/  -1.231739516d0  (incf x 1.0d0))) 
    (incf ser (/   0.120858003d-2 (incf x 1.0d0)))
    (incf ser (/  -0.536382d-5    (incf x 1.0d0)))
    (+ (log (* 2.50662827465d0 ser)) tmp)))



