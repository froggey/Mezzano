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

;;; AXIS LABELING

(defun down (x d)
  "Round x down modulo d."
  (- x (mod x d)))

(defun hypot (x y)
  "Computes the hypotenuse of a right triangle with sides of length X and Y.
   Ie. (sqrt (+ (expt x 2) (expt y 2))) done carefully."
  (when (< x 0.0) (setq x (- x)))
  (when (< y 0.0) (setq y (- y)))
  (when (< x y) (psetq x y y x))
  (cond ((zerop x) 0)
	((zerop y) x)
	(t (* x (sqrt (+ 1.0 (* (setq y (/ y x)) y)))))))

(defun AUTOTICK-internal (xmin xmax div max-ticks &rest choices)
  (let ((range (/ (abs (- xmax xmin)) div))
	(ifac 1.0)
	(tick 1.0))
    ;; Beat range so that 1.0 <= range <= 10.0 and compute ifac
    ;; so that original-range = ifac * range
    (loop while (> range 10.0) do
      (setq ifac (* ifac 10.0))
      (setq range (/ range 10.0)))
    (loop while (<= range 1.0) do
      (setq ifac (/ ifac 10.0))
      (setq range (* range 10.0)))
    (setq tick
	  (loop for c in choices
		when (<= (* range c) max-ticks)
		  do (return (/ 1.0 c))
		finally (return 1.0)))
    (* tick ifac div))) 

(defun TIME-AUTOTICK (xmin xmax)
  (let* ((range (abs (- xmax xmin)))
	 (interval (do ((intervals '#.(list (* 365 86400) (* 7 86400)
					    86400 3600 60 1)
				   (cdr intervals)))
		       ((null intervals) 1)
		     (when (> range (first intervals))
		       (return (if (> range (* 2 (first intervals)))
				   (first intervals)
				   (or (second intervals) 1))))))
	 (tick (values (round (autotick-internal xmin xmax interval 12 4 2)))))
    (if (> interval 3600) tick
	(* (values (ceiling tick interval)) interval))))

(defun auto-tick (min max)
  (let* ((range (- max min))
	 (tick (expt 10 (truncate (log range 10))))
	 (count (/ range tick)))
    (coerce (cond ((<= count 2) (/ tick 5))
		  ((<= count 5) (/ tick 2))
		  (t tick))
	    'single-float)))

(defun make-adjustable-string (&optional (size 7))
  ;; (internal to float-to-string)
  ;; Cons a new string every time; if you reuse an old string, you get graphics
  ;; turds.
  (make-array size
	      :element-type 'character
	      :adjustable t
	      :fill-pointer 0))

(defun float-to-string (number &optional (max-digits *max-digits*) (string nil))
  "Converts a FLOAT into as short a string as possible while providing
   max-digits of accuracy.  The string may take (+ MAX-DIGITS 6)
   characters to print.  If OUTPUT-STRING, a string with a fill pointer
   is provided, it will be filled.  Otherwise a new string will be created."
  ;; Sliders call this within mouse-tracking, so it needs to be fast.
  (or max-digits (setq max-digits *max-digits*))	; user provided nil?
  ;; String is assumed to have a fill pointer.
  (if string
      (setf (fill-pointer string) 0)
      (setq string (make-adjustable-string (+ max-digits (if (minusp number) 2 1)))))
  (if (zerop number)
      ;; Handle zero as a special case.  float-to-string-internal can't deal with it?
      (progn (vector-push-extend #\0 string) string)
      (float-to-string-internal number max-digits string)))

(defun float-to-string-internal (number max-digits string
				 &aux (exponent 0) ilength flength
				      elength exponent-p
				      (extension 5))
  (when (< number 0)	; Sign.
    (vector-push-extend #\- string extension)
    (setq number (abs number)))
  (loop while (>= number 10.0)			
	do (setq number (/ number 10.0))
	   (incf exponent))
  (loop while (< number 1.0)
	do (setq number (* number 10.0))
	   (decf exponent))
  ;; now original (abs number) = number * 10^exponent, 1.0 <= number < 10.0
  (incf number (/ 5.0 (expt 10 max-digits)))	; Round up
  (when (>= number 10.0)	; But not too much.
    (setq number (/ number 10.0))
    (incf exponent))
  (cond ((or (> exponent (1- max-digits))	; E format.
	     (< exponent -3))
	 (setq ilength 1
	       exponent-p t))
	(t (setq ilength	; F format.
		 (if (>= exponent 0) (1+ exponent) 0)
		 exponent-p nil)))
  (macrolet
    ((push-digits (number length string)
       `(dotimes (.i. ,length)
	 (vector-push-extend (digit-char (values (floor ,number))) ,string extension)
	  (setf ,number (mod (* 10.0 ,number) 10.0)))))
    (push-digits number ilength string)	; Integer part.
    (setq flength (- max-digits ilength))	; Fractional part.
    (when (or (> flength 0) exponent-p (< exponent 0))
      (vector-push-extend #\. string extension)
      (when (not exponent-p)
	(loop while (< exponent -1)
	      do (vector-push-extend #\0 string extension)
		 (incf exponent)))
      (when (not (= number 0.0))
	(push-digits number flength string))
      (loop while (char= (char string (decf (fill-pointer string))) #\0)
	    finally (incf (fill-pointer string)))
      (if (char= (char string (1- (fill-pointer string))) #\.)
	  (decf (fill-pointer string))))
    (when exponent-p	; Exponent
      (vector-push-extend #\e string extension)
      (when (< exponent 0)
	(vector-push-extend #\- string extension)
	(setq exponent (abs exponent)))
      (setq elength 1)
      (loop while (>= exponent 10.0)
	    do (setq exponent (/ exponent 10.0))
	       (incf elength))
      (push-digits exponent elength string))
    string))

(defun LINEAR-AXIS
			; Draw a linear axis
       (xmin ymin	; Axis is drawn between (xmin ymin) and (xmax ymax)
	     xmax ymax	; on the x-y window. (in pixels)
	     umin umax	; Axis units corresponding to min and max points
	     dtick	; Tick spacing in axis units.  
	     tick-size	; Length of tick in pixels.  Ticks are draw on the
	                ; left side of axis if tick-size > 0, else right side.
	     tick-numbering ; Should axis numbers be added?  They are placed
			    ; on the side of the axis opposite the ticks.
			    ; Values are NIL, :MINIMAL, or :EACH.
	     draw-line
	     axis-number
	     label)
  (if (< umax umin) (rotatef umax umin))
  (if (minusp dtick) (setq dtick (- dtick)))
  (let* ((cos (- xmax xmin))
	 (sin (- ymax ymin))
	 (l (hypot cos sin))
	 (u-scale (/ l (float (- umax umin))))
	 (ufirst (+ (down umin dtick) dtick)); U value of first tick mark in from left.
	 (smallnum (* dtick .1))
	 (-smallnum (- smallnum))
	 u-tick
	 v-tick)
    (declare (short-float u-scale))
    (setq cos (/ cos l))	; line of axis is x*sin - y*cos + b = 0
    (setq sin (/ sin l))
    (when (and (not (zerop ufirst))
	       (< (/ (abs ufirst) dtick) 0.001))
      ;; UFIRST wants to be 0 but isn't due to roundoff.
      (setq ufirst 0.0))	; Make it 0.
    (cond ((< (abs (- ufirst umin)) (* 0.1 dtick))
	   (setq ufirst (+ ufirst dtick))))
    (setq u-tick (values (truncate (* tick-size (- sin)))))
    (setq v-tick (values (truncate (* tick-size cos))))
    (funcall draw-line xmin ymin xmax ymax)	; Axis line.
    ;; Float these so our declarations are correct.
    (or (floatp ufirst) (setq ufirst (float ufirst)))
    (or (floatp dtick) (setq dtick (float ufirst)))
    (or (floatp umin) (setq umin (float umin)))
    (macrolet
	((x-along (u) ;; x and y point u along line x*sin - y*cos + b = 0
	   `(+ xmin (* cos (values (round (* u-scale
					     (- (the short-float ,u)
						(the short-float umin))))))))
	 (y-along (u) `(+ ymin (* sin (values (round (* u-scale
							(- (the short-float ,u)
							   (the short-float umin))))))))
	 (~= (a b) `(<= -smallnum (- ,a ,b) smallnum)))
      (do* ((u ufirst (+ u dtick))	; Place tick marks
	    (ulast (let ((ulast (down umax dtick)))
		     ;; Tick inside right edge of axis.
		     (if (< (abs (- ulast umax)) (* 0.1 dtick))
			 (- ulast dtick)
		       ulast))))
	  ((> u umax))
	(when (or (and (eq tick-numbering :minimal)
		       (or (~= u ufirst) (~= u ulast)))
		  (and (eq tick-numbering :each) (<= ufirst u ulast)))
	  (funcall axis-number (x-along u) (y-along u) 
		   (if (< (/ (abs u) dtick) 0.001) ; u wants to be 0
		       0
		     u)))
	(let ((x (x-along u))
	      (y (y-along u)))
	  (funcall draw-line x y (+ x u-tick) (+ y v-tick)))))
    (if label (funcall label
		       (values (round (+ xmin xmax) 2))
		       (values (round (+ ymin ymax) 2))))
    ))

