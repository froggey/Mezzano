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

#||
;;; Example equations.
(setq e (make-instance 'equation-data :equation '(* (sin (* a x)) (sin (* b x)))
	       :variable 'x :min 0 :max 10 :increment .1 :parameters '((a 2) (b 3))))

(setq f (make-instance 'equation-data :equation '(+ (* a x) (* b (^ x 2)) (* c (^ x 3)))
		       :variable 'x :min 0 :max 10 :increment .1
		       :parameters '((a 1) (b 5) (c -.5))))

||#
(defclass EQUATION-DATA (graph-data)
  ((equation :initform nil :initarg :equation :reader equation)
   (variable :initform 'x :initarg :variable :accessor variable)
   (min :initform 0 :initarg :min)
   (max :initform 100 :initarg :max)
   (increment :initform nil :initarg :increment)
   (parameters :initform nil :initarg :parameters)
   (data-function :initform nil :initarg :data-function))
  (:default-initargs :symbologies '(:line) :line-style 0)
  (:documentation "Equations of a single variable."))

(defmethod make-name :around ((self equation-data))
  (with-slots (name equation) self
    (if (not name)
	(if (not equation) (call-next-method)
	    (setq name (format nil "~a" equation))))))

(defmethod initialize-instance :after ((self equation-data) &key &allow-other-keys)
   (with-slots (equation) self
     (when equation (setf (equation self) equation))))

(defmethod (setf equation) (new-equation (self equation-data))
  (with-slots (equation parameters) self
    (setf equation new-equation)
    (merge-parameters self)
    (setf (name self) (format nil "~a" (subst-param parameters new-equation)))))

(defmethod merge-parameters ((self equation-data))
  "Merge the current parameters with any new parameters."
  (with-slots (parameters equation variable data-function) self
    (let ((found ()))
      (labels ((find-parameters (an-equation a-variable)
		 "Given an EQUATION in some VARIABLE, extract a list of PARAMETERS."
		 (cond ((null an-equation) found)
		       ((symbolp an-equation)
			(if (eq an-equation a-variable) found
			    (if (member an-equation found) found
				(progn (push an-equation found)
				       found))))
		       ((listp an-equation)
			(dolist (arg (cdr an-equation))
			  (find-parameters arg a-variable))
			found))))
	(setq parameters
	      (loop with parm-ass
		    for parm in (find-parameters equation variable)
		    when (setq parm-ass (assoc parm parameters :test #'eq))
		    collect parm-ass
		    else collect (list parm 0))))
      (setq data-function
	    (let ((*terminal-io* *terminal-io*))
	      (compile
	       nil
	       `(lambda (,variable ,@(map 'list #'(lambda (p) (first p)) parameters))
		 ,equation)))))))

(defmethod (setf variable) :after (new (self equation-data))
  (declare (ignore new))
  (merge-parameters self))

(defmethod map-data ((self equation-data) function the-data)
  (declare (ignore the-data))
  (with-slots (parameters increment data-function max min) self
    (let ((p1 (list nil nil))			; Needless consing avoided.
	  (p2 (list nil nil))
	  (args (map 'list #'(lambda (p) (second p)) parameters)))
      (if (null increment) (setq increment (/ (- max min) 100)))
      (loop for x from min to max by increment
	    do (setf (first p1) x
		     (second p1) (apply data-function x args))
	    (funcall function p1)
	    (psetq p1 p2 p2 p1)))))


(defmethod datum-presentation ((self equation-data) datum)
  (multiple-value-bind (x y) (datum-position self datum)
    (list x y)))				; copy equation data

(defun subst-param (alist form &aux car cdr)
    (cond ((symbolp form)
	   (cond ((setq car (assoc form alist :test #'eq))
		  (cadr car))
		 (t form)))
	  ((listp form)
	   (setq car (subst-param alist (car form))
		 cdr (subst-param alist (cdr form)))
	   (cond ((and (eq (car form) car)
		       (eq (cdr form) cdr))
		  form)
		 (t (cons car cdr))))
	  (t form)))


(defclass equation-data-lisp-title-mixin
    ()
  ()
  (:documentation
   "Automatically generate a title which is a lisp expression for
the equation."))

(defmethod title ((self equation-data-lisp-title-mixin))
  (with-slots (parameters equation) self
    (format nil "~a" (subst-param parameters equation))))


(defclass z-transform-data (equation-data)
  ()
  (:default-initargs :variable 'z :min 0 :max 0.5 :increment .01))

(defmethod map-data ((self z-transform-data) function the-data)
  (declare (ignore the-data))
  (with-slots (parameters data-function min max increment)
      self
    (let ((p1 (list nil nil))			; Needless consing avoided.
	  (p2 (list nil nil))
	  (args (map 'list #'(lambda (p) (second p)) parameters)))
      (loop for x from min to max by increment
	    do (setf (first p1) x
		     (second p1) (abs (apply data-function
					     (cis (* x #.(coerce pi 'single-float)))
					     args)))
	    (funcall function p1)
	    (psetq p1 p2 p2 p1)))))


(defclass GRAPH-SAMPLE-DATA-MIXIN ()
	((sample-data :initform () :initarg :sample-data :accessor sample-data))
	)

(declare-required-method compute (SELF))

(defmethod initialize-instance :after ((self graph-sample-data-mixin) &key)
  (when (sample-data self) (compute self)))

;;; BW: this was removed because it was computing twice...

(defmethod (setf sample-data) :after (new (self graph-sample-data-mixin))
  (when new (compute self)))


(defclass histogram-data (graph-sample-data-mixin graph-data)   ; KRA: Replace with ST:ESSENTIAL-SAMPLE
  ((min :initform nil :initarg :min)
   (max :initform nil :initarg :max)
   (bin-count :initform nil :initarg :bin-count)
   (bin-size :initform nil :initarg :bin-size)
   (bins :initform () :initarg :bins))
  (:default-initargs :symbologies '(:bar))
  (:documentation "A Histogram of a sample of data."))

(defmethod sample-limits ((self histogram-data))
  (with-slots (sample-data) self
    (loop for datum in sample-data
	  minimize datum into sample-min
	  maximize datum into sample-max
	  finally (return (values sample-min sample-max)))))

(defmethod compute ((self histogram-data))
  "Compute the histogram."
  (with-slots (min max bin-count bin-size bins sample-data) self
    (unless (and min max)
      (multiple-value-bind (sample-min sample-max)
	  (sample-limits self)
	(setq min (or min sample-min))
	(setq max (or max sample-max))))
    (setq bin-count (or bin-count (values (truncate (max 20 (sqrt (length sample-data))))))
	  bin-size (or bin-size (float (/ (- max min) bin-count))))
    ;; Force bar-width to be the "right" size, regardless of what the user wants.
    (setf (bar-width self) bin-size)
    (if (or (null bins) (< (length bins) bin-count))	; !KRA: Should return old array.
	(setq bins (make-array bin-count :initial-element 0))
	(fill bins 0))
    (loop with bin
	  for value in sample-data
	  do
      (setq bin (values (truncate (- value min) bin-size)))
      ;; pile all data outside min and max into the first and last bins:
      (incf (aref bins (max 0 (min (1- bin-count) bin)))))))

(defmethod data ((self histogram-data))
  (with-slots (sample-data bin-count bin-size bins min) self
    (when sample-data
      (loop for bin from 0 to (1- bin-count)
	    collect `(,(+ min (* bin bin-size)) ,(aref bins bin))))))

(defun gaussian-random-sample (n)
  ;; Random sample of size n from a Gaussian (0 1) population.
  (loop repeat n
	collect (gaussian-random 0 1)))


;;; The following dataset (MULTIDIMENSIONAL-DATA) and graph class
;;; (GRAPH-WITH-RESELECTABLE-AXES) go together.  They are for the case where
;;; individual datums are not really xy pairs but more like feature vectors with many
;;; dimensions.  You provide two accessor functions, one each for the x and y axes,
;;; using the SET-AXES function.  Those two axes will then be displayed.  SET-AXES
;;; may then be used to change the dimensions being displayed without having to make
;;; a new graph.

(defclass MULTIDIMENSIONAL-DATA (graph-data-dither-mixin
				  accessor-datum-mixin
				  graph-data)
    ()
  (:default-initargs :symbologies '(:scatter))
  (:documentation
    "Useful if your datums have several dimensions.  Use X and Y accessor functions
     to control which ones you get on a graph."))

(defmethod set-axes ((self MULTIDIMENSIONAL-DATA) x-accessor y-accessor)
  (setf (x-accessor self) x-accessor (y-accessor self) y-accessor))

(defclass reselectable-axes-mixin ()
    ((x-accessor :initform nil :initarg :x-accessor
		  :accessor x-accessor)
     (y-accessor :initform nil :initarg :y-accessor
		  :accessor y-accessor)))

(defmethod set-axes ((self reselectable-axes-mixin) x-accessor y-accessor)
  (setf (x-accessor self) x-accessor (y-accessor self) y-accessor))

(defclass graph-with-reselectable-axes (reselectable-axes-mixin annotated-graph)
    ()
  (:default-initargs :auto-scale :both :show-legend nil)
  (:documentation
    "Useful if your datums have several dimensions.  Use X and Y accessor functions
     to control which ones you get on a graph.  Datasets must handle the SET-AXES method."))

(defmethod label-this-accessor ((self graph-with-reselectable-axes) accessor)
  (present-to-string accessor))

(defmethod set-axes :after ((self graph-with-reselectable-axes) x-accessor y-accessor)
  (dolist (d (datasets self)) (set-axes d x-accessor y-accessor))
  (setf (x-label self) (label-this-accessor self x-accessor)
	(y-label self) (label-this-accessor self y-accessor))
  ;; flush old annotations...
  ;; (setf (annotations self) nil)
  (setf (auto-scale self) :both))

(defmethod initialize-instance :after ((self graph-with-reselectable-axes) &key)
  (when (and (x-accessor self) (y-accessor self))
    (set-axes self (x-accessor self) (y-accessor self))))


;; EQUATIONS FOR A LINE:
;; 
;; 1.  y = m*x + b
;;   Parameters: m, b.
;;   blows up when m ~ 1/0; therefore looser.
;; 
;; Redundent represtentations are safer:
;; 2.  c*y - s*x = r; m = s/c ; sqrt (c^2 + s^2) = 1; r = c*b
;; 
;; 3.  A*x + B*y = C; A = -s; B = c; C = r = c*b = B*b.
;; Where [a,b] is a unit vector and c is the distance from the origin
;;     to the line.
;;   m = s/c  = -A/B ; b = C/B.
;; 


(defun line-between-points (x1 y1 x2 y2 &aux c s l)
  ;; Returns values A B C s.t. A*x + b*y = C is the equation of the line between points
  ;; (X1 Y1), (X2 Y2).
  (setq s (- (- y2 y1))
	c (- x2 x1)
	l (sqrt (+ (expt c 2) (expt s 2)))
	s (/ s l)
	c (/ c l))
  (values  s c (+ (* s x1) (* c y1))))

(defun line-slope-intercept (a b c)
  ;; Returns values of equivalent Slope and intercept given the line A*x + By = C.
  (if (zerop b) (values nil nil)
      (values (/ (- a) b) (/ c b))))

(defun line-y-internal (x a b c)
  ;; Returns NIL when Y cannot be determined.
  (unless (zerop b)
    (/ (- c (* a x)) b)))

(defun line-x-internal (y a b c)
  ;; Returns NIL when X cannot be determined.
  (unless (zerop a)
    (/ (- c (* b y)) a)))


(defclass LINE-MIXIN ()
  ((line-a :initform nil :initarg :line-a)
   (line-b :initform nil :initarg :line-b)
   (line-c :initform nil :initarg :line-c))
  )

(defmethod line ((self line-mixin))
  (with-slots (line-a line-b line-c) self
    (values line-a line-b line-c)))

(defmethod set-line-between-points ((self line-mixin) x1 y1 x2 y2)
  (with-slots (line-a line-b line-c) self
    (multiple-value-setq (line-a line-b line-c)
      (line-between-points x1 y1 x2 y2))))

(defmethod set-line ((self line-mixin) a b c)
  (with-slots (line-a line-b line-c) self
    (setq line-a a line-b b line-c c)))

(defmethod line-y ((self line-mixin) x)
  (with-slots (line-a line-b line-c) self
    (line-y-internal x line-a line-b line-c)))

(defmethod line-x ((self line-mixin) y)
  (with-slots (line-a line-b line-c) self
    (line-x-internal y line-a line-b line-c)))

(defmethod slope-intercept ((self line-mixin))
  (with-slots (line-a line-b line-c) self
    (line-slope-intercept line-a line-b line-c)))

(defmethod set-slope-intercept ((self line-mixin) slope intercept)
  (with-slots (line-a line-b line-c) self
    (multiple-value-setq (line-a line-b line-c)
      (line-between-points 0.0 intercept 1.0 (+ slope intercept)))))


(defclass LINE-DATA
    (line-mixin
     ;; A simplified version for GRAPH-DATA
     graph-data-color-mixin
     graph-datum-line-symbology-mixin
     basic-graph-datum-symbology-mixin
     basic-graph-data)
  ())

;;; Specialize this by providing a :COMPUTE method.
(defclass BASIC-LINE-FIT (graph-sample-data-mixin line-data)
  ())


#||
;;; Following is probably incomplete.
(defclass l2-fit
	()
	(basic-line-fit))

(defmethod fit ((self l2-fit))
  (loop with x-mean = 0 and y-mean = 0
	and points = (data dataset)
	do x-mean)
  )
;;; m = xy-mean // xx-mean; b = y-mean - m * x-mean

(defmethod fit-x-mean ((self l2-fit))
  (setq slope 0)
  (setq intercept
         (loop for data-pair in (data dataset)
	      with x-sum = 0 and x-count = 0
	      do (setq x-count (1+ x-count))
	      (setq x-sum (+ x-sum (car data-pair)))
	      finally (return (fixr (// x-sum x-count))))))

(defmethod fit-y-mean ((self l2-fit))
  (setq slope 10.e16)
  (setq intercept
	(loop for data-pair in (data dataset)
	      with y-sum = 0 and y-count = 0
	      do (setq y-count (1+ y-count))
	      (setq y-sum (+ y-sum (cadr data-pair)))
	      finally (return (fixr (// y-sum y-count))))))
||#
