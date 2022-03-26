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

#|
GRAPHICS DEVICE PRIMITIVES

This defines a simple, easy to port (?), interface to many graphics devices.
  DEVICE-DRAW-POINT
  DEVICE-DRAW-LINE
  DEVICE-DRAW-LINES
  DEVICE-DRAW-RECTANGLE
  DEVICE-DRAW-TRIANGLE
  DEVICE-TEXT

COORDINATE SYSTEMS:

SCREEN
  The coordinates used to draw on the graphics stream.  This is perhaps a misnomer,
  it should have been called STREAM coordinates.  These are the coordinates that
  the underlying GUI functions use (e.g. DRAW-LINE, STREAM-POINTER-POSITION*, etc.).
  Origin is at upper left, x increases to right, y increases downward.

UV
  The coordinate system used by DEVICE-DRAW-* routines to draw on the stream.  
  Origin is in the lower left, x increase to right, y increase upward.

[Actually, the UV origin is not at the lower left of the stream, but the lower left of
the first viewport.  For streams that scroll, graphs that are not placed in the first
viewport get a negative V.  It would seem that UV coordinates serve the same role as
screen coordinates, and UV could probably be phased out without a loss of functionality
or convenience.  JPM]

XY
  The coordinate system seen on the x and y axes of a graph, as well as 
  the coordinate system used by the datasets.

[Historically, mouse coordinates have also been used.  However, this coordinate
system is highly unportable, and it always ended up being an intermediate stage 
to some other coordinate system.  Thus this coordinate system has been exorcised
and should be avoided in the future.  JPM 1-29-91.]

|#

(defun stream-height (stream)
  "Height of the viewport."
  (multiple-value-bind (ignore height)
      (stream-viewport-size stream)
    (declare (ignore ignore))
    (truncate height)))

(defmacro uv-to-screen (screen u v)
  `(values ,u (- (stream-height ,screen) ,v)))

(defmacro screen-to-uv (screen u v)
  `(values ,u (- (the fixnum (stream-height ,screen)) (the fixnum ,v))))


;;; Clipping:  It is assumed that the underlying graphics system can do real
;;; clipping.  We clip lines, and if something will overlap the clip rectangle, we
;;; let the underlying system handle the details.  
;;; In General we are constantly fighting with clipping which tends to
;;; grow huge bitmaps (football field sized) if your aren't careful.
;;; So, clip as much as possible ourselves, relying on Genera as
;;; little as possible.  So there are macros for specifying the
;;; clip region that the DEVICE- routines and another macro to let
;;; Genera do the clipping after we are sure we don't get a football field. 

(defvar *CLIP-RECTANGLE*
	'(0 2000 0 2000)
  "Screen coordinates of clipping rectangle, left right bottom top.
 (and (< left right) (< bottom top)) so bottom is above top if you look at screen!!")

(defmacro WITH-CLIPPING-SCREEN-RECTANGLE ((stream le re be te) &body body)
  `(with-stack-list (*clip-rectangle* ,le ,re ,be ,te)
     ,stream					; Ignored
     ,@body))

(defmacro WITH-CLIPPING-UV-RECTANGLE ((stream le re be te) &body body)
  `(multiple-value-bind (.le. .be.)
       (uv-to-screen ,stream ,le ,be)
     (multiple-value-bind (.re. .te.)
	 (uv-to-screen ,stream ,re ,te)
       (with-clipping-screen-rectangle (,stream .le. .re. .te. .be.)	; -y
	 ,@body))))

(defvar *clim-clip-rectangle* 
    (make-bounding-rectangle 0 0 1 1)
  "Reused by with-clipping-internal to reduce consing.")

(defmacro WITH-CLIPPING-INTERNAL ((stream) &body body)
  ;; This does the final clipping details.
  `(let* ((r *clip-rectangle*)
	  (le (pop r))
	  (re (pop r))
	  (be (pop r))
	  (te (pop r)))
    (setf (clim:rectangle-edges* *clim-clip-rectangle*)
     (values le te re be))
    (with-drawing-options (,stream :clipping-region *clim-clip-rectangle*)
      ,@body)))

(defun POINT-IN-RECTANGLE-P (x y left right bottom top)
  (declare (fixnum x y left right bottom top))
  (and (< left x right) (< bottom y top)))

(defun POINT-IN-CLIP-RECTANGLE-P (x y &optional (region *clip-rectangle*))
  (point-in-rectangle-p x y
			(pop region) (pop region)
			(pop region) (pop region)))

(defun RECTANGLES-OVERLAP-P (l1 r1 b1 t1 l2 r2 b2 t2)
  ;; clim does this sort of thing already
  (assert (and (<= l1 r1) (<= b1 t1) (<= l2 r2) (<= b2 t2)))
  (macrolet
    ((intervals-overlap-p (l1 r1 l2 r2)
       `(not (or (> ,l2 ,r1) (> ,l1 ,r2)))))
    (and (intervals-overlap-p l1 r1 l2 r2)
	 (intervals-overlap-p b1 t1 b2 t2))))

(defun rectangle-overlaps-clip-rectangle-p (l2 r2 b2 t2)
  (apply #'rectangles-overlap-p l2 r2 b2 t2 *clip-rectangle*))

#|
code:
        xl  xr
       9 I  8 I 10
yt ----------------
       1 I  0 I  2
yb ----------------
       5 I  4 I  6
|#

(defun %clip-line (x1 y1 x2 y2 xl xr yb yt)
  ;; After W.M. Newman, and R.F. Sproull, Principles of Interactive Computer
  ;; Graphics, McGraw-hill, 1973, p. 124
  ;; Declaring the range of these integers helps inline more of the functions.  JPM.
  (macrolet
    ((code (x y)
       `(logior (if (< ,x xl) 1
		    (if (> ,x xr) 2 0))
		(if (< ,y yb) 4
		    (if (> ,y yt) 8 0))))
     (clip (y1 y2 xl x1 x2)
       `(setq ,y1 (let ((d (- ,x2 ,x1)))
		    (declare (fixnum d))
		    (if (zerop d) ,y1
		      (+ ,y1 (values (truncate
				      (the (integer -1000000 1000000)
					(* (the (integer -1000000 1000000) (- ,y2 ,y1))
					   (the (integer -1000000 1000000) (- ,xl ,x1))))
				      d)))))
	      ,x1 ,xl))
     (clip-point (c1 x1 y1 x2 y2)
       `(unless (zerop ,c1)
	  (if (zerop (ldb (byte 1 0) ,c1))
	      (if (not (zerop (ldb (byte 1 1) ,c1))) (clip ,y1 ,y2 xr ,x1 ,x2))
	      (clip ,y1 ,y2 xl ,x1 ,x2))
	  (if (zerop (ldb (byte 1 2) ,c1))
	      (if (not (zerop (ldb (byte 1 3) ,c1))) (clip ,x1 ,x2 yt ,y1 ,y2))
	      (clip ,x1 ,x2 yb ,y1 ,y2)))))
    (let ((c1 (code x1 y1))
	  (c2 (code x2 y2)))
      (declare (type (integer 0 9) c1 c2))
      (loop (and (zerop c1) (zerop c2) (return (values x1 y1 x2 y2)))
	    (or (zerop (logand c1 c2)) (return nil))
	    (clip-point c1 x1 y1 x2 y2)
	    (clip-point c2 x2 y2 x1 y1)
	    (setq c1 (code x1 y1)
		  c2 (code x2 y2))))))

(defmacro clip-line-to-clip-rectangle (x1 y1 x2 y2)
  `(apply #'%clip-line ,x1 ,y1 ,x2 ,y2 *clip-rectangle*))


;;; "PORTABLE" GRAPHICS DEVICE INTERFACE FOR GRAPHS.

#|
WITH-GRAPHICS-STYLE lets you set default values of things as if they are keywords.
Actually, they are generic functions that may produce side effects.  For example
a :COLOR may actually change an ALU too.  So, we must have WITH-GRAPHICS-STYLE-SAVED 
save all the state that could be side effected.

Actually, the above was just a plan, that didn't work out because you can't make draw on a
bitmap.  So, although i don't like this, the defaults are currently special variables.
Alternatively, i could modify the graphics::drawing-state of the window.

Really actually, this plan failed too.  We just give you the basic Symbolics defaults
without any way to override them.  So this is commented out.

|#

(defvar %dash-pattern 0)			
(defvar %thickness 0)
(defvar %line-end-shape :round)
(defvar %line-joint-shape :miter)
(defvar %pattern nil)

(defmacro WITH-GRAPHICS-STYLE ((stream &rest plist) &body body)
  (declare (ignore stream))
  (flet ((%f (keyword) (intern (format nil "%~A" (string keyword)) 'graph)))
    `(let ,(loop for p on plist by #'cddr
		  collect `(,(%f (first p)) ,(second p)))
       ,@body)))

(defun DEVICE-DRAW-POINT (stream u v &key (alu %alu)  &allow-other-keys)
  (multiple-value-setq (u v) (uv-to-screen stream u v))
  (when (point-in-clip-rectangle-p u v)
    (draw-point u v :stream stream :alu alu)))


(defconstant *DASH-PATTERN-SIZE* 64 "Length of dashed pattern in pixels.")
(defconstant *DASH-STEP-SIZE* (/ *dash-pattern-size* 8))
(defvar *DASH-PATTERNS*
	     #2A((8  7  6  5  4  3  2  1)
		 (7  6  5  4  3  2  1 -1)
		 (5  4  3  2  1 -1  1 -1)
		 (4  3  2  1 -1  2  1 -1)
		 (3  2  1 -1  3  2  1 -1)
		 (3  2  1 -1  1 -1  1 -1)
		 (2  1 -1  2  1 -1  1 -1)
		 (1 -1  1 -1  1 -1  1 -1))
	     "Dashed line patterns.  -1 -> lift pen.")

(defvar *dash-pattern-alist*
	     '(("----------------" :value 0)
	       ("------- ------- " :value 1)
	       ("----- - ----- - " :value 2)
	       ("---- -- ---- -- " :value 3)
	       ("--- --- --- --- " :value 4)
	       ("--- - - --- - - " :value 5)
	       ("-- -- - -- -- - " :value 6)
	       ("- - - - - - - - " :value 7))
  "Dashed line patterns for menu choosing.")

(defun dash-line  (style x1 y1 x2 y2 ds &key
		   (stream *standard-output*)
		   (alu %alu)
		   (thickness %thickness)
		   (pattern %pattern))
  "Draw a dashed-line between (x1 y1) and (x2 y2) in the given style
   0 <= style <= 7."
  ;;(declare (values ds))
  (assert (numberp ds))
  (let ((dash-patterns *dash-patterns*)
	(dash-step-size *dash-step-size*)
	(dash-pattern-size *dash-pattern-size*))
    (setq style (mod style 8))
    (flet ((draw-line-piece (a b c d)
	     (draw-line a b c d
			:stream stream
			:alu alu
			:thickness thickness
			:line-end-shape :butt
			:pattern pattern)))
      (if
	(= 0 style)
	(progn (draw-line-piece x1 y1 x2 y2)
	       0.0)
	(let ((x-last x1)
	      (y-last y1))
	  (macrolet
	    ((n-steps (i-step) `(abs (aref dash-patterns style ,i-step)))
	     (plot (x y) `(if (< (aref dash-patterns style i-step) 0)	; Pen up?
			      (setq x-last ,x y-last ,y)
			      (progn (draw-line-piece x-last y-last ,x ,y)
				     (setq x-last ,x y-last ,y)))))
	    (let* ((dx (- x2 x1))		; Motion along line.
		   (dy (- y2 y1))
		   (length (sqrt (+ (* dx dx) (* dy dy))))	; Length of line segement.
		   (step-left			; Fraction of step remaining from before.
		     (/ (mod ds dash-step-size) dash-step-size))
		   (steps-init			; Initial step to take.
		     (n-steps (values (truncate (- (/ ds dash-step-size) step-left))))))
	      (when (not (zerop length))
		(let ((S (/ dash-step-size length)))
		  (setq dx (* dx S)		; DX pixels / step.
			dy (* dy S)))		; DY pixels / step.
		(loop for i-step = (values (truncate (- (/ ds dash-step-size) step-left)))
				 then (mod (+ i-step steps) 8)
		      as steps = (n-steps i-step)	; Steps to to take this iteration.
		      for length-so-far		; Length of line drawn so far.
			  = (- (* steps-init dash-step-size) step-left)
					then (+ length-so-far (* steps dash-step-size))
		      for x = (+ (- x1 (* step-left dx)) (* dx steps-init))
			    then (+ x (* dx steps)) 
		      for y = (+ (- y1 (* step-left dy)) (* dy steps-init))
			    then (+ y (* dy steps))
		      while (< length-so-far length)
		      do (plot x y)
		      finally (plot x2 y2)))
	      (mod (+ ds length) dash-pattern-size))))))))

(defun flip-alu-p (alu)
  "Determine if this alu represents XOR drawing."
  (eq alu :flipping))	; Color

(defun DEVICE-DRAW-LINE (stream x1 y1 x2 y2 
			 &key (alu %alu) (dash-pattern %dash-pattern)
			 (dash-ds 0)
			 (thickness %thickness)
			 (line-end-shape %line-end-shape) (pattern %pattern)
			 (transform t)
			 &allow-other-keys)
  "Draw a dashed-line between (x1 y1) and (x2 y2) in the given dash-pattern
   0 <= dash-pattern <= 7.
   Alters instance variables dash-ds and last-style."
  ;; KRA: Currently, we do not use with-clipping-internal here though for thick lines,
  ;; we should.  We also don't draw caps on thick dashed lines yet.
  (let ((end-point-p (flip-alu-p alu)))
    (when transform
      (multiple-value-setq (x1 y1) (uv-to-screen stream x1 y1))
      (multiple-value-setq (x2 y2) (uv-to-screen stream x2 y2)))
    (multiple-value-setq (x1 y1 x2 y2) (clip-line-to-clip-rectangle x1 y1 x2 y2))
    (if x1
	(if (zerop dash-pattern)
	    (progn
	      (draw-line x1 y1 x2 y2 :stream stream :alu alu
				  :thickness thickness :line-end-shape
				  (if end-point-p line-end-shape :no-end-point))
	      0.0)
	    (progn
	      (dash-line
		dash-pattern x1 y1 x2 y2 dash-ds
		:stream stream
		:alu alu
		:thickness thickness
		:pattern pattern)
	      ;; draw end caps here.
	      ))
	dash-ds)))				; Don't change ds.

(defun device-draw-lines (stream points &rest keys &key &allow-other-keys)
  (let ((ds (or (getf keys :dash-ds) 0.0)))
    (do ((points points (cddr points)))
	((null (cddr points)) ds)
      (setq ds (apply #'device-draw-line stream
		      (first points) (second points)
		      (third points) (fourth points)
		      keys)))))

;;; The value of this depends on the lisp implementation.
(defconstant *return* #.(elt (format nil "~%") 0))

(defun draw-text-internal (stream u v text &rest keys &key (rotation 0) &allow-other-keys)
  "Draw text with (left,top) as given, but do the right thing with return characters."
  (let* ((line-height (stream-line-height stream))
	 (text-v v)
	 (text-u u)
	 (start 0)
	 (count 0)
	 end
	 (length (length text))
	 (COS-FACTOR (values (truncate (* line-height (cos rotation)))))
	 (SIN-FACTOR (values (truncate (* line-height (sin rotation))))))
    (when (plusp length)
      (loop
	(setq END (or (position *return* text :test #'char= :start start) length))
	(when (> (- end start) 1)
	  (apply #'draw-string-image
		 (if (or (plusp start) (< end length))
		     (subseq text start end) ; Gross CONSING!
		   text)
		 (values (truncate text-u)) (values (truncate text-v))
		 :attachment-y :top
		 :stream stream keys))
	(incf count)
	(incf text-v COS-FACTOR)
	(incf text-u SIN-FACTOR)
	(if (= end length) (return))
	(setq start (1+ end))))))

(defun device-text (stream x y label &rest keys &key (attachment-y :baseline)
		    &allow-other-keys)
  ;; Use draw-string-image rather than draw-string so that some callers can
  ;; provide :rotation.
  (apply #'draw-string-image label (values (truncate x)) (values (truncate y))
	 :stream stream :attachment-y attachment-y keys))

(defun symbol-inside-rectangle-p (x y size/2 left right bottom top)
  (and (< left (- x size/2))
       (< (+ x size/2) right)
       (< bottom (- y size/2))
       (< (+ y size/2) top)))

(defun symbol-outside-rectangle-p (x y size/2 left right bottom top)
  (or (< right (- x size/2))
      (< (+ x size/2) left)
      (< top (- y size/2))
      (< (+ y size/2) bottom)))

(defmacro with-symbol-clipping ((stream u v size/2) &body body)
  `(if (apply #'symbol-inside-rectangle-p ,u ,v ,size/2 *clip-rectangle*)
       (progn ,@body)
       (if (not (apply #'symbol-outside-rectangle-p ,u ,v ,size/2 *clip-rectangle*))
	   (with-clipping-internal (,stream)
	     ,@body))))

(defun device-draw-diamond (stream u v size &rest keys)
  "Given screen coordinates, clip and draw a diamond."
  (declare (fixnum u v size))
  (setq size (values (truncate size 2)))
  (with-symbol-clipping (stream u v size)
    (let ((points (list u           (+ v size)
			(- u size) v
			u           (- v size)
			(+ u size) v
			u           (+ v size))))
      ;; No stack allocation, please, redisplay needs the list permanently.
      (apply #'draw-polygon points :stream stream keys))))

(defun device-draw-equilateral-triangle (stream u v side &rest keys)
  "Given screen coordinates, clip and draw a triangle centered on the given point."
  (declare (fixnum u v side))
  (let* ((x (values (truncate side 2)))
	 (y (values (round (the fixnum x) #.(sqrt 3.0)))))
    (declare (fixnum x y))
    (with-symbol-clipping (stream u v x)
      (apply #'draw-triangle
		    (- u x) (+ v y)
		    u (- v (the fixnum (* y 2)))
		    (+ u x) (+ v y)
		    :stream stream keys))))

(defun DEVICE-DRAW-CIRCLE (stream u v radius &rest keys)
  "Given screen coordinates, clip and draw a circle."
  (declare (fixnum u v radius))
  (let ((r (values (truncate radius 2))))
    (with-symbol-clipping (stream u v r)
      (apply #'draw-circle u v radius :stream stream keys))))

(defun DEVICE-DRAW-TRIANGLE (stream u1 v1 u2 v2 u3 v3 &rest keys &key &allow-other-keys)
  "Given screen coordinates, clip and draw a triangle."
  (declare (fixnum u1 v1 u2 v2 u3 v3))
  (if (or (clip-line-to-clip-rectangle u1 v1 u2 v2)	; Is triangle in clipping region?
	  (clip-line-to-clip-rectangle u2 v2 u3 v3))
      (with-clipping-internal (stream)
	(apply #'draw-triangle u1 v1 u2 v2 u3 v3 :stream stream keys))))

(defun DEVICE-DRAW-RECTANGLE (stream left right bottom top &rest keys
			      &key &allow-other-keys)
  "Given screen coordinates, clip and draw a rectangle."
  (declare (fixnum left right bottom top))
  (if (< right left) (psetq left right right left))
  (if (< bottom top) (psetq top bottom bottom top))
  (let ((r *clip-rectangle*))
    (setq left (max left (pop r))
	  right (min right (pop r))
	  top (max top (pop r))			; -y
	  bottom (min bottom (pop r)))
    (when (and (< left right) (< top bottom))	; Rectangles overlap.
      (apply #'draw-rectangle left right top bottom :stream stream keys))))

;;;**************************************************
;;;
;;; MAKE-OPTIMIZED-LINE-DISPLAYER
;;;
;;; Returns a closure capable of drawing a line.  The arguments to the closure
;;; are (stream x1 y1 x2 y2).  Here we put all our smarts about how to draw lines
;;; as fast as possible.

(defun make-optimized-line-displayer (alu thickness record-output-history)
  (let ((line-style nil))
    (cond (record-output-history
	   #'(lambda (stream from-x from-y to-x to-y)
	       (progn
                 ;; 192 bytes
                 (unless line-style
                   (setq line-style 
                         (make-line-style :thickness thickness)))
                 (with-drawing-options (stream :ink alu 
                                               :line-style line-style)
                   (medium-draw-line* 
                    stream from-x from-y to-x to-y)))))
	  (t
	   #'(lambda (stream from-x from-y to-x to-y)
               (unless line-style
                 (setq line-style (make-line-style :thickness thickness)))
               (clim:draw-line* stream from-x from-y to-x to-y
                                :ink alu :line-style line-style))))))


