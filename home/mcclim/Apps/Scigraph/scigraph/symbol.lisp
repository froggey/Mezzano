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

;;; CENTERED SYMBOLS

;;; DRAW-SYMBOL is the vanilla way of doing the output.  However, for the
;;; case where every call uses the same arguments (which is the most common
;;; case), SYMBOL-DISPLAYER is provided.  The latter returns a closure that
;;; can be called as many times as necessary.  The benefit is that method
;;; dispatch is done only once.

;;; KRA 27APR93: DRAW-SYMBOL is no longer used in Scigraph, although it may
;;; be useful to users.  Perhaps it should be removed.

(defgeneric symbol-displayer (type alu thickness filled))

(defmethod symbol-displayer ((type (eql :x)) alu thickness filled)
  (declare (ignore filled))
  #'(lambda (stream u v size)
      (draw-line (+ u size) (+ v size) (- u size) (- v size)
		 :stream stream :alu alu :thickness thickness)
      (draw-line (- u size) (+ v size) (+ u size) (- v size)
		 :stream stream :alu alu :thickness thickness)))

(defmethod symbol-displayer ((type (eql :box)) alu thickness filled)
  #'(lambda (stream u v size)
      (draw-rectangle
       (- u (1- size)) (+ u (1- size))
       ;; JPM: why is this 2 pixels off?
       (- v size) (+ v size)
       :stream stream :alu alu :thickness thickness :filled filled)))

(defmethod symbol-displayer ((type (eql :+)) alu thickness filled)
  (declare (ignore filled))
  #'(lambda (stream u v size)
      (draw-line u (+ v size) u (- v size)
		 :stream stream :alu alu :thickness thickness)
      (draw-line (- u size) v (+ u size) v
		 :stream stream :alu alu :thickness thickness)))

(defmethod symbol-displayer ((type (eql :*)) alu thickness filled)
  (let ((a (symbol-displayer :+ alu thickness filled))
	(b (symbol-displayer :x alu thickness filled)))
    #'(lambda (stream u v size)
	(funcall a stream u v size)
	(funcall b stream u v size))))

(defmethod symbol-displayer ((type (eql :triangle)) alu thickness filled)
  #'(lambda (stream u v size)
      (device-draw-equilateral-triangle
       stream u v (* size 2) :alu alu :thickness thickness :filled filled)))

(defmethod symbol-displayer ((type (eql :diamond)) alu thickness filled)
  #'(lambda (stream u v size)
	(device-draw-diamond stream u v (* size 2)
			     :alu alu :thickness thickness :filled filled)))

(defmethod symbol-displayer ((type (eql :point)) alu thickness filled)
  ;; Try to make this one of the fast ones.
  ;; Assume the alu is already cached on the stream as the foreground color.
  (declare (ignore thickness filled))
  #'(lambda (stream u v size)
      (declare (ignore size))
      (draw-point* stream u v)))

(defmethod symbol-displayer ((type (eql :CIRCLE)) alu thickness filled)
  (if filled
      #'(lambda (stream u v size)
	  (draw-circle u v size :stream stream :alu alu :filled filled))
      #'(lambda (stream u v size)
	  (draw-circle u v size :stream stream :alu alu
		       :thickness thickness))))

;;;
;;; Symbol presentation type.
;;;

(defun draw-avv-symbol (symbol size stream selected-p)
  (multiple-value-bind (x y) (stream-cursor-position* stream)
    (let ((displayer (symbol-displayer symbol %draw 0 nil))
	  (h (max (+ size 2) (stream-line-height stream))))
      (if (not selected-p)
	  ;; Draw something, even if not selected, so that really
	  ;; tiny symbols are still easy to choose with the mouse.
	  (draw-rectangle x (+ x h) y (+ y h)
			  :stream stream
			  :alu %erase
			  :filled t))
      (funcall displayer stream
	       (+ x (values (truncate h 2)))
	       (+ y (values (truncate h 2)))
	       (values (truncate size 2)))
      (if selected-p
	  (draw-rectangle x (+ x h) y (+ y h)
			  :stream stream
			  :alu %flip
			  :filled t)))))

(define-presentation-type-abbreviation graph-symbol
    (&key (symbols '(:+ :x :* :point :triangle
		     :box :diamond :circle))
	  (size 10) graph)
  ;; Can't simply call this 'color' because that already names a class.
  `((member ,@symbols)
    :name-key string-capitalize))
