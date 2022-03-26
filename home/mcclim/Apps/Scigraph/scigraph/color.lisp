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

;;;
;;; A feeble color naming facility.  These names represent a selection
;;; of about 30 of the more than 700 names defined by X11.  We don't want
;;; too many colors, because menus should be short and because there are limits to
;;; how many colors X Windows will let you use.
;;;

(defvar *color-specifications*
	'(;; Primary colors.
	  (:Green   0.0 1.0 0.0)
	  ;;(:Blue    0.0 0.0 1.0)
	  (:Magenta 1.0 0.0 1.0)
	  (:Cyan    0.0 1.0 1.0)
	  ;;(:Yellow  1.0 1.0 0.0)
	  (:Red     1.0 0.0 0.0)

	  ;; Earth Tones
	  (:gold 1.0 .843 0.0)
	  (:goldenrod .855 .647 .125)
	  (:khaki .941 .902 .549)
	  (:olivedrab .420 .557 .137)
	  (:dark-khaki .741 .718 .420)
	  (:peachpuff 1.0 .855 .725)
	  (:sienna 0.627 0.322 0.176)	
	  (:orange 1.0 0.647 0.0)
	  ;; (:maroon 0.690 0.188 0.376)	
	  (:firebrick 0.698 0.133 0.133)
	  (:salmon 0.980 0.502 0.447)
	  (:aquamarine 0.498 1.0 0.831)
	  ;; (:dark-green 0.0 0.392 0.0)
	  ;; (:green-yellow 0.678 1.0 0.184)
	  (:pink 1.000 0.753 0.796)
	  ;; (:orchid .855 .439 .839)
	  (:violet-red 0.816 0.125 0.565)
	  (:violet 0.933 0.510 0.933)
	  
	  ;; Grays (chosen to make sense in 8-bit color)
	  ;;(:dark-slate-gray 0.184 0.31 0.31)
	  (:Black   0.0 0.0 0.0)
	  (:gray25 0.25 0.25 0.25)
	  (:gray50 0.500 0.500 0.500)
	  (:gray75 0.75 0.75 0.75)
	  (:White   1.0 1.0 1.0)

	  ;; Blues
	  (:royal-blue .255 .412 .882)
	  (:sky-blue 0.529 0.808 0.922)
	  (:steel-blue 0.275 0.510 0.706) 
	  (:light-blue 0.678 0.847 0.902)
	  ;; (:navy-blue 0.0 0.0 0.502)
	  (:dark-turquoise 0.0 0.808 0.820)
	  (:turquoise 0.251 0.878 0.816)
	  ;; (:indian-red 0.804 0.361 0.361)
	  )
  "List of (color-name red green blue) for each color a user 
   can choose from for drawing graphs.")

(defvar *colors* nil "Used for pop-edit.")
(defvar *color-hash* (make-hash-table :test 'equal))

(defun alu-for-color (color-name)
    "Translate a color name to an ink/alu"
    (or *colors* (initialize-color-system))
    (or (gethash color-name *color-hash* nil)
	(gethash :white *color-hash* nil)))

(defun make-color-choices (color-specifications &optional reinitialize)
    "Makes an Alist of colors to choose from."
    (if reinitialize (setq *color-hash* (make-hash-table)))
    (loop for (name red green blue) in color-specifications
       do
         (setf (gethash name *color-hash*) (clim:make-rgb-color red green blue))
         (pushnew (list (string-capitalize name) :value name)
                  *colors*
                  :test #'equal)))

(defun initialize-color-system ()
  "Initialize the color screen if available."
  ;; Called by ALU-FOR-COLOR the first time an alu is needed.
  (make-color-choices *color-specifications*))

(defun alu-for-stream (stream color-name)
  "Translate a color name to an ink/alu"
  (if (color-stream-p stream)
      (alu-for-color color-name)
      (if (eq color-name :black) %erase %draw)))

(defun draw-color-swatch (stream color-name pretty-name selected-p &optional size)
  "Draw a sample of the given color at the current cursor position."
  (declare (ignore pretty-name))
  (or size (setq size (stream-line-height stream)))
  (let ((rad (1- (values (truncate size 2)))))
    (multiple-value-bind (x y) (stream-cursor-position* stream)
      (draw-circle (+ x rad 1) (+ y rad 1) rad
		   :filled t
		   :stream stream :alu (alu-for-stream stream color-name))
      (draw-rectangle x (+ x size -1) y (+ y size -1)
		      :stream stream :filled nil :alu (if selected-p %draw %erase)))))

(defun display-colors (&optional
		       (stream *standard-output*)
		       (colors *colors*))
  "Display the current colors and their names."
  (dolist (color-name colors)
    (setq color-name (third color-name))
    (terpri stream)
    (draw-color-swatch stream color-name nil nil)
    (stream-increment-cursor-position stream 15 0)
    (format stream " ~A " color-name)))

(defun display-gray-wash (&optional
			  (stream *standard-output*)
			  (quantum (/ 1.0 200)))
  "Display the range of gray scales."
  ;; Useful on 8-bit color systems to see what resolution there really is.
  (window-clear stream)
  (multiple-value-bind (left top right bottom) (stream-viewport stream)
    (let* ((intensity 0.0)
	   (width (- right left))
	   (height (- bottom top))
	   (increment 0.0))
      (setq left (+ left (* .1 width))
	    increment (* .8 quantum width)
	    right (+ left increment)
	    top (+ top (* .1 height))
	    bottom (- bottom (* .1 height)))
      (loop
	(if (> intensity 1.0) (return))
	(let ((gray (clim:make-rgb-color intensity intensity intensity)))
	  (draw-rectangle left right bottom top :stream stream :alu gray :filled t)
	  (setq left right right (+ right increment))
	  (incf intensity quantum))))))

(clim:define-presentation-type-abbreviation color-presentation ()
  ;; Can't simply call this 'color' because that already names a class.
  `((member ,@(mapcar #'third *colors*))
    :name-key string-capitalize
    :printer present-color
    :highlighter highlight-color))

(defun present-color (object stream &key acceptably)
  (declare (ignore acceptably))
  (with-room-for-graphics (stream)
    (draw-color-swatch stream object
		       (string object)
		       nil)))

(defun highlight-color (continuation object stream)
  (clim:surrounding-output-with-border
   (stream)
   (funcall continuation object stream)))
