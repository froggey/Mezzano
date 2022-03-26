;;; -*- Syntax: Common-lisp; Package: graph -*-
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
;;; Mouse stuff
;;;

(defun uv-under-mouse (stream)
  "The UV position of the mouse."
  (multiple-value-bind (x y) (stream-pointer-position* stream)
    (screen-to-uv stream x y)))

(defmacro button-case (button &key left middle right)
  "Implementation-specific way to dispatch based on the button pushed."
  `(cond
    ,@(when left
        `(((event-matches-gesture-name-p ,button :select)
           ,left)))
    ,@(when middle
        `(((event-matches-gesture-name-p ,button :describe)
           ,middle)))
    ,@(when right
        `(((event-matches-gesture-name-p ,button :menu)
           ,right)))))

(defmethod post-mouse-documentation (stream string)
  (declare (ignore stream))
  (clim-extensions:frame-display-pointer-documentation-string
   *application-frame* string))

(defmacro with-mouse-documentation ((window string) &body body)
  `(unwind-protect
       (progn (post-mouse-documentation ,window (or ,string " ")) ,@body)
     (post-mouse-documentation ,window " ")))

(defmacro with-pointer-cursor ((sheet cursor) &body body)
  `(let ((.old. (sheet-pointer-cursor ,sheet)))
     (unwind-protect
	 (progn (setf (sheet-pointer-cursor ,sheet) ,cursor)
		,@body)
       (setf (sheet-pointer-cursor ,sheet) .old.))))

;;; DRAG-ICON is used to do most all of the mouse tracking.  It differs from
;;; dragging-output in that the latter simply does output once and drags
;;; the output record around the screen.  This function explicitly erases and
;;; redraws, which is useful if the shape of the output depends upon its location
;;; (e.g. sliders).
(defun drag-icon (stream draw-it erase-it move-it 
		  &optional documentation (cursor :move))
  "Mouse tracker for dragging graphic objects."
  ;; Erase the object before calling this function.
  ;; Dont forget to redraw the object after this function returns.
  ;; This requirement gives the caller the freedom to use an "abbreviated"
  ;; drawing for the inner loop, which may be necessary to create the
  ;; illusion of animation.
  (unless (extended-input-stream-p stream)
	  (error "Cannot track the mouse on this stream (~S)" stream))
  (with-pointer-cursor (stream cursor)
    (let (last-x last-y
	  (movements 0)
	  ;; If we have had some movement and then the mouse is released, we
	  ;; probably want to quit the loop.  We don't count the first few because the
	  ;; user might still be releasing the button that got him here.
	  (down-threshold 0)
	  (up-threshold 0))
      ;; Sometimes we get rationals.
      ;; (declare (fixnum last-x last-y movements))
      (unless documentation
	(setq documentation "Click/Release mouse to set new position"))
      (multiple-value-setq (last-x last-y) (stream-pointer-position* stream))
      (unless (and last-x last-y) (beep) (setq last-x 0 last-y 0))
      (flet ((update-position (x y)
	       ;; "pixel" positions are often ratios and floats in clim
	       (post-mouse-documentation stream documentation)
	       (let ((dx (- x last-x))
		     (dy (- y last-y)))
		 ;;(declare (fixnum dx dy) (fixnum x y))
		 (when (or (not (zerop dx)) (not (zerop dy)))
		   ;;(draw-circle last-x last-y 5 :filled t :alu %flip :stream stream)
		   ;;(draw-circle x y 5 :filled t :alu %flip :stream stream)
		   (incf movements)
		   (funcall erase-it stream)
		   (funcall move-it dx dy)
		   (setq last-x x last-y y)
		   (funcall draw-it stream)
		   ;; In X-windows, you need to force any buffered output.
		   (force-output stream)
		   )))
	     (button-clicked (button release-p)
	       ;; Seem to get spurious left-click button releases shortly
	       ;; after entering the tracker (13 movements).  Maybe leftover
	       ;; from the presentation menu that got us here...
	       (when (if release-p
			 (> movements up-threshold)
		       (> movements down-threshold))
		 (return-from drag-icon (values button last-x last-y)))))
        (with-output-recording-disabled (stream)
	  (unwind-protect
	      (progn
		(funcall draw-it stream)
		(force-output stream)
		(with-mouse-documentation (stream documentation)
		  (tracking-pointer (stream)
					 (:pointer-motion
					  (x y)
					  (update-position (values (truncate x)) (values (truncate y))))
					 (:pointer-button-press 
					  (event x y) 
					  (update-position (values (truncate x)) (values (truncate y)))
					  (button-clicked event nil))
					 (:pointer-button-release 
					  (event x y) 
					  (update-position (values (truncate x)) (values (truncate y)))
					  (button-clicked event t)))))
	    ;; CLIM leaves the button event resulting from :button-press in the input
	    ;; buffer, so take it out now.
	    (funcall erase-it stream)
	    (force-output stream)))))))

#|
(defun test-tracking (&optional (stream *standard-output*))
  (let ((string "Test Tracking") x y)
    (multiple-value-setq (x y) (stream-pointer-position* stream))
    (drag-icon stream
	       #'(lambda (stream) (draw-string string x y :stream stream :alu %flip))
	       #'(lambda (stream) (draw-string string x y :stream stream :alu %flip))
	       #'(lambda (dx dy)
		   (incf x dx)
		   (incf y dy)
		   (setq string (format nil "~S ~S" x y))))))
|#

(defun device-mouse-point (stream
			   &optional
			   (documentation "Mouse-Left: Select Point; Mouse-Middle: Cancel"))
  "Returns u,v and gesture chosen by the mouse."
  ;; It is good to draw a 'cursor' even though the true mouse cursor is drawn as
  ;; well, so that the user can see by looking at the screen what the hell is
  ;; going on.
  (let ((fudge 10) button)
    (multiple-value-bind (x y) (stream-pointer-position* stream)
      (flet ((x-marks-the-spot (str)
	       (draw-line x (- y fudge) x (+ y fudge) :stream str :alu %flip)
	       (draw-line (- x fudge) y (+ x fudge) y :stream str :alu %flip)
	       (draw-circle x y fudge :stream stream :alu %flip)))
	(setq button (drag-icon stream
				#'x-marks-the-spot
				#'x-marks-the-spot
				#'(lambda (dx dy) (incf x dx) (incf y dy))
				documentation))
	(multiple-value-setq (x y) (screen-to-uv stream x y))
	(button-case button
		     :left (values x y button)
		     :right (values x y button))))))

(defun shift-p (window)
  "Determine whether the shift key is depressed."
  (logtest +shift-key+
           (port-modifier-state (port window))))

(defun mouse-input-rectangle (stream)
  "Return edges of rectangle in stream coordinates."
  ;;(declare (values left top right bottom button))
  (multiple-value-bind (left top)
      (device-mouse-point stream)
    (when left
      (multiple-value-setq (left top) (uv-to-screen stream left top))
      (multiple-value-bind (right bottom) (stream-pointer-position* stream)
	(let (button)
	  (flet ((drawit (str)
		   (shift-p str)
		   (draw-rectangle left right top bottom :stream str :alu %flip)))
	    (setq button (drag-icon stream
				    #'drawit
				    #'drawit 
				    #'(lambda (dx dy)
					(when (shift-p stream)
					  (incf left dx)
					  (incf top dy))
					(incf right dx)
					(incf bottom dy))
				    "Mouse-Left: Done; Mouse-Middle: Cancel; Shift: Drag"))
	    (if (< right left) (psetq left right right left))
	    (if (< top bottom) (psetq top bottom bottom top))
	    (button-case
	      button
	      :left (values left top right bottom button)
	      :right (values left top right bottom button))))))))

(defun device-specify-rectangle (stream)
  "Ask user to specify a rectangle on STREAM with the mouse.
   Returns LEFT TOP RIGHT BOTTOM in UV coordinates."
  ;;(declare (values left top right bottom))
  (multiple-value-bind (left top right bottom button)
      (mouse-input-rectangle stream)
    (when left
      (multiple-value-setq (left top) (screen-to-uv stream left top))
      (multiple-value-setq (right bottom) (screen-to-uv stream right bottom))
      (values (min left right) (max top bottom)
	      (max left right) (min top bottom)
	      button))))

(defun map-polygon-edges (function corners)
  (let* ((this (car (last corners)))
	 (next (pop corners))
	 (x1 (pop this))
	 (y1 (pop this))
	 (x2 (pop next))
	 (y2 (pop next)))
    (loop
      (if (not x2) (return))
      (funcall function x1 y1 x2 y2)
      (setq next (pop corners))
      (setq x1 x2 y1 y2)
      (setq x2 (pop next) y2 (pop next)))))

(defun draw-screen-polygon (corners stream alu)
  (map-polygon-edges
    #'(lambda (x1 y1 x2 y2)
	(draw-line x1 y1 x2 y2 :stream stream :alu alu))
    corners))

(defun select-screen-polygon (stream &optional (cursor :position))
  "Select a sequence of points in screen coordinates.  Finish by clicking on first point."
  (with-output-recording-disabled (stream)
    (multiple-value-bind (lastx lasty) (device-mouse-point stream)
      (when lastx
	(multiple-value-setq (lastx lasty) (uv-to-screen stream lastx lasty))
	(when lastx
	  (sleep .4)			; wait for button release.
	  (let* ((first (list lastx lasty))
		 (first-highlighted nil)
		 (points (list first))
		 (rad 5)
		 (documentation
		  "Mouse-Left: Select Point; Mouse-Middle: Cancel; Mouse-Right: Finish")
		 (x lastx)
		 (y lasty))
	    (unwind-protect
		(block tracking
		  (with-pointer-cursor (stream cursor)
		    (labels ((distance (x1 y1 x2 y2)
			       (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
			     (near-first (x0 y0)
			       (< (distance x0 y0 (car first) (cadr first)) rad))
			     (highlight-first ()
			       (setq first-highlighted (not first-highlighted))
			       (draw-circle (car first) (cadr first) rad :filled nil
					    :alu %flip :stream stream))
			     (rubberband (x0 y0)
			       (draw-line lastx lasty x0 y0 :stream stream :alu %flip))
			     (update-position (x0 y0)
			       (post-mouse-documentation stream documentation)
			       (Rubberband x y)
			       (setq x x0 y y0)
			       (rubberband x y)
			       (if first-highlighted
				   (if (near-first x0 y0) nil (highlight-first))
				 (if (near-first x0 y0) (highlight-first) nil))
			       (force-output stream))
			     (all-done ()
			       (update-position (car first) (cadr first))
			       (if first-highlighted (highlight-first))
			       (force-output stream)
			       (return-from tracking points))
			     (button-clicked (button)
			       (button-case
				button
				:middle
				(progn
				  ;; cancel
				  (update-position (car first) (cadr first))
				  (push (list (truncate x) (truncate y)) points)
				  (if first-highlighted (highlight-first))
				  (return-from tracking nil))
				:right (all-done)
				:left
				(cond ((near-first x y) (all-done))
				      (t 
				       ;; select another point
				       (push (list (truncate x) (truncate y)) points)
				       (setq lastx x lasty y))))))
		      (with-mouse-documentation (stream documentation)
			(tracking-pointer
			 (stream)
			 (:pointer-motion (x y) (update-position x y))
			 (:pointer-button-press (event x y)
						(update-position x y)
						(button-clicked event)))
			points))))
	      ;; Erase results when done:
	      (draw-screen-polygon points stream %flip))))))))

