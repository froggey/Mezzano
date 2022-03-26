;;; -*- Mode: Lisp; Package: CLIM-DEMO -*-

;;;  (c) copyright 2005 by 
;;;           Tim Moore (moore@bricoworks.com)

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

;;; This is an example of multiprocessing in which a thread -- running a
;;; simulation or responding to external events -- sends events to the
;;; application thread that cause it to refresh the display. The two threads
;;; share state variables protected by a lock; the application thread uses a
;;; condition variable to signal the simulation thread to change its
;;; behavior. One could also envision sending the simulation state in a
;;; message to the application thread and using an event queue to send control
;;; messages back to the simulation thread, eliminating the need for explicit
;;; locks. Perhaps in anothor demo...
;;;
;;; Based on an idea described by Paul Werkowski in the mcclim-devel mailing
;;; list.

(defpackage #:clim-demo.stopwatch
  (:use #:clim #:clim-lisp)
  (:export #:stopwatch #:run-stopwatch))
(in-package #:clim-demo.stopwatch)

(define-application-frame stopwatch ()
  (;; state of the timer
   (start-time :accessor start-time :initform 0
	       :documentation "In internal time units")
   (elapsed-time :accessor elapsed-time :initform 0
		 :documentation "In internal time units")
   (stop-time :accessor stop-time :initform 0
	      :documentation "In count-down mode, elapsed time to stop")
   (mode :accessor mode :initform :stopped)
   ;; data displayed by main thread
   (hours :accessor hours :initform 0)
   (minutes :accessor minutes :initform 0)
   (seconds :accessor seconds :initform 0)
   (clock-process :accessor clock-process :initform nil)
   (condition-variable :accessor condition-variable
		       :initform (clim-sys:make-condition-variable))
   (clock-lock :accessor clock-lock :initform (clim-sys:make-lock)))
    (:pointer-documentation t)
  (:panes
   (clock :application
	  :width 300 :height 200
	  :display-function 'draw-clock
	  :incremental-redisplay t
	  :scroll-bars nil)
   (commands :interactor :height 100))
  (:layouts
   (default
     (vertically ()
       clock
       commands))))

(define-presentation-type clock ()
  :inherit-from t)

(defmacro with-locking-bind (var-forms lock &body body)
  "Bind the variables in VAR-FORMS under the protection of LOCK, then release
  the lock for the BODY. Declarations are permitted in BODY."
  (let ((vars (mapcar #'(lambda (form)
			  (if (consp form)
			      (car form)
			      form))
		      var-forms))
	(vals (mapcar #'(lambda (form)
			  (if (consp form)
			      (cadr form)
			      nil))
		      var-forms)))
    `(multiple-value-bind ,vars
	 (clim-sys:with-lock-held (,lock)
	   (values ,@vals))
       ,@body)))

(defun draw-clock (frame pane)
  (with-locking-bind
      ((hours (hours frame))
       (minutes (minutes frame))
       (seconds (seconds frame))
       (elapsed-time (elapsed-time frame)))
      (clock-lock frame)
    ;; Center the clock face in the pane
    (with-bounding-rectangle* (min-x min-y max-x max-y)
	(sheet-region pane)
      (let ((record
	     (with-output-recording-options (pane :record t :draw nil)
	       (with-output-as-presentation (pane elapsed-time 'clock
						  :single-box t)
		 (with-text-size (pane :huge)
		   ;; Use a table because otherwise its tricky to get the
		   ;; stream state just right for each individual
		   ;; updating-output form.
		   (formatting-table (pane :x-spacing 0)
		     (formatting-row (pane)
		       (formatting-cell (pane)
			 (updating-output (pane :unique-id 'hours
					   :cache-value hours)
			   (format pane "~D:" hours)))
		       (formatting-cell (pane)
			 (updating-output (pane :unique-id 'minutes
					   :cache-value minutes)
			   (format pane "~2,'0D:" minutes)))
		       (formatting-cell (pane)
			 (let ((secs (truncate seconds)))
			   (updating-output (pane :unique-id 'seconds
					     :cache-value secs)
			     (format pane "~2,'0D" secs)))))))))))
	(with-bounding-rectangle* (r-minx r-miny r-maxx r-maxy)
	    record
	  (let ((width (- r-maxx r-minx))
		(height (- r-maxy r-miny)))
	    (setf (output-record-position record)
		  (values (+ min-x (/ (- max-x min-x width) 2.0))
			  (+ min-y (/ (- max-y min-y height) 2.0))))))
	(replay record pane)))))

(defclass my-event (device-event)
  ()
  (:default-initargs :modifier-state 0))

;;; Paul's example, which works in Lispworks' CLIM, doesn't specialize
;;; on the client argument. Mcclim has a primary method defined on
;;; standard-sheet-input-mixin which does nothing, so we need to
;;; specialize the first argument here to override that method.
(defmethod handle-event ((client application-pane) (event my-event))
  (with-application-frame (frame)
    (redisplay-frame-pane frame client)))

(defun decode-time (internal-time)
  (multiple-value-bind (hours min-rem)
      (truncate (float (/ internal-time internal-time-units-per-second)) 3600.0)
	  (multiple-value-bind (minutes secs)
	      (truncate min-rem 60.0)
	    (values hours minutes (float secs)))))

;;; The simulation thread function.
(defun update-clock (frame client)
  (let ((tls (frame-top-level-sheet frame))
	(clock-lock (clock-lock frame)))
    (clim-sys:with-lock-held (clock-lock)
      (loop
	 (when (eq (mode frame) :exit)
	   (return-from update-clock nil))
	 (when (eq (mode frame) :stopped)
	   (clim-sys:condition-wait (condition-variable frame) clock-lock)
	   (setf (start-time frame) (get-internal-real-time)
		 (elapsed-time frame) 0))
	 (when (eq (mode frame)  :running)
	   (let ((new-time (- (get-internal-real-time) (start-time frame))))
	     (setf (elapsed-time frame) new-time)
	     (multiple-value-bind (hours minutes seconds)
		 (decode-time new-time)
	       (when (or (/= hours (hours frame))
			 (/= minutes (minutes frame))
			 (/= seconds (seconds frame)))
		 (setf (hours frame) hours 
		       (minutes frame) minutes 
		       (seconds frame) seconds)
		 (queue-event tls (make-instance 'my-event :sheet client))))
	     (clim-sys:condition-wait (condition-variable frame)
				      clock-lock
				      .1)))))))

(defmethod run-frame-top-level ((frame stopwatch) &key)
  (let ((clock-pane (find-pane-named frame 'clock)))
    (setf (clock-process frame)
	  (clim-sys:make-process #'(lambda ()
				     (update-clock frame clock-pane))
				 :name "ticker")))
  (unwind-protect
       (call-next-method)
    (when (clock-process frame)
      (clim-sys:destroy-process (clock-process frame))
      (setf (clock-process frame) nil))))

(define-stopwatch-command (com-toggle-watch :name t)
    ()
  (setf (mode *application-frame*)
	(if (eq (mode *application-frame*) :stopped)
	    :running
	    :stopped))
  (clim-sys:with-lock-held ((clock-lock *application-frame*) "clock lock")
    (clim-sys:condition-notify (condition-variable *application-frame*))))

(define-presentation-to-command-translator com-click-stopwatch
    (clock com-toggle-watch stopwatch)
    (object)
  nil)

(define-stopwatch-command (com-quit :name t)
    ()
  (setf (mode *application-frame*) :exit)
  (clim-sys:condition-notify (condition-variable *application-frame*))
  (frame-exit *application-frame*))

(defun run-stopwatch ()
  (run-frame-top-level (make-application-frame 'stopwatch)))
