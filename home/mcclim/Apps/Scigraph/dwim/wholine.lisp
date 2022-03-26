;;; -*- Syntax: Common-lisp; Package: DWIM -*-
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

(in-package :dwim)

;;; The status line is a small pane associated with a frame which provides
;;; status information, such as:
;;;  1. time of day
;;;  2. user name
;;;  3. process state, usually one of "User Input", "Run", "GC", or "Error!".
;;;  4. progress notes
;;;
;;; To use this code, you must:
;;;  a. Provide a method STATUS-PANE that takes your frame as an argument
;;;      and that returns some pane of your frame.
;;;      Such a pane needs a redisplay function.  REFRESH-STATUS-LINE is
;;;      defined below for this purpose.
;;;  b. Provide a method STATUS-LINE which returns an instance of the
;;;      status-line structure.  The best way to do this is to provide
;;;      a slot on your frame whose :initform is (MAKE-STATUS-LINE stream) and
;;;      whose :accessor is STATUS-LINE.  (The stream argument to MAKE-STATUS-LINE
;;;      should be the status pane.)
;;;  c. Initialize the status line (and for clim-0.9,
;;;     bind *frame-for-status-line*):
;;;
;;;      (defmethod dart-frame-top-level ((frame dart-frame))
;;;         (initialize-status-line)
;;;         (let ((*frame-for-status-line* frame))
;;;           (loop
;;;             (lcl:with-simple-restart
;;;                   (dart-top-level "Abort to DART Top Level")
;;;               (clim:clim-top-level frame)))))
;;;
;;;
;;;
;;; If you modify this code, be careful.  These things should happen with
;;; little or no overhead.  In addition, this code must be error free; an
;;; error while advising the GC or debugger could cause lisp to terminate
;;; itself rather hastily.

;;; default width of a field;
(defparameter *status-width* 150)

;;; leftmost positions of each field, as a percentage of pane width:
(defconstant time-left .05)
(defconstant username-left .25)
(defconstant process-left .4)
(defconstant progress-left .6)

(defparameter *include-machine-name-in-status-line-p* nil)

(defun whoami ()
  (let ((host-string (and *include-machine-name-in-status-line-p*
                          (let ((raw-host-string (getenv "HOST")))
                            (cond ((null raw-host-string) nil)
                                  ((let ((dot-pos (position #\. raw-host-string)))
                                     (if dot-pos
                                         (subseq (the string raw-host-string) 0 dot-pos)
                                         raw-host-string)))))))
        (user-string (getenv "USER")))
    (if host-string
        (concatenate 'string user-string "@" host-string)
        user-string)))


;;; frequently used strings:
(defvar empty-string " ")
(defparameter run-string    "Please Wait")
(defparameter input-string  "Ready")
(defparameter error-string  "Unexpected Condition")
(defparameter GC-string     "Reclaiming Memory")
(defparameter expand-string "Expanding Memory")

(defclass status-line ()
    ((stream :initform nil :initarg :stream :accessor status-line-stream)
     (time :initform empty-string :accessor status-line-time)
     (ptime :initform nil :accessor status-line-ptime)
     (username :initform (whoami) :accessor status-line-username)
     (pusername :initform nil :accessor status-line-pusername)
     (process :initform run-string :accessor status-line-process)
     (pprocess :initform nil :accessor status-line-pprocess)
     (progress :initform empty-string :accessor status-line-progress)
     (pprogress :initform nil :accessor status-line-pprogress)
     (thermometer :initform 0 :accessor status-line-thermometer)))

(defun make-status-line (stream) (make-instance 'status-line :stream stream))

(defvar *frame-for-status-line* nil
  "Used for progress notes, but only for clim 0.9.")

(defun frame-for-status-line ()
  (and (boundp 'clim:*application-frame*) clim:*application-frame*))

(defmethod status-pane ((any t)) nil)

(defmethod status-line ((any t)) nil)

(defmethod mouse-documentation-pane ((any t)) nil)

(defvar *status-line-sheet-lock* nil)

(defmacro sheet-lock ((window) &body body)
  "Get a lock on this window, or wait for it."
  (declare (ignore window))
  ;; Yes, yes, I know I should have a different lock for each status
  ;; pane.  But after all, there is almost always only one of them,
  ;; and the time spent updating a status line is very small indeed.
  `(clim-sys:with-lock-held (*status-line-sheet-lock*) ,@body))

(defmethod draw-status-element
	   ((status-line status-line) (element-name t) string x y stream)
  (draw-string string x y :stream stream))

(defmethod process-status-element
	   ((status-line status-line)
	    field-name presentation-name string column
	    &optional (record-p t) (status-width *status-width*))
  (let* ((stream (status-line-stream status-line))
	 presentation)
    (when (and stream status-line)
      (multiple-value-bind (left top right bottom) (stream-viewport stream)
	(setq column (truncate (* column (- right left))))
	(sheet-lock (stream)
	  (setf (slot-value status-line field-name) string)
	  (setq presentation (slot-value status-line presentation-name))
	  (setf (slot-value status-line presentation-name) nil)
	  (if (and record-p presentation)
	      (clim:erase-output-record presentation stream nil))
	  (let* ((minx column)
		 (maxx (+ column status-width))
		 (miny top)
		 (fudge-factor 15)
		 (maxy bottom))
	    (with-output-recording-disabled (stream)
	      (draw-rectangle minx maxx maxy miny :stream stream
			      :filled t :alu %erase))
	    (if record-p
		(setf (slot-value status-line presentation-name)
		      (with-output-as-presentation (:stream stream
						    :object string
						    :type 'string)
			(draw-status-element status-line field-name string
					     minx (+ miny fudge-factor) stream)))
		(with-output-recording-disabled (stream)
		  (draw-status-element status-line field-name string
				       minx (+ miny fudge-factor) stream)))
	    (force-output stream)
	    ))))))

(defmethod set-status-line ((frame t) (field (eql 'status-line-time)) string
			    &optional (record-p t))
  (let* ((status-line (status-line frame)))
    (when status-line
      (process-status-element status-line 'time 'ptime
			      string time-left record-p))))

(defmethod set-status-line ((frame t) (field (eql 'status-line-username)) string
			    &optional (record-p t))
  (let* ((status-line (status-line frame)))
    (when status-line
      (process-status-element status-line 'username 'pusername
			      string username-left record-p))))

(defmethod set-status-line ((frame t) (field (eql 'status-line-process)) string
			    &optional (record-p t))
  (let* ((status-line (status-line frame)))
    (when status-line
      (process-status-element status-line 'process 'pprocess
			      string process-left record-p))))

(defmethod set-status-line ((frame t) (field (eql 'status-line-progress)) string
			    &optional (record-p t))
  (let* ((status-line (status-line frame)))
    (when status-line
      (process-status-element status-line 'progress 'pprogress
			      string progress-left record-p 400))))

(defmacro with-status-line ((string field &optional
				    (frame '(frame-for-status-line))
				    (record-p t))
			    &body body)
  (let ((old (gensym)) (f (gensym)) (status-line (gensym)))
    `(let* ((,f ,frame)
	    (,status-line (and ,f (status-line ,f)))
	    (,old (and ,status-line (funcall ,field ,status-line))))
      (unwind-protect
	   (progn (or (not ,f) (set-status-line ,f ,field ,string ,record-p))
		  ,@body)
	(or (not ,f) (set-status-line ,f ,field ,old ,record-p))))))

(defmacro with-process-state ((string) &body body)
  `(with-status-line (,string 'status-line-process) ,@body))

(defun set-all-status-lines (string field &key how-many (record-p t))
  "Find all frames and notify them with this string"
  ;; HOW-MANY can be used to limit the number of frames notified.
  ;; This is used to limit the amount of consing.
  (let ((count 0))
    (for-each-frame (frame)
       (when (and (status-pane frame)
		  (or (not how-many) (< count how-many)))
	 (set-status-line frame field string record-p)
	 (incf count)))))

(defmethod refresh-status-line (frame pane)
  "Redisplay a pane that is a status line."
  (declare (ignore pane))
  (let* ((line (status-line frame)))
    (when line
      (dolist (field '(status-line-time
		       status-line-username
		       status-line-process
		       status-line-progress))
	(set-status-line frame field (funcall field line))))))

(defun advise (name old-name new-function)
  (unless (fboundp old-name)
    (setf (symbol-function old-name) (symbol-function name))
    (setf (symbol-function name) new-function)
    name))

(defun unadvise (name old-name)
  (when (fboundp old-name)
    (setf (symbol-function name) (symbol-function old-name))
    (fmakunbound old-name)
    name))

(defun string-for-process-whostate (process)
  (declare (ignorable process))
  (let ((whostate
	 #+allegro (mp:process-whostate process)))
    (if (and whostate (search "Input" whostate :test #'equalp))
	input-string run-string)))

(defun repair-unbelievable-status-lines ()
  ;; Sometimes the status line does not get reset properly,
  ;; particularly after a GC event.  So this
  ;; function is used by the clock process to repair mistakes.
  (for-each-frame (frame)
    (when (status-pane frame)
      (set-status-line
	frame
	'status-line-process
	(let* ((p (frame-top-level-process frame)))
	  (if p (string-for-process-whostate p) "no process"))))))

(defvar *time-type* :normal
  "user-customizable feature")

(defvar *months*
  (vector nil "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
	  "Sep" "Oct" "Nov" "Dec"))

(defun integer-string (integer)
  "Stringify an integer (without using the pretty printer)."
  ;; We seem to be having some wierd lucid bug involving the
  ;; pretty printer, so don't pretty print.
  (let* ((posint (abs integer))
	 (order (if (zerop integer) 1
		    (+ (truncate (log posint 10))
		       (if (minusp integer) 2 1))))
	 (string (make-string order :initial-element #\0))
	 digit)
    (when (minusp integer)
      (setf (elt string 0) #\-))
    (loop
	(if (zerop posint) (return))
      (multiple-value-setq (posint digit) (truncate posint 10))
      (decf order)
      (setf (elt string order) (code-char (+ digit 48))))
    string))

(defun time-string (&optional (type *time-type*))
  ;; This used to be done with ~D format directives, but there
  ;; seems to be some kind of lucid bug that I can't identify.
  (multiple-value-bind (sec min hour day month) (get-decoded-time)
    (declare (ignore sec))
    (ecase type
      ((:normal :12-hour)
       (let ((morning (< hour 12)))
	 (setq hour (cond ((> hour 12) (- hour 12))
			  ((= hour 0) 12)
			  (t hour)))
	 (concatenate 'string
		      (aref *months* month)
		      " "
		      (integer-string day)
		      " "
		      (integer-string hour)
		      ":"
		      (if (<= min 9) "0" "")
		      (integer-string min)
		      (if morning "am" "pm"))))
      ((:military :24-hour)
       (concatenate 'string
		    (integer-string day)
		    " "
		    (aref *months* month)
		    " "
		    (integer-string hour)
		    (if (<= min 9) ":0" ":")
		    (integer-string min))))))

(defvar *clock-quantum* 20
  "Seconds between clock ticks.")

(defun clock-top-level (&optional (quantum *clock-quantum*))
  "What the clock process actually does."
  (loop
    (ignore-errors
     ;; Ignore them because they arise unavoidably at times when
     ;; the number of frames is changing and when somebody bypasses
     ;; my simple window lock.  You might miss one clock cycle but
     ;; the next one will (probably) work fine.
     (set-all-status-lines (time-string) 'status-line-time)
     (sleep quantum))))

(let ((clock-process nil))
  (defun start-clock ()
    (or clock-process
	(setq clock-process
	      (process-run-function "Clock Process" 'clock-top-level))))
  (defun clock () clock-process)
  (defun stop-clock ()
    (let ((process clock-process))
      (when process
	(clim-sys:destroy-process process)
	(setq clock-process nil))))
  )

;;;
;;; The application must initialize this facility explicitly at run time
;;; by calling the function (INITIALIZE-STATUS-LINE).  Do not initialize
;;; at load time, that is too early.
;;;

(defun realize-username (&optional (string (whoami)))
  "Update all status lines with the current username."
  (set-all-status-lines string 'status-line-username))

(defun initialize-status-line ()
  "Do this once at run time to get everything started."
  (advise-debugger)
  (advise-read-frame)
  (advise-menus)
  (start-clock)
  (realize-username))

(defun halt-status-line ()
  "Undo the side effects of INITIALIZE-STATUS-LINE."
  (unadvise-debugger)
  (unadvise-read-frame)
  (stop-clock))

(defmacro noting-progress ((string) &body body)
  "Place STRING in the right side of the current status pane."
  `(let ((frame (frame-for-status-line)))
     (if (and frame (status-line frame))
	 (with-status-line (,string 'status-line-progress frame)
	   (unwind-protect (progn ,@body)
	     (note-progress 0.0 1.0 frame)))
	 (progn ,@body))))

(defun note-progress (numerator &optional
		      (denominator 1.0)
		      (frame (frame-for-status-line)))
  "Move the status line progress thermometer."
  (let ((status-line (and frame (status-line frame))))
    (if status-line
	(when (not (eq (status-line-progress status-line) empty-string))
	  (let ((stream (status-line-stream status-line))
		(old-therm (status-line-thermometer status-line))
		(new-therm (max 0.0 (min (float (/ numerator denominator)) 1.0))))
	    (when (and stream (not (eql old-therm new-therm)))
	      (setf (status-line-thermometer status-line) new-therm)
	      (multiple-value-bind (left top right) (stream-viewport stream)
		(let* ((column (truncate (* progress-left (- right left))))
		       (x column)
		       (y (+ top 20))
		       (width (- right left)))
		  (with-output-recording-disabled (stream)
		    (when (< new-therm old-therm)
		      (draw-line x y
				 (+ x (* old-therm (- width x))) y
				 :stream stream :alu %erase))
		    (when (and (plusp new-therm)
			       (> new-therm old-therm))
		      (draw-line (+ x (* old-therm (- width x))) y
				 (+ x (* new-therm (- width x))) y
				 :stream stream :alu %draw)
		      ;; KRA 09JUL93: JM had this f-o commented out.  However,
		      ;; it lets user see actual progress.  If this is too
		      ;; slow we should be smarter about drawing fewer lines.
		      (force-output stream)))))))))))

;;;
;;; Modify the underlying system.
;;;

(defun advise-menus ()
  "Modify menus so that the process state is 'Ready'"
  (advise 'clim::menu-choose-from-drawer
	  'old-menu-choose-from-drawer
	  #'(lambda (&rest arguments)
	      (with-process-state (input-string)
		(apply 'old-menu-choose-from-drawer arguments)))))

(defmethod clim:read-frame-command :around ((frame t) &key stream)
  (declare (ignore stream))
  (with-process-state (input-string) (call-next-method)))

(defmethod clim:execute-frame-command :around ((frame t) command)
  (declare (ignore command))
  (with-process-state (run-string) (call-next-method)))

(defun advise-read-frame ()
  (advise 'clim-internals::invoke-accepting-values
	  'old-invoke-accepting-values
	  #'(lambda (&rest arguments)
	      (with-process-state (input-string)
		(apply 'old-invoke-accepting-values arguments)))))

(defun unadvise-read-frame ()
  (unadvise 'clim-internals::invoke-accepting-values
	    'old-invoke-accepting-values))


(defvar *panic* nil			; dont panic yet
  "Dynamically bound to prevent recursively entering the debugger.")

(defvar *debugger-name* nil )

(defun unadvise-debugger ()
  (unadvise *debugger-name* 'old-invoke-debugger))

(defun advise-debugger ()
  "Modify debugger behavior such that if we fall into the debugger,
   all our applications find out via the process status string."

  (unadvise-debugger)

  (when *debugger-name*
    (advise
     *debugger-name*
     'old-invoke-debugger
     #'(lambda (&rest arguments)
	 (if *panic*
	     (apply #'invoke-debugger arguments)
	   (let* ((*panic* t)
		  (*standard-input* *terminal-io*)
		  (*standard-output* *standard-input*)
		  (*query-io* *standard-input*)
		  (*terminal-io* *standard-input*)
		  (*trace-output* *standard-input*))
	     (unwind-protect
		 (progn
		   (ignore-errors
		    (set-all-status-lines error-string 'status-line-process))
		   (apply #'invoke-debugger arguments))
	       (ignore-errors
		(set-all-status-lines run-string 'status-line-process)
		))))))))
