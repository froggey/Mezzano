;;; -*- Mode: Lisp; Package: ESA -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2006-2007 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Emacs-Style Application

(in-package :esa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Querying ESAs.

(defvar *esa-instance* nil
  "This symbol should be bound to an ESA instance, though any
object will do, provided the proper methods are defined. It will
be used as the argument to the various \"query\" functions
defined by ESA. For the vast majority of ESAs, `*esa-instance*'
will probably have the same value as `*application-frame*'.")

(defgeneric buffers (esa)
  (:documentation "Return a list of all the buffers of the application."))

(defgeneric esa-current-buffer (esa)
  (:documentation "Return the current buffer of the ESA instance ESA."))

(defgeneric (setf esa-current-buffer) (new-buffer esa)
  (:documentation
   #.(format nil "Replace the current buffer of the ESA instance~@
                  ESA with NEW-BUFFER.")))

(defun current-buffer ()
  "Return the currently active buffer of the running ESA."
  (esa-current-buffer *esa-instance*))

(defun (setf current-buffer) (new-buffer)
  #.(format nil "Replace the current buffer of the current running~@
                 ESA instance with NEW-BUFFER.")
  (setf (esa-current-buffer *esa-instance*) new-buffer))

(defgeneric windows (esa)
  (:documentation "Return a list of all the windows of the ESA.")
  (:method ((esa application-frame))
    '()))

(defgeneric esa-current-window (esa)
  (:documentation "Return the currently active window of ESA."))

(defun current-window ()
  "Return the currently active window of the running ESA instance."
  (esa-current-window *esa-instance*))

(defgeneric esa-command-table (esa)
  (:documentation "Return command table of ESA."))

(defvar *previous-command* nil
  #.(format nil "When a command is being executed, the command~@
                 previously executed by the application."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Info pane, a pane that displays some information about another pane

(defclass info-pane (application-pane)
  ((master-pane :initarg :master-pane :reader master-pane))
  (:default-initargs
   :background +gray85+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Minibuffer pane

(defgeneric minibuffer (application-frame)
  (:documentation "Return the minibuffer of APPLICATION-FRAME."))

(defvar *minibuffer* nil
  "The minibuffer pane of the running application.")

(defvar *minimum-message-time* 1
  "The minimum number of seconds a minibuffer message will be
  displayed." )

(defclass minibuffer-pane (application-pane)
  ((message :initform nil
            :accessor message
            :documentation "An output record containing whatever
message is supposed to be displayed in the minibuffer.")
   (message-time :initform 0
                 :accessor message-time
                 :documentation "The universal time at which the
current message was set."))
  (:default-initargs
   :display-function 'display-minibuffer
   :display-time :command-loop
   :incremental-redisplay t))

(defmethod handle-repaint ((pane minibuffer-pane) region)
  (when (and (message pane)
             (> (get-universal-time)
                (+ *minimum-message-time* (message-time pane))))
    (window-clear pane)
    (setf (message pane) nil))
  (call-next-method))

(defmethod (setf message) :after (new-value (pane minibuffer-pane))
  (change-space-requirements pane))

(defmethod pane-needs-redisplay ((pane minibuffer-pane))
  ;; Always call the display function, never clear the window. This
  ;; allows us to time-out the message in the minibuffer.
  (values t nil))

(defun display-minibuffer (frame pane)
  (declare (ignore frame))
  (handle-repaint pane +everywhere+))

(defmethod stream-accept :around ((pane minibuffer-pane) type &rest args)
  (declare (ignore args))
  (when (message pane)
    (setf (message pane) nil))
  (window-clear pane)
  ;; FIXME: this isn't the friendliest way of indicating a parse
  ;; error: there's no feedback, unlike emacs' quite nice "[no
  ;; match]".
  (unwind-protect
       (loop
        (handler-case
            (with-input-focus (pane)
              (return (call-next-method)))
          (parse-error () nil)))
    (window-clear pane)))

(defmethod stream-accept ((pane minibuffer-pane) type &rest args
                          &key (view (stream-default-view pane))
                          &allow-other-keys)
  ;; default CLIM prompting is OK for now...
  (apply #'prompt-for-accept pane type view args)
  ;; but we need to turn some of ACCEPT-1 off.
  (apply #'accept-1-for-minibuffer pane type args))

(defmethod compose-space ((pane minibuffer-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let* ((sr (call-next-method))
           (height (max (text-style-height (medium-merged-text-style medium)
                                           medium)
                        (bounding-rectangle-height (stream-output-history pane)))))
      (make-space-requirement
       :height height :min-height height :max-height height
       :width (space-requirement-width sr)
       :min-width (space-requirement-min-width sr)
       :max-width (space-requirement-max-width sr)))))

;;; simpler version of McCLIM's internal operators of the same names:
;;; HANDLE-EMPTY-INPUT to make default processing work, EMPTY-INPUT-P
;;; and INVOKE-HANDLE-EMPTY-INPUT to support it.  We don't support
;;; recursive bouncing to see who most wants to handle the empty
;;; input, but that's OK, because we are always conceptually one-level
;;; deep in accept (even if sometimes we call ACCEPT recursively for
;;; e.g. command-names and arguments).
(defmacro handle-empty-input ((stream) input-form &body handler-forms)
  "see climi::handle-empty-input"
  (let ((input-cont (gensym "INPUT-CONT"))
        (handler-cont (gensym "HANDLER-CONT")))
    `(flet ((,input-cont ()
	      ,input-form)
	    (,handler-cont ()
	      ,@handler-forms))
       (declare (dynamic-extent #',input-cont #',handler-cont))
       (invoke-handle-empty-input ,stream #',input-cont #',handler-cont))))

;;; The code that signalled the error might have consumed the gesture, or
;;; not.
;;; XXX Actually, it would be a violation of the `accept' protocol to consume
;;; the gesture, but who knows what random accept methods are doing.
(defun empty-input-p
    (stream begin-scan-pointer activation-gestures delimiter-gestures)
  (let ((scan-pointer (stream-scan-pointer stream))
	(fill-pointer (fill-pointer (stream-input-buffer stream))))
    ;; activated?
    (cond ((and (eql begin-scan-pointer scan-pointer)
		(eql scan-pointer fill-pointer))
	   t)
	  ((or (eql begin-scan-pointer scan-pointer)
	       (eql begin-scan-pointer (1- scan-pointer)))
	   (let ((gesture 
                  (aref (stream-input-buffer stream) begin-scan-pointer)))
	     (and (characterp gesture)
                  (flet ((gesture-matches-p (g)
                           (if (characterp g)
                               (char= gesture g)
                               ;; FIXME: not quite portable --
                               ;; apparently
                               ;; EVENT-MATCHES-GESTURE-NAME-P need
                               ;; not work on raw characters
                               (event-matches-gesture-name-p gesture g))))
                    (or (some #'gesture-matches-p activation-gestures)
                        (some #'gesture-matches-p delimiter-gestures))))))
	  (t nil))))

(defun invoke-handle-empty-input
    (stream input-continuation handler-continuation)
  (unless (input-editing-stream-p stream)
    (return-from invoke-handle-empty-input (funcall input-continuation)))
  (let ((begin-scan-pointer (stream-scan-pointer stream))
	(activation-gestures *activation-gestures*)
	(delimiter-gestures *delimiter-gestures*))
    (block empty-input
      (handler-bind 
          ((parse-error
            #'(lambda (c)
                (declare (ignore c))
                (when (empty-input-p stream begin-scan-pointer 
                                     activation-gestures delimiter-gestures)
                  (return-from empty-input nil)))))
	(return-from invoke-handle-empty-input (funcall input-continuation))))
    (funcall handler-continuation)))

(defun accept-1-for-minibuffer
    (stream type
     &key
       (view (stream-default-view stream))
       (default nil defaultp) (default-type nil default-type-p)
       provide-default insert-default (replace-input t)
       history active-p prompt prompt-mode display-default
       query-identifier (activation-gestures nil activationsp)
       (additional-activation-gestures nil additional-activations-p)
       (delimiter-gestures nil delimitersp)
       (additional-delimiter-gestures nil  additional-delimiters-p))
  (declare (ignore provide-default history active-p
		   prompt prompt-mode
		   display-default query-identifier))
  (when (and defaultp (not default-type-p))
    (error ":default specified without :default-type"))
  (when (and activationsp additional-activations-p)
    (error "only one of :activation-gestures or ~
            :additional-activation-gestures may be passed to accept."))
  (unless (or activationsp additional-activations-p *activation-gestures*)
    (setq activation-gestures *standard-activation-gestures*))
  (with-input-editing 
      ;; this is the main change from CLIM:ACCEPT-1 -- no sensitizer.
      (stream :input-sensitizer nil)
    ;; KLUDGE: no call to CLIMI::WITH-INPUT-POSITION here, but that's
    ;; OK because we are always going to create a new editing stream
    ;; for each call to accept/accept-1-for-minibuffer, so the default
    ;; default for the BUFFER-START argument to REPLACE-INPUT is
    ;; right.
    (when (and insert-default
               (not (stream-rescanning-p stream)))
      ;; Insert the default value to the input stream. It should
      ;; become fully keyboard-editable. We do not want to insert
      ;; the default if we're rescanning, only during initial
      ;; setup.
      (presentation-replace-input stream default default-type view))
    (with-input-context (type)
      (object object-type event options)
      (with-activation-gestures ((if additional-activations-p
				     additional-activation-gestures
				     activation-gestures)
				 :override activationsp)
	(with-delimiter-gestures ((if additional-delimiters-p
				      additional-delimiter-gestures
				      delimiter-gestures)
				  :override delimitersp)
	  (let ((accept-results nil))
	    (climi::handle-empty-input
	     (stream)
	     (setq accept-results
		   (multiple-value-list
		    (if defaultp
			(funcall-presentation-generic-function
			 accept type stream view
			 :default default :default-type default-type)
			(funcall-presentation-generic-function
			 accept type stream view))))
	     ;; User entered activation or delimiter gesture
	     ;; without any input.
	     (if defaultp
		 (presentation-replace-input
		  stream default default-type view :rescan nil)
		 (simple-parse-error
		  "Empty input for type ~S with no supplied default"
		  type))
	     (setq accept-results (list default default-type)))
	    ;; Eat trailing activation gesture
	    ;; XXX what about pointer gestures?
	    ;; XXX and delimiter gestures?
	    ;;
	    ;; deleted check for *RECURSIVE-ACCEPT-P*
	    (let ((ag (read-char-no-hang stream nil stream t)))
	      (unless (or (null ag) (eq ag stream))
		(unless (activation-gesture-p ag)
		  (unread-char ag stream))))
	    (values (car accept-results)
		    (if (cdr accept-results) (cadr accept-results) type)))))
      ;; A presentation was clicked on, or something.
      (t
       (when (and replace-input 
                  (getf options :echo t)
                  (not (stream-rescanning-p stream)))
         (presentation-replace-input 
          stream object object-type view :rescan nil))
       (values object object-type)))))

(defgeneric invoke-with-minibuffer-stream (minibuffer continuation))

(defmethod invoke-with-minibuffer-stream ((minibuffer minibuffer-pane) continuation)
  (window-clear minibuffer)
  (setf (message minibuffer)
        (with-new-output-record (minibuffer)
          (setf (message-time minibuffer) (get-universal-time))
          (filling-output (minibuffer :fill-width (bounding-rectangle-width minibuffer))
            (funcall continuation minibuffer)))))

(defmethod invoke-with-minibuffer-stream ((minibuffer pointer-documentation-pane) continuation)
  (clim-extensions:with-output-to-pointer-documentation (stream (pane-frame minibuffer))
    (funcall continuation stream)))

(defmethod invoke-with-minibuffer-stream ((minibuffer null) continuation)
  nil)

(defmacro with-minibuffer-stream ((stream-symbol)
                                  &body body)
  "Bind `stream-symbol' to the minibuffer stream and evaluate
  `body'. This macro makes sure to setup the initial blanking of
  the minibuffer as well as taking care of for how long the
  message should be displayed."
  `(invoke-with-minibuffer-stream *minibuffer*
                                  #'(lambda (,stream-symbol)
                                      ,@body)))

(defun display-message (format-string &rest format-args)
  "Display a message in the minibuffer. Composes the string based
on the `format-string' and the `format-args'."
  (with-minibuffer-stream (minibuffer)
    (apply #'format minibuffer format-string format-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ESA pane mixin

(defclass esa-pane-mixin ()
  (;; allows a certain number of commands to have some minimal memory
   (previous-command :initform nil :accessor previous-command)
   (command-table :initarg :command-table :accessor esa-command-table)))

(defmethod previous-command ((pane pane))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Command processing

(defparameter *esa-abort-gestures* `((:keyboard #\g ,(make-modifier-state :control))))

(defparameter *current-gesture* nil)

(defparameter *command-processor* nil
  "While a command is being run, this symbol will be dynamically
bound to the current command processor.")

(defun find-gestures (gestures start-table)
  (loop with table = (find-command-table start-table)
	for (gesture . rest) on gestures
	for item = (find-keystroke-item  gesture table :errorp nil)
	while item
	do (if (eq (command-menu-item-type item) :command)
	       (return (if (null rest) item nil))
	       (setf table (command-menu-item-value item)))
	finally (return item)))

(defun find-gestures-with-inheritance (gestures start-table)
  (or (find-gestures gestures start-table)
      (some (lambda (table)
	      (find-gestures-with-inheritance gestures table))
	    (command-table-inherit-from
	     (find-command-table start-table)))))


(defun gesture-matches-gesture-name-p (gesture gesture-name)
  (event-matches-gesture-name-p gesture gesture-name))

(defvar *meta-digit-table*
  (loop for i from 0 to 9
	collect (list :keyboard (digit-char i) (make-modifier-state :meta))))

(defun meta-digit (gesture)
  (position gesture *meta-digit-table*
	    :test #'gesture-matches-gesture-name-p))

(defun proper-gesture-p (gesture)
  "Return non-NIL if `gesture' is a proper gesture, NIL
otherwise. A proper gesture is loosely defined as any gesture
that is not just the sole pressing of a modifier key."
  (or (characterp gesture)
      (and (typep gesture 'keyboard-event)
           (or (keyboard-event-character gesture)
               (not (member (keyboard-event-key-name
                             gesture)
                            '(:control-left :control-right
                              :shift-left :shift-right
                              :meta-left :meta-right
                              :super-left :super-right
                              :hyper-left :hyper-right
                              :shift-lock :caps-lock
                              :alt-left :alt-right)))))))

(define-condition unbound-gesture-sequence (simple-condition)
  ((%gestures :initarg :gestures
              :reader gestures
              :initform '()
              :documentation "A list of the provided gestures
that resulted in the signalling of this condition."))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Gesture sequence that cannot
possibly result in command invocation encountered.")))
  (:documentation "This condition is signalled during gesture
processing, when a sequence of gestures has been entered that
does not, and cannot by the addition of more gestures, result in
preferring to a command."))

(defclass command-processor ()
  ((%recordingp :initform nil :accessor recordingp)
   (%executingp :initform nil :accessor executingp)
   (%recorded-keys :initform '() :accessor recorded-keys)
   (%remaining-keys :initform '() :accessor remaining-keys)
   (%accumulated-gestures :initform '() :accessor accumulated-gestures)
   (%overriding-handler :initform nil
                      :accessor overriding-handler
                      :documentation "When non-NIL, any action on
the command processor will be forwarded to this object.")
   (%command-executor :initform 'execute-frame-command
                      :accessor command-executor
                      :initarg :command-executor
                      :documentation "The object used to execute
commands. Will be coerced to a function and called with two
arguments, the command processor and the command."))
  (:documentation "The command processor is fed gestures and will
execute commands or signal conditions when the provided getures
unambigiously suggest one of these actions. ESA command
processing works through instances of this class."))

(defgeneric process-gesture (command-processor gesture)
  (:documentation "Tell the command processor to process
`gesture'. This might result in either the execution of a command
or the signalling of `unbound-gesture-sequence'. This is the
fundamental interface to the command processor."))

(defgeneric directly-processing-p (command-processor)
  (:documentation "Return true if `command-processor' is directly
  processing commands. In most cases, this means that
  `overriding-handler' is null.")
  (:method ((command-processor command-processor))
    (null (overriding-handler command-processor))))

(defgeneric command-for-unbound-gestures (thing gestures)
  (:documentation "Called when `gestures' is input by the user
and there is no associated command in the current command
table. The function should return either a (possibly incomplete)
command or NIL. In the latter case (which is handled by a default
method), the gestures will be treated as actual unbound
gestures. `Thing' is something that might be interested in
commands, at the beginning usually a command processor, but it
can call the function for other objects it knows in order to get
their opinion. `Gestures' is a list of gestures.")
  (:method (thing gestures)
    nil))

(defclass instant-macro-execution-mixin ()
  ()
  (:documentation "Subclasses of this class will immediately
  process the gestures of a macro when macro processing is
  started by setting `executingp'. This is essential for
  event-based command processing schemes."))

(defmethod (setf executingp) :after ((new-val (eql t)) (drei instant-macro-execution-mixin))
  (loop until (null (remaining-keys drei))
	for gesture = (pop (remaining-keys drei))
	do (process-gesture drei gesture)
	finally (setf (executingp drei) nil)))

(defclass asynchronous-command-processor (command-processor
                                          instant-macro-execution-mixin)
  ()
  (:documentation "Helper class that provides behavior necessary
for a command processor that expects to receive gestures through
asynchronous event handling, and not through
`esa-read-gesture'."))

(defmethod process-gesture :before ((command-processor asynchronous-command-processor) gesture)
  (when (and (find gesture *abort-gestures*
              :test #'gesture-matches-gesture-name-p)
             (directly-processing-p command-processor))
    (setf (accumulated-gestures command-processor) nil)
    (signal 'abort-gesture :event gesture)))

(defclass dead-key-merging-command-processor (command-processor)
  ((%dead-key-state :accessor dead-key-state
                    :initform nil
                    :documentation "The state of dead key
handling as per `merging-dead-keys'."))
  (:documentation "Helper class useful for asynchronous command
processors, merges incoming dead keys with the following key."))

(defmethod process-gesture :around ((command-processor dead-key-merging-command-processor) gesture)
  (merging-dead-keys (gesture (dead-key-state command-processor))
    (call-next-method command-processor gesture)))

(defclass command-loop-command-processor (command-processor)
  ((%command-table :reader esa-command-table
                   :initarg :command-table
                   :initform nil)
   (%end-condition :reader end-condition
                   :initarg :end-condition
                   :initform (constantly nil)
                   :documentation "When this function of zero
arguments returns true, the `command-loop-command-processor' will
disable itself in its associated super command processor and call
its `end-function', effectively dropping out of the
sub-command-loop.")
   (%end-function :reader end-function
                  :initarg :end-function
                  :initform (constantly nil)
                  :documentation "This function of zero arguments
will be called when the command processor disables itself.")
   (%abort-function :reader abort-function
                    :initarg :abort-function
                    :initform (constantly nil)
                    :documentation "This function is called if
the command processor encounters an abort gesture.")
   (%super-command-processor :reader super-command-processor
                             :initarg :super-command-processor
                             :initform (error "Must provide a super command processor.")
                             :documentation "The command
processor that the `command-loop-command-processor' object
handles gestures for."))
  (:default-initargs
   :command-executor #'(lambda (processor command)
                         (funcall
                          (command-executor (super-command-processor processor))
                          (super-command-processor processor)
                          command)))
  (:documentation "This class is used to run sub-command-loops
within the primary command loop of an application (for example,
to do stuff such as incremental search)."))

(defgeneric end-command-loop (command-processor)
  (:documentation "End the simulated command loop controlled by
`command-processor'.")
  (:method ((command-processor command-processor))
    nil))

(defmethod end-command-loop ((command-processor command-loop-command-processor))
  (when (overriding-handler command-processor)
    (end-command-loop (overriding-handler command-processor)))
  (setf (overriding-handler (super-command-processor command-processor)) nil))

(defmethod process-gesture :around ((command-processor command-loop-command-processor) gesture)
  (cond ((find gesture *abort-gestures*
          :test #'gesture-matches-gesture-name-p)
         ;; It is to be expected that the abort function might signal
         ;; `abort-gesture'. If that happens, we must end the command
         ;; loop, but ONLY if this is signalled.
         (handler-case (funcall (abort-function command-processor))
           (abort-gesture (c)
             (end-command-loop command-processor)
             (signal c))))
        (t
         (call-next-method)
         (when (funcall (end-condition command-processor))
           (funcall (end-function command-processor))
           (end-command-loop command-processor)))))

(defun process-gestures-for-numeric-argument (gestures)
  "Processes a list of gestures for numeric argument
information. Returns three values: prefix argument, a bool value
indicating whether prefix was given and a list of remaining
gestures to handle. Accepts: EITHER C-u, optionally followed by
other C-u's, optionally followed by a minus sign, optionally
followed by decimal digits; OR An optional M-minus, optionally
followed by M-decimal-digits.  You cannot mix C-u and M-digits.
C-u gives a numarg of 4. Additional C-u's multiply by 4 (e.g. C-u
C-u C-u = 64).  After C-u you can enter decimal digits, possibly
preceded by a minus (but not a plus) sign. C-u 3 4 = 34, C-u - 3
4 = -34. Note that C-u 3 - prints 3 '-'s.  M-1 M-2 = 12. M-- M-1
M-2 = -12. As a special case, C-u - and M-- = -1.  In the absence
of a prefix arg returns 1 (and nil)."
  (let ((first-gesture (pop gestures)))
    (cond ((gesture-matches-gesture-name-p
	    first-gesture 'universal-argument)
	   (let ((numarg 4))
	     (loop for gesture = (first gestures)
		   while (gesture-matches-gesture-name-p
			  gesture 'universal-argument)
		   do (setf numarg (* 4 numarg))
		      (pop gestures))
	     (let ((gesture (pop gestures))
		   (sign +1))
	       (when (and (characterp gesture)
			  (char= gesture #\-))
		 (setf gesture (pop gestures)
		       sign -1))
               (cond ((and (characterp gesture)
			   (digit-char-p gesture 10))
                      (setf numarg (digit-char-p gesture 10))
		      (loop for gesture = (first gestures)
			    while (and (characterp gesture)
				       (digit-char-p gesture 10))
			    do (setf numarg (+ (* 10 numarg)
					       (digit-char-p gesture 10)))
			       (pop gestures)
			    finally (return (values (* numarg sign) t gestures))))
		     (t
		      (values (if (minusp sign) -1 numarg) t
                              (when gesture
                                (cons gesture gestures))))))))
	  ((or (meta-digit first-gesture)
	       (gesture-matches-gesture-name-p
		first-gesture 'meta-minus))
	   (let ((numarg 0)
		 (sign +1))
	     (cond ((meta-digit first-gesture)
		    (setf numarg (meta-digit first-gesture)))
		   (t (setf sign -1)))
	     (loop for gesture = (first gestures)
		   while (meta-digit gesture)
		   do (setf numarg (+ (* 10 numarg) (meta-digit gesture)))
		      (pop gestures)
		   finally (return (values (if (and (= sign -1) (= numarg 0))
					       -1
					       (* sign numarg))
					   t gestures)))))
	  (t (values 1 nil (when first-gesture
                             (cons first-gesture gestures)))))))

(defgeneric process-gestures (command-processor)
  (:documentation "Process the gestures accumulated in
`command-processor', returning T if there are no gestures
accumulated or the accumulated gestures correspond to a
command. In this case, the command will also be executed and the
list of accumulated gestures set to NIL. Will return NIL if the
accumulated gestures do not yet correspond to a command, but
eventually could, if more gestures are provided. Signals
`unbound-gesture-sequence' if the accumulated gestures could
never refer to a command."))

(defmethod process-gestures ((command-processor command-processor))
  (multiple-value-bind (prefix-arg prefix-p gestures)
      (process-gestures-for-numeric-argument
       (accumulated-gestures command-processor))
    (flet ((commandp (object)
             (or (listp object) (symbolp object))))
      (cond ((null gestures)
             t)
            (t
             (let* ((command-table (esa-command-table command-processor))
                    (item (or (find-gestures-with-inheritance gestures command-table)
                              (command-for-unbound-gestures command-processor gestures))))
               (cond 
                 ((not item)
                  (setf (accumulated-gestures command-processor) nil)
                  (error 'unbound-gesture-sequence :gestures gestures))
                 ((or (commandp item) ; c-f-u-g does not return a menu-item.
                      (eq (command-menu-item-type item) :command))
                  (let ((command (if (commandp item) item
                                     (command-menu-item-value item)))
                        (*current-gesture* (first (last gestures)))
                        (*standard-input* (or *minibuffer* *standard-input*)))
                    (unless (consp command)
                      (setf command (list command)))
                    ;; Call `*partial-command-parser*' to handle numeric
                    ;; argument.
                    (unwind-protect 
                         (setq command
                               (funcall *partial-command-parser*
                                        (esa-command-table command-processor)
                                        *standard-input*
                                        command 0 (when prefix-p prefix-arg)))
                      ;; If we are macrorecording, store whatever the user
                      ;; did to invoke this command.
                      (when (recordingp command-processor)
                        (setf (recorded-keys command-processor)
                              (append (accumulated-gestures command-processor)
                                      (recorded-keys command-processor))))
                      (setf (accumulated-gestures command-processor) nil))
                    (funcall (command-executor command-processor) command-processor command)
                    nil))
                 (t t))))))))

(defmethod process-gesture :around ((command-processor command-processor) gesture)
  (with-accessors ((overriding-handler overriding-handler)) command-processor
    (if overriding-handler
        (let ((*command-processor* overriding-handler))
          (process-gesture overriding-handler gesture))
        (call-next-method))))

(defmethod process-gesture ((command-processor command-processor) gesture)
  (setf (accumulated-gestures command-processor)
        (nconc (accumulated-gestures command-processor)
               (list gesture)))
  (process-gestures command-processor))

(defun esa-read-gesture (&key (command-processor *command-processor*)
                         (stream *standard-input*))
  (unless (null (remaining-keys command-processor))
    (return-from esa-read-gesture
      (pop (remaining-keys command-processor))))
  (loop for gesture = (read-gesture :stream stream)
	until (proper-gesture-p gesture)
	finally (return gesture)))

(defun esa-unread-gesture (gesture &key (command-processor *command-processor*)
                           (stream *standard-input*))
  (cond ((recordingp command-processor)
         (cond ((equal (first (recorded-keys command-processor)) gesture)
                (pop (recorded-keys command-processor)))
               ((equal (first (accumulated-gestures command-processor)) gesture)
                (pop (accumulated-gestures command-processor))))
	 (unread-gesture gesture :stream stream))
	((executingp command-processor)
	 (push gesture (remaining-keys command-processor)))
	(t 
	 (unread-gesture gesture :stream stream))))

(define-gesture-name universal-argument :keyboard (#\u :control))

(define-gesture-name meta-minus :keyboard (#\- :meta))

(defgeneric process-gestures-or-command (command-processor)
  (:documentation "Process gestures for
`command-processor' (typically an application frame), look up the
corresponding commands in `command-table' and invoke them using
`command-executor'."))

(defmethod process-gestures-or-command :around ((command-processor application-frame))
  (with-input-context 
      ('menu-item)
      (object)
      (with-input-context 
          (`(command :command-table ,(esa-command-table command-processor)))
          (object)
          (call-next-method)
        (command
         (funcall (command-executor command-processor)
                  command-processor object)))
    (menu-item
     (let ((command (command-menu-item-value object)))
       (unless (listp command)
         (setq command (list command)))       
       (when (member *unsupplied-argument-marker* command :test #'eq)
         (setq command
               (funcall
                *partial-command-parser*
                (esa-command-table command-processor)
                *standard-input* command 0)))
       (funcall (command-executor command-processor)
                command-processor command)))))

(defmethod process-gestures-or-command :around ((command-processor command-processor))
  (handler-case (call-next-method)
    (abort-gesture (c)
      ;; If the user aborts, we want to forget whatever previous
      ;; gestures he entered since the last command execution.
      (setf (accumulated-gestures command-processor) nil)
      (signal c))))

(defmethod process-gestures-or-command ((command-processor command-processor))
  ;; Build up a list of gestures and repeatedly pass them to
  ;; `process-gestures'. This "clumsy" approach is chosen because we
  ;; want ESA command processing to support asynchronous operation as
  ;; well, something that either requires this kind of repeated
  ;; rescanning of accumulated input data or some yet-unimplemented
  ;; complex state retaining mechanism (such as continuations).
  (loop (let ((*current-gesture* (esa-read-gesture :command-processor command-processor)))
          (unless (process-gesture command-processor *current-gesture*)
            (return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ESA frame mixin

(defclass esa-frame-mixin (command-processor)
  ((windows :accessor windows)))

(defmethod esa-current-buffer ((esa esa-frame-mixin))
  (first (buffers esa)))

(defmethod esa-current-window ((esa esa-frame-mixin))
  (first (windows esa)))

(defmethod esa-command-table ((frame esa-frame-mixin))
  (find-applicable-command-table frame))

;; Defaults for non-ESA-frames.
(defmethod recordingp ((frame application-frame))
  nil)

(defmethod executingp ((frame application-frame))
  nil)

(defmethod recorded-keys ((frame application-frame))
  nil)

(defmethod remaining-keys ((frame application-frame))
  nil)

(defmethod minibuffer ((application-frame esa-frame-mixin))
  (frame-standard-input application-frame))

(defmethod redisplay-frame-panes :around ((frame esa-frame-mixin) &key force-p)
  (declare (ignore force-p))
  (when (null (remaining-keys frame))
    (setf (executingp frame) nil)
    (call-next-method)))

(defmethod execute-frame-command :after ((frame esa-frame-mixin) command)
  ;; FIXME: I'm not sure that we want to do this for commands sent
  ;; from other threads; we almost certainly don't want to do it twice
  ;; in such cases...
  (setf (previous-command (esa-current-window frame)) command))

(defmethod execute-frame-command :around ((frame esa-frame-mixin) command)
  (call-next-method)
  (when (eq frame *application-frame*)
    (redisplay-frame-panes frame)))

(defgeneric find-applicable-command-table (frame)
  (:documentation "Return the command table object that commands
on `frame' should be found in."))

(defmethod find-applicable-command-table ((frame esa-frame-mixin))
  (esa-command-table (car (windows frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Top level

(defvar *extended-command-prompt*
  "The prompt used when querying the user for an extended
command. This only applies when the ESA command parser is being
used.")

(defgeneric esa-top-level (frame &key
                                 command-parser
                                 command-unparser
                                 partial-command-parser
                                 prompt)
  (:documentation "Run a top-level loop for `frame', reading
  gestures and invoking the appropriate commands."))

(defmacro define-esa-top-level ((frame command-parser
                                       command-unparser
                                       partial-command-parser
                                       prompt) &key bindings)
  `(defmethod esa-top-level (,frame &key
                             (,command-parser 'esa-command-parser)
                             ;; FIXME: maybe customize this?  Under what
                             ;; circumstances would it be used?  Maybe try
                             ;; turning the clim listener into an ESA?
                             (,command-unparser  'command-line-command-unparser)
                             (,partial-command-parser 'esa-partial-command-parser)
                             (,prompt "Extended Command: "))
     ,(let ((frame (unlisted frame)))
           `(with-slots (windows) ,frame
              (let ((*standard-output* (car windows))
                    (*standard-input* (frame-standard-input ,frame))
                    (*minibuffer* (minibuffer ,frame))
                    (*print-pretty* nil)
                    (*abort-gestures* *esa-abort-gestures*)
                    (*command-parser* ,command-parser)
                    (*command-unparser* ,command-unparser)
                    (*partial-command-parser* ,partial-command-parser)
                    (*extended-command-prompt* ,prompt)
                    (*pointer-documentation-output*
                     (frame-pointer-documentation-output ,frame))
                    (*esa-instance* ,frame))
                (unless (eq (frame-state ,frame) :enabled)
                  (enable-frame ,frame))
                (redisplay-frame-panes ,frame :force-p t)
                (loop
                   do (restart-case
                          (handler-case
                              (let* ((*command-processor* ,frame)
                                     (command-table (find-applicable-command-table ,frame))
                                     ,@bindings)
                                ;; for presentation-to-command-translators,
                                ;; which are searched for in
                                ;; (frame-command-table *application-frame*)
                                (redisplay-frame-pane ,frame (frame-standard-input ,frame))
                                (setf (frame-command-table ,frame) command-table)
                                (process-gestures-or-command ,frame))
                            (unbound-gesture-sequence (c)
                              (display-message "~A is not bound" (gesture-name (gestures c)))
                              (redisplay-frame-panes ,frame))
                            (abort-gesture (c)
                              (if (overriding-handler ,frame)
                                  (let ((*command-processor* (overriding-handler ,frame)))
                                    (process-gesture (overriding-handler ,frame)
                                                     (climi::%abort-gesture-event c)))
                                  (display-message "Quit"))
                              (redisplay-frame-panes ,frame)))
                        (return-to-esa ()
                          (setf (overriding-handler ,frame) nil)
                          (setf (remaining-keys ,frame) nil)))))))))

(define-esa-top-level (frame command-parser
                             command-unparser
                             partial-command-parser
                             prompt))

(defmacro simple-command-loop (command-table loop-condition
                               &optional end-clauses (abort-clauses '((signal 'abort-gesture :event *current-gesture*))))
  `(progn (setf (overriding-handler *command-processor*)
                (make-instance 'command-loop-command-processor
                               :command-table ,command-table
                               :end-condition #'(lambda ()
                                                  (not ,loop-condition))
                               :super-command-processor *command-processor*
                               :end-function #'(lambda ()
                                                 ,@end-clauses)
                               :abort-function #'(lambda ()
                                                   ,@abort-clauses)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Event handling.

(defgeneric convert-to-gesture (event)
  (:documentation "Convert `event' (which must be an input event)
  to a CLIM gesture, or NIL, if this is not possible."))

(defmethod convert-to-gesture ((ev event))
  nil)

(defmethod convert-to-gesture ((ev character))
  ev)

(defmethod convert-to-gesture ((ev symbol))
  ev)

(defmethod convert-to-gesture ((ev key-press-event))
  (let ((modifiers (event-modifier-state ev))
	(event ev)
	(char nil))
    (when (or (zerop modifiers)
	      (eql modifiers +shift-key+))
      (setq char (keyboard-event-character ev)))
    (if char
        (climi::char-for-read char)
	event)))

(defmethod convert-to-gesture ((ev pointer-button-press-event))
  ev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; command table manipulation

;;; Helper to avoid calling find-keystroke-item at load time. In Classic CLIM
;;; that function doesn't work if not connected to a port.

(defun compare-gestures (g1 g2)
  (and (eql (car g1) (car g2))
       (eql (apply #'make-modifier-state (cdr g1))
	    (apply #'make-modifier-state (cdr g2)))))

(defun find-gesture-item (table gesture)
  (map-over-command-table-keystrokes
     (lambda (name gest item)
       (declare (ignore name))
       (when (compare-gestures gesture gest)
	 (return-from find-gesture-item item)))
     table)
  nil)

(defun ensure-subtable (table gesture)
  (let* ((event (make-instance
		'key-press-event
		:key-name nil
		:key-character (car gesture)
		:modifier-state (apply #'make-modifier-state (cdr gesture))))
	 (item (find-keystroke-item event table :errorp nil)))
    (when (or (null item) (not (eq (command-menu-item-type item) :menu)))
      (let ((name (gensym)))
	(make-command-table name :errorp nil)
	(add-menu-item-to-command-table table (symbol-name name)
					:menu name
					:keystroke gesture)))
    (command-menu-item-value
     (find-keystroke-item event table :errorp nil))))

(defun set-key (command table gestures)
  ;; WTF?
  #-(and)
  (unless (consp command)
    (setf command (list command)))
  (let ((gesture (car gestures)))
    (cond ((null (cdr gestures))
	   (add-keystroke-to-command-table
	    table gesture :command command :errorp nil)
	   (when (and (listp gesture)
		      (find :meta gesture))
             ;; KLUDGE: this is a workaround for poor McCLIM
             ;; behaviour; really this canonization should happen in
             ;; McCLIM's input layer.
	     (set-key command table
		      (list (list :escape)
			    (let ((esc-list (remove :meta gesture)))
			      (if (and (= (length esc-list) 2)
				       (find :shift esc-list))
				  (remove :shift esc-list)
				  esc-list))))))
	  (t (set-key command
		      (ensure-subtable table gesture)
		      (cdr gestures))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; standard key bindings 

;;; global

(define-command-table global-esa-table)

(define-command (com-quit :name t :command-table global-esa-table) ()
  "Exit.
First ask if modified buffers should be saved. If you decide not to save a modified buffer, you will be asked to confirm your decision to exit."
  (frame-exit *application-frame*))

(set-key 'com-quit 'global-esa-table '((#\x :control) (#\c :control)))

(define-command (com-extended-command
		 :command-table global-esa-table)
    ()
  "Prompt for a command name and arguments, then run it."
  (let ((item (handler-case
                  (accept
                   `(command :command-table ,(find-applicable-command-table *application-frame*))
                   ;; this gets erased immediately anyway
                   :prompt "" :prompt-mode :raw)
                ((or command-not-accessible command-not-present) ()
                  (beep)
                 (display-message "No such command")
                 (return-from com-extended-command nil)))))
    (execute-frame-command *application-frame* item)))

(set-key 'com-extended-command 'global-esa-table '((#\x :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Help

(defgeneric invoke-with-help-stream (esa title continuation)
  (:documentation
   #.(format nil "Invoke CONTINUATION with a single argument - a stream~@
                  for writing on-line help for ESA onto. The stream~@
                  should have the title, or name, TITLE (a string), but the~@
                  specific meaning of this is left to the respective ESA.")))

(defmethod invoke-with-help-stream (frame title continuation)
  (funcall continuation
           (open-window-stream
            :label title
            :input-buffer (climi::frame-event-queue *application-frame*)
            :width 400)))

(defmacro with-help-stream ((stream title) &body body)
  "Evaluate `body' with `stream' bound to a stream suitable for
writing help information on. `Title' must evaluate to a string,
and will be used for naming the resulting stream, if that makes
sense for the ESA."
  `(invoke-with-help-stream *esa-instance* ,title
                            #'(lambda (,stream)
                                ,@body)))

(defun read-gestures-for-help (command-table)
  (with-input-focus (t)
    (loop for gestures = (list (esa-read-gesture))
            then (nconc gestures (list (esa-read-gesture)))
          for item = (find-gestures-with-inheritance gestures command-table)
          unless item
            do (return (values nil gestures))
          when (eq (command-menu-item-type item) :command)
            do (return (values (command-menu-item-value item) gestures)))))

(defun describe-key-briefly (frame)
  (let ((command-table (find-applicable-command-table frame)))
    (multiple-value-bind (command gestures)
	(read-gestures-for-help command-table)
      (when (consp command)
	(setf command (car command)))
      (display-message "~{~A ~}~:[is not bound~;runs the command ~:*~A~]"
		       (mapcar #'gesture-name gestures)
		       (or (command-line-name-for-command
			    command command-table :errorp nil)
			   command)))))

(defgeneric gesture-name (gesture))

(defmethod gesture-name ((char character))
  (if (and (graphic-char-p char)
           (not (char= char #\Space)))
      (string char)
      (or (char-name char)
          char)))

(defun translate-name-and-modifiers (key-name modifiers)
  (with-output-to-string (s)
      (loop for (modifier name) on (list
					;(+alt-key+ "A-")
					+hyper-key+ "H-"
					+super-key+ "s-"
					+meta-key+ "M-"
					+control-key+ "C-")
	      by #'cddr
	    when (plusp (logand modifier modifiers))
	      do (princ name s))
      (princ (if (typep key-name 'character)
		 (gesture-name key-name)
		 key-name) s)))

(defmethod gesture-name ((ev keyboard-event))
  (let ((key-name (keyboard-event-key-name ev))
	(modifiers (event-modifier-state ev)))
    (translate-name-and-modifiers key-name modifiers)))

(defmethod gesture-name ((gesture list))
  (cond ((eq (car gesture) :keyboard)
	 (translate-name-and-modifiers (second gesture) (third gesture)))
	;; Assume `gesture' is a list of gestures.
	(t (format nil "~{~A~#[~; ~; ~]~}" (mapcar #'gesture-name gesture)))))

(defun find-keystrokes-for-command (command command-table)
  (let ((keystrokes '()))
    (labels ((helper (command command-table prefix)
               (map-over-command-table-keystrokes
                #'(lambda (menu-name keystroke item)
                    (declare (ignore menu-name))
                    (cond ((and (eq (command-menu-item-type item) :command)
                                (or (and (symbolp (command-menu-item-value item))
                                         (eq (command-menu-item-value item) command))
                                    (and (listp (command-menu-item-value item))
                                         (eq (car (command-menu-item-value item)) command))))
                           (push (cons keystroke prefix) keystrokes))
                          ((eq (command-menu-item-type item) :menu)
                           (helper command (command-menu-item-value item) (cons keystroke prefix)))
                          (t nil)))
                command-table)))
      (helper command command-table nil)
      keystrokes)))

(defun find-keystrokes-for-command-with-inheritance (command start-table)
  (let ((keystrokes '()))
    (labels  ((helper (table)
		(let ((keys (find-keystrokes-for-command command table)))
		  (when keys (push keys keystrokes))
		  (dolist (subtable (command-table-inherit-from
				     (find-command-table table)))
		    (helper subtable)))))
      (helper start-table))
    keystrokes))

(defun find-all-keystrokes-and-commands (command-table)
  (let ((results '()))
    (labels ((helper (command-table prefix)
	       (map-over-command-table-keystrokes
		#'(lambda (menu-name keystroke item)
		    (declare (ignore menu-name))
		    (cond ((eq (command-menu-item-type item) :command) 
			   (push (cons (cons keystroke prefix)
				       (command-menu-item-value item))
				 results))
			  ((eq (command-menu-item-type item) :menu)
			   (helper (command-menu-item-value item) (cons keystroke prefix)))
			  (t nil)))
		command-table)))
      (helper command-table nil)
      results)))

(defun find-all-keystrokes-and-commands-with-inheritance (start-table)
  (let ((results '()))
    (labels  ((helper (table)
		(let ((res (find-all-keystrokes-and-commands table)))
		  (when res  (setf results (nconc res results)))
		  (dolist (subtable (command-table-inherit-from
				     (find-command-table table)))
		    (helper subtable)))))
      (helper start-table))
    results))

(defun find-all-commands-and-keystrokes-with-inheritance (start-table)
  (let ((results '()))
    (map-over-command-table-commands
     (lambda (command)
       (let ((keys (find-keystrokes-for-command-with-inheritance command start-table)))
	 (push (cons command keys) results)))
     start-table
     :inherited t)
    results))

(defun sort-by-name (list)
  (sort list #'string< :key (lambda (item) 
                              (symbol-name (if (listp (cdr item)) 
                                               (cadr item) 
                                               (cdr item))))))

(defun sort-by-keystrokes (list)
  (sort list (lambda (a b)
	       (cond ((and (characterp a)
			   (characterp b))
		      (char< a b))
		     ((characterp a)
		      t)
		     ((characterp b)
		      nil)
		     (t (string< (symbol-name a)
				 (symbol-name b)))))
	:key (lambda (item) (second (first (first item))))))

(defun describe-bindings (stream command-table
			  &optional (sort-function #'sort-by-name))
  (formatting-table (stream)
    (loop for (keys . command)
	  in (funcall sort-function
		      (find-all-keystrokes-and-commands-with-inheritance
			   command-table))
          when (consp command) do (setq command (car command))
	  do (formatting-row (stream) 
	       (formatting-cell (stream :align-x :right)
		 (with-text-style (stream '(:sans-serif nil nil))
		   (present command
                            `(command-name :command-table ,command-table)
                            :stream stream)))
	       (formatting-cell (stream)
		 (with-drawing-options (stream :ink +dark-blue+
					       :text-style '(:fix nil nil))
		   (format stream "~&~{~A~^ ~}"
			   (mapcar #'gesture-name (reverse keys))))))
	  count command into length
	  finally (change-space-requirements stream
			 :height (* length (stream-line-height stream)))
		  (scroll-extent stream 0 0))))

(defun print-docstring-for-command (command-name command-table &optional (stream *standard-output*))
  "Print documentation for `command-name', which should 
   be a symbol bound to a function, to `stream'. If no 
   documentation can be found, this fact will be printed to the stream."
  (declare (ignore command-table))
  ;; This needs more regex magic. Also, it is only an interim
  ;; solution.
  (with-text-style (stream '(:sans-serif nil nil))
    (let* ((command-documentation (or (documentation command-name 'function)
                                     "This command is not documented."))
	   (first-newline (position #\Newline command-documentation))
	   (first-line (subseq command-documentation 0 first-newline)))
      ;; First line is special
      (format stream "~A~%" first-line)
      (when first-newline
	(let* ((rest (subseq command-documentation first-newline))
	       (paras (delete ""
			      (loop for start = 0 then (+ 2 end)
				    for end = (search '(#\Newline #\Newline) rest :start2 start)
				    collecting
				    (nsubstitute #\Space #\Newline (subseq rest start end))
				    while end)
			      :test #'string=)))
	  (dolist (para paras)
	    (terpri stream)
	    (let ((words (loop with length = (length para)
			       with index = 0
			       with start = 0
			       while (< index length)
			       do (loop until (>= index length)
					while (member (char para index) '(#\Space #\Tab))
					do (incf index))
				  (setf start index)
				  (loop until (>= index length)
					until (member (char para index) '(#\Space #\Tab))
					do (incf index))
			       until (= start index)
			       collecting (string-trim '(#\Space #\Tab #\Newline)
							(subseq para start index)))))
	      (loop with margin = (stream-text-margin stream)
		    with space-width = (stream-character-width stream #\Space)
		    with current-width = 0
		    for word in words
		    for word-width = (stream-string-width stream word)
		    when (> (+ word-width current-width)
				   margin)
		      do (terpri stream)
			 (setf current-width 0)
		    do (princ word stream)
		       (princ #\Space stream)
		       (incf current-width (+ word-width space-width))))
	    (terpri stream)))))))

(defun describe-command-binding-to-stream (gesture command &key 
                                           (command-table (find-applicable-command-table *application-frame*))
                                           (stream *standard-output*))
  "Describe `command' as invoked by `gesture' to `stream'."
  (let* ((command-name (if (listp command)
                           (first command)
                           command))        
         (command-args (if (listp command)
                           (rest command)))
         (real-command-table (or (command-accessible-in-command-table-p 
                                  command-name
                                  command-table)
                                 command-table)))
    (with-text-style (stream '(:sans-serif nil nil))
      (princ "The gesture " stream)
      (with-drawing-options (stream :ink +dark-blue+
				    :text-style '(:fix nil nil))
        (princ gesture stream))
      (princ " is bound to the command " stream)
      (if (command-present-in-command-table-p command-name real-command-table)
          (with-text-style (stream '(nil :bold nil))
	    (present command-name `(command-name :command-table ,command-table) :stream stream))
          (present command-name 'symbol :stream stream))
      (princ " in " stream)
      (present real-command-table 'command-table :stream stream)
      (format stream ".~%")
      (when command-args
        (apply #'format stream
	       "This binding invokes the command with these arguments: ~@{~A~^, ~}.~%"
	       (mapcar #'(lambda (arg)
                           (cond ((eq arg *unsupplied-argument-marker*)
                                  "unsupplied-argument")
                                 ((eq arg *numeric-argument-marker*)
                                  "numeric-argument")
                                 (t arg))) command-args)))
      (terpri stream)
      (print-docstring-for-command command-name command-table stream)
      (scroll-extent stream 0 0))))

(defun describe-command-to-stream
    (command-name &key 
     (command-table (find-applicable-command-table *application-frame*))
     (stream *standard-output*))
  "Describe `command' to `stream'."
  (let ((keystrokes (find-keystrokes-for-command-with-inheritance command-name command-table)))
    (with-text-style (stream '(:sans-serif nil nil))
      (with-text-style (stream '(nil :bold nil))
	(present command-name `(command-name :command-table ,command-table) :stream stream))
      (princ " calls the function " stream)
      (present command-name 'symbol :stream stream)
      (princ " and is accessible in " stream)
      (if (command-accessible-in-command-table-p command-name command-table)
          (present (command-accessible-in-command-table-p command-name command-table)
                   'command-table
                   :stream stream)
          (princ "an unknown command table" stream))
      
      (format stream ".~%")
      (when (plusp (length keystrokes))
        (princ "It is bound to " stream)
        (loop for gestures-list on (first keystrokes)
	      do (with-drawing-options (stream :ink +dark-blue+
					       :text-style '(:fix nil nil))
		   (format stream "~{~A~^ ~}"
			   (mapcar #'gesture-name (reverse (first gestures-list)))))
	      when (not (null (rest gestures-list)))
		do (princ ", " stream))
        (terpri stream))
      (terpri stream)
      (print-docstring-for-command command-name command-table stream)
      (scroll-extent stream 0 0))))

;;; help commands

(define-command-table help-table)

(define-command (com-describe-key-briefly :name t :command-table help-table) ()
  "Prompt for a key and show the command it invokes."  
  (display-message "Describe key briefly:")
  (redisplay-frame-panes *application-frame*)
  (describe-key-briefly *application-frame*))

(set-key 'com-describe-key-briefly 'help-table '((#\h :control) (#\c)))

(define-command (com-where-is :name t :command-table help-table) ()
  "Prompt for a command name and show the key that invokes it."
  (let* ((command-table (find-applicable-command-table *application-frame*))
	 (command
	  (handler-case
	      (accept
	       `(command-name :command-table
			      ,command-table)
	       :prompt "Where is command")
	    (error () (progn (beep)
			     (display-message "No such command")
			     (return-from com-where-is nil)))))
	 (keystrokes (find-keystrokes-for-command-with-inheritance command command-table)))
    (display-message "~A is ~:[not on any key~;~:*on ~{~A~^, ~}~]"
		     (command-line-name-for-command command command-table)
		     (mapcar (lambda (keys)
			       (format nil "~{~A~^ ~}"
				       (mapcar #'gesture-name (reverse keys))))
			     (car keystrokes)))))

(set-key 'com-where-is 'help-table '((#\h :control) (#\w)))

(define-command (com-describe-bindings :name t :command-table help-table)
    ((sort-by-keystrokes 'boolean :prompt "Sort by keystrokes?"))
  "Show which keys invoke which commands.
Without a numeric prefix, sorts the list by command name. With a numeric prefix, sorts by key."
  (let ((command-table (find-applicable-command-table *application-frame*)))
    (with-help-stream (stream (format nil "Help: Describe Bindings"))
      (describe-bindings stream command-table
                         (if sort-by-keystrokes
                             #'sort-by-keystrokes
                             #'sort-by-name)))))

(set-key `(com-describe-bindings ,*numeric-argument-marker*) 'help-table '((#\h :control) (#\b)))

(define-command (com-describe-key :name t :command-table help-table)
    ()
  "Display documentation for the command invoked by a given gesture sequence. 
When invoked, this command will wait for user input. If the user inputs a gesture 
sequence bound to a command available in the syntax of the current buffer,
documentation and other details will be displayed in a typeout pane."
  (let ((command-table (find-applicable-command-table *application-frame*)))
    (display-message "Describe Key:")
    (redisplay-frame-panes *application-frame*)
    (multiple-value-bind (command gestures)
        (read-gestures-for-help command-table)
      (let ((gesture-name (format nil "~{~A~#[~; ~; ~]~}"
                                  (mapcar #'gesture-name gestures))))
        (if command
            (with-help-stream (out-stream (format nil "~10THelp: Describe Key for ~A" gesture-name))
              (describe-command-binding-to-stream gesture-name command
               :command-table command-table
               :stream out-stream))
            (display-message "Unbound gesture: ~A" gesture-name))))))

(set-key 'com-describe-key
         'help-table
         '((#\h :control) (#\k)))

(define-command (com-describe-command :name t :command-table help-table)
    ((command 'command-name :prompt "Describe command"))
  "Display documentation for the given command."
  (let ((command-table (find-applicable-command-table *application-frame*)))
    (with-help-stream (out-stream (format nil "~10THelp: Describe Command for ~A"
					  (command-line-name-for-command command
									 command-table
									 :errorp nil)))
      (describe-command-to-stream command
       :command-table command-table
       :stream out-stream))))

(set-key `(com-describe-command ,*unsupplied-argument-marker*)
         'help-table
         '((#\h :control) (#\f)))

(define-presentation-to-command-translator describe-command
    (command-name com-describe-command help-table
                  :gesture :select
                  :documentation "Describe command")
    (object)
    (list object))

(define-command (com-apropos-command :name t :command-table help-table)
    ((words '(sequence string) :prompt "Search word(s)"))
  "Shows commands with documentation matching the search words.
Words are comma delimited. When more than two words are given, the documentation must match any two."
  ;; 23.8.6 "It is unspecified whether accept returns a list or a vector."
  (setf words (coerce words 'list))
  (when words
    (let* ((command-table (find-applicable-command-table *application-frame*))
	   (results (loop for (function . keys)
			  in (find-all-commands-and-keystrokes-with-inheritance
				  command-table)
			  when (consp function)
			    do (setq function (car function))
			  when (let ((documentation (or (documentation function 'function) ""))
				     (score 0))
				 (cond
				   ((> (length words) 1)
				    (loop for word in words
					  until (> score 1)
					  when (or
						 (search word (symbol-name function)
						       :test #'char-equal)
						 (search word documentation :test #'char-equal))
					    do (incf score)
					  finally (return (> score 1))))
				   (t (or
				       (search (first words) (symbol-name function)
					       :test #'char-equal)
				       (search (first words) documentation :test #'char-equal)))))
			    collect (cons function keys))))
      (if (null results)
	  (display-message "No results for ~{~A~^, ~}" words)
          (with-help-stream (out-stream (format nil "~10THelp: Apropos ~{~A~^, ~}" words))
            (loop for (command . keys) in results
                  for documentation = (or (documentation command 'function)
                                          "Not documented.")
                  do (with-text-style (out-stream '(:sans-serif :bold nil))
                       (present command
                                `(command-name :command-table ,command-table)
                                :stream out-stream))
                  (with-drawing-options (out-stream :ink +dark-blue+
                                                    :text-style '(:fix nil nil))
                    (format out-stream "~30T~:[M-x ... RETURN~;~:*~{~A~^, ~}~]"
                            (mapcar (lambda (keystrokes)
                                      (format nil "~{~A~^ ~}"
                                              (mapcar #'gesture-name (reverse keystrokes))))
                                    (car keys))))
                  (with-text-style (out-stream '(:sans-serif nil nil))
                    (format out-stream "~&~2T~A~%"
                            (subseq documentation 0 (position #\Newline documentation))))
                  count command into length
                  finally (change-space-requirements out-stream
                           :height (* length (stream-line-height out-stream)))
                  (scroll-extent out-stream 0 0)))))))

(set-key `(com-apropos-command ,*unsupplied-argument-marker*)
	 'help-table
	 '((#\h :control) (#\a)))

(define-menu-table help-menu-table (help-table)
  'com-where-is
  '(com-describe-bindings nil)
  '(com-describe-bindings t)
  'com-describe-key
  `(com-describe-command ,*unsupplied-argument-marker*)
  `(com-apropos-command ,*unsupplied-argument-marker*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Keyboard macros

(define-command-table keyboard-macro-table)

(define-command (com-start-kbd-macro
		 :name t
		 :command-table keyboard-macro-table)
    ()
  "Start recording keys to define a keyboard macro.
Use C-x ) to finish recording the macro, and C-x e to run it."
  (setf (recordingp *command-processor*) t)
  (setf (recorded-keys *command-processor*) '()))

(set-key 'com-start-kbd-macro 'keyboard-macro-table '((#\x :control) #\())

(define-command (com-end-kbd-macro
		 :name t
		 :command-table keyboard-macro-table)
    ()
  "Finish recording keys that define a keyboard macro.
Use C-x ( to start recording a macro, and C-x e to run it."
  (setf (recordingp *command-processor*) nil)
  (setf (recorded-keys *command-processor*)
	;; this won't work if the command was invoked in any old way
	(reverse (cddr (recorded-keys *command-processor*)))))

(set-key 'com-end-kbd-macro 'keyboard-macro-table '((#\x :control) #\)))

(define-command (com-call-last-kbd-macro
		 :name t
		 :command-table keyboard-macro-table)
    ((count 'integer :prompt "How many times?" :default 1))
  "Run the last keyboard macro that was defined.
Use C-x ( to start and C-x ) to finish recording a keyboard macro."
  (setf (remaining-keys *command-processor*)
        (loop repeat count append (recorded-keys *command-processor*)))
  (setf (executingp *command-processor*) t))

(set-key `(com-call-last-kbd-macro ,*numeric-argument-marker*)
         'keyboard-macro-table '((#\x :control) #\e))

(define-menu-table keyboard-macro-menu-table (keyboard-macro-table)
  'com-start-kbd-macro
  'com-end-kbd-macro
  `(com-call-last-kbd-macro ,*unsupplied-argument-marker*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; example application

(defclass example-info-pane (info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :display-function 'display-info
      :incremental-redisplay t))

(defun display-info (frame pane)
  (declare (ignore frame))
  (format pane "Pane name: ~s" (pane-name (master-pane pane))))

(defclass example-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20))

(defclass example-pane (esa-pane-mixin application-pane)
  ((contents :initform "hello" :accessor contents)))

(define-application-frame example (esa-frame-mixin
				   standard-application-frame)
  ()
  (:panes
   (window (let* ((my-pane (make-pane 'example-pane
				      :width 900 :height 400
				      :display-function 'display-my-pane
				      :command-table 'global-example-table))
		  (my-info-pane (make-pane 'example-info-pane
					   :master-pane my-pane
					   :width 900)))
	     (setf (windows *application-frame*) (list my-pane))
	     (vertically ()
			 (scrolling ()
				    my-pane)
			 my-info-pane)))
   (minibuffer (make-pane 'example-minibuffer-pane :width 900)))
  (:layouts
   (default (vertically (:scroll-bars nil)
			window
			minibuffer)))
  (:top-level (esa-top-level)))

(defun display-my-pane (frame pane)
  (declare (ignore frame))
  (princ (contents pane) *standard-output*))

(defun example (&key (width 900) (height 400))
  "Starts up the example application"
  (let ((frame (make-application-frame
		'example
		:width width :height height)))
    (run-frame-top-level frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands and key bindings

(define-command-table global-example-table
    :inherit-from (global-esa-table keyboard-macro-table))
