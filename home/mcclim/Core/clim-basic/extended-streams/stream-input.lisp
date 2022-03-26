;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001,2002 by Tim Moore (moore@bricoworks.com)

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

(in-package :clim-internals)

;;; X11 returns #\Return and #\Backspace where we want to see
;;; #\Newline and #\Delete at the stream-read-char level.  Dunno if
;;; this is the right place to do the transformation...

;;;  Why exactly do we want to see #\Delete instead of #\Backspace?
;;;  There is a separate Delete key, unless your keyboard is
;;;  strange. --Hefner

(defconstant +read-char-map+ '((#\Return . #\Newline)
                               #+nil (#\Backspace . #\Delete)))

(defvar *abort-gestures* '(:abort))

(defvar *accelerator-gestures* nil)

(defvar *input-wait-test* nil)
(defvar *input-wait-handler* nil)
(defvar *pointer-button-press-handler* nil)

(define-condition abort-gesture (condition)
  ((event :reader %abort-gesture-event :initarg :event)))

(defmethod abort-gesture-event ((condition abort-gesture))
  (%abort-gesture-event condition))

(define-condition accelerator-gesture (condition)
  ((event :reader %accelerator-gesture-event :initarg :event)
   (numeric-argument :reader %accelerator-gesture-numeric-argument
                     :initarg :numeric-argument
                     :initform 1)))

(defmethod accelerator-gesture-event ((condition accelerator-gesture))
  (%accelerator-gesture-event condition))

(defmethod accelerator-gesture-numeric-argument
    ((condition accelerator-gesture))
  (%accelerator-gesture-numeric-argument condition))

(defun char-for-read (char)
  (let ((new-char (cdr (assoc char +read-char-map+))))
    (or new-char char)))

(defun unmap-char-for-read (char)
  (let ((new-char (car (rassoc char +read-char-map+))))
    (or new-char char)))

;;; Streams are subclasses of standard-sheet-input-mixin regardless of whether
;;; or not we are multiprocessing.  In single-process mode the blocking calls to
;;; stream-read-char, stream-read-gesture are what cause process-next-event to
;;; be called.  It's most convenient to let process-next-event queue up events
;;; for the stream and then see what we've got after it returns.

(defclass standard-input-stream (fundamental-character-input-stream
                                 standard-sheet-input-mixin)
  ((unread-chars :initform nil :accessor stream-unread-chars)))

;;; XXX: fixing stream shisophrenia is a subject of the next input-refactor pass.
;;; 1. What about EOF?
;;; 2. GESTURE-OBJECT should be already coerced to a character!
;;;   2a. We don't handle modifiers here, so it is double wrong, see stream-process-gesture.
;;; 3. HANDLE-EVENT should have been invoked on a completely different level. -- jd 2018-12-20
(defmethod stream-read-char ((pane standard-input-stream))
  (if (stream-unread-chars pane)
      (pop (stream-unread-chars pane))
      (let ((event (event-read pane)))  ; (1)
        (if (and (typep event 'key-press-event)  ; (2)
                 (keyboard-event-character event)) ; (2a)
            (let ((char (char-for-read (keyboard-event-character event))))
              (stream-write-char pane char)
              (return-from stream-read-char char))
            ;; (3)
            (handle-event (event-sheet event) event)))))

(defmethod stream-unread-char ((pane standard-input-stream) char)
  (push char (stream-unread-chars pane)))

(defmethod stream-read-char-no-hang ((pane standard-input-stream))
  (if (stream-unread-chars pane)
      (pop (stream-unread-chars pane))
    (loop for event = (event-read-no-hang pane)
        if (null event)
           return nil
        if (and (typep event 'key-press-event)
                (keyboard-event-character event))
          return (char-for-read (keyboard-event-character event))
        else
          do (handle-event (event-sheet event) event))))

(defmethod stream-clear-input ((pane standard-input-stream))
  (setf (stream-unread-chars pane) nil)
  (loop for event = (event-read-no-hang pane)
        if (null event)
           return nil
        else
        do (handle-event (event-sheet event) event))
  nil)

(defclass dead-key-merging-mixin ()
  ((state :initform *dead-key-table*)
   ;; Avoid name clash with standard-extended-input-stream.
   (last-deadie-gesture)
   (last-state))
  (:documentation "A mixin class for extended input streams that
takes care of handling dead keys. This is done by still passing
every gesture on, but accenting the final one as per the dead
keys read."))

(defmethod stream-read-gesture :around
    ((stream dead-key-merging-mixin)
     &key timeout peek-p
     (input-wait-test *input-wait-test*)
     (input-wait-handler *input-wait-handler*)
     (pointer-button-press-handler
      *pointer-button-press-handler*))
  (with-slots (state last-deadie-gesture last-state) stream
    (handler-case
        (loop with start-time = (get-internal-real-time)
              with end-time = start-time
              do (multiple-value-bind (gesture reason)
                     (call-next-method stream
                      :timeout (when timeout
                                 (- timeout (/ (- end-time start-time)
                                               internal-time-units-per-second)))
                      :peek-p peek-p
                      :input-wait-test input-wait-test
                      :input-wait-handler input-wait-handler
                      :pointer-button-press-handler
                      pointer-button-press-handler)
                   (when (null gesture)
                     (return (values nil reason)))
                   (setf end-time (get-internal-real-time)
                         last-deadie-gesture gesture
                         last-state state)
                   (merging-dead-keys (gesture state)
                     (return gesture))))
      ;; Policy decision: an abort cancels the current composition.
      (abort-gesture (c)
        (setf state *dead-key-table*)
        (signal c)))))

(defmethod stream-unread-gesture :around ((stream dead-key-merging-mixin) gesture)
  (if (typep gesture '(or keyboard-event character))
      (with-slots (state last-deadie-gesture last-state) stream
        (setf state last-state)
        (call-next-method stream last-deadie-gesture))
      (call-next-method)))

(defclass standard-extended-input-stream (extended-input-stream
                                          ;; FIXME: is this still needed?
                                          standard-sheet-input-mixin
                                          dead-key-merging-mixin)
  ((pointer)
   (cursor :initarg :text-cursor)
   (last-gesture :accessor last-gesture :initform nil
    :documentation "Holds the last gesture returned by stream-read-gesture
(not peek-p), untransformed, so it can easily be unread.")))

(defmethod stream-set-input-focus ((stream standard-extended-input-stream))
  (let ((port (or (port stream)
                  (port *application-frame*))))
    (prog1 (port-keyboard-input-focus port)
      (setf (port-keyboard-input-focus port) stream))))

(defmacro with-input-focus ((stream) &body body)
  (when (eq stream t)
    (setq stream '*standard-input*))
  (let ((old-stream (gensym "OLD-STREAM")))
    `(let ((,old-stream (stream-set-input-focus ,stream)))
       (unwind-protect (locally ,@body)
         (when ,old-stream
           (stream-set-input-focus ,old-stream))))))

(defun read-gesture (&key
                     (stream *standard-input*)
                     timeout
                     peek-p
                     (input-wait-test *input-wait-test*)
                     (input-wait-handler *input-wait-handler*)
                     (pointer-button-press-handler
                      *pointer-button-press-handler*))
  (stream-read-gesture stream
                       :timeout timeout
                       :peek-p peek-p
                       :input-wait-test input-wait-test
                       :input-wait-handler input-wait-handler
                       :pointer-button-press-handler
                       pointer-button-press-handler))


;;; Do streams care about any other events?
(defun handle-non-stream-event (buffer)
  (let* ((event (event-queue-peek buffer))
         (sheet (and event (event-sheet event))))
    (if (and event
             (or (and (gadgetp sheet)
                      (gadget-active-p sheet))
                 (not (and (typep sheet 'clim-stream-pane)
                           (or (typep event 'key-press-event)
                               (typep event 'pointer-button-press-event))))))
        (progn
          (event-queue-read buffer)	;eat it
          (handle-event (event-sheet event) event)
          t)
        nil)))

(defun pop-gesture (buffer peek-p)
  (if peek-p
      (event-queue-peek buffer)
      (event-queue-read-no-hang buffer)))

(defun repush-gesture (gesture buffer)
  (event-queue-prepend buffer gesture))

(defmethod stream-process-gesture ((stream standard-extended-input-stream) gesture type)
  (declare (ignore type))
  (typecase gesture
    ((or character symbol pointer-button-event)
     (values gesture (type-of gesture)))
    (key-press-event
     (let ((modifiers (event-modifier-state gesture))
           (character (keyboard-event-character gesture)))
       (if (and (or (zerop modifiers)
                    (eql modifiers +shift-key+))
                character)
           (values (char-for-read character) 'standard-character)
           (values gesture (type-of gesture)))))
    (otherwise
     nil)))

(defmethod stream-read-gesture ((stream standard-extended-input-stream)
                                &key timeout peek-p
                                (input-wait-test *input-wait-test*)
                                (input-wait-handler *input-wait-handler*)
                                (pointer-button-press-handler
                                 *pointer-button-press-handler*))
  (with-encapsulating-stream (estream stream)
    (let ((*input-wait-test* input-wait-test)
          (*input-wait-handler* input-wait-handler)
          (*pointer-button-press-handler* pointer-button-press-handler)
          (buffer (stream-input-buffer stream)))
      (tagbody
         ;; Wait for input... or not
         ;; XXX decay timeout.
       wait-for-char
       (multiple-value-bind (available reason)
           (stream-input-wait estream
                              :timeout timeout
                              :input-wait-test input-wait-test)
         (unless available
           (case reason
             (:timeout
              (assert timeout () "Reason is timeout but timeout is null!.")
              (return-from stream-read-gesture
                (values nil :timeout)))
             (:input-wait-test
              ;; input-wait-handler might leave the event for us.
              ;; This is actually quite messy; I'd like to confine
              ;; handle-event to stream-input-wait, but we can't loop
              ;; back to it because the input handler will continue to
              ;; decline to read the event :(
              (let ((event (event-queue-peek buffer)))
                (when input-wait-handler
                  (funcall input-wait-handler stream))
                (let ((current-event (event-queue-peek buffer)))
                  (when (or (not current-event)
                            (not (eq event current-event)))
                    ;; If there's a new event input-wait-test needs to
                    ;; take a look at it.
                    (go wait-for-char)))))
             (t (go wait-for-char)))))
         ;; An event should  be in the stream buffer now.
         (when (handle-non-stream-event buffer)
           (go wait-for-char))
         (let* ((raw-gesture (pop-gesture buffer peek-p))
                (gesture (stream-process-gesture stream raw-gesture nil)))
           ;; Sometimes key press events get generated with a key code for which
           ;; there is no keysym.  This seems to happen on my machine when keys
           ;; are hit rapidly in succession.  I'm not sure if this is a hardware
           ;; problem with my keyboard, and this case is probably better handled
           ;; in the backend, but for now the case below handles the problem. --
           ;; moore
           (cond ((null gesture)
                  (go wait-for-char))
                 ((and pointer-button-press-handler
                       (typep gesture 'pointer-button-press-event))
                  (funcall pointer-button-press-handler stream gesture))
                 ((loop for gesture-name in *abort-gestures*
                        thereis (event-matches-gesture-name-p gesture gesture-name))
                  (signal 'abort-gesture :event gesture))
                 ((loop for gesture-name in *accelerator-gestures*
                        thereis (event-matches-gesture-name-p gesture gesture-name))
                  (signal 'accelerator-gesture :event gesture))
                 (t (setf (last-gesture stream) raw-gesture)
                    (return-from stream-read-gesture gesture))))
         (go wait-for-char)))))


(defmethod stream-input-wait ((stream standard-extended-input-stream)
                              &key timeout input-wait-test)
  (block exit
    (let ((buffer (stream-input-buffer stream)))
      (tagbody
       check-buffer
         (when-let ((event (event-queue-peek buffer)))
           (when (and input-wait-test (funcall input-wait-test stream))
             (return-from exit (values nil :input-wait-test)))
           (if (handle-non-stream-event buffer)
               (go check-buffer)
               (return-from exit t)))
         ;; Event queue has been drained, time to block waiting for new events.
         (unless (event-queue-listen-or-wait buffer :timeout timeout)
           (return-from exit (values nil :timeout)))
         (go check-buffer)))))

(defun unread-gesture (gesture &key (stream *standard-input*))
  (stream-unread-gesture stream gesture))

(defmethod stream-unread-gesture ((stream standard-extended-input-stream)
                                  gesture)
  (declare (ignore gesture))
  (with-encapsulating-stream (estream stream)
    (let ((gesture (last-gesture stream)))
      (when gesture
        (setf (last-gesture stream) nil)
        (repush-gesture gesture (stream-input-buffer estream))))))

;;; Standard stream methods on standard-extended-input-stream.  Ignore any
;;; pointer gestures in the input buffer.
;;;
;;; Is stream-read-gesture allowed to return :eof?

(defmethod stream-read-char ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop
         with char and reason
         do (setf (values char reason) (stream-read-gesture estream))
         until (or (characterp char) (eq reason :eof))
         finally (return (if (eq reason :eof)
                             reason
                             (char-for-read char))))))

(defmethod stream-read-char-no-hang ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop
       with char and reason
       do (setf (values char reason) (stream-read-gesture estream :timeout 0))
       until (or (characterp char) (eq reason :timeout) (eq reason :eof) )
       finally (return (cond ((eq reason :timeout)
                              nil)
                             ((eq reason :eof)
                              :eof)
                             (t (char-for-read char)))))))

(defmethod stream-unread-char ((stream standard-extended-input-stream)
                               char)
  (with-encapsulating-stream (estream stream)
    (stream-unread-gesture estream (unmap-char-for-read char))))

(defmethod stream-peek-char ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop
       with char and reason
       do (setf (values char reason) (stream-read-gesture estream :peek-p t))
       until (or (characterp char) (eq reason :eof))
       do (stream-read-gesture estream) ; consume pointer gesture
       finally (return (if (eq reason :eof)
                           reason
                           (char-for-read char))))))

(defmethod stream-listen ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop
       with char and reason
         do (setf (values char reason) (stream-read-gesture estream
                                                            :timeout 0
                                                            :peek-p t))
         until (or (characterp char) (eq reason :eof) (eq reason :timeout))
         do (stream-read-gesture estream) ; consume pointer gesture
         finally (return (characterp char)))))

(defmethod stream-clear-input ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop
       with char and reason
         do (setf (values char reason) (stream-read-gesture estream
                                                            :timeout 0
                                                            :peek-p t))
         until (or (eq reason :eof) (eq reason :timeout))
         do (stream-read-gesture estream) ; consume pointer gesture
         ))
  nil)

;;; stream-read-line returns a second value of t if terminated by eof.
(defmethod stream-read-line ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (let ((result (make-array 1
                              :element-type 'character
                              :adjustable t
                              :fill-pointer 0)))
      (loop for char = (stream-read-char estream)
            while (and (characterp char) (not (char= char #\Newline)))
            do (vector-push-extend char result)
            finally (return (values (subseq result 0)
                                    (not (characterp char))))))))

;;; stream-read-gesture on string strings. Needed for accept-from-string.

;;; XXX Evil hack because "string-stream" isn't the superclass of
;;; string streams in CMUCL/SBCL...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *string-input-stream-class* (with-input-from-string (s "foo")
                                        (class-name (class-of s)))))

(defmethod stream-read-gesture ((stream #.*string-input-stream-class*)
                                &key peek-p
                                &allow-other-keys)
  (let ((char (if peek-p
                  (peek-char nil stream nil nil)
                  (read-char stream nil nil))))
    (if char
        char
        (values nil :eof))))

(defmethod stream-unread-gesture ((stream #.*string-input-stream-class*)
                                  gesture)
  (unread-char gesture stream))

;;; Gestures

(defparameter *gesture-names* (make-hash-table))

(defmacro define-gesture-name (name type gesture-spec &key (unique t))
  `(add-gesture-name ',name ',type ',gesture-spec ,@(and unique
                                                         `(:unique ',unique))))

(defun delete-gesture-name (name)
  "Delete the gesture named by the symbol `name' from the list of
known gestures."
  (remhash name *gesture-names*))

;;; XXX perhaps this should be in the backend somewhere?
(defconstant +name-to-char+ '((:newline . #\newline)
                              (:linefeed . #\linefeed)
                              (:return . #\return)
                              (:tab . #\tab)
                              (:backspace . #\backspace)
                              (:page . #\page)
                              (:rubout . #\rubout)))

(defun realize-gesture-spec (type gesture-spec)
  ;; Some CLIM code (scigraph) assumes that gesture-spec can be a symbol.
  (unless (listp gesture-spec)
    (setq gesture-spec (list gesture-spec)))
  (destructuring-bind (device-name . modifiers)
      gesture-spec
    (let* ((modifier-state (apply #'make-modifier-state modifiers)))
      (cond ((and (eq type :keyboard)
                  (symbolp device-name))
             (setq device-name (or (cdr (assoc device-name +name-to-char+))
                                   device-name)))
            ((and (member type '(:pointer-button
                                 :pointer-button-press
                                 :pointer-button-release)
                          :test #'eq))
             (let ((real-device-name
                    (case device-name
                      (:left       +pointer-left-button+)
                      (:middle     +pointer-middle-button+)
                      (:right      +pointer-right-button+)
                      (:wheel-up   +pointer-wheel-up+)
                      (:wheel-down +pointer-wheel-down+)
                      (t (error "~S is not a known button" device-name)))))
               (setq device-name real-device-name))))
      (values type device-name modifier-state))))

(defun add-gesture-name (name type gesture-spec &key unique)
      (let ((gesture-entry (multiple-value-list (realize-gesture-spec type gesture-spec))))
        (if unique
            (setf (gethash name *gesture-names*) (list gesture-entry))
            (push gesture-entry (gethash name *gesture-names*)))))

(defgeneric character-gesture-name (name))

(defmethod character-gesture-name ((name character))
  name)

(defmethod character-gesture-name ((name symbol))
  (let ((entry (car (gethash name *gesture-names*))))
    (if entry
        (destructuring-bind (type device-name modifier-state)
            entry
          (if (and (eq type :keyboard)
                   (eql modifier-state 0))
              device-name
              nil))
        nil)))

(defgeneric %event-matches-gesture (event type device-name modifier-state))

(defmethod %event-matches-gesture (event type device-name modifier-state)
  (declare (ignore event type device-name modifier-state))
  nil)

(defmethod %event-matches-gesture ((event key-press-event)
                                   (type (eql :keyboard))
                                   device-name
                                   modifier-state)
  (let ((character (keyboard-event-character event))
        (name      (keyboard-event-key-name event)))
    (and (if character
             (eql character device-name)
             (eql name device-name))
         (eql (event-modifier-state event) modifier-state))))

(defmethod %event-matches-gesture ((event pointer-button-press-event)
                                   type
                                   device-name
                                   modifier-state)
  (and (or (eql type :pointer-button-press)
           (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

(defmethod %event-matches-gesture ((event pointer-button-release-event)
                                   type
                                   device-name
                                   modifier-state)
  (and (or (eql type :pointer-button-release)
           (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

(defmethod %event-matches-gesture ((event pointer-button-event)
                                   type
                                   device-name
                                   modifier-state)
  (and (or (eql type :pointer-button-press)
           (eql type :pointer-button-release)
           (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

;;; Because gesture objects are either characters or event objects, support
;;; characters here too.

(defmethod %event-matches-gesture ((event character)
                                   (type (eql :keyboard))
                                   device-name
                                   modifier-state)
  (and (eql event device-name)
       (eql modifier-state 0)))

(defun event-matches-gesture-name-p (event gesture-name)
  ;; Just to be nice, we special-case literal characters here.  We also
  ;; special-case literal 'physical' gesture specs of the form (type device-name
  ;; modifier-state).  The CLIM spec requires neither of these things.
  (let ((gesture-entry
         (typecase gesture-name
           (character (list (multiple-value-list (realize-gesture-spec :keyboard gesture-name))))
           (cons (list gesture-name)) ; Literal physical gesture
           (t (gethash gesture-name *gesture-names*)))))
    (loop for (type device-name modifier-state) in gesture-entry
          do (when (%event-matches-gesture event
                                           type
                                           device-name
                                           modifier-state)
               (return-from event-matches-gesture-name-p t))
          finally (return nil))))

(defun modifier-state-matches-gesture-name-p (modifier-state gesture-name)
  (loop for (nil nil gesture-state) in (gethash gesture-name *gesture-names*)
        do (when (eql gesture-state modifier-state)
             (return-from modifier-state-matches-gesture-name-p t))
        finally (return nil)))


(defun make-modifier-state (&rest modifiers)
  (loop for result = 0 then (logior (case modifier
                                      (:shift +shift-key+)
                                      (:control +control-key+)
                                      (:meta +meta-key+)
                                      (:super +super-key+)
                                      (:hyper +hyper-key+)
                                      (t (error "~S is not a known modifier" modifier)))
                                    result)
        for modifier in modifiers
        finally (return result)))

;;; Standard gesture names

(define-gesture-name :abort :keyboard (#\c :control))
(define-gesture-name :clear-input :keyboard (#\u :control))
(define-gesture-name :complete :keyboard (:tab))
(define-gesture-name :help :keyboard (#\/ :control))
(define-gesture-name :possibilities :keyboard (#\? :control))

(define-gesture-name :select :pointer-button-press (:left))
(define-gesture-name :describe :pointer-button-press (:middle))
(define-gesture-name :menu :pointer-button-press (:right))
(define-gesture-name :edit :pointer-button-press (:left :meta))
(define-gesture-name :delete :pointer-button-press (:middle :shift))

;;; Define so we have a gesture for #\newline that we can use in
;;; *standard-activation-gestures*

(define-gesture-name :newline :keyboard (#\newline))
(define-gesture-name :return :keyboard (#\return))

;;; The standard delimiter

(define-gesture-name command-delimiter :keyboard (#\space))

;;; Extension: support for handling abort gestures that appears to be
;;; in real CLIM

;;; From the hyperspec, more or less

(defun invoke-condition-restart (c)
  (let ((restarts (compute-restarts c)))
    (loop for i from 0
          for restart in restarts
          do (format t "~&~D: ~A~%" i restart))
    (loop with n = nil
          and k = (length restarts)
          until (and (integerp n) (>= n 0) (< n k))
          do (progn
               (format t "~&Option: ")
               (setq n (read))
               (fresh-line))
          finally
          #-cmu (invoke-restart (nth n restarts))
          #+cmu (funcall (conditions::restart-function (nth n restarts))))))

(defmacro catch-abort-gestures (format-args &body body)
  `(restart-case
       (handler-bind ((abort-gesture #'invoke-condition-restart))
         ,@body)
     (nil ()
       :report (lambda (s) (format s ,@format-args))
       :test (lambda (c) (typep c 'abort-gesture))
       nil)))

;;; 22.4 The Pointer Protocol
;;;
;;; Implemented by the back end.  Sort of.

;;; FIXME: I think the standard-pointer should absorb some of the
;;; common methods that are currently entirely provided by the
;;; backends.

(defclass standard-pointer (pointer)
  ((port :reader port :initarg :port)
   (state-lock :reader state-lock :initform (make-lock "pointer lock"))
   (button-state :initform 0 )
   (modifier-state :initform 0)))

(defgeneric pointer-sheet (pointer))

(defmethod pointer-sheet ((pointer pointer))
  (port-pointer-sheet (port pointer)))

(defgeneric (setf pointer-sheet) (sheet pointer))

(defgeneric pointer-button-state (pointer))

(defgeneric pointer-modifier-state (pointer))

(defgeneric pointer-position (pointer))

(defgeneric* (setf pointer-position) (x y pointer))

(defgeneric synthesize-pointer-motion-event (pointer)
  (:documentation "Create a CLIM pointer motion event based on the current pointer state."))

(defgeneric pointer-cursor (pointer))

(defgeneric (setf pointer-cursor) (cursor pointer))

(defmethod pointer-button-state ((pointer standard-pointer))
  (with-lock-held ((state-lock pointer))
    (slot-value pointer 'button-state)))

(defmethod pointer-modifier-state ((pointer standard-pointer))
  (with-lock-held ((state-lock pointer))
    (slot-value pointer 'modifier-state)))

(defmethod pointer-update-state
    ((pointer standard-pointer) (event keyboard-event))
  (with-lock-held ((state-lock pointer))
    (setf (slot-value pointer 'modifier-state) (event-modifier-state event))))

(defmethod pointer-update-state
    ((pointer standard-pointer) (event pointer-button-press-event))
  (with-lock-held ((state-lock pointer))
    (setf (slot-value pointer 'button-state)
          (logior (slot-value pointer 'button-state)
                  (pointer-event-button event)))))

(defmethod pointer-update-state
    ((pointer standard-pointer) (event pointer-button-release-event))
  (with-lock-held ((state-lock pointer))
    (setf (slot-value pointer 'button-state)
          (logandc2 (slot-value pointer 'button-state)
                    (pointer-event-button event)))))

(defmethod stream-pointer-position ((stream standard-extended-input-stream)
                                    &key (pointer
                                          (port-pointer (port stream))))
  (multiple-value-bind (x y)
      (pointer-position pointer)
    (let ((graft (graft (port pointer))))
      (untransform-position (sheet-delta-transformation stream graft) x y))))

(defmethod* (setf stream-pointer-position) (x y (stream standard-extended-input-stream))
  (let ((graft (graft stream))
        (pointer (port-pointer (port stream))))
    (multiple-value-bind (x y)
        (transform-position (sheet-delta-transformation stream graft) x y)
      (setf (pointer-position pointer) (values x y)))))
