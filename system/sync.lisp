;;;; Syncronization primitives.
;;;;
;;;; This is the high-level side, expanding on what the supervisor provides.

(defpackage :mezzano.sync
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor))
  (:import-from :mezzano.supervisor
                #:wait-for-objects
                #:get-object-event
                #:thread-pool-block)
  (:export #:wait-for-objects
           #:wait-for-objects-with-timeout
           #:get-object-event

           #:name

           #:thread-pool-block

           #:always-false-event
           #:always-true-event

           #:semaphore
           #:make-semaphore
           #:semaphore-value
           #:semaphore-limit
           #:semaphore-up
           #:semaphore-down

           #:mailbox
           #:make-mailbox
           #:mailbox-capacity
           #:mailbox-n-pending-messages
           #:mailbox-empty-p
           #:mailbox-send-possible-event
           #:mailbox-receive-possible-event
           #:mailbox-send
           #:mailbox-receive
           #:mailbox-peek
           #:mailbox-flush

           #:watchable-set
           #:make-watchable-set
           #:watchable-set-add-item
           #:watchable-set-rem-item
           #:watchable-set-items
           #:watchable-set-contains
           #:watchable-set-add-watcher
           #:watchable-set-rem-watcher
           ))

(in-package :mezzano.sync)

(defgeneric thread-pool-block (thread-pool blocking-function &rest arguments)
  (:documentation "Called when the current thread's thread-pool slot
is non-NIL and the thread is about to block. The thread-pool slot
is bound to NIL for the duration of the call."))

(defgeneric get-object-event (object)
  (:documentation "Return the underlying event associated with OBJECT.
This must return an event object."))

(defgeneric name (object)
  (:documentation "Return the name of OBJECT.")
  (:method (object) nil))

(defun wait-for-objects-with-timeout (timeout &rest objects)
  "As with WAIT-FOR-OBJECTS, but with a timeout.
If TIMEOUT is NIL then this is equivalent to WAIT-FOR-OBJECTS.
Otherwise it is as if a timer object with the given TIMEOUT was included with OBJECTS.
Returns NIL if the timeout expires.
Returns the number of seconds remaining as a secondary value if TIMEOUT is non-NIL."
  (cond ((null timeout)
         ;; No timeout.
         (values (apply #'wait-for-objects objects)
                 nil))
        ((not (plusp timeout))
         ;; Special case, zero or negative timeout - just poll the events.
         (values (loop
                    for object in objects
                    when (event-wait (get-object-event object) nil)
                    collect object)
                 0))
        (t
         ;; Arbitrary timeout.
         (sup:with-timer (timer :relative timeout :name 'wait-for-objects-with-timeout)
           (values (remove timer (apply #'wait-for-objects (list* timer objects)))
                   (sup:timer-remaining timer))))))

(defmethod get-object-event ((object sup:event))
  object)

(defmethod name ((object sup:event))
  (sup:event-name object))

(defmethod print-object ((object sup:event) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((name (sup:event-name object)))
      (when name
        (format stream "~A " name))
      (format stream "=> ~S" (sup:event-state object)))))

(mezzano.internals::defglobal *always-false-event* (sup:make-event :name "Always false"))
(defun always-false-event () *always-false-event*)

(mezzano.internals::defglobal *always-true-event* (sup:make-event :name "Always true" :state t))
(defun always-true-event () *always-true-event*)

(defmethod get-object-event ((object sup:timer))
  (sup::timer-event object))

(defmethod name ((object sup:timer))
  (sup:timer-name object))

(defmethod print-object ((object sup:timer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((name (sup:timer-name object))
          (deadline (sup:timer-deadline object)))
      (when name
        (format stream "~A " name))
      (cond (deadline
             (let ((remaining (- deadline (get-internal-run-time))))
               (cond ((<= remaining 0)
                      (format stream "[expired ~D seconds ago]"
                              (float (/ (- remaining) internal-time-units-per-second))))
                     (t
                      (format stream "[~D seconds remaining]"
                              (float (/ remaining internal-time-units-per-second)))))))
            (t
             (format stream "[disarmed]"))))))

(defmethod get-object-event ((object sup:simple-irq))
  (sup::simple-irq-event object))

(defmethod name ((object sup:simple-irq))
  `(:irq ,(sup:simple-irq-irq object)))

(defmethod print-object ((object sup:simple-irq) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":Irq ~A :Pending ~A :Masked ~A"
            (sup:simple-irq-irq object)
            (sup:simple-irq-pending-p object)
            (sup:simple-irq-masked-p object))))

(defmethod get-object-event ((object sup:irq-fifo))
  (sup::irq-fifo-data-available object))

(defmethod name ((object sup:irq-fifo))
  (sup:irq-fifo-name object))

(defmethod print-object ((object sup:irq-fifo) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (sup:irq-fifo-name object))))

(defmethod get-object-event ((object sup:thread))
  (sup::thread-join-event object))

(defmethod name ((object sup:thread))
  (sup:thread-name object))

;;;; Semaphore.

(defclass semaphore ()
  ((%not-zero-event :reader semaphore-not-zero-event)
   (%lock :initform (sup:make-mutex "Internal semaphore lock") :reader semaphore-lock)
   (%value :initarg :value :accessor %semaphore-value :type (integer 0))
   (%limit :initarg :limit :reader semaphore-limit :type (or null (integer 0))))
  (:default-initargs :value 0 :limit nil))

(defmethod initialize-instance :after ((instance semaphore) &key name)
  (when (and (semaphore-limit instance)
             (> (%semaphore-value instance) (semaphore-limit instance)))
    (error "Semaphore initial value ~D exceeds limit ~D"
           (%semaphore-value instance) (semaphore-limit instance)))
  (setf (slot-value instance '%not-zero-event)
        (sup:make-event :name name
                        :state (not (zerop (%semaphore-value instance))))))

(defmethod print-object ((object semaphore) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (name object)
      (format stream "~A " (name object)))
    (format stream "~A" (semaphore-value object))
    (when (semaphore-limit object)
      (format stream "/~A" (semaphore-limit object)))))

(defmethod get-object-event ((object semaphore))
  (semaphore-not-zero-event object))

;;; Public API:

(defun make-semaphore (&key name (value 0) limit)
  (check-type value (integer 0))
  (check-type limit (or null (integer 0)))
  (make-instance 'semaphore :name name :value value :limit limit))

(defmethod name ((object semaphore))
  (sup:event-name (semaphore-not-zero-event object)))

(defun semaphore-value (semaphore)
  "Return SEMAPHORE's current value."
  (%semaphore-value semaphore))

(defun semaphore-up (semaphore)
  "Increment SEMAPHORE."
  (sup:with-mutex ((semaphore-lock semaphore) :resignal-errors t)
    (let ((limit (semaphore-limit semaphore)))
      (when (and limit (eql (%semaphore-value semaphore) limit))
        (error "Semaphore at limit ~D" limit)))
    (incf (%semaphore-value semaphore))
    (setf (sup:event-state (semaphore-not-zero-event semaphore)) t))
  (values))

(defun semaphore-down (semaphore &key (wait-p t))
  "Decrement SEMAPHORE.
If SEMAPHORE's current value is 0, then this will block if WAIT-P is true
until SEMAPHORE is incremented.
Returns true if SEMAPHORE was decremented, false if WAIT-P is false and the semapore's value is 0."
  (loop
     (sup:with-mutex ((semaphore-lock semaphore))
       (when (not (zerop (%semaphore-value semaphore)))
         (decf (%semaphore-value semaphore))
         (when (zerop (%semaphore-value semaphore))
           (setf (sup:event-state (semaphore-not-zero-event semaphore)) nil))
         (return t)))
     (when (not wait-p)
       (return nil))
     (sup:event-wait (semaphore-not-zero-event semaphore))))

;;;; Mailbox. A buffered communication channel.

(defclass mailbox ()
  ((%name :reader name :initarg :name)
   (%capacity :reader mailbox-capacity :initarg :capacity :type (or null (integer 1)))
   (%not-full-event :reader mailbox-send-possible-event)
   (%not-empty-event :reader mailbox-receive-possible-event)
   (%n-pending :initform 0 :reader mailbox-n-pending-messages)
   (%head :accessor mailbox-head)
   (%tail :accessor mailbox-tail)
   (%lock :reader mailbox-lock))
  (:default-initargs :name nil :capacity nil))

(defmethod print-object ((object mailbox) stream)
  (cond ((name object)
         (print-unreadable-object (object stream :type t :identity t)
           (format stream "~A" (name object))))
        (t
         (print-unreadable-object (object stream :type t :identity t)))))

(defmethod initialize-instance :after ((instance mailbox) &key)
  (check-type (mailbox-capacity instance) (or null (integer 1)))
  (setf (slot-value instance '%lock) (sup:make-mutex instance))
  ;; Mailbox is initially empty.
  (setf (slot-value instance '%not-full-event) (sup:make-event
                                                :name `(mailbox-send-possible-event ,instance)
                                                :state t)
        (slot-value instance '%not-empty-event) (sup:make-event
                                                 :name `(mailbox-receive-possible-event ,instance)
                                                 :state nil))
  (setf (mailbox-head instance) (cons nil nil)
        (mailbox-tail instance) (mailbox-head instance)))

(defmethod get-object-event ((object mailbox))
  ;; Mailbox is ready for receiving as long as it isn't empty
  (slot-value object '%not-empty-event))

;;; Public API:

(defun make-mailbox (&key name capacity)
  "Create a new mailbox.
CAPACITY can be NIL to indicate that there should be no limit on the number of buffered items
or a positive integer to restrict the buffer to that many items.
Returns two values representing the send & receive sides of the mailbox.
Items are sent and received in FIFO order."
  (check-type capacity (or null (integer 1)))
  (make-instance 'mailbox
                 :name name
                 :capacity capacity))

(defun mailbox-send (value mailbox &key (wait-p t))
  "Push a value into the mailbox.
If the mailbox is at capacity, this will block if WAIT-P is true.
Returns true if the value was pushed, false if the mailbox is full and WAIT-P is false."
  (loop
     (sup:with-mutex ((mailbox-lock mailbox))
       (when (or (not (mailbox-capacity mailbox))
                 (< (mailbox-n-pending-messages mailbox) (mailbox-capacity mailbox)))
         ;; Space available, append to the message list.
         (let ((link (cons nil nil)))
           (setf (car (mailbox-tail mailbox)) value
                 (cdr (mailbox-tail mailbox)) link
                 (mailbox-tail mailbox) link))
         (setf (sup:event-state (mailbox-receive-possible-event mailbox)) t)
         (incf (slot-value mailbox '%n-pending))
         (when (eql (mailbox-n-pending-messages mailbox) (mailbox-capacity mailbox))
           ;; Mailbox now full.
           (setf (sup:event-state (mailbox-send-possible-event mailbox)) nil))
         (return t)))
     (when (not wait-p)
       (return nil))
     (sup:event-wait (mailbox-send-possible-event mailbox))))

(defun mailbox-receive (mailbox &key (wait-p t))
  "Pop a value from the mailbox.
If the mailbox is empty, this will block if WAIT-P is true."
  (loop
     (sup:with-mutex ((mailbox-lock mailbox))
       (when (not (zerop (mailbox-n-pending-messages mailbox)))
         ;; Messages pending.
         ;; Grab the first one.
         (let ((message (pop (mailbox-head mailbox))))
           (when (endp (cdr (mailbox-head mailbox)))
             ;; This was the last message.
             (setf (sup:event-state (mailbox-receive-possible-event mailbox)) nil))
           (decf (slot-value mailbox '%n-pending))
           (setf (sup:event-state (mailbox-send-possible-event mailbox)) t)
           (return (values message t))))
       (when (not wait-p)
         (return (values nil nil))))
     (sup:event-wait (mailbox-receive-possible-event mailbox))))

(defun mailbox-peek (mailbox &key (wait-p t))
  "Peek at the next pending message in the mailbox, if any.
Like MAILBOX-RECEIVE, but leaves the message in the mailbox."
  (loop
     (sup:with-mutex ((mailbox-lock mailbox))
       (when (not (zerop (mailbox-n-pending-messages mailbox)))
         ;; Messages pending.
         ;; Grab the first one.
         (return (values (first (mailbox-head mailbox))
                         t)))
       (when (not wait-p)
         (return (values nil nil))))
     (sup:event-wait (mailbox-receive-possible-event mailbox))))

(defun mailbox-flush (mailbox)
  "Empty MAILBOX, returning a list of all pending messages."
  (sup:with-mutex ((mailbox-lock mailbox))
    (let ((messages (butlast (mailbox-head mailbox))))
      (setf (mailbox-head mailbox) (cons nil nil)
            (mailbox-tail mailbox) (mailbox-head mailbox))
      (setf (slot-value mailbox '%n-pending) 0)
      (setf (sup:event-state (mailbox-send-possible-event mailbox)) t
            (sup:event-state (mailbox-receive-possible-event mailbox)) nil)
      messages)))

(defun mailbox-empty-p (mailbox)
  "Returns true if there are no messages waiting."
  (zerop (mailbox-n-pending-messages mailbox)))

;;;; Watchable set.
;;;; A set of objects that reports when objects are added or removed.

(defclass watchable-set ()
  ((%name :initarg :name :reader name)
   (%items :initform '()
           :accessor %watchable-set-items)
   (%lock :initform (sup:make-mutex "watchable-set lock")
          :reader watchable-set-lock)
   (%watchers :initform '() :accessor watchable-set-watchers))
  (:default-initargs :name nil))

;;; Public API:

(defun make-watchable-set (&key name initial-contents)
  (let ((set (make-instance 'watchable-set :name name)))
    (setf (%watchable-set-items set) (copy-list initial-contents))
    set))

(defun watchable-set-add-item (item set)
  "Add ITEM to SET.
Returns true if the item was added to the set; false if the
set already contained it. Notifications are only sent if the
set does not contain the item.
If a notification mailbox is full, then the message is not sent."
  (sup:with-mutex ((watchable-set-lock set))
    (cond ((member item (%watchable-set-items set))
           nil)
          (t
           (push item (%watchable-set-items set))
           (loop
              for (add-mbox . rem-mbox) in (watchable-set-watchers set)
              do (mailbox-send item add-mbox :wait-p nil))
           t))))

(defun watchable-set-rem-item (item set)
  "Remove ITEM from SET.
Returns true if the item was removed to the set; false if the
set did not contain it. Notifications are only sent if the
set does contained the item.
If a notification mailbox is full, then the message is not sent."
  (sup:with-mutex ((watchable-set-lock set))
    (cond ((member item (%watchable-set-items set))
           (setf (%watchable-set-items set)
                 (remove item (%watchable-set-items set)))
           (loop
              for (add-mbox . rem-mbox) in (watchable-set-watchers set)
              do (mailbox-send item rem-mbox :wait-p nil))
           t)
          (t nil))))

(defun watchable-set-items (set)
  "Return all items currently contained in SET."
  (%watchable-set-items set))

(defun watchable-set-contains (item set)
  "Returns true if SET contains ITEM."
  (sup:with-mutex ((watchable-set-lock set))
    (member item (%watchable-set-items set))))

(defun watchable-set-add-watcher (add-mbox rem-mbox set)
  "Register ADD-MBOX/REM-MBOX as watchers for SET.
Any items already conatined in SET will be sent to ADD-MBOX.
Returns a tag that can be passed to WATCHABLE-SET-REM-WATCHER to unregister."
  (check-type add-mbox mailbox)
  (check-type rem-mbox mailbox)
  (sup:with-mutex ((watchable-set-lock set))
    (let ((tag (cons add-mbox rem-mbox)))
      (push tag (watchable-set-watchers set))
      (dolist (item (%watchable-set-items set))
        (mailbox-send item add-mbox :wait-p nil))
      tag)))

(defun watchable-set-rem-watcher (tag set)
  (sup:with-mutex ((watchable-set-lock set))
    (setf (watchable-set-watchers set)
          (remove tag (watchable-set-watchers set))))
  (values))

;;;; Compatibility wrappers.

(deftype sup:latch ()
  'event)

(defun sup:latch-p (object)
  (typep object 'sup:latch))

(defun sup:make-latch (&optional name)
  (make-event :name name))

(defun sup:latch-reset (latch)
  (setf (event-state latch) nil))

(defun sup:latch-wait (latch)
  (event-wait latch)
  (values))

(defun sup:latch-trigger (latch)
  (setf (event-state latch) t))

(deftype sup:fifo ()
  'mailbox)

(defun sup:fifo-p (object)
  (typep object 'sup:fifo))

(defun sup:make-fifo (size &key (element-type 't))
  (declare (ignore element-type))
  (make-mailbox :capacity size))

(defun sup:fifo-push (value fifo &optional (wait-p t))
  (mailbox-send value fifo :wait-p wait-p))

(defun sup:fifo-pop (fifo &optional (wait-p t))
  (mailbox-receive fifo :wait-p wait-p))

(defun sup:fifo-reset (fifo)
  (mailbox-flush fifo))

(defun sup:fifo-size (fifo)
  (mailbox-capacity fifo))

(defun sup:fifo-element-type (fifo)
  (declare (ignore fifo))
  't)
