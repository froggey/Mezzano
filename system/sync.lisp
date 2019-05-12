;;;; Copyright (c) 2019 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Syncronization primitives.
;;;;
;;;; This is the high-level side, expanding on what the supervisor provides.

(defpackage :mezzano.sync
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor))
  (:import-from :mezzano.supervisor
                #:wait-for-objects
                #:get-object-event)
  (:export #:wait-for-objects
           #:wait-for-objects-with-timeout
           #:get-object-event))

(in-package :mezzano.sync)

(defgeneric get-object-event (object))

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
         (let ((timer (sup:make-timer :relative timeout)))
           (unwind-protect
                (values (remove timer (apply #'wait-for-objects (list* timer objects)))
                        (sup:timer-remaining timer))
             ;; Be a good citizen and disarm the timer once we're done with it.
             ;; This stops it from hanging around longer than it needs to.
             (sup:timer-disarm timer))))))

(defmethod get-object-event ((object sup:event))
  object)

(defmethod print-object ((object sup:event) stream)
  (let ((name (sup:event-name object)))
    (cond (name
           (print-unreadable-object (object stream :type t :identity t)
             (format stream "~A" name)))
          (t
           (print-unreadable-object (object stream :type t :identity t))))))

(defmethod get-object-event ((object sup:timer))
  (sup::timer-event object))

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
