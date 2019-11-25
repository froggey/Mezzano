;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

(in-package :mezzano.driver.usb.threads)

(defconstant +number-of-threads+ 4)

(defvar *thread-pool* NIL)

(defstruct thread-pool
  threads
  lock
  cvar
  event-fifo)

(defun worker-thread-main (pool thread-num)
  (sup:debug-print-line "USB worker thread " thread-num " started")
  (let ((lock (thread-pool-lock pool))
        (cvar (thread-pool-cvar pool))
        (event-fifo (thread-pool-event-fifo pool))
        (event))
    (loop
       (sup:with-mutex ((thread-pool-lock pool))
         (sup:condition-wait cvar lock)
         (setf event (sup:fifo-pop event-fifo)))
       (block :process-event
         (handler-bind
             ((error
               (lambda (c)
                 (ignore-errors
                   (let ((*standard-output* *error-output*))
                     (format *error-output* "~&Error ~A.~%" c)
                     (mezzano.internals::backtrace)))
                 (return-from :process-event (values nil c))))
              (controller-disconnect
               (lambda (c)
                 (delete-controller (disconnect-hcd c))
                 (return-from :process-event) (values nil c))))
           (cond ((typep event 'interrupt-event)
                  (unwind-protect
                       (handle-interrupt-event (interrupt-event-type event)
                                               (interrupt-event-hcd event)
                                               event)
                    (when (not (eq (interrupt-event-type event) :free))
                      (free-interrupt-event (interrupt-event-hcd event) event))))
                 ((typep event 'usb-event)
                  (handle-usb-event (usb-event-type event)
                                    (usb-event-dest event)
                                    event))))))))

(defun create-worker-thread (pool name thread-num)
  (push (sup:make-thread
         #'(lambda () (worker-thread-main pool thread-num)) :name name)
        (thread-pool-threads pool)))

(defun create-thread-pool ()
  (when (null *thread-pool*)
    (setf *thread-pool* (make-thread-pool
                         :threads nil
                         :lock (sup:make-mutex "Thread-Pool lock")
                         :cvar (sup:make-condition-variable "Thread-Pool cvar")
                         :event-fifo (sup:make-fifo nil)))
    (dotimes (i +number-of-threads+)
      (create-worker-thread *thread-pool* (format nil "USB worker ~D" i) i)))
  (thread-pool-event-fifo *thread-pool*))

(defun enqueue-event (event)
  (sup:fifo-push event (thread-pool-event-fifo *thread-pool*))
  (sup:condition-notify (thread-pool-cvar *thread-pool*)))
