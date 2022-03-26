#|
Copyright 2006,2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(defpackage bordeaux-threads/test
  (:use #:cl #:bordeaux-threads #:fiveam)
  (:shadow #:with-timeout))

(in-package #:bordeaux-threads/test)

(def-suite :bordeaux-threads)
(def-fixture using-lock () 
  (let ((lock (make-lock)))
    (&body)))
(in-suite :bordeaux-threads)

(test should-have-current-thread
  (is (current-thread)))

(test current-thread-identity
  (let* ((box (list nil))
         (thread (make-thread (lambda ()
                                (setf (car box) (current-thread))))))
    (join-thread thread)
    (is (eql (car box) thread))))

(test join-thread-return-value
  (is (eql 0 (join-thread (make-thread (lambda () 0))))))

(test should-identify-threads-correctly
  (is (threadp (current-thread)))
  (is (threadp (make-thread (lambda () t) :name "foo")))
  (is (not (threadp (make-lock)))))

(test should-retrieve-thread-name
  (is (equal "foo" (thread-name (make-thread (lambda () t) :name "foo")))))

(test interrupt-thread
  (let* ((box (list nil))
         (thread (make-thread (lambda ()
                                (setf (car box)
                                      (catch 'new-thread
                                        (sleep 60)
                                        'not-interrupted))))))
    (sleep 1)
    (interrupt-thread thread (lambda ()
                               (throw 'new-thread 'interrupted)))
    (join-thread thread)
    (is (eql 'interrupted (car box)))))

(test should-lock-without-contention
  (with-fixture using-lock ()
    (is (acquire-lock lock t))
    (release-lock lock)
    (is (acquire-lock lock nil))
    (release-lock lock)))

(defun set-equal (set-a set-b)
  (and (null (set-difference set-a set-b))
       (null (set-difference set-b set-a))))

(test default-special-bindings
  (locally (declare (special *a* *c*))
    (let* ((the-as 50) (the-bs 150) (*b* 42)
           some-a some-b some-other-a some-other-b
           (*default-special-bindings*
            `((*a* . (funcall ,(lambda () (incf the-as))))
              (*b* . (funcall ,(lambda () (incf the-bs))))
              ,@*default-special-bindings*))
           (threads (list (make-thread
                           (lambda ()
                             (setf some-a *a* some-b *b*)))
                          (make-thread
                           (lambda ()
                             (setf some-other-a *a*
                                   some-other-b *b*))))))
      (declare (special *b*))
      (thread-yield)
      (is (not (boundp '*a*)))
      (loop while (some #'thread-alive-p threads)
            do (thread-yield))
      (is (set-equal (list some-a some-other-a) '(51 52)))
      (is (set-equal (list some-b some-other-b) '(151 152)))
      (is (not (boundp '*a*))))))


(defparameter *shared* 0)
(defparameter *lock* (make-lock))

(test should-have-thread-interaction
  ;; this simple test generates N process. Each process grabs and
  ;; releases the lock until SHARED has some value, it then
  ;; increments SHARED. the outer code first sets shared 1 which
  ;; gets the thing running and then waits for SHARED to reach some
  ;; value. this should, i think, stress test locks.
  (setf *shared* 0)
  (flet ((worker (i)
           (loop
             do (with-lock-held (*lock*)
                  (when (= i *shared*)
                    (incf *shared*)
                    (return)))
                (thread-yield)
                (sleep 0.001))))
    (let* ((procs (loop
                    for i from 1 upto 2
                    ;; create a new binding to protect against implementations that
                    ;; mutate instead of binding the loop variable
                    collect (let ((i i))
                              (make-thread (lambda ()
                                             (funcall #'worker i))
                                           :name (format nil "Proc #~D" i))))))
      (with-lock-held (*lock*)
        (incf *shared*))
      (block test
        (loop
          until (with-lock-held (*lock*)
                  (= (1+ (length procs)) *shared*))
          do (with-lock-held (*lock*)
               (is (>= (1+ (length procs)) *shared*)))
             (thread-yield)
             (sleep 0.001))))))


(defparameter *condition-variable* (make-condition-variable))

(test condition-variable
  (setf *shared* 0)
  (flet ((worker (i)
           (with-lock-held (*lock*)
             (loop
               until (= i *shared*)
               do (condition-wait *condition-variable* *lock*))
             (incf *shared*))
           (condition-notify *condition-variable*)))
    (let ((num-procs 100))
      (dotimes (i num-procs)
        ;; create a new binding to protect against implementations that
        ;; mutate instead of binding the loop variable
        (let ((i i))
          (make-thread (lambda ()
                         (funcall #'worker i))
                       :name (format nil "Proc #~D" i))))
      (with-lock-held (*lock*)
        (loop
          until (= num-procs *shared*)
          do (condition-wait *condition-variable* *lock*)))
      (is (equal num-procs *shared*)))))

;; Generally safe sanity check for the locks and single-notify
#+(and lispworks (not lispworks6))
(test condition-variable-lw
  (let ((condition-variable (make-condition-variable :name "Test"))
        (test-lock (make-lock))
        (completed nil))
    (dotimes (id 6)
      (let ((id id))
        (make-thread (lambda ()
                       (with-lock-held (test-lock)
                         (condition-wait condition-variable test-lock)
                         (push id completed)
                         (condition-notify condition-variable))))))
    (sleep 2)
    (if completed
        (print "Failed: Premature passage through condition-wait")
        (print "Successfully waited on condition"))
    (condition-notify condition-variable)
    (sleep 2)
    (if (and completed
             (eql (length completed) 6)
             (equal (sort completed #'<)
                    (loop for id from 0 to 5 collect id)))
        (print "Success: All elements notified")
        (print (format nil "Failed: Of 6 expected elements, only ~A proceeded" completed)))
    (bt::with-cv-access condition-variable
      (if (and
           (not (or (car wait-tlist) (cdr wait-tlist)))
           (zerop (hash-table-count wait-hash))
           (zerop (hash-table-count unconsumed-notifications)))
          (print "Success: condition variable restored to initial state")
          (print "Error: condition variable retains residue from completed waiters")))
    (setq completed nil)
    (dotimes (id 6)
          (let ((id id))
            (make-thread (lambda ()
                           (with-lock-held (test-lock)
                             (condition-wait condition-variable test-lock)
                             (push id completed))))))
    (sleep 2)
    (condition-notify condition-variable)
    (sleep 2)
    (if (= (length completed) 1)
        (print "Success: Notify-single only notified a single waiter to restart")
        (format t "Failure: Notify-single restarted ~A items" (length completed)))
    (condition-notify condition-variable)
    (sleep 2)
    (if (= (length completed) 2)
        (print "Success: second Notify-single only notified a single waiter to restart")
        (format t "Failure: Two Notify-singles restarted ~A items" (length completed)))
    (loop for i from 0 to 5 do (condition-notify condition-variable))
    (print "Note:  In the case of any failures, assume there are outstanding waiting threads")
    (values)))

#+(or abcl allegro clisp clozure ecl lispworks6 mezzano sbcl scl)
(test condition-wait-timeout
  (let ((lock (make-lock))
        (cvar (make-condition-variable))
        (flag nil))
    (make-thread (lambda () (sleep 0.4) (setf flag t)))
    (with-lock-held (lock)
      (condition-wait cvar lock :timeout 0.2)
      (is (null flag))
      (sleep 0.4)
      (is (eq t flag)))))
