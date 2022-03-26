;;;; -*- indent-tabs-mode: nil -*-

(in-package #:bordeaux-threads)

;;; Helper macros

(defmacro defdfun (name args doc &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (fboundp ',name)
       (defun ,name ,args ,@body))
     (setf (documentation ',name 'function)
           (or (documentation ',name 'function) ,doc))))

(defmacro defdmacro (name args doc &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (fboundp ',name)
       (defmacro ,name ,args ,@body))
     (setf (documentation ',name 'function)
           (or (documentation ',name 'function) ,doc))))

;;; Thread Creation

(defdfun start-multiprocessing ()
  "If the host implementation uses user-level threads, start the
scheduler and multiprocessing, otherwise do nothing.
It is safe to call repeatedly."
  nil)

(defdfun make-thread (function &key name
                      (initial-bindings *default-special-bindings*))
  "Creates and returns a thread named NAME, which will call the
  function FUNCTION with no arguments: when FUNCTION returns, the
  thread terminates. NAME defaults to \"Anonymous thread\" if unsupplied.

  On systems that do not support multi-threading, MAKE-THREAD will
  signal an error.

  The interaction between threads and dynamic variables is in some
  cases complex, and depends on whether the variable has only a global
  binding (as established by e.g. DEFVAR/DEFPARAMETER/top-level SETQ)
  or has been bound locally (e.g. with LET or LET*) in the calling
  thread.

  - Global bindings are shared between threads: the initial value of a
    global variable in the new thread will be the same as in the
    parent, and an assignment to such a variable in any thread will be
    visible to all threads in which the global binding is visible.

  - Local bindings, such as the ones introduced by INITIAL-BINDINGS,
    are local to the thread they are introduced in, except that

  - Local bindings in the the caller of MAKE-THREAD may or may not be
    shared with the new thread that it creates: this is
    implementation-defined. Portable code should not depend on
    particular behaviour in this case, nor should it assign to such
    variables without first rebinding them in the new thread."
  (%make-thread (binding-default-specials function initial-bindings)
                (or name "Anonymous thread")))

(defdfun %make-thread (function name)
  "The actual implementation-dependent function that creates threads."
  (declare (ignore function name))
  (error (make-threading-support-error)))

(defdfun current-thread ()
  "Returns the thread object for the calling
  thread. This is the same kind of object as would be returned by
  MAKE-THREAD."
  nil)

(defdfun threadp (object)
  "Returns true if object is a thread, otherwise NIL."
  (declare (ignore object))
  nil)

(defdfun thread-name (thread)
  "Returns the name of the thread, as supplied to MAKE-THREAD."
  (declare (ignore thread))
  "Main thread")

;;; Resource contention: locks and recursive locks

(defdfun lock-p (object)
  "Returns T if OBJECT is a lock; returns NIL otherwise."
  (declare (ignore object))
  nil)

(defdfun recursive-lock-p (object)
  "Returns T if OBJECT is a recursive lock; returns NIL otherwise."
  (declare (ignore object))
  nil)

(defdfun make-lock (&optional name)
  "Creates a lock (a mutex) whose name is NAME. If the system does not
  support multiple threads this will still return some object, but it
  may not be used for very much."
  ;; In CLIM-SYS this is a freshly consed list (NIL). I don't know if
  ;; there's some good reason it should be said structure or that it
  ;; be freshly consed - EQ comparison of locks?
  (declare (ignore name))
  (list nil))

(defdfun acquire-lock (lock &optional wait-p)
  "Acquire the lock LOCK for the calling thread.
  WAIT-P governs what happens if the lock is not available: if WAIT-P
  is true, the calling thread will wait until the lock is available
  and then acquire it; if WAIT-P is NIL, ACQUIRE-LOCK will return
  immediately. ACQUIRE-LOCK returns true if the lock was acquired and
  NIL otherwise.

  This specification does not define what happens if a thread
  attempts to acquire a lock that it already holds. For applications
  that require locks to be safe when acquired recursively, see instead
  MAKE-RECURSIVE-LOCK and friends."
  (declare (ignore lock wait-p))
  t)

(defdfun release-lock (lock)
  "Release LOCK. It is an error to call this unless
  the lock has previously been acquired (and not released) by the same
  thread. If other threads are waiting for the lock, the
  ACQUIRE-LOCK call in one of them will now be able to continue.

  This function has no interesting return value."
  (declare (ignore lock))
  (values))

(defdmacro with-lock-held ((place) &body body)
  "Evaluates BODY with the lock named by PLACE, the value of which
  is a lock created by MAKE-LOCK. Before the forms in BODY are
  evaluated, the lock is acquired as if by using ACQUIRE-LOCK. After the
  forms in BODY have been evaluated, or if a non-local control transfer
  is caused (e.g. by THROW or SIGNAL), the lock is released as if by
  RELEASE-LOCK.

  Note that if the debugger is entered, it is unspecified whether the
  lock is released at debugger entry or at debugger exit when execution
  is restarted."
  `(when (acquire-lock ,place t)
     (unwind-protect
          (locally ,@body)
       (release-lock ,place))))

(defdfun make-recursive-lock (&optional name)
  "Create and return a recursive lock whose name is NAME. A recursive
  lock differs from an ordinary lock in that a thread that already
  holds the recursive lock can acquire it again without blocking. The
  thread must then release the lock twice before it becomes available
  for another thread."
  (declare (ignore name))
  (list nil))

(defdfun acquire-recursive-lock (lock)
  "As for ACQUIRE-LOCK, but for recursive locks."
  (declare (ignore lock))
  t)

(defdfun release-recursive-lock (lock)
  "Release the recursive LOCK. The lock will only
  become free after as many Release operations as there have been
  Acquire operations. See RELEASE-LOCK for other information."
  (declare (ignore lock))
  (values))

(defdmacro with-recursive-lock-held ((place &key timeout) &body body)
  "Evaluates BODY with the recursive lock named by PLACE, which is a
reference to a recursive lock created by MAKE-RECURSIVE-LOCK. See
WITH-LOCK-HELD etc etc"
  (declare (ignore timeout))
  `(when (acquire-recursive-lock ,place)
     (unwind-protect
          (locally ,@body)
       (release-recursive-lock ,place))))

;;; Resource contention: condition variables

;;; A condition variable provides a mechanism for threads to put
;;; themselves to sleep while waiting for the state of something to
;;; change, then to be subsequently woken by another thread which has
;;; changed the state.
;;;
;;; A condition variable must be used in conjunction with a lock to
;;; protect access to the state of the object of interest. The
;;; procedure is as follows:
;;;
;;; Suppose two threads A and B, and some kind of notional event
;;; channel C. A is consuming events in C, and B is producing them.
;;; CV is a condition-variable
;;;
;;; 1) A acquires the lock that safeguards access to C
;;; 2) A threads and removes all events that are available in C
;;; 3) When C is empty, A calls CONDITION-WAIT, which atomically
;;;    releases the lock and puts A to sleep on CV
;;; 4) Wait to be notified; CONDITION-WAIT will acquire the lock again
;;;    before returning
;;; 5) Loop back to step 2, for as long as threading should continue
;;;
;;; When B generates an event E, it
;;; 1) acquires the lock guarding C
;;; 2) adds E to the channel
;;; 3) calls CONDITION-NOTIFY on CV to wake any sleeping thread
;;; 4) releases the lock
;;;
;;; To avoid the "lost wakeup" problem, the implementation must
;;; guarantee that CONDITION-WAIT in thread A atomically releases the
;;; lock and sleeps. If this is not guaranteed there is the
;;; possibility that thread B can add an event and call
;;; CONDITION-NOTIFY between the lock release and the sleep - in this
;;; case the notify call would not see A, which would be left sleeping
;;; despite there being an event available.

(defdfun thread-yield ()
  "Allows other threads to run. It may be necessary or desirable to
  call this periodically in some implementations; others may schedule
  threads automatically. On systems that do not support
  multi-threading, this does nothing."
  (values))

(defdfun make-condition-variable (&key name)
  "Returns a new condition-variable object for use
  with CONDITION-WAIT and CONDITION-NOTIFY."
  (declare (ignore name))
  nil)

(defdfun condition-wait (condition-variable lock &key timeout)
  "Atomically release LOCK and enqueue the calling
  thread waiting for CONDITION-VARIABLE. The thread will resume when
  another thread has notified it using CONDITION-NOTIFY; it may also
  resume if interrupted by some external event or in other
  implementation-dependent circumstances: the caller must always test
  on waking that there is threading to be done, instead of assuming
  that it can go ahead.

  It is an error to call function this unless from the thread that
  holds LOCK.

  If TIMEOUT is nil or not provided, the system always reacquires LOCK
  before returning to the caller. In this case T is returned.

  If TIMEOUT is non-nil, the call will return after at most TIMEOUT
  seconds (approximately), whether or not a notification has occurred.
  Either NIL or T will be returned. A return of NIL indicates that the
  lock is no longer held and that the timeout has expired. A return of
  T indicates that the lock is held, in which case the timeout may or
  may not have expired.

  **NOTE**: The behavior of CONDITION-WAIT with TIMEOUT diverges from
  the POSIX function pthread_cond_timedwait. The former may return
  without the lock being held while the latter always returns with the
  lock held.

  In an implementation that does not support multiple threads, this
  function signals an error."
  (declare (ignore condition-variable lock timeout))
  (error (make-threading-support-error)))

(defdfun condition-notify (condition-variable)
  "Notify at least one of the threads waiting for
  CONDITION-VARIABLE. It is implementation-dependent whether one or
  more than one (and possibly all) threads are woken, but if the
  implementation is capable of waking only a single thread (not all
  are) this is probably preferable for efficiency reasons. The order
  of wakeup is unspecified and does not necessarily relate to the
  order that the threads went to sleep in.

  CONDITION-NOTIFY has no useful return value. In an implementation
  that does not support multiple threads, it has no effect."
  (declare (ignore condition-variable))
  (values))

;;; Timeouts

(defdmacro with-timeout ((timeout) &body body)
  "Execute `BODY' and signal a condition of type TIMEOUT if the execution of
BODY does not complete within `TIMEOUT' seconds. On implementations which do not
support WITH-TIMEOUT natively and don't support threads either it has no effect."
  (declare (ignorable timeout))
  #+thread-support
  (let ((ok-tag (gensym "OK"))
        (timeout-tag (gensym "TIMEOUT"))
        (caller (gensym "CALLER"))
        (sleeper (gensym "SLEEPER")))
    (once-only (timeout)
      `(let (,sleeper)
         (multiple-value-prog1
             (catch ',ok-tag
               (catch ',timeout-tag
                 (let ((,caller (current-thread)))
                   (setf ,sleeper
                         (make-thread #'(lambda ()
                                          (sleep ,timeout)
                                          (interrupt-thread ,caller
                                                            #'(lambda ()
                                                                (ignore-errors
                                                                  (throw ',timeout-tag nil)))))
                                      :name (format nil "WITH-TIMEOUT thread serving: ~S."
                                                    (thread-name ,caller))))
                   (throw ',ok-tag (progn ,@body))))
               (error 'timeout :length ,timeout))
           (when (thread-alive-p ,sleeper)
             (destroy-thread ,sleeper))))))
  #-thread-support
  `(progn
     ,@body))

;;; Introspection/debugging

;;; The following functions may be provided for debugging purposes,
;;; but are not advised to be called from normal user code.

(defdfun all-threads ()
  "Returns a sequence of all of the threads. This may not
  be freshly-allocated, so the caller should not modify it."
  (error (make-threading-support-error)))

(defdfun interrupt-thread (thread function)
  "Interrupt THREAD and cause it to evaluate FUNCTION
  before continuing with the interrupted path of execution. This may
  not be a good idea if THREAD is holding locks or doing anything
  important. On systems that do not support multiple threads, this
  function signals an error."
  (declare (ignore thread function))
  (error (make-threading-support-error)))

(defdfun destroy-thread (thread)
  "Terminates the thread THREAD, which is an object
  as returned by MAKE-THREAD. This should be used with caution: it is
  implementation-defined whether the thread runs cleanup forms or
  releases its locks first.

  Destroying the calling thread is an error."
  (declare (ignore thread))
  (error (make-threading-support-error)))

(defdfun thread-alive-p (thread)
  "Returns true if THREAD is alive, that is, if
  DESTROY-THREAD has not been called on it."
  (declare (ignore thread))
  (error (make-threading-support-error)))

(defdfun join-thread (thread)
  "Wait until THREAD terminates. If THREAD
  has already terminated, return immediately."
  (declare (ignore thread))
  (error (make-threading-support-error)))
