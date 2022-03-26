;;;; -*- indent-tabs-mode: nil -*-

;; Lispworks condition support is simulated, albeit via a lightweight wrapper over
;; its own polling-based wait primitive.  Waiters register with the condition variable,
;; and use MP:process-wait which queries for permission to proceed at its own (usspecified) interval.
;; http://www.lispworks.com/documentation/lw51/LWRM/html/lwref-445.htm
;; A wakeup callback (on notify) is provided to lighten this query to not have to do a hash lookup
;; on every poll (or have to serialize on the condition variable) and a mechanism is put
;; in place to unregister any waiter that exits wait for other reasons,
;; and to resend any (single) notification that may have been consumed before this (corner
;; case).  Much of the complexity present is to support single notification (as recommended in
;; the spec); but a distinct condition-notify-all is provided for reference.
;; Single-notification follows a first-in first-out ordering
;;
;; Performance:  With 1000 threads waiting on one condition-variable, the steady-state hit (at least
;; as tested on a 3GHz Win32 box) is noise - hovering at 0% on Task manager.
;; While not true zero like a true native solution, the use of the Lispworks native checks appear
;; fast enough to be an equivalent substitute (thread count will cause issue before the
;; waiting overhead becomes significant)
(defstruct (condition-variable (:constructor make-lw-condition (name)))
  name
  (lock (mp:make-lock :name "For condition-variable") :type mp:lock :read-only t)
  (wait-tlist (cons nil nil) :type cons :read-only t)
  (wait-hash (make-hash-table :test 'eq) :type hash-table :read-only t)
  ;; unconsumed-notifications is to track :remove-from-consideration
  ;; for entries that may have exited prematurely - notification is sent through
  ;; to someone else, and offender is removed from hash and list
  (unconsumed-notifications (make-hash-table :test 'eq) :type hash-table :read-only t))

(defun make-condition-variable (&key name)
  (make-lw-condition name))

(defmacro with-cv-access (condition-variable &body body)
  (let ((cv-sym (gensym))
        (slots '(lock wait-tlist wait-hash unconsumed-notifications)))
    `(let ((,cv-sym ,condition-variable))
       (with-slots ,slots
           ,cv-sym
         (macrolet ((locked (&body body) `(mp:with-lock (lock) ,@body)))
           (labels ((,(gensym) () ,@slots))) ; Trigger expansion of the symbol-macrolets to ignore
           ,@body)))))

(defmacro defcvfun (function-name (condition-variable &rest args) &body body)
  `(defun ,function-name (,condition-variable ,@args)
     (with-cv-access ,condition-variable
       ,@body)))
#+lispworks (editor:setup-indent "defcvfun" 2 2 7) ; indent defcvfun

; utility function thath assumes process is locked on condition-variable's lock.
(defcvfun do-notify-single (condition-variable) ; assumes already locked
  (let ((id (caar wait-tlist)))
    (when id
      (pop (car wait-tlist))
      (unless (car wait-tlist) ; check for empty
        (setf (cdr wait-tlist) nil))
      (funcall (gethash id wait-hash)) ; call waiter-wakeup
      (remhash id wait-hash) ; absence of entry = permission to proceed
      (setf (gethash id unconsumed-notifications) t))))

;; Added for completeness/to show how it's done in this paradigm; but
;; The symbol for this call is not exposed in the api
(defcvfun condition-notify-all (condition-variable)
  (locked
   (loop for waiter-wakeup being the hash-values in wait-hash do (funcall waiter-wakeup))
   (clrhash wait-hash)
   (clrhash unconsumed-notifications) ; don't care as everyone just got notified
   (setf (car wait-tlist) nil)
   (setf (cdr wait-tlist) nil)))

;; Currently implemented so as to notify only one waiting thread
(defcvfun condition-notify (condition-variable)
  (locked (do-notify-single condition-variable)))

(defun delete-from-tlist (tlist element)
  (let ((deleter
         (lambda ()
           (setf (car tlist) (cdar tlist))
           (unless (car tlist)
             (setf (cdr tlist) nil)))))
    (loop for cons in (car tlist) do
          (if (eq element (car cons))
              (progn
                (funcall deleter)
                (return nil))
            (let ((cons cons))
              (setq deleter
                    (lambda ()
                      (setf (cdr cons) (cddr cons))
                      (unless (cdr cons)
                        (setf (cdr tlist) cons)))))))))

(defun add-to-tlist-tail (tlist element)
  (let ((new-link (cons element nil)))
    (cond
     ((car tlist)
      (setf (cddr tlist) new-link)
      (setf (cdr tlist) new-link))
     (t
      (setf (car tlist) new-link)
      (setf (cdr tlist) new-link)))))

(defcvfun condition-wait (condition-variable lock- &key timeout)
  (signal-error-if-condition-wait-timeout timeout)
  (mp:process-unlock lock-)
  (unwind-protect ; for the re-taking of the lock.  Guarding all of the code
      (let ((wakeup-allowed-to-proceed nil)
            (wakeup-lock (mp:make-lock :name "wakeup lock for condition-wait")))
        ;; wakeup-allowed-to-proceed is an optimisation to avoid having to serialize all waiters and
        ;; search the hashtable.  That it is locked is for safety/completeness, although
        ;; as wakeup-allowed-to-proceed only transitions nil -> t, and that missing it once or twice is
        ;; moot in this situation, it would be redundant even if ever a Lispworks implementation ever became
        ;; non-atomic in its assigments
        (let ((id (cons nil nil))
              (clean-exit nil))
          (locked
           (add-to-tlist-tail wait-tlist id)
           (setf (gethash id wait-hash) (lambda () (mp:with-lock (wakeup-lock) (setq wakeup-allowed-to-proceed t)))))
          (unwind-protect
              (progn
                (mp:process-wait
                 "Waiting for notification"
                 (lambda ()
                   (when (mp:with-lock (wakeup-lock) wakeup-allowed-to-proceed)
                     (locked (not (gethash id wait-hash))))))
                (locked (remhash id unconsumed-notifications))
                (setq clean-exit t)) ; Notification was consumed
            ;; Have to call remove-from-consideration just in case process was interrupted
            ;; rather than having condition met
            (unless clean-exit ; clean-exit is just an optimization
              (locked
               (when (gethash id wait-hash) ; not notified - must have been interrupted
                 ;; Have to unsubscribe
                 (remhash id wait-hash)
                 (delete-from-tlist wait-tlist id))
               ;; note - it's possible to be removed from wait-hash/wait-tlist (in notify-single); but still have an unconsumed notification!
               (when (gethash id unconsumed-notifications) ; Must have exited for reasons unrelated to notification
                 (remhash id unconsumed-notifications) ; Have to pass on the notification to an eligible waiter
                 (do-notify-single condition-variable)))))))
    (mp:process-lock lock-))
  t)

(define-condition-wait-compiler-macro)
