;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; High-level interrupt management.

(in-package :mezzano.supervisor)

(declaim (inline ensure-interrupts-enabled ensure-interrupts-disabled))
(defun ensure-interrupts-enabled ()
  (when (not (sys.int::%interrupt-state))
    (panic "Interrupts disabled when they shouldn't be.")))

(defun ensure-interrupts-disabled ()
  (when (sys.int::%interrupt-state)
    (panic "Interrupts enabled when they shouldn't be.")))

(defmacro without-interrupts (&body body)
  "Execute body with local IRQs inhibited."
  (let ((irq-state (gensym)))
    `(let ((,irq-state (sys.int::%save-irq-state)))
       (ensure-on-wired-stack)
       (%disable-interrupts)
       (unwind-protect
            (progn ,@body)
         (sys.int::%restore-irq-state ,irq-state)))))

(defmacro safe-without-interrupts ((&rest captures) &body body)
  "Execute body with local IRQs inhibited.
This can be used when executing on any stack.
RETURN-FROM/GO must not be used to leave this form."
  (let ((sp (gensym))
        (fp (gensym)))
    `(%run-on-wired-stack-without-interrupts (,sp ,fp ,@captures)
      (declare (ignore ,sp ,fp))
      ,@body)))

(defmacro %run-on-wired-stack-without-interrupts ((sp fp &rest captures) &body body)
  (assert (<= (length captures) 3))
  (assert (every #'symbolp captures))
  `(%call-on-wired-stack-without-interrupts
    (lambda (,sp ,fp ,@captures)
      ,@body)
    nil
    ,@captures))

(defun place-spinlock-initializer ()
  :unlocked)

(defmacro initialize-place-spinlock (place)
  `(setf ,place (place-spinlock-initializer)))

(defmacro acquire-place-spinlock (place &environment environment)
  (let ((self (gensym))
        (old-value (gensym)))
    (multiple-value-bind (vars vals old-sym new-sym cas-form read-form)
        (sys.int::get-cas-expansion place environment)
      `(let ((,self (local-cpu-info))
             ,@(mapcar #'list vars vals))
         (ensure-interrupts-disabled)
         (block nil
           ;; Attempt one.
           (let* ((,old-sym :unlocked)
                  (,new-sym ,self)
                  (,old-value ,cas-form))
             (when (eq ,old-value :unlocked)
               ;; Prev value was :unlocked, have locked the lock.
               (return))
             (when (eq ,old-value ,self)
               (panic "Spinlock " ',place " held by self!")))
           ;; Loop until acquired.
           (loop
              ;; Read (don't CAS) the place until it goes back to :unlocked.
              (loop
                 (when (eq ,read-form :unlocked)
                   (return))
                 (sys.int::cpu-relax))
              ;; Cas the place, try to lock it.
              (let* ((,old-sym :unlocked)
                     (,new-sym ,self)
                     (,old-value ,cas-form))
                ;; Prev value was :unlocked, have locked the lock.
                (when (eq ,old-value :unlocked)
                  (return)))))
         (values)))))

(defmacro release-place-spinlock (place)
  `(progn (setf ,place :unlocked)
          (values)))

(defmacro with-place-spinlock ((place) &body body)
  `(progn
     (acquire-place-spinlock ,place)
     (unwind-protect
          (progn ,@body)
       (release-place-spinlock ,place))))

(defmacro with-symbol-spinlock ((lock) &body body)
  (check-type lock symbol)
  `(with-place-spinlock ((sys.int::symbol-global-value ',lock))
     ,@body))
