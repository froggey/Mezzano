;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; High-level interrupt management.

(in-package :mezzano.supervisor)

(declaim (inline ensure-interrupts-enabled ensure-interrupts-disabled))
(defun ensure-interrupts-enabled ()
  (unless (sys.int::%interrupt-state)
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
               (panic "Spinlock " ',place " held by self. " ,self " " (local-cpu-object))))
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

(defmacro ensure-place-spinlock-held (place)
  (let ((holder (gensym)))
    `(let ((,holder ,place))
       (ensure (eql ,holder (local-cpu-info)) "Expected lock " ',place " to be held by " (local-cpu-info) " but is held by " ,holder))))

(defmacro acquire-symbol-spinlock (lock)
  (check-type lock symbol)
  `(acquire-place-spinlock (sys.int::symbol-global-value ',lock)))

(defmacro release-symbol-spinlock (lock)
  (check-type lock symbol)
  `(release-place-spinlock (sys.int::symbol-global-value ',lock)))

(defmacro with-symbol-spinlock ((lock) &body body)
  (check-type lock symbol)
  `(with-place-spinlock ((sys.int::symbol-global-value ',lock))
     ,@body))

(defmacro ensure-symbol-spinlock-held (lock)
  (check-type lock symbol)
  `(ensure-place-spinlock-held (sys.int::symbol-global-value ',lock)))

(sys.int::defglobal *page-fault-hook* nil)

(defmacro with-page-fault-hook (((&optional frame info fault-address) &body hook-body) &body body)
  (let ((old (gensym))
        (frame (or frame (gensym "FRAME")))
        (info (or info (gensym "INFO")))
        (fault-address (or fault-address (gensym "FAULT-ADDRESS"))))
    `(flet ((page-fault-hook-fn (,frame ,info ,fault-address)
              (declare (ignorable ,frame ,info ,fault-address))
              ,@hook-body))
       (declare (dynamic-extent #'page-fault-hook-fn))
       (ensure-interrupts-disabled)
       (let ((,old *page-fault-hook*))
         (unwind-protect
              (progn
                (setf *page-fault-hook* #'page-fault-hook-fn)
                ,@body)
           (setf *page-fault-hook* ,old))))))

;;; Introspection.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun interrupt-frame-register-offset (register)
  (ecase register
    (:ss   5)
    (:rsp  4)
    (:rflags 3)
    (:cs   2)
    (:rip  1)
    (:rbp  0)
    (:rax -1)
    (:rcx -2)
    (:rdx -3)
    (:rbx -4)
    (:rsi -5)
    (:rdi -6)
    (:r8  -7)
    (:r9  -8)
    (:r10 -9)
    (:r11 -10)
    (:r12 -11)
    (:r13 -12)
    (:r14 -13)
    (:r15 -14)))
)

(define-compiler-macro interrupt-frame-raw-register (&whole whole frame register)
  (let ((offset (ignore-errors (interrupt-frame-register-offset register))))
    (if offset
        `(sys.int::memref-signed-byte-64 (interrupt-frame-pointer ,frame)
                                         ,offset)
        whole)))

(define-compiler-macro (setf interrupt-frame-raw-register) (&whole whole value frame register)
  (let ((offset (ignore-errors (interrupt-frame-register-offset register))))
    (if offset
        `(setf (sys.int::memref-signed-byte-64 (interrupt-frame-pointer ,frame)
                                               ,offset)
               ,value)
        whole)))

(define-compiler-macro interrupt-frame-value-register (&whole whole frame register)
  (let ((offset (ignore-errors (interrupt-frame-register-offset register))))
    (if offset
        `(sys.int::memref-t (interrupt-frame-pointer ,frame) ,offset)
        whole)))

(define-compiler-macro (setf interrupt-frame-value-register) (&whole whole value frame register)
  (let ((offset (ignore-errors (interrupt-frame-register-offset register))))
    (if offset
        `(setf (sys.int::memref-t (interrupt-frame-pointer ,frame) ,offset)
               ,value)
        whole)))

(defun interrupt-frame-pointer (frame)
  (sys.int::%object-ref-t frame 0))

(defun interrupt-frame-raw-register (frame register)
  (sys.int::memref-unsigned-byte-64 (interrupt-frame-pointer frame)
                                    (interrupt-frame-register-offset register)))

(defun (setf interrupt-frame-raw-register) (value frame register)
  (setf (sys.int::memref-unsigned-byte-64 (interrupt-frame-pointer frame)
                                          (interrupt-frame-register-offset register))
        value))

(defun interrupt-frame-value-register (frame register)
  (sys.int::memref-t (interrupt-frame-pointer frame)
                     (interrupt-frame-register-offset register)))

(defun (setf interrupt-frame-value-register) (value frame register)
  (setf (sys.int::memref-t (interrupt-frame-pointer frame)
                           (interrupt-frame-register-offset register))
        value))

;;; IRQs

(defstruct (irq
             (:area :wired))
  platform-number
  attachments
  (count 0))

(defstruct (irq-attachment
             (:area :wired))
  irq
  device
  handler
  exclusive-p
  pending-eoi)

(defun irq-deliver (interrupt-frame irq)
  (incf (irq-count irq))
  (let ((accept-count 0)
        (pending-count 0))
    (dolist (attachment (irq-attachments irq))
      (when (irq-attachment-pending-eoi attachment)
        (debug-print-line "Received IRQ " irq " masked by " attachment "?"))
      (let ((status (funcall (irq-attachment-handler attachment) interrupt-frame irq)))
        (case status
          (:rejected) ; Attachment was not expecting this interrupt.
          (:completed ; Attachment accepted the interrupt and has completed work.
           (incf accept-count))
          (:accepted ; Attachment accepted the interrupt, but has oustanding work and will issue a separate EOI.
           (incf accept-count)
           (incf pending-count)
           (setf (irq-attachment-pending-eoi attachment) t))
          (t
           (panic "Attachment " attachment " handler " (irq-attachment-handler attachment) " on IRQ " irq " returned invalid status " status)))))
    (when (zerop accept-count)
      (debug-print-line "No handler accepted IRQ " irq))
    (unless (zerop pending-count)
      ;; Mask the IRQ until all EOIs are delivered.
      (platform-mask-irq (irq-platform-number irq)))))

(defun irq-attach (irq handler device &key exclusive)
  (cond (exclusive
         (unless (endp (irq-attachments irq))
           (debug-print-line "Cannot exclusively attach to IRQ " irq " - in use")
           (return-from irq-attach nil)))
        (t
         (when (and (irq-attachments irq)
                    (irq-attachment-exclusive-p (first (irq-attachments irq))))
           (debug-print-line "Cannot attach to IRQ " irq " - in exclusive use")
           (return-from irq-attach nil))))
  (let ((attachment (make-irq-attachment :irq irq
                                         :device device
                                         :handler handler
                                         :exclusive-p exclusive)))
    (push-wired attachment (irq-attachments irq))
    ;; Unmask the IRQ if this is the first attachment.
    (when (endp (rest (irq-attachments irq)))
      (platform-unmask-irq (irq-platform-number irq)))
    attachment))

(defun irq-eoi (attachment)
  (unless (irq-attachment-pending-eoi attachment)
    (debug-print-line "Multiple EOI calls for attachment " attachment))
  (setf (irq-attachment-pending-eoi attachment) nil)
  ;; Unmask the IRQ if all attachments have EOI'd.
  (let ((irq (irq-attachment-irq attachment)))
    (when (dolist (a (irq-attachments irq) t)
            (when (irq-attachment-pending-eoi a)
              (return nil)))
      (platform-unmask-irq (irq-platform-number irq))))
  (values))

;;; Simple IRQ handler.
;;; When an IRQ is received, the IRQ is masked and a latch is triggered.

(defstruct (simple-irq
             (:area :wired)
             (:constructor %make-simple-irq))
  irq
  function
  attachment
  latch
  (state :masked))

(defun make-simple-irq (irq-number latch)
  (declare (sys.c::closure-allocation :wired))
  (let* ((irq (platform-irq irq-number))
         (simple-irq (%make-simple-irq :irq irq
                                       :latch latch))
         (fn (lambda (interrupt-frame irq)
               (declare (ignore interrupt-frame))
               (case (simple-irq-state simple-irq)
                 ((:masked :masked-eoi-pending)
                  :rejected)
                 (:unmasked
                  (setf (simple-irq-state simple-irq) :masked-eoi-pending)
                  (latch-trigger (simple-irq-latch simple-irq))
                  :accepted)))))
    (setf (simple-irq-function simple-irq) fn
          (simple-irq-attachment simple-irq) (irq-attach irq fn simple-irq))
    simple-irq))

(defun simple-irq-attach (simple-irq)
  (values))

(defun simple-irq-mask (simple-irq)
  ;; TODO: CAS this slot, or use some locking.
  (case (simple-irq-state simple-irq)
    (:masked-eoi-pending
     (setf (simple-irq-state simple-irq) :masked)
     (irq-eoi (simple-irq-attachment simple-irq)))
    (:masked)
    (:unmasked
     (setf (simple-irq-state simple-irq) :masked)))
  (values))

(defun simple-irq-unmask (simple-irq)
  ;; TODO: CAS this slot, or use some locking.
  (let ((prev (simple-irq-state simple-irq)))
    (setf (simple-irq-state simple-irq) :unmasked)
    (when (eql prev :masked-eoi-pending)
      (irq-eoi (simple-irq-attachment simple-irq))))
  (values))
