;;;; High-level interrupt management.

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

;;; TATAS (test-and-test-and-set) spinlocks -- general purpose, supports nesting.
(defun place-spinlock-initializer ()
  :unlocked)

(defmacro initialize-place-spinlock (place)
  `(setf ,place (place-spinlock-initializer)))

(defmacro acquire-place-spinlock (place &environment environment)
  (let ((self (gensym))
        (old-value (gensym)))
    (multiple-value-bind (vars vals old-sym new-sym cas-form read-form)
        (sys.int::get-cas-expansion place environment)
      `(let ((,self (local-cpu))
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
               (panic "Spinlock " ',place " held by self. " ,self " " (local-cpu))))
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

(defmacro release-place-spinlock (place &environment environment)
  (multiple-value-bind (vars vals old-sym new-sym cas-form read-form write-form)
      (sys.int::get-cas-expansion place environment)
    (declare (ignore old-sym cas-form read-form))
    `(let* (,@(mapcar #'list vars vals)
            (,new-sym :unlocked)
            (,old-sym ,read-form))
       ,cas-form
       (values))))

(defmacro with-place-spinlock ((place) &body body)
  `(progn
     (acquire-place-spinlock ,place)
     (unwind-protect
          (progn ,@body)
       (release-place-spinlock ,place))))

(defmacro ensure-place-spinlock-held (place)
  (let ((holder (gensym)))
    `(let ((,holder ,place))
       (ensure (eql ,holder (local-cpu)) "Expected lock " ',place " to be held by " (local-cpu-info) " but is held by " ,holder))))

(defmacro acquire-symbol-spinlock (lock)
  (check-type lock symbol)
  `(acquire-place-spinlock ,lock))

(defmacro release-symbol-spinlock (lock)
  (check-type lock symbol)
  `(release-place-spinlock ,lock))

(defmacro with-symbol-spinlock ((lock) &body body)
  (check-type lock symbol)
  `(with-place-spinlock (,lock)
     ,@body))

(defmacro ensure-symbol-spinlock-held (lock)
  (check-type lock symbol)
  `(ensure-place-spinlock-held ,lock))

;;; MCS (Mellor-Crummy-Scott) queue-based spinlocks -- fair, FIFO, each CPU
;;; spins on its own cache line.  CANNOT be nested on the same CPU.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mcs-cas-target (place)
    "Convert a spinlock place to a form suitable for CAS.
Bare symbols become (sys.int::symbol-global-value 'SYM);
struct-accessor forms are returned as-is."
    (if (symbolp place)
        `(sys.int::symbol-global-value ',place)
        place)))

(defmacro acquire-mcs-spinlock (place)
  "Acquire a spinlock using MCS fair queuing.
NOTE: do NOT nest MCS spinlock acquisitions on the same CPU."
  (let ((mcs-node (gensym "MCS-NODE"))
        (prev (gensym "PREV"))
        (cas-target (mcs-cas-target place)))
    `(let ((,mcs-node (cpu-mcs-node (local-cpu))))
       (ensure-interrupts-disabled)
       (setf (mcs-node-next ,mcs-node) nil
             (mcs-node-locked ,mcs-node) nil)
       (let ((,prev nil))
         (loop
           (setf ,prev ,place)
           (when (eql (sys.int::cas ,cas-target ,prev ,mcs-node) ,prev)
             (return)))
         (if (null ,prev)
             (setf (mcs-node-locked ,mcs-node) t)
             (progn
               (setf (mcs-node-next ,prev) ,mcs-node)
               (loop until (mcs-node-locked ,mcs-node)
                     do (sys.int::cpu-relax))))
       ;; Acquire barrier: make sure protected-data reads are not
       ;; reordered before the lock is observed held.  Required on
       ;; weakly-ordered ARM64 (x86-64 TSO folds this into the CAS).
       #+arm64 (cpu-memory-barrier)
       (values)))))

(defmacro release-mcs-spinlock (place)
  "Release an MCS spinlock."
  (let ((mcs-node (gensym "MCS-NODE"))
        (cas-target (mcs-cas-target place)))
    `(let ((,mcs-node (cpu-mcs-node (local-cpu))))
       ;; Release barrier: make sure all protected-data stores from the
       ;; critical section are visible before the handoff (or before the
       ;; lock word goes to nil for the uncontended release).  Required
       ;; on weakly-ordered ARM64.
       #+arm64 (cpu-memory-barrier)
       (block release-mcs-spinlock
         (if (null (mcs-node-next ,mcs-node))
             (if (eql (sys.int::cas ,cas-target ,mcs-node nil) ,mcs-node)
                 (return-from release-mcs-spinlock)
                 (loop until (mcs-node-next ,mcs-node)
                       do (sys.int::cpu-relax))))
         (setf (mcs-node-locked (mcs-node-next ,mcs-node)) t))
       (setf (mcs-node-locked ,mcs-node) nil)
       (values))))

(defmacro with-mcs-spinlock ((place) &body body)
  `(progn
     (acquire-mcs-spinlock ,place)
     (unwind-protect
          (progn ,@body)
       (release-mcs-spinlock ,place))))

(defmacro ensure-mcs-spinlock-held (place)
  (declare (ignore place))
  `(ensure (mcs-node-locked (cpu-mcs-node (local-cpu)))
           "Expected lock to be held by current CPU"))

(defmacro with-page-fault-hook (((&optional frame info fault-address) &body hook-body) &body body)
  (let ((old (gensym))
        (frame (or frame (gensym "FRAME")))
        (info (or info (gensym "INFO")))
        (fault-address (or fault-address (gensym "FAULT-ADDRESS")))
        (ist-state (gensym))
        (exit-block (gensym "EXIT")))
    `(block ,exit-block
       (flet ((page-fault-hook-fn (,frame ,info ,fault-address ,ist-state)
                (declare (ignorable ,frame ,info ,fault-address ,ist-state))
                (macrolet ((abandon-page-fault (&optional values)
                             `(progn
                                (restore-page-fault-ist ,',ist-state)
                                (return-from ,',exit-block ,values))))
                  ,@hook-body)))
         (declare (dynamic-extent #'page-fault-hook-fn))
         (ensure-interrupts-disabled)
         (let ((,old (local-cpu-page-fault-hook)))
           (unwind-protect
                (progn
                  (setf (local-cpu-page-fault-hook) #'page-fault-hook-fn)
                  ,@body)
             (setf (local-cpu-page-fault-hook) ,old)))))))

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
  (count 0)
    (lock :unlocked)
)

(defstruct (irq-attachment
             (:area :wired))
  irq
  device
  handler
  exclusive-p
  pending-eoi)

(defun irq-deliver (interrupt-frame irq)
  (with-place-spinlock ((irq-lock irq))
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
      (when (not (zerop pending-count))
        ;; Mask the IRQ until all EOIs are delivered.
        (platform-mask-irq (irq-platform-number irq))))))

(defun irq-attach (irq handler device &key exclusive)
  (cond (exclusive
         (when (not (endp (irq-attachments irq)))
           (debug-print-line "Cannot exclusively attach to IRQ " irq " - in use")
           (return-from irq-attach nil)))
        (t
         (when (and (irq-attachments irq)
                    (irq-attachment-exclusive-p (first (irq-attachments irq))))
           (debug-print-line "Cannot attach to IRQ " irq " - in exclusive use")
           (return-from irq-attach nil))))
  (let* ((attachment (make-irq-attachment :irq irq
                                          :device device
                                          :handler handler
                                          :exclusive-p exclusive))
         (cons (sys.int::cons-in-area attachment nil :wired)))
    (safe-without-interrupts (irq cons)
      (with-place-spinlock ((irq-lock irq))
        (setf (cdr cons) (irq-attachments irq)
              (irq-attachments irq) cons)
        ;; Unmask the IRQ if this is the first attachment.
        (when (endp (rest (irq-attachments irq)))
          (platform-unmask-irq (irq-platform-number irq)))))
    attachment))

(defun irq-eoi (attachment)
  (safe-without-interrupts (attachment)
    (let ((irq (irq-attachment-irq attachment)))
      (with-place-spinlock ((irq-lock irq))
        (when (not (irq-attachment-pending-eoi attachment))
          (debug-print-line "Multiple EOI calls for attachment " attachment))
        (setf (irq-attachment-pending-eoi attachment) nil)
        ;; Unmask the IRQ if all attachments have EOI'd.
        (when (dolist (a (irq-attachments irq) t)
                (when (irq-attachment-pending-eoi a)
                  (return nil)))
          (platform-unmask-irq (irq-platform-number irq))))))
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
  event
  (state :masked)
    (lock :unlocked)
)

(defun make-simple-irq (irq-number &optional latch)
  (declare (mezzano.compiler::closure-allocation :wired))
  (let* ((irq (platform-irq irq-number))
         (simple-irq (%make-simple-irq :irq irq
                                       :latch latch))
         (event (make-event :name simple-irq))
         (fn (lambda (interrupt-frame irq)
               (declare (ignore interrupt-frame irq))
               (with-place-spinlock ((simple-irq-lock simple-irq))
                 (case (simple-irq-state simple-irq)
                   ((:masked :masked-eoi-pending)
                    :rejected)
                   (:unmasked
                    (setf (simple-irq-state simple-irq) :masked-eoi-pending)
                    (when (simple-irq-latch simple-irq)
                      (setf (event-state (simple-irq-latch simple-irq)) t))
                    (setf (event-state (simple-irq-event simple-irq)) t)
                    :accepted))))))
    (setf (simple-irq-event simple-irq) event
          (simple-irq-function simple-irq) fn
          (simple-irq-attachment simple-irq) (irq-attach irq fn simple-irq))
    simple-irq))

(defun simple-irq-attach (simple-irq)
  (declare (ignore simple-irq))
  (values))

(defun simple-irq-mask (simple-irq)
  (safe-without-interrupts (simple-irq)
    (with-place-spinlock ((simple-irq-lock simple-irq))
      (case (simple-irq-state simple-irq)
        (:masked-eoi-pending
         (setf (simple-irq-state simple-irq) :masked)
         (setf (event-state (simple-irq-event simple-irq)) nil)
         (irq-eoi (simple-irq-attachment simple-irq)))
        (:masked)
        (:unmasked
         (setf (simple-irq-state simple-irq) :masked)))))
  (values))

(defun simple-irq-unmask (simple-irq)
  (safe-without-interrupts (simple-irq)
    (with-place-spinlock ((simple-irq-lock simple-irq))
      (let ((prev (simple-irq-state simple-irq)))
        (setf (simple-irq-state simple-irq) :unmasked)
        (when (eql prev :masked-eoi-pending)
          (setf (event-state (simple-irq-event simple-irq)) nil)
          (irq-eoi (simple-irq-attachment simple-irq))))))
  (values))

(defun simple-irq-pending-p (simple-irq)
  "Returns true if an IRQ has been delivered and the SIMPLE-IRQ is waiting for an EOI."
  (event-state (simple-irq-event simple-irq)))

(defun simple-irq-masked-p (simple-irq)
  "Return if SIMPLE-IRQ has been masked.
Returns true if it has either been masked manually with SIMPLE-IRQ-MASK or
automatically through IRQ delivery."
  ;; Could be :MASKED or :MASKED-EOI-PENDING
  (not (eql (simple-irq-state simple-irq) :unmasked)))
