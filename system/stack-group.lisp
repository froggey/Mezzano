(in-package :sys.int)

;;;; Bits in the stack-group control status field.
;;; The state field.
(defconstant +stack-group-state-mask+ #b1111)
(defconstant +stack-group-active+ #b0000)
(defconstant +stack-group-resumable+ #b0001)
(defconstant +stack-group-awaiting-return+ #b0010)
(defconstant +stack-group-awaiting-initial-call+ #b0011)
(defconstant +stack-group-exhausted+ #b0100)
;;; Safe bit.
(defconstant +stack-group-safe+ #b10000)
(defconstant +stack-group-uninterruptable+ #b100000)

(defun %array-like-p (object)
  (eql (%tag-field object) +tag-array-like+))

(defun %array-like-header (object)
  (memref-unsigned-byte-64 (ash (%pointer-field object) 4) 0))

(defun %array-like-type (object)
  (logand (1- (ash 1 +array-type-size+))
          (ash (%array-like-header object) (- +array-type-shift+))))

(defun stack-group-p (object)
  (and (%array-like-p object)
       (eql (%array-like-type object) +array-type-stack-group+)))

(defun make-stack-group (name &key
                                control-stack-size
                                data-stack-size
                                binding-stack-size
                                (safe t)
                                (interruptable t))
  (unless control-stack-size (setf control-stack-size 8192))
  (unless data-stack-size (setf data-stack-size 8192))
  (unless binding-stack-size (setf binding-stack-size 512))
  (when (oddp binding-stack-size)
    (decf binding-stack-size))
  ;; Allocate stack and the stack-group object.
  (let* ((sg (%allocate-array-like +array-type-stack-group+ 511 511 :static))
         (sg-pointer (ash (%pointer-field sg) 4))
	 (cs-pointer (%allocate-stack control-stack-size))
	 (ds-pointer (%allocate-stack data-stack-size))
	 (bs-pointer (%allocate-stack binding-stack-size)))
    ;; Set state.
    (setf (memref-t sg-pointer 2) (logior (if safe +stack-group-safe+ 0)
                                          (if interruptable 0 +stack-group-uninterruptable+)
                                          +stack-group-exhausted+))
    ;; Set name.
    (setf (memref-t sg-pointer 4) (string name))
    ;; Control stack base/size.
    (setf (memref-unsigned-byte-64 sg-pointer 5) cs-pointer
	  (memref-t sg-pointer 6) control-stack-size)
    ;; Data stack base/size.
    (setf (memref-unsigned-byte-64 sg-pointer 7) ds-pointer
	  (memref-t sg-pointer 8) data-stack-size)
    ;; Binding stack base/size.
    (setf (memref-unsigned-byte-64 sg-pointer 9) bs-pointer
          (memref-t sg-pointer 10) binding-stack-size)
    ;; Resumer.
    (setf (memref-t sg-pointer 11) nil)
    sg))

(defun stack-group-state (sg)
  (check-type sg (satisfies stack-group-p) "a stack-group")
  (let* ((control (memref-t (ash (%pointer-field sg) 4) 2))
         (state (logand control +stack-group-state-mask+)))
    (svref #(:active
             :resumable
             :awaiting-return
             :awaiting-initial-call
             :exhausted)
           state)))

(defun stack-group-name (sg)
  (check-type sg (satisfies stack-group-p) "a stack-group")
  (memref-t (ash (%pointer-field sg) 4) 4))

(defun stack-group-resumer (sg)
  (check-type sg (satisfies stack-group-p) "a stack-group")
  (memref-t (ash (%pointer-field sg) 4) 11))

(defun (setf stack-group-resumer) (value sg)
  (check-type sg (satisfies stack-group-p) "a stack-group")
  (check-type value (or null (satisfies stack-group-p)))
  (setf (memref-t (ash (%pointer-field sg) 4) 11) value))

(defun stack-group-preset (stack-group function &rest arguments)
  (declare (dynamic-extent arguments))
  (stack-group-preset-common stack-group #x202 function arguments))

(defun stack-group-preset-no-interrupts (stack-group function &rest arguments)
  (declare (dynamic-extent arguments))
  (stack-group-preset-common stack-group #x2 function arguments))

(defun stack-group-preset-common (stack-group initial-flags function arguments)
  (declare (dynamic-extent arguments))
  (check-type function function)
  (when (eq (stack-group-state stack-group) :active)
    (error "Cannot preset an active stack-group."))
  ;; FIXME: should be done with gc defered.
  (let* ((sg-pointer (ash (%pointer-field stack-group) 4))
         (cs-base (memref-unsigned-byte-64 sg-pointer 5))
	 (cs-size (memref-t sg-pointer 6))
	 (cs-pointer (+ cs-base (* cs-size 8)))
	 (ds-base (memref-unsigned-byte-64 sg-pointer 7))
	 (ds-size (memref-t sg-pointer 8))
	 (ds-pointer (+ ds-base (* ds-size 8)))
	 (bs-base (memref-unsigned-byte-64 sg-pointer 9))
	 (bs-size (memref-t sg-pointer 10))
         (arg-count (length arguments)))
    ;; Clear the binding stack.
    (dotimes (i bs-size)
      (setf (memref-t bs-base i) 0))
    ;; Clear the TLS slots.
    (dotimes (i (- 512 12 64))
      (setf (memref-signed-byte-64 sg-pointer (+ 12 i)) -2))
    ;; Initialize the FXSAVE save area.
    (dotimes (i 64)
      (setf (memref-unsigned-byte-64 sg-pointer (+ 448 i)) 0))
    (setf (ldb (byte 32 0) (memref-unsigned-byte-64 sg-pointer (+ 448 3))) #x1F80)
    ;; Copy arguments to the data stack.
    (dolist (arg (nreverse arguments))
      (setf (memref-t (decf ds-pointer 8) 0) arg))
    ;; Push the function on the data stack.
    (setf (memref-t (decf ds-pointer 8) 0) function)
    ;; And the number of arguments.
    (setf (memref-t (decf ds-pointer 8) 0) arg-count)
    ;; Clear resumer.
    (setf (stack-group-resumer stack-group) nil)
    ;; Initialize the binding stack pointer.
    (setf (memref-unsigned-byte-64 sg-pointer 1) (+ bs-base (* bs-size 8)))
    ;; Push initial stuff on the control stack.
    ;; Must match the frame %%stack-group-resume expects!
    (setf (memref-t (decf cs-pointer 8) 0) #'%%initial-stack-group-function)
    ;; Initial EFLAGS, interrupts enabled.
    (setf (memref-unsigned-byte-64 (decf cs-pointer 8) 0) initial-flags)
    ;; Data stack pointer.
    (setf (memref-unsigned-byte-64 (decf cs-pointer 8) 0) ds-pointer)
    ;; Data stack frame pointer.
    (setf (memref-unsigned-byte-64 (decf cs-pointer 8) 0) 0)
    ;; Control stack frame pointer.
    (setf (memref-unsigned-byte-64 (decf cs-pointer 8) 0) 0)
    (setf (memref-unsigned-byte-64 sg-pointer 3) cs-pointer)
    ;; Mark the SG as ready to go.
    (setf (memref-t sg-pointer 2) (logior (logand (memref-t sg-pointer 2)
                                                  (lognot +stack-group-state-mask+))
                                          +stack-group-awaiting-initial-call+)))
  ;; Done!
  stack-group)

(define-lap-function current-stack-group ()
  (sys.lap-x86:mov32 :ecx #xC0000101) ; IA32_GS_BASE
  (sys.lap-x86:rdmsr)
  (sys.lap-x86:shl64 :rdx 32)
  (sys.lap-x86:or64 :rax :rdx)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:mov32 :ecx 8) ; fixnum 1
  (sys.lap-x86:ret))

(define-lap-function %%initial-stack-group-function ()
  ;; Initialize the FPU.
  (sys.lap-x86:fninit)
  ;; The control and binding stacks are empty.
  ;; The data stack contains the argument count, the function and the arguments.
  (sys.lap-x86:mov64 :rcx (:lsp))
  (sys.lap-x86:mov64 :r13 (:lsp 8))
  (sys.lap-x86:add64 :lsp 16)
  (sys.lap-x86:test64 :rcx :rcx)
  (sys.lap-x86:jz do-call)
  ;; One+ arguments.
  (sys.lap-x86:mov64 :r8 (:lsp))
  (sys.lap-x86:add64 :lsp 8)
  (sys.lap-x86:cmp64 :rcx 8) ; fixnum 1
  (sys.lap-x86:je do-call)
  ;; Two+ arguments.
  (sys.lap-x86:mov64 :r9 (:lsp))
  (sys.lap-x86:add64 :lsp 8)
  (sys.lap-x86:cmp64 :rcx 16) ; fixnum 2
  (sys.lap-x86:je do-call)
  ;; Three+ arguments.
  (sys.lap-x86:mov64 :r10 (:lsp))
  (sys.lap-x86:add64 :lsp 8)
  (sys.lap-x86:cmp64 :rcx 24) ; fixnum 3
  (sys.lap-x86:je do-call)
  ;; Four+ arguments.
  (sys.lap-x86:mov64 :r11 (:lsp))
  (sys.lap-x86:add64 :lsp 8)
  (sys.lap-x86:cmp64 :rcx 32) ; fixnum 4
  (sys.lap-x86:je do-call)
  ;; Five+ arguments.
  (sys.lap-x86:mov64 :r12 (:lsp))
  (sys.lap-x86:add64 :lsp 8)
  ;; Call the function.
  do-call
  (sys.lap-x86:call :r13)
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; Mark the current stack group as exhausted and return to the invoking SG.
  ;; The current stack group is already marked as active, so no need to clear
  ;; the state bits.
  (sys.lap-x86:gs)
  (sys.lap-x86:or64 ((- (* 2 8) #b0111)) #b0100000)
  ;; Call stack-group-return.
  (sys.lap-x86:mov64 :r13 (:constant stack-group-return))
  (sys.lap-x86:mov32 :ecx 8) ; fixnum 1
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:ud2))

(define-lap-function %%stack-group-resume ()
  ;; Save the current state.
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:push :lsp)
  (sys.lap-x86:push :lfp)
  (sys.lap-x86:push :cfp)
  (sys.lap-x86:gs)
  (sys.lap-x86:fxsave (#.(- 4096 512 +tag-array-like+)))
  ;; Mark this stack group as :resumable.
  ;; FIXME: This probably shouldn't change the stack-group state.
  ;; %%i-s-g-f has to change it to exhausted, but this overwrites it.
  (sys.lap-x86:gs)
  (sys.lap-x86:and64 ((- (* 2 8) #b0111)) -121) ; (lognot #b1111000)
  (sys.lap-x86:gs)
  (sys.lap-x86:or64 ((- (* 2 8) #b0111)) #b0001000)
  ;; Save CSP to the current stack group.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 ((- (* 3 8) #b0111)) :csp)
  ;; Switch to the new stack group.
  (sys.lap-x86:mov64 :csp (:r8 (- (* 3 8) #b0111)))
  (sys.lap-x86:mov32 :ecx #xC0000101) ; IA32_GS_BASE
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:mov64 :rdx :r8)
  (sys.lap-x86:shr64 :rdx 32)
  (sys.lap-x86:wrmsr)
  ;; Mark this stack group as :active.
  (sys.lap-x86:gs)
  (sys.lap-x86:and64 ((- (* 2 8) #b0111)) -121) ; (lognot #b1111000)
  ;; Restore state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxrstor (#.(- 4096 512 +tag-array-like+)))
  (sys.lap-x86:pop :cfp)
  (sys.lap-x86:pop :lfp)
  (sys.lap-x86:pop :lsp)
  (sys.lap-x86:popf)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:mov64 :r8 :r9)
  (sys.lap-x86:mov32 :ecx 8) ; fixnum 1
  (sys.lap-x86:ret))

(defun stack-group-resume (sg &optional value)
  (check-type sg (satisfies stack-group-p) "a stack-group")
  (%%stack-group-resume sg value))

(defun stack-group-return (&optional value)
  "Return to the invoking stack group."
  (let ((resumer (stack-group-resumer (current-stack-group))))
    (unless resumer
      (error "No invoking stack group!"))
    (stack-group-resume resumer value)))

;;; TODO: Enforce SAFE.
(defun stack-group-invoke (sg &optional value)
  (check-type sg (satisfies stack-group-p) "a stack-group")
  (setf (stack-group-resumer sg) (current-stack-group))
  (%%stack-group-resume sg value))
