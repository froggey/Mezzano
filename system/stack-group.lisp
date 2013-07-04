(in-package :sys.int)

;;; Possible stack group states:
;;; Active       Running on some CPU.
;;;               Stack state is not valid.
;;; Exhausted    Task has finished and is no longer runnable. It must be preset before use.
;;;               Stack state is not valid.
;;; Yielded      Task has voluntarily stopped running for some reason (blocking, not yet started, etc).
;;;               Stack contains the saved CFP, LFP, LSP, RFlags and RIP.
;;; Interrupted  Task has been asynchronously interrupted.
;;;               Stack contains yield frame followed by an interrupt frame.

;;; Yielded stack layout:
;;;  0 CFP
;;;  1 LFP
;;;  2 LSP
;;;  3 RFlags
;;;  4 RIP

;;; Interrupted control stack layout:
;;;  0 CFP
;;;  1 LFP
;;;  2 LSP
;;;  3 RFlags (in the interrupt handler, IF clear)
;;;  4 RIP (in the interrupt handler)
;;;  5 RDI
;;;  6 RSI
;;;  7 RDX
;;;  8 RCX
;;;  9 RAX
;;; 10 RIP
;;; 11 CS
;;; 12 RFlags
;;; 13 RSP
;;; 14 SS
;;; Interrupted data stack layout:
;;;  0 R8
;;;  1 R9
;;;  2 R10
;;;  3 R11
;;;  4 R12
;;;  5 R13
;;;  6 RBX

;;; Stack-group object layout. Offsets for %array-like-ref.
;;;       0 Binding stack pointer (ub64)
;;;       1 Flags (fixnum)
;;;       2 Name
;;;       3 Control stack pointer (ub64)
;;;       4 CS base (ub64)
;;;       5 CS size in bytes (ub64)
;;;       6 DS base (ub64)
;;;       7 DS size in bytes (ub64)
;;;       8 BS base (ub64)
;;;       9 BS size in bytes (ub64)
;;;  10-447 TLS slots.
;;; 447-511 FXSAVE area.
;;; Stack bases and sizes must be aligned so they can be treated as fixnums by the GC.

;;;; Bits in the stack-group control status field.
;;; The state field.
(defconstant +stack-group-state-size+ 2)
(defconstant +stack-group-state-position+ 0)
(defconstant +stack-group-active+       #b00 "Task is running.")
(defconstant +stack-group-exhausted+    #b01 "Task is no longer runnable.")
(defconstant +stack-group-yielded+      #b10 "Task has yielded to another task.")
(defconstant +stack-group-interrupted+  #b11 "Task has been interrupted.")

;;; Stack group can't be interrupted. "Interrupted" means the ctrl-esc break.
(defconstant +stack-group-uninterruptable+ #b100)

;;; 512 less a word for the array header.
(defconstant +stack-group-size+ 511)
(defconstant +stack-group-fxsave-area-size+ 64) ; 64 words, 512 octets.

;;; Offsets of various stack group fields.
(defconstant +stack-group-offset-binding-stack-pointer+ 0)
(defconstant +stack-group-offset-flags+                 1)
(defconstant +stack-group-offset-control-stack-pointer+ 2)
(defconstant +stack-group-offset-name+                  3)
(defconstant +stack-group-offset-control-stack-base+    4)
(defconstant +stack-group-offset-control-stack-size+    5)
(defconstant +stack-group-offset-data-stack-base+       6)
(defconstant +stack-group-offset-data-stack-size+       7)
(defconstant +stack-group-offset-binding-stack-base+    8)
(defconstant +stack-group-offset-binding-stack-size+    9)
(defconstant +stack-group-offset-tls-slots+            10)
;; FXSAVE area must be at the end, GC doesn't scan it.
(defconstant +stack-group-offset-fxsave-area+ (- +stack-group-size+
                                                 +stack-group-fxsave-area-size+))

(defconstant +stack-group-tls-slots-size+ (- +stack-group-offset-fxsave-area+
                                             +stack-group-offset-tls-slots+))

(defvar *default-control-stack-size* 8192)
(defvar *default-data-stack-size* 8192)
(defvar *default-binding-stack-size* 512)

(defun stack-group-p (object)
  (and (%array-like-p object)
       (eql (%array-like-type object) +array-type-stack-group+)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (%define-type-symbol 'stack-group 'stack-group-p))

(defun make-stack-group (name &key
                                control-stack-size
                                data-stack-size
                                binding-stack-size
                                (interruptable t))
  (unless control-stack-size (setf control-stack-size *default-control-stack-size*))
  (unless data-stack-size (setf data-stack-size *default-data-stack-size*))
  (unless binding-stack-size (setf binding-stack-size *default-binding-stack-size*))
  (when (oddp binding-stack-size)
    (decf binding-stack-size))
  ;; Allocate stack and the stack-group object.
  (let* ((sg (%allocate-array-like +array-type-stack-group+ +stack-group-size+ +stack-group-size+ :static))
	 (cs-pointer (%allocate-stack control-stack-size))
	 (ds-pointer (%allocate-stack data-stack-size))
	 (bs-pointer (%allocate-stack binding-stack-size)))
    ;; Set state.
    (setf (%array-like-ref-t sg +stack-group-offset-flags+)
          (logior (if interruptable 0 +stack-group-uninterruptable+)
                  +stack-group-exhausted+))
    ;; Set name.
    (setf (%array-like-ref-t sg +stack-group-offset-name+) (string name))
    ;; Control stack base/size.
    (setf (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-control-stack-base+) cs-pointer
	  (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-control-stack-size+) (* control-stack-size 8))
    ;; Data stack base/size.
    (setf (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-data-stack-base+) ds-pointer
	  (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-data-stack-size+) (* data-stack-size 8))
    ;; Binding stack base/size.
    (setf (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-binding-stack-base+) bs-pointer
	  (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-binding-stack-size+) (* binding-stack-size 8))
    sg))

(defun stack-group-flags (stack-group)
  "Read the stack-group flags slot."
  (check-type stack-group stack-group)
  (%array-like-ref-t stack-group +stack-group-offset-flags+))

(defun stack-group-state (stack-group)
  "Return the current state of STACK-GROUP."
  (svref #(:active :exhausted :yielded :interrupted)
         (ldb (byte +stack-group-state-size+ +stack-group-state-position+)
              (stack-group-flags stack-group))))

(defun stack-group-active-p (stack-group)
  "Return true if STACK-GROUP is active (running)."
  (eql (ldb (byte +stack-group-state-size+ +stack-group-state-position+)
            (stack-group-flags stack-group))
       +stack-group-active+))

(defun stack-group-interruptable-p (stack-group)
  "Return true if STACK-GROUP can be interrupted."
  (not (logtest (stack-group-flags stack-group)
                +stack-group-uninterruptable+)))

(defun stack-group-name (stack-group)
  "Return STACK-GROUP's name."
  (check-type stack-group stack-group)
  (%array-like-ref-t stack-group +stack-group-offset-name+))

(defun stack-group-preset (stack-group function &rest arguments)
  (declare (dynamic-extent arguments))
  (stack-group-preset-common stack-group #x202 function arguments))

(defun stack-group-preset-no-interrupts (stack-group function &rest arguments)
  (declare (dynamic-extent arguments))
  (stack-group-preset-common stack-group #x2 function arguments))

;; ###: lock sg
(defun stack-group-preset-common (stack-group initial-flags function arguments)
  (check-type stack-group stack-group)
  (check-type function function)
  (when (stack-group-active-p stack-group)
    (error "Attempting to preset an active stack-group."))
  ;; FIXME: should be done with gc defered.
  (let* ((cs-base (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-control-stack-base+))
	 (cs-size (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-control-stack-size+))
	 (cs-pointer (+ cs-base cs-size))
         (ds-base (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-data-stack-base+))
	 (ds-size (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-data-stack-size+))
	 (ds-pointer (+ ds-base ds-size))
         (bs-base (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-binding-stack-base+))
	 (bs-size (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-binding-stack-size+))
         (bs-pointer (+ bs-base bs-size)))
    (flet ((ds-push (value)
             (setf (memref-t (decf ds-pointer 8) 0) value))
           (cs-push (value)
             (setf (memref-unsigned-byte-64 (decf cs-pointer 8) 0) value)))
      ;; Clear the binding stack.
      (dotimes (i (truncate bs-size 8))
        (setf (memref-t bs-base i) 0))
      ;; Clear the TLS slots.
      (dotimes (i +stack-group-tls-slots-size+)
        (setf (%array-like-ref-signed-byte-64 stack-group (+ +stack-group-offset-tls-slots+ i)) -2))
      ;; Initialize the FXSAVE save area.
      (dotimes (i 64)
        (setf (%array-like-ref-unsigned-byte-64 stack-group (+ +stack-group-offset-fxsave-area+ i)) 0))
      ;; Configure the MXCSR correctly. No interrupts!
      (setf (ldb (byte 32 0)
                 (%array-like-ref-unsigned-byte-64 stack-group
                                                   (+ +stack-group-offset-fxsave-area+ 3)))
            #x1F80)
      ;; Copy arguments to the data stack.
      (dolist (arg (reverse arguments))
        (ds-push arg))
      ;; Push the function on the data stack.
      (ds-push function)
      ;; And the number of arguments.
      (ds-push (length arguments))
      ;; Initialize the binding stack pointer.
      (setf (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-binding-stack-pointer+) bs-pointer)
      ;; Push initial stuff on the control stack.
      ;; Must match the frame %%switch-to-stack-group expects!
      (cs-push (lisp-object-address #'%%initial-stack-group-function))
      ;; Initial EFLAGS.
      (cs-push initial-flags)
      ;; Data stack pointer.
      (cs-push ds-pointer)
      ;; Data stack frame pointer.
      (cs-push 0)
      ;; Control stack frame pointer.
      (cs-push 0)
      ;; Set saved CSP.
      (setf (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-control-stack-pointer+) cs-pointer)
      ;; Mark the SG as ready to go.
      (setf (ldb (byte +stack-group-state-size+ +stack-group-state-position+)
                 (%array-like-ref-t stack-group +stack-group-offset-flags+))
            +stack-group-yielded+)))
  ;; Done!
  stack-group)

(defconstant +msr-ia32-gs-base+ #xC0000101)

(defun current-stack-group ()
  (%%assemble-value (msr +msr-ia32-gs-base+) 0))

(define-lap-function %%initial-stack-group-function ()
  ;; Initialize the FPU.
  (sys.lap-x86:fninit)
  ;; The control and binding stacks are empty.
  ;; The data stack contains the argument count, the function and the arguments.
  (sys.lap-x86:mov64 :rcx (:lsp))
  (sys.lap-x86:mov64 :r13 (:lsp 8))
  (sys.lap-x86:add64 :lsp 16)
  ;; Pop arguments into registers.
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
  ;; Function has returned.
  (sys.lap-x86:mov64 :r13 (:constant %stack-group-exhausted))
  (sys.lap-x86:mov32 :ecx 0) ; fixnum 0
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:ud2))

(defun %stack-group-exhausted ()
  "Called when the stack group's function returns."
  (error "Stack group exhausted."))

(define-lap-function %%switch-to-stack-group ()
  ;; Save the current state.
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:push :lsp)
  (sys.lap-x86:push :lfp)
  (sys.lap-x86:push :cfp)
  (sys.lap-x86:gs)
  (sys.lap-x86:fxsave (#.(+ (- +tag-array-like+)
                            (* (1+ +stack-group-offset-fxsave-area+) 8))))
  ;; Save CSP to the current stack group.
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (#.(+ (- +tag-array-like+)
                           (* (1+ +stack-group-offset-control-stack-pointer+) 8)))
                     :csp)
  ;; Switch to the new stack group.
  (sys.lap-x86:mov32 :ecx #.+msr-ia32-gs-base+)
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:mov64 :rdx :r8)
  (sys.lap-x86:shr64 :rdx 32)
  (sys.lap-x86:wrmsr)
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 :csp
                     (#.(+ (- +tag-array-like+)
                           (* (1+ +stack-group-offset-control-stack-pointer+) 8))))
  ;; Mark this stack group as :active.
  (sys.lap-x86:gs)
  (sys.lap-x86:and64 (#.(+ (- +tag-array-like+)
                           (* (1+ +stack-group-offset-flags+) 8)))
                     #.(ash (lognot (1- (ash 1 +stack-group-state-size+)))
                            (+ +stack-group-state-position+ 3)))
  ;; Restore state.
  (sys.lap-x86:gs)
  (sys.lap-x86:fxrstor (#.(+ (- +tag-array-like+)
                             (* (1+ +stack-group-offset-fxsave-area+) 8))))
  (sys.lap-x86:pop :cfp)
  (sys.lap-x86:pop :lfp)
  (sys.lap-x86:pop :lsp)
  (sys.lap-x86:popf)
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:mov32 :ecx 0) ; fixnum 0
  (sys.lap-x86:ret))

;;; ### Need to lock the sg.
(defun switch-to-stack-group (stack-group &optional exhaustp)
  "Switch to STACK-GROUP.
If EXHAUSTP is true, the current stack-group will be marked as :EXHAUSTED,
otherwise it will be marked as :YIELDED."
  (check-type stack-group stack-group)
  (ecase (stack-group-state stack-group)
    (:active (error "Stack group ~S is already active." stack-group))
    (:exhausted (error "Stack group ~S is exhausted." stack-group))
    ((:yielded :interrupted)
     ;; Mark as yielded.
     (setf (ldb (byte +stack-group-state-size+
                      +stack-group-state-position+)
                (%array-like-ref-t (current-stack-group) +stack-group-offset-flags+))
           (if exhaustp +stack-group-exhausted+ +stack-group-yielded+))
     (%%switch-to-stack-group stack-group))))

(defun %stack-group-interrupted-reg (stack-group register)
  "Read a saved register from an :INTERRUPTED stack-group."
  (let ((csp (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-control-stack-pointer+)))
    (ecase register
      (:cfp (memref-unsigned-byte-64 csp 0))
      (:lfp (memref-unsigned-byte-64 csp 1))
      (:lsp (memref-unsigned-byte-64 csp 2))
      (:pc (memref-unsigned-byte-64 csp 10))
      (:csp (memref-unsigned-byte-64 csp 13)))))

(defun %stack-group-step (stack-group)
  "Single-step an interrupted stack-group."
  ;; Set the trap bit and clear the interrupt bit!
  ;; No interrupts while running in the other stack-group.
  (let* ((flags (%stack-group-interrupted-reg stack-group :flags))
         (saved-interrupt-flag (ldb (byte 1 9) flags)))
    (setf (ldb (byte 1 9) flags) 0 ; IF
          (ldb (byte 1 8) flags) 1); TF
    (setf (%stack-group-interrupted-reg stack-group :flags) flags)
    ;; Set the current sg's state as yielded.
    ;; ### lock current sg...
    (setf (ldb (byte +stack-group-state-size+
                     +stack-group-state-position+)
               (%array-like-ref-t (current-stack-group) +stack-group-offset-flags+))
          +stack-group-yielded+)
    (setf *debug-trap-stack-group* (current-stack-group))
    (%%switch-to-stack-group stack-group)
    (setf *debug-trap-stack-group* nil)
    ;; Restore the saved IF and turn TF off.
    (let* ((new-flags (%stack-group-interrupted-reg stack-group :flags)))
      (setf (ldb (byte 1 9) new-flags) saved-interrupt-flag ; IF
            (ldb (byte 1 8) new-flags) 0); TF
      (setf (%stack-group-interrupted-reg stack-group :flags) new-flags))))

(defun %frame-function (cfp)
  "Read the function slot of a frame."
  (memref-t cfp -2))

(defun normalize-stack-group (stack-group)
  "Run STACK-GROUP until it reaches a point where it is safe to inspect.
STACK-GROUP should be locked on entry, but may be temporarily unlocked during normalization."
  (check-type stack-group stack-group)
  (ecase (stack-group-state stack-group)
    (:active (error "Stack group ~S is active." stack-group))
    ;; Exhausted stack groups are done and can always be inspected.
    (:exhausted)
    ;; Yielded stack groups have stopped voluntarily, so won't be in the middle of something.
    (:yielded)
    (:interrupted
     ;; Stack-group has been interrupted.
     ;; Single-step it until it is no longer in a protected region.
     (loop
        (let* ((pc (%stack-group-interrupted-reg stack-group :pc))
               (cfp (%stack-group-interrupted-reg stack-group :cfp))
               (fn (%frame-function cfp))
               (fn-addr (logand (lisp-object-address fn) (lognot #b1111)))
               (pc-rel (- fn-addr pc))
               (n-gc-entries (function-gc-info fn)))
          ;; If PC is outside FN, then it's actually running in some other function
          ;; and hasn't set up a stack frame yet. Safe to inspect in this state.
          (when (not (and (<= fn-addr pc) (< pc (+ fn-addr (function-code-size fn)))))
            (return-from normalize-stack-group))
          (dotimes (i n-gc-entries
                    (return-from normalize-stack-group))
            (multiple-value-bind (start end)
                (function-gc-info-entry fn i)
              (when (and (<= start pc-rel)
                         (< pc-rel end))
                (return))))
          (%stack-group-step stack-group))))))
