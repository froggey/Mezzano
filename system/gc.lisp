;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Weak pointers:
;;;
;;; "Weak References: Data Types and Implementation" - Bruno Haible
;;; http://www.haible.de/bruno/papers/cs/weak/WeakDatastructures-writeup.html
;;;
;;; "Streching the storage manager: weak pointers and stable names in Haskell"
;;; - Simon Peyton Jones, Simon Marlow, and Conal Elliott
;;; http://community.haskell.org/~simonmar/papers/weak.pdf

(in-package :sys.int)

(defglobal *gc-debug-scavenge-stack* nil)
(defglobal *gc-debug-freelist-rebuild* nil)
(defglobal *gc-debug-metadata* t)
(defglobal *gc-debug-validate-intergenerational-pointers* nil)

(defglobal *gc-enable-logging*)

;;; GC Meters.
(defglobal *old-objects-copied* 0)
(defglobal *objects-copied* 0)
(defglobal *words-copied* 0)
(defglobal *gc-transport-counts* (make-array 64 :area :pinned :initial-element 0))
(defglobal *gc-transport-old-counts* (make-array 64 :area :pinned :initial-element 0))
(defglobal *gc-transport-cycles* (make-array 64 :area :pinned :initial-element 0))

(defglobal *gc-last-general-address* 0)
(defglobal *gc-last-cons-address* 0)

(defglobal *gc-in-progress* nil)

;; State of the dynamic pointer mark bit. This is part of the pointer, not part
;; of the object itself.
(defglobal *dynamic-mark-bit*)
;; State of the object header mark bit, used for pinned objects.
(defglobal *pinned-mark-bit* 0)

(defglobal *gc-force-major-cycle* nil)

(defglobal *gc-gen0-cycles* 0)
(defglobal *gc-gen1-cycles* 0)
(defglobal *gc-major-cycles* 0)

;; List of weak pointers that need to be updated.
(defglobal *weak-pointer-worklist* nil)
;; List of finalizers that don't be need to be run yet.
(defglobal *known-finalizers* nil)
;; List of finalizers that should be run now.
(defglobal *pending-finalizers* nil)

(defglobal *gc-cycles* 0 "Number of collections performed.")
(defglobal *gc-epoch* 0)
(defglobal *gc-time* 0.0 "Time in seconds taken by the GC so far.")

(defun gc-reset-stats ()
  (setf *gc-cycles* 0)
  (setf *words-copied* 0
        *objects-copied* 0
        *old-objects-copied* 0)
  (fill *gc-transport-counts* 0)
  (fill *gc-transport-old-counts* 0)
  (fill *gc-transport-cycles* 0)
  (values))

(defun gc-stats ()
  (format t "Spent ~:D seconds in the GC over ~:D collections.~%" *gc-time* *gc-cycles*)
  (format t "  Copied ~:D objects, ~:D new, ~:D words.~%" *objects-copied* (- *objects-copied* *old-objects-copied*) *words-copied*)
  (format t "Transport stats:~%")
  (dotimes (i (min (length *gc-transport-cycles*)
                   (length *gc-transport-counts*)
                   (length *gc-transport-old-counts*)))
    (when (not (zerop (aref *gc-transport-counts* i)))
      (format t "  ~:D: ~:D objects, ~:D new, ~:D cycles.~%"
              (expt 2 i)
              (aref *gc-transport-counts* i)
              (- (aref *gc-transport-counts* i) (aref *gc-transport-old-counts* i))
              (aref *gc-transport-cycles* i)))))

(defun gc-log (&rest things)
  (declare (dynamic-extent things))
  (when *gc-enable-logging*
    (apply 'mezzano.supervisor:debug-print-line things)))

;; The background gc thread runs the GC when physical memory usage exceeds some
;; threshold. The idea is to prevent swapping, with the assumption that a GC
;; cycle will be faster than thrashing.
(defvar *gc-poll-interval* 0.1)
(defvar *gc-memory-use-threshold* 0.1)
(defvar *auto-gc-thread* (mezzano.supervisor:make-thread #'auto-gc-worker :name "Background GC"))

(defun maybe-gc ()
  (multiple-value-bind (free total)
      (mezzano.supervisor:physical-memory-statistics)
    (when (< (/ (float free) total) *gc-memory-use-threshold*)
      (gc))))

(defun auto-gc-worker ()
  (loop
     (sleep *gc-poll-interval*)
     (maybe-gc)))

(defvar *gc-lock* (mezzano.supervisor:make-mutex "Garbage Collector Lock"))
(defvar *gc-cvar* (mezzano.supervisor:make-condition-variable "Garbage Collector Cvar"))
(defvar *gc-requested* nil)
(defvar *gc-thread* (mezzano.supervisor:make-thread #'gc-worker :name "Garbage Collector" :stack-size (* 2 1024 1024)))

(defun gc (&key full)
  "Run a garbage-collection cycle."
  (check-type full (or boolean (member 0 1)))
  (mezzano.supervisor:with-mutex (*gc-lock*)
    (let ((epoch *gc-epoch*))
      (setf *gc-requested* t)
      (when full
        ;; TODO: Merge full and the current force cycle.
        ;; 0+1 -> 1, 0+:major -> :major, 1+:major -> :major
        (setf *gc-force-major-cycle* full))
      (mezzano.supervisor:condition-notify *gc-cvar* t)
      (loop
         (when (not (eql epoch *gc-epoch*))
           (return))
         (mezzano.supervisor:condition-wait *gc-cvar* *gc-lock*)))))

(defun gc-worker ()
  (loop
     (let ((force-major nil))
       (mezzano.supervisor:with-mutex (*gc-lock*)
         ;; Notify any waiting threads that the GC epoch has changed.
         (mezzano.supervisor:condition-notify *gc-cvar* t)
         ;; Wait for a GC request.
         (loop
            (when *gc-requested*
              (setf force-major *gc-force-major-cycle*
                    *gc-force-major-cycle* nil)
              (setf *gc-requested* nil)
              (return))
            (mezzano.supervisor:condition-wait *gc-cvar* *gc-lock*)))
       (when *gc-in-progress*
         (mezzano.supervisor:panic "Nested GC?!"))
       (mezzano.supervisor:with-world-stopped ()
         ;; Set *GC-IN-PROGRESS* globally, not with a binding.
         ;; This ensures that it is visible across all threads, especially
         ;; threads not stopped by with-world-stopped.
         (unwind-protect
              (let ((gc-start (get-internal-run-time)))
                (setf *gc-in-progress* t)
                (if (integerp force-major)
                    (gc-cycle nil force-major)
                    (gc-cycle force-major nil))
                (let* ((gc-end (get-internal-run-time))
                       (total-time (- gc-end gc-start))
                       (total-seconds (/ total-time (float internal-time-units-per-second))))
                  (gc-log "GC took " (truncate (* total-seconds 1000)) "ms")
                  (incf *gc-time* total-seconds)))
           (setf *gc-in-progress* nil)))
       ;; TODO: catch & report errors.
       (run-finalizers))))

(declaim (inline immediatep))
(defun immediatep (object)
  "Return true if OBJECT is an immediate object."
  (or (fixnump object)
      (%value-has-tag-p object +tag-character+)
      (%value-has-tag-p object +tag-single-float+)
      (%value-has-tag-p object +tag-byte-specifier+)))

(defmacro scavengef (place cycle-kind &environment env)
  "Scavenge PLACE. Only update PLACE if the scavenged value is different.
This is required to make the GC interrupt safe."
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (let ((orig (gensym "ORIG")))
      `(let* (,@(mapcar #'list vars vals)
              (,orig ,getter)
              (,(car stores) (scavenge-object ,orig ,cycle-kind)))
           (when (not (eq ,orig ,(car stores)))
             ,setter)))))

(defun scavenge-many (address n cycle-kind)
  (dotimes (i n)
    (scavengef (memref-t address i) cycle-kind)))

;;; This only scavenges the stack/registers. Scavenging the actual
;;; thread object is done by scan-thread.
(defun scavenge-current-thread ()
  ;; Grovel around in the current stack frame to grab needed stuff.
  (let* ((frame-pointer (read-frame-pointer))
         (return-address (memref-unsigned-byte-64 frame-pointer 1))
         (stack-pointer (+ frame-pointer 16)))
    (scan-thread (mezzano.supervisor:current-thread) :major)
    (gc-log "Scav GC stack")
    (scavenge-stack stack-pointer
                    (memref-unsigned-byte-64 frame-pointer 0)
                    return-address
                    :major)))

(defun scavenge-object (object cycle-kind)
  "Scavenge one object, returning an updated pointer."
  (when (immediatep object)
    ;; Don't care about immediate objects, return them unchanged.
    (return-from scavenge-object object))
  (let ((address (ash (%pointer-field object) 4)))
    (ecase (ldb (byte +address-tag-size+ +address-tag-shift+) address)
      ((#.+address-tag-general+
        #.+address-tag-cons+)
       (ecase cycle-kind
         ((:major 1)
          ;; Major GC: All dynamic allocations are transported to newspace.
          (when (eql (mask-field +address-generation+ address) *dynamic-mark-bit*)
            (return-from scavenge-object object))
          (transport-object object cycle-kind))
         (0
          ;; Minor GC: Dynamic generation 0 allocations are transported to generation 1.
          (when (not (eql (ldb +address-generation+ address) +address-generation-0+))
            (return-from scavenge-object object))
          (transport-object object cycle-kind))))
      (#.+address-tag-pinned+
       (when (eql cycle-kind :major)
         (mark-pinned-object object))
       object)
      (#.+address-tag-stack+
       ;; Don't scavenge DX objects, handled by scavenge-regular-stack-frame.
       object))))

(defun scan-error (object)
  (mezzano.supervisor:panic "Unscannable object " object))

(defun scan-generic (object size cycle-kind)
  "Scavenge SIZE words pointed to by OBJECT."
  (scavenge-many (ash (%pointer-field object) 4) size cycle-kind))

(defun scavenge-stack-n-incoming-arguments (frame-pointer stack-pointer framep
                                            layout-length n-args cycle-kind)
  (let ((n-values (max 0 (- n-args 5))))
    (when *gc-debug-scavenge-stack*
      (gc-log
       "  n-args " n-args
       "  n-values " n-values
       "  from " (if framep
                     (+ frame-pointer 16)
                     (+ stack-pointer (* layout-length 8)))))
    ;; There are N-VALUES values above the return address.
    (if framep
        ;; Skip saved fp and return address.
        (scavenge-many (+ frame-pointer 16) n-values cycle-kind)
        ;; Skip any layout values.
        (scavenge-many (+ stack-pointer (* layout-length 8))
                       n-values
                       cycle-kind))))

(defun scavenge-regular-stack-frame (frame-pointer stack-pointer framep
                                     layout-address layout-length
                                     incoming-arguments pushed-values
                                     cycle-kind)
  (when (and framep
             (zerop frame-pointer))
    (mezzano.supervisor:panic "Zero frame-pointer in framed function."))
  ;; Scan stack slots.
  (dotimes (slot layout-length)
    (multiple-value-bind (offset bit)
        (truncate slot 8)
      (when *gc-debug-scavenge-stack*
        (gc-log
         "ss: " slot " " offset ":" bit "  " (memref-unsigned-byte-8 layout-address offset)))
      (when (logbitp bit (memref-unsigned-byte-8 layout-address offset))
        (flet ((scav-one (base offset)
                 (let ((value (memref-t base offset)))
                   (when *gc-debug-scavenge-stack*
                     (gc-log
                      "Scav stack slot " offset
                      "  " (lisp-object-address value)))
                   (cond ((%value-has-tag-p value +tag-dx-root-object+)
                          ;; DX root, convert it to a normal object pointer and scan.
                          (gc-log
                           "Scav DX root " (lisp-object-address value))
                          (scan-object (%%assemble-value (ash (%pointer-field value) 4)
                                                         +tag-object+)
                                       cycle-kind))
                         ;; Normal object. Don't do anything interesting.
                         (t (scavengef (memref-t base offset) cycle-kind))))))
          (cond (framep
                 (scav-one frame-pointer (- -1 slot)))
                (t
                 (scav-one stack-pointer slot)))))))
  (dotimes (slot pushed-values)
    (when *gc-debug-scavenge-stack*
      (gc-log "Scav pv " slot))
    (scavengef (memref-t stack-pointer slot) cycle-kind))
  ;; Scan incoming arguments.
  (when incoming-arguments
    ;; Stored as fixnum on the stack.
    (when *gc-debug-scavenge-stack*
      (gc-log "IA in slot " (- -1 incoming-arguments)))
    (scavenge-stack-n-incoming-arguments
     frame-pointer stack-pointer framep
     layout-length
     (if framep
         (memref-t frame-pointer (- -1 incoming-arguments))
         (memref-t stack-pointer incoming-arguments))
     cycle-kind)))

(defun debug-stack-frame (framep interruptp pushed-values pushed-values-register
                          layout-address layout-length
                          multiple-values incoming-arguments
                          block-or-tagbody-thunk extra-registers
                          offset restart)
  (when *gc-debug-scavenge-stack*
    (gc-log "offset: " offset)
    (if framep
        (gc-log "frame")
        (gc-log "no-frame"))
    (if interruptp
        (gc-log "interrupt")
        (gc-log "no-interrupt"))
    (gc-log "pv: " pushed-values)
    (gc-log "pvr: " pushed-values-register)
    (if multiple-values
        (gc-log "mv: " multiple-values)
        (gc-log "no-multiple-values"))
    (gc-log "Layout addr: " layout-address)
    (gc-log "  Layout len: " layout-length)
    (cond (incoming-arguments
           (gc-log "ia: " incoming-arguments))
          (t (gc-log "no-incoming-arguments")))
    (if block-or-tagbody-thunk
        (gc-log "btt: " block-or-tagbody-thunk)
        (gc-log "no-btt"))
    (if extra-registers
        (gc-log "xr: " extra-registers)
        (gc-log "no-xr"))
    (if restart
        (gc-log "restart")
        (gc-log "no-restart"))))

(defun scavenge-interrupt-stack-frame (interrupt-frame-pointer cycle-kind)
  ;; Thread has stopped due to an interrupt.
  ;; Examine it, then perform normal stack scavenging.
  (when *gc-debug-scavenge-stack* (gc-log "Scav interrupted frame... " interrupt-frame-pointer))
  (let* ((interrupt-stack-pointer (- interrupt-frame-pointer (* 14 8)))
         (return-address (memref-unsigned-byte-64 interrupt-stack-pointer 15))
         (frame-pointer (memref-unsigned-byte-64 interrupt-stack-pointer 14))
         (stack-pointer (memref-unsigned-byte-64 interrupt-stack-pointer 18))
         (fn (return-address-to-function return-address))
         (fn-address (logand (lisp-object-address fn) -16))
         (fn-offset (- return-address fn-address)))
    (when *gc-debug-scavenge-stack*
      (gc-log "ISP: " interrupt-stack-pointer)
      (gc-log "RA: " return-address)
      (gc-log "FP: " frame-pointer)
      (gc-log "SP: " stack-pointer)
      (gc-log "FNa: " fn-address)
      (gc-log "FNo: " fn-offset))
    (scavenge-object fn cycle-kind)
    (multiple-value-bind (framep interruptp pushed-values pushed-values-register
                          layout-address layout-length
                          multiple-values incoming-arguments
                          block-or-tagbody-thunk extra-registers
                          entry-offset restart)
        (gc-info-for-function-offset fn fn-offset)
      (when *gc-debug-metadata*
        (flet ((bad-metadata (message)
                 (setf *gc-debug-scavenge-stack* t
                       *gc-enable-logging* t)
                 (gc-log "fn: " fn " " (function-pool-object fn 0))
                 (gc-log "RA: " return-address)
                 (gc-log "FP: " frame-pointer)
                 (gc-log "SP: " stack-pointer)
                 (gc-log "FNa: " fn-address)
                 (gc-log "FNo: " fn-offset)
                 (debug-stack-frame framep interruptp pushed-values pushed-values-register
                                    layout-address layout-length
                                    multiple-values incoming-arguments
                                    block-or-tagbody-thunk extra-registers
                                    entry-offset restart)
                 (mezzano.supervisor:panic "Bad GC metadata: " message)))
          (declare (dynamic-extent #'bad-metadata))
          ;; Validate metadata.
          (cond (interruptp
                 (when (not (eql layout-length 0))
                   (bad-metadata "Non-zero layout-length in an :INTERRUPT frame"))
                 (when (not (eql pushed-values 0))
                   (bad-metadata "Non-zero :PUSHED-VALUES in an :INTERRUPT frame"))
                 (when pushed-values-register
                   (bad-metadata "Non-NIL :PUSHED-VALUES-REGISTER in an :INTERRUPT frame"))
                 (when multiple-values
                   (bad-metadata ":MULTIPLE-VALUES invalid in an :INTERRUPT frame"))
                 (when incoming-arguments
                   (bad-metadata ":INCOMING-ARGUMENTS invalid in an :INTERRUPT frame"))
                 (when block-or-tagbody-thunk
                   (bad-metadata ":BLOCK-OR-TAGBODY-THUNK invalid in an :INTERRUPT frame"))
                 (when extra-registers
                   (bad-metadata ":EXTRA-REGISTERS invalid in an :INTERRUPT frame"))
                 (when restart
                   (bad-metadata ":RESTART invalid in an :INTERRUPT frame")))
                (t
                 ;; arm64 uses a link register
                 #-arm64
                 (when (and (not framep)
                            (eql layout-length 0))
                   (bad-metadata "Tried to unwind through function with no available return address"))
                 (when (and (not framep)
                            (not (eql pushed-values 0)))
                   (bad-metadata "Non-zero :PUSHED-VALUES is incompatible with :NO-FRAME. Use :LAYOUT."))
                 (when (and (not framep)
                            pushed-values-register)
                   (bad-metadata ":PUSHED-VALUES-REGISTER is incompatible with :NO-FRAME."))
                 ;; Not all settings are valid in arm64.
                 #+arm64
                 (when (and (not (eql extra-registers nil))
                            (not (eql extra-registers :rax)))
                   (bad-metadata ":EXTRA-REGISTERS has undefined setting"))))))
      (when interruptp
        ;; Thread is partway through popping an interrupt frame.
        ;; Finish the rest of the work and retry.
        ;; See %%PARTIAL-SAVE-RETURN-THUNK.
        (cond (framep
               ;; MV, FPU & GPR state not restored. Copy to save area.
               (sys.int::%copy-words interrupt-stack-pointer
                                     stack-pointer
                                     20)
               (sys.int::%copy-words (+ interrupt-stack-pointer (* 20 8))
                                     (+ stack-pointer (* 20 8))
                                     (truncate 512 8))
               (sys.int::%copy-words (+ interrupt-stack-pointer (* 20 8) 512)
                                     (+ stack-pointer (* 20 8) 512)
                                     (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+)))
              (t
               ;; Only the iret frame remains on the stack.
               (sys.int::%copy-words (+ interrupt-stack-pointer (* 15 8))
                                        (* 15 8))
                                     stack-pointer
                                     5))
        (return-from scavenge-interrupt-stack-frame
          (scavenge-interrupt-stack-frame interrupt-stack-pointer cycle-kind)))
      ;; Unconditionally scavenge the saved data registers.
      (scavengef (memref-signed-byte-64 interrupt-stack-pointer 7) cycle-kind) ; r8
      (scavengef (memref-signed-byte-64 interrupt-stack-pointer 6) cycle-kind) ; r9
      (scavengef (memref-signed-byte-64 interrupt-stack-pointer 5) cycle-kind) ; r10
      (scavengef (memref-signed-byte-64 interrupt-stack-pointer 4) cycle-kind) ; r11
      (scavengef (memref-signed-byte-64 interrupt-stack-pointer 3) cycle-kind) ; r12
      (scavengef (memref-signed-byte-64 interrupt-stack-pointer 2) cycle-kind) ; r13
      (scavengef (memref-signed-byte-64 interrupt-stack-pointer 10) cycle-kind) ; rbx
      #+x86-64
      (ecase extra-registers
        ((nil))
        ((:rax)
         (scavengef (memref-t interrupt-stack-pointer 13) cycle-kind)) ; rax
        ((:rax-rcx)
         (scavengef (memref-t interrupt-stack-pointer 13) cycle-kind) ; rax
         (scavengef (memref-t interrupt-stack-pointer 12) cycle-kind)) ; rcx
        ((:rax-rcx-rdx)
         (scavengef (memref-t interrupt-stack-pointer 13) cycle-kind) ; rax
         (scavengef (memref-t interrupt-stack-pointer 12) cycle-kind) ; rcx
         (scavengef (memref-t interrupt-stack-pointer 11) cycle-kind))) ; rdx
      #+arm64
      (ecase extra-registers
        ((nil))
        ((:rax)
         ;; x9 (rax) contains an interior pointer into :x1 (r9)
         (let ((offset (- (memref-signed-byte-64 interrupt-stack-pointer 13) ; x9
                          (memref-signed-byte-64 interrupt-stack-pointer 6)))) ; x1
           (scavengef (memref-signed-byte-64 interrupt-stack-pointer 6) cycle-kind)
           (setf (memref-signed-byte-64 interrupt-stack-pointer 13)
                 (+ (memref-signed-byte-64 interrupt-stack-pointer 6)
                    offset)))))
      (when block-or-tagbody-thunk
        ;; Active NLX thunk, true stack/frame pointers stored in the NLX info
        ;; pointed to by RAX.
        (let ((nlx-info (memref-signed-byte-64 interrupt-stack-pointer 13)))
          (setf stack-pointer (memref-signed-byte-64 nlx-info 2)
                frame-pointer (memref-signed-byte-64 nlx-info 3))))
      (when multiple-values
        ;; Scavenge the MV area.
        (let* ((n-values (+ (memref-t interrupt-stack-pointer 12) multiple-values))
               (n-mv-area-values (max 0 (- n-values 5))))
          (scavenge-many (+ interrupt-stack-pointer
                            (* 20 8)
                            512)
                         n-mv-area-values
                         cycle-kind)))
      (when (eql incoming-arguments :rcx)
        ;; Prevent SCAVENGE-REGULAR-STACK-FRAME from seeing :RCX in incoming-arguments.
        (setf incoming-arguments nil)
        (when *gc-debug-scavenge-stack*
          (gc-log "ia-count " (memref-t interrupt-stack-pointer 12)))
        (scavenge-stack-n-incoming-arguments
         frame-pointer stack-pointer framep
         layout-length
         (memref-t interrupt-stack-pointer 12)
         cycle-kind))
      (when restart
        ;; rip
        (setf (memref-t interrupt-stack-pointer 15) (+ fn-address entry-offset)))
      (scavenge-regular-stack-frame frame-pointer stack-pointer framep
                                    layout-address layout-length
                                    incoming-arguments
                                    (+ pushed-values
                                       (if pushed-values-register
                                           (memref-t interrupt-stack-pointer 12)
                                           0))
                                    cycle-kind)
      (cond #+arm64
            ((and (not framep)
                  (eql layout-length 0))
             ;; Special case: lr contains the return address and there is no return address on the stack.
             (scavenge-stack
              ;; Stack pointer needs the return address popped off,
              ;; and any layout variables.
              stack-pointer
              ;; Frame pointer should be unchanged.
              frame-pointer
              ;; Return address in x30.
              (memref-t interrupt-frame-pointer 16)
              cycle-kind))
            ((not framep)
             ;; No frame, carefully pick out the new values.
             (scavenge-stack
              ;; Stack pointer needs the return address popped off,
              ;; and any layout variables.
              (+ stack-pointer (* layout-length 8))
              ;; Frame pointer should be unchanged.
              frame-pointer
              ;; Return address should be above the layout variables.
              (memref-unsigned-byte-64 stack-pointer (1- layout-length))
              cycle-kind))
            ((not (zerop frame-pointer))
             (scavenge-stack (+ frame-pointer 16) ; sp
                             (memref-unsigned-byte-64 frame-pointer 0) ; fp
                             (memref-unsigned-byte-64 frame-pointer 1) ; ra
                             cycle-kind))
            (t (when *gc-debug-scavenge-stack*
                 (gc-log "Done scav stack.")))))))

(defun scavenge-stack (stack-pointer frame-pointer return-address cycle-kind)
  (when *gc-debug-scavenge-stack* (gc-log "Scav stack..."))
  (loop
     (when *gc-debug-scavenge-stack*
       (gc-log "SP: " stack-pointer)
       (gc-log "FP: " frame-pointer)
       (gc-log "RA: " return-address))
     (when (zerop return-address)
       (when *gc-debug-scavenge-stack* (gc-log "Done scav stack."))
       (return))
     (let* ((fn (return-address-to-function return-address))
            (fn-address (logand (lisp-object-address fn) -16))
            (fn-offset (- return-address fn-address)))
       (when *gc-debug-scavenge-stack*
         (gc-log "fn: " fn-address)
         (gc-log "fnoffs: " fn-offset))
       (scavenge-object fn cycle-kind)
       (multiple-value-bind (framep interruptp pushed-values pushed-values-register
                                    layout-address layout-length
                                    multiple-values incoming-arguments
                                    block-or-tagbody-thunk extra-registers
                                    entry-offset restart)
           (gc-info-for-function-offset fn fn-offset)
         (when *gc-debug-metadata*
           (flet ((bad-metadata (message)
                    (setf *gc-debug-scavenge-stack* t
                          *gc-enable-logging* t)
                    (gc-log "fn: " fn " " (function-pool-object fn 0))
                    (gc-log "RA: " return-address)
                    (gc-log "FP: " frame-pointer)
                    (gc-log "SP: " stack-pointer)
                    (gc-log "FNa: " fn-address)
                    (gc-log "FNo: " fn-offset)
                    (debug-stack-frame framep interruptp pushed-values pushed-values-register
                                       layout-address layout-length
                                       multiple-values incoming-arguments
                                       block-or-tagbody-thunk extra-registers
                                       entry-offset restart)
                    (mezzano.supervisor:panic "Bad GC metadata: " message)))
             (declare (dynamic-extent #'bad-metadata))
             ;; Validate metadata.
             (cond (interruptp
                    (when (not framep)
                      (bad-metadata "Interrupted :NO-FRAME :INTERRUPTED frame"))
                    (when (not (eql layout-length 0))
                      (bad-metadata "Non-zero layout-length in an :INTERRUPT frame"))
                    (when (not (eql pushed-values 0))
                      (bad-metadata "Non-zero :PUSHED-VALUES in an :INTERRUPT frame"))
                    (when pushed-values-register
                      (bad-metadata "Non-NIL :PUSHED-VALUES-REGISTER in an :INTERRUPT frame"))
                    (when multiple-values
                      (bad-metadata ":MULTIPLE-VALUES invalid in an :INTERRUPT frame"))
                    (when incoming-arguments
                      (bad-metadata ":INCOMING-ARGUMENTS invalid in an :INTERRUPT frame"))
                    (when block-or-tagbody-thunk
                      (bad-metadata ":BLOCK-OR-TAGBODY-THUNK invalid in an :INTERRUPT frame"))
                    (when extra-registers
                      (bad-metadata ":EXTRA-REGISTERS invalid in an :INTERRUPT frame"))
                    (when restart
                      (bad-metadata ":RESTART invalid in an :INTERRUPT frame")))
                   (t
                    (when (and (not framep)
                               (not (eql pushed-values 0)))
                      (bad-metadata "Non-zero :PUSHED-VALUES is incompatible with :NO-FRAME. Use :LAYOUT."))
                    ;; The remaining tests are because they refer to registers.
                    ;; Functions seen by SCAVENGE-STACK are all in the middle of call instructions, so
                    ;; referring to registers makes no sense.
                    (when pushed-values-register
                      (bad-metadata "PUSHED-VALUES-REGISTER seen outside full-save'd function."))
                    (when (and multiple-values (not (eql multiple-values 0)))
                      ;; :MULTIPLE-VALUES 0 is permitted, but ignored.
                      ;; This occurs immediately after call instructions that expect a multiple-value result.
                      (bad-metadata "Non-zero/non-nil :MULTIPLE-VALUES seen outside full-save'd function."))
                    (when block-or-tagbody-thunk
                      (bad-metadata ":BLOCK-OR-TAGBODY-THUNK seen outside full-save'd function."))
                    (when (eql incoming-arguments :rcx)
                      (bad-metadata ":INCOMING-ARGUMENTS :RCX seen outside full-save'd function."))
                    (when extra-registers
                      (bad-metadata ":EXTRA-REGISTERS seen outside full-save'd function."))
                    (when restart
                      (bad-metadata ":RESTART seen outside full-save'd function."))
                    (when (and (not framep)
                               (eql layout-length 0))
                      (bad-metadata "Tried to unwind through function with no available return address"))))))
         (cond (interruptp
                (scavenge-interrupt-stack-frame frame-pointer cycle-kind)
                (return))
               (t
                (scavenge-regular-stack-frame frame-pointer stack-pointer framep
                                              layout-address layout-length
                                              incoming-arguments pushed-values
                                              cycle-kind)
                (cond (framep
                       (psetf return-address (memref-unsigned-byte-64 frame-pointer 1)
                              stack-pointer (+ frame-pointer 16)
                              frame-pointer (memref-unsigned-byte-64 frame-pointer 0)))
                      (t
                       ;; No frame, carefully pick out the new values.
                       (gc-log "Unwinding through no-frame function")
                       ;; Frame pointer remains unchanged.
                       ;; Return address should be above the layout variables.
                       (setf return-address (memref-unsigned-byte-64 stack-pointer (1- layout-length)))
                       ;; Stack pointer needs the return address popped off,
                       ;; and any layout variables.
                       (setf stack-pointer (+ stack-pointer (* layout-length 8)))))))))))

(defun scavenge-thread-data-registers (thread cycle-kind)
  (scavengef (mezzano.supervisor:thread-state-r8-value thread) cycle-kind)
  (scavengef (mezzano.supervisor:thread-state-r9-value thread) cycle-kind)
  (scavengef (mezzano.supervisor:thread-state-r10-value thread) cycle-kind)
  (scavengef (mezzano.supervisor:thread-state-r11-value thread) cycle-kind)
  (scavengef (mezzano.supervisor:thread-state-r12-value thread) cycle-kind)
  (scavengef (mezzano.supervisor:thread-state-r13-value thread) cycle-kind)
  (scavengef (mezzano.supervisor:thread-state-rbx-value thread) cycle-kind))

(defun scavenge-full-save-thread (thread cycle-kind)
  ;; Thread has stopped due to an interrupt.
  ;; Examine it, then perform normal stack scavenging.
  (when *gc-debug-scavenge-stack* (gc-log "Scav full-save thread..." thread))
  (let* ((return-address (mezzano.supervisor:thread-state-rip thread))
         (frame-pointer (mezzano.supervisor:thread-frame-pointer thread))
         (stack-pointer (mezzano.supervisor:thread-stack-pointer thread))
         (fn (return-address-to-function return-address))
         (fn-address (logand (lisp-object-address fn) -16))
         (fn-offset (- return-address fn-address)))
    (when *gc-debug-scavenge-stack*
      (gc-log "RA: " return-address)
      (gc-log "FP: " frame-pointer)
      (gc-log "SP: " stack-pointer)
      (gc-log "FNa: " fn-address)
      (gc-log "FNo: " fn-offset))
    (multiple-value-bind (framep interruptp pushed-values pushed-values-register
                          layout-address layout-length
                          multiple-values incoming-arguments
                          block-or-tagbody-thunk extra-registers
                          entry-offset restart)
        (gc-info-for-function-offset fn fn-offset)
      (when *gc-debug-metadata*
        (flet ((bad-metadata (message)
                 (setf *gc-debug-scavenge-stack* t
                       *gc-enable-logging* t)
                 (gc-log "fn: " fn " " (function-pool-object fn 0))
                 (gc-log "RA: " return-address)
                 (gc-log "FP: " frame-pointer)
                 (gc-log "SP: " stack-pointer)
                 (gc-log "FNa: " fn-address)
                 (gc-log "FNo: " fn-offset)
                 (debug-stack-frame framep interruptp pushed-values pushed-values-register
                                    layout-address layout-length
                                    multiple-values incoming-arguments
                                    block-or-tagbody-thunk extra-registers
                                    entry-offset restart)
                 (mezzano.supervisor:panic "Bad GC metadata: " message)))
          (declare (dynamic-extent #'bad-metadata))
          ;; Validate metadata.
          (cond (interruptp
                 (when (not (eql layout-length 0))
                   (bad-metadata "Non-zero layout-length in an :INTERRUPT frame"))
                 (when (not (eql pushed-values 0))
                   (bad-metadata "Non-zero :PUSHED-VALUES in an :INTERRUPT frame"))
                 (when pushed-values-register
                   (bad-metadata "Non-NIL :PUSHED-VALUES-REGISTER in an :INTERRUPT frame"))
                 (when multiple-values
                   (bad-metadata ":MULTIPLE-VALUES invalid in an :INTERRUPT frame"))
                 (when incoming-arguments
                   (bad-metadata ":INCOMING-ARGUMENTS invalid in an :INTERRUPT frame"))
                 (when block-or-tagbody-thunk
                   (bad-metadata ":BLOCK-OR-TAGBODY-THUNK invalid in an :INTERRUPT frame"))
                 (when extra-registers
                   (bad-metadata ":EXTRA-REGISTERS invalid in an :INTERRUPT frame"))
                 (when restart
                   (bad-metadata ":RESTART invalid in an :INTERRUPT frame")))
                (t
                 ;; arm64 uses a link register
                 #-arm64
                 (when (and (not framep)
                            (eql layout-length 0))
                   (bad-metadata "Tried to unwind through function with no available return address"))
                 (when (and (not framep)
                            (not (eql pushed-values 0)))
                   (bad-metadata "Non-zero :PUSHED-VALUES is incompatible with :NO-FRAME. Use :LAYOUT."))
                 (when (and (not framep)
                            pushed-values-register)
                   (bad-metadata ":PUSHED-VALUES-REGISTER is incompatible with :NO-FRAME."))
                 ;; Not all settings are valid in arm64.
                 #+arm64
                 (when (and (not (eql extra-registers nil))
                            (not (eql extra-registers :rax)))
                   (bad-metadata ":EXTRA-REGISTERS has undefined setting"))))))
      (when interruptp
        ;; Thread is partway through popping an interrupt frame.
        ;; Finish the rest of the work and retry.
        ;; See %%PARTIAL-SAVE-RETURN-THUNK.
        (cond (framep
               ;; MV, FPU & GPR state not restored. Copy to save area.
               (sys.int::%copy-words (mezzano.runtime::%object-slot-address thread mezzano.supervisor::+thread-interrupt-save-area+)
                                     stack-pointer
                                     20)
               (sys.int::%copy-words (mezzano.runtime::%object-slot-address thread mezzano.supervisor::+thread-fx-save-area+)
                                     (+ stack-pointer (* 20 8))
                                     (truncate 512 8))
               (sys.int::%copy-words (mezzano.runtime::%object-slot-address thread mezzano.supervisor::+thread-mv-slots-start+)
                                     (+ stack-pointer (* 20 8) 512)
                                     (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+)))
              (t
               ;; Only the iret frame remains on the stack.
               (sys.int::%copy-words (+ (mezzano.supervisor::%object-slot-address thread mezzano.supervisor::+thread-interrupt-save-area+)
                                        (* 15 8))
                                     stack-pointer
                                     5)))
        (return-from scavenge-full-save-thread
          (scavenge-full-save-thread thread cycle-kind)))
      ;; Unconditionally scavenge the saved data registers.
      (scavenge-thread-data-registers thread cycle-kind)
      #+x86-64
      (ecase extra-registers
        ((nil))
        ((:rax)
         (scavengef (mezzano.supervisor:thread-state-rax-value thread) cycle-kind))
        ((:rax-rcx)
         (scavengef (mezzano.supervisor:thread-state-rax-value thread) cycle-kind)
         (scavengef (mezzano.supervisor:thread-state-rcx-value thread) cycle-kind))
        ((:rax-rcx-rdx)
         (scavengef (mezzano.supervisor:thread-state-rax-value thread) cycle-kind)
         (scavengef (mezzano.supervisor:thread-state-rcx-value thread) cycle-kind)
         (scavengef (mezzano.supervisor:thread-state-rdx-value thread) cycle-kind)))
      #+arm64
      (ecase extra-registers
        ((nil))
        ((:rax)
         ;; x9 (rax) contains an interior pointer into :x1 (r9)
         (let ((offset (- (mezzano.supervisor:thread-state-rax thread)
                          (mezzano.supervisor:thread-state-r9 thread))))
           (scavengef (mezzano.supervisor:thread-state-r9 thread) cycle-kind)
           (setf (mezzano.supervisor:thread-state-rax thread)
                 (+ (mezzano.supervisor:thread-state-r9 thread)
                    offset)))))
      (when block-or-tagbody-thunk
        ;; Active NLX thunk, true stack/frame pointers stored in the NLX info
        ;; pointed to by RAX.
        (let ((nlx-info (mezzano.supervisor:thread-state-rax thread)))
          (setf stack-pointer (memref-signed-byte-64 nlx-info 2)
                frame-pointer (memref-signed-byte-64 nlx-info 3))))
      (when multiple-values
        ;; Scavenge the MV area.
        (let* ((n-values (+ (mezzano.supervisor:thread-state-rcx-value thread) multiple-values))
               (n-mv-area-values (max 0 (- n-values 5))))
          (scavenge-many (+ (ash (%pointer-field thread) 4)
                            8
                            (* mezzano.supervisor::+thread-mv-slots-start+ 8))
                         n-mv-area-values
                         cycle-kind)))
      (when (eql incoming-arguments :rcx)
        ;; Prevent SCAVENGE-REGULAR-STACK-FRAME from seeing :RCX in incoming-arguments.
        (setf incoming-arguments nil)
        (when *gc-debug-scavenge-stack*
          (gc-log "ia-count " (mezzano.supervisor:thread-state-rcx-value thread)))
        (scavenge-stack-n-incoming-arguments
         frame-pointer stack-pointer framep
         layout-length
         (mezzano.supervisor::thread-state-rcx-value thread)
         cycle-kind))
      (when restart
        (setf (mezzano.supervisor:thread-state-rip thread) (+ fn-address entry-offset)))
      (scavenge-regular-stack-frame frame-pointer stack-pointer framep
                                    layout-address layout-length
                                    incoming-arguments
                                    (+ pushed-values
                                       (if pushed-values-register
                                           (mezzano.supervisor:thread-state-rcx-value thread)
                                           0))
                                    cycle-kind)
      (cond #+arm64
            ((and (not framep)
                  (eql layout-length 0))
             ;; Special case: lr contains the return address and there is no return address on the stack.
             (scavenge-stack
              ;; Stack pointer needs the return address popped off,
              ;; and any layout variables.
              stack-pointer
              ;; Frame pointer should be unchanged.
              frame-pointer
              ;; Return address in x30.
              (mezzano.supervisor:thread-state-cs thread)
              cycle-kind))
            ((not framep)
             ;; No frame, carefully pick out the new values.
             (scavenge-stack
              ;; Stack pointer needs the return address popped off,
              ;; and any layout variables.
              (+ stack-pointer (* layout-length 8))
              ;; Frame pointer should be unchanged.
              frame-pointer
              ;; Return address should be above the layout variables.
              (memref-unsigned-byte-64 stack-pointer (1- layout-length))
              cycle-kind))
            ((not (zerop frame-pointer))
             (scavenge-stack (+ frame-pointer 16) ; sp
                             (memref-unsigned-byte-64 frame-pointer 0) ; fp
                             (memref-unsigned-byte-64 frame-pointer 1) ; ra
                             cycle-kind))
            (t (when *gc-debug-scavenge-stack*
                 (gc-log "Done scav stack.")))))))

(defun scavengable-thread-p (object)
  (not (or (eql object (mezzano.supervisor:current-thread))
           ;; Don't even think about looking at the stacks of these
           ;; threads. They may run at any time, even with the world
           ;; stopped.
           ;; Things aren't so bad though, they (should) only contain
           ;; pointers to wired objects, and the objects they do point
           ;; to should be pointed to by other live objects.
           (eql (mezzano.supervisor:thread-priority object) :supervisor)
           (eql (mezzano.supervisor:thread-priority object) :idle))))

(defun scan-thread (object cycle-kind)
  (when *gc-debug-scavenge-stack* (gc-log "Scav thread " object))
  ;; Scavenge various parts of the thread.
  (scavengef (mezzano.supervisor:thread-name object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-state object) cycle-kind)
  ;; FIXME: Mark stack.
  (scavengef (mezzano.supervisor:thread-stack object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-special-stack-pointer object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-wait-item object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-%next object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-%prev object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-pending-footholds object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-mutex-stack object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-global-next object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-global-prev object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-priority object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-pager-argument-1 object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-pager-argument-2 object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-pager-argument-3 object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-unsleep-helper object) cycle-kind)
  (scavengef (mezzano.supervisor:thread-unsleep-helper-argument object) cycle-kind)
  ;; Scavenge the binding cache to prevent stale symbol value cells from
  ;; being reused.
  (loop
     for i from mezzano.supervisor::+thread-symbol-cache-start+ below mezzano.supervisor::+thread-symbol-cache-end+
     do (scavengef (%object-ref-t object i) cycle-kind))
  ;; Only scan the thread's stack and MV area when it's alive.
  (case (mezzano.supervisor:thread-state object)
    (:dead) ; Nothing.
    (0
     ;; This is a partially-initialized thread.
     ;; It has nothing on the stack that needs to be scanned, but its
     ;; data registers may contain live references.
     (scavenge-thread-data-registers object cycle-kind))
    (t
     (when (scavengable-thread-p object)
       (cond ((mezzano.supervisor:thread-full-save-p object)
              (scavenge-full-save-thread object cycle-kind))
             (t (let* ((stack-pointer (mezzano.supervisor:thread-stack-pointer object))
                       (frame-pointer (mezzano.supervisor:thread-frame-pointer object))
                       (return-address (memref-unsigned-byte-64 stack-pointer
                                                                #-arm64 0
                                                                #+arm64 1)))
                  (scavenge-stack stack-pointer frame-pointer return-address cycle-kind))))))))

(defun gc-info-for-function-offset (function offset)
  ;; Defaults.
  (let ((framep nil)
        (interruptp nil)
        (pushed-values 0)
        (pushed-values-register nil)
        (layout-address 0)
        (layout-length 0)
        (multiple-values nil)
        ;; Default to RCX here for closures & other stuff. Generally the right thing.
        ;; Stuff can override if needed.
        (incoming-arguments :rcx)
        (block-or-tagbody-thunk nil)
        (extra-registers nil)
        (this-offset 0)
        (restart nil))
    (block nil
      (flet ((f (entry-offset
                 entry-framep entry-interruptp
                 entry-pushed-values entry-pushed-values-register
                 entry-layout-address entry-layout-length
                 entry-multiple-values entry-incoming-arguments
                 entry-block-or-tagbody-thunk entry-extra-registers
                 entry-restart)
               (when (< offset entry-offset)
                 ;; This metadata entry is past the offset, return the previous values.
                 (return))
               (setf framep entry-framep
                     interruptp entry-interruptp
                     pushed-values entry-pushed-values
                     pushed-values-register entry-pushed-values-register
                     layout-address entry-layout-address
                     layout-length entry-layout-length
                     multiple-values entry-multiple-values
                     incoming-arguments entry-incoming-arguments
                     block-or-tagbody-thunk entry-block-or-tagbody-thunk
                     extra-registers entry-extra-registers
                     this-offset entry-offset
                     restart entry-restart)))
        (declare (dynamic-extent #'f))
        (map-function-gc-metadata #'f function)))
    (debug-stack-frame framep interruptp pushed-values pushed-values-register
                       layout-address layout-length
                       multiple-values incoming-arguments
                       block-or-tagbody-thunk extra-registers
                       this-offset restart)
    (values framep interruptp pushed-values pushed-values-register
            layout-address layout-length multiple-values
            incoming-arguments block-or-tagbody-thunk
            extra-registers this-offset restart)))

(defun scan-object-1 (object cycle-kind)
  ;; Dispatch again based on the type.
  (case (%object-tag object)
    ((#.+object-tag-array-t+
      #.+object-tag-closure+
      #.+object-tag-funcallable-instance+)
     ;; simple-vector
     ;; 1+ to account for the header word.
     (scan-generic object (1+ (%object-header-data object)) cycle-kind))
    ((#.+object-tag-simple-string+
      #.+object-tag-string+
      #.+object-tag-simple-array+
      #.+object-tag-array+)
     ;; Dimensions don't need to be scanned
     (scan-generic object 4 cycle-kind))
    ((#.+object-tag-complex-rational+
      #.+object-tag-ratio+)
     (scan-generic object 3 cycle-kind))
    (#.+object-tag-symbol+
     (scan-generic object 8 cycle-kind))
    (#.+object-tag-structure-object+
     (scan-generic object (1+ (%object-header-data object)) cycle-kind))
    (#.+object-tag-std-instance+
     (scan-generic object 4 cycle-kind))
    (#.+object-tag-function-reference+
     (scan-generic object 4 cycle-kind))
    (#.+object-tag-function+
     (scan-function object cycle-kind))
    ;; Things that don't need to be scanned.
    ((#.+object-tag-array-fixnum+
      #.+object-tag-array-bit+
      #.+object-tag-array-unsigned-byte-2+
      #.+object-tag-array-unsigned-byte-4+
      #.+object-tag-array-unsigned-byte-8+
      #.+object-tag-array-unsigned-byte-16+
      #.+object-tag-array-unsigned-byte-32+
      #.+object-tag-array-unsigned-byte-64+
      #.+object-tag-array-signed-byte-1+
      #.+object-tag-array-signed-byte-2+
      #.+object-tag-array-signed-byte-4+
      #.+object-tag-array-signed-byte-8+
      #.+object-tag-array-signed-byte-16+
      #.+object-tag-array-signed-byte-32+
      #.+object-tag-array-signed-byte-64+
      #.+object-tag-array-single-float+
      #.+object-tag-array-double-float+
      #.+object-tag-array-short-float+
      #.+object-tag-array-long-float+
      #.+object-tag-array-complex-single-float+
      #.+object-tag-array-complex-double-float+
      #.+object-tag-array-complex-short-float+
      #.+object-tag-array-complex-long-float+
      #.+object-tag-bignum+
      #.+object-tag-double-float+
      #.+object-tag-short-float+
      #.+object-tag-long-float+
      ;; not complex-rational or ratio, they may hold other numbers.
      #.+object-tag-complex-single-float+
      #.+object-tag-complex-double-float+
      #.+object-tag-complex-short-float+
      #.+object-tag-complex-long-float+
      #.+object-tag-mmx-vector+
      #.+object-tag-sse-vector+
      #.+object-tag-unbound-value+))
    (#.+object-tag-thread+
     (scan-thread object cycle-kind))
    (#.+object-tag-weak-pointer+
     (scan-weak-pointer object cycle-kind))
    (#.+object-tag-delimited-continuation+
     (scan-delimited-continuation object cycle-kind))
    (t (scan-error object))))

(defun scan-delimited-continuation (object cycle-kind)
  (gc-log "Scan delimited continuation " object)
  (let ((entry-point (%object-ref-signed-byte-64 object +function-entry-point+)))
    (when (not (zerop entry-point))
      (scavenge-object (return-address-to-function entry-point) cycle-kind)))
  (scavengef (%object-ref-t object +delimited-continuation-stack+) cycle-kind)
  (scavengef (%object-ref-t object +delimited-continuation-state+) cycle-kind)
  (let ((stack-pointer (%object-ref-signed-byte-64 object +delimited-continuation-stack-pointer+)))
    (when (not (zerop stack-pointer))
      ;; Scan the stack. God willing.
      (let ((frame-pointer (memref-unsigned-byte-64 stack-pointer 2))
            (return-address (memref-unsigned-byte-64 stack-pointer 3)))
        (scavenge-stack stack-pointer frame-pointer return-address cycle-kind)))))

(defun scan-weak-pointer (object cycle-kind)
  (ecase cycle-kind
    (:major
     ;; Add to the worklist, only when previously live.
     (when (logbitp +weak-pointer-header-livep+ (%object-header-data object))
       (setf (%object-ref-t object +weak-pointer-link+) *weak-pointer-worklist*
             *weak-pointer-worklist* object)))
    ((0 1)
     ;; Weak pointer processing doesn't occur during a minor cycle.
     ;; If the key was previously live, assume it is still alive.
     (when (logbitp +weak-pointer-header-livep+ (%object-header-data object))
       (scavengef (%object-ref-t object +weak-pointer-key+) cycle-kind)
       (scavengef (%object-ref-t object +weak-pointer-value+) cycle-kind))))
  (scavengef (%object-ref-t object +weak-pointer-finalizer-link+) cycle-kind)
  (scavengef (%object-ref-t object +weak-pointer-finalizer+) cycle-kind))

(defun scan-function (object cycle-kind)
  ;; Scan the constant pool.
  (let* ((address (ash (%pointer-field object) 4))
         (header (%object-header-data object))
         (mc-size (* (ldb +function-header-code-size+ header) 16))
         (pool-size (ldb +function-header-pool-size+ header)))
    (scavenge-many (+ address mc-size) pool-size cycle-kind)))

(defun scan-object (object cycle-kind)
  "Scan one object, updating pointer fields."
  (cond
    ((%value-has-tag-p object +tag-cons+)
     (scan-generic object 2 cycle-kind))
    ((%value-has-tag-p object +tag-object+)
     (scan-object-1 object cycle-kind))
    (t (scan-error object))))

(defun transport-error (object)
  (mezzano.supervisor:panic "Untransportable object " object))

(defun transport-object (object cycle-kind)
  "Transport LENGTH words from oldspace to newspace, returning
a pointer to the new object. Leaves a forwarding pointer in place."
  (let* ((address (ash (%pointer-field object) 4))
         (first-word (memref-t address 0)))
    ;; Check for a GC forwarding pointer.
    ;; Do this before getting the length as the forwarding pointer will
    ;; have overwritten the header word.
    (when (%value-has-tag-p first-word +tag-gc-forward+)
      (return-from transport-object
        (%%assemble-value (ash (%pointer-field first-word) 4)
                          (%tag-field object))))
    (really-transport-object object cycle-kind)))

(defun really-transport-object (object cycle-kind)
  (let* ((address (ash (%pointer-field object) 4))
         (first-word (memref-t address 0))
         (start-time (tsc))
         (length nil)
         (new-address nil))
    (setf length (object-size object))
    (when (not length)
      (transport-error object))
    ;; Update meters.
    (incf *objects-copied*)
    (incf *words-copied* length)
    ;; Find a new location.
    (ecase cycle-kind
      ((:major 1)
       ;; Objects going to newspace.
       (cond ((consp object)
              (setf new-address (logior (ash +address-tag-cons+ +address-tag-shift+)
                                        *cons-area-bump*
                                        *dynamic-mark-bit*))
              (incf *cons-area-bump* (* length 8))
              (when (< address *gc-last-cons-address*)
                (incf *old-objects-copied*)))
             (t
              (setf new-address (logior (ash +address-tag-general+ +address-tag-shift+)
                                        *general-area-bump*
                                        *dynamic-mark-bit*))
              (incf *general-area-bump* (* length 8))
              (when (oddp length)
                (setf (memref-t new-address length) 0)
                (incf *general-area-bump* 8))
              (when (< address *gc-last-general-address*)
                (incf *old-objects-copied*)))))
      (0
       ;; Objects going to gen1
       (cond ((consp object)
              (setf new-address (logior (ash +address-tag-cons+ +address-tag-shift+)
                                        (dpb +address-generation-1+ +address-generation+ 0)
                                        *cons-area-gen1-bump*))
              (incf *cons-area-gen1-bump* (* length 8))
              (when (< address *gc-last-cons-address*)
                (incf *old-objects-copied*)))
             (t
              (setf new-address (logior (ash +address-tag-general+ +address-tag-shift+)
                                        (dpb +address-generation-1+ +address-generation+ 0)
                                        *general-area-gen1-bump*))
              (incf *general-area-gen1-bump* (* length 8))
              (when (oddp length)
                (setf (memref-t new-address length) 0)
                (incf *general-area-gen1-bump* 8))
              (when (< address *gc-last-general-address*)
                (incf *old-objects-copied*))))))
    ;; Energize!
    (%copy-words new-address address length)
    ;; Leave a forwarding pointer.
    (setf (memref-t address 0) (%%assemble-value new-address +tag-gc-forward+))
    ;; Update meter.
    (let ((cycles (- (tsc) start-time))
          (bin (integer-length (1- (* length 8)))))
      (incf (svref *gc-transport-counts* bin))
      (incf (svref *gc-transport-cycles* bin) cycles)
      (when (< address (if (consp object) *gc-last-cons-address* *gc-last-general-address*))
        (incf (svref *gc-transport-old-counts* bin))))
    ;; Update object starts.
    ;; Conses are exempt as the cons area has a uniform layout.
    (when (not (consp object))
      (loop
         for card from (align-up new-address +card-size+) below (+ new-address (* length 8)) by +card-size+
         for delta = (- new-address card)
         do (setf (card-table-offset card)
                  (if (<= delta (- (* (1- (ash 1 (byte-size +card-table-entry-offset+))) 16)))
                      nil
                      delta))))
    ;; Complete! Return the new object
    (%%assemble-value new-address (%tag-field object))))

(defun object-size (object)
  (cond
    ((%value-has-tag-p object +tag-cons+)
     2)
    ((%value-has-tag-p object +tag-object+)
     (let ((length (%object-header-data object)))
       ;; Dispatch again based on the type.
       (case (%object-tag object)
         ((#.+object-tag-array-t+
           #.+object-tag-array-fixnum+
           #.+object-tag-structure-object+
           #.+object-tag-closure+
           #.+object-tag-funcallable-instance+)
          ;; simple-vector, std-instance or structure-object.
          ;; 1+ to account for the header word.
          (1+ length))
         ((#.+object-tag-array-bit+
           #.+object-tag-array-signed-byte-1+)
          (1+ (ceiling length 64)))
         ((#.+object-tag-array-unsigned-byte-2+
           #.+object-tag-array-signed-byte-2+)
          (1+ (ceiling length 32)))
         ((#.+object-tag-array-unsigned-byte-4+
           #.+object-tag-array-signed-byte-4+)
          (1+ (ceiling length 16)))
         ((#.+object-tag-array-unsigned-byte-8+
           #.+object-tag-array-signed-byte-8+)
          (1+ (ceiling length 8)))
         ((#.+object-tag-array-unsigned-byte-16+
           #.+object-tag-array-signed-byte-16+
           #.+object-tag-array-short-float+)
          (1+ (ceiling length 4)))
         ((#.+object-tag-array-unsigned-byte-32+
           #.+object-tag-array-signed-byte-32+
           #.+object-tag-array-single-float+
           #.+object-tag-array-complex-short-float+)
          (1+ (ceiling length 2)))
         ((#.+object-tag-array-unsigned-byte-64+
           #.+object-tag-array-signed-byte-64+
           #.+object-tag-array-double-float+
           #.+object-tag-array-complex-single-float+
           #.+object-tag-bignum+)
          (1+ length))
         ((#.+object-tag-array-long-float+
           #.+object-tag-array-complex-double-float+)
          (1+ (* length 2)))
         ((#.+object-tag-array-complex-long-float+)
          (1+ (* length 4)))
         (#.+object-tag-double-float+
          2)
         (#.+object-tag-long-float+
          4)
         (#.+object-tag-short-float+
          2)
         (#.+object-tag-complex-rational+
          4)
         (#.+object-tag-complex-short-float+
          2)
         (#.+object-tag-complex-single-float+
          2)
         (#.+object-tag-complex-double-float+
          4)
         (#.+object-tag-complex-long-float+
          8)
         (#.+object-tag-ratio+
          4)
         (#.+object-tag-mmx-vector+
          2)
         (#.+object-tag-sse-vector+
          4)
         (#.+object-tag-symbol+
          8)
         (#.+object-tag-std-instance+
          4)
         (#.+object-tag-function-reference+
          4)
         (#.+object-tag-function+
          ;; The size of a function is the sum of the MC, the GC info and the constant pool.
          (ceiling (+ (* (ldb +function-header-code-size+ length) 16)  ; mc size
                      (* (ldb +function-header-pool-size+ length) 8)  ; pool size
                      (ldb +function-header-metadata-size+ length)) ; gc-info size.
                   8))
         ((#.+object-tag-simple-string+
           #.+object-tag-string+
           #.+object-tag-simple-array+
           #.+object-tag-array+)
          (+ 4 length))
         (#.+object-tag-unbound-value+
          2)
         (#.+object-tag-thread+
          512)
         (#.+object-tag-weak-pointer+
          6)
         (#.+object-tag-delimited-continuation+
          6)
         (t
          (object-size-error object)))))
    (t
     (object-size-error object))))

(defun object-size-error (object)
  (mezzano.supervisor:panic "Sizing invalid object " object))

(defun mark-pinned-object (object)
  (let ((address (ash (%pointer-field object) 4)))
    (cond ((consp object)
           ;; The object header for conses is 16 bytes behind the address.
           (when (not (eql (ldb (byte +object-type-size+ +object-type-shift+)
                                (memref-unsigned-byte-64 address -2))
                           +object-tag-cons+))
             (mezzano.supervisor:panic "Invalid pinned cons " object))
           (when (not (eql (logand (memref-unsigned-byte-64 address -2)
                                   +pinned-object-mark-bit+)
                           *pinned-mark-bit*))
             ;; Not marked, mark it.
             (setf (memref-unsigned-byte-64 address -2) (logior (logand (memref-unsigned-byte-64 address -2)
                                                                        (lognot +pinned-object-mark-bit+))
                                                                *pinned-mark-bit*))
             ;; And scan.
             (scan-object object :major)))
          (t (when (eql (sys.int::%object-tag object) +object-tag-freelist-entry+)
               (mezzano.supervisor:panic "Marking freelist entry " object))
             (when (not (eql (logand (memref-unsigned-byte-8 address 0) ; Read carefully, no bignums.
                                     +pinned-object-mark-bit+)
                             *pinned-mark-bit*))
               ;; Not marked, mark it.
               (setf (memref-unsigned-byte-8 address 0) (logior (logand (memref-unsigned-byte-8 address 0)
                                                                        (lognot +pinned-object-mark-bit+))
                                                                *pinned-mark-bit*))
               ;; And scan.
               (scan-object object :major))))))

(defvar *scavenge-general-finger*)
(defvar *scavenge-cons-finger*)

(defun scavenge-dynamic ()
  (loop
     (gc-log
      "General. Limit: " *general-area-limit*
      "  Bump: " *general-area-bump*
      "  Curr: " *scavenge-general-finger*)
     (gc-log
      "Cons.    Limit: " *cons-area-limit*
      "  Bump: " *cons-area-bump*
      "  Curr: " *scavenge-cons-finger*)
     ;; Stop when both area sets have been fully scavenged.
     (when (and (eql *scavenge-general-finger* *general-area-bump*)
                (eql *scavenge-cons-finger* *cons-area-bump*))
       (return))
     (gc-log "Scav main seq")
     ;; Scavenge general area.
     (loop
        (when (eql *scavenge-general-finger* *general-area-bump*)
          (return))
        (let* ((object (%%assemble-value (logior *scavenge-general-finger*
                                                 (ash +address-tag-general+ +address-tag-shift+)
                                                 *dynamic-mark-bit*)
                                         +tag-object+))
               (size (object-size object)))
          (when (oddp size)
            (incf size))
          (scan-object object :major)
          (incf *scavenge-general-finger* (* size 8))))
     ;; Scavenge cons area.
     (loop
        (when (eql *scavenge-cons-finger* *cons-area-bump*)
          (return))
        ;; Cons region is just pointers.
        (let ((addr (logior *scavenge-cons-finger*
                            (ash +address-tag-cons+ +address-tag-shift+)
                            *dynamic-mark-bit*)))
          (scavengef (memref-t addr 0) :major)
          (scavengef (memref-t addr 1) :major))
        (incf *scavenge-cons-finger* 16))))

(defun size-of-pinned-area-allocation (address)
  "Return the size of an allocation in the wired or pinned area."
  (let ((type (ash (memref-unsigned-byte-8 address 0) (- +object-type-shift+))))
    (case type
      (#.+object-tag-cons+ 4)
      (#.+object-tag-freelist-entry+ (ash (memref-unsigned-byte-64 address 0)
                                          (- +object-data-shift+)))
      (t (object-size (%%assemble-value address +tag-object+))))))

(defun align-up (value boundary)
  (logand (+ value (1- boundary)) (lognot (1- boundary))))

(defun align-down (value boundary)
  (logand value (lognot (1- boundary))))

(defun find-next-free-object (start limit)
  (loop
     (when (>= start limit)
       (return nil))
     (when (not (eql (logand (memref-unsigned-byte-8 start 0) +pinned-object-mark-bit+)
                     *pinned-mark-bit*))
       ;; Not marked, must be free.
       (return start))
     (let ((size (* (align-up (size-of-pinned-area-allocation start) 2) 8)))
       (loop
          for card from (align-up start +card-size+) below (+ start size) by +card-size+
          for delta = (- start card)
          do (setf (card-table-offset card)
                   (if (<= delta (- (* (1- (ash 1 (byte-size +card-table-entry-offset+))) 16)))
                       nil
                       delta)))
       (incf start size))))

(defun make-freelist-header (len)
  (when *gc-debug-freelist-rebuild*
    (gc-log "hdr " len))
  (logior *pinned-mark-bit*
          (ash +object-tag-freelist-entry+ +object-type-shift+)
          (ash (align-up len 2) +object-data-shift+)))

(defun dump-pinned-area (base limit)
  (let ((offset base))
    (loop
       (when (>= offset limit)
         (return))
       (let* ((type (ash (memref-unsigned-byte-8 offset 0) (- +object-type-shift+)))
              (size (size-of-pinned-area-allocation offset)))
         (gc-log offset " " size " " type
                 " "
                 (when (eql type +object-tag-freelist-entry+)
                   (mezzano.runtime::freelist-entry-next offset)))
         (incf offset (* (align-up size 2) 8))))))

(defun finish-freelist-entry (bins start len)
  (let ((bin (integer-length len)))
    (when *gc-debug-freelist-rebuild*
      (gc-log "finalize entry @" start "  len " len " bin " bin))
    (setf (memref-unsigned-byte-64 start 0) (make-freelist-header len)
          (memref-t start 1) (svref bins bin))
    (setf (svref bins bin) start)
    (loop
       for card from (align-up start +card-size+) below (+ start (* len 8)) by +card-size+
       for delta = (- start card)
       do (setf (card-table-offset card)
                (if (<= delta (- (* (1- (ash 1 (byte-size +card-table-entry-offset+))) 16)))
                    nil
                    delta)))))

(defun rebuild-freelist (bins name base limit)
  "Sweep the pinned/wired area chain and rebuild the freelist.
Additionally update the card table offset fields."
  (gc-log "rebuild freelist " name)
  (dotimes (i 64)
    (setf (svref bins i) nil))
  ;; Build the freelist.
  (let* ((entry-start (find-next-free-object base limit))
         (entry-len 0)
         (current entry-start))
    (when (not entry-start)
      (when *gc-debug-freelist-rebuild*
        (gc-log "done (empty)"))
      (return-from rebuild-freelist))
    (when *gc-debug-freelist-rebuild*
      (gc-log "begin at " current))
    (loop
       ;; Expand this entry as much as possible.
       (let* ((len (align-up (size-of-pinned-area-allocation current) 2))
              (next-addr (+ current (* len 8))))
         (when *gc-debug-freelist-rebuild*
           (gc-log "cur: " current " len: " len " next: " next-addr))
         (incf entry-len len)
         (cond
           ((>= next-addr limit)
            (finish-freelist-entry bins entry-start entry-len)
            (return))
           ;; Test the mark bit.
           ((eql (logand (memref-unsigned-byte-8 next-addr 0) +pinned-object-mark-bit+)
                 *pinned-mark-bit*)
            ;; Is marked, finish this entry and start the next one.
            (setf current (find-next-free-object next-addr limit))
            (when *gc-debug-freelist-rebuild*
              (gc-log "marked, next is " current))
            (finish-freelist-entry bins entry-start entry-len)
            (when (not current)
              (return))
            (setf entry-start current
                  entry-len 0))
           (t
            ;; Advance to the next object
            (setf current next-addr)))))))

(defun other-gen2-area-from-mark-bit (d-m-b)
  (if (eql d-m-b (dpb sys.int::+address-generation-2-a+
                      sys.int::+address-generation+
                      0))
      (dpb sys.int::+address-generation-2-b+ sys.int::+address-generation+ 0)
      (dpb sys.int::+address-generation-2-a+ sys.int::+address-generation+ 0)))

(defun minor-scan-thread (thread gen)
  (scan-thread thread gen))

(defun minor-scan-at (address gen)
  (let ((type (ash (memref-unsigned-byte-8 address 0) (- +object-type-shift+))))
    (case type
      (#.+object-tag-cons+
       ;; Scanning a cons represented as a headered object.
       (scavengef (memref-t address 2) gen)
       (scavengef (memref-t address 3) gen)
       32)
      (#.+object-tag-freelist-entry+
       ;; Skip freelist entries.
       (* (ash (memref-unsigned-byte-64 address 0)
               (- +object-data-shift+))
          8))
      (t
       (let ((object (%%assemble-value address +tag-object+)))
         (scan-object object gen)
         (* (align-up (object-size object) 2) 8))))))

(defun card-table-dirty-p (address gen)
  (let ((dirty-gen (card-table-dirty-gen address)))
    (and dirty-gen
         (<= dirty-gen gen))))

(defun minor-scan-range (start size gen)
  (gc-log "minor scan " start "-" (+ start size) " " gen)
  (let ((end (+ start size))
        (current start))
    (loop
       ;; State one. Looking for a dirty card.
       (loop
          (when (>= current end)
            (return-from minor-scan-range))
          (when (card-table-dirty-p current gen)
            (gc-log "Hit minor card " current)
            (return))
          #++(gc-log "Skip minor card " current)
          (incf current +card-size+))
       ;; State two. Found a dirty card. CURRENT is somewhere in the card.
       ;; Find the start of that object. This may be behind current.
       (setf current (base-address-of-internal-pointer current))
       (gc-log "Base is " current)
       ;; Scan this object and all objects until current points to a non-dirty card.
       (loop
          (incf current (minor-scan-at current gen))
          (when (>= current end)
            (return-from minor-scan-range))
          (when (not (card-table-dirty-p current gen))
            (return)))
       (setf current (logand current (lognot (1- +card-size+)))))))

(defun minor-scan-cons-range (start size gen)
  (gc-log "minor cons scan " start "-" (+ start size) " " gen)
  (loop
     for current from start below (+ start size) by +card-size+
     do
       (cond ((card-table-dirty-p current gen)
              (gc-log "Hit minor cons card " current)
              (dotimes (i (/ +card-size+ 8))
                (scavengef (memref-t current i) gen)))
             (t
              #++(gc-log "Skip minor cons card " current)))))

(defun verify-one (address field-address gen)
  (let ((object (memref-t field-address)))
    (when (immediatep object)
      ;; Don't care about immediate objects, return them unchanged.
      (return-from verify-one object))
    (let ((object-address (ash (%pointer-field object) 4)))
      (ecase (ldb (byte +address-tag-size+ +address-tag-shift+) object-address)
        ((#.+address-tag-general+
          #.+address-tag-cons+)
         (when (<= (ldb +address-generation+ object-address) gen)
           (mezzano.supervisor:panic "Object at address " address " contains young gen pointer " object-address " in field " field-address " "
                                     (ldb +address-generation+ object-address) " " gen)))
        ((#.+address-tag-pinned+ #.+address-tag-stack+)
         object)))))

(defun verify-generic (object size gen)
  "Scavenge SIZE words pointed to by OBJECT."
  (let ((address (ash (%pointer-field object) 4)))
    (dotimes (i size)
      (verify-one address (+ address (* i 8)) gen))))

(defun verify-object (object gen)
  ;; Dispatch again based on the type.
  (case (%object-tag object)
    ((#.+object-tag-array-t+
      #.+object-tag-closure+
      #.+object-tag-funcallable-instance+)
     ;; simple-vector
     ;; 1+ to account for the header word.
     (verify-generic object (1+ (%object-header-data object)) gen))
    ((#.+object-tag-simple-string+
      #.+object-tag-string+
      #.+object-tag-simple-array+
      #.+object-tag-array+)
     ;; Dimensions don't need to be scanned
     (verify-generic object 4 gen))
    ((#.+object-tag-complex-rational+
      #.+object-tag-ratio+)
     (verify-generic object 3 gen))
    (#.+object-tag-symbol+
     (verify-generic object 8 gen))
    (#.+object-tag-structure-object+
     (verify-generic object (1+ (%object-header-data object)) gen))
    (#.+object-tag-std-instance+
     (verify-generic object 4 gen))
    (#.+object-tag-function-reference+
     (verify-generic object 4 gen))
    (#.+object-tag-function+
     ;; Not implemented.
     nil)
    ;; Things that don't need to be scanned.
    ((#.+object-tag-array-fixnum+
      #.+object-tag-array-bit+
      #.+object-tag-array-unsigned-byte-2+
      #.+object-tag-array-unsigned-byte-4+
      #.+object-tag-array-unsigned-byte-8+
      #.+object-tag-array-unsigned-byte-16+
      #.+object-tag-array-unsigned-byte-32+
      #.+object-tag-array-unsigned-byte-64+
      #.+object-tag-array-signed-byte-1+
      #.+object-tag-array-signed-byte-2+
      #.+object-tag-array-signed-byte-4+
      #.+object-tag-array-signed-byte-8+
      #.+object-tag-array-signed-byte-16+
      #.+object-tag-array-signed-byte-32+
      #.+object-tag-array-signed-byte-64+
      #.+object-tag-array-single-float+
      #.+object-tag-array-double-float+
      #.+object-tag-array-short-float+
      #.+object-tag-array-long-float+
      #.+object-tag-array-complex-single-float+
      #.+object-tag-array-complex-double-float+
      #.+object-tag-array-complex-short-float+
      #.+object-tag-array-complex-long-float+
      #.+object-tag-bignum+
      #.+object-tag-double-float+
      #.+object-tag-short-float+
      #.+object-tag-long-float+
      ;; not complex-rational or ratio, they may hold other numbers.
      #.+object-tag-complex-single-float+
      #.+object-tag-complex-double-float+
      #.+object-tag-complex-short-float+
      #.+object-tag-complex-long-float+
      #.+object-tag-mmx-vector+
      #.+object-tag-sse-vector+
      #.+object-tag-unbound-value+))
    (#.+object-tag-thread+
     ;; not implemented
     nil)
    (#.+object-tag-weak-pointer+
     ;; not implemented
     nil)
    (#.+object-tag-delimited-continuation+
     ;; not implemented
     nil)
    (t (scan-error object))))

(defun verify-at (address gen)
  (let ((type (ash (memref-unsigned-byte-8 address 0) (- +object-type-shift+))))
    (case type
      (#.+object-tag-cons+
       ;; Scanning a cons represented as a headered object.
       (verify-one address (+ address 16) gen)
       (verify-one address (+ address 24) gen)
       32)
      (#.+object-tag-freelist-entry+
       ;; Skip freelist entries.
       (* (ash (memref-unsigned-byte-64 address 0)
               (- +object-data-shift+))
          8))
      (t
       (let ((object (%%assemble-value address +tag-object+)))
         (verify-object object gen)
         (* (align-up (object-size object) 2) 8))))))

(defun verify-range (start size gen)
  (loop
     with end = (+ start size)
     with current = start
     until (>= current end)
     do
       (incf current (verify-at current gen))))

(defun verify-cons-range (start size gen)
  (loop
     for current from start below (+ start size) by 16
     do
       (verify-one current (+ current 0) gen)
       (verify-one current (+ current 8) gen)))

(defun gc-dump-area-state ()
  (mezzano.supervisor:debug-print-line "Wired: " *wired-area-base* " " (- *wired-area-bump* *wired-area-base*))
  (mezzano.supervisor:debug-print-line "Pinned: " *pinned-area-base* " " (- *pinned-area-bump* *pinned-area-base*))
  (mezzano.supervisor:debug-print-line "General gen0: " *general-area-gen0-bump* " " *general-area-gen0-limit*)
  (mezzano.supervisor:debug-print-line "General gen1: " *general-area-gen1-bump* " " *general-area-gen1-limit*)
  (mezzano.supervisor:debug-print-line "General gen2: " *general-area-bump* " " *general-area-limit*)
  (mezzano.supervisor:debug-print-line "General expansion: " mezzano.runtime::*general-area-expansion-granularity*)
  (mezzano.supervisor:debug-print-line "Cons gen0: " *cons-area-gen0-bump* " " *cons-area-gen0-limit*)
  (mezzano.supervisor:debug-print-line "Cons gen1: " *cons-area-gen1-bump* " " *cons-area-gen1-limit*)
  (mezzano.supervisor:debug-print-line "Cons gen2: " *cons-area-bump* " " *cons-area-limit*)
  (mezzano.supervisor:debug-print-line "Cons expansion: " mezzano.runtime::*cons-area-expansion-granularity*)
  (mezzano.supervisor:debug-print-line "Dynamic mark bit: " *dynamic-mark-bit*)
  (mezzano.supervisor:debug-print-line "Wired stack bump: " *wired-stack-area-bump* " stack bump: " *stack-area-bump* " total: " *bytes-allocated-to-stacks*)
  (mezzano.supervisor:debug-print-line "Allocation fudge: " mezzano.runtime::*allocation-fudge* " store fudge: " mezzano.supervisor::*store-fudge-factor*)
  (mezzano.supervisor:debug-print-line "Remaining: " (mezzano.runtime::bytes-remaining)))

(defun gc-insufficient-space ()
  (mezzano.supervisor:debug-print-line "Insufficient space for garbage collection!")
  (gc-dump-area-state)
  (mezzano.supervisor:panic "Insufficient space for garbage collection!"))

(defun update-dirty-generation (start size gen)
  (gc-log "Update dirty generation " start " " (+ start size) " " gen)
  (loop
     with next-gen = (1+ gen)
     for addr from start below (+ start size) by +card-size+
     do
       (let ((current (card-table-dirty-gen addr)))
         (when (and current (< current next-gen))
           (setf (card-table-dirty-gen addr) next-gen)))))

(defun gc-minor-cycle (gen)
  "Collect all generations <= gen into gen+1."
  (gc-log "Minor GC. Gen " gen)
  ;; Gen1 bump pointers will be changed by the scanning.
  (let ((general-bump (ecase gen
                        (0 *general-area-gen1-bump*)
                        (1 *general-area-bump*)))
        (cons-bump (ecase gen
                     (0 *cons-area-gen1-bump*)
                     (1 *cons-area-bump*)))
        (general-limit (ecase gen
                        (0 *general-area-gen1-limit*)
                        (1 *general-area-limit*)))
        (cons-limit (ecase gen
                     (0 *cons-area-gen1-limit*)
                     (1 *cons-area-limit*)))
        (target-generation (ecase gen
                             (0 (dpb +address-generation-1+ +address-generation+ 0))
                             (1 *dynamic-mark-bit*)))
        (general-total (ecase gen
                         (0 *general-area-gen0-limit*)
                         (1 (+ *general-area-gen0-limit* *general-area-gen1-limit*))))
        (cons-total (ecase gen
                         (0 *cons-area-gen0-limit*)
                         (1 (+ *cons-area-gen0-limit* *cons-area-gen1-limit*)))))
    ;; Allocate newspace in the target generation.
    ;; This allocates the maximum space required up front to avoid complicating transport.
    (when (not (mezzano.supervisor:allocate-memory-range (+ (logior target-generation
                                                                    (ash +address-tag-general+ +address-tag-shift+))
                                                            general-limit)
                                                         general-total
                                                         (logior +block-map-present+
                                                                 +block-map-writable+
                                                                 +block-map-zero-fill+)))
      (gc-insufficient-space))
    (when (not (mezzano.supervisor:allocate-memory-range (+ (logior target-generation
                                                                    (ash +address-tag-cons+ +address-tag-shift+))
                                                            cons-limit)
                                                         cons-total
                                                         (logior +block-map-present+
                                                                 +block-map-writable+
                                                                 +block-map-zero-fill+)))
      (gc-insufficient-space))
    ;; Disable dirty bit tracking on the target generation.
    (mezzano.supervisor:protect-memory-range (logior target-generation
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             general-limit
                                             (logior +block-map-present+
                                                     +block-map-writable+))
    (mezzano.supervisor:protect-memory-range (logior target-generation
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             cons-limit
                                             (logior +block-map-present+
                                                     +block-map-writable+))
    ;; Scan roots.
    ;; All thread stacks.
    (loop
       for thread = mezzano.supervisor::*all-threads* then (mezzano.supervisor::thread-global-next thread)
       until (null thread)
       do (minor-scan-thread thread gen))
    ;; The remembered set.
    (mezzano.supervisor:update-wired-dirty-bits) ; Transfer dirty bits from the wired area to the card table.
    (minor-scan-range *wired-area-base* (- *wired-area-bump* *wired-area-base*) gen)
    (minor-scan-range *pinned-area-base* (- *pinned-area-bump* *pinned-area-base*) gen)
    (when (eql gen 0)
      (minor-scan-range (logior (ash +address-tag-general+ +address-tag-shift+)
                                *dynamic-mark-bit*)
                        *general-area-bump*
                        gen)
      (minor-scan-cons-range (logior (ash +address-tag-cons+ +address-tag-shift+)
                                     *dynamic-mark-bit*)
                             *cons-area-bump*
                             gen))
    (minor-scan-range (logior (ash +address-tag-general+ +address-tag-shift+)
                              target-generation)
                      general-bump
                      gen)
    (minor-scan-cons-range (logior (ash +address-tag-cons+ +address-tag-shift+)
                                   target-generation)
                           cons-bump
                           gen)
    ;; Now scan all copied objects. They'll have been copied into gen1 and extend from
    ;; the original bump pointer up to the current bump pointer.
    (setf *scavenge-general-finger* general-bump
          *scavenge-cons-finger* cons-bump)
    (multiple-value-bind (target-general-bump target-general-limit target-cons-bump target-cons-limit)
        (ecase gen
          (0 (values '*general-area-gen1-bump* '*general-area-gen1-limit* '*cons-area-gen1-bump* '*cons-area-gen1-limit*))
          (1 (values '*general-area-bump* '*general-area-limit* '*cons-area-bump* '*cons-area-limit*)))
      (loop
         (gc-log
          "Minor General. Limit: " (symbol-value target-general-limit)
          "  Bump: " (symbol-value target-general-bump)
          "  Curr: " *scavenge-general-finger*)
         (gc-log
          "Minor Cons.    Limit: " (symbol-value target-cons-limit)
          "  Bump: " (symbol-value target-cons-bump)
          "  Curr: " *scavenge-cons-finger*)
         ;; Stop when both area sets have been fully scavenged.
         (when (and (eql *scavenge-general-finger* (symbol-value target-general-bump))
                    (eql *scavenge-cons-finger* (symbol-value target-cons-bump)))
           (return))
         (gc-log "Scav minor seq")
         ;; Scavenge general area.
         (loop
            (when (eql *scavenge-general-finger* (symbol-value target-general-bump))
              (return))
            (let* ((object (%%assemble-value (logior *scavenge-general-finger*
                                                     (ash +address-tag-general+ +address-tag-shift+)
                                                     target-generation)
                                             +tag-object+))
                   (size (object-size object)))
              (when (oddp size)
                (incf size))
              (scan-object object gen)
              (incf *scavenge-general-finger* (* size 8))))
         ;; Scavenge cons area.
         (loop
            (when (eql *scavenge-cons-finger* (symbol-value target-cons-bump))
              (return))
            (let ((object (%%assemble-value (logior *scavenge-cons-finger*
                                                    (ash +address-tag-cons+ +address-tag-shift+)
                                                    target-generation)
                                            +tag-cons+)))
              (scan-object object gen)
              (incf *scavenge-cons-finger* 16)))))
    ;; Collection complete.
    (when *gc-debug-validate-intergenerational-pointers*
      ;; Make sure wired, pinned, gen1, gen2 don't contain gen0 pointers.
      (verify-range *wired-area-base*
                    (- *wired-area-bump* *wired-area-base*)
                    +address-generation-0+)
      (verify-range *pinned-area-base*
                    (- *pinned-area-bump* *pinned-area-base*)
                    +address-generation-0+)
      (verify-range (logior (ash +address-tag-general+ +address-tag-shift+)
                            *dynamic-mark-bit*)
                    *general-area-bump*
                    +address-generation-0+)
      (verify-cons-range (logior (ash +address-tag-cons+ +address-tag-shift+)
                                 *dynamic-mark-bit*)
                         *cons-area-bump*
                         +address-generation-0+)
      (when (eql gen 0)
        (verify-range (logior (ash +address-tag-general+ +address-tag-shift+)
                              (dpb +address-generation-1+ +address-generation+ 0))
                      *general-area-gen1-bump*
                      +address-generation-0+)
        (verify-cons-range (logior (ash +address-tag-cons+ +address-tag-shift+)
                                   (dpb +address-generation-1+ +address-generation+ 0))
                           *cons-area-gen1-bump*
                           +address-generation-0+)))
    ;; Clear target generation dirty bits, there are no objects in younger generations.
    (update-dirty-generation *wired-area-base* (- *wired-area-bump* *wired-area-base*) gen)
    (update-dirty-generation *pinned-area-base* (- *pinned-area-bump* *pinned-area-base*) gen)
    (mezzano.supervisor:protect-memory-range *pinned-area-base*
                                             (- *pinned-area-bump* *pinned-area-base*)
                                             (logior +block-map-present+
                                                     +block-map-writable+
                                                     +block-map-track-dirty+))
    (update-dirty-generation (logior (ash +address-tag-general+ +address-tag-shift+)
                                     target-generation)
                             general-bump
                             gen)
    (update-dirty-generation (logior (ash +address-tag-cons+ +address-tag-shift+)
                                     target-generation)
                             cons-bump
                             gen)
    (when (eql gen 0)
      (update-dirty-generation (logior (ash +address-tag-general+ +address-tag-shift+)
                                       *dynamic-mark-bit*)
                               *general-area-bump*
                               gen)
      (update-dirty-generation (logior (ash +address-tag-cons+ +address-tag-shift+)
                                       *dynamic-mark-bit*)
                               *cons-area-bump*
                               gen)
      ;; Refresh dirty tracking on gen2.
      (mezzano.supervisor:protect-memory-range (logior (ash +address-tag-general+ +address-tag-shift+)
                                                       *dynamic-mark-bit*)
                                               *general-area-limit*
                                               (logior +block-map-present+
                                                       +block-map-writable+
                                                       +block-map-track-dirty+))
      (mezzano.supervisor:protect-memory-range (logior (ash +address-tag-cons+ +address-tag-shift+)
                                                       *dynamic-mark-bit*)
                                               *cons-area-limit*
                                               (logior +block-map-present+
                                                       +block-map-writable+
                                                       +block-map-track-dirty+)))
    ;; Trim target down to the bump pointer and tag it for dirty tracking.
    (let ((new-limit (align-up (ecase gen
                                 (0 *general-area-gen1-bump*)
                                 (1 *general-area-bump*))
                               +allocation-minimum-alignment+))
          (current-limit (ecase gen
                           (0 (+ *general-area-gen0-limit* *general-area-gen1-limit*))
                           (1 (+ *general-area-gen0-limit* *general-area-gen1-limit* *general-area-limit*)))))
      (mezzano.supervisor:protect-memory-range (logior target-generation
                                                       (ash +address-tag-general+ +address-tag-shift+))
                                               new-limit
                                               (logior +block-map-present+
                                                       +block-map-writable+
                                                       +block-map-track-dirty+))
      (mezzano.supervisor:release-memory-range (logior target-generation
                                                       new-limit
                                                       (ash +address-tag-general+ +address-tag-shift+))
                                               (- current-limit new-limit))
      (ecase gen
        (0 (setf *general-area-gen1-limit* new-limit))
        (1 (setf *general-area-limit* new-limit))))
    (let ((new-limit (align-up (ecase gen
                                 (0 *cons-area-gen1-bump*)
                                 (1 *cons-area-bump*))
                               +allocation-minimum-alignment+))
          (current-limit (ecase gen
                           (0 (+ *cons-area-gen0-limit* *cons-area-gen1-limit*))
                           (1 (+ *cons-area-gen0-limit* *cons-area-gen1-limit* *cons-area-limit*)))))
      (mezzano.supervisor:protect-memory-range (logior target-generation
                                                       (ash +address-tag-cons+ +address-tag-shift+))
                                               new-limit
                                               (logior +block-map-present+
                                                       +block-map-writable+
                                                       +block-map-track-dirty+))
      (mezzano.supervisor:release-memory-range (logior target-generation
                                                       new-limit
                                                       (ash +address-tag-cons+ +address-tag-shift+))
                                               (- current-limit new-limit))
      (ecase gen
        (0 (setf *cons-area-gen1-limit* new-limit))
        (1 (setf *cons-area-limit* new-limit))))
    ;; Free younger generations.
    (mezzano.supervisor:release-memory-range (logior (dpb +address-generation-0+ +address-generation+ 0)
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             *general-area-gen0-limit*)
    (mezzano.supervisor:release-memory-range (logior (dpb +address-generation-0+ +address-generation+ 0)
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             *cons-area-gen0-limit*)
    (setf *general-area-gen0-bump* 0
          *general-area-gen0-limit* 0
          *cons-area-gen0-bump* 0
          *cons-area-gen0-limit* 0)
    (when (<= 1 gen)
      (mezzano.supervisor:release-memory-range (logior (dpb +address-generation-1+ +address-generation+ 0)
                                                       (ash +address-tag-general+ +address-tag-shift+))
                                               *general-area-gen1-limit*)
      (mezzano.supervisor:release-memory-range (logior (dpb +address-generation-1+ +address-generation+ 0)
                                                       (ash +address-tag-cons+ +address-tag-shift+))
                                               *cons-area-gen1-limit*)
      (setf *general-area-gen1-bump* 0
            *general-area-gen1-limit* 0
            *cons-area-gen1-bump* 0
            *cons-area-gen1-limit* 0))))

(defun gc-major-cycle ()
  "Collect all generations, promoting all live data to gen2."
    (gc-log "Major GC.")
  ;; Reset the weak pointer worklist.
  (setf *weak-pointer-worklist* '())
  (let ((prev-dynamic-mark-bit *dynamic-mark-bit*)
        ;; Limits may be increased during transport as objects age.
        (prev-general-limit *general-area-limit*)
        (prev-cons-limit *cons-area-limit*)
        (maximum-general-limit (+ *general-area-gen0-limit*
                                  *general-area-gen1-limit*
                                  *general-area-limit*))
        (maximum-cons-limit (+ *cons-area-gen0-limit*
                               *cons-area-gen1-limit*
                               *cons-area-limit*)))
    ;; Flip.
    (psetf *dynamic-mark-bit* (other-gen2-area-from-mark-bit *dynamic-mark-bit*)
           *pinned-mark-bit* (logxor *pinned-mark-bit* +pinned-object-mark-bit+))
    (gc-log "Newspace " (logior *dynamic-mark-bit* (ash +address-tag-general+ +address-tag-shift+))
            "-" (+ (logior *dynamic-mark-bit* (ash +address-tag-general+ +address-tag-shift+))
                   maximum-general-limit))
    (gc-log "Newspace " (logior *dynamic-mark-bit* (ash +address-tag-cons+ +address-tag-shift+))
            "-" (+ (logior *dynamic-mark-bit* (ash +address-tag-cons+ +address-tag-shift+))
                   maximum-cons-limit))
    (gc-log "Oldspace " (logior prev-dynamic-mark-bit (ash +address-tag-general+ +address-tag-shift+))
            "-" (+ (logior *dynamic-mark-bit* (ash +address-tag-general+ +address-tag-shift+)) *general-area-limit*))
    (gc-log "Oldspace " (logior prev-dynamic-mark-bit (ash +address-tag-cons+ +address-tag-shift+))
            "-" (+ (logior *dynamic-mark-bit* (ash +address-tag-cons+ +address-tag-shift+)) *cons-area-limit*))
    ;; Mark all oldspace as non-tracked. gen1 and the pinned area.
    (mezzano.supervisor:protect-memory-range *pinned-area-base*
                                             (- *pinned-area-bump* *pinned-area-base*)
                                             (logior +block-map-present+
                                                     +block-map-writable+))
    (mezzano.supervisor:protect-memory-range (logior (dpb +address-generation-1+ +address-generation+ 0)
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             *general-area-gen1-limit*
                                             (logior +block-map-present+
                                                     +block-map-writable+))
    (mezzano.supervisor:protect-memory-range (logior (dpb +address-generation-1+ +address-generation+ 0)
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             *cons-area-gen1-limit*
                                             (logior +block-map-present+
                                                     +block-map-writable+))
    (mezzano.supervisor:protect-memory-range (logior prev-dynamic-mark-bit
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             *general-area-limit*
                                             (logior +block-map-present+
                                                     +block-map-writable+))
    (mezzano.supervisor:protect-memory-range (logior prev-dynamic-mark-bit
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             *cons-area-limit*
                                             (logior +block-map-present+
                                                     +block-map-writable+))
    ;; Allocate newspace.
    ;; This allocates the maximum space required up front to avoid complicating transport.
    (when (not (mezzano.supervisor:allocate-memory-range (logior *dynamic-mark-bit*
                                                                 (ash +address-tag-general+ +address-tag-shift+))
                                                         maximum-general-limit
                                                         (logior +block-map-present+
                                                                 +block-map-writable+
                                                                 +block-map-zero-fill+)))
      (gc-insufficient-space))
    (when (not (mezzano.supervisor:allocate-memory-range (logior *dynamic-mark-bit*
                                                                 (ash +address-tag-cons+ +address-tag-shift+))
                                                         maximum-cons-limit
                                                         (logior +block-map-present+
                                                                 +block-map-writable+
                                                                 +block-map-zero-fill+)))
      (gc-insufficient-space))
    (setf *general-area-bump* 0
          *cons-area-bump* 0
          *scavenge-general-finger* 0
          *scavenge-cons-finger* 0)
    (gc-log "Scav roots")
    ;; Scavenge NIL to start things off.
    (scavenge-object 'nil :major)
    ;; And various important other roots.
    (scavenge-object (%unbound-value) :major)
    (scavenge-object (%undefined-function) :major)
    (scavenge-object (%closure-trampoline) :major)
    (scavenge-object (%funcallable-instance-trampoline) :major)
    ;; Scavenge the current thread's stack.
    (scavenge-current-thread)
    ;; Now do the bulk of the work by scavenging the dynamic areas.
    (scavenge-dynamic)
    ;; Weak pointers.
    (update-weak-pointers)
    (finalizer-processing)
    ;; Flush oldspace.
    (mezzano.supervisor:release-memory-range (logior (dpb +address-generation-0+ +address-generation+ 0)
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             *general-area-gen0-limit*)
    (mezzano.supervisor:release-memory-range (logior (dpb +address-generation-0+ +address-generation+ 0)
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             *cons-area-gen0-limit*)
    (mezzano.supervisor:release-memory-range (logior (dpb +address-generation-1+ +address-generation+ 0)
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             *general-area-gen1-limit*)
    (mezzano.supervisor:release-memory-range (logior (dpb +address-generation-1+ +address-generation+ 0)
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             *cons-area-gen1-limit*)
    (setf *general-area-gen0-bump* 0
          *general-area-gen0-limit* 0
          *general-area-gen1-bump* 0
          *general-area-gen1-limit* 0
          *cons-area-gen0-bump* 0
          *cons-area-gen0-limit* 0
          *cons-area-gen1-bump* 0
          *cons-area-gen1-limit* 0)
    (mezzano.supervisor:release-memory-range (logior prev-dynamic-mark-bit
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             prev-general-limit)
    (mezzano.supervisor:release-memory-range (logior prev-dynamic-mark-bit
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             prev-cons-limit)
    ;; Trim newspace down.
    (let ((new-limit (align-up *general-area-bump* +allocation-minimum-alignment+)))
      (mezzano.supervisor:release-memory-range (logior *dynamic-mark-bit*
                                                       new-limit
                                                       (ash +address-tag-general+ +address-tag-shift+))
                                               (- maximum-general-limit new-limit))
      (setf *general-area-limit* new-limit))
    (let ((new-limit (align-up *cons-area-bump* +allocation-minimum-alignment+)))
      (mezzano.supervisor:release-memory-range (logior *dynamic-mark-bit*
                                                       new-limit
                                                       (ash +address-tag-cons+ +address-tag-shift+))
                                               (- maximum-cons-limit new-limit))
      (setf *cons-area-limit* new-limit))
    ;; Mark newspace as trackable.
    (mezzano.supervisor:protect-memory-range (logior *dynamic-mark-bit*
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             *general-area-limit*
                                             (logior +block-map-present+
                                                     +block-map-writable+
                                                     +block-map-track-dirty+))
    (mezzano.supervisor:protect-memory-range (logior *dynamic-mark-bit*
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             *cons-area-limit*
                                             (logior +block-map-present+
                                                     +block-map-writable+
                                                     +block-map-track-dirty+))
    ;; Rebuild freelists.
    (rebuild-freelist *wired-area-free-bins* :wired *wired-area-base* *wired-area-bump*)
    (rebuild-freelist *pinned-area-free-bins* :pinned *pinned-area-base* *pinned-area-bump*)
    ;; Make sure wired, pinned, gen1, gen2 don't contain gen0 pointers.
    (when *gc-debug-validate-intergenerational-pointers*
      (verify-range *wired-area-base*
                    (- *wired-area-bump* *wired-area-base*)
                    +address-generation-1+)
      (verify-range *pinned-area-base*
                    (- *pinned-area-bump* *pinned-area-base*)
                    +address-generation-1+)
      (verify-range (logior (ash +address-tag-general+ +address-tag-shift+)
                            *dynamic-mark-bit*)
                    *general-area-bump*
                    +address-generation-1+)
      (verify-cons-range (logior (ash +address-tag-cons+ +address-tag-shift+)
                                 *dynamic-mark-bit*)
                         *cons-area-bump*
                         +address-generation-1+))
    ;; Reset the pinned area's dirty bits.
    ;; Newspace's dirty bits have been cleared by the allocate & reprotect.
    (loop
       for i from *pinned-area-base* below *pinned-area-bump* by +card-size+
       do (setf (card-table-dirty-gen i) nil))
    (mezzano.supervisor:protect-memory-range *pinned-area-base*
                                             (- *pinned-area-bump* *pinned-area-base*)
                                             (logior +block-map-present+
                                                     +block-map-writable+
                                                     +block-map-track-dirty+))
    (mezzano.supervisor:update-wired-dirty-bits)
    (loop
       for i from *wired-area-base* below *wired-area-bump* by +card-size+
       do (setf (card-table-dirty-gen i) nil))
    (setf *gc-last-general-address* (logior (ash +address-tag-general+ +address-tag-shift+)
                                            *general-area-bump*
                                            *dynamic-mark-bit*)
          *gc-last-cons-address* (logior (ash +address-tag-cons+ +address-tag-shift+)
                                         *cons-area-bump*
                                         *dynamic-mark-bit*))))

(defun gc-cycle (force-major target-generation)
  (mezzano.supervisor::set-gc-light t)
  (gc-log "GC in progress... " force-major " " target-generation)
  (incf *gc-cycles*)
  (when (not (or force-major target-generation))
    ;; Figure out exactly what kind of collection to do.
    ;; FIXME: This is probably pretty lame. It doesn't seem to do any gen1 collections...
    (let ((gen0-size (+ *general-area-gen0-limit* *cons-area-gen0-limit*))
          (gen1-size (+ *general-area-gen1-limit* *cons-area-gen1-limit*))
          (gen2-size (+ *general-area-limit* *cons-area-limit*))
          (remaining (mezzano.runtime::bytes-remaining)))
      (cond ((>= (mezzano.runtime::bytes-remaining) (* 32 1024 1024))
             (setf target-generation 0))
            ((< (+ gen0-size gen1-size) (* gen2-size *generation-size-ratio*)) ; kinda arbitrary
             (setf force-major t))
            ((< gen0-size (* gen1-size *generation-size-ratio*))
             (setf target-generation 1))
            (t
             (setf target-generation 0)))))
  (when *gc-enable-logging*
    (gc-dump-area-state))
  (cond (force-major
         (incf *gc-major-cycles*)
         (gc-major-cycle))
        (t
         (ecase target-generation
           (0 (incf *gc-gen0-cycles*))
           (1 (incf *gc-gen1-cycles*)))
         (gc-minor-cycle target-generation)))
  (setf mezzano.runtime::*general-area-expansion-granularity* +allocation-minimum-alignment+
        mezzano.runtime::*cons-area-expansion-granularity* +allocation-minimum-alignment+)
  (when *gc-enable-logging*
    (gc-dump-area-state))
  (incf *gc-epoch*)
  (gc-log "GC complete")
  (mezzano.supervisor::set-gc-light nil))

(defun base-address-of-internal-pointer (address)
  "Find the base address of the object pointed to be ADDRESS.
Address should be an internal pointer to a live object in static space.
No type information will be provided."
  (let ((current-address address))
    (loop
         (let ((card (align-down current-address +card-size+))
               (entry (card-table-offset current-address)))
           (when entry
             ;; Found the start of an object near the target.
             ;; Walk forward to find the actual object.
             (let ((offset (+ card entry)))
               (loop
                  (when (eql offset address)
                    (return))
                  (when (> offset address)
                    (error "Failed to find object containing ~X" address))
                  (let* ((size (* (size-of-pinned-area-allocation offset) 8))
                         (aligned-size (align-up size 16)))
                    (when (<= offset address (+ offset size -1))
                      (return))
                    (when (<= offset address (+ offset aligned-size -1))
                      (error "Internal pointer " address " matches object " offset " with size " size " but is past the end"))
                    (incf offset aligned-size)))
               (return offset))))
       ;; Entry not found. Go back a card (TODO: should go back a whole bunch of cards.)
       (decf current-address +card-size+))))

(deftype weak-pointer ()
  '(satisfies weak-pointer-p))

(defun weak-pointer-p (object)
  (%object-of-type-p object +object-tag-weak-pointer+))

(defun weak-pointer-pair (object)
  "Returns the key, the value, and T if the key is still live, otherwise NIL, NIL and NIL."
  (check-type object weak-pointer)
  ;; Make a strong reference to key & value first.
  ;; It'll be set to some other live value if the weak pointer is dead.
  (let ((key (%object-ref-t object +weak-pointer-key+))
        (value (%object-ref-t object +weak-pointer-value+)))
    ;; Inspect the livep header bit.
    (if (logbitp +weak-pointer-header-livep+ (%object-header-data object))
        (values key value t)
        (values nil nil nil))))

(defun weak-pointer-key (object)
  "Returns the key and T if the key is still live, otherwise NIL and NIL."
  (check-type object weak-pointer)
  ;; Make a strong reference to key first.
  ;; It'll be set to some other live value if the weak pointer is dead.
  (let ((key (%object-ref-t object +weak-pointer-key+)))
    ;; Inspect the livep header bit.
    (if (logbitp +weak-pointer-header-livep+ (%object-header-data object))
        (values key t)
        (values nil nil))))

(defun weak-pointer-value (object)
  "Returns the value and T if the key is still live, otherwise NIL and NIL."
  (check-type object weak-pointer)
  ;; Make a strong reference to value first.
  ;; It'll be set to some other live value if the weak pointer is dead.
  (let ((value (%object-ref-t object +weak-pointer-value+)))
    ;; Inspect the livep header bit.
    (if (logbitp +weak-pointer-header-livep+ (%object-header-data object))
        (values value t)
        (values nil nil))))

(defun examine-weak-pointer-key (key)
  (when (immediatep key)
    ;; Immediate objects are always live.
    (return-from examine-weak-pointer-key
      (values key t)))
  (let ((address (ash (%pointer-field key) 4)))
    (ecase (ldb (byte +address-tag-size+ +address-tag-shift+) address)
      ((#.+address-tag-general+
        #.+address-tag-cons+)
       ;; Look for a forwarding pointer.
       (let ((first-word (memref-t address 0)))
         (cond ((%value-has-tag-p first-word +tag-gc-forward+)
                ;; Object is still live.
                (values (%%assemble-value (ash (%pointer-field first-word) 4)
                                          (%tag-field key))
                        t))
               (t ;; Object is dead.
                (values nil nil)))))
      (#.+address-tag-pinned+
       (cond ((eql (logand (memref-unsigned-byte-64 address
                                                    (if (consp key)
                                                        -2
                                                        0))
                           +pinned-object-mark-bit+)
                   *pinned-mark-bit*)
              ;; Object is live.
              (values key t))
             (t
              (values nil nil)))))))

(defun update-weak-pointers ()
  (loop
     with did-something = nil
     do
       (do* ((new-worklist nil)
             (iter *weak-pointer-worklist*))
            ((not iter)
             (setf *weak-pointer-worklist* new-worklist))
         (let ((weak-pointer iter))
           (setf iter (%object-ref-t weak-pointer +weak-pointer-link+))
           (multiple-value-bind (new-key livep)
               (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-key+))
             (cond (livep
                    ;; Key is live.
                    ;; Drop this weak pointer from the worklist.
                    (setf did-something t)
                    (setf (%object-ref-t weak-pointer +weak-pointer-key+) new-key)
                    ;; Keep the value live.
                    (scavengef (%object-ref-t weak-pointer +weak-pointer-value+) :major))
                   (t ;; Key is either dead or not scanned yet. Add to the new worklist.
                    (setf (%object-ref-t weak-pointer +weak-pointer-link+) new-worklist
                          new-worklist weak-pointer))))))
     ;; Stop when no more memory needs to be scavenged.
       (when (not did-something)
         (return))
       (setf did-something nil)
       (scavenge-dynamic))
  ;; Final pass, no more memory will be scanned.
  ;; All weak pointers left on the worklist should be dead.
  (do ((weak-pointer *weak-pointer-worklist* (%object-ref-t weak-pointer +weak-pointer-link+)))
      ((not weak-pointer))
    (multiple-value-bind (new-key livep)
        (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-key+))
      (declare (ignore new-key))
      (when livep
        (mezzano.supervisor:panic "Weak pointer key live?")))
    (setf (%object-ref-t weak-pointer +weak-pointer-key+) nil
          (%object-ref-t weak-pointer +weak-pointer-value+) nil
          (%object-header-data weak-pointer) 0)))

(defun finalizer-processing ()
  ;; Walk the list of known finalizers, remove any weak pointers
  ;; that have become dead and add them to the pending finalizers list.
  (do* ((weak-pointer *known-finalizers* next)
        (next (when weak-pointer (%object-ref-t weak-pointer +weak-pointer-finalizer-link+))
              (when weak-pointer (%object-ref-t weak-pointer +weak-pointer-finalizer-link+)))
        (prev nil))
       ((null weak-pointer))
    (cond ((logbitp +weak-pointer-header-livep+ (%object-header-data weak-pointer))
           ;; Still live.
           (setf prev weak-pointer))
          (t
           ;; This one has become dead.
           (cond (prev
                  (setf (%object-ref-t prev +weak-pointer-finalizer-link+)
                        (%object-ref-t weak-pointer +weak-pointer-finalizer-link+)))
                 (t
                  (setf *known-finalizers* (%object-ref-t weak-pointer +weak-pointer-finalizer-link+))))
           (setf (%object-ref-t weak-pointer +weak-pointer-finalizer-link+) *pending-finalizers*
                 *pending-finalizers* weak-pointer)))))

(defun run-finalizers ()
  (flet ((pop-finalizer ()
           (loop
              for f = *pending-finalizers*
              when (eql (cas (symbol-global-value '*pending-finalizers*)
                             f
                             (%object-ref-t f +weak-pointer-finalizer-link+))
                        f)
              do (return f))))
    (loop
       for finalizer = (pop-finalizer)
       until (not finalizer)
       do
         (funcall (%object-ref-t finalizer +weak-pointer-finalizer+))
         ;; Leave the weak pointer completely empty.
         ;; No references to any other object.
         (setf (%object-ref-t finalizer +weak-pointer-finalizer+) nil))))
