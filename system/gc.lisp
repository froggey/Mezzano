;;;; The Garbage Collector

;;; Weak pointers:
;;;
;;; "Weak References: Data Types and Implementation" - Bruno Haible
;;; http://www.haible.de/bruno/papers/cs/weak/WeakDatastructures-writeup.html
;;;
;;; "Streching the storage manager: weak pointers and stable names in Haskell"
;;; - Simon Peyton Jones, Simon Marlow, and Conal Elliott
;;; http://community.haskell.org/~simonmar/papers/weak.pdf

(in-package :mezzano.internals)

(defglobal *gc-debug-scavenge-stack* nil)
(defglobal *gc-debug-freelist-rebuild* nil)
(defglobal *gc-debug-metadata* t)
(defglobal *gc-debug-validate-intergenerational-pointers* nil)

(defglobal *gc-enable-logging*)

;;; GC Meters.
(defglobal *old-objects-copied* 0)
(defglobal *objects-copied* 0)
(defglobal *words-copied* 0)

(defglobal *gc-enable-transport-metering* nil)
(defglobal *gc-transport-counts* (make-array 64 :area :pinned :initial-element 0))
(defglobal *gc-transport-old-counts* (make-array 64 :area :pinned :initial-element 0))
(defglobal *gc-transport-cycles* (make-array 64 :area :pinned :initial-element 0))

(defglobal *gc-in-progress* nil)

;; State of the dynamic pointer mark bit (aka +address-semispace+).
;; This is part of the pointer, not part of the object itself.
(defglobal *young-gen-newspace-bit*)
(defglobal *old-gen-newspace-bit*)
;; Identical to *YOUNG-GEN-NEWSPACE-BIT*, but shifted right by one so
;; the fast assembly allocation functions can use it directly.
(defglobal *young-gen-newspace-bit-raw*)
;; Used to track the last address that was scanned in newspace
(defglobal *scavenge-general-young-finger*)
(defglobal *scavenge-general-old-finger*)
(defglobal *scavenge-cons-young-finger*)
(defglobal *scavenge-cons-old-finger*)

(defglobal *gc-force-major-cycle* nil)

(defglobal *gc-minor-cycles* 0)
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
  (setf *gc-cycles* 0
        *gc-minor-cycles* 0
        *gc-major-cycles* 0)
  (setf *words-copied* 0
        *objects-copied* 0
        *old-objects-copied* 0)
  (fill *gc-transport-counts* 0)
  (fill *gc-transport-old-counts* 0)
  (fill *gc-transport-cycles* 0)
  (values))

(defun gc-stats ()
  (format t "Spent ~:D seconds in the GC over ~:D collections (~:D minor, ~:D major).~%"
          *gc-time* *gc-cycles* *gc-minor-cycles* *gc-major-cycles*)
  (format t "  Copied ~:D objects, ~:D new, ~:D words.~%"
          *objects-copied* (- *objects-copied* *old-objects-copied*) *words-copied*)
  (format t "Transport stats:~%")
  (dotimes (i (min (length *gc-transport-cycles*)
                   (length *gc-transport-old-counts*)
                   (length *gc-transport-counts*)))
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
(defvar *gc-poll-interval* 10.0)
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
  "Run a garbage-collection cycle.
If FULL is true, then a major collection will be forced."
  (%gc :reason 'gc :full full :major-required full))

(defun %gc (&key full reason major-required)
  "Like GC, but internal. This is the entry point the allocation functions use."
  (when *gc-enable-logging*
    (mezzano.supervisor:debug-print-line
     (if full "Full " "")
     (if major-required "Major " "")
     "GC triggered by " reason))
  ;; Clear the thread pool over this bit so that CONDITION-WAIT
  ;; doesn't try to call THREAD-POOL-BLOCK. T-P-B allocates and
  ;; that could cause a recursive call back into GC.
  (mezzano.supervisor:inhibit-thread-pool-blocking-hijack
    (mezzano.supervisor:with-mutex (*gc-lock*)
      (let ((epoch *gc-epoch*))
        (setf *gc-requested* t)
        (when (or major-required full)
          (setf *gc-force-major-cycle* t))
        (mezzano.supervisor:condition-notify *gc-cvar* t)
        (mezzano.supervisor:condition-wait-for (*gc-cvar* *gc-lock*)
                                               (not (eql epoch *gc-epoch*))))))
  (values))

(defun gc-worker ()
  (loop
     (let ((force-major nil))
       (mezzano.supervisor:with-mutex (*gc-lock*)
         ;; Notify any waiting threads that the GC epoch has changed.
         (mezzano.supervisor:condition-notify *gc-cvar* t)
         ;; Wait for a GC request.
         (mezzano.supervisor:condition-wait-for (*gc-cvar* *gc-lock*)
           *gc-requested*)
         (setf force-major *gc-force-major-cycle*
               *gc-force-major-cycle* nil)
         (setf *gc-requested* nil))
       (when *gc-in-progress*
         (mezzano.supervisor:panic "Nested GC?!"))
       (mezzano.supervisor:with-world-stopped ()
         ;; Set *GC-IN-PROGRESS* globally, not with a binding.
         ;; This ensures that it is visible across all threads, especially
         ;; threads not stopped by with-world-stopped.
         (unwind-protect
              (let ((gc-start (get-internal-run-time)))
                (setf *gc-in-progress* t)
                (gc-cycle force-major)
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
      (%value-has-tag-p object +tag-immediate+)))

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

(declaim (inline object-base-address))
(defun object-base-address (object)
  (logand (lisp-object-address object) (lognot #xF)))

(defun scavenge-object (object cycle-kind)
  "Scavenge one object, returning an updated pointer."
  (when (immediatep object)
    ;; Don't care about immediate objects, return them unchanged.
    (return-from scavenge-object object))
  (when (%value-has-tag-p object +tag-instance-header+)
    ;; Instance headers will be returned unchanged, and the pointed to
    ;; layout will be scavenged.
    ;; This assumes that instance headers only refer to pinned/wired objects
    ;; and don't need to move.
    (scavenge-object (mezzano.runtime::%unpack-instance-header object) cycle-kind)
    (return-from scavenge-object object))
  (let ((address (object-base-address object)))
    (ecase (ldb (byte +address-tag-size+ +address-tag-shift+) address)
      ((#.+address-tag-general+
        #.+address-tag-cons+)
       (ecase cycle-kind
         (:major
          (cond ((logtest address +address-old-generation+)
                 (cond ((eql (logand address +address-semispace+) *old-gen-newspace-bit*)
                        ;; Object in old gen's newspace.
                        object)
                       (t
                        ;; Object in old gen's oldspace, transport it.
                        (transport-object object cycle-kind))))
                (t
                 (cond ((eql (logand address +address-semispace+) *young-gen-newspace-bit*)
                        ;; Object in young gen's newspace.
                        object)
                       (t
                        ;; Object in young gen's oldspace, transport it.
                        (transport-object object cycle-kind))))))
         (:minor
          (cond ((logtest address +address-old-generation+)
                 ;; Object in the old generation.
                 object)
                (t
                 (cond ((eql (logand address +address-semispace+) *young-gen-newspace-bit*)
                        ;; Object in young gen's newspace.
                        object)
                       (t
                        ;; Object in young gen's oldspace, transport it.
                        (transport-object object cycle-kind))))))))
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
  (scavenge-many (object-base-address object) size cycle-kind))

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
                          (scan-object (%%assemble-value (object-base-address value)
                                                         +tag-object+)
                                       cycle-kind))
                         ;; Normal object. Don't do anything interesting.
                         (t
                          (scavengef (memref-t base offset) cycle-kind))))))
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
                                     mezzano.supervisor::+thread-mv-slots-size+))
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
               (sys.int::%copy-words (mezzano.runtime::%object-slot-address thread mezzano.supervisor::+thread-fxsave-area+)
                                     (+ stack-pointer (* 20 8))
                                     (truncate 512 8))
               (sys.int::%copy-words (mezzano.runtime::%object-slot-address thread mezzano.supervisor::+thread-mv-slots+)
                                     (+ stack-pointer (* 20 8) 512)
                                     mezzano.supervisor::+thread-mv-slots-size+))
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
          (scavenge-many (+ (object-base-address thread)
                            8
                            (* mezzano.supervisor::+thread-mv-slots+ 8))
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
  (when (function-reference-p function)
    (when *gc-debug-scavenge-stack*
      (gc-log "In FREF " function ":" offset))
    ;; Peer through the fref to get the real target function.
    (setf function (function-reference-function function))
    (when (not function)
      ;; Unbound fref, going to raise-undefined-function.
      (setf function #'raise-undefined-function))
    ;; Peel away any closures or funcallable instances.
    (loop
       while (funcallable-instance-p function)
       do (setf function (funcallable-instance-function function)))
    (when (closure-p function)
      (setf function (%closure-function function)))
    ;; Starting at the beginning, skipping the header.
    (setf offset 16))
  (when (not (%object-of-type-p function +object-tag-function+))
    (mezzano.supervisor:panic function " is not a function with GCMD"))
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
      #.+object-tag-symbol-value-cell+)
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
     (scan-generic object 6 cycle-kind))
    ((#.+object-tag-instance+
      #.+object-tag-funcallable-instance+)
     (flet ((scan-layout (layout)
              (let ((heap-layout (layout-heap-layout layout))
                    (heap-size (layout-heap-size layout)))
                (cond ((eql heap-layout 't)
                       ;; All slots boxed
                       (scan-generic object
                                     ;; 1+ to account for the header word.
                                     (1+ heap-size)
                                     cycle-kind))
                      (heap-layout
                       ;; Bit vector of slot boxedness.
                       (loop
                          for i below heap-size
                          when (eql (aref heap-layout i) 1)
                          do (scavengef (%object-ref-t object i) cycle-kind)))))))
       (declare (dynamic-extent #'scan-layout))
       (let ((direct-layout (%instance-layout object)))
         (scavenge-object direct-layout cycle-kind)
         (cond ((layout-p direct-layout)
                (scan-layout direct-layout))
               (t ;; Obsolete instance.
                ;; Much like a regular instance, but the layout comes from the
                ;; obsolete layout instead of directly from the object.
                (scan-layout (mezzano.runtime::obsolete-instance-layout-old-layout
                              direct-layout))))))
     (when (mezzano.supervisor:threadp object)
       (scan-thread object cycle-kind)))
    (#.+object-tag-function-reference+
     ;; Scan the first 4 words normally.
     (scan-generic object 4 cycle-kind)
     ;; Now pull the target function out of the branch and scan that.
     ;; We may have caught (SETF FUNCTION-REFERENCE-FUNCTION) halfway
     ;; through an update.
     (let ((rel-target (%object-ref-signed-byte-32 object (1+ (* +fref-code+ 2)))))
       (unless (eql rel-target 0) ; must be active
         (let* ((fref-jmp-address (+ (mezzano.runtime::%object-slot-address object (1+ +fref-code+))))
                (entry-point (+ fref-jmp-address rel-target)))
           ;; Reconstruct the object and scan it.
           ;; It is assumed to be an +object-tag-function+.
           (scan-function (%%assemble-value (- entry-point 16) +tag-object+)
                          cycle-kind)))))
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
      #.+object-tag-sse-vector+))
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
    (:minor
     ;; Weak pointer processing doesn't occur during a minor cycle.
     ;; If the key was previously live, assume it is still alive.
     (when (logbitp +weak-pointer-header-livep+ (%object-header-data object))
       (scavengef (%object-ref-t object +weak-pointer-key+) cycle-kind)
       (scavengef (%object-ref-t object +weak-pointer-value+) cycle-kind))))
  (scavengef (%object-ref-t object +weak-pointer-finalizer-link+) cycle-kind)
  (scavengef (%object-ref-t object +weak-pointer-finalizer+) cycle-kind))

(defun scan-function (object cycle-kind)
  ;; Scan the constant pool.
  (let* ((address (object-base-address object))
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

(defun transport-object (object cycle-kind)
  "Transport LENGTH words from oldspace to newspace, returning
a pointer to the new object. Leaves a forwarding pointer in place."
  (let* ((address (object-base-address object))
         (first-word (memref-t address 0)))
    ;; Check for a GC forwarding pointer.
    ;; Do this before getting the length as the forwarding pointer will
    ;; have overwritten the header word.
    (when (%value-has-tag-p first-word +tag-gc-forward+)
      (return-from transport-object
        (%%assemble-value (object-base-address first-word)
                          (%tag-field object))))
    (when (and (or (instance-p object)
                   (funcallable-instance-p object))
               (not (layout-p (%instance-layout object))))
      ;; This is an obsolete instance. Don't transport it at all,
      ;; instead replace it with the updated instance.
      ;; FIXME: There's a race-condition here.
      ;; If a one thread is accessing an obsolete instance (because it
      ;; was superseded partway through the access) then this will
      ;; update the old instance to point at the new instance and the
      ;; thread will perform the access on the new instance with the old
      ;; slot location. Such accesses should be performed in a GC restart
      ;; region or similar. Or this could be turned off, it's just an
      ;; optimization.
      (let ((new-instance (mezzano.runtime::obsolete-instance-layout-new-instance
                           (%instance-layout object))))
        (when (funcallable-instance-p object)
          ;; Keep the funcallable instance entry point correct.
          (setf (%object-ref-t new-instance +function-entry-point+)
                (%object-ref-t object +function-entry-point+))
          (setf (%object-ref-t new-instance +funcallable-instance-function+)
                (%object-ref-t object +funcallable-instance-function+)))
        (return-from transport-object
          (scavenge-object new-instance cycle-kind))))
    (really-transport-object object cycle-kind)))

(defun allocate-for-transport (object address length)
  "Allocate memory in the appropriate area for this object."
  (cond ((consp object)
         ;; Object staying in or moving to old generation.
         (prog1
             (logior (ash +address-tag-cons+ +address-tag-shift+)
                     +address-old-generation+
                     *cons-area-old-gen-bump*
                     *old-gen-newspace-bit*)
           (incf *cons-area-old-gen-bump* (* length 8))))
        (t
         ;; Object staying in or moving to old generation.
         (prog1
             (logior (ash +address-tag-general+ +address-tag-shift+)
                     +address-old-generation+
                     *general-area-old-gen-bump*
                     *old-gen-newspace-bit*)
           (incf *general-area-old-gen-bump* (* length 8))))))

(defun transport-meter-update (start-time length address)
  (let ((cycles (- (tsc) start-time))
        (bin (integer-length (1- (* length 8)))))
    (incf (svref *gc-transport-counts* bin))
    (when (logtest address +address-old-generation+)
      (incf *old-objects-copied*)
      (incf (svref *gc-transport-old-counts* bin)))
    (incf (svref *gc-transport-cycles* bin) cycles)))

(defun transport-object-start-update (object new-address length)
  ;; Conses are exempt as the cons area has a uniform layout.
  (when (not (consp object))
    (loop
       for card from (align-up new-address +card-size+) below (+ new-address (* length 8)) by +card-size+
       for delta = (- new-address card)
       do (setf (card-table-offset card)
                (if (<= delta (- (* (1- (ash 1 (byte-size +card-table-entry-offset+))) 16)))
                    nil
                    delta)))))

(defun really-transport-object (object cycle-kind)
  (let* ((address (object-base-address object))
         (start-time (when *gc-enable-transport-metering* (tsc)))
         (length (object-size object))
         (new-address nil))
    ;; Update meters.
    (incf *objects-copied*)
    (incf *words-copied* length)
    ;; Find a new location.
    (setf new-address (allocate-for-transport object address
                                              (if (oddp length)
                                                  (1+ length)
                                                  length)))
    ;; Energize!
    (%copy-words new-address address length)
    (when (oddp length)
      ;; Make sure the trailing word is always zero.
      (setf (memref-t new-address length) 0))
    ;; Leave a forwarding pointer.
    (setf (memref-t address 0) (%%assemble-value new-address +tag-gc-forward+))
    (when *gc-enable-transport-metering*
      ;; Update meters.
      (transport-meter-update start-time length address))
    ;; Update object starts.
    (transport-object-start-update object new-address length)
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
           #.+object-tag-closure+
           #.+object-tag-symbol-value-cell+)
          ;; simple-vector-like.
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
         (#.+object-tag-short-float+
          2)
         (#.+object-tag-long-float+
          4)
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
          6)
         ((#.+object-tag-instance+
           #.+object-tag-funcallable-instance+)
          (let ((direct-layout (%instance-layout object)))
            (if (layout-p direct-layout)
                (1+ (layout-heap-size direct-layout))
                (1+ (layout-heap-size
                     (mezzano.runtime::obsolete-instance-layout-old-layout
                      direct-layout))))))
         (#.+object-tag-function-reference+
          8)
         (#.+object-tag-function+
          ;; The size of a function is the sum of the MC, the GC info and the constant pool.
          (let ((sz (ceiling (+ (* (ldb +function-header-code-size+ length) 16)  ; mc size
                                (* (ldb +function-header-pool-size+ length) 8)  ; pool size
                                (ldb +function-header-metadata-size+ length)) ; gc-info size.
                             8)))
            ;; And rounded up to 4 words/32 bytes.
            (align-up sz 4)))
         ((#.+object-tag-simple-string+
           #.+object-tag-string+
           #.+object-tag-simple-array+
           #.+object-tag-array+)
          (+ 4 length))
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

#+x86-64
(define-lap-function set-mark-bit ((object))
  "Set the mark bit for OBJECT, returning the old value."
  (sys.lap-x86:cmp64 :rcx #.(ash 1 +n-fixnum-bits+))
  (sys.lap-x86:jne BAIL)
  ;; Convert to bit index into card table.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax #.(rational (log +octets-per-mark-bit+ 2)))
  ;; Fetch & set the bit.
  (sys.lap-x86:mov64 :rsi #.+mark-bit-region-base+)
  (sys.lap-x86:bts64 (:rsi) :rax)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:cmov64c :r8 (:constant t))
  (sys.lap-x86:ret)
  BAIL
  (sys.lap-x86:jmp (:named-call set-mark-bit-slow)))

(defun #-x86-64 set-mark-bit #+x86-64 set-mark-bit-slow (object)
  "Set the mark bit for OBJECT, returning the old value."
  (let ((address (object-base-address object)))
    (multiple-value-bind (byte-index bit-index)
        (truncate (truncate address +octets-per-mark-bit+) 8)
      (let ((byte (memref-unsigned-byte-8 +mark-bit-region-base+ byte-index))
            (field (ash 1 bit-index)))
        (cond ((logtest field byte)
               ;; Mark bit set already.
               t)
              (t
               ;; Not set, set it.
               (setf (memref-unsigned-byte-8 +mark-bit-region-base+ byte-index)
                     (logior byte field))
               nil))))))

(defun get-mark-bit (address)
  "Read the mark bit for address."
  (when (logtest address (1- +octets-per-mark-bit+))
    (mezzano.supervisor:panic "Tried to get mark bit for misaligned address " address))
  (multiple-value-bind (byte-index bit-index)
      (truncate (truncate address +octets-per-mark-bit+) 8)
    (let ((byte (memref-unsigned-byte-8 +mark-bit-region-base+ byte-index))
          (field (ash 1 bit-index)))
      (logtest field byte))))

(defun mark-pinned-object (object)
  (cond ((consp object)
         ;; Reconstruct an object-tagged cons for set-mark-bit.
         ;; The object header for conses is 16 bytes behind the address.
         (let ((address (- (object-base-address object) 16)))
           (when (not (eql (ldb (byte +object-type-size+ +object-type-shift+)
                                (memref-unsigned-byte-8 address))
                           +object-tag-cons+))
             (mezzano.supervisor:panic "Invalid pinned cons " object))
           (when (not (set-mark-bit (%%assemble-value address +tag-object+)))
             ;; Was not marked, scan it.
             (scan-object object :major))))
        ;; If it isn't a cons, then it must be a regular object.
        ;; Make sure it isn't a freelist entry.
        ((mezzano.runtime::%%object-of-type-p object +object-tag-freelist-entry+)
         (mezzano.supervisor:panic "Marking freelist entry " object))
        (t
         ;; Normal object.
         (when (not (set-mark-bit object))
           ;; Was not marked, scan it.
           (scan-object object :major)))))

(defun scavenge-dynamic (cycle-kind)
  (loop
     (gc-log
      "Old general. " cycle-kind " Limit: " *general-area-old-gen-limit*
      "  Bump: " *general-area-old-gen-bump*
      "  Curr: " *scavenge-general-old-finger*)
     (gc-log
      "Old cons.    " cycle-kind " Limit: " *cons-area-old-gen-limit*
      "  Bump: " *cons-area-old-gen-bump*
      "  Curr: " *scavenge-cons-old-finger*)
     ;; Stop when all area sets have been fully scavenged.
     (when (and (eql *scavenge-general-old-finger* *general-area-old-gen-bump*)
                (eql *scavenge-cons-old-finger* *cons-area-old-gen-bump*))
       (return))
     (gc-log "Scav main seq")
     ;; Scavenge general area.
     (loop
        (when (eql *scavenge-general-old-finger* *general-area-old-gen-bump*)
          (return))
        (let* ((object (%%assemble-value (logior *scavenge-general-old-finger*
                                                 (ash +address-tag-general+ +address-tag-shift+)
                                                 *old-gen-newspace-bit*
                                                 +address-old-generation+)
                                         +tag-object+))
               (size (object-size object)))
          (when (oddp size)
            (incf size))
          (scan-object object cycle-kind)
          (incf *scavenge-general-old-finger* (* size 8))))
     ;; Scavenge cons area.
     (loop
        (when (eql *scavenge-cons-old-finger* *cons-area-old-gen-bump*)
          (return))
        ;; Cons region is just pointers.
        (let ((addr (logior *scavenge-cons-old-finger*
                            (ash +address-tag-cons+ +address-tag-shift+)
                            *old-gen-newspace-bit*
                            +address-old-generation+)))
          (scavengef (memref-t addr 0) cycle-kind)
          (scavengef (memref-t addr 1) cycle-kind))
        (incf *scavenge-cons-old-finger* 16))))

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

(defun find-next-free-object (start limit usage-sym)
  (loop
     (when (>= start limit)
       (return nil))
     (when (not (get-mark-bit start))
       ;; Not marked, must be free.
       (return start))
     (let* ((size-in-words (align-up (size-of-pinned-area-allocation start) 2))
            (size (* size-in-words 8)))
       (incf (symbol-value usage-sym) size-in-words)
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
  (logior (ash +object-tag-freelist-entry+ +object-type-shift+)
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

(defun rebuild-freelist (bins name base limit usage-sym)
  "Sweep the pinned/wired area chain and rebuild the freelist.
Additionally update the card table offset fields and clear the mark bits."
  (gc-log "rebuild freelist " name)
  ;; Reset bins.
  (dotimes (i 64)
    (setf (svref bins i) nil))
  ;; And usage counts.
  (setf (symbol-value usage-sym) 0)
  ;; Build the freelist.
  (let* ((entry-start (find-next-free-object base limit usage-sym))
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
           ((get-mark-bit next-addr)
            ;; Is marked, finish this entry and start the next one.
            (setf current (find-next-free-object next-addr limit usage-sym))
            (when *gc-debug-freelist-rebuild*
              (gc-log "marked, next is " current))
            (finish-freelist-entry bins entry-start entry-len)
            (when (not current)
              (return))
            (setf entry-start current
                  entry-len 0))
           (t
            ;; Advance to the next object
            (setf current next-addr))))))
  ;; Flush mark bits.
  (mezzano.supervisor:release-memory-range
   (+ +mark-bit-region-base+ (truncate base (* +octets-per-mark-bit+ 8)))
   (truncate (- limit base) (* +octets-per-mark-bit+ 8))))

(defun minor-scan-thread (thread)
  (scan-thread thread :minor))

(defun minor-scan-at (address)
  (let ((type (ash (memref-unsigned-byte-8 address 0) (- +object-type-shift+))))
    (case type
      (#.+object-tag-cons+
       ;; Scanning a cons represented as a headered object.
       (scavengef (memref-t address 2) :minor)
       (scavengef (memref-t address 3) :minor)
       32)
      (#.+object-tag-freelist-entry+
       ;; Skip freelist entries.
       (* (ash (memref-unsigned-byte-64 address 0)
               (- +object-data-shift+))
          8))
      (t
       (let ((object (%%assemble-value address +tag-object+)))
         (scan-object object :minor)
         (* (align-up (object-size object) 2) 8))))))

(defun card-table-dirty-p (address)
  (card-table-dirty-gen address))

(defun minor-scan-range (start size)
  (gc-log "minor scan " start "-" (+ start size))
  (let ((end (+ start size))
        (current start))
    (loop
       ;; State one. Looking for a dirty card.
       (loop
          (when (>= current end)
            (return-from minor-scan-range))
          (when (card-table-dirty-p current)
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
          (incf current (minor-scan-at current))
          (when (>= current end)
            (return-from minor-scan-range))
          (when (not (card-table-dirty-p current))
            (return)))
       (setf current (logand current (lognot (1- +card-size+)))))))

(defun minor-scan-cons-range (start size)
  (gc-log "minor cons scan " start "-" (+ start size))
  (loop
     for current from start below (+ start size) by +card-size+
     do
       (cond ((card-table-dirty-p current)
              (gc-log "Hit minor cons card " current)
              (dotimes (i (/ +card-size+ 8))
                (scavengef (memref-t current i) :minor)))
             (t
              #++(gc-log "Skip minor cons card " current)))))

(defun verify-one (address field-address)
  (let ((object (memref-t field-address)))
    (when (immediatep object)
      ;; Don't care about immediate objects, return them unchanged.
      (return-from verify-one object))
    (when (%value-has-tag-p object +tag-instance-header+)
      ;; Instance headers will be returned unchanged, and the pointed to
      ;; layout will be scavenged.
      ;; This assumes that instance header only refer to pinned/wired objects
      ;; and don't need to move.
      (verify-object (mezzano.runtime::%unpack-instance-header object))
      (return-from verify-one object))
    (let ((object-address (object-base-address object)))
      (ecase (ldb (byte +address-tag-size+ +address-tag-shift+) object-address)
        ((#.+address-tag-general+
          #.+address-tag-cons+)
         (when (not (logtest object-address +address-old-generation+))
           (mezzano.supervisor:panic "Object at address " address " contains young gen pointer " object-address " in field " field-address)))
        ((#.+address-tag-pinned+ #.+address-tag-stack+)
         nil)))))

(defun verify-generic (object size)
  "Scavenge SIZE words pointed to by OBJECT."
  (let ((address (object-base-address object)))
    (dotimes (i size)
      (verify-one address (+ address (* i 8))))))

(defun verify-object (object)
  ;; Dispatch again based on the type.
  (case (%object-tag object)
    ((#.+object-tag-array-t+
      #.+object-tag-closure+
      #.+object-tag-symbol-value-cell+)
     ;; simple-vector
     ;; 1+ to account for the header word.
     (verify-generic object (1+ (%object-header-data object))))
    ((#.+object-tag-simple-string+
      #.+object-tag-string+
      #.+object-tag-simple-array+
      #.+object-tag-array+)
     ;; Dimensions don't need to be scanned
     (verify-generic object 4))
    ((#.+object-tag-complex-rational+
      #.+object-tag-ratio+)
     (verify-generic object 3))
    (#.+object-tag-symbol+
     (verify-generic object 6))
    ((#.+object-tag-instance+
      #.+object-tag-funcallable-instance+)
     (let ((direct-layout (%instance-layout object)))
       (cond ((layout-p direct-layout)
              (let* ((layout direct-layout)
                     (heap-layout (layout-heap-layout layout))
                     (heap-size (layout-heap-size layout)))
                (cond ((eql heap-layout 't)
                       ;; All slots boxed
                       (verify-generic object (1+ heap-size)))
                      (heap-layout
                       ;; Bit vector of slot boxedness.
                       ;; TODO: Not implemented.
                       nil))))
             (t ;; Obsolete instance
              ;; Much like a regular instance, but the layout comes from the
              ;; obsolete layout instead of directly from the object.
              (let* ((layout (mezzano.runtime::obsolete-instance-layout-old-layout
                              direct-layout))
                     (heap-layout (layout-heap-layout layout))
                     (heap-size (layout-heap-size layout)))
                (cond ((eql heap-layout 't)
                       ;; All slots boxed
                       (verify-generic object (1+ heap-size)))
                      (heap-layout
                       ;; Bit vector of slot boxedness.
                       ;; TODO: Not implemented.
                       nil)))))))
    (#.+object-tag-function-reference+
     ;; Only the first 4 words. The remaining words are code words.
     (verify-generic object 4))
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
      #.+object-tag-sse-vector+))
    (#.+object-tag-weak-pointer+
     ;; not implemented
     nil)
    (#.+object-tag-delimited-continuation+
     ;; not implemented
     nil)
    (t (scan-error object))))

(defun verify-at (address)
  (let ((type (ash (memref-unsigned-byte-8 address 0) (- +object-type-shift+))))
    (case type
      (#.+object-tag-cons+
       ;; Scanning a cons represented as a headered object.
       (verify-one address (+ address 16))
       (verify-one address (+ address 24))
       32)
      (#.+object-tag-freelist-entry+
       ;; Skip freelist entries.
       (* (ash (memref-unsigned-byte-64 address 0)
               (- +object-data-shift+))
          8))
      (t
       (let ((object (%%assemble-value address +tag-object+)))
         (verify-object object)
         (* (align-up (object-size object) 2) 8))))))

(defun verify-range (start size)
  (loop
     with end = (+ start size)
     with current = start
     until (>= current end)
     do
       (incf current (verify-at current))))

(defun verify-cons-range (start size)
  (loop
     for current from start below (+ start size) by 16
     do
       (verify-one current (+ current 0))
       (verify-one current (+ current 8))))

(defun validate-intergenerational-pointers ()
  (when *gc-debug-validate-intergenerational-pointers*
    ;; Make sure wired, pinned, and old gen don't contain young gen pointers.
    (verify-range *wired-area-base*
                  (- *wired-area-bump* *wired-area-base*))
    (verify-range *pinned-area-base*
                  (- *pinned-area-bump* *pinned-area-base*))
    (verify-range *wired-function-area-limit*
                  (- *function-area-base* *wired-function-area-limit*))
    (verify-range *function-area-base*
                  (- *function-area-limit* *function-area-base*))
    (verify-range (logior (ash +address-tag-general+ +address-tag-shift+)
                          +address-old-generation+
                          *old-gen-newspace-bit*)
                  *general-area-old-gen-bump*)
    (verify-cons-range (logior (ash +address-tag-cons+ +address-tag-shift+)
                               +address-old-generation+
                               *old-gen-newspace-bit*)
                       *cons-area-old-gen-bump*)))

(defun gc-dump-area-state ()
  (mezzano.supervisor:debug-print-line "Wired: " *wired-area-base* " " (- *wired-area-bump* *wired-area-base*))
  (mezzano.supervisor:debug-print-line "Pinned: " *pinned-area-base* " " (- *pinned-area-bump* *pinned-area-base*))
  (mezzano.supervisor:debug-print-line "General young gen: " *general-area-young-gen-bump* " " *general-area-young-gen-limit*)
  (mezzano.supervisor:debug-print-line "General old gen: " *general-area-old-gen-bump* " " *general-area-old-gen-limit*)
  (mezzano.supervisor:debug-print-line "General expansion: " mezzano.runtime::*general-area-expansion-granularity*)
  (mezzano.supervisor:debug-print-line "Cons young gen: " *cons-area-young-gen-bump* " " *cons-area-young-gen-limit*)
  (mezzano.supervisor:debug-print-line "Cons old gen: " *cons-area-old-gen-bump* " " *cons-area-old-gen-limit*)
  (mezzano.supervisor:debug-print-line "Cons expansion: " mezzano.runtime::*cons-area-expansion-granularity*)
  (mezzano.supervisor:debug-print-line "Young gen newspace bit: " *young-gen-newspace-bit*)
  (mezzano.supervisor:debug-print-line "Old gen newspace bit: " *old-gen-newspace-bit*)
  (mezzano.supervisor:debug-print-line "Wired stack bump: " *wired-stack-area-bump* " stack bump: " *stack-area-bump* " total: " *bytes-allocated-to-stacks*)
  (mezzano.supervisor:debug-print-line "Allocation fudge: " mezzano.runtime::*allocation-fudge* " store fudge: " mezzano.supervisor::*store-fudge-factor*)
  (mezzano.supervisor:debug-print-line "Remaining: " (mezzano.runtime::bytes-remaining)))

(defun gc-insufficient-space ()
  (mezzano.supervisor:debug-print-line "Insufficient space for garbage collection!")
  (gc-dump-area-state)
  (mezzano.supervisor:panic "Insufficient space for garbage collection!"))

(defun flush-dirty-card-bits (start size)
  (gc-log "Flush dirty card bits " start " " (+ start size))
  (loop
     for i from start below (+ start size) by +card-size+
     do (setf (card-table-dirty-gen i) nil)))

(defun rearm-gc-write-barrier-common ()
  (flush-dirty-card-bits *wired-area-base* (- *wired-area-bump* *wired-area-base*))
  (flush-dirty-card-bits *pinned-area-base* (- *pinned-area-bump* *pinned-area-base*))
  (mezzano.supervisor:protect-memory-range *pinned-area-base*
                                           (- *pinned-area-bump* *pinned-area-base*)
                                           (logior +block-map-present+
                                                   +block-map-writable+
                                                   +block-map-track-dirty+))
  (flush-dirty-card-bits *wired-function-area-limit*
                         (- *function-area-base* *wired-function-area-limit*))
  (flush-dirty-card-bits *function-area-base* (- *function-area-limit* *function-area-base*))
  (mezzano.supervisor:protect-memory-range *function-area-base*
                                           (- *function-area-limit* *function-area-base*)
                                           (logior +block-map-present+
                                                   +block-map-writable+
                                                   +block-map-track-dirty+)))

(defun gc-minor-cycle ()
  "Collect the young generation."
  (gc-log "Minor GC.")
  (let ((general-bump *general-area-old-gen-bump*)
        (cons-bump *cons-area-old-gen-bump*)
        (general-limit *general-area-old-gen-limit*)
        (cons-limit *cons-area-old-gen-limit*)
        (target-generation (logior *old-gen-newspace-bit*
                                   +address-old-generation+))
        (young-oldspace *young-gen-newspace-bit*)
        (general-total *general-area-young-gen-limit*)
        (cons-total *cons-area-young-gen-limit*))
    ;; Flip young gen's semispace
    (setf *young-gen-newspace-bit* (logxor *young-gen-newspace-bit*
                                           +address-semispace+)
          *young-gen-newspace-bit-raw* (ash *young-gen-newspace-bit* -1))
    ;; Allocate required newspace.
    ;; The total space required in the old generation is the size of the young gen,
    ;; plus what ever is currently in the old generation.
    ;; This allocates the maximum space required up front to avoid complicating transport.
    ;; These should never fail. If they do it is because the remaining space
    ;; calculations in the area expansion functions are wrong.
    (when (not (mezzano.supervisor:allocate-memory-range
                (+ (logior target-generation
                           (ash +address-tag-general+ +address-tag-shift+))
                   general-limit)
                general-total
                (logior +block-map-present+
                        +block-map-writable+
                        +block-map-zero-fill+)))
      (gc-insufficient-space))
    (when (not (mezzano.supervisor:allocate-memory-range
                (+ (logior target-generation
                           (ash +address-tag-cons+ +address-tag-shift+))
                   cons-limit)
                cons-total
                (logior +block-map-present+
                        +block-map-writable+
                        +block-map-zero-fill+)))
      (gc-insufficient-space))
    ;; Temporarily disable dirty bit tracking on the target generation.
    ;; Write barriers are off for the duration of the GC cycle.
    (mezzano.supervisor:protect-memory-range
     (logior target-generation
             (ash +address-tag-general+ +address-tag-shift+))
     general-limit
     (logior +block-map-present+
             +block-map-writable+))
    (mezzano.supervisor:protect-memory-range
     (logior target-generation
             (ash +address-tag-cons+ +address-tag-shift+))
     cons-limit
     (logior +block-map-present+
             +block-map-writable+))
    ;; Scan roots.
    ;; All thread stacks.
    (loop
       for thread = mezzano.supervisor::*all-threads* then (mezzano.supervisor::thread-global-next thread)
       until (null thread)
       do (minor-scan-thread thread))
    ;; The remembered set.
    (mezzano.supervisor:update-wired-dirty-bits) ; Transfer dirty bits from the wired area to the card table.
    (minor-scan-range *wired-area-base* (- *wired-area-bump* *wired-area-base*))
    (minor-scan-range *pinned-area-base* (- *pinned-area-bump* *pinned-area-base*))
    (minor-scan-range *wired-function-area-limit* (- *function-area-base* *wired-function-area-limit*))
    (minor-scan-range *function-area-base* (- *function-area-limit* *function-area-base*))
    (minor-scan-range (logior (ash +address-tag-general+ +address-tag-shift+)
                              target-generation)
                      general-bump)
    (minor-scan-cons-range (logior (ash +address-tag-cons+ +address-tag-shift+)
                                   target-generation)
                           cons-bump)
    ;; Now scan all copied objects. They'll have been copied into
    ;; the old generation and extend from the original bump pointer
    ;; up to the current bump pointer.
    (setf *scavenge-general-old-finger* general-bump
          *scavenge-cons-old-finger* cons-bump)
    (scavenge-dynamic :minor)
    ;; Collection complete.
    (validate-intergenerational-pointers)
    ;; Clear target generation dirty bits, there are no objects in younger generations.
    (rearm-gc-write-barrier-common)
    (flush-dirty-card-bits (logior (ash +address-tag-general+ +address-tag-shift+)
                                   target-generation)
                           general-bump)
    (flush-dirty-card-bits (logior (ash +address-tag-cons+ +address-tag-shift+)
                                   target-generation)
                           cons-bump)
    ;; Trim target down to the bump pointer and tag it for dirty tracking.
    (let ((new-limit (align-up *general-area-old-gen-bump*
                               +allocation-minimum-alignment+))
          (current-limit (+ *general-area-young-gen-limit*
                            *general-area-old-gen-limit*)))
      (mezzano.supervisor:protect-memory-range
       (logior target-generation
               (ash +address-tag-general+ +address-tag-shift+))
       new-limit
       (logior +block-map-present+
               +block-map-writable+
               +block-map-track-dirty+))
      (mezzano.supervisor:release-memory-range
       (logior target-generation
               new-limit
               (ash +address-tag-general+ +address-tag-shift+))
       (- current-limit new-limit))
      (setf *general-area-old-gen-limit* new-limit))
    (let ((new-limit (align-up *cons-area-old-gen-bump*
                               +allocation-minimum-alignment+))
          (current-limit (+ *cons-area-young-gen-limit*
                            *cons-area-old-gen-limit*)))
      (mezzano.supervisor:protect-memory-range
       (logior target-generation
               (ash +address-tag-cons+ +address-tag-shift+))
       new-limit
       (logior +block-map-present+
               +block-map-writable+
               +block-map-track-dirty+))
      (mezzano.supervisor:release-memory-range
       (logior target-generation
               new-limit
               (ash +address-tag-cons+ +address-tag-shift+))
       (- current-limit new-limit))
      (setf *cons-area-old-gen-limit* new-limit))
    ;; Free younger generation.
    (mezzano.supervisor:release-memory-range
     (logior young-oldspace
             (ash +address-tag-general+ +address-tag-shift+))
     *general-area-young-gen-limit*)
    (mezzano.supervisor:release-memory-range
     (logior young-oldspace
             (ash +address-tag-cons+ +address-tag-shift+))
     *cons-area-young-gen-limit*)
    (setf *general-area-young-gen-bump* 0
          *general-area-young-gen-limit* 0
          *cons-area-young-gen-bump* 0
          *cons-area-young-gen-limit* 0)))

(defun allocate-mark-bit-region (base limit)
  (when (not (mezzano.supervisor:allocate-memory-range
              (+ +mark-bit-region-base+ (truncate base (* +octets-per-mark-bit+ 8)))
              (truncate (- limit base) (* +octets-per-mark-bit+ 8))
              (logior +block-map-present+
                      +block-map-writable+
                      +block-map-zero-fill+)))
    (gc-insufficient-space)))

(defun gc-major-cycle ()
  "Collect both generations."
    (gc-log "Major GC.")
  ;; Reset the weak pointer worklist.
  (setf *weak-pointer-worklist* '())
  (let ((young-oldspace *young-gen-newspace-bit*)
        (old-oldspace *old-gen-newspace-bit*)
        (prev-general-limit *general-area-old-gen-limit*)
        (prev-cons-limit *cons-area-old-gen-limit*)
        (maximum-general-limit (+ *general-area-young-gen-limit*
                                  *general-area-old-gen-limit*))
        (maximum-cons-limit (+ *cons-area-young-gen-limit*
                               *cons-area-old-gen-limit*)))
    ;; Flip.
    (setf *young-gen-newspace-bit* (logxor *young-gen-newspace-bit* +address-semispace+)
          *young-gen-newspace-bit-raw* (ash *young-gen-newspace-bit* -1)
          *old-gen-newspace-bit* (logxor *old-gen-newspace-bit* +address-semispace+))
    (gc-log "Gnrl Newspace " (logior *old-gen-newspace-bit* +address-old-generation+ (ash +address-tag-general+ +address-tag-shift+))
            "-" (+ (logior *old-gen-newspace-bit* +address-old-generation+ (ash +address-tag-general+ +address-tag-shift+))
                   maximum-general-limit))
    (gc-log "Cons Newspace " (logior *old-gen-newspace-bit* +address-old-generation+ (ash +address-tag-cons+ +address-tag-shift+))
            "-" (+ (logior *old-gen-newspace-bit* +address-old-generation+ (ash +address-tag-cons+ +address-tag-shift+))
                   maximum-cons-limit))
    (gc-log "Gnrl Oldspace " (logior old-oldspace +address-old-generation+ (ash +address-tag-general+ +address-tag-shift+))
            "-" (+ (logior old-oldspace +address-old-generation+ (ash +address-tag-general+ +address-tag-shift+)) *general-area-old-gen-limit*))
    (gc-log "Cons Oldspace " (logior old-oldspace +address-old-generation+ (ash +address-tag-cons+ +address-tag-shift+))
            "-" (+ (logior old-oldspace +address-old-generation+ (ash +address-tag-cons+ +address-tag-shift+)) *cons-area-old-gen-limit*))
    ;; Allocate newspace.
    ;; This allocates the maximum space required up front to avoid complicating transport.
    (when (not (mezzano.supervisor:allocate-memory-range
                (logior *old-gen-newspace-bit*
                        +address-old-generation+
                        (ash +address-tag-general+ +address-tag-shift+))
                maximum-general-limit
                (logior +block-map-present+
                        +block-map-writable+
                        +block-map-zero-fill+)))
      (gc-insufficient-space))
    (when (not (mezzano.supervisor:allocate-memory-range
                (logior *old-gen-newspace-bit*
                        +address-old-generation+
                        (ash +address-tag-cons+ +address-tag-shift+))
                maximum-cons-limit
                (logior +block-map-present+
                        +block-map-writable+
                        +block-map-zero-fill+)))
      (gc-insufficient-space))
    (setf *general-area-old-gen-bump* 0
          *cons-area-old-gen-bump* 0
          *scavenge-general-old-finger* 0
          *scavenge-cons-old-finger* 0)
    ;; Allocate pinned mark bit regions.
    (allocate-mark-bit-region *wired-area-base* *wired-area-bump*)
    (allocate-mark-bit-region *pinned-area-base* *pinned-area-bump*)
    (allocate-mark-bit-region *wired-function-area-limit* *function-area-base*)
    (allocate-mark-bit-region *function-area-base* *function-area-limit*)
    (gc-log "Scav roots")
    ;; Scavenge NIL to start things off.
    (scavenge-object 'nil :major)
    ;; And various important other roots.
    (scavenge-object (%unbound-value) :major)
    (scavenge-object (%symbol-binding-cache-sentinel) :major)
    ;; Scavenge the current thread's stack.
    (scavenge-current-thread)
    ;; Now do the bulk of the work by scavenging the dynamic areas.
    (scavenge-dynamic :major)
    ;; Weak pointers.
    (update-weak-pointers)
    (finalizer-processing)
    ;; Flush oldspace.
    (mezzano.supervisor:release-memory-range
     (logior young-oldspace
             (ash +address-tag-general+ +address-tag-shift+))
     *general-area-young-gen-limit*)
    (mezzano.supervisor:release-memory-range
     (logior young-oldspace
             (ash +address-tag-cons+ +address-tag-shift+))
     *cons-area-young-gen-limit*)
    (setf *general-area-young-gen-bump* 0
          *general-area-young-gen-limit* 0
          *cons-area-young-gen-bump* 0
          *cons-area-young-gen-limit* 0)
    (mezzano.supervisor:release-memory-range
     (logior old-oldspace
             +address-old-generation+
             (ash +address-tag-general+ +address-tag-shift+))
     prev-general-limit)
    (mezzano.supervisor:release-memory-range
     (logior old-oldspace
             +address-old-generation+
             (ash +address-tag-cons+ +address-tag-shift+))
     prev-cons-limit)
    ;; Trim newspace down.
    (let ((new-limit (align-up *general-area-old-gen-bump*
                               +allocation-minimum-alignment+)))
      (mezzano.supervisor:release-memory-range
       (logior *old-gen-newspace-bit*
               new-limit
               +address-old-generation+
               (ash +address-tag-general+ +address-tag-shift+))
       (- maximum-general-limit new-limit))
      (setf *general-area-old-gen-limit* new-limit))
    (let ((new-limit (align-up *cons-area-old-gen-bump*
                               +allocation-minimum-alignment+)))
      (mezzano.supervisor:release-memory-range
       (logior *old-gen-newspace-bit*
               new-limit
               +address-old-generation+
               (ash +address-tag-cons+ +address-tag-shift+))
       (- maximum-cons-limit new-limit))
      (setf *cons-area-old-gen-limit* new-limit))
    ;; Mark newspace as trackable.
    (mezzano.supervisor:protect-memory-range
     (logior *old-gen-newspace-bit*
             +address-old-generation+
             (ash +address-tag-general+ +address-tag-shift+))
     *general-area-old-gen-limit*
     (logior +block-map-present+
             +block-map-writable+
             +block-map-track-dirty+))
    (mezzano.supervisor:protect-memory-range
     (logior *old-gen-newspace-bit*
             +address-old-generation+
             (ash +address-tag-cons+ +address-tag-shift+))
     *cons-area-old-gen-limit*
     (logior +block-map-present+
             +block-map-writable+
             +block-map-track-dirty+))
    ;; Rebuild freelists.
    (rebuild-freelist *wired-area-free-bins*
                      :wired
                      *wired-area-base* *wired-area-bump*
                      '*wired-area-usage*)
    (rebuild-freelist *pinned-area-free-bins*
                      :pinned
                      *pinned-area-base* *pinned-area-bump*
                      '*pinned-area-usage*)
    (rebuild-freelist *wired-function-area-free-bins*
                      :wired-function
                      *wired-function-area-limit* *function-area-base*
                      '*wired-function-area-usage*)
    (rebuild-freelist *function-area-free-bins*
                      :function
                      *function-area-base* *function-area-limit*
                      '*function-area-usage*)
    (validate-intergenerational-pointers)
    ;; Reset card table.
    ;; Newspace's card table does not need to be cleared as it
    ;; was freshly allocated.
    (mezzano.supervisor:update-wired-dirty-bits)
    (rearm-gc-write-barrier-common)))

(defglobal *gc-major-heap-low-threshold* (* 32 1024 1024)
  "The GC will always do a major cycle if there is is less than this much memory available.")

(defun gc-cycle (force-major)
  (mezzano.supervisor::set-gc-light t)
  (gc-log "GC in progress... " force-major)
  (incf *gc-cycles*)
  (let ((young-gen-size (+ *general-area-young-gen-limit* *cons-area-young-gen-limit*))
        (old-gen-size (+ *general-area-old-gen-limit* *cons-area-old-gen-limit*)))
    (when (and (not force-major)
               ;; Only do the test if the system is actually brushing up against
               ;; the memory limit.
               (< (mezzano.runtime::bytes-remaining) *gc-major-heap-low-threshold*)
               ;; If the young generation is however much larger than the old
               ;; generation it is probably safe to assume that there's plenty of
               ;; garbage available to free.
               (< young-gen-size (* old-gen-size *generation-size-ratio*)))
      (setf force-major t)))
  (when *gc-enable-logging*
    (gc-dump-area-state))
  (cond (force-major
         (incf *gc-major-cycles*)
         (gc-major-cycle))
        (t
         (incf *gc-minor-cycles*)
         (gc-minor-cycle)))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sys.int::%define-type-symbol
   'weak-pointer
   'weak-pointer-p))

(declaim (inline weak-pointer-p))
(defun weak-pointer-p (object)
  (%object-of-type-p object +object-tag-weak-pointer+))

(defun weak-pointer-weakness (weak-pointer)
  "Return the weakness of WEAK-POINTER."
  (check-type weak-pointer weak-pointer)
  (decode-weak-pointer-weakness
   (ldb +weak-pointer-header-weakness+ (%object-header-data weak-pointer))))

(defun weak-pointer-pair (weak-pointer)
  "Returns the key, the value, and T if the key is still live, otherwise NIL, NIL and NIL."
  (check-type weak-pointer weak-pointer)
  ;; Make a strong reference to key & value first.
  ;; They'll be set to NIL if the weak pointer is dead.
  (let ((key (%object-ref-t weak-pointer +weak-pointer-key+))
        (value (%object-ref-t weak-pointer +weak-pointer-value+)))
    ;; Inspect the livep header bit.
    (if (logbitp +weak-pointer-header-livep+ (%object-header-data weak-pointer))
        (values key value t)
        (values nil nil nil))))

(defun weak-pointer-key (weak-pointer)
  "Returns the key and T if the key is still live, otherwise NIL and NIL."
  (check-type weak-pointer weak-pointer)
  ;; Make a strong reference to key first.
  ;; It'll be set to NIL if the weak pointer is dead.
  (let ((key (%object-ref-t weak-pointer +weak-pointer-key+)))
    ;; Inspect the livep header bit.
    (if (logbitp +weak-pointer-header-livep+ (%object-header-data weak-pointer))
        (values key t)
        (values nil nil))))

(defun weak-pointer-value (weak-pointer)
  "Returns the value and T if the key is still live, otherwise NIL and NIL."
  (check-type weak-pointer weak-pointer)
  ;; Make a strong reference to value first.
  ;; It'll be set to NIL if the weak pointer is dead.
  (let ((value (%object-ref-t weak-pointer +weak-pointer-value+)))
    ;; Inspect the livep header bit.
    (if (logbitp +weak-pointer-header-livep+ (%object-header-data weak-pointer))
        (values value t)
        (values nil nil))))

;;; Fast unsafe readers for key/value.
;;; These return NIL when the weak-pointer is dead.
(declaim (inline %weak-pointer-key))
(defun %weak-pointer-key (weak-pointer)
  (%object-ref-t weak-pointer +weak-pointer-key+))

(declaim (inline %weak-pointer-value))
(defun %weak-pointer-value (weak-pointer)
  (%object-ref-t weak-pointer +weak-pointer-value+))

(defun weak-pointer-live-p (weak-pointer)
  "Returns true if the weak pointer is still live."
  (check-type weak-pointer weak-pointer)
  (logbitp +weak-pointer-header-livep+ (%object-header-data weak-pointer)))

(defun examine-weak-pointer-key (key)
  (when (immediatep key)
    ;; Immediate objects are always live.
    (return-from examine-weak-pointer-key
      (values key t)))
  (let* ((address (object-base-address key))
         (address-tag (ldb (byte +address-tag-size+ +address-tag-shift+) address)))
    (ecase address-tag
      ((#.+address-tag-general+
        #.+address-tag-cons+)
       ;; Look for a forwarding pointer.
       (let ((header-byte (memref-unsigned-byte-8 address 0)))
         (cond ((eql (ldb +tag-field+ header-byte) +tag-gc-forward+)
                ;; Object is still live.
                (values (%%assemble-value (object-base-address (memref-t address 0))
                                          (%tag-field key))
                        t))
               ;; Look for a heap number, treat these as live so that
               ;; weak pointers are more useful for arbitrary keys.
               ;; Thinking about EQL caches for GF discriminators.
               ;; This object's header is still intact, it's fine
               ;; to do a type test on it.
               ((and (eql address-tag +address-tag-general+)
                     (numberp key))
                ;; Value is a heap number. Treat it as live.
                (values (transport-object key :major) t))
               (t ;; Object is dead.
                (values nil nil)))))
      (#.+address-tag-pinned+
       (cond ((get-mark-bit (if (consp key)
                                (- address 16)
                                address))
              ;; Object is live.
              (values key t))
             (t
              (values nil nil)))))))

(defun update-one-weak-pointer (weak-pointer)
  (ecase (weak-pointer-weakness weak-pointer)
    (:key
     (multiple-value-bind (new-key livep)
         (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-key+))
       (when livep
         (setf (%object-ref-t weak-pointer +weak-pointer-key+) new-key)
         ;; Keep the value live.
         (scavengef (%object-ref-t weak-pointer +weak-pointer-value+) :major)
         t)))
    (:value
     (multiple-value-bind (new-value livep)
         (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-value+))
       (when livep
         (setf (%object-ref-t weak-pointer +weak-pointer-value+) new-value)
         ;; Keep the key live.
         (scavengef (%object-ref-t weak-pointer +weak-pointer-key+) :major)
         t)))
    (:key-and-value
     (multiple-value-bind (new-key key-livep)
         (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-key+))
       (multiple-value-bind (new-value value-livep)
           (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-value+))
         ;; Key and value must be live.
         (when (and key-livep value-livep)
           (setf (%object-ref-t weak-pointer +weak-pointer-key+) new-key
                 (%object-ref-t weak-pointer +weak-pointer-value+) new-value)
           t))))
    (:key-or-value
     (multiple-value-bind (new-key key-livep)
         (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-key+))
       (multiple-value-bind (new-value value-livep)
           (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-value+))
         ;; Either key or value must be live.
         (cond ((and key-livep value-livep)
                ;; Both live, need to update both and scav neither.
                (setf (%object-ref-t weak-pointer +weak-pointer-key+) new-key
                      (%object-ref-t weak-pointer +weak-pointer-value+) new-value)
                t)
               (key-livep
                ;; Key is live, scav value.
                (setf (%object-ref-t weak-pointer +weak-pointer-key+) new-key)
                (scavengef (%object-ref-t weak-pointer +weak-pointer-value+) :major)
                t)
               (key-livep
                ;; Value is live, scav key.
                (setf (%object-ref-t weak-pointer +weak-pointer-value+) new-value)
                (scavengef (%object-ref-t weak-pointer +weak-pointer-key+) :major)
                t)
               (t
                ;; Neither live. Weak pointer appears dead.
                nil)))))))

(defun check-weak-pointer-dead (weak-pointer)
  (ecase (weak-pointer-weakness weak-pointer)
    (:key
     (multiple-value-bind (new-key livep)
         (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-key+))
       (declare (ignore new-key))
       (when livep
         (mezzano.supervisor:panic "Weak pointer key live?"))))
    (:value
     (multiple-value-bind (new-value livep)
         (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-value+))
       (declare (ignore new-value))
       (when livep
         (mezzano.supervisor:panic "Weak pointer key live?"))))
    (:key-and-value
     ;; Both key and value must be live.
     (multiple-value-bind (new-key key-livep)
         (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-key+))
       (declare (ignore new-key))
       (multiple-value-bind (new-value value-livep)
           (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-value+))
         (declare (ignore new-value))
         (when (and key-livep value-livep)
           (mezzano.supervisor:panic "Weak pointer key live?")))))
    (:key-or-value
     ;; Either key or value should be live.
     (multiple-value-bind (new-key key-livep)
         (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-key+))
       (declare (ignore new-key))
       (multiple-value-bind (new-value value-livep)
           (examine-weak-pointer-key (%object-ref-t weak-pointer +weak-pointer-value+))
         (declare (ignore new-value))
         (when (or key-livep value-livep)
           (mezzano.supervisor:panic "Weak pointer key live?")))))))

(defun update-weak-pointers ()
  (loop
     with did-something = nil
     do
       (do* ((new-worklist nil)
             (iter *weak-pointer-worklist*))
            ((not iter)
             (setf *weak-pointer-worklist* new-worklist))
         (let ((weak-pointer iter))
           (when (not (weak-pointer-p weak-pointer))
             (mezzano.supervisor:panic
              "Invalid object " weak-pointer " on weak pointer worklist (1)"))
           (setf iter (%object-ref-t weak-pointer +weak-pointer-link+))
           (cond ((update-one-weak-pointer weak-pointer)
                  ;; Weak pointer is live. Drop from the worklist.
                  (setf did-something t))
                 (t
                  ;; Key is either dead or not scanned yet. Add to the new worklist.
                  (setf (%object-ref-t weak-pointer +weak-pointer-link+) new-worklist
                        new-worklist weak-pointer)))))
     ;; Stop when no more memory needs to be scavenged.
       (when (not did-something)
         (return))
       (setf did-something nil)
       (scavenge-dynamic :major))
  ;; Final pass, no more memory will be scanned.
  ;; All weak pointers left on the worklist should be dead.
  (do ((weak-pointer *weak-pointer-worklist*))
      ((not weak-pointer))
    (when (not (weak-pointer-p weak-pointer))
      (mezzano.supervisor:panic
       "Invalid object " weak-pointer " on weak pointer worklist (2)"))
    (check-weak-pointer-dead weak-pointer)
    (setf (%object-ref-t weak-pointer +weak-pointer-key+) nil
          (%object-ref-t weak-pointer +weak-pointer-value+) nil)
    ;; Clear the live bit, leaving the other fields alone.
    (setf (%object-header-data weak-pointer)
          (logand (%object-header-data weak-pointer)
                  (lognot (ash 1 +weak-pointer-header-livep+))))
    (setf weak-pointer (%object-ref-t weak-pointer +weak-pointer-link+))))

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
              when (or (not f)
                       (eql (cas (symbol-global-value '*pending-finalizers*)
                                 f
                                 (%object-ref-t f +weak-pointer-finalizer-link+))
                            f))
              do (return f))))
    (loop
       for finalizer = (pop-finalizer)
       until (not finalizer)
       do
         (funcall (%object-ref-t finalizer +weak-pointer-finalizer+))
         ;; Leave the weak pointer completely empty.
         ;; No references to any other object.
         (setf (%object-ref-t finalizer +weak-pointer-finalizer+) nil))))
