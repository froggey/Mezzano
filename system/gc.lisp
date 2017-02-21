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
(defglobal *gc-debug-metadata* nil)

(defglobal *gc-enable-logging* nil)

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
(defglobal *dynamic-mark-bit* 0)
;; State of the object header mark bit, used for pinned objects.
(defglobal *pinned-mark-bit* 0)

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
(defvar *gc-thread* (mezzano.supervisor:make-thread #'gc-worker :name "Garbage Collector" :stack-size (* 1024 1024)))

(defun gc ()
  "Run a garbage-collection cycle."
  (mezzano.supervisor:with-mutex (*gc-lock*)
    (let ((epoch *gc-epoch*))
      (setf *gc-requested* t)
      (mezzano.supervisor:condition-notify *gc-cvar* t)
      (loop
         (when (not (eql epoch *gc-epoch*))
           (return))
         (mezzano.supervisor:condition-wait *gc-cvar* *gc-lock*)))))

(defun gc-worker ()
  (loop
     (mezzano.supervisor:with-mutex (*gc-lock*)
       ;; Notify any waiting threads that the GC epoch has changed.
       (mezzano.supervisor:condition-notify *gc-cvar* t)
       ;; Wait for a GC request.
       (loop
          (when *gc-requested*
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
              (gc-cycle)
              (let* ((gc-end (get-internal-run-time))
                     (total-time (- gc-end gc-start))
                     (total-seconds (/ total-time (float internal-time-units-per-second))))
                (gc-log "GC took " (truncate (* total-seconds 1000)) "ms")
                (incf *gc-time* total-seconds)))
         (setf *gc-in-progress* nil)))
     ;; TODO: catch & report errors.
     (run-finalizers)))

(declaim (inline immediatep))
(defun immediatep (object)
  "Return true if OBJECT is an immediate object."
  (or (fixnump object)
      (%value-has-tag-p object +tag-character+)
      (%value-has-tag-p object +tag-single-float+)
      (%value-has-tag-p object +tag-byte-specifier+)))

(defmacro scavengef (place &environment env)
  "Scavenge PLACE. Only update PLACE if the scavenged value is different.
This is required to make the GC interrupt safe."
  (multiple-value-bind (vars vals stores setter getter)
      (get-setf-expansion place env)
    (let ((orig (gensym "ORIG")))
      `(let* (,@(mapcar #'list vars vals)
              (,orig ,getter)
              (,(car stores) (scavenge-object ,orig)))
           (when (not (eq ,orig ,(car stores)))
             ,setter)))))

(defun scavenge-many (address n)
  (dotimes (i n)
    (scavengef (memref-t address i))))

;;; This only scavenges the stack/registers. Scavenging the actual
;;; thread object is done by scan-thread.
(defun scavenge-current-thread ()
  ;; Grovel around in the current stack frame to grab needed stuff.
  (let* ((frame-pointer (read-frame-pointer))
         (return-address (memref-unsigned-byte-64 frame-pointer 1))
         (stack-pointer (+ frame-pointer 16)))
    (scan-thread (mezzano.supervisor:current-thread))
    (gc-log "Scav GC stack")
    (scavenge-stack stack-pointer
                    (memref-unsigned-byte-64 frame-pointer 0)
                    return-address)))

(defun scavenge-object (object)
  "Scavenge one object, returning an updated pointer."
  (when (immediatep object)
    ;; Don't care about immediate objects, return them unchanged.
    (return-from scavenge-object object))
  (let ((address (ash (%pointer-field object) 4)))
    (ecase (ldb (byte +address-tag-size+ +address-tag-shift+) address)
      (#.+address-tag-general+
       (when (eql (logand (ash 1 +address-newspace/oldspace-bit+) address) *dynamic-mark-bit*)
         (return-from scavenge-object object))
       (transport-object object))
      (#.+address-tag-cons+
       (when (eql (logand (ash 1 +address-newspace/oldspace-bit+) address) *dynamic-mark-bit*)
         (return-from scavenge-object object))
       (transport-object object))
      (#.+address-tag-pinned+
       (mark-pinned-object object)
       object)
      (#.+address-tag-stack+
       ;; Don't scavenge DX objects, handled by scavenge-regular-stack-frame.
       object))))

(defun scan-error (object)
  (mezzano.supervisor:panic "Unscannable object " object))

(defun scan-generic (object size)
  "Scavenge SIZE words pointed to by OBJECT."
  (scavenge-many (ash (%pointer-field object) 4) size))

(defun scavenge-stack-n-incoming-arguments (frame-pointer stack-pointer framep
                                            layout-length n-args)
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
        (scavenge-many (+ frame-pointer 16) n-values)
        ;; Skip any layout values.
        (scavenge-many (+ stack-pointer (* layout-length 8))
                       n-values))))

(defun scavenge-regular-stack-frame (frame-pointer stack-pointer framep
                                     layout-address layout-length
                                     incoming-arguments pushed-values)
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
                          ;; Don't scan it if it's below the stack pointer. This can
                          ;; happen when a thread is interrupted during a non-local exit.
                          ;; The exit may cause a DX object's scope to be exited, which
                          ;; requires the DX pointer to be cleared. If the thread is
                          ;; interrupted before the pointer can be cleared, this happens.
                          (when (>= (lisp-object-address value) stack-pointer)
                            (gc-log
                             "Scav DX root " (lisp-object-address value))
                            (scan-object (%%assemble-value (ash (%pointer-field value) 4)
                                                           +tag-object+))))
                         ;; Normal object. Don't do anything interesting.
                         (t (scavengef (memref-t base offset)))))))
          (cond (framep
                 (scav-one frame-pointer (- -1 slot)))
                (t
                 (scav-one stack-pointer slot)))))))
  (dotimes (slot pushed-values)
    (when *gc-debug-scavenge-stack*
      (gc-log "Scav pv " slot))
    (scavengef (memref-t stack-pointer slot)))
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
         (memref-t stack-pointer incoming-arguments)))))

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

(defun scavenge-stack (stack-pointer frame-pointer return-address)
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
       (scavenge-object fn)
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
             (when interruptp
               (bad-metadata ":INTERRUPT not defined or implemented."))
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
               (bad-metadata "Tried to unwind through function with no available return address"))))
         (scavenge-regular-stack-frame frame-pointer stack-pointer framep
                                       layout-address layout-length
                                       incoming-arguments pushed-values)
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
                (setf stack-pointer (+ stack-pointer (* layout-length 8)))))))))

(defun scavenge-thread-data-registers (thread)
  (scavengef (mezzano.supervisor:thread-state-r8-value thread))
  (scavengef (mezzano.supervisor:thread-state-r9-value thread))
  (scavengef (mezzano.supervisor:thread-state-r10-value thread))
  (scavengef (mezzano.supervisor:thread-state-r11-value thread))
  (scavengef (mezzano.supervisor:thread-state-r12-value thread))
  (scavengef (mezzano.supervisor:thread-state-r13-value thread))
  (scavengef (mezzano.supervisor:thread-state-rbx-value thread)))

(defun scavenge-full-save-thread (thread)
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
    ;; Unconditionally scavenge the saved data registers.
    (scavenge-thread-data-registers thread)
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
          (when interruptp
            (bad-metadata ":INTERRUPT not defined or implemented."))
          (when (and (not framep)
                     (not (eql pushed-values 0)))
            (bad-metadata "Non-zero :PUSHED-VALUES is incompatible with :NO-FRAME. Use :LAYOUT."))
          (when (and (not framep)
                     pushed-values-register)
            (bad-metadata ":PUSHED-VALUES-REGISTER is incompatible with :NO-FRAME."))
          ;; arm64 uses a link register
          #-arm64
          (when (and (not framep)
                     (eql layout-length 0))
            (bad-metadata "Tried to unwind through function with no available return address"))
          ;; Not all settings are valid in arm64.
          #+arm64
          (when (or (not (eql extra-registers nil))
                    (not (eql extra-registers :rax)))
            (bad-metadata ":EXTRA-REGISTERS has undefined setting"))))
      #+x86-64
      (ecase extra-registers
        ((nil))
        ((:rax)
         (scavengef (mezzano.supervisor:thread-state-rax-value thread)))
        ((:rax-rcx)
         (scavengef (mezzano.supervisor:thread-state-rax-value thread))
         (scavengef (mezzano.supervisor:thread-state-rcx-value thread)))
        ((:rax-rcx-rdx)
         (scavengef (mezzano.supervisor:thread-state-rax-value thread))
         (scavengef (mezzano.supervisor:thread-state-rcx-value thread))
         (scavengef (mezzano.supervisor:thread-state-rdx-value thread))))
      #+arm64
      (ecase extra-registers
        ((nil))
        ((:rax)
         ;; x9 (rax) contains an interior pointer into :x1 (r9)
         (let ((offset (- (mezzano.supervisor:thread-state-rax thread)
                          (mezzano.supervisor:thread-state-r9 thread))))
           (scavengef (mezzano.supervisor:thread-state-r9 thread))
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
                         n-mv-area-values)))
      (when (eql incoming-arguments :rcx)
        ;; Prevent SCAVENGE-REGULAR-STACK-FRAME from seeing :RCX in incoming-arguments.
        (setf incoming-arguments nil)
        (when *gc-debug-scavenge-stack*
          (gc-log "ia-count " (mezzano.supervisor:thread-state-rcx-value thread)))
        (scavenge-stack-n-incoming-arguments
         frame-pointer stack-pointer framep
         layout-length
         (mezzano.supervisor::thread-state-rcx-value thread)))
      (when restart
        (setf (mezzano.supervisor:thread-state-rip thread) (+ fn-address entry-offset)))
      (scavenge-regular-stack-frame frame-pointer stack-pointer framep
                                    layout-address layout-length
                                    incoming-arguments
                                    (+ pushed-values
                                       (if pushed-values-register
                                           (mezzano.supervisor:thread-state-rcx-value thread)
                                           0)))
      (cond #+arm64
            ((and (not framep)
                  (eql layout-length 0))
             ;; Special case: lr contains the return address and there is return address on the stack.
             (scavenge-stack
              ;; Stack pointer needs the return address popped off,
              ;; and any layout variables.
              stack-pointer
              ;; Frame pointer should be unchanged.
              frame-pointer
              ;; Return address in x30.
              (mezzano.supervisor:thread-state-cs thread)))
            ((not framep)
             ;; No frame, carefully pick out the new values.
             (scavenge-stack
              ;; Stack pointer needs the return address popped off,
              ;; and any layout variables.
              (+ stack-pointer (* layout-length 8))
              ;; Frame pointer should be unchanged.
              frame-pointer
              ;; Return address should be above the layout variables.
              (memref-unsigned-byte-64 stack-pointer (1- layout-length))))
            ((not (zerop frame-pointer))
             (scavenge-stack (+ frame-pointer 16) ; sp
                             (memref-unsigned-byte-64 frame-pointer 0) ; fp
                             (memref-unsigned-byte-64 frame-pointer 1))) ; ra
            (t (when *gc-debug-scavenge-stack*
                 (gc-log "Done scav stack.")))))))

(defun scan-thread (object)
  (when *gc-debug-scavenge-stack* (gc-log "Scav thread " object))
  ;; Scavenge various parts of the thread.
  (scavengef (mezzano.supervisor:thread-name object))
  (scavengef (mezzano.supervisor:thread-state object))
  (scavengef (mezzano.supervisor:thread-lock object))
  ;; FIXME: Mark stack.
  (scavengef (mezzano.supervisor:thread-stack object))
  (scavengef (mezzano.supervisor:thread-special-stack-pointer object))
  (scavengef (mezzano.supervisor:thread-wait-item object))
  (scavengef (mezzano.supervisor:thread-%next object))
  (scavengef (mezzano.supervisor:thread-%prev object))
  (scavengef (mezzano.supervisor:thread-pending-footholds object))
  (scavengef (mezzano.supervisor:thread-mutex-stack object))
  (scavengef (mezzano.supervisor:thread-global-next object))
  (scavengef (mezzano.supervisor:thread-global-prev object))
  (scavengef (mezzano.supervisor:thread-priority object))
  (scavengef (mezzano.supervisor:thread-pager-argument-1 object))
  (scavengef (mezzano.supervisor:thread-pager-argument-2 object))
  (scavengef (mezzano.supervisor:thread-pager-argument-3 object))
  ;; Only scan the thread's stack and MV area when it's alive.
  (case (mezzano.supervisor:thread-state object)
    (:dead) ; Nothing.
    (0
     ;; This is a partially-initialized thread.
     ;; It has nothing on the stack that needs to be scanned, but its
     ;; data registers may contain live references.
     (scavenge-thread-data-registers object))
    (t
     (when (not (or (eql object (mezzano.supervisor:current-thread))
                    ;; Don't even think about looking at the stacks of these
                    ;; threads. They may run at any time, even with the world
                    ;; stopped.
                    ;; Things aren't so bad though, they (should) only contain
                    ;; pointers to wired objects, and the objects they do point
                    ;; to should be pointed to by other live objects.
                    (eql object sys.int::*bsp-idle-thread*)
                    (eql object sys.int::*pager-thread*)
                    (eql object sys.int::*disk-io-thread*)
                    (eql object sys.int::*snapshot-thread*)))
       (cond ((mezzano.supervisor:thread-full-save-p object)
              (scavenge-full-save-thread object))
             (t (let* ((stack-pointer (mezzano.supervisor:thread-stack-pointer object))
                       (frame-pointer (mezzano.supervisor:thread-frame-pointer object))
                       (return-address (memref-unsigned-byte-64 stack-pointer
                                                                #-arm64 0
                                                                #+arm64 1)))
                  (scavenge-stack stack-pointer frame-pointer return-address))))))))

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

(defun scan-object-1 (object)
  ;; Dispatch again based on the type.
  (case (%object-tag object)
    ((#.+object-tag-array-t+
      #.+object-tag-closure+
      #.+object-tag-funcallable-instance+)
     ;; simple-vector
     ;; 1+ to account for the header word.
     (scan-generic object (1+ (%object-header-data object))))
    ((#.+object-tag-simple-string+
      #.+object-tag-string+
      #.+object-tag-simple-array+
      #.+object-tag-array+)
     ;; Dimensions don't need to be scanned
     (scan-generic object 4))
    ((#.+object-tag-complex-rational+
      #.+object-tag-ratio+)
     (scan-generic object 3))
    (#.+object-tag-symbol+
     (scan-generic object 8))
    (#.+object-tag-structure-object+
     (scan-generic object (1+ (%object-header-data object))))
    (#.+object-tag-std-instance+
     (scan-generic object 4))
    (#.+object-tag-function-reference+
     (scan-generic object 4))
    (#.+object-tag-function+
     (scan-function object))
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
      #.+object-tag-array-xmm-vector+
      #.+object-tag-bignum+
      #.+object-tag-double-float+
      #.+object-tag-short-float+
      #.+object-tag-long-float+
      ;; not complex-rational or ratio, they may hold other numbers.
      #.+object-tag-complex-single-float+
      #.+object-tag-complex-double-float+
      #.+object-tag-complex-short-float+
      #.+object-tag-complex-long-float+
      #.+object-tag-xmm-vector+
      #.+object-tag-unbound-value+))
    (#.+object-tag-thread+
     (scan-thread object))
    (#.+object-tag-weak-pointer+
     ;; Add to the worklist, only when previously live.
     (when (logbitp +weak-pointer-header-livep+ (%object-header-data object))
       (setf (%object-ref-t object +weak-pointer-link+) *weak-pointer-worklist*
             *weak-pointer-worklist* object))
     (scavengef (%object-ref-t object +weak-pointer-finalizer-link+))
     (scavengef (%object-ref-t object +weak-pointer-finalizer+)))
    (t (scan-error object))))

(defun scan-function (object)
  ;; Scan the constant pool.
  (let* ((address (ash (%pointer-field object) 4))
         (mc-size (* (memref-unsigned-byte-16 address 1) 16))
         (pool-size (memref-unsigned-byte-16 address 2)))
    (scavenge-many (+ address mc-size) pool-size)))

(defun scan-object (object)
  "Scan one object, updating pointer fields."
  (cond
    ((%value-has-tag-p object +tag-cons+)
     (scan-generic object 2))
    ((%value-has-tag-p object +tag-object+)
     (scan-object-1 object))
    (t (scan-error object))))

(defun transport-error (object)
  (mezzano.supervisor:panic "Untransportable object " object))

(defun transport-object (object)
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
    (really-transport-object object)))

(defun really-transport-object (object)
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
             (incf *old-objects-copied*))))
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
           #.+object-tag-array-complex-double-float+
           #.+object-tag-array-xmm-vector+)
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
         (#.+object-tag-xmm-vector+
          4)
         (#.+object-tag-symbol+
          8)
         (#.+object-tag-std-instance+
          4)
         (#.+object-tag-function-reference+
          4)
         (#.+object-tag-function+
          ;; The size of a function is the sum of the MC, the GC info and the constant pool.
          (ceiling (+ (* (ldb (byte 16 8) length) 16)  ; mc size
                      (* (ldb (byte 16 24) length) 8)  ; pool size
                      (ldb (byte 16 40) length)) ; gc-info size.
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
          6))))))

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
             (scan-object object)))
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
               (scan-object object))))))

#+(or)(defun sweep-stacks ()
  (gc-log "sweeping stacks")
  (do* ((reversed-result nil)
        (last-free nil)
        (current *gc-stack-ranges* next)
        (next (cdr current) (cdr current)))
       ((endp current)
        ;; Reverse the result list.
        (do ((result nil)
             (i reversed-result))
            ((endp i)
             (setf *gc-stack-ranges* result))
          (psetf i (cdr i)
                 (cdr i) result
                 result i)))
    (cond ((gc-stack-range-marked (first current))
           ;; This one is allocated & still in use.
           (assert (gc-stack-range-allocated (first current)))
           (setf (rest current) reversed-result
                 reversed-result current))
          ((and last-free
                (eql (gc-stack-range-end last-free)
                     (gc-stack-range-start (first current))))
           ;; Free and can be merged.
           (setf (gc-stack-range-end last-free)
                 (gc-stack-range-end (first current))))
          (t ;; Free, but no last-free.
           (setf last-free (first current)
                 (gc-stack-range-allocated (first current)) nil
                 (rest current) reversed-result
                 reversed-result current)))))

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
          (scan-object object)
          (incf *scavenge-general-finger* (* size 8))))
     ;; Scavenge cons area.
     (loop
        (when (eql *scavenge-cons-finger* *cons-area-bump*)
          (return))
        ;; Cons region is just pointers.
        (let ((addr (logior *scavenge-cons-finger*
                            (ash +address-tag-cons+ +address-tag-shift+)
                            *dynamic-mark-bit*)))
          (scavengef (memref-t addr 0))
          (scavengef (memref-t addr 1)))
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

(defun find-next-free-object (start limit)
  (loop
     (when (>= start limit)
       (return nil))
     (when (not (eql (logand (memref-unsigned-byte-8 start 0) +pinned-object-mark-bit+)
                     *pinned-mark-bit*))
       ;; Not marked, must be free.
       (return start))
     (incf start (* (align-up (size-of-pinned-area-allocation start) 2) 8))))

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
    (setf (svref bins bin) start)))

(defun rebuild-freelist (bins name base limit)
  "Sweep the pinned/wired area chain and rebuild the freelist."
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

(defun gc-cycle ()
  (mezzano.supervisor::set-gc-light t)
  (gc-log "GC in progress...")
  (incf *gc-cycles*)
  ;; Reset the weak pointer worklist.
  (setf *weak-pointer-worklist* '())
  ;; Flip.
  (psetf *dynamic-mark-bit* (logxor *dynamic-mark-bit* (ash 1 +address-newspace/oldspace-bit+))
         *pinned-mark-bit* (logxor *pinned-mark-bit* +pinned-object-mark-bit+))
  (setf *general-area-bump* 0
        *cons-area-bump* 0
        *scavenge-general-finger* 0
        *scavenge-cons-finger* 0)
  ;; Unprotect newspace.
  (mezzano.supervisor:protect-memory-range (logior *dynamic-mark-bit*
                                                   (ash +address-tag-general+ +address-tag-shift+))
                                           *general-area-limit*
                                           (logior +block-map-present+
                                                   +block-map-writable+
                                                   +block-map-zero-fill+))
  (mezzano.supervisor:protect-memory-range (logior *dynamic-mark-bit*
                                                   (ash +address-tag-cons+ +address-tag-shift+))
                                           *cons-area-limit*
                                           (logior +block-map-present+
                                                   +block-map-writable+
                                                   +block-map-zero-fill+))
  (gc-log "Scav roots")
  ;; Scavenge NIL to start things off.
  (scavenge-object 'nil)
  ;; And various important other roots.
  (scavenge-object (%unbound-value))
  (scavenge-object (%undefined-function))
  (scavenge-object (%closure-trampoline))
  (scavenge-object (%funcallable-instance-trampoline))
  ;; Scavenge the current thread's stack.
  (scavenge-current-thread)
  ;; Now do the bulk of the work by scavenging the dynamic areas.
  (scavenge-dynamic)
  ;; Weak pointers.
  (update-weak-pointers)
  (finalizer-processing)
  ;; Inhibit access to oldspace.
  (mezzano.supervisor:protect-memory-range (logior (logxor *dynamic-mark-bit* (ash 1 +address-newspace/oldspace-bit+))
                                                   (ash +address-tag-general+ +address-tag-shift+))
                                           *general-area-limit*
                                           +block-map-zero-fill+)
  (mezzano.supervisor:protect-memory-range (logior (logxor *dynamic-mark-bit* (ash 1 +address-newspace/oldspace-bit+))
                                                   (ash +address-tag-cons+ +address-tag-shift+))
                                           *cons-area-limit*
                                           +block-map-zero-fill+)
  ;; Rebuild freelists.
  (rebuild-freelist *wired-area-free-bins* :wired (* 2 1024 1024) *wired-area-bump*)
  (rebuild-freelist *pinned-area-free-bins* :pinned (* 2 1024 1024 1024) *pinned-area-bump*)
  ;; Trim the dynamic areas.
  (let ((new-limit (align-up *general-area-bump* #x200000)))
    (mezzano.supervisor:release-memory-range (logior new-limit
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             (- *general-area-limit* new-limit))
    (mezzano.supervisor:release-memory-range (logior (ash 1 +address-newspace/oldspace-bit+)
                                                     new-limit
                                                     (ash +address-tag-general+ +address-tag-shift+))
                                             (- *general-area-limit* new-limit))
    (setf *general-area-limit* new-limit))
  (let ((new-limit (align-up *cons-area-bump* #x200000)))
    (mezzano.supervisor:release-memory-range (logior new-limit
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             (- *cons-area-limit* new-limit))
    (mezzano.supervisor:release-memory-range (logior (ash 1 +address-newspace/oldspace-bit+)
                                                     new-limit
                                                     (ash +address-tag-cons+ +address-tag-shift+))
                                             (- *cons-area-limit* new-limit))
    (setf *cons-area-limit* new-limit))
  (setf *gc-last-general-address* (logior (ash +address-tag-general+ +address-tag-shift+)
                                          *general-area-bump*
                                          *dynamic-mark-bit*)
        *gc-last-cons-address* (logior (ash +address-tag-cons+ +address-tag-shift+)
                                       *cons-area-bump*
                                       *dynamic-mark-bit*))
  (incf *gc-epoch*)
  (gc-log "GC complete")
  (mezzano.supervisor::set-gc-light nil))

(defun base-address-of-internal-pointer (address)
  "Find the base address of the object pointed to be ADDRESS.
Address should be an internal pointer to a live object in static space.
No type information will be provided."
  (flet ((search (start limit)
           (let ((offset start))
             (loop
                (when (>= offset limit)
                  (return))
                (let* ((type (ash (memref-unsigned-byte-8 offset 0) (- +object-type-shift+)))
                       (size (size-of-pinned-area-allocation offset)))
                  (when (and (not (eql type +object-tag-freelist-entry+))
                             (<= offset address (+ offset (* size 8) -1)))
                    (return-from base-address-of-internal-pointer
                      offset))
                  (incf offset (* (align-up size 2) 8)))))))
    ;; Search wired area.
    (search (* 2 1024 1024) *wired-area-bump*)
    ;; Search pinned area.
    (search (* 2 1024 1024 1024) *pinned-area-bump*)))

(deftype weak-pointer ()
  '(satisfies weak-pointer-p))

(defun weak-pointer-p (object)
  (%object-of-type-p object +object-tag-weak-pointer+))

(defun weak-pointer-value (object)
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
                    (scavengef (%object-ref-t weak-pointer +weak-pointer-value+)))
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
