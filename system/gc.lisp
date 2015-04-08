;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defvar *gc-debug-scavenge-stack* nil)
(defvar *gc-debug-freelist-rebuild* nil)
(defvar *gc-debug-metadata* t)

;;; GC Meters.
(defvar *objects-copied* 0)
(defvar *words-copied* 0)

(defvar *gc-in-progress* nil)

;; State of the dynamic pointer mark bit. This is part of the pointer, not part
;; of the object itself.
(defvar *dynamic-mark-bit* 0)
;; State of the object header mark bit, used for pinned objects.
(defvar *pinned-mark-bit* 0)

;; How many bytes the allocators can expand their areas by before a GC must occur.
;; This is shared between all areas.
(defvar *memory-expansion-remaining* 0)

(defvar *gc-epoch* 0)

(defun room (&optional (verbosity :default))
  (let ((total-used 0)
        (total 0))
    (format t "General area: ~:D/~:D words used (~D%).~%"
            (truncate *general-area-bump* 8) (truncate *general-area-limit* 8)
            (truncate (* *general-area-bump* 100) *general-area-limit*))
    (incf total-used (truncate *general-area-bump* 8))
    (incf total (truncate *general-area-limit* 8))
    (format t "Cons area: ~:D/~:D words used (~D%).~%"
            (truncate *cons-area-bump* 8) (truncate *cons-area-limit* 8)
            (truncate (* *cons-area-bump* 100) *cons-area-limit*))
    (incf total-used (truncate *cons-area-bump* 8))
    (incf total (truncate *cons-area-limit* 8))
    (multiple-value-bind (allocated-words total-words largest-free-space)
        (pinned-area-info (* 2 1024 1024) *wired-area-bump*)
      (format t "Wired area: ~:D/~:D words allocated (~D%).~%"
              allocated-words total-words
              (truncate (* allocated-words 100) total-words))
      (format t "  Largest free area: ~:D words.~%" largest-free-space)
      (incf total-used allocated-words)
      (incf total total-words))
    (multiple-value-bind (allocated-words total-words largest-free-space)
        (pinned-area-info (* 2 1024 1024 1024) *pinned-area-bump*)
      (format t "Pinned area: ~:D/~:D words allocated (~D%).~%"
              allocated-words total-words
              (truncate (* allocated-words 100) total-words))
      (format t "  Largest free area: ~:D words.~%" largest-free-space)
      (incf total-used allocated-words)
      (incf total total-words))
    (format t "Total ~:D/~:D words used (~D%).~%"
            total-used total
            (truncate (* total-used 100) total))
    (multiple-value-bind (n-free-blocks total-blocks)
        (mezzano.supervisor:store-statistics)
      (format t "~:D/~:D store blocks used (~D%).~%"
              (- total-blocks n-free-blocks) total-blocks
              (truncate (* (- total-blocks n-free-blocks) 100) total-blocks)))
    (multiple-value-bind (n-free-page-frames total-page-frames)
        (mezzano.supervisor:physical-memory-statistics)
      (format t "~:D/~:D physical pages used (~D%).~%"
              (- total-page-frames n-free-page-frames) total-page-frames
              (truncate (* (- total-page-frames n-free-page-frames) 100)
                        total-page-frames)))
    (format t "~:D words to next GC.~%" (truncate *memory-expansion-remaining* 8)))
  (values))

(defun pinned-area-info (base limit)
  ;; Yup. Scanning a pinned area atomically requires the world to be stopped.
  (mezzano.supervisor:with-world-stopped
    (let ((allocated-words 0)
          (total-words 0)
          (offset 0)
          (largest-free-space 0))
      (loop
         (when (>= (+ base (* offset 8)) limit)
           (return))
         (let ((size (align-up (size-of-pinned-area-allocation (+ base (* offset 8))) 2))
               ;; Carefully read the type, avoid bignums.
               (type (ldb (byte +object-type-size+ +object-type-shift+)
                          (memref-unsigned-byte-8 base offset))))
           (incf total-words size)
           (cond ((not (eql type +object-tag-freelist-entry+))
                  (incf allocated-words size))
                 (t ; free block.
                  (setf largest-free-space (max largest-free-space size))))
           (incf offset size)))
      (values allocated-words total-words largest-free-space))))

(defun gc ()
  "Run a garbage-collection cycle."
  (when *gc-in-progress*
    (error "Nested GC?!"))
  (mezzano.supervisor:with-world-stopped
    ;; Set *GC-IN-PROGRESS* globally, not with a binding.
    (unwind-protect
         (progn
           (setf *gc-in-progress* t)
           (gc-cycle))
      (setf *gc-in-progress* nil))))

(declaim (inline immediatep))
(defun immediatep (object)
  "Return true if OBJECT is an immediate object."
  (case (%tag-field object)
    ((#.+tag-fixnum-000+ #.+tag-fixnum-001+
      #.+tag-fixnum-010+ #.+tag-fixnum-011+
      #.+tag-fixnum-100+ #.+tag-fixnum-101+
      #.+tag-fixnum-110+ #.+tag-fixnum-111+
      #.+tag-character+ #.+tag-single-float+)
     t)
    (t nil)))

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
    (mezzano.supervisor:debug-print-line "Scav GC stack")
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
      (mezzano.supervisor:debug-print-line
       "  n-args " n-args
       "  n-values " n-values
       "  from " (if framep
                     (+ frame-pointer 16)
                     (+ stack-pointer (* (1+ layout-length) 8)))))
    ;; There are N-VALUES values above the return address.
    (if framep
        ;; Skip saved fp and return address.
        (scavenge-many (+ frame-pointer 16) n-values)
        ;; Skip return address and any layout values.
        (scavenge-many (+ stack-pointer (* (1+ layout-length) 8)) n-values))))

(defun scavenge-regular-stack-frame (frame-pointer stack-pointer framep
                                     layout-address layout-length
                                     incoming-arguments pushed-values)
  ;; Scan stack slots.
  (dotimes (slot layout-length)
    (multiple-value-bind (offset bit)
        (truncate slot 8)
      (when *gc-debug-scavenge-stack*
        (mezzano.supervisor:debug-print-line
         "ss: " slot " " offset ":" bit "  " (memref-unsigned-byte-8 layout-address offset)))
      (when (logbitp bit (memref-unsigned-byte-8 layout-address offset))
        (flet ((scav-one (base offset)
                 (let ((value (memref-t base offset)))
                   (when *gc-debug-scavenge-stack*
                     (mezzano.supervisor:debug-print-line
                      "Scav stack slot " offset
                      "  " (lisp-object-address value)))
                   (cond ((eql (%tag-field value) +tag-dx-root-object+)
                          ;; DX root, convert it to a normal object pointer and scan.
                          ;; Don't scan it if it's below the stack pointer. This can
                          ;; happen when a thread is interrupted during a non-local exit.
                          ;; The exit may cause a DX object's scope to be exited, which
                          ;; requires the DX pointer to be cleared. If the thread is
                          ;; interrupted before the pointer can be cleared, this happens.
                          (when (>= (lisp-object-address value) stack-pointer)
                            (mezzano.supervisor:debug-print-line
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
      (mezzano.supervisor:debug-print-line "Scav pv " slot))
    (scavengef (memref-t stack-pointer slot)))
  ;; Scan incoming arguments.
  (when incoming-arguments
    ;; Stored as fixnum on the stack.
    (when *gc-debug-scavenge-stack*
      (mezzano.supervisor:debug-print-line "IA in slot " (- -1 incoming-arguments)))
    (scavenge-stack-n-incoming-arguments
     frame-pointer stack-pointer framep
     layout-length
     (if framep
         (memref-t frame-pointer (- -1 incoming-arguments))
         (memref-t stack-pointer incoming-arguments)))))

(defun debug-stack-frame (framep interruptp pushed-values pushed-values-register
                          layout-address layout-length
                          multiple-values incoming-arguments
                          block-or-tagbody-thunk extra-registers)
  (when *gc-debug-scavenge-stack*
    (if framep
        (mezzano.supervisor:debug-print-line "frame")
        (mezzano.supervisor:debug-print-line "no-frame"))
    (if interruptp
        (mezzano.supervisor:debug-print-line "interrupt")
        (mezzano.supervisor:debug-print-line "no-interrupt"))
    (mezzano.supervisor:debug-print-line "pv: " pushed-values)
    (mezzano.supervisor:debug-print-line "pvr: " pushed-values-register)
    (if multiple-values
        (mezzano.supervisor:debug-print-line "mv: " multiple-values)
        (mezzano.supervisor:debug-print-line "no-multiple-values"))
    (mezzano.supervisor:debug-print-line "Layout addr: " layout-address)
    (mezzano.supervisor:debug-print-line "  Layout len: " layout-length)
    (cond (incoming-arguments
           (mezzano.supervisor:debug-print-line "ia: " incoming-arguments))
          (t (mezzano.supervisor:debug-print-line "no-incoming-arguments")))
    (if block-or-tagbody-thunk
        (mezzano.supervisor:debug-print-line "btt: " block-or-tagbody-thunk)
        (mezzano.supervisor:debug-print-line "no-btt"))
    (if extra-registers
        (mezzano.supervisor:debug-print-line "xr: " extra-registers)
        (mezzano.supervisor:debug-print-line "no-xr"))))

(defun scavenge-stack (stack-pointer frame-pointer return-address)
  (when *gc-debug-scavenge-stack* (mezzano.supervisor:debug-print-line "Scav stack..."))
  (loop
     (when *gc-debug-scavenge-stack*
       (mezzano.supervisor:debug-print-line "SP: " stack-pointer)
       (mezzano.supervisor:debug-print-line "FP: " frame-pointer)
       (mezzano.supervisor:debug-print-line "RA: " return-address))
     (when (zerop return-address)
       (when *gc-debug-scavenge-stack* (mezzano.supervisor:debug-print-line "Done scav stack."))
       (return))
     (let* ((fn (return-address-to-function return-address))
            (fn-address (logand (lisp-object-address fn) -16))
            (fn-offset (- return-address fn-address)))
       (when *gc-debug-scavenge-stack*
         (mezzano.supervisor:debug-print-line "fn: " fn-address)
         (mezzano.supervisor:debug-print-line "fnoffs: " fn-offset))
       (scavenge-object fn)
       (multiple-value-bind (framep interruptp pushed-values pushed-values-register
                                    layout-address layout-length
                                    multiple-values incoming-arguments
                                    block-or-tagbody-thunk extra-registers)
           (gc-info-for-function-offset fn fn-offset)
         (when *gc-debug-metadata*
           (flet ((bad-metadata (message)
                    (let ((*gc-debug-scavenge-stack* t))
                      (mezzano.supervisor:debug-print-line "RA: " return-address)
                      (mezzano.supervisor:debug-print-line "FP: " frame-pointer)
                      (mezzano.supervisor:debug-print-line "SP: " stack-pointer)
                      (mezzano.supervisor:debug-print-line "FNa: " fn-address)
                      (mezzano.supervisor:debug-print-line "FNo: " fn-offset)
                      (debug-stack-frame framep interruptp pushed-values pushed-values-register
                                         layout-address layout-length
                                         multiple-values incoming-arguments
                                         block-or-tagbody-thunk extra-registers))
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
               (bad-metadata ":EXTRA-REGISTERS seen outside full-save'd function."))))
         (scavenge-regular-stack-frame frame-pointer stack-pointer framep
                                       layout-address layout-length
                                       incoming-arguments pushed-values)
         ;; Stop after seeing a zerop frame pointer.
         (when (eql frame-pointer 0)
           (when *gc-debug-scavenge-stack* (mezzano.supervisor:debug-print-line "Done scav stack."))
           (return))
         (when (not framep) ; ???
           (mezzano.supervisor:panic "No frame, but no end in sight?"))
         (psetf return-address (memref-unsigned-byte-64 frame-pointer 1)
                stack-pointer (+ frame-pointer 16)
                frame-pointer (memref-unsigned-byte-64 frame-pointer 0))))))

(defun scavenge-full-save-thread (thread)
  ;; Thread has stopped due to an interrupt.
  ;; Examine it, then perform normal stack scavenging.
  (when *gc-debug-scavenge-stack* (mezzano.supervisor:debug-print-line "Scav full-save thread..."))
  (let* ((return-address (mezzano.supervisor:thread-state-rip thread))
         (frame-pointer (mezzano.supervisor:thread-frame-pointer thread))
         (stack-pointer (mezzano.supervisor:thread-stack-pointer thread))
         (fn (return-address-to-function return-address))
         (fn-address (logand (lisp-object-address fn) -16))
         (fn-offset (- return-address fn-address)))
    (when *gc-debug-scavenge-stack*
      (mezzano.supervisor:debug-print-line "RA: " return-address)
      (mezzano.supervisor:debug-print-line "FP: " frame-pointer)
      (mezzano.supervisor:debug-print-line "SP: " stack-pointer)
      (mezzano.supervisor:debug-print-line "FNa: " fn-address)
      (mezzano.supervisor:debug-print-line "FNo: " fn-offset))
    ;; Unconditionally scavenge the saved data registers.
    (scavengef (mezzano.supervisor:thread-state-r8-value thread))
    (scavengef (mezzano.supervisor:thread-state-r9-value thread))
    (scavengef (mezzano.supervisor:thread-state-r10-value thread))
    (scavengef (mezzano.supervisor:thread-state-r11-value thread))
    (scavengef (mezzano.supervisor:thread-state-r12-value thread))
    (scavengef (mezzano.supervisor:thread-state-r13-value thread))
    (scavengef (mezzano.supervisor:thread-state-rbx-value thread))
    (multiple-value-bind (framep interruptp pushed-values pushed-values-register
                          layout-address layout-length
                          multiple-values incoming-arguments
                          block-or-tagbody-thunk extra-registers)
        (gc-info-for-function-offset fn fn-offset)
      (when *gc-debug-metadata*
        (flet ((bad-metadata (message)
                 (let ((*gc-debug-scavenge-stack* t))
                   (mezzano.supervisor:debug-print-line "RA: " return-address)
                   (mezzano.supervisor:debug-print-line "FP: " frame-pointer)
                   (mezzano.supervisor:debug-print-line "SP: " stack-pointer)
                   (mezzano.supervisor:debug-print-line "FNa: " fn-address)
                   (mezzano.supervisor:debug-print-line "FNo: " fn-offset)
                   (debug-stack-frame framep interruptp pushed-values pushed-values-register
                                      layout-address layout-length
                                      multiple-values incoming-arguments
                                      block-or-tagbody-thunk extra-registers))
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
            (bad-metadata ":PUSHED-VALUES-REGISTER is incompatible with :NO-FRAME."))))
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
          (scavenge-many (+ address 8 (* mezzano.supervisor::+thread-mv-slots-start+ 8))
                         (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+))))
      (when (eql incoming-arguments :rcx)
        ;; Prevent SCAVENGE-REGULAR-STACK-FRAME from seeing :RCX in incoming-arguments.
        (setf incoming-arguments nil)
        (when *gc-debug-scavenge-stack*
          (mezzano.supervisor:debug-print-line "ia-count " (mezzano.supervisor:thread-state-rcx-value thread)))
        (scavenge-stack-n-incoming-arguments
         frame-pointer stack-pointer framep
         layout-length
         (mezzano.supervisor::thread-state-rcx-value thread)))
      (scavenge-regular-stack-frame frame-pointer stack-pointer framep
                                    layout-address layout-length
                                    incoming-arguments
                                    (+ pushed-values
                                       (if pushed-values-register
                                           (mezzano.supervisor:thread-state-rcx-value thread)
                                           0)))
      (cond ((not framep)
             ;; No frame, carefully pick out the new values.
             (scavenge-stack
              ;; Stack pointer needs the return address popped off,
              ;; and any layout variables.
              (+ stack-pointer (* (1+ layout-length) 8))
              ;; Frame pointer should be unchanged.
              frame-pointer
              ;; Return address should be above the layout variables.
              (memref-unsigned-byte-64 stack-pointer layout-length)))
            ((not (zerop frame-pointer))
             (scavenge-stack (+ frame-pointer 16) ; sp
                             (memref-unsigned-byte-64 frame-pointer 0) ; fp
                             (memref-unsigned-byte-64 frame-pointer 1))) ; ra
            (t (when *gc-debug-scavenge-stack*
                 (mezzano.supervisor:debug-print-line "Done scav stack.")))))))

(defun scan-thread (object)
  (when *gc-debug-scavenge-stack* (mezzano.supervisor:debug-print-line "Scav thread " object))
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
  ;; Only scan the thread's stack, MV area & TLS area when it's alive.
  (when (not (eql (mezzano.supervisor:thread-state object) :dead))
    (let ((address (ash (%pointer-field object) 4)))
      ;; Unconditonally scavenge the TLS area.
      (scavenge-many (+ address 8 (* mezzano.supervisor::+thread-tls-slots-start+ 8))
                     (- mezzano.supervisor::+thread-tls-slots-end+ mezzano.supervisor::+thread-tls-slots-start+))
      (when (not (or (eql object (mezzano.supervisor:current-thread))
                     ;; Don't even think about looking at the stacks of these threads. They may run at
                     ;; any time, even with the world stopped.
                     ;; Things aren't so bad though, they (should) only contain pointers to wired objects,
                     ;; and the objects they do point to should be pointed to by other live objects.
                     (eql object sys.int::*bsp-idle-thread*)
                     (eql object sys.int::*pager-thread*)
                     (eql object sys.int::*disk-io-thread*)
                     (eql object sys.int::*snapshot-thread*)))
        (cond ((mezzano.supervisor:thread-full-save-p object)
               (scavenge-full-save-thread object))
              (t (let* ((stack-pointer (mezzano.supervisor:thread-stack-pointer object))
                        (frame-pointer (mezzano.supervisor:thread-frame-pointer object))
                        (return-address (memref-unsigned-byte-64 stack-pointer 0)))
                   (scavenge-stack stack-pointer frame-pointer return-address))))))))

(defun gc-info-for-function-offset (function offset)
  (multiple-value-bind (info-address length)
      (function-gc-info function)
    (let ((position 0)
          ;; Defaults.
          (framep nil)
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
          (extra-registers nil))
      (flet ((consume ()
               ;; Read one byte from the GC metadata.
               (when (>= position length)
                 (mezzano.supervisor:panic "Reached end of GC Info??"))
               (prog1 (memref-unsigned-byte-8 info-address position)
                 (incf position))))
        (declare (dynamic-extent #'consume))
        (loop
           (when (>= position length)
             ;; No more metadata entries past here.
             (return))
           (let ((address 0))
             ;; Read first byte of address, this is where we can terminate.
             (let ((byte (consume))
                   (offset 0))
               (setf address (ldb (byte 7 0) byte)
                     offset 7)
               (when (logtest byte #x80)
                 ;; Read remaining bytes.
                 (loop (let ((byte (consume)))
                         (setf (ldb (byte 7 offset) address)
                               (ldb (byte 7 0) byte))
                         (incf offset 7)
                         (unless (logtest byte #x80)
                           (return))))))
             (when (< offset address)
               ;; This metadata entry is past the offset, return the previous values.
               (return))
             ;; Read flag/pvr byte & mv-and-iabtt.
             (let ((flags-and-pvr (consume))
                   (mv-and-iabtt (consume)))
               (setf framep (logtest flags-and-pvr #b0001))
               (setf interruptp (logtest flags-and-pvr #b0010))
               (setf pushed-values-register (if (logtest flags-and-pvr #b10000)
                                                :rcx
                                                nil))
               (setf extra-registers (case (ldb (byte 2 6) flags-and-pvr)
                                       (0 nil)
                                       (1 :rax)
                                       (2 :rax-rcx)
                                       (3 :rax-rcx-rdx)))
               (if (eql (ldb (byte 4 0) mv-and-iabtt) 15)
                   (setf multiple-values nil)
                   (setf multiple-values (ldb (byte 4 0) mv-and-iabtt)))
               (setf block-or-tagbody-thunk nil
                     incoming-arguments nil)
               (when (logtest flags-and-pvr #b0100)
                 (setf block-or-tagbody-thunk :rax))
               (when (logtest flags-and-pvr #b1000)
                 (setf incoming-arguments (if (eql (ldb (byte 4 4) mv-and-iabtt) 15)
                                              :rcx
                                              (ldb (byte 4 4) mv-and-iabtt)))))
             ;; Read vs32 pv.
             (let ((shift 0)
                   (value 0))
               (loop
                  (let ((b (consume)))
                    (when (not (logtest b #x80))
                      (setf value (logior value (ash (logand b #x3F) shift)))
                      (when (logtest b #x40)
                        (setf value (- value)))
                      (return))
                    (setf value (logior value (ash (logand b #x7F) shift)))
                    (incf shift 7)))
               (setf pushed-values value))
             ;; Read vu32 n-layout bits.
             (let ((shift 0)
                   (value 0))
               (loop
                  (let ((b (consume)))
                    (setf value (logior value (ash (logand b #x7F) shift)))
                    (when (not (logtest b #x80))
                      (return))
                    (incf shift 7)))
               (setf layout-length value)
               (setf layout-address (+ info-address position))
               ;; Consume layout bits.
               (incf position (ceiling layout-length 8))))))
      (debug-stack-frame framep interruptp pushed-values pushed-values-register
                         layout-address layout-length
                         multiple-values incoming-arguments
                         block-or-tagbody-thunk extra-registers)
      (values framep interruptp pushed-values pushed-values-register
              layout-address layout-length multiple-values
              incoming-arguments block-or-tagbody-thunk
              extra-registers))))

(defun scan-object-1 (object)
  ;; Careful here. Functions with lots of GC info can have the header fall
  ;; into bignumness when read as a ub64.
  (let* ((address (ash (%pointer-field object) 4))
         (type (ldb (byte +object-type-size+ +object-type-shift+)
                    (memref-unsigned-byte-8 address 0))))
    ;; Dispatch again based on the type.
    (case type
      (#.+object-tag-array-t+
       ;; simple-vector
       ;; 1+ to account for the header word.
       (scan-generic object (1+ (ldb (byte +object-data-size+ +object-data-shift+)
                                     (memref-unsigned-byte-64 address 0)))))
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
       (scan-generic object 6))
      (#.+object-tag-structure-object+
       (scan-generic object (1+ (ldb (byte +object-data-size+ +object-data-shift+)
                                     (memref-unsigned-byte-64 address 0)))))
      (#.+object-tag-std-instance+
       (scan-generic object 3))
      (#.+object-tag-function-reference+
       (scan-generic object 4))
      ((#.+object-tag-function+
        #.+object-tag-closure+
        #.+object-tag-funcallable-instance+)
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
      (t (scan-error object)))))

(defun scan-function (object)
  ;; Scan the constant pool.
  (let* ((address (ash (%pointer-field object) 4))
         (mc-size (* (memref-unsigned-byte-16 address 1) 16))
         (pool-size (memref-unsigned-byte-16 address 2)))
    (scavenge-many (+ address mc-size) pool-size)))

(defun scan-object (object)
  "Scan one object, updating pointer fields."
  (case (%tag-field object)
    (#.+tag-cons+
     (scan-generic object 2))
    (#.+tag-object+
     (scan-object-1 object))
    (t (scan-error object))))

(defun transport-error (object)
  (mezzano.supervisor:panic "Untransportable object " object))

(defun transport-object (object)
  "Transport LENGTH words from oldspace to newspace, returning
a pointer to the new object. Leaves a forwarding pointer in place."
  (let* ((length nil)
         (address (ash (%pointer-field object) 4))
         (first-word (memref-t address 0))
         (new-address nil))
    ;; Check for a GC forwarding pointer.
    ;; Do this before getting the length.
    (when (eql (%tag-field first-word) +tag-gc-forward+)
      (return-from transport-object
        (%%assemble-value (ash (%pointer-field first-word) 4)
                          (%tag-field object))))
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
           (incf *cons-area-bump* (* length 8)))
          (t
           (setf new-address (logior (ash +address-tag-general+ +address-tag-shift+)
                                     *general-area-bump*
                                     *dynamic-mark-bit*))
           (incf *general-area-bump* (* length 8))
           (when (oddp length)
             (setf (memref-t new-address length) 0)
             (incf *general-area-bump* 8))))
    ;; Energize!
    (%fast-copy new-address address (* length 8))
    ;; Leave a forwarding pointer.
    (setf (memref-t address 0) (%%assemble-value new-address +tag-gc-forward+))
    ;; Complete! Return the new object
    (%%assemble-value new-address (%tag-field object))))

(defun object-size (object)
  (case (%tag-field object)
    ;; FIXME? conses are 4 words when not in the cons area.
    (#.+tag-cons+ 2)
    (#.+tag-object+
     (let ((length (%object-header-data object)))
       ;; Dispatch again based on the type.
       (case (%object-tag object)
         ((#.+object-tag-array-t+
           #.+object-tag-array-fixnum+
           #.+object-tag-structure-object+)
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
          6)
         (#.+object-tag-std-instance+
          3)
         (#.+object-tag-function-reference+
          4)
         ((#.+object-tag-function+
           #.+object-tag-closure+
           #.+object-tag-funcallable-instance+)
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
          512))))))

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
               (mezzano.supervisor:debug-print-line
                "Marking freelist entry " object))
             (when (not (eql (logand (memref-unsigned-byte-64 address 0)
                                          +pinned-object-mark-bit+)
                                  *pinned-mark-bit*))
               ;; Not marked, mark it.
               (setf (memref-unsigned-byte-64 address 0) (logior (logand (memref-unsigned-byte-64 address 0)
                                                                         (lognot +pinned-object-mark-bit+))
                                                                 *pinned-mark-bit*))
               ;; And scan.
               (scan-object object))))))

#+(or)(defun sweep-stacks ()
  (mezzano.supervisor:debug-print-line "sweeping stacks")
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

(defun scavenge-dynamic ()
  (let ((general-finger 0)
        (cons-finger 0))
    (loop
       (mezzano.supervisor:debug-print-line
        "General. Limit: " *general-area-limit*
        "  Bump: " *general-area-bump*
        "  Curr: " general-finger)
       (mezzano.supervisor:debug-print-line
        "Cons.    Limit: " *cons-area-limit*
        "  Bump: " *cons-area-bump*
        "  Curr: " cons-finger)
       ;; Stop when both area sets have been fully scavenged.
       (when (and (eql general-finger *general-area-bump*)
                  (eql cons-finger *cons-area-bump*))
         (return))
       (mezzano.supervisor:debug-print-line "Scav main seq")
       ;; Scavenge general area.
       (loop
          (when (eql general-finger *general-area-bump*)
            (return))
          (let* ((object (%%assemble-value (logior general-finger
                                                   (ash +address-tag-general+ +address-tag-shift+)
                                                   *dynamic-mark-bit*)
                                           +tag-object+))
                 (size (object-size object)))
            (when (oddp size)
              (incf size))
            (scan-object object)
            (incf general-finger (* size 8))))
       ;; Scavenge cons area.
       (loop
          (when (eql cons-finger *cons-area-bump*)
            (return))
          ;; Cons region is just pointers.
          (let ((addr (logior cons-finger
                              (ash +address-tag-cons+ +address-tag-shift+)
                              *dynamic-mark-bit*)))
            (scavengef (memref-t addr 0))
            (scavengef (memref-t addr 1)))
          (incf cons-finger 16)))))

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
  (logior *pinned-mark-bit*
          (ash +object-tag-freelist-entry+ +object-type-shift+)
          (ash (align-up len 2) +object-data-shift+)))

(defun rebuild-freelist (freelist-symbol base limit)
  "Sweep the pinned/wired area chain and rebuild the freelist."
  (mezzano.supervisor:debug-print-line "rebuild freelist " freelist-symbol)
  ;; Set initial freelist entry.
  (let ((initial (find-next-free-object base limit)))
    (when (not initial)
      (setf (symbol-value freelist-symbol) '())
      (when *gc-debug-freelist-rebuild*
        (mezzano.supervisor:debug-print-line "done (empty)"))
      (return-from rebuild-freelist))
    (when *gc-debug-freelist-rebuild*
      (mezzano.supervisor:debug-print-line "initial: " initial))
    (setf (memref-unsigned-byte-64 initial 0) (make-freelist-header (size-of-pinned-area-allocation initial))
          (memref-t initial 1) '()
          (symbol-value freelist-symbol) initial))
  ;; Build the freelist.
  (let ((current (symbol-value freelist-symbol)))
    (loop
       ;; Expand this entry as much as possible.
       (let* ((len (ash (memref-unsigned-byte-64 current 0) (- +object-data-shift+)))
              (next-addr (+ current (* len 8))))
         (when *gc-debug-freelist-rebuild*
           (mezzano.supervisor:debug-print-line "len: " len "  next: " next-addr))
         (when (>= next-addr limit)
           (when *gc-debug-freelist-rebuild*
             (mezzano.supervisor:debug-print-line "done (limit)"))
           (when mezzano.runtime::*paranoid-allocation*
             (dotimes (i (- len 2))
               (setf (memref-signed-byte-64 current (+ i 2)) -1)))
           (return))
         ;; Test the mark bit.
         (cond ((eql (logand (memref-unsigned-byte-8 next-addr 0) +pinned-object-mark-bit+)
                     *pinned-mark-bit*)
                ;; Is marked, finish this entry and start the next one.
                (setf next-addr (find-next-free-object current limit))
                (when (not next-addr)
                  (when *gc-debug-freelist-rebuild*
                    (mezzano.supervisor:debug-print-line "done"))
                  (when mezzano.runtime::*paranoid-allocation*
                    (dotimes (i (- len 2))
                      (setf (memref-signed-byte-64 current (+ i 2)) -1)))
                  (return))
                (when *gc-debug-freelist-rebuild*
                  (mezzano.supervisor:debug-print-line "adv: " next-addr))
                (setf (memref-unsigned-byte-64 next-addr 0) (make-freelist-header (size-of-pinned-area-allocation next-addr))
                      (memref-t next-addr 1) '())
                (setf (memref-t current 1) next-addr
                      current next-addr))
               (t ;; Not marked, expand to cover this entry.
                (setf (memref-unsigned-byte-64 current 0) (make-freelist-header (+ len (size-of-pinned-area-allocation next-addr))))))))))

(defun gc-cycle ()
  (mezzano.supervisor::set-gc-light t)
  (mezzano.supervisor:debug-print-line "GC in progress...")
  ;; Clear per-cycle meters
  (setf *objects-copied* 0
        *words-copied* 0)
  ;; Flip.
  (psetf *dynamic-mark-bit* (logxor *dynamic-mark-bit* (ash 1 +address-newspace/oldspace-bit+))
         *pinned-mark-bit* (logxor *pinned-mark-bit* +pinned-object-mark-bit+))
  (setf *general-area-bump* 0
        *cons-area-bump* 0)
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
  (mezzano.supervisor:debug-print-line "Scav roots")
  ;; Scavenge NIL to start things off.
  (scavenge-object 'nil)
  ;; And various important other roots.
  (scavenge-object (%unbound-value))
  (scavenge-object (%unbound-tls-slot))
  (scavenge-object (%undefined-function))
  (scavenge-object (%closure-trampoline))
  ;; Scavenge the current thread's stack.
  (scavenge-current-thread)
  ;; Now do the bulk of the work by scavenging the dynamic areas.
  ;; No scavenging can take place after this.
  (scavenge-dynamic)
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
  (rebuild-freelist '*wired-area-freelist* (* 2 1024 1024) *wired-area-bump*)
  (rebuild-freelist '*pinned-area-freelist* (* 2 1024 1024 1024) *pinned-area-bump*)
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
  (multiple-value-bind (n-free-blocks total-blocks)
      (mezzano.supervisor:store-statistics)
    (declare (ignore total-blocks))
    ;; Always leave about 1MB worth of blocks free, need to update the block map & freelist.
    (setf *memory-expansion-remaining* (logand (* (- n-free-blocks 256) #x1000) (lognot #x3FFFFF)))
    (mezzano.supervisor:debug-print-line "Set *M-E-R* to " *memory-expansion-remaining* " (" n-free-blocks " blocks remain)"))
  (incf *gc-epoch*)
  (mezzano.supervisor:debug-print-line "GC complete")
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
