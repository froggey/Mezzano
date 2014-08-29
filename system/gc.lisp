(in-package :sys.int)

(defvar *gc-debug-scavenge-stack* nil)

;;; GC Meters.
(defvar *objects-copied* 0)
(defvar *words-copied* 0)

;;; Current GC generation, this is set to a new object every time addresses flip.
;;; This will eventually be used to implement eq/eql hash-tables.
(defvar *gc-generation* nil)

(defvar *gc-in-progress* nil)

;; Set in a dynamic value's pointer after the GC has marked through it.
(defvar *dynamic-mark-bit* 0)
;; Set in a pinned object's header after the GC has scanned it.
(defvar *pinned-mark-bit* 0)

(defconstant +marked-through-bit+ (expt 2 44))

(defun room (&optional (verbosity :default))
  (dolist (extent mezzanine.supervisor::*extent-table*)
    (format t "~A~%" extent))
  (values))

(defun gc ()
  "Run a garbage-collection cycle."
  (when *gc-in-progress*
    (error "Nested GC?!"))
  (mezzanine.supervisor::with-world-stopped
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
    (let ((orig (gensym)))
      `(let* (,@(mapcar #'list vars vals)
              (,orig ,getter)
              (,(car stores) (scavenge-object ,orig)))
       (when (not (eq ,orig ,(car stores)))
         ,setter)))))

(defun scavenge-many (address n)
  (dotimes (i n)
    (scavengef (memref-t address i))))

;;; This only scavenges the stack/registers. Scavenging the actual
;;; stack-group object is done by scan-stack-group, assuming the
;;; current stack-group is actually reachable.
(defun scavenge-current-thread ()
  ;; Grovel around in the current stack frame to grab needed stuff.
  (let* ((frame-pointer (read-frame-pointer))
         (return-address (memref-unsigned-byte-64 frame-pointer 1))
         (stack-pointer (+ frame-pointer 16)))
    ;; Unconditonally scavenge the TLS area and the binding stack.
    (mumble "Scav GC stack")
    (scavenge-stack stack-pointer
                    (memref-unsigned-byte-64 frame-pointer 0)
                    return-address
                    nil)))

(defun scavenge-object (object)
  "Scavenge one object, returning an updated pointer."
  (when (immediatep object)
    ;; Don't care about immediate objects, return them unchanged.
    (return-from scavenge-object object))
  (let ((address (ash (%pointer-field object) 4)))
    (ecase (ldb (byte 3 45) address)
      (1 ;; dyn/dyn-c/nursery
       (when (eql (logand +marked-through-bit+ address) *mark-bit*)
         (return-from scavenge-object object))
       (transport-object object))
      (2 ;; pinned
       (mark-pinned-object object)
       object)
      (3 ;; stack
       ;; TODO: Track scanned stack objects. Allocate a cons with dynamic-extent
       ;; and push on some symbol.
       ;; Alternatively, could use the address marked through bit, and make the
       ;; compiler set it correctly when it creates DX objects.
       (scan-object object)
       object))))

(defun scan-error (object)
  (mumble-hex (lisp-object-address object))
  (mumble " ")
  (mumble-hex (memref-unsigned-byte-64 (ash (%pointer-field object) 4) 0))
  (emergency-halt "unscannable object"))

(defun scan-generic (object size)
  "Scavenge SIZE words pointed to by OBJECT."
  (scavenge-many (ash (%pointer-field object) 4) size))

(defun scavenge-stack-n-incoming-arguments (frame-pointer stack-pointer framep
                                            layout-length n-args)
  (let ((n-values (max 0 (- n-args 5))))
    (when *gc-debug-scavenge-stack*
      (mumble-hex n-args "  n-args ")
      (mumble-hex n-values "  n-values ")
      (if framep
          (mumble-hex (+ frame-pointer 16) "  from " t)
          (mumble-hex (+ stack-pointer (* (1+ layout-length) 8)) "  from " t)))
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
        (mumble-hex slot "ss: ")
        (mumble-hex offset " ")
        (mumble-hex bit ":")
        (mumble-hex (memref-unsigned-byte-8 layout-address offset) "  " t))
      (when (logbitp bit (memref-unsigned-byte-8 layout-address offset))
        (cond (framep
               (when *gc-debug-scavenge-stack*
                 (mumble-hex (- -1 slot) "Scav stack slot ")
                 (mumble-hex (lisp-object-address (memref-t frame-pointer (- -1 slot))) "  " t))
               (scavengef (memref-t frame-pointer (- -1 slot))))
              (t
               (when *gc-debug-scavenge-stack*
                 (mumble-hex slot "Scav no-frame stack slot ")
                 (mumble-hex (lisp-object-address (memref-t stack-pointer slot)) "  " t))
               (scavengef (memref-t stack-pointer slot)))))))
  (dotimes (slot pushed-values)
    (when *gc-debug-scavenge-stack*
      (mumble-hex slot "Scav pv "))
    (scavengef (memref-t stack-pointer slot)))
  ;; Scan incoming arguments.
  (when incoming-arguments
    ;; Stored as fixnum on the stack.
    (when *gc-debug-scavenge-stack*
      (mumble-hex (- -1 incoming-arguments) "IA in slot "))
    (scavenge-stack-n-incoming-arguments
     frame-pointer stack-pointer framep
     layout-length
     (if framep
         (memref-t frame-pointer (- -1 incoming-arguments))
         (memref-t stack-pointer incoming-arguments)))))

(defun debug-stack-frame (framep interruptp pushed-values pushed-values-register
                          layout-address layout-length
                          multiple-values incoming-arguments block-or-tagbody-thunk)
  (when *gc-debug-scavenge-stack*
    (if framep
        (mumble "frame")
        (mumble "no-frame"))
    (if interruptp
        (mumble "interrupt")
        (mumble "no-interrupt"))
    (mumble-hex pushed-values "pv: " t)
    (mumble-hex (lisp-object-address pushed-values-register) "pvr: " t)
    (if multiple-values
        (mumble-hex multiple-values "mv: " t)
        (mumble "no-multiple-values"))
    (mumble-hex layout-address "Layout addr: ")
    (mumble-hex layout-length "  Layout len: " t)
    (cond ((integerp incoming-arguments)
           (mumble-hex incoming-arguments "ia: " t))
          (incoming-arguments
           (mumble-hex (lisp-object-address incoming-arguments) "ia: " t))
          (t (mumble "no-incoming-arguments")))
    (if block-or-tagbody-thunk
        (mumble-hex (lisp-object-address block-or-tagbody-thunk) "btt: " t)
        (mumble "no-btt"))))

(defun scavenge-stack (stack-pointer frame-pointer return-address sg-interruptedp)
  (when *gc-debug-scavenge-stack* (mumble "Scav stack..."))
  (tagbody LOOP
     (when *gc-debug-scavenge-stack*
       (mumble-hex stack-pointer "SP: " t)
       (mumble-hex frame-pointer "FP: " t)
       (mumble-hex return-address "RA: " t))
     (let* ((fn-address (base-address-of-internal-pointer return-address))
            (fn-offset (- return-address fn-address))
            (fn (%%assemble-value fn-address +tag-object+)))
       (when *gc-debug-scavenge-stack*
         (mumble-hex fn-address "fn: " t)
         (mumble-hex fn-offset "fnoffs: " t))
       (scavenge-object fn)
       (multiple-value-bind (framep interruptp pushed-values pushed-values-register
                                    layout-address layout-length
                                    multiple-values incoming-arguments block-or-tagbody-thunk)
           (gc-info-for-function-offset fn fn-offset)
         (when (or (if sg-interruptedp
                       (not interruptp)
                       interruptp)
                   (and (not (eql pushed-values 0))
                        (or interruptp
                            (not framep)))
                   pushed-values-register
                   (and multiple-values (not (eql multiple-values 0)))
                   (or (keywordp incoming-arguments)
                       (and incoming-arguments (not framep)))
                   block-or-tagbody-thunk)
           (let ((*gc-debug-scavenge-stack* t))
             (debug-stack-frame framep interruptp pushed-values pushed-values-register
                                layout-address layout-length
                                multiple-values incoming-arguments block-or-tagbody-thunk))
           (emergency-halt "TODO! GC SG stuff."))
         (cond (interruptp
                (when (not framep)
                  (emergency-halt "non-frame interrupt gc entry"))
                (let* ((other-return-address (memref-unsigned-byte-64 frame-pointer 1))
                       (other-frame-pointer (memref-unsigned-byte-64 frame-pointer 0))
                       (other-stack-pointer (memref-unsigned-byte-64 frame-pointer 4))
                       (other-fn-address (base-address-of-internal-pointer other-return-address))
                       (other-fn-offset (- other-return-address other-fn-address))
                       (other-fn (%%assemble-value other-fn-address +tag-object+)))
                  (when *gc-debug-scavenge-stack*
                    (mumble-hex other-return-address "oRA: " t)
                    (mumble-hex other-frame-pointer "oFP: " t)
                    (mumble-hex other-stack-pointer "oSP: " t)
                    (mumble-hex other-fn-address "oFNa: " t)
                    (mumble-hex other-fn-offset "oFNo: " t))
                  ;; Unconditionally scavenge the saved data registers.
                  (scavengef (memref-t frame-pointer -12)) ; r8
                  (scavengef (memref-t frame-pointer -11)) ; r9
                  (scavengef (memref-t frame-pointer -10)) ; r10
                  (scavengef (memref-t frame-pointer -9)) ; r11
                  (scavengef (memref-t frame-pointer -8)) ; r12
                  (scavengef (memref-t frame-pointer -7)) ; r13
                  (scavengef (memref-t frame-pointer -6)) ; rbx
                  (multiple-value-bind (other-framep other-interruptp other-pushed-values other-pushed-values-register
                                                     other-layout-address other-layout-length
                                                     other-multiple-values other-incoming-arguments other-block-or-tagbody-thunk)
                      (gc-info-for-function-offset other-fn other-fn-offset)
                    (debug-stack-frame other-framep other-interruptp other-pushed-values other-pushed-values-register
                                       other-layout-address other-layout-length
                                       other-multiple-values other-incoming-arguments other-block-or-tagbody-thunk)
                    (when (or other-interruptp
                              (and (not (eql other-pushed-values 0))
                                   (or other-interruptp
                                       (not other-framep)))
                              (not (eql other-pushed-values-register nil))
                              #+nil(not (or (eql other-pushed-values-register nil)
                                       (eql other-pushed-values-register :rcx)))
                              (and other-multiple-values (not (eql other-multiple-values 0)))
                              (and (keywordp other-incoming-arguments) (not (eql other-incoming-arguments :rcx)))
                              other-block-or-tagbody-thunk)
                      (let ((*gc-debug-scavenge-stack* t))
                        (mumble-hex other-return-address "oRA: " t)
                        (mumble-hex other-frame-pointer "oFP: " t)
                        (mumble-hex other-stack-pointer "oSP: " t)
                        (mumble-hex other-fn-address "oFNa: " t)
                        (mumble-hex other-fn-offset "oFNo: " t)
                        (debug-stack-frame other-framep other-interruptp other-pushed-values other-pushed-values-register
                                           other-layout-address other-layout-length
                                           other-multiple-values other-incoming-arguments other-block-or-tagbody-thunk))
                      (emergency-halt "TODO! GC SG stuff. (interrupt)"))
                    (when (keywordp other-incoming-arguments)
                      (when (not (eql other-incoming-arguments :rcx))
                        (let ((*gc-debug-scavenge-stack* t))
                          (debug-stack-frame other-framep other-interruptp other-pushed-values other-pushed-values-register
                                             other-layout-address other-layout-length
                                             other-multiple-values other-incoming-arguments other-block-or-tagbody-thunk))
                        (emergency-halt "TODO? incoming-arguments not in RCX"))
                      (setf other-incoming-arguments nil)
                      (mumble-hex (memref-t frame-pointer -2) "ia-count ")
                      (scavenge-stack-n-incoming-arguments
                       other-frame-pointer other-stack-pointer other-framep
                       other-layout-length
                       ;; RCX.
                       (memref-t frame-pointer -2)))
                    (scavenge-regular-stack-frame other-frame-pointer other-stack-pointer other-framep
                                                  other-layout-address other-layout-length
                                                  other-incoming-arguments other-pushed-values)
                    (setf sg-interruptedp nil)
                    (cond (other-framep
                           (psetf stack-pointer other-stack-pointer
                                  frame-pointer other-frame-pointer))
                          (t ;; No frame, carefully pick out the new values.
                           ;; Frame pointer should be unchanged.
                           (setf frame-pointer other-frame-pointer)
                           ;; Stack pointer needs the return address popped off,
                           ;; and any layout variables.
                           (setf stack-pointer (+ other-stack-pointer (* (1+ other-layout-length) 8)))
                           ;; Return address should be one below the stack pointer.
                           (setf return-address (memref-unsigned-byte-64 stack-pointer -1))
                           ;; Skip other code and just loop again.
                           (go LOOP))))))
               (t (when sg-interruptedp
                    (emergency-halt "interrupted sg, but not interrupt frame?"))
                  (scavenge-regular-stack-frame frame-pointer stack-pointer framep
                                                layout-address layout-length
                                                incoming-arguments pushed-values)))
         ;; Stop after seeing a zerop frame pointer.
         (if (eql frame-pointer 0)
             (return-from scavenge-stack))
         (if (not framep)
             (emergency-halt "No frame, but no end in sight?"))
         (psetf return-address (memref-unsigned-byte-64 frame-pointer 1)
                stack-pointer (+ frame-pointer 16)
                frame-pointer (memref-unsigned-byte-64 frame-pointer 0))))
     (go LOOP))
  (when *gc-debug-scavenge-stack* (mumble "Done scav stack.")))

(defun scan-thread (object)
  ;; Scavenge various parts of the thread.
  (scavengef (mezzanine.supervisor:thread-name object))
  (scavengef (mezzanine.supervisor:thread-state object))
  (scavengef (mezzanine.supervisor:thread-lock object))
  ;; FIXME: Mark stack.
  (scavengef (mezzanine.supervisor:thread-stack object))
  (scavengef (mezzanine.supervisor:thread-special-stack-pointer object))
  (scavengef (mezzanine.supervisor:thread-preemption-disable-depth object))
  (scavengef (mezzanine.supervisor:thread-preemption-pending object))
  (scavengef (mezzanine.supervisor:thread-%next object))
  (scavengef (mezzanine.supervisor:thread-%prev object))
  (scavengef (mezzanine.supervisor:thread-foothold-disable-depth object))
  ;; Only scan the thread's stack, MV area & TLS area when it's alive.
  (when (not (eql (mezzanine.supervisor:thread-state object) :dead))
    (let* ((address (ash (%pointer-field object) 4))
           (stack-pointer (mezzanine.supervisor:thread-stack-pointer object))
           (frame-pointer (memref-unsigned-byte-64 stack-pointer 0))
           (return-address (memref-unsigned-byte-64 stack-pointer 2)))
      ;; Unconditonally scavenge the TLS area and the binding stack.
      (scavenge-many (+ address 8 (* mezzanine.supervisor::+thread-mv-slots-start+ 8))
                     (- mezzanine.supervisor::+thread-mv-slots-end+ mezzanine.supervisor::+thread-mv-slots-start+))
      (scavenge-many (+ address 8 (* mezzanine.supervisor::+thread-tls-slots-start+ 8))
                     (- mezzanine.supervisor::+thread-tls-slots-end+ mezzanine.supervisor::+thread-tls-slots-start+))
      (scavenge-stack (+ stack-pointer (* 3 8)) frame-pointer return-address
                      ;; FIXME: Was it interrupted or not???
                      nil))))

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
          (block-or-tagbody-thunk nil))
      ;; Macroize because the compiler would allocate an environment/lambda for this otherwise.
      (macrolet ((consume (&optional (errorp t))
                   `(progn
                      (when (>= position length)
                        ,(if errorp
                             `(emergency-halt "Reached end of GC Info??")
                             `(debug-stack-frame framep interruptp pushed-values pushed-values-register
                                                 layout-address layout-length
                                                 multiple-values incoming-arguments block-or-tagbody-thunk))
                        (return-from gc-info-for-function-offset
                          (values framep interruptp pushed-values pushed-values-register
                                  layout-address layout-length multiple-values
                                  incoming-arguments block-or-tagbody-thunk)))
                      (prog1 (memref-unsigned-byte-8 info-address position)
                        (incf position))))
                 (register-id (reg)
                   `(ecase ,reg
                      (0 :rax)
                      (1 :rcx)
                      (2 :rdx)
                      (3 :rbx)
                      (4 :rsp)
                      (5 :rbp)
                      (6 :rsi)
                      (7 :rdi)
                      (8 :r8)
                      (9 :r9)
                      (10 :r10)
                      (11 :r11)
                      (12 :r12)
                      (13 :r13)
                      (14 :r14)
                      (15 :r15))))
        (loop (let ((address 0))
                ;; Read first byte of address, this is where we can terminate.
                (let ((byte (consume nil))
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
                  (debug-stack-frame framep interruptp pushed-values pushed-values-register
                                     layout-address layout-length
                                     multiple-values incoming-arguments block-or-tagbody-thunk)
                  (return-from gc-info-for-function-offset
                          (values framep interruptp pushed-values pushed-values-register
                                  layout-address layout-length multiple-values
                                  incoming-arguments block-or-tagbody-thunk)))
                ;; Read flag/pvr byte & mv-and-iabtt.
                (let ((flags-and-pvr (consume))
                      (mv-and-iabtt (consume)))
                  (setf framep (logtest flags-and-pvr #b0001))
                  (setf interruptp (logtest flags-and-pvr #b0010))
                  (if (eql (ldb (byte 4 4) flags-and-pvr) 4)
                      (setf pushed-values-register nil)
                      (setf pushed-values-register
                            (register-id (ldb (byte 4 4) flags-and-pvr))))
                  (if (eql (ldb (byte 4 0) mv-and-iabtt) 15)
                      (setf multiple-values nil)
                      (setf multiple-values (ldb (byte 4 0) mv-and-iabtt)))
                  (setf block-or-tagbody-thunk nil
                        incoming-arguments nil)
                  (when (logtest flags-and-pvr #b0100)
                    (setf block-or-tagbody-thunk :rax))
                  (when (logtest flags-and-pvr #b1000)
                    (if (eql (ldb (byte 4 4) mv-and-iabtt) 15)
                        :rcx
                        (ldb (byte 4 4) mv-and-iabtt))))
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
                  (incf position (ceiling layout-length 8)))))))))

(defun scan-array-like (object)
  ;; Careful here. Functions with lots of GC info can have the header fall
  ;; into bignumness when read as a ub64.
  (let* ((address (ash (%pointer-field object) 4))
         (type (ldb (byte +array-type-size+ +array-type-shift+)
                    (memref-unsigned-byte-8 address 0))))
    ;; Dispatch again based on the type.
    (case type
      (#.+object-tag-array-t+
       ;; simple-vector
       ;; 1+ to account for the header word.
       (scan-generic object (1+ (ldb (byte +array-length-size+ +array-length-shift+)
                                     (memref-unsigned-byte-64 address 0)))))
      ((#.+object-tag-memory-array+
        #.+object-tag-simple-string+
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
       (when (hash-table-p object)
         (setf (hash-table-rehash-required object) 't))
       (scan-generic object (1+ (ldb (byte +array-length-size+ +array-length-shift+)
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
     (scan-array-like object))
    (t (scan-error object))))

(defun transport-error (object)
  (mumble-hex (lisp-object-address object))
  (mumble " ")
  (mumble-hex (memref-unsigned-byte-64 (ash (%pointer-field object) 4) 0))
  (emergency-halt "untransportable object"))

(defun transport-generic (object length)
  "Transport LENGTH words from oldspace to newspace, returning
a pointer to the new object. Leaves a forwarding pointer in place."
  (let* ((address (ash (%pointer-field object) 4))
         (first-word (memref-t address 0))
         (new-address nil))
    ;; Check for a GC forwarding pointer.
    (when (eql (%tag-field first-word) +tag-gc-forward+)
      (return-from transport-generic
        (%%assemble-value (ash (%pointer-field first-word) 4)
                          (%tag-field object))))
    ;; Update meters.
    (incf *objects-copied*)
    (incf *words-copied* length)
    ;; Copy words.
    (setf new-address (+ *newspace* (ash *newspace-offset* 3)))
    (%fast-copy new-address address (ash length 3))
    ;; Update newspace size.
    (incf *newspace-offset* length)
    (when (oddp length)
      (setf (memref-t new-address length) 0)
      (incf *newspace-offset*))
    ;; Leave a forwarding pointer.
    (setf (memref-t address 0) (%%assemble-value new-address +tag-gc-forward+))
    ;; Complete! Return the new object
    (%%assemble-value new-address (%tag-field object))))

(defun transport-array-like (object)
  (let* ((address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 address 0))
         (length (ldb (byte +array-length-size+ +array-length-shift+) header))
         (type (ldb (byte +array-type-size+ +array-type-shift+) header)))
    ;; Check for a forwarding pointer before the type check.
    ;; This test is duplicated from transport-generic.
    (when (eql (ldb (byte 4 0) header) +tag-gc-forward+)
      (return-from transport-array-like
        (%%assemble-value (logand header (lognot #b1111))
                          +tag-object+)))
    (when (hash-table-p object)
      (setf (hash-table-rehash-required object) 't))
    ;; Dispatch again based on the type.
    (case type
      ((#.+object-tag-array-t+
        #.+object-tag-array-fixnum+
        #.+object-tag-structure-object+)
       ;; simple-vector, std-instance or structure-object.
       ;; 1+ to account for the header word.
       (transport-generic object (1+ length)))
      (#.+object-tag-symbol+
       (transport-generic object 6))
      (#.+object-tag-std-instance+
       (transport-generic object 3))
      (#.+object-tag-function-reference+
       (transport-generic object 4))
      ((#.+object-tag-memory-array+
        #.+object-tag-simple-string+
        #.+object-tag-string+
        #.+object-tag-simple-array+
        #.+object-tag-array+)
       (transport-generic object (+ 4 length)))
      ;; Nothing else can be transported
      (t (transport-error object)))))

(defun transport-object (object)
  "Transport an object in oldspace to newspace.
Leaves pointer fields unchanged and returns the new object."
  (case (%tag-field object)
    (#.+tag-cons+
     (transport-generic object 2))
    (#.+tag-object+
     (transport-array-like object))
    (t (transport-error object))))

(defun mark-static-object (object)
  (let ((address (ash (%pointer-field object) 4)))
    (when (not (logbitp +static-header-used-bit+ (memref-unsigned-byte-64 address -1)))
      (mumble-hex object)
      (emergency-halt "Marking free static object."))
    (when (eql (ldb (byte 1 +static-header-mark-bit+)
                    (memref-unsigned-byte-64 address -1))
               *static-mark-bit*)
      ;; Object has already been marked.
      (return-from mark-static-object))
    (setf (ldb (byte 1 +static-header-mark-bit+)
               (memref-unsigned-byte-64 address -1))
          *static-mark-bit*)
    (with-gc-trace (object #\s)
      (scan-object object))))

(defun sweep-static-space (space)
  (mumble "Sweeping static space")
  (let ((offset 0)
        (last-free-tag nil))
    (loop (let ((size (memref-unsigned-byte-64 space offset))
                (info (memref-unsigned-byte-64 space (+ offset 1))))
            (when (and (logbitp +static-header-used-bit+ info)
                       (not (eql (ldb (byte 1 +static-header-mark-bit+) info)
                                 *static-mark-bit*)))
              ;; Allocated, but not marked. Must not be reachable.
              (setf (ldb (byte 1 +static-header-used-bit+) (memref-unsigned-byte-64 space (+ offset 1))) 0))
            (if (not (logbitp +static-header-used-bit+ (memref-unsigned-byte-64 space (+ offset 1))))
                ;; Free tag.
                (cond (last-free-tag
                       ;; Merge adjacent free tags.
                       (incf (memref-unsigned-byte-64 space last-free-tag) (+ size 2))
                       (when (logbitp +static-header-end-bit+ info)
                         ;; Last tag.
                         (setf (ldb (byte 1 +static-header-end-bit+)
                                    (memref-unsigned-byte-64 space (1+ last-free-tag)))
                               1)
                         (return)))
                      (t ;; Previous free tag.
                       (setf last-free-tag offset)))
                ;; Allocated tag.
                (setf last-free-tag nil))
            (when (logbitp +static-header-end-bit+ info)
              (return))
            (incf offset (+ size 2))))))

(defun sweep-stacks ()
  (mumble "sweeping stacks")
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

(defun gc-cycle ()
  (let ((old-offset *newspace-offset*))
    (set-gc-light)
    (mumble "GC in progress...")
    ;; Allow access to the soon-to-be-newspace.
    (setf (ldb (byte 2 0) (memref-unsigned-byte-32 *oldspace-paging-bits* 0)) 3)
    ;; Clear per-cycle meters
    (setf *objects-copied* 0
          *words-copied* 0)
    ;; Flip.
    (psetf *oldspace* *newspace*
           *newspace* *oldspace*
           *oldspace-paging-bits* *newspace-paging-bits*
           *newspace-paging-bits* *oldspace-paging-bits*
           *newspace-offset* 0
           *static-mark-bit* (logxor *static-mark-bit* 1))
    ;; Wipe stack mark bits.
    (dolist (entry *gc-stack-ranges*)
      (setf (gc-stack-range-marked entry) nil))
    ;; Scavenge NIL to start things off.
    (scavenge-object 'nil)
    ;; And scavenge the current registers and stacks.
    (scavenge-current-stack-group 1 2 3 4 5)
    ;; Now do the bulk of the work by scavenging newspace.
    (scavenge-newspace)
    ;; Make oldspace inaccessible.
    (setf (ldb (byte 2 0) (memref-unsigned-byte-32 *oldspace-paging-bits* 0)) 0)
    ;; Flush TLB.
    (setf (%cr3) (%cr3))
    ;; Sweep static space.
    (sweep-static-space *small-static-area*)
    (setf *small-static-area-hint* 0)
    (sweep-static-space *large-static-area*)
    (setf *large-static-area-hint* 0)
    (sweep-stacks)
    (mumble "complete")
    (clear-gc-light)))


(defun cons (car cdr)
  (cons-in-area car cdr))

(defun cons-in-area (car cdr &optional area)
  (with-interrupts-disabled ()
    (let ((cons (%%assemble-value (%raw-allocate 2 area) +tag-cons+)))
      (setf (car cons) car
            (cdr cons) cdr)
      cons)))

(defun %allocate-array-like (tag word-count length &optional area)
  "Allocate a array-like object. All storage is initialized to zero.
WORD-COUNT must be the number of words used to store the data, not including
the header word. LENGTH is the number of elements in the array."
  (with-interrupts-disabled ()
    ;; Align and account for the header word.
    (if (oddp word-count)
        (incf word-count 1)
        (incf word-count 2))
    (let ((address (%raw-allocate word-count area)))
      ;; Clear memory.
      (dotimes (i word-count)
        (setf (memref-unsigned-byte-64 address i) 0))
      ;; Set header word.
      (setf (memref-unsigned-byte-64 address 0)
            (logior (ash length +array-length-shift+)
                    (ash tag +array-type-shift+)))
      ;; Return value.
      (%%assemble-value address +tag-object+))))

(defun allocate-std-instance (class slots &optional area)
  (let ((value (%allocate-array-like +object-tag-std-instance+ 2 2 area)))
    (setf (std-instance-class value) class
          (std-instance-slots value) slots)
    value))

(defun %make-struct (length &optional area)
  (%allocate-array-like +object-tag-structure-object+ length length area))

(defun make-function-with-fixups (tag machine-code fixups constants gc-info)
  (with-interrupts-disabled ()
    (let* ((mc-size (ceiling (+ (length machine-code) 16) 16))
           (gc-info-size (ceiling (length gc-info) 8))
           (pool-size (length constants))
           (total (+ (* mc-size 2) pool-size gc-info-size)))
      (when (oddp total)
        (incf total))
      (let ((address (%raw-allocate total :static)))
        ;; Initialize header.
        (setf (memref-unsigned-byte-64 address 0) 0
              (memref-unsigned-byte-64 address 1) (+ address 16)
              (memref-unsigned-byte-16 address 0) (ash tag +array-type-shift+)
              (memref-unsigned-byte-16 address 1) mc-size
              (memref-unsigned-byte-16 address 2) pool-size
              (memref-unsigned-byte-16 address 3) (length gc-info))
        ;; Initialize code.
        (dotimes (i (length machine-code))
          (setf (memref-unsigned-byte-8 address (+ i 16)) (aref machine-code i)))
        ;; Apply fixups.
        (dolist (fixup fixups)
          (let ((value (case (car fixup)
                         ((nil t)
                          (lisp-object-address (car fixup)))
                         (undefined-function
                          (lisp-object-address *undefined-function-thunk*))
                         (:unbound-tls-slot
                          (lisp-object-address (%unbound-tls-slot)))
                         (:unbound-value
                          (lisp-object-address (%unbound-value)))
                         (t (error "Unsupported fixup ~S." (car fixup))))))
            (dotimes (i 4)
              (setf (memref-unsigned-byte-8 address (+ (cdr fixup) i))
                    (logand (ash value (* i -8)) #xFF)))))
        ;; Initialize constant pool.
        (dotimes (i (length constants))
          (setf (memref-t (+ address (* mc-size 16)) i) (aref constants i)))
        ;; Initialize GC info.
        (let ((gc-info-offset (+ address (* mc-size 16) (* pool-size 8))))
          (dotimes (i (length gc-info))
            (setf (memref-unsigned-byte-8 gc-info-offset i) (aref gc-info i))))
        (%%assemble-value address +tag-object+)))))

(defun make-function (machine-code constants gc-info)
  (make-function-with-fixups +object-tag-function+ machine-code '() constants gc-info))

(defun make-closure (function environment)
  "Allocate a closure object."
  (check-type function function)
  (with-interrupts-disabled ()
    (let* ((address (%raw-allocate 6 :static))
           (entry-point (%array-like-ref-unsigned-byte-64 function 0))
           ;; Jmp's address is entry-point - <address-of-instruction-after-jmp>
           (rel-entry-point (- entry-point (+ address (* 7 4)))))
      ;; Initialize and clear constant slots.
      ;; Function tag, flags and MC size.
      (setf (memref-unsigned-byte-32 address 0) (logior #x00020000
                                                        (ash +object-tag-closure+
                                                             +array-type-shift+))
            ;; Constant pool size and slot count.
            (memref-unsigned-byte-32 address 1) #x00000002
            ;; Entry point
            (memref-unsigned-byte-64 address 1) (+ address 16)
            ;; The code.
            ;; mov64 :rbx (:rip 17)/pool[1]
            (memref-unsigned-byte-32 address 4) #x111D8B48
            ;; jmp entry-point
            (memref-unsigned-byte-32 address 5) #xE9000000
            ;; jmp's rel32 address ended up being nicely aligned. lucky!
            (memref-signed-byte-32 address 6) rel-entry-point
            (memref-unsigned-byte-32 address 7) #xCCCCCCCC)
      (let ((value (%%assemble-value address +tag-object+)))
        ;; Initialize constant pool
        (setf (memref-t address 4) function
              (memref-t address 5) environment)
        value))))

(defun allocate-funcallable-std-instance (function class slots)
  "Allocate a funcallable instance."
  (check-type function function)
  (with-interrupts-disabled ()
    (let ((address (%raw-allocate 8 :static))
          (entry-point (%array-like-ref-unsigned-byte-64 function 0)))
      ;; Initialize and clear constant slots.
      ;; Function tag, flags and MC size.
      (setf (memref-unsigned-byte-32 address 0) (logior #x00020000
                                                        (ash +object-tag-funcallable-instance+
                                                             +array-type-shift+))
            ;; Constant pool size and slot count.
            (memref-unsigned-byte-32 address 1) #x00000003
            ;; Entry point
            (memref-unsigned-byte-64 address 1) (+ address 16)
            ;; The code.
            ;; jmp (:rip 2)/pool[-1]
            (memref-unsigned-byte-32 address 4) #x000225FF
            (memref-unsigned-byte-32 address 5) #xCCCC0000
            ;; entry-point
            (memref-unsigned-byte-64 address 3) entry-point)
      (let ((value (%%assemble-value address +tag-object+)))
        ;; Initialize constant pool
        (setf (memref-t address 4) function
              (memref-t address 5) class
              (memref-t address 6) slots)
        value))))

(defun make-symbol-in-area (name &optional area)
  (check-type name string)
  (with-interrupts-disabled ()
    (let* ((symbol (%allocate-array-like +object-tag-symbol+ 6 0 area)))
      ;; symbol-name.
      (setf (%array-like-ref-t symbol 0) name)
      (makunbound symbol)
      (setf (symbol-fref symbol) nil
            (symbol-plist symbol) nil
            (symbol-package symbol) nil)
      symbol)))

(defun make-symbol (name)
  (check-type name string)
  (make-symbol-in-area name nil))

(define-lap-function %%make-bignum-128-rdx-rax ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push :rdx)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 #.(ash 2 +n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:pop (:r8 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:pop (:r8 #.(+ (- +tag-object+) 16)))
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret))

(define-lap-function %%make-bignum-64-rax ()
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push 0)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r13 (:function %make-bignum-of-length))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:pop (:r8 #.(+ (- +tag-object+) 8)))
  (sys.lap-x86:mov32 :ecx #.(ash 1 +n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret))

;;; This is used by the bignum code so that bignums and fixnums don't have
;;; to be directly compared.
(defun %make-bignum-from-fixnum (n)
  (with-interrupts-disabled ()
    (let* ((address (%raw-allocate 2 :static)))
      (setf (memref-unsigned-byte-64 address 0) (logior (ash 1 +array-length-shift+)
                                                        (ash +object-tag-bignum+ +array-type-shift+))
            (memref-signed-byte-64 address 1) n)
      (%%assemble-value address +tag-object+))))

(defun %make-bignum-of-length (words)
  (with-interrupts-disabled ()
    (let* ((address (%raw-allocate (+ 1 words (if (logtest words 1) 0 1)) :static)))
      (setf (memref-unsigned-byte-64 address 0) (logior (ash words +array-length-shift+)
                                                        (ash +object-tag-bignum+ +array-type-shift+)))
      (%%assemble-value address +tag-object+))))

(defun %allocate-stack (length)
  (when (oddp length)
    (incf length))
  (setf length (* length 8))
  ;; Arrange for the error to be thrown after we leave the w-i-disabled region.
  (or (with-interrupts-disabled ()
        (dolist (entry *gc-stack-ranges*)
          (when (and (not (gc-stack-range-allocated entry))
                     (>= (- (gc-stack-range-end entry)
                            (gc-stack-range-start entry))
                         length))
            (cond ((= (- (gc-stack-range-end entry)
                         (gc-stack-range-start entry))
                      length)
                   ;; Same length, just mark as allocated.
                   (setf (gc-stack-range-allocated entry) t)
                   (return entry))
                  (t ;; Split & resort.
                   (let ((new (make-gc-stack-range :allocated t
                                                   :start (gc-stack-range-start entry)
                                                   :end (+ (gc-stack-range-start entry)
                                                           length))))
                     (setf (gc-stack-range-start entry) (gc-stack-range-end new))
                     #+(or)(setf *gc-stack-ranges* (merge 'list (list new) *gc-stack-ranges*
                                                          #'< :key #'gc-stack-range-start))
                     (setf *gc-stack-ranges* (sort (list* new *gc-stack-ranges*)
                                                   #'< :key #'gc-stack-range-start))
                     (return new)))))))
        (error "No more space for stacks!")))

(defun base-address-of-internal-pointer (address)
  "Find the base address of the object pointed to be ADDRESS.
Address should be an internal pointer to a live object in static space.
No type information will be provided."
  (flet ((search (space)
           (let ((offset 0))
             (with-interrupts-disabled ()
               (loop (let ((size (memref-unsigned-byte-64 space offset))
                           (info (memref-unsigned-byte-64 space (+ offset 1))))
                       (when (and (<= (+ space (* (+ offset 2) 8)) address)
                                  (< address (+ space (* (+ offset size 2) 8))))
                         (return-from base-address-of-internal-pointer
                           (values (+ space (* (+ offset 2) 8)) t)))
                       (when (logbitp +static-header-end-bit+ info)
                         (return))
                       (incf offset (+ size 2))))))))
    (search *large-static-area*)
    (search *small-static-area*)))
