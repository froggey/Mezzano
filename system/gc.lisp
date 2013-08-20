(in-package :sys.int)

(declaim (special *oldspace* *newspace* *newspace-offset* *semispace-size*
                  *oldspace-paging-bits* *newspace-paging-bits*))
(declaim (special *small-static-area* *small-static-area-hint*))
(declaim (special *large-static-area* *large-static-area-hint*))
(declaim (special *static-mark-bit*))
(declaim (special *stack-bump-pointer* *stack-bump-pointer-limit*))
(declaim (special *bump-pointer*))
(declaim (special *verbose-gc*))
;;; GC Meters.
(declaim (special *objects-copied* *words-copied*))
(declaim (special *multiboot-info* *kboot-tag-list*))
(setf *verbose-gc* nil)
(setf *objects-copied* 0
      *words-copied* 0)

(defconstant +static-header-mark-bit+ 0)
(defconstant +static-header-used-bit+ 1)
(defconstant +static-header-end-bit+ 2)

(defvar *gc-in-progress* nil)

(defvar *gc-stack-ranges*)

;; TODO: a weak pointer to the allocating stack group would be nice.
(defstruct gc-stack-range
  allocated
  marked
  start
  end)

;; Run once during cold load to create the first stack range objects.
(defun gc-init-stack-area ()
  (let* ((sg (current-stack-group))
         (cs-base (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-control-stack-base+))
	 (cs-size (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-control-stack-size+))
         (bs-base (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-binding-stack-base+))
	 (bs-size (%array-like-ref-unsigned-byte-64 sg +stack-group-offset-binding-stack-size+))
         (cs-range (make-gc-stack-range :allocated t
                                        :start cs-base
                                        :end (+ cs-base cs-size)))
         (bs-range (make-gc-stack-range :allocated t
                                        :start bs-base
                                        :end (+ bs-base bs-size)))
         (free-range (make-gc-stack-range :allocated nil
                                          :start *stack-bump-pointer*
                                          :end *stack-bump-pointer-limit*)))
    (setf (%array-like-ref-t sg +stack-group-offset-binding-stack-range+) bs-range
          (%array-like-ref-t sg +stack-group-offset-control-stack-range+) cs-range)
    (setf *gc-stack-ranges* (sort (list cs-range bs-range free-range)
                                  #'<
                                  :key #'gc-stack-range-start))))
(gc-init-stack-area)

(defun gc-init-system-memory ()
  (setf *system-memory-map* (canonicalize-memory-map
                             (cond (*multiboot-info*
                                    (multiboot-mmap-add-reserved-regions
                                     (multiboot-memory-map)))
                                   (*kboot-tag-list*
                                    (kboot-memory-map))
                                   (t #()))))
  ;; Allocate DMA memory from the largest :FREE region.
  (let ((best-start nil)
        (best-size 0))
    (loop for entry across *system-memory-map* do
         (when (and (eql (memory-map-entry-type entry) :free)
                    (> (memory-map-entry-length entry) best-size))
           (setf best-start (memory-map-entry-base entry)
                 best-size (memory-map-entry-length entry))))
    (cond (best-start
           (setf *bump-pointer* (+ #x8000000000 best-start)
                 *bump-pointer* (logand (+ *bump-pointer* #xFFF) (lognot #xFFF)))
           (format t "DMA bump pointer at ~X, region length ~D.~%" *bump-pointer* best-size))
          (t (format t "No free memory for DMA bump pointer???~%")))))

#+nil(add-hook '*early-initialize-hook* 'gc-init-system-memory)

;;; FIXME: Should use unwind-protect but that conses!!!
;;; TODO: Require that this can never nest (ie interrupts are on "all" the time).
(defmacro with-interrupts-disabled (options &body code)
  `(let ((istate (%interrupt-state)))
     (%cli)
     (multiple-value-prog1 (progn ,@code) (when istate (%sti)))))

;;; FIXME: Don't use with-interrupts-disabled.
;;; Suppress preemption (SBCL pseudo-atomic-like operation).

(defun room (&optional (verbosity :default))
  (let ((total-used 0)
        (total 0))
    (fresh-line)
    (format t "Dynamic space: ~:D/~:D words allocated (~D%).~%"
            *newspace-offset* *semispace-size*
            (truncate (* *newspace-offset* 100) *semispace-size*))
    (incf total-used *newspace-offset*)
    (incf total *semispace-size*)
    (multiple-value-bind (allocated-words total-words largest-free-space)
        (static-area-info *small-static-area*)
      (format t "Small static space: ~:D/~:D words allocated (~D%).~%"
              allocated-words total-words
              (truncate (* allocated-words 100) total-words))
      (format t "  Largest free area: ~:D words.~%" largest-free-space)
      (incf total-used allocated-words)
      (incf total total-words))
    (multiple-value-bind (allocated-words total-words largest-free-space)
        (static-area-info *large-static-area*)
      (format t "Large static space: ~:D/~:D words allocated (~D%).~%"
              allocated-words total-words
              (truncate (* allocated-words 100) total-words))
      (format t "  Largest free area: ~:D words.~%" largest-free-space)
      (incf total-used allocated-words)
      (incf total total-words))
    (multiple-value-bind (allocated-words total-words largest-free-space)
        (stack-area-info)
      (format t "Stack area: ~:D/~:D words allocated (~D%).~%"
              allocated-words total-words
              (truncate (* allocated-words 100) total-words))
      (format t "  Largest free area: ~:D words.~%" largest-free-space)
      (incf total-used allocated-words)
      (incf total total-words))
    (format t "Total ~:D/~:D words used (~D%).~%"
            total-used total
            (truncate (* total-used 100) total))
    (values)))

(defun static-area-info (space)
  (let ((allocated-words 0)
        (total-words 0)
        (offset 0)
        (largest-free-space 0))
    (with-interrupts-disabled ()
      (loop (let ((size (memref-unsigned-byte-64 space offset))
                  (info (memref-unsigned-byte-64 space (+ offset 1))))
              (incf total-words (+ size 2))
              (cond ((logbitp +static-header-used-bit+ info)
                     (incf allocated-words (+ size 2)))
                    (t ; free block.
                     (setf largest-free-space (max largest-free-space size))))
              (when (logbitp +static-header-end-bit+ info)
                (return))
              (incf offset (+ size 2)))))
    (values allocated-words total-words largest-free-space)))

(defun stack-area-info ()
  (let ((allocated-words 0)
        (total-words 0)
        (largest-free-space 0))
    (with-interrupts-disabled ()
      (dolist (entry *gc-stack-ranges*)
        (let ((size (truncate (- (gc-stack-range-end entry)
                                 (gc-stack-range-start entry))
                              8)))
          (incf total-words size)
          (if (gc-stack-range-allocated entry)
              (incf allocated-words size)
              (setf largest-free-space (max largest-free-space size))))))
    (values allocated-words total-words largest-free-space)))

(defun gc ()
  "Run a garbage-collection cycle."
  (with-interrupts-disabled ()
    (%gc)))

(declaim (inline oldspace-pointer-p))
(defun oldspace-pointer-p (address)
  (<= *oldspace*
      address
      (+ 1 *oldspace* (ash *semispace-size* 3))))

(declaim (inline newspace-pointer-p))
(defun newspace-pointer-p (address)
  (<= *newspace*
      address
      (+ 1 *newspace* (ash *semispace-size* 3))))

(declaim (inline static-pointer-p))
(defun static-pointer-p (address)
  (< address #x80000000))

(declaim (inline immediatep))
(defun immediatep (object)
  "Return true if OBJECT is an immediate object."
  (case (%tag-field object)
    ((#.+tag-even-fixnum+ #.+tag-odd-fixnum+
      #.+tag-character+ #.+tag-single-float+
      #.+tag-unbound-value+)
     t)
    (t nil)))

#+nil(defmacro with-gc-trace ((object prefix) &body body)
  (let ((object-sym (gensym))
        (result-sym (gensym)))
    `(let ((,object-sym ,object))
       (when *verbose-gc*
         (gc-trace ,object-sym #\> ,prefix))
       (let ((,result-sym (progn ,@body)))
         (when *verbose-gc*
           (gc-trace ,result-sym #\~ ,prefix)
           (gc-trace ,object-sym #\< ,prefix))
         ,result-sym))))

(defmacro with-gc-trace ((object prefix) &body body)
  (declare (ignore object prefix))
  `(progn ,@body))

;; FIXME: evaluation rules...
(defmacro scavengef (place)
  "Scavenge PLACE."
  `(setf ,place (scavenge-object ,place)))

(defun scavenge-many (address n)
  (dotimes (i n)
    (scavengef (memref-t address i))))

(defun scavenge-newspace ()
  (mumble "Scav newspace")
  (do ((pointer 0))
      ((>= pointer *newspace-offset*))
    ;; Walk newspace, updating pointers as we go.
    (let ((n (- *newspace-offset* pointer)))
      (scavenge-many (+ *newspace* (* pointer 8)) n)
      (incf pointer n))))

;;; Arguments and MV return are to force the data registers on to the stack.
;;; This does not work for RBX or R13, but RBX is smashed by the function
;;; return and R13 shouldn't matter.
;;; This only scavenges the stacks/register. Scavenging the actual
;;; stack-group object is done by scan-stack-group, assuming the
;;; current stack-group is actually reachable.
(defun scavenge-current-stack-group (a1 a2 a3 a4 a5)
  (let* ((object (current-stack-group))
         (address (ash (%pointer-field object) 4))
         (bs-base (%array-like-ref-unsigned-byte-64 object +stack-group-offset-binding-stack-base+))
         (bs-size (%array-like-ref-unsigned-byte-64 object +stack-group-offset-binding-stack-size+))
         (binding-stack-pointer (%array-like-ref-unsigned-byte-64 object +stack-group-offset-binding-stack-pointer+))
         ;; Grovel around in the current stack frame to grab needed stuff.
         (frame-pointer (read-frame-pointer))
         (return-address (memref-unsigned-byte-64 frame-pointer 1))
         (stack-pointer (+ frame-pointer 16)))
    ;; Unconditonally scavenge the TLS area and the binding stack.
    (mumble "Scav GC TLS")
    (scavenge-many (+ address 8 (* +stack-group-offset-tls-slots+ 8))
                   +stack-group-tls-slots-size+)
    (mumble "Scav GC binding stack")
    (scavenge-many binding-stack-pointer
                   (ash (- (+ bs-base bs-size) binding-stack-pointer) -3))
    (mumble "Scav GC control stack")
    (scavenge-stack stack-pointer (memref-unsigned-byte-64 frame-pointer 0) return-address
                    nil))
  (values a1 a2 a3 a4 a5))

(defun scavenge-object (object)
  "Scavenge one object, returning an updated pointer."
  (when (immediatep object)
    ;; Don't care about immediate objects, return them unchanged.
    (return-from scavenge-object object))
  (let ((address (ash (%pointer-field object) 4)))
    (cond ((oldspace-pointer-p address)
           ;; Object is in oldspace, transport to newspace.
           (with-gc-trace (object #\t)
             (transport-object object)))
          ((newspace-pointer-p address)
           ;; Object is already in newspace.
           object)
          ((static-pointer-p address)
           ;; Object is in the static area, mark and scan.
           (mark-static-object object)
           object)
          (t
           ;; Assume the pointer is on the stack.
           ;; TODO: Track scanned stack objects. Allocate a cons with dynamic-extent
           ;; and push on some symbol.
           (with-gc-trace (object #\k)
             (scan-object object))
           object))))

(defun scan-error (object)
  (mumble-hex (lisp-object-address object))
  (mumble " ")
  (mumble-hex (memref-unsigned-byte-64 (ash (%pointer-field object) 4) 0))
  (emergency-halt "unscannable object"))

(defun scan-generic (object size)
  "Scavenge SIZE words pointed to by OBJECT."
  (scavenge-many (ash (%pointer-field object) 4) size))

(defvar *gc-debug-scavenge-stack* nil)

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
                                     incoming-arguments)
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

(defun scavenge-stack (stack-pointer frame-pointer return-address sg-interruptedp)
  (when *gc-debug-scavenge-stack* (mumble "Scav stack..."))
  (loop
     (when *gc-debug-scavenge-stack*
       (mumble-hex stack-pointer "SP: " t)
       (mumble-hex frame-pointer "FP: " t)
       (mumble-hex return-address "RA: " t))
     (let* ((fn-address (base-address-of-internal-pointer return-address))
            (fn-offset (- return-address fn-address))
            (fn (%%assemble-value fn-address +tag-function+)))
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
                   (not (eql pushed-values 0)) pushed-values-register
                   (and multiple-values (not (eql multiple-values 0)))
                   (or (keywordp incoming-arguments)
                       (and incoming-arguments (not framep)))
                   block-or-tagbody-thunk)
           (emergency-halt "TODO! GC SG stuff."))
         (cond (interruptp
                (when (not framep)
                  (emergency-halt "non-frame interrupt gc entry"))
                (let* ((other-return-address (memref-unsigned-byte-64 frame-pointer 1))
                       (other-frame-pointer (memref-unsigned-byte-64 frame-pointer 0))
                       (other-stack-pointer (memref-unsigned-byte-64 frame-pointer 4))
                       (other-fn-address (base-address-of-internal-pointer other-return-address))
                       (other-fn-offset (- other-return-address other-fn-address))
                       (other-fn (%%assemble-value other-fn-address +tag-function+)))
                  (mumble-hex other-return-address "oRA: " t)
                  (mumble-hex other-frame-pointer "oFP: " t)
                  (mumble-hex other-stack-pointer "oSP: " t)
                  (mumble-hex other-fn-address "oFNa: " t)
                  (mumble-hex other-fn-offset "oFNo: " t)
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
                    (when (or other-interruptp
                              (not (eql other-pushed-values 0)) other-pushed-values-register
                              (and other-multiple-values (not (eql other-multiple-values 0)))
                              (and (keywordp other-incoming-arguments) (not (eql other-incoming-arguments :rcx)))
                              other-block-or-tagbody-thunk)
                      (emergency-halt "TODO! GC SG stuff. (interrupt)"))
                    (when (keywordp other-incoming-arguments)
                      (when (not (eql other-incoming-arguments :rcx))
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
                                                  other-incoming-arguments)
                    (psetf stack-pointer other-stack-pointer
                           frame-pointer other-frame-pointer))))
               (t (when sg-interruptedp
                    (emergency-halt "interrupted sg, but not interrupt frame?"))
                  (scavenge-regular-stack-frame frame-pointer stack-pointer framep
                                                layout-address layout-length
                                                incoming-arguments)))
         ;; Stop after seeing a zerop frame pointer.
         (if (eql frame-pointer 0)
             (return))
         (if (not framep)
             (emergency-halt "No frame, but no end in sight?"))
         (psetf return-address (memref-unsigned-byte-64 frame-pointer 1)
                stack-pointer (+ frame-pointer 16)
                frame-pointer (memref-unsigned-byte-64 frame-pointer 0)
                sg-interruptedp nil))))
  (when *gc-debug-scavenge-stack* (mumble "Done scav stack.")))

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
                             `(when *gc-debug-scavenge-stack*
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
                        (mumble "no-btt")))
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
                  (ecase (ldb (byte 2 2) flags-and-pvr)
                    (#b00)
                    (#b01 (setf block-or-tagbody-thunk
                                (register-id (ldb (byte 4 4) mv-and-iabtt))))
                    (#b10 (setf incoming-arguments
                                (register-id (ldb (byte 4 4) mv-and-iabtt))))
                    (#b11 (setf incoming-arguments
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
                  (incf position (ceiling layout-length 8)))))))))

(defun scan-array-like (object)
  (let* ((address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 address 0))
         (length (ldb (byte +array-length-size+ +array-length-shift+) header))
         (type (ldb (byte +array-type-size+ +array-type-shift+) header)))
    ;; Dispatch again based on the type.
    (cond ((member type '(#.+array-type-t+
                          #.+array-type-std-instance+
                          #.+array-type-struct+))
           ;; simple-vector, std-instance or structure-object.
           (when (hash-table-p object)
             (setf (hash-table-rehash-required object) 't))
           ;; 1+ to account for the header word.
           (scan-generic object (1+ length)))
          ((eql type +array-type-stack-group+)
           (scan-stack-group object))
          ;; Ignore numeric arrays and bignums.
          ;; Array-type 0 is simple-vector, but that's handled above.
          ((or (<= type +last-array-type+)
               (eql type +array-type-bignum+)))
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
    (#.+tag-symbol+
     (scan-generic object 6))
    (#.+tag-array-header+
     (scan-generic object 4))
    (#.+tag-array-like+
     (scan-array-like object))
    (#.+tag-function+
     (scan-function object))
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
                          +tag-array-like+)))
    (when (hash-table-p object)
      (setf (hash-table-rehash-required object) 't))
    ;; Dispatch again based on the type.
    (case type
      ((#.+array-type-t+
        #.+array-type-std-instance+
        #.+array-type-struct+)
       ;; simple-vector, std-instance or structure-object.
       ;; 1+ to account for the header word.
       (transport-generic object (1+ length)))
      ;; Nothing else can be transported
      (t (transport-error object)))))

(defun transport-object (object)
  "Transport an object in oldspace to newspace.
Leaves pointer fields unchanged and returns the new object."
  (case (%tag-field object)
    (#.+tag-cons+
     (transport-generic object 2))
    (#.+tag-symbol+
     (transport-generic object 6))
    (#.+tag-array-header+
     (transport-generic object 4))
    (#.+tag-array-like+
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

(defun %gc ()
  (when *gc-in-progress*
    (error "Nested GC?!"))
  (unwind-protect
       (progn (setf *gc-in-progress* t)
              (%sti)
              (gc-cycle)
              (%cli))
    (setf *gc-in-progress* nil)))

;;; This is the fundamental dynamic allocation function.
;;; It ensures there is enough space on the dynamic heap to
;;; allocate WORDS words of memory and returns a fixnum address
;;; to the allocated memory. It violates GC invariants by twiddling
;;; *newspace-offset* without clearing memory, so must be called
;;; with the GC defered (currently by using WITH-INTERRUPTS-DISABLED)
;;; and the caller must clear the returned memory before reenabling the GC.
;;; Additionally, the number of words to allocate must be even to ensure
;;; correct alignment.
(defun %raw-allocate (words &optional area)
  (when (and (boundp '*gc-in-progress*)
             *gc-in-progress*)
    (emergency-halt "Allocating from inside the GC!"))
  (ecase area
    ((nil :dynamic)
     (when (> (+ *newspace-offset* words) *semispace-size*)
       (%gc)
       (when (> (+ *newspace-offset* words) *semispace-size*)
         ;; Oh dear. No memory.
         (emergency-halt "Out of memory.")))
     (prog1 (+ *newspace* (ash *newspace-offset* 3))
       (incf *newspace-offset* words)))
    (:static
     (multiple-value-bind (space hint)
         (if (<= words 30)
             (values *small-static-area* *small-static-area-hint*)
             (values *large-static-area* *large-static-area-hint*))
       (let ((address (or (when (not (zerop hint))
                            (%attempt-static-allocation space words hint))
                          (%attempt-static-allocation space words 0))))
         (unless address
           (%gc)
           (setf address (%attempt-static-allocation space words 0))
           (unless address
             (mumble "Static space exhausted.")
             (error 'simple-storage-condition
                    :format-control "Static space exhausted during allocation of size ~:D words."
                    :format-arguments (list words))
             (emergency-halt "Static space exhausted.")))
         address)))))

(defun %attempt-static-allocation (space words hint)
  (loop
     (let ((size (memref-unsigned-byte-64 space hint))
           (info (memref-unsigned-byte-64 space (+ hint 1))))
       (when (and (>= size words)
                  (not (logbitp +static-header-used-bit+ info)))
         ;; Large enough to satisfy and free.
         (unless (= size words)
           ;; Larger than required. Split it.
           (setf (memref-unsigned-byte-64 space (+ hint 2 words)) (- size words 2)
                 (memref-unsigned-byte-64 space (+ hint 3 words)) (logand info (ash 1 +static-header-end-bit+))
                 (memref-unsigned-byte-64 space hint) words
                 (ldb (byte 1 +static-header-end-bit+) (memref-unsigned-byte-64 space (+ hint 1))) 0))
         ;; Initialize the static header words.
         (setf (ldb (byte 1 +static-header-mark-bit+) (memref-unsigned-byte-64 space (+ hint 1))) *static-mark-bit*
               (ldb (byte 1 +static-header-used-bit+) (memref-unsigned-byte-64 space (+ hint 1))) 1)
         ;; Update the hint value, be careful to avoid running past the end of static space.
         (let ((new-hint (if (logtest (memref-unsigned-byte-64 space (+ hint 1)) #b100)
                             0
                             (+ hint 2 words))))
           ;; Arf.
           (cond ((eql space *small-static-area*)
                  (setf *small-static-area-hint* new-hint))
                 ((eql space *large-static-area*)
                  (setf *large-static-area-hint* new-hint))
                 (t (error "Unknown space ~X??" space))))
         (return (+ space (* hint 8) 16)))
       (when (logbitp +static-header-end-bit+ info)
         ;; Last tag.
         (return nil))
       (incf hint (+ size 2)))))

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
      (%%assemble-value address +tag-array-like+))))

(defun allocate-std-instance (class slots &optional area)
  (let ((value (%allocate-array-like +array-type-std-instance+ 2 2 area)))
    (setf (std-instance-class value) class
          (std-instance-slots value) slots)
    value))

(defun %make-struct (length &optional area)
  (%allocate-array-like +array-type-struct+ length length area))

(define-lap-function %%add-function-to-bochs-debugger ()
  (sys.lap-x86:mov32 :eax 1)
  (sys.lap-x86:xchg16 :cx :cx)
  (sys.lap-x86:ret))

(defun make-function-with-fixups (tag machine-code fixups constants gc-info)
  (with-interrupts-disabled ()
    (let* ((mc-size (ceiling (+ (length machine-code) 12) 16))
           (gc-info-size (ceiling (length gc-info) 8))
           (pool-size (length constants))
           (total (+ (* mc-size 2) pool-size gc-info-size)))
      (when (oddp total)
        (incf total))
      (let ((address (%raw-allocate total :static)))
        (%%add-function-to-bochs-debugger address
                                          (+ (length machine-code) 12)
                                          (aref constants 0))
        ;; Initialize header.
        (setf (memref-unsigned-byte-64 address 0) 0
              (memref-unsigned-byte-16 address 0) tag
              (memref-unsigned-byte-16 address 1) mc-size
              (memref-unsigned-byte-16 address 2) pool-size
              (memref-unsigned-byte-16 address 3) (length gc-info))
        ;; Initialize code.
        (dotimes (i (length machine-code))
          (setf (memref-unsigned-byte-8 address (+ i 12)) (aref machine-code i)))
        ;; Apply fixups.
        (dolist (fixup fixups)
          (let ((value (cond ((member (car fixup) '(nil t))
                              (lisp-object-address (car fixup)))
                             ((eql (car fixup) 'undefined-function)
                              (lisp-object-address *undefined-function-thunk*))
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
        (%%assemble-value address +tag-function+)))))

(defun make-function (machine-code constants gc-info)
  (make-function-with-fixups +function-type-function+ machine-code '() constants gc-info))

(defun make-closure (function environment)
  "Allocate a closure object."
  (check-type function function)
  (with-interrupts-disabled ()
    (let ((address (%raw-allocate 6 :static)))
      ;; Initialize and clear constant slots.
      ;; Function tag, flags and MC size.
      (setf (memref-unsigned-byte-32 address 0) #x00020001
            ;; Constant pool size and slot count.
            (memref-unsigned-byte-32 address 1) #x00000002
            (memref-unsigned-byte-32 address 2) #x00000000
            ;; The code.
            ;; mov64 :rbx (:rip 21)/pool[1]
            (memref-unsigned-byte-32 address 3) #x151D8B48
            ;; jmp (:rip 7)/pool[0]
            (memref-unsigned-byte-32 address 4) #xFF000000
            (memref-unsigned-byte-32 address 5) #x00000725
            (memref-unsigned-byte-32 address 6) #xCCCCCC00
            (memref-unsigned-byte-32 address 7) #xCCCCCCCC)
      (let ((value (%%assemble-value address +tag-function+)))
        ;; Initialize constant pool
        (setf (memref-t address 4) function
              (memref-t address 5) environment)
        value))))

(defun allocate-funcallable-std-instance (function class slots)
  "Allocate a funcallable instance."
  (check-type function function)
  (with-interrupts-disabled ()
    (let ((address (%raw-allocate 8 :static)))
      ;; Initialize and clear constant slots.
      ;; Function tag, flags and MC size.
      (setf (memref-unsigned-byte-32 address 0) #x00020003
            ;; Constant pool size and slot count.
            (memref-unsigned-byte-32 address 1) #x00000003
            (memref-unsigned-byte-32 address 2) #x00000000
            ;; The code.
            ;; jmp (:rip 13)/pool[0]
            (memref-unsigned-byte-32 address 3) #x000E25FF
            (memref-unsigned-byte-32 address 4) #xCCCC0000
            (memref-unsigned-byte-32 address 5) #xCCCCCCCC
            (memref-unsigned-byte-32 address 6) #xCCCCCCCC
            (memref-unsigned-byte-32 address 7) #xCCCCCCCC)
      (let ((value (%%assemble-value address +tag-function+)))
        ;; Initialize constant pool
        (setf (memref-t address 4) function
              (memref-t address 5) class
              (memref-t address 6) slots)
        value))))

(defun %make-array-header (dimensions fill-pointer info storage &optional area)
  (with-interrupts-disabled ()
    (let ((value (%%assemble-value (%raw-allocate 4 area) +tag-array-header+)))
      (setf (%array-header-dimensions value) dimensions
            (%array-header-fill-pointer value) fill-pointer
            (%array-header-info value) info
            (%array-header-storage value) storage)
      value)))

(defun make-symbol-in-area (name &optional area)
  (check-type name string)
  (with-interrupts-disabled ()
    (let* ((simp-name (sys.int::simplify-string name))
           (address (%raw-allocate 6 area))
           (symbol (%%assemble-value address +tag-symbol+)))
      ;; symbol-name.
      (setf (memref-t address 0) simp-name
            ;; Must be done before makunbound to prevent random
            ;; TLS slots from being smashed.
            (%symbol-flags symbol) 0)
      (makunbound symbol)
      (%fmakunbound symbol)
      (setf (symbol-plist symbol) nil
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
  (sys.lap-x86:mov64 :rcx 8) ; fixnum 1
  (sys.lap-x86:mov64 :r8 16) ; fixnum 2
  (sys.lap-x86:mov64 :r13 (:constant %make-bignum-of-length))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:pop (:r8 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:pop (:r8 #.(+ (- +tag-array-like+) 16)))
  (sys.lap-x86:mov32 :ecx 8) ; fixnum 1
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
  (sys.lap-x86:mov64 :rcx 8) ; fixnum 1
  (sys.lap-x86:mov64 :r8 8) ; fixnum 1
  (sys.lap-x86:mov64 :r13 (:constant %make-bignum-of-length))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:pop (:r8 #.(+ (- +tag-array-like+) 8)))
  (sys.lap-x86:mov32 :ecx 8) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret))

;;; This is used by the bignum code so that bignums and fixnums don't have
;;; to be directly compared.
(defun %make-bignum-from-fixnum (n)
  (with-interrupts-disabled ()
    (let* ((address (%raw-allocate 2 :static)))
      (setf (memref-unsigned-byte-64 address 0) (logior (ash 1 +array-length-shift+)
                                                        (ash +array-type-bignum+ +array-type-shift+))
            (memref-signed-byte-64 address 1) n)
      (%%assemble-value address +tag-array-like+))))

(defun %make-bignum-of-length (words)
  (with-interrupts-disabled ()
    (let* ((address (%raw-allocate (+ 1 words (if (logtest words 1) 0 1)) :static)))
      (setf (memref-unsigned-byte-64 address 0) (logior (ash words +array-length-shift+)
                                                        (ash +array-type-bignum+ +array-type-shift+)))
      (%%assemble-value address +tag-array-like+))))

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

(defun allocate-dma-buffer (length &optional (bitsize 8) signedp)
  (check-type bitsize (member 8 16 32 64))
  (with-interrupts-disabled ()
    (unless (zerop (logand *bump-pointer* #xFFF))
      (incf *bump-pointer* (- #x1000 (logand *bump-pointer* #xFFF))))
    (let ((address *bump-pointer*))
      (incf *bump-pointer* length)
      (unless (zerop (logand *bump-pointer* #xFFF))
        (incf *bump-pointer* (- #x1000 (logand *bump-pointer* #xFFF))))
      (values (make-array (truncate length (truncate bitsize 8))
                          :element-type (list (if signedp
                                                  'signed-byte
                                                  'unsigned-byte)
                                              bitsize)
                          :memory address)
              (- address #x8000000000)))))

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
