(defpackage #:cold-generator
  (:use #:cl #:iterate #:nibbles))

(in-package #:cold-generator)

(defparameter *source-files*
  '("../cold/test.lisp"
    "../gc.lisp"
    "../runtime-array.lisp"
    "../runtime-numbers.lisp"
    "../runtime-support.lisp"
    "../stack-group.lisp"
    "../bootstrap/setf.lisp"
    "../bootstrap/setf-full.lisp"
    "../bootstrap/string.lisp"
    "../bootstrap/type.lisp"
    "../bootstrap/sequence.lisp"
    "../bootstrap/array.lisp"
    "../bootstrap/numbers.lisp"
    "../bootstrap/defstruct.lisp"
    "../bootstrap/hash-table.lisp"
    "../bootstrap/early-cons.lisp"
    "../bootstrap/reader.lisp"
    "../bootstrap/backquote.lisp"
    "../character.lisp"
    "../printer.lisp"
    "../cold-stream.lisp"
    "../cold/eval.lisp"
    "../cold/load.lisp"
    "../format.lisp"
    "../type.lisp"
    "../interrupt.lisp"
    "../memory.lisp"
    "../dump.lisp"
    "../bootstrap/cons-compiler-macros.lisp"
    "../bootstrap/defmacro.lisp"
    "../bootstrap/basic-macros.lisp"
    "../bootstrap/data-types.lisp"
    "../bootstrap/parse.lisp"
    "../pci.lisp"))

(defparameter *special-source-files*
  '(("../packages.lisp" *package-system*)))

(defparameter *warm-source-files*
  '("../closette.lisp"
    "../runtime-misc.lisp"
    "../bootstrap/condition.lisp"
    "../bootstrap/restarts.lisp"
    "../bootstrap/error.lisp"
    "../debug.lisp"
    "../eval.lisp"
    "../stream.lisp"
    "../process.lisp"
    "../lap.lisp"
    "../lap-x86.lisp"
    "../compiler/package.lisp"
    "../compiler/compiler.lisp"
    "../compiler/pass1.lisp"
    "../compiler/constprop.lisp"
    "../compiler/simplify.lisp"
    "../compiler/lift.lisp"
    "../compiler/inline.lisp"
    "../compiler/codegen.lisp"
    "../compiler/builtins.lisp"
    "../interrupt-compiler.lisp"
    "../keyboard.lisp"
    "../pci.lisp"
    "../framebuffer.lisp"
    "../bochs-vbe.lisp"
    "../ethernet.lisp"
    "../rtl8139.lisp"
    "../graphics.lisp"
    "../peek.lisp"
    "../telnet.lisp"
    "../irc.lisp"))

(defun compile-warm-source (&optional force)
  (dolist (file *warm-source-files*)
    (let ((llf-path (merge-pathnames (make-pathname :type "llf" :defaults file))))
      (when (or (not (probe-file llf-path))
                (<= (file-write-date llf-path) (file-write-date file))
                force)
        (format t "~A is out of date will be recompiled.~%" llf-path)
        (sys.c::cross-compile-file file)))))

(defconstant +tag-even-fixnum+   #b0000)
(defconstant +tag-cons+          #b0001)
(defconstant +tag-symbol+        #b0010)
(defconstant +tag-array-header+  #b0011)
(defconstant +tag-std-instance+  #b0100)
;;(defconstant +tag-+  #b0101)
;;(defconstant +tag-+  #b0110)
(defconstant +tag-array-like+    #b0111)
(defconstant +tag-odd-fixnum+    #b1000)
;;(defconstant +tag-+  #b1001)
(defconstant +tag-character+     #b1010)
(defconstant +tag-single-float+  #b1011)
(defconstant +tag-function+      #b1100)
;;(defconstant +tag-+  #b1101)
(defconstant +tag-unbound-value+ #b1110)
(defconstant +tag-gc-forward+    #b1111)

(defparameter *undefined-function-thunk*
  `((sys.lap-x86:cmp64 :rcx ,(* 5 8))
    (sys.lap-x86:jl register-args-only)
    ;; Have to spill R12 on the stack.
    (sys.lap-x86:mov64 (:lsp -8) nil)
    (sys.lap-x86:sub64 :lsp 8)
    (sys.lap-x86:mov64 (:lsp) :r12)
    register-args-only
    ;; Shuffle registers up.
    (sys.lap-x86:mov64 :r12 :r11)
    (sys.lap-x86:mov64 :r11 :r10)
    (sys.lap-x86:mov64 :r10 :r8)
    (sys.lap-x86:mov64 :r9 :r8)
    ;; Pass invoked-through as the first argument.
    (sys.lap-x86:mov64 :r8 :r13)
    (sys.lap-x86:add64 :rcx ,(* 1 8))
    ;; Tail call through to RAISE-UNDEFINED-FUNCTION and let that
    ;; handle the heavy work.
    (sys.lap-x86:mov64 :r13 (:constant raise-undefined-function))
    (sys.lap-x86:jmp (:symbol-function :r13)))
  "Code for the undefined function thunk.")

(defparameter *setup-function*
  `((sys.lap-x86:!code32)
    (sys.lap-x86:mov32 :esp initial-stack)
    ;; Compute the start of the function.
    (sys.lap-x86:call get-eip)
    get-eip
    (sys.lap-x86:pop :esi)
    ;; Set ESI to the start of the function.
    (sys.lap-x86:sub32 :esi get-eip)
    ;; Enable long mode.
    (sys.lap-x86:movcr :eax :cr4)
    (sys.lap-x86:or32 :eax #x000000A0)
    (sys.lap-x86:movcr :cr4 :eax)
    (sys.lap-x86:mov32 :eax initial-page-table)
    (sys.lap-x86:movcr :cr3 :eax)
    (sys.lap-x86:mov32 :ecx #xC0000080)
    (sys.lap-x86:rdmsr)
    (sys.lap-x86:or32 :eax #x00000100)
    (sys.lap-x86:wrmsr)
    (sys.lap-x86:movcr :eax :cr0)
    (sys.lap-x86:or32 :eax #x80000000)
    (sys.lap-x86:movcr :cr0 :eax)
    (sys.lap-x86:lgdt (:esi gdtr))
    (sys.lap-x86:lidt (:esi idtr))
    ;; There was a far jump here, but that's hard to make position-independent.
    (sys.lap-x86:push #x0008)
    (sys.lap-x86:lea32 :eax (:esi long64))
    (sys.lap-x86:push :eax)
    (sys.lap-x86:retf)
    (sys.lap-x86:!code64)
    long64
    (sys.lap-x86:xor32 :eax :eax)
    (sys.lap-x86:movseg :ds :eax)
    (sys.lap-x86:movseg :es :eax)
    (sys.lap-x86:movseg :fs :eax)
    (sys.lap-x86:movseg :gs :eax)
    (sys.lap-x86:movseg :ss :eax)
    ;; Save the multiboot pointer.
    (sys.lap-x86:mov32 :ebx :ebx)
    (sys.lap-x86:shl64 :rbx 3)
    (sys.lap-x86:mov64 :r8 (:constant *multiboot-info*))
    (sys.lap-x86:mov64 (:symbol-value :r8) :rbx)
    ;; SSE init.
    ;; Set CR4.OSFXSR and CR4.OSXMMEXCPT.
    (sys.lap-x86:movcr :rax :cr4)
    (sys.lap-x86:or64 :rax #x00000600)
    (sys.lap-x86:movcr :cr4 :rax)
    ;; Clear CR0.EM and set CR0.MP.
    (sys.lap-x86:movcr :rax :cr0)
    (sys.lap-x86:and64 :rax -5)
    (sys.lap-x86:or64 :rax #x00000002)
    (sys.lap-x86:movcr :cr0 :rax)
    ;; Clear FPU/SSE state.
    (sys.lap-x86:push #x1F80)
    (sys.lap-x86:ldmxcsr (:rsp))
    (sys.lap-x86:add64 :rsp 8)
    (sys.lap-x86:fninit)
    ;; Preset the initial stack group.
    (sys.lap-x86:mov64 :r8 (:constant *initial-stack-group*))
    (sys.lap-x86:mov64 :r8 (:symbol-value :r8))
    (sys.lap-x86:mov64 :csp (:r8 ,(- (* 5 8) +tag-array-like+)))
    (sys.lap-x86:add64 :csp (:r8 ,(- (* 6 8) +tag-array-like+)))
    (sys.lap-x86:mov64 :lsp (:r8 ,(- (* 7 8) +tag-array-like+)))
    (sys.lap-x86:add64 :lsp (:r8 ,(- (* 8 8) +tag-array-like+)))
    ;; Clear binding stack.
    (sys.lap-x86:mov64 :rdi (:r8 ,(- (* 9 8) +tag-array-like+)))
    (sys.lap-x86:mov64 :rcx (:r8 ,(- (* 10 8) +tag-array-like+)))
    (sys.lap-x86:sar64 :rcx 3)
    (sys.lap-x86:xor32 :eax :eax)
    (sys.lap-x86:rep)
    (sys.lap-x86:stos64)
    ;; Set the binding stack pointer.
    (sys.lap-x86:mov64 (:r8 ,(- (* 1 8) +tag-array-like+)) :rdi)
    ;; Clear TLS binding slots.
    (sys.lap-x86:lea64 :rdi (:r8 ,(- (* 12 8) +tag-array-like+)))
    (sys.lap-x86:mov64 :rax -2)
    (sys.lap-x86:mov32 :ecx 500)
    (sys.lap-x86:rep)
    (sys.lap-x86:stos64)
    ;; Mark the SG as running/unsafe.
    (sys.lap-x86:mov64 (:r8 ,(- (* 2 8) +tag-array-like+)) 0)
    ;; Initialize GS.
    (sys.lap-x86:mov64 :rax :r8)
    (sys.lap-x86:mov64 :rdx :r8)
    (sys.lap-x86:sar64 :rdx 32)
    (sys.lap-x86:mov64 :rcx #xC0000101)
    (sys.lap-x86:wrmsr)
    ;; Clear frame pointers.
    (sys.lap-x86:mov64 :cfp 0)
    (sys.lap-x86:mov64 :lfp 0)
    ;; Clear data registers.
    (sys.lap-x86:xor32 :r8d :r8d)
    (sys.lap-x86:xor32 :r9d :r9d)
    (sys.lap-x86:xor32 :r10d :r10d)
    (sys.lap-x86:xor32 :r11d :r11d)
    (sys.lap-x86:xor32 :r12d :r12d)
    (sys.lap-x86:xor32 :ebx :ebx)
    ;; Prepare for call.
    (sys.lap-x86:mov64 :r13 (:constant initialize-lisp))
    (sys.lap-x86:xor32 :ecx :ecx)
    ;; Call the entry function.
    (sys.lap-x86:call (:symbol-function :r13))
    ;; Crash if it returns.
    here
    (sys.lap-x86:ud2)
    (sys.lap-x86:jmp here)
    #+nil(:align 4) ; TODO!! ######
    gdtr
    (:d16/le gdt-length)
    (:d32/le gdt)
    idtr
    (:d16/le idt-length)
    (:d32/le idt)))

(defparameter *static-area-limit* (* 64 1024 1024))
(defparameter *dynamic-area-semispace-limit* (* 32 1024 1024))

(defparameter *static-area-base*  #x0000200000)
(defparameter *static-area-size*  (- #x0080000000 *static-area-base*))
(defparameter *dynamic-area-base* #x0080000000)
(defparameter *dynamic-area-size* #x0080000000) ; 2GB
(defparameter *stack-area-base*   #x0100000000)
(defparameter *stack-area-size*   #x0040000000) ; 1GB
(defparameter *linear-map*        #x8000000000)
(defparameter *physical-load-address* #x0000200000)

(defvar *support-offset*)
(defvar *static-offset*)
(defvar *dynamic-offset*)
(defvar *stack-offset*)

(defvar *support-data*)
(defvar *static-data*)
(defvar *dynamic-data*)

(defvar *symbol-table*)
(defvar *keyword-table*)
(defvar *setf-table*)
(defvar *struct-table*)
(defvar *undefined-function-address*)
(defvar *load-time-evals*)

(defvar *function-map*)
(defvar *pending-fixups*)

(defun allocate (word-count &optional (area :dynamic))
  (when (oddp word-count) (incf word-count))
  (ecase area
    (:dynamic
     (prog1 (+ (truncate *dynamic-area-base* 8) *dynamic-offset*)
       (incf *dynamic-offset* word-count)
       (when (> *dynamic-offset* (truncate (/ *dynamic-area-size* 2) 8))
         (error "Allocation of ~S words exceeds dynamic area size." word-count))))
    (:static
     (let ((address (+ (truncate *static-area-base* 8) *static-offset* 2)))
       (incf *static-offset* (+ word-count 2))
       (when (> *static-offset* (truncate *static-area-size* 8))
         (error "Allocation of ~S words exceeds static area size." word-count))
       (setf (ldb (byte 1 1) (word (- address 1))) 1
             (word (- address 2)) word-count)
       address))
    (:support
     ;; Force alignment.
     (unless (zerop (logand word-count #x1FF))
       (incf word-count #x1FF)
       (decf word-count (logand word-count #x1FF)))
     (prog1 (+ (truncate (+ *linear-map* *physical-load-address* #x200000) 8)
               *support-offset*)
       (incf *support-offset* word-count)))))

(defun allocate-stack (size style)
  (check-type style (member :control :data :binding))
  ;; Force alignment.
  (unless (zerop (logand size #x1FF))
    (incf size #x1FF)
    (decf size (logand size #x1FF)))
  (prog1 (+ (truncate *stack-area-base* 8) *stack-offset*)
    (incf *stack-offset* size)
    (when (> *stack-offset* (truncate (/ *stack-area-size* 2) 8))
       (error "Allocation of ~S words exceeds stack area size." size))))

(defun storage-info-for-address (address)
  (let ((byte-address (* address 8)))
    (cond ((<= *static-area-base* byte-address (+ *static-area-base* *static-area-size* -1))
           (let ((offset (- address (truncate *static-area-base* 8))))
             (assert (< offset *static-offset*))
             (when (>= (* offset 8) (length *static-data*))
               (adjust-array *static-data* (* (ceiling *static-offset* #x40000) #x200000)))
             (values offset *static-data*)))
          ((<= *dynamic-area-base* byte-address (+ *dynamic-area-base* (/ *dynamic-area-size* 2)))
           (let ((offset (- address (truncate *dynamic-area-base* 8))))
             (assert (< offset *dynamic-offset*))
             (when (>= (* offset 8) (length *dynamic-data*))
               (adjust-array *dynamic-data* (* (ceiling *dynamic-offset* #x40000) #x200000)))
             (values offset *dynamic-data*)))
          ((<= (+ *linear-map* *physical-load-address* #x200000)
               byte-address
               (+ *linear-map* *physical-load-address* #x200000 (* *support-offset* 8)))
           (let ((offset (- address (truncate (+ *linear-map* *physical-load-address* #x200000) 8))))
             (when (>= (* offset 8) (length *support-data*))
               (format t "Resizing support-data to ~S bytes, while accessing address ~X (offset ~X) ~
                          support-offset is ~X~%"
                       (* (ceiling *support-offset* #x40000) #x200000) address offset (* *support-offset* 8))
               (adjust-array *support-data* (* (ceiling *support-offset* #x40000) #x200000)))
             (values offset *support-data*)))
          (t (error "Unknown address #x~X.~%" address)))))

(defun (setf word) (new-value address)
  (multiple-value-bind (offset data-vector)
      (storage-info-for-address address)
    (setf (ub64ref/le data-vector (* offset 8)) new-value)))

(defun word (address)
  (multiple-value-bind (offset data-vector)
      (storage-info-for-address address)
    (ub64ref/le data-vector (* offset 8))))

(defun compile-lap-function (code &optional (area :static) extra-symbols constant-values)
  "Compile a list of LAP code as a function. Constants must by symbols only."
  (multiple-value-bind (mc constants fixups)
      (sys.lap-x86:assemble (list* (list :d32/le 0 0 0) code) ; 12 byte header.
	:base-address 0
        :initial-symbols (list* '(nil . :fixup)
                                '(t . :fixup)
                                extra-symbols))
    (setf mc (adjust-array mc (* (ceiling (length mc) 16) 16) :fill-pointer t))
    (let ((total-size (+ (* (truncate (length mc) 16) 2)
                         (length constants))))
      (when (oddp total-size) (incf total-size))
      (let ((address (allocate total-size area)))
        ;; Copy machine code into the area.
        (dotimes (i (truncate (length mc) 8))
          (setf (word (+ address i)) (nibbles:ub64ref/le mc (* i 8))))
        ;; Set header word.
        (setf (word address) (logior (ash (truncate (length mc) 16) 16)
                                     (ash (length constants) 32)))
        ;; Write constant pool.
        (dotimes (i (length constants))
          (cond ((assoc (aref constants i) constant-values)
                 (setf (word (+ address
                                (truncate (length mc) 8)
                                i))
                       (cdr (assoc (aref constants i) constant-values))))
                (t (check-type (aref constants i) symbol)
                   (push (list (list 'quote (aref constants i))
                               (+ address
                                  (truncate (length mc) 8)
                                  i)
                               0
                               :full64)
                         *pending-fixups*))))
        (dolist (fixup fixups)
          (push (list (car fixup) address (cdr fixup) :signed32)
                *pending-fixups*))
        address))))

(defun make-value (address tag)
  (logior (* address 8) tag))

(defun pointer-part (value)
  (ash (ldb (byte 64 4) value) 1))

(defun tag-part (value)
  (ldb (byte 4 0) value))

(defun make-fixnum (value)
  (check-type value (signed-byte 61))
  (ldb (byte 64 0) (ash value 3)))

(defconstant +array-type-t+ 0)
(defconstant +array-type-base-char+ 1)
(defconstant +array-type-character+ 2)
(defconstant +array-type-bit+ 3)
(defconstant +array-type-unsigned-byte-2+ 4)
(defconstant +array-type-unsigned-byte-4+ 5)
(defconstant +array-type-unsigned-byte-8+ 6)
(defconstant +array-type-unsigned-byte-16+ 7)
(defconstant +array-type-unsigned-byte-32+ 8)
(defconstant +array-type-unsigned-byte-64+ 9)
(defconstant +array-type-signed-byte-1+ 10)
(defconstant +array-type-signed-byte-2+ 11)
(defconstant +array-type-signed-byte-4+ 12)
(defconstant +array-type-signed-byte-8+ 13)
(defconstant +array-type-signed-byte-16+ 14)
(defconstant +array-type-signed-byte-32+ 15)
(defconstant +array-type-signed-byte-64+ 16)
(defconstant +array-type-single-float+ 17)
(defconstant +array-type-double-float+ 18)
(defconstant +array-type-long-float+ 19)
(defconstant +array-type-xmm-vector+ 20)
(defconstant +array-type-complex-single-float+ 21)
(defconstant +array-type-complex-double-float+ 22)
(defconstant +array-type-complex-long-float+ 23)
(defconstant +last-array-type+ 23)
(defconstant +array-type-bignum+ 25)
(defconstant +array-type-stack-group+ 30)
(defconstant +array-type-struct+ 31)

(defun store-string (string &optional (area :dynamic))
  (let ((address (allocate (1+ (ceiling (length string) 8)) area)))
    ;; Header word.
    (setf (word address) (logior (ash (length string) 8) (ash +array-type-base-char+ 1)))
    (dotimes (i (ceiling (length string) 8))
      (let ((value 0))
        (dotimes (j 8)
          (when (< (+ (* i 8) j) (length string))
            (setf (ldb (byte 8 64) value) (char-code (char string (+ (* i 8) j)))))
          (setf value (ash value -8)))
        (setf (word (+ address 1 i)) value)))
    address))

(defun symbol-address (name keywordp &optional (createp t))
  (or (gethash name (if keywordp *keyword-table* *symbol-table*))
      (when (not createp)
        (error "Symbol ~A~A does not exist."
               (if keywordp #\: "")
               name))
      (let ((address (allocate 6 :static)))
        (setf (word address) (make-value (store-string name)
                                         +tag-array-like+)
              (word (+ address 1)) (make-value (gethash "T" *symbol-table*) +tag-symbol+)
              (word (+ address 2)) (if keywordp
                                       (make-value address +tag-symbol+)
                                       (make-value 0 +tag-unbound-value+))
              (word (+ address 3)) (make-value *undefined-function-address* +tag-function+)
              (word (+ address 4)) (make-value (gethash "NIL" *symbol-table*) +tag-symbol+)
              ;; fixme, keywords should be constant.
              (word (+ address 5)) (make-fixnum 0))
        (setf (gethash name (if keywordp *keyword-table* *symbol-table*)) address))))

(defun resolve-setf-symbol (symbol)
  (or (gethash (symbol-name symbol) *setf-table*)
      (let ((name (symbol-name symbol))
            (address (allocate 6)))
        (setf (word address) (make-value (store-string name)
                                         +tag-array-like+)
              (word (+ address 1)) (make-value (gethash "NIL" *symbol-table*) +tag-symbol+)
              (word (+ address 2)) (make-value 0 +tag-unbound-value+)
              (word (+ address 3)) (make-value *undefined-function-address* +tag-function+)
              (word (+ address 4)) (make-value (gethash "NIL" *symbol-table*) +tag-symbol+)
              (word (+ address 5)) (make-fixnum 0))
        (setf (gethash name *setf-table*) address))))

;; fixme, nil and t should be constant.
(defun create-support-objects ()
  "Create NIL, T and the undefined function thunk."
  (let ((nil-value (allocate 6 :static))
        (t-value (allocate 6 :static))
        (undef-fn (compile-lap-function *undefined-function-thunk* :static)))
    (setf (word nil-value) (make-value (store-string "NIL")
                                         +tag-array-like+)
          (word (+ nil-value 1)) (make-value t-value +tag-symbol+)
          (word (+ nil-value 2)) (make-value nil-value +tag-symbol+)
          (word (+ nil-value 3)) (make-value undef-fn +tag-function+)
          (word (+ nil-value 4)) (make-value nil-value +tag-symbol+)
          (word (+ nil-value 5)) (make-fixnum 0))
    (setf (word t-value) (make-value (store-string "T")
                                       +tag-array-like+)
          (word (+ t-value 1)) (make-value t-value +tag-symbol+)
          (word (+ t-value 2)) (make-value t-value +tag-symbol+)
          (word (+ t-value 3)) (make-value undef-fn +tag-function+)
          (word (+ t-value 4)) (make-value nil-value +tag-symbol+)
          (word (+ t-value 5)) (make-fixnum 0))
    (format t "NIL at word ~X~%" nil-value)
    (format t "  T at word ~X~%" t-value)
    (format t "UDF at word ~X~%" undef-fn)
    (setf (gethash "NIL" *symbol-table*) nil-value
          (gethash "T" *symbol-table*) t-value
          *undefined-function-address* undef-fn)))

(defun write-image (name description)
  (declare (ignore description))
  (with-open-file (s (format nil "~A.image" name)
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
    ;; Must match the page-table code!
    ;; First comes the first 2MB of the static area.
    (write-sequence *static-data* s :end #x200000)
    ;; Then the support area.
    (write-sequence *support-data* s)
    ;; Then the rest of the static area.
    (write-sequence *static-data* s :start #x200000)
    ;; Finally, the dynamic area.
    (write-sequence *dynamic-data* s))
  (values))

(defun total-image-size ()
  (+ (/ *static-area-limit* 8)
     (/ (* *dynamic-area-semispace-limit* 2) 8)
     (* (ceiling *support-offset* #x40000) #x40000)
     (* (1+ (ceiling *stack-offset* #x40000)) #x40000)))

(defun image-data-size ()
  (+ (* (ceiling *static-offset* #x40000) #x40000)
     (* (ceiling *dynamic-offset* #x40000) #x40000)
     (* (ceiling *support-offset* #x40000) #x40000)))

(defun array-header (tag length)
  (logior (ash tag 1)
          (ash length 8)))

(defun pack-halfwords (low high)
  (check-type low (unsigned-byte 32))
  (check-type high (unsigned-byte 32))
  (dpb high (byte 32 32) low))

(defconstant +page-table-present+  #b0000000000001)
(defconstant +page-table-writable+ #b0000000000010)
(defconstant +page-table-user+     #b0000000000100)
(defconstant +page-table-pwt+      #b0000000001000)
(defconstant +page-table-pcd+      #b0000000010000)
(defconstant +page-table-accessed+ #b0000000100000)
(defconstant +page-table-dirty+    #b0000001000000)
(defconstant +page-table-large+    #b0000010000000)
(defconstant +page-table-global+   #b0000100000000)
(defconstant +page-table-pat+      #b1000000000000)

(defun create-page-tables ()
  (let ((pml4 (allocate 512 :support))
        (data-pml3 (allocate 512 :support))
        (phys-pml3 (allocate 512 :support))
        (data-pml2 (allocate (* 512 512) :support))
        (phys-pml2 (allocate (* 512 512) :support))
        (phys-curr *physical-load-address*))
    (format t "PML4 at ~X~%" (* pml4 8))
    (format t "Data PML3 at ~X. PML2s at ~X~%" (* data-pml3 8) (* data-pml2 8))
    (format t "Phys PML3 at ~X. PML2s at ~X~%" (* phys-pml3 8) (* phys-pml2 8))
    (dotimes (i 512)
      ;; Clear PML4.
      (setf (word (+ pml4 i)) 0)
      ;; Link PML3s to PML2s.
      (setf (word (+ data-pml3 i)) (logior (+ (- (* data-pml2 8) *linear-map*) (* i 4096))
                                           +page-table-present+
                                           +page-table-global+))
      (setf (word (+ phys-pml3 i)) (logior (+ (- (* phys-pml2 8) *linear-map*) (* i 4096))
                                           +page-table-present+
                                           +page-table-global+))
      (dotimes (j 512)
        ;; Clear the data PML2.
        (setf (word (+ data-pml2 (* i 512) j)) 0)
        ;; Map the physical PML2 to the first 512GB of physical memory.
        (setf (word (+ phys-pml2 (* i 512) j)) (logior (* (+ (* i 512) j) #x200000)
                                                       +page-table-present+
                                                       +page-table-global+
                                                       +page-table-large+))))
    ;; Link the PML4 to the PML3s.
    ;; Data PML3 at 0, physical PML3 at 512GB.
    (setf (word (+ pml4 0)) (logior (- (* data-pml3 8) *linear-map*)
                                    +page-table-present+
                                    +page-table-global+))
    (setf (word (+ pml4 1)) (logior (- (* phys-pml3 8) *linear-map*)
                                    +page-table-present+
                                    +page-table-global+))
    ;; Identity map the first 2MB of static space.
    (setf (word (+ data-pml2 (truncate phys-curr #x200000)))
          (logior phys-curr
                  +page-table-present+
                  +page-table-global+
                  +page-table-large+))
    (incf phys-curr #x200000)
    ;; Skip over the support area.
    (incf phys-curr (* #x200000 (ceiling *support-offset* #x40000)))
    ;; The rest of the static area.
    (dotimes (i (1- (ceiling *static-offset* #x40000)))
      (setf (word (+ data-pml2 1 (truncate *static-area-base* #x200000) i))
            (logior phys-curr
                  +page-table-present+
                  +page-table-global+
                  +page-table-large+))
      (incf phys-curr #x200000))
    ;; The dynamic area.
    (dotimes (i (ceiling *dynamic-offset* #x40000))
      (setf (word (+ data-pml2 (truncate *dynamic-area-base* #x200000) i))
            (logior phys-curr
                  +page-table-present+
                  +page-table-global+
                  +page-table-large+))
      (incf phys-curr #x200000))
    ;; Now map any left-over memory in dynamic/static space in at the end using the BSS.
    (dotimes (i (truncate (- (/ *static-area-limit* 8) *static-offset*) #x40000))
      (setf (word (+ data-pml2 (truncate *static-area-base* #x200000) (ceiling *static-offset* #x40000) i))
            (logior phys-curr
                  +page-table-present+
                  +page-table-global+
                  +page-table-large+))
      (incf phys-curr #x200000))
    (dotimes (i (truncate (- (/ *dynamic-area-semispace-limit* 8) *dynamic-offset*) #x40000))
      (setf (word (+ data-pml2 (truncate *dynamic-area-base* #x200000) (ceiling *dynamic-offset* #x40000) i))
            (logior phys-curr
                  +page-table-present+
                  +page-table-global+
                  +page-table-large+))
      (incf phys-curr #x200000))
    ;; And future-newspace also comes from the BSS.
    (dotimes (i (ceiling (/ *dynamic-area-semispace-limit* 8) #x40000))
      (setf (word (+ data-pml2 (truncate *dynamic-area-base* #x200000) (ceiling (/ *dynamic-area-size* 2) #x200000) i))
            (logior phys-curr
                    +page-table-present+
                    +page-table-global+
                    +page-table-large+))
      (incf phys-curr #x200000))
    ;; As does stack space.
    (dotimes (i (1+ (ceiling *stack-offset* #x40000)))
      (setf (word (+ data-pml2 (truncate *stack-area-base* #x200000) i))
            (logior phys-curr
                    +page-table-present+
                    +page-table-global+
                    +page-table-large+))
      (incf phys-curr #x200000))
    (values (- (* pml4 8) *linear-map*) (* data-pml3 8))))

(defun create-initial-stack-group ()
  (let* ((address (allocate 512 :static))
         (control-stack-size 2048)
         (control-stack (allocate-stack control-stack-size :control))
         (data-stack-size 2048)
         (data-stack (allocate-stack data-stack-size :data))
         (binding-stack-size 1024)
         (binding-stack (allocate-stack binding-stack-size :binding)))
    ;; Array tag.
    (setf (word (+ address 0)) (array-header +array-type-stack-group+ 511))
    ;; Binding stack pointer.
    (setf (word (+ address 1)) (make-fixnum (+ binding-stack binding-stack-size)))
    ;; State word. Unsafe, active.
    (setf (word (+ address 2)) (make-fixnum 0))
    ;; Saved control stack pointer.
    (setf (word (+ address 3)) (make-fixnum (+ control-stack control-stack-size)))
    ;; Name.
    (setf (word (+ address 4)) (make-value (store-string "Initial stack group") +tag-array-like+))
    ;; Control stack base.
    (setf (word (+ address 5)) (make-fixnum control-stack))
    ;; Control stack size.
    (setf (word (+ address 6)) (make-fixnum control-stack-size))
    ;; Data stack base.
    (setf (word (+ address 7)) (make-fixnum data-stack))
    ;; Data stack size.
    (setf (word (+ address 8)) (make-fixnum data-stack-size))
    ;; Binding stack base.
    (setf (word (+ address 9)) (make-fixnum binding-stack))
    ;; Binding stack size.
    (setf (word (+ address 10)) (make-fixnum binding-stack-size))
    ;; Resumer.
    (setf (word (+ address 12)) (make-value (symbol-address "NIL" nil) +tag-symbol+))
    ;; Start of TLS slots.
    (dotimes (i (- 512 13))
      (setf (word (+ address 13 i)) #xFFFFFFFFFFFFFFFE))
    (setf (word (+ (symbol-address "*INITIAL-STACK-GROUP*" nil) 2))
          (make-value address +tag-array-like+))))

(defun (setf cold-symbol-value) (value symbol)
  (setf (word (+ (symbol-address (symbol-name symbol) (keywordp symbol)) 2)) value))

(defun generate-toplevel-form-array (functions symbol)
  ;; Generate array of toplevel forms to eval.
  (let* ((n (length functions))
         (toplevel-forms (allocate (1+ n))))
    (setf (word toplevel-forms) (array-header +array-type-t+ n))
    (iter (for i from 0)
          (for fn in functions)
          (setf (word (+ toplevel-forms 1 i)) fn))
    (setf (cold-symbol-value symbol) (make-value toplevel-forms +tag-array-like+))))

(defun generate-obarray (symtab target-symbol)
  (let ((obarray (allocate (1+ (hash-table-count symtab)))))
    (setf (word obarray) (array-header +array-type-t+ (hash-table-count symtab)))
    (iter (for (nil address) in-hashtable symtab)
          (for i from 0)
          (setf (word (+ obarray 1 i)) (make-value address +tag-symbol+)))
    (setf (cold-symbol-value target-symbol) (make-value obarray +tag-array-like+))))

(defun generate-struct-obarray (symtab target-symbol)
  (let ((obarray (allocate (1+ (hash-table-count symtab)))))
    (setf (word obarray) (array-header +array-type-t+ (hash-table-count symtab)))
    (iter (for (nil address) in-hashtable symtab)
          (for i from 0)
          (setf (word (+ obarray 1 i)) (make-value (first address) +tag-array-like+)))
    (setf (cold-symbol-value target-symbol) (make-value obarray +tag-array-like+))))

(defun generate-string-array (sequence target-symbol)
  (let ((obarray (allocate (1+ (length sequence)))))
    (setf (word obarray) (array-header +array-type-t+ (length sequence)))
    (iter (for object in-sequence sequence)
          (for i from 0)
          (setf (word (+ obarray 1 i)) (make-value (store-string object) +tag-array-like+)))
    (setf (cold-symbol-value target-symbol) (make-value obarray +tag-array-like+))))

(defun write-map-file (image-name map)
  (with-open-file (s (format nil "~A.map" image-name)
                     :direction :output
                     :if-exists :supersede)
    (iter (for (addr name) in (sort (copy-list map) '< :key 'first))
          (format s "~X ~A~%" (make-value addr +tag-function+) name))))

;; Ugh.
(defun load-compiler-builtins ()
  (sys.c::save-compiler-builtins "%%compiler-builtins.llf")
  (load-source-file "%%compiler-builtins.llf" t))

(defun save-ub8-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (ceiling (length vec) 8)) area)))
    (setf (word address) (array-header +array-type-unsigned-byte-8+ (length vec)))
    (dotimes (i (ceiling (length vec) 8))
      (let ((value 0))
        (dotimes (j 8)
          (when (< (+ (* i 8) j) (length vec))
            (setf (ldb (byte 8 64) value) (aref vec (+ (* i 8) j))))
          (setf value (ash value -8)))
        (setf (word (+ address 1 i)) value)))
    (make-value address +tag-array-like+)))

(defun save-ub32-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (ceiling (length vec) 2)) area)))
    (setf (word address) (array-header +array-type-unsigned-byte-32+ (length vec)))
    (dotimes (i (ceiling (length vec) 2))
      (let ((value 0))
        (dotimes (j 2)
          (when (< (+ (* i 2) j) (length vec))
            (setf (ldb (byte 32 64) value) (aref vec (+ (* i 2) j))))
          (setf value (ash value -32)))
        (setf (word (+ address 1 i)) value)))
    (make-value address +tag-array-like+)))

(defun save-ub64-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (length vec)) area)))
    (setf (word address) (array-header +array-type-unsigned-byte-64+ (length vec)))
    (iter (for x in-sequence vec)
          (for i from 1)
          (setf (word (+ address i)) x))
    (make-value address +tag-array-like+)))

(defun save-simple-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (length vec)) area)))
    (setf (word address) (array-header +array-type-t+ (length vec)))
    (iter (for x in-sequence vec)
          (for i from 1)
          (setf (word (+ address i)) (save-object x area)))
    (make-value address +tag-array-like+)))

(defun save-object (object &optional (area :dynamic))
  (etypecase object
    ((signed-byte 61) (make-fixnum object))
    (string (make-value (store-string object area)
                        +tag-array-like+))
    (cons (vcons (save-object (car object))
                 (save-object (cdr object))))
    (symbol (make-value (symbol-address (symbol-name object)
                                        (keywordp object))
                        +tag-symbol+))
    ((vector (unsigned-byte 8))
     (save-ub8-vector object area))
    ((vector (unsigned-byte 32))
     (save-ub32-vector object area))
    ((vector (unsigned-byte 64))
     (save-ub64-vector object area))
    ((vector t)
     (save-simple-vector object area))))

(defun save-unifont-data (path area)
  (let ((unifont-data (with-open-file (s path)
                        (build-unicode:generate-unifont-table s))))
    (save-object unifont-data area)))

(defun make-image (image-name &optional description extra-source-files)
  (let ((*support-offset* 0)
        (*static-offset* 0)
        (*dynamic-offset* 0)
        (*stack-offset* 0)
        (*support-data* (make-array #x200000 :element-type '(unsigned-byte 8) :adjustable t))
        (*static-data* (make-array #x200000 :element-type '(unsigned-byte 8) :adjustable t))
        (*dynamic-data* (make-array #x200000 :element-type '(unsigned-byte 8) :adjustable t))
        (*pending-fixups* '())
        (*symbol-table* (make-hash-table :test 'equal))
        (*keyword-table* (make-hash-table :test 'equal))
        (*setf-table* (make-hash-table :test 'equal))
        (*struct-table* (make-hash-table))
        (*undefined-function-address* nil)
        (*function-map* '())
        (setup-fn nil)
        (gdt nil)
        (idt nil)
        (multiboot nil)
        (initial-stack nil)
        (initial-pml4)
        (data-pml3)
        (cl-symbol-names (with-open-file (s "../cl-symbols.lisp-expr") (read s)))
        (system-symbol-names (with-open-file (s "../system-symbols.lisp-expr") (read s))))
    (create-support-objects)
    (setf multiboot (allocate 5 :static)
          initial-stack (allocate 8 :static)
          gdt (allocate 3 :static)
          idt (allocate 257 :static))
    ;; Create setup function.
    (setf setup-fn (compile-lap-function *setup-function* :static
                                         (list (cons 'gdt (* (1+ gdt) 8))
                                               (cons 'gdt-length (1- (* 2 8)))
                                               (cons 'idt (* (1+ idt) 8))
                                               (cons 'idt-length (1- (* 256 8)))
                                               (cons 'initial-stack (* (+ initial-stack 8) 8))
                                               ;; PML4 must be at this address
                                               (cons 'initial-page-table #x400000))))
    (setf (cold-symbol-value '*%setup-function*) (make-value setup-fn +tag-function+))
    (create-initial-stack-group)
    (let ((*load-time-evals* '()))
      (load-compiler-builtins)
      (load-source-files *source-files* t)
      (generate-toplevel-form-array (reverse *load-time-evals*) '*cold-toplevel-forms*))
    (iter (for (file symbol) in *special-source-files*)
          (let ((*load-time-evals* '()))
            (load-source-file file nil)
            (generate-toplevel-form-array (reverse *load-time-evals*) symbol)))
    (let ((*load-time-evals* '()))
      (load-source-files extra-source-files nil)
      (generate-toplevel-form-array (reverse *load-time-evals*) '*additional-cold-toplevel-forms*))
    (setf (cold-symbol-value '*unifont-bmp*) (save-unifont-data "../unifontfull-5.1.20080820.hex" :static))
    (multiple-value-bind (planes name-store encoding-table name-trie)
        (build-unicode:generate-unicode-data-tables (build-unicode:read-unicode-data "../UnicodeData.txt"))
      (setf (cold-symbol-value '*unicode-info*) (save-object planes :static)
            (cold-symbol-value '*unicode-name-store*) (save-ub8-vector name-store :static)
            (cold-symbol-value '*unicode-encoding-table*) (save-object encoding-table :static)
            (cold-symbol-value '*unicode-name-trie*) (save-object name-trie :static)))
    ;; Poke a few symbols to ensure they exist.
    (mapc (lambda (sym) (symbol-address (string sym) nil))
          '(*gdt* *idt* *%setup-stack* *%setup-function*
            *multiboot-header* *multiboot-info*
            *initial-obarray* *initial-keyword-obarray*
            *initial-setf-obarray* *initial-structure-obarray*
            *newspace-offset* *semispace-size* *newspace* *oldspace*
            *static-bump-pointer* *static-area-size* *static-mark-bit*
            *oldspace-paging-bits* *newspace-paging-bits*
            *stack-bump-pointer*
            *static-area* *static-area-hint*
            *unifont-bmp*
            *unicode-info* *unicode-name-store*
            *unicode-encoding-table* *unicode-name-trie*))
    (setf (cold-symbol-value '*undefined-function-thunk*) (make-value *undefined-function-address* +tag-function+))
    (generate-string-array cl-symbol-names '*cl-symbols*)
    (generate-string-array system-symbol-names '*system-symbols*)
    (generate-obarray *symbol-table* '*initial-obarray*)
    (generate-obarray *keyword-table* '*initial-keyword-obarray*)
    (generate-obarray *setf-table* '*initial-setf-obarray*)
    (generate-struct-obarray *struct-table* '*initial-structure-obarray*)
    ;; Create GDT.
    (setf (word gdt) (array-header +array-type-unsigned-byte-64+ 2)
          (word (+ gdt 1)) 0
          (word (+ gdt 2)) #x00209A0000000000)
    (setf (cold-symbol-value '*gdt*) (make-value gdt +tag-array-like+))
    ;; Create IDT.
    (setf (word idt) (array-header +array-type-unsigned-byte-64+ 256))
    (dotimes (i 256)
      (setf (word (1+ idt)) 0))
    (setf (cold-symbol-value '*idt*) (make-value idt +tag-array-like+))
    ;; Create the setup stack.
    (setf (word initial-stack) (array-header +array-type-unsigned-byte-64+ 7))
    (setf (cold-symbol-value '*%setup-stack*) (make-value initial-stack +tag-array-like+))
    (format t "Entry point at ~X~%" (make-value setup-fn +tag-function+))
    ;; Generate page tables.
    (setf (values initial-pml4 data-pml3) (create-page-tables))
    ;; Create multiboot header.
    (setf (word multiboot) (array-header +array-type-unsigned-byte-32+ 8)
          (word (+ multiboot 1)) (pack-halfwords #x1BADB002 #x00010003)
          (word (+ multiboot 2)) (pack-halfwords (ldb (byte 32 0) (- (+ #x1BADB002 #x00010003)))
                                                 (* (1+ multiboot) 8))
          (word (+ multiboot 3)) (pack-halfwords *physical-load-address*
                                                 (+ *physical-load-address* (* (image-data-size) 8)))
          (word (+ multiboot 4)) (pack-halfwords (+ *physical-load-address*
                                                    (* (total-image-size) 8))
                                                 (make-value setup-fn +tag-function+)))
    (setf (cold-symbol-value '*multiboot-header*) (make-value multiboot +tag-array-like+))
    ;; Initialize GC twiddly bits.
    (flet ((set-value (symbol value)
             (format t "~A is ~X~%" symbol value)
             (setf (cold-symbol-value symbol) (make-fixnum value))))
      (set-value '*newspace-offset* *dynamic-offset*)
      (set-value '*semispace-size* (/ *dynamic-area-semispace-limit* 8))
      (set-value '*newspace* *dynamic-area-base*)
      (set-value '*oldspace* (+ *dynamic-area-base* (/ *dynamic-area-size* 2)))
      (set-value '*static-area* *static-area-base*)
      (set-value '*static-mark-bit* 0)
      (set-value '*static-area-hint* 0)
      (set-value '*bump-pointer* (+ *linear-map* *physical-load-address* (* (total-image-size) 8)))
      (set-value '*oldspace-paging-bits* (+ data-pml3 (* (/ (+ *dynamic-area-base* (/ *dynamic-area-size* 2)) #x40000000) 8)))
      (set-value '*newspace-paging-bits* (+ data-pml3 (* (/ *dynamic-area-base* #x40000000) 8)))
      (set-value '*stack-bump-pointer* (+ (* *stack-offset* 8) *stack-area-base*))
      (set-value '*stack-bump-pointer-limit* (+ (* (1+ (ceiling *stack-offset* #x40000)) #x200000) *stack-area-base*)))
    ;; Write the boundary tag for the static area's free part.
    (let ((*static-offset* (+ *static-offset* 2)))
      (format t "~:D/~:D words free in static space.~%"
              (- (truncate *static-area-limit* 8) (- *static-offset* 2))
              (truncate *static-area-limit* 8))
      (setf (word (+ (truncate *static-area-base* 8) *static-offset* -2)) (- (truncate *static-area-limit* 8) *static-offset*)
            (word (+ (truncate *static-area-base* 8) *static-offset* -1)) #b100))
    (apply-fixups *pending-fixups*)
    (write-map-file image-name *function-map*)
    (write-image image-name description)))

(defun load-source-files (files set-fdefinitions)
  (mapc (lambda (f) (load-source-file f set-fdefinitions)) files))

(defvar *load-should-set-fdefinitions*)

(defconstant +llf-end-of-load+ #xFF)
(defconstant +llf-backlink+ #x01)
(defconstant +llf-function+ #x02)
(defconstant +llf-cons+ #x03)
(defconstant +llf-symbol+ #x04)
(defconstant +llf-uninterned-symbol+ #x05)
(defconstant +llf-unbound+ #x06)
(defconstant +llf-string+ #x07)
(defconstant +llf-setf-symbol+ #x08)
(defconstant +llf-integer+ #x09)
(defconstant +llf-invoke+ #x0A)
(defconstant +llf-setf-fdefinition+ #x0B)
(defconstant +llf-simple-vector+ #x0C)
(defconstant +llf-character+ #x0D)
(defconstant +llf-structure-definition+ #x0E)
(defconstant +llf-single-float+ #x10)

(defun make-bignum (value)
  (let* ((length (ceiling (1+ (integer-length value)) 64))
         (address (allocate (1+ length))))
    (setf (word address) (array-header +array-type-bignum+ length))
    (dotimes (i length)
      (setf (word (+ address 1 i)) (ldb (byte 64 (* i 64)) value)))
    (make-value address +tag-array-like+)))

;;; Mostly duplicated from the file compiler...
(defun load-integer (stream)
  (let ((value 0) (shift 0))
    (loop
         (let ((b (read-byte stream)))
           (when (not (logtest b #x80))
             (setf value (logior value (ash (logand b #x3F) shift)))
             (if (logtest b #x40)
                 (return (- value))
                 (return value)))
           (setf value (logior value (ash (logand b #x7F) shift)))
           (incf shift 7)))))

(defun utf8-sequence-length (byte)
  (cond
    ((eql (logand byte #x80) #x00)
     (values 1 byte))
    ((eql (logand byte #xE0) #xC0)
     (values 2 (logand byte #x1F)))
    ((eql (logand byte #xF0) #xE0)
     (values 3 (logand byte #x0F)))
    ((eql (logand byte #xF8) #xF0)
     (values 4 (logand byte #x07)))
    (t (error "Invalid UTF-8 lead byte ~S." byte))))

(defun load-character (stream)
  (multiple-value-bind (length value)
      (utf8-sequence-length (read-byte stream))
    ;; Read remaining bytes. They must all be continuation bytes.
    (dotimes (i (1- length))
      (let ((byte (read-byte stream)))
        (unless (eql (logand byte #xC0) #x80)
          (error "Invalid UTF-8 continuation byte ~S." byte))
        (setf value (logior (ash value 6) (logand byte #x3F)))))
    value))

(defun load-ub8-vector (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type '(unsigned-byte 8))))
    (read-sequence seq stream)
    seq))

(defun load-string (stream)
  (let* ((len (load-integer stream))
         (address (allocate (1+ (ceiling len 2)))))
    ;; Header word.
    (setf (word address) (logior (ash len 8) (ash +array-type-character+ 1)))
    (dotimes (i (ceiling len 2))
      (let ((value 0))
        (dotimes (j 2)
          (when (< (+ (* i 2) j) len)
            (setf (ldb (byte 32 64) value) (load-character stream)))
          (setf value (ash value -32)))
        (setf (word (+ address 1 i)) value)))
    (make-value address +tag-array-like+)))

(defun load-string* (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type 'character)))
    (dotimes (i len)
      (setf (aref seq i) (code-char (load-character stream))))
    seq))

(defun ensure-structure-layout-compatible (definition slots)
  (unless (equal (third definition) slots)
    (error "Incompatible redefinition of structure. ~S ~S~%" definition slots)))

(defun load-structure-definition (name* slots*)
  (let* ((name (extract-object name*))
         (slots (extract-object slots*))
         (definition (gethash name *struct-table*)))
    (cond (definition
           (ensure-structure-layout-compatible definition slots)
           (make-value (first definition) +tag-array-like+))
          (t (let ((address (allocate 4 :static)))
               (setf (word address) (array-header +array-type-struct+ 3))
               (setf (word (+ address 1)) (make-value (symbol-address "NIL" nil) +tag-symbol+))
               (setf (word (+ address 2)) name*)
               (setf (word (+ address 3)) slots*)
               (setf (gethash name *struct-table*) (list address name slots))
               (make-value address +tag-array-like+))))))

(defun extract-array (address element-width)
  (let* ((size (ldb (byte 56 8) (word address)))
         (array (make-array size))
         (elements-per-word (/ 64 element-width)))
    (dotimes (i size)
      (multiple-value-bind (word offset)
          (truncate i elements-per-word)
        (setf (aref array i) (ldb (byte element-width (* offset element-width))
                                  (word (+ address 1 word))))))
    array))

(defun extract-object (value)
  (let ((address (pointer-part value)))
    (ecase (tag-part value)
      ;; FIXME: Negative numbers...
      ((#.+tag-even-fixnum+
        #.+tag-odd-fixnum+)
       (ash value -3))
      (#.+tag-cons+
       (cons (extract-object (word address))
             (extract-object (word (1+ address)))))
      (#.+tag-symbol+
       (let ((name (extract-object (word address)))
             (package (word (+ address 1))))
         (when (eql package (make-value (symbol-address "NIL" nil) +tag-symbol+))
           (error "Attemping to extract an uninterned symbol."))
         (intern name '#:cold-generator)))
      (#.+tag-array-like+
       (ecase (ldb (byte 6 1) (word address))
         (#.+array-type-base-char+
          (map 'simple-string 'code-char (extract-array address 8)))
         (#.+array-type-character+
          (map 'simple-string 'code-char (extract-array address 32))))))))

(defun vintern (name &optional keywordp)
  (make-value (symbol-address name keywordp) +tag-symbol+))

(defun vcons (car cdr)
  (let ((address (allocate 2)))
    (setf (word address) car
          (word (1+ address)) cdr)
    (make-value address +tag-cons+)))

(defun vlist (&rest args)
  (if args
      (vcons (first args) (apply 'vlist (rest args)))
      (vintern "NIL")))

(defun load-llf-function (stream stack)
  ;; n constants on stack.
  ;; list of fixups on stack.
  ;; +llf-function+
  ;; tag. (byte)
  ;; mc size in bytes. (integer)
  ;; number of constants. (integer)
  (let* ((tag (read-byte stream))
         (mc-length (load-integer stream))
         ;; mc-length does not include the 12 byte function header.
         (mc (make-array (* (ceiling (+ mc-length 12) 8) 8)
                         :element-type '(unsigned-byte 8)
                         :initial-element 0))
         (n-constants (load-integer stream))
         (fixups (vector-pop stack))
         ;; Pull n constants off the value stack.
         (constants (subseq stack (- (length stack) n-constants)))
         (total-size (+ (* (ceiling (+ mc-length 12) 16) 2)
                        n-constants))
         (address (allocate total-size :static)))
    ;; Pop constants off.
    (decf (fill-pointer stack) n-constants)
    ;; Read mc bytes.
    (read-sequence mc stream :start 12 :end (+ 12 mc-length))
    ;; Copy machine code bytes.
    (dotimes (i (ceiling (+ mc-length 12) 8))
      (setf (word (+ address i)) (nibbles:ub64ref/le mc (* i 8))))
    ;; Set function header.
    (setf (word address) 0)
    (setf (ldb (byte 16 0) (word address)) tag
          (ldb (byte 16 16) (word address)) (ceiling (+ mc-length 12) 16)
          (ldb (byte 16 32) (word address)) n-constants)
    ;; Set constant pool.
    (dotimes (i (length constants))
      (setf (word (+ address (* (ceiling (+ mc-length 12) 16) 2) i))
            (aref constants i)))
    ;; Add to the function map.
    (push (list address (extract-object (aref constants 0)))
          *function-map*)
    ;; Add fixups to the list.
    (dolist (fixup (extract-object fixups))
      (push (list (car fixup) address (cdr fixup) :signed32)
            *pending-fixups*))
    ;; Done.
    (make-value address +tag-function+)))

(defun load-llf-vector (stream stack)
  (let* ((len (load-integer stream))
         (address (allocate (1+ len))))
    ;; Header word.
    (setf (word address) (logior (ash len 8) (ash +array-type-t+ 1)))
    ;; Drop vector values and copy them into the image.
    (decf (fill-pointer stack) len)
    (dotimes (i len)
      (setf (word (+ address 1 i)) (aref stack (+ (length stack) i))))
    (make-value address +tag-array-like+)))

(defun load-one-object (command stream stack)
  (ecase command
    (#.+llf-function+
     (load-llf-function stream stack))
    (#.+llf-cons+
     (let* ((car (vector-pop stack))
            (cdr (vector-pop stack)))
       (vcons car cdr)))
    (#.+llf-symbol+
     (let* ((name (load-string* stream))
            (package (load-string* stream)))
       (make-value (symbol-address name (string= package "KEYWORD"))
                   +tag-symbol+)))
    (#.+llf-uninterned-symbol+
     (let ((plist (vector-pop stack))
           (fn (vector-pop stack))
           (value (vector-pop stack))
           (name (vector-pop stack))
           (address (allocate 6)))
       ;; FN and VALUE may be the unbound tag.
       (setf (word address) name
             (word (+ address 1)) (make-value (symbol-address "NIL" nil) +tag-symbol+)
             (word (+ address 2)) value
             (word (+ address 3)) (if (eql fn +tag-unbound-value+)
                                      (make-value *undefined-function-address* +tag-function+)
                                      fn)
             (word (+ address 4)) plist
             (word (+ address 5)) (make-fixnum 0))
       (make-value address +tag-symbol+)))
    (#.+llf-unbound+ +tag-unbound-value+)
    (#.+llf-string+ (load-string stream))
    (#.+llf-setf-symbol+
     (let ((symbol (vector-pop stack)))
       (make-value (resolve-setf-symbol (extract-object symbol)) +tag-symbol+)))
    (#.+llf-integer+
     (let ((value (load-integer stream)))
       (typecase value
         ((signed-byte 61) (make-fixnum value))
         (t (make-bignum value)))))
    (#.+llf-invoke+
     ;; `(funcall ',fn)
     (let* ((fn (vector-pop stack))
            (form (vlist (vintern "FUNCALL")
                         (vlist (vintern "QUOTE")
                                fn))))
       (push form *load-time-evals*))
     nil)
    (#.+llf-setf-fdefinition+
     (let* ((base-name (vector-pop stack))
            (fn-value (vector-pop stack))
            (name (resolve-function-name base-name)))
       (if *load-should-set-fdefinitions*
           (setf (word (+ (pointer-part name) 3)) fn-value)
           ;; `(funcall #'(setf symbol-function) ',fn-value ',name)
           (push (vlist (vintern "FUNCALL")
                        (vlist (vintern "FUNCTION") (vlist (vintern "SETF") (vintern "SYMBOL-FUNCTION")))
                        (vlist (vintern "QUOTE") fn-value)
                        (vlist (vintern "QUOTE") name))
                   *load-time-evals*)))
     nil)
    (#.+llf-simple-vector+
     (load-llf-vector stream stack))
    (#.+llf-character+
     (logior (ash (load-character stream) 4)
             +tag-character+))
    (#.+llf-structure-definition+
     (let ((slots (vector-pop stack))
           (name (vector-pop stack)))
       (load-structure-definition name slots)))
    (#.+llf-single-float+
     (logior (ash (load-integer stream) 32)
             +tag-single-float+))))

(defun load-llf (stream)
  (let ((omap (make-hash-table))
        (stack (make-array 64 :adjustable t :fill-pointer 0)))
    (loop (let ((command (read-byte stream)))
            (case command
              (#.+llf-end-of-load+
               (return))
              (#.+llf-backlink+
               (let ((id (load-integer stream)))
                 (assert (< id (hash-table-count omap)) () "Object id ~S out of bounds." id)
                 (vector-push-extend (gethash id omap) stack)))
              (t (let ((value (load-one-object command stream stack))
                       (id (hash-table-count omap)))
                   (when value
                     (setf (gethash id omap) value)
                     (vector-push-extend value stack)))))))))

(defun resolve-function-name (value)
  (let ((name (extract-object value)))
    (cond ((symbolp name)
           value)
          ((and (= (list-length name) 2)
                (eql (first name) 'setf)
                (symbolp (second name)))
           (make-value (resolve-setf-symbol (second name)) +tag-symbol+))
          (t (error "Unknown function name ~S." name)))))

(defun load-source-file (file set-fdefinitions)
  (let ((llf-path (merge-pathnames (make-pathname :type "llf" :defaults file)))
        (*load-should-set-fdefinitions* set-fdefinitions))
    (when (and (not (string-equal (pathname-type (pathname file)) "llf"))
               (or (not (probe-file llf-path))
                   (<= (file-write-date llf-path) (file-write-date file))))
      (format t "~A is out of date will be recompiled.~%" llf-path)
      (sys.c::cross-compile-file file))
    (format t ";; Loading ~S.~%" llf-path)
    (with-open-file (s llf-path :element-type '(unsigned-byte 8))
      ;; Check the header.
      (assert (and (eql (read-byte s) #x4C)
                   (eql (read-byte s) #x4C)
                   (eql (read-byte s) #x46)
                   (eql (read-byte s) #x00)))
      ;; Read forms.
      (load-llf s))))

(defun apply-fixups (fixups)
  (mapc 'apply-fixup fixups))

(defun apply-fixup (fixup)
  (destructuring-bind (what address byte-offset type) fixup
    (let* ((value (if (consp what)
                      (make-value (symbol-address (symbol-name (second what))
                                                    (keywordp (second what))
                                                    t)
                                    +tag-symbol+)
                      (ecase what
                        ((nil t) (make-value (symbol-address (symbol-name what) nil)
                                               +tag-symbol+))
                        ((:undefined-function undefined-function)
                         (make-value *undefined-function-address* +tag-function+)))))
           (length (ecase type
                     (:signed32 (check-type value (signed-byte 32))
                                4)
                     (:full64 (check-type value (unsigned-byte 64))
                              8))))
      (dotimes (byte length)
        (multiple-value-bind (word byten)
            (truncate (+ byte-offset byte) 8)
          (setf (ldb (byte 8 (* byten 8)) (word (+ address word)))
                (ldb (byte 8 (* byte 8)) value)))))))
