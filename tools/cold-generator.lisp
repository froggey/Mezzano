(defpackage :cold-generator
  (:use :cl :iterate :nibbles))

(in-package :cold-generator)

(defparameter *source-files*
  '("supervisor/entry.lisp"
    "supervisor/interrupts.lisp"))


  #+nil'("kboot.lisp" ; ### must be before cold-start.lisp, for the constants.
    "cold-start.lisp"
    "system/gc.lisp"
    "system/runtime-array.lisp"
    "system/runtime-numbers.lisp"
    "system/runtime-support.lisp"
    "system/stack-group.lisp"
    "system/setf.lisp"
    "system/string.lisp"
    "system/type.lisp"
    "system/sequence.lisp"
    "system/array.lisp"
    "system/numbers.lisp"
    "system/defstruct.lisp"
    "system/hash-table.lisp"
    "system/early-cons.lisp"
    "system/reader.lisp"
    "system/backquote.lisp"
    "system/character.lisp"
    "system/printer.lisp"
    "cold-stream.lisp"
    "system/load.lisp"
    "system/format.lisp"
    "system/interrupt.lisp"
    "memory.lisp"
    "dump.lisp"
    "system/cons-compiler-macros.lisp"
    "system/defmacro.lisp"
    "system/basic-macros.lisp"
    "system/data-types.lisp"
    "system/parse.lisp"
    "pci.lisp"
    "framebuffer.lisp"
    "system/describe.lisp"
    "system/stuff.lisp")

(defparameter *special-source-files*
  '(#+nil("system/packages.lisp" sys.int::*package-system*)))

(defparameter *warm-source-files*
  '("system/closette.lisp"
    "system/runtime-misc.lisp"
    "system/condition.lisp"
    "system/restarts.lisp"
    "system/error.lisp"
    "system/coerce.lisp"
    "system/debug.lisp"
    "system/eval.lisp"
    "system/stream.lisp"
    "process.lisp"
    "lap.lisp"
    "lap-x86.lisp"
    "system/ansi-loop.lisp"
    "compiler/package.lisp"
    "compiler/compiler.lisp"
    "compiler/pass1.lisp"
    "compiler/constprop.lisp"
    "compiler/simplify.lisp"
    "compiler/lift.lisp"
    "compiler/inline.lisp"
    "compiler/kill-temps.lisp"
    "compiler/codegen.lisp"
    "compiler/builtins.lisp"
    "compiler/branch-tension.lisp"
    "compiler/lower-environment.lisp"
    "compiler/lower-special-bindings.lisp"
    "drivers/keyboard.lisp"
    "framebuffer-stream.lisp"
    "drivers/bochs-vbe.lisp"
    "ethernet.lisp"
    "drivers/rtl8139.lisp"
    "drivers/virtio-net.lisp"
    "graphics.lisp"
    "blit.lisp"
    "windows.lisp"
    "misc.lisp"
    "peek.lisp"
    "xterm.lisp"
    "system/time.lisp"
    "file.lisp"
    "telnet.lisp"
    "irc.lisp"
    "mandelbrot.lisp"
    "system/file-compiler.lisp"))

(defun compile-warm-source (&optional force)
  (dolist (file *warm-source-files*)
    (let ((llf-path (merge-pathnames (make-pathname :type "llf" :defaults file))))
      (when (or (not (probe-file llf-path))
                (<= (file-write-date llf-path) (file-write-date file))
                force)
        (format t "~A is out of date will be recompiled.~%" llf-path)
        (sys.c::cross-compile-file file)))))

;; FIXME! Save args.
(defparameter *undefined-function-thunk*
  `(;; Pass invoked-through as the first argument.
    (sys.lap-x86:mov64 :r8 :r13)
    (sys.lap-x86:mov32 :ecx ,(ash 1 sys.int::+n-fixnum-bits+))
    ;; Tail call through to RAISE-UNDEFINED-FUNCTION and let that
    ;; handle the heavy work.
    (sys.lap-x86:mov64 :r13 (:function sys.int::raise-undefined-function))
    (sys.lap-x86:jmp (:r13 ,(+ (- sys.int::+tag-object+)
                               8
                               (* sys.int::+fref-entry-point+ 8)))))
  "Code for the undefined function thunk.")

(defparameter *small-static-area-size* (* 8 1024 1024))
(defparameter *large-static-area-size* (* 56 1024 1024))
(defparameter *dynamic-area-semispace-limit* (* 32 1024 1024))

(defparameter *static-area-base*  #x0000200000)
(defparameter *static-area-size*  (- #x0080000000 *static-area-base*))
(defparameter *dynamic-area-base* #x0080000000)
(defparameter *dynamic-area-size* #x0080000000) ; 2GB
(defparameter *stack-area-base*   #x0100000000)
(defparameter *stack-area-size*   #x0040000000) ; 1GB
(defparameter *linear-map*        #x8000000000)
(defparameter *physical-load-address* #x0000200000)

(defvar *static-offset*)
(defvar *dynamic-offset*)
(defvar *stack-offset*)

(defvar *static-data*)
(defvar *dynamic-data*)

(defvar *symbol-table*)
(defvar *reverse-symbol-table*)
;; Hash-table mapping function names to function references.
(defvar *fref-table*)
(defvar *struct-table*)
(defvar *unbound-value-address*)
(defvar *unbound-tls-slot-address*)
(defvar *undefined-function-address*)
(defvar *load-time-evals*)
(defvar *string-dedup-table*)

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
       address))))

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
          (t (error "Unknown address #x~X.~%" address)))))

(defvar *word-locks*)

(defun lock-word (address)
  (setf (gethash address *word-locks*) t))

(defun (setf word) (new-value address)
  (when (gethash address *word-locks*)
    (error "Attempted to write locked word at ~X~%" address))
  (multiple-value-bind (offset data-vector)
      (storage-info-for-address address)
    (setf (ub64ref/le data-vector (* offset 8)) new-value)))

(defun word (address)
  (multiple-value-bind (offset data-vector)
      (storage-info-for-address address)
    (ub64ref/le data-vector (* offset 8))))

(defun compile-lap-function (code &key (area :static) extra-symbols constant-values (position-independent t))
  "Compile a list of LAP code as a function. Constants must only be symbols."
  (let ((base-address (if position-independent
                          0
                          (ecase area
                            (:static
                             (+ *static-area-base* (* *static-offset* 8) 16))))))
    (multiple-value-bind (mc constants fixups symbols gc-info)
        (let ((sys.lap-x86:*function-reference-resolver* #'sys.c::resolve-fref))
          (sys.lap-x86:assemble (list* (list :d64/le 0 0) code) ; 16 byte header.
            :base-address base-address
            :initial-symbols (list* '(nil . :fixup)
                                    '(t . :fixup)
                                    extra-symbols)))
      (declare (ignore symbols))
      (setf mc (adjust-array mc (* (ceiling (length mc) 16) 16) :fill-pointer t))
      (let ((total-size (+ (* (truncate (length mc) 16) 2)
                           (length constants)
                           (* (ceiling (length gc-info) 8) 8))))
        (when (oddp total-size) (incf total-size))
        (let ((address (allocate total-size area)))
          ;; Copy machine code into the area.
          (dotimes (i (truncate (length mc) 8))
            (setf (word (+ address i)) (nibbles:ub64ref/le mc (* i 8))))
          ;; Set header word.
          (setf (word address) 0)
          (setf (ldb (byte 16 0) (word address)) (ash sys.int::+object-tag-function+
                                                      sys.int::+array-type-shift+) ; tag
                (ldb (byte 16 16) (word address)) (truncate (length mc) 16)
                (ldb (byte 16 32) (word address)) (length constants)
                (ldb (byte 16 48) (word address)) (length gc-info))
          (setf (word (1+ address)) (* (+ address 2) 8))
          ;; Copy GC bytes.
          (setf gc-info (adjust-array gc-info (* (ceiling (length gc-info) 8) 8)))
          (dotimes (i (ceiling (length gc-info) 8))
            (setf (word (+ address
                           (* (truncate (length mc) 16) 2)
                           (length constants)
                           i))
                  (nibbles:ub64ref/le gc-info (* i 8))))
          ;; Write constant pool.
          (dotimes (i (length constants))
            (cond ((assoc (aref constants i) constant-values)
                   (setf (word (+ address
                                  (truncate (length mc) 8)
                                  i))
                         (cdr (assoc (aref constants i) constant-values))))
                  (t (check-type (aref constants i)
                                 (or symbol sys.c::cross-fref))
                     (push (list (list 'quote (aref constants i))
                                 (+ address
                                    (truncate (length mc) 8)
                                    i)
                                 0
                                 :full64
                                 code)
                           *pending-fixups*))))
          (dolist (fixup fixups)
            (assert (>= (cdr fixup) 16))
            (push (list (car fixup) address (- (cdr fixup) base-address) :signed32 code)
                  *pending-fixups*))
          address)))))

(defun make-value (address tag)
  (logior (* address 8) tag))

(defun pointer-part (value)
  (ash (ldb (byte 64 4) value) 1))

(defun tag-part (value)
  (ldb (byte 4 0) value))

(defun make-fixnum (value)
  (assert (typep value `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+))))
  (ldb (byte 64 0) (ash value sys.int::+n-fixnum-bits+)))

(defun unbound-value ()
  (make-value *unbound-value-address* sys.int::+tag-object+))

(defun store-string (string)
  (let ((object-address (allocate 6 :static))
        (data-address (allocate (1+ (ceiling (length string) 8)) :static)))
    ;; String container
    (setf (word (+ object-address 0)) (array-header sys.int::+object-tag-string+ 1)
          (word (+ object-address 1)) (make-value data-address sys.int::+tag-object+)
          (word (+ object-address 2)) (make-value (symbol-address "NIL" "COMMON-LISP" nil) sys.int::+tag-object+)
          (word (+ object-address 3)) (make-value (symbol-address "NIL" "COMMON-LISP" nil) sys.int::+tag-object+)
          (word (+ object-address 4)) (make-fixnum (length string)))
    ;; Byte data.
    ;; Header word.
    (setf (word data-address) (array-header sys.int::+object-tag-array-unsigned-byte-8+ (length string)))
    (dotimes (i (ceiling (length string) 8))
      (let ((value 0))
        (dotimes (j 8)
          (when (< (+ (* i 8) j) (length string))
            (setf (ldb (byte 8 64) value) (char-code (char string (+ (* i 8) j)))))
          (setf value (ash value -8)))
        (setf (word (+ data-address 1 i)) value)))
    object-address))

(defun symbol-address (name package &optional (createp t))
  (or (gethash (cons name package) *symbol-table*)
      (when (not createp)
        (error "Symbol ~A::~A does not exist."
               package
               name))
      (let ((address (allocate 6 :static)))
        ;; fixme, keywords should be constant.
        (setf (word (+ address 0)) (array-header sys.int::+object-tag-symbol+ 0)
              (word (+ address 1)) (make-value (store-string name)
                                         sys.int::+tag-object+)
              (word (+ address 2)) (make-value (symbol-address package "KEYWORD") sys.int::+tag-object+)
              (word (+ address 3)) (if (string= package "KEYWORD")
                                       (make-value address sys.int::+tag-object+)
                                       (unbound-value))
              (word (+ address 4)) (make-value (gethash '("NIL" . "COMMON-LISP") *symbol-table*) sys.int::+tag-object+)
              (word (+ address 5)) (make-value (gethash '("NIL" . "COMMON-LISP") *symbol-table*) sys.int::+tag-object+))
        (setf (gethash address *reverse-symbol-table*) (cons name package)
              (gethash (cons name package) *symbol-table*) address))))

;; fixme, nil and t should be constant.
(defun create-support-objects ()
  "Create NIL, T and the undefined function thunk."
  (let ((nil-value (allocate 6 :static))
        (t-value (allocate 6 :static))
        (keyword-keyword (allocate 6 :static))
        (cl-keyword (allocate 6 :static))
        (unbound-val (allocate 2 :static))
        (unbound-tls-val (allocate 2 :static))
        (undef-fn (compile-lap-function *undefined-function-thunk*)))
    (format t "NIL at word ~X~%" nil-value)
    (format t "  T at word ~X~%" t-value)
    (format t "UDF at word ~X~%" undef-fn)
    (format t "UBV at word ~X~%" unbound-val)
    (setf (gethash '("NIL" . "COMMON-LISP") *symbol-table*) nil-value
          (gethash nil-value *reverse-symbol-table*) '("NIL" . "COMMON-LISP")
          (gethash '("T" . "COMMON-LISP") *symbol-table*) t-value
          (gethash t-value *reverse-symbol-table*) '("T" . "COMMON-LISP")
          (gethash '("KEYWORD" . "KEYWORD") *symbol-table*) keyword-keyword
          (gethash keyword-keyword *reverse-symbol-table*) '("KEYWORD" . "KEYWORD")
          (gethash '("COMMON-LISP" . "KEYWORD") *symbol-table*) cl-keyword
          (gethash cl-keyword *reverse-symbol-table*) '("COMMON-LISP" . "KEYWORD")
          *undefined-function-address* undef-fn
          *unbound-value-address* unbound-val
          *unbound-tls-slot-address* unbound-tls-val)
    (setf (word (+ nil-value 0)) (array-header sys.int::+object-tag-symbol+ 0) ; flags & header
          (word (+ nil-value 1)) (make-value (store-string "NIL")
                                       sys.int::+tag-object+) ; name
          (word (+ nil-value 2)) (make-value cl-keyword sys.int::+tag-object+) ; package
          (word (+ nil-value 3)) (make-value nil-value sys.int::+tag-object+) ; value
          (word (+ nil-value 4)) (make-value nil-value sys.int::+tag-object+) ; function
          (word (+ nil-value 5)) (make-value nil-value sys.int::+tag-object+)) ; plist
    (setf (word (+ t-value 0)) (array-header sys.int::+object-tag-symbol+ 0)
          (word (+ t-value 1)) (make-value (store-string "T")
                                     sys.int::+tag-object+)
          (word (+ t-value 2)) (make-value cl-keyword sys.int::+tag-object+)
          (word (+ t-value 3)) (make-value t-value sys.int::+tag-object+)
          (word (+ t-value 4)) (make-value nil-value sys.int::+tag-object+)
          (word (+ t-value 5)) (make-value nil-value sys.int::+tag-object+))
    (setf (word (+ keyword-keyword 0)) (array-header sys.int::+object-tag-symbol+ 0)
          (word (+ keyword-keyword 1)) (make-value (store-string "KEYWORD")
                                     sys.int::+tag-object+)
          (word (+ keyword-keyword 2)) (make-value keyword-keyword sys.int::+tag-object+)
          (word (+ keyword-keyword 3)) (make-value keyword-keyword sys.int::+tag-object+)
          (word (+ keyword-keyword 4)) (make-value nil-value sys.int::+tag-object+)
          (word (+ keyword-keyword 5)) (make-value nil-value sys.int::+tag-object+))
    (setf (word (+ cl-keyword 0)) (array-header sys.int::+object-tag-symbol+ 0)
          (word (+ cl-keyword 1)) (make-value (store-string "COMMON-LISP")
                                     sys.int::+tag-object+)
          (word (+ cl-keyword 2)) (make-value keyword-keyword sys.int::+tag-object+)
          (word (+ cl-keyword 3)) (make-value cl-keyword sys.int::+tag-object+)
          (word (+ cl-keyword 4)) (make-value nil-value sys.int::+tag-object+)
          (word (+ cl-keyword 5)) (make-value nil-value sys.int::+tag-object+))
    (setf (word unbound-val) (array-header sys.int::+object-tag-unbound-value+ 0))
    (setf (word unbound-tls-val) (array-header sys.int::+object-tag-unbound-value+ 1))))

(defun write-image (name entry-fref initial-process idt-size idt-pointer)
  (with-open-file (s (make-pathname :type "image" :defaults name)
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
    ;; Image header.
    (let ((header (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; Magic.
      (replace header #(#x00 #x4D #x65 #x7A #x7A #x61 #x6E #x69 #x6E #x65 #x49 #x6D #x61 #x67 #x65 #x00)
               :start1 0)
      ;; UUID.
      (dotimes (i 16)
        (setf (aref header (+ 16 i)) (case i
                                       (9 (logior #x40 (random 16)))
                                       (7 (logior (random 64) #x80))
                                       (t (random 256)))))
      ;; Major & minor version.
      (setf (ub16ref/le header 32) 0
            (ub16ref/le header 34) 1)
      ;; Number of extents.
      (setf (ub32ref/le header 36) 4)
      ;; Entry fref.
      (setf (ub64ref/le header 40) entry-fref)
      ;; Initial process.
      (setf (ub64ref/le header 48) initial-process)
      ;; System IDT info.
      (setf (ub16ref/le header 64) idt-size
            (ub64ref/le header 66) idt-pointer)
      (flet ((extent (id store-base virtual-base size flags)
               (setf (ub64ref/le header (+ 80 (* id 32) 0)) store-base
                     (ub64ref/le header (+ 80 (* id 32) 8)) virtual-base
                     (ub64ref/le header (+ 80 (* id 32) 16)) size
                     (ub64ref/le header (+ 80 (* id 32) 32)) flags)))
        ;; The static area extent.
        (extent 0
                #x1000
                *static-area-base*
                (+ *small-static-area-size* *large-static-area-size*)
                0)
        ;; The dynamic area extent (newspace).
        (extent 1
                (+ #x1000
                   *small-static-area-size*
                   *large-static-area-size*)
                *dynamic-area-base*
                *dynamic-area-semispace-limit*
                0)
        ;; The dynamic area extent (oldspace).
        (extent 2
                (+ #x1000
                   *small-static-area-size*
                   *large-static-area-size*
                   *dynamic-area-semispace-limit*)
                (+ *dynamic-area-base* (/ *dynamic-area-size* 2))
                *dynamic-area-semispace-limit*
                0)
        ;; The stack area extent.
        (extent 3
                (+ #x1000
                   *small-static-area-size*
                   *large-static-area-size*
                   (* *dynamic-area-semispace-limit* 2))
                *stack-area-base*
                (* (1+ (ceiling *stack-offset* #x40000)) #x200000)
                0))
      ;; Write it out.
      (write-sequence header s))
    ;; Touch the very end of the image, to make it the right size.
    (file-position s (1- (+ #x1000
                            *small-static-area-size*
                            *large-static-area-size*
                            (* *dynamic-area-semispace-limit* 2)
                            (* (1+ (ceiling *stack-offset* #x40000)) #x200000))))
    (write-byte 0 s)
    ;; First, the static area.
    (file-position s #x1000)
    (write-sequence *static-data* s)
    ;; Then, the dynamic area.
    (file-position s (+ #x1000
                        *small-static-area-size*
                        *large-static-area-size*))
    (write-sequence *dynamic-data* s))
  (values))

(defun total-image-size ()
  (+ (* (/ *small-static-area-size* 8) 2)
     (* (/ *large-static-area-size* 8) 2)
     (/ (* *dynamic-area-semispace-limit* 2) 8)
     (* (1+ (ceiling *stack-offset* #x40000)) #x40000)))

(defun image-data-size ()
  (+ (* (ceiling *static-offset* #x40000) #x40000)
     (* (ceiling *dynamic-offset* #x40000) #x40000)))

(defun array-header (tag length)
  (logior (ash tag sys.int::+array-type-shift+)
          (ash length sys.int::+array-length-shift+)))

(defun pack-halfwords (low high)
  (check-type low (unsigned-byte 32))
  (check-type high (unsigned-byte 32))
  (dpb high (byte 32 32) low))

(defun create-initial-stack-group ()
  (let* ((address (allocate 512 :static))
         (control-stack-size 16384)
         (control-stack (allocate-stack control-stack-size :control))
         (binding-stack-size 1024)
         (binding-stack (allocate-stack binding-stack-size :binding)))
    ;; Array tag.
    (setf (word (+ address 0)) (array-header sys.int::+object-tag-stack-group+ 511))
    ;; Binding stack pointer.
    (setf (word (+ address 1)) (* (+ binding-stack binding-stack-size) 8))
    ;; State word. Unsafe, active.
    (setf (word (+ address 2)) (make-fixnum 0))
    ;; Saved control stack pointer.
    (setf (word (+ address 3)) (* (+ control-stack control-stack-size) 8))
    ;; Name.
    (setf (word (+ address 4)) (make-value (store-string "Initial stack group") sys.int::+tag-object+))
    ;; Control stack base. Byte pointer stored as ub64.
    (setf (word (+ address 5)) (* control-stack 8))
    ;; Control stack size.
    (setf (word (+ address 6)) (* control-stack-size 8))
    ;; Binding stack base.
    (setf (word (+ address 7)) (* binding-stack 8))
    ;; Binding stack size.
    (setf (word (+ address 8)) (* binding-stack-size 8))
    ;; Start of TLS slots.
    (dotimes (i (- 512 9))
      (setf (word (+ address 9 i)) (make-value *unbound-tls-slot-address*
                                               sys.int::+tag-object+)))
    (setf (cold-symbol-value 'sys.int::*initial-stack-group*)
          (make-value address sys.int::+tag-object+))))

(defun canonical-symbol-package (symbol)
  (when (keywordp symbol)
    (return-from canonical-symbol-package "KEYWORD"))
  (let ((package (symbol-package symbol)))
    (cond ((eql package (find-package "CL"))
           "COMMON-LISP")
          ((eql package (find-package "SYS.INT"))
           "SYSTEM.INTERNALS")
          ((eql package (find-package "SYSTEM"))
           "SYSTEM")
          ((eql package (find-package "SYS.FORMAT"))
           "SYS.FORMAT")
          (t (error "Not touching package ~S (for symbol ~A)." package symbol)))))

(defun (setf cold-symbol-value) (value symbol)
  (setf (word (+ (symbol-address (symbol-name symbol)
                                 (canonical-symbol-package symbol))
                 1
                 sys.c::+symbol-value+))
        value))

(defun generate-toplevel-form-array (functions symbol)
  ;; Generate array of toplevel forms to eval.
  (let* ((n (length functions))
         (toplevel-forms (allocate (1+ n))))
    (setf (word toplevel-forms) (array-header sys.int::+object-tag-array-t+ n))
    (iter (for i from 0)
          (for fn in functions)
          (setf (word (+ toplevel-forms 1 i)) fn))
    (setf (cold-symbol-value symbol) (make-value toplevel-forms sys.int::+tag-object+))))

(defun generate-obarray (symtab target-symbol)
  (let ((obarray (allocate (1+ (hash-table-count symtab)))))
    (setf (word obarray) (array-header sys.int::+object-tag-array-t+ (hash-table-count symtab)))
    (iter (for (nil address) in-hashtable symtab)
          (for i from 0)
          (setf (word (+ obarray 1 i)) (make-value address sys.int::+tag-object+)))
    (setf (cold-symbol-value target-symbol) (make-value obarray sys.int::+tag-object+))))

(defun generate-fref-obarray (symtab target-symbol)
  (let ((obarray (allocate (1+ (hash-table-count symtab)))))
    (setf (word obarray) (array-header sys.int::+object-tag-array-t+ (hash-table-count symtab)))
    (iter (for (name address) in-hashtable symtab)
          (for i from 0)
          (setf (word (+ obarray 1 i)) (make-value address sys.int::+tag-object+)))
    (setf (cold-symbol-value target-symbol) (make-value obarray sys.int::+tag-object+))))

(defun generate-struct-obarray (symtab target-symbol)
  (let ((obarray (allocate (1+ (hash-table-count symtab)))))
    (setf (word obarray) (array-header sys.int::+object-tag-array-t+ (hash-table-count symtab)))
    (iter (for (nil address) in-hashtable symtab)
          (for i from 0)
          (setf (word (+ obarray 1 i)) (make-value (first address) sys.int::+tag-object+)))
    (setf (cold-symbol-value target-symbol) (make-value obarray sys.int::+tag-object+))))

(defun generate-string-array (sequence target-symbol)
  (let ((obarray (allocate (1+ (length sequence)))))
    (setf (word obarray) (array-header sys.int::+object-tag-array-t+ (length sequence)))
    (iter (for object in-sequence sequence)
          (for i from 0)
          (setf (word (+ obarray 1 i)) (make-value (store-string object) sys.int::+tag-object+)))
    (setf (cold-symbol-value target-symbol) (make-value obarray sys.int::+tag-object+))))

(defun write-map-file (image-name map)
  (with-open-file (s (format nil "~A.map" image-name)
                     :direction :output
                     :if-exists :supersede)
    (let ((*print-right-margin* 10000))
      (iter (for (addr name) in (sort (copy-list map) '< :key 'first))
            (format s "~X ~A~%" (* (+ addr 2) 8)
                    (cl-ppcre:regex-replace (string #\Newline)
                                            (format nil "~A" name)
                                            "#\\Newline"))))))

;; Ugh.
(defun load-compiler-builtins ()
  (sys.c::save-compiler-builtins "%%compiler-builtins.llf")
  (load-source-file "%%compiler-builtins.llf" t))

(defun save-ub1-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (ceiling (length vec) 64)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-bit+ (length vec)))
    (dotimes (i (ceiling (length vec) 64))
      (let ((value 0))
        (dotimes (j 64)
          (when (< (+ (* i 64) j) (length vec))
            (setf (ldb (byte 1 64) value) (aref vec (+ (* i 64) j))))
          (setf value (ash value -1)))
        (setf (word (+ address 1 i)) value)))
    (make-value address sys.int::+tag-object+)))

(defun save-ub2-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (ceiling (length vec) 32)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-unsigned-byte-2+ (length vec)))
    (dotimes (i (ceiling (length vec) 32))
      (let ((value 0))
        (dotimes (j 32)
          (when (< (+ (* i 32) j) (length vec))
            (setf (ldb (byte 2 64) value) (aref vec (+ (* i 32) j))))
          (setf value (ash value -2)))
        (setf (word (+ address 1 i)) value)))
    (make-value address sys.int::+tag-object+)))

(defun save-ub4-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (ceiling (length vec) 16)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-unsigned-byte-4+ (length vec)))
    (dotimes (i (ceiling (length vec) 16))
      (let ((value 0))
        (dotimes (j 16)
          (when (< (+ (* i 16) j) (length vec))
            (setf (ldb (byte 4 64) value) (aref vec (+ (* i 16) j))))
          (setf value (ash value -4)))
        (setf (word (+ address 1 i)) value)))
    (make-value address sys.int::+tag-object+)))

(defun save-ub8-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (ceiling (length vec) 8)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-unsigned-byte-8+ (length vec)))
    (dotimes (i (ceiling (length vec) 8))
      (let ((value 0))
        (dotimes (j 8)
          (when (< (+ (* i 8) j) (length vec))
            (setf (ldb (byte 8 64) value) (aref vec (+ (* i 8) j))))
          (setf value (ash value -8)))
        (setf (word (+ address 1 i)) value)))
    (make-value address sys.int::+tag-object+)))

(defun save-ub16-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (ceiling (length vec) 4)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-unsigned-byte-16+ (length vec)))
    (dotimes (i (ceiling (length vec) 4))
      (let ((value 0))
        (dotimes (j 4)
          (when (< (+ (* i 4) j) (length vec))
            (setf (ldb (byte 16 64) value) (aref vec (+ (* i 4) j))))
          (setf value (ash value -16)))
        (setf (word (+ address 1 i)) value)))
    (make-value address sys.int::+tag-object+)))

(defun save-ub32-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (ceiling (length vec) 2)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-unsigned-byte-32+ (length vec)))
    (dotimes (i (ceiling (length vec) 2))
      (let ((value 0))
        (dotimes (j 2)
          (when (< (+ (* i 2) j) (length vec))
            (setf (ldb (byte 32 64) value) (aref vec (+ (* i 2) j))))
          (setf value (ash value -32)))
        (setf (word (+ address 1 i)) value)))
    (make-value address sys.int::+tag-object+)))

(defun save-ub64-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (length vec)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-unsigned-byte-64+ (length vec)))
    (iter (for x in-sequence vec)
          (for i from 1)
          (setf (word (+ address i)) x))
    (make-value address sys.int::+tag-object+)))

(defun save-simple-vector (vec &optional (area :dynamic))
  (let ((address (allocate (1+ (length vec)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-t+ (length vec)))
    (iter (for x in-sequence vec)
          (for i from 1)
          (setf (word (+ address i)) (save-object x area)))
    (make-value address sys.int::+tag-object+)))

(defun save-object (object &optional (area :dynamic))
  (etypecase object
    ((signed-byte 63) (make-fixnum object))
    (string
     (let ((value (gethash object *string-dedup-table*)))
       (unless value
         (setf value (make-value (store-string object)
                                 sys.int::+tag-object+))
         (setf (gethash object *string-dedup-table*) value))
       value))
    (cons (vcons (save-object (car object))
                 (save-object (cdr object))))
    (symbol (make-value (symbol-address (symbol-name object)
                                        (canonical-symbol-package object))
                        sys.int::+tag-object+))
    ((vector (unsigned-byte 1))
     (save-ub1-vector object :static))
    ((vector (unsigned-byte 2))
     (save-ub2-vector object :static))
    ((vector (unsigned-byte 4))
     (save-ub4-vector object :static))
    ((vector (unsigned-byte 8))
     (save-ub8-vector object :static))
    ((vector (unsigned-byte 16))
     (save-ub16-vector object :static))
    ((vector (unsigned-byte 32))
     (save-ub32-vector object :static))
    ((vector (unsigned-byte 64))
     (save-ub64-vector object :static))
    ((vector t)
     (save-simple-vector object area))))

(defun save-unifont-data (path)
  (multiple-value-bind (tree data)
      (with-open-file (s path)
        (build-unicode:generate-unifont-table s))
    (setf (cold-symbol-value 'sys.int::*unifont-bmp*) (save-object tree :static))
    (setf (cold-symbol-value 'sys.int::*unifont-bmp-data*) (save-object data :static))))

;; Handlers for the defined CPU exceptions, and a bool indicating if they take
;; an error code or not.
(defparameter *cpu-exception-info*
  '((sys.int::%divide-error-handler nil)           ; 0
    (sys.int::%debug-exception-handler nil)        ; 1
    (sys.int::%nonmaskable-interrupt-handler nil)  ; 2
    (sys.int::%breakpoint-handler nil)             ; 3
    (sys.int::%overflow-handler nil)               ; 4
    (sys.int::%bound-exception-handler nil)        ; 5
    (sys.int::%invalid-opcode-handler nil)         ; 6
    (sys.int::%device-not-available-handler nil)   ; 7
    (sys.int::%double-fault-handler t)             ; 8
    nil                                            ; 9
    (sys.int::%invalid-tss-handler t)              ; 10
    (sys.int::%segment-not-present-handler t)      ; 11
    (sys.int::%stack-segment-fault-handler t)      ; 12
    (sys.int::%general-protection-fault-handler t) ; 13
    (sys.int::%page-fault-handler t)               ; 14
    nil                                            ; 15
    (sys.int::%math-fault-handler nil)             ; 16
    (sys.int::%alignment-check-handler t)          ; 17
    (sys.int::%machine-check-handler nil)          ; 18
    (sys.int::%simd-exception-handler nil)         ; 19
    nil                                            ; 20
    nil                                            ; 21
    nil                                            ; 22
    nil                                            ; 23
    nil                                            ; 24
    nil                                            ; 25
    nil                                            ; 26
    nil                                            ; 27
    nil                                            ; 28
    nil                                            ; 29
    nil                                            ; 30
    nil))                                          ; 31

(defparameter *common-interrupt-code*
  `(;; Common code for all interrupts.
    ;; There's an interrupt frame set up, a per-interrupt value in r9, and an fref for the high-level handler in r13.
    ;; Generate a DX interrupt frame object, then call the handler with it.
    ;; Realign the stack.
    (sys.lap-x86:and64 :rsp ,(lognot 15))
    (sys.lap-x86:sub64 :rsp 16) ; 2 elements.
    (sys.lap-x86:mov64 (:rsp) ,(ash sys.int::+object-tag-interrupt-frame+ sys.int::+array-type-shift+)) ; header
    (sys.lap-x86:lea64 :rax (:rbp :rbp)) ; Convert frame pointer to fixnum.
    (sys.lap-x86:mov64 (:rsp 8) :rax) ; 2nd element.
    (sys.lap-x86:lea64 :r8 (:rsp ,sys.int::+tag-object+))
    (sys.lap-x86:mov32 :ecx ,(ash 2 sys.int::+n-fixnum-bits+)) ; 2 args.
    (:gc :frame :interrupt t)
    (sys.lap-x86:call (:object :r13 ,sys.int::+fref-entry-point+))
    ;; Restore registers, then return.
    (sys.lap-x86:mov64 :r15 (:rbp -112))
    (sys.lap-x86:mov64 :r14 (:rbp -104))
    (sys.lap-x86:mov64 :r13 (:rbp -96))
    (sys.lap-x86:mov64 :r12 (:rbp -88))
    (sys.lap-x86:mov64 :r11 (:rbp -80))
    (sys.lap-x86:mov64 :r10 (:rbp -72))
    (sys.lap-x86:mov64 :r9 (:rbp -64))
    (sys.lap-x86:mov64 :r8 (:rbp -56))
    (sys.lap-x86:mov64 :rdi (:rbp -48))
    (sys.lap-x86:mov64 :rsi (:rbp -40))
    (sys.lap-x86:mov64 :rbx (:rbp -32))
    (sys.lap-x86:mov64 :rdx (:rbp -24))
    (sys.lap-x86:mov64 :rcx (:rbp -16))
    (sys.lap-x86:mov64 :rax (:rbp -8))
    (sys.lap-x86:leave)
    (sys.lap-x86:iret)))

(defparameter *common-user-interrupt-code*
  `(;; Common code for interrupts 32+.
    ;; There's an incomplete interrupt frame set up and an interrupt number in rax (fixnum).
    ;; Save registers to fill in the rest of the interrupt frame.
    (sys.lap-x86:push :rcx) ; -16 (-2)
    (sys.lap-x86:push :rdx) ; -24 (-3)
    (sys.lap-x86:push :rbx) ; -32 (-4)
    (sys.lap-x86:push :rsi) ; -40 (-5)
    (sys.lap-x86:push :rdi) ; -48 (-6)
    (sys.lap-x86:push :r8)  ; -56 (-7)
    (sys.lap-x86:push :r9)  ; -64 (-8)
    (sys.lap-x86:push :r10) ; -72 (-9)
    (sys.lap-x86:push :r11) ; -80 (-10)
    (sys.lap-x86:push :r12) ; -88 (-11)
    (sys.lap-x86:push :r13) ; -96 (-12)
    (sys.lap-x86:push :r14) ; -104 (-13)
    (sys.lap-x86:push :r15) ; -112 (-14)
    ;; Fall into the common interrupt code.
    (sys.lap-x86:mov64 :r9 :rax)
    (sys.lap-x86:mov64 :r13 (:function sys.int::%user-interrupt-handler))
    (sys.lap-x86:jmp interrupt-common)))

(defun create-exception-isr (handler error-code-p)
  (append
   (if error-code-p
       ;; Create interrupt frame and pull error code off the stack.
       ;; There aren't any free registers yet, so we have to shuffle things around.
       ;; Don't use xchg, because that has an implicit lock (slow).
       '((sys.lap-x86:push :rax)
         (sys.lap-x86:mov64 :rax (:rsp 8))  ; load error code into rax, freeing up the fp location
         (sys.lap-x86:mov64 (:rsp 8) :rbp)  ; really create the stack frame.
         (sys.lap-x86:lea64 :rbp (:rsp 8))) ; rsp is slightly offset because of the push rax.
       ;; Create interrupt frame. No error code, so no shuffling needed.
       '((sys.lap-x86:push :rbp)
         (sys.lap-x86:mov64 :rbp :rsp)
         (sys.lap-x86:push :rax)))
   ;; Frame looks like:
   ;; +40 SS
   ;; +32 RSP
   ;; +24 RFlags
   ;; +16 CS
   ;; +8  RIP
   ;; +0  RBP
   ;; -8  RAX
   ;; Save registers to fill in the rest of the interrupt frame.
   ;; RAX holds the saved error code (if any).
   `((sys.lap-x86:push :rcx) ; -16 (-2)
     (sys.lap-x86:push :rdx) ; -24 (-3)
     (sys.lap-x86:push :rbx) ; -32 (-4)
     (sys.lap-x86:push :rsi) ; -40 (-5)
     (sys.lap-x86:push :rdi) ; -48 (-6)
     (sys.lap-x86:push :r8)  ; -56 (-7)
     (sys.lap-x86:push :r9)  ; -64 (-8)
     (sys.lap-x86:push :r10) ; -72 (-9)
     (sys.lap-x86:push :r11) ; -80 (-10)
     (sys.lap-x86:push :r12) ; -88 (-11)
     (sys.lap-x86:push :r13) ; -96 (-12)
     (sys.lap-x86:push :r14) ; -104 (-13)
     (sys.lap-x86:push :r15) ; -112 (-14)
     ;; Jump to the common exception code.
     (sys.lap-x86:mov64 :r13 (:function ,handler)))
   (if error-code-p
       '((sys.lap-x86:lea64 :r9 (:rax :rax))) ; Convert error code to fixnum.
       '((sys.lap-x86:mov32 :r9d nil))) ; Nothing interesting
   '((sys.lap-x86:jmp interrupt-common))))

(defun create-user-interrupt-isr (index)
  ;; Create interrupt frame. No error code, so no shuffling needed.
  `((sys.lap-x86:push :rbp)
    (sys.lap-x86:mov64 :rbp :rsp)
    ;; Frame looks like:
    ;; +40 SS
    ;; +32 RSP
    ;; +24 RFlags
    ;; +16 CS
    ;; +8  RIP
    ;; +0  RBP
    ;; Save RAX, then jump to common code with the interrupt number.
    (sys.lap-x86:push :rax) ; -8 (-1)
    (sys.lap-x86:mov32 :eax ,(ash index sys.int::+n-fixnum-bits+))
    (sys.lap-x86:jmp user-interrupt-common)))

(defun make-idt-entry (&key (offset 0) (segment #x0008)
                         (present t) (dpl 0) (ist nil)
                         (interrupt-gate-p t))
  (check-type offset (signed-byte 64))
  (check-type segment (unsigned-byte 16))
  (check-type dpl (unsigned-byte 2))
  (check-type ist (or null (unsigned-byte 3)))
  (let ((value 0))
    (setf (ldb (byte 16 48) value) (ldb (byte 16 16) offset)
          (ldb (byte 1 47) value) (if present 1 0)
          (ldb (byte 2 45) value) dpl
          (ldb (byte 4 40) value) (if interrupt-gate-p
                                      #b1110
                                      #b1111)
          (ldb (byte 3 16) value) (or ist 0)
          (ldb (byte 16 16) value) segment
          (ldb (byte 16 0) value) (ldb (byte 16 0) offset))
    value))

(defun create-low-level-interrupt-support ()
  "Generate the IDT and the ISR thunks that call into Lisp.
Return the size & address of the IDT, suitable for use with LIDT.
Doing this in the cold-generator avoids needing to put low-level single-use
setup code into the supervisor.
The bootloader provides a per-cpu GDT & TSS."
  ;; For maximum flexibility, make the IDT as large as possible.
  (let* ((idt-size 256)
         (idt (allocate (1+ (* idt-size 2)) :static))) ; IDT entries are 16 bytes.
    ;; Create IDT.
    ;; IDT entries in 64-bit mode are 128 bits long, there is no ub128 vector type,
    ;; so a doubled-up ub64 vector is used instead.
    (setf (word idt) (array-header sys.int::+object-tag-array-unsigned-byte-64+ (* idt-size 2)))
    (dotimes (i idt-size)
      (setf (word (+ idt (* i 2))) 0
            (word (+ idt (* i 2) 1)) 0))
    (setf (cold-symbol-value 'sys.int::+interrupt-descriptor-table+) (make-value idt sys.int::+tag-object+))
    ;; Generate the ISR thunks.
    (let* ((exception-isrs (loop
                              for (handler error-code-p) in *cpu-exception-info*
                              collect
                                (when handler
                                  (create-exception-isr handler error-code-p))))
           (user-interrupt-isrs (loop
                                   for i from 32 below idt-size
                                   collect (create-user-interrupt-isr i)))
           (common-code (compile-lap-function *common-interrupt-code*
                                              :area :static ; ### should be :support.
                                              :position-independent nil))
           (common-user-code (compile-lap-function *common-user-interrupt-code*
                                                   :area :static ; ### should be :support.
                                                   :position-independent nil
                                                   :extra-symbols (list (cons 'interrupt-common (+ (* common-code 8) 16))))))
      (loop
         for isr in (append exception-isrs user-interrupt-isrs)
         for i from 0
         when isr do
           ;; Assemble the ISR and update the IDT entry
           (let* ((addr (compile-lap-function isr
                                              :area :static ; ### should be :support.
                                              :position-independent nil
                                              :extra-symbols (list (cons 'interrupt-common (+ (* common-code 8) 16))
                                                                   (cons 'user-interrupt-common (+ (* common-user-code 8) 16)))))
                  (entry (make-idt-entry :offset (+ (* addr 8) 16) :segment 8)))
             (setf (word (+ idt 1 (* i 2))) (ldb (byte 64 0) entry)
                   (word (+ idt 1 (* i 2) 1)) (ldb (byte 64 64) entry)))))
    (values (1- (* idt-size 16)) (+ (* idt 8) 8))))

(defun make-image (image-name &key extra-source-files)
  (let ((*word-locks* (make-hash-table))
        (*static-offset* 0)
        (*dynamic-offset* 0)
        (*stack-offset* 0)
        (*static-data* (make-array #x200000 :element-type '(unsigned-byte 8) :adjustable t))
        (*dynamic-data* (make-array #x200000 :element-type '(unsigned-byte 8) :adjustable t))
        (*pending-fixups* '())
        (*symbol-table* (make-hash-table :test 'equal))
        (*reverse-symbol-table* (make-hash-table))
        (*fref-table* (make-hash-table :test 'equal))
        (*struct-table* (make-hash-table))
        (*undefined-function-address* nil)
        (*function-map* '())
        (*string-dedup-table* (make-hash-table :test 'equal))
        (initial-process)
        (cl-symbol-names (with-open-file (s "cl-symbols.lisp-expr") (read s)))
        (system-symbol-names (remove-duplicates
                              (iter (for sym in-package :system external-only t)
                                    (collect (symbol-name sym)))
                              :test #'string=))
        idt-size idt-pointer)
    ;; Generate the support objects. NIL/T/etc, and the initial process.
    (create-support-objects)
    (setf (values idt-size idt-pointer) (create-low-level-interrupt-support))
    (setf initial-process (create-initial-stack-group))
    ;; The system needs to know where the undefined function value is.
    ;; Put it in the value cell of a symbol, not the function cell.
    ;; If it were in the function cell, then the symbol would appear to be unbound.
    ;; Might not be up to date with the fref changes?
    (setf (cold-symbol-value 'sys.int::*undefined-function-thunk*) (make-value *undefined-function-address* sys.int::+tag-object+))
    ;; Load all cold source files, emitting the top-level forms into an array
    ;; FIXME: Top-level forms generally show up as functions in .LLF files,
    ;;        this should be a vector of callable functions, not evalable forms.
    (let ((*load-time-evals* '()))
      (load-compiler-builtins)
      (load-source-files *source-files* t)
      (generate-toplevel-form-array (reverse *load-time-evals*) 'sys.int::*cold-toplevel-forms*))
    ;; Certain cold LLF files are special and must be deferred until after the
    ;; cold load has done a bit of bootstrapping. Put those top-level forms
    ;; in a special symbol.
    (iter (for (file symbol) in *special-source-files*)
          (let ((*load-time-evals* '()))
            (load-source-file file nil)
            (generate-toplevel-form-array (reverse *load-time-evals*) symbol)))
    ;; Extra LLF files can be included in the cold load for processing after bootstrap is over.
    (let ((*load-time-evals* '()))
      (load-source-files extra-source-files nil)
      (generate-toplevel-form-array (reverse *load-time-evals*) 'sys.int::*additional-cold-toplevel-forms*))
    #+nil(format t "Saving Unifont...~%")
    #+nil(save-unifont-data "tools/unifont-5.1.20080820.hex")
    #+nil(format t "Saving Unicode data...~%")
    #+nil(multiple-value-bind (planes name-store encoding-table name-trie)
        (build-unicode:generate-unicode-data-tables (build-unicode:read-unicode-data "tools/UnicodeData.txt"))
      (setf (cold-symbol-value 'sys.int::*unicode-info*) (save-object planes :static)
            (cold-symbol-value 'sys.int::*unicode-name-store*) (save-ub8-vector name-store :static)
            (cold-symbol-value 'sys.int::*unicode-encoding-table*) (save-object encoding-table :static)
            (cold-symbol-value 'sys.int::*unicode-name-trie*) (save-object name-trie :static)))
    #+nil(format t "Saving PCI IDs...~%")
    #+nil(let* ((pci-ids (build-pci-ids:build-pci-ids "tools/pci.ids"))
           (object (save-object pci-ids :static)))
      (setf (cold-symbol-value 'sys.int::*pci-ids*) object))
    ;; Poke a few symbols to ensure they exist.
    (mapc (lambda (sym) (symbol-address (string sym) "SYSTEM.INTERNALS"))
          '(sys.int::*initial-obarray* sys.int::*initial-keyword-obarray*
            sys.int::*initial-fref-obarray* sys.int::*initial-structure-obarray*
            sys.int::*newspace-offset* sys.int::*semispace-size* sys.int::*newspace* sys.int::*oldspace*
            sys.int::*static-bump-pointer* sys.int::*static-area-size* sys.int::*static-mark-bit*
            sys.int::*stack-bump-pointer*
            sys.int::*static-area* sys.int::*static-area-hint*
            #+nil sys.int::*unifont-bmp* #+nil sys.int::*unifont-bmp-data*
            #+nil sys.int::*unicode-info* #+nil sys.int::*unicode-name-store*
            #+nil sys.int::*unicode-encoding-table* #+nil sys.int::*unicode-name-trie*
            ))
    ;; Make sure there's a keyword for each package.
    (iter (for ((nil . package-name) nil) in-hashtable *symbol-table*)
          (symbol-address package-name "KEYWORD"))
    ;; Poke all the CL & SYSTEM symbols
    (dolist (name cl-symbol-names)
      (symbol-address name "COMMON-LISP"))
    (dolist (name system-symbol-names)
      (symbol-address name "SYSTEM"))
    (generate-obarray *symbol-table* 'sys.int::*initial-obarray*)
    (generate-fref-obarray *fref-table* 'sys.int::*initial-fref-obarray*)
    (generate-struct-obarray *struct-table* 'sys.int::*initial-structure-obarray*)
    ;; Initialize GC twiddly bits.
    (flet ((set-value (symbol value)
             (format t "~A is ~X~%" symbol value)
             (setf (cold-symbol-value symbol) (make-fixnum value))))
      (set-value 'sys.int::*newspace-offset* *dynamic-offset*)
      (set-value 'sys.int::*semispace-size* (/ *dynamic-area-semispace-limit* 8))
      (set-value 'sys.int::*newspace* *dynamic-area-base*)
      (set-value 'sys.int::*oldspace* (+ *dynamic-area-base* (/ *dynamic-area-size* 2)))
      (set-value 'sys.int::*large-static-area* *static-area-base*)
      (set-value 'sys.int::*large-static-area-hint* 0)
      (set-value 'sys.int::*small-static-area* (+ *static-area-base* *large-static-area-size*))
      (set-value 'sys.int::*small-static-area-hint* 0)
      (set-value 'sys.int::*static-mark-bit* 0)
      (set-value 'sys.int::*bump-pointer* (+ *linear-map* *physical-load-address* (* (total-image-size) 8)))
      (set-value 'sys.int::*stack-bump-pointer* (+ (* *stack-offset* 8) *stack-area-base*))
      (set-value 'sys.int::*stack-bump-pointer-limit* (+ (* (1+ (ceiling *stack-offset* #x40000)) #x200000) *stack-area-base*)))
    ;; Write the boundary tag for the static area's free part.
    ;; This does not write the boundary tag for the small static area!
    (let ((*static-offset* (+ *static-offset* 2)))
      (format t "~:D/~:D words free in static space.~%"
              (- (truncate *large-static-area-size* 8) (- *static-offset* 2))
              (truncate *large-static-area-size* 8))
      (setf (word (+ (truncate *static-area-base* 8) *static-offset* -2)) (- (truncate *large-static-area-size* 8) *static-offset*)
            (word (+ (truncate *static-area-base* 8) *static-offset* -1)) #b100))
    (apply-fixups *pending-fixups*)
    (write-map-file image-name *function-map*)
    (write-image image-name
                 (make-value (function-reference 'sys.int::bootloader-entry-point)
                             sys.int::+tag-object+)
                 initial-process
                 idt-size idt-pointer)))

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
(defconstant +llf-proper-list+ #x11)
(defconstant +llf-package+ #x12)
(defconstant +llf-integer-vector+ #x13)
(defconstant +llf-add-backlink+ #x14)
(defconstant +llf-bit-vector+ #x18)
(defconstant +llf-function-reference+ #x19)

(defun make-bignum (value)
  (let* ((length (ceiling (1+ (integer-length value)) 64))
         (address (allocate (1+ length) :static)))
    (setf (word address) (array-header sys.int::+object-tag-bignum+ length))
    (dotimes (i length)
      (setf (word (+ address 1 i)) (ldb (byte 64 (* i 64)) value)))
    (make-value address sys.int::+tag-object+)))

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
         (object-address (allocate 6 :static))
         (string-data (make-array len :element-type '(unsigned-byte 32)))
         (min-width 1))
    ;; Read all characters and figure out how wide the data vector must be.
    (dotimes (i len)
      (let ((ch (load-character stream)))
        (setf (aref string-data i) ch)
        (setf min-width (min min-width
                             (cond ((>= ch (expt 2 16)) 4)
                                   ((>= ch (expt 2 8)) 2)
                                   (t 1))))))
    (let ((data-value (ecase min-width
                        (4 (save-ub32-vector string-data :static))
                        (2 (save-ub16-vector string-data :static))
                        (1 (save-ub8-vector string-data :static)))))
      ;; String container
      (setf (word (+ object-address 0)) (array-header sys.int::+object-tag-string+ 1)
            (word (+ object-address 1)) data-value
            (word (+ object-address 2)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
            (word (+ object-address 3)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
            (word (+ object-address 4)) (make-fixnum len))
      (make-value object-address sys.int::+tag-object+))))

(defun load-string* (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type 'character)))
    (dotimes (i len)
      (setf (aref seq i) (code-char (load-character stream))))
    seq))

(defun ensure-structure-layout-compatible (definition slots)
  (unless (equal (third definition) slots)
    (error "Incompatible redefinition of structure. ~S ~S~%" definition slots)))

(defun load-structure-definition (name* slots* parent* area*)
  (let* ((name (extract-object name*))
         (slots (extract-object slots*))
         (definition (gethash name *struct-table*)))
    (cond (definition
           (ensure-structure-layout-compatible definition slots)
           (make-value (first definition) sys.int::+tag-object+))
          (t (let ((address (allocate 7 :static)))
               (setf (word address) (array-header sys.int::+object-tag-structure-object+ 6))
               (setf (word (+ address 1)) 0) ; uninitialized definition
               (setf (word (+ address 2)) name*)
               (setf (word (+ address 3)) slots*)
               (setf (word (+ address 4)) parent*)
               (setf (word (+ address 5)) area*)
               (setf (word (+ address 6)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))
               (setf (gethash name *struct-table*) (list address name slots))
               (make-value address sys.int::+tag-object+))))))

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
      ((#.sys.int::+tag-fixnum-000+ #.sys.int::+tag-fixnum-001+
        #.sys.int::+tag-fixnum-010+ #.sys.int::+tag-fixnum-011+
        #.sys.int::+tag-fixnum-100+ #.sys.int::+tag-fixnum-101+
        #.sys.int::+tag-fixnum-110+ #.sys.int::+tag-fixnum-111+)
       (ash value (- sys.int::+n-fixnum-bits+)))
      (#.sys.int::+tag-character+
       (code-char (ash value -4)))
      (#.sys.int::+tag-cons+
       (cons (extract-object (word address))
             (extract-object (word (1+ address)))))
      (#.sys.int::+tag-object+
       (let* ((header (word address))
              (tag (ldb (byte sys.int::+array-type-size+
                              sys.int::+array-type-shift+)
                        header)))
         (ecase tag
           (#.sys.int::+object-tag-symbol+
            (destructuring-bind (name . package)
                (gethash address *reverse-symbol-table*)
              (unless (and name package)
                (error "Unknown symbol ~S at address ~X?"
                       (extract-object (word address))
                       address))
              (intern name package)))
           ((#.sys.int::+object-tag-simple-string+
             #.sys.int::+object-tag-string+)
            (map 'simple-string 'code-char (extract-object (word (1+ address)))))
           ((#.sys.int::+object-tag-array-unsigned-byte-8+)
            (extract-array address 8))
           ((#.sys.int::+object-tag-array-unsigned-byte-16+)
            (extract-array address 16))
           ((#.sys.int::+object-tag-array-unsigned-byte-32+)
            (extract-array address 32))))))))

(defun vintern (name package)
  (make-value (symbol-address name package) sys.int::+tag-object+))

(defun vcons (car cdr)
  (let ((address (allocate 2)))
    (setf (word address) car
          (word (1+ address)) cdr)
    (make-value address sys.int::+tag-cons+)))

(defun vlist (&rest args)
  (if args
      (vcons (first args) (apply 'vlist (rest args)))
      (vintern "NIL" "COMMON-LISP")))

(defun load-llf-function (stream stack)
  ;; n constants on stack.
  ;; list of fixups on stack.
  ;; +llf-function+
  ;; tag. (byte)
  ;; mc size in bytes. (integer)
  ;; number of constants. (integer)
  ;; gc-info-length in bytes. (integer)
  ;; mc
  ;; gc-info
  (let* ((tag (read-byte stream))
         (mc-length (load-integer stream))
         ;; mc-length does not include the 16 byte function header.
         (mc (make-array (* (ceiling (+ mc-length 16) 8) 8)
                         :element-type '(unsigned-byte 8)
                         :initial-element 0))
         (n-constants (load-integer stream))
         (gc-info-length (load-integer stream))
         (gc-info (make-array (* (ceiling gc-info-length 8) 8)
                              :element-type '(unsigned-byte 8)))
         (fixups (vector-pop stack))
         ;; Pull n constants off the value stack.
         (constants (subseq stack (- (length stack) n-constants)))
         (total-size (+ (* (ceiling (+ mc-length 16) 16) 2)
                        n-constants
                        (ceiling gc-info-length 8)))
         (address (allocate total-size :static)))
    ;; Pop constants off.
    (decf (fill-pointer stack) n-constants)
    ;; Read mc bytes.
    (read-sequence mc stream :start 16 :end (+ 16 mc-length))
    ;; Copy machine code bytes.
    (dotimes (i (ceiling (+ mc-length 16) 8))
      (setf (word (+ address i)) (nibbles:ub64ref/le mc (* i 8)))
      #+nil(when (not (member i '(0 1)))
        (lock-word (+ address i))))
    ;; Read GC bytes.
    (read-sequence gc-info stream :end gc-info-length)
    ;; Copy GC bytes.
    (dotimes (i (ceiling gc-info-length 8))
      (setf (word (+ address
                     (* (ceiling (+ mc-length 16) 16) 2)
                     n-constants
                     i))
            (nibbles:ub64ref/le gc-info (* i 8)))
      (lock-word (+ address
                    (* (ceiling (+ mc-length 16) 16) 2)
                    n-constants
                    i)))
    ;; Set function header.
    (setf (word address) 0)
    (setf (word (1+ address)) (* (+ address 2) 8))
    (lock-word (1+ address))
    (setf (ldb (byte  8 0) (word address)) (ash tag sys.int::+array-type-shift+)
          (ldb (byte 16 16) (word address)) (ceiling (+ mc-length 16) 16)
          (ldb (byte 16 32) (word address)) n-constants
          (ldb (byte 16 48) (word address)) gc-info-length)
    (lock-word address)
    ;; Set constant pool.
    (dotimes (i (length constants))
      (setf (word (+ address (* (ceiling (+ mc-length 16) 16) 2) i))
            (aref constants i))
      (lock-word (+ address (* (ceiling (+ mc-length 16) 16) 2) i)))
    ;; Add to the function map.
    (push (list address (extract-object (aref constants 0)))
          *function-map*)
    ;; Add fixups to the list.
    (dolist (fixup (extract-object fixups))
      (assert (>= (cdr fixup) 16))
      (push (list (car fixup) address (cdr fixup) :signed32 (aref constants 1))
            *pending-fixups*))
    ;; Done.
    (make-value address sys.int::+tag-object+)))

(defun load-llf-vector (stream stack)
  (let* ((len (load-integer stream))
         (address (allocate (1+ len))))
    ;; Header word.
    (setf (word address) (array-header sys.int::+object-tag-array-t+ len))
    ;; Drop vector values and copy them into the image.
    (decf (fill-pointer stack) len)
    (dotimes (i len)
      (setf (word (+ address 1 i)) (aref stack (+ (length stack) i))))
    (make-value address sys.int::+tag-object+)))

(defun generate-fref-name (name)
  "Turn a Lisp name into an address."
  (cond
    ((symbolp name)
     (vintern (symbol-name name) (package-name (symbol-package name))))
    ((and (listp name)
          (eql (length name) 2)
          (eql (first name) 'setf)
          (symbolp (second name)))
     (vlist (vintern "SETF" "COMMON-LISP")
            (vintern (symbol-name (second name))
                     (package-name (symbol-package (second name))))))
    (t (error "Bad function name ~S." name))))

(defun function-reference (name)
  "Get the function-reference associated with NAME, returning an untagged address.
Tag with +TAG-OBJECT+."
  (let ((fref (gethash name *fref-table*)))
    (unless fref
      ;; They don't need to go in the dynamic area, but they'll probably last forever.
      ;; So they go in the static area for now.
      (setf fref (allocate 4 :static))
      (setf (word (+ fref 0)) (array-header sys.int::+object-tag-function-reference+ 0)
            (word (+ fref 1)) (generate-fref-name name)
            (word (+ fref 2)) (vintern "NIL" "COMMON-LISP")
            (word (+ fref 3)) (word (1+ *undefined-function-address*)))
      (setf (gethash name *fref-table*) fref)
      (when (symbolp name)
        (let ((sym-addr (symbol-address (symbol-name name)
                                        (package-name (symbol-package name)))))
          (setf (word (+ sym-addr 4)) (make-value fref sys.int::+tag-object+)))))
    fref))

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
       (make-value (symbol-address name package)
                   sys.int::+tag-object+)))
    (#.+llf-uninterned-symbol+
     (let ((plist (vector-pop stack))
           (fn (vector-pop stack))
           (value (vector-pop stack))
           (name (vector-pop stack))
           (address (allocate 6)))
       ;; FN and VALUE may be the unbound tag.
       (setf (word (+ address 0)) (array-header sys.int::+object-tag-symbol+ 0)
             (word (+ address 1)) name
             (word (+ address 2)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
             (word (+ address 3)) value
             (word (+ address 4)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
             (word (+ address 5)) plist)
       (unless (eql fn (unbound-value))
         (error "Uninterned symbol with function not supported."))
       (make-value address sys.int::+tag-object+)))
    (#.+llf-unbound+ (unbound-value))
    (#.+llf-string+ (load-string stream))
    (#.+llf-integer+
     (let ((value (load-integer stream)))
       (typecase value
         ((signed-byte 63) (make-fixnum value))
         (t (make-bignum value)))))
    (#.+llf-invoke+
     ;; `(funcall ',fn)
     (let* ((fn (vector-pop stack))
            (form (vlist (vintern "FUNCALL" "COMMON-LISP")
                         (vlist (vintern "QUOTE" "COMMON-LISP")
                                fn))))
       (push form *load-time-evals*))
     nil)
    (#.+llf-setf-fdefinition+
     (let* ((base-name (vector-pop stack))
            (fn-value (vector-pop stack))
            (name (extract-object base-name)))
       (cond (*load-should-set-fdefinitions*
              (let ((fref (function-reference name)))
                (setf (word (+ fref 2)) fn-value
                      (word (+ fref 3)) (word (1+ (pointer-part fn-value))))))
             (t ;; `(funcall #'(setf symbol-function) ',fn-value ',name)
              (push (vlist (vintern "FUNCALL" "COMMON-LISP")
                           (vlist (vintern "FUNCTION" "COMMON-LISP") (vlist (vintern "SETF" "COMMON-LISP") (vintern "FDEFINITION" "COMMON-LISP")))
                           (vlist (vintern "QUOTE" "COMMON-LISP") fn-value)
                           (vlist (vintern "QUOTE" "COMMON-LISP") base-name))
                    *load-time-evals*))))
     nil)
    (#.+llf-simple-vector+
     (load-llf-vector stream stack))
    (#.+llf-character+
     (logior (ash (load-character stream) 4)
             sys.int::+tag-character+))
    (#.+llf-structure-definition+
     (let ((area (vector-pop stack))
           (parent (vector-pop stack))
           (slots (vector-pop stack))
           (name (vector-pop stack)))
       (load-structure-definition name slots parent area)))
    (#.+llf-single-float+
     (logior (ash (load-integer stream) 32)
             sys.int::+tag-single-float+))
    (#.+llf-proper-list+
     (let ((list (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))
           (length (load-integer stream)))
       (dotimes (i length)
         (setf list (vcons (vector-pop stack) list)))
       list))
    (#.+llf-integer-vector+
     (let* ((len (load-integer stream))
            (address (allocate (1+ len))))
       ;; Header word.
       (setf (word address) (array-header sys.int::+object-tag-array-t+ len))
       (dotimes (i len)
         (let ((value (load-integer stream)))
           (setf (word (+ address 1 i)) (typecase value
                                          ((signed-byte 63) (make-fixnum value))
                                          (t (make-bignum value))))))
       (make-value address sys.int::+tag-object+)))
    (#.+llf-bit-vector+
     (let* ((len (load-integer stream))
            (address (allocate (1+ (ceiling len 64)))))
       ;; Header word.
       (setf (word address) (array-header sys.int::+object-tag-array-bit+ len))
       (dotimes (i (ceiling len 8))
         (let ((octet (read-byte stream)))
           (multiple-value-bind (word offset)
               (truncate i 8)
             (setf (ldb (byte 8 (* offset 8))
                        (word (+ address 1 word)))
                   octet))))
       (make-value address sys.int::+tag-object+)))
    (#.+llf-function-reference+
     (let* ((name (vector-pop stack))
            (truname (extract-object name)))
       (make-value (function-reference truname)
                   sys.int::+tag-object+)))))

(defun load-llf (stream)
  (let ((omap (make-hash-table))
        (stack (make-array 64 :adjustable t :fill-pointer 0)))
    (loop (let ((command (read-byte stream)))
            (case command
              (#.+llf-end-of-load+
               (return))
              (#.+llf-backlink+
               (let ((id (load-integer stream)))
                 (multiple-value-bind (value value-p)
                     (gethash id omap)
                   (unless value-p
                     (error "Unknown backlink ID ~D." id))
                   (vector-push-extend value stack))))
              (#.+llf-add-backlink+
               (let ((id (load-integer stream)))
                 (multiple-value-bind (existing-value existing-value-p)
                     (gethash id omap)
                   (declare (ignore existing-value))
                   (when existing-value-p
                     (error "Duplicate backlink ID ~D." id)))
                 (setf (gethash id omap) (vector-pop stack))))
              (t (let ((value (load-one-object command stream stack)))
                   (when value
                     (vector-push-extend value stack)))))))))

(defun resolve-function-name (value)
  (let ((name (extract-object value)))
    (cond ((symbolp name)
           value)
          ((and (= (list-length name) 2)
                (eql (first name) 'setf)
                (symbolp (second name)))
           (make-value (resolve-setf-symbol (second name)) sys.int::+tag-object+))
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
  (destructuring-bind (what address byte-offset type debug-info) fixup
    (let* ((value (if (consp what)
                      (etypecase (second what)
                        (symbol
                         (make-value (symbol-address (symbol-name (second what))
                                                     (canonical-symbol-package (second what))
                                                     t)
                                     sys.int::+tag-object+))
                        (sys.c::cross-fref
                         (make-value (function-reference (sys.c::cross-fref-name (second what)))
                                     sys.int::+tag-object+)))
                      (ecase what
                        ((nil t) (make-value (symbol-address (symbol-name what) "COMMON-LISP")
                                             sys.int::+tag-object+))
                        (:unbound-tls-slot
                         (make-value *unbound-tls-slot-address*
                                     sys.int::+tag-object+))
                        (:unbound-value (unbound-value))
                        ((:undefined-function undefined-function)
                         (make-value *undefined-function-address* sys.int::+tag-object+)))))
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
