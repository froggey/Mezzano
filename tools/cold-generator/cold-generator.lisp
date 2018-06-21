;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :cold-generator)

(defparameter *supervisor-source-files*
  '("supervisor/entry.lisp"
    ("supervisor/x86-64/cpu.lisp" :x86-64)
    ("supervisor/arm64/cpu.lisp" :arm64)
    "supervisor/interrupts.lisp"
    ("supervisor/x86-64/interrupts.lisp" :x86-64)
    ("supervisor/arm64/interrupts.lisp" :arm64)
    ("supervisor/arm64/gic.lisp" :arm64)
    "supervisor/debug.lisp"
    "supervisor/serial.lisp"
    ("supervisor/uart.lisp" :arm64)
    "supervisor/disk.lisp"
    "supervisor/partition.lisp"
    "supervisor/ata.lisp"
    "supervisor/ahci.lisp"
    "supervisor/cdrom.lisp"
    "supervisor/thread.lisp"
    ("supervisor/x86-64/thread.lisp" :x86-64)
    ("supervisor/arm64/thread.lisp" :arm64)
    "supervisor/sync.lisp"
    "supervisor/physical.lisp"
    "supervisor/snapshot.lisp"
    ("supervisor/x86-64/snapshot.lisp" :x86-64)
    ("supervisor/arm64/snapshot.lisp" :arm64)
    "supervisor/store.lisp"
    "supervisor/pager.lisp"
    ("supervisor/x86-64/pager.lisp" :x86-64)
    ("supervisor/arm64/pager.lisp" :arm64)
    "supervisor/time.lisp"
    ("supervisor/x86-64/time.lisp" :x86-64)
    ("supervisor/arm64/time.lisp" :arm64)
    "supervisor/ps2.lisp"
    "supervisor/video.lisp"
    "supervisor/pci.lisp"
    "supervisor/virtio.lisp"
    "supervisor/virtio-pci.lisp"
    "supervisor/virtio-mmio.lisp"
    "supervisor/virtio-block.lisp"
    "supervisor/virtio-input.lisp"
    "supervisor/virtio-gpu.lisp"
    "supervisor/virtualbox.lisp"
    "supervisor/profiler.lisp"
    "supervisor/support.lisp"
    "supervisor/acpi.lisp"
    "supervisor/efi.lisp"
    ("supervisor/x86-64/platform.lisp" :x86-64)
    ("supervisor/arm64/platform.lisp" :arm64)
    "runtime/runtime.lisp"
    ("runtime/runtime-x86-64.lisp" :x86-64)
    ("runtime/runtime-arm64.lisp" :arm64)
    "system/data-types.lisp"
    "runtime/allocate.lisp"
    "runtime/cons.lisp"
    "runtime/numbers.lisp"
    ("runtime/float-x86-64.lisp" :x86-64)
    ("runtime/float-arm64.lisp" :arm64)
    "runtime/string.lisp"
    "runtime/array.lisp"
    "runtime/struct.lisp"
    "runtime/symbol.lisp"
    "runtime/function.lisp"
    "supervisor/fdt.lisp"))

(defparameter *source-files*
  '("system/cold-start.lisp"
    "system/defstruct.lisp"
    "system/cons.lisp"
    "system/sequence.lisp"
    "system/runtime-array.lisp"
    "system/array.lisp"
    "system/printer.lisp"
    "system/stuff.lisp"
    "system/runtime-support.lisp"
    "system/type.lisp"
    "system/setf.lisp"
    "system/cas.lisp"
    "system/string.lisp"
    "system/hash-table.lisp"
    "system/runtime-numbers.lisp"
    ("system/bignum-x86-64.lisp" :x86-64)
    ("system/bignum-arm64.lisp" :arm64)
    "system/numbers.lisp"
    "system/gc.lisp"
    "system/room.lisp"
    "system/reader.lisp"
    "system/character.lisp"
    "system/backquote.lisp"
    "system/format.lisp"
    "system/defmacro.lisp"
    "system/basic-macros.lisp"
    "system/parse.lisp"
    "system/load.lisp"
    "system/time.lisp"
    "system/delimited-continuations.lisp"
    ("system/delimited-continuations-x86-64.lisp" :x86-64)
))

(defparameter *special-source-files*
  '(("system/packages.lisp" sys.int::*package-system*)))

(defparameter *warm-source-files*
  '("system/clos/package.lisp"
    "system/clos/macros.lisp"
    "system/clos/single-dispatch-emf-table.lisp"
    "system/clos/boot.lisp"
    "system/clos/closette.lisp"
    "system/clos/method-combination.lisp"
    "system/describe.lisp"
    "system/runtime-misc.lisp"
    "system/condition.lisp"
    "system/restarts.lisp"
    "system/error.lisp"
    "system/coerce.lisp"
    "system/debug.lisp"
    "system/full-eval.lisp"
    "system/fast-eval.lisp"
    "system/eval.lisp"
    "system/gray-streams.lisp"
    "system/standard-streams.lisp"
    "system/stream.lisp"
    "system/ansi-loop.lisp"
    "system/environment.lisp"
    "compiler/package.lisp"
    "compiler/compiler.lisp"
    "compiler/lap.lisp"
    "compiler/lap-x86.lisp"
    "compiler/lap-arm64.lisp"
    "compiler/environment.lisp"
    "compiler/global-environment.lisp"
    "compiler/ast.lisp"
    "compiler/ast-generator.lisp"
    "compiler/pass1.lisp"
    "compiler/constprop.lisp"
    "compiler/simplify.lisp"
    "compiler/lift.lisp"
    "compiler/inline.lisp"
    "compiler/kill-temps.lisp"
    "compiler/keyword-arguments.lisp"
    "compiler/simplify-arguments.lisp"
    "compiler/dynamic-extent.lisp"
    "compiler/codegen-x86-64.lisp"
    "compiler/builtins-x86-64/builtins.lisp"
    "compiler/builtins-x86-64/array.lisp"
    "compiler/builtins-x86-64/character.lisp"
    "compiler/builtins-x86-64/cons.lisp"
    "compiler/builtins-x86-64/memory.lisp"
    "compiler/builtins-x86-64/misc.lisp"
    "compiler/builtins-x86-64/numbers.lisp"
    "compiler/builtins-x86-64/objects.lisp"
    "compiler/builtins-x86-64/unwind.lisp"
    "compiler/branch-tension.lisp"
    "compiler/codegen-arm64.lisp"
    "compiler/builtins-arm64/builtins.lisp"
    "compiler/builtins-arm64/cons.lisp"
    "compiler/builtins-arm64/memory.lisp"
    "compiler/builtins-arm64/misc.lisp"
    "compiler/builtins-arm64/numbers.lisp"
    "compiler/builtins-arm64/objects.lisp"
    "compiler/builtins-arm64/unwind.lisp"
    "compiler/lower-environment.lisp"
    "compiler/lower-special-bindings.lisp"
    "compiler/value-aware-lowering.lisp"
    "compiler/simplify-control-flow.lisp"
    "compiler/blexit.lisp"
    "compiler/transforms.lisp"
    "compiler/backend/backend.lisp"
    "compiler/backend/instructions.lisp"
    "compiler/backend/cfg.lisp"
    "compiler/backend/analysis.lisp"
    "compiler/backend/dominance.lisp"
    "compiler/backend/convert-ast.lisp"
    "compiler/backend/multiple-values.lisp"
    "compiler/backend/ssa.lisp"
    "compiler/backend/passes.lisp"
    "compiler/backend/debug.lisp"
    "compiler/backend/register-allocation.lisp"
    "compiler/backend/canon.lisp"
    "compiler/backend/x86-64/x86-64.lisp"
    "compiler/backend/x86-64/target.lisp"
    "compiler/backend/x86-64/codegen.lisp"
    "compiler/backend/x86-64/builtin.lisp"
    "compiler/backend/x86-64/misc.lisp"
    "compiler/backend/x86-64/object.lisp"
    "compiler/backend/x86-64/memory.lisp"
    "compiler/backend/x86-64/number.lisp"
    "compiler/backend/x86-64/simd.lisp"
    "compiler/backend/arm64/arm64.lisp"
    "compiler/backend/arm64/target.lisp"
    "compiler/backend/arm64/codegen.lisp"
    "compiler/backend/arm64/builtin.lisp"
    "compiler/backend/arm64/misc.lisp"
    "compiler/backend/arm64/object.lisp"
    "compiler/backend/arm64/number.lisp"
    ("runtime/simd.lisp" :x86-64)
    "system/file-compiler.lisp"
    "system/xp-package.lisp"
    "system/xp.lisp"
    "system/xp-format.lisp"
    "system/xp-printers.lisp"
    "system/profiler.lisp"
    "drivers/network-card.lisp"
    "drivers/virtio-net.lisp"
    ("drivers/rtl8168.lisp" :x86-64)
    "drivers/sound.lisp"
    ("drivers/intel-hda.lisp" :x86-64)
    "net/package.lisp"
    "net/network.lisp"
    "net/ethernet.lisp"
    "net/arp.lisp"
    "net/ip.lisp"
    "net/udp.lisp"
    "net/tcp.lisp"
    "net/dns.lisp"
    "net/network-setup.lisp"
    "file/fs.lisp"
    "file/remote.lisp"
    "config.lisp"
    "ipl.lisp"))

(defparameter *cl-symbol-list-file* "tools/cl-symbols.lisp-expr")
(defparameter *8x8-debug-font* "tools/font8x8")
(defparameter *unifont* "tools/unifont-5.1.20080820.hex")
(defparameter *unicode-data* "tools/UnicodeData.txt")
(defparameter *pci-ids* "tools/pci.ids")

(defvar *symbol-table*)
(defvar *reverse-symbol-table*)
;; Hash-table mapping function names to function references.
(defvar *fref-table*)
(defvar *struct-table*)
(defvar *unbound-value-address*)
(defvar *undefined-function-address*)
(defvar *closure-trampoline-address*)
(defvar *f-i-trampoline-address*)
(defvar *load-time-evals*)
(defvar *string-dedup-table*)
(defvar *structure-definition-definition*)
(defvar *structure-slot-definition-definition*)
(defvar *image-to-cross-slot-definitions*)

(defvar *function-map*)
(defvar *pending-fixups*)

;;; Memory allocation.

;; Wired area starts near 0.
(defconstant +wired-area-base+ sys.int::+allocation-minimum-alignment+)
;; Pinned at 512G.
(defconstant +pinned-area-base+ (* 512 1024 1024 1024))
;; Wired area stops at 2G, below the pinned area.
(defconstant +wired-area-limit+ (* 2 1024 1024 1024))

;; Wired stack area starts at the bottom of the stack area.
(defconstant +wired-stack-area-base+ 0)
;; Not set to 512GB because bootloader is slow & dumb.
(defconstant +wired-stack-area-limit+ (* 2 1024 1024 1024))
;; Leave a gap, for future expansion.
(defconstant +stack-area-base+ (* 512 1024 1024 1024))

;; Past this, the address starts to infringe on the address info bits.
;; Technically, the pinned area doesn't care about bit 44, but be consistent.
(defconstant +area-limit+ (expt 2 44))

(defvar *wired-area-bump*)
(defvar *wired-area-data*)
(defvar *wired-area-store*)
(defvar *pinned-area-bump*)
(defvar *pinned-area-data*)
(defvar *pinned-area-store*)
(defvar *general-area-bump*)
(defvar *general-area-data*)
(defvar *general-area-store*)
(defvar *cons-area-bump*)
(defvar *cons-area-data*)
(defvar *cons-area-store*)

(defvar *card-offsets*)

(defstruct stack
  base
  size
  store)

(defvar *stack-list*)
(defvar *stack-area-bump*)
(defvar *stack-area-bytes*)

(defvar *store-bump*)

(defvar *default-general-allocation-area* :general)
(defvar *default-cons-allocation-area* :cons)
(defvar *default-pinned-allocation-area* :pinned)

(defun align-up (value boundary)
  (incf value (1- boundary))
  (- value (rem value boundary)))

(defun allocate-1 (size bump-symbol data-symbol data-offset limit tag name generation)
  (when (>= (+ (symbol-value bump-symbol) size) limit)
    (error "~A area exceeded limit." name))
  (let ((address (logior (symbol-value bump-symbol)
                         (ash tag sys.int::+address-tag-shift+)
                         (cross-cl:dpb generation sys.int::+address-generation+ 0))))
    (incf (symbol-value bump-symbol) size)
    ;; Keep data vector page aligned, but don't expand it too often.
    ;; Keeping it 2MB aligned is important - WRITE-IMAGE relies on this to
    ;; provide zeros in unallocated parts of the area.
    (let ((dv-size (align-up (symbol-value bump-symbol) sys.int::+allocation-minimum-alignment+)))
      (when (not (eql (- dv-size data-offset)
                      (length (symbol-value data-symbol))))
        (setf (symbol-value data-symbol) (adjust-array (symbol-value data-symbol)
                                                       (- dv-size data-offset)
                                                       :element-type '(unsigned-byte 8)
                                                       :initial-element 0))))
    ;; Update the card table starts.
    (loop
       for card from (align-up address sys.int::+card-size+) below (+ address size) by sys.int::+card-size+
       do (setf (gethash card *card-offsets*) (cons (- address card) address)))
    (/ address 8)))

(defun allocate (word-count &optional area)
  (when (oddp word-count) (incf word-count))
  (let ((size (* word-count 8)))
    (ecase (or area *default-general-allocation-area*)
      (:wired
       (allocate-1 size '*wired-area-bump* '*wired-area-data* +wired-area-base+ +wired-area-limit+ sys.int::+address-tag-pinned+ "wired" 0))
      (:pinned
       (allocate-1 size '*pinned-area-bump* '*pinned-area-data* +pinned-area-base+ +area-limit+ sys.int::+address-tag-pinned+ "pinned" 0))
      (:general
       (allocate-1 size '*general-area-bump* '*general-area-data* 0 +area-limit+ sys.int::+address-tag-general+ "general" sys.int::+address-generation-2-a+))
      (:cons
       (allocate-1 size '*cons-area-bump* '*cons-area-data* 0 +area-limit+ sys.int::+address-tag-cons+ "cons" sys.int::+address-generation-2-a+)))))

(defun area-for-address (address)
  (let ((byte-address (* address 8)))
    (case (ldb (byte sys.int::+address-tag-size+
                     sys.int::+address-tag-shift+)
               byte-address)
      (#.sys.int::+address-tag-pinned+
       (cond ((<= +pinned-area-base+ byte-address (1- +area-limit+))
              (values (/ (- (logand byte-address (1- +area-limit+)) +pinned-area-base+) 8)
                      *pinned-area-data*))
             ((<= +wired-area-base+ byte-address (1- +wired-area-limit+))
              (values (/ (- (logand byte-address (1- +area-limit+)) +wired-area-base+) 8)
                      *wired-area-data*))
             (t (error "Unknown address #x~X" address))))
      ;;(#.sys.int::+address-tag-stack+)
      (#.sys.int::+address-tag-general+
       (values (/ (logand byte-address (1- +area-limit+)) 8)
               *general-area-data*))
      (#.sys.int::+address-tag-cons+
       (values (/ (logand byte-address (1- +area-limit+)) 8)
               *cons-area-data*))
      (t (error "Unknown address #x~X" address)))))

(defun create-stack (size)
  ;; Lower guard region.
  (incf *stack-area-bump* #x200000)
  (setf size (align-up size #x1000))
  (let* ((address (logior (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+)
                          *stack-area-bump*))
         (info (make-stack :base address :size size)))
    (incf *stack-area-bytes* size)
    (incf *stack-area-bump* (align-up size #x200000))
    (push info *stack-list*)
    info))

(defvar *word-locks*)

(defun lock-word (address)
  (setf (gethash address *word-locks*) t))

(defun (setf word) (new-value address)
  (when (gethash address *word-locks*)
    (error "Attempted to write locked word at ~X~%" address))
  (multiple-value-bind (offset data-vector)
      (area-for-address address)
    (setf (ub64ref/le data-vector (* offset 8)) new-value)))

(defun word (address)
  (multiple-value-bind (offset data-vector)
      (area-for-address address)
    (ub64ref/le data-vector (* offset 8))))

(defun assemble (code-list &rest args)
  (let ((sys.lap:*function-reference-resolver* #'sys.c::resolve-fref))
    (apply #'sys.lap:perform-assembly-using-target
           (sys.c::canonicalize-target sys.c::*target-architecture*)
           code-list
           args)))

(defun compile-lap-function (code &key (area *default-pinned-allocation-area*) extra-symbols constant-values (position-independent t) (name 'sys.int::support-function))
  "Compile a list of LAP code as a function. Constants must only be symbols."
  (let ((base-address (if position-independent
                          0
                          *wired-area-bump*)))
    (multiple-value-bind (mc constants fixups symbols gc-info)
        (assemble (list* (list :d64/le 0 0) code) ; 16 byte header.
          :base-address base-address
          :initial-symbols (list* '(nil . :fixup)
                                  '(t . :fixup)
                                  extra-symbols)
          ;; Name & debug info.
          :info (list name nil))
      (declare (ignore symbols))
      (setf mc (let ((array (make-array (* (ceiling (length mc) 16) 16)
                                        :fill-pointer t
                                        :element-type '(unsigned-byte 8))))
                 (replace array mc)))
      (let ((total-size (+ (* (truncate (length mc) 16) 2)
                           (length constants)
                           (ceiling (length gc-info) 8))))
        (when (oddp total-size) (incf total-size))
        (let ((address (allocate total-size area)))
          ;; Copy machine code into the area.
          (dotimes (i (truncate (length mc) 8))
            (setf (word (+ address i)) (nibbles:ub64ref/le mc (* i 8))))
          ;; Set header word.
          (setf (word address) (function-header (- (length mc) 16) (length constants) (length gc-info)))
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

(defun store-string (string &optional area)
  (let ((object-address (allocate 6 area))
        (data-address (allocate (1+ (ceiling (length string) 8)) area)))
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
  (setf package (canonical-package-name package))
  (or (gethash (cons name package) *symbol-table*)
      (when (not createp)
        (error "Symbol ~A::~A does not exist."
               package
               name))
      ;; Symbols are always wired.
      (let ((address (allocate-symbol name package)))
        (populate-symbol address
                         name (intern package "KEYWORD")
                         (if (string= package "KEYWORD")
                             (make-value address sys.int::+tag-object+)
                             (unbound-value))
                         (string= package "KEYWORD"))
        address)))

(defun allocate-symbol (name package)
  (let ((address (allocate 8 :wired)))
    (setf (gethash address *reverse-symbol-table*) (cons name package)
          (gethash (cons name package) *symbol-table*) address)
    address))

(defun populate-symbol (address name package value &optional is-constant)
  (let ((global-cell (allocate 4 :wired)))
    (setf (word (+ address 0)) (array-header sys.int::+object-tag-symbol+
                                             (if is-constant
                                                 (ash sys.int::+symbol-mode-constant+ (cross-cl:byte-position sys.int::+symbol-header-mode+))
                                                 0)) ; flags & header
          (word (+ address 1)) (make-value (store-string name :wired) sys.int::+tag-object+) ; name
          (word (+ address 2)) (vsym package) ; package
          (word (+ address 3)) (make-value global-cell sys.int::+tag-object+) ; value
          (word (+ address 4)) (vsym nil) ; function
          (word (+ address 5)) (vsym nil) ; plist
          (word (+ address 6)) (vsym t)) ;type
    (setf (word (+ global-cell 0)) (array-header sys.int::+object-tag-array-t+ 3)
          (word (+ global-cell 1)) (vsym nil)
          (word (+ global-cell 2)) (make-value address sys.int::+tag-object+)
          (word (+ global-cell 3)) value)))

(defun populate-structure-definition (address name slots parent area)
  (setf (word (+ address 0)) (array-header sys.int::+object-tag-structure-object+ 6)
        (word (+ address 1)) (make-value *structure-definition-definition* sys.int::+tag-object+)
        (word (+ address 2)) (vsym name)
        (word (+ address 3)) (apply #'vlist (mapcar (lambda (def)
                                                      (apply #'vmake-struct-slot-def (mapcar #'vsym def)))
                                                    slots))
        (word (+ address 4)) (or parent (vsym nil))
        (word (+ address 5)) (vsym area)
        (word (+ address 6)) (vsym nil)
        (gethash name *struct-table*) (list address name slots)))

(defun create-support-objects ()
  "Create NIL, T and the undefined function thunk."
  ;; Create initial symbols. Don't need the unbound value yet.
  (let ((nil-value (allocate-symbol "NIL" "COMMON-LISP"))
        (t-value (allocate-symbol "T" "COMMON-LISP"))
        (keyword-keyword (allocate-symbol "KEYWORD" "KEYWORD"))
        (cl-keyword (allocate-symbol "COMMON-LISP" "KEYWORD")))
    (format t "NIL at word ~X~%" nil-value)
    (format t "  T at word ~X~%" t-value)
    (populate-symbol nil-value         "NIL"         :common-lisp (vsym nil)          t)
    (populate-symbol t-value           "T"           :common-lisp (vsym t)            t)
    (populate-symbol keyword-keyword   "KEYWORD"     :keyword     (vsym :keyword)     t)
    (populate-symbol cl-keyword        "COMMON-LISP" :keyword     (vsym :common-lisp) t))
  ;; Create the unbound values.
  (setf *unbound-value-address* (allocate 2 :wired))
  (setf (word *unbound-value-address*) (array-header sys.int::+object-tag-unbound-value+ 0))
  (format t "UBV at word ~X~%" *unbound-value-address*)
  ;; Create trampoline functions.
  (setf *undefined-function-address* (compile-lap-function (ecase sys.c::*target-architecture*
                                                             (:x86-64 cold-generator.x86-64:*undefined-function-thunk*)
                                                             (:arm64 cold-generator.arm64:*undefined-function-thunk*))
                                                           :area :wired
                                                           :name 'sys.int::%%undefined-function-trampoline)
        *closure-trampoline-address* (compile-lap-function (ecase sys.c::*target-architecture*
                                                             (:x86-64 cold-generator.x86-64:*closure-trampoline*)
                                                             (:arm64 cold-generator.arm64:*closure-trampoline*))
                                                           :area :wired
                                                           :name 'sys.int::%%closure-trampoline)
        *f-i-trampoline-address* (compile-lap-function (ecase sys.c::*target-architecture*
                                                         (:x86-64 cold-generator.x86-64:*funcallable-instance-trampoline*)
                                                         (:arm64 cold-generator.arm64:*funcallable-instance-trampoline*))
                                                       :area :wired
                                                       :name 'sys.int::%%funcallable-instance-trampoline))
  (format t "UDF at word ~X~%" *undefined-function-address*)
  ;; And finally the initial structure definitions.
  (setf *structure-definition-definition* (allocate 7 :wired)
        *structure-slot-definition-definition* (allocate 7 :wired))
  (populate-structure-definition *structure-definition-definition*
                                 'sys.int::structure-definition
                                 ;; name, accessor, initform, type, read-only
                                 '((sys.int::name   sys.int::structure-name             nil t t)
                                   (sys.int::slots  sys.int::structure-slots            nil t t)
                                   (sys.int::parent sys.int::structure-parent           nil t t)
                                   (sys.int::area   sys.int::structure-area             nil t t)
                                   (sys.int::class  sys.int::structure-definition-class nil t nil))
                                 nil
                                 :wired)
  (populate-structure-definition *structure-slot-definition-definition*
                                 'sys.int::structure-slot-definition
                                 '((sys.int::name      sys.int::structure-slot-name      nil t t)
                                   (sys.int::accessor  sys.int::structure-slot-accessor  nil t t)
                                   (sys.int::initform  sys.int::structure-slot-initform  nil t t)
                                   (sys.int::type      sys.int::structure-slot-type      nil t t)
                                   (sys.int::read-only sys.int::structure-slot-read-only nil t t))
                                 nil
                                 :wired))

(defun vmake-struct-slot-def (name accessor initial-value type read-only)
  (let ((addr (allocate 7 :wired)))
    (setf (word (+ addr 0)) (array-header sys.int::+object-tag-structure-object+ 6)
          (word (+ addr 1)) (make-value *structure-slot-definition-definition* sys.int::+tag-object+)
          (word (+ addr 2)) name
          (word (+ addr 3)) accessor
          (word (+ addr 4)) initial-value
          (word (+ addr 5)) type
          (word (+ addr 6)) read-only)
    (make-value addr sys.int::+tag-object+)))

(defun vmake-structure (type &rest initargs &key &allow-other-keys)
  ;; Look up the associated structure definition.
  (let* ((def (sys.int::get-structure-type type))
         (n-slots (length (sys.c::structure-type-slots def)))
         (address (allocate (+ 2 n-slots) ; header + type + slots
                            (sys.c::structure-type-area def))))
    ;; header
    (setf (word (+ address 0)) (array-header sys.int::+object-tag-structure-object+ (1+ n-slots))) ; includes type
    ;; type
    (setf (word (+ address 1)) (make-value (first (or (gethash type *struct-table*)
                                                      (error "Missing structure ~S?" type)))
                                           sys.int::+tag-object+))
    ;; Initialize slots to NIL.
    (loop
       for i from 2
       repeat n-slots
       do
         (setf (word (+ address i)) (vsym nil)))
    ;; Populate from initargs.
    (loop
       for (name value) on initargs by #'cddr
       ;; Initargs are keywords, slot names are other symbols.
       ;; Hack around this by comparing symbol names.
       for loc = (or (position name (sys.c::structure-type-slots def)
                               :test #'string=
                               :key #'sys.c::structure-slot-name)
                     (error "Unknown slot ~S in structure ~S" name type))
       do
         (format t "Position of slot ~S in ~S is ~S @ ~X~%" name type loc (+ address 2 loc))
         (setf (word (+ address 2 loc)) value))
    (make-value address sys.int::+tag-object+)))

(defun structure-slot (object type slot)
  (let* ((def (sys.int::get-structure-type type))
         (index (or (position slot (sys.c::structure-type-slots def)
                              :key #'sys.c::structure-slot-name)
                    (error "Unknown slot ~S in structure ~S" slot type))))
    (word (+ (pointer-part object) 2 index))))

(defun (setf structure-slot) (value object type slot)
  (let* ((def (sys.int::get-structure-type type))
         (index (or (position slot (sys.c::structure-type-slots def)
                              :key #'sys.c::structure-slot-name)
                    (error "Unknown slot ~S in structure ~S" slot type))))
    (format t "Position of slot ~S in ~S is ~S (setf) @ ~X~%" slot type index (+ (truncate object 8) 2 index))
    (setf (word (+ (pointer-part object) 2 index)) value)))

(defun add-page-to-block-map (bml4 block virtual-address flags)
  (let ((bml4e (ldb (byte 9 39) virtual-address))
        (bml3e (ldb (byte 9 30) virtual-address))
        (bml2e (ldb (byte 9 21) virtual-address))
        (bml1e (ldb (byte 9 12) virtual-address)))
    (unless (aref bml4 bml4e)
      (setf (aref bml4 bml4e) (make-array 512 :initial-element nil)))
    (let ((bml3 (aref bml4 bml4e)))
      (unless (aref bml3 bml3e)
        (setf (aref bml3 bml3e) (make-array 512 :initial-element nil)))
      (let ((bml2 (aref bml3 bml3e)))
        (unless (aref bml2 bml2e)
          (setf (aref bml2 bml2e) (make-array 512 :initial-element nil)))
        (let ((bml1 (aref bml2 bml2e)))
          (assert (not (aref bml1 bml1e)))
          (setf (aref bml1 bml1e) (logior (ash block sys.int::+block-map-id-shift+)
                                          flags)))))))

(defun add-region-to-block-map (bml4 store-base virtual-base size flags)
  (dotimes (i size)
    (add-page-to-block-map bml4 (+ store-base i) (+ virtual-base (* i #x1000)) flags)))

(defun write-block-map (s image-offset block-offset level)
  (let ((data (make-array #x1000 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i 512)
      (let ((e (aref level i)))
        (etypecase e
          (null)
          (vector
           ;; Next level
           (let* ((next-block *store-bump*))
             (incf *store-bump* #x1000)
             (write-block-map s image-offset next-block e)
             (setf (nibbles:ub64ref/le data (* i 8)) (ash (/ next-block #x1000) sys.int::+block-map-id-shift+))))
          ((unsigned-byte 64)
           ;; Value.
           (setf (nibbles:ub64ref/le data (* i 8)) e)))))
    (file-position s (+ image-offset block-offset))
    (write-sequence data s)))

(defun load-image-header (path)
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      data)))

(defun write-card-table (stream image-offset bml4 start size)
  (assert (zerop (rem start sys.int::+allocation-minimum-alignment+)))
  (assert (zerop (rem size sys.int::+allocation-minimum-alignment+)))
  (let ((table (make-array (* (/ size sys.int::+card-size+) sys.int::+card-table-entry-size+)
                           :element-type '(unsigned-byte 8)
                           :initial-element 0))
        (store-base *store-bump*))
    (loop
       for i below size by sys.int::+card-size+
       for offset = (car (or (gethash (+ start i) *card-offsets*)
                             (error "Missing card offset for address ~X" (+ start i))))
       do
         (assert (not (plusp offset)))
         (assert (not (logtest offset 15)))
         (setf (nibbles:ub32ref/le table (* (/ i sys.int::+card-size+) sys.int::+card-table-entry-size+))
               (cross-cl:dpb (min (1- (expt 2 (cross-cl:byte-size sys.int::+card-table-entry-offset+)))
                                  (/ (- offset) 16))
                             sys.int::+card-table-entry-offset+
                             0)))
    (file-position stream (+ image-offset store-base))
    (write-sequence table stream)
    (incf *store-bump* (length table))
    (add-region-to-block-map bml4
                             (/ store-base #x1000)
                             (+ sys.int::+card-table-base+
                                (* (/ start sys.int::+card-size+) sys.int::+card-table-entry-size+))
                             (/ (* (/ size sys.int::+card-size+) sys.int::+card-table-entry-size+) #x1000)
                             (logior sys.int::+block-map-present+
                                     sys.int::+block-map-writable+
                                     sys.int::+block-map-wired+))))

(defun write-image (s entry-fref initial-thread image-size header-path uuid)
  (format t "Writing image file to ~A.~%" (namestring s))
  (let* ((image-header-data (when header-path
                              (load-image-header header-path)))
         (image-offset (if image-header-data
                           (length image-header-data)
                           0))
         (bml4-block *store-bump*)
         (bml4 (make-array 512 :initial-element nil))
         (free-block-list (+ *store-bump* #x1000)))
    (when (and header-path
               (not image-size))
      ;; When a header is used, a full disk image is being created, not
      ;; a stand-alone image.
      ;; Set a reasonably sensible default image size if none was provided.
      (setf image-size (* 512 1024 1024)))
    (when image-size
      (decf image-size image-offset)
      (format t "Generating ~:D byte image.~%" image-size))
    (when image-header-data
      (format t "Using ~S as the image header.~%" header-path))
    (format t "BML4 at offset ~X~%" bml4-block)
    (format t "FBL  at offset ~X~%" free-block-list)
    (when image-header-data
      (write-sequence image-header-data s)
      ;; Update the size of the third partition entry, the Mezzano partiton.
      (file-position s #x1EA)
      (nibbles:write-ub32/le (truncate image-size 512) s))
    (incf *store-bump* #x2000)
    (when image-size
      (file-position s (1- (+ image-offset image-size)))
      (write-byte 0 s))
    (file-position s image-offset)
    ;; Image header.
    (let ((header (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; Magic.
      (replace header #(#x00 #x4D #x65 #x7A #x7A #x61 #x6E #x69 #x6E #x65 #x49 #x6D #x61 #x67 #x65 #x00)
               :start1 0)
      ;; UUID.
      (replace header uuid :start1 16)
      ;; Major version.
      (setf (ub16ref/le header 32) 0)
      ;; Minor version.
      (setf (ub16ref/le header 34) 24)
      ;; Entry fref.
      (setf (ub64ref/le header 40) entry-fref)
      ;; Initial thread.
      (setf (ub64ref/le header 48) initial-thread)
      ;; NIL.
      (setf (ub64ref/le header 56) (make-value (symbol-address "NIL" "COMMON-LISP")
                                               sys.int::+tag-object+))
      ;; Architecture.
      (setf (ub32ref/le header 64) (ecase sys.c::*target-architecture*
                                     (:x86-64 sys.int::+llf-arch-x86-64+)
                                     (:arm64 sys.int::+llf-arch-arm64+)))
      ;; 65-96 free.
      ;; Top-level block map.
      (setf (ub64ref/le header 96) (/ bml4-block #x1000))
      ;; Free block list.
      (setf (ub64ref/le header 104) (/ free-block-list #x1000))
      ;; Write it out.
      (write-sequence header s))
    ;; Write areas.
    ;; The *foo-AREA-DATA* vectors are all padded out to 2MB, so writing
    ;; them out in their entirety is fine.
    (file-position s (+ image-offset *wired-area-store*))
    (format t "Wired area at ~X, ~:D bytes.~%"
            *wired-area-store* (length *wired-area-data*))
    (write-sequence *wired-area-data* s)
    (format t "Pinned area at ~X, ~:D bytes.~%"
            *pinned-area-store* (length *pinned-area-data*))
    (file-position s (+ image-offset *pinned-area-store*))
    (write-sequence *pinned-area-data* s)
    (format t "General area at ~X, ~:D bytes.~%"
            *general-area-store* (length *general-area-data*))
    (file-position s (+ image-offset *general-area-store*))
    (write-sequence *general-area-data* s)
    (format t "Cons area at ~X, ~:D bytes.~%"
            *cons-area-store* (length *cons-area-data*))
    (file-position s (+ image-offset *cons-area-store*))
    (write-sequence *cons-area-data* s)
    ;; Write initial card table. Includes adding it to the block map.
    (write-card-table s image-offset bml4 +wired-area-base+ (- *wired-area-bump* +wired-area-base+))
    (write-card-table s image-offset bml4 +pinned-area-base+ (- *pinned-area-bump* +pinned-area-base+))
    (write-card-table s image-offset bml4 (logior (ash sys.int::+address-tag-general+ sys.int::+address-tag-shift+)
                                                  (cross-cl:dpb sys.int::+address-generation-2-a+ sys.int::+address-generation+ 0))
                      *general-area-bump*)
    (write-card-table s image-offset bml4 (logior (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                                                  (cross-cl:dpb sys.int::+address-generation-2-a+ sys.int::+address-generation+ 0))
                      *cons-area-bump*)
    ;; Generate the block map.
    (add-region-to-block-map bml4
                             (/ *wired-area-store* #x1000)
                             +wired-area-base+
                             (/ (- (align-up *wired-area-bump* sys.int::+allocation-minimum-alignment+) +wired-area-base+) #x1000)
                             (logior sys.int::+block-map-present+
                                     sys.int::+block-map-writable+
                                     sys.int::+block-map-wired+))
    (add-region-to-block-map bml4
                             (/ *pinned-area-store* #x1000)
                             +pinned-area-base+
                             (/ (- (align-up *pinned-area-bump* sys.int::+allocation-minimum-alignment+) +pinned-area-base+) #x1000)
                             (logior sys.int::+block-map-present+
                                     sys.int::+block-map-writable+))
    (add-region-to-block-map bml4
                             (/ *general-area-store* #x1000)
                             (logior (ash sys.int::+address-tag-general+ sys.int::+address-tag-shift+)
                                     (cross-cl:dpb sys.int::+address-generation-2-a+ sys.int::+address-generation+ 0))
                             (/ (align-up *general-area-bump* sys.int::+allocation-minimum-alignment+) #x1000)
                             (logior sys.int::+block-map-present+
                                     sys.int::+block-map-writable+
                                     sys.int::+block-map-track-dirty+))
    (add-region-to-block-map bml4
                             (/ *cons-area-store* #x1000)
                             (logior (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                                     (cross-cl:dpb sys.int::+address-generation-2-a+ sys.int::+address-generation+ 0))
                             (/ (align-up *cons-area-bump* sys.int::+allocation-minimum-alignment+) #x1000)
                             (logior sys.int::+block-map-present+
                                     sys.int::+block-map-writable+
                                     sys.int::+block-map-track-dirty+))
    (dolist (stack *stack-list*)
      (add-region-to-block-map bml4
                               (/ (stack-store stack) #x1000)
                               (stack-base stack)
                               (/ (stack-size stack) #x1000)
                               (logior sys.int::+block-map-present+
                                       sys.int::+block-map-writable+
                                       sys.int::+block-map-zero-fill+
                                       sys.int::+block-map-wired+)))
    ;; Now write it out.
    (write-block-map s image-offset bml4-block bml4)
    ;; Create the freelist.
    ;; One entry, allocating our storage area.
    (let ((freelist-data (make-array #x1000 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (nibbles:ub64ref/le freelist-data 0) 0
            (nibbles:ub64ref/le freelist-data 8) (ash (/ *store-bump* #x1000) 1))
      (file-position s (+ image-offset free-block-list))
      (write-sequence freelist-data s)))
  (values))

(defun array-header (tag length)
  (logior (ash tag sys.int::+object-type-shift+)
          (ash length sys.int::+object-data-shift+)))

(defun pack-halfwords (low high)
  (check-type low (unsigned-byte 32))
  (check-type high (unsigned-byte 32))
  (dpb high (byte 32 32) low))

(defun create-thread (name &key stack-size (initial-state :runnable))
  (check-type initial-state (member :active :runnable :sleeping :dead))
  (let* ((address (allocate 512 :wired))
         (stack (create-stack (* stack-size 8)))
         (stack-object (let ((*default-cons-allocation-area* :wired))
                         (vcons (make-fixnum (stack-base stack))
                                (make-fixnum (stack-size stack))))))
    (format t "~X ~X  ~X~%" (stack-base stack) (stack-size stack)
            (+ (stack-base stack) (stack-size stack)))
    ;; Array tag.
    (setf (word (+ address 0)) (array-header sys.int::+object-tag-thread+ 0))
    ;; Name.
    (setf (word (+ address 1)) (make-value (store-string name :wired)
                                           sys.int::+tag-object+))
    ;; State.
    (setf (word (+ address 2)) (vsym initial-state))
    ;; Lock.
    (setf (word (+ address 3)) (vsym :unlocked))
    ;; Stack.
    (setf (word (+ address 4)) stack-object)
    ;; Stack pointer.
    (setf (word (+ address 5)) (+ (stack-base stack)
                                  (stack-size stack)))
    ;; +6, unused
    ;; Special stack pointer.
    (setf (word (+ address 7)) (vsym 'nil))
    ;; +8 self.
    (setf (word (+ address 9)) (make-value address sys.int::+tag-object+))
    ;; Next.
    (setf (word (+ address 10)) (vsym 'nil))
    ;; Prev.
    (setf (word (+ address 11)) (vsym 'nil))
    ;; Pending footholds.
    (setf (word (+ address 12)) (vsym 'nil))
    ;; Inhibit footholds.
    (setf (word (+ address 13)) (make-fixnum 1))
    ;; mutex stack.
    (setf (word (+ address 14)) (vsym 'nil))
    ;; Priority
    (setf (word (+ address 17)) (vsym ':normal))
    (make-value address sys.int::+tag-object+)))

(defun create-initial-thread ()
  (setf (cold-symbol-value 'sys.int::*initial-thread*)
        (create-thread "Initial thread"
                       :stack-size (* 16 1024)
                       :initial-state :active)))

(defun canonical-package-name (package-name)
  (cond ((or (string= package-name "CL")
             (string= package-name "COMMON-LISP"))
         "COMMON-LISP")
        ((string= package-name "KEYWORD")
         "KEYWORD")
        (t
         (package-name (or (find-package package-name)
                           (error "Unknown package ~S" package-name))))))

(defun canonical-symbol-package (symbol)
  (when (keywordp symbol)
    (return-from canonical-symbol-package "KEYWORD"))
  (let ((package (symbol-package symbol)))
    (cond ((eql package (find-package "CL"))
           "COMMON-LISP")
          ((eql package (find-package "SYS.INT"))
           "SYSTEM.INTERNALS")
          ((eql package (find-package "SYS.FORMAT"))
           "SYS.FORMAT")
          (t (error "Not touching package ~S (for symbol ~A)." package symbol)))))

(defun vsym (symbol)
  (make-value (symbol-address (symbol-name symbol) (canonical-symbol-package symbol))
              sys.int::+tag-object+))

(defun (setf cold-symbol-value) (value symbol)
  (let ((global-cell (word (+ (symbol-address (symbol-name symbol)
                                              (canonical-symbol-package symbol))
                              1
                              sys.int::+symbol-value+))))
    (setf (word (+ (pointer-part global-cell) 3)) value)))

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

(defun write-map-file (pathname map)
  (with-open-file (s pathname
                     :direction :output
                     :if-exists :supersede)
    (format t "Writing map file to ~A.~%" (namestring s))
    (let ((*print-right-margin* 10000))
      (iter (for (addr name) in (sort (copy-list map) '< :key 'first))
            (format s "~X ~A~%" (* (+ addr 2) 8)
                    (cl-ppcre:regex-replace (string #\Newline)
                                            (format nil "~A" name)
                                            "#\\Newline"))))))

(defun build-directory ()
  (merge-pathnames (make-pathname :directory `(:relative ,(format nil "~(build-~A~)" sys.c::*target-architecture*)))))

;; Ugh.
(defun load-compiler-builtins ()
  (let ((llf-path (merge-pathnames "%%compiler-builtins.llf"
                                   (build-directory))))
    (ensure-directories-exist llf-path)
    (sys.c::save-compiler-builtins llf-path
                                   sys.c::*target-architecture*)
    (load-source-file llf-path t t)))

(defun maybe-compile-file (path)
  (let ((llf-path (merge-pathnames (make-pathname :type "llf" :defaults path)
                                   (build-directory))))
    (ensure-directories-exist llf-path)
    (with-open-file (s llf-path
                       :element-type '(unsigned-byte 8)
                       :if-does-not-exist nil)
      (when s
        (handler-case (validate-llf-header s)
          (invalid-llf (c)
            (format t "Rebuilding ~A: ~A~%" llf-path c)
            (delete-file s)))))
    (when (or (not (probe-file llf-path))
              (<= (file-write-date llf-path) (file-write-date path)))
      (format t "~A is out of date will be recompiled.~%" llf-path)
      (sys.c::cross-compile-file path :output-file llf-path))
    llf-path))

(defun save-ub1-vector (vec &optional area)
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

(defun save-ub2-vector (vec &optional area)
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

(defun save-ub4-vector (vec &optional area)
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

(defun save-ub8-vector (vec &optional area)
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

(defun save-ub16-vector (vec &optional area)
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

(defun save-ub32-vector (vec &optional area)
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

(defun save-ub64-vector (vec &optional area)
  (let ((address (allocate (1+ (length vec)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-unsigned-byte-64+ (length vec)))
    (iter (for x in-sequence vec)
          (for i from 1)
          (setf (word (+ address i)) x))
    (make-value address sys.int::+tag-object+)))

(defun save-simple-vector (vec &optional area)
  (let ((address (allocate (1+ (length vec)) area)))
    (setf (word address) (array-header sys.int::+object-tag-array-t+ (length vec)))
    (iter (for x in-sequence vec)
          (for i from 1)
          (setf (word (+ address i)) (save-object x area)))
    (make-value address sys.int::+tag-object+)))

(defun save-object (object &optional area)
  (etypecase object
    ((signed-byte 63) (make-fixnum object))
    (string
     (let ((value (gethash object *string-dedup-table*)))
       (unless value
         (setf value (make-value (store-string object area)
                                 sys.int::+tag-object+))
         (setf (gethash object *string-dedup-table*) value))
       value))
    (cons (vcons (save-object (car object) area)
                 (save-object (cdr object) area)))
    (symbol (make-value (symbol-address (symbol-name object)
                                        (canonical-symbol-package object))
                        sys.int::+tag-object+))
    ((vector (unsigned-byte 1))
     (save-ub1-vector object area))
    ((vector (unsigned-byte 2))
     (save-ub2-vector object area))
    ((vector (unsigned-byte 4))
     (save-ub4-vector object area))
    ((vector (unsigned-byte 8))
     (save-ub8-vector object area))
    ((vector (unsigned-byte 16))
     (save-ub16-vector object area))
    ((vector (unsigned-byte 32))
     (save-ub32-vector object area))
    ((vector (unsigned-byte 64))
     (save-ub64-vector object area))
    ((vector t)
     (save-simple-vector object area))))

(defun save-unifont-data (path)
  (multiple-value-bind (tree data)
      (with-open-file (s path)
        (build-unicode:generate-unifont-table s))
    (setf (cold-symbol-value 'sys.int::*unifont-bmp*) (save-object tree :pinned))
    (setf (cold-symbol-value 'sys.int::*unifont-bmp-data*) (save-object data :pinned))))

(defun save-debug-8x8-font (path)
  (let* ((font-data (with-open-file (s path) (read s)))
         (font-array (make-array 128 :initial-element nil)))
    (assert (eql (array-dimension font-data 0) 128))
    (dotimes (i 128)
      (let ((array (make-array (* 8 8) :element-type '(unsigned-byte 32))))
        (setf (aref font-array i) array)
        (dotimes (y 8)
          (let ((line (aref font-data i y)))
            (dotimes (x 8)
              (setf (aref array (+ (* y 8) x)) (if (logbitp x line)
                                                   #xFF000000
                                                   #xFFFFFFFF)))))))
    (setf (cold-symbol-value 'sys.int::*debug-8x8-font*) (save-object font-array :wired))))

(defun finalize-areas ()
  "Assign store addresses to each area, and build pinned/wired area freelists."
  (let ((wired-free-bins (allocate (1+ 64) :wired))
        (pinned-free-bins (allocate (1+ 64) :wired))
        ;; Ensure a minium amount of free space in :wired.
        ;; And :pinned as well, but that matters less.
        (wired-free-area (allocate (* 8 1024 1024) :wired))
        (pinned-free-area (allocate (* 1 1024 1024) :pinned)))
    ;; Allocate enough to align the area sizes up to the allocation alignment.
    (flet ((align (area area-bump)
             (allocate (/ (- sys.int::+allocation-minimum-alignment+
                             (rem area-bump
                                  sys.int::+allocation-minimum-alignment+))
                          8)
                       area)))
      (align :wired *wired-area-bump*)
      (align :pinned *pinned-area-bump*)
      (align :general *general-area-bump*)
      (align :cons *cons-area-bump*))
    (setf (word wired-free-bins) (array-header sys.int::+object-tag-array-t+ 64)
          (word pinned-free-bins) (array-header sys.int::+object-tag-array-t+ 64))
    (dotimes (i 64)
      (setf (word (+ wired-free-bins 1 i)) (vsym nil))
      (setf (word (+ pinned-free-bins 1 i)) (vsym nil)))
    (setf (cold-symbol-value 'sys.int::*wired-area-free-bins*) (make-value wired-free-bins sys.int::+tag-object+)
          (cold-symbol-value 'sys.int::*pinned-area-free-bins*) (make-value pinned-free-bins sys.int::+tag-object+))
    (setf *wired-area-bump* (align-up *wired-area-bump* sys.int::+allocation-minimum-alignment+))
    (let ((wired-size (truncate (- *wired-area-bump* (ldb (byte 44 0) (* wired-free-area 8))) 8)))
      (setf (word wired-free-area) (logior (ash sys.int::+object-tag-freelist-entry+ sys.int::+object-type-shift+)
                                           (ash wired-size sys.int::+object-data-shift+))
            (word (1+ wired-free-area)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))
      (setf (word (+ wired-free-bins 1 (integer-length wired-size))) (make-fixnum (* wired-free-area 8))))
    (setf *pinned-area-bump* (align-up *pinned-area-bump* sys.int::+allocation-minimum-alignment+))
    (let ((pinned-size (truncate (- *pinned-area-bump* (ldb (byte 44 0) (* pinned-free-area 8))) 8)))
      (setf (word pinned-free-area) (logior (ash sys.int::+object-tag-freelist-entry+ sys.int::+object-type-shift+)
                                            (ash pinned-size sys.int::+object-data-shift+))
            (word (1+ pinned-free-area)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))
      (setf (word (+ pinned-free-bins 1 (integer-length pinned-size))) (make-fixnum (* pinned-free-area 8))))
    (setf *wired-area-store* *store-bump*)
    (incf *store-bump* (- *wired-area-bump* +wired-area-base+))
    (setf *pinned-area-store* *store-bump*)
    (incf *store-bump* (- *pinned-area-bump* +pinned-area-base+))
    (setf *general-area-store* *store-bump*)
    (incf *store-bump* (* (align-up *general-area-bump* sys.int::+allocation-minimum-alignment+) 2))
    (setf *cons-area-store* *store-bump*)
    (incf *store-bump* (* (align-up *cons-area-bump* sys.int::+allocation-minimum-alignment+) 2))
    (dolist (stack *stack-list*)
      (setf (stack-store stack) *store-bump*)
      (incf *store-bump* (stack-size stack)))
    (assert (zerop (rem *wired-area-bump* sys.int::+allocation-minimum-alignment+)))
    (assert (zerop (rem *pinned-area-bump* sys.int::+allocation-minimum-alignment+)))
    (assert (zerop (rem *general-area-bump* sys.int::+allocation-minimum-alignment+)))
    (assert (zerop (rem *cons-area-bump* sys.int::+allocation-minimum-alignment+)))))

(defun generate-uuid ()
  (let ((uuid (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref uuid i) (case i
                            (9 (logior #x40 (random 16)))
                            (7 (logior (random 64) #x80))
                            (t (random 256)))))
    uuid))

(defun format-uuid (stream object &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  ;; Printed UUIDs are super weird.
  (format stream "~2,'0X~2,'0X~2,'0X~2,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X-~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X"
          ;; Byteswapped.
          (aref object 3) (aref object 2) (aref object 1) (aref object 0)
          (aref object 5) (aref object 4)
          (aref object 7) (aref object 6)
          ;; Not byteswapped.
          (aref object 8) (aref object 9)
          (aref object 10) (aref object 11) (aref object 12) (aref object 13) (aref object 14) (aref object 15)))

(defun parse-uuid (string)
  (assert (eql (length string) 36))
  (assert (eql (char string 8) #\-))
  (assert (eql (char string 13) #\-))
  (assert (eql (char string 18) #\-))
  (assert (eql (char string 23) #\-))
  (flet ((b (start)
           (parse-integer string :radix 16 :start start :end (+ start 2))))
    (vector
     ;; First group. Byteswapped.
     (b 6) (b 4) (b 2) (b 0)
     ;; Second group. Byteswapped.
     (b 11) (b 9)
     ;; Third group. Byteswapped.
     (b 16) (b 14)
     ;; Fourth group. Not byteswapped.
     (b 19) (b 21)
     ;; Fifth group. Not byteswapped.
     (b 24) (b 26) (b 28) (b 30) (b 32) (b 34))))

(defun git-revision ()
  "Return the current git hash as a string or NIL if it can't be determined."
  (ignore-errors
    (values (uiop/run-program:run-program '("git" "rev-parse" "HEAD")
                                          :output '(:string :stripped t)))))

(defun make-image (image-name &key extra-source-files header-path image-size map-file-name (architecture :x86-64) uuid)
  (cond ((stringp uuid)
         (setf uuid (parse-uuid uuid)))
        ((not uuid)
         (setf uuid (generate-uuid))))
  (let* ((sys.c::*target-architecture* architecture)
         (sys.int::*features* (list* sys.c::*target-architecture* sys.int::*features*))
         (*wired-area-bump* +wired-area-base+)
         (*wired-area-data* (make-array #x1000 :element-type '(unsigned-byte 8) :adjustable t))
         (*wired-area-store* nil)
         (*pinned-area-bump* +pinned-area-base+)
         (*pinned-area-data* (make-array #x1000 :element-type '(unsigned-byte 8) :adjustable t))
         (*pinned-area-store* nil)
         (*general-area-bump* 0)
         (*general-area-data* (make-array #x1000 :element-type '(unsigned-byte 8) :adjustable t))
         (*general-area-store* nil)
         (*cons-area-bump* 0)
         (*cons-area-data* (make-array #x1000 :element-type '(unsigned-byte 8) :adjustable t))
         (*cons-area-store* nil)
         (*stack-area-bump* 0)
         (*stack-area-bytes* 0)
         (*stack-list* '())
         (*store-bump* #x1000) ; header is 4k
         (*word-locks* (make-hash-table))
         (*pending-fixups* '())
         (*symbol-table* (make-hash-table :test 'equal))
         (*reverse-symbol-table* (make-hash-table))
         (*fref-table* (make-hash-table :test 'equal))
         (*struct-table* (make-hash-table))
         (*undefined-function-address* nil)
         (*closure-trampoline-address* nil)
         (*f-i-trampoline-address* nil)
         (*function-map* '())
         (*string-dedup-table* (make-hash-table :test 'equal))
         (initial-thread)
         (cl-symbol-names (with-open-file (s *cl-symbol-list-file*) (read s)))
         (pf-exception-stack (create-stack (* 128 1024)))
         (irq-stack (create-stack (* 128 1024)))
         (wired-stack (create-stack (* 128 1024)))
         (*image-to-cross-slot-definitions* (make-hash-table))
         (*card-offsets* (make-hash-table)))
    ;; Generate the support objects. NIL/T/etc, and the initial thread.
    (create-support-objects)
    (ecase sys.c::*target-architecture*
      (:x86-64
       (cold-generator.x86-64:create-low-level-interrupt-support))
      (:arm64))
    (setf (cold-symbol-value 'sys.int::*exception-stack-base*) (make-fixnum (stack-base pf-exception-stack))
          (cold-symbol-value 'sys.int::*exception-stack-size*) (make-fixnum (stack-size pf-exception-stack)))
    (setf (cold-symbol-value 'sys.int::*irq-stack-base*) (make-fixnum (stack-base irq-stack))
          (cold-symbol-value 'sys.int::*irq-stack-size*) (make-fixnum (stack-size irq-stack)))
    (setf (cold-symbol-value 'sys.int::*bsp-wired-stack-base*) (make-fixnum (stack-base wired-stack))
          (cold-symbol-value 'sys.int::*bsp-wired-stack-size*) (make-fixnum (stack-size wired-stack)))
    (setf initial-thread (create-initial-thread))
    ;; Load all cold source files, emitting the top-level forms into an array
    ;; FIXME: Top-level forms generally show up as functions in .LLF files,
    ;;        this should be a vector of callable functions, not evalable forms.
    (let ((*load-time-evals* '()))
      (load-compiler-builtins)
      (load-source-files *supervisor-source-files* t t)
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
    (format t "Saving 8x8 debug font.~%")
    (save-debug-8x8-font *8x8-debug-font*)
    (format t "Saving Unifont...~%")
    (save-unifont-data *unifont*)
    (format t "Saving Unicode data...~%")
    (multiple-value-bind (planes name-store encoding-table name-trie)
        (build-unicode:generate-unicode-data-tables (build-unicode:read-unicode-data *unicode-data*))
      (setf (cold-symbol-value 'sys.int::*unicode-info*) (save-object planes :pinned)
            (cold-symbol-value 'sys.int::*unicode-name-store*) (save-ub8-vector name-store :pinned)
            (cold-symbol-value 'sys.int::*unicode-encoding-table*) (save-object encoding-table :pinned)
            (cold-symbol-value 'sys.int::*unicode-name-trie*) (save-object name-trie :pinned)))
    ;; Bake the compiled warm source files in.
    (let ((warm-files (make-array 10 :adjustable t :fill-pointer 0)))
      (dolist (file (mapcar (lambda (x)
                              (if (consp x)
                                  (first x)
                                  x))
                            (remove-if-not (lambda (x)
                                             (or (not (consp x))
                                                 (member sys.c::*target-architecture* (rest x))))
                                           *warm-source-files*)))
        ;; HACK! Force use of the new compiler building the SIMD/float functions.
        (let ((sys.c::*use-new-compiler* (if (member file '("runtime/simd.lisp"
                                                            "runtime/float-x86-64.lisp"))
                                             t
                                             sys.c::*use-new-compiler*))
              (llf-path (maybe-compile-file file)))
          (format t "Loading ~A.~%" llf-path)
          (with-open-file (warm llf-path :element-type '(unsigned-byte 8))
            (let ((vec (make-array (file-length warm) :element-type '(unsigned-byte 8))))
              (read-sequence vec warm)
              (vector-push-extend (cons (pathname-name llf-path) vec) warm-files)))))
      (setf (cold-symbol-value 'sys.int::*warm-llf-files*) (save-object warm-files :pinned)))
    (format t "Saving PCI IDs...~%")
    (let* ((pci-ids (build-pci-ids:build-pci-ids *pci-ids*))
           (object (save-object pci-ids :wired)))
      (setf (cold-symbol-value 'sys.int::*pci-ids*) object))
    ;; Poke a few symbols to ensure they exist. This avoids memory allocation after finalize-areas runs.
    (format t "Final tweaks...~%")
    (mapc (lambda (sym) (symbol-address (string sym) (package-name (symbol-package sym))))
          '(sys.int::*initial-obarray* sys.int::*initial-keyword-obarray*
            sys.int::*initial-fref-obarray* sys.int::*initial-structure-obarray*
            sys.int::*unifont-bmp* sys.int::*unifont-bmp-data*
            sys.int::*unicode-info* sys.int::*unicode-name-store*
            sys.int::*unicode-encoding-table* sys.int::*unicode-name-trie*
            sys.int::*bsp-idle-thread*
            sys.int::*snapshot-thread*
            sys.int::*pager-thread*
            sys.int::*disk-io-thread*
            sys.int::*initial-areas*
            sys.int::*wired-area-bump*
            sys.int::*pinned-area-bump*
            sys.int::*general-area-bump*
            sys.int::*cons-area-bump*
            sys.int::*stack-area-bump*
            :wired :pinned :general :cons :nursery :stack
            sys.int::*structure-type-type*
            sys.int::*structure-slot-type*
            sys.int::*wired-area-free-bins*
            sys.int::*pinned-area-free-bins*
            sys.int::*bytes-allocated-to-stacks*
            ))
    (loop
       for (what address byte-offset type debug-info) in *pending-fixups*
       when (and (consp what)
                 (symbolp (second what)))
       do (symbol-address (string (second what))
                          (package-name (symbol-package (second what)))))
    (setf (cold-symbol-value 'sys.int::*supervisor-log-buffer*)
          (save-object (make-array (* 1024 1024)
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)
                       :wired))
    (setf (cold-symbol-value 'sys.int::*bsp-idle-thread*)
          (create-thread "BSP idle thread"
                         :stack-size (* 16 1024)))
    (setf (cold-symbol-value 'sys.int::*snapshot-thread*)
          (create-thread "Snapshot thread"
                         :stack-size (* 128 1024)
                         :initial-state :sleeping))
    (setf (cold-symbol-value 'sys.int::*pager-thread*)
          (create-thread "Pager thread"
                         :stack-size (* 128 1024)
                         :initial-state :sleeping))
    (setf (cold-symbol-value 'sys.int::*disk-io-thread*)
          (create-thread "Disk IO thread"
                         :stack-size (* 128 1024)
                         :initial-state :sleeping))
    (setf (cold-symbol-value 'sys.int::*bsp-info-vector*)
          (save-object (make-array (1- (/ (* 2 #x1000) 8))
                                   :element-type '(unsigned-byte 64)
                                   :initial-element 0)
                       :wired))
    (let ((git-rev (git-revision)))
      (setf (cold-symbol-value 'sys.int::*git-revision*)
            (if git-rev
                (make-value (store-string git-rev) sys.int::+tag-object+)
                (vsym nil))))
    ;; Make sure there's a keyword for each package.
    (iter (for ((nil . package-name) nil) in-hashtable *symbol-table*)
          (symbol-address package-name "KEYWORD"))
    ;; Poke all the CL & SYSTEM symbols
    (dolist (name cl-symbol-names)
      (symbol-address name "COMMON-LISP"))
    (generate-obarray *symbol-table* 'sys.int::*initial-obarray*)
    (generate-fref-obarray *fref-table* 'sys.int::*initial-fref-obarray*)
    (generate-struct-obarray *struct-table* 'sys.int::*initial-structure-obarray*)
    (let ((actual-general-area-bump *general-area-bump*)
          (actual-cons-area-bump *cons-area-bump*))
      (finalize-areas)
      ;; Initialize GC twiddly bits and stuff.
      (flet ((set-value (symbol value)
               (format t "~A is ~X~%" symbol value)
               (setf (cold-symbol-value symbol) (make-fixnum value))))
        (set-value 'sys.int::*wired-area-base* +wired-area-base+)
        (set-value 'sys.int::*wired-area-bump* *wired-area-bump*)
        (set-value 'sys.int::*pinned-area-base* +pinned-area-base+)
        (set-value 'sys.int::*pinned-area-bump* *pinned-area-bump*)
        (set-value 'sys.int::*general-area-bump* actual-general-area-bump)
        (set-value 'sys.int::*general-area-limit* *general-area-bump*)
        (set-value 'sys.int::*cons-area-bump* actual-cons-area-bump)
        (set-value 'sys.int::*cons-area-limit* *cons-area-bump*)
        (set-value 'sys.int::*wired-stack-area-bump* *stack-area-bump*)
        (set-value 'sys.int::*stack-area-bump* +stack-area-base+)
        (set-value 'sys.int::*bytes-allocated-to-stacks* *stack-area-bytes*)))
    (setf (cold-symbol-value 'sys.int::*structure-type-type*) (make-value *structure-definition-definition* sys.int::+tag-object+))
    (setf (cold-symbol-value 'sys.int::*structure-slot-type*) (make-value *structure-slot-definition-definition* sys.int::+tag-object+))
    (apply-fixups *pending-fixups*)
    (write-map-file (merge-pathnames (or map-file-name (format nil "~A.map" image-name))
                                     (build-directory))
                    *function-map*)
    (format t "UUID ~/cold-generator::format-uuid/~%" uuid)
    (if (streamp image-name)
        (write-image image-name
                     (make-value (function-reference 'sys.int::bootloader-entry-point)
                                 sys.int::+tag-object+)
                     initial-thread
                     image-size
                     header-path
                     uuid)
        (with-open-file (s (merge-pathnames (make-pathname :type "image" :defaults image-name)
                                            (build-directory))
                           :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
          (write-image s
                       (make-value (function-reference 'sys.int::bootloader-entry-point)
                                   sys.int::+tag-object+)
                       initial-thread
                       image-size
                       header-path
                       uuid)))))

(defun load-source-files (files set-fdefinitions &optional wired)
  (dolist (f files)
    (cond ((consp f)
           (when (member sys.c::*target-architecture* (rest f))
             (load-source-file (first f) set-fdefinitions wired)))
          (t
           (load-source-file f set-fdefinitions wired)))))

(defun make-bignum (value)
  (let* ((length (ceiling (1+ (integer-length value)) 64))
         (address (allocate (1+ length))))
    (setf (word address) (array-header sys.int::+object-tag-bignum+ length))
    (dotimes (i length)
      (setf (word (+ address 1 i)) (ldb (byte 64 (* i 64)) value)))
    (make-value address sys.int::+tag-object+)))

(defun structure-slot-equal (x y)
  (and (eql (sys.c::structure-slot-name x)
            (sys.c::structure-slot-name y))
       (or (eql (sys.c::structure-slot-type x)
                (sys.c::structure-slot-type y))
           ;; FIXME: This needs to be a proper type comparison...
           (equal (extract-object (sys.c::structure-slot-type x))
                  (extract-object (sys.c::structure-slot-type y))))
       (eql (sys.c::structure-slot-read-only x)
            (sys.c::structure-slot-read-only y))))

(defun ensure-structure-layout-compatible (definition slots)
  (let ((definition-slots (third definition)))
    (unless (and (eql (length definition-slots) (length slots))
                 (every #'structure-slot-equal slots definition-slots))
      (error "Incompatible redefinition of structure. ~S ~S~%" definition slots))))

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

(defun value-is-function-p (value)
  (and (eql (tag-part value) sys.int::+tag-object+)
       (let* ((address (pointer-part value))
              (header (word address))
              (tag (ldb (byte sys.int::+object-type-size+
                              sys.int::+object-type-shift+)
                        header)))
         (or (eql tag sys.int::+object-tag-function+)
             (eql tag sys.int::+object-tag-closure+)
             (eql tag sys.int::+object-tag-funcallable-instance+)))))

(defun extract-object (value)
  (let ((slot-def (gethash value *image-to-cross-slot-definitions*)))
    (when slot-def
      (return-from extract-object slot-def)))
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
       ;; Avoid recursing down lists.
       (let* ((result (cons nil nil))
              (tail result))
         (loop
            (setf (cdr tail) (cons (extract-object (word address)) :incomplete)
                  tail (cdr tail))
            (setf value (word (1+ address))
                  address (pointer-part value))
            (when (not (eql (tag-part value) sys.int::+tag-cons+))
              (setf (cdr tail) (extract-object value))
              (return (cdr result))))))
      (#.sys.int::+tag-object+
       (let* ((header (word address))
              (tag (ldb (byte sys.int::+object-type-size+
                              sys.int::+object-type-shift+)
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
  (cond ((eql *default-cons-allocation-area* :cons)
         (let ((address (allocate 2 :cons)))
           (setf (word address) car
                 (word (1+ address)) cdr)
           (make-value address sys.int::+tag-cons+)))
        (t
         (let ((address (allocate 4 *default-cons-allocation-area*)))
           (setf (word address) (array-header sys.int::+object-tag-cons+ 0)
                 (word (+ address 2)) car
                 (word (+ address 3)) cdr)
           (make-value (+ address 2) sys.int::+tag-cons+)))))

(defun vlist (&rest args)
  (if args
      (vcons (first args) (apply 'vlist (rest args)))
      (vintern "NIL" "COMMON-LISP")))

(defun vlist* (arg &rest args)
  (if args
      (vcons arg (apply 'vlist* args))
      arg))

(defun function-header (code-length pool-length metadata-length &optional (tag sys.int::+object-tag-function+))
  (assert (< (ceiling (+ code-length 16) 16) (expt 2 (cross-cl:byte-size sys.int::+function-header-code-size+))))
  (assert (< pool-length (expt 2 (cross-cl:byte-size sys.int::+function-header-pool-size+))))
  (assert (< metadata-length (expt 2 (cross-cl:byte-size sys.int::+function-header-metadata-size+))))
  (array-header tag
                (logior (cross-cl:dpb (ceiling (+ code-length 16) 16) sys.int::+function-header-code-size+ 0)
                        (cross-cl:dpb pool-length sys.int::+function-header-pool-size+ 0)
                        (cross-cl:dpb metadata-length sys.int::+function-header-metadata-size+ 0))))

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
    ((and (listp name)
          (eql (length name) 2)
          (eql (first name) 'sys.int::cas)
          (symbolp (second name)))
     (vlist (vintern "CAS" "SYSTEM.INTERNALS")
            (vintern (symbol-name (second name))
                     (package-name (symbol-package (second name))))))
    (t (error "Bad function name ~S." name))))

(defun function-reference (name)
  "Get the function-reference associated with NAME, returning an untagged address.
Tag with +TAG-OBJECT+."
  (let ((fref (gethash name *fref-table*)))
    (unless fref
      ;; Frefs are wired, just like symbols.
      (setf fref (allocate 4 :wired))
      (setf (word (+ fref 0)) (array-header sys.int::+object-tag-function-reference+ 0)
            (word (+ fref 1)) (generate-fref-name name)
            (word (+ fref 2)) (make-value *undefined-function-address* sys.int::+tag-object+)
            (word (+ fref 3)) (word (1+ *undefined-function-address*)))
      (setf (gethash name *fref-table*) fref)
      (when (symbolp name)
        (let ((sym-addr (symbol-address (symbol-name name)
                                        (package-name (symbol-package name)))))
          (setf (word (+ sym-addr 4)) (make-value fref sys.int::+tag-object+)))))
    fref))

(defun apply-fixups (fixups)
  (mapc 'apply-fixup fixups))

(defun apply-fixup (fixup)
  (destructuring-bind (what address byte-offset type debug-info) fixup
    (declare (ignore debug-info))
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
                        (:unbound-value (unbound-value))
                        ((:undefined-function undefined-function)
                         (make-value *undefined-function-address* sys.int::+tag-object+))
                        ((:closure-trampoline closure-trampoline)
                         (make-value *closure-trampoline-address* sys.int::+tag-object+))
                        ((:funcallable-instance-trampoline funcallable-instance)
                         (make-value *f-i-trampoline-address* sys.int::+tag-object+)))))
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

(defparameter *cross-source-files*
  '("system/basic-macros.lisp"
    "system/defmacro.lisp"
    "system/backquote.lisp"
    "system/setf.lisp"
    "system/cas.lisp"
    "system/defstruct.lisp"
    "system/condition.lisp"
    "system/restarts.lisp"
    "system/error.lisp"
    "system/type.lisp"
    "system/runtime-array.lisp"
    "system/array.lisp"
    "system/sequence.lisp"
    "system/hash-table.lisp"
    "system/packages.lisp"
    "system/gray-streams.lisp"
    "system/stream.lisp"
    "system/reader.lisp"
    "system/printer.lisp"
    "system/numbers.lisp"
    "system/character.lisp"
    "system/clos/package.lisp"
    "system/clos/macros.lisp"
    "system/clos/closette.lisp"
    "system/data-types.lisp"
    "system/gc.lisp"
    "system/cold-start.lisp"
    "system/cons.lisp"
    "system/runtime-numbers.lisp"
    "supervisor/thread.lisp"
    "supervisor/interrupts.lisp"
    "supervisor/entry.lisp"
    "supervisor/physical.lisp"
    "supervisor/x86-64/cpu.lisp"
    "supervisor/arm64/cpu.lisp"
    "supervisor/support.lisp"
    "runtime/struct.lisp"
    "runtime/array.lisp"
    "runtime/symbol.lisp"
    "system/stuff.lisp"
)
  "These files are loaded into the compiler environment so other source
files will be compiled correctly.")

(defun set-up-cross-compiler ()
  (with-compilation-unit ()
    (flet ((load-files (file-list)
             (dolist (f file-list)
               (cond ((consp f)
                      (sys.c::load-for-cross-compiler (first f)))
                     (t
                      (sys.c::load-for-cross-compiler f))))))
      (load-files *cross-source-files*)
      (load-files *supervisor-source-files*)
      (load-files *source-files*)
      (load-files *warm-source-files*))))
