;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :cold-generator
  (:use :cl :iterate :nibbles))

(in-package :cold-generator)

(defparameter *supervisor-source-files*
  '("supervisor/entry.lisp"
    "supervisor/cpu.lisp"
    "supervisor/interrupts.lisp"
    "supervisor/debug.lisp"
    "supervisor/serial.lisp"
    "supervisor/disk.lisp"
    "supervisor/ata.lisp"
    "supervisor/thread.lisp"
    "supervisor/physical.lisp"
    "supervisor/snapshot.lisp"
    "supervisor/store.lisp"
    "supervisor/pager.lisp"
    "supervisor/time.lisp"
    "supervisor/ps2.lisp"
    "supervisor/video.lisp"
    "supervisor/pci.lisp"
    "supervisor/virtio.lisp"
    "supervisor/virtio-net.lisp"
    "supervisor/profiler.lisp"
    "runtime/runtime.lisp"
    "runtime/allocate.lisp"
    "runtime/numbers.lisp"
    "runtime/string.lisp"))

(defparameter *source-files*
  '("system/cold-start.lisp"
    "system/defstruct.lisp"
    "system/early-cons.lisp"
    "system/sequence.lisp"
    "system/runtime-array.lisp"
    "system/array.lisp"
    "system/printer.lisp"
    "system/stuff.lisp"
    "system/runtime-support.lisp"
    "system/type.lisp"
    "system/setf.lisp"
    "system/string.lisp"
    "system/hash-table.lisp"
    "system/runtime-numbers.lisp"
    "system/numbers.lisp"
    "system/reader.lisp"
    "system/character.lisp"
    "system/backquote.lisp"
    "system/format.lisp"
    "system/cons-compiler-macros.lisp"
    "system/defmacro.lisp"
    "system/basic-macros.lisp"
    "system/data-types.lisp"
    "system/parse.lisp"
    "system/describe.lisp"
    "system/gc.lisp"
    "system/load.lisp"
    "system/time.lisp"
    "system/profiler.lisp"
))

(defparameter *special-source-files*
  '(("system/packages.lisp" sys.int::*package-system*)))

(defparameter *warm-source-files*
  '("system/closette.lisp"
    "system/runtime-misc.lisp"
    "system/condition.lisp"
    "system/restarts.lisp"
    "system/error.lisp"
    "system/coerce.lisp"
    "system/debug.lisp"
    "system/full-eval.lisp"
    "system/fast-eval.lisp"
    "system/eval.lisp"
    "system/stream.lisp"
    "system/ansi-loop.lisp"
    "misc.lisp"
    "compiler/package.lisp"
    "lap.lisp"
    "lap-x86.lisp"
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
    "system/file-compiler.lisp"
    "gui/package.lisp"
    "gui/blit.lisp"
    "gui/compositor.lisp"
    "gui/input-drivers.lisp"
    "system/unifont.lisp"
    "gui/basic-repl.lisp"
    "net/ethernet.lisp"
    "file/fs.lisp"
    "file/remote.lisp"
    "ipl.lisp"))

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

(defparameter *closure-trampoline*
  `(;; Load the real function from the fref.
    (sys.lap-x86:mov64 :rbx (:object :r13 1))
    ;; Invoke the real function via the FUNCTION calling convention.
    ;; This will work even if the fref was altered or made funbound.
    ;; (SETF FUNCTION-REFERENCE-FUNCTION) will set the function to the
    ;; undefined-function thunk in that case, so everything works fine.
    (sys.lap-x86:jmp (:rbx ,(+ (- sys.int::+tag-object+) 8))))
  "Trampoline used for calling a closure via an fref.")

(defvar *symbol-table*)
(defvar *reverse-symbol-table*)
;; Hash-table mapping function names to function references.
(defvar *fref-table*)
(defvar *struct-table*)
(defvar *unbound-value-address*)
(defvar *unbound-tls-slot-address*)
(defvar *undefined-function-address*)
(defvar *closure-trampoline-address*)
(defvar *load-time-evals*)
(defvar *string-dedup-table*)
(defvar *structure-definition-definition*)

(defvar *function-map*)
(defvar *pending-fixups*)

;;; Memory allocation.

;; Wired area starts at 2M.
(defconstant +wired-area-base+ (* 2 1024 1024))
;; Pinned at 2G.
(defconstant +pinned-area-base+ (* 2 1024 1024 1024))
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

(defstruct stack
  base
  size
  store)

(defvar *stack-list*)
(defvar *stack-area-bump*)

(defvar *store-bump*)

(defvar *default-general-allocation-area* :general)
(defvar *default-cons-allocation-area* :cons)
(defvar *default-pinned-allocation-area* :pinned)

(defun align-up (value boundary)
  (incf value (1- boundary))
  (- value (rem value boundary)))

(defun allocate-1 (size bump-symbol data-symbol data-offset limit tag name)
  (when (>= (+ (symbol-value bump-symbol) size) limit)
    (error "~A area exceeded limit." name))
  (let ((address (symbol-value bump-symbol)))
    (incf (symbol-value bump-symbol) size)
    ;; Keep data vector page aligned, but don't expand it too often.
    (let ((dv-size (align-up (symbol-value bump-symbol) #x200000)))
      (when (not (eql (- dv-size data-offset)
                      (length (symbol-value data-symbol))))
        (setf (symbol-value data-symbol) (adjust-array (symbol-value data-symbol)
                                                       (- dv-size data-offset)
                                                       :element-type '(unsigned-byte 8)))))
    (/ (logior address (ash tag sys.int::+address-tag-shift+)) 8)))

(defun allocate (word-count &optional area)
  (when (oddp word-count) (incf word-count))
  (let ((size (* word-count 8)))
    (ecase (or area *default-general-allocation-area*)
      (:wired
       (allocate-1 size '*wired-area-bump* '*wired-area-data* +wired-area-base+ +wired-area-limit+ sys.int::+address-tag-pinned+ "wired"))
      (:pinned
       (allocate-1 size '*pinned-area-bump* '*pinned-area-data* +pinned-area-base+ +area-limit+ sys.int::+address-tag-pinned+ "pinned"))
      (:general
       (allocate-1 size '*general-area-bump* '*general-area-data* 0 +area-limit+ sys.int::+address-tag-general+ "general"))
      (:cons
       (allocate-1 size '*cons-area-bump* '*cons-area-data* 0 +area-limit+ sys.int::+address-tag-cons+ "cons")))))

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

(defun compile-lap-function (code &key (area *default-pinned-allocation-area*) extra-symbols constant-values (position-independent t))
  "Compile a list of LAP code as a function. Constants must only be symbols."
  (let ((base-address (if position-independent
                          0
                          *wired-area-bump*)))
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
                           (ceiling (length gc-info) 8))))
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
  (or (gethash (cons name package) *symbol-table*)
      (when (not createp)
        (error "Symbol ~A::~A does not exist."
               package
               name))
      ;; Symbols are always wired.
      (let ((address (allocate 6 :wired)))
        ;; fixme, keywords should be constant.
        (setf (word (+ address 0)) (array-header sys.int::+object-tag-symbol+ 0)
              (word (+ address 1)) (make-value (store-string name :wired)
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
  (let ((nil-value (allocate 6 :wired))
        (t-value (allocate 6 :wired))
        (keyword-keyword (allocate 6 :wired))
        (cl-keyword (allocate 6 :wired))
        (unbound-val (allocate 2 :wired))
        (unbound-tls-val (allocate 2 :wired))
        (undef-fn (compile-lap-function *undefined-function-thunk* :area :wired))
        (closure-tramp (compile-lap-function *closure-trampoline* :area :wired))
        (struct-def-def (allocate 7 :wired)))
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
          *closure-trampoline-address* closure-tramp
          *unbound-value-address* unbound-val
          *unbound-tls-slot-address* unbound-tls-val)
    (setf (word (+ nil-value 0)) (array-header sys.int::+object-tag-symbol+ 0) ; flags & header
          (word (+ nil-value 1)) (make-value (store-string "NIL" :wired)
                                       sys.int::+tag-object+) ; name
          (word (+ nil-value 2)) (make-value cl-keyword sys.int::+tag-object+) ; package
          (word (+ nil-value 3)) (make-value nil-value sys.int::+tag-object+) ; value
          (word (+ nil-value 4)) (make-value nil-value sys.int::+tag-object+) ; function
          (word (+ nil-value 5)) (make-value nil-value sys.int::+tag-object+)) ; plist
    (setf (word (+ t-value 0)) (array-header sys.int::+object-tag-symbol+ 0)
          (word (+ t-value 1)) (make-value (store-string "T" :wired)
                                     sys.int::+tag-object+)
          (word (+ t-value 2)) (make-value cl-keyword sys.int::+tag-object+)
          (word (+ t-value 3)) (make-value t-value sys.int::+tag-object+)
          (word (+ t-value 4)) (make-value nil-value sys.int::+tag-object+)
          (word (+ t-value 5)) (make-value nil-value sys.int::+tag-object+))
    (setf (word (+ keyword-keyword 0)) (array-header sys.int::+object-tag-symbol+ 0)
          (word (+ keyword-keyword 1)) (make-value (store-string "KEYWORD" :wired)
                                     sys.int::+tag-object+)
          (word (+ keyword-keyword 2)) (make-value keyword-keyword sys.int::+tag-object+)
          (word (+ keyword-keyword 3)) (make-value keyword-keyword sys.int::+tag-object+)
          (word (+ keyword-keyword 4)) (make-value nil-value sys.int::+tag-object+)
          (word (+ keyword-keyword 5)) (make-value nil-value sys.int::+tag-object+))
    (setf (word (+ cl-keyword 0)) (array-header sys.int::+object-tag-symbol+ 0)
          (word (+ cl-keyword 1)) (make-value (store-string "COMMON-LISP" :wired)
                                     sys.int::+tag-object+)
          (word (+ cl-keyword 2)) (make-value keyword-keyword sys.int::+tag-object+)
          (word (+ cl-keyword 3)) (make-value cl-keyword sys.int::+tag-object+)
          (word (+ cl-keyword 4)) (make-value nil-value sys.int::+tag-object+)
          (word (+ cl-keyword 5)) (make-value nil-value sys.int::+tag-object+))
    (setf (word unbound-val) (array-header sys.int::+object-tag-unbound-value+ 0))
    (setf (word unbound-tls-val) (array-header sys.int::+object-tag-unbound-value+ 1))
    (setf *structure-definition-definition* struct-def-def
          (word (+ struct-def-def 0)) (array-header sys.int::+object-tag-structure-object+ 6)
          (word (+ struct-def-def 1)) (make-value *structure-definition-definition* sys.int::+tag-object+)
          (word (+ struct-def-def 2)) (vsym 'sys.int::structure-definition)
          ;; (name accessor initial-value type read-only atomic).
          (word (+ struct-def-def 3)) (vlist (vlist (vsym 'sys.int::name)   (vsym 'sys.int::structure-name)   (vsym 'nil) (vsym 't) (vsym 't)   (vsym 'nil))
                                             (vlist (vsym 'sys.int::slots)  (vsym 'sys.int::structure-slots)  (vsym 'nil) (vsym 't) (vsym 't)   (vsym 'nil))
                                             (vlist (vsym 'sys.int::parent) (vsym 'sys.int::structure-parent) (vsym 'nil) (vsym 't) (vsym 't)   (vsym 'nil))
                                             (vlist (vsym 'sys.int::area)   (vsym 'sys.int::structure-area)   (vsym 'nil) (vsym 't) (vsym 't)   (vsym 'nil))
                                             (vlist (vsym 'sys.int::class)  (vsym 'sys.int::structure-class)  (vsym 'nil) (vsym 't) (vsym 'nil) (vsym 'nil)))
          (word (+ struct-def-def 4)) (vsym 'nil)
          (word (+ struct-def-def 5)) (vsym :wired)
          (word (+ struct-def-def 6)) (vsym nil)
          (gethash 'sys.int::structure-definition *struct-table*) (list struct-def-def
                                                                        'sys.int::structure-definition
                                                                        '((sys.int::name sys.int::structure-name nil t t nil)
                                                                          (sys.int::slots sys.int::structure-slots nil t t nil)
                                                                          (sys.int::parent sys.int::structure-parent nil t t nil)
                                                                          (sys.int::area sys.int::structure-area nil t t nil)
                                                                          (sys.int::class sys.int::structure-class nil t nil nil))))))

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
             (setf (nibbles:ub64ref/le data (* i 8)) (logior (ash (/ next-block #x1000) sys.int::+block-map-id-shift+)
                                                             sys.int::+block-map-present+
                                                             sys.int::+block-map-writable+))))
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

(defun write-image (name entry-fref initial-thread image-size header-path)
  (with-open-file (s (make-pathname :type "image" :defaults name)
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
    (let* ((image-header-data (when header-path
                                (load-image-header header-path)))
           (image-offset (if image-header-data
                             (length image-header-data)
                             0))
           (bml4-block *store-bump*)
           (bml4 (make-array 512 :initial-element nil))
           (free-block-list (+ *store-bump* #x1000)))
      (decf image-size image-offset)
      (format t "Generating ~:D byte image.~%" image-size)
      (when image-header-data
        (format t "Using ~S as the image header.~%" header-path))
      (format t "BML4 at offset ~X~%" bml4-block)
      (format t "FBL  at offset ~X~%" free-block-list)
      (when image-header-data
        (write-sequence image-header-data s)
        ;; Update the size of the second partition entry, the Mezzano partiton.
        (file-position s #x1DA)
        (nibbles:write-ub32/le (truncate image-size 512) s))
      (incf *store-bump* #x2000)
      (file-position s (1- (+ image-offset image-size)))
      (write-byte 0 s)
      (file-position s image-offset)
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
        ;; Major version.
        (setf (ub16ref/le header 32) 0)
        ;; Minor version.
        (setf (ub16ref/le header 34) 20)
        ;; Number of extents.
        (setf (ub32ref/le header 36) 2)
        ;; Entry fref.
        (setf (ub64ref/le header 40) entry-fref)
        ;; Initial thread.
        (setf (ub64ref/le header 48) initial-thread)
        ;; NIL.
        (setf (ub64ref/le header 56) (make-value (symbol-address "NIL" "COMMON-LISP")
                                                 sys.int::+tag-object+))
        (setf (ub64ref/le header 64) (/ image-size #x1000))
        ;; 72-96 free.
        ;; Top-level block map.
        (setf (ub64ref/le header 96) (/ bml4-block #x1000))
        ;; Free block list.
        (setf (ub64ref/le header 104) (/ free-block-list #x1000))
        (flet ((extent (id virtual-base size flags &optional (extra 0))
                 (setf (ub64ref/le header (+ 112 (* id 32) 0)) virtual-base
                       (ub64ref/le header (+ 112 (* id 32) 8)) size
                       (ub64ref/le header (+ 112 (* id 32) 16)) flags
                       (ub64ref/le header (+ 112 (* id 32) 24)) extra)))
          (extent 0 +wired-area-base+ (- +wired-area-limit+ +wired-area-base+) 0)
          (extent 1
                  (logior +wired-stack-area-base+ (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+))
                  (- +wired-stack-area-limit+ +wired-stack-area-base+)
                  0))
        ;; Write it out.
        (write-sequence header s))
      ;; Write areas.
      (file-position s (+ image-offset *wired-area-store*))
      (format t "Wired area at ~X, ~:D bytes.~%"
              *wired-area-store* (- (align-up *wired-area-bump* #x1000) +wired-area-base+))
      (write-sequence *wired-area-data* s :end (- (align-up *wired-area-bump* #x1000) +wired-area-base+))
      (format t "Pinned area at ~X, ~:D bytes.~%"
              *pinned-area-store* (- (align-up *pinned-area-bump* #x1000) +pinned-area-base+))
      (file-position s (+ image-offset *pinned-area-store*))
      (write-sequence *pinned-area-data* s :end (- (align-up *pinned-area-bump* #x1000) +pinned-area-base+))
      (format t "General area at ~X, ~:D bytes.~%"
              *general-area-store* (align-up *general-area-bump* #x1000))
      (file-position s (+ image-offset *general-area-store*))
      (write-sequence *general-area-data* s :end (align-up *general-area-bump* #x1000))
      (format t "Cons area at ~X, ~:D bytes.~%"
              *cons-area-store* (align-up *cons-area-bump* #x1000))
      (file-position s (+ image-offset *cons-area-store*))
      (write-sequence *cons-area-data* s :end (align-up *cons-area-bump* #x1000))
      ;; Generate the block map.
      (add-region-to-block-map bml4
                               (/ *wired-area-store* #x1000)
                               +wired-area-base+
                               (/ (- (align-up *wired-area-bump* #x200000) +wired-area-base+) #x1000)
                               (logior sys.int::+block-map-present+
                                       sys.int::+block-map-writable+))
      (add-region-to-block-map bml4
                               (/ *pinned-area-store* #x1000)
                               +pinned-area-base+
                               (/ (- (align-up *pinned-area-bump* #x200000) +pinned-area-base+) #x1000)
                               (logior sys.int::+block-map-present+
                                       sys.int::+block-map-writable+))
      (add-region-to-block-map bml4
                               (/ *general-area-store* #x1000)
                               (ash sys.int::+address-tag-general+ sys.int::+address-tag-shift+)
                               (/ (align-up *general-area-bump* #x200000) #x1000)
                               (logior sys.int::+block-map-present+
                                       sys.int::+block-map-writable+))
      (add-region-to-block-map bml4
                               (/ (+ *general-area-store* (align-up *general-area-bump* #x200000)) #x1000)
                               (logior (ash sys.int::+address-tag-general+ sys.int::+address-tag-shift+)
                                       (ash 1 sys.int::+address-mark-bit+))
                               (/ (align-up *general-area-bump* #x200000) #x1000)
                               (logior sys.int::+block-map-zero-fill+))
      (add-region-to-block-map bml4
                               (/ *cons-area-store* #x1000)
                               (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                               (/ (align-up *cons-area-bump* #x200000) #x1000)
                               (logior sys.int::+block-map-present+
                                       sys.int::+block-map-writable+))
      (add-region-to-block-map bml4
                               (/ (+ *cons-area-store* (align-up *cons-area-bump* #x200000)) #x1000)
                               (logior (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                                       (ash 1 sys.int::+address-mark-bit+))
                               (/ (align-up *cons-area-bump* #x200000) #x1000)
                               (logior sys.int::+block-map-zero-fill+))
      (dolist (stack *stack-list*)
        (add-region-to-block-map bml4
                                 (/ (stack-store stack) #x1000)
                                 (stack-base stack)
                                 (/ (stack-size stack) #x1000)
                                 (logior sys.int::+block-map-present+
                                         sys.int::+block-map-writable+
                                         sys.int::+block-map-zero-fill+)))
      ;; Now write it out.
      (write-block-map s image-offset bml4-block bml4)
      ;; Create the freelist.
      ;; One entry, allocating our storage area.
      (let ((freelist-data (make-array #x1000 :element-type '(unsigned-byte 8) :initial-element 0)))
        (setf (nibbles:ub64ref/le freelist-data 0) 0
              (nibbles:ub64ref/le freelist-data 8) (ash (/ *store-bump* #x1000) 1))
        (file-position s (+ image-offset free-block-list))
        (write-sequence freelist-data s))))
  (values))

(defun array-header (tag length)
  (logior (ash tag sys.int::+array-type-shift+)
          (ash length sys.int::+array-length-shift+)))

(defun pack-halfwords (low high)
  (check-type low (unsigned-byte 32))
  (check-type high (unsigned-byte 32))
  (dpb high (byte 32 32) low))

(defun create-thread (name &key stack-size (initial-state :runnable) (preemption-disable-depth 0) (foothold-disable-depth 0))
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
    (setf (word (+ address 1)) (make-value (store-string name)
                                           sys.int::+tag-object+))
    ;; State.
    (setf (word (+ address 2)) (make-value (symbol-address (symbol-name initial-state) "KEYWORD")
                                           sys.int::+tag-object+))
    ;; Lock.
    (setf (word (+ address 3)) (make-value (symbol-address "UNLOCKED" "KEYWORD")
                                           sys.int::+tag-object+))
    ;; Stack.
    (setf (word (+ address 4)) stack-object)
    ;; Stack pointer.
    (setf (word (+ address 5)) (+ (stack-base stack)
                                  (stack-size stack)))
    ;; +6, unused
    ;; Special stack pointer.
    (setf (word (+ address 7)) (make-value (symbol-address "NIL" "COMMON-LISP")
                                           sys.int::+tag-object+))
    ;; Preemption disable depth.
    (setf (word (+ address 8)) (make-fixnum preemption-disable-depth))
    ;; Preemption pending.
    (setf (word (+ address 9)) (make-value (symbol-address "NIL" "COMMON-LISP")
                                           sys.int::+tag-object+))
    ;; Next.
    (setf (word (+ address 10)) (make-value (symbol-address "NIL" "COMMON-LISP")
                                            sys.int::+tag-object+))
    ;; Prev.
    (setf (word (+ address 11)) (make-value (symbol-address "NIL" "COMMON-LISP")
                                            sys.int::+tag-object+))
    ;; foothold disable depth.
    (setf (word (+ address 12)) (make-fixnum foothold-disable-depth))
    ;; mutex stack.
    (setf (word (+ address 14)) (vsym 'nil))
    (make-value address sys.int::+tag-object+)))

(defun create-initial-thread ()
  (setf (cold-symbol-value 'sys.int::*initial-thread*)
        (create-thread "Initial thread"
                       :stack-size (* 16 1024)
                       :initial-state :active
                       :preemption-disable-depth 1
                       :foothold-disable-depth 1)))

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

(defun vsym (symbol)
  (make-value (symbol-address (symbol-name symbol) (canonical-symbol-package symbol))
              sys.int::+tag-object+))

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
  (load-source-file "%%compiler-builtins.llf" t t))

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

;; Handlers for the defined CPU exceptions, a bool indicating if they take
;; an error code or not and the IST to use.
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
    (sys.int::%page-fault-handler t 1)             ; 14
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

(defun create-low-level-interrupt-support ()
  "Generate the ISR thunks that call into Lisp."
  ;; For maximum flexibility, create ISRs for all the IDT entries.
  (let* ((idt-size 256)
         (isr-table (allocate (1+ idt-size) :wired)))
    ;; Create the ISR table.
    (setf (word isr-table) (array-header sys.int::+object-tag-array-t+ idt-size))
    (setf (cold-symbol-value 'sys.int::*interrupt-service-routines*) (make-value isr-table sys.int::+tag-object+))
    ;; Generate the ISR thunks.
    (let* ((exception-isrs (loop
                              for (handler error-code-p ist) in *cpu-exception-info*
                              collect
                                (when handler
                                  (create-exception-isr handler error-code-p))))
           (user-interrupt-isrs (loop
                                   for i from 32 below idt-size
                                   collect (create-user-interrupt-isr i)))
           (common-code (compile-lap-function *common-interrupt-code*
                                              :area :wired
                                              :position-independent nil))
           (common-user-code (compile-lap-function *common-user-interrupt-code*
                                                   :area :wired
                                                   :position-independent nil
                                                   :extra-symbols (list (cons 'interrupt-common (+ (* common-code 8) 16))))))
      (let ((fref (function-reference 'sys.int::%%common-isr-thunk)))
        (setf (word (+ fref 1 sys.int::+fref-function+)) (make-value common-code sys.int::+tag-object+)
              (word (+ fref 1 sys.int::+fref-entry-point+)) (+ (* common-code 8) 16)))
      (let ((fref (function-reference 'sys.int::%%common-user-isr-thunk)))
        (setf (word (+ fref 1 sys.int::+fref-function+)) (make-value common-user-code sys.int::+tag-object+)
              (word (+ fref 1 sys.int::+fref-entry-point+)) (+ (* common-user-code 8) 16)))
      (loop
         for isr in (append exception-isrs user-interrupt-isrs)
         for i from 0
         when isr do
           ;; Assemble the ISR and update the ISR entry.
           (let ((addr (compile-lap-function isr
                                             :area :wired
                                             :position-independent nil
                                             :extra-symbols (list (cons 'interrupt-common (+ (* common-code 8) 16))
                                                                  (cons 'user-interrupt-common (+ (* common-user-code 8) 16))))))
             (setf (word (+ isr-table 1 i)) (make-value addr sys.int::+tag-object+)))
         else do
           (setf (word (+ isr-table 1 i)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))))))

(defun finalize-areas ()
  "Assign store addresses to each area, and build pinned/wired area freelists."
  ;; Ensure a minium amount of free space in :wired.
  ;; And :pinned as well, but that matters less.
  (let ((wired-free-area (allocate (* 2048 1024) :wired))
        (pinned-free-area (allocate (* 1024 1024) :pinned)))
    (setf *wired-area-bump* (align-up *wired-area-bump* #x200000))
    (setf (word wired-free-area) (logior (ash sys.int::+object-tag-freelist-entry+ sys.int::+array-type-shift+)
                                         (ash (truncate (- *wired-area-bump*
                                                           (ldb (byte 44 0) (* wired-free-area 8)))
                                                        8)
                                              sys.int::+array-length-shift+))
          (word (1+ wired-free-area)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))
    (setf (cold-symbol-value 'sys.int::*wired-area-freelist*) (make-fixnum (* wired-free-area 8)))
    (setf *pinned-area-bump* (align-up *pinned-area-bump* #x200000))
    (setf (word pinned-free-area) (logior (ash sys.int::+object-tag-freelist-entry+ sys.int::+array-type-shift+)
                                          (ash (truncate (- *pinned-area-bump*
                                                            (ldb (byte 44 0) (* pinned-free-area 8)))
                                                         8)
                                               sys.int::+array-length-shift+))
          (word (1+ pinned-free-area)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))
    (setf (cold-symbol-value 'sys.int::*pinned-area-freelist*) (make-fixnum (* pinned-free-area 8)))
    (setf *wired-area-bump* (align-up *wired-area-bump* #x200000))
    (setf *wired-area-store* *store-bump*)
    (incf *store-bump* (- *wired-area-bump* +wired-area-base+))
    (setf *pinned-area-bump* (align-up *pinned-area-bump* #x200000))
    (setf *pinned-area-store* *store-bump*)
    (incf *store-bump* (- *pinned-area-bump* +pinned-area-base+))
    (setf *general-area-store* *store-bump*)
    (incf *store-bump* (* (align-up *general-area-bump* #x200000) 2))
    (setf *cons-area-store* *store-bump*)
    (incf *store-bump* (* (align-up *cons-area-bump* #x200000) 2))
    (dolist (stack *stack-list*)
      (setf (stack-store stack) *store-bump*)
      (incf *store-bump* (stack-size stack)))))

(defun make-image (image-name &key extra-source-files header-path (image-size (* 256 1024 1024)))
  (let* ((*wired-area-bump* +wired-area-base+)
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
         (*function-map* '())
         (*string-dedup-table* (make-hash-table :test 'equal))
         (initial-thread)
         (cl-symbol-names (with-open-file (s "tools/cl-symbols.lisp-expr") (read s)))
         (system-symbol-names (remove-duplicates
                               (iter (for sym in-package :system external-only t)
                                     (collect (symbol-name sym)))
                               :test #'string=))
         (pf-exception-stack (create-stack (* 128 1024)))
         (irq-stack (create-stack (* 128 1024))))
    ;; Generate the support objects. NIL/T/etc, and the initial thread.
    (create-support-objects)
    (create-low-level-interrupt-support)
    (setf (cold-symbol-value 'sys.int::*exception-stack-base*) (make-fixnum (stack-base pf-exception-stack))
          (cold-symbol-value 'sys.int::*exception-stack-size*) (make-fixnum (stack-size pf-exception-stack)))
    (setf (cold-symbol-value 'sys.int::*irq-stack-base*) (make-fixnum (stack-base irq-stack))
          (cold-symbol-value 'sys.int::*irq-stack-size*) (make-fixnum (stack-size irq-stack)))
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
    (format t "Saving Unifont...~%")
    (save-unifont-data "tools/unifont-5.1.20080820.hex")
    (format t "Saving Unicode data...~%")
    (multiple-value-bind (planes name-store encoding-table name-trie)
        (build-unicode:generate-unicode-data-tables (build-unicode:read-unicode-data "tools/UnicodeData.txt"))
      (setf (cold-symbol-value 'sys.int::*unicode-info*) (save-object planes :pinned)
            (cold-symbol-value 'sys.int::*unicode-name-store*) (save-ub8-vector name-store :pinned)
            (cold-symbol-value 'sys.int::*unicode-encoding-table*) (save-object encoding-table :pinned)
            (cold-symbol-value 'sys.int::*unicode-name-trie*) (save-object name-trie :pinned)))
    ;; Bake the compiled warm source files in.
    (let ((warm-files (make-array 10 :adjustable t :fill-pointer 0)))
      (dolist (file *warm-source-files*)
        (let ((llf-path (merge-pathnames (make-pathname :type "llf" :defaults file))))
          (when (or (not (probe-file llf-path))
                    (<= (file-write-date llf-path) (file-write-date file)))
            (format t "~A is out of date will be recompiled.~%" llf-path)
            (sys.c::cross-compile-file file))
          (format t "Loading ~A.~%" llf-path)
          (with-open-file (warm llf-path :element-type '(unsigned-byte 8))
            (let ((vec (make-array (file-length warm) :element-type '(unsigned-byte 8))))
              (read-sequence vec warm)
              (vector-push-extend (cons (pathname-name llf-path) vec) warm-files)))))
      (setf (cold-symbol-value 'sys.int::*warm-llf-files*) (save-object warm-files :pinned)))
    #+nil(format t "Saving PCI IDs...~%")
    #+nil(let* ((pci-ids (build-pci-ids:build-pci-ids "tools/pci.ids"))
           (object (save-object pci-ids :static)))
      (setf (cold-symbol-value 'sys.int::*pci-ids*) object))
    ;; Poke a few symbols to ensure they exist. This avoids memory allocation after finalize-areas runs.
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
            sys.int::*next-symbol-tls-slot*
            sys.int::*wired-area-freelist*
            sys.int::*wired-area-bump*
            sys.int::*pinned-area-freelist*
            sys.int::*pinned-area-bump*
            sys.int::*general-area-bump*
            sys.int::*cons-area-bump*
            sys.int::*stack-area-bump*
            :wired :pinned :general :cons :nursery :stack
            ))
    (setf (cold-symbol-value 'sys.int::*bsp-idle-thread*)
          (create-thread "BSP idle thread"
                         :stack-size (* 16 1024)
                         :preemption-disable-depth 1
                         :foothold-disable-depth 1))
    (setf (cold-symbol-value 'sys.int::*snapshot-thread*)
          (create-thread "Snapshot thread"
                         :stack-size (* 128 1024)
                         :preemption-disable-depth 1
                         :foothold-disable-depth 1
                         :initial-state :sleeping))
    (setf (cold-symbol-value 'sys.int::*pager-thread*)
          (create-thread "Pager thread"
                         :stack-size (* 128 1024)
                         :preemption-disable-depth 1
                         :foothold-disable-depth 1
                         :initial-state :sleeping))
    (setf (cold-symbol-value 'sys.int::*disk-io-thread*)
          (create-thread "Disk IO thread"
                         :stack-size (* 128 1024)
                         :preemption-disable-depth 1
                         :foothold-disable-depth 1
                         :initial-state :sleeping))
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
    (finalize-areas)
    ;; Initialize GC twiddly bits and stuff.
    (flet ((set-value (symbol value)
             (format t "~A is ~X~%" symbol value)
             (setf (cold-symbol-value symbol) (make-fixnum value))))
      (set-value 'sys.int::*next-symbol-tls-slot* (eval (read-from-string "MEZZANO.SUPERVISOR::+THREAD-TLS-SLOTS-START+")))
      (set-value 'sys.int::*wired-area-bump* *wired-area-bump*)
      (set-value 'sys.int::*pinned-area-bump* *pinned-area-bump*)
      (set-value 'sys.int::*general-area-bump* *general-area-bump*)
      (set-value 'sys.int::*cons-area-bump* *cons-area-bump*)
      (set-value 'sys.int::*wired-stack-area-bump* *stack-area-bump*)
      (set-value 'sys.int::*stack-area-bump* +stack-area-base+))
    (setf (cold-symbol-value 'sys.int::*structure-type-type*) (make-value *structure-definition-definition* sys.int::+tag-object+))
    (apply-fixups *pending-fixups*)
    (write-map-file image-name *function-map*)
    (write-image image-name
                 (make-value (function-reference 'sys.int::%%bootloader-entry-point)
                             sys.int::+tag-object+)
                 initial-thread
                 image-size
                 header-path)))

(defun load-source-files (files set-fdefinitions &optional wired)
  (mapc (lambda (f) (load-source-file f set-fdefinitions wired)) files))

(defvar *load-should-set-fdefinitions*)

(defun make-bignum (value)
  (let* ((length (ceiling (1+ (integer-length value)) 64))
         (address (allocate (1+ length))))
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
         (string-data (make-array len :element-type '(unsigned-byte 32)))
         (min-width 1))
    ;; Read all characters and figure out how wide the data vector must be.
    (dotimes (i len)
      (let ((ch (load-character stream)))
        (setf (aref string-data i) ch)
        (setf min-width (max min-width
                             (cond ((>= ch (expt 2 16)) 4)
                                   ((>= ch (expt 2 8)) 2)
                                   (t 1))))))
    (let* ((as-string (map 'string #'code-char string-data))
           (existing (gethash as-string *string-dedup-table*)))
      (unless existing
        (let ((object-address (allocate 6))
              (data-value (ecase min-width
                            (4 (save-ub32-vector string-data))
                            (2 (save-ub16-vector string-data))
                            (1 (save-ub8-vector string-data)))))
          ;; String container
          (setf (word (+ object-address 0)) (array-header sys.int::+object-tag-string+ 1)
                (word (+ object-address 1)) data-value
                (word (+ object-address 2)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
                (word (+ object-address 3)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
                (word (+ object-address 4)) (make-fixnum len))
          (setf existing (make-value object-address sys.int::+tag-object+)
                (gethash as-string *string-dedup-table*) existing)))
      existing)))

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
          (t (let ((address (allocate 7 :wired)))
               (setf (word address) (array-header sys.int::+object-tag-structure-object+ 6))
               (setf (word (+ address 1)) (make-value *structure-definition-definition* sys.int::+tag-object+))
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
         (address (allocate total-size *default-pinned-allocation-area*)))
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

(defun load-one-object (command stream stack)
  (ecase command
    (#.sys.int::+llf-function+
     (load-llf-function stream stack))
    (#.sys.int::+llf-cons+
     (let* ((car (vector-pop stack))
            (cdr (vector-pop stack)))
       (vcons car cdr)))
    (#.sys.int::+llf-symbol+
     (let* ((name (load-string* stream))
            (package (load-string* stream)))
       (make-value (symbol-address name package)
                   sys.int::+tag-object+)))
    (#.sys.int::+llf-uninterned-symbol+
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
    (#.sys.int::+llf-unbound+ (unbound-value))
    (#.sys.int::+llf-string+ (load-string stream))
    (#.sys.int::+llf-integer+
     (let ((value (load-integer stream)))
       (typecase value
         ((signed-byte 63) (make-fixnum value))
         (t (make-bignum value)))))
    (#.sys.int::+llf-invoke+
     ;; `(funcall ',fn)
     (let* ((fn (vector-pop stack))
            (form (vlist (vintern "FUNCALL" "COMMON-LISP")
                         (vlist (vintern "QUOTE" "COMMON-LISP")
                                fn))))
       (push form *load-time-evals*))
     nil)
    (#.sys.int::+llf-setf-fdefinition+
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
    (#.sys.int::+llf-simple-vector+
     (load-llf-vector stream stack))
    (#.sys.int::+llf-character+
     (logior (ash (load-character stream) 4)
             sys.int::+tag-character+))
    (#.sys.int::+llf-structure-definition+
     (let ((area (vector-pop stack))
           (parent (vector-pop stack))
           (slots (vector-pop stack))
           (name (vector-pop stack)))
       (load-structure-definition name slots parent area)))
    (#.sys.int::+llf-single-float+
     (logior (ash (load-integer stream) 32)
             sys.int::+tag-single-float+))
    (#.sys.int::+llf-proper-list+
     (let ((list (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))
           (length (load-integer stream)))
       (dotimes (i length)
         (setf list (vcons (vector-pop stack) list)))
       list))
    (#.sys.int::+llf-integer-vector+
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
    (#.sys.int::+llf-bit-vector+
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
    (#.sys.int::+llf-function-reference+
     (let* ((name (vector-pop stack))
            (truname (extract-object name)))
       (make-value (function-reference truname)
                   sys.int::+tag-object+)))))

(defun load-llf (stream)
  (let ((omap (make-hash-table))
        (stack (make-array 64 :adjustable t :fill-pointer 0)))
    (loop (let ((command (read-byte stream)))
            (case command
              (#.sys.int::+llf-end-of-load+
               (return))
              (#.sys.int::+llf-backlink+
               (let ((id (load-integer stream)))
                 (multiple-value-bind (value value-p)
                     (gethash id omap)
                   (unless value-p
                     (error "Unknown backlink ID ~D." id))
                   (vector-push-extend value stack))))
              (#.sys.int::+llf-add-backlink+
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

(defun load-source-file (file set-fdefinitions &optional wired)
  (let ((llf-path (merge-pathnames (make-pathname :type "llf" :defaults file)))
        (*load-should-set-fdefinitions* set-fdefinitions)
        (*default-general-allocation-area* (if wired :wired :general))
        (*default-cons-allocation-area* (if wired :wired :cons))
        (*default-pinned-allocation-area* (if wired :wired :pinned)))
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
                   (eql (read-byte s) #x01))
              ()
              "Bad LLF magic. Probably old-style LLF, please remove and rebuild.")
      (let ((version (load-integer s)))
        (assert (eql version sys.int::*llf-version*)
                ()
                "Bad LLF version ~D, wanted version ~D." version sys.int::*llf-version*))
      ;; Read forms.
      (load-llf s))))

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
                        (:unbound-tls-slot
                         (make-value *unbound-tls-slot-address*
                                     sys.int::+tag-object+))
                        (:unbound-value (unbound-value))
                        ((:undefined-function undefined-function)
                         (make-value *undefined-function-address* sys.int::+tag-object+))
                        ((:closure-trampoline closure-trampoline)
                         (make-value *closure-trampoline-address* sys.int::+tag-object+)))))
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
