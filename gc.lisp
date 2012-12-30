(in-package #:sys.int)

(declaim (special *oldspace* *newspace* *newspace-offset* *semispace-size*
                  *oldspace-paging-bits* *newspace-paging-bits*))
(declaim (special *static-area* *static-mark-bit* *static-area-hint*))
(declaim (special *stack-bump-pointer* *stack-bump-pointer-limit*))
(declaim (special *bump-pointer*))
(declaim (special *verbose-gc*))
;;; GC Meters.
(declaim (special *objects-copied* *words-copied*))
(declaim (special *multiboot-info* *kboot-tag-list*))
(setf *verbose-gc* nil)
(setf *objects-copied* 0
      *words-copied* 0)

(defvar *gc-in-progress* nil)

(defconstant +multiboot-flag-mem-info+     #b00000001)
(defconstant +multiboot-flag-boot-device+  #b00000010)
(defconstant +multiboot-flag-command-line+ #b00000100)
(defconstant +multiboot-flag-modules+      #b00001000)
(defconstant +multiboot-flag-aout-symbols+ #b00010000)
(defconstant +multiboot-flag-elf-symbols+  #b00100000)
(defconstant +multiboot-flag-memory-map+   #b01000000)

(defun gc-init-system-memory ()
  ;; Compute the true end of the image by examining the multiboot header.
  ;; FIXME: This must be done as part of the initialization process.
  ;; FIXME: Makes major assumptions regarding how modules are laid out in memory.
  (when (and *multiboot-info*
             (logtest (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 0) +multiboot-flag-modules+))
    (let ((module-count (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 5))
          (module-base (+ #x8000000000 (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 6))))
      (unless (zerop module-count)
        (setf *bump-pointer* (+ #x8000000000
                                (memref-unsigned-byte-32 module-base (+ (* (1- module-count) 4) 1)))))))
  ;; Allocate DMA memory from the largest free MEMORY region.
  (when *kboot-tag-list*
    (flet ((p/8 (addr) (memref-unsigned-byte-8 (+ #x8000000000 addr) 0))
           (p/16 (addr) (memref-unsigned-byte-16 (+ #x8000000000 addr) 0))
           (p/32 (addr) (memref-unsigned-byte-32 (+ #x8000000000 addr) 0))
           (p/64 (addr) (memref-unsigned-byte-64 (+ #x8000000000 addr) 0)))
      (let ((addr *kboot-tag-list*)
            ;; For sanity checking.
            (max-addr (+ *kboot-tag-list* 1024))
            (best-start nil)
            (best-size 0))
        (loop (when (>= addr max-addr) (return))
           (let ((type (p/32 (+ addr 0)))
                 (size (p/32 (+ addr 4))))
             (when (and (eql addr *kboot-tag-list*)
                        (not (eql type +kboot-tag-core+)))
               (format t "CORE tag not first in the list?~%")
               (return))
             (case type
               (#.+kboot-tag-none+ (return))
               (#.+kboot-tag-core+
                (unless (eql addr *kboot-tag-list*)
                  (format t "CORE tag not first in the list?~%")
                  (return))
                (setf max-addr (+ *kboot-tag-list* (p/32 (+ addr 16)))))
               (#.+kboot-tag-memory+
                (let ((start (p/64 (+ addr 8)))
                      (length (p/64 (+ addr 16)))
                      (type (p/8 (+ addr 24))))
                  (when (and (eql type +kboot-memory-free+)
                             (> length best-size))
                    (setf best-start start
                          best-size length)))))
             (incf addr (round-up size 8))))
        (when best-start
          (setf *bump-pointer* (+ best-start #x8000000000))))))
  (when (logtest *bump-pointer* #xFFF)
    (setf *bump-pointer* (+ (logand *bump-pointer* (lognot #xFFF)) #x1000))))

#+nil(add-hook '*early-initialize-hook* 'gc-init-system-memory)

(defvar *gc-stack-group* (make-stack-group "GC"
                                           :control-stack-size 50000
                                           :data-stack-size 50000))
(stack-group-preset *gc-stack-group* #'gc-task)

;;; FIXME: Should use unwind-protect but that conses!!!
;;; TODO: Require that this can never nest (ie interrupts are on "all" the time).
(defmacro with-interrupts-disabled (options &body code)
  `(let ((istate (%interrupt-state)))
     (%cli)
     (multiple-value-prog1 (progn ,@code) (when istate (%sti)))))

;;; FIXME: Don't use with-interrupts-disabled.
;;; Suppress preemption (SBCL pseudo-atomic-like operation).

(defun room (&optional (verbosity :default))
  (fresh-line)
  (format t "Dynamic space: ~:D/~:D words allocated (~D%).~%"
          *newspace-offset* *semispace-size*
          (truncate (* *newspace-offset* 100) *semispace-size*))
  (multiple-value-bind (allocated-words total-words)
      (static-area-info)
    (format t "Static space: ~:D/~:D words allocated (~D%).~%"
            allocated-words total-words
            (truncate (* allocated-words 100) total-words)))
  (format t "Stack space: ~:D/~:D words allocated (~D%).~%"
          (truncate (- *stack-bump-pointer* #x100000000) 8)
          (truncate (- *stack-bump-pointer-limit* #x100000000) 8)
          (truncate (* (- *stack-bump-pointer* #x100000000) 100)
                    (- *stack-bump-pointer-limit* #x100000000)))
  (values))

(defun static-area-info ()
  (let ((allocated-words 0)
        (total-words 0)
        (offset 0))
    (with-interrupts-disabled ()
      (loop (let ((size (memref-unsigned-byte-64 *static-area* offset))
                  (info (memref-unsigned-byte-64 *static-area* (+ offset 1))))
              (incf total-words (+ size 2))
              (when (logtest info #b010)
                (incf allocated-words (+ size 2)))
              (when (logtest info #b100)
                (return))
              (incf offset (+ size 2)))))
    (values allocated-words total-words)))

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

(defun scan-error (object transport)
  (declare (ignore object transport))
  (emergency-halt "unscannable object"))

(defun scan-generic (object transportp size)
  (let* ((old-address (ash (%pointer-field object) 4))
         (new-address old-address)
         (first-value (memref-t old-address 0)))
    ;; Check for a forwarding pointer.
    (when (eql (%tag-field first-value) +tag-gc-forward+)
      (return-from scan-generic
        (%%assemble-value (ash (%pointer-field first-value) 4) (%tag-field object))))
    (when transportp
      (incf *objects-copied*)
      (incf *words-copied* size)
      ;; Leave a forwarding pointer.
      (setf new-address (+ *newspace* (ash *newspace-offset* 3)))
      ;; Copy fields without updating them.
      ;; This prevents SCAN-GENERIC from breaking when it's copying a symbol
      ;; that it uses.
      (%fast-copy new-address old-address (ash size 3))
      (setf (memref-t old-address 0) (%%assemble-value new-address +tag-gc-forward+))
      (incf *newspace-offset* size))
    (setf (memref-t new-address 0) (gc-object first-value))
    (dotimes (i (1- size))
      (setf (memref-t new-address (1+ i))
            (gc-object (memref-t old-address (1+ i)))))
    (%%assemble-value new-address (%tag-field object))))

(defun scan-cons (object transportp)
  (scan-generic object transportp 2))

(defun scan-symbol (object transportp)
  (scan-generic object transportp 6))

(defun scan-array-header (object transportp)
  (scan-generic object transportp 4))

(defun scan-array-t (object transportp)
  (let* ((old-address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 old-address 0))
         (length (ldb (byte 56 8) header))
         (word-count (1+ length))
         (new-address old-address))
    (when (hash-table-p object)
      (setf (hash-table-rehash-required object) 't))
    (when (oddp word-count) (incf word-count))
    (when transportp
      (incf *objects-copied*)
      (incf *words-copied* word-count)
      (setf new-address (+ *newspace* (ash *newspace-offset* 3)))
      (incf *newspace-offset* word-count)
      ;; Set the GC flag and leave a forwarding pointer.
      (setf (memref-unsigned-byte-64 old-address 0) (logior new-address 1)))
    (setf (memref-unsigned-byte-64 new-address 0) header)
    (dotimes (i length)
      (setf (memref-t new-address (1+ i))
            (gc-object (memref-t old-address (1+ i)))))
    (%%assemble-value new-address +tag-array-like+)))

(defun scan-stack-group (object transportp)
  (when transportp
    (emergency-halt "Attempting to transport a stack-group!"))
  (let ((address (ash (%pointer-field object) 4)))
    (dotimes (i (- 511 64)) ; skip the fxsave area at the end.
      (let ((x (memref-t address (1+ i))))
        (when (not (eql (%tag-field x) +tag-unbound-value+))
          (setf (memref-t address (1+ i)) (gc-object x)))))
    ;; Scan the data/binding stacks only when the sg is not active.
    (when (not (eql (logand (memref-t address 2) +stack-group-state-mask+) +stack-group-active+))
      ;; FIXME: Need to scan the control stack as well, along with the saved RIP and stuff.
      (let* ((ds-base (memref-unsigned-byte-64 address 7))
             (ds-size (memref-unsigned-byte-64 address 8))
             (bs-base (memref-unsigned-byte-64 address 9))
             (bs-size (memref-unsigned-byte-64 address 10))
             (binding-stack-pointer (memref-unsigned-byte-64 address 1))
             (control-stack-pointer (memref-unsigned-byte-64 address 3))
             (data-stack-pointer (memref-unsigned-byte-64 control-stack-pointer 2)))
        (do ((fp (memref-unsigned-byte-64 control-stack-pointer 0)
                 (memref-unsigned-byte-64 fp 0)))
            ((= fp 0))
          (setf (memref-t fp -2)
                (gc-object (memref-t fp -2))))
        (dotimes (i (ash (- (+ ds-base ds-size) data-stack-pointer) -3))
          (setf (memref-t data-stack-pointer i)
                (gc-object (memref-t data-stack-pointer i))))
        (dotimes (i (ash (- (+ bs-base bs-size) binding-stack-pointer) -3))
          (setf (memref-t binding-stack-pointer i)
                (gc-object (memref-t binding-stack-pointer i))))))
    object))

(defun scan-numeric-array (object transportp width)
  (when (not transportp)
    ;; Numeric arrays have nothing to scan so just return.
    (return-from scan-numeric-array object))
  (let* ((old-address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 old-address 0))
         (length (ldb (byte 56 8) header))
         (word-count (1+ (ceiling (* length width) 64)))
         (new-address (+ *newspace* (ash *newspace-offset* 3))))
    (when (oddp word-count) (incf word-count))
    (incf *objects-copied*)
    (incf *words-copied* word-count)
    (incf *newspace-offset* word-count)
    ;; Set the GC flag and leave a forwarding pointer.
    (setf (memref-unsigned-byte-64 old-address 0) (logior new-address 1))
    ;; Just copy data over.
    (setf (memref-unsigned-byte-64 new-address 0) header)
    (%fast-copy (+ new-address 8) (+ old-address 8) (ash (1- word-count) 3))
    (%%assemble-value new-address +tag-array-like+)))

(defun scan-array-like (object transportp)
  (let* ((address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 address 0)))
    ;; Check the GC bit.
    (when (logtest header 1)
      (return-from scan-array-like
        (%%assemble-value (logand header -2)
                          +tag-array-like+)))
    ;; Dispatch again based on the type.
    (case (ldb (byte 5 1) header)
      (0  (scan-array-t object transportp)) ; simple-vector
      (1  (scan-numeric-array object transportp 8)) ; base-char
      (2  (scan-numeric-array object transportp 32)) ; character
      (3  (scan-numeric-array object transportp 1)) ; bit
      (4  (scan-numeric-array object transportp 2)) ; unsigned-byte 2
      (5  (scan-numeric-array object transportp 4)) ; unsigned-byte 4
      (6  (scan-numeric-array object transportp 8)) ; unsigned-byte 8
      (7  (scan-numeric-array object transportp 16)) ; unsigned-byte 16
      (8  (scan-numeric-array object transportp 32)) ; unsigned-byte 32
      (9  (scan-numeric-array object transportp 64)) ; unsigned-byte 64
      (10 (scan-numeric-array object transportp 1)) ; signed-byte 1
      (11 (scan-numeric-array object transportp 2)) ; signed-byte 2
      (12 (scan-numeric-array object transportp 4)) ; signed-byte 4
      (13 (scan-numeric-array object transportp 8)) ; signed-byte 8
      (14 (scan-numeric-array object transportp 16)) ; signed-byte 16
      (15 (scan-numeric-array object transportp 32)) ; signed-byte 32
      (16 (scan-numeric-array object transportp 64)) ; signed-byte 64
      (17 (scan-numeric-array object transportp 32)) ; single-float
      (18 (scan-numeric-array object transportp 64)) ; double-float
      (19 (scan-numeric-array object transportp 128)) ; long-float
      (20 (scan-numeric-array object transportp 128)) ; xmm-vector
      (21 (scan-numeric-array object transportp 64)) ; complex single-float
      (22 (scan-numeric-array object transportp 128)) ; complex double-float
      (23 (scan-numeric-array object transportp 256)) ; complex long-float
      (24 (scan-error object transportp)) ; unused (24)
      (25 (scan-numeric-array object transportp 64)) ; bignum
      (26 (scan-error object transportp)) ; unused (26)
      (27 (scan-error object transportp)) ; unused (27)
      (28 (scan-error object transportp)) ; unused (28)
      (29 (scan-array-t object transportp)) ; std-instance
      (30 (scan-stack-group object transportp))
      (31 (scan-array-t object transportp)) ; struct
      (t (scan-error object transportp)))))

(defun scan-function (object transportp)
  (when transportp
    (emergency-halt "Attempting to transport a function!"))
  ;; Scan the constant pool.
  (let* ((address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 address 0))
         (mc-size (ash (ldb (byte 16 16) header) 1))
         (pool-size (ldb (byte 16 32) header)))
    (dotimes (i pool-size)
      (setf (memref-t address (+ mc-size i))
            (gc-object (memref-t address (+ mc-size i)))))
    object))

(defmacro with-gc-trace ((object prefix) &body body)
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

#+nil(defmacro with-gc-trace ((object prefix) &body body)
  (declare (ignore object prefix))
  `(progn ,@body))

(defun scan-object (object transportp)
  (case (%tag-field object)
    (0  (scan-error object transportp)) ; even-fixnum
    (1  (scan-cons object transportp))
    (2  (scan-symbol object transportp))
    (3  (scan-array-header object transportp))
    (4  (scan-error object transportp)) ; unused
    (5  (scan-error object transportp)) ; unused
    (6  (scan-error object transportp)) ; unused
    (7  (scan-array-like object transportp))
    (8  (scan-error object transportp)) ; odd-fixnum
    (9  (scan-error object transportp)) ; unused
    (10 (scan-error object transportp)) ; character
    (11 (scan-error object transportp)) ; unused
    (12 (scan-function object transportp))
    (13 (scan-error object transportp)) ; unused
    (14 (scan-error object transportp)) ; unbound-value
    (15 (scan-error object transportp))))

(defun scan-and-transport (object)
  (with-gc-trace (object #\d)
    (scan-object object t)))

(defun mark-static-object (object)
  (let ((address (ash (%pointer-field object) 4)))
    (when (zerop (ldb (byte 1 1) (memref-unsigned-byte-64 address -1)))
      (emergency-halt "Marking free static object."))
    (when (eql (ldb (byte 1 0) (memref-unsigned-byte-64 address -1)) *static-mark-bit*)
      ;; Object has already been marked.
      (return-from mark-static-object))
    (setf (ldb (byte 1 0) (memref-unsigned-byte-64 address -1)) *static-mark-bit*)
    (with-gc-trace (object #\s)
      (scan-object object nil))))

(defun gc-object (object)
  (when (or (fixnump object)
            (floatp object)
            (eql (%tag-field object) +tag-unbound-value+))
    ;; Attempting to shift the pointer-field of negative fixnums
    ;; about will cause a fixnum overflow.
    ;; TLS-unbound tags also have the same issue.
    (return-from gc-object object))
  (let ((address (ash (%pointer-field object) 4)))
    (cond ((characterp object)
           ;; Do nothing with immediate objects.
           object)
          ((oldspace-pointer-p address)
           ;; Object is in oldspace, transport to newspace.
           (scan-and-transport object))
          ((newspace-pointer-p address)
           ;; Object is already in newspace.
           ;; Can this ever happen??
           object)
          ((static-pointer-p address)
           ;; Object is in the static area, mark and scan.
           (mark-static-object object)
           object)
          (t
           ;; Assume the pointer is on the stack.
           ;; TODO: Track scanned stack objects. Allocate a cons with dynamic-extent
           ;; and push on some symbol.
           (scan-object object nil)))))

;;; Arguments and MV return are to force the data registers on to the stack.
;;; This does not work for RBX or R13, but RBX is smashed by the function
;;; return and R13 will be holding a newspace pointer by the time this
;;; function is called.
(defun transport-registers-and-stack (a1 a2 a3 a4 a5)
  (let* ((sg-pointer (ash (%pointer-field (current-stack-group)) 4))
	 (ds-base (memref-unsigned-byte-64 sg-pointer 7))
	 (ds-size (memref-unsigned-byte-64 sg-pointer 8))
	 (bs-base (memref-unsigned-byte-64 sg-pointer 9))
	 (bs-size (memref-unsigned-byte-64 sg-pointer 10))
         (binding-stack-pointer (memref-unsigned-byte-64 sg-pointer 1))
         (data-stack-pointer (ash (%%get-data-stack-pointer) 3)))
    (mumble "Scanning control stack")
    (do ((fp (read-frame-pointer)
             (memref-unsigned-byte-64 fp 0)))
        ((= fp 0))
      (setf (memref-t fp -2)
            (gc-object (memref-t fp -2))))
    (mumble "Scanning data stack")
    (dotimes (i (ash (- (+ ds-base ds-size) data-stack-pointer) -3))
      (setf (memref-t data-stack-pointer i)
            (gc-object (memref-t data-stack-pointer i))))
    (mumble "Scanning binding stack")
    (dotimes (i (ash (- (+ bs-base bs-size) binding-stack-pointer) -3))
      (setf (memref-t binding-stack-pointer i)
            (gc-object (memref-t binding-stack-pointer i)))))
  (values a1 a2 a3 a4 a5))

;;; TODO: This needs to coalesce adjacent free areas.
(defun sweep-static-space ()
  (mumble "Sweeping static space")
  (let ((offset 0)
        (last-free-tag nil))
    (loop (let ((size (memref-unsigned-byte-64 *static-area* offset))
                (info (memref-unsigned-byte-64 *static-area* (+ offset 1))))
            (when (and (logtest info #b010)
                       (not (eql (ldb (byte 1 0) info) *static-mark-bit*)))
              ;; Allocated, but not marked. Must not be reachable.
              (setf (ldb (byte 1 1) (memref-unsigned-byte-64 *static-area* (+ offset 1))) 0))
            (if (zerop (ldb (byte 1 1) (memref-unsigned-byte-64 *static-area* (+ offset 1))))
                ;; Free tag.
                (cond (last-free-tag
                       ;; Merge adjacent free tags.
                       (incf (memref-unsigned-byte-64 *static-area* last-free-tag) (+ size 2))
                       (when (logtest info #b100)
                         ;; Last tag.
                         (setf (ldb (byte 1 2) (memref-unsigned-byte-64 *static-area* (1+ last-free-tag))) 1)
                         (return)))
                      (t ;; Previous free tag.
                       (setf last-free-tag offset)))
                ;; Allocated tag.
                (setf last-free-tag nil))
            (when (logtest info #b100)
              (return))
            (incf offset (+ size 2))))
    (setf *static-area-hint* 0)))

(defun gc-task ()
  (loop
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
       ;; Transport the major root, NIL.
       (gc-object 'nil)
       ;; Transport registers, the data stack and the binding stack.
       (transport-registers-and-stack 1 2 3 4 5)
       ;; Make oldspace inaccessible.
       (setf (ldb (byte 2 0) (memref-unsigned-byte-32 *oldspace-paging-bits* 0)) 0)
       ;; Flush TLB.
       (setf (%cr3) (%cr3))
       ;; Sweep static space.
       (sweep-static-space)
       (mumble "complete")
       (clear-gc-light)
       (stack-group-return t))))

(defun %gc ()
  (when *gc-in-progress*
    (error "Nested GC?!"))
  (unwind-protect
       (progn (setf *gc-in-progress* t)
              (stack-group-invoke *gc-stack-group*))
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
     (let ((address (or (when (not (zerop *static-area-hint*))
                          (%attempt-static-allocation words *static-area-hint*))
                        (%attempt-static-allocation words 0))))
       (unless address
         (%gc)
         (setf address (%attempt-static-allocation words 0))
         (unless address
           (emergency-halt "Static space exhausted.")))
       address))))

(defun %attempt-static-allocation (words hint)
  (loop
     (let ((size (memref-unsigned-byte-64 *static-area* hint))
           (info (memref-unsigned-byte-64 *static-area* (+ hint 1))))
       (when (and (>= size words)
                  (not (logtest info #b010)))
         ;; Large enough to satisfy and free.
         (unless (= size words)
           ;; Larger than required. Split it.
           (setf (memref-unsigned-byte-64 *static-area* (+ hint 2 words)) (- size words 2)
                 (memref-unsigned-byte-64 *static-area* (+ hint 3 words)) (logand info #b100)
                 (memref-unsigned-byte-64 *static-area* hint) words
                 (ldb (byte 1 2) (memref-unsigned-byte-64 *static-area* (+ hint 1))) 0))
         ;; Initialize the static header words.
         (setf (ldb (byte 1 0) (memref-unsigned-byte-64 *static-area* (+ hint 1))) *static-mark-bit*
               (ldb (byte 1 1) (memref-unsigned-byte-64 *static-area* (+ hint 1))) 1)
         (setf *static-area-hint* (+ hint 2 words))
         (return (+ *static-area* (* hint 8) 16)))
       (when (logtest info #b100)
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
            (logior (ash length 8) (ash tag 1)))
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
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:ret))

(defun make-function-with-fixups (tag machine-code fixups constants)
  (with-interrupts-disabled ()
    (let* ((mc-size (ceiling (+ (length machine-code) 12) 16))
           (pool-size (length constants))
           (total (+ (* mc-size 2) pool-size)))
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
              (memref-unsigned-byte-16 address 2) pool-size)
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
        (%%assemble-value address +tag-function+)))))

(defun make-function (machine-code constants)
  (make-function-with-fixups +function-type-function+ machine-code '() constants))

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

(defun make-symbol (name &optional area)
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

(define-lap-function %%make-bignum-128-rdx-rax ()
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:push :rdx)
  ;; Allocate a 2 word bignum.
  (sys.lap-x86:mov64 :rcx 8)
  (sys.lap-x86:mov64 :r8 32) ; fixnum 4 (ugh)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:mov64 (:r8) #.(logior (ash 2 8) (ash +array-type-bignum+ 1)))
  ;; Set values.
  (sys.lap-x86:pop (:r8 16))
  (sys.lap-x86:pop (:r8 8))
  ;; pointer to value.
  (sys.lap-x86:or64 :r8 #.+tag-array-like+)
  ;; GC back on.
  (sys.lap-x86:popf)
  ;; Single value return
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:ret))

(define-lap-function %%make-bignum-64-rax ()
  (sys.lap-x86:pushf)
  (sys.lap-x86:cli)
  (sys.lap-x86:push 0) ; alignment
  (sys.lap-x86:push :rax)
  ;; Allocate a 1 word bignum.
  (sys.lap-x86:mov64 :rcx 8)
  (sys.lap-x86:mov64 :r8 16) ; fixnum 2 (ugh)
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:mov64 (:r8) #.(logior (ash 1 8) (ash +array-type-bignum+ 1)))
  ;; Set values.
  (sys.lap-x86:pop (:r8 8))
  ;; realign stack.
  (sys.lap-x86:pop :rax)
  ;; pointer to value.
  (sys.lap-x86:or64 :r8 #.+tag-array-like+)
  ;; GC back on.
  (sys.lap-x86:popf)
  ;; Single value return
  (sys.lap-x86:mov64 :rbx :lsp)
  (sys.lap-x86:mov32 :ecx 8)
  (sys.lap-x86:ret))

;;; This is used by the bignum code so that bignums and fixnums don't have
;;; to be directly compared.
(defun %make-bignum-from-fixnum (n)
  (with-interrupts-disabled ()
    (let* ((address (%raw-allocate 2)))
      (setf (memref-unsigned-byte-64 address 0) (logior (ash 1 8) (ash +array-type-bignum+ 1))
            (memref-unsigned-byte-64 address 1) n)
      (%%assemble-value address +tag-array-like+))))

(defun %allocate-stack (length)
  (when (oddp length)
    (incf length))
  (with-interrupts-disabled ()
    (prog1 *stack-bump-pointer*
      (incf *stack-bump-pointer* (* length 8))
      (when (> *stack-bump-pointer* *stack-bump-pointer-limit*)
        (error "No more space for stacks!")))))

(defun allocate-dma-buffer (length)
  (with-interrupts-disabled ()
    (unless (zerop (logand *bump-pointer* #xFFF))
      (incf *bump-pointer* (logand *bump-pointer* #xFFF)))
    (let ((address *bump-pointer*))
      (incf *bump-pointer* length)
      (unless (zerop (logand *bump-pointer* #xFFF))
        (incf *bump-pointer* (logand *bump-pointer* #xFFF)))
      (values (make-array length
                          :element-type '(unsigned-byte 8)
                          :memory address)
              (- address #x8000000000)))))
