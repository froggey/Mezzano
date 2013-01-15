(in-package #:sys.int)

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
  (multiple-value-bind (allocated-words total-words largest-free-space)
      (static-area-info *small-static-area*)
    (format t "Small static space: ~:D/~:D words allocated (~D%).~%"
            allocated-words total-words
            (truncate (* allocated-words 100) total-words))
    (format t "  Largest free area: ~:D words.~%" largest-free-space))
  (multiple-value-bind (allocated-words total-words largest-free-space)
      (static-area-info *large-static-area*)
    (format t "Large static space: ~:D/~:D words allocated (~D%).~%"
            allocated-words total-words
            (truncate (* allocated-words 100) total-words))
    (format t "  Largest free area: ~:D words.~%" largest-free-space))
  (format t "Stack space: ~:D/~:D words allocated (~D%).~%"
          (truncate (- *stack-bump-pointer* #x100000000) 8)
          (truncate (- *stack-bump-pointer-limit* #x100000000) 8)
          (truncate (* (- *stack-bump-pointer* #x100000000) 100)
                    (- *stack-bump-pointer-limit* #x100000000)))
  (values))

(defun static-area-info (space)
  (let ((allocated-words 0)
        (total-words 0)
        (offset 0)
        (largest-free-space 0))
    (with-interrupts-disabled ()
      (loop (let ((size (memref-unsigned-byte-64 space offset))
                  (info (memref-unsigned-byte-64 space (+ offset 1))))
              (incf total-words (+ size 2))
              (cond ((logtest info #b010)
                     (incf allocated-words (+ size 2)))
                    (t ; free block.
                     (setf largest-free-space (max largest-free-space size))))
              (when (logtest info #b100)
                (return))
              (incf offset (+ size 2)))))
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

(defun scavenge-many (address n)
  (dotimes (i n)
    (setf (memref-t address i)
          (scavenge-object (memref-t address i)))))

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
  (let* ((sg-pointer (ash (%pointer-field (current-stack-group)) 4))
	 (ds-base (memref-unsigned-byte-64 sg-pointer 7))
	 (ds-size (memref-unsigned-byte-64 sg-pointer 8))
	 (bs-base (memref-unsigned-byte-64 sg-pointer 9))
	 (bs-size (memref-unsigned-byte-64 sg-pointer 10))
         (binding-stack-pointer (memref-unsigned-byte-64 sg-pointer 1))
         (data-stack-pointer (ash (%%get-data-stack-pointer) 3)))
    (mumble "Scav control stack")
    (do ((fp (read-frame-pointer)
             (memref-unsigned-byte-64 fp 0)))
        ((= fp 0))
      (setf (memref-t fp -2) (scavenge-object (memref-t fp -2))))
    (mumble "Scav data stack")
    (scavenge-many data-stack-pointer
                   (ash (- (+ ds-base ds-size) data-stack-pointer) -3))
    (mumble "Scav binding stack")
    (scavenge-many binding-stack-pointer
                   (ash (- (+ bs-base bs-size) binding-stack-pointer) -3)))
  (values a1 a2 a3 a4 a5))

(defun scavenge-object (object)
  "Scavange one object, returning an updated pointer."
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

(defun scan-stack-group (object)
  (let ((address (ash (%pointer-field object) 4)))
    (scavenge-many address (- 512 64)) ; skip the fxsave area at the end.
    ;; Scan the data/binding stacks only when the sg is not active.
    (when (not (eql (logand (memref-t address 2) +stack-group-state-mask+) +stack-group-active+))
      ;; FIXME: Need to scan the saved RIP, or at least the function associated with it.
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
          (setf (memref-t fp -2) (scavenge-object (memref-t fp -2))))
        (scavenge-many data-stack-pointer
                       (ash (- (+ ds-base ds-size) data-stack-pointer) -3))
        (scavenge-many binding-stack-pointer
                       (ash (- (+ bs-base bs-size) binding-stack-pointer) -3))))))

(defun scan-array-like (object)
  (let* ((address (ash (%pointer-field object) 4))
         (header (memref-unsigned-byte-64 address 0))
         (length (ldb (byte 56 8) header))
         (type (ldb (byte 5 3) header)))
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
         (header (memref-unsigned-byte-64 address 0))
         (mc-size (ash (ldb (byte 16 16) header) 1))
         (pool-size (ldb (byte 16 32) header)))
    (scavenge-many (+ address (* mc-size 8)) pool-size)))

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
         (type (ldb (byte 5 3) header))
         (length (ldb (byte 56 8) header)))
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
    (when (zerop (ldb (byte 1 1) (memref-unsigned-byte-64 address -1)))
      (mumble-hex object)
      (emergency-halt "Marking free static object."))
    (when (eql (ldb (byte 1 0) (memref-unsigned-byte-64 address -1)) *static-mark-bit*)
      ;; Object has already been marked.
      (return-from mark-static-object))
    (setf (ldb (byte 1 0) (memref-unsigned-byte-64 address -1)) *static-mark-bit*)
    (with-gc-trace (object #\s)
      (scan-object object))))

(defun sweep-static-space (space)
  (mumble "Sweeping static space")
  (let ((offset 0)
        (last-free-tag nil))
    (loop (let ((size (memref-unsigned-byte-64 space offset))
                (info (memref-unsigned-byte-64 space (+ offset 1))))
            (when (and (logtest info #b010)
                       (not (eql (ldb (byte 1 0) info) *static-mark-bit*)))
              ;; Allocated, but not marked. Must not be reachable.
              (setf (ldb (byte 1 1) (memref-unsigned-byte-64 space (+ offset 1))) 0))
            (if (zerop (ldb (byte 1 1) (memref-unsigned-byte-64 space (+ offset 1))))
                ;; Free tag.
                (cond (last-free-tag
                       ;; Merge adjacent free tags.
                       (incf (memref-unsigned-byte-64 space last-free-tag) (+ size 2))
                       (when (logtest info #b100)
                         ;; Last tag.
                         (setf (ldb (byte 1 2) (memref-unsigned-byte-64 space (1+ last-free-tag))) 1)
                         (return)))
                      (t ;; Previous free tag.
                       (setf last-free-tag offset)))
                ;; Allocated tag.
                (setf last-free-tag nil))
            (when (logtest info #b100)
              (return))
            (incf offset (+ size 2))))))

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
                  (not (logtest info #b010)))
         ;; Large enough to satisfy and free.
         (unless (= size words)
           ;; Larger than required. Split it.
           (setf (memref-unsigned-byte-64 space (+ hint 2 words)) (- size words 2)
                 (memref-unsigned-byte-64 space (+ hint 3 words)) (logand info #b100)
                 (memref-unsigned-byte-64 space hint) words
                 (ldb (byte 1 2) (memref-unsigned-byte-64 space (+ hint 1))) 0))
         ;; Initialize the static header words.
         (setf (ldb (byte 1 0) (memref-unsigned-byte-64 space (+ hint 1))) *static-mark-bit*
               (ldb (byte 1 1) (memref-unsigned-byte-64 space (+ hint 1))) 1)
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
            (logior (ash length 8) (ash tag +array-type-shift+)))
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
  (sys.lap-x86:mov64 :rcx 16)
  (sys.lap-x86:mov64 :r8 32) ; fixnum 4 (ugh)
  (sys.lap-x86:mov64 :r9 (:constant :static))
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:mov64 (:r8) #.(logior (ash 2 8) (ash +array-type-bignum+ +array-type-shift+)))
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
  (sys.lap-x86:mov64 :rcx 16)
  (sys.lap-x86:mov64 :r8 16) ; fixnum 2 (ugh)
  (sys.lap-x86:mov64 :r9 (:constant :static))
  (sys.lap-x86:mov64 :r13 (:constant %raw-allocate))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :lsp :rbx)
  ;; fixnum to pointer.
  (sys.lap-x86:sar64 :r8 3)
  ;; Set the header.
  (sys.lap-x86:mov64 (:r8) #.(logior (ash 1 8) (ash +array-type-bignum+ +array-type-shift+)))
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
    (let* ((address (%raw-allocate 2 :static)))
      (setf (memref-unsigned-byte-64 address 0) (logior (ash 1 8) (ash +array-type-bignum+ +array-type-shift+))
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
