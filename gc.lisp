(in-package #:sys.int)

(declaim (special *oldspace* *newspace* *newspace-offset* *semispace-size*))

;;; FIXME: Should use unwind-protect but that conses!!!
;;; TODO: Require that this can never nest (ie interrupts are on "all" the time).
(defmacro with-interrupts-disabled (options &body code)
  `(let ((istate (%interrupt-state)))
     (%cli)
     (prog1 (progn ,@code) (when istate (%sti)))))

;;; FIXME: Don't use with-interrupts-disabled.
;;; Suppress preemption (SBCL pseudo-atomic-like operation).

(defun room (&optional (verbosity :default))
  (format t "~&~D/~D words allocated (~D%).~%"
          *newspace-offset* *semispace-size*
          (truncate (* *newspace-offset* 100) *semispace-size*))
  (values))

;;; This is the fundamental dynamic allocation function.
;;; It ensures there is enough space on the dynamic heap to
;;; allocate WORDS words of memory and returns a fixnum address
;;; to the allocated memory. It violates GC invariants by twiddling
;;; *newspace-offset* without clearing memory, so must be called
;;; with the GC defered (currently by using WITH-INTERRUPTS-DISABLED)
;;; and the caller must clear the returned memory before reenabling the GC.
;;; Additionally, the number of words to allocate must be even.
(defun %raw-allocate (words)
  (when (> (+ *newspace-offset* words) *semispace-size*)
    (gc)
    (when (> (+ *newspace-offset* words) *semispace-size*)
      ;; Oh dear. No memory.
      (emergency-halt "Out of memory.")))
  (prog1 (+ *newspace* (ash *newspace-offset* 3))
    (incf *newspace-offset* words)))

(defun cons (car cdr)
  (with-interrupts-disabled ()
    (let ((cons (%%assemble-value (%raw-allocate 2) +tag-cons+)))
      (setf (car cons) car
            (cdr cons) cdr)
      cons)))

(defun allocate-std-instance (class slots)
  (with-interrupts-disabled ()
    (let ((value (%%assemble-value (%raw-allocate 2) +tag-std-instance+)))
      (setf (std-instance-class value) class
            (std-instance-slots value) slots)
      value)))

(defun %allocate-array-like (tag word-count length)
  "Allocate a array-like object. All storage is initialized to zero.
WORD-COUNT must be the number of words used to store the data, not including
the header word. LENGTH is the number of elements in the array."
  (with-interrupts-disabled ()
    ;; Align and account for the header word.
    (if (oddp word-count)
        (incf word-count 1)
        (incf word-count 2))
    (let ((address (%raw-allocate word-count)))
      ;; Clear memory.
      (dotimes (i word-count)
        (setf (memref-unsigned-byte-64 address i) 0))
      ;; Set header word.
      (setf (memref-unsigned-byte-64 address 0)
            (logior (ash length 8) (ash tag 1)))
      ;; Return value.
      (%%assemble-value address +tag-array-like+))))

(defun %make-struct (length)
  (%allocate-array-like 31 length length))

(defun make-closure (function environment)
  "Allocate a closure object."
  (check-type function function)
  (with-interrupts-disabled ()
    (let ((address (%raw-allocate 6)))
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
            (memref-unsigned-byte-32 address 7) #xCCCCCCCC
            ;; Constant pool.
            (memref-unsigned-byte-32 address 8) 0
            (memref-unsigned-byte-32 address 9) 0
            (memref-unsigned-byte-32 address 10) 0
            (memref-unsigned-byte-32 address 11) 0)
      (let ((value (%%assemble-value address +tag-function+)))
        ;; Initialize constant pool
        (setf (memref-t address 4) function
              (memref-t address 5) environment)
        value))))

(defun %make-array-header (dimensions fill-pointer info storage)
  (with-interrupts-disabled ()
    (let ((value (%%assemble-value (%raw-allocate 4) +tag-array-header+)))
      (setf (%array-header-dimensions value) dimensions
            (%array-header-fill-pointer value) fill-pointer
            (%array-header-info value) info
            (%array-header-storage value) storage)
      value)))

(defun make-symbol (name)
  (check-type name string)
  (with-interrupts-disabled ()
    (let* ((address (%raw-allocate 6))
           (symbol (%%assemble-value address +tag-symbol+)))
      ;; symbol-name.
      (setf (memref-t address 0) (sys.int::simplify-string name))
      (makunbound symbol)
      (%fmakunbound symbol)
      (setf (symbol-plist symbol) nil
            (symbol-package symbol) nil
            (%symbol-flags symbol) 0)
      symbol)))

(defun %allocate-stack (length)
  (when (oddp length)
    (incf length))
  (prog1 *bump-pointer*
    (incf *bump-pointer* (* length 8))))
