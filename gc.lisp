(in-package #:sys.int)

(declaim (special *bump-pointer*))

(defun cons (car cdr)
  (let* ((address *bump-pointer*)
         (val (%%assemble-value address 1)))
    (setf (car val) car
          (cdr val) cdr)
    (incf *bump-pointer* 16)
    val))

(defun allocate-std-instance (class slots)
  (let* ((address *bump-pointer*)
         (val (%%assemble-value address #b0100)))
    (setf (std-instance-class val) class
          (std-instance-slots val) slots)
    (incf *bump-pointer* 16)
    val))

(defun %allocate-array-like (tag word-count length)
  "Allocate a array-like object. All storage is initialized to zero.
WORD-COUNT must be the number of words used to store the data, not including
the header word. LENGTH is the number of elements in the array."
  (let ((address *bump-pointer*))
    ;; Align and account for the header word.
    (if (oddp word-count)
        (incf word-count 1)
        (incf word-count 2))
    ;; Clear memory.
    (dotimes (i word-count)
      (setf (memref-unsigned-byte-64 address i) 0))
    ;; Set header word.
    (setf (memref-unsigned-byte-64 address 0)
          (logior (ash length 8) (ash tag 1)))
    ;; Advance pointer.
    (incf *bump-pointer* (* word-count 8))
    ;; Return value.
    (%%assemble-value address #b0111)))

(defun %make-struct (length)
  (%allocate-array-like 31 length length))

(defun make-closure (function environment)
  "Allocate a closure object."
  (check-type function function)
  (let ((address *bump-pointer*))
    (incf *bump-pointer* (* 6 8))
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
    (let ((value (%%assemble-value address #b1100)))
      ;; Initialize constant pool
      (setf (memref-t address 4) function
            (memref-t address 5) environment)
      value)))

(defun %make-array-header (dimensions fill-pointer info storage)
  (let* ((address *bump-pointer*)
         (val (%%assemble-value address #b0011)))
    (incf *bump-pointer* (* 4 8))
    (setf (%array-header-dimensions val) dimensions
          (%array-header-fill-pointer val) fill-pointer
          (%array-header-info val) info
          (%array-header-storage val) storage)
    val))

(defun make-symbol (name)
  (check-type name string)
  (let* ((address *bump-pointer*)
         (val (%%assemble-value address #b0010)))
    (incf *bump-pointer* (* 8 6))
    ;; symbol-name.
    (setf (memref-t address 0) (sys.int::simplify-string name))
    (makunbound val)
    (%fmakunbound val)
    (setf (symbol-plist val) nil
          (symbol-package val) nil
          (%symbol-flags val) 0)
    val))
