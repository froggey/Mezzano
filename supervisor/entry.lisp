(in-package :mezzanine.supervisor)

;;; FIXME: Should not be here.
;;; >>>>>>

(defun sys.int::current-thread ()
  (sys.int::%%assemble-value (sys.int::msr sys.int::+msr-ia32-gs-base+) 0))

(defun integerp (object)
  (sys.int::fixnump object))

(defun sys.int::%coerce-to-callable (object)
  (etypecase object
    (function object)
    (symbol
     (sys.int::%array-like-ref-t
      (sys.int::%array-like-ref-t object sys.c::+symbol-function+)
      sys.int::+fref-function+))))

;; Hardcoded string accessor, the support stuff for arrays doesn't function at this point.
(defun char (string index)
  (assert (sys.int::character-array-p string) (string))
  (let ((data (sys.int::%array-like-ref-t string 0)))
    (assert (and (<= 0 index)
                 (< index (sys.int::%object-header-data data)))
            (string index))
    (code-char
     (case (sys.int::%object-tag data)
       (#.sys.int::+object-tag-array-unsigned-byte-8+
        (sys.int::%array-like-ref-unsigned-byte-8 data index))
       (#.sys.int::+object-tag-array-unsigned-byte-16+
        (sys.int::%array-like-ref-unsigned-byte-16 data index))
       (#.sys.int::+object-tag-array-unsigned-byte-32+
        (sys.int::%array-like-ref-unsigned-byte-32 data index))
       (t 0)))))

(defun length (sequence)
  (if (sys.int::character-array-p sequence)
      (sys.int::%array-like-ref-t sequence 3)
      nil))

(defun code-char (code)
  (sys.int::%%assemble-value (ash code 4) sys.int::+tag-character+))

(defun char-code (character)
  (logand (ash (sys.int::lisp-object-address character) -4) #x1FFFFF))

(declaim (special sys.int::*newspace* sys.int::*newspace-offset*))

(defvar *allocator-lock*)

(defun sys.int::make-simple-vector (size &optional area)
  (declare (ignore area))
  (let ((words (1+ size)))
    (when (oddp words)
      (incf words))
    (with-spinlock (*allocator-lock*)
      ;; Assume we have enough memory to do the allocation...
      ;; And that the memory is already zero initialized.
      (let ((addr (+ sys.int::*newspace* (ash sys.int::*newspace-offset* 3))))
        (incf sys.int::*newspace-offset* words)
        ;; Write array header.
        (setf (sys.int::memref-unsigned-byte-64 addr 0)
              (ash size sys.int::+array-length-shift+))
        (sys.int::%%assemble-value addr sys.int::+tag-object+)))))

;;; <<<<<<

(defun initialize-initial-thread ()
  (let* ((sg (sys.int::current-thread))
         (bs-base (sys.int::%array-like-ref-unsigned-byte-64 sg sys.int::+stack-group-offset-binding-stack-base+))
         (bs-size (sys.int::%array-like-ref-unsigned-byte-64 sg sys.int::+stack-group-offset-binding-stack-size+)))
    ;; Clear binding stack.
    (dotimes (i (truncate bs-size 8))
      (setf (sys.int::memref-unsigned-byte-64 bs-base i) 0))
    ;; Set the binding stack pointer.
    (setf (sys.int::%array-like-ref-unsigned-byte-64 sg sys.int::+stack-group-offset-binding-stack-pointer+)
          (+ bs-base bs-size))
    ;; Reset the TLS binding slots.
    (dotimes (i sys.int::+stack-group-tls-slots-size+)
      (setf (sys.int::%array-like-ref-t sg (+ sys.int::+stack-group-offset-tls-slots+ i))
            (sys.int::%unbound-tls-slot)))))

(defun sys.int::bootloader-entry-point ()
  ;; The bootloader current does not properly initialize the
  ;; initial thread, do that now.
  (initialize-initial-thread)
  (setf *allocator-lock* :unlocked)
  (initialize-interrupts)
  (initialize-i8259)
  (sys.int::%sti)
  (initialize-debug-serial #x3F8 4)
  (debug-serial-write-line "Hello, Debug World!")
  (loop))
