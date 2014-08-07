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
  (initialize-interrupts)
  (initialize-i8259)
  (sys.int::%sti)
  (initialize-debug-serial #x3F8 4)
  (loop))
