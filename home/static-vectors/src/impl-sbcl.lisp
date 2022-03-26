;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- SBCL implementation
;;;

(in-package :static-vectors)

(declaim (inline fill-foreign-memory))
(defun fill-foreign-memory (pointer length value)
  "Fill LENGTH octets in foreign memory area POINTER with VALUE."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-kernel:system-area-ub8-fill value pointer 0 length)
  pointer)

(declaim (inline replace-foreign-memory))
(defun replace-foreign-memory (dst-ptr src-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (sb-kernel:system-area-ub8-copy src-ptr 0 dst-ptr 0 length)
  dst-ptr)

(defconstant +array-header-size+
  (* sb-vm:vector-data-offset sb-vm:n-word-bytes))

(declaim (inline vector-widetag-and-n-bits))
(defun vector-widetag-and-n-bits (type)
  (let ((upgraded-type (upgraded-array-element-type type)))
    (case upgraded-type
      ((nil t) (error "~A is not a specializable array element type" type))
      (t
       #+#.(cl:if (cl:find-symbol "%VECTOR-WIDETAG-AND-N-BITS" "SB-IMPL")
                  '(and) '(or))
       (sb-impl::%vector-widetag-and-n-bits type)
       #+#.(cl:if (cl:find-symbol "%VECTOR-WIDETAG-AND-N-BITS-SHIFT" "SB-IMPL")
                  '(and) '(or))
       (multiple-value-bind (widetag shift)
           (sb-impl::%vector-widetag-and-n-bits-shift type)
         (values widetag (ash 1 shift)))))))

(declaim (inline static-alloc))
(defun static-alloc (size)
  (let ((ptr (foreign-alloc :char :count size)))
    (if (null-pointer-p ptr)
        ;; FIXME: signal proper error condition
        (error 'storage-condition)
        ptr)))

(declaim (inline %allocation-size))
(defun %allocation-size (length widetag n-bits)
  (flet ((string-widetag-p (widetag)
           (or (= widetag sb-vm:simple-base-string-widetag)
               #+sb-unicode
               (= widetag sb-vm:simple-character-string-widetag))))
    (+ (* 2 sb-vm:n-word-bytes
          (ceiling
           (* (if (string-widetag-p widetag)
                    (1+ length)  ; for the final #\Null
                    length)
              n-bits)
           (* 2 sb-vm:n-word-bits)))
       +array-header-size+)))

(declaim (inline vector-from-pointer))
(defun vector-from-pointer (pointer widetag length)
  (setf (sb-sys:sap-ref-word pointer                  0) widetag
        (sb-sys:sap-ref-word pointer sb-vm:n-word-bytes) (sb-vm:fixnumize length))
  (sb-kernel:%make-lisp-obj (logior (pointer-address pointer)
                                    sb-vm:other-pointer-lowtag)))

(declaim (inline %%allocate-static-vector))
(defun %%allocate-static-vector (allocation-size widetag length)
  (vector-from-pointer (static-alloc allocation-size)
                       widetag
                       length))

(defun %allocate-static-vector (length element-type)
  (multiple-value-bind (widetag n-bits)
      (vector-widetag-and-n-bits element-type)
    (let ((allocation-size
           (%allocation-size length widetag n-bits)))
      (%%allocate-static-vector allocation-size widetag length))))

(define-compiler-macro %allocate-static-vector
    (&whole form length element-type &environment env)
  (cond
    ((constantp element-type env)
     (let ((element-type (eval-constant element-type env)))
       (multiple-value-bind (widetag n-bits)
           (vector-widetag-and-n-bits element-type)
         (if (constantp length env)
             (let ((allocation-size (%allocation-size length widetag n-bits)))
               `(the* (simple-array ,element-type (,length))
                      (%%allocate-static-vector ,allocation-size ,widetag ,length)))
             (once-only (length)
               (let ((allocation-size `(%allocation-size ,length ,widetag ,n-bits)))
                 `(the* (simple-array ,element-type (*))
                        (%%allocate-static-vector ,allocation-size ,widetag ,length))))))))
    (t form)))

(declaim (inline static-vector-address))
(defun static-vector-address (vector)
  "Return a foreign pointer to VECTOR(including its header).
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (logandc2 (sb-kernel:get-lisp-obj-address vector)
            sb-vm:lowtag-mask))

(declaim (inline static-vector-pointer))
(defun static-vector-pointer (vector &key (offset 0))
  "Return a foreign pointer to the beginning of VECTOR + OFFSET octets.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (check-type offset unsigned-byte)
  (make-pointer (+ (static-vector-address vector)
                   +array-header-size+
                   offset)))

(declaim (inline free-static-vector))
(defun free-static-vector (vector)
  "Free VECTOR, which must be a vector created by MAKE-STATIC-VECTOR."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (foreign-free (make-pointer (static-vector-address vector)))
  (values))

(defmacro with-static-vector ((var length &rest args
                               &key (element-type '(unsigned-byte 8))
                                 initial-contents initial-element)
                              &body body)
  "Bind PTR-VAR to a static vector of length LENGTH and execute BODY
within its dynamic extent. The vector is freed upon exit."
  (declare (ignore element-type initial-contents initial-element))
  `(sb-sys:without-interrupts
     (let ((,var (make-static-vector ,length ,@args)))
       (unwind-protect
            (sb-sys:with-local-interrupts ,@body)
         (when ,var (free-static-vector ,var))))))
