;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Mezzano implementation
;;;

(in-package :static-vectors)

(defun fill-foreign-memory (pointer length value)
  "Fill LENGTH octets in foreign memory area POINTER with VALUE."
  )

(defun replace-foreign-memory (dst-ptr src-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  )

;; (defconstant +array-header-size+
;;   (* sb-vm:vector-data-offset sb-vm:n-word-bytes))

(defun vector-widetag-and-n-bits (type)
  )

(defun static-alloc (size)
  )

(defun %allocation-size (length widetag n-bits)
  )

(defun vector-from-pointer (pointer widetag length)
  )

(defun %%allocate-static-vector (allocation-size widetag length)
  )

(defun %allocate-static-vector (length element-type)
  )

(define-compiler-macro %allocate-static-vector
    (&whole form length element-type &environment env)
  )

(defun static-vector-address (vector)
  "Return a foreign pointer to VECTOR(including its header).
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  )

(defun static-vector-pointer (vector &key (offset 0))
  "Return a foreign pointer to the beginning of VECTOR + OFFSET octets.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  )

(defun free-static-vector (vector)
  "Free VECTOR, which must be a vector created by MAKE-STATIC-VECTOR."
  )

(defmacro with-static-vector ((var length &rest args
                                   &key (element-type '(unsigned-byte 8))
                                   initial-contents initial-element)
                              &body body)
  "Bind PTR-VAR to a static vector of length LENGTH and execute BODY
within its dynamic extent. The vector is freed upon exit."
  `(let ((,var (make-static-vector ,length ,@args)))
     (unwind-protect
          (progn ,@body)
       (when ,var (free-static-vector ,var)))))
