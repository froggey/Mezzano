;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Functions which are built in to the compiler and have custom code generators.

(in-package :mezzano.compiler.codegen.arm64)

(defparameter *builtins* (make-hash-table :test #'equal))

(defmacro defbuiltin (name lambda-list (&optional (emit-function t) suppress-binding-stack-check) &body body)
  `(progn (setf (gethash ',name *builtins*)
                (list ',lambda-list
                      (lambda ,lambda-list
                        (declare (sys.int::lambda-name ,name)
                                 ,@(when suppress-binding-stack-check
                                    '((sys.int::suppress-ssp-checking))))
                        ,@body)
                      ',emit-function
                      ',name
                      ',suppress-binding-stack-check))
          ',name))

;; Produce an alist of symbol names and their associated functions.
(defun generate-builtin-functions ()
  (let ((functions '()))
    (maphash (lambda (symbol info)
               (declare (ignore symbol))
               (when (third info)
                 (push (list (fourth info)
                             `(lambda ,(first info)
                                (declare (sys.int::lambda-name ,(fourth info)))
                                (funcall #',(fourth info) ,@(first info))))
                       functions)))
             *builtins*)
    functions))

(defun match-builtin (symbol arg-count)
  (let ((x (gethash symbol *builtins*)))
    (when (and x (eql (length (first x)) arg-count))
      (second x))))

(defun box-unsigned-byte-64-x10 ()
  "Box the unboxed (unsigned-byte 64) in X10 into R0."
  (let ((overflow-label (gensym))
        (resume (gensym)))
    (emit-trailer (overflow-label)
      (emit
       ;; Prod the sign flag.
       `(lap:ands :xzr :x10 :x10)
       ;; Build bignum.
       `(lap:ldr :x7 (:function sys.int::%%make-bignum-64-x10))
       ;; Result needs a 128-bit bignum when the high bit is set.
       `(lap:ldr :x6 (:function sys.int::%%make-bignum-128-x10-x11))
       `(lap:csel.mi :x7 :x6 :x7)
       `(lap:orr :x11 :xzr :xzr))
      (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
      (emit `(lap:blr :x9)
            `(lap:b ,resume)))
    (smash-x0)
    (emit
     ;; Convert to fixnum & check for unsigned overflow.
     ;; Assumes fixnum size of 1!
     `(lap:adds :x0 :x10 :x10)
     `(lap:b.cs ,overflow-label)
     `(lap:b.mi ,overflow-label)
     resume)))

(defun quoted-constant-p (tag)
  (and (consp tag)
       (consp (cdr tag))
       (null (cddr tag))
       (eql (first tag) 'quote)))

(defun constant-type-p (tag type)
  (and (quoted-constant-p tag)
       (typep (second tag) type)))

(defun call-support-function (name n-args)
  (emit `(lap:ldr :x7 (:function ,name)))
  (load-constant :x5 n-args)
  (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
  (emit `(lap:blr :x9)))

(defun raise-type-error (reg typespec)
  (unless (eql reg :x0)
    (emit `(lap:orr :x0 ,reg :xzr)))
  (load-constant :x1 typespec)
  (call-support-function 'sys.int::raise-type-error 2)
  (emit `(lap:hlt 321))
  nil)

(defun fixnum-check (reg &optional (typespec 'fixnum))
  (let ((type-error-label (gensym)))
    (emit-trailer (type-error-label)
      (raise-type-error reg typespec))
    (emit `(lap:ands :xzr ,reg ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,type-error-label))))
