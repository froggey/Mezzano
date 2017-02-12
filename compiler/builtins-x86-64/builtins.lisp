;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Functions which are built in to the compiler and have custom code generators.

(in-package :mezzano.compiler.codegen.x86-64)

(defparameter *builtins* (make-hash-table :test #'equal))

(defun small-fixnum-p (integer)
  (typep integer `(signed-byte ,(- 32 sys.int::+fixnum-tag-mask+))))

(defmacro defbuiltin (name lambda-list (&optional (emit-function t) suppress-binding-stack-check) &body body)
  `(progn (setf (gethash ',name *builtins*)
                (list ',lambda-list
                      (lambda ,lambda-list
                        (declare (sys.int::lambda-name ,name)
                                 ,@(when suppress-binding-stack-check
                                     '((sys.int::suppress-ssp-checking))))
                        (block ,(if (consp name)
                                    (second name)
                                    name)
                          ,@body))
                      ',emit-function
                      ',name
                      ',suppress-binding-stack-check))
          ',name))

(defun emit-tag-check (reg tag type)
  "Emit a value tag type check. Smashes AL."
  (let ((type-error-label (gensym)))
    (emit-trailer (type-error-label)
      (raise-type-error reg type))
    (emit `(sys.lap-x86:mov8 :al ,(sys.lap-x86::convert-width reg 8))
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,tag)
          `(sys.lap-x86:jne ,type-error-label))))

(defun emit-object-type-check (reg object-tag type original)
  "Emit an object tag type check. Smashes AL."
  (unless (and (quoted-constant-p original)
             (typep (second original) type))
    (let ((type-error-label (gensym)))
      (emit-trailer (type-error-label)
        (raise-type-error reg type))
      (emit `(sys.lap-x86:mov8 :al ,(sys.lap-x86::convert-width reg 8))
            `(sys.lap-x86:and8 :al #b1111)
            `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
            `(sys.lap-x86:jne ,type-error-label)
            `(sys.lap-x86:mov8 :al (,reg ,(- sys.int::+tag-object+)))
            `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                       sys.int::+object-type-shift+))
            `(sys.lap-x86:cmp8 :al ,(ash object-tag sys.int::+object-type-shift+))
            `(sys.lap-x86:jne ,type-error-label)))))

(defmacro define-tag-type-predicate (name tag)
  `(defbuiltin ,name (object) ()
     (load-in-reg :r8 object t)
     (emit `(sys.lap-x86:mov8 :al :r8l)
           `(sys.lap-x86:and8 :al #b1111)
           `(sys.lap-x86:cmp8 :al ,,tag))
     (predicate-result :e)))

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

(defun quoted-constant-p (tag)
  (and (consp tag)
       (consp (cdr tag))
       (null (cddr tag))
       (eql (first tag) 'quote)))

(defun constant-type-p (tag type)
  (and (quoted-constant-p tag)
       (typep (second tag) type)))

(defun box-unsigned-byte-64-rax ()
  "Box the unboxed (unsigned-byte 64) in RAX into R8."
  (let ((overflow-label (gensym))
        (resume (gensym)))
    (emit-trailer (overflow-label)
      (emit
       ;; Undo the shift.
       `(sys.lap-x86:rcr64 :rax 1)
       ;; Prod the sign flag.
       `(sys.lap-x86:test64 :rax :rax)
       ;; Build bignum.
       `(sys.lap-x86:mov64 :r13 (:function sys.int::%%make-bignum-64-rax))
       ;; Result needs a 128-bit bignum when the high bit is set.
       `(sys.lap-x86:cmov64s :r13 (:function sys.int::%%make-bignum-128-rdx-rax))
       `(sys.lap-x86:xor32 :edx :edx)
       `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
       `(sys.lap-x86:jmp ,resume)))
    (smash-r8)
    (emit
     ;; Convert to fixnum & check for unsigned overflow.
     ;; Assumes fixnum size of 1!
     `(sys.lap-x86:shl64 :rax 1)
     `(sys.lap-x86:jc ,overflow-label)
     `(sys.lap-x86:js ,overflow-label)
     `(sys.lap-x86:mov64 :r8 :rax)
     resume)))

(defun call-support-function (name n-args &optional (single-value t))
  (declare (ignore single-value))
  (emit `(sys.lap-x86:mov64 :r13 (:function ,name))
        `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw n-args))
        `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))))

(defun emit-type-error (register type)
  (let ((label (gensym "type-error")))
    (emit-trailer (label)
      (raise-type-error register type))
    label))

(defun unpack-ub32-fixnum-into-register (value target scratch)
  (let ((reg64 (sys.lap-x86::convert-width target 64))
        (reg32 (sys.lap-x86::convert-width target 32)))
    (cond ((and (quoted-constant-p value)
                (typep (second value) '(unsigned-byte 32)))
           (emit `(sys.lap-x86:mov32 ,reg32 ,(second value))))
          (t (let ((type-error-label (emit-type-error scratch '(unsigned-byte 32))))
               (load-in-r8 value t)
               (smash-r8)
               (emit `(sys.lap-x86:test64 ,scratch ,sys.int::+fixnum-tag-mask+)
                     `(sys.lap-x86:jnz ,type-error-label)
                     `(sys.lap-x86:mov64 ,reg64 ,scratch)
                     `(sys.lap-x86:sar64 ,reg64 ,(+ 32 sys.int::+n-fixnum-bits+))
                     `(sys.lap-x86:jnz ,type-error-label)
                     `(sys.lap-x86:mov64 ,reg64 ,scratch)
                     `(sys.lap-x86:shr64 ,reg64 ,sys.int::+n-fixnum-bits+)))))))
