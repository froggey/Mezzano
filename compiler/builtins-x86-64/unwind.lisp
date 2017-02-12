;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with binding, unbinding and unwinding.
;;;; Everything to do with the special stack.

(in-package :mezzano.compiler.codegen.x86-64)

;;; Touching the special stack directly.

(defbuiltin sys.int::%%special-stack-pointer () ()
  (smash-r8)
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 :r8 (,+binding-stack-gs-offset+)))
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::%%special-stack-pointer) (value) ()
  (load-in-r8 value t)
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 (,+binding-stack-gs-offset+) :r8))
  value)

;;; Examining non-local exit instances.

(defun get-block/tagbody-info-binding-stack-pointer (info)
  ;; Read the saved binding stack pointer from a block/tagbody-info.
  (load-in-r8 info t)
  (smash-r8)
  (emit `(sys.lap-x86:mov64 :r8 (:r8 8)))
  (setf *r8-value* (list (gensym))))

(defbuiltin sys.int::%%block-info-binding-stack-pointer (block-info) ()
  (get-block/tagbody-info-binding-stack-pointer block-info))

(defbuiltin sys.int::%%tagbody-info-binding-stack-pointer (tagbody-info) ()
  (get-block/tagbody-info-binding-stack-pointer tagbody-info))

;;; Manipulating the special stack.

(defun push-special-stack (object value &optional (tmp :rbx) (tmp2 :r12))
  (let ((slots (allocate-control-stack-slots 4 t)))
    ;; Flush slots.
    (emit `(sys.lap-x86:mov64 (:stack ,(+ slots 3)) ,(ash 3 sys.int::+object-data-shift+))
          `(sys.lap-x86:mov64 (:stack ,(+ slots 2)) nil)
          `(sys.lap-x86:mov64 (:stack ,(+ slots 1)) nil)
          `(sys.lap-x86:mov64 (:stack ,(+ slots 0)) nil))
    ;; Generate pointer.
    (emit `(sys.lap-x86:lea64 ,tmp (:rbp ,(+ (control-stack-frame-offset (+ slots 3))
                                             sys.int::+tag-object+))))
    ;; Store bits.
    (emit `(sys.lap-x86:mov64 (:stack ,(+ slots 1)) ,object)
          `(sys.lap-x86:mov64 (:stack ,(+ slots 0)) ,value))
    ;; Store link.
    (emit `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 ,tmp2 (,+binding-stack-gs-offset+))
          `(sys.lap-x86:mov64 (:stack ,(+ slots 2)) ,tmp2))
    ;; Push.
    (emit `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 (,+binding-stack-gs-offset+) ,tmp))))

(defbuiltin sys.int::%%bind (symbol value) (t nil)
  (load-in-reg :r9 symbol t)
  (load-in-reg :r10 value t)
  (smash-r8)
  ;; Arrange for the new special stack entry to be stored in :r8,
  ;; so it can be returned.
  (push-special-stack :r9 :r10 :r8)
  ;; Also update the cache for this symbol.
  ;; Recompute the symbol hash.
  (emit `(sys.lap-x86:mov64 :rax :r9)
        `(sys.lap-x86:shr32 :eax 4)
        `(sys.lap-x86:and32 :eax ,(1- 128)))
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 ,(object-ea nil
                                        :slot 128
                                        :index '(:rax 8))
                            :r8))
  (setf *r8-value* (list (gensym))))

(defbuiltin sys.int::%%push-special-stack (a b) (t nil)
  (load-in-reg :r9 b t)
  (load-in-reg :r8 a t)
  (push-special-stack :r8 :r9)
  ''nil)

(defbuiltin sys.int::%%unbind () (t nil)
  ;; Top entry in the binding stack is a special variable binding.
  ;; It's a symbol and the current value.
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 :rbx (,+binding-stack-gs-offset+)))
  ;; Pop the stack.
  (emit `(sys.lap-x86:mov64 :r10 ,(object-ea :rbx :slot 0))
        `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 (,+binding-stack-gs-offset+) :r10))
  ;; Recompute the symbol hash.
  (emit `(sys.lap-x86:mov64 :rax ,(object-ea :rbx
                                             :slot sys.int::+symbol-value-cell-symbol+))
        `(sys.lap-x86:shr32 :eax 4)
        `(sys.lap-x86:and32 :eax ,(1- 128)))
  ;; Flush the binding cell cache for this entry.
  (let ((after-flush (gensym)))
    (emit `(sys.lap-x86:gs)
          `(sys.lap-x86:cmp64 ,(object-ea nil
                                          :slot 128
                                          :index '(:rax 8))
                              :rbx))
    (emit `(sys.lap-x86:jne ,after-flush))
    (emit `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 ,(object-ea nil
                                          :slot 128
                                          :index '(:rax 8))
                              0))
    (emit after-flush))
  ''nil)

(defbuiltin sys.int::%%disestablish-block-or-tagbody () (t nil)
  ;; Top entry in the binding stack is a block or tagbody entry.
  ;; It's a environment simple-vector & an offset.
  ;; Pop the stack & set env[offset] = NIL.
  (smash-r8)
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 :rbx (,+binding-stack-gs-offset+))
        `(sys.lap-x86:mov64 :r8 ,(object-ea :rbx :slot 1))
        `(sys.lap-x86:mov64 :rcx ,(object-ea :rbx :slot 2))
        `(sys.lap-x86:mov64 ,(object-ea :r8 :index `(:rcx ,(/ 8 (ash 1 sys.int::+n-fixnum-bits+)))) nil)
        `(sys.lap-x86:mov64 :rbx ,(object-ea :rbx :slot 0))
        `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 (,+binding-stack-gs-offset+) :rbx))
  ''nil)

(defbuiltin sys.int::%%disestablish-unwind-protect () (t nil)
  ;; Top entry in the binding stack is an unwind-protect entry.
  ;; It's a function and environment object.
  ;; Pop the stack & call the function with the environment object.
  (smash-r8)
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 :r8 (,+binding-stack-gs-offset+))
        `(sys.lap-x86:mov64 :r13 ,(object-ea :r8 :slot 1))
        `(sys.lap-x86:mov64 :rbx ,(object-ea :r8 :slot 2))
        `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot 0))
        `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 (,+binding-stack-gs-offset+) :r8)
        `(sys.lap-x86:xor32 :ecx :ecx)
        `(sys.lap-x86:call ,(object-ea :r13 :slot 0)))
  ''nil)
