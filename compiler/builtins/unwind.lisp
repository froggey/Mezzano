;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with binding, unbinding and unwinding.
;;;; Everything to do with the special stack.

(in-package :sys.c)

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

;;; TODO: Check for constants here.
(defbuiltin sys.int::%%bind (symbol value) (t nil)
  ;; Don't kill here, going to reload & kill later in the function.
  (load-in-reg :r9 symbol)
  (smash-r8)
  (let ((has-tls-slot (gensym)))
    ;; Ensure there is a TLS slot.
    (emit `(sys.lap-x86:mov32 :eax ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:shr32 :eax ,+tls-offset-shift+)
          `(sys.lap-x86:and32 :eax #xFFFF)
          `(sys.lap-x86:jnz ,has-tls-slot))
    ;; Nope, allocate a new one.
    (emit `(sys.lap-x86:mov64 :r8 :r9))
    (call-support-function 'sys.int::%allocate-tls-slot 1)
    (load-in-reg :r9 symbol t)
    (emit `(sys.lap-x86:mov64 :rax :r8)
          `(sys.lap-x86:shr32 :eax ,sys.int::+n-fixnum-bits+)
          has-tls-slot
          ;; Save the old value on the binding stack.
          ;; Read the old symbol value.
          `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 :r10 ((:rax 8) ,+tls-base-offset+)))
    (push-special-stack :r9 :r10))
  ;; Store new value.
  (load-in-r8 value t)
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 ((:rax 8) ,+tls-base-offset+) :r8))
  ''nil)

;;; TODO: Check for constants here.
(defbuiltin sys.int::%%push-special-stack (a b) (t nil)
  (load-in-reg :r9 b t)
  (load-in-reg :r8 a t)
  (push-special-stack :r8 :r9)
  ''nil)

(defbuiltin sys.int::%%unbind () (t nil)
  ;; Top entry in the binding stack is a special variable binding.
  ;; It's a symbol and the old value.
  ;; Pop the stack & restore the old value.
  (smash-r8)
  (emit `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 :rbx (,+binding-stack-gs-offset+))
        `(sys.lap-x86:mov64 :r8 ,(object-ea :rbx :slot 1))
        `(sys.lap-x86:mov64 :r9 ,(object-ea :rbx :slot 2))
        `(sys.lap-x86:mov32 :edx ,(object-ea :r8 :slot -1))
        `(sys.lap-x86:shr32 :edx ,+tls-offset-shift+)
        `(sys.lap-x86:and32 :edx #xFFFF)
        `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 ((:rdx 8) ,+tls-base-offset+) :r9)
        `(sys.lap-x86:mov64 :rbx ,(object-ea :rbx :slot 0))
        `(sys.lap-x86:gs)
        `(sys.lap-x86:mov64 (,+binding-stack-gs-offset+) :rbx))
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
