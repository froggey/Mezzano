;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with binding, unbinding and unwinding.
;;;; Everything to do with the special stack.

(in-package :mezzano.compiler.codegen.arm64)

;;; Touching the special stack directly.

(defbuiltin sys.int::%%special-stack-pointer () ()
  (smash-x0)
  (emit-object-load :x0 :x28 :slot 6) ;; ### special-stack-pointer
  (setf *x0-value* (list (gensym))))

(defbuiltin (setf sys.int::%%special-stack-pointer) (value) ()
  (load-in-x0 value t)
  (emit-object-store :x0 :x28 :slot 6) ;; ### special-stack-pointer
  value)

;;; Examining non-local exit instances.

(defun get-block/tagbody-info-binding-stack-pointer (info)
  ;; Read the saved binding stack pointer from a block/tagbody-info.
  (load-in-x0 info t)
  (smash-x0)
  (emit `(lap:ldr :x0 (:x0 8)))
  (setf *x0-value* (list (gensym))))

(defbuiltin sys.int::%%block-info-binding-stack-pointer (block-info) ()
  (get-block/tagbody-info-binding-stack-pointer block-info))

(defbuiltin sys.int::%%tagbody-info-binding-stack-pointer (tagbody-info) ()
  (get-block/tagbody-info-binding-stack-pointer tagbody-info))

;;; Manipulating the special stack.

(defun push-special-stack (object value &optional (tmp :x6) (tmp2 :x4))
  (let ((slots (allocate-control-stack-slots 4 t)))
    ;; Flush slots.
    (load-literal tmp (ash 3 sys.int::+object-data-shift+))
    (emit-stack-store tmp (+ slots 3))
    (emit-stack-store :x26 (+ slots 2))
    (emit-stack-store :x26 (+ slots 1))
    (emit-stack-store :x26 (+ slots 0))
    ;; Generate pointer.
    (load-literal tmp (+ (control-stack-frame-offset (+ slots 3))
                         sys.int::+tag-object+))
    (emit `(lap:add ,tmp :x29 ,tmp))
    ;; Store bits.
    (emit-stack-store object (+ slots 1))
    (emit-stack-store value (+ slots 0))
    ;; Store link.
    (emit-object-load tmp2 :x28 :slot 6) ;; ### special-stack-pointer
    (emit-stack-store tmp2 (+ slots 2))
    ;; Push.
    (emit-object-store tmp :x28 :slot 6))) ;; ### special-stack-pointer

(defbuiltin sys.int::%%bind (symbol value) (t nil)
  (load-in-reg :x1 symbol t)
  (load-in-reg :x2 value t)
  (smash-x0)
  ;; Arrange for the new special stack entry to be stored in :r8,
  ;; so it can be returned.
  (push-special-stack :x1 :x2 :x0)
  ;; Also update the cache for this symbol.
  ;; Recompute the symbol hash.
  (emit `(lap:add :x9 :xzr :x1 :lsr 1)
        `(lap:and :x9 :x9 ,(ash (1- 128) 3)))
  (emit `(lap:add :x9 :x9 ,(object-slot-displacement 128))
        `(lap:str :x0 (:x28 :x9)))
  (setf *x0-value* (list (gensym))))

(defbuiltin sys.int::%%push-special-stack (a b) (t nil)
  (load-in-reg :x1 b t)
  (load-in-reg :x0 a t)
  (push-special-stack :x0 :x1)
  ''nil)

(defbuiltin sys.int::%%unbind () (t nil)
  ;; Top entry in the binding stack is a special variable binding.
  ;; It's a symbol and the current value.
  (emit-object-load :x6 :x28 :slot 6) ;; ### special-stack-pointer
  ;; Pop the stack.
  (emit-object-load :x2 :x6 :slot 0)
  (emit-object-store :x2 :x28 :slot 6) ;; ### special-stack-pointer
  ;; Recompute the symbol hash.
  (emit-object-load :x9 :x6 :slot sys.int::+symbol-value-cell-symbol+)
  (emit `(lap:add :x9 :xzr :x9 :lsr 1)
        `(lap:and :x9 :x9 ,(ash (1- 128) 3))
        `(lap:add :x9 :x9 ,(object-slot-displacement 128)))
  ;; Flush the binding cell cache for this entry.
  (let ((after-flush (gensym)))
    (emit `(lap:ldr :x10 (:x28 :x9))
          `(lap:subs :xzr :x10 :x6)
          `(lap:b.ne ,after-flush))
    (emit `(lap:str :xzr (:x28 :x9)))
    (emit after-flush))
  ''nil)

(defbuiltin sys.int::%%disestablish-block-or-tagbody () (t nil)
  ;; Top entry in the binding stack is a block or tagbody entry.
  ;; It's a environment simple-vector & an offset.
  ;; Pop the stack & set env[offset] = NIL.
  (smash-x0)
  (emit-object-load :x6 :x28 :slot 6) ; ### special-stack-pointer
  (emit-object-load :x0 :x6 :slot 1)
  (emit-object-load :x5 :x6 :slot 2)
  (emit `(lap:add :x5 :xzr :x5 :lsl 2)
        `(lap:sub :x5 :x5 ,(- (object-slot-displacement 0)))
        `(lap:str :x26 (:x0 :x5)))
  (emit-object-load :x6 :x6 :slot 0)
  (emit-object-store :x6 :x28 :slot 6) ; ### special-stack-pointer
  ''nil)

(defbuiltin sys.int::%%disestablish-unwind-protect () (t nil)
  ;; Top entry in the binding stack is an unwind-protect entry.
  ;; It's a function and environment object.
  ;; Pop the stack & call the function with the environment object.
  (smash-x0)
  (emit-object-load :x0 :x28 :slot 6) ; ### special-stack-pointer
  (emit-object-load :x7 :x0 :slot 1)
  (emit-object-load :x6 :x0 :slot 2)
  (emit-object-load :x0 :x0 :slot 0)
  (emit-object-store :x0 :x28 :slot 6) ; ### special-stack-pointer
  (load-constant :x5 0)
  (emit-object-load :x9 :x7 :slot sys.int::+function-entry-point+)
  (emit `(lap:blr :x9))
  ''nil)
