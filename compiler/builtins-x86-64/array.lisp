;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with arrays.
;;;; Builtins for accessing array elements are in memory.lisp.

(in-package :mezzano.compiler.codegen.x86-64)

(defbuiltin sys.int::%simple-1d-array-p (object) ()
  (let ((false-out (gensym))
        (out (gensym)))
    (load-in-reg :r8 object t)
    (emit `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,false-out)
          `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash sys.int::+last-simple-1d-array-object-tag+
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:jnbe ,false-out)
          `(sys.lap-x86:mov64 :r8 t)
          `(sys.lap-x86:jmp ,out)
          false-out
          `(sys.lap-x86:mov64 :r8 nil)
          out)
    (setf *r8-value* (list (gensym)))))

;;; Simple-vectors.

(defbuiltin simple-vector-p (object) ()
  (let ((out (gensym)))
    (load-in-r8 object t)
    (emit `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,out)
          `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:test8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                        sys.int::+object-type-shift+))
          ;; Subtle. OUT can be reached through either the tag check
          ;; or through the array type check. Both checks clear ZF when
          ;; they fail.
          out)
    (predicate-result :z)))

;;; Character arrays.

(defbuiltin sys.int::character-array-p (object) ()
  (let ((out (gensym)))
    (load-in-reg :r8 object t)
    (smash-r8)
    ;; Check tag.
    (emit `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,out)
          `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:or8 :al ,(ash sys.int::+array-type-simple-bit+
                                      sys.int::+object-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash sys.int::+object-tag-string+
                                       sys.int::+object-type-shift+))
          ;; Subtle. OUT can be reached through either the tag check
          ;; or through the array type check. Both checks clear ZF when
          ;; they fail.
          out)
    (predicate-result :z)))

;;; Arrays in general.

(defbuiltin arrayp (object) ()
  (let ((out (gensym)))
    (load-in-reg :r9 object t)
    (smash-r8)
    (emit `(sys.lap-x86:mov64 :r8 nil)
          ;; Check tag.
          `(sys.lap-x86:mov8 :al :r9l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,out)
          ;; Check object tag.
          `(sys.lap-x86:mov8 :al ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:cmp8 :al
                             ;; Complex arrays include simple arrays.
                             ,(ash sys.int::+last-complex-array-object-tag+
                                   sys.int::+object-type-shift+))
          `(sys.lap-x86:cmov64be :r8 (:constant t))
          out)
    (setf *r8-value* (list (gensym)))))

;;; Complex arrays.

(defbuiltin sys.int::complex-array-p (object) ()
  (let ((out (gensym)))
    (load-in-reg :r9 object t)
    (smash-r8)
    (emit `(sys.lap-x86:mov64 :r8 nil)
          ;; Check tag.
          `(sys.lap-x86:mov8 :al :r9l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,out)
          ;; Check object tag.
          `(sys.lap-x86:mov8 :al ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:sub8 :al ,(ash sys.int::+first-complex-array-object-tag+
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash (- sys.int::+last-complex-array-object-tag+
                                          sys.int::+first-function-object-tag+)
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:cmov64be :r8 (:constant t))
          out)
    (setf *r8-value* (list (gensym)))))
