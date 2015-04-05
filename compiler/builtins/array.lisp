;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with arrays.
;;;; Builtins for accessing array elements are in memory.lisp.

(in-package :sys.c)

(defbuiltin sys.int::%simple-1d-array-p (object) ()
  (let ((false-out (gensym))
        (out (gensym)))
    (load-in-reg :r8 object t)
    (emit `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,false-out)
          `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash sys.int::+last-simple-1d-array-object-tag+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:jnbe ,false-out)
          `(sys.lap-x86:mov64 :r8 t)
          `(sys.lap-x86:jmp ,out)
          false-out
          `(sys.lap-x86:mov64 :r8 nil)
          out)
    (setf *r8-value* (list (gensym)))))

;;; Examining the object header for simple arrays.

(defbuiltin sys.int::%simple-1d-array-length (array) ()
  (let ((type-error-label (gensym)))
    (emit-trailer (type-error-label)
		  (raise-type-error :r8 '(simple-array * (*))))
    (load-in-r8 array t)
    (smash-r8)
    (emit `(sys.lap-x86:mov8 :al :r8l)
	  `(sys.lap-x86:and8 :al #b1111)
	  `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
	  `(sys.lap-x86:jne ,type-error-label)
	  ;; Ensure that it is a simple-array, not a struct or bignum or similar.
	  `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
	  `(sys.lap-x86:cmp8 :al ,(ash sys.int::+last-simple-1d-array-object-tag+
                                       sys.int::+array-type-shift+))
	  `(sys.lap-x86:jnbe ,type-error-label)
	  ;; Convert length to fixnum.
	  `(sys.lap-x86:shr64 :rax ,sys.int::+array-length-shift+)
          `(sys.lap-x86:shl64 :rax ,sys.int::+n-fixnum-bits+)
	  `(sys.lap-x86:mov64 :r8 :rax))
    (setf *r8-value* (list (gensym)))))

(defbuiltin sys.int::%simple-array-type (array) ()
  (let ((type-error-label (gensym)))
    (emit-trailer (type-error-label)
		  (raise-type-error :r8 '(simple-array * (*))))
    (load-in-r8 array t)
    (smash-r8)
    (emit `(sys.lap-x86:mov8 :al :r8l)
	  `(sys.lap-x86:and8 :al #b1111)
	  `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
	  `(sys.lap-x86:jne ,type-error-label)
	  ;; Ensure that it is a simple-array, not a struct or bignum or similar.
	  `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
	  `(sys.lap-x86:cmp8 :al ,(ash sys.int::+last-simple-1d-array-object-tag+
                                       sys.int::+array-type-shift+))
	  `(sys.lap-x86:jnbe ,type-error-label)
	  ;; Convert tag to fixnum. Low 3 bits are for the GC, always clear.
          `(sys.lap-x86:and32 :eax ,(ash (- (ash 1 sys.int::+array-type-size+) 1) sys.int::+array-type-shift+))
	  `(sys.lap-x86:shr32 :eax ,sys.int::+array-type-shift+)
          `(sys.lap-x86:shl32 :eax ,sys.int::+n-fixnum-bits+)
	  `(sys.lap-x86:mov32 :r8d :eax))
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
          `(sys.lap-x86:test8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                        sys.int::+array-type-shift+))
          ;; Subtle. OUT can be reached through either the tag check
          ;; or through the array type check. Both checks clear ZF when
          ;; they fail.
          out)
    (predicate-result :z)))

(defbuiltin svref (simple-vector index) ()
  (let ((type-error-label (gensym))
        (bounds-error-label (gensym)))
    (emit-trailer (type-error-label)
      (raise-type-error :r8 'simple-vector))
    (emit-trailer (bounds-error-label)
      (emit `(sys.lap-x86:mov64 :r13 (:function sys.int::raise-bounds-error))
            `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 2))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:ud2)))
    (load-in-reg :r8 simple-vector t)
    (smash-r8)
    (load-in-reg :r9 index t)
    (fixnum-check :r9)
    (emit `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error-label)
          ;; Load header word.
          `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot -1))
          ;; Check array type.
          `(sys.lap-x86:test8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                        sys.int::+array-type-shift+))
          `(sys.lap-x86:jnz ,type-error-label)
          ;; Check bounds.
          `(sys.lap-x86:mov64 :rcx :r9)
          `(sys.lap-x86:shr64 :rcx ,sys.int::+n-fixnum-bits+)
          `(sys.lap-x86:shr64 :rax ,sys.int::+array-length-shift+)
          `(sys.lap-x86:cmp64 :rcx :rax)
          `(sys.lap-x86:jae ,bounds-error-label)
          ;; Load!
          `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :index '(:rcx 8))))
    (setf *r8-value* (list (gensym)))))

(defbuiltin sys.int::%svref (simple-vector index) ()
  "Fast SVREF. No type check or bounds check."
  (load-in-r8 simple-vector t)
  (smash-r8)
  (cond ((quoted-constant-p index)
         (emit `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot (second index)))))
        (t (load-in-reg :r9 index t)
           (emit `(sys.lap-x86:mov64 :rax :r9)
                 `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+))
           (emit `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :index '(:rax 8))))))
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf svref) (value simple-vector index) ()
  (let ((type-error-label (gensym))
        (bounds-error-label (gensym)))
    (emit-trailer (type-error-label)
      (raise-type-error :r9 'simple-vector))
    (emit-trailer (bounds-error-label)
      (emit `(sys.lap-x86:mov64 :r8 :r9)
            `(sys.lap-x86:mov64 :r9 :r10)
            `(sys.lap-x86:mov64 :r13 (:function sys.int::raise-bounds-error))
            `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 2))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:ud2)))
    (load-in-reg :r8 value t)
    (load-in-reg :r9 simple-vector t)
    (load-in-reg :r10 index t)
    (fixnum-check :r10)
    (emit `(sys.lap-x86:mov8 :al :r9l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error-label)
          ;; Load header word.
          `(sys.lap-x86:mov64 :rax ,(object-ea :r9 :slot -1))
          ;; Check array type.
          `(sys.lap-x86:test8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                        sys.int::+array-type-shift+))
          `(sys.lap-x86:jnz ,type-error-label)
          ;; Check bounds.
          `(sys.lap-x86:mov64 :rcx :r10)
          `(sys.lap-x86:shr64 :rcx ,sys.int::+n-fixnum-bits+)
          `(sys.lap-x86:shr64 :rax ,sys.int::+array-length-shift+)
          `(sys.lap-x86:cmp64 :rcx :rax)
          `(sys.lap-x86:jae ,bounds-error-label)
          ;; Store!
          `(sys.lap-x86:mov64 ,(object-ea :r9 :index '(:rcx 8)) :r8))
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%svref) (value simple-vector index) ()
  "Fast SVREF. No type check or bounds check."
  (load-in-reg :r8 value t)
  (load-in-reg :r9 simple-vector t)
  (cond ((quoted-constant-p index)
         (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :slot (second index)) :r8)))
        (t (load-in-reg :r10 index t)
           (emit `(sys.lap-x86:mov64 :rax :r10)
                 `(sys.lap-x86:sar64 :rax ,sys.int::+n-fixnum-bits+))
           (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :index '(:rax 8)) :r8))))
  (setf *r8-value* (list (gensym))))

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
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:or8 :al ,(ash sys.int::+array-type-simple-bit+
                                      sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash sys.int::+object-tag-string+
                                       sys.int::+array-type-shift+))
          ;; Subtle. OUT can be reached through either the tag check
          ;; or through the array type check. Both checks clear ZF when
          ;; they fail.
          out)
    (predicate-result :z)))

(define-array-like-predicate sys.int::simple-character-array-p sys.int::+object-tag-simple-string+)

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
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al
                             ;; Complex arrays include simple arrays.
                             ,(ash sys.int::+last-complex-array-object-tag+
                                   sys.int::+array-type-shift+))
          `(sys.lap-x86:cmov64be :r8 (:constant t))
          out)
    (setf *r8-value* (list (gensym)))))

;;; Complex arrays.

(defbuiltin sys.int::%complex-array-storage (complex-array) ()
  (let ((type-error (emit-type-error :r8 '(and array
                                           (not (simple-array * (*)))))))
    (load-in-reg :r8 complex-array t)
    (smash-r8)
    (emit ;; Check tag.
          `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error)
          ;; Check object tag.
          `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:sub8 :al ,(ash sys.int::+first-complex-array-object-tag+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash (- sys.int::+last-complex-array-object-tag+
                                          sys.int::+first-complex-array-object-tag+)
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:ja ,type-error)
          `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot 0)))
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%complex-array-storage) (value complex-array) ()
  (let ((type-error (emit-type-error :r9 '(and array
                                           (not (simple-array * (*)))))))
    (load-in-reg :r8 value t)
    (load-in-reg :r9 complex-array t)
    (emit ;; Check tag.
          `(sys.lap-x86:mov8 :al :r9l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error)
          ;; Check object tag.
          `(sys.lap-x86:mov8 :al ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:sub8 :al ,(ash sys.int::+first-complex-array-object-tag+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash (- sys.int::+last-complex-array-object-tag+
                                          sys.int::+first-complex-array-object-tag+)
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:ja ,type-error)
          `(sys.lap-x86:mov64 ,(object-ea :r9 :slot 0) :r8))
    value))

(defbuiltin sys.int::%complex-array-fill-pointer (complex-array) ()
  (let ((type-error (emit-type-error :r8 '(and array
                                           (not (simple-array * (*)))))))
    (load-in-reg :r8 complex-array t)
    (smash-r8)
    (emit ;; Check tag.
          `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error)
          ;; Check object tag.
          `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:sub8 :al ,(ash sys.int::+first-complex-array-object-tag+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash (- sys.int::+last-complex-array-object-tag+
                                          sys.int::+first-complex-array-object-tag+)
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:ja ,type-error)
          `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot 1)))
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%complex-array-fill-pointer) (value complex-array) ()
  (let ((type-error (emit-type-error :r9 '(and array
                                           (not (simple-array * (*)))))))
    (load-in-reg :r8 value t)
    (load-in-reg :r9 complex-array t)
    (emit ;; Check tag.
          `(sys.lap-x86:mov8 :al :r9l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error)
          ;; Check object tag.
          `(sys.lap-x86:mov8 :al ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:sub8 :al ,(ash sys.int::+first-complex-array-object-tag+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash (- sys.int::+last-complex-array-object-tag+
                                          sys.int::+first-complex-array-object-tag+)
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:ja ,type-error)
          `(sys.lap-x86:mov64 ,(object-ea :r9 :slot 1) :r8))
    value))

(defbuiltin sys.int::%complex-array-info (complex-array) ()
  (let ((type-error (emit-type-error :r8 '(and array
                                           (not (simple-array * (*)))))))
    (load-in-reg :r8 complex-array t)
    (smash-r8)
    (emit ;; Check tag.
          `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error)
          ;; Check object tag.
          `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:sub8 :al ,(ash sys.int::+first-complex-array-object-tag+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash (- sys.int::+last-complex-array-object-tag+
                                          sys.int::+first-complex-array-object-tag+)
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:ja ,type-error)
          `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot 2)))
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::%complex-array-info) (value complex-array) ()
  (let ((type-error (emit-type-error :r9 '(and array
                                           (not (simple-array * (*)))))))
    (load-in-reg :r8 value t)
    (load-in-reg :r9 complex-array t)
    (emit ;; Check tag.
          `(sys.lap-x86:mov8 :al :r9l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error)
          ;; Check object tag.
          `(sys.lap-x86:mov8 :al ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:sub8 :al ,(ash sys.int::+first-complex-array-object-tag+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash (- sys.int::+last-complex-array-object-tag+
                                          sys.int::+first-complex-array-object-tag+)
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:ja ,type-error)
          `(sys.lap-x86:mov64 ,(object-ea :r9 :slot 2) :r8))
    value))

;; FIXME: Type & limit checking here.
(defbuiltin sys.int::%complex-array-dimension (array axis) ()
  (load-in-reg :r9 axis t)
  (load-in-reg :r8 array t)
  (smash-r8)
  (emit `(sys.lap-x86:mov64 :r8 ,(object-ea :r8
                                            :slot 3
                                            :index `(:r9 ,(/ 8 (ash 1 sys.int::+n-fixnum-bits+))))))
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::%complex-array-dimension) (value array axis) ()
  (load-in-reg :r8 value t)
  (load-in-reg :r9 axis t)
  (load-in-reg :r10 array t)
  (emit `(sys.lap-x86:mov64 ,(object-ea :r10
                                        :slot 3
                                        :index `(:r9 ,(/ 8 (ash 1 sys.int::+n-fixnum-bits+))))
                            :r8))
  value)
