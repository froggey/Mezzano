;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for examining objects.

(in-package :sys.c)

;;; Examining the object header.

(defbuiltin sys.int::%object-tag (thing) ()
  (load-in-r8 thing t)
  (smash-r8)
  (emit `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
        `(sys.lap-x86:and32 :eax #xFF)
        `(sys.lap-x86:shr32 :eax ,sys.int::+array-type-shift+)
        `(sys.lap-x86:shl32 :eax ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:mov32 :r8d :eax))
  (setf *r8-value* (list (gensym))))

(defbuiltin sys.int::%object-header-data (value) ()
  (load-in-reg :r8 value t)
  (smash-r8)
  (emit `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot -1))
        `(sys.lap-x86:and64 :r8 ,(lognot (1- (ash 1 sys.int::+array-length-shift+))))
        `(sys.lap-x86:shr64 :r8 ,(- sys.int::+array-length-shift+
                                    sys.int::+n-fixnum-bits+)))
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::%object-header-data) (value object) ()
  (load-in-reg :rax value t)
  (load-in-reg :r8 object t)
  (emit `(sys.lap-x86:shl64 :rax ,(- sys.int::+array-length-shift+
                                     sys.int::+n-fixnum-bits+))
        ;; low 8 bits of the header only.
        `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
        `(sys.lap-x86:mov64 ,(object-ea :r8 :slot -1) :rax))
  value)

;;; Structures.

(define-array-like-predicate system.internals::structure-object-p
    sys.int::+object-tag-structure-object+)

(defbuiltin system.internals::%struct-slot (object slot) ()
  (let ((type-error-label (gensym))
        (bounds-error-label (gensym)))
    (emit-trailer (type-error-label)
      (raise-type-error :r8 'structure-object))
    (emit-trailer (bounds-error-label)
      (emit `(sys.lap-x86:mov64 :r13 (:function sys.int::raise-bounds-error))
            `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 2))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:ud2)))
    (load-in-reg :r9 slot t)
    (fixnum-check :r9)
    (load-in-reg :r8 object t)
    (smash-r8)
    (emit `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error-label)
          `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash sys.int::+object-tag-structure-object+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:jne ,type-error-label)
          ;; Convert size and slot number to integers.
          `(sys.lap-x86:shr64 :rax ,sys.int::+array-length-shift+)
          `(sys.lap-x86:mov64 :rcx :r9)
          `(sys.lap-x86:shr64 :rcx ,sys.int::+n-fixnum-bits+)
          ;; Check bounds.
          `(sys.lap-x86:cmp64 :rcx :rax)
          `(sys.lap-x86:jae ,bounds-error-label)
          ;; Load.
          `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :index '(:rcx 8))))
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf system.internals::%struct-slot) (value object slot) ()
  (let ((type-error-label (gensym))
        (bounds-error-label (gensym)))
    (emit-trailer (type-error-label)
      (raise-type-error :r9 'structure-object))
    (emit-trailer (bounds-error-label)
      (emit `(sys.lap-x86:mov64 :r8 :r9)
            `(sys.lap-x86:mov64 :r9 :r10)
            `(sys.lap-x86:mov64 :r13 (:function sys.int::raise-bounds-error))
            `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 2))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:ud2)))
    (load-in-reg :r10 slot t)
    (fixnum-check :r10)
    (load-in-reg :r9 object t)
    (load-in-reg :r8 value t)
    (emit `(sys.lap-x86:mov8 :al :r9l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error-label)
          `(sys.lap-x86:mov64 :rax ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash sys.int::+object-tag-structure-object+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:jne ,type-error-label)
          ;; Convert size and slot number to integers.
          `(sys.lap-x86:shr64 :rax ,sys.int::+array-length-shift+)
          `(sys.lap-x86:mov64 :rcx :r10)
          `(sys.lap-x86:shr64 :rcx ,sys.int::+n-fixnum-bits+)
          ;; Check bounds.
          `(sys.lap-x86:cmp64 :rcx :rax)
          `(sys.lap-x86:jae ,bounds-error-label)
          ;; Store.
          `(sys.lap-x86:mov64 ,(object-ea :r9 :index '(:rcx 8)) :r8))
    *r8-value*))

(defbuiltin sys.int::%cas-struct-slot (object slot old new) ()
  (let ((type-error-label (gensym))
        (bounds-error-label (gensym)))
    (emit-trailer (type-error-label)
      (raise-type-error :r9 'structure-object))
    (emit-trailer (bounds-error-label)
      (emit `(sys.lap-x86:mov64 :r8 :r9)
            `(sys.lap-x86:mov64 :r9 :r10)
            `(sys.lap-x86:mov64 :r13 (:function sys.int::raise-bounds-error))
            `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 2))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:ud2)))
    (load-in-reg :r10 slot t)
    (fixnum-check :r10)
    (load-in-reg :r9 object t)
    (load-in-reg :r11 new t)
    (load-in-reg :r8 old t)
    (smash-r8)
    (emit `(sys.lap-x86:mov8 :al :r9l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
          `(sys.lap-x86:jne ,type-error-label)
          `(sys.lap-x86:mov64 :rax ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash sys.int::+object-tag-structure-object+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:jne ,type-error-label)
          ;; Convert size and slot number to integers.
          `(sys.lap-x86:shr64 :rax ,sys.int::+array-length-shift+)
          `(sys.lap-x86:mov64 :rcx :r10)
          `(sys.lap-x86:shr64 :rcx ,sys.int::+n-fixnum-bits+)
          ;; Check bounds.
          `(sys.lap-x86:cmp64 :rcx :rax)
          `(sys.lap-x86:jae ,bounds-error-label)
          ;; Load value.
          `(sys.lap-x86:mov64 :rax :r8)
          (emit-gc-info :extra-registers :rax)
          `(sys.lap-x86:lock)
          `(sys.lap-x86:cmpxchg ,(object-ea :r9 :index '(:rcx 8)) :r11))
    (cond ((member *for-value* '(:multiple :tail))
           ;; Return success and the old value.
           (emit `(sys.lap-x86:mov64 :r9 :rax))
           (emit-gc-info)
           (emit `(sys.lap-x86:mov64 :r8 nil)
                 `(sys.lap-x86:cmov64z :r8 (:constant t)))
           (load-constant :rcx 2)
           :multiple)
          (t ;; Just return the success state.
           (emit-gc-info)
           (predicate-result :z)))))

;;; Standard object instances.

(define-array-like-predicate sys.int::std-instance-p sys.int::+object-tag-std-instance+)
(define-array-like-accessor sys.int::std-instance-class
    sys.int::std-instance sys.int::+object-tag-std-instance+
    0)
(define-array-like-accessor sys.int::std-instance-slots
    sys.int::std-instance sys.int::+object-tag-std-instance+
    1)

;;; Functions.

(defbuiltin functionp (object) ()
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
          `(sys.lap-x86:sub8 :al ,(ash sys.int::+first-function-object-tag+
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+array-type-size+))
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash (- sys.int::+last-function-object-tag+
                                          sys.int::+first-function-object-tag+)
                                       sys.int::+array-type-shift+))
          `(sys.lap-x86:cmov64be :r8 (:constant t))
          out)
    (setf *r8-value* (list (gensym)))))
