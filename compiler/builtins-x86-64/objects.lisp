;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for examining objects.

(in-package :mezzano.compiler.codegen.x86-64)

;;; Examining the object header.

(defbuiltin sys.int::%object-tag (thing) ()
  (load-in-r8 thing t)
  (smash-r8)
  (emit `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
        `(sys.lap-x86:and32 :eax #xFF)
        `(sys.lap-x86:shr32 :eax ,sys.int::+object-type-shift+)
        `(sys.lap-x86:shl32 :eax ,sys.int::+n-fixnum-bits+)
        `(sys.lap-x86:mov32 :r8d :eax))
  (setf *r8-value* (list (gensym))))

(defbuiltin sys.int::%object-header-data (value) ()
  (load-in-reg :r8 value t)
  (smash-r8)
  (emit `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot -1))
        `(sys.lap-x86:and64 :r8 ,(lognot (1- (ash 1 sys.int::+object-data-shift+))))
        `(sys.lap-x86:shr64 :r8 ,(- sys.int::+object-data-shift+
                                    sys.int::+n-fixnum-bits+)))
  (setf *r8-value* (list (gensym))))

(defbuiltin (setf sys.int::%object-header-data) (value object) ()
  (load-in-reg :rax value t)
  (load-in-reg :r8 object t)
  (emit `(sys.lap-x86:shl64 :rax ,(- sys.int::+object-data-shift+
                                     sys.int::+n-fixnum-bits+))
        ;; low 8 bits of the header only.
        `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
        `(sys.lap-x86:mov64 ,(object-ea :r8 :slot -1) :rax))
  value)

;;(defun object-of-type-p (object type)
;;  (and (%value-has-tag-p object +tag-object+)
;;       (eql (%object-tag object) type)))
(defbuiltin sys.int::%object-of-type-p (object object-tag) ()
  (cond ((constant-type-p object-tag `(unsigned-byte ,sys.int::+object-type-size+))
         ;; Fast case where the tag is a constant.
         (let ((out (gensym)))
           (load-in-reg :r8 object t)
           ;; Test tag = +tag-object+
           (emit `(sys.lap-x86:mov8 :al :r8l)
                 `(sys.lap-x86:and8 :al #b1111)
                 `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
                 `(sys.lap-x86:jne ,out))
           ;; Test object type.
           (emit `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
                 `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                              sys.int::+object-type-shift+))
                 `(sys.lap-x86:cmp8 :al ,(ash (second object-tag)
                                              sys.int::+object-type-shift+)))
           (emit out)
           (predicate-result :e)))
        (t ;; Slow path.
         (load-in-reg :r8 object t)
         (load-in-reg :r10 object-tag t)
         (let ((out (gensym)))
           ;; Test tag = +tag-object+
           (emit `(sys.lap-x86:mov8 :al :r8l)
                 `(sys.lap-x86:and8 :al #b1111)
                 `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
                 `(sys.lap-x86:jne ,out))
           ;; Test object type.
           (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot -1))
                 `(sys.lap-x86:and64 :rax ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                                sys.int::+object-type-shift+))
                 ;; Convert type to fixnum.
                 `(sys.lap-x86:shr64 :rax ,(- sys.int::+object-type-shift+
                                            sys.int::+n-fixnum-bits+))
                 `(sys.lap-x86:cmp64 :rax :r10))
           (emit out)
           (predicate-result :e)))))

;;(defun type-check (object object-tag expected-type)
;;  (unless (and (%value-has-tag-p object +tag-object+)
;;               (eql (%object-tag object) object-tag))
;;    (raise-type-error object expected-type)))
(defbuiltin sys.int::%type-check (object object-tag expected-type) ()
  (block nil
    (cond ((and (constant-type-p object-tag `(unsigned-byte ,sys.int::+object-type-size+))
                (quoted-constant-p expected-type))
           (when (and (quoted-constant-p object)
                      (typep (second object) (second expected-type)))
             (return ''nil))
           ;; Fast case where the tag & type are constants.
           (let ((type-error-label (gensym)))
             (emit-trailer (type-error-label)
               (raise-type-error :r8 (second expected-type)))
             (load-in-reg :r8 object t)
             ;; Test tag = +tag-object+
             (emit `(sys.lap-x86:mov8 :al :r8l)
                   `(sys.lap-x86:and8 :al #b1111)
                   `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
                   `(sys.lap-x86:jne ,type-error-label))
             ;; Test object type.
             (emit `(sys.lap-x86:mov8 :al ,(object-ea :r8 :slot -1))
                   `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                                sys.int::+object-type-shift+))
                   `(sys.lap-x86:cmp8 :al ,(ash (second object-tag)
                                                sys.int::+object-type-shift+))
                   `(sys.lap-x86:jne ,type-error-label))))
          (t ;; Slow path.
           (load-in-reg :r8 object t)
           (load-in-reg :r10 object-tag t)
           (let ((type-error-label (gensym)))
             (emit-trailer (type-error-label)
               (load-in-reg :r9 expected-type t)
               (call-support-function 'sys.int::raise-type-error 2)
               (emit `(sys.lap-x86:ud2)))
             ;; Test tag = +tag-object+
             (emit `(sys.lap-x86:mov8 :al :r8l)
                   `(sys.lap-x86:and8 :al #b1111)
                   `(sys.lap-x86:cmp8 :al ,sys.int::+tag-object+)
                   `(sys.lap-x86:jne ,type-error-label))
             ;; Test object type.
             (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot -1))
                   `(sys.lap-x86:and64 :rax ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                                  sys.int::+object-type-shift+))
                   ;; Convert type to fixnum.
                   `(sys.lap-x86:shr64 :rax ,(- sys.int::+object-type-shift+
                                                sys.int::+n-fixnum-bits+))
                   `(sys.lap-x86:cmp64 :rax :r10)
                   `(sys.lap-x86:jne ,type-error-label)))))
    *r8-value*))

;;(defun bounds-check (object slot)
;;  (unless (fixnump slot)
;;    (raise-type-error slot 'fixnum))
;;  (unless (< slot (%object-header-data object))
;;    (raise-bounds-error object slot)))
(defbuiltin sys.int::%bounds-check (object slot) ()
  (let ((bounds-error-label (gensym))
        (constant-slot (and (constant-type-p slot `(unsigned-byte ,sys.int::+object-data-size+))
                            (second slot))))
    (emit-trailer (bounds-error-label)
      (when constant-slot
        (load-constant :r9 constant-slot))
      (call-support-function 'sys.int::raise-bounds-error 2)
      (emit `(sys.lap-x86:ud2)))
    (load-in-reg :r8 object t)
    ;; Load & mask length.
    (emit `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:and64 :rax ,(lognot (ldb (byte 64 0) (lognot (ash (1- (ash 1 sys.int::+object-data-size+)) sys.int::+object-data-shift+))))))
    (cond ((and constant-slot
                (typep (ash constant-slot sys.int::+object-data-shift+)
                       '(signed-byte 32)))
           ;; Compare directly against the header. The header layout allows
           ;; the comparison to be done safely without masking away the type
           ;; bits.
           (emit `(sys.lap-x86:cmp64 :rax ,(ash constant-slot sys.int::+object-data-shift+))))
          (constant-slot
           ;; Same as above, but the slot won't fit in an imm32.
           (emit `(sys.lap-x86:mov64 :rcx ,(ash constant-slot sys.int::+object-data-shift+))
                 `(sys.lap-x86:cmp64 :rax :rcx)))
          (t ;; Nonconstant. Do fixnum check, then unbox & compare.
           (load-in-reg :r9 slot t)
           (fixnum-check :r9)
           (emit `(sys.lap-x86:mov64 :rcx :r9)
                 `(sys.lap-x86:shl64 :rcx ,(- sys.int::+object-data-shift+
                                              sys.int::+n-fixnum-bits+))
                 `(sys.lap-x86:cmp64 :rax :rcx))))
    ;; Comparison done.
    ;; value >= slot
    (emit `(sys.lap-x86:jbe ,bounds-error-label)))
  *r8-value*)

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
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:and8 :al ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:cmp8 :al ,(ash (- sys.int::+last-function-object-tag+
                                          sys.int::+first-function-object-tag+)
                                       sys.int::+object-type-shift+))
          `(sys.lap-x86:cmov64be :r8 (:constant t))
          out)
    (setf *r8-value* (list (gensym)))))
