;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for examining objects.

(in-package :mezzano.compiler.codegen.arm64)

;;; Examining the object header.

(defbuiltin sys.int::%object-tag (thing) ()
  (load-in-x0 thing t)
  (smash-x0)
  (emit-object-load :x9 :x0 :slot -1)
  (emit `(lap:and :x9 :x9 #b11111100)
        `(lap:add :x0 :xzr :x9 :lsr 1))
  (setf *x0-value* (list (gensym))))

(defbuiltin sys.int::%object-header-data (value) ()
  (load-in-reg :x0 value t)
  (smash-x0)
  (emit-object-load :x0 :x0 :slot -1)
  (emit `(lap:and :x0 :x0 ,(lognot (1- (ash 1 sys.int::+object-data-shift+))))
        `(lap:add :x0 :xzr :x0 :lsr ,(- sys.int::+object-data-shift+
                                        sys.int::+n-fixnum-bits+)))
  (setf *x0-value* (list (gensym))))

(defbuiltin (setf sys.int::%object-header-data) (value object) ()
  (load-in-reg :x9 value t)
  (load-in-reg :x0 object t)
  (emit-object-load :x10 :x0 :slot -1)
  (emit `(lap:add :x9 :xzr :x9 :lsl ,(- sys.int::+object-data-shift+
                                        sys.int::+n-fixnum-bits+))
        ;; low 8 bits of the header only.
        `(lap:and :x10 :x10 #xFF)
        `(lap:orr :x9 :x9 :x10))
  (emit-object-store :x9 :x0 :slot -1)
  value)

;;(defun object-of-type-p (object type)
;;  (and (%value-has-tag-p object +tag-object+)
;;       (eql (%object-tag object) type)))
(defbuiltin sys.int::%object-of-type-p (object object-tag) ()
  (cond ((constant-type-p object-tag `(unsigned-byte ,sys.int::+object-type-size+))
         ;; Fast case where the tag is a constant.
         (let ((out (gensym)))
           (load-in-reg :x0 object t)
           ;; Test tag = +tag-object+
           (emit `(lap:and :x9 :x0 #b1111)
                 `(lap:subs :xzr :x9 ,sys.int::+tag-object+)
                 `(lap:b.ne ,out))
           ;; Test object type.
           (emit-object-load :x9 :x0 :slot -1)
           (emit `(lap:and :x9 :x9 #b11111100)
                 `(lap:subs :xzr :x9 ,(ash (second object-tag)
                                           sys.int::+object-type-shift+)))
           (emit out)
           (predicate-result :eq)))
        (t ;; Slow path.
         (load-in-reg :x0 object t)
         (load-in-reg :x2 object-tag t)
         (let ((out (gensym)))
           ;; Test tag = +tag-object+
           (emit `(lap:and :x9 :x0 #b1111)
                 `(lap:subs :xzr :x9 ,sys.int::+tag-object+)
                 `(lap:b.ne ,out))
           ;; Test object type.
           (emit-object-load :x9 :x0 :slot -1)
           (emit `(lap:and :x9 :x9 #b11111100)
                 `(lap:add :x9 :xzr :x9 :asr ,sys.int::+n-fixnum-bits+)
                 `(lap:subs :xzr :x9 :x2))
           (emit out)
           (predicate-result :eq)))))

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
               (raise-type-error :x0 (second expected-type)))
             (load-in-reg :x0 object t)
             ;; Test tag = +tag-object+
             (emit `(lap:and :x9 :x0 #b1111)
                   `(lap:subs :xzr :x9 ,sys.int::+tag-object+)
                   `(lap:b.ne ,type-error-label))
             ;; Test object type.
             (emit-object-load :x9 :x0 :slot -1)
             (emit `(lap:and :x9 :x9 #b11111100)
                   `(lap:subs :xzr :x9 ,(ash (second object-tag)
                                             sys.int::+object-type-shift+))
                   `(lap:b.ne ,type-error-label))))
          (t ;; Slow path.
           (load-in-reg :x0 object t)
           (load-in-reg :x2 object-tag t)
           (let ((type-error-label (gensym)))
             (emit-trailer (type-error-label)
               (load-in-reg :x1 expected-type t)
               (call-support-function 'sys.int::raise-type-error 2)
               (emit `(lap:hlt 0)))
             ;; Test tag = +tag-object+
             (emit `(lap:and :x9 :x0 #b1111)
                   `(lap:subs :xzr :x9 ,sys.int::+tag-object+)
                   `(lap:b.ne ,type-error-label))
             ;; Test object type.
             (emit-object-load :x9 :x0 :slot -1)
             (emit `(lap:and :x9 :x9 #b11111100)
                   `(lap:add :x9 :xzr :x9 :asr ,(- sys.int::+object-type-shift+
                                                   sys.int::+n-fixnum-bits+))
                   `(lap:subs :xzr :x9 :x2)
                   `(lap:b.ne ,type-error-label)))))
    *x0-value*))

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
        (load-constant :x1 constant-slot))
      (call-support-function 'sys.int::raise-bounds-error 2)
      (emit `(lap:hlt 0)))
    (load-in-reg :x0 object t)
    ;; Load & mask length.
    (emit-object-load :x9 :x0 :slot -1)
    (emit `(lap:and :x9 :x9 ,(lognot (ldb (byte 64 0) (lognot (ash (1- (ash 1 sys.int::+object-data-size+)) sys.int::+object-data-shift+))))))
    (cond (constant-slot
           ;; Compare directly against the header. The header layout allows
           ;; the comparison to be done safely without masking away the type
           ;; bits.
           (load-literal :x10 (ash constant-slot sys.int::+object-data-shift+))
           (emit `(lap:subs :xzr :x9 :x10)))
          (t ;; Nonconstant. Do fixnum check, then unbox & compare.
           (load-in-reg :x1 slot t)
           (fixnum-check :x1)
           (emit `(lap:add :x10 :xzr :x1 :lsl ,(- sys.int::+object-data-shift+
                                                  sys.int::+n-fixnum-bits+))
                 `(lap:subs :xzr :x9 :x10))))
    ;; Comparison done.
    ;; value >= slot
    (emit `(lap:b.ls ,bounds-error-label)))
  *x0-value*)
