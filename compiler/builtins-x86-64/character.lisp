;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with characters.

(in-package :mezzano.compiler.codegen.x86-64)

(define-tag-type-predicate characterp sys.int::+tag-character+)

(defbuiltin char-code (char) ()
  (let ((type-error-label (gensym)))
    (emit-trailer (type-error-label)
		  (raise-type-error :r8 'character))
    (load-in-r8 char t)
    (smash-r8)
    (emit `(sys.lap-x86:mov8 :al :r8l)
	  `(sys.lap-x86:and8 :al #b1111)
	  `(sys.lap-x86:cmp8 :al ,sys.int::+tag-character+)
	  `(sys.lap-x86:jne ,type-error-label)
	  ;; Mask away the non-code bits.
	  `(sys.lap-x86:and32 :r8d #x01fffff0)
	  ;; Shift to fixnum.
	  `(sys.lap-x86:shr32 :r8d ,(- 4 sys.int::+n-fixnum-bits+)))
    (setf *r8-value* (list (gensym)))))

(defbuiltin sys.int::char-bits (character) ()
  (let ((type-error-label (gensym)))
    (emit-trailer (type-error-label)
      (raise-type-error :r8 'character))
    (load-in-r8 character t)
    (smash-r8)
    (emit `(sys.lap-x86:mov8 :al :r8l)
          `(sys.lap-x86:and8 :al #b1111)
          `(sys.lap-x86:cmp8 :al ,sys.int::+tag-character+)
          `(sys.lap-x86:jne ,type-error-label)
          `(sys.lap-x86:and32 :r8d #x1e000000)
          `(sys.lap-x86:shr32 :r8d ,(- 25 sys.int::+n-fixnum-bits+)))
    (setf *r8-value* (list (gensym)))))

(defbuiltin char-int (char) ()
  (let ((type-error-label (gensym)))
    (emit-trailer (type-error-label)
		  (raise-type-error :r8 'character))
    (load-in-r8 char t)
    (smash-r8)
    (emit `(sys.lap-x86:mov8 :al :r8l)
	  `(sys.lap-x86:and8 :al #b1111)
	  `(sys.lap-x86:cmp8 :al ,sys.int::+tag-character+)
	  `(sys.lap-x86:jne ,type-error-label)
	  ;; Mask away the tag bits.
	  `(sys.lap-x86:and32 :r8d -16)
	  ;; Shift to fixnum.
	  `(sys.lap-x86:shr32 :r8d ,(- 4 sys.int::+n-fixnum-bits+)))
    (setf *r8-value* (list (gensym)))))
