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
