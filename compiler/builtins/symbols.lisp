;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with symbols.

(in-package :sys.c)

(defbuiltin symbol-value (symbol) ()
  (let ((unbound-error-label (gensym))
        (no-tls-slot (gensym))
        (test-bound (gensym)))
    (emit-trailer (unbound-error-label)
      (emit `(sys.lap-x86:mov64 :r8 :r9))
      (call-support-function 'sys.int::raise-unbound-error 1)
      (emit `(sys.lap-x86:ud2)))
    (load-in-reg :r9 symbol t)
    (emit-object-type-check :r9 sys.int::+object-tag-symbol+ 'symbol symbol)
    (smash-r8)
    (emit ;; Extract the TLS offset.
          `(sys.lap-x86:mov64 :rax ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:shr64 :rax ,+tls-offset-shift+)
          `(sys.lap-x86:and32 :eax #xFFFF)
          `(sys.lap-x86:jz ,no-tls-slot)
          ;; Read from the TLS slot.
          `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 :r8 ((:rax 8) ,+tls-base-offset+))
          ;; Check if the TLS slot holds a value.
          `(sys.lap-x86:cmp64 :r8 :unbound-tls-slot)
          `(sys.lap-x86:jne ,test-bound)
          no-tls-slot
	  `(sys.lap-x86:mov64 :r8 ,(object-ea :r9 :slot sys.int::+symbol-value+))
          test-bound
	  `(sys.lap-x86:cmp64 :r8 :unbound-value)
	  `(sys.lap-x86:je ,unbound-error-label))
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf symbol-value) (value symbol) ()
  (let ((no-tls-slot (gensym))
        (out (gensym)))
    (load-in-reg :r9 symbol t)
    (load-in-reg :r8 value t)
    (emit-object-type-check :r9 sys.int::+object-tag-symbol+ 'symbol symbol)
    (emit ;; Extract the TLS offset.
          `(sys.lap-x86:mov64 :rax ,(object-ea :r9 :slot -1))
          `(sys.lap-x86:shr64 :rax ,+tls-offset-shift+)
          `(sys.lap-x86:and32 :eax #xFFFF)
          `(sys.lap-x86:jz ,no-tls-slot)
          ;; Check if the TLS slot holds a value.
          `(sys.lap-x86:gs)
          `(sys.lap-x86:cmp64 ((:rax 8) ,+tls-base-offset+) :unbound-tls-slot)
          `(sys.lap-x86:je ,no-tls-slot)
          `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 ((:rax 8) ,+tls-base-offset+) :r8)
          `(sys.lap-x86:jmp ,out)
          no-tls-slot
          `(sys.lap-x86:mov64 ,(object-ea :r9 :slot sys.int::+symbol-value+) :r8)
          out)
    *r8-value*))

(defbuiltin boundp (symbol) ()
  (let ((no-tls-slot (gensym))
        (out (gensym)))
    (load-in-reg :r8 symbol t)
    (emit-object-type-check :r8 sys.int::+object-tag-symbol+ 'symbol symbol)
    (smash-r8)
    (emit ;; Extract the TLS offset.
          `(sys.lap-x86:mov64 :rax ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:shr64 :rax ,+tls-offset-shift+)
          `(sys.lap-x86:and32 :eax #xFFFF)
          `(sys.lap-x86:jz ,no-tls-slot)
          ;; Read from the TLS slot.
          `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 :r9 ((:rax 8) ,+tls-base-offset+))
          ;; Check if the TLS slot holds a value.
          `(sys.lap-x86:cmp64 :r9 :unbound-tls-slot)
          `(sys.lap-x86:je ,no-tls-slot)
	  `(sys.lap-x86:cmp64 :r9 :unbound-value)
          `(sys.lap-x86:jmp ,out)
          no-tls-slot
	  `(sys.lap-x86:cmp64 ,(object-ea :r8 :slot sys.int::+symbol-value+)
                              :unbound-value)
          out)
    (predicate-result :ne)))

(defbuiltin makunbound (symbol) ()
  (let ((no-tls-slot (gensym))
        (out (gensym)))
    (load-in-reg :r8 symbol t)
    (emit-object-type-check :r8 sys.int::+object-tag-symbol+ 'symbol symbol)
    (emit ;; Extract the TLS offset.
          `(sys.lap-x86:mov32 :eax ,(object-ea :r8 :slot -1))
          `(sys.lap-x86:shr32 :eax ,+tls-offset-shift+)
          `(sys.lap-x86:and32 :eax #xFFFF)
          `(sys.lap-x86:jz ,no-tls-slot)
          ;; Check if the TLS slot holds a value.
          `(sys.lap-x86:gs)
          `(sys.lap-x86:cmp64 ((:rax 8) ,+tls-base-offset+) :unbound-tls-slot)
          `(sys.lap-x86:je ,no-tls-slot)
          `(sys.lap-x86:gs)
          `(sys.lap-x86:mov64 ((:rax 8) ,+tls-base-offset+) :unbound-value)
          `(sys.lap-x86:jmp ,out)
          no-tls-slot
	  `(sys.lap-x86:mov64 ,(object-ea :r8 :slot sys.int::+symbol-value+) :unbound-value)
          out)
    *r8-value*))
