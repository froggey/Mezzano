;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for dealing with symbols.

(in-package :sys.c)

(define-array-like-predicate symbolp sys.int::+object-tag-symbol+)
(define-array-like-reader symbol-name symbol sys.int::+object-tag-symbol+ sys.int::+symbol-name+)
(define-array-like-accessor symbol-package symbol sys.int::+object-tag-symbol+ sys.int::+symbol-package+)

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

(defbuiltin sys.int::symbol-global-value (symbol) ()
  (let ((unbound-error-label (gensym)))
    (emit-trailer (unbound-error-label)
      (emit `(sys.lap-x86:mov64 :r8 :r9))
      (call-support-function 'sys.int::raise-unbound-error 1)
      (emit `(sys.lap-x86:ud2)))
    (load-in-reg :r9 symbol t)
    (emit-object-type-check :r9 sys.int::+object-tag-symbol+ 'symbol symbol)
    (smash-r8)
    (emit `(sys.lap-x86:mov64 :r8 ,(object-ea :r9 :slot sys.int::+symbol-value+))
	  `(sys.lap-x86:cmp64 :r8 :unbound-value)
	  `(sys.lap-x86:je ,unbound-error-label))
    (setf *r8-value* (list (gensym)))))

(defbuiltin (setf sys.int::symbol-global-value) (value symbol) ()
  (load-in-reg :r9 symbol t)
  (load-in-reg :r8 value t)
  (emit-object-type-check :r9 sys.int::+object-tag-symbol+ 'symbol symbol)
  (emit `(sys.lap-x86:mov64 ,(object-ea :r9 :slot sys.int::+symbol-value+) :r8))
  *r8-value*)

(defbuiltin sys.int::%cas-symbol-global-value (symbol old new) ()
  (load-in-reg :r9 symbol t)
  (load-in-reg :r11 new t)
  (load-in-reg :r8 old t)
  (smash-r8)
  (emit-object-type-check :r9 sys.int::+object-tag-symbol+ 'symbol symbol)
  ;; CAS.
  (emit `(sys.lap-x86:mov64 :rax :r8))
  (emit-gc-info :extra-registers :rax)
  (emit `(sys.lap-x86:lock)
        `(sys.lap-x86:cmpxchg ,(object-ea :r9 :slot sys.int::+symbol-value+) :r11))
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
         (predicate-result :z))))

(define-array-like-accessor sys.int::symbol-fref symbol sys.int::+object-tag-symbol+ sys.int::+symbol-function+)

;; TODO type checking? ensure value is a plist?
(define-array-like-accessor symbol-plist symbol sys.int::+object-tag-symbol+ sys.int::+symbol-plist+)

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
