;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defparameter *perform-tce* nil
  "When true, attempt to eliminate tail calls.")
(defparameter *suppress-builtins* nil
  "When T, the built-in functions will not be used and full calls will
be generated instead.")
(defparameter *enable-branch-tensioner* t)
(defparameter *enable-stack-alignment-checking* nil)
(defparameter *trace-asm* nil)

(defvar *run-counter* nil)
(defvar *load-list* nil)
(defvar *r8-value* nil)
(defvar *stack-values* nil)
(defvar *for-value* nil)
(defvar *rename-list* nil)
(defvar *code-accum* nil)
(defvar *trailers* nil)
(defvar *current-lambda-name* nil)
(defvar *gc-info-fixups* nil)
(defvar *active-nl-exits* nil)

(defconstant +binding-stack-gs-offset+ (- (* 7 8) sys.int::+tag-object+))
(defconstant +tls-base-offset+ (- sys.int::+tag-object+))
(defconstant +tls-offset-shift+ (+ sys.int::+object-data-shift+ 2))

(defun emit (&rest instructions)
  (dolist (i instructions)
    (push i *code-accum*)))

(defun comment (&rest stuff)
  (emit `(:comment ,@stuff)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun object-ea (base &key (slot 0) index)
  (if index
      (list base index (+ (- sys.int::+tag-object+) 8 (* slot 8)))
      (list base (+ (- sys.int::+tag-object+) 8 (* slot 8)))))
)

(defmacro emit-trailer ((&optional name (default-gc-info t)) &body body)
  `(push (let ((*code-accum* '()))
	   ,(when name
		  `(emit ,name))
           ,@(when default-gc-info
                   (list '(emit-gc-info)))
	   (progn ,@body)
	   (nreverse *code-accum*))
	 *trailers*))

(defun fixnump (object)
  (typep object '(signed-byte 63)))

(defun fixnum-to-raw (integer)
  (check-type integer (signed-byte 63))
  (ash integer sys.int::+n-fixnum-bits+))

(defun character-to-raw (character)
  (check-type character character)
  (logior (ash (char-int character) 4)
          sys.int::+tag-character+))

(defun control-stack-frame-offset (slot)
  "Convert a control stack slot number to an offset."
  (- (* (1+ slot) 8)))

(defun control-stack-slot-ea (slot)
  "Return an effective address for a control stack slot."
  `(:stack ,slot))

;;; TODO: This should work on a stack-like system so that slots can be
;;; reused when the allocation is no longer needed.
(defun allocate-control-stack-slots (count &optional boxed)
  (when (oddp count) (incf count))
  (when (oddp (length *stack-values*))
    (vector-push-extend nil *stack-values*))
  (prog1 (length *stack-values*)
    (dotimes (i count)
      (vector-push-extend (if boxed
                              '(:boxed . :home)
                              '(:unboxed . :home))
                          *stack-values*))))

(defun add-dx-root (stack-slot)
  (do ((i *active-nl-exits* (cdr i)))
      ((endp i))
    (push stack-slot (car i))))

(defun no-tail (value-mode)
  (ecase value-mode
    ((:multiple :predicate t nil) value-mode)
    (:tail :multiple)))

(defun codegen-lambda (lambda)
  (let* ((*current-lambda* lambda)
         (*current-lambda-name* (or (lambda-information-name lambda)
                                    (list 'lambda :in (or *current-lambda-name*
                                                          (when *compile-file-pathname*
                                                            (princ-to-string *compile-file-pathname*))))))
         (*run-counter* 0)
         (*load-list* '())
         (*r8-value* nil)
         (*stack-values* (make-array 8 :fill-pointer 0 :adjustable t))
         (*for-value* t)
         (*rename-list* '())
         (*code-accum* '())
         (*trailers* '())
         (arg-registers '(:r8 :r9 :r10 :r11 :r12))
         (*gc-info-fixups* '())
         (*active-nl-exits* '()))
    ;; Check some assertions.
    ;; No keyword arguments, no special arguments, no non-constant
    ;; &optional init-forms and no non-local arguments.
    (assert (not (lambda-information-enable-keys lambda)) ()
            "&KEY arguments did not get lowered!")
    (assert (every (lambda (arg)
                     (lexical-variable-p arg))
                   (lambda-information-required-args lambda)))
    (assert (every (lambda (arg)
                     (and (lexical-variable-p (first arg))
                          (quoted-form-p (second arg))
                          (or (null (third arg))
                              (lexical-variable-p (first arg)))))
                   (lambda-information-optional-args lambda)))
    (assert (or (null (lambda-information-rest-arg lambda))
                (lexical-variable-p (lambda-information-rest-arg lambda))))
    ;; Free up :RBX quickly.
    (let ((env-arg (lambda-information-environment-arg lambda)))
      (when env-arg
        (let ((ofs (find-stack-slot)))
          (setf (aref *stack-values* ofs) (cons env-arg :home))
          (when (not (getf (lambda-information-plist lambda) 'unwind-protect-cleanup))
            ;; Read environment pointer from closure object.
            (emit `(sys.lap-x86:mov64 :rbx ,(object-ea :rbx :slot 2))))
          (emit `(sys.lap-x86:mov64 (:stack ,ofs) :rbx)))))
    ;; Compile argument setup code.
    (let ((current-arg-index 0))
      (dolist (arg (lambda-information-required-args lambda))
        (incf current-arg-index)
        (let ((ofs (find-stack-slot)))
          (setf (aref *stack-values* ofs) (cons arg :home))
          (if arg-registers
              (emit `(sys.lap-x86:mov64 (:stack ,ofs) ,(pop arg-registers)))
              (emit `(sys.lap-x86:mov64 :r8 (:cfp ,(* (+ (- current-arg-index 6) 2) 8)))
                    `(sys.lap-x86:mov64 (:stack ,ofs) :r8)))))
      (dolist (arg (lambda-information-optional-args lambda))
        (let ((mid-label (gensym))
              (end-label (gensym))
              (var-ofs (find-stack-slot))
              (sup-ofs nil))
          (setf (aref *stack-values* var-ofs) (cons (first arg) :home))
          (when (and (third arg)
                     (not (zerop (lexical-variable-use-count (third arg)))))
            (setf sup-ofs (find-stack-slot))
            (setf (aref *stack-values* sup-ofs) (cons (third arg) :home)))
          ;; Check if this argument was supplied.
          (emit `(sys.lap-x86:cmp64 :rcx ,(fixnum-to-raw current-arg-index))
                `(sys.lap-x86:jle ,mid-label))
          ;; Argument supplied, stash wherever.
          (if arg-registers
              (emit `(sys.lap-x86:mov64 (:stack ,var-ofs) ,(pop arg-registers)))
              (emit `(sys.lap-x86:mov64 :r8 (:cfp ,(* (+ (- current-arg-index 5) 2) 8)))
                    `(sys.lap-x86:mov64 (:stack ,var-ofs) :r8)))
          (when sup-ofs
            (emit `(sys.lap-x86:mov64 (:stack ,sup-ofs) t)))
          (emit `(sys.lap-x86:jmp ,end-label)
                mid-label)
          ;; Argument not supplied. Init-form is a quoted constant.
          (let ((tag (second arg)))
            (load-in-r8 tag t)
            (setf *r8-value* nil)
            (emit `(sys.lap-x86:mov64 (:stack ,var-ofs) :r8))
            (when sup-ofs
              (emit `(sys.lap-x86:mov64 (:stack ,sup-ofs) nil))))
          (emit end-label)
          (incf current-arg-index))))
    ;; Deal with &REST late to avoid excess register spilling.
    (let ((rest-arg (lambda-information-rest-arg lambda)))
      (when (and rest-arg
                 ;; Avoid generating code &REST code when the variable isn't used.
                 (not (zerop (lexical-variable-use-count rest-arg))))
        (emit-rest-list lambda arg-registers)))
    ;; No longer in argument setup mode, switch over to normal GC info.
    (emit-gc-info)
    (let ((code-tag (let ((*for-value* (if *perform-tce* :tail :multiple)))
                      (cg-form `(progn ,@(lambda-information-body lambda))))))
      (when code-tag
        (load-multiple-values code-tag)
        (emit `(sys.lap-x86:leave)
              `(:gc :no-frame)
              `(sys.lap-x86:ret))))
    (let* ((final-code (nconc (generate-entry-code lambda)
                              (nreverse *code-accum*)
                              (apply #'nconc *trailers*)))
           (homes (loop for (var . loc) across *stack-values*
                     for i from 0
                     when (and (lexical-variable-p var)
                               (eql loc :home))
                     collect (list (lexical-variable-name var) i))))
      ;; Fix all the GC instructions.
      (dolist (inst *gc-info-fixups*)
        (setf (rest (last inst)) (list :layout (coerce (loop for value across *stack-values*
                                                          collect (if (equal value '(:unboxed . :home))
                                                                      0
                                                                      1))
                                                       'bit-vector))))
      (when *enable-branch-tensioner*
        (setf final-code (tension-branches final-code)))
      (when *trace-asm*
        (format t "~S:~%" *current-lambda-name*)
        (format t "Final values: ~S~%" *stack-values*)
        (format t "~{~S~%~}" final-code))
      (sys.int::assemble-lap
       final-code
       *current-lambda-name*
       (list :debug-info
             *current-lambda-name*
             homes
             (when (lambda-information-environment-layout lambda)
               (position (first (lambda-information-environment-layout lambda))
                         *stack-values*
                         :key #'car))
             (second (lambda-information-environment-layout lambda))
             (when *compile-file-pathname*
               (princ-to-string *compile-file-pathname*))
             sys.int::*top-level-form-number*
             (lambda-information-lambda-list lambda)
             (lambda-information-docstring lambda))))))

(defun emit-gc-info (&rest extra-stuff)
  (let ((thing (list* :gc :frame extra-stuff)))
    (push thing *gc-info-fixups*)
    (emit thing)))

(defun generate-entry-code (lambda)
  (let ((entry-label (gensym "ENTRY"))
	(invalid-arguments-label (gensym "BADARGS"))
        (stack-alignment-error (gensym "STACK-ALIGNMENT")))
    (emit-trailer (invalid-arguments-label nil)
      (emit `(:gc :frame)
            `(sys.lap-x86:xor32 :ecx :ecx)
            `(sys.lap-x86:mov64 :r13 (:function sys.int::raise-invalid-argument-error))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:ud2)))
    (when *enable-stack-alignment-checking*
      (emit-trailer (stack-alignment-error nil)
        (emit `(:gc :frame)
              `(sys.lap-x86:sub64 :rsp 8)
              `(sys.lap-x86:mov64 :r13 (:function sys.int::raise-stack-alignment-error))
              `(sys.lap-x86:xor32 :ecx :ecx)
              `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
              `(sys.lap-x86:ud2))))
    (nconc
     (list entry-label
	   ;; Create control stack frame.
           `(:gc :no-frame :incoming-arguments :rcx)
	   `(sys.lap-x86:push :cfp)
           `(:gc :no-frame :incoming-arguments :rcx :layout #*0)
	   `(sys.lap-x86:mov64 :cfp :csp)
           `(:gc :frame :incoming-arguments :rcx))
     (when *enable-stack-alignment-checking*
       (list `(sys.lap-x86:test64 :rsp 8)
             `(sys.lap-x86:jnz ,stack-alignment-error)))
     ;; Emit the argument count test.
     (cond ((lambda-information-rest-arg lambda)
	    ;; If there are no required parameters, then don't generate a lower-bound check.
	    (when (lambda-information-required-args lambda)
	      ;; Minimum number of arguments.
	      (list `(sys.lap-x86:cmp32 :ecx ,(fixnum-to-raw (length (lambda-information-required-args lambda))))
		    `(sys.lap-x86:jl ,invalid-arguments-label))))
	   ((and (lambda-information-required-args lambda)
		 (lambda-information-optional-args lambda))
	    ;; A range.
	    (list `(sys.lap-x86:mov32 :eax :ecx)
                  `(sys.lap-x86:sub32 :eax ,(fixnum-to-raw (length (lambda-information-required-args lambda))))
		  `(sys.lap-x86:cmp32 :eax ,(fixnum-to-raw (length (lambda-information-optional-args lambda))))
		  `(sys.lap-x86:ja ,invalid-arguments-label)))
	   ((lambda-information-optional-args lambda)
	    ;; Maximum number of arguments.
	    (list `(sys.lap-x86:cmp32 :ecx ,(fixnum-to-raw (length (lambda-information-optional-args lambda))))
		  `(sys.lap-x86:ja ,invalid-arguments-label)))
	   ((lambda-information-required-args lambda)
	    ;; Exact number of arguments.
	    (list `(sys.lap-x86:cmp32 :ecx ,(fixnum-to-raw (length (lambda-information-required-args lambda))))
		  `(sys.lap-x86:jne ,invalid-arguments-label)))
	   ;; No arguments
	   (t (list `(sys.lap-x86:test32 :ecx :ecx)
		    `(sys.lap-x86:jnz ,invalid-arguments-label))))
     (let ((n-slots (length *stack-values*)))
       (when (oddp n-slots) (incf n-slots))
       ;; Adjust stack.
       (list `(sys.lap-x86:sub64 :rsp ,(* n-slots 8))))
     ;; Flush stack slots.
     (loop for value across *stack-values*
        for i from 0
        unless (equal value '(:unboxed . :home))
        collect `(sys.lap-x86:mov64 (:stack ,i) nil))
     (list `(:gc :frame :incoming-arguments :rcx
                 :layout ,(coerce (loop for value across *stack-values*
                                     collect (if (equal value '(:unboxed . :home))
                                                 0
                                                 1))
                                  'bit-vector))))))

(defun emit-dx-rest-list (lambda arg-registers)
  (let* ((regular-argument-count (+ (length (lambda-information-required-args lambda))
                                    (length (lambda-information-optional-args lambda))))
         (rest-clear-loop-head (gensym "REST-CLEAR-LOOP-HEAD"))
         (rest-loop-head (gensym "REST-LOOP-HEAD"))
         (rest-loop-end (gensym "REST-LOOP-END"))
         (rest-list-done (gensym "REST-LIST-DONE"))
         (control-slots (allocate-control-stack-slots 2 t))
         ;; Number of arguments processed and total number of arguments.
         (saved-argument-count (+ control-slots 0))
         (rest-dx-root (+ control-slots 1)))
    ;; Assemble the rest list into R13.
    ;; RCX holds the argument count.
    ;; RBX and R13 are free. Argument registers may or may not be free
    ;; depending on the number of required/optional arguments.
    ;; Number of supplied arguments.
    (emit `(sys.lap-x86:mov64 ,(control-stack-slot-ea saved-argument-count) :rcx))
    ;; Tell the GC to used the number of arguments saved on the stack. RCX will
    ;; be used later.
    (emit-gc-info :incoming-arguments saved-argument-count)
    ;; The cons cells are allocated in one single chunk.
    (emit `(sys.lap-x86:mov64 :r13 nil))
    ;; Remove required/optional arguments from the count.
    ;; If negative or zero, the &REST list is empty.
    (cond ((zerop regular-argument-count)
           (emit `(sys.lap-x86:test64 :rcx :rcx))
           (emit `(sys.lap-x86:jz ,rest-list-done)))
          (t
           (emit `(sys.lap-x86:sub64 :rcx ,(fixnum-to-raw regular-argument-count)))
           (emit `(sys.lap-x86:jle ,rest-list-done))))
    ;; Save the length.
    (emit `(sys.lap-x86:mov64 :rdx :rcx))
    ;; Double it, each cons takes two words.
    (emit `(sys.lap-x86:shl64 :rdx 1))
    ;; Add a header word and word of padding so it can be treated like a simple-vector.
    (emit `(sys.lap-x86:add64 :rdx ,(fixnum-to-raw 2)))
    ;; Fixnum to raw integer * 8.
    (emit `(sys.lap-x86:shl64 :rdx ,(- 3 sys.int::+n-fixnum-bits+)))
    ;; Allocate on the stack.
    (emit `(sys.lap-x86:sub64 :rsp :rdx))
    ;; Generate the simple-vector header. simple-vector tag is zero, doesn't need to be set here.
    (emit `(sys.lap-x86:lea64 :rdx ((:rcx 2) ,(fixnum-to-raw 1)))) ; *2 as conses are 2 words and +1 for padding word at the start.
    (emit `(sys.lap-x86:shl64 :rdx ,(- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))
    (emit `(sys.lap-x86:mov64 (:rsp) :rdx))
    ;; Clear the padding slot.
    (emit `(sys.lap-x86:mov64 (:rsp 8) 0))
    ;; For each cons, clear the car and set the cdr to the next cons.
    (emit `(sys.lap-x86:lea64 :rdi (:rsp 16)))
    (emit `(sys.lap-x86:mov64 :rdx :rcx))
    (emit rest-clear-loop-head)
    (emit `(sys.lap-x86:mov64 (:rdi 0) 0)) ; car
    (emit `(sys.lap-x86:lea64 :rax (:rdi ,(+ 16 sys.int::+tag-cons+))))
    (emit `(sys.lap-x86:mov64 (:rdi 8) :rax)) ; cdr
    (emit `(sys.lap-x86:add64 :rdi 16))
    (emit `(sys.lap-x86:sub64 :rdx ,(fixnum-to-raw 1)))
    (emit `(sys.lap-x86:ja ,rest-clear-loop-head))
    ;; Set the cdr of the final cons to NIL.
    (emit `(sys.lap-x86:mov64 (:rdi -8) nil))
    ;; Create the DX root object for the vector.
    (emit `(sys.lap-x86:lea64 :rax (:rsp ,sys.int::+tag-dx-root-object+)))
    (emit `(sys.lap-x86:mov64 ,(control-stack-slot-ea rest-dx-root) :rax))
    ;; It's now safe to write values into the list/vector.
    (emit `(sys.lap-x86:lea64 :rdi (:rsp 16)))
    ;; Add register arguments to the list.
    (loop
       for reg in arg-registers
       do (emit `(sys.lap-x86:mov64 (:rdi) ,reg)
                `(sys.lap-x86:add64 :rdi 16)
                `(sys.lap-x86:sub64 :rcx ,(fixnum-to-raw 1))
                `(sys.lap-x86:jz ,rest-loop-end)))
    ;; Now add the stack arguments.
    ;; Skip past required/optional arguments on the stack, the saved frame pointer and the return address.
    (emit `(sys.lap-x86:lea64 :rsi (:rbp ,(* (+ (max 0 (- regular-argument-count 5)) 2) 8))))
    (emit rest-loop-head)
    ;; Load from stack.
    (emit `(sys.lap-x86:mov64 :r8 (:rsi)))
    ;; Store into current car.
    (emit `(sys.lap-x86:mov64 (:rdi) :r8))
    ;; Advance.
    (emit `(sys.lap-x86:add64 :rsi 8))
    (emit `(sys.lap-x86:add64 :rdi 16))
    ;; Stop when no more arguments.
    (emit `(sys.lap-x86:sub64 :rcx ,(fixnum-to-raw 1)))
    (emit `(sys.lap-x86:jnz ,rest-loop-head))
    (emit rest-loop-end)
    ;; There were &REST arguments, create the cons.
    (emit `(sys.lap-x86:lea64 :r13 (:rsp ,(+ 16 sys.int::+tag-cons+))))
    ;; Code above jumps directly here with NIL in R13 when there are no arguments.
    (emit rest-list-done)))

(defun emit-normal-rest-list (lambda arg-registers)
  (let* ((regular-argument-count (+ (length (lambda-information-required-args lambda))
                                    (length (lambda-information-optional-args lambda))))
         (rest-loop-head (gensym "REST-LOOP-HEAD"))
         (rest-loop-test (gensym "REST-LOOP-TEST"))
         (rest-loop-end (gensym "REST-LOOP-END"))
         (reg-arg-tags (loop for reg in arg-registers collect (list (gensym))))
         ;; Number of arguments processed and total number of arguments.
         (control-slots (allocate-control-stack-slots 4 t))
         (result-head-cons (+ control-slots 1)) ; slot 1 = car, slot 0 = cdr. stack slots are numbered backwards.
         (rest-tail (+ control-slots 1)) ; only the cdr of result-head-cons is used, misuse the car as the tail of the rest list.
         (saved-argument-count (+ control-slots 2))
         (processed-argument-count (+ control-slots 3)))
    ;; Assemble the rest list into R13.
    ;; RCX holds the argument count.
    ;; RBX and R13 are free. Argument registers may or may not be free
    ;; depending on the number of required/optional arguments.
    ;; Save all the argument registers that may contain &REST arguments.
    (loop
       for slot = (find-stack-slot)
       for tag in reg-arg-tags
       for reg in arg-registers
       do
         (setf (aref *stack-values* slot) tag)
         (emit `(sys.lap-x86:mov64 (:stack ,slot) ,reg))
         (push tag *load-list*))
    ;; Number of arguments processed. Skip register arguments.
    (emit `(sys.lap-x86:mov64 (:stack ,processed-argument-count) ,(fixnum-to-raw (max regular-argument-count 5))))
    ;; Number of supplied arguments.
    (emit `(sys.lap-x86:mov64 (:stack ,saved-argument-count) :rcx))
    ;; Tell the GC to used the number of arguments saved on the stack. RCX will
    ;; be used later.
    (emit-gc-info :incoming-arguments saved-argument-count)
    ;; Create the result cell. Always create this as dynamic-extent, it
    ;; is only used during rest-list creation.
    (emit `(sys.lap-x86:lea64 :rbx (:rbp ,(+ (control-stack-frame-offset result-head-cons) sys.int::+tag-cons+))))
    ;; Stash in the tail slot.
    (emit `(sys.lap-x86:mov64 (:stack ,rest-tail) :rbx))
    ;; Add register arguments to the list.
    (loop
       for i from regular-argument-count
       for tag in reg-arg-tags
       do
         (emit `(sys.lap-x86:cmp64 (:stack ,saved-argument-count) ,(fixnum-to-raw i))
               `(sys.lap-x86:jle ,rest-loop-end))
         (load-in-r8 tag t)
         (emit `(sys.lap-x86:mov64 :r9 nil)
               `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 2))
               `(sys.lap-x86:mov64 :r13 (:function cons))
               `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
               `(sys.lap-x86:mov64 :rbx (:stack ,rest-tail))
               `(sys.lap-x86:mov64 (:cdr :rbx) :r8)
               `(sys.lap-x86:mov64 (:stack ,rest-tail) :r8)))
    ;; All register arguments are in the list.
    ;; Now add the stack arguments.
    (emit `(sys.lap-x86:mov64 :rax (:stack ,processed-argument-count))
          `(sys.lap-x86:jmp ,rest-loop-test)
          rest-loop-head)
    ;; Load current value. -5 + 2. Skip registers, return address & fp.
    (emit `(sys.lap-x86:mov64 :r8 (:rbp (:rax ,(/ 8 (ash 1 sys.int::+n-fixnum-bits+))) -24)))
    ;; Create a new cons.
    (emit `(sys.lap-x86:mov64 :r9 nil)
          `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 2))
          `(sys.lap-x86:mov64 :r13 (:function cons))
          `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
          `(sys.lap-x86:mov64 :r9 (:stack ,rest-tail))
          `(sys.lap-x86:mov64 (:cdr :r9) :r8)
          `(sys.lap-x86:mov64 (:stack ,rest-tail) :r8))
    ;; Advance processed count & test for end.
    (emit `(sys.lap-x86:add64 (:stack ,processed-argument-count) ,(fixnum-to-raw 1))
          `(sys.lap-x86:mov64 :rax (:stack ,processed-argument-count))
          rest-loop-test
          `(sys.lap-x86:cmp64 :rax (:stack ,saved-argument-count))
          `(sys.lap-x86:jl ,rest-loop-head)
          rest-loop-end)
    ;; The rest list has been created!
    ;; Pull the pointer from the CDR of the result cons.
    (emit `(sys.lap-x86:mov64 :r13 (:stack ,(1- result-head-cons))))))

(defun emit-rest-list (lambda arg-registers)
  (let* ((rest-arg (lambda-information-rest-arg lambda))
         (dx-rest (lexical-variable-dynamic-extent rest-arg)))
    (if dx-rest
        (emit-dx-rest-list lambda arg-registers)
        (emit-normal-rest-list lambda arg-registers))
    ;; The rest list has been created in R13.
    (let ((ofs (find-stack-slot)))
      (setf (aref *stack-values* ofs) (cons rest-arg :home))
      (emit `(sys.lap-x86:mov64 (:stack ,ofs) :r13)))))

(defun cg-form (form)
  (flet ((save-tag (tag)
	   (when (and tag *for-value* (not (keywordp tag)))
	     (push tag *load-list*))
	   tag))
    (etypecase form
      (cons (case (first form)
	      ((block) (save-tag (cg-block form)))
	      ((function) (save-tag (cg-function form)))
	      ((go) (cg-go form))
	      ((if) (save-tag (cg-if form)))
	      ((let) (cg-let form))
	      ((load-time-value) (error "LOAD-TIME-VALUE seen in CG-FORM."))
	      ((multiple-value-bind) (save-tag (cg-multiple-value-bind form)))
	      ((multiple-value-call) (save-tag (cg-multiple-value-call form)))
	      ((multiple-value-prog1) (save-tag (cg-multiple-value-prog1 form)))
	      ((progn) (cg-progn form))
	      ((quote) (cg-quote form))
	      ((return-from) (cg-return-from form))
	      ((setq) (cg-setq form))
	      ((tagbody) (cg-tagbody form))
	      ((the) (cg-the form))
	      ((unwind-protect) (error "UWIND-PROTECT not lowered."))
              ((sys.int::%jump-table) (cg-jump-table form))
              ((sys.c::make-dx-simple-vector) (save-tag (cg-make-dx-simple-vector form)))
	      (t (save-tag (cg-function-form form)))))
      (lexical-variable
       (save-tag (cg-variable form)))
      (lambda-information
       (let ((tag (cg-lambda form)))
         (when (and (consp tag)
                    (symbolp (car tag))
                    (null (cdr tag))
                    (not (eql 'quote (car tag))))
           (save-tag tag))
         tag)))))

(defun cg-make-dx-simple-vector (form)
  (destructuring-bind (size) (rest form)
    (assert (and (quoted-form-p size)
                 (fixnump (second size)))
            (size)
            "Size of dx simple-vector must be known at cg time.")
    (let ((words (1+ (second size))))
      (when (oddp words)
        (incf words))
      (smash-r8)
      (let ((slots (allocate-control-stack-slots words t)))
        ;; Set the header.
        (emit `(sys.lap-x86:mov64 ,(control-stack-slot-ea (+ slots words -1)) ,(ash (second size) sys.int::+object-data-shift+)))
        ;; Generate pointer.
        (emit `(sys.lap-x86:lea64 :r8 (:rbp ,(+ (control-stack-frame-offset (+ slots words -1))
                                                sys.int::+tag-object+)))))))
  (setf *r8-value* (list (gensym))))

(defun emit-nlx-thunk (thunk-name target-label multiple-values-active)
  (emit-trailer (thunk-name nil)
    (if multiple-values-active
        (emit-gc-info :block-or-tagbody-thunk :rax :multiple-values 0)
        (emit-gc-info :block-or-tagbody-thunk :rax))
    (emit `(sys.lap-x86:mov64 :csp (:rax 16))
          `(sys.lap-x86:mov64 :cfp (:rax 24)))
    (dolist (slot (first *active-nl-exits*))
      (emit `(sys.lap-x86:mov64 (:stack ,slot) nil)))
    (emit `(sys.lap-x86:jmp ,target-label))))

(defun cg-block (form)
  (let* ((info (second form))
         (exit-label (gensym "block"))
         (thunk-label (gensym "block-thunk"))
         (escapes (block-information-env-var info))
         (*for-value* *for-value*)
         (*active-nl-exits* (list* '() *active-nl-exits*)))
    ;; Allowing predicate values here is too complicated.
    (when (eql *for-value* :predicate)
      (setf *for-value* t))
    ;; Disable tail calls when the BLOCK escapes.
    ;; TODO: When tailcalling, configure the escaping block so
    ;; control returns to the caller.
    (when (and escapes (eql *for-value* :tail))
      (setf *for-value* :multiple))
    (setf (block-information-return-mode info) *for-value*
          (block-information-count info) 0)
    (when escapes
      (smash-r8)
      (let ((slot (find-stack-slot))
            (control-info (allocate-control-stack-slots 4)))
        (setf (aref *stack-values* slot) (cons info :home))
        ;; Construct jump info.
        (emit `(sys.lap-x86:lea64 :rax (:rip ,thunk-label))
              `(sys.lap-x86:mov64 ,(control-stack-slot-ea (+ control-info 3)) :rax)
              `(sys.lap-x86:gs)
              `(sys.lap-x86:mov64 :rax (,+binding-stack-gs-offset+))
              `(sys.lap-x86:mov64 ,(control-stack-slot-ea (+ control-info 2)) :rax)
              `(sys.lap-x86:mov64 ,(control-stack-slot-ea (+ control-info 1)) :csp)
              `(sys.lap-x86:mov64 ,(control-stack-slot-ea (+ control-info 0)) :cfp)
              ;; Save pointer to info
              `(sys.lap-x86:lea64 :rax ,(control-stack-slot-ea (+ control-info 3)))
              `(sys.lap-x86:mov64 (:stack ,slot) :rax))))
    (prog1
        (let* ((*rename-list* (cons (list (second form) exit-label) *rename-list*))
               (stack-slots (set-up-for-branch))
               (tag (cg-form `(progn ,@(cddr form)))))
          (cond ((and *for-value* tag (/= (block-information-count info) 0))
                 ;; Returning a value, exit is reached normally and there were return-from forms reached.
                 ;; Unify the results, so :MULTIPLE is always returned.
                 (let ((return-mode nil))
                   (ecase *for-value*
                     ((:multiple :tail)
                      (unless (eql tag :multiple)
                        (load-multiple-values tag)
                        (smash-r8))
                      (setf return-mode :multiple))
                     (t (load-in-r8 tag t)
                        (smash-r8)
                        (setf return-mode (list (gensym)))))
                   (emit exit-label)
                   (when (member *for-value* '(:multiple :tail))
                     (emit-gc-info :multiple-values 0))
                   (setf *stack-values* (copy-stack-values stack-slots)
                         *r8-value* return-mode)))
                ((and *for-value* tag)
                 ;; Returning a value, exit is reached normally, but no return-from forms were reached.
                 tag)
                ((and *for-value* (/= (block-information-count info) 0))
                 ;; Returning a value, exit is not reached normally, but there were return-from forms reached.
                 (smash-r8)
                 (emit exit-label)
                 (when (member *for-value* '(:multiple :tail))
                   (emit-gc-info :multiple-values 0))
                 (setf *stack-values* (copy-stack-values stack-slots)
                       *r8-value* (if (member *for-value* '(:multiple :tail))
                                      :multiple
                                      (list (gensym)))))
                ((/= (block-information-count info) 0)
                 ;; Not returning a value, but there were return-from forms reached.
                 (smash-r8)
                 (emit exit-label)
                 (setf *stack-values* (copy-stack-values stack-slots)
                       *r8-value* (list (gensym))))
                ;; No value returned, no return-from forms reached.
                (t nil)))
      (when escapes
        (emit-nlx-thunk thunk-label exit-label (member *for-value* '(:multiple :tail)))))))

(defun cg-function (form)
  (let ((name (second form))
        (undefined (gensym "function-undefined"))
        (resume (gensym "function-resume")))
    (smash-r8)
    (emit-trailer (undefined)
      ;; When the function is undefined, fall back on FDEFINITION.
      (load-constant :rcx 1)
      (load-constant :r8 name)
      (emit `(sys.lap-x86:mov64 :r13 (:function fdefinition))
            `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
            `(sys.lap-x86:jmp ,resume)))
    (emit `(sys.lap-x86:mov64 :r8 (:function ,name))
          `(sys.lap-x86:mov64 :r8 ,(object-ea :r8 :slot sys.int::+fref-function+))
          `(sys.lap-x86:cmp64 :r8 :undefined-function)
          `(sys.lap-x86:je ,undefined)
          resume)
    (setf *r8-value* (list (gensym)))))

(defun cg-go (form)
  (let ((tag (assoc (second form) *rename-list*)))
    (smash-r8)
    (cond (tag ;; Local jump.
           (emit `(sys.lap-x86:jmp ,(second tag))))
          (t ;; Non-local exit.
           (let ((tagbody-tag (let ((*for-value* t))
                                (cg-form (third form)))))
             (load-in-reg :rax tagbody-tag t)
             ;; RAX holds the tagbody info.
             (emit
              ;; Get R8 as well.
              `(sys.lap-x86:xor32 :r8d :r8d)
              ;; GO GO GO!
              `(sys.lap-x86:mov64 :rdx (:rax 0))
              `(sys.lap-x86:add64 :rdx (:rdx ,(* (position (second form)
                                                           (tagbody-information-go-tags
                                                            (go-tag-tagbody (second form))))
                                                 8)))
              `(sys.lap-x86:jmp :rdx)))))
    'nil))

(defun branch-to (label) (declare (ignore label)))
(defun emit-label (label)
  (emit label))

(defun tag-saved-on-stack-p (tag)
  (dotimes (i (length *stack-values*) nil)
    (let ((x (aref *stack-values* i)))
      (when (or (eq tag x)
		(and (consp tag) (consp x)
		     (eql (car tag) (car x))
		     (eql (cdr tag) (cdr x))))
	(return t)))))

(defun set-up-for-branch ()
  ;; Save variables on the load list that might be modified to the stack.
  (smash-r8)
  (dolist (l *load-list*)
    (when (and (consp l) (lexical-variable-p (car l))
	       (not (eql (lexical-variable-write-count (car l)) 0)))
      ;; Don't save if there is something satisfying this already.
      (multiple-value-bind (loc true-tag)
	  (value-location l)
	(declare (ignore loc))
	(unless (tag-saved-on-stack-p true-tag)
	  (load-in-r8 l nil)
	  (smash-r8 t)))))
  (let ((new-values (make-array (length *stack-values*) :initial-contents *stack-values*)))
    ;; Now flush any values that aren't there to satisfy the load list.
    (dotimes (i (length new-values))
      (when (condemed-p (aref new-values i))
	(setf (aref new-values i) nil)))
    new-values))

(defun copy-stack-values (values)
  "Copy the VALUES array, ensuring that it's at least as long as the current *stack-values*, is adjustable and contains all unboxed slots."
  (let ((new (make-array (length *stack-values*) :adjustable t :fill-pointer t :initial-element nil)))
    (setf (subseq new 0) values)
    (dotimes (i (length *stack-values*))
      (when (equal (aref *stack-values* i) '(:unboxed . :home))
        (assert (or (null (aref new i))
                    (equal (aref new i) '(:unboxed . :home))))
        (setf (aref new i) '(:unboxed . :home))))
    new))

;;; (predicate inverse jump-instruction cmov-instruction)
(defstruct predicate-instruction
  inverse jump-instruction cmov-instruction)
(defparameter *predicate-instructions-1*
  '((:o  :no  sys.lap-x86:jo   sys.lap-x86:cmov64o)
    (:no :o   sys.lap-x86:jno  sys.lap-x86:cmov64no)
    (:b  :nb  sys.lap-x86:jb   sys.lap-x86:cmov64b)
    (:nb :b   sys.lap-x86:jnb  sys.lap-x86:cmov64nb)
    (:c  :nc  sys.lap-x86:jc   sys.lap-x86:cmov64c)
    (:nc :c   sys.lap-x86:jnc  sys.lap-x86:cmov64nc)
    (:ae :nae sys.lap-x86:jae  sys.lap-x86:cmov64ae)
    (:nae :ae sys.lap-x86:jnae sys.lap-x86:cmov64nae)
    (:e  :ne  sys.lap-x86:je   sys.lap-x86:cmov64e)
    (:ne :e   sys.lap-x86:jne  sys.lap-x86:cmov64ne)
    (:z  :nz  sys.lap-x86:jz   sys.lap-x86:cmov64z)
    (:nz :z   sys.lap-x86:jnz  sys.lap-x86:cmov64nz)
    (:be :nbe sys.lap-x86:jbe  sys.lap-x86:cmov64be)
    (:nbe :be sys.lap-x86:jnbe sys.lap-x86:cmov64nbe)
    (:a  :na  sys.lap-x86:ja   sys.lap-x86:cmov64a)
    (:na :a   sys.lap-x86:jna  sys.lap-x86:cmov64na)
    (:s  :ns  sys.lap-x86:js   sys.lap-x86:cmov64s)
    (:ns :s   sys.lap-x86:jns  sys.lap-x86:cmov64ns)
    (:p  :np  sys.lap-x86:jp   sys.lap-x86:cmov64p)
    (:np :p   sys.lap-x86:jnp  sys.lap-x86:cmov64np)
    (:pe :po  sys.lap-x86:jpe  sys.lap-x86:cmov64pe)
    (:po :pe  sys.lap-x86:jpo  sys.lap-x86:cmov64po)
    (:l  :nl  sys.lap-x86:jl   sys.lap-x86:cmov64l)
    (:nl :l   sys.lap-x86:jnl  sys.lap-x86:cmov64nl)
    (:ge :nge sys.lap-x86:jge  sys.lap-x86:cmov64ge)
    (:nge :ge sys.lap-x86:jnge sys.lap-x86:cmov64nge)
    (:le :nle sys.lap-x86:jle  sys.lap-x86:cmov64le)
    (:nle :le sys.lap-x86:jnle sys.lap-x86:cmov64nle)
    (:g  :ng  sys.lap-x86:jg   sys.lap-x86:cmov64g)
    (:ng :g   sys.lap-x86:jng  sys.lap-x86:cmov64ng)))
(defparameter *predicate-instructions*
  (let ((ht (make-hash-table :test 'eq)))
    (mapc (lambda (i)
            (setf (gethash (first i) ht)
                  (make-predicate-instruction
                   :inverse (second i)
                   :jump-instruction (third i)
                   :cmov-instruction (fourth i))))
          *predicate-instructions-1*)
    ht))

(defun predicate-info (pred)
  (or (gethash pred *predicate-instructions*)
      (error "Unknown predicate ~S." pred)))

(defun invert-predicate (pred)
  (predicate-instruction-inverse (predicate-info pred)))

(defun load-predicate (pred)
  (smash-r8)
  (emit `(sys.lap-x86:mov64 :r8 nil)
        `(sys.lap-x86:mov64 :r9 t)
        `(,(predicate-instruction-cmov-instruction (predicate-info pred)) :r8 :r9)))

(defun predicate-result (pred)
  (cond ((eql *for-value* :predicate)
         pred)
        (t (load-predicate pred)
           (setf *r8-value* (list (gensym))))))

(defun cg-if (form)
  (let* ((else-label (gensym))
	 (end-label (gensym))
	 (test-tag (let ((*for-value* :predicate))
		     (cg-form (second form))))
	 (branch-count 0)
	 (stack-slots (set-up-for-branch))
	 (loc (when (and test-tag (not (keywordp test-tag)))
                (value-location test-tag t))))
    (when (null test-tag)
      (return-from cg-if))
    (cond ((keywordp test-tag)) ; Nothing for predicates.
          ((and (consp loc) (eq (first loc) :stack))
	   (emit `(sys.lap-x86:cmp64 (:stack ,(second loc)) nil)))
	  (t (load-in-r8 test-tag)
	     (emit `(sys.lap-x86:cmp64 :r8 nil))))
    (let ((r8-at-cond *r8-value*)
	  (stack-at-cond (make-array (length *stack-values*) :initial-contents *stack-values*)))
      ;; This is a little dangerous and relies on SET-UP-FOR-BRANCH not
      ;; changing the flags.
      (cond ((keywordp test-tag)
             ;; Invert the sense.
             (emit `(,(predicate-instruction-jump-instruction
                       (predicate-info (invert-predicate test-tag))) ,else-label)))
            (t (emit `(sys.lap-x86:je ,else-label))))
      (branch-to else-label)
      (let ((tag (cg-form (third form))))
	(when tag
	  (when *for-value*
            (case *for-value*
              ((:multiple :tail) (load-multiple-values tag))
              (:predicate (if (keywordp tag)
                              (load-predicate tag)
                              (load-in-r8 tag t)))
              (t (load-in-r8 tag t))))
	  (emit `(sys.lap-x86:jmp ,end-label))
	  (incf branch-count)
	  (branch-to end-label)))
      (setf *r8-value* r8-at-cond
	    *stack-values* (copy-stack-values stack-at-cond))
      (emit-label else-label)
      (emit-gc-info)
      (let ((tag (cg-form (fourth form))))
	(when tag
	  (when *for-value*
            (case *for-value*
              ((:multiple :tail) (load-multiple-values tag))
              (:predicate (if (keywordp tag)
                              (load-predicate tag)
                              (load-in-r8 tag t)))
              (t (load-in-r8 tag t))))
	  (incf branch-count)
	  (branch-to end-label)))
      (emit-label end-label)
      (setf *stack-values* (copy-stack-values stack-slots))
      (unless (zerop branch-count)
        (cond ((member *for-value* '(:multiple :tail))
               (emit-gc-info :multiple-values 0)
               (setf *r8-value* :multiple))
              (t (emit-gc-info)
                 (setf *r8-value* (list (gensym)))))))))

(defun localp (var)
  (or (null (lexical-variable-used-in var))
      (and (null (cdr (lexical-variable-used-in var)))
	   (eq (car (lexical-variable-used-in var)) (lexical-variable-definition-point var)))))

(defun cg-let (form)
  (let* ((bindings (second form))
         (variables (mapcar 'first bindings))
         (body (cddr form)))
    ;; Ensure there are no non-local variables or special bindings.
    (assert (every (lambda (x)
                     (and (lexical-variable-p x)
                          (localp x)))
                   variables))
    (dolist (b (second form))
      (let* ((var (first b))
             (init-form (second b)))
        (cond ((zerop (lexical-variable-use-count var))
               (let ((*for-value* nil))
                 (cg-form init-form)))
              (t
               (let ((slot (find-stack-slot)))
                 (setf (aref *stack-values* slot) (cons var :home))
                 (let* ((*for-value* t)
                        (tag (cg-form init-form)))
                   (load-in-r8 tag t)
                   (setf *r8-value* (cons var :dup))
                   (emit `(sys.lap-x86:mov64 (:stack ,slot) :r8))))))))
    (cg-form `(progn ,@body))))

(defun gensym-many (things)
  (loop for x in things collect (gensym)))

(defun cg-multiple-value-bind (form)
  (let ((variables (second form))
        (value-form (third form))
        (body (cdddr form)))
    ;; Ensure there are no non-local variables or special bindings.
    (assert (every (lambda (x)
                     (and (lexical-variable-p x)
                          (localp x)))
                   variables))
    ;; Initialize local variables to NIL.
    (dolist (var variables)
      (when (not (zerop (lexical-variable-use-count var)))
        (let ((slot (find-stack-slot)))
          (setf (aref *stack-values* slot) (cons var :home))
          (emit `(sys.lap-x86:mov64 (:stack ,slot) nil)))))
    ;; Compile the value-form.
    (let ((value-tag (let ((*for-value* :multiple))
                       (cg-form value-form))))
      (load-multiple-values value-tag))
    ;; Bind variables.
    (let* ((jump-targets (gensym-many variables))
           (no-vals-label (gensym))
           (var-count (length variables))
           (value-locations (nreverse (subseq '(:r8 :r9 :r10 :r11 :r12) 0 (min 5 var-count)))))
      (dotimes (i (- var-count 5))
        (push i value-locations))
      (dotimes (i var-count)
        (emit `(sys.lap-x86:cmp64 :rcx ,(fixnum-to-raw (- var-count i)))
              `(sys.lap-x86:jae ,(nth i jump-targets))))
      (emit `(sys.lap-x86:jmp ,no-vals-label))
      (loop for var in (reverse variables)
         for label in jump-targets do
           (emit label)
           (cond ((zerop (lexical-variable-use-count var))
                  (pop value-locations))
                 (t (let ((register (cond ((integerp (first value-locations))
                                           (emit `(sys.lap-x86:gs)
                                                 `(sys.lap-x86:mov64 :r13 (,(+ (- 8 sys.int::+tag-object+)
                                                                               (* (+ #+(or)sys.int::+stack-group-offset-mv-slots+
                                                                                     32 ; fixme. should be +thread-mv-slots-start+
                                                                                     (pop value-locations))
                                                                                  8)))))
                                           :r13)
                                          (t (pop value-locations)))))
                      (emit `(sys.lap-x86:mov64 (:stack ,(position (cons var :home)
                                                                   *stack-values*
                                                                   :test 'equal))
                                                ,register))))))
      (emit no-vals-label))
    (emit-gc-info)
    (cg-form `(progn ,@body))))

(defun emit-funcall-common ()
  "Emit the common code for funcall, argument registers must
be set up and the function must be in RBX.
Returns an appropriate tag."
  (emit `(sys.lap-x86:call ,(object-ea :rbx :slot sys.int::+function-entry-point+)))
  (cond ((member *for-value* '(:multiple :tail))
         (emit-gc-info :multiple-values 0)
         :multiple)
        (t (emit-gc-info)
           (setf *r8-value* (list (gensym))))))

(defun cg-multiple-value-call (form)
  (let ((function (second form))
        (value-forms (cddr form)))
    (cond ((null value-forms)
           ;; Just like a regular call.
           (cg-function-form `(funcall ,function)))
          ((null (cdr value-forms))
           ;; Single value form.
           (let ((fn-tag (let ((*for-value* t)) (cg-form `(sys.int::%coerce-to-callable ,function)))))
             (when (not fn-tag)
               (return-from cg-multiple-value-call nil))
             (let ((value-tag (let ((*for-value* :multiple))
                                (cg-form (first value-forms)))))
               (case value-tag
                 ((nil) (return-from cg-multiple-value-call nil))
                 ((:multiple)
                  (let ((stack-pointer-save-area (allocate-control-stack-slots 1)))
                    (emit `(sys.lap-x86:mov64 ,(control-stack-slot-ea stack-pointer-save-area) :rsp))
                    ;; Copy values in the sg-mv area to the stack. RCX holds the number of values to copy +5
                    (let ((loop-head (gensym))
                          (loop-exit (gensym))
                          (clear-loop-head (gensym)))
                      ;; RAX = n values to copy (count * 8).
                      (emit `(sys.lap-x86:lea64 :rax ((:rcx ,(/ 8 (ash 1 sys.int::+n-fixnum-bits+))) ,(- (* 5 8))))
                            `(sys.lap-x86:cmp64 :rax 0)
                            `(sys.lap-x86:jle ,loop-exit)
                            `(sys.lap-x86:sub64 :rsp :rax)
                            `(sys.lap-x86:and64 :rsp ,(lognot 8))
                            ;; Clear stack slots.
                            `(sys.lap-x86:mov64 :rdx :rax)
                            clear-loop-head
                            `(sys.lap-x86:mov64 (:rsp :rdx -8) 0)
                            `(sys.lap-x86:sub64 :rdx 8)
                            `(sys.lap-x86:jnz ,clear-loop-head)
                            ;; Copy values.
                            `(sys.lap-x86:mov64 :rdi :rsp)
                            `(sys.lap-x86:mov32 :esi ,(+ (- 8 sys.int::+tag-object+)
                                                         ;; fixme. should be +thread-mv-slots-start+
                                                         (* #+(or)sys.int::+stack-group-offset-mv-slots+ 32 8))))
                      ;; Switch to the right GC mode.
                      (emit-gc-info :pushed-values -5 :pushed-values-register :rcx :multiple-values 0)
                      (emit loop-head
                            `(sys.lap-x86:gs)
                            `(sys.lap-x86:mov64 :rbx (:rsi))
                            `(sys.lap-x86:mov64 (:rdi) :rbx)
                            `(sys.lap-x86:add64 :rdi 8)
                            `(sys.lap-x86:add64 :rsi 8)
                            `(sys.lap-x86:sub64 :rax 8)
                            `(sys.lap-x86:jae ,loop-head)
                            loop-exit)
                      ;; All done with the MV area.
                      (emit-gc-info :pushed-values -5 :pushed-values-register :rcx))
                    (smash-r8)
                    (load-in-reg :rbx fn-tag t)
                    (prog1 (emit-funcall-common)
                      (emit `(sys.lap-x86:mov64 :rsp ,(control-stack-slot-ea stack-pointer-save-area))))))
                 (t ;; Single value.
                  (load-in-reg :rbx fn-tag t)
                  (load-constant :rcx 1)
                  (load-in-reg :r8 value-tag t)
                  (emit-funcall-common))))))
          (t (error "M-V-CALL with >1 form not lowered")))))

(defun cg-multiple-value-prog1 (form)
  (cond
    ((null *for-value*)
     ;; Not for value
     (cg-progn form))
    (t (let ((tag (let ((*for-value* (case *for-value*
                                       (:predicate t)
                                       (:tail :multiple)
                                       (t *for-value*))))
                    (cg-form (second form))))
             (sv-save-area (allocate-control-stack-slots 1 t))
             (saved-stack-pointer (allocate-control-stack-slots 1))
             (save-done (gensym "VALUES-SAVE-DONE"))
             (save-loop-head (gensym "VALUES-SAVE-LOOP")))
         (smash-r8)
         (when (eql tag :multiple)
           ;; Allocate an appropriately sized DX simple vector.
           ;; Add one for the header, then round the count up to an even number.
           (emit `(sys.lap-x86:lea64 :rax (:rcx ,(fixnum-to-raw 2))))
           (emit `(sys.lap-x86:and64 :rax ,(fixnum-to-raw (lognot 1))))
           ;; Save RSP.
           (emit `(sys.lap-x86:mov64 (:stack ,saved-stack-pointer) :rsp))
           ;; Adjust RSP. rax to raw * 8.
           (emit `(sys.lap-x86:shl64 :rax ,(- 3 sys.int::+n-fixnum-bits+)))
           (emit `(sys.lap-x86:sub64 :rsp :rax))
           ;; Write the simple-vector header.
           (emit `(sys.lap-x86:mov64 :rax :rcx))
           (emit `(sys.lap-x86:shl64 :rax ,(- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))
           (emit `(sys.lap-x86:mov64 (:rsp) :rax))
           ;; Clear the SV body. Don't modify RCX, needed for MV GC info.
           (let ((clear-loop-head (gensym "MVP1-CLEAR-LOOP"))
                 (clear-loop-end (gensym "MVP1-CLEAR-LOOP-END")))
             (emit `(sys.lap-x86:mov64 :rdx :rcx))
             (emit `(sys.lap-x86:test64 :rdx :rdx))
             (emit `(sys.lap-x86:jz ,clear-loop-end))
             (emit `(sys.lap-x86:lea64 :rdi (:rsp 8)))
             (emit `(sys.lap-x86:xor32 :eax :eax))
             (emit clear-loop-head)
             (emit `(sys.lap-x86:stos64))
             (emit `(sys.lap-x86:sub64 :rdx ,(fixnum-to-raw 1)))
             (emit `(sys.lap-x86:jnz ,clear-loop-head))
             (emit clear-loop-end))
           ;; Create & save the DX root value.
           (emit `(sys.lap-x86:lea64 :rax (:rsp ,sys.int::+tag-dx-root-object+)))
           (emit `(sys.lap-x86:mov64 (:stack ,sv-save-area) :rax))
           ;; Save MV registers.
           (loop
              for reg in '(:r8 :r9 :r10 :r11 :r12)
              for offset from 0
              do
                (emit `(sys.lap-x86:cmp64 :rcx ,(fixnum-to-raw offset)))
                (emit `(sys.lap-x86:jle ,save-done))
                ;; 1+ to skip header.
                (emit `(sys.lap-x86:mov64 (:rsp ,(* (1+ offset) 8)) ,reg)))
           ;; Save values in the MV area.
           ;; Number of values remaining.
           (emit `(sys.lap-x86:mov64 :rax :rcx))
           (emit `(sys.lap-x86:sub64 :rax ,(fixnum-to-raw 5)))
           (emit `(sys.lap-x86:jle ,save-done))
           ;; Save into the simple-vector.
           (emit `(sys.lap-x86:lea64 :rdi (:rsp ,(* 6 8)))) ; skip header and registers.
           ;; Load from the MV area.
           (emit `(sys.lap-x86:mov64 :rsi ,(+ (- 8 sys.int::+tag-object+)
                                              ;; fixme. should be +thread-mv-slots-start+
                                              (* #+(or)sys.int::+stack-group-offset-mv-slots+ 32 8))))
           ;; Save the values into a simple-vector.
           (emit save-loop-head)
           (emit `(sys.lap-x86:gs))
           (emit `(sys.lap-x86:mov64 :rbx (:rsi)))
           (emit `(sys.lap-x86:mov64 (:rdi) :rbx))
           (emit `(sys.lap-x86:add64 :rsi 8))
           (emit `(sys.lap-x86:add64 :rdi 8))
           (emit `(sys.lap-x86:sub64 :rax ,(fixnum-to-raw 1)))
           (emit `(sys.lap-x86:jnz ,save-loop-head))
           ;; Finished saving values. Turn MV GC mode off.
           (emit save-done)
           (emit-gc-info)
           (add-dx-root sv-save-area))
         (let ((*for-value* nil))
           (when (not (cg-progn `(progn ,@(cddr form))))
             ;; No return.
             (setf *load-list* (delete tag *load-list*))
             (return-from cg-multiple-value-prog1 'nil)))
         (smash-r8)
         ;; Drop the tag from the load-list to prevent duplicates caused by cg-form
         (setf *load-list* (delete tag *load-list*))
         (when (eql tag :multiple)
           ;; Create a normal object from the saved dx root.
           (emit `(sys.lap-x86:mov64 :rax (:stack ,sv-save-area)))
           (emit `(sys.lap-x86:lea64 :r8 (:rax ,(- sys.int::+tag-object+
                                                   sys.int::+tag-dx-root-object+))))
           ;; Call helper.
           (emit `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 1)))
           (emit `(sys.lap-x86:mov64 :r13 (:function sys.int::values-simple-vector)))
           (emit `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+)))
           (emit-gc-info :multiple-values 0)
           ;; Kill the dx root and restore the old stack pointer.
           (emit `(sys.lap-x86:mov64 (:stack ,sv-save-area) nil))
           (emit `(sys.lap-x86:mov64 :rsp (:stack ,saved-stack-pointer))))
         tag))))

(defun cg-progn (form)
  (if (rest form)
      (do ((i (rest form) (rest i)))
	  ((endp (rest i))
	   (cg-form (first i)))
	(let* ((*for-value* nil)
	       (tag (cg-form (first i))))
	  (when (null tag)
	    (return-from cg-progn 'nil))))
      (cg-form ''nil)))

(defun cg-quote (form)
  form)

(defun cg-return-from (form)
  (let* ((local-info (assoc (second form) *rename-list*))
         (*for-value* (block-information-return-mode (second form)))
         (target-tag (when (not local-info)
                       (let ((*for-value* t))
                         (cg-form (fourth form)))))
         (tag (cg-form (third form))))
    (unless tag (return-from cg-return-from nil))
    (cond ((member *for-value* '(:multiple :tail))
           (load-multiple-values tag))
          (*for-value*
           (load-in-r8 tag t)))
    (incf (block-information-count (second form)))
    (smash-r8)
    (cond (local-info ;; Local jump.
           (emit `(sys.lap-x86:jmp ,(second local-info))))
          (t ;; Non-local exit.
           (load-in-reg :rax target-tag t)
           (emit `(sys.lap-x86:jmp (:rax 0)))))
    'nil))

(defun find-variable-home (var)
  (dotimes (i (length *stack-values*)
	    (error "No home for ~S?" var))
    (let ((x (aref *stack-values* i)))
      (when (and (consp x) (eq (car x) var) (eq (cdr x) :home))
	(return i)))))

(defun cg-setq (form)
  (let ((var (second form))
	(val (third form)))
    (assert (localp var))
    ;; Copy var if there are unsatisfied tags on the load list.
    (dolist (l *load-list*)
      (when (and (consp l) (eq (car l) var))
        ;; Don't save if there is something satisfying this already.
        (multiple-value-bind (loc true-tag)
            (value-location l)
          (declare (ignore loc))
          (unless (tag-saved-on-stack-p true-tag)
            (load-in-r8 l nil)
            (smash-r8 t)))))
    (let ((tag (let ((*for-value* t)) (cg-form val)))
          (home (find-variable-home var)))
      (when (null tag)
        (return-from cg-setq))
      (load-in-r8 tag t)
      (emit `(sys.lap-x86:mov64 (:stack ,home) :r8))
      (setf *r8-value* (cons var :dup))
      (cons var (incf *run-counter* 2)))))

(defun tagbody-localp (info)
  (dolist (tag (tagbody-information-go-tags info) t)
    (unless (or (null (go-tag-used-in tag))
		(and (null (cdr (go-tag-used-in tag)))
		     (eq (car (go-tag-used-in tag)) (tagbody-information-definition-point info))))
      (return nil))))

;;; FIXME: Everything must return a valid tag if control flow follows.
(defun cg-tagbody (form)
  (let ((*for-value* nil)
	(stack-slots nil)
	(*rename-list* *rename-list*)
	(last-value t)
        (escapes (not (tagbody-localp (second form))))
        (jump-table (gensym))
        (tag-labels (mapcar (lambda (tag)
                              (declare (ignore tag))
                              (gensym))
                            (tagbody-information-go-tags (second form))))
        (thunk-labels (mapcar (lambda (tag)
                                (declare (ignore tag))
                                (gensym))
                              (tagbody-information-go-tags (second form))))
        (*active-nl-exits* (list* '() *active-nl-exits*)))
    (when escapes
      (smash-r8)
      (let ((slot (find-stack-slot))
            (control-info (allocate-control-stack-slots 4)))
        ;; Construct jump info.
        (emit `(sys.lap-x86:lea64 :rax (:rip ,jump-table))
              `(sys.lap-x86:mov64 ,(control-stack-slot-ea (+ control-info 3)) :rax)
              `(sys.lap-x86:gs)
              `(sys.lap-x86:mov64 :rax (,+binding-stack-gs-offset+))
              `(sys.lap-x86:mov64 ,(control-stack-slot-ea (+ control-info 2)) :rax)
              `(sys.lap-x86:mov64 ,(control-stack-slot-ea (+ control-info 1)) :csp)
              `(sys.lap-x86:mov64 ,(control-stack-slot-ea (+ control-info 0)) :cfp)
              ;; Save in the environment.
              `(sys.lap-x86:lea64 :rax ,(control-stack-slot-ea (+ control-info 3)))
              `(sys.lap-x86:mov64 (:stack ,slot) :rax))
        (setf (aref *stack-values* slot) (cons (second form) :home))))
    (setf stack-slots (set-up-for-branch))
    (mapcar (lambda (tag label)
              (push (list tag label) *rename-list*))
            (tagbody-information-go-tags (second form)) tag-labels)
    (dolist (stmt (cddr form))
      (if (go-tag-p stmt)
	  (progn
	    (smash-r8)
	    (setf *stack-values* (copy-stack-values stack-slots))
	    (setf last-value t)
	    (emit (second (assoc stmt *rename-list*))))
	  (setf last-value (cg-form stmt))))
    (when escapes
      ;; Emit the jump-table.
      ;; TODO: Prune local labels out.
      (emit-trailer (jump-table nil)
        (dolist (i thunk-labels)
          (emit `(:d64/le (- ,i ,jump-table)))))
      ;; And the all the thunks.
      (loop
         for thunk in thunk-labels
         for label in tag-labels do
           (emit-nlx-thunk thunk label nil)))
    (if last-value
	''nil
	'nil)))

(defun cg-the (form)
  (cg-form (third form)))

(defun value-location (tag &optional kill)
  (when kill
    (setf *load-list* (delete tag *load-list*)))
  (cond ((eq (car tag) 'quote)
	 (values (if (and (consp *r8-value*)
			  (eq (car *r8-value*) 'quote)
			  (eql (cadr tag) (cadr *r8-value*)))
		     :r8
		     tag)
		 tag))
	((null (cdr tag))
	 (values (if (eq tag *r8-value*)
		     :r8
		     (dotimes (i (length *stack-values*)
			       (error "Cannot find tag ~S." tag))
		       (when (eq tag (aref *stack-values* i))
			 (return (list :stack i)))))
		 tag))
	((lexical-variable-p (car tag))
	 ;; Search for the lowest numbered time that is >= to the tag time.
	 (let ((best (when (and (consp *r8-value*) (eq (car *r8-value*) (car tag))
				(integerp (cdr *r8-value*)) (>= (cdr *r8-value*) (cdr tag)))
		       *r8-value*))
	       (best-loc :r8)
	       (home-loc nil)
	       (home nil))
	   (dotimes (i (length *stack-values*))
	     (let ((val (aref *stack-values* i)))
	       (when (and (consp val) (eq (car val) (car tag)))
		 (cond ((eq (cdr val) :home)
			(setf home (cons (car val) *run-counter*)
			      home-loc (list :stack i)))
		       ((and (integerp (cdr val)) (>= (cdr val) (cdr tag))
			     (or (null best)
				 (< (cdr val) (cdr best))))
			(setf best val
			      best-loc (list :stack i)))))))
	   (values (or (when best
			 best-loc)
		       ;; R8 might hold a duplicate (thanks to let or setq), use that instead of home.
		       (when (and *r8-value* (consp *r8-value*) (eq (car *r8-value*) (car tag)) (eq (cdr *r8-value*) :dup))
			 :r8)
		       home-loc
		       (error "Cannot find tag ~S." tag))
		   (or best
		       (when (and *r8-value* (consp *r8-value*) (eq (car *r8-value*) (car tag)) (eq (cdr *r8-value*) :dup))
			 *r8-value*)
		       home))))
	(t (error "What kind of tag is this? ~S" tag))))

(defun condemed-p (tag)
  (cond ((eq (cdr tag) :home)
	 nil)
	((eq (cdr tag) :dup)
	 t)
	(t (dolist (v *load-list* t)
	     (when (eq (first tag) (first v))
	       (if (null (rest tag))
		   (return nil)
		   ;; Figure out the best tag that satisfies this load.
		   (let ((best (when (and (consp *r8-value*) (eq (car *r8-value*) (car tag))
					  (integerp (cdr *r8-value*)) (>= (cdr *r8-value*) (cdr tag)))
				 *r8-value*)))
		     (dotimes (i (length *stack-values*))
		       (let ((val (aref *stack-values* i)))
			 (when (and (consp val) (eq (car val) (car v))
				    (integerp (cdr val)) (>= (cdr val) (cdr v))
				    (or (null best)
					(< (cdr val) (cdr best))))
			   (setf best val))))
		     (when (eq best tag)
		       (return nil)))))))))

(defun find-stack-slot ()
  ;; Find a free stack slot, or allocate a new one.
  (dotimes (i (length *stack-values*)
	    (vector-push-extend nil *stack-values*))
    (when (or (null (aref *stack-values* i))
	      (condemed-p (aref *stack-values* i)))
      (setf (aref *stack-values* i) nil)
      (return i))))

(defun smash-r8 (&optional do-not-kill-r8)
  "Check if the value in R8 is on the load-list and flush it to the stack if it is."
  ;; Avoid flushing if it's already on the stack.
  (when (and *r8-value*
             (not (eql *r8-value* :multiple))
	     (not (condemed-p *r8-value*))
	     (not (tag-saved-on-stack-p *r8-value*)))
    (let ((slot (find-stack-slot)))
      (setf (aref *stack-values* slot) *r8-value*)
      (emit `(sys.lap-x86:mov64 (:stack ,slot) :r8))))
  (unless do-not-kill-r8
    (setf *r8-value* nil)))

(defun load-constant (register value)
  (cond ((eql value 0)
	 (emit `(sys.lap-x86:xor64 ,register ,register)))
	((eq value 'nil)
	 (emit `(sys.lap-x86:mov64 ,register nil)))
	((eq value 't)
	 (emit `(sys.lap-x86:mov64 ,register t)))
	((fixnump value)
	 (emit `(sys.lap-x86:mov64 ,register ,(fixnum-to-raw value))))
	((characterp value)
	 (emit `(sys.lap-x86:mov64 ,register ,(character-to-raw value))))
	(t (emit `(sys.lap-x86:mov64 ,register (:constant ,value))))))

(defun load-multiple-values (tag)
  (cond ((eql tag :multiple))
        (t (load-in-r8 tag t)
           (emit `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 1))))))

(defun load-in-r8 (tag &optional kill)
  (multiple-value-bind (loc true-tag)
      (value-location tag nil)
    (unless (eq loc :r8)
      (smash-r8)
      (ecase (first loc)
	((quote) (load-constant :r8 (second loc)))
	((:stack) (emit `(sys.lap-x86:mov64 :r8 (:stack ,(second loc))))))
      (setf *r8-value* true-tag))
    (when kill
      (setf *load-list* (delete tag *load-list*)))))

(defun load-in-reg (reg tag &optional kill)
  (if (eql reg :r8)
      (load-in-r8 tag kill)
      (let ((loc (value-location tag nil)))
	(unless (eql loc reg)
	  (if (eql loc :r8)
	      (emit `(sys.lap-x86:mov64 ,reg :r8))
	      (ecase (first loc)
		((quote) (load-constant reg (second loc)))
		((:stack) (emit `(sys.lap-x86:mov64 ,reg (:stack ,(second loc))))))))
	(when kill
	  (setf *load-list* (delete tag *load-list*))))))

(defun flush-arguments-from-stack (arg-forms)
  (let ((stack-count (max 0 (- (length arg-forms) 5))))
    (when (plusp stack-count)
      (when (oddp stack-count) (incf stack-count))
      (emit `(sys.lap-x86:add64 :csp ,(* stack-count 8))))))

(defun prep-arguments-for-call (arg-forms)
  (when arg-forms
    (let ((args '())
	  (arg-count 0))
      (let ((*for-value* t))
	(dolist (f arg-forms)
	  (push (cg-form f) args)
	  (incf arg-count)
	  (when (null (first args))
	    ;; Non-local control transfer, don't actually need those results now.
	    (dolist (i (rest args))
	      (setf *load-list* (delete i *load-list*)))
	    (return-from prep-arguments-for-call nil))))
      (setf args (nreverse args))
      (let ((stack-count (- arg-count 5)))
	(when (plusp stack-count)
          (when (oddp stack-count)
            (incf stack-count))
          (emit `(sys.lap-x86:sub64 :csp ,(* stack-count 8)))
	  ;; Load values on the stack.
	  ;; Use r13 here to preserve whatever is in r8.
          ;; Must load first values first, so the GC can track properly.
          (loop for i from 0
             for j in (nthcdr 5 args) do
               (load-in-reg :r13 j t)
               (emit `(sys.lap-x86:mov64 (:csp ,(* i 8)) :r13))
               (emit-gc-info :pushed-values (1+ i)))))
      ;; Load other values in registers.
      (when (> arg-count 4)
	(load-in-reg :r12 (nth 4 args) t))
      (when (> arg-count 3)
	(load-in-reg :r11 (nth 3 args) t))
      (when (> arg-count 2)
	(load-in-reg :r10 (nth 2 args) t))
      (when (> arg-count 1)
	(load-in-reg :r9 (nth 1 args) t))
      (when (> arg-count 0)
	(load-in-r8 (nth 0 args) t))))
  t)

;; Compile a VALUES form.
(defun cg-values (forms)
  (cond ((null forms)
         ;; No values.
         (cond ((member *for-value* '(:multiple :tail))
                ;; R8 must hold NIL.
                (load-in-r8 ''nil)
                (emit `(sys.lap-x86:xor32 :ecx :ecx))
                :multiple)
               (t (cg-form ''nil))))
        ((null (rest forms))
         ;; Single value.
         (let ((*for-value* t))
           (cg-form (first forms))))
        ((member *for-value* '(:multiple :tail))
         ;; Multiple-values and compiling for multiple values.
         (let ((args '())
               (arg-count 0))
           ;; Compile arguments.
           (let ((*for-value* t))
             (dolist (f forms)
               (push (cg-form f) args)
               (incf arg-count)
               (when (null (first args))
                 ;; Non-local control transfer, don't actually need those results now.
                 (dolist (i (rest args))
                   (setf *load-list* (delete i *load-list*)))
                 (return-from cg-values nil))))
           (comment 'values)
           (setf args (nreverse args))
           ;; Load the first values in registers.
           (when (> arg-count 4)
             (load-in-reg :r12 (nth 4 args) t))
           (when (> arg-count 3)
             (load-in-reg :r11 (nth 3 args) t))
           (when (> arg-count 2)
             (load-in-reg :r10 (nth 2 args) t))
           (when (> arg-count 1)
             (load-in-reg :r9 (nth 1 args) t))
           (when (> arg-count 0)
             (load-in-r8 (nth 0 args) t))
           ;; Only the register values.
           (load-constant :rcx (min (length args) 5))
           ;; Now add MVs to the stack one by one.
           (loop for i from 0
              for value in (nthcdr 5 args) do
                (load-in-reg :r13 value t)
                (emit `(sys.lap-x86:gs)
                      `(sys.lap-x86:mov64 (,(+ (- 8 sys.int::+tag-object+)
                                               ;; fixme. should be +thread-mv-slots-start+
                                               (* #+(or)sys.int::+stack-group-offset-mv-slots+ 32 8)
                                               (* i 8)))
                                          :r13))
                (emit-gc-info :multiple-values 1)
                (emit `(sys.lap-x86:add64 :rcx ,(fixnum-to-raw 1)))
                (emit-gc-info :multiple-values 0))
           :multiple))
        (t ;; VALUES behaves like PROG1 when not compiling for multiple values.
         (let ((tag (cg-form (first forms))))
           (unless tag (return-from cg-values nil))
           (let ((*for-value* nil))
             (dolist (f (rest forms))
               (when (not (cg-form f))
                 (setf *load-list* (delete tag *load-list*))
                 (return-from cg-values nil))))
           tag))))

;; ### TCE here
(defun cg-function-form (form)
  (let ((fn (when (not *suppress-builtins*)
              (match-builtin (first form) (length (rest form))))))
    (cond ((and (eql *for-value* :predicate)
                (or (eql (first form) 'null)
                    (eql (first form) 'not))
                (= (length form) 2))
           (let* ((tag (cg-form (second form)))
                  (loc (when (and tag (not (keywordp tag)))
                         (value-location tag t))))
             (cond ((null tag) nil)
                   ((keywordp tag)
                    ;; Invert the sense.
                    (invert-predicate tag))
                   ;; Perform (eql nil ...).
                   ((and (consp loc) (eq (first loc) :stack))
                    (emit `(sys.lap-x86:cmp64 (:stack ,(second loc)) nil))
                    :e)
                   (t (load-in-r8 tag)
                      (emit `(sys.lap-x86:cmp64 :r8 nil))
                      :e))))
          (fn
	   (let ((args '()))
	     (let ((*for-value* t))
	       (dolist (f (rest form))
		 (push (cg-form f) args)
		 (when (null (first args))
		   ;; Non-local control transfer, don't actually need those results now.
		   (dolist (i (rest args))
		     (setf *load-list* (delete i *load-list*)))
		   (return-from cg-function-form nil))))
             (comment (first form))
	     (apply fn (nreverse args))))
	  ((and (eql (first form) 'funcall)
		(rest form))
	   (let* ((fn-tag (let ((*for-value* t)) (cg-form (if (typep (second form) 'lambda-information)
                                                              (second form)
                                                              `(sys.int::%coerce-to-callable ,(second form)))))))
	     (cond ((prep-arguments-for-call (cddr form))
                    (comment 'funcall)
		    (load-in-reg :rbx fn-tag t)
		    (smash-r8)
		    (load-constant :rcx (length (cddr form)))
                    (cond ((can-tail-call (cddr form))
                           (emit-tail-call (object-ea :rbx :slot 0) (second form)))
                          (t (emit `(sys.lap-x86:call ,(object-ea :rbx :slot 0)))))
                    (if (member *for-value* '(:multiple :tail))
                        (emit-gc-info :multiple-values 0)
                        (emit-gc-info))
                    (flush-arguments-from-stack (cddr form))
                    (cond ((can-tail-call (cddr form)) nil)
                          ((member *for-value* '(:multiple :tail))
                           :multiple)
                          (t (setf *r8-value* (list (gensym))))))
		   (t ;; Flush the unused function.
		    (setf *load-list* (delete fn-tag *load-list*))))))
          ((eql (first form) 'values)
           (cg-values (rest form)))
	  (t (when (prep-arguments-for-call (rest form))
               (comment (first form))
               (emit `(sys.lap-x86:mov64 :r13 (:function ,(first form))))
	       (smash-r8)
	       (load-constant :rcx (length (rest form)))
               (cond ((can-tail-call (rest form))
                      (emit-tail-call (object-ea :r13 :slot sys.int::+fref-entry-point+) (first form))
                      nil)
                     (t (emit `(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+)))
                        (if (member *for-value* '(:multiple :tail))
                            (emit-gc-info :multiple-values 0)
                            (emit-gc-info))
                        (flush-arguments-from-stack (cdr form))
                        (cond ((member *for-value* '(:multiple :tail))
                               :multiple)
                              (t (setf *r8-value* (list (gensym))))))))))))

(defun can-tail-call (args)
  (and (eql *for-value* :tail)
       (<= (length args) 5)))

(defun emit-tail-call (where &optional what)
  (declare (ignorable what))
  #+nil(format t "Performing tail call to ~S in ~S~%"
          what (lambda-information-name *current-lambda*))
  (emit `(sys.lap-x86:leave)
        `(:gc :no-frame))
  (emit `(sys.lap-x86:jmp ,where)))

(defun cg-variable (form)
  (assert (localp form))
  (cons form (incf *run-counter*)))

(defun cg-lambda (form)
  (list 'quote (codegen-lambda form)))

(defun raise-type-error (reg typespec)
  (unless (eql reg :r8)
    (emit `(sys.lap-x86:mov64 :r8 ,reg)))
  (load-constant :r9 typespec)
  (emit `(sys.lap-x86:mov64 :r13 (:function sys.int::raise-type-error))
        `(sys.lap-x86:mov32 :ecx ,(fixnum-to-raw 2))
	`(sys.lap-x86:call ,(object-ea :r13 :slot sys.int::+fref-entry-point+))
	`(sys.lap-x86:ud2))
  nil)

(defun fixnum-check (reg &optional (typespec 'fixnum))
  (let ((type-error-label (gensym)))
    (emit-trailer (type-error-label)
      (raise-type-error reg typespec))
    (emit `(sys.lap-x86:test64 ,reg ,sys.int::+fixnum-tag-mask+)
	  `(sys.lap-x86:jnz ,type-error-label))))

(defun cg-jump-table (form)
  (destructuring-bind (value &rest jumps) (cdr form)
    (let ((tag (let ((*for-value* t))
                 (cg-form value)))
          (jump-table (gensym "jump-table")))
      ;; Build the jump table.
      ;; Every jump entry must be a local GO with no special bindings.
      (emit-trailer (jump-table nil)
        (dolist (j jumps)
          (assert (and (listp j) (eql (first j) 'go)))
          (let ((go-tag (assoc (second j) *rename-list*)))
            (assert go-tag () "GO tag not local")
            (emit `(:d64/le (- ,(second go-tag) ,jump-table))))))
      ;; Jump.
      (load-in-r8 tag t)
      (smash-r8)
      (emit `(sys.lap-x86:lea64 :rax (:rip ,jump-table))
            `(sys.lap-x86:add64 :rax (:rax (:r8 ,(/ 8 (ash 1 sys.int::+n-fixnum-bits+)))))
            `(sys.lap-x86:jmp :rax))
      nil)))
