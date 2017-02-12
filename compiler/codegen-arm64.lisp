;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.codegen.arm64)

(defvar *run-counter* nil)
(defvar *load-list* nil)
(defvar *x0-value* nil)
(defvar *stack-values* nil)
(defvar *for-value* nil)
(defvar *rename-list* nil)
(defvar *code-accum* nil)
(defvar *trailers* nil)
(defvar *current-lambda* nil)
(defvar *current-lambda-name* nil)
(defvar *gc-info-fixups* nil)
(defvar *active-nl-exits* nil)
(defvar *literal-pool*)

(defun emit (&rest instructions)
  (dolist (i instructions)
    (push i *code-accum*)))

(defun comment (&rest stuff)
  (emit `(:comment ,@stuff)))

(defmacro emit-trailer ((&optional name (default-gc-info t)) &body body)
  `(push (let ((*code-accum* '()))
           ,(when name
                  `(emit ,name))
           ,@(when default-gc-info
                   (list '(emit-gc-info)))
           (progn ,@body)
           (nreverse *code-accum*))
         *trailers*))

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

(defun kill-value (tag)
  (setf *load-list* (delete tag *load-list*)))

(defun value-location (tag &optional kill)
  (when kill
    (kill-value tag))
  (cond ((eq (car tag) 'quote)
         (values (if (and (consp *x0-value*)
                          (eq (car *x0-value*) 'quote)
                          (eql (cadr tag) (cadr *x0-value*)))
                     :x0
                     tag)
                 tag))
        ((null (cdr tag))
         (values (if (eq tag *x0-value*)
                     :x0
                     (dotimes (i (length *stack-values*)
                               (error "Cannot find tag ~S." tag))
                       (when (eq tag (aref *stack-values* i))
                         (return (list :stack i)))))
                 tag))
        ((lexical-variable-p (car tag))
         ;; Search for the lowest numbered time that is >= to the tag time.
         (let ((best (when (and (consp *x0-value*) (eq (car *x0-value*) (car tag))
                                (integerp (cdr *x0-value*)) (>= (cdr *x0-value*) (cdr tag)))
                       *x0-value*))
               (best-loc :x0)
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
                       ;; X0 might hold a duplicate (thanks to let or setq), use that instead of home.
                       (when (and *x0-value* (consp *x0-value*) (eq (car *x0-value*) (car tag)) (eq (cdr *x0-value*) :dup))
                         :x0)
                       home-loc
                       (error "Cannot find tag ~S." tag))
                   (or best
                       (when (and *x0-value* (consp *x0-value*) (eq (car *x0-value*) (car tag)) (eq (cdr *x0-value*) :dup))
                         *x0-value*)
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
                   (let ((best (when (and (consp *x0-value*) (eq (car *x0-value*) (car tag))
                                          (integerp (cdr *x0-value*)) (>= (cdr *x0-value*) (cdr tag)))
                                 *x0-value*)))
                     (dotimes (i (length *stack-values*))
                       (let ((val (aref *stack-values* i)))
                         (when (and (consp val) (eq (car val) (car v))
                                    (integerp (cdr val)) (>= (cdr val) (cdr v))
                                    (or (null best)
                                        (< (cdr val) (cdr best))))
                           (setf best val))))
                     (when (eq best tag)
                       (return nil)))))))))

(defun cg-go (form)
  (let ((tag (assoc (ast-target form) *rename-list*)))
    (smash-x0)
    (cond (tag ;; Local jump.
           (emit `(lap:b ,(second tag))))
          (t ;; Non-local exit.
           (let ((tagbody-tag (let ((*for-value* t))
                                (cg-form (ast-info form)))))
             (load-in-reg :x9 tagbody-tag t)
             ;; RAX holds the tagbody info.
             (emit
              ;; Get X0 as well.
              `(lap:orr :x0 :xzr :xzr)
              ;; GO GO GO!
              `(lap:ldr :x10 (:x9 0))
              `(lap:ldr :x11 (:x10 ,(* (position (ast-target form)
                                                 (tagbody-information-go-tags
                                                  (go-tag-tagbody (ast-target form))))
                                       8)))
              `(lap:add :x10 :x10 :x11)
              `(lap:br :x10)))))
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
  (smash-x0)
  (dolist (l *load-list*)
    (when (and (consp l) (lexical-variable-p (car l))
               (not (eql (lexical-variable-write-count (car l)) 0)))
      ;; Don't save if there is something satisfying this already.
      (multiple-value-bind (loc true-tag)
          (value-location l)
        (declare (ignore loc))
        (unless (tag-saved-on-stack-p true-tag)
          (load-in-x0 l nil)
          (smash-x0 t)))))
  (let ((new-values (make-array (length *stack-values*) :initial-contents *stack-values*)))
    ;; Now flush any values that aren't there to satisfy the load list.
    (dotimes (i (length new-values))
      (when (condemed-p (aref new-values i))
        (setf (aref new-values i) nil)))
    new-values))

(defun copy-stack-values (values)
  "Copy the VALUES array, ensuring that it's at least as long as the current *stack-values*, is adjustable and contains all non-value slots."
  (let ((new (make-array (length *stack-values*) :adjustable t :fill-pointer t :initial-element nil)))
    (setf (subseq new 0) values)
    (dotimes (i (length *stack-values*))
      (when (or (equal (aref *stack-values* i) '(:unboxed . :home))
                (equal (aref *stack-values* i) '(:boxed . :home)))
        (assert (or (null (aref new i))
                    (equal (aref new i) (aref *stack-values* i))))
        (setf (aref new i) (aref *stack-values* i))))
    new))

(defun find-stack-slot ()
  ;; Find a free stack slot, or allocate a new one.
  (dotimes (i (length *stack-values*)
            (vector-push-extend nil *stack-values*))
    (when (or (null (aref *stack-values* i))
              (condemed-p (aref *stack-values* i)))
      (setf (aref *stack-values* i) nil)
      (return i))))

(defun code-for-reg-immediate-mem-op (inst reg base offset &optional temp)
  (when (not temp)
    (setf temp :x12))
  (cond ((or (<= -256 offset 255)
             (and (<= 0 offset 16380)
                  (zerop (logand offset #b111))))
         (list `(,inst ,reg (,base ,offset))))
        (t
         (append (code-for-load-literal temp offset)
                 (list `(,inst ,reg (,base ,temp)))))))

(defun object-slot-displacement (slot &optional scale)
  (+ (- sys.int::+tag-object+) 8 (* slot (or scale 8))))

(defun emit-object-op (inst reg base slot &optional scale)
  (apply #'emit (code-for-reg-immediate-mem-op
                 inst reg base (object-slot-displacement slot scale))))

(defun emit-object-load (reg base &key (slot 0))
  (emit-object-op 'lap:ldr reg base slot))

(defun emit-object-store (reg base &key (slot 0))
  (emit-object-op 'lap:str reg base slot))

(defun code-for-stack-op (inst reg slot &optional temp)
  (code-for-reg-immediate-mem-op inst reg :x29 (control-stack-frame-offset slot) temp))

(defun emit-stack-op (inst reg slot &optional temp)
  (apply #'emit (code-for-stack-op inst reg slot temp)))

(defun emit-stack-load (reg slot &optional temp)
  (emit-stack-op 'lap:ldr reg slot temp))

(defun emit-stack-store (reg slot &optional temp)
  (emit-stack-op 'lap:str reg slot temp))

(defun smash-x0 (&optional do-not-kill-x0)
  "Check if the value in X0 is on the load-list and flush it to the stack if it is."
  ;; Avoid flushing if it's already on the stack.
  (when (and *x0-value*
             (not (eql *x0-value* :multiple))
             (not (condemed-p *x0-value*))
             (not (tag-saved-on-stack-p *x0-value*)))
    (let ((slot (find-stack-slot)))
      (setf (aref *stack-values* slot) *x0-value*)
      (emit-stack-store :x0 slot)))
  (unless do-not-kill-x0
    (setf *x0-value* nil)))

(defun code-for-load-literal (register value)
  (cond ((keywordp value)
         (let ((pos (position value *literal-pool*)))
           (when (not pos)
             (setf pos (vector-push-extend value *literal-pool*)))
           (list `(:comment :literal ,value)
                 `(lap:ldr ,register (:pc (+ arm-literal-pool ,(* pos 8)))))))
        ((zerop value)
         (list `(lap:orr ,register :xzr :xzr)))
        ((<= 0 value 65535)
         (list `(lap:movz ,register ,value)))
        (t
         (let ((pos (position value *literal-pool*)))
           (when (not pos)
             (setf pos (vector-push-extend value *literal-pool*)))
           (list `(:comment :literal ,value)
                 `(lap:ldr ,register (:pc (+ arm-literal-pool ,(* pos 8)))))))))

(defun load-literal (register value)
  (apply #'emit (code-for-load-literal register value)))

(defun load-constant (register value)
  (cond ((eq value 'nil)
         (emit `(lap:orr ,register :xzr :x26)))
        ((fixnump value)
         (load-literal register (fixnum-to-raw value)))
        ((characterp value)
         (load-literal register (character-to-raw value)))
        (t (emit `(lap:ldr ,register (:constant ,value))))))

(defun load-multiple-values (tag)
  (cond ((eql tag :multiple))
        (t (load-in-x0 tag t)
           (load-literal :x5 (fixnum-to-raw 1)))))

(defun load-in-x0 (tag &optional kill)
  (multiple-value-bind (loc true-tag)
      (value-location tag nil)
    (unless (eq loc :x0)
      (smash-x0)
      (ecase (first loc)
        ((quote) (load-constant :x0 (second loc)))
        ((:stack)
         (emit-stack-load :x0 (second loc))))
      (setf *x0-value* true-tag))
    (when kill
      (kill-value tag))))

(defun load-in-reg (reg tag &optional kill)
  (if (eql reg :x0)
      (load-in-x0 tag kill)
      (let ((loc (value-location tag nil)))
        (unless (eql loc reg)
          (if (eql loc :x0)
              (emit `(lap:orr ,reg :xzr :x0))
              (ecase (first loc)
                ((quote)
                 (load-constant reg (second loc)))
                ((:stack)
                 (emit-stack-load reg (second loc))))))
        (when kill
          (kill-value tag)))))

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
         (*current-lambda-name* (lambda-information-name lambda))
         (*run-counter* 0)
         (*load-list* '())
         (*x0-value* nil)
         (*stack-values* (make-array 8 :fill-pointer 0 :adjustable t))
         (*for-value* t)
         (*rename-list* '())
         (*code-accum* '())
         (*trailers* '())
         (arg-registers '(:x0 :x1 :x2 :x3 :x4))
         (*gc-info-fixups* '())
         (*active-nl-exits* '())
         (*literal-pool* (make-array 0 :adjustable t :fill-pointer 0)))
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
    (assert (or (null (lambda-information-fref-arg lambda))
                (lexical-variable-p (lambda-information-fref-arg lambda))))
    (assert (or (null (lambda-information-closure-arg lambda))
                (lexical-variable-p (lambda-information-closure-arg lambda))))
    (assert (or (null (lambda-information-count-arg lambda))
                (lexical-variable-p (lambda-information-count-arg lambda))))
    ;; Stash :x6 (closure), :x7 (fref), and :x5 (count) away.
    (let ((fref-arg (lambda-information-fref-arg lambda)))
      (when fref-arg
        (let ((ofs (find-stack-slot)))
          (setf (aref *stack-values* ofs) (cons fref-arg :home))
          (emit-stack-store :x7 ofs))))
    (let ((closure-arg (lambda-information-closure-arg lambda)))
      (when closure-arg
        (let ((ofs (find-stack-slot)))
          (setf (aref *stack-values* ofs) (cons closure-arg :home))
          (emit-stack-store :x6 ofs))))
    (let ((count-arg (lambda-information-count-arg lambda)))
      (when count-arg
        (let ((ofs (find-stack-slot)))
          (setf (aref *stack-values* ofs) (cons count-arg :home))
          (emit-stack-store :x5 ofs))))
    ;; Free up :x6 quickly.
    (let ((env-arg (lambda-information-environment-arg lambda)))
      (when env-arg
        (let ((ofs (find-stack-slot)))
          (setf (aref *stack-values* ofs) (cons env-arg :home))
          (when (not (getf (lambda-information-plist lambda) 'sys.c::unwind-protect-cleanup))
            ;; Read environment pointer from closure object.
            (emit-object-load :x6 :x6 :slot 2))
          (emit-stack-store :x6 ofs))))
    ;; Compile argument setup code.
    (let ((current-arg-index 0))
      (dolist (arg (lambda-information-required-args lambda))
        (let ((ofs (find-stack-slot)))
          (setf (aref *stack-values* ofs) (cons arg :home))
          (cond (arg-registers
                 (emit-stack-store (pop arg-registers) ofs))
                (t
                 (emit `(lap:ldr :x0 (:x29 ,(* (+ (- current-arg-index 5) 2) 8))))
                 (emit-stack-store :x0 ofs))))
        (incf current-arg-index))
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
          (emit `(lap:subs :xzr :x5 ,(fixnum-to-raw current-arg-index))
                `(lap:b.le ,mid-label))
          ;; Argument supplied, stash wherever.
          (cond (arg-registers
                 (emit-stack-store (pop arg-registers) var-ofs))
                (t
                 (emit `(lap:ldr :x0 (:x29 ,(* (+ (- current-arg-index 5) 2) 8))))
                 (emit-stack-store :x0 var-ofs)))
          (when sup-ofs
            (emit `(lap:ldr :x0 (:constant t)))
            (emit-stack-store :x0 sup-ofs))
          (emit `(lap:b ,end-label)
                mid-label)
          ;; Argument not supplied. Init-form is a quoted constant.
          (let ((tag `',(ast-value (second arg))))
            (load-in-x0 tag t)
            (setf *x0-value* nil)
            (emit-stack-store :x0 var-ofs)
            (when sup-ofs
              (emit-stack-store :x26 sup-ofs)))
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
    (let ((code-tag (let ((*for-value* (if sys.c::*perform-tce* :tail :multiple)))
                      (cg-form (lambda-information-body lambda)))))
      (when code-tag
        (load-multiple-values code-tag)
        (emit `(lap:add :sp :x29 0)
              `(:gc :frame :multiple-values 0)
              `(lap:ldp :x29 :x30 (:post :sp 16))
              `(:gc :no-frame :multiple-values 0)
              `(lap:ret))))
    (let* ((final-code (nconc (generate-entry-code lambda)
                              (nreverse *code-accum*)
                              (apply #'nconc *trailers*)
                              (list* 'arm-literal-pool
                                     (loop
                                        for val across *literal-pool*
                                        collect `(:d64/le ,val)))))
           (homes (loop for (var . loc) across *stack-values*
                     for i from 0
                     when (and (lexical-variable-p var)
                               (not (getf (lexical-variable-plist var) 'hide-from-debug-info))
                               (eql loc :home))
                     collect (list (lexical-variable-name var) i))))
      ;; Fix all the GC instructions.
      (dolist (inst *gc-info-fixups*)
        (setf (rest (last inst)) (list :layout (coerce (loop for value across *stack-values*
                                                          collect (if (equal value '(:unboxed . :home))
                                                                      0
                                                                      1))
                                                       'bit-vector))))
      #+(or)
      (when sys.c::*enable-branch-tensioner*
        (setf final-code (tension-branches final-code)))
      (when sys.c::*trace-asm*
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
               (namestring *compile-file-pathname*))
             sys.int::*top-level-form-number*
             (lambda-information-lambda-list lambda)
             (lambda-information-docstring lambda))
       nil
       :arm64))))

(defun emit-gc-info (&rest extra-stuff)
  (let ((thing (list* :gc :frame extra-stuff)))
    (push thing *gc-info-fixups*)
    (emit thing)))

(defun generate-entry-code (lambda)
  (let ((entry-label (gensym "ENTRY"))
        (invalid-arguments-label (gensym "BADARGS")))
    (emit-trailer (invalid-arguments-label nil)
      (emit `(:gc :frame)
            `(lap:orr :x5 :xzr :xzr)
            `(lap:ldr :x7 (:function sys.int::raise-invalid-argument-error)))
      (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
      (emit `(lap:blr :x9)
            `(lap:hlt 0)))
    (nconc
     (list entry-label
           ;; Create control stack frame.
           ;; FIXME: Not quite right. ARM calls start with no return address
           ;; pushed! must modify no-frame layout meaning to assume ra.
           `(:gc :no-frame :incoming-arguments :rcx)
           `(lap:stp :x29 :x30 (:pre :sp -16))
           `(:gc :no-frame :incoming-arguments :rcx :layout #*0)
           `(lap:add :x29 :sp :xzr)
           `(:gc :frame :incoming-arguments :rcx))
     ;; Emit the argument count test.
     (cond ((lambda-information-rest-arg lambda)
            ;; If there are no required parameters, then don't generate a lower-bound check.
            (when (lambda-information-required-args lambda)
              ;; Minimum number of arguments.
              (list `(lap:subs :xzr :x5 ,(fixnum-to-raw (length (lambda-information-required-args lambda))))
                    `(lap:b.lt ,invalid-arguments-label))))
           ((and (lambda-information-required-args lambda)
                 (lambda-information-optional-args lambda))
            ;; A range.
            (list `(lap:sub :x9 :x5 ,(fixnum-to-raw (length (lambda-information-required-args lambda))))
                  `(lap:subs :xzr :x9 ,(fixnum-to-raw (length (lambda-information-optional-args lambda))))
                  `(lap:b.hi ,invalid-arguments-label)))
           ((lambda-information-optional-args lambda)
            ;; Maximum number of arguments.
            (list `(lap:subs :xzr :x5 ,(fixnum-to-raw (length (lambda-information-optional-args lambda))))
                  `(lap:b.hi ,invalid-arguments-label)))
           ((lambda-information-required-args lambda)
            ;; Exact number of arguments.
            (list `(lap:subs :xzr :x5 ,(fixnum-to-raw (length (lambda-information-required-args lambda))))
                  `(lap:b.ne ,invalid-arguments-label)))
           ;; No arguments
           (t (list `(lap:cbnz :x5 ,invalid-arguments-label))))
     (let ((n-slots (length *stack-values*)))
       (when (oddp n-slots) (incf n-slots))
       ;; Adjust stack.
       (list `(lap:sub :sp :sp ,(* n-slots 8))))
     ;; Flush stack slots.
     (loop
        for value across *stack-values*
        for i from 0
        unless (equal value '(:unboxed . :home))
        append (code-for-stack-op 'lap:str :x26 i))
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
    ;; Assemble the rest list into X7.
    ;; X5 holds the argument count.
    ;; X6 and X7 are free. Argument registers may or may not be free
    ;; depending on the number of required/optional arguments.
    ;; Number of supplied arguments.
    (emit-stack-store :x5 saved-argument-count)
    ;; Tell the GC to used the number of arguments saved on the stack. X5 will
    ;; be used later.
    (emit-gc-info :incoming-arguments saved-argument-count)
    ;; The cons cells are allocated in one single chunk.
    (emit `(lap:orr :x7 :xzr :x26))
    ;; Remove required/optional arguments from the count.
    ;; If negative or zero, the &REST list is empty.
    (cond ((zerop regular-argument-count)
           (emit `(lap:cbz :x5 ,rest-list-done)))
          (t
           (emit `(lap:subs :x5 :x5 ,(fixnum-to-raw regular-argument-count)))
           (emit `(lap:b.le ,rest-list-done))))
    ;; Save the length, and double it. Each cons takes two words.
    (emit `(lap:add :x10 :xzr :x5 :lsl 1))
    ;; Add a header word and word of padding so it can be treated like a simple-vector.
    (emit `(lap:add :x10 :x10 ,(fixnum-to-raw 2)))
    ;; Allocate on the stack, converting fixnum to raw integer * 8.
    (emit `(lap:sub :sp :sp :x10 :lsl ,(- 3 sys.int::+n-fixnum-bits+)))
    ;; Generate the simple-vector header. simple-vector tag is zero, doesn't need to be set here.
    ;; *2 as conses are 2 words and +1 for padding word at the start.
    (emit `(lap:add :x10 :x5 :x5)
          `(lap:add :x10 :x10 ,(fixnum-to-raw 1)))
    (emit `(lap:add :x10 :xzr :x10 :lsl ,(- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))
    (emit `(lap:str :x10 (:sp)))
    ;; Clear the padding slot.
    (emit `(lap:str :xzr (:sp 8)))
    ;; For each cons, clear the car and set the cdr to the next cons.
    (emit `(lap:add :x12 :sp 16))
    (emit `(lap:orr :x10 :xzr :x5))
    (emit rest-clear-loop-head)
    (emit `(lap:add :x9 :x12 ,(+ 16 sys.int::+tag-cons+)))
    (emit `(lap:str :xzr (:post :x12 8))) ; car
    (emit `(lap:str :x9 (:post :x12 8))) ; cdr
    (emit `(lap:subs :x10 :x10 ,(fixnum-to-raw 1)))
    (emit `(lap:b.hi ,rest-clear-loop-head))
    ;; Set the cdr of the final cons to NIL.
    (emit `(lap:str :x26 (:x12 -8)))
    ;; Create the DX root object for the vector.
    (emit `(lap:add :x9 :sp ,sys.int::+tag-dx-root-object+))
    (emit-stack-store :x9 rest-dx-root)
    ;; It's now safe to write values into the list/vector.
    (emit `(lap:add :x12 :sp 16))
    ;; Add register arguments to the list.
    (loop
       for reg in arg-registers
       do (emit `(lap:str ,reg (:post :x12 16))
                `(lap:subs :x5 :x5 ,(fixnum-to-raw 1))
                `(lap:b.eq ,rest-loop-end)))
    ;; Now add the stack arguments.
    ;; Skip past required/optional arguments on the stack, the saved frame pointer and the return address.
    (emit `(lap:add :x11 :x29 ,(* (+ (max 0 (- regular-argument-count 5)) 2) 8)))
    (emit rest-loop-head)
    ;; Load from stack.
    (emit `(lap:ldr :x0 (:post :x11 8)))
    ;; Store into current car.
    (emit `(lap:str :x0 (:post :x12 16)))
    ;; Stop when no more arguments.
    (emit `(lap:subs :x5 :x5 ,(fixnum-to-raw 1)))
    (emit `(lap:b.ne ,rest-loop-head))
    (emit rest-loop-end)
    ;; There were &REST arguments, create the cons.
    (emit `(lap:add :x7 :sp ,(+ 16 sys.int::+tag-cons+)))
    ;; Code above jumps directly here with NIL in R13 when there are no arguments.
    (emit rest-list-done)))

(defun emit-rest-list (lambda arg-registers)
  (let* ((rest-arg (lambda-information-rest-arg lambda))
         (dx-rest (lexical-variable-dynamic-extent rest-arg)))
    (emit-dx-rest-list lambda arg-registers)
    ;; The rest list has been created in R13.
    (when (not dx-rest)
      ;; Call COPY-LIST to convert the dx list into a heap list.
      (emit `(lap:orr :x0 :xzr :x7)
            `(lap:ldr :x7 (:function copy-list))
            `(lap:orr :x5 :xzr ,(fixnum-to-raw 1)))
      (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
      (emit `(lap:blr :x9)
            `(lap:orr :x7 :xzr :x0)))
    (let ((ofs (find-stack-slot)))
      (setf (aref *stack-values* ofs) (cons rest-arg :home))
      (emit-stack-store :x7 ofs))))

(defun cg-form (form)
  (flet ((save-tag (tag)
           (when (and tag *for-value* (not (keywordp tag)))
             (push tag *load-list*))
           tag))
    (etypecase form
      (ast-block (save-tag (cg-block form)))
      (ast-function (save-tag (cg-function form)))
      (ast-go (cg-go form))
      (ast-if (save-tag (cg-if form)))
      (ast-let (cg-let form))
      (ast-multiple-value-bind (save-tag (cg-multiple-value-bind form)))
      (ast-multiple-value-call (save-tag (cg-multiple-value-call form)))
      (ast-multiple-value-prog1 (save-tag (cg-multiple-value-prog1 form)))
      (ast-progn (cg-progn form))
      (ast-quote (cg-quote form))
      (ast-return-from (cg-return-from form))
      (ast-setq (cg-setq form))
      (ast-tagbody (cg-tagbody form))
      (ast-the (cg-the form))
      (ast-call (save-tag (cg-function-form form)))
      (ast-jump-table (cg-jump-table form))
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
  (destructuring-bind (size)
      (ast-arguments form)
    (assert (and (quoted-form-p size)
                 (fixnump (ast-value size)))
            (size)
            "Size of dx simple-vector must be known at cg time.")
    (let ((words (1+ (ast-value size))))
      (when (oddp words)
        (incf words))
      (smash-x0)
      (let ((slots (allocate-control-stack-slots words t)))
        ;; Set the header.
        (load-literal :x9 (ash (ast-value size) sys.int::+object-data-shift+))
        (emit-stack-store :x9 (+ slots words -1))
        ;; Generate pointer.
        (load-literal :x9 (+ (control-stack-frame-offset (+ slots words -1))
                             sys.int::+tag-object+))
        (emit `(lap:add :x0 :x29 :x9)))))
  (setf *x0-value* (list (gensym))))

(defun emit-nlx-thunk (thunk-name target-label multiple-values-active)
  (emit-trailer (thunk-name nil)
    (if multiple-values-active
        (emit-gc-info :block-or-tagbody-thunk :rax :multiple-values 0)
        (emit-gc-info :block-or-tagbody-thunk :rax))
    (emit `(lap:ldr :x10 (:x9 16))
          `(lap:add :sp :x10 :xzr)
          `(lap:ldr :x29 (:x9 24)))
    (dolist (slot (first *active-nl-exits*))
      (emit-stack-store :x26 slot))
    (emit `(lap:b ,target-label))))

(defun cg-block (form)
  (let* ((info (ast-info form))
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
      (smash-x0)
      (let ((slot (find-stack-slot))
            (control-info (allocate-control-stack-slots 4)))
        (setf (aref *stack-values* slot) (cons info :home))
        ;; Construct jump info.
        (emit `(lap:adr :x9 ,thunk-label))
        (emit-stack-store :x9 (+ control-info 3))
        (emit-object-load :x9 :x28 :slot 6) ;; ### special-stack-pointer
        (emit-stack-store :x9 (+ control-info 2))
        (emit `(lap:add :x9 :sp :xzr))
        (emit-stack-store :x9 (+ control-info 1))
        (emit-stack-store :x29 (+ control-info 0))
        ;; Save pointer to info
        (load-literal :x9 (control-stack-frame-offset (+ control-info 3)))
        (emit `(lap:add :x9 :x29 :x9))
        (emit-stack-store :x9 slot)))
    (prog1
        (let* ((*rename-list* (cons (list info exit-label) *rename-list*))
               (stack-slots (set-up-for-branch))
               (tag (cg-form (ast-body form))))
          (cond ((and *for-value* tag (/= (block-information-count info) 0))
                 ;; Returning a value, exit is reached normally and there were return-from forms reached.
                 ;; Unify the results, so :MULTIPLE is always returned.
                 (let ((return-mode nil))
                   (ecase *for-value*
                     ((:multiple :tail)
                      (unless (eql tag :multiple)
                        (load-multiple-values tag)
                        (smash-x0))
                      (setf return-mode :multiple))
                     (t (load-in-x0 tag t)
                        (smash-x0)
                        (setf return-mode (list (gensym)))))
                   (emit exit-label)
                   (when (member *for-value* '(:multiple :tail))
                     (emit-gc-info :multiple-values 0))
                   (setf *stack-values* (copy-stack-values stack-slots)
                         *x0-value* return-mode)))
                ((and *for-value* tag)
                 ;; Returning a value, exit is reached normally, but no return-from forms were reached.
                 tag)
                ((and *for-value* (/= (block-information-count info) 0))
                 ;; Returning a value, exit is not reached normally, but there were return-from forms reached.
                 (smash-x0)
                 (emit exit-label)
                 (when (member *for-value* '(:multiple :tail))
                   (emit-gc-info :multiple-values 0))
                 (setf *stack-values* (copy-stack-values stack-slots)
                       *x0-value* (if (member *for-value* '(:multiple :tail))
                                      :multiple
                                      (list (gensym)))))
                ((/= (block-information-count info) 0)
                 ;; Not returning a value, but there were return-from forms reached.
                 (smash-x0)
                 (emit exit-label)
                 (setf *stack-values* (copy-stack-values stack-slots)
                       *x0-value* (list (gensym))))
                ;; No value returned, no return-from forms reached.
                (t nil)))
      (when escapes
        (emit-nlx-thunk thunk-label exit-label (member *for-value* '(:multiple :tail)))))))

(defun cg-function (form)
  (let ((name (ast-name form))
        (undefined (gensym "function-undefined"))
        (resume (gensym "function-resume")))
    (smash-x0)
    (emit-trailer (undefined)
      ;; When the function is undefined, fall back on FDEFINITION.
      (load-constant :x5 1)
      (load-constant :x0 name)
      (emit `(lap:ldr :x7 (:function fdefinition)))
      (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
      (emit `(lap:blr :x9)
            `(lap:b ,resume)))
    (emit `(lap:ldr :x0 (:function ,name)))
    (emit-object-load :x0 :x0 :slot sys.int::+fref-function+)
    (load-literal :x9 :undefined-function)
    (emit `(lap:subs :xzr :x0 :x9)
          `(lap:b.eq ,undefined)
          resume)
    (setf *x0-value* (list (gensym)))))

;;; (predicate inverse jump-instruction cmov-instruction)
(defstruct predicate-instruction
  inverse jump-instruction cmov-instruction)
(defparameter *predicate-instructions-1*
  '((:eq  :ne lap:b.eq lap:csel.eq)
    (:ne  :eq lap:b.ne lap:csel.ne)
    (:cs  :cc lap:b.cs lap:csel.cs)
    (:cc  :cs lap:b.cc lap:csel.cc)
    (:mi  :pl lap:b.mi lap:csel.mi)
    (:pl  :mi lap:b.pl lap:csel.pl)
    (:vs  :vc lap:b.vs lap:csel.vs)
    (:vc  :vs lap:b.vc lap:csel.vc)
    (:hi  :ls lap:b.hi lap:csel.hi)
    (:ls  :hi lap:b.ls lap:csel.ls)
    (:ge  :lt lap:b.ge lap:csel.ge)
    (:lt  :ge lap:b.lt lap:csel.lt)
    (:gt  :le lap:b.gt lap:csel.gt)
    (:le  :ge lap:b.le lap:csel.le)))
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
  (smash-x0)
  (emit `(lap:ldr :x0 (:constant t))
        `(,(predicate-instruction-cmov-instruction (predicate-info pred))
           :x0 :x0 :x26)))

(defun predicate-result (pred)
  (cond ((eql *for-value* :predicate)
         pred)
        (t (load-predicate pred)
           (setf *x0-value* (list (gensym))))))

(defun cg-if (form)
  (let* ((else-label (gensym))
         (end-label (gensym))
         (test-tag (let ((*for-value* :predicate))
                     (cg-form (ast-test form))))
         (branch-count 0)
         (stack-slots (set-up-for-branch)))
    (when (null test-tag)
      (return-from cg-if))
    (when (not (keywordp test-tag))
      (kill-value test-tag))
    (cond ((keywordp test-tag)) ; Nothing for predicates.
          (t (load-in-x0 test-tag)
             (emit `(lap:subs :xzr :x0 :x26))))
    (let ((x0-at-cond *x0-value*)
          (stack-at-cond (make-array (length *stack-values*) :initial-contents *stack-values*)))
      ;; This is a little dangerous and relies on SET-UP-FOR-BRANCH not
      ;; changing the flags.
      (cond ((keywordp test-tag)
             ;; Invert the sense.
             (emit `(,(predicate-instruction-jump-instruction
                       (predicate-info (invert-predicate test-tag))) ,else-label)))
            (t (emit `(lap:b.eq ,else-label))))
      (branch-to else-label)
      (let ((tag (cg-form (ast-if-then form))))
        (when tag
          (when *for-value*
            (case *for-value*
              ((:multiple :tail)
               (load-multiple-values tag))
              (:predicate (if (keywordp tag)
                              (load-predicate tag)
                              (load-in-x0 tag t)))
              (t (load-in-x0 tag t))))
          (emit `(lap:b ,end-label))
          (incf branch-count)
          (branch-to end-label)))
      (setf *x0-value* x0-at-cond
            *stack-values* (copy-stack-values stack-at-cond))
      (emit-label else-label)
      (emit-gc-info)
      (let ((tag (cg-form (ast-if-else form))))
        (when tag
          (when *for-value*
            (case *for-value*
              ((:multiple :tail)
               (load-multiple-values tag))
              (:predicate (if (keywordp tag)
                              (load-predicate tag)
                              (load-in-x0 tag t)))
              (t (load-in-x0 tag t))))
          (incf branch-count)
          (branch-to end-label)))
      (emit-label end-label)
      (setf *stack-values* (copy-stack-values stack-slots))
      (unless (zerop branch-count)
        (cond ((member *for-value* '(:multiple :tail))
               (emit-gc-info :multiple-values 0)
               (setf *x0-value* :multiple))
              (t (emit-gc-info)
                 (setf *x0-value* (list (gensym)))))))))

(defun cg-let (form)
  (let* ((bindings (ast-bindings form))
         (variables (mapcar 'first bindings))
         (body (ast-body form)))
    ;; Ensure there are no non-local variables or special bindings.
    (assert (every (lambda (x)
                     (and (lexical-variable-p x)
                          (localp x)))
                   variables))
    (dolist (b bindings)
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
                   (load-in-x0 tag t)
                   (setf *x0-value* (cons var :dup))
                   (emit-stack-store :x0 slot)))))))
    (cg-form body)))

(defun gensym-many (things)
  (loop for x in things collect (gensym)))

(defun cg-multiple-value-bind (form)
  (let ((variables (ast-bindings form))
        (value-form (ast-value-form form))
        (body (ast-body form)))
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
          (emit-stack-store :x26 slot))))
    ;; Compile the value-form.
    (let ((value-tag (let ((*for-value* :multiple))
                       (cg-form value-form))))
      (load-multiple-values value-tag))
    ;; Bind variables.
    (let* ((jump-targets (gensym-many variables))
           (no-vals-label (gensym))
           (var-count (length variables))
           (value-locations (nreverse (subseq '(:x0 :x1 :x2 :x3 :x4) 0 (min 5 var-count)))))
      (dotimes (i (- var-count 5))
        (push i value-locations))
      (dotimes (i var-count)
        (emit `(lap:subs :xzr :x5 ,(fixnum-to-raw (- var-count i)))
              `(lap:b.cs ,(nth i jump-targets))))
      (emit `(lap:b ,no-vals-label))
      (loop
         for var in (reverse variables)
         for label in jump-targets
         do
           (emit label)
           (cond ((zerop (lexical-variable-use-count var))
                  (pop value-locations))
                 (t (let ((register (cond ((integerp (first value-locations))
                                           (emit-object-load :x7 :x28
                                                             :slot (+ #+(or)sys.int::+stack-group-offset-mv-slots+
                                                                      32 ; fixme. should be +thread-mv-slots-start+
                                                                      (pop value-locations)))
                                           :x7)
                                          (t (pop value-locations)))))
                      (emit-stack-store register
                                        (position (cons var :home)
                                                  *stack-values*
                                                  :test 'equal))))))
      (emit no-vals-label))
    (emit-gc-info)
    (cg-form body)))

(defun emit-funcall-common ()
  "Emit the common code for funcall, argument registers must
be set up and the function must be in RBX.
Returns an appropriate tag."
  (emit-object-load :x9 :x6 :slot sys.int::+function-entry-point+)
  (emit `(lap:blr :x9))
  (cond ((member *for-value* '(:multiple :tail))
         (emit-gc-info :multiple-values 0)
         :multiple)
        (t (emit-gc-info)
           (setf *x0-value* (list (gensym))))))

(defun cg-multiple-value-call (form)
  (let ((value-form (ast-value-form form))
        (fn-tag (let ((*for-value* t))
                  (cg-form (make-instance 'ast-call
                                          :name 'sys.int::%coerce-to-callable
                                          :arguments (list (ast-function-form form)))))))
    (when (not fn-tag)
      (return-from cg-multiple-value-call nil))
    (let ((value-tag (let ((*for-value* :multiple))
                       (cg-form value-form))))
      (case value-tag
        ((nil)
         (return-from cg-multiple-value-call nil))
        ((:multiple)
         (let ((stack-pointer-save-area (allocate-control-stack-slots 1)))
           (emit `(lap:add :x9 :sp 0))
           (emit-stack-store :x9 stack-pointer-save-area)
           ;; Copy values in the sg-mv area to the stack. RCX holds the number of values to copy +5
           (let ((loop-head (gensym))
                 (loop-exit (gensym))
                 (clear-loop-head (gensym)))
             ;; X9 = n values to copy (count * 8).
             (emit `(lap:add :x9 :xzr :x5 :lsl ,(1- (integer-length (/ 8 (ash 1 sys.int::+n-fixnum-bits+)))))
                   `(lap:subs :x9 :x9 (* 5 8))
                   `(lap:b.le ,loop-exit)
                   `(lap:and :x10 :x9 ,(lognot 15))
                   `(lap:sub :sp :sp :x10)
                   ;; Clear stack slots.
                   `(lap:add :x10 :sp :x9)
                   clear-loop-head
                   `(lap:str :xzr (:x10 -8))
                   `(lap:subs :x10 :x10 8)
                   `(lap:subs :xzr :sp :x10)
                   `(lap:b.ne ,clear-loop-head)
                   ;; Copy values.
                   `(lap:add :x12 :sp :xzr)
                   `(lap:movz :x11 ,(+ (- 8 sys.int::+tag-object+)
                                       ;; fixme. should be +thread-mv-slots-start+
                                       (* 32 8))))
             ;; Switch to the right GC mode.
             (emit-gc-info :pushed-values -5 :pushed-values-register :rcx :multiple-values 0)
             (emit loop-head
                   `(lap:ldr :x6 (:x28 :x11))
                   `(lap:str :x6 (:x12))
                   `(lap:add :x12 :x12 8)
                   `(lap:add :x11 :x11 8)
                   `(lap:subs :x9 :x9 8)
                   `(lap:b.cc ,loop-head)
                   loop-exit)
             ;; All done with the MV area.
             (emit-gc-info :pushed-values -5 :pushed-values-register :rcx))
           (smash-x0)
           (load-in-reg :x6 fn-tag t)
           (prog1
               (emit-funcall-common)
             (emit-stack-load :x9 stack-pointer-save-area)
             (emit `(lap:add :sp :x9 0)))))
        (t ;; Single value.
         (load-in-reg :x6 fn-tag t)
         (load-constant :x5 1)
         (load-in-reg :x0 value-tag t)
         (emit-funcall-common))))))

(defun cg-multiple-value-prog1 (form)
  (cond
    ((null *for-value*)
     ;; Not for value
     (cg-form (make-instance 'ast-progn :forms (list (ast-value-form form)
                                                     (ast-body form)))))
    (t (let ((tag (let ((*for-value* (case *for-value*
                                       (:predicate t)
                                       (:tail :multiple)
                                       (t *for-value*))))
                    (cg-form (ast-value-form form))))
             (sv-save-area (allocate-control-stack-slots 1 t))
             (saved-stack-pointer (allocate-control-stack-slots 1))
             (save-done (gensym "VALUES-SAVE-DONE"))
             (save-loop-head (gensym "VALUES-SAVE-LOOP")))
         (smash-x0)
         (when (eql tag :multiple)
           ;; Allocate an appropriately sized DX simple vector.
           ;; Add one for the header, then round the count up to an even number.
           (emit `(lap:add :x9 :x5 ,(fixnum-to-raw 2)))
           (emit `(lap:and :x9 :x9 ,(fixnum-to-raw (lognot 1))))
           ;; Save SP.
           (emit `(lap:add :x10 :sp :xzr))
           (emit-stack-store :x10 saved-stack-pointer)
           ;; Adjust RSP. rax to raw * 8.
           (emit `(lap:add :x9 :xzr :x9 :lsl ,(- 3 sys.int::+n-fixnum-bits+)))
           (emit `(lap:sub :sp :sp :x9))
           ;; Write the simple-vector header.
           (emit `(lap:add :x9 :xzr :x5 :lsl ,(- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))
           (emit `(lap:str :x9 (:sp)))
           ;; Clear the SV body. Don't modify RCX, needed for MV GC info.
           (let ((clear-loop-head (gensym "MVP1-CLEAR-LOOP"))
                 (clear-loop-end (gensym "MVP1-CLEAR-LOOP-END")))
             (emit `(lap:orr :x10 :xzr :x5))
             (emit `(lap:cbz :x10 ,clear-loop-end))
             (emit `(lap:add :x11 :sp 8))
             (emit clear-loop-head)
             (emit `(lap:str :xzr (:post :x11 8)))
             (emit `(lap:sub :x10 :x10 ,(fixnum-to-raw 1)))
             (emit `(lap:cbnz :x10 ,clear-loop-head))
             (emit clear-loop-end))
           ;; Create & save the DX root value.
           (emit `(lap:add :x9 :sp ,sys.int::+tag-dx-root-object+))
           (emit-stack-store :x9 sv-save-area)
           ;; Save MV registers.
           (loop
              for reg in '(:x0 :x1 :x2 :x3 :x4)
              for offset from 0
              do
                (emit `(lap:subs :xzr :x5 ,(fixnum-to-raw offset)))
                (emit `(lap:b.le ,save-done))
                ;; 1+ to skip header.
                (emit `(lap:str ,reg (:sp ,(* (1+ offset) 8)))))
           ;; Save values in the MV area.
           ;; Number of values remaining.
           (emit `(lap:subs :x9 :x5 ,(fixnum-to-raw 5)))
           (emit `(lap:b.le ,save-done))
           ;; Save into the simple-vector.
           (emit `(lap:add :x12 :sp ,(* 6 8))) ; skip header and registers.
           ;; Load from the MV area.
           (emit `(lap:add :x11 :x28 ,(+ (- 8 sys.int::+tag-object+)
                                         ;; fixme. should be +thread-mv-slots-start+
                                         (* #+(or)sys.int::+stack-group-offset-mv-slots+ 32 8))))
           ;; Save the values into a simple-vector.
           (emit save-loop-head)
           (emit `(lap:ldr :x6 (:post :x11 8)))
           (emit `(lap:str :x6 (:post :x12 8)))
           (emit `(lap:subs :x9 :x9 ,(fixnum-to-raw 1)))
           (emit `(lap:b.ne ,save-loop-head))
           ;; Finished saving values. Turn MV GC mode off.
           (emit save-done)
           (emit-gc-info)
           (add-dx-root sv-save-area))
         (let ((*for-value* nil))
           (when (not (cg-form (ast-body form)))
             ;; No return.
             (kill-value tag)
             (return-from cg-multiple-value-prog1 'nil)))
         (smash-x0)
         ;; Drop the tag from the load-list to prevent duplicates caused by cg-form
         (kill-value tag)
         (when (eql tag :multiple)
           ;; Create a normal object from the saved dx root.
           (emit-stack-load :x9 sv-save-area)
           (emit `(lap:add :x0 :x9 ,(- sys.int::+tag-object+
                                       sys.int::+tag-dx-root-object+)))
           ;; Call helper.
           (load-literal :x5 (fixnum-to-raw 1))
           (emit `(lap:ldr :x7 (:function sys.int::values-simple-vector)))
           (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
           (emit `(lap:blr :x9))
           (emit-gc-info :multiple-values 0)
           ;; Kill the dx root and restore the old stack pointer.
           (emit-stack-store :x26 sv-save-area)
           (emit-stack-load :x9 saved-stack-pointer)
           (emit `(lap:add :sp :x9 :xzr)))
         tag))))

(defun cg-progn (form)
  (if (ast-forms form)
      (do ((i (ast-forms form) (rest i)))
          ((endp (rest i))
           (cg-form (first i)))
        (let* ((*for-value* nil)
               (tag (cg-form (first i))))
          (when (null tag)
            (return-from cg-progn 'nil))))
      (cg-form (make-instance 'ast-quote :value 'nil))))

(defun cg-quote (form)
  `(quote ,(ast-value form)))

(defun cg-return-from (form)
  (let* ((local-info (assoc (ast-target form) *rename-list*))
         (*for-value* (block-information-return-mode (ast-target form)))
         (target-tag (when (not local-info)
                       (let ((*for-value* t))
                         (cg-form (ast-info form)))))
         (tag (cg-form (ast-value form))))
    (unless tag (return-from cg-return-from nil))
    (cond ((member *for-value* '(:multiple :tail))
           (load-multiple-values tag))
          (*for-value*
           (load-in-x0 tag t)))
    (incf (block-information-count (ast-target form)))
    (smash-x0)
    (cond (local-info ;; Local jump.
           (emit `(lap:b ,(second local-info))))
          (t ;; Non-local exit.
           (load-in-reg :x9 target-tag t)
           (emit `(lap:ldr :x10 (:x9))
                 `(lap:br :x10))))
    'nil))

(defun find-variable-home (var)
  (dotimes (i (length *stack-values*)
            (error "No home for ~S?" var))
    (let ((x (aref *stack-values* i)))
      (when (and (consp x) (eq (car x) var) (eq (cdr x) :home))
        (return i)))))

(defun cg-setq (form)
  (let ((var (ast-setq-variable form))
        (val (ast-value form)))
    (assert (localp var))
    ;; Copy var if there are unsatisfied tags on the load list.
    (dolist (l *load-list*)
      (when (and (consp l) (eq (car l) var))
        ;; Don't save if there is something satisfying this already.
        (multiple-value-bind (loc true-tag)
            (value-location l)
          (declare (ignore loc))
          (unless (tag-saved-on-stack-p true-tag)
            (load-in-x0 l nil)
            (smash-x0 t)))))
    (let ((tag (let ((*for-value* t))
                 (cg-form val)))
          (home (find-variable-home var)))
      (when (null tag)
        (return-from cg-setq))
      (load-in-x0 tag t)
      (emit-stack-store :x0 home)
      (setf *x0-value* (cons var :dup))
      (cons var (incf *run-counter* 2)))))

(defun tagbody-localp (info)
  (dolist (tag (tagbody-information-go-tags info) t)
    (unless (or (null (go-tag-used-in tag))
                (and (null (cdr (go-tag-used-in tag)))
                     (eq (car (go-tag-used-in tag)) (lexical-variable-definition-point info))))
      (return nil))))

(defun cg-tagbody (form)
  (let ((*for-value* nil)
        (stack-slots nil)
        (*rename-list* *rename-list*)
        (need-exit-label nil)
        (exit-label (gensym "tagbody-exit"))
        (escapes (not (tagbody-localp (ast-info form))))
        (jump-table (gensym))
        (tag-labels (mapcar (lambda (tag)
                              (declare (ignore tag))
                              (gensym))
                            (tagbody-information-go-tags (ast-info form))))
        (thunk-labels (mapcar (lambda (tag)
                                (declare (ignore tag))
                                (gensym))
                              (tagbody-information-go-tags (ast-info form))))
        (*active-nl-exits* (list* '() *active-nl-exits*)))
    (when escapes
      (smash-x0)
      (let ((slot (find-stack-slot))
            (control-info (allocate-control-stack-slots 4)))
        ;; Construct jump info.
        (emit `(lap:adr :x9 ,jump-table))
        (emit-stack-store :x9 (+ control-info 3))
        (emit-object-load :x9 :x28 :slot 6) ;; ### special-stack-pointer
        (emit-stack-store :x9 (+ control-info 2))
        (emit `(lap:add :x9 :sp :xzr))
        (emit-stack-store :x9 (+ control-info 1))
        (emit-stack-store :x29 (+ control-info 0))
        ;; Save in the environment.
        (load-literal :x9 (control-stack-frame-offset (+ control-info 3)))
        (emit `(lap:add :x9 :x29 :x9))
        (emit-stack-store :x9 slot)
        (setf (aref *stack-values* slot) (cons (ast-info form) :home))))
    (setf stack-slots (set-up-for-branch))
    (mapcar (lambda (tag label)
              (push (list tag label) *rename-list*))
            (tagbody-information-go-tags (ast-info form)) tag-labels)
    (loop
       for (go-tag stmt) in (ast-statements form)
       do
         (smash-x0)
         (setf *stack-values* (copy-stack-values stack-slots))
         (emit (second (assoc go-tag *rename-list*)))
         (when (cg-form stmt)
           (setf need-exit-label t)
           (emit `(lap:b ,exit-label))))
    (when escapes
      ;; Emit the jump-table.
      ;; TODO: Prune local labels out.
      (emit-trailer (jump-table nil)
        (dolist (i thunk-labels)
          (emit `(:d64/le (- ,i ,jump-table)))))
      ;; And the all the thunks.
      (loop
         for thunk in thunk-labels
         for label in tag-labels
         do
           (emit-nlx-thunk thunk label nil)))
    (cond (need-exit-label
           (emit exit-label)
           ''nil)
          (t nil))))

(defun cg-the (form)
  (cg-form (ast-value form)))

(defun flush-arguments-from-stack (arg-forms)
  (let ((stack-count (max 0 (- (length arg-forms) 5))))
    (when (plusp stack-count)
      (when (oddp stack-count) (incf stack-count))
      (emit `(lap:add :sp :sp ,(* stack-count 8))))))

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
              (kill-value i))
            (return-from prep-arguments-for-call nil))))
      (setf args (nreverse args))
      (let ((stack-count (- arg-count 5)))
        (when (plusp stack-count)
          (when (oddp stack-count)
            (incf stack-count))
          (emit `(lap:sub :sp :sp ,(* stack-count 8)))
          ;; Load values on the stack.
          ;; Use x7 here to preserve whatever is in x0.
          ;; Must load first values first, so the GC can track properly.
          (loop
             for i from 0
             for j in (nthcdr 5 args)
             do
               (load-in-reg :x7 j t)
               (emit `(lap:str :x7 (:sp ,(* i 8))))
               (emit-gc-info :pushed-values (1+ i)))))
      ;; Load other values in registers.
      (when (> arg-count 4)
        (load-in-reg :x4 (nth 4 args) t))
      (when (> arg-count 3)
        (load-in-reg :x3 (nth 3 args) t))
      (when (> arg-count 2)
        (load-in-reg :x2 (nth 2 args) t))
      (when (> arg-count 1)
        (load-in-reg :x1 (nth 1 args) t))
      (when (> arg-count 0)
        (load-in-x0 (nth 0 args) t))))
  t)

;; Compile a VALUES form.
(defun cg-values (forms)
  (cond ((null forms)
         ;; No values.
         (cond ((member *for-value* '(:multiple :tail))
                ;; X0 must hold NIL.
                (load-in-x0 ''nil)
                (emit `(lap:orr :x5 :xzr :xzr))
                :multiple)
               (t (cg-form (make-instance 'ast-quote :value 'nil)))))
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
                   (kill-value i))
                 (return-from cg-values nil))))
           (comment 'values)
           (setf args (nreverse args))
           ;; Load the first values in registers.
           (when (> arg-count 4)
             (load-in-reg :x4 (nth 4 args) t))
           (when (> arg-count 3)
             (load-in-reg :x3 (nth 3 args) t))
           (when (> arg-count 2)
             (load-in-reg :x2 (nth 2 args) t))
           (when (> arg-count 1)
             (load-in-reg :x1 (nth 1 args) t))
           (when (> arg-count 0)
             (load-in-x0 (nth 0 args) t))
           ;; Only the register values.
           (load-constant :x5 (min (length args) 5))
           ;; Now add MVs to the stack one by one.
           (loop for i from 0
              for value in (nthcdr 5 args) do
                (load-in-reg :x7 value t)
                ;; fixme. should be +thread-mv-slots-start+
                (emit-object-store :x7 :x28 :slot (+ 32 i))
                (emit-gc-info :multiple-values 1)
                (emit `(lap:add :x5 :x5 ,(fixnum-to-raw 1)))
                (emit-gc-info :multiple-values 0))
           :multiple))
        (t ;; VALUES behaves like PROG1 when not compiling for multiple values.
         (let ((tag (cg-form (first forms))))
           (unless tag (return-from cg-values nil))
           (let ((*for-value* nil))
             (dolist (f (rest forms))
               (when (not (cg-form f))
                 (kill-value tag)
                 (return-from cg-values nil))))
           tag))))

(defun cg-builtin (fn form)
  (let ((args '()))
    (let ((*for-value* t))
      (dolist (f (ast-arguments form))
        (push (cg-form f) args)
        (when (null (first args))
          ;; Non-local control transfer, don't actually need those results now.
          (dolist (i (rest args))
            (kill-value i))
          (return-from cg-builtin nil))))
    (comment (ast-name form))
    (apply fn (nreverse args))))

(defun cg-funcall (form)
  (let* ((fn-tag (let ((*for-value* t))
                   (cg-form (if (typep (first (ast-arguments form)) 'lambda-information)
                                (first (ast-arguments form))
                                (make-instance 'ast-call
                                               :name 'sys.int::%coerce-to-callable
                                               :arguments (list (first (ast-arguments form)))))))))
    (cond ((prep-arguments-for-call (rest (ast-arguments form)))
           (comment 'funcall)
           (load-in-reg :x6 fn-tag t)
           (smash-x0)
           (load-constant :x5 (length (rest (ast-arguments form))))
           (cond ((can-tail-call (rest (ast-arguments form)))
                  (emit-tail-call :x6 0))
                 (t
                  (emit-object-load :x9 :x6 :slot 0)
                  (emit `(lap:blr :x9))))
           (if (member *for-value* '(:multiple :tail))
               (emit-gc-info :multiple-values 0)
               (emit-gc-info))
           (flush-arguments-from-stack (rest (ast-arguments form)))
           (cond ((can-tail-call (rest (ast-arguments form))) nil)
                 ((member *for-value* '(:multiple :tail))
                  :multiple)
                 (t (setf *x0-value* (list (gensym))))))
          (t ;; Flush the unused function.
           (kill-value fn-tag)))))

(defun cg-call (form)
  (when (prep-arguments-for-call (ast-arguments form))
    (comment (ast-name form))
    (emit `(lap:ldr :x7 (:function ,(ast-name form))))
    (smash-x0)
    (load-constant :x5 (length (ast-arguments form)))
    (cond ((can-tail-call (ast-arguments form))
           (emit-tail-call :x7 sys.int::+fref-entry-point+)
           nil)
          (t
           (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
           (emit `(lap:blr :x9))
           (if (member *for-value* '(:multiple :tail))
               (emit-gc-info :multiple-values 0)
               (emit-gc-info))
           (flush-arguments-from-stack (ast-arguments form))
           (cond ((member *for-value* '(:multiple :tail))
                  :multiple)
                 (t (setf *x0-value* (list (gensym)))))))))

(defun cg-function-form (form)
  (let ((fn (when (not sys.c::*suppress-builtins*)
              (match-builtin (ast-name form) (length (ast-arguments form))))))
    (cond ((eql (ast-name form) 'sys.c::make-dx-simple-vector)
           (cg-make-dx-simple-vector form))
          (fn
           (cg-builtin fn form))
          ((and (eql (ast-name form) 'funcall)
                (ast-arguments form))
           (cg-funcall form))
          ((eql (ast-name form) 'values)
           (cg-values (ast-arguments form)))
          (t (cg-call form)))))

(defun can-tail-call (args)
  (and (eql *for-value* :tail)
       (<= (length args) 5)))

(defun emit-tail-call (where where-slot)
  (emit `(lap:add :sp :x29 0))
  (emit `(:gc :frame :multiple-values 0))
  (emit `(lap:ldp :x29 :x30 (:post :sp 16)))
  ;; Tail calls can only be performed when there are no arguments on the stack.
  ;; So :GC :NO-FRAME is fine here.
  (emit `(:gc :no-frame))
  (emit-object-load :x9 where :slot where-slot)
  (emit `(lap:br :x9)))

(defun cg-variable (form)
  (assert (localp form))
  (cons form (incf *run-counter*)))

(defun cg-lambda (form)
  (list 'quote (codegen-lambda form)))

(defun cg-jump-table (form)
  (let* ((value (ast-value form))
         (jumps (ast-targets form))
         (tag (let ((*for-value* t))
                (cg-form value)))
         (jump-table (gensym "jump-table")))
    ;; Build the jump table.
    ;; Every jump entry must be a local GO with no special bindings.
    (emit-trailer (jump-table nil)
      (dolist (j jumps)
        (assert (typep j 'ast-go))
        (let ((go-tag (assoc (ast-target j) *rename-list*)))
          (assert go-tag () "GO tag not local")
          (emit `(:d64/le (- ,(second go-tag) ,jump-table))))))
    ;; Jump.
    (load-in-x0 tag t)
    (smash-x0)
    (emit `(lap:adr :x9 ,jump-table))
    (emit `(lap:add :x0 :xzr :x0 :lsl ,(1- (integer-length (/ 8 (ash 1 sys.int::+n-fixnum-bits+))))))
    (emit `(lap:ldr :x10 (:x9 :x0)))
    (emit `(lap:add :x9 :x9 :x10))
    (emit `(lap:br :x9))
    nil))
