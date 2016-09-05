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

(defconstant +binding-stack-gs-offset+ (- (* 7 8) sys.int::+tag-object+))

(defun emit (&rest instructions)
  (dolist (i instructions)
    (push i *code-accum*)))

(defun comment (&rest stuff)
  (emit `(:comment ,@stuff)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun object-ea (base &key (slot 0) index)
    (append
     (when base
       (list base))
     (when index
       (list index))
     (list (+ (- sys.int::+tag-object+) 8 (* slot 8)))))
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

(defun value-location (tag &optional kill)
  (when kill
    (setf *load-list* (delete tag *load-list*)))
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

(defun smash-x0 (&optional do-not-kill-x0)
  "Check if the value in X0 is on the load-list and flush it to the stack if it is."
  ;; Avoid flushing if it's already on the stack.
  (when (and *x0-value*
             (not (eql *x0-value* :multiple))
             (not (condemed-p *x0-value*))
             (not (tag-saved-on-stack-p *x0-value*)))
    (let ((slot (find-stack-slot)))
      (setf (aref *stack-values* slot) *x0-value*)
      (emit `(lap:str :x0 (:stack ,slot)))))
  (unless do-not-kill-x0
    (setf *x0-value* nil)))

(defun load-literal (register value)
  (cond ((zerop value)
         (emit `(lap:orr ,register :wzr 0)))
        (t
         (emit `(lap:ldr ,register (:literal ,value))))))

(defun load-constant (register value)
  (cond ((eq value 'nil)
         (emit `(lap:orr ,register :wzr :x26)))
        ((fixnump value)
         (load-literal register (fixnum-to-raw value)))
        ((characterp value)
         (load-literal register (character-to-raw value)))
        (t (emit `(lap:ldr ,register (:constant ,value))))))

(defun load-multiple-values (tag)
  (cond ((eql tag :multiple))
        (t (load-in-x0 tag t)
           (emit `(lap:ldr :x5 (:literal ,(fixnum-to-raw 1)))))))

(defun load-in-x0 (tag &optional kill)
  (multiple-value-bind (loc true-tag)
      (value-location tag nil)
    (unless (eq loc :x0)
      (smash-x0)
      (ecase (first loc)
        ((quote) (load-constant :x0 (second loc)))
        ((:stack)
         (emit `(lap:ldr :x0 (:stack ,(second loc))))))
      (setf *x0-value* true-tag))
    (when kill
      (setf *load-list* (delete tag *load-list*)))))

(defun load-in-reg (reg tag &optional kill)
  (if (eql reg :x0)
      (load-in-x0 tag kill)
      (let ((loc (value-location tag nil)))
        (unless (eql loc reg)
          (if (eql loc :x0)
              (emit `(lap:orr ,reg :wzr :x0))
              (ecase (first loc)
                ((quote) (load-constant reg (second loc)))
                ((:stack) (emit `(lap:ldr ,reg (:stack ,(second loc))))))))
        (when kill
          (setf *load-list* (delete tag *load-list*))))))

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
    ;; Free up :x6 quickly.
    (let ((env-arg (lambda-information-environment-arg lambda)))
      (when env-arg
        (let ((ofs (find-stack-slot)))
          (setf (aref *stack-values* ofs) (cons env-arg :home))
          (when (not (getf (lambda-information-plist lambda) 'sys.c::unwind-protect-cleanup))
            ;; Read environment pointer from closure object.
            (emit `(lap:ldr :x6 ,(object-ea :x6 :slot 2))))
          (emit `(lap:str :x6 (:stack ,ofs))))))
    ;; Compile argument setup code.
    (let ((current-arg-index 0))
      (dolist (arg (lambda-information-required-args lambda))
        (incf current-arg-index)
        (let ((ofs (find-stack-slot)))
          (setf (aref *stack-values* ofs) (cons arg :home))
          (if arg-registers
              (emit `(lap:str ,(pop arg-registers) (:stack ,ofs)))
              (emit `(lap:ldr :x0 (:x29 ,(* (+ (- current-arg-index 6) 2) 8)))
                    `(lap:str :x0 (:stack ,ofs))))))
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
          (if arg-registers
              (emit `(lap:str ,(pop arg-registers) (:stack ,var-ofs)))
              (emit `(lap:ldr :x0 (:x29 ,(* (+ (- current-arg-index 5) 2) 8)))
                    `(lap:str :x0 (:stack ,var-ofs))))
          (when sup-ofs
            (emit `(lap:ldr :x0 (:constant t))
                  `(lap:str :x0 (:stack ,sup-ofs))))
          (emit `(lap:b ,end-label)
                mid-label)
          ;; Argument not supplied. Init-form is a quoted constant.
          (let ((tag `',(ast-value (second arg))))
            (load-in-x0 tag t)
            (setf *x0-value* nil)
            (emit `(lap:str :x0 (:stack ,var-ofs)))
            (when sup-ofs
              (emit `(lap:str :x29 (:stack ,sup-ofs)))))
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
        (emit `(lap:add :sp :x30 0)
              `(:gc :frame :multiple-values 0)
              `(lap:ldp :x29 :x30 (:post :sp 16))
              `(:gc :no-frame :multiple-values 0)
              `(lap:ret))))
    (let* ((final-code (nconc (generate-entry-code lambda)
                              (nreverse *code-accum*)
                              (apply #'nconc *trailers*)))
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
             (lambda-information-docstring lambda))))))
