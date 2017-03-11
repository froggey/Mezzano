;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.lap)

(defvar *function-reference-resolver* nil
  "Function used to convert :function memory references into constants.")

(defvar *current-address* nil
  "Address of the current instruction.")
(defvar *machine-code* nil
  "Buffer containing emitted machine code.")
(defvar *symbol-table* nil
  "The current symbol table.")
(defvar *prev-symbol-table* nil)
(defvar *constant-pool* nil
  "The constant pool.")
(defvar *mc-end* nil)
(defvar *missing-symbols* nil)
(defvar *bytes-emitted* nil)
(defvar *fixups* nil)
(defvar *gc-data* nil)

(defparameter *settle-limit* 50)

(defun emit (&rest bytes)
  "Emit bytes to the output stream."
  (dolist (i bytes)
    (check-type i (unsigned-byte 8))
    (incf *current-address*)
    (incf *bytes-emitted*)
    (vector-push-extend i *machine-code*)))

(defun perform-assembly (instruction-set code-list &key (base-address 0) (initial-symbols '()) info &allow-other-keys)
  "Assemble a list of instructions, returning a u-b 8 vector of machine code,
a vector of constants and an alist of symbols & addresses."
  (do ((*constant-pool* (let ((x (make-array (+ (length info) 16)
                                             :fill-pointer (length info)
                                             :adjustable t)))
                          (setf (subseq x 0 (length info)) info)
                          x))
       (prev-bytes-emitted nil)
       (*bytes-emitted* 0)
       (failed-instructions 0 0)
       (*missing-symbols* '())
       (*mc-end* nil (+ base-address (length *machine-code*)))
       (*machine-code* nil)
       (prev-mc nil)
       (*current-address* base-address base-address)
       (*symbol-table* nil)
       (*prev-symbol-table* (make-hash-table) *symbol-table*)
       (*fixups* '())
       (*gc-data* '())
       (attempt 0 (1+ attempt)))
      ((and (eql prev-bytes-emitted *bytes-emitted*)
            (equalp prev-mc *machine-code*))
       (when *missing-symbols*
         (error "Assembly failed. Missing symbols: ~S." *missing-symbols*))
       (setf *gc-data* (reverse *gc-data*))
       ;; Flush identical GC info entries.
       (setf *gc-data* (loop for entry in *gc-data*
                          with prev = nil
                          unless (equal (rest prev) (rest entry))
                          collect entry
                          do (setf prev entry)))
       (values *machine-code*
               *constant-pool*
               *fixups*
               (let ((alist '()))
                 (maphash (lambda (k v)
                            (push (cons k v) alist))
                          *symbol-table*)
                 alist)
               (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                      (mapcar 'encode-gc-info *gc-data*))))
    (when (> attempt *settle-limit*)
      (error "Internal assembler error. Code has not settled after ~D iterations." attempt))
    (setf prev-bytes-emitted *bytes-emitted*
          *bytes-emitted* 0
          prev-mc *machine-code*
          *machine-code* (make-array 128
                                   :element-type '(unsigned-byte 8)
                                   :fill-pointer 0
                                   :adjustable t)
          *symbol-table* (let ((hash-table (make-hash-table)))
                           (dolist (x initial-symbols)
                             (setf (gethash (first x) hash-table) (rest x)))
                           hash-table)
          *missing-symbols* '()
          *fixups* '()
          *gc-data* '())
    (dolist (i code-list)
      (etypecase i
        (symbol (when (gethash i *symbol-table*)
                  (cerror "Replace the existing symbol." "Duplicate symbol ~S." i))
                (setf (gethash i *symbol-table*) *current-address*))
        (cons (with-simple-restart (continue "Skip it.")
                (if (keywordp (first i))
                    (ecase (first i)
                      (:comment)
                      (:d8 (apply 'emit-d8 (rest i)))
                      (:d16/le (apply 'emit-d16/le (rest i)))
                      (:d32/le (apply 'emit-d32/le (rest i)))
                      (:d64/le (apply 'emit-d64/le (rest i)))
                      (:gc (apply 'emit-gc (rest i))))
                    (let ((handler (gethash (first i) instruction-set)))
                      (if handler
                          (unless (funcall handler i)
                            (incf failed-instructions))
                          (error "Unrecognized instruction ~S." (first i)))))))))))

(defun emit-d8 (&rest args)
  (dolist (a args)
    (emit a)))

(defun emit-d16/le (&rest args)
  (dolist (a args)
    (setf a (or (resolve-immediate a) 0))
    (check-type a (unsigned-byte 16))
    (emit (ldb (byte 8 0) a)
          (ldb (byte 8 8) a))))

(defun emit-d32/le (&rest args)
  (dolist (a args)
    (let ((val (or (resolve-immediate a) 0)))
      (cond ((eql val :fixup)
             (note-fixup a)
             (emit #xFF #xFF #xFF #xFF))
            (t
             (check-type val (unsigned-byte 32))
             (emit (ldb (byte 8 0) val)
                   (ldb (byte 8 8) val)
                   (ldb (byte 8 16) val)
                   (ldb (byte 8 24) val)))))))

(defun emit-d64/le (&rest args)
  (dolist (a args)
    (let ((val (or (resolve-immediate a) 0)))
      (cond ((eql val :fixup)
             (note-fixup a)
             (emit #xFF #xFF #xFF #xFF #x00 #x00 #x00 #x00))
            (t
             (check-type val (or (unsigned-byte 64)
                                 (signed-byte 64)))
             (emit (ldb (byte 8 0) val)
                   (ldb (byte 8 8) val)
                   (ldb (byte 8 16) val)
                   (ldb (byte 8 24) val)
                   (ldb (byte 8 32) val)
                   (ldb (byte 8 40) val)
                   (ldb (byte 8 48) val)
                   (ldb (byte 8 56) val)))))))

(defun immediatep (thing)
  "Test if THING is an immediate value."
  (or (symbolp thing)
      (integerp thing)))

(defun resolve-immediate (value)
  "Convert an immediate value to an integer."
  (etypecase value
    (cons (cond ((keywordp (first value))
                 (ecase (first value)
                   (:constant-address
                    (when *mc-end*
                      (+ (* (ceiling *mc-end* 16) 16)
                         (* (or (position (second value) *constant-pool*)
                                (vector-push-extend (second value) *constant-pool*)) 8))))))
                (t (let ((args (mapcar #'resolve-immediate (rest value))))
                     (unless (member nil args)
                       (apply (first value) args))))))
    (symbol (let ((val (gethash value *symbol-table*))
                  (old-val (gethash value *prev-symbol-table*)))
              (cond (val)
                    (old-val)
                    (t (pushnew value *missing-symbols*)
                       nil))))
    (integer value)))

(defun note-fixup (name)
  (push (cons name *current-address*) *fixups*))

(defun emit-gc (&rest args)
  (destructuring-bind (frame-mode &key (layout #*) (pushed-values 0) incoming-arguments interrupt multiple-values pushed-values-register block-or-tagbody-thunk extra-registers restart)
      args
    (check-type frame-mode (member :frame :no-frame))
    (check-type layout bit-vector)
    (check-type pushed-values (signed-byte 32))
    (check-type incoming-arguments (or null
                                       (eql :rcx)
                                       (integer 0 14)))
    (check-type interrupt boolean)
    (check-type multiple-values (or null (unsigned-byte 4)))
    (check-type pushed-values-register (or null
                                           (member :rax :rcx :rdx :rbx :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)))
    (check-type block-or-tagbody-thunk (or null
                                           (member :rax)))
    ;; Canonicalise keyword order, so EQUAL can be used to strip duplicates.
    (let ((gc-keys '()))
      (when extra-registers
        (setf (getf gc-keys :extra-registers) extra-registers))
      (when block-or-tagbody-thunk
        (setf (getf gc-keys :block-or-tagbody-thunk) block-or-tagbody-thunk))
      (when pushed-values-register
        (setf (getf gc-keys :pushed-values-register) pushed-values-register))
      (when multiple-values
        (setf (getf gc-keys :multiple-values) multiple-values))
      (when interrupt
        (setf (getf gc-keys :interrupt) interrupt))
      (when incoming-arguments
        (setf (getf gc-keys :incoming-arguments) incoming-arguments))
      (when (not (zerop pushed-values))
        (setf (getf gc-keys :pushed-values) pushed-values))
      (when (not (zerop (length layout)))
        (setf (getf gc-keys :layout) layout))
      (when restart
        (setf (getf gc-keys :restart) t))
      ;; Most recent takes priority.
      (if (eql (first (first *gc-data*)) *current-address*)
          (setf (first *gc-data*) (list* *current-address*
                                         frame-mode
                                         gc-keys))
          (push (list* *current-address*
                       frame-mode
                       gc-keys)
                *gc-data*)))))

(defun append-vu32 (value vector)
  (let ((low-bits (ldb (byte 7 0) value))
        (high-bits (ash value -7)))
    (cond ((zerop high-bits)
           (vector-push-extend low-bits vector))
          (t (vector-push-extend (logior low-bits #x80) vector)
             (append-vu32 high-bits vector)))))

(defun append-vs32 (integer vector)
  (let ((negativep (minusp integer)))
    (when negativep (setf integer (- integer)))
    (do ()
        ((zerop (logand integer (lognot #x3F)))
         (vector-push-extend (logior integer (if negativep #x40 0))
                             vector))
      (vector-push-extend (logior #x80 (logand integer #x7F))
                          vector)
      (setf integer (ash integer -7)))))

(defun encode-gc-info (info)
  (destructuring-bind (address frame-mode
                               &key
                               (layout #*)
                               (pushed-values 0)
                               incoming-arguments
                               interrupt
                               multiple-values
                               pushed-values-register
                               block-or-tagbody-thunk
                               extra-registers
                               restart)
      info
    (let ((bytes (make-array 10 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (append-vu32 address bytes)
      (vector-push-extend (logior (ecase frame-mode
                                    (:frame
                                     #b00000001)
                                    (:no-frame 0))
                                  (cond
                                    (interrupt
                                     #b00000010)
                                    (t 0))
                                  (cond
                                    (block-or-tagbody-thunk
                                     (assert (eql block-or-tagbody-thunk :rax))
                                     #b00000100)
                                    (t 0))
                                  (cond
                                    (incoming-arguments
                                     #b00001000)
                                    (t 0))
                                  (cond
                                    (pushed-values-register
                                     (assert (eql pushed-values-register :rcx))
                                     #b00010000)
                                    (t 0))
                                  (ecase extra-registers
                                    ((nil) 0)
                                    ((:rax)         #b00100000)
                                    ((:rax-rcx)     #b01000000)
                                    ((:rax-rcx-rdx) #b01100000))
                                  (cond
                                    (restart
                                     #b10000000)
                                    (t 0)))
                          bytes)
      (vector-push-extend (logior (or multiple-values #b1111)
                                  (ash (cond ((keywordp incoming-arguments)
                                              (assert (eql incoming-arguments :rcx))
                                              15)
                                             (incoming-arguments
                                              (check-type incoming-arguments (integer 0 (15)))
                                              incoming-arguments)
                                             (t 15))
                                       4))
                          bytes)
      (append-vs32 pushed-values bytes)
      (append-vu32 (length layout) bytes)
      (dotimes (i (ceiling (length layout) 8))
        (let ((byte 0))
          (dotimes (bit 8)
            (let ((offs (+ (* i 8) bit)))
              (when (< offs (length layout))
                (setf (ldb (byte 1 bit) byte) (bit layout offs)))))
          (vector-push-extend byte bytes)))
      bytes)))
