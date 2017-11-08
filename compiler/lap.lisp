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
(defvar *relocations* nil)
(defvar *symbols* nil)
(defvar *chunks*)
(defvar *in-pass1* nil)

(defparameter *settle-limit* 500)

(defvar *instruction-is-variably-sized*)

(defclass chunk ()
  ((%code :initarg :code :accessor chunk-code)))

(defclass fixed-sized-chunk (chunk)
  ((%symbols :initarg :symbols :reader chunk-symbols)
   (%relocations :initarg :relocations :reader chunk-relocations)
   (%gcmd :initarg :gcmd :reader chunk-gcmd)
   (%fixups :initarg :fixups :reader chunk-fixups)))

(defclass variably-sized-chunk (chunk)
  ((%instruction :initarg :instruction :reader chunk-instruction)))

(defun note-variably-sized-instruction ()
  (when *in-pass1*
    ;; Finish the current chunk.
    (push (make-instance 'fixed-sized-chunk
                         :code *machine-code*
                         :symbols *symbols*
                         :relocations *relocations*
                         :gcmd (reverse *gc-data*)
                         :fixups *fixups*)
          *chunks*)
    (setf *machine-code* (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
          *symbols* '()
          *relocations* '()
          *gc-data* '()
          *fixups* '()))
  (setf *instruction-is-variably-sized* t))

(defun emit (&rest bytes)
  "Emit bytes to the output stream."
  (dolist (i bytes)
    (check-type i (unsigned-byte 8))
    (incf *current-address*)
    ;(incf *bytes-emitted*)
    (vector-push-extend i *machine-code*)))

(defun emit-relocation (kind immediate addend)
  (when *instruction-is-variably-sized*
    (error "Variably-sized instructions must not have relocations."))
  (push (list kind immediate addend *current-address* (length *machine-code*)) *relocations*)
  (values))

(defun perform-relocation (vector offset kind immediate addend address what)
  (declare (ignore what))
  (ecase kind
    (:rel32le
     (let ((value (- (+ immediate addend) address)))
       (check-type value (signed-byte 32))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) value)
             (aref vector (+ offset 1)) (ldb (byte 8 8) value)
             (aref vector (+ offset 2)) (ldb (byte 8 16) value)
             (aref vector (+ offset 3)) (ldb (byte 8 24) value))))
    (:abs8
     (let ((value (+ immediate addend)))
       (check-type value (signed-byte 8))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) value))))
    (:absu8
     (let ((value (+ immediate addend)))
       (check-type value (unsigned-byte 8))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) value))))
    (:abs16le
     (let ((value (+ immediate addend)))
       (check-type value (signed-byte 16))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) value)
             (aref vector (+ offset 1)) (ldb (byte 8 8) value))))
    (:absu16le
     (let ((value (+ immediate addend)))
       (check-type value (unsigned-byte 16))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) value)
             (aref vector (+ offset 1)) (ldb (byte 8 8) value))))
    (:abs32le
     (let ((value (+ immediate addend)))
       (check-type value (signed-byte 32))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) value)
             (aref vector (+ offset 1)) (ldb (byte 8 8) value)
             (aref vector (+ offset 2)) (ldb (byte 8 16) value)
             (aref vector (+ offset 3)) (ldb (byte 8 24) value))))
    (:absu32le
     (let ((value (+ immediate addend)))
       (check-type value (unsigned-byte 32))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) value)
             (aref vector (+ offset 1)) (ldb (byte 8 8) value)
             (aref vector (+ offset 2)) (ldb (byte 8 16) value)
             (aref vector (+ offset 3)) (ldb (byte 8 24) value))))
    (:abs64le
     (let ((value (+ immediate addend)))
       (check-type value (signed-byte 64))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) value)
             (aref vector (+ offset 1)) (ldb (byte 8 8) value)
             (aref vector (+ offset 2)) (ldb (byte 8 16) value)
             (aref vector (+ offset 3)) (ldb (byte 8 24) value)
             (aref vector (+ offset 4)) (ldb (byte 8 32) value)
             (aref vector (+ offset 5)) (ldb (byte 8 40) value)
             (aref vector (+ offset 6)) (ldb (byte 8 48) value)
             (aref vector (+ offset 7)) (ldb (byte 8 56) value))))
    (:absu64le
     (let ((value (+ immediate addend)))
       (check-type value (unsigned-byte 64))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) value)
             (aref vector (+ offset 1)) (ldb (byte 8 8) value)
             (aref vector (+ offset 2)) (ldb (byte 8 16) value)
             (aref vector (+ offset 3)) (ldb (byte 8 24) value)
             (aref vector (+ offset 4)) (ldb (byte 8 32) value)
             (aref vector (+ offset 5)) (ldb (byte 8 40) value)
             (aref vector (+ offset 6)) (ldb (byte 8 48) value)
             (aref vector (+ offset 7)) (ldb (byte 8 56) value))))))

(defun perform-relocations (base-address)
  (loop for (kind immediate addend address) in *relocations* do
       (perform-relocation *machine-code*
                           (- address base-address)
                           kind
                           (or (resolve-immediate immediate)
                               (error "Failed to resolve immediate ~S during relocation" immediate))
                           addend
                           address
                           immediate)))

(defun assemble-one-instruction (instruction-set instruction)
  (if (keywordp (first instruction))
      (ecase (first instruction)
        (:comment)
        (:align
         (note-variably-sized-instruction)
         (destructuring-bind (alignment) (rest instruction)
           (let ((misalignment (rem *current-address* alignment)))
             (when (not (zerop misalignment))
               (loop
                  repeat (- alignment misalignment)
                  do (emit 0))))))
        (:d8 (apply 'emit-d8 (rest instruction)))
        (:d16/le (apply 'emit-d16/le (rest instruction)))
        (:d32/le (apply 'emit-d32/le (rest instruction)))
        (:d64/le (apply 'emit-d64/le (rest instruction)))
        (:gc (apply 'emit-gc (rest instruction))))
      (let ((handler (gethash (first instruction) instruction-set)))
        (if handler
            (funcall handler instruction)
            (error "Unrecognized instruction ~S." (first instruction))))))

(defun perform-assembly-pass1 (instruction-set code-list base-address)
  (let ((*current-address* base-address)
        (*in-pass1* t)
        (*machine-code* (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (*relocations* '())
        (*symbols* '())
        (*fixups* '())
        (*gc-data* '())
        (*chunks* '()))
    (dolist (i code-list)
      (let ((*instruction-is-variably-sized* nil))
        (etypecase i
          (symbol
           (when (gethash i *symbol-table*)
             (error "Duplicate symbol ~S." i))
           ;; Don't bake symbol addresses into fixed-size chunks.
           (setf (gethash i *symbol-table*) t)
           (push (list i (length *machine-code*)) *symbols*))
          (cons
           (assemble-one-instruction instruction-set i)))
        (when *instruction-is-variably-sized*
          ;; Package this single instruction up into a chunk.
          (push (make-instance 'variably-sized-chunk
                               :instruction i
                               :code *machine-code*)
                *chunks*)
          (assert (endp *symbols*))
          (setf *machine-code* (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
                *relocations* '()))))
    (push (make-instance 'fixed-sized-chunk
                         :code *machine-code*
                         :symbols *symbols*
                         :relocations *relocations*
                         :gcmd (reverse *gc-data*)
                         :fixups *fixups*)
          *chunks*)
    (reverse *chunks*)))

(defun mc-equal (a b)
  "Test if two machine code arrays are equal."
  (and (eql (length a) (length b))
       (dotimes (i (length a) t)
         (when (not (eql (aref a i) (aref b i)))
           (return nil)))))

(defun perform-assembly (instruction-set code-list &key (base-address 0) (initial-symbols '()) info &allow-other-keys)
  "Assemble a list of instructions, returning a u-b 8 vector of machine code,
a vector of constants and an alist of symbols & addresses."
  (let ((*constant-pool* (make-array (length info)
                                     :fill-pointer t
                                     :adjustable t
                                     :initial-contents info))
        (*symbol-table* (make-hash-table))
        (*missing-symbols* '())
        (*mc-end* nil))
    (dolist (x initial-symbols)
      (setf (gethash (first x) *symbol-table*) (rest x)))
    (let ((chunks (perform-assembly-pass1 instruction-set code-list base-address)))
      ;; Determine approximate symbol locations.
      (let ((*current-address* base-address))
        (dolist (chunk chunks)
          (when (typep chunk 'fixed-sized-chunk)
            (loop for (name offset) in (chunk-symbols chunk) do
                 (setf (gethash name *symbol-table*) (+ *current-address* offset))))
          (incf *current-address* (length (chunk-code chunk)))))
      ;; Walk each chunk, reassembling variably-sized chunks to produce the final layout.
      (loop
           (let ((*current-address* base-address)
                 (changed-something nil))
             (dolist (chunk chunks)
               (etypecase chunk
                 (fixed-sized-chunk
                  ;; Update symbol addresses.
                  (loop for (name offset) in (chunk-symbols chunk) do
                       (setf (gethash name *symbol-table*) (+ *current-address* offset)))
                  (incf *current-address* (length (chunk-code chunk))))
                 (variably-sized-chunk
                  ;; This instruction needs to be fully reassembled.
                  (let ((*machine-code* (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
                        (*instruction-is-variably-sized* t))
                    (assemble-one-instruction instruction-set (chunk-instruction chunk))
                    (when (not (mc-equal (chunk-code chunk) *machine-code*))
                      (setf changed-something t
                            (chunk-code chunk) *machine-code*))))))
             (when *missing-symbols*
               (error "Missing symbols ~S." *missing-symbols*))
             (when (not changed-something)
               (setf *mc-end* *current-address*)
               (return))))
      ;; Now we have the final layout.
      (let ((result-mc (make-array (loop
                                      for chunk in chunks
                                      summing (length (chunk-code chunk)))
                                   :element-type '(unsigned-byte 8)
                                   :fill-pointer 0))
            (result-fixups '())
            (result-gcmd '())
            (*current-address* base-address))
        (dolist (chunk chunks)
          (let ((offset (length result-mc)))
            (incf (fill-pointer result-mc) (length (chunk-code chunk)))
            (replace result-mc (chunk-code chunk) :start1 offset))
          (when (typep chunk 'fixed-sized-chunk)
            (loop for (name offset) in (chunk-symbols chunk) do
                 (assert (eql (gethash name *symbol-table*) (+ *current-address* offset))
                         ()
                         "Bug! Symbol ~S changed address from ~X to ~X in final pass."
                         name (gethash name *symbol-table*) (+ *current-address* offset)))
            (loop for (kind immediate addend address offset) in (chunk-relocations chunk) do
                 (perform-relocation result-mc
                                     (- (+ *current-address* offset) base-address)
                                     kind
                                     (or (resolve-immediate immediate)
                                         (error "Failed to resolve immediate ~S during relocation" immediate))
                                     addend
                                     (+ *current-address* offset)
                                     immediate))
            (loop for (offset . metadata) in (chunk-gcmd chunk) do
                 (let ((address (+ *current-address* offset)))
                   ;; More recent entries overwrite older entries.
                   (cond ((eql (first (first result-gcmd)) address)
                          (setf (rest (first result-gcmd)) metadata))
                         ;; Do nothing if the previous entry is the same as the new entry.
                         ((and result-gcmd
                               (equal (rest (first result-gcmd)) metadata)))
                         (t
                          (push (cons address metadata) result-gcmd)))))
            (loop for (name . offset) in (chunk-fixups chunk) do
                 (push (cons name (+ *current-address* offset)) result-fixups)))
          (incf *current-address* (length (chunk-code chunk))))
        (values result-mc
                *constant-pool*
                result-fixups
                (let ((alist '()))
                  (maphash (lambda (k v)
                             (push (cons k v) alist))
                           *symbol-table*)
                  alist)
                (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                       (mapcar 'encode-gc-info (reverse result-gcmd))))))))

(defun perform-assembly-old (instruction-set code-list &key (base-address 0) (initial-symbols '()) info &allow-other-keys)
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
       (*relocations* '())
       (attempt 0 (1+ attempt))
       (*instruction-is-variably-sized* nil))
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
          *gc-data* '()
          *relocations* '())
    (dolist (i code-list)
      (etypecase i
        (symbol (when (gethash i *symbol-table*)
                  (cerror "Replace the existing symbol." "Duplicate symbol ~S." i))
                (setf (gethash i *symbol-table*) *current-address*))
        (cons (if (keywordp (first i))
                  (ecase (first i)
                    (:comment)
                    (:align
                     (destructuring-bind (alignment) (rest i)
                       (let ((misalignment (rem (length *machine-code*) alignment)))
                         (when (not (zerop misalignment))
                           (loop
                              repeat (- alignment misalignment)
                              do (emit 0))))))
                    (:d8 (apply 'emit-d8 (rest i)))
                    (:d16/le (apply 'emit-d16/le (rest i)))
                    (:d32/le (apply 'emit-d32/le (rest i)))
                    (:d64/le (apply 'emit-d64/le (rest i)))
                    (:gc (apply 'emit-gc (rest i))))
                  (let ((handler (gethash (first i) instruction-set)))
                    (if handler
                        (unless (funcall handler i)
                          (incf failed-instructions))
                        (error "Unrecognized instruction ~S." (first i)))))
              (setf *instruction-is-variably-sized* nil))))
    (when (not (zerop attempt))
      (perform-relocations base-address))))

(defun emit-d8 (&rest args)
  (dolist (a args)
    (emit a)))

(defun emit-d16/le (&rest args)
  (dolist (a args)
    (let ((val (resolve-immediate a)))
      (cond (val
             (check-type val (unsigned-byte 16))
             (emit (ldb (byte 8 0) val)
                   (ldb (byte 8 8) val)))
            (t
             (emit-relocation :abs16le a 0)
             (emit 0 0))))))

(defun emit-d32/le (&rest args)
  (dolist (a args)
    (let ((val (resolve-immediate a)))
      (cond ((eql val :fixup)
             (note-fixup a)
             (emit #xFF #xFF #xFF #xFF))
            (val
             (check-type val (unsigned-byte 32))
             (emit (ldb (byte 8 0) val)
                   (ldb (byte 8 8) val)
                   (ldb (byte 8 16) val)
                   (ldb (byte 8 24) val)))
            (t
             (emit-relocation :abs32le a 0)
             (emit 0 0 0 0))))))

(defun emit-d64/le (&rest args)
  (dolist (a args)
    (let ((val (resolve-immediate a)))
      (cond ((eql val :fixup)
             (note-fixup a)
             (emit #xFF #xFF #xFF #xFF #x00 #x00 #x00 #x00))
            (val
             (check-type val (or (unsigned-byte 64)
                                 (signed-byte 64)))
             (emit (ldb (byte 8 0) val)
                   (ldb (byte 8 8) val)
                   (ldb (byte 8 16) val)
                   (ldb (byte 8 24) val)
                   (ldb (byte 8 32) val)
                   (ldb (byte 8 40) val)
                   (ldb (byte 8 48) val)
                   (ldb (byte 8 56) val)))
            (t
             (emit-relocation :abs64le a 0)
             (emit 0 0 0 0 0 0 0 0))))))

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
                    (if *mc-end*
                        (+ (* (ceiling *mc-end* 16) 16)
                           (* (or (position (second value) *constant-pool*)
                                  (vector-push-extend (second value) *constant-pool*)) 8))
                        nil))))
                (t (let ((args (mapcar #'resolve-immediate (rest value))))
                     (if (member nil args)
                         nil
                         (apply (first value) args))))))
    (symbol (let ((val (gethash value *symbol-table*)))
              (cond ((eql val 't) nil)
                    (val)
                    (t
                     (when (not *in-pass1*)
                       (pushnew value *missing-symbols*))
                     nil))))
    (integer value)))

(defun note-fixup (name)
  (when *instruction-is-variably-sized*
    (error "Variably-sized instructions must not have fixups."))
  (push (cons name (length *machine-code*)) *fixups*))

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
      (if (eql (first (first *gc-data*)) (length *machine-code*))
          (setf (first *gc-data*) (list* (length *machine-code*)
                                         frame-mode
                                         gc-keys))
          (push (list* (length *machine-code*)
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
