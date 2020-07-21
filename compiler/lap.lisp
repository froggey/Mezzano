;;;; Generic assembler driver and support functionality.

(in-package :mezzano.lap)

(defvar *function-reference-resolver* nil
  "Function used to convert :function memory references into constants.")

(defgeneric perform-assembly-using-target (target code-list &rest args))

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
(defvar *debug-data*)
(defvar *relocations* nil)
(defvar *symbols* nil)
(defvar *chunks*)
(defvar *in-pass1* nil)

(defparameter *settle-limit* 500)

(defvar *instruction-is-variably-sized*)

(defclass label ()
  ((%name :initarg :name :reader label-name))
  (:default-initargs :name nil))

(defun make-label (&optional name)
  (make-instance 'label :name name))

(defmethod print-object ((instance label) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (if (label-name instance)
        (format stream "~S" (label-name instance))
        (format stream "<anonymous>"))))

(defclass chunk ()
  ((%code :initarg :code :accessor chunk-code)))

(defclass fixed-sized-chunk (chunk)
  ((%symbols :initarg :symbols :reader chunk-symbols)
   (%relocations :initarg :relocations :reader chunk-relocations)
   (%gcmd :initarg :gcmd :reader chunk-gcmd)
   (%debug-md :initarg :debug-md :reader chunk-debug-md)
   (%fixups :initarg :fixups :reader chunk-fixups)))

(defclass variably-sized-chunk (chunk)
  ((%instruction :initarg :instruction :reader chunk-instruction)))

(defun note-variably-sized-instruction ()
  "Must be called when the current instruction is either variably-sized or depends on the current address."
  (when *in-pass1*
    ;; Finish the current chunk.
    (push (make-instance 'fixed-sized-chunk
                         :code *machine-code*
                         :symbols *symbols*
                         :relocations *relocations*
                         :gcmd (reverse *gc-data*)
                         :debug-md (reverse *debug-data*)
                         :fixups *fixups*)
          *chunks*)
    (setf *machine-code* (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
          *symbols* '()
          *relocations* '()
          *gc-data* '()
          *debug-data* '()
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
             (aref vector (+ offset 7)) (ldb (byte 8 56) value))))
    (:arm-pcrel
     (let* ((value (- immediate address))
            (instruction (logior addend
                                 (ash (ldb (byte 19 2) value) 5))))
       (assert (not (logtest value #b11)))
       (assert (<= -1048576 value 1048575))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) instruction)
             (aref vector (+ offset 1)) (ldb (byte 8 8) instruction)
             (aref vector (+ offset 2)) (ldb (byte 8 16) instruction)
             (aref vector (+ offset 3)) (ldb (byte 8 24) instruction))))
    (:arm-pcrel-adr
     (let* ((value (- immediate address))
            (instruction (logior addend
                                 (ash (ldb (byte 19 2) value) 5)
                                 (ash (ldb (byte 2 0) value) 29))))
       (assert (<= -1048576 value 1048575))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) instruction)
             (aref vector (+ offset 1)) (ldb (byte 8 8) instruction)
             (aref vector (+ offset 2)) (ldb (byte 8 16) instruction)
             (aref vector (+ offset 3)) (ldb (byte 8 24) instruction))))
    (:arm-pcrel-b
     (let* ((value (- immediate address))
            (instruction (logior addend
                                 (ldb (byte 26 2) value))))
       (assert (not (logtest value #b11)))
       (assert (<= -134217728 value 134217727))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) instruction)
             (aref vector (+ offset 1)) (ldb (byte 8 8) instruction)
             (aref vector (+ offset 2)) (ldb (byte 8 16) instruction)
             (aref vector (+ offset 3)) (ldb (byte 8 24) instruction))))
    (:arm-pcrel-imm14
     (let* ((value (- immediate address))
            (instruction (logior addend
                                 (ash (ldb (byte 14 2) value) 5))))
       (assert (not (logtest value #b11)))
       (assert (<= -32767 value 32767))
       (setf (aref vector (+ offset 0)) (ldb (byte 8 0) instruction)
             (aref vector (+ offset 1)) (ldb (byte 8 8) instruction)
             (aref vector (+ offset 2)) (ldb (byte 8 16) instruction)
             (aref vector (+ offset 3)) (ldb (byte 8 24) instruction))))))

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
         (destructuring-bind (alignment &key (value 0)) (rest instruction)
           (let ((misalignment (rem *current-address* alignment)))
             (when (not (zerop misalignment))
               (loop
                  repeat (- alignment misalignment)
                  do (emit value))))))
        (:d8 (apply 'emit-d8 (rest instruction)))
        (:d16/le (apply 'emit-d16/le (rest instruction)))
        (:d32/le (apply 'emit-d32/le (rest instruction)))
        (:d64/le (apply 'emit-d64/le (rest instruction)))
        (:gc (apply 'emit-gc (rest instruction)))
        (:debug (apply 'emit-debug (rest instruction))))
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
        (*debug-data* '())
        (*chunks* '()))
    (dolist (i code-list)
      (let ((*instruction-is-variably-sized* nil))
        (etypecase i
          ((or symbol label)
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
                         :debug-md (reverse *debug-data*)
                         :fixups *fixups*)
          *chunks*)
    (reverse *chunks*)))

(defun mc-equal (a b)
  "Test if two machine code arrays are equal."
  (and (eql (length a) (length b))
       (dotimes (i (length a) t)
         (when (not (eql (aref a i) (aref b i)))
           (return nil)))))

(defun delta-compress-debug-info-1 (old-layout new-layout)
  (let ((result '())
        (current '()))
    ;; Bring variable definitions in-line.
    (do ((old old-layout (rest old))
         (new new-layout (rest new))
         (index 0 (1+ index)))
        ((or (endp old) (endp new)
             (not (eql (first (first old)) (first (first new))))
             (not (equal (cdddr (first old)) (cdddr (first new)))))
         (when (not (endp old))
           (push `(:drop ,index) result))
         (when (not (endp new))
           (dolist (e new)
             (push e current)
             (push `(:add ,@e) result)))
         (setf current (reverse current)))
      (push (first old) current))
    ;; Now update locations.
    (loop
       for i from 0
       for prev-layout in current
       for curr-layout in new-layout
       when (not (and (eql (second prev-layout) (second curr-layout))
                      (eql (third prev-layout) (third curr-layout))))
       do (push `(:update ,i ,(second curr-layout) ,(third curr-layout)) result))
    (reverse result)))

(defun delta-compress-debug-info (info)
  (loop
     with previous-layout = '()
     for entry in info
     collect (destructuring-bind (offset &key layout)
                 entry
               (prog1
                   (list offset :compressed-layout (delta-compress-debug-info-1 previous-layout layout))
                 (setf previous-layout layout)))))


(defun encode-debug-representation (repr)
  (ecase repr
    (:value sys.int::+debug-repr-value+)
    (single-float sys.int::+debug-repr-single-float+)
    (double-float sys.int::+debug-repr-double-float+)
    (mezzano.simd:mmx-vector sys.int::+debug-repr-mmx-vector+)
    (mezzano.simd:sse-vector sys.int::+debug-repr-sse-vector+)
    (fixnum sys.int::+debug-repr-fixnum+)
    (:unsigned-byte-64 sys.int::+debug-repr-unsigned-byte-64+)
    (:signed-byte-64 sys.int::+debug-repr-signed-byte-64+)))

(defun encode-debug-location (location repr output)
  (cond ((integerp location)
         (cond ((< location 127)
                (vector-push-extend (logior #x80 location) output)) ; Stack, short form.
               (t
                (vector-push-extend #xFF output) ; Stack, with following vu32.
                (append-vu32 location output))))
        (t
         ;; A register.
         (append-vu32 (or (position location sys.int::*debug-x86-64-register-encodings*)
                          (position location sys.int::*debug-arm64-register-encodings*)
                          (error "Unable to encode debug location ~S" location))
                      output)))
  (append-vu32 (encode-debug-representation repr) output))

(defun encode-compressed-layout (layout output constant-pool)
  (loop for (op . data) in layout do
       (ecase op
         (:add
          (destructuring-bind (name location repr &key hidden) data
            (vector-push-extend (if hidden
                                    sys.int::+debug-add-hidden-var-op+
                                    sys.int::+debug-add-var-op+)
                                output)
            (append-vu32 (position name constant-pool) output)
            (encode-debug-location location repr output)))
         (:drop
          (destructuring-bind (index) data
            (cond ((<= 0 index 15)
                   (vector-push-extend (logior sys.int::+debug-drop-n-op+ index) output))
                  (t
                   (vector-push-extend sys.int::+debug-drop-op+ output)
                   (append-vu32 index output)))))
         (:update
          (destructuring-bind (index location repr) data
            (cond ((<= 0 index 15)
                   (vector-push-extend (logior sys.int::+debug-update-n-op+ index) output))
                  (t
                   (vector-push-extend sys.int::+debug-update-op+ output)
                   (append-vu32 index output)))
            (encode-debug-location location repr output)))))
  (vector-push-extend sys.int::+debug-end-entry-op+ output))

(defun encode-debug-info (info constant-pool)
  (let ((delta-compressed-info (delta-compress-debug-info info))
        (result (make-array 16 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8))))
    (dolist (entry delta-compressed-info)
      (destructuring-bind (offset &key compressed-layout) entry
        (append-vu32 offset result)
        (encode-compressed-layout compressed-layout result constant-pool)))
    result))

(defvar *last-gc-data*)

(defun current-gc-metadata ()
  "Return the currently active GC metadata.
Should only be called from assembler macros."
  (or *last-gc-data*
      (error "No current GC metadata")))

(defun expand-macros (instruction-set code-list)
  (loop
     for inst in code-list
     when (not (listp inst))
     collect inst
     else
     append (cond ((eql (first inst) :progn)
                   (expand-macros instruction-set (rest inst)))
                  (t
                   (let ((handler (gethash (first inst) instruction-set)))
                     (when (eql (first inst) :gc)
                       (setf *last-gc-data* (rest inst)))
                     (if (and (consp handler)
                              (eql (first handler) :macro))
                         (expand-macros instruction-set (funcall (second handler) inst))
                         (list inst)))))))

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
    (let ((*last-gc-data* nil))
      (setf code-list (expand-macros instruction-set code-list)))
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
            (result-debug-md '())
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
            (loop for (offset . metadata) in (chunk-debug-md chunk) do
                 (let ((address (+ *current-address* offset)))
                   ;; More recent entries overwrite older entries.
                   (cond ((eql (first (first result-debug-md)) address)
                          (setf (rest (first result-debug-md)) metadata))
                         ;; Do nothing if the previous entry is the same as the new entry.
                         ((and result-debug-md
                               (equal (rest (first result-debug-md)) metadata)))
                         (t
                          (push (cons address metadata) result-debug-md)))))
            (loop for (name . offset) in (chunk-fixups chunk) do
                 (push (cons name (+ *current-address* offset)) result-fixups)))
          (incf *current-address* (length (chunk-code chunk))))
        (setf result-debug-md (reverse result-debug-md))
        (when (and (>= (length *constant-pool*) 2)
                   (consp (aref *constant-pool* 1))
                   (eql (first (aref *constant-pool* 1)) :debug-info)
                   (>= (length (aref *constant-pool* 1)) 10))
          ;; Make sure all variable names are present in the constant pool.
          (dolist (entry result-debug-md)
            (destructuring-bind (offset &key layout) entry
              (declare (ignore offset))
              (dolist (var layout)
                (add-to-constant-pool (first var)))))
          (setf (tenth (aref *constant-pool* 1)) (encode-debug-info result-debug-md *constant-pool*)))
        (values result-mc
                *constant-pool*
                result-fixups
                (let ((alist '()))
                  (maphash (lambda (k v)
                             (push (cons k v) alist))
                           *symbol-table*)
                  alist)
                ;; Avoid (apply #'concatenate ...) here, slower and functions with many GC entries
                ;; may exceed CALL-ARGUMENTS-LIMIT.
                (let* ((encoded-gcmd-entries (mapcar 'encode-gc-info (reverse result-gcmd)))
                       (total-length (loop
                                        for entry in encoded-gcmd-entries
                                        summing (length entry)))
                       (gcmd (make-array total-length :element-type '(unsigned-byte 8))))
                  (loop
                     with position = 0
                     for entry in encoded-gcmd-entries
                     do
                       (setf (subseq gcmd position) entry)
                       (incf position (length entry)))
                  gcmd))))))

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
  (typep thing '(or symbol label integer (cons (eql :immediate)))))

(defun add-to-constant-pool (value)
  (or (position value *constant-pool*)
      (vector-push-extend value *constant-pool*)))

(defun resolve-immediate (value)
  "Convert an immediate value to an integer."
  (when (and (consp value)
             (eql (first value) :immediate))
    (setf value (second value)))
  (etypecase value
    (cons
     (cond ((keywordp (first value))
            (ecase (first value)
              (:constant-address
               (if *mc-end*
                   (+ (* (ceiling *mc-end* 16) 16)
                      (* (add-to-constant-pool (second value)) 8))
                   nil))))
           (t (let ((args (mapcar #'resolve-immediate (rest value))))
                (if (member nil args)
                    nil
                    (apply (first value) args))))))
    ((or symbol label)
     (let ((val (gethash value *symbol-table*)))
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

(defun emit-debug (current-layout)
  (let ((debug-data (list :layout current-layout)))
    ;; Most recent takes priority.
    (if (eql (first (first *debug-data*)) (length *machine-code*))
        (setf (first *debug-data*) (list* (length *machine-code*)
                                          debug-data))
        (push (list* (length *machine-code*)
                     debug-data)
              *debug-data*))))

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
                                     (ash 1 sys.int::+gcmd-flag0-frame+))
                                    (:no-frame 0))
                                  (cond
                                    (interrupt
                                     (ash 1 sys.int::+gcmd-flag0-interrupt+))
                                    (t 0))
                                  (cond
                                    (block-or-tagbody-thunk
                                     (assert (eql block-or-tagbody-thunk :rax))
                                     (ash 1 sys.int::+gcmd-flag0-block-or-tagbody-thunk+))
                                    (t 0))
                                  (cond
                                    (incoming-arguments
                                     (ash 1 sys.int::+gcmd-flag0-incoming-arguments+))
                                    (t 0))
                                  (cond
                                    (pushed-values-register
                                     (assert (eql pushed-values-register :rcx))
                                     (ash 1 sys.int::+gcmd-flag0-pushed-values-register+))
                                    (t 0))
                                  (dpb (ecase extra-registers
                                         ((nil)          0)
                                         ((:rax)         1)
                                         ((:rax-rcx)     2)
                                         ((:rax-rcx-rdx) 3))
                                       sys.int::+gcmd-flag0-extra-registers+
                                       0)
                                  (cond
                                    (restart
                                     (ash 1 sys.int::+gcmd-flag0-restart+))
                                    (t 0)))
                          bytes)
      (vector-push-extend (logior (dpb (or multiple-values #b1111)
                                       sys.int::+gcmd-flag1-multiple-values+
                                       0)
                                  (dpb (cond ((keywordp incoming-arguments)
                                              (assert (eql incoming-arguments :rcx))
                                              15)
                                             (incoming-arguments
                                              (check-type incoming-arguments (integer 0 (15)))
                                              incoming-arguments)
                                             (t 15))
                                       sys.int::+gcmd-flag1-incoming-arguments-location+
                                       0))
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
