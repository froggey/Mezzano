;;;; DISASSEMBLE, generic bits

(defpackage :mezzano.disassemble
  (:use :cl)
  (:local-nicknames (:sys.int :mezzano.internals))
  (:export #:disassemble
           #:make-disassembler-context
           #:disassembler-context-function
           #:instruction-at
           #:print-instruction
           #:*print-gc-metadata*
           #:*print-debug-metadata*

           #:disassembler-context
           #:make-disassembler-context-using-architecture
           #:disassembler-context-function
           #:context-function
           #:context-code-offset
           #:context-label-table
           #:context-instructions

           #:consume-ub8
           #:consume-ub16/le
           #:consume-ub32/le
           #:consume-ub64/le
           #:consume-sb8
           #:consume-sb16/le
           #:consume-sb32/le
           #:consume-sb64/le

           #:instruction
           #:inst-offset
           #:inst-size

           #:disassemble-one-instruction
           #:code-initial-offset
           #:code-end
           #:code-byte

           #:label))

(in-package :mezzano.disassemble)

(defvar *print-gc-metadata* t)
(defvar *print-debug-metadata* nil)

(defun defmethod-name-to-method (defmethod-name)
  "Convert a (DEFMETHOD name {qualifiers} (specializers...)) name to a method object."
  (let* ((name (second defmethod-name))
         (qualifiers (subseq defmethod-name 2 (1- (length defmethod-name))))
         (specializers (loop
                          for spec in (first (last defmethod-name))
                          collect (cond ((symbolp spec)
                                         (find-class spec))
                                        ((and (consp spec)
                                              (eql (first spec) 'eql))
                                         `(eql ,(eval (second spec))))
                                        (t
                                         (error "Unknown specializer ~S" spec)))))
         (generic (fdefinition name)))
    (when (not (typep generic 'generic-function))
      (error "Can't resolve ~S: ~S is not a generic-function"
             defmethod-name generic))
    (find-method generic qualifiers specializers)))

(defun disassemble (fn &key (gc-metadata *print-gc-metadata*) (debug-metadata *print-debug-metadata*) architecture)
  (when (and (consp fn) (eql (first fn) 'lambda))
    (setf fn (compile nil fn)))
  (when (and (consp fn) (eql (first fn) 'defmethod))
    (setf fn (mezzano.clos::method-fast-function (defmethod-name-to-method fn) nil nil)))
  (when (and (not (functionp fn))
             (not (sys.int::function-reference-p fn)))
    (setf fn (fdefinition fn)))
  (check-type fn (or function sys.int::function-reference))
  (setf architecture (mezzano.compiler::canonicalize-target architecture))
  (let ((*print-gc-metadata* gc-metadata)
        (*print-debug-metadata* debug-metadata)
        (*print-pretty* nil)
        (fundamental-fn (peel-function fn)))
    (cond ((eql fundamental-fn fn)
           (format t "~S:~%" fn))
          (t
           (format t "~S (implemented by ~S):~%" fn fundamental-fn)))
    (disassemble-subfunction fundamental-fn architecture)
    ;; Recursively traverse the function looking for closures and disassemble them too.
    (let ((closures '())
          (worklist (list fundamental-fn)))
      (loop
         until (endp worklist)
         for fn = (pop worklist)
         do
           (when (not (member fn closures))
             (when (not (eql fn fundamental-fn))
               (push fn closures))
             (when (not (sys.int::function-reference-p fn))
               (dotimes (i (sys.int::function-pool-size fn))
                 (let ((entry (sys.int::function-pool-object fn i)))
                   (when (sys.int::%object-of-type-p entry sys.int::+object-tag-function+)
                     (push entry worklist)))))))
      (setf closures (reverse closures))
      (dolist (fn closures)
        (format t "----------~%" fn)
        (format t "~S:~%" fn)
        (disassemble-subfunction fn architecture))))
  nil)

(defgeneric print-instruction (context instruction
                               &key print-annotations print-labels))

(defun disassemble-subfunction (function architecture)
  (let ((base-address (logand (sys.int::lisp-object-address function) -16))
        (offset (code-initial-offset function))
        (context (make-disassembler-context function architecture))
        (gc-md (if (sys.int::function-reference-p function)
                   '()
                   (sys.int::decode-function-gc-info function)))
        (debug-md (if (sys.int::function-reference-p function)
                      '()
                      (sys.int::decompress-precise-debug-info
                       (sys.int::decode-precise-debug-info
                        function
                        (sys.int::debug-info-precise-variable-data
                         (sys.int::function-debug-info function)))))))
    (loop
       for decoded across (context-instructions context)
       do
         (when (and gc-md
                    (>= offset (first (first gc-md))))
           (when *print-gc-metadata*
             (format t "~7T~8,'0X:~50T~S~%" (+ base-address offset) `(:gc ,@(rest (first gc-md)))))
           (pop gc-md))
         (when (and debug-md
                    (>= offset (first (first debug-md))))
           (when *print-debug-metadata*
             (format t "~7T~8,'0X:~50T~S~%" (+ base-address offset) `(:debug ,@(rest (first debug-md)))))
           (pop debug-md))
         (let ((label (label context offset)))
           (when label
             (format t " L~D" label)))
         (format t "~7T~8,'0X: " (+ base-address offset))
         (cond (decoded
                (format t "~{~2,'0X ~}~50T"
                        (loop
                           repeat (inst-size decoded)
                           for i from offset
                           collect (code-byte function i)))
                (print-instruction context decoded)
                (terpri)
                (incf offset (inst-size decoded)))
               (t
                (format t "<bad ~2,'0X>~%"
                        (code-byte function offset))
                (incf offset 1))))))

(defun peel-function (function)
  "Remove layers of closures and funcallable-instances from FUNCTION."
  (when (sys.int::function-reference-p function)
    (return-from peel-function function))
  (check-type function function)
  (let ((fundamental-fn function))
    (when (sys.int::funcallable-instance-p fundamental-fn)
      (setf fundamental-fn (sys.int::funcallable-instance-function fundamental-fn)))
    (when (sys.int::funcallable-instance-p fundamental-fn)
      ;; Bail out if there are multiple levels to funcallable-instances.
      (error "~S contains a nested funcallable-instance ~S." function fundamental-fn))
    (when (sys.int::closure-p fundamental-fn)
      (setf fundamental-fn (sys.int::%closure-function fundamental-fn)))
    (assert (sys.int::%object-of-type-p fundamental-fn sys.int::+object-tag-function+))
    fundamental-fn))

(defun code-end (function-like)
  (if (sys.int::function-reference-p function-like)
      64 ; Function references are this long.
      (sys.int::function-code-size function-like)))

(defun code-initial-offset (function-like)
  (if (sys.int::function-reference-p function-like)
      32
      16))

(defun code-byte (function-like offset)
  (if (sys.int::function-reference-p function-like)
      (sys.int::%object-ref-unsigned-byte-8 function-like (- offset 8))
      (sys.int::function-code-byte function-like offset)))

(defgeneric make-disassembler-context-using-architecture (architecture &rest initargs &key &allow-other-keys))
(defgeneric disassemble-one-instruction (context))

(defun make-disassembler-context (function &optional architecture)
  (setf architecture (mezzano.compiler::canonicalize-target architecture))
  (setf function (peel-function function))
  (let ((context (make-disassembler-context-using-architecture architecture :function function))
        (true-end (code-end function))
        (offset (code-initial-offset function)))
      ;; Find the approximate end of the function. The size is rounded up to 16 bytes and
      ;; it's padded with zeros.
      (loop
         (when (not (zerop (code-byte function (1- true-end))))
           (return))
         (decf true-end))
      ;; Disassemble all instructions.
      (handler-case
          (loop
             (when (>= offset true-end)
               (return))
             (let ((inst (disassemble-one-instruction context)))
               (vector-push-extend inst
                                   (context-instructions context))
               (incf offset (if inst
                                (inst-size inst)
                                1))))
        (read-past-end-of-machine-code ()))
      context))

(defun slot-to-thread-slot-name (slot)
  (let* ((thread-struct (find-class 'mezzano.supervisor:thread))
         (thread-slots (mezzano.clos:class-slots thread-struct)))
    (dolist (slot-def thread-slots)
      (let* ((location (mezzano.clos:slot-definition-location slot-def))
             (offset (mezzano.runtime::location-offset location))
             (type (mezzano.runtime::location-type location))
             (size (if (eql type mezzano.runtime::+location-type-t+)
                       8
                       (mezzano.runtime::location-type-scale type)))
             (fv (mezzano.clos:structure-slot-definition-fixed-vector slot-def))
             (end-offset (+ offset (* size (or fv 1)))))
        (cond (fv
               (cond ((eql type mezzano.runtime::+location-type-t+)
                      (when (and (zerop (rem slot 8))
                                 (<= offset slot (1- end-offset)))
                        (return
                          (format nil "thread-~(~A~)+~D"
                                  (mezzano.clos:slot-definition-name slot-def)
                                  (- (truncate slot 8) (mezzano.runtime::location-offset-t location))))))
                     (t
                      (when (<= offset slot (1- end-offset))
                        (return
                          (format nil "thread-~(~A~)+~D"
                                  (mezzano.clos:slot-definition-name slot-def)
                                  (truncate (- slot offset) size)))))))
              (t
               (when (<= offset slot (1- end-offset))
                 (return
                   (format nil "thread-~(~A~)" (mezzano.clos:slot-definition-name slot-def))))))))))

(defun type-tag-to-name (tag)
  (when (zerop (ldb (byte 2 0) tag))
    (format nil "object-tag-~(~A~)" (aref sys.int::*object-tags-to-basic-types* (ash tag -2)))))

(defclass disassembler-context ()
  ((%function :initarg :function :reader context-function :reader disassembler-context-function)
   (%offset :accessor context-code-offset)
   (%label-table :initform (make-hash-table) :reader context-label-table)
   (%instructions :initform (make-array 0 :adjustable t :fill-pointer 0) :reader context-instructions)))

(defmethod initialize-instance :after ((instance disassembler-context) &key function)
  (setf (slot-value instance '%offset) (code-initial-offset function)))

(defclass instruction ()
  ((%offset :reader inst-offset)))

(defgeneric inst-size (instruction))

(defun instruction-at (context offset)
  (loop
     for inst across (context-instructions context)
     when (and inst (eql (inst-offset inst) offset))
     do (return inst)))

(defun label (context offset &key createp)
  (let ((table (context-label-table context)))
    (when (and createp
               (not (gethash offset table)))
      (setf (gethash offset table) (hash-table-count table)))
    (values (gethash offset table))))

(define-condition read-past-end-of-machine-code (error) ())

(defun consume-octet (context)
  (when (>= (context-code-offset context) (code-end (context-function context)))
    (error 'read-past-end-of-machine-code))
  (prog1
      (code-byte (context-function context) (context-code-offset context))
    (incf (context-code-offset context))))

(defun consume-word/le (context n-octets signedp)
  (loop
     with result = 0
     for i from 0 by 8
     repeat n-octets
     do
       (setf result (logior result (ash (consume-octet context) i)))
     finally
       (return (if signedp
                   (sys.int::sign-extend result (* n-octets 8))
                   result))))

(defun consume-ub8 (context)
  (consume-octet context))

(defun consume-ub16/le (context)
  (consume-word/le context 2 nil))

(defun consume-ub32/le (context)
  (consume-word/le context 4 nil))

(defun consume-ub64/le (context)
  (consume-word/le context 8 nil))

(defun consume-sb8 (context)
  (sys.int::sign-extend (consume-octet context) 8))

(defun consume-sb16/le (context)
  (consume-word/le context 2 t))

(defun consume-sb32/le (context)
  (consume-word/le context 4 t))

(defun consume-sb64/le (context)
  (consume-word/le context 8 t))
