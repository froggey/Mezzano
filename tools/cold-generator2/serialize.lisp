;;;; Serializes an environment to an in-memory image

(defpackage :mezzano.cold-generator.serialize
  (:use :cl)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:util #:mezzano.cold-generator.util)
                    (#:sys.int #:mezzano.internals))
  (:export #:make-image
           #:serialize-image
           #:serialize-object
           #:area-name
           #:area-base
           #:area-data
           #:area-size
           #:area-object-starts
           #:image-wired-area
           #:image-pinned-area
           #:image-general-area
           #:image-cons-area
           #:image-wired-function-area
           #:image-function-area
           #:image-initial-stack-pointer
           #:do-image-stacks
           #:write-map-file
           #:write-symbol-table
           ))

(in-package :mezzano.cold-generator.serialize)

;; Wired area starts near 0.
(defconstant +wired-area-base+ sys.int::+allocation-minimum-alignment+)
;; Pinned at 1T.
(defconstant +pinned-area-base+ (* 1 1024 1024 1024 1024))
;; Wired area stops at 2G, below the pinned area.
(defconstant +wired-area-limit+ (* 2 1024 1024 1024))

;; Function area starts at 512G.
;; The wired function expands down from here, the normal area expands upwards.
(defconstant +function-area-base+ (* 512 1024 1024 1024))
;; The total size (wired & normal) of the function area is limited to 2G
;; by x86-64 branch limits.
(defconstant +function-area-total-size-limit+ (* 2 1024 1024 1024))
;; For the sake of sanity (so the cold-generator doesn't need to expand the
;; wired function area, as it grows downwards), assume that the initial
;; wired function area is this big.
;; This more than enough to hold the initial functions.
(defconstant +cold-generator-assumed-wired-function-area-size+ (* 8 1024 1024))

;; Wired stack area starts at the bottom of the stack area.
(defconstant +wired-stack-area-base+ 0)
;; Not set to 512GB because bootloader is slow & dumb.
(defconstant +wired-stack-area-limit+ (* 2 1024 1024 1024))
;; Leave a gap, for future expansion.
(defconstant +stack-area-base+ (* 512 1024 1024 1024))

(defclass area ()
  ((%name :initarg :name :reader area-name)
   (%base :initarg :base :reader area-base)
   (%data :initform (make-array #x1000 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
          :reader area-data)
   (%starts :initform (make-array 0 :adjustable t :fill-pointer 0)
            :reader area-object-starts)))

(defun area-size (area)
  (length (area-data area)))

(defclass image ()
  ((%wired-area :initarg :wired-area :reader image-wired-area)
   (%pinned-area :initarg :pinned-area :reader image-pinned-area)
   (%general-area :initarg :general-area :reader image-general-area)
   (%cons-area :initarg :cons-area :reader image-cons-area)
   (%wired-function-area :initarg :wired-function-area :reader image-wired-function-area)
   (%function-area :initarg :function-area :reader image-function-area)
   (%object-values :initform (make-hash-table) :reader image-object-values)
   ;; (object . area) => value
   (%dedup-table :initform (make-hash-table :test 'equal)
                 :reader image-dedup-table)
   (%stack-bump :initform 0 :accessor image-stack-bump)
   (%stack-total :initform 0 :accessor image-stack-total)
   (%stack-bases :initform (make-hash-table) :reader image-stack-bases)
   (%finalizedp :initform nil :reader image-finalized-p)
   (%initial-stack-pointer :reader image-initial-stack-pointer)))

(defmacro do-image-stacks ((base size image &optional result) &body body)
  (let ((stack (gensym "STACK"))
        (base-temp (gensym "BASE")))
    `(progn
       (maphash (lambda (,stack ,base-temp)
                  ((lambda (,base ,size)
                     ,@body)
                   ,base-temp (env:stack-size ,stack)))
                (image-stack-bases ,image))
       ,result)))

(defgeneric serialize-object (object image environment)
  (:documentation "Serialize an object into the image, returning the object's value."))

(defgeneric allocate-object (object image environment)
  (:documentation "Allocate space in the image for OBJECT and return a tagged value to it.
Must not call SERIALIZE-OBJECT."))

(defgeneric initialize-object (object value image environment)
  (:documentation "Populate OBJECT's in-image fields."))

(defmethod serialize-object (object image environment)
  (let ((existing (gethash object (image-object-values image))))
    (when existing
      (return-from serialize-object existing)))
  (let ((value (allocate-object object image environment)))
    (setf (gethash object (image-object-values image)) value)
    (initialize-object object value image environment)
    value))

(defun allocate (n-words image area-name tag)
  (when (image-finalized-p image)
    (error "Allocating in a finalized image"))
  (when (oddp n-words) (incf n-words))
  (when (member area-name '(:wired-function :function))
    ;; Force 4 word alignment in function areas.
    (incf n-words 3)
    (setf n-words (logand n-words (lognot 3))))
  (let* ((area (ecase area-name
                 (:wired (image-wired-area image))
                 (:pinned (image-pinned-area image))
                 (:general (image-general-area image))
                 (:cons (image-cons-area image))
                 (:wired-function (image-wired-function-area image))
                 (:function (image-function-area image))))
         (offset (length (area-data area)))
         (address (+ (area-base area) offset))
         (new-length (+ (length (area-data area)) (* n-words 8))))
    (when (> new-length (array-dimension (area-data area) 0))
      (adjust-array (area-data area) (max (* (array-dimension (area-data area) 0) 2)
                                          new-length)))
    (setf (fill-pointer (area-data area)) new-length)
    (vector-push-extend offset (area-object-starts area))
    (logior address tag)))

(defun make-object-header (type data)
  (check-type type (unsigned-byte 6))
  (check-type data (unsigned-byte 56))
  (logior (ash type sys.int::+object-type-shift+)
          (ash data sys.int::+object-data-shift+)))

(defun object-area (image object)
  (flet ((test-area (area)
           (when (<= (area-base area)
                     object
                     (+ (area-base area) (length (area-data area))))
             area)))
    (or (test-area (image-wired-area image))
        (test-area (image-pinned-area image))
        (test-area (image-general-area image))
        (test-area (image-cons-area image))
        (test-area (image-wired-function-area image))
        (test-area (image-function-area image))
        (error "No area for object ~X in image ~S" object image))))

(defun object-slot-location (image object slot)
  (declare (ignore image))
  (+ (- object sys.int::+tag-object+)
     8
     (* slot 8)))

(defun object-slot-area-offset (image object slot)
  (let ((area (object-area image object)))
    (- (object-slot-location image object slot) (area-base area))))

(defun object-slot (image object slot)
  (let ((area (object-area image object)))
    (nibbles:ub64ref/le (area-data area)
                        (object-slot-area-offset image object slot))))

(defun replace-object-data-octets (image object sequence &key (slot 0) (offset 0) (start 0) (end nil))
  (let ((area (object-area image object)))
    (replace (area-data area) sequence
             :start1 (+ (object-slot-area-offset image object slot) offset)
             :start2 start :end2 end)))

(defun (setf object-slot) (value image object slot)
  (let ((area (object-area image object)))
    (setf (nibbles:ub64ref/le (area-data area)
                              (object-slot-area-offset image object slot))
          value)))

(defun object-car (image object)
  (let ((area (object-area image object)))
    (nibbles:ub64ref/le (area-data area)
                        (- object sys.int::+tag-cons+ (area-base area)))))

(defun (setf object-car) (value image object)
  (let ((area (object-area image object)))
    (setf (nibbles:ub64ref/le (area-data area)
                              (- object sys.int::+tag-cons+ (area-base area)))
          value)))

(defun object-cdr (image object)
  (let ((area (object-area image object)))
    (nibbles:ub64ref/le (area-data area)
                        (+ (- object sys.int::+tag-cons+ (area-base area)) 8))))

(defun (setf object-cdr) (value image object)
  (let ((area (object-area image object)))
    (setf (nibbles:ub64ref/le (area-data area)
                              (+ (- object sys.int::+tag-cons+ (area-base area)) 8))
          value)))

(defun initialize-object-header (image object type data)
  (setf (object-slot image object -1) (make-object-header type data)))

(defmethod allocate-object ((object symbol) image environment)
  (allocate 6 image :wired sys.int::+tag-object+))

(defmethod initialize-object ((object symbol) value image environment)
  ;; TODO: Include symbol mode.
  ;; TODO: Include plist.
  (initialize-object-header image value sys.int::+object-tag-symbol+ 0)
  (setf (object-slot image value sys.int::+symbol-name+)
        (serialize-object (env:cross-symbol-name environment object) image environment))
  (assert (eql (env:object-area environment (env:cross-symbol-name environment object)) :wired))
  (setf (object-slot image value sys.int::+symbol-package+)
        (serialize-object (env:cross-symbol-package environment object) image environment))
  (setf (object-slot image value sys.int::+symbol-value+)
        (serialize-object (env:symbol-global-value-cell environment object nil) image environment))
  (setf (object-slot image value sys.int::+symbol-function+)
        (serialize-object (env:function-reference environment object nil) image environment))
  (setf (object-slot image value sys.int::+symbol-type+)
        (serialize-object (env:cross-symbol-type environment object) image environment)))

(defmethod allocate-object ((object env:symbol-value-cell) image environment)
  (allocate 6 image :wired sys.int::+tag-object+))

(defmethod initialize-object ((object env:symbol-value-cell) value image environment)
  (initialize-object-header image value sys.int::+object-tag-symbol-value-cell+ 4)
  (setf (object-slot image value 0) ; link
        (serialize-object nil image environment))
  (setf (object-slot image value sys.int::+symbol-value-cell-symbol+)
        value)
  (cond ((env:symbol-global-boundp environment (env:symbol-value-cell-symbol object))
         (setf (object-slot image value sys.int::+symbol-value-cell-value+)
               (serialize-object (env:symbol-value-cell-value object) image environment)))
        (t
         (setf (object-slot image value sys.int::+symbol-value-cell-value+)
               (serialize-object (env:find-special environment :unbound-value) image environment))))
  (setf (object-slot image value 3) ; actual symbol
        (serialize-object (env:symbol-value-cell-symbol object) image environment)))

(defmethod allocate-object ((object env:function-reference) image environment)
  (allocate 8 image :wired-function sys.int::+tag-object+))

(defmethod initialize-object ((object env:function-reference) value image environment)
  (initialize-object-header image value sys.int::+object-tag-function-reference+ 0)
  (setf (object-slot image value sys.int::+fref-name+)
        (serialize-object (env:function-reference-name object) image environment))
  ;; Undefined frefs point directly at raise-undefined-function.
  (let ((undef-fref (serialize-object (env:function-reference
                                       environment
                                       (env:translate-symbol
                                        environment
                                        'sys.int::raise-undefined-function))
                                    image environment)))
    (setf (object-slot image value sys.int::+fref-undefined-entry-point+)
          (object-slot-location image undef-fref sys.int::+fref-code+)))
  (let* ((fn (env:function-reference-function object))
         (fn-value (serialize-object fn image environment)))
    (if fn
        (setf (object-slot image value sys.int::+fref-function+) fn-value)
        (setf (object-slot image value sys.int::+fref-function+) value))
    ;; Code...
    ;; (nop (:rax))
    ;; (jmp <direct-target>)
    (setf (object-slot image value (+ sys.int::+fref-code+ 0))
          (logior #xE9001F0F
                  (if fn
                      (let* ((entry-point (object-slot image fn-value sys.int::+function-entry-point+))
                             ;; Address is *after* the jump.
                             (fref-jmp-address (+ (object-slot-location image value sys.int::+fref-code+) 8))
                             (rel-jump (- entry-point fref-jmp-address)))
                        (check-type rel-jump (signed-byte 32))
                        (ash (ldb (byte 32 0) rel-jump) 32))
                      0)))
    ;; (mov :rbx (:rip fref-function))
    ;; (jmp ...
    (setf (object-slot image value (+ sys.int::+fref-code+ 1))
          #xFFFFFFFFE91D8B48)
    ;; ... (:object :rbx entry-point))
    ;; (ud2)
    (setf (object-slot image value (+ sys.int::+fref-code+ 2))
          #x0B0FFF63)))

(defmethod allocate-object ((object env:cross-compiled-function) image environment)
  (let* ((total-size (+ (* (ceiling (+ (length (env:function-machine-code object)) 16) 16) 2)
                        (length (env:function-constants object))
                        (ceiling (length (env:function-gc-metadata object)) 8)))
         (value (allocate total-size image
                          (or (env:object-area environment object)
                              :function)
                          sys.int::+tag-object+)))
    ;; Always set the entry point, gets read by other functions.
    (setf (object-slot image value sys.int::+function-entry-point+)
          (+ (- value sys.int::+tag-object+) 16))
    value))

(defun function-header (code-length pool-length metadata-length &optional (tag sys.int::+object-tag-function+))
  (assert (< (ceiling (+ code-length 16) 16) (expt 2 (cross-cl:byte-size sys.int::+function-header-code-size+))))
  (assert (< pool-length (expt 2 (cross-cl:byte-size sys.int::+function-header-pool-size+))))
  (assert (< metadata-length (expt 2 (cross-cl:byte-size sys.int::+function-header-metadata-size+))))
  (logior (ash tag sys.int::+object-type-shift+)
          (ash (logior (cross-cl:dpb (ceiling (+ code-length 16) 16) sys.int::+function-header-code-size+ 0)
                       (cross-cl:dpb pool-length sys.int::+function-header-pool-size+ 0)
                       (cross-cl:dpb metadata-length sys.int::+function-header-metadata-size+ 0))
               sys.int::+object-data-shift+)))

(defmethod initialize-object ((object env:cross-compiled-function) object-value image environment)
  ;; Copy machine code.
  (replace-object-data-octets
   image object-value (env:function-machine-code object)
   ;; After the header & entry point
   :slot 1)
  ;; Copy constants.
  (let ((origin (+ 1
                   (* (ceiling (length (env:function-machine-code object)) 16) 2))))
    (dotimes (i (length (env:function-constants object)))
      (setf (object-slot image object-value (+ origin i))
            (serialize-object (aref (env:function-constants object) i)
                              image environment))))
  ;; Copy GC metadata.
  (replace-object-data-octets
   image object-value (env:function-gc-metadata object)
   ;; After the header, machine code, and constants.
   :slot (+ 1
            (* (ceiling (length (env:function-machine-code object)) 16) 2)
            (length (env:function-constants object))))
  ;; Initialize header.
  (setf (object-slot image object-value -1)
        (function-header (length (env:function-machine-code object))
                         (length (env:function-constants object))
                         (length (env:function-gc-metadata object))))
  ;; Apply fixups.
  (loop
     for (target . byte-offset) in (env:function-fixups object)
     do
       (etypecase target
         (symbol
          (let ((value (serialize-object
                        (env:find-special environment target)
                        image environment)))
            (dotimes (byte 4)
              (multiple-value-bind (word byten)
                  (truncate (+ byte-offset -16 byte) 8)
                (setf (ldb (byte 8 (* byten 8))
                           (object-slot image object-value (+ 1 word)))
                      (ldb (byte 8 (* byte 8)) value))))))
         (env:function-reference
          (let* ((fref-val (serialize-object target image environment))
                 (entry (object-slot-location image fref-val sys.int::+fref-code+))
                 (absolute-origin (+ (- object-value sys.int::+tag-object+)
                                     byte-offset
                                     4))
                 (value (- entry absolute-origin)))
            (check-type value (signed-byte 32))
            (dotimes (byte 4)
              (multiple-value-bind (word byten)
                  (truncate (+ byte-offset -16 byte) 8)
                (setf (ldb (byte 8 (* byten 8))
                           (object-slot image object-value (+ 1 word)))
                      (ldb (byte 8 (* byte 8)) value)))))))))

;; TODO: Avoid recursing down lists. Customize SERIALIZE-OBJECT.
(defmethod allocate-object ((object cons) image environment)
  (let ((area (env:object-area environment object)))
    (ecase area
      ((nil) (allocate 2 image :cons sys.int::+tag-cons+))
      ((:pinned :wired)
       (let ((value (allocate 4 image area sys.int::+tag-object+)))
         (initialize-object-header image value sys.int::+object-tag-cons+ 0)
         (+ (- value sys.int::+tag-object+) 16 sys.int::+tag-cons+))))))

(defmethod initialize-object ((object cons) object-value image environment)
  (setf (object-car image object-value) (serialize-object (car object) image environment)
        (object-cdr image object-value) (serialize-object (cdr object) image environment)))

(defmethod allocate-object ((object string) image environment)
  (let* ((area (or (env:object-area environment object)
                   :general))
         (str (allocate 6 image area sys.int::+tag-object+))
         (data (allocate (1+ (ceiling (length object) 8)) image area sys.int::+tag-object+)))
    ;; TODO: Support strings with characters outside the latin-1 range.
    ;; Will need to allocate a wider string container.
    ;; String container.
    (initialize-object-header image str sys.int::+object-tag-simple-string+ 1)
    (setf (object-slot image str sys.int::+complex-array-storage+) data)
    (setf (object-slot image str sys.int::+complex-array-fill-pointer+)
          (serialize-object nil image environment))
    (setf (object-slot image str sys.int::+complex-array-info+)
          (serialize-object nil image environment))
    (setf (object-slot image str sys.int::+complex-array-axis-0+)
          (serialize-object (length object) image environment))
    ;; String data, bytes.
    (initialize-object-header image data sys.int::+object-tag-array-unsigned-byte-8+ (length object))
    (dotimes (i (ceiling (length object) 8))
      (let ((value 0))
        (dotimes (j 8)
          (when (< (+ (* i 8) j) (length object))
            (setf (ldb (byte 8 64) value) (char-code (char object (+ (* i 8) j)))))
          (setf value (ash value -8)))
        (setf (object-slot image data i) value)))
    str))

(defmethod initialize-object ((object string) value image environment)
  nil)

;; FIXME: Need to take these from the specialized array definitions.
(defun array-element-type-size (element-type)
  (cond ((equal element-type 'bit)
         (values 1 sys.int::+object-tag-array-bit+))
        ((equal element-type '(unsigned-byte 8))
         (values 8 sys.int::+object-tag-array-unsigned-byte-8+))
        ((equal element-type '(unsigned-byte 16))
         (values 16 sys.int::+object-tag-array-unsigned-byte-16+))
        ((equal element-type '(unsigned-byte 32))
         (values 32 sys.int::+object-tag-array-unsigned-byte-32+))
        ((equal element-type '(unsigned-byte 64))
         (values 64 sys.int::+object-tag-array-unsigned-byte-64+))
        ((equal element-type 't)
         (values 64 sys.int::+object-tag-array-t+))
        (t (error "Unsupported array element-type ~S" element-type))))

(defmethod allocate-object ((object array) image environment)
  (assert (eql (array-rank object) 1)) ; TODO
  (let* ((element-type (env:cross-array-element-type environment object))
         (area (or (env:object-area environment object)
                   :general))
         (n-elements (length object)))
    (multiple-value-bind (element-size object-type)
        (array-element-type-size element-type)
      (let* ((elements-per-word (/ 64 element-size))
             (total-data-words (ceiling (* n-elements element-size) 64))
             (image-array (allocate (1+ total-data-words) image area sys.int::+tag-object+)))
        (initialize-object-header image image-array object-type n-elements)
        ;; Write the data directly for numeric arrays.
        (cond
          ((equal element-type '(unsigned-byte 8))
           (replace-object-data-octets image image-array object))
          ((not (eql element-type 't))
           (dotimes (i total-data-words)
             (let ((value 0))
               (dotimes (j elements-per-word)
                 (let ((idx (+ (* i elements-per-word) j)))
                   (when (< idx n-elements)
                     (setf (ldb (byte element-size 64) value)
                           (aref object idx))))
                 (setf value (ash value (- element-size))))
               (setf (object-slot image image-array i) value)))))
        image-array))))

(defmethod initialize-object ((object array) value image environment)
  (when (eql (env:cross-array-element-type environment object) 't)
    (dotimes (i (length object))
      (setf (object-slot image value i)
            (serialize-object (aref object i) image environment)))))

(defmethod serialize-object :around ((object array) image environment)
  ;; Deduplicate strings & bit-vectors in the image.
  (let* ((key (cons object (env:object-area environment object)))
         (existing (gethash key (image-dedup-table image))))
    (when (not existing)
      (setf existing (call-next-method)
            (gethash key (image-dedup-table image)) existing))
    existing))

(defmethod serialize-object ((object character) image environment)
  (logior (ash (char-code object)
               (+ (cross-cl:byte-position sys.int::+immediate-tag+)
                  (cross-cl:byte-size sys.int::+immediate-tag+)))
          (cross-cl:dpb sys.int::+immediate-tag-character+
                        sys.int::+immediate-tag+
                        0)
          sys.int::+tag-immediate+))

(defmethod allocate-object ((object integer) image environment)
  ;; Saving a bignum.
  (assert (not (typep object `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))))
  (let* ((n-data-words (ceiling (1+ (integer-length object)) 64))
         (value (allocate (1+ n-data-words) image :general sys.int::+tag-object+)))
    (initialize-object-header image value sys.int::+object-tag-bignum+ n-data-words)
    (dotimes (i n-data-words)
      (setf (object-slot image value i) (ldb (byte 64 (* i 64)) object)))
    value))

(defmethod initialize-object ((object integer) value image environment)
  nil)

(defmethod serialize-object :around ((object integer) image environment)
  (cond ((typep object `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
         ;; Fixnum.
         (ash (ldb (byte (- 64 sys.int::+n-fixnum-bits+) 0) object) sys.int::+n-fixnum-bits+))
        (t
         ;; Deduplicate bignums in the image.
         (let* ((key (cons object (env:object-area environment object)))
                (existing (gethash key (image-dedup-table image))))
           (when (not existing)
             (setf existing (call-next-method)
                   (gethash key (image-dedup-table image)) existing))
           existing))))

(defmethod serialize-object ((object single-float) image environment)
  (logior (ash (sys.int::%single-float-as-integer object) 32)
          (cross-cl:dpb sys.int::+immediate-tag-single-float+
                        sys.int::+immediate-tag+
                        0)
          sys.int::+tag-immediate+))

(defmethod allocate-object ((object cross-support::cross-short-float) image environment)
  (allocate 2 image :general sys.int::+tag-object+))

(defmethod initialize-object ((object cross-support::cross-short-float) value image environment)
  (initialize-object-header image value sys.int::+object-tag-short-float+ 0)
  (setf (object-slot image value 0) (sys.int::%short-float-as-integer object)))

(defmethod serialize-object :around ((object cross-support::cross-short-float) image environment)
  ;; Deduplicate short floats in the image.
  (let* ((key (cons object nil))
         (existing (gethash key (image-dedup-table image))))
    (when (not existing)
      (setf existing (call-next-method)
            (gethash key (image-dedup-table image)) existing))
    existing))

(defmethod allocate-object ((object double-float) image environment)
  (allocate 2 image :general sys.int::+tag-object+))

(defmethod initialize-object ((object double-float) value image environment)
  (initialize-object-header image value sys.int::+object-tag-double-float+ 0)
  (setf (object-slot image value 0) (sys.int::%double-float-as-integer object)))

(defmethod serialize-object :around ((object double-float) image environment)
  ;; Deduplicate double floats in the image.
  (let* ((key (cons object nil))
         (existing (gethash key (image-dedup-table image))))
    (when (not existing)
      (setf existing (call-next-method)
            (gethash key (image-dedup-table image)) existing))
    existing))

(defmethod allocate-object ((object cross-support::cross-complex-short-float) image environment)
  (allocate 2 image :general sys.int::+tag-object+))

(defmethod initialize-object ((object cross-support::cross-complex-short-float) value image environment)
  (initialize-object-header image value sys.int::+object-tag-complex-short-float+ 0)
  (setf (object-slot image value 0)
        (logior (sys.int::%short-float-as-integer (cross-support::cross-complex-short-float-realpart object))
                (ash (sys.int::%short-float-as-integer (cross-support::cross-complex-short-float-imagpart object)) 16))))

(defmethod allocate-object ((object complex) image environment)
  (etypecase (realpart object)
    (rational
     (allocate 4 image :general sys.int::+tag-object+))
    (single-float
     (allocate 2 image :general sys.int::+tag-object+))
    (double-float
     (allocate 4 image :general sys.int::+tag-object+))))

(defmethod initialize-object ((object complex) value image environment)
  (etypecase (realpart object)
    (rational
     (initialize-object-header image value sys.int::+object-tag-complex-rational+ 0)
     (setf (object-slot image value 0)
           (serialize-object (realpart object) image environment))
     (setf (object-slot image value 1)
           (serialize-object (realpart object) image environment)))
    (single-float
     (initialize-object-header image value sys.int::+object-tag-complex-single-float+ 0)
     (setf (object-slot image value 0)
           (logior (sys.int::%single-float-as-integer (realpart object))
                   (ash (sys.int::%single-float-as-integer (imagpart object)) 32))))
    (double-float
     (initialize-object-header image value sys.int::+object-tag-complex-double-float+ 0)
     (setf (object-slot image value 1) (sys.int::%double-float-as-integer (realpart object)))
     (setf (object-slot image value 2) (sys.int::%double-float-as-integer (imagpart object))))))

(defmethod serialize-object :around ((object complex) image environment)
  ;; Deduplicate double floats in the image.
  (let* ((key (cons object nil))
         (existing (gethash key (image-dedup-table image))))
    (when (not existing)
      (setf existing (call-next-method)
            (gethash key (image-dedup-table image)) existing))
    existing))

(defmethod serialize-object ((object env:cross-byte) image environment)
  (logior (ash (env:cross-byte-size object) 6)
          (ash (env:cross-byte-position object) 19)
          (cross-cl:dpb sys.int::+immediate-tag-byte-specifier+
                        sys.int::+immediate-tag+
                        0)
          sys.int::+tag-immediate+))

(defun structure-definition-layout (structure-definition image environment)
  "Return the layout associated with STRUCTURE-DEFINITION."
  (check-type structure-definition env:structure-definition)
  (let ((class (env:find-environment-class environment
                                           (env:structure-definition-name structure-definition))))
    (serialize-object (env:cross-class-instance-slot-value
                       class
                       (env:translate-symbol environment 'mezzano.clos::slot-storage-layout))
                      image
                      environment)))

(defun resolve-structure-definition (environment instance-type)
  (etypecase instance-type
    (symbol
     (env:find-structure-definition environment (env:translate-symbol environment instance-type)))
    (env:structure-definition
     instance-type)))

(defun initialize-instance-header (image environment object instance-type)
  (let* ((sdef (resolve-structure-definition environment instance-type))
         (layout (structure-definition-layout sdef image environment)))
    (initialize-object-header image object sys.int::+object-tag-instance+ layout)))

(defmethod serialize-object ((object env:instance-header) image environment)
  (let* ((header-object (env:instance-header-object object))
         (layout (structure-definition-layout header-object image environment)))
    (logior (ash layout sys.int::+object-data-shift+)
            (ash sys.int::+object-tag-instance+ sys.int::+object-type-shift+)
            sys.int::+tag-instance-header+)))

(defun object-slot-by-name (image environment sdef object-value slot-name)
  (object-slot image object-value
               (mezzano.runtime::location-offset-t
                (env:structure-slot-definition-location
                 (find (env:translate-symbol environment slot-name)
                       (env:structure-definition-slots sdef)
                       :key 'env:structure-slot-definition-name)))))

(defun (setf object-slot-by-name) (value image environment sdef object-value slot-name)
  (setf (object-slot image object-value
                     (mezzano.runtime::location-offset-t
                      (env:structure-slot-definition-location
                       (find (env:translate-symbol environment slot-name)
                             (env:structure-definition-slots sdef)
                             :key 'env:structure-slot-definition-name))))
        value))

(defmethod serialize-object ((object env:structure-definition) image environment)
  (serialize-object (env:find-environment-class
                     environment
                     (env:structure-definition-name object))
                    image
                    environment))

(defmethod allocate-object ((object env:instance-object) image environment)
  (let ((sdef (env:instance-structure-definition object)))
    (allocate (1+ (env:structure-definition-size sdef))
              image
              (env:structure-definition-area sdef)
              sys.int::+tag-object+)))

(defun initialize-instance-slot-by-location (instance-value slot-value location index image environment)
  (let ((loc-type (mezzano.runtime::location-type location)))
    (cond ((eql loc-type mezzano.runtime::+location-type-t+)
           (setf (object-slot image instance-value (+ (mezzano.runtime::location-offset-t location) index))
                 (serialize-object slot-value image environment)))
          (t
           ;; TODO: Implement single-/double-float locations
           (let* ((loc-offset (mezzano.runtime::location-offset location))
                  (loc-element-size (mezzano.runtime::location-type-scale loc-type))
                  (loc-element-bit-size (* loc-element-size 8)))
             (multiple-value-bind (slot-index slot-offset)
                 (truncate (+ loc-offset (* index loc-element-size)) (/ 8 loc-element-size))
               (setf (ldb (byte loc-element-bit-size (* slot-offset loc-element-bit-size))
                          (object-slot image instance-value (/ slot-index loc-element-size)))
                     (ldb (byte 0 loc-element-bit-size) slot-value))))))))

(defun initialize-instance-slot (instance-value slot-value slot index image environment)
  (initialize-instance-slot-by-location instance-value
                                        slot-value
                                        (env:structure-slot-definition-location slot)
                                        index
                                        image
                                        environment))

(defmethod initialize-object ((object env:instance-object) value image environment)
  (let ((sdef (env:instance-structure-definition object)))
    (initialize-instance-header image environment value sdef)
    (dolist (slot (env:structure-definition-slots sdef))
      (cond ((env:structure-slot-definition-fixed-vector slot)
             (dotimes (i (env:structure-slot-definition-fixed-vector slot))
               (initialize-instance-slot value
                                         (aref (slot-value object (env:structure-slot-definition-name slot)) i)
                                         slot i
                                         image environment)))
            (t
             (initialize-instance-slot value
                                       (slot-value object (env:structure-slot-definition-name slot))
                                       slot 0
                                       image environment))))))

(defmethod allocate-object ((object env:stack) image environment)
  ;; Allocate memory for the stack itself.
  ;; Lower guard region.
  (incf (image-stack-bump image) #x200000)
  (let ((address (logior (ash sys.int::+address-tag-stack+
                              sys.int::+address-tag-shift+)
                         (image-stack-bump image)))
        (true-size (+ mezzano.supervisor::+thread-stack-soft-guard-size+
                      (env:stack-size object))))
    (incf (image-stack-bump image) true-size)
    (setf (image-stack-bump image) (util:align-up (image-stack-bump image) #x200000))
    (setf (gethash object (image-stack-bases image)) address)
    (incf (image-stack-total image) true-size))
  ;; A wired cons.
  (let ((value (allocate 4 image :wired sys.int::+tag-object+)))
    (initialize-object-header image value sys.int::+object-tag-cons+ 0)
    (+ (- value sys.int::+tag-object+) 16 sys.int::+tag-cons+)))

(defmethod initialize-object ((object env:stack) value image environment)
  (setf (object-car image value)
        (serialize-object (gethash object (image-stack-bases image))
                          image environment))
  (setf (object-cdr image value)
        (serialize-object (env:stack-size object)
                          image environment)))

(defmethod allocate-object ((object sys.int::layout) image environment)
  (let ((layout-sdef (env:find-structure-definition
                      environment
                      (env:translate-symbol environment 'sys.int::layout))))
    (allocate (1+ (env:structure-definition-size layout-sdef))
              image :wired sys.int::+tag-object+)))

(defmethod initialize-object ((object sys.int::layout) value image environment)
  (let ((layout-sdef (env:find-structure-definition
                      environment
                      (env:translate-symbol environment 'sys.int::layout))))
    (initialize-instance-header image environment value 'sys.int::layout)
    (setf (object-slot-by-name image environment layout-sdef value 'sys.int::class)
          (serialize-object (sys.int::layout-class object) image environment))
    (setf (object-slot-by-name image environment layout-sdef value 'sys.int::obsolete)
          (serialize-object (sys.int::layout-obsolete object) image environment))
    (setf (object-slot-by-name image environment layout-sdef value 'sys.int::heap-size)
          (serialize-object (sys.int::layout-heap-size object) image environment))
    (setf (object-slot-by-name image environment layout-sdef value 'sys.int::heap-layout)
          (serialize-object (sys.int::layout-heap-layout object) image environment))
    (setf (object-slot-by-name image environment layout-sdef value 'sys.int::area)
          (serialize-object (sys.int::layout-area object) image environment))
    (setf (object-slot-by-name image environment layout-sdef value 'sys.int::instance-slots)
          (serialize-object (sys.int::layout-instance-slots object) image environment))))

(defmethod allocate-object ((object env:cross-class-instance) image environment)
  (let ((layout (env:cross-class-instance-layout object)))
    (allocate (1+ (sys.int::layout-heap-size layout))
              image
              (or (sys.int::layout-area layout) :general)
              sys.int::+tag-object+)))

(defmethod initialize-object ((object env:cross-class-instance) value image environment)
  (let ((layout (env:cross-class-instance-layout object)))
    (initialize-object-header image value sys.int::+object-tag-instance+
                              (serialize-object layout image environment))
    (loop
       for i below (length (sys.int::layout-instance-slots layout)) by 2
       for slot-name = (svref (sys.int::layout-instance-slots layout) i)
       for slot-location = (svref (sys.int::layout-instance-slots layout) (1+ i))
       do (initialize-instance-slot-by-location value
                                                (if (env:cross-class-instance-slot-boundp object slot-name)
                                                    (env:cross-class-instance-slot-value object slot-name)
                                                    (env:find-special environment :unbound-slot-value))
                                                slot-location 0
                                                image environment))))

(defmethod serialize-object ((object env:layout-proxy) image environment)
  (structure-definition-layout (env:layout-proxy-structure-definition object)
                               image environment))

(defun image-symbol-value (image environment symbol)
  (let ((cell (serialize-object
               (env:symbol-global-value-cell
                environment
                (env:translate-symbol environment symbol))
               image environment)))
    (object-slot image cell sys.int::+symbol-value-cell-value+)))

(defun (setf image-symbol-value) (value image environment symbol)
  (let ((cell (serialize-object
               (env:symbol-global-value-cell
                environment
                (env:translate-symbol environment symbol))
               image environment)))
    (setf (object-slot image cell sys.int::+symbol-value-cell-value+) value)))

(defun finalize-areas (image environment)
  "Build pinned/wired freelists and update GC variables with final area information."
  (let ((wired-free-bins (allocate (1+ 64) image :wired sys.int::+tag-object+))
        (pinned-free-bins (allocate (1+ 64) image :wired sys.int::+tag-object+))
        (wired-function-free-bins (allocate (1+ 64) image :wired sys.int::+tag-object+))
        (function-free-bins (allocate (1+ 64) image :wired sys.int::+tag-object+))
        ;; End of data part, will be followed by free memory after area alignment.
        (wired-area-bump (length (area-data (image-wired-area image))))
        (pinned-area-bump (length (area-data (image-pinned-area image))))
        (general-area-bump (length (area-data (image-general-area image))))
        (cons-area-bump (length (area-data (image-cons-area image))))
        (wired-function-area-bump (length (area-data (image-wired-function-area image))))
        (function-area-bump (length (area-data (image-function-area image)))))
    ;; Ensure a minium amount of free space in :wired.
    ;; And :pinned as well, but that matters less.
    (allocate (* 8 1024 1024) image :wired 0)
    (allocate (* 1 1024 1024) image :pinned 0)
    ;; No further allocation permitted beyond this point.
    (setf (slot-value image '%finalizedp) t)
    ;; Make sure all symbols that are about to be touched are present in-image.
    (dolist (sym '(sys.int::*wired-area-free-bins*
                   sys.int::*pinned-area-free-bins*
                   sys.int::*wired-area-base*
                   sys.int::*wired-area-bump*
                   sys.int::*wired-area-usage*
                   sys.int::*pinned-area-base*
                   sys.int::*pinned-area-bump*
                   sys.int::*pinned-area-usage*
                   sys.int::*general-area-old-gen-bump*
                   sys.int::*general-area-old-gen-limit*
                   sys.int::*cons-area-old-gen-bump*
                   sys.int::*cons-area-old-gen-limit*
                   sys.int::*wired-stack-area-bump*
                   sys.int::*stack-area-bump*
                   sys.int::*bytes-allocated-to-stacks*
                   sys.int::*function-area-base*
                   sys.int::*wired-function-area-limit*
                   sys.int::*wired-function-area-free-bins*
                   sys.int::*wired-function-area-usage*
                   sys.int::*function-area-limit*
                   sys.int::*function-area-free-bins*
                   sys.int::*function-area-usage*))
      ;; This will fail if they're missing.
      (serialize-object (env:translate-symbol environment sym) image environment))
    ;; Better hope we got this right first try.
    (assert (<= (length (area-data (image-wired-function-area image)))
                +cold-generator-assumed-wired-function-area-size+))
    (adjust-array (area-data (image-wired-function-area image))
                  +cold-generator-assumed-wired-function-area-size+
                  :fill-pointer t)
    ;; Align the area sizes up to the minimum alignment.
    (flet ((align (area)
             (let* ((bump (length (area-data area)))
                    (aligned (util:align-up
                              bump sys.int::+allocation-minimum-alignment+)))
               (adjust-array (area-data area) aligned :fill-pointer t))))
      (align (image-wired-area image))
      (align (image-pinned-area image))
      (align (image-general-area image))
      (align (image-cons-area image))
      (align (image-function-area image)))
    ;; General sanity check, the function area total must not exceed the limit.
    (assert (<= (+ (length (area-data (image-wired-function-area image)))
                   (length (area-data (image-function-area image))))
                +function-area-total-size-limit+))
    ;; Initialize the wired/pinned area freelist bins.
    (flet ((init-freelist (bins area area-bump sym usage-sym)
             (initialize-object-header image bins sys.int::+object-tag-array-t+ 64)
             (dotimes (i 64)
               (setf (object-slot image bins i) (serialize-object nil image environment)))
             (setf (image-symbol-value image environment sym) bins)
             (setf (image-symbol-value image environment usage-sym) area-bump)
             (let* ((area-end (length (area-data area)))
                    (n-free-words (truncate (- area-end area-bump) 8)))
               ;; Write freelist entry.
               (setf (nibbles:ub64ref/le (area-data area) area-bump)
                     (logior (ash sys.int::+object-tag-freelist-entry+ sys.int::+object-type-shift+)
                             (ash n-free-words sys.int::+object-data-shift+)))
               (setf (nibbles:ub64ref/le (area-data area) (+ area-bump 8))
                     (serialize-object nil image environment))
               (setf (object-slot image bins (integer-length n-free-words))
                     (ash (+ (area-base area) area-bump)
                          sys.int::+n-fixnum-bits+)))))
      (init-freelist wired-free-bins (image-wired-area image) wired-area-bump 'sys.int::*wired-area-free-bins* 'sys.int::*wired-area-usage*)
      (init-freelist pinned-free-bins (image-pinned-area image) pinned-area-bump 'sys.int::*pinned-area-free-bins* 'sys.int::*pinned-area-usage*)
      (init-freelist wired-function-free-bins (image-wired-function-area image) wired-function-area-bump 'sys.int::*wired-function-area-free-bins* 'sys.int::*wired-function-area-usage*)
      (init-freelist function-free-bins (image-function-area image) function-area-bump 'sys.int::*function-area-free-bins* 'sys.int::*function-area-usage*))
    ;; Assign GC variables.
    ;; Wired/pinned area bumps are the total size of the area including free parts
    ;; They also count from address 0, not the area start.
    (setf (image-symbol-value image environment 'sys.int::*wired-area-base*)
          (serialize-object (area-base (image-wired-area image))
                            image environment))
    (setf (image-symbol-value image environment 'sys.int::*wired-area-bump*)
          (serialize-object (+ (area-base (image-wired-area image))
                               (length (area-data (image-wired-area image))))
                            image environment))
    (setf (image-symbol-value image environment 'sys.int::*pinned-area-base*)
          (serialize-object (area-base (image-pinned-area image))
                            image environment))
    (setf (image-symbol-value image environment 'sys.int::*pinned-area-bump*)
          (serialize-object (+ (area-base (image-pinned-area image))
                               (length (area-data (image-pinned-area image))))
                            image environment))
    ;; General/cons area bumps point at the end of allocated data and
    ;; are area-relative.
    (setf (image-symbol-value image environment 'sys.int::*general-area-old-gen-bump*)
          (serialize-object general-area-bump
                            image environment))
    (setf (image-symbol-value image environment 'sys.int::*general-area-old-gen-limit*)
          (serialize-object (length (area-data (image-general-area image)))
                            image environment))
    (setf (image-symbol-value image environment 'sys.int::*cons-area-old-gen-bump*)
          (serialize-object cons-area-bump
                            image environment))
    (setf (image-symbol-value image environment 'sys.int::*cons-area-old-gen-limit*)
          (serialize-object (length (area-data (image-cons-area image)))
                            image environment))
    ;; Stack areas.
    (setf (image-symbol-value image environment 'sys.int::*wired-stack-area-bump*)
          (serialize-object (image-stack-bump image)
                            image environment))
    (setf (image-symbol-value image environment 'sys.int::*stack-area-bump*)
          (serialize-object +stack-area-base+
                            image environment))
    (setf (image-symbol-value image environment 'sys.int::*bytes-allocated-to-stacks*)
          (serialize-object (image-stack-total image)
                            image environment))
    ;; Function area limits are the lowest address for the wired area
    ;; and the highest address for the normal area.
    (setf (image-symbol-value image environment 'sys.int::*wired-function-area-limit*)
          (serialize-object (area-base (image-wired-function-area image))
                            image environment))
    (setf (image-symbol-value image environment 'sys.int::*function-area-limit*)
          (serialize-object (+ (area-base (image-function-area image))
                               (length (area-data (image-function-area image))))
                            image environment))
    ;; Wired function area extends downwards to limit to here, normal
    ;; function area extends upwards to limit.
    (setf (image-symbol-value image environment 'sys.int::*function-area-base*)
          (serialize-object +function-area-base+
                            image environment))
    ;; Update the image's initial stack pointer value.
    (let* ((initial-thread (env:symbol-global-value environment (env:translate-symbol environment 'sys.int::*initial-thread*)))
           (stack (env:structure-slot-value environment initial-thread 'mezzano.supervisor::stack)))
      (setf (slot-value image '%initial-stack-pointer)
            (+ (gethash stack (image-stack-bases image))
               (env:stack-size stack))))
    (values)))

(defun make-area (name base)
  (make-instance 'area :name name :base base))

(defun make-image (environment)
  (declare (ignore environment))
  (make-instance
   'image
   :wired-area (make-area :wired +wired-area-base+)
   :pinned-area (make-area :pinned +pinned-area-base+)
   :general-area (make-area
                  :general
                  (logior (cross-cl:dpb sys.int::+address-tag-general+
                                        sys.int::+address-tag+ 0)
                          sys.int::+address-old-generation+))
   :cons-area (make-area
               :cons
               (logior (cross-cl:dpb sys.int::+address-tag-cons+
                                     sys.int::+address-tag+ 0)
                       sys.int::+address-old-generation+))
   :wired-function-area (make-area
                         :wired-function
                         (- +function-area-base+
                            +cold-generator-assumed-wired-function-area-size+))
   :function-area (make-area :function +function-area-base+)))

(defun serialize-image (environment)
  "Create a new image from ENVIRONMENT"
  (let ((image (make-image environment)))
    ;; Serialize NIL as the very first thing, this gives it a reasonably stable
    ;; value across images. Helps with debugging.
    (serialize-object 'nil image environment)
    ;; Main part: Serialize all symbols & objects reachable from them.
    ;; TODO: This traverses the object graph in depth-first order, leading
    ;; to functions being scattered over the image randomly. It'd be nice
    ;; to traverse in load order which would cluster functions from the same
    ;; file together.
    (env:do-all-environment-symbols (symbol environment)
      (serialize-object symbol image environment))
    ;; Tell the GC the area sizes.
    (finalize-areas image environment)
    ;; That's all, folks.
    image))

(defun write-map-file (map-file-path image)
  (with-open-file (s map-file-path
                     :direction :output
                     :if-exists :supersede)
    (let ((*print-right-margin* 10000)
          (functions '()))
      (maphash (lambda (obj addr)
                 (typecase obj
                   (env:cross-compiled-function
                    (push (cons (+ (- addr sys.int::+tag-object+) 16)
                                (aref (env:function-constants obj) 0))
                          functions))
                   (env:function-reference
                    (push (cons (+ (- addr sys.int::+tag-object+) 32)
                                (format nil "{Fref ~A}" (env:function-reference-name obj)))
                          functions))))
               (image-object-values image))
      (loop
         for (addr . name) in (sort functions '< :key 'car)
         do (format s "~X ~A~%" addr
                    (cl-ppcre:regex-replace (string #\Newline)
                                            (format nil "~A" name)
                                            "#\\Newline"))))))

(defun write-symbol-table (map-file-path image)
  (with-open-file (s map-file-path
                     :direction :output
                     :if-exists :supersede)
    (let ((*print-right-margin* 10000)
          (symbols '()))
      (maphash (lambda (obj addr)
                 (when (symbolp obj)
                   (push (list (symbol-name obj) addr (object-slot image addr sys.int::+symbol-value-cell-value+))
                         symbols)))
               (image-object-values image))
      (loop
         for (name addr cell-addr) in (sort symbols '< :key 'second)
         do (format s "~X ~X ~A~%" addr cell-addr
                    (cl-ppcre:regex-replace (string #\Newline)
                                            (format nil "~A" name)
                                            "#\\Newline"))))))
