;;;; Copyright (c) 2011-2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Loading from compiled files.

(in-package :cold-generator)

(define-condition invalid-llf (simple-error) ())

(defun validate-llf-header (stream)
  ;; Check the header.
  (when (not (and (eql (read-byte stream) #x4C)
                  (eql (read-byte stream) #x4C)
                  (eql (read-byte stream) #x46)
                  (eql (read-byte stream) #x01)))
    (error 'invalid-llf
           :format-control "Bad LLF magic."))
  (let ((version (load-integer stream)))
    (when (not (eql version sys.int::*llf-version*))
      (error 'invalid-llf
             :format-control "Bad LLF version ~D, wanted version ~D."
             :format-arguments (list version sys.int::*llf-version*))))
  (let ((arch (case (load-integer stream)
                (#.sys.int::+llf-arch-x86-64+ :x86-64)
                (#.sys.int::+llf-arch-arm64+ :arm64)
                (t :unknown))))
    (when (not (eql arch sys.c::*target-architecture*))
      (error 'invalid-llf
             :format-control "LLF compiled for wrong architecture ~S. Wanted ~S."
             :format-arguments (list arch sys.c::*target-architecture*)))))

(defvar *load-should-set-fdefinitions*)

;;; Mostly duplicated from the file compiler...
(defun load-integer (stream)
  (let ((value 0) (shift 0))
    (loop
         (let ((b (read-byte stream)))
           (when (not (logtest b #x80))
             (setf value (logior value (ash (logand b #x3F) shift)))
             (if (logtest b #x40)
                 (return (- value))
                 (return value)))
           (setf value (logior value (ash (logand b #x7F) shift)))
           (incf shift 7)))))

(defun utf8-sequence-length (byte)
  (cond
    ((eql (logand byte #x80) #x00)
     (values 1 byte))
    ((eql (logand byte #xE0) #xC0)
     (values 2 (logand byte #x1F)))
    ((eql (logand byte #xF0) #xE0)
     (values 3 (logand byte #x0F)))
    ((eql (logand byte #xF8) #xF0)
     (values 4 (logand byte #x07)))
    (t (error "Invalid UTF-8 lead byte ~S." byte))))

(defun load-character (stream)
  (multiple-value-bind (length value)
      (utf8-sequence-length (read-byte stream))
    ;; Read remaining bytes. They must all be continuation bytes.
    (dotimes (i (1- length))
      (let ((byte (read-byte stream)))
        (unless (eql (logand byte #xC0) #x80)
          (error "Invalid UTF-8 continuation byte ~S." byte))
        (setf value (logior (ash value 6) (logand byte #x3F)))))
    value))

(defun load-ub8-vector (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type '(unsigned-byte 8))))
    (read-sequence seq stream)
    seq))

(defun load-string (stream)
  (let* ((len (load-integer stream))
         (string-data (make-array len :element-type '(unsigned-byte 32)))
         (min-width 1))
    ;; Read all characters and figure out how wide the data vector must be.
    (dotimes (i len)
      (let ((ch (load-character stream)))
        (setf (aref string-data i) ch)
        (setf min-width (max min-width
                             (cond ((>= ch (expt 2 16)) 4)
                                   ((>= ch (expt 2 8)) 2)
                                   (t 1))))))
    (let* ((as-string (map 'string #'code-char string-data))
           (existing (gethash as-string *string-dedup-table*)))
      (unless existing
        (let ((object-address (allocate 6))
              (data-value (ecase min-width
                            (4 (save-ub32-vector string-data))
                            (2 (save-ub16-vector string-data))
                            (1 (save-ub8-vector string-data)))))
          ;; String container
          (setf (word (+ object-address 0)) (array-header sys.int::+object-tag-string+ 1)
                (word (+ object-address 1)) data-value
                (word (+ object-address 2)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
                (word (+ object-address 3)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
                (word (+ object-address 4)) (make-fixnum len))
          (setf existing (make-value object-address sys.int::+tag-object+)
                (gethash as-string *string-dedup-table*) existing)))
      existing)))

(defun load-string* (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type 'character)))
    (dotimes (i len)
      (setf (aref seq i) (code-char (load-character stream))))
    seq))

(defun load-structure-definition (name* slots* parent* area*)
  (let* ((name (extract-object name*))
         (slots (extract-object slots*))
         (definition (gethash name *struct-table*)))
    (cond (definition
           (ensure-structure-layout-compatible definition slots)
           (make-value (first definition) sys.int::+tag-object+))
          (t (let ((address (allocate 7 :wired)))
               (setf (word address) (array-header sys.int::+object-tag-structure-object+ 6))
               (setf (word (+ address 1)) (make-value *structure-definition-definition* sys.int::+tag-object+))
               (setf (word (+ address 2)) name*)
               (setf (word (+ address 3)) slots*)
               (setf (word (+ address 4)) parent*)
               (setf (word (+ address 5)) area*)
               (setf (word (+ address 6)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))
               (setf (gethash name *struct-table*) (list address name slots))
               (make-value address sys.int::+tag-object+))))))

(defun cross-value-p (value)
  (and (consp value)
       (eql (first value) :cross-value)))

(defun convert-host-tree-to-cross (tree)
  (cond ((cross-value-p tree)
         (second tree))
        ((consp tree)
         (vcons (convert-host-tree-to-cross (car tree))
                (convert-host-tree-to-cross (cdr tree))))
        ((symbolp tree)
         (vintern (symbol-name tree)
                  (canonical-symbol-package tree)))
        (t
         (error "Unsupported object ~S" tree))))

(defun stack-pop (stack &optional (evaluation-mode :force))
  (let ((value (vector-pop stack)))
    (cond ((integerp value)
           (if (eql evaluation-mode :lazy)
               `(:cross-value ,value)
               value))
          (t
           (ecase evaluation-mode
             (:force
              ;; TODO: Evaluate if possible, error otherwise.
              (error "Force evaluation of ~S." value))
             (:load
              ;; Put it in load-time-evals.
              ;; TODO: Try to constant-fold as much as possible.
              (push (convert-host-tree-to-cross (first value))
                    *load-time-evals*)
              nil)
             (:lazy
              (first value)))))))

(defun load-llf-function (stream stack)
  ;; n constants on stack.
  ;; list of fixups on stack.
  ;; +llf-function+
  ;; tag. (byte)
  ;; mc size in bytes. (integer)
  ;; number of constants. (integer)
  ;; gc-info-length in bytes. (integer)
  ;; mc
  ;; gc-info
  (let* ((tag (read-byte stream))
         (mc-length (load-integer stream))
         ;; mc-length does not include the 16 byte function header.
         (mc (make-array (* (ceiling (+ mc-length 16) 8) 8)
                         :element-type '(unsigned-byte 8)
                         :initial-element 0))
         (n-constants (load-integer stream))
         (gc-info-length (load-integer stream))
         (gc-info (make-array (* (ceiling gc-info-length 8) 8)
                              :element-type '(unsigned-byte 8)))
         (fixups (stack-pop stack))
         ;; Pull n constants off the value stack.
         (constants (reverse (loop
                                repeat n-constants
                                collect (stack-pop stack))))
         (total-size (+ (* (ceiling (+ mc-length 16) 16) 2)
                        n-constants
                        (ceiling gc-info-length 8)))
         (address (allocate total-size *default-pinned-allocation-area*)))
    ;; Read mc bytes.
    (read-sequence mc stream :start 16 :end (+ 16 mc-length))
    ;; Copy machine code bytes.
    (dotimes (i (ceiling (+ mc-length 16) 8))
      (setf (word (+ address i)) (nibbles:ub64ref/le mc (* i 8)))
      #+nil(when (not (member i '(0 1)))
        (lock-word (+ address i))))
    ;; Read GC bytes.
    (read-sequence gc-info stream :end gc-info-length)
    ;; Copy GC bytes.
    (dotimes (i (ceiling gc-info-length 8))
      (setf (word (+ address
                     (* (ceiling (+ mc-length 16) 16) 2)
                     n-constants
                     i))
            (nibbles:ub64ref/le gc-info (* i 8)))
      (lock-word (+ address
                    (* (ceiling (+ mc-length 16) 16) 2)
                    n-constants
                    i)))
    ;; Set function header.
    (setf (word address) 0)
    (setf (word (1+ address)) (* (+ address 2) 8))
    (lock-word (1+ address))
    (setf (word address) (function-header mc-length n-constants gc-info-length tag))
    (lock-word address)
    ;; Set constant pool.
    (dotimes (i (length constants))
      (setf (word (+ address (* (ceiling (+ mc-length 16) 16) 2) i))
            (elt constants i))
      (lock-word (+ address (* (ceiling (+ mc-length 16) 16) 2) i)))
    ;; Add to the function map.
    (push (list address (extract-object (elt constants 0)))
          *function-map*)
    ;; Add fixups to the list.
    (dolist (fixup (extract-object fixups))
      (assert (>= (cdr fixup) 16))
      (push (list (car fixup) address (cdr fixup) :signed32 (elt constants 1))
            *pending-fixups*))
    ;; Done.
    (make-value address sys.int::+tag-object+)))

(defun load-llf-vector (stream stack)
  (let* ((len (load-integer stream))
         (address (allocate (1+ len))))
    ;; Header word.
    (setf (word address) (array-header sys.int::+object-tag-array-t+ len))
    ;; Drop vector values and copy them into the image.
    (decf (fill-pointer stack) len)
    (dotimes (i len)
      (setf (word (+ address 1 i)) (aref stack (+ (length stack) i))))
    (make-value address sys.int::+tag-object+)))

(defun load-structure-slot-definition (name accessor initform type read-only)
  (let ((image-def (vmake-struct-slot-def name accessor initform type read-only))
        (cross-def (sys.int::make-struct-slot-definition name accessor initform type read-only)))
    (setf (gethash image-def *image-to-cross-slot-definitions*) cross-def)
    image-def))

(defun maybe-eval-funcall-n (name name-value args-values)
  (cond ((and *load-should-set-fdefinitions*
              (eql name 'sys.int::%defun)
              (eql (length args-values) 2)
              (cross-value-p (elt args-values 0))
              (cross-value-p (elt args-values 1)))
         (let* ((defun-name-value (second (elt args-values 0)))
                (fn-value (second (elt args-values 1)))
                (defun-name (extract-object defun-name-value))
                (fref (function-reference defun-name)))
           (setf (word (+ fref 2)) fn-value
                 (word (+ fref 3)) (word (1+ (pointer-part fn-value))))
           defun-name-value))
        (t
         nil)))

(defun load-one-object (command stream stack)
  (ecase command
    (#.sys.int::+llf-function+
     (load-llf-function stream stack))
    (#.sys.int::+llf-cons+
     (let* ((car (stack-pop stack))
            (cdr (stack-pop stack)))
       (vcons car cdr)))
    (#.sys.int::+llf-symbol+
     (let* ((name (load-string* stream))
            (package (load-string* stream)))
       (make-value (symbol-address name package)
                   sys.int::+tag-object+)))
    (#.sys.int::+llf-uninterned-symbol+
     (let ((plist (stack-pop stack))
           (fn (stack-pop stack))
           (value (stack-pop stack))
           (name (stack-pop stack))
           (address (allocate 8 :wired))
           (global-cell (allocate 4 :wired)))
       ;; FN and VALUE may be the unbound tag.
       (setf (word (+ address 0)) (array-header sys.int::+object-tag-symbol+ 0)
             (word (+ address 1)) name
             (word (+ address 2)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
             (word (+ address 3)) (make-value global-cell sys.int::+tag-object+)
             (word (+ address 4)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+)
             (word (+ address 5)) plist
             (word (+ address 6)) (vsym t))
       (setf (word (+ global-cell 0)) (array-header sys.int::+object-tag-array-t+ 3)
             (word (+ global-cell 1)) (vsym nil)
             (word (+ global-cell 2)) (make-value address sys.int::+tag-object+)
             (word (+ global-cell 3)) value)
       (unless (eql fn (unbound-value))
         (error "Uninterned symbol with function not supported."))
       (make-value address sys.int::+tag-object+)))
    (#.sys.int::+llf-unbound+ (unbound-value))
    (#.sys.int::+llf-string+ (load-string stream))
    (#.sys.int::+llf-integer+
     (let ((value (load-integer stream)))
       (typecase value
         ((signed-byte 63) (make-fixnum value))
         (t (make-bignum value)))))
    (#.sys.int::+llf-simple-vector+
     (load-llf-vector stream stack))
    (#.sys.int::+llf-character+
     (logior (ash (load-character stream) 4)
             sys.int::+tag-character+))
    (#.sys.int::+llf-structure-definition+
     (let ((area (stack-pop stack))
           (parent (stack-pop stack))
           (slots (stack-pop stack))
           (name (stack-pop stack)))
       (load-structure-definition name slots parent area)))
    (#.sys.int::+llf-structure-slot-definition+
     (let ((read-only (stack-pop stack))
           (type (stack-pop stack))
           (initform (stack-pop stack))
           (accessor (stack-pop stack))
           (name (stack-pop stack)))
       (load-structure-slot-definition name accessor initform type read-only)))
    (#.sys.int::+llf-single-float+
     (logior (ash (load-integer stream) 32)
             sys.int::+tag-single-float+))
    (#.sys.int::+llf-double-float+
     (let* ((bits (load-integer stream))
            (address (allocate 2)))
       (setf (word address) (array-header sys.int::+object-tag-double-float+ 0)
             (word (1+ address)) bits)
       (make-value address sys.int::+tag-object+)))
    (#.sys.int::+llf-proper-list+
     (let ((list (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))
           (length (load-integer stream)))
       (dotimes (i length)
         (setf list (vcons (stack-pop stack) list)))
       list))
    (#.sys.int::+llf-integer-vector+
     (let* ((len (load-integer stream))
            (address (allocate (1+ len))))
       ;; Header word.
       (setf (word address) (array-header sys.int::+object-tag-array-t+ len))
       (dotimes (i len)
         (let ((value (load-integer stream)))
           (setf (word (+ address 1 i)) (typecase value
                                          ((signed-byte 63) (make-fixnum value))
                                          (t (make-bignum value))))))
       (make-value address sys.int::+tag-object+)))
    (#.sys.int::+llf-bit-vector+
     (let* ((len (load-integer stream))
            (address (allocate (1+ (ceiling len 64)))))
       ;; Header word.
       (setf (word address) (array-header sys.int::+object-tag-array-bit+ len))
       (dotimes (i (ceiling len 8))
         (let ((octet (read-byte stream)))
           (multiple-value-bind (word offset)
               (truncate i 8)
             (setf (ldb (byte 8 (* offset 8))
                        (word (+ address 1 word)))
                   octet))))
       (make-value address sys.int::+tag-object+)))
    (#.sys.int::+llf-function-reference+
     (let* ((name (stack-pop stack))
            (truname (extract-object name)))
       (make-value (function-reference truname)
                   sys.int::+tag-object+)))
    (#.sys.int::+llf-byte+
     (let ((size (load-integer stream))
           (position (load-integer stream)))
       (logior (ash size 4)
               (ash position 18)
               sys.int::+tag-byte-specifier+)))
    (#.sys.int::+llf-funcall-n+
     (let* ((n-args-value (stack-pop stack))
            (n-args (extract-object n-args-value))
            (fn-name-value (stack-pop stack))
            (fn-name (and (not (value-is-function-p fn-name-value))
                          (extract-object fn-name-value)))
            (args-values (reverse (loop
                                     repeat n-args
                                     collect (stack-pop stack :lazy))))
            (value (and fn-name
                        (maybe-eval-funcall-n fn-name fn-name-value args-values))))
       (cond (value)
             (t
              ;; Not able to evaluate the function.
              (list
               `(funcall ,(if (value-is-function-p fn-name-value)
                              `(:cross-value ,fn-name-value)
                              `#'(:cross-value ,fn-name-value))
                         ,@(loop
                              for arg in args-values
                              collect (if (cross-value-p arg)
                                          `',arg
                                          arg))))))))
    (#.sys.int::+llf-drop+
     (stack-pop stack :load)
     nil)
    (#.sys.int::+llf-complex-rational+
     (let* ((realpart-numerator (load-integer stream))
            (realpart-denominator (load-integer stream))
            (imagpart-numerator (load-integer stream))
            (imagpart-denominator (load-integer stream))
            (address (allocate 4)))
       ;; TODO: Support ratios.
       (assert (eql realpart-denominator 1))
       (assert (eql imagpart-denominator 1))
       ;; Header word.
       (setf (word address) (ash sys.int::+object-tag-complex-rational+
                                 sys.int::+object-type-shift+))
       (setf (word (+ address 1)) (typecase realpart-numerator
                                    ((signed-byte 63) (make-fixnum realpart-numerator))
                                    (t (make-bignum realpart-numerator)))
             (word (+ address 2)) (typecase imagpart-numerator
                                    ((signed-byte 63) (make-fixnum imagpart-numerator))
                                    (t (make-bignum imagpart-numerator))))
       (make-value address sys.int::+tag-object+)))
    (#.sys.int::+llf-complex-single-float+
     (let* ((realpart (load-integer stream))
            (imagpart (load-integer stream))
            (address (allocate 2)))
       ;; Header word.
       (setf (word address) (ash sys.int::+object-tag-complex-single-float+
                                 sys.int::+object-type-shift+))
       (setf (word (+ address 1)) (logior realpart
                                          (ash imagpart 32)))
       (make-value address sys.int::+tag-object+)))
    (#.sys.int::+llf-complex-double-float+
     (let* ((realpart (load-integer stream))
            (imagpart (load-integer stream))
            (address (allocate 4)))
       ;; Header word.
       (setf (word address) (ash sys.int::+object-tag-complex-double-float+
                                 sys.int::+object-type-shift+))
       (setf (word (+ address 1)) realpart
             (word (+ address 2)) imagpart)
       (make-value address sys.int::+tag-object+)))
    (#.sys.int::+llf-typed-array+
     (let* ((n-dimensions (load-integer stream))
            (dimensions (loop for i from 0 below n-dimensions
                           collect (load-integer stream)))
            (element-type (extract-object (stack-pop stack)))
            (temp-array (make-array dimensions :element-type element-type))
            (total-size (array-total-size temp-array)))
       ;; Drop vector values and copy them into the image.
       (decf (fill-pointer stack) total-size)
       (dotimes (i total-size)
         (setf (row-major-aref temp-array i) (extract-object (aref stack (+ (length stack) i)))))
       (save-object temp-array)))
))

(defun load-llf (stream)
  (let ((omap (make-hash-table))
        (stack (make-array 64 :adjustable t :fill-pointer 0)))
    (loop (let ((command (read-byte stream)))
            (case command
              (#.sys.int::+llf-end-of-load+
               (return))
              (#.sys.int::+llf-backlink+
               (let ((id (load-integer stream)))
                 (multiple-value-bind (value value-p)
                     (gethash id omap)
                   (unless value-p
                     (error "Unknown backlink ID ~D." id))
                   (vector-push-extend value stack))))
              (#.sys.int::+llf-add-backlink+
               (let ((id (load-integer stream)))
                 (multiple-value-bind (existing-value existing-value-p)
                     (gethash id omap)
                   (declare (ignore existing-value))
                   (when existing-value-p
                     (error "Duplicate backlink ID ~D." id)))
                 (setf (gethash id omap) (stack-pop stack))))
              (t (let ((value (load-one-object command stream stack)))
                   (when value
                     (vector-push-extend value stack)))))))))

(defun load-source-file (file set-fdefinitions &optional wired)
  (let ((*load-should-set-fdefinitions* set-fdefinitions)
        (*default-general-allocation-area* (if wired :wired :general))
        (*default-cons-allocation-area* (if wired :wired :cons))
        (*default-pinned-allocation-area* (if wired :wired :pinned))
        (llf-path (if (string-equal (pathname-type (pathname file)) "llf")
                      file
                      (maybe-compile-file file))))
    (format t ";; Loading ~S.~%" llf-path)
    (with-open-file (s llf-path :element-type '(unsigned-byte 8))
      ;; Check the header.
      (validate-llf-header s)
      ;; Read forms.
      (load-llf s))))
