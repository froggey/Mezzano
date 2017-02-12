;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defvar *load-verbose* nil)
(defvar *load-print* nil)

(defvar *load-pathname* nil)
(defvar *load-truename* nil)

(defvar *modules* '())
(defvar *module-provider-functions* '())
(defvar *require-hooks* '())

(defvar *noisy-load* nil)
(defvar *load-wired* nil "When true, allocate objects in the wired area.")

(defun llf-command-name (command)
  (ecase command
    (#.+llf-end-of-load+ 'end-of-load)
    (#.+llf-backlink+ 'backlink)
    (#.+llf-function+ 'function)
    (#.+llf-cons+ 'cons)
    (#.+llf-symbol+ 'symbol)
    (#.+llf-uninterned-symbol+ 'uninterned-symbol)
    (#.+llf-unbound+ 'unbound)
    (#.+llf-string+ 'string)
    (#.+llf-integer+ 'integer)
    (#.+llf-simple-vector+ 'simple-vector)
    (#.+llf-character+ 'character)
    (#.+llf-character-with-bits+ 'character-with-bits)
    (#.+llf-structure-definition+ 'structure-definition)
    (#.+llf-structure-slot-definition+ 'structure-slot-definition)
    (#.+llf-single-float+ 'single-float)
    (#.+llf-proper-list+ 'proper-list)
    (#.+llf-package+ 'package)
    (#.+llf-integer-vector+ 'integer-vector)
    (#.+llf-add-backlink+ 'add-backlink)
    (#.+llf-ratio+ 'ratio)
    (#.+llf-array+ 'array)
    (#.+llf-bit-vector+ 'bit-vector)
    (#.+llf-function-reference+ 'function-reference)
    (#.+llf-byte+ 'byte)
    (#.+llf-double-float+ 'double-float)
    (#.+llf-typed-array+ 'typed-array)
    (#.+llf-funcall-n+ 'funcall-n)
    (#.+llf-drop+ 'drop)
    (#.+llf-complex-rational+ 'complex-rational)
    (#.+llf-complex-single-float+ 'complex-single-float)
    (#.+llf-complex-double-float+ 'complex-double-float)))

(defun llf-architecture-name (id)
  (case id
    (#.+llf-arch-x86-64+ :x86-64)
    (#.+llf-arch-arm64+ :arm64)
    (t :unknown)))

(defun check-llf-header (stream)
  (assert (and (eql (%read-byte stream) #x4C)
               (eql (%read-byte stream) #x4C)
               (eql (%read-byte stream) #x46)
               (eql (%read-byte stream) #x01))
          ()
          "Bad LLF magic while loading ~S. Probably old-style LLF, please remove and rebuild."
          stream)
  (let ((version (load-integer stream)))
    (assert (eql version *llf-version*)
            ()
            "Bad LLF version ~D, wanted version ~D, while loading ~S."
            version *llf-version* stream))
  (let ((arch (llf-architecture-name (load-integer stream))))
    (assert (eql arch
                 #+x86-64 :x86-64
                 #+arm64 :arm64) ()
            "LLF compiled for wrong architecture ~S. Wanted ~S."
            arch (current-architecture))))

(defun load-integer (stream)
  (let ((value 0) (shift 0))
    (loop
         (let ((b (%read-byte stream)))
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
      (utf8-sequence-length (%read-byte stream))
    ;; Read remaining bytes. They must all be continuation bytes.
    (dotimes (i (1- length))
      (let ((byte (%read-byte stream)))
        (unless (eql (logand byte #xC0) #x80)
          (error "Invalid UTF-8 continuation byte ~S." byte))
        (setf value (logior (ash value 6) (logand byte #x3F)))))
    (code-char value)))

(defun load-string (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type 'character :area (if *load-wired*
                                                                 :wired
                                                                 nil))))
    (dotimes (i len)
      (setf (aref seq i) (load-character stream)))
    seq))

(defun load-llf-function (stream stack)
  ;; n constants on stack.
  ;; list of fixups on stack.
  ;; +llf-function+
  ;; tag. (byte)
  ;; mc size in bytes. (integer)
  ;; number of constants. (integer)
  ;; gc-info-length in bytes. (integer)
  (let* ((tag (%read-byte stream))
         (mc-length (load-integer stream))
         (mc (make-array mc-length
                         :element-type '(unsigned-byte 8)
                         :initial-element 0))
         (n-constants (load-integer stream))
         (gc-info-length (load-integer stream))
         (gc-info (make-array gc-info-length
                              :element-type '(unsigned-byte 8)))
         (fixups (vector-pop stack))
         ;; Pull n constants off the value stack.
         (constants (subseq stack (- (length stack) n-constants))))
    ;; Pop constants off.
    (decf (fill-pointer stack) n-constants)
    ;; Read mc bytes.
    (%read-sequence mc stream)
    ;; Read gc-info bytes.
    (%read-sequence gc-info stream)
    (make-function-with-fixups tag mc fixups constants gc-info *load-wired*)))

(defun load-llf-vector (stream stack)
  (let* ((len (load-integer stream))
         (vector (subseq stack (- (length stack) len))))
    ;; Drop vector values.
    (decf (fill-pointer stack) len)
    (if *load-wired*
        (make-array (length vector)
                    :initial-contents vector
                    :area :wired)
        vector)))

(defun structure-slot-definition-compatible (x y)
  (and (eql (structure-slot-name x) (structure-slot-name y))
       (equal (structure-slot-type x) (structure-slot-type y))))

(defun load-llf-structure-definition (stream stack)
  (let* ((area (vector-pop stack))
         (parent (vector-pop stack))
         (slots (vector-pop stack))
         (name (vector-pop stack))
         (definition (get name 'structure-type)))
    (cond (definition
           (unless (and (eql (length (structure-slots definition)) (length slots))
                        (every #'structure-slot-definition-compatible (structure-slots definition) slots))
             (error "Incompatible redefinition of structure. ~S ~S ~S~%" definition (structure-slots definition) slots))
           definition)
          (t
           (let ((def (make-struct-definition name slots parent area)))
             (%defstruct def)
             def)))))

(defun load-llf-structure-slot-definition (stream stack)
  (let* ((read-only (vector-pop stack))
         (type (vector-pop stack))
         (initform (vector-pop stack))
         (accessor (vector-pop stack))
         (name (vector-pop stack)))
    (make-struct-slot-definition name accessor initform type read-only)))

(defun load-llf-array (stream stack)
  (let* ((n-dimensions (load-integer stream))
         (dimensions (loop for i from 0 below n-dimensions
                        collect (load-integer stream)))
         (array (make-array dimensions :area (if *load-wired* :wired nil)))
         (n-elements (array-total-size array))
         (start (- (length stack) n-elements)))
    (dotimes (i n-elements)
      (setf (row-major-aref array i) (aref stack (+ start i))))
    (decf (fill-pointer stack) n-elements)
    array))

(defun load-llf-typed-array (stream stack)
  (let* ((n-dimensions (load-integer stream))
         (dimensions (loop for i from 0 below n-dimensions
                        collect (load-integer stream)))
         (element-type (vector-pop stack))
         (array (make-array dimensions
                            :area (if *load-wired* :wired nil)
                            :element-type element-type))
         (n-elements (array-total-size array))
         (start (- (length stack) n-elements)))
    (dotimes (i n-elements)
      (setf (row-major-aref array i) (aref stack (+ start i))))
    (decf (fill-pointer stack) n-elements)
    array))

(defvar *magic-unbound-value* (cons "Magic unbound value" nil))

(defun load-one-object (command stream stack)
  (when *noisy-load*
    (format t "~S~%" (llf-command-name command)))
  (ecase command
    (#.+llf-function+
     (load-llf-function stream stack))
    (#.+llf-cons+
     (let* ((car (vector-pop stack))
            (cdr (vector-pop stack)))
       (cons-in-area car cdr (if *load-wired* :wired nil))))
    (#.+llf-symbol+
     (let* ((name (load-string stream))
            (package (load-string stream)))
       (intern name package)))
    (#.+llf-uninterned-symbol+
     (let* ((plist (vector-pop stack))
            (fn (vector-pop stack))
            (value (vector-pop stack))
            (name (vector-pop stack))
            (symbol (make-symbol name)))
       (setf (symbol-plist symbol) plist)
       (unless (eql fn *magic-unbound-value*)
         (setf (symbol-function symbol) fn))
       (unless (eql value *magic-unbound-value*)
         (setf (symbol-value symbol) value))
       symbol))
    (#.+llf-unbound+ *magic-unbound-value*)
    (#.+llf-string+ (load-string stream))
    (#.+llf-integer+ (load-integer stream))
    (#.+llf-simple-vector+
     (load-llf-vector stream stack))
    (#.+llf-character+ (load-character stream))
    (#.+llf-character-with-bits+
     (let ((ch (load-character stream))
           (bits (load-integer stream)))
       (%make-character (char-code ch) bits)))
    (#.+llf-structure-definition+
     (load-llf-structure-definition stream stack))
    (#.+llf-structure-slot-definition+
     (load-llf-structure-slot-definition stream stack))
    (#.+llf-single-float+
     (%integer-as-single-float (load-integer stream)))
    (#.+llf-double-float+
     (%integer-as-double-float (load-integer stream)))
    (#.+llf-proper-list+
     (let ((list '())
           (len (load-integer stream)))
       (dotimes (i len)
         (setf list (cons-in-area (vector-pop stack) list (if *load-wired* :wired nil))))
       list))
    (#.+llf-package+
     (let ((package (load-string stream)))
       (or (find-package package)
           (error "No such package ~S." package))))
    (#.+llf-integer-vector+
     (let* ((len (load-integer stream))
            (vec (make-array len :area (if *load-wired* :wired nil))))
       (dotimes (i len)
         (setf (aref vec i) (load-integer stream)))
       vec))
    (#.+llf-ratio+
     (let* ((num (load-integer stream))
            (denom (load-integer stream)))
       (/ num denom)))
    (#.+llf-array+
     (load-llf-array stream stack))
    (#.+llf-bit-vector+
     (let* ((len (load-integer stream))
            (n-octets (ceiling len 8))
            (vec (make-array len :element-type 'bit :area (if *load-wired* :wired nil))))
       (dotimes (i n-octets)
         (let ((octet (%read-byte stream)))
           (dotimes (j 8)
             (when (>= (+ (* i 8) j) len)
               (return))
             (setf (bit vec (+ (* i 8) j)) (ldb (byte 1 j) octet)))))
       vec))
    (#.+llf-function-reference+
     (function-reference (vector-pop stack)))
    (#.+llf-byte+
     (byte (load-integer stream)
           (load-integer stream)))
    (#.+llf-typed-array+
     (load-llf-typed-array stream stack))
    (#.+llf-funcall-n+
     (let* ((n-args (vector-pop stack))
            (fn (vector-pop stack))
            (args (reverse (loop
                              repeat n-args
                              collect (vector-pop stack)))))
       (values (apply (if (functionp fn)
                          fn
                          (fdefinition fn))
                      args))))
    (#.+llf-drop+
     (vector-pop stack)
     (values))
    (#.+llf-complex-rational+
     (let* ((realpart-numerator (load-integer stream))
            (realpart-denominator (load-integer stream))
            (realpart (/ realpart-numerator realpart-denominator))
            (imagpart-numerator (load-integer stream))
            (imagpart-denominator (load-integer stream))
            (imagpart (/ imagpart-numerator imagpart-denominator)))
       (complex realpart imagpart)))
    (#.+llf-complex-single-float+
     (let ((realpart (%integer-as-single-float (load-integer stream)))
           (imagpart (%integer-as-single-float (load-integer stream))))
       (complex realpart imagpart)))
    (#.+llf-complex-double-float+
     (let ((realpart (%integer-as-double-float (load-integer stream)))
           (imagpart (%integer-as-double-float (load-integer stream))))
       (complex realpart imagpart)))))

(defun load-llf (stream &optional (*load-wired* nil))
  (check-llf-header stream)
  (let ((*package* *package*)
        (omap (make-hash-table))
        (stack (make-array 64 :adjustable t :fill-pointer 0)))
    (loop (let ((command (%read-byte stream)))
            (case command
              (#.+llf-end-of-load+
               (when *noisy-load*
                 (format t "END-OF-LOAD~%"))
               (when (not (eql (length stack) 0))
                 (error "Bug! Stack not empty after LLF load."))
               (return))
              (#.+llf-backlink+
               (let ((id (load-integer stream)))
                 (multiple-value-bind (value value-p)
                     (gethash id omap)
                   (unless value-p
                     (error "Unknown backlink ID ~D." id))
                   (when *noisy-load*
                     (format t "BACKLINK ~S~%" id))
                   (vector-push-extend value stack))))
              (#.+llf-add-backlink+
               (let ((id (load-integer stream)))
                 (multiple-value-bind (existing-value existing-value-p)
                     (gethash id omap)
                   (declare (ignore existing-value))
                   (when existing-value-p
                     (error "Duplicate backlink ID ~D." id)))
                 (when *noisy-load*
                   (format t "ADD-BACKLINK ~S~%" id))
                 (setf (gethash id omap) (vector-pop stack))))
              (t (let ((value (multiple-value-list (load-one-object command stream stack))))
                   (when value
                     (vector-push-extend (first value) stack)))))))))

(defun load-lisp-source (stream)
  (let ((*readtable* *readtable*)
        (*package* *package*)
        (*load-truename* (ignore-errors (pathname stream)))
        (*load-pathname* (ignore-errors (pathname stream)))
        (eof (cons nil nil)))
    (loop (let ((form (read stream nil eof)))
            (when (eql form eof) (return))
            (when *load-print*
              (let ((*print-level* 3)
                    (*print-length* 3))
                (format t "~&~@<;; ~@;Loading ~S~:>~%" form)))
            (eval form)))
    t))

(defun load-from-stream (stream wired)
  (when *load-verbose*
    (format t ";;; Loading from ~S~%" stream))
  (cond ((subtypep (stream-element-type stream) 'character)
         (when wired
           (error "Only compiled filed can be loaded wired."))
         (load-lisp-source stream))
        (t
         (load-llf stream wired))))

(defun load (filespec &key
             (verbose *load-verbose*)
             (print *load-print*)
             (if-does-not-exist t)
             (external-format :default)
             wired)
  (let ((*load-verbose* verbose)
        (*load-print* print))
    (cond ((streamp filespec)
           (let* ((*load-pathname* (ignore-errors (pathname filespec)))
                  (*load-truename* (ignore-errors (pathname filespec))))
             (load-from-stream filespec wired)))
          (t (let* ((path (merge-pathnames filespec))
                    (*load-pathname* (pathname path))
                    (*load-truename* (pathname path)))
               (with-open-file (stream filespec
                                       :if-does-not-exist (if if-does-not-exist
                                                              :error
                                                              nil)
                                       :element-type (if (string-equal (pathname-type path) "LLF")
                                                         '(unsigned-byte 8)
                                                         'character)
                                       :external-format (if (string-equal (pathname-type path) "LLF")
                                                            :default
                                                            external-format))
                 (when stream
                   (load-from-stream stream wired))))))))

(defun provide (module-name)
  (pushnew (string module-name) *modules*
           :test #'string=)
  (values))

(defun require (module-name &optional pathname-list)
  (unless (member (string module-name) *modules*
                  :test #'string=)
    (if pathname-list
        (if (listp pathname-list)
            (dolist (pathname pathname-list)
              (load pathname))
            (load pathname-list))
        (dolist (hook (append *module-provider-functions*
                              *require-hooks*)
                 (error "Unable to REQUIRE module ~A." module-name))
          (when (funcall hook module-name)
            (return)))))
  (values))
