;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defvar *load-verbose* nil)
(defvar *load-print* nil)

(defvar *load-pathname* nil)
(defvar *load-truename* nil)

(defvar *modules* '())
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
    (#.+llf-setf-symbol+ 'setf-symbol)
    (#.+llf-integer+ 'integer)
    (#.+llf-invoke+ 'invoke)
    (#.+llf-setf-fdefinition+ 'setf-fdefinition)
    (#.+llf-simple-vector+ 'simple-vector)
    (#.+llf-character+ 'character)
    (#.+llf-character-with-bits+ 'character-with-bits)
    (#.+llf-structure-definition+ 'structure-definition)
    (#.+llf-single-float+ 'single-float)
    (#.+llf-proper-list+ 'proper-list)
    (#.+llf-package+ 'package)
    (#.+llf-integer-vector+ 'integer-vector)
    (#.+llf-add-backlink+ 'add-backlink)
    (#.+llf-ratio+ 'ratio)
    (#.+llf-array+ 'array)
    (#.+llf-funcall+ 'funcall)
    (#.+llf-bit-vector+ 'bit-vector)
    (#.+llf-function-reference+ 'function-reference)))

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
            version *llf-version* stream)))

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

(defun load-llf-structure-definition (stream stack)
  (let* ((area (vector-pop stack))
         (parent (vector-pop stack))
         (slots (vector-pop stack))
         (name (vector-pop stack))
         (definition (get name 'structure-type)))
    (cond (definition
           (unless (equal (structure-slots definition) slots)
             (error "Incompatible redefinition of structure. ~S ~S~%" definition slots))
           definition)
          (t (make-struct-definition name slots parent area)))))

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
    (#.+llf-invoke+
     (let ((fn (vector-pop stack)))
       (funcall fn))
     (values))
    (#.+llf-setf-fdefinition+
     (let ((name (vector-pop stack))
           (fn (vector-pop stack)))
       (setf (fdefinition name) fn))
     (values))
    (#.+llf-simple-vector+
     (load-llf-vector stream stack))
    (#.+llf-character+ (load-character stream))
    (#.+llf-character-with-bits+
     (let ((ch (load-character stream))
           (bits (load-integer stream)))
       (%make-character (char-code ch) bits)))
    (#.+llf-structure-definition+
     (load-llf-structure-definition stream stack))
    (#.+llf-single-float+
     (%integer-as-single-float (load-integer stream)))
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
     (/ (load-integer stream)
        (load-integer stream)))
    (#.+llf-array+
     (load-llf-array stream stack))
    (#.+llf-funcall+
     (values (funcall (vector-pop stack))))
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
     (function-reference (vector-pop stack)))))

(defun load-llf (stream &optional (*load-wired* nil))
  (check-llf-header stream)
  (let ((*package* *package*)
        (omap (make-hash-table))
        (stack (make-array 64 :adjustable t :fill-pointer 0)))
    (loop (let ((command (%read-byte stream)))
            (case command
              (#.+llf-end-of-load+
               (return))
              (#.+llf-backlink+
               (let ((id (load-integer stream)))
                 (multiple-value-bind (value value-p)
                     (gethash id omap)
                   (unless value-p
                     (error "Unknown backlink ID ~D." id))
                   (vector-push-extend value stack))))
              (#.+llf-add-backlink+
               (let ((id (load-integer stream)))
                 (multiple-value-bind (existing-value existing-value-p)
                     (gethash id omap)
                   (declare (ignore existing-value))
                   (when existing-value-p
                     (error "Duplicate backlink ID ~D." id)))
                 (setf (gethash id omap) (vector-pop stack))))
              (t (let ((value (multiple-value-list (load-one-object command stream stack))))
                   (when value
                     (vector-push-extend (first value) stack)))))))))

(defun load-lisp-source (stream)
  (let ((*readtable* *readtable*)
        (*package* *package*)
        (*load-truename* stream)
        (*load-pathname* stream)
        (eof (cons nil nil)))
    (loop (let ((form (read stream nil eof)))
            (when (eql form eof) (return))
            (when *load-print* (format t ";; Loading ~S~%" form))
            (eval form)))
    t))

(defun load-from-stream (stream)
  (when *load-verbose*
    (format t ";;; Loading from ~S~%" stream))
  (if (subtypep (stream-element-type stream) 'character)
      (load-lisp-source stream)
      (load-llf stream)))

(defun load (filespec &key
             (verbose *load-verbose*)
             (print *load-print*)
             (if-does-not-exist t)
             (external-format :default))
  (let ((*load-verbose* verbose)
        (*load-print* print))
    (cond ((streamp filespec)
           (let* ((*load-pathname* (pathname filespec))
                  (*load-truename* (pathname filespec)))
             (load-from-stream filespec)))
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
                   (load-from-stream stream))))))))

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
        (dolist (hook *require-hooks*
                 (error "Unable to REQUIRE module ~A." module-name))
          (when (funcall hook module-name)
            (return)))))
  (values))

(defun pprint-indent (relative-to n &optional stream)
  (check-type relative-to (member :block :current))
  (check-type n real)
  nil)
