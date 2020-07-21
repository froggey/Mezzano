;;;; Loads compiled files into an cold-generator environment

(defpackage :mezzano.cold-generator.load
  (:use :cl)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:util #:mezzano.cold-generator.util)
                    (#:sys.int #:mezzano.internals))
  (:export #:load-compiled-file
           #:invalid-llf
           #:validate-llf-header
           ))

(in-package :mezzano.cold-generator.load)

(defclass loader ()
  ((%environment :initarg :environment :reader loader-environment)
   (%allocation-area :initarg :allocation-area :reader loader-allocation-area)
   (%stream :initarg :stream :reader loader-stream)
   (%object-map :initform (make-hash-table) :reader loader-object-map)
   (%stack :initform (make-array 64 :adjustable t :fill-pointer 0) :reader loader-stack)
   (%if-depth :initform nil :accessor loader-if-depth)
   (%load-time-forms :initform '() :accessor loader-load-time-forms)))

(define-condition invalid-llf (simple-error) ())

(defun validate-llf-header (stream target)
  ;; Check the header.
  (when (not (and (eql (read-byte stream) #x4C)
                  (eql (read-byte stream) #x4C)
                  (eql (read-byte stream) #x46)
                  (eql (read-byte stream) #x01)))
    (error 'invalid-llf
           :format-control "Bad LLF magic."))
  (let ((version (read-byte stream)))
    (when (not (eql version sys.int::*llf-version*))
      (error 'invalid-llf
             :format-control "Bad LLF version ~D, wanted version ~D."
             :format-arguments (list version sys.int::*llf-version*))))
  (let ((arch (case (read-byte stream)
                (#.sys.int::+llf-arch-x86-64+ :x86-64)
                (#.sys.int::+llf-arch-arm64+ :arm64)
                (t :unknown))))
    (when (not (eql arch target))
      (error 'invalid-llf
             :format-control "LLF compiled for wrong architecture ~S. Wanted ~S."
             :format-arguments (list arch target)))))

(defun load-byte (loader)
  (read-byte (loader-stream loader)))

(defun load-integer (loader)
  (let ((value 0) (shift 0))
    (loop
       (let ((b (load-byte loader)))
         (when (not (logtest b #x80))
           (setf value (logior value (ash (logand b #x3F) shift)))
           (if (logtest b #x40)
               (return (- value))
               (return value)))
         (setf value (logior value (ash (logand b #x7F) shift)))
         (incf shift 7)))))

(defun load-character (loader)
  (multiple-value-bind (length value)
      (util:utf-8-sequence-length (load-byte loader))
    ;; Read remaining bytes. They must all be continuation bytes.
    (dotimes (i (1- length))
      (let ((byte (load-byte loader)))
        (unless (eql (logand byte #xC0) #x80)
          (error "Invalid UTF-8 continuation byte ~S." byte))
        (setf value (logior (ash value 6) (logand byte #x3F)))))
    (or (code-char value)
        (error "Implementation missing character U+~X" value))))

(defun load-string (loader)
  (let* ((len (load-integer loader))
         (seq (make-array len :element-type 'character)))
    (dotimes (i len)
      (setf (aref seq i) (load-character loader)))
    seq))

(defun load-byte-vector (loader length)
  (let ((vec (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence vec (loader-stream loader))
    vec))

(defun load-short-float (loader)
  (let ((bits (load-integer loader)))
    (sys.int::%integer-as-short-float bits)))

(defun load-single-float (loader)
  (let ((bits (load-integer loader)))
    (sys.int::%integer-as-single-float bits)))

(defun load-double-float (loader)
  (let ((bits (load-integer loader)))
    (sys.int::%integer-as-double-float bits)))

(defun stack-push (value loader &optional (mode :value))
  (check-type mode (member :value :lazy))
  (vector-push-extend (cons mode value) (loader-stack loader)))

(defun stack-pop (loader)
  (let* ((entry (vector-pop (loader-stack loader)))
         (mode (car entry))
         (value (cdr entry)))
    (ecase mode
      (:value value)
      (:lazy (error "Can't evaluate lazy value ~S" value)))))

(defun stack-pop-lazy (loader)
  "Pop the stack, returning a form that evaluates to the value."
  (let* ((entry (vector-pop (loader-stack loader)))
         (mode (car entry))
         (value (cdr entry)))
    (ecase mode
      (:value `',value)
      (:lazy value))))

(defun load-llf-function (loader)
  ;; n constants on stack.
  ;; list of fixups on stack.
  ;; +llf-function+
  ;; tag. (byte)
  ;; mc size in bytes. (integer)
  ;; number of constants. (integer)
  ;; gc-info-length in bytes. (integer)
  ;; mc
  ;; gc-info
  (let* ((tag (load-byte loader))
         (mc-length (load-integer loader))
         (n-constants (load-integer loader))
         (gc-info-length (load-integer loader))
         (mc (load-byte-vector loader mc-length))
         (gc-info (load-byte-vector loader gc-info-length))
         (fixups (stack-pop loader))
         ;; Pull n constants off the value stack.
         (constants (reverse (loop
                                repeat n-constants
                                collect (stack-pop loader)))))
    (assert (eql tag sys.int::+object-tag-function+))
    (env:make-function (loader-environment loader)
                       mc
                       gc-info
                       constants
                       fixups
                       (ecase (loader-allocation-area loader)
                         ((nil) :function)
                         ((:wired) :wired-function)))))

(defun load-structure-definition (loader)
  (let* ((has-standard-constructor (stack-pop loader))
         (docstring (stack-pop loader))
         (sealed (stack-pop loader))
         (layout (stack-pop loader))
         (size (stack-pop loader))
         (area (stack-pop loader))
         (parent (stack-pop loader))
         (slots (stack-pop loader))
         (name (stack-pop loader))
         (sdef (env:make-structure-definition
                (loader-environment loader)
                name
                slots
                parent
                area
                size
                layout
                sealed
                docstring
                has-standard-constructor)))
    (env:register-structure-definition (loader-environment loader) sdef)))

(defun load-structure-slot-definition (loader)
  (let ((documentation (stack-pop loader))
        (dcas-sibling (stack-pop loader))
        (align (stack-pop loader))
        (fixed-vector (stack-pop loader))
        (location (stack-pop loader))
        (read-only (stack-pop loader))
        (type (stack-pop loader))
        (initform (stack-pop loader))
        (accessor (stack-pop loader))
        (name (stack-pop loader)))
    (env:make-structure-slot-definition
     (loader-environment loader)
     name accessor initform type read-only
     location fixed-vector align dcas-sibling
     documentation)))

(defun load-one-object (loader command)
  (ecase command
    (#.sys.int::+llf-function+
     (load-llf-function loader))
    (#.sys.int::+llf-cons+
     (let* ((car (stack-pop loader))
            (cdr (stack-pop loader)))
       (env:cons-in-area car cdr (loader-environment loader) (loader-allocation-area loader))))
    (#.sys.int::+llf-symbol+
     (let* ((name (load-string loader))
            (package (load-string loader)))
       (env:intern (loader-environment loader) name package)))
    (#.sys.int::+llf-uninterned-symbol+
     (let ((name (stack-pop loader)))
       (env:make-symbol (loader-environment loader) name)))
    (#.sys.int::+llf-string+
     (let ((str (load-string loader)))
       (env:make-array (loader-environment loader)
                       (length str)
                       :element-type 'character
                       :area (loader-allocation-area loader)
                       :initial-contents str)))
    (#.sys.int::+llf-integer+
     (load-integer loader))
    (#.sys.int::+llf-simple-vector+
     (let ((length (load-integer loader)))
       (env:make-array (loader-environment loader)
                       length
                       :area (loader-allocation-area loader))))
    (#.sys.int::+llf-character+
     (load-character loader))
    (#.sys.int::+llf-structure-definition+
     (load-structure-definition loader))
    (#.sys.int::+llf-structure-slot-definition+
     (load-structure-slot-definition loader))
    (#.sys.int::+llf-short-float+
     (load-short-float loader))
    (#.sys.int::+llf-single-float+
     (load-single-float loader))
    (#.sys.int::+llf-double-float+
     (load-double-float loader))
    (#.sys.int::+llf-proper-list+
     (let ((list nil)
           (length (load-integer loader)))
       (dotimes (i length)
         (setf list (env:cons-in-area (stack-pop loader)
                                      list
                                      (loader-environment loader)
                                      (loader-allocation-area loader))))
       list))
    (#.sys.int::+llf-integer-vector+
     (let* ((len (load-integer loader))
            (vec (env:make-array (loader-environment loader) len :area (loader-allocation-area loader))))
       (dotimes (i len)
         (setf (aref vec i) (load-integer loader)))
       vec))
    (#.sys.int::+llf-bit-vector+
     (let* ((len (load-integer loader))
            (n-octets (ceiling len 8))
            (vec (env:make-array (loader-environment loader) len :element-type 'bit :area (loader-allocation-area loader))))
       (dotimes (i n-octets)
         (let ((octet (load-byte loader)))
           (dotimes (j 8)
             (when (>= (+ (* i 8) j) len)
               (return))
             (setf (bit vec (+ (* i 8) j)) (ldb (byte 1 j) octet)))))
       vec))
    (#.sys.int::+llf-function-reference+
     (let* ((name (stack-pop loader)))
       (env:function-reference (loader-environment loader) name)))
    (#.sys.int::+llf-byte+
     (let ((size (load-integer loader))
           (position (load-integer loader)))
       (env:make-byte size position)))
    (#.sys.int::+llf-funcall-n+
     (let* ((n-args (stack-pop loader))
            (fn (stack-pop-lazy loader))
            (args (reverse (loop
                              repeat n-args
                              collect (stack-pop-lazy loader)))))
       ;; FN may be a quoted function name, handle this specially.
       (cond ((and (listp fn)
                   (eql (first fn) 'quote)
                   (typep (second fn) '(or symbol
                                        (cons symbol (cons symbol null)))))
              (stack-push `(funcall #',(second fn) ,@args) loader :lazy))
             (t
              (stack-push `(funcall ,fn ,@args) loader :lazy)))
       (values nil t)))
    (#.sys.int::+llf-drop+
     (let* ((entry (vector-pop (loader-stack loader)))
            (mode (car entry))
            (value (cdr entry)))
       (ecase mode
         (:lazy
          ;; Lazy values get evaluated at load-time.
          (push value (loader-load-time-forms loader)))
         (:value
          ;; Values are just values & don't need any load-time work.
          nil)))
     (values nil t))
    (#.sys.int::+llf-complex-rational+
     (let* ((realpart-numerator (load-integer loader))
            (realpart-denominator (load-integer loader))
            (imagpart-numerator (load-integer loader))
            (imagpart-denominator (load-integer loader)))
       (complex (/ realpart-numerator realpart-denominator)
                (/ imagpart-numerator imagpart-denominator))))
    (#.sys.int::+llf-complex-short-float+
     (let* ((realpart (load-short-float loader))
            (imagpart (load-short-float loader)))
       (cross-support::make-cross-complex-short-float :realpart realpart :imagpart imagpart)))
    (#.sys.int::+llf-complex-single-float+
     (let* ((realpart (load-single-float loader))
            (imagpart (load-single-float loader)))
       (complex realpart imagpart)))
    (#.sys.int::+llf-complex-double-float+
     (let* ((realpart (load-double-float loader))
            (imagpart (load-double-float loader)))
       (complex realpart imagpart)))
    (#.sys.int::+llf-typed-array+
     (let* ((n-dimensions (load-integer loader))
            (dimensions (loop for i from 0 below n-dimensions
                           collect (load-integer loader)))
            (element-type (stack-pop loader))
            (array (env:make-array (loader-environment loader)
                                   dimensions
                                   :element-type element-type
                                   :area (loader-allocation-area loader)))
            (total-size (array-total-size array)))
       (dotimes (i total-size)
         (setf (row-major-aref array (- total-size i 1))
               (stack-pop loader)))
       array))
    (#.sys.int::+llf-instance-header+
     (env:make-instance-header
      (loader-environment loader)
      (stack-pop loader)))
    (#.sys.int::+llf-symbol-global-value-cell+
     (let* ((symbol (stack-pop loader)))
       (env:symbol-global-value-cell (loader-environment loader) symbol)))
    (#.sys.int::+llf-if+
     (let ((test (stack-pop-lazy loader)))
       (push (list 'if test :uninitialized :uninitialized) (loader-if-depth loader)))
     (values nil t))
    (#.sys.int::+llf-else+
     (let ((then (stack-pop-lazy loader)))
       (setf (third (first (loader-if-depth loader))) then))
     (values nil t))
    (#.sys.int::+llf-fi+
     (let ((else (stack-pop-lazy loader)))
       (setf (fourth (first (loader-if-depth loader))) else))
     (stack-push (pop (loader-if-depth loader)) loader :lazy)
     (values nil t))
    (#.sys.int::+llf-layout+
     (let ((sdef (stack-pop loader)))
       (env:make-layout-proxy sdef)))
    (#.sys.int::+llf-initialize-array+
     (let* ((length (load-integer loader))
            (elements (make-array length)))
       (dotimes (i length)
         (setf (aref elements (- length i 1)) (stack-pop loader)))
       (let ((array (stack-pop loader)))
         (dotimes (i length)
           (setf (row-major-aref array i) (aref elements i)))
         array)))
))

(defun load-compiled-file (environment filespec &key wired)
  (format t ";; Loading ~S~%" filespec)
  (with-open-file (stream filespec :element-type '(unsigned-byte 8))
    (validate-llf-header stream (env:environment-target environment))
    (let ((loader (make-instance 'loader
                                 :allocation-area (if wired :wired nil)
                                 :environment environment
                                 :stream stream)))
      (loop
         (let ((command (load-byte loader)))
           (case command
             (#.sys.int::+llf-end-of-load+
              (return))
             (#.sys.int::+llf-backlink+
              (let ((id (load-integer loader)))
                (multiple-value-bind (value value-p)
                    (gethash id (loader-object-map loader))
                  (unless value-p
                    (error "Unknown backlink ID ~D." id))
                  (stack-push value loader))))
             (#.sys.int::+llf-add-backlink+
              (let ((id (load-integer loader)))
                (when (nth-value 1 (gethash id (loader-object-map loader)))
                  (error "Duplicate backlink ID ~D." id))
                (setf (gethash id (loader-object-map loader)) (stack-pop loader))))
             (t
              (multiple-value-bind (value inhibit-value-p)
                  (load-one-object loader command)
                (when (not inhibit-value-p)
                  (stack-push value loader)))))))
      (reverse (loader-load-time-forms loader)))))
