(in-package #:sys.int)

(declaim (special *cold-toplevel-forms*
                  *package-system*
                  *additional-cold-toplevel-forms*
                  *initial-obarray*
                  *initial-keyword-obarray*
                  *initial-setf-obarray*
                  *initial-structure-obarray*)
         (special *terminal-io*
                  *standard-output*
                  *standard-input*
                  *debug-io*
                  *screen-offset*
                  *keyboard-shifted*))

(defun write-char (character &optional stream)
  (cold-write-char character stream))

(defun start-line-p (stream)
  (cold-start-line-p stream))

(defun read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (cold-read-char stream))

(defun unread-char (character &optional stream)
  (cold-unread-char character stream))

(defun peek-char (&optional peek-type s (eof-error-p t) eof-value recursive-p)
  (cond ((eql peek-type nil)
         (let ((ch (cold-read-char s)))
           (cold-unread-char ch s)
           ch))
        ((eql peek-type t)
         (do ((ch (cold-read-char s)
                  (cold-read-char s)))
             ((not (whitespace[2]p ch))
              (cold-unread-char ch s)
              ch)))
        ((characterp peek-type)
         (error "TODO: character peek."))
        (t (error "Bad peek type ~S." peek-type))))

(defun read-line (&optional (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (do ((result (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
       (c (read-char input-stream eof-error-p nil recursive-p)
          (read-char input-stream eof-error-p nil recursive-p)))
      ((or (null c)
           (eql c #\Newline))
       (if (and (null c) (eql (length result) 0))
           (values eof-value t)
           (values result (null c))))
    (vector-push-extend c result)))

(defun %read-byte (stream)
  (prog1 (aref (car stream) (cdr stream))
    (incf (cdr stream))))

(defun %read-sequence (seq stream)
  (setf (subseq seq 0) (subseq (car stream) (cdr stream) (+ (cdr stream) (length seq))))
  (incf (cdr stream) (length seq)))

(defun yes-or-no-p (&optional control &rest arguments)
  (declare (dynamic-extent arguments))
  (when control
    (write-char #\Newline)
    (apply 'format t control arguments)
    (write-char #\Space))
  (format t "(Yes or No) ")
  (loop
     (let ((line (read-line)))
       (when (string-equal line "yes")
         (return t))
       (when (string-equal line "no")
         (return nil)))
     (write-char #\Newline)
     (format t "Please respond with \"yes\" or \"no\". ")))

(defvar *cold-stream*)
(defun streamp (object)
  (eql object *cold-stream*))

(defun backtrace (&optional limit)
  (do ((i 0 (1+ i))
       (fp (read-frame-pointer)
           (memref-unsigned-byte-64 fp 0)))
      ((or (and limit (> i limit))
           (= fp 0)))
    (write-char #\Newline)
    (write-integer fp 16)
    (write-char #\Space)
    (let* ((fn (memref-t fp -2))
           (name (when (functionp fn) (function-name fn))))
      (write-integer (lisp-object-address fn) 16)
      (when name
        (write-char #\Space)
        (write name)))))

(defun raise-overflow (&optional lhs rhs what)
  (write-char #\!)
  (write-char #\%)
  (write what)
  (write-char #\Space)
  (write lhs)
  (write-char #\Space)
  (write rhs)
  (backtrace)
  (loop (%hlt)))

(defun error (datum &rest arguments)
  (write-char #\!)
  (write datum)
  (write-char #\Space)
  (write arguments)
  (backtrace)
  (loop (%hlt)))

(defun pathnamep (x) nil)
(defun pathnames-equal (x y) nil)

;; NOTE: incomplete.
(defun equal (x y)
  (cond
    ((eql x y))
    ((stringp x)
     (and (stringp y)
          (string= x y)))
    ((consp x)
     (and (consp y)
	  (equal (car x) (car y))
	  (equal (cdr x) (cdr y))))
    ((and (pathnamep x) (pathnamep y))
     (pathnames-equal x y))))

(defun equalp (x y)
  (typecase x
    (character (and (characterp y)
                    (char-equal x y)))
    (number (and (numberp y)
                 (= x y)))
    (cons (and (consp y)
               (equalp (car x) (car y))
               (equalp (cdr x) (cdr y))))
    (vector (and (vectorp y)
                 (eql (length x) (length y))
                 (dotimes (i (length x) t)
                   (when (not (equalp (aref x i) (aref y i)))
                     (return nil)))))
    (array (and (arrayp y)
                (equalp (array-dimensions x) (array-dimensions y))
                (dotimes (i (array-total-size x) t)
                  (when (not (equalp (row-major-aref x i) (row-major-aref y i)))
                    (return nil)))))
    ;; TODO: structures and hash-tables.
    (t (eq x y))))

(defun %with-stream-editor (stream recursive-p function)
  (funcall function))

(defun find-package-or-die (name)
  t)
(defun find-package (name)
  t)
(defun keywordp (object)
  (find object *initial-keyword-obarray*))
(defun find-symbol (name &optional package)
  (let ((pkg (if (string= package "KEYWORD")
                 *initial-keyword-obarray*
                 *initial-obarray*)))
    (dotimes (i (length pkg) (values nil nil))
      (when (string= name (symbol-name (aref pkg i)))
        (return (values (aref pkg i) :internal))))))
(defun cold-intern (name &optional package)
  (let ((pkg (if (string= package "KEYWORD")
                 *initial-keyword-obarray*
                 *initial-obarray*)))
    (dotimes (i (length pkg)
              (let ((sym (make-symbol (string name))))
                (setf (symbol-package sym) (if (string= package "KEYWORD") :keyword t))
                (when (string= package "KEYWORD")
                  (setf (symbol-value sym) sym
                        (symbol-mode sym) :constant))
                (vector-push-extend sym pkg)
                (values sym nil)))
      (when (string= name (symbol-name (aref pkg i)))
        (return (values (aref pkg i) :internal))))))
(defun package-name (package)
  (ecase package
    ((t) "SYSTEM")
    ((:keyword) "KEYWORD")))

(defun %defmacro (name function)
  (funcall #'(setf macro-function) function name))

(defun %compiler-defun (name source-lambda)
  (let ((sym (function-symbol name)))
    (when (or (get sym 'inline-mode)
              (get sym 'inline-form))
      (setf (get sym 'inline-form) source-lambda)))
  nil)

(defun %defun (name lambda)
  (setf (fdefinition name) lambda)
  name)

;; TODO: Symbol macros, macroexpand-hook.
(defun macroexpand-1 (form &optional env)
  (if (consp form)
      (let ((mf (macro-function (car form) env)))
	(if mf
	    (values (funcall mf form env) t)
	    (values form nil)))
      (values form nil)))

(defun macroexpand (form &optional env)
  (let ((did-expand nil))
    (do () (nil)
       (multiple-value-bind (expansion expanded-p)
           (macroexpand-1 form env)
         (if expanded-p
             (setf form expansion
                   did-expand t)
             (return (values form did-expand)))))))

(defun %defstruct (structure-type)
  (setf (get (structure-name structure-type) 'structure-type) structure-type))

(defun function-symbol (name)
  "Convert a function name to a symbol."
  (cond ((symbolp name) name)
	((and (consp name)
	      (= (list-length name) 2)
	      (eql (first name) 'setf)
	      (symbolp (second name)))
	 (let ((sym (get (second name) 'setf-symbol)))
	   (unless sym
	     (setf sym (make-symbol (symbol-name (second name)))
                   (get sym 'setf-symbol-backlink) (second name)
		   (get (second name) 'setf-symbol) sym))
	   sym))
	(t (error "Invalid function name ~S." name))))

(defun fdefinition (name)
  (symbol-function (function-symbol name)))

(defun (setf fdefinition) (value name)
  (setf (symbol-function (function-symbol name)) value))

(defun compiler-macro-function (name &optional environment)
  (get (function-symbol name) 'compiler-macro-function))

(defun (setf compiler-macro-function) (value name &optional environment)
  (setf (get (function-symbol name) 'compiler-macro-function) value))

(defun simplify-string (string)
  (if (simple-string-p string)
      string
      (make-array (length string)
                  :element-type (if (every 'sys.int::base-char-p string)
                                    'base-char
                                    'character)
                  :initial-contents string)))

(declaim (inline identity))
(defun identity (thing)
  thing)

(declaim (inline complement))
(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(defun set-difference (list-1 list-2)
  (let ((result '()))
    (dolist (e list-1)
      (when (not (member e list-2))
	(setq result (cons e result))))
    result))

(defun union (list-1 list-2)
  (let ((result (copy-list list-1)))
    (dolist (e list-2)
      (when (not (member e list-1))
	(setq result (cons e result))))
    result))

(defun intersection (list-1 list-2)
  (when list-1
    (if (member (first list-1) list-2)
        (cons (first list-1) (intersection (rest list-1) list-2))
        (intersection (rest list-1) list-2))))

(declaim (special * ** ***))

(defun repl ()
  (let ((* nil) (** nil) (*** nil))
    (loop
       (fresh-line)
       (write-char #\>)
       (let ((form (read)))
         (fresh-line)
         (let ((result (multiple-value-list (eval form))))
           (setf *** **
                 ** *
                 * (first result))
           (when result
             (dolist (v result)
               (fresh-line)
               (write v))))))))

(defvar *early-initialize-hook* '())
(defvar *initialize-hook* '())

(defun add-hook (hook function)
  (unless (boundp hook)
    (setf (symbol-value hook) '()))
  (pushnew function (symbol-value hook)))

(defun set-gc-light ())
(defun clear-gc-light ())

(defun emergency-halt (message)
  (%cli)
  (dotimes (i (%simple-array-length message))
    (let ((code (logand (char-code (schar message i)) #xFF)))
      (setf (io-port/8 #xE9) code)
      (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 i)
            (logior code #x7000))))
  (loop (%hlt)))

(defun gc-trace (object direction prefix)
  (setf (io-port/8 #xE9) (char-code direction))
  (setf (io-port/8 #xE9) (logand (char-code prefix) #xFF))
  (let ((pointer (%pointer-field object))
        (tag (%tag-field object)))
    (dotimes (i 15)
      (setf (io-port/8 #xE9) (hexify (logand (ash pointer (* -4 (- 14 i))) #b1111))))
    (setf (io-port/8 #xE9) (hexify tag))
    (setf (io-port/8 #xE9) #x0A)))

(defun mumble (message)
  (mumble-string message)
  (setf (io-port/8 #xE9) #x0A))

;;; Used while the GC is copying, so no lookup tables.
(defun hexify (nibble)
  (cond ((<= 0 nibble 9)
         (+ nibble (char-code #\0)))
        (t (+ (- nibble 10) (char-code #\A)))))

(defun mumble-string (message)
  (dotimes (i (%simple-array-length message))
    (let ((code (logand (char-code (schar message i)) #xFF)))
      (setf (io-port/8 #xE9) code)
      (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 i)
            (logior code #x7000)))))
(defun mumble-hex (number)
  (dotimes (i 16)
    (setf (io-port/8 #xE9) (hexify (logand (ash number (* -4 (- 15 i))) #b1111)))))

(defun make-case-correcting-stream (stream case)
  stream)

(defun describe-symbol (object stream)
  (format stream "~S is a symbol, with address ~X~%" object (lisp-object-address object))
  (if (symbol-package object)
      (format stream "  ~A is in the ~A package~%" object (package-name (symbol-package object))))
  (if (boundp object)
      (format stream "  ~A has the value ~S~%" object (symbol-value object)))
  (if (fboundp object)
      (format stream "  ~A is the function ~S~%" object (symbol-function object)))
  (if (symbol-plist object)
      (format stream "  ~A has the plist ~S~%" object (symbol-plist object)))
  (when (symbol-mode object)
    (format stream "  ~A is declared ~A~%" object (symbol-mode object)))
  (when (symbol-tls-slot object)
    (format stream "  ~A uses the TLS slot ~D~%" (symbol-tls-slot object))))

(defun describe-character (object stream)
  (format stream "~S is a ~S~%" object (type-of object)))

(defun describe-float (object stream)
  (format stream "~D is a single-precision floating-point number.~%" object))

(defun describe-function (object stream)
  (multiple-value-bind (lambda-expression closure-p name)
      (function-lambda-expression object)
    (format stream "~S is a ~A" object (if closure-p "closure" "function"))
    (when name
      (format stream " named ~S" name))
    (format stream "~%")))

(defun describe-array (object stream)
  (format stream "~S is a~A array of ~S, with address ~X~%" object
          (if (typep object 'simple-array) " simple" "n")
          (array-element-type object)
          (lisp-object-address object))
  (format stream "  It is ~D elements long.~%" (array-total-size object))
  (if (array-has-fill-pointer-p object)
      (format stream "  It has a fill-pointer of ~S.~%" (fill-pointer object)))
  (format stream "  It has dimensions ~S.~%" (array-dimensions object)))

(defun describe-bignum (object stream)
  (format stream "~S is a bignum, with address ~X~%"
          object (lisp-object-address object)))

(defun describe-structure (object stream)
  (format stream "~S is a structure, with address ~X~%"
          object (lisp-object-address object)))

(defun describe-stack-group (object stream)
  (format stream "~S is a stack-group, with address ~X~%"
          object (lisp-object-address object)))

(defun describe (object &optional (stream *standard-output*))
  (case stream
    ((nil) (setf stream *standard-output*))
    ((t) (setf stream *terminal-io*)))
  (case (logand (lisp-object-address object) 15)
    (#b0000 (format stream "~D is an even fixnum.~%" object))
    ;; TODO: Identify proper/dotted/circular lists.
    (#b0001 (format stream "~S is a list, with address ~X~%" object (lisp-object-address object)))
    (#b0010 (describe-symbol object stream))
    (#b0011 (describe-array object stream)) ; simple array.
    (#b0100 (describe-object object stream)) ; instance object.
    ;; #b0101
    ;; #b0110
    (#b0111 (cond ((structure-object-p object)
                   (describe-structure object stream))
                  ((stack-group-p object)
                   (describe-stack-group object stream))
                  ((bignump object)
                   (describe-bignum object))
                  (t (describe-array object stream))))
    (#b1000 (format stream "~D is an odd fixnum.~%" object))
    ;; #b1001
    (#b1010 (describe-character object stream))
    (#b1011 (describe-float object stream))
    (#b1100 (if (funcallable-std-instance-p object)
                (describe-object object stream)
                (describe-function object stream)))
    ;; #b1101
    (#b1110 (format stream "This is an unbound value marker.~%"))
    (#b1111 (format stream "This is a GC forwarding pointer, pointing to address ~X~%" (logand (lisp-object-address object)
                                                                                               (lognot #xF))))
    (t (format stream "~S is an unknown/invalid object, with address ~X~%" object (lisp-object-address object))))
  (values))

(declaim (special *features* *macroexpand-hook*))

(defun multiboot-module-count ()
  (if (logtest (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 0) +multiboot-flag-modules+)
      (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 5)
      0))

(defun multiboot-module-info ()
  (if (logtest (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 0) +multiboot-flag-modules+)
      (make-array (* (multiboot-module-count) 4)
                  :element-type '(unsigned-byte 32)
                  :memory (+ #x8000000000 (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 6)))
      (make-array 0 :element-type '(unsigned-byte 32))))

(defparameter *maximum-multiboot-command-line-length* 100)

(defun multiboot-module (id)
  (assert (< id (multiboot-module-count)))
  (let* ((info (multiboot-module-info))
         (base (aref info (+ (* id 4) 0)))
         (end (aref info (+ (* id 4) 1)))
         (size (- end base))
         (command-line (+ #x8000000000 (aref info (+ (* id 4) 2))))
         (cmdl-len (dotimes (i *maximum-multiboot-command-line-length*
                             *maximum-multiboot-command-line-length*)
                     (when (eql (memref-unsigned-byte-8 command-line i) 0)
                       (return i)))))
    (values (make-array size
                        :element-type '(unsigned-byte 8)
                        :memory (+ #x8000000000 base))
            (make-array cmdl-len
                        :element-type 'base-char
                        :memory command-line))))

(defun multiboot-module-data (id)
  (multiple-value-bind (data command-line)
      (multiboot-module id)
    (declare (ignore command-line))
    data))

(defun multiboot-module-command-line (id)
  (multiple-value-bind (data command-line)
      (multiboot-module id)
    (declare (ignore data))
    command-line))

(defun mini-multiboot-module-stream (id)
  (cons (multiboot-module id) 0))

(defun %defconstant (name value &optional docstring)
  (proclaim `(special ,name))
  (setf (symbol-value name) value)
  (proclaim `(constant ,name))
  name)

(defun enter-debugger (condition)
  (write-char #\!)
  (write condition)
  (backtrace)
  (loop (%hlt)))

(defun initialize-lisp ()
  (setf *next-symbol-tls-slot* 12
        *array-types* #(t
                        base-char
                        character
                        bit
                        (unsigned-byte 2)
                        (unsigned-byte 4)
                        (unsigned-byte 8)
                        (unsigned-byte 16)
                        (unsigned-byte 32)
                        (unsigned-byte 64)
                        (signed-byte 1)
                        (signed-byte 2)
                        (signed-byte 4)
                        (signed-byte 8)
                        (signed-byte 16)
                        (signed-byte 32)
                        (signed-byte 64)
                        single-float
                        double-float
                        long-float
                        xmm-vector
                        (complex single-float)
                        (complex double-float)
                        (complex long-float))
        *package* nil
        *cold-stream* (make-cold-stream)
        *terminal-io* *cold-stream*
        *standard-output* *cold-stream*
        *standard-input* *cold-stream*
        *debug-io* *cold-stream*
        *screen-offset* 0
        *keyboard-shifted* nil
        *early-initialize-hook* '()
        *initialize-hook* '())
  (setf *print-base* 10.
        *print-escape* t
        *print-readably* nil
        *print-safe* t)
  (setf *features* '(:unicode :little-endian :x86-64 :lisp-os :ieee-floating-point :ansi-cl :common-lisp)
        *macroexpand-hook* 'funcall
        most-positive-fixnum #.(- (expt 2 60) 1)
        most-negative-fixnum #.(- (expt 2 60)))
  (setf (symbol-function 'intern) #'cold-intern)
  ;; Initialize defstruct and patch up all the structure types.
  (bootstrap-defstruct)
  (dotimes (i (length *initial-structure-obarray*))
    (setf (%struct-slot (svref *initial-structure-obarray* i) 0) *structure-type-type*))
  (write-line "Cold image coming up...")
  ;; Hook SETF symbols up.
  (dotimes (i (length *initial-setf-obarray*))
    (let* ((sym (svref *initial-setf-obarray* i))
           (other (find (symbol-name sym) *initial-obarray* :test 'string= :key 'symbol-name)))
      (assert other)
      (setf (get sym 'setf-symbol-backlink) other
            (get other 'setf-symbol) sym)))
  (dotimes (i (length *cold-toplevel-forms*))
    (eval (svref *cold-toplevel-forms* i)))
  (setf *initial-obarray* (make-array (length *initial-obarray*)
                                      :fill-pointer t :adjustable t
                                      :initial-contents *initial-obarray*))
  (setf *initial-keyword-obarray* (make-array (length *initial-keyword-obarray*)
                                              :fill-pointer t :adjustable t
                                              :initial-contents *initial-keyword-obarray*))
  (dotimes (i (length *initial-keyword-obarray*))
    (setf (symbol-package (aref *initial-keyword-obarray* i)) :keyword
          (symbol-mode (aref *initial-keyword-obarray* i)) :constant))
  (dolist (sym '(nil t most-positive-fixnum most-negative-fixnum))
    (setf (symbol-mode sym) :constant))
  ;; Pull in the real package system.
  ;; If anything goes wrong before init-package-sys finishes then things
  ;; break in terrible ways.
  (dotimes (i (length *package-system*))
    (eval (svref *package-system* i)))
  (initialize-package-system)
  (let ((*package* *package*))
    (dotimes (i (length *additional-cold-toplevel-forms*))
      (eval (svref *additional-cold-toplevel-forms* i))))
  (gc)
  (setf (fdefinition 'initialize-lisp) #'reinitialize-lisp)
  (reinitialize-lisp))

(defun reinitialize-lisp ()
  (init-isa-pic)
  (cold-stream-init)
  (gc-init-system-memory)
  (mapc 'funcall *early-initialize-hook*)
  (%sti)
  (pci-device-scan)
  (write-line "Hello, world.")
  (dotimes (i (multiboot-module-count))
    (format t "Loading module ~S.~%" (multiboot-module-command-line i))
    (mini-load-llf (mini-multiboot-module-stream i)))
  (mapc 'funcall *initialize-hook*)
  (terpri)
  (write-char #\*)
  (write-char #\O)
  (write-char #\K)
  (repl)
  (loop (%hlt)))
