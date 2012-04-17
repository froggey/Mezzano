(defpackage #:genesis
  (:use :cl :iterate))

(in-package #:genesis)

(defun read-s-expression (filespec)
  (with-open-file (s filespec)
    (read s)))

(defparameter *cl-symbol-names* (read-s-expression "~/Documents/LispOS/cl-symbols.lisp-expr"))
(defparameter *system-symbol-names* (read-s-expression "~/Documents/LispOS/system-symbols.lisp-expr"))

(defparameter *interned-symbols* (make-hash-table :test 'equal)
  "A hash-table of all non-keyword symbols that have been created
via GENESIS-INTERN.")
(declaim (type hash-table *interned-symbols*))
(setf (gethash "NIL" *interned-symbols*) 'nil)

(defparameter *interned-keywords* (make-hash-table :test 'equal)
  "A hash-table of all keyword symbols that have been created via
GENESIS-INTERN.")
(declaim (type hash-table *interned-keywords*))

(defparameter *use-bootstrap-package-system* t)

(defun genesis-intern (name &optional keywordp)
  (cond (keywordp
         (or (gethash name *interned-keywords*)
             (if *use-bootstrap-package-system*
                 (let ((sym (make-symbol name)))
                   (setf (gethash name *interned-keywords*) sym)
                   (setf (symbol-value sym) sym)
                   (setf (get sym :genesis-symbol-mode) :constant)
                   sym)
                 ;; Call in to the system.
                 (funcall (genesis-intern "INTERN") name "KEYWORD"))))
        ((string= name "NIL") nil)
        (t (or (gethash name *interned-symbols*)
               (if *use-bootstrap-package-system*
                   (let ((sym (make-symbol name)))
                     (setf (gethash name *interned-symbols*) sym)
                     sym)
                   ;; Call in to the system.
                   (funcall (genesis-intern "INTERN") name "SYS.INT"))))))

(defun genesis-symbol-constant-p (symbol)
  (or (eql symbol 'nil)
      (eql (get symbol :genesis-symbol-mode) :constant)))

(defun genesis-symbol-special-p (symbol)
  (or (eql symbol 'nil)
      (eql (get symbol :genesis-symbol-mode) :special)
      (eql (get symbol :genesis-symbol-mode) :constant)))

(defun genesis-macro-function (symbol &optional env)
  (declare (type symbol symbol)
           (type list env))
  (dolist (e env)
    (unless (consp e)
      (error "Funny environment..."))
    (when (eql (first e) :macros)
      (let ((x (assoc symbol (rest e))))
        (when x
          (return-from genesis-macro-function (cdr x)))))
    (when (eql (first e) (genesis-intern "MACROS" t))
      (let ((x (assoc symbol (rest e))))
        (when x
          (return-from genesis-macro-function (cdr x))))))
  (getf (genesis-symbol-plist symbol) (genesis-intern "%MACRO-FUNCTION")))

(defun genesis-symbol-package (symbol)
  (get symbol 'genesis-symbol-package))

(defun (setf genesis-symbol-package) (value symbol)
  (setf (get symbol 'genesis-symbol-package) value))

(defun genesis-symbol-plist (symbol)
  (get symbol 'genesis-symbol-plist))

(defun (setf genesis-symbol-plist) (value symbol)
  (setf (get symbol 'genesis-symbol-plist) value))

(defun symbol-setf-function (name)
  "Return the symbol coresponding to NAME's SETF function."
  (or (getf (genesis-symbol-plist name) (genesis-intern "SETF-SYMBOL"))
      (setf (getf (genesis-symbol-plist name) (genesis-intern "SETF-SYMBOL"))
            (make-symbol (format nil "(SETF ~A)" name)))))

(defun resolve-function-name (name)
  (cond ((symbolp name)
         name)
        ((and (= (length name) 2)
              (eql (first name) (genesis-intern "SETF"))
              (symbolp (second name)))
         (symbol-setf-function (second name)))
        (t (error "Invalid function name ~S." name))))

(defun genesis-fdefinition (name)
  (symbol-function (resolve-function-name name)))

(defun (setf genesis-fdefinition) (value name)
  (let ((sym (resolve-function-name name)))
    (setf (getf (genesis-symbol-plist sym) (genesis-intern "%MACRO-FUNCTION")) nil
          (symbol-function sym) value)))

(defun early-load (pathspec)
  (format t "; Loading file ~S:~%" pathspec)
  (with-open-file (s pathspec)
    (do ((form (primitive-read s nil s)
	       (primitive-read s nil s)))
	((eql form s) t)
      (format t "; Loading ~S~%" form)
      (genesis-eval form))))

(defmacro defbuiltin (name lambda-list &body body)
  (let ((other-name (cond ((symbolp name) (make-symbol (symbol-name name)))
                          (t (list (first name) (make-symbol (symbol-name (second name))))))))
    `(progn (setf (genesis-fdefinition ,(cond ((symbolp name)
                                               `(genesis-intern ',(symbol-name name)))
                                              ((and (= (length name) 2)
                                                    (eql (first name) 'setf)
                                                    (symbolp (second name)))
                                               `(list (genesis-intern "SETF")
                                                      (genesis-intern ,(symbol-name (second name)))))
                                              (t (error "Bad built-in name ~S." name))))
                  (flet ((,other-name ,lambda-list ,@body))
                    #',other-name))
            ',name)))

;;; Forward a genesis function to a built-in function.
(defmacro define-forwarding-builtin (name &optional host-name (should-inline t))
  (let ((genesis-name (cond ((symbolp name)
                             `(genesis-intern ,(symbol-name name)))
                            ((and (= (length name) 2)
                                  (eql (first name) 'setf)
                                  (symbolp (second name)))
                             `(list (genesis-intern "SETF")
                                    (genesis-intern ,(symbol-name (second name)))))
                            (t (error "Bad built-in name ~S." name)))))
    ;; Inlining slows stuff down for some reason. Tell translate to replace calls
    ;; instead.
    `(progn
       ,(when should-inline
          `(setf (get (resolve-function-name ,genesis-name) 'genesis-replace-with)
                 ',(or host-name name)))
       (eval `(defun ,(resolve-function-name ,genesis-name) (&rest args)
                (declare (dynamic-extent args))
                (declare (optimize speed))
                (apply #',',(or host-name name) args))))))

(setf (symbol-value (genesis-intern "T")) (genesis-intern "T")
      (get (genesis-intern "T") :genesis-symbol-mode) :special)
(setf (symbol-value (genesis-intern "*FEATURES*"))
      (list (genesis-intern "X86-64" t)
            (genesis-intern "LISP-OS" t)
            (genesis-intern "IEEE-FLOATING-POINT" t)
            (genesis-intern "ANSI-CL" t)
            (genesis-intern "COMMON-LISP" t))
      (get (genesis-intern "*FEATURES*") :genesis-symbol-mode) :special)
(setf (symbol-value (genesis-intern "*MACROEXPAND-HOOK*"))
      (genesis-intern "FUNCALL")
      (get (genesis-intern "*MACROEXPAND-HOOK*") :genesis-symbol-mode) :special)
(setf (symbol-value (genesis-intern "MOST-NEGATIVE-FIXNUM")) (- (expt 2 60))
      (get (genesis-intern "MOST-NEGATIVE-FIXNUM") :genesis-symbol-mode) :constant)
(setf (symbol-value (genesis-intern "MOST-POSITIVE-FIXNUM")) (- (expt 2 60) 1)
      (get (genesis-intern "MOST-POSITIVE-FIXNUM") :genesis-symbol-mode) :constant)

(defbuiltin /show0 (control &rest arguments)
  (apply 'format t control arguments))

(define-forwarding-builtin macro-function genesis-macro-function)

(defun make-macro-error-function (symbol)
  (let ((rest-sym (gensym "REST")))
    (genesis-eval (list (genesis-intern "FUNCTION")
                        (list (genesis-intern "LAMBDA")
                              (list (genesis-intern "&REST")
                                    rest-sym)
                              (list (genesis-intern "DECLARE")
                                    (list (genesis-intern "IGNORE") rest-sym))
                              (list (genesis-intern "ERROR")
                                    (list (genesis-intern "QUOTE")
                                          (genesis-intern "UNDEFINED-FUNCTION"))
                                    (genesis-intern "NAME" t)
                                    (list (genesis-intern "QUOTE")
                                          symbol)))))))

(defbuiltin (setf macro-function) (value symbol &optional env)
  (when env
    (error "TODO: (setf macro-function) with environment."))
  (cond (value
         (setf (getf (genesis-symbol-plist symbol) (genesis-intern "%MACRO-FUNCTION")) value
               (symbol-function symbol) (make-macro-error-function symbol)))
        (t (setf (getf (genesis-symbol-plist symbol) (genesis-intern "%MACRO-FUNCTION")) nil)
           (fmakunbound symbol)
           nil)))

(define-forwarding-builtin funcall)
(define-forwarding-builtin apply)
(define-forwarding-builtin eq)
(define-forwarding-builtin eql)
(define-forwarding-builtin null)
(define-forwarding-builtin not)
(define-forwarding-builtin values)
(define-forwarding-builtin values-list)
(define-forwarding-builtin eval genesis-eval nil)
(define-forwarding-builtin error error nil)
(define-forwarding-builtin functionp)

(define-forwarding-builtin car)
(define-forwarding-builtin (setf car))
(define-forwarding-builtin cdr)
(define-forwarding-builtin (setf cdr))
(define-forwarding-builtin cons)
(define-forwarding-builtin consp)
(define-forwarding-builtin list)
(define-forwarding-builtin list*)
(define-forwarding-builtin copy-list)
(define-forwarding-builtin endp)

(define-forwarding-builtin fdefinition genesis-fdefinition nil)
(define-forwarding-builtin (setf fdefinition) (setf genesis-fdefinition) nil)

(define-forwarding-builtin gensym gensym nil)
(defbuiltin make-symbol (name)
  (make-symbol (crunch-string name)))
(define-forwarding-builtin symbolp)
(define-forwarding-builtin boundp)
(define-forwarding-builtin fboundp)
(define-forwarding-builtin makunbound)
(define-forwarding-builtin fmakunbound)

(define-forwarding-builtin symbol-name)
(define-forwarding-builtin symbol-package genesis-symbol-package)
(define-forwarding-builtin (setf symbol-package) (setf genesis-symbol-package))
(define-forwarding-builtin symbol-value)
(define-forwarding-builtin (setf symbol-value))
(define-forwarding-builtin symbol-function)
(defbuiltin (setf symbol-function) (value symbol)
  (setf (get symbol :genesis-macro-function) nil
         (symbol-function symbol) value))
(define-forwarding-builtin symbol-plist genesis-symbol-plist)
(define-forwarding-builtin (setf symbol-plist) (setf genesis-symbol-plist))

(define-forwarding-builtin load early-load nil)

(defbuiltin proclaim (form)
  (cond ((eql (first form) (genesis-intern "SPECIAL"))
         (dolist (symbol (rest form))
           (setf (get symbol :genesis-symbol-mode) :special)))
        ((eql (first form) (genesis-intern "CONSTANT"))
         (dolist (symbol (rest form))
           (setf (get symbol :genesis-symbol-mode) :constant)))
        ((eql (first form) (genesis-intern "INLINE"))
         (dolist (symbol (rest form))
           (setf (getf (genesis-symbol-plist symbol) (genesis-intern "INLINE-MODE"))
                 (genesis-intern "T"))))
        ((eql (first form) (genesis-intern "NOTINLINE"))
         (dolist (symbol (rest form))
           (setf (getf (genesis-symbol-plist symbol) (genesis-intern "INLINE-MODE"))
                 nil)))))

(define-forwarding-builtin 1+)
(define-forwarding-builtin 1-)
(define-forwarding-builtin +)
(define-forwarding-builtin binary-+ +)
(define-forwarding-builtin -)
(define-forwarding-builtin binary-- -)
(define-forwarding-builtin *)
(define-forwarding-builtin binary-* *)
(define-forwarding-builtin /)
(define-forwarding-builtin binary-/ /)
(define-forwarding-builtin rem)
(define-forwarding-builtin ceiling)
(define-forwarding-builtin floor)
(define-forwarding-builtin truncate)
(define-forwarding-builtin byte)
(define-forwarding-builtin ldb)
(define-forwarding-builtin dpb)
(define-forwarding-builtin ash)
(define-forwarding-builtin logand)
(define-forwarding-builtin binary-logand logand)
(define-forwarding-builtin logior)
(define-forwarding-builtin binary-logior logior)
(define-forwarding-builtin logxor)
(define-forwarding-builtin binary-logxor logxor)
(define-forwarding-builtin lognot)
(define-forwarding-builtin expt)
(define-forwarding-builtin zerop)
(define-forwarding-builtin plusp)
(define-forwarding-builtin minusp)
(define-forwarding-builtin evenp)
(define-forwarding-builtin oddp)
(define-forwarding-builtin min)
(define-forwarding-builtin max)
(define-forwarding-builtin <)
(define-forwarding-builtin binary-< <)
(define-forwarding-builtin <=)
(define-forwarding-builtin binary-<= <=)
(define-forwarding-builtin >)
(define-forwarding-builtin binary-> >)
(define-forwarding-builtin >=)
(define-forwarding-builtin binary->= >=)
(define-forwarding-builtin =)
(define-forwarding-builtin binary-= =)
(define-forwarding-builtin /=)
(defbuiltin fixnump (object)
  (typep object '(signed-byte 61)))
(define-forwarding-builtin integerp)
(define-forwarding-builtin realp)
(define-forwarding-builtin numberp)

(defstruct genesis-struct
  slots)

(defbuiltin %make-struct (size &optional area)
  (unless (null area)
    (error "Genesis cannot allocate in a specific area."))
  (make-genesis-struct :slots (make-array size)))
(define-forwarding-builtin structure-object-p genesis-struct-p)
(defbuiltin %struct-slot (struct slot)
  (aref (genesis-struct-slots struct) slot))
(defbuiltin (setf %struct-slot) (value struct slot)
  (setf (aref (genesis-struct-slots struct) slot) value))

;;; Concat-symbols must intern in *PACKAGE*, genesis-intern always uses SYS.INT.
(defbuiltin concat-symbols (&rest symbols)
  (let ((name (apply 'concatenate 'string (mapcar 'string symbols))))
    (if *use-bootstrap-package-system*
        (genesis-intern name)
        (genesis-eval (list (genesis-intern "INTERN") name)))))

(define-forwarding-builtin string string nil)
(define-forwarding-builtin stringp stringp nil)
(define-forwarding-builtin string= string= nil)
(define-forwarding-builtin char= char= nil)
(define-forwarding-builtin characterp)

(define-forwarding-builtin char-code)
(define-forwarding-builtin code-char)
;; Avoid spreading any host-specific attributes about.
(define-forwarding-builtin char-int char-code)
(defbuiltin char-bits (char)
  (declare (ignore char))
  0)

(defbuiltin name-char (name)
  (name-char (crunch-string name)))

(define-forwarding-builtin schar char)
(define-forwarding-builtin (setf schar) (setf char))
(define-forwarding-builtin char-upcase char-upcase nil)
(define-forwarding-builtin char-downcase char-downcase nil)
(define-forwarding-builtin upper-case-p upper-case-p nil)
(define-forwarding-builtin lower-case-p lower-case-p nil)

(defun crunch-string (string)
  "Convert a string to a simple-string or a simple-base-string."
  (if (array-header-p string)
      (crunch-string (subseq (array-header-storage string)
                             0
                             (or (array-header-fill-pointer string)
                                 (length (array-header-storage string)))))
      (make-array (length string)
                  :element-type (if (every #'standard-char-p string)
                                    'standard-char
                                    'character)
                  :initial-contents string)))

(defbuiltin simplify-string (string)
  (crunch-string string))

;;; The big function for switching over to the full package system.
(defbuiltin jettison-bootstrap-package-system ()
  ;; Ensure that the CL and SYS symbols exist.
  (mapc 'genesis-intern *cl-symbol-names*)
  (mapc 'genesis-intern *system-symbol-names*)
  (format t "Jettisoning bootstrap package system... ")
  ;; Prevent GENESIS-INTERN from creating new symbols.
  (setf *use-bootstrap-package-system* nil)
  (let ((import-fn (genesis-intern "IMPORT"))
	(export-fn (genesis-intern "EXPORT"))
	(sys-int-syms '())
	(sys-syms '())
	(cl-syms '())
	(key-syms '()))
    ;; Figure out the correct package for each symbol and
    ;; clear the package slot.
    (maphash (lambda (name sym)
               (setf (genesis-symbol-package sym) nil)
               (cond ((member name *cl-symbol-names* :test #'string=)
                      (push sym cl-syms))
                     ((member name *system-symbol-names* :test #'string=)
                      (push sym sys-syms))
                     (t (push sym sys-int-syms))))
             *interned-symbols*)
    ;; Now import symbols into the packages.
    (funcall import-fn cl-syms "COMMON-LISP")
    (funcall import-fn sys-syms "SYSTEM")
    (funcall import-fn sys-int-syms "SYSTEM.INTERNALS")
    ;; Export symbols from just CL & SYS.
    (funcall export-fn cl-syms "COMMON-LISP")
    (funcall export-fn sys-syms "SYSTEM")
    ;; Do the same for keywords.
    (maphash (lambda (name sym)
               (declare (ignore name))
               (setf (genesis-symbol-package sym) nil)
               (push sym key-syms))
             *interned-keywords*)
    (funcall import-fn key-syms "KEYWORD")
    (funcall export-fn key-syms "KEYWORD")
    (format t "Done.~%")))

(defstruct array-header
  dimensions
  fill-pointer
  info
  storage)

(define-forwarding-builtin %array-header-p array-header-p)
(defbuiltin %make-array-header (dimensions fill-pointer info storage &optional area)
  (unless (null area)
    (error "Genesis cannot allocate in a specific area."))
  (make-array-header :dimensions dimensions
                     :fill-pointer fill-pointer
                     :info info
                     :storage storage))
(define-forwarding-builtin %array-header-dimensions array-header-dimensions)
(define-forwarding-builtin (setf %array-header-dimensions) (setf array-header-dimensions))
(define-forwarding-builtin %array-header-fill-pointer array-header-fill-pointer)
(define-forwarding-builtin (setf %array-header-fill-pointer) (setf array-header-fill-pointer))
(define-forwarding-builtin %array-header-info array-header-info)
(define-forwarding-builtin (setf %array-header-info) (setf array-header-info))
(define-forwarding-builtin %array-header-storage array-header-storage)
(define-forwarding-builtin (setf %array-header-storage) (setf array-header-storage))

(define-forwarding-builtin %simple-array-p vectorp)
(define-forwarding-builtin %simple-array-length length)
(define-forwarding-builtin simple-string-p stringp)
(defbuiltin %simple-array-aref (array index)
  (aref array index))
(defbuiltin (setf %simple-array-aref) (value array index)
  (setf (aref array index) value))
(define-forwarding-builtin simple-vector-p)
(define-forwarding-builtin svref)
(define-forwarding-builtin (setf svref))

(defparameter *array-element-types*
  `((bit . ,(genesis-intern "BIT"))
    ((unsigned-byte 2) . (,(genesis-intern "UNSIGNED-BYTE") 2))
    ((unsigned-byte 4) . (,(genesis-intern "UNSIGNED-BYTE") 4))
    ((unsigned-byte 8) . (,(genesis-intern "UNSIGNED-BYTE") 8))
    ((unsigned-byte 16) . (,(genesis-intern "UNSIGNED-BYTE") 16))
    ((unsigned-byte 32) . (,(genesis-intern "UNSIGNED-BYTE") 32))
    ((unsigned-byte 64) . (,(genesis-intern "UNSIGNED-BYTE") 64))
    ((signed-byte 1) . (,(genesis-intern "SIGNED-BYTE") 1))
    ((signed-byte 2) . (,(genesis-intern "SIGNED-BYTE") 2))
    ((signed-byte 4) . (,(genesis-intern "SIGNED-BYTE") 4))
    ((signed-byte 8) . (,(genesis-intern "SIGNED-BYTE") 8))
    ((signed-byte 16) . (,(genesis-intern "SIGNED-BYTE") 16))
    ((signed-byte 32) . (,(genesis-intern "SIGNED-BYTE") 32))
    ((signed-byte 64) . (,(genesis-intern "SIGNED-BYTE") 64))
    (base-char . ,(genesis-intern "BASE-CHAR"))
    (character . ,(genesis-intern "CHARACTER"))
    (single-float . ,(genesis-intern "SINGLE-FLOAT"))
    (double-float . ,(genesis-intern "DOUBLE-FLOAT"))
    (t . (genesis-intern "T"))))

(defbuiltin %simple-array-element-type (array)
  (let ((element-type (array-element-type array)))
    (dolist (x *array-element-types* (error "Impossible"))
      (when (subtypep element-type (car x))
        (return (cdr x))))))

(defun convert-element-type (element-type)
  "Convert an element-type to a host element-type (recreating lists & symbols)."
  (cond ((eql element-type (genesis-intern "T")) 't)
	((eql element-type (genesis-intern "BIT")) 'bit)
	((eql element-type (genesis-intern "BASE-CHAR")) 'base-char)
	((eql element-type (genesis-intern "CHARACTER")) 'character)
	((eql element-type (genesis-intern "SINGLE-FLOAT")) 'single-float)
	((eql element-type (genesis-intern "DOUBLE-FLOAT")) 'double-float)
	((eql element-type (genesis-intern "LONG-FLOAT")) 'long-float)
	((and (listp element-type)
              (= (length element-type) 2)
	      (eql (first element-type) (genesis-intern "UNSIGNED-BYTE"))
              (integerp (second element-type)))
	 (list 'unsigned-byte (second element-type)))
        ((and (listp element-type)
              (= (length element-type) 2)
	      (eql (first element-type) (genesis-intern "SIGNED-BYTE"))
              (integerp (second element-type)))
	 (list 'signed-byte (second element-type)))
	(t (error "Unknown element type ~S." element-type))))

(defbuiltin %allocate-and-fill-array (length element-type initial-element &optional area)
  (unless (null area)
    (error "Genesis cannot allocate in a specific area."))
  (make-array length :element-type (convert-element-type element-type) :initial-element initial-element))
(defbuiltin %allocate-and-clear-array (length element-type &optional area)
  (unless (null area)
    (error "Genesis cannot allocate in a specific area."))
  (make-array length :element-type (convert-element-type element-type)))

(defbuiltin open (pathspec)
  (open pathspec))
(defbuiltin close (stream)
  (close stream))
(define-forwarding-builtin streamp streamp nil)
(define-forwarding-builtin read-char read-char nil)
(defbuiltin peek-char (&optional peek-type (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (peek-char (cond ((eql peek-type (genesis-intern "T")) t)
                   (t peek-type))
             stream
             eof-error-p
             eof-value
             recursive-p))
(define-forwarding-builtin unread-char unread-char nil)
(defbuiltin format (stream control &rest arguments)
  (apply 'format
         (if (eql stream (genesis-intern "T"))
             't
             stream)
         control
         arguments))

(defstruct (genesis-std-instance
             (:constructor make-genesis-std-instance (class slots)))
  class
  slots)

(defmethod print-object ((object genesis-std-instance) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(define-forwarding-builtin allocate-std-instance make-genesis-std-instance)
(define-forwarding-builtin std-instance-p genesis-std-instance-p)
(define-forwarding-builtin std-instance-class genesis-std-instance-class)
(define-forwarding-builtin (setf std-instance-class) (setf genesis-std-instance-class))
(define-forwarding-builtin std-instance-slots genesis-std-instance-slots)
(define-forwarding-builtin (setf std-instance-slots) (setf genesis-std-instance-slots))

(defparameter *object-genesis-addresses* (make-hash-table :weakness :key))
(defparameter *next-object-address* 0)
(declaim (type fixnum *next-object-address*))

(defbuiltin lisp-object-address (object)
  (typecase object
    ((signed-byte 61) (ldb (byte 60 0) object))
    (character (char-code object))
    (t (or (gethash object *object-genesis-addresses*)
           (setf (gethash object *object-genesis-addresses*) (incf *next-object-address*))))))

(defbuiltin (setf compiler-macro-function) (value name &optional environment)
  (when environment
    (error "TODO: setf compiler-macro-function with environment."))
  (setf (getf (genesis-symbol-plist name) (genesis-intern "COMPILER-MACRO-FUNCTION"))
        value))

(defbuiltin #:variable-information (symbol &optional env)
  (declare (ignore env))
  (if (eql symbol 'nil)
      (genesis-intern "CONSTANT" t)
      (ecase (get symbol :genesis-symbol-mode)
        ((nil) nil)
        ((:special) (genesis-intern "SPECIAL" t))
        ((:constant) (genesis-intern "CONSTANT" t))
        ((:symbol-macro) (genesis-intern "SYMBOL-MACRO" t)))))

(defbuiltin #:symbol-macro-function (symbol &optional env)
  nil)
