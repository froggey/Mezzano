;;;; Bootstrap macros and functions for the cross-compiler.

(in-package :cross-support)

(defun make-hash-table (&rest args &key test size rehash-size rehash-threshold synchronized enforce-gc-invariant-keys weakness)
  (declare (ignore test size rehash-size rehash-threshold synchronized enforce-gc-invariant-keys weakness))
  (remf args :synchronized)
  (remf args :enforce-gc-invariant-keys)
  (apply #'cl:make-hash-table args))

(defvar *system-macros* (make-hash-table :test 'eq))
(defvar *system-compiler-macros* (make-hash-table :test 'equal))
(defvar *system-symbol-macros* (make-hash-table :test 'eq))
(defvar *system-symbol-declarations* (make-hash-table :test 'eq))
(defvar *structure-types* (make-hash-table :test 'eq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-&environment (orig-lambda-list)
    (do* ((lambda-list (copy-list orig-lambda-list))
          (prev nil i)
          (i lambda-list (cdr i)))
         ((null i) (values lambda-list nil))
      (when (eql (first i) '&environment)
        (assert (not (null (cdr i))) ()
                "Missing variable after &ENVIRONMENT.")
        (if prev
            (setf (cdr prev) (cddr i))
            (setf lambda-list (cddr i)))
        (assert (not (member '&environment lambda-list)) ()
                "Duplicate &ENVIRONMENT variable in lambda-list ~S." orig-lambda-list)
        (return (values lambda-list (second i)))))))

(cl:defmacro def-x-macro (name lambda-list &body body)
  (let ((whole))
    (multiple-value-bind (fixed-lambda-list env)
        (remove-&environment lambda-list)
      (when (null env)
        (setf env (gensym)))
      (if (eql (first fixed-lambda-list) '&whole)
          (setf whole (second fixed-lambda-list)
                fixed-lambda-list (cddr fixed-lambda-list))
          (setf whole (gensym)))
      `(setf (gethash ',name *system-macros*)
             (lambda (,whole ,env)
               (declare (ignorable ,whole ,env))
               (destructuring-bind ,fixed-lambda-list (cdr ,whole)
                 (block ,name ,@body)))))))

(setf (gethash 'cross-cl:defconstant *system-macros*)
      (cl:macro-function 'cross-cl:defconstant))

(def-x-macro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *package* (sys.int::find-package-or-die ',name))))

(defun sys.int::find-package-or-die (name)
  (or (find-package name) (error "Can't find package ~S." name)))

(def-x-macro defun (name lambda-list &body body)
  (let ((the-lambda `(lambda ,lambda-list
                       (declare (sys.int::lambda-name ,name))
                       ,@body)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (sys.int::%compiler-defun ',name ',the-lambda))
       (sys.int::%defun ',name ,the-lambda))))

(defvar *inline-modes* (make-hash-table :test #'equal))
(defvar *inline-forms* (make-hash-table :test #'equal))
(defvar *function-types* (make-hash-table :test #'equal))

(defun sys.int::%compiler-defun (name source-lambda)
  (when (or (gethash name *inline-modes*)
            (gethash name *inline-forms*))
      (setf (gethash name *inline-forms*) source-lambda))
  nil)

(defun cas-hash-table (key hash-table default old new)
  ;; This isn't a truely thread-safe implementation, but the cold
  ;; generator is single threaded anyway.
  (let ((existing (gethash key hash-table default)))
    (when (eql old existing)
      (setf (gethash key hash-table) new))
    existing))

(defun sys.int::set-variable-docstring (name docstring)
  (declare (ignore name docstring)))

(defun sys.int::set-setf-docstring (name docstring)
  (declare (ignore name docstring)))

(defun sys.int::set-type-docstring (name docstring)
  (declare (ignore name docstring)))

(defun sys.int::set-variable-source-location (name source-location &optional style)
  (declare (ignore name source-location style)))

(defvar *variable-types* (make-hash-table))

;; Enough to load the full DEFMACRO.
(def-x-macro defmacro (name lambda-list &body body)
  (multiple-value-bind (fixed-lambda-list supplied-env-sym)
      (remove-&environment lambda-list)
    (let ((env-sym (or supplied-env-sym (gensym "ENV")))
          (whole-sym))
      (cond ((eql (first fixed-lambda-list) '&whole)
             (assert (not (null (cdr fixed-lambda-list))) ()
                     "Missing variable after &WHOLE in macro lambda-list ~S."
                     lambda-list)
             (setf whole-sym (second fixed-lambda-list)
                   fixed-lambda-list (cddr fixed-lambda-list)))
            (t (setf whole-sym (gensym))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (sys.int::%defmacro ',name (lambda (,whole-sym ,env-sym)
                                      (declare (sys.int::lambda-name (defmacro ,name)))
                                      ,@(unless supplied-env-sym
                                          `((declare (ignore ,env-sym))))
                                      (destructuring-bind ,fixed-lambda-list (cdr ,whole-sym)
                                        (block ,name ,@body))))))))

(defun sys.int::%defmacro (name lambda &optional lambda-list)
  (declare (ignore lambda-list))
  (unless (cl:macro-function name)
    (setf (cl:macro-function name) lambda))
  (setf (gethash name *system-macros*) lambda))

(defun sys.int::%defconstant (name value source-location &optional docstring)
  (declare (ignore source-location docstring))
  (when (or (not (boundp name))
            (not (loose-constant-equal (symbol-value name) value)))
    (eval `(cl:defconstant ,name ',value))))

(def-x-macro declaim (&rest declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (x) `(sys.int::proclaim ',x)) declaration-specifiers)))

(defun proclaim-symbol-mode (sym mode)
  (check-type sym symbol)
  (when (not (or (null (sys.int::symbol-mode sym))
                 (eql (sys.int::symbol-mode sym) mode)))
    (cerror "Continue" "Symbol ~S being changed from ~S to ~S."
            sym (sys.int::symbol-mode sym) mode))
  (setf (gethash sym *system-symbol-declarations*) mode))

;; SYS.INT shadows some CL symbols in the cross-environment. When loaded in the true
;; system, the correct symbol will be used.
(defun sys.int::proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (sys.int::constant
     (dolist (sym (rest declaration-specifier))
       (proclaim-symbol-mode sym :constant)))
    (special
     (dolist (sym (rest declaration-specifier))
       (proclaim-symbol-mode sym :special)
       (unless (eql (symbol-package sym) (find-package "CL"))
         (cl:proclaim `(special ,sym)))))
    (sys.int::global
     (dolist (sym (rest declaration-specifier))
       (proclaim-symbol-mode sym :global)
       (unless (eql (symbol-package sym) (find-package "CL"))
         (cl:proclaim `(special ,sym)))))
    (inline
     (dolist (name (rest declaration-specifier))
       (setf (gethash name *inline-modes*) t)))
    (notinline
     (dolist (name (rest declaration-specifier))
       (setf (gethash name *inline-modes*) nil)))
    (sys.int::maybe-inline
     (dolist (name (rest declaration-specifier))
       (setf (gethash name *inline-modes*) :maybe)))
    (type
     (destructuring-bind (typespec &rest vars)
         (rest declaration-specifier)
       (dolist (name vars)
         (setf (gethash name *variable-types*) typespec))))
    (ftype
     (destructuring-bind (typespec &rest vars)
         (rest declaration-specifier)
       (dolist (name vars)
         (setf (gethash name *function-types*) typespec))))
    (t (warn "Unknown declaration ~S" (first declaration-specifier)))))

(defun sys.int::symbol-mode (symbol)
  (check-type symbol symbol)
  (sys.int::variable-information symbol))

(defun mezzano.runtime::symbol-type (symbol)
  (check-type symbol symbol)
  (values (gethash symbol *variable-types* 't)))

(defun sys.int::dotted-list-length (list)
  "Returns the length of LIST if list is a proper list. Returns NIL if LIST is a circular list."
  ;; Implementation from the HyperSpec
  (do ((n 0 (+ n 2))             ; Counter.
       (fast list (cddr fast))   ; Fast pointer: leaps by 2.
       (slow list (cdr slow)))   ; Slow pointer: leaps by 1.
      (nil)
    ;; If fast pointer hits the end, return the count.
    (when (atom fast) (return n))
    (when (atom (cdr fast)) (return (1+ n)))
    ;; If fast pointer eventually equals slow pointer,
    ;;  then we must be stuck in a circular list.
    (when (and (eq fast slow) (> n 0)) (return nil))))

(defun sys.int::concat-symbols (&rest symbols)
  (intern (apply 'concatenate 'string (mapcar 'string symbols))))

(defstruct cross-struct
  type
  data)

(defun sys.int::%defstruct (def &key location)
  (declare (ignore location))
  (when (gethash (sys.int::structure-definition-name def) *structure-types*)
    ;; FIXME: Check compatibility here.
    (return-from sys.int::%defstruct def))
  (unless (member (sys.int::structure-definition-name def)
                  '(sys.int::structure-definition
                    sys.int::structure-slot-definition
                    sys.int::layout))
    (let ((predicate (gensym (string (sys.int::structure-definition-name def)))))
      (setf (symbol-function predicate) (lambda (x)
                                          (sys.int::structure-type-p x def)))
      (unless (or (eql (symbol-package (sys.int::structure-definition-name def))
                       (find-package "CL"))
                  (eql (symbol-package (sys.int::structure-definition-name def))
                       (find-package "MEZZANO.COMPILER")))
        (eval `(cl:deftype ,(sys.int::structure-definition-name def) () '(satisfies ,predicate))))))
  (setf (gethash (sys.int::structure-definition-name def) *structure-types*) def))

(defun sys.int::%allocate-struct (definition)
  (when (symbolp definition)
    (setf definition (sys.int::get-structure-type definition)))
  (make-cross-struct
   :type definition
   :data (make-array (length (sys.int::structure-definition-slots definition)))))

(defun sys.int::structure-slot-index (def slot-name)
  (position slot-name
            (sys.int::structure-definition-slots (sys.int::get-structure-type def))
            :key #'sys.int::structure-slot-definition-name))

(defun sys.int::%struct-slot (struct def slot-name)
  (aref (cross-struct-data struct) (sys.int::structure-slot-index def slot-name)))
(defun (setf sys.int::%struct-slot) (value struct def slot-name)
  (setf (aref (cross-struct-data struct) (sys.int::structure-slot-index def slot-name)) value))

(defun sys.int::get-structure-type (name &optional (errorp t))
  (or (gethash name *structure-types*)
      (and errorp
           (error "Unknown structure type ~S." name))))

(defun sys.int::structure-type-p (object struct-type)
  (when (cross-struct-p object)
    (do ((object-type (cross-struct-type object)
                      (sys.int::structure-definition-parent object-type)))
        ((not (sys.int::structure-definition-p object-type))
         nil)
      ;; Work by name as defstruct isn't unifying struct definitions any more.
      (when (eq (sys.int::structure-definition-name object-type)
                (sys.int::structure-definition-name struct-type))
        (return t)))))

(defconstant sys.int::most-positive-fixnum (- (expt 2 62) 1))
(defconstant sys.int::most-negative-fixnum (- (expt 2 62)))
(alexandria:define-constant sys.int::lambda-list-keywords
    '(&allow-other-keys &aux &body &environment &key &optional &rest &whole sys.int::&closure)
  :test 'equal)
(defvar sys.int::*features* '(:unicode :little-endian :mezzano :ieee-floating-point :ansi-cl :common-lisp))

;; Replicated from system/package.lisp. Needed to define packages in package.lisp
(in-package :mezzano.internals)
(defmacro defpackage (defined-package-name &rest options)
  (let ((nicknames '())
        (documentation nil)
        (use-list '())
        (import-list '())
        (export-list '())
        (intern-list '())
        (shadow-list '())
        (shadow-import-list '())
        (local-nicknames '()))
    (dolist (o options)
      (ecase (first o)
        (:nicknames
         (dolist (n (rest o))
           (pushnew (string n) nicknames)))
        (:documentation
         (when documentation
           (error "Multiple documentation options in DEFPACKAGE form."))
         (unless (or (eql 2 (length o))
                     (not (stringp (second o))))
           (error "Invalid documentation option in DEFPACKAGE form."))
         (setf documentation (second o)))
        (:use
         (dolist (u (rest o))
           (if (packagep u)
               (pushnew u use-list)
               (pushnew (string u) use-list))))
        (:import-from
         (let ((package (find-package-or-die (second o))))
           (dolist (name (cddr o))
             (multiple-value-bind (symbol status)
                 (find-symbol (string name) package)
               (unless status
                 (error "No such symbol ~S in package ~S." (string name) package))
               (pushnew symbol import-list)))))
        (:export
         (dolist (name (cdr o))
           (pushnew name export-list)))
        (:intern
         (dolist (name (cdr o))
           (pushnew name intern-list)))
        (:shadow
         (dolist (name (cdr o))
           (pushnew name shadow-list)))
        (:shadowing-import-from
         (let ((package (find-package-or-die (second o))))
           (dolist (name (cddr o))
             (multiple-value-bind (symbol status)
                 (find-symbol (string name) package)
               (unless status
                 (error "No such symbol ~S in package ~S." (string name) package))
               (pushnew symbol shadow-import-list)))))
        (:size)
        (:local-nicknames
         (setf local-nicknames (append local-nicknames (rest o))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%defpackage ,(string defined-package-name)
                    :nicknames ',nicknames
                    :documentation ',documentation
                    :uses ',use-list
                    :imports ',import-list
                    :exports ',export-list
                    :interns ',intern-list
                    :shadows ',shadow-list
                    :shadowing-imports ',shadow-import-list
                    :local-nicknames ',local-nicknames))))

(in-package :cross-support)

(defun sys.int::%defpackage (name &key
                                    nicknames
                                    documentation
                                    ((:uses use-list))
                                    ((:imports import-list))
                                    ((:exports export-list))
                                    ((:interns intern-list))
                                    ((:shadows shadow-list))
                                    ((:shadowing-imports shadowing-import-list))
                                    ((:local-nicknames package-local-nicknames)))
  (eval `(cl:defpackage ,name
           (:nicknames ,@nicknames)
           ,@(mapcar (lambda (symbol)
                       `(:import-from ,(package-name (symbol-package symbol)) ,symbol))
                     import-list)
           (:export ,@export-list)
           (:intern ,@intern-list)
           (:shadow ,@shadow-list)
           ,@(mapcar (lambda (symbol)
                       `(:shadowing-import-from ,(package-name (symbol-package symbol)) ,symbol))
                     shadowing-import-list)
           ,@(when documentation
               `((:documentation ,documentation)))
           (:use ,@(mapcar (lambda (package)
                             (if (eql (find-package package)
                                      (find-package :cl))
                                 :cross-cl
                                 package))
                           use-list))
           (:local-nicknames ,@(loop
                                  for (nickname real-name) in package-local-nicknames
                                    collect (list nickname (if (eql (find-package real-name) (find-package :cl))
                                                               :cross-cl
                                                               real-name)))))))

(defun sys.int::round-up (n boundary)
  (if (zerop (rem n boundary))
      n
      (+ n boundary (- (rem n boundary)))))

(defun sys.int::%integer-as-single-float (integer)
  (check-type integer (unsigned-byte 32))
  #+sbcl (sb-kernel:make-single-float (if (logtest integer #x80000000)
                                          (logior integer (lognot #x7FFFFFFF))
                                          integer))
  #-sbcl (error "Not supported on this platform."))

(defun sys.int::%integer-as-double-float (integer)
  (check-type integer (unsigned-byte 64))
  #+sbcl
  (let ((lo (ldb (byte 32 0) integer))
        (hi (ldb (byte 32 32) integer)))
    (sb-kernel:make-double-float
     (if (logtest hi #x80000000)
         (logior hi (lognot #x7FFFFFFF))
         hi)
     lo))
  #-sbcl (error "Not supported on this platform."))

(defun sys.int::binary-= (x y) (= x y))
(defun sys.int::binary-< (x y) (< x y))
(defun sys.int::binary-<= (x y) (<= x y))
(defun sys.int::binary-> (x y) (> x y))
(defun sys.int::binary->= (x y) (>= x y))
(defun sys.int::binary-+ (x y) (+ x y))
(defun sys.int::binary-- (x y) (- x y))
(defun sys.int::binary-* (x y) (* x y))
(defun sys.int::binary-logand (x y) (logand x y))
(defun sys.int::binary-logeqv (x y) (logeqv x y))
(defun sys.int::binary-logior (x y) (logior x y))
(defun sys.int::binary-logxor (x y) (logxor x y))
(defun mezzano.runtime::%fixnum-< (x y) (< x y))

(defun convert-internal-time-units (time)
  (* time
     (/ internal-time-units-per-second
        cl:internal-time-units-per-second)))

(defun get-internal-run-time ()
  (convert-internal-time-units
   (cl:get-internal-run-time)))

(defun get-internal-real-time ()
  (convert-internal-time-units
   (cl:get-internal-real-time)))

(defun sys.int::%type-check (object tag type)
  (declare (ignore tag))
  (when (not (typep object type))
    (error "Type error: ~S not of type ~S." object type)))

(defun sys.int::latin1-char-p (character)
  (check-type character character)
  (< (char-code character) 256))

(defun sys.int::frob-stream (stream default)
  (cond ((eql stream 'nil)
         default)
        ((eql stream 't)
         *terminal-io*)
        (t
         ;; TODO: check that the stream is open.
         (check-type stream stream)
         stream)))

(defun sys.int::frob-input-stream (stream)
  (sys.int::frob-stream stream *standard-input*))

(defun sys.int::frob-output-stream (stream)
  (sys.int::frob-stream stream *standard-output*))

(defmacro sys.int::with-stream-editor ((stream recursive-p) &body body)
  (declare (ignore stream recursive-p))
  `(progn ,@body))

(defun sys.int::typeexpand (type &optional environment)
  (declare (ignore environment))
  type)
