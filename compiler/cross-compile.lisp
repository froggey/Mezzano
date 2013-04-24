;;;; Support functions for cross-compilation.

(in-package :sys.c)

(define-condition sys.int::simple-style-warning (style-warning simple-condition) ())

(defconstant sys.int::+tag-even-fixnum+   #b0000)
(defconstant sys.int::+tag-cons+          #b0001)
(defconstant sys.int::+tag-symbol+        #b0010)
(defconstant sys.int::+tag-array-header+  #b0011)
;;(defconstant sys.int::+tag-+  #b0100)
;;(defconstant sys.int::+tag-+  #b0101)
;;(defconstant sys.int::+tag-+  #b0110)
(defconstant sys.int::+tag-array-like+    #b0111)
(defconstant sys.int::+tag-odd-fixnum+    #b1000)
;;(defconstant sys.int::+tag-+  #b1001)
(defconstant sys.int::+tag-character+     #b1010)
(defconstant sys.int::+tag-single-float+  #b1011)
(defconstant sys.int::+tag-function+      #b1100)
;;(defconstant sys.int::+tag-+  #b1101)
(defconstant sys.int::+tag-unbound-value+ #b1110)
(defconstant sys.int::+tag-gc-forward+    #b1111)

(defconstant sys.int::+array-type-t+ 0)
(defconstant sys.int::+array-type-base-char+ 1)
(defconstant sys.int::+array-type-character+ 2)
(defconstant sys.int::+array-type-bit+ 3)
(defconstant sys.int::+array-type-unsigned-byte-2+ 4)
(defconstant sys.int::+array-type-unsigned-byte-4+ 5)
(defconstant sys.int::+array-type-unsigned-byte-8+ 6)
(defconstant sys.int::+array-type-unsigned-byte-16+ 7)
(defconstant sys.int::+array-type-unsigned-byte-32+ 8)
(defconstant sys.int::+array-type-unsigned-byte-64+ 9)
(defconstant sys.int::+array-type-signed-byte-1+ 10)
(defconstant sys.int::+array-type-signed-byte-2+ 11)
(defconstant sys.int::+array-type-signed-byte-4+ 12)
(defconstant sys.int::+array-type-signed-byte-8+ 13)
(defconstant sys.int::+array-type-signed-byte-16+ 14)
(defconstant sys.int::+array-type-signed-byte-32+ 15)
(defconstant sys.int::+array-type-signed-byte-64+ 16)
(defconstant sys.int::+array-type-single-float+ 17)
(defconstant sys.int::+array-type-double-float+ 18)
(defconstant sys.int::+array-type-long-float+ 19)
(defconstant sys.int::+array-type-xmm-vector+ 20)
(defconstant sys.int::+array-type-complex-single-float+ 21)
(defconstant sys.int::+array-type-complex-double-float+ 22)
(defconstant sys.int::+array-type-complex-long-float+ 23)
(defconstant sys.int::+last-array-type+ 23)
(defconstant sys.int::+array-type-bignum+ 25)
(defconstant sys.int::+array-type-stack-group+ 30)
(defconstant sys.int::+array-type-struct+ 31)

(defconstant sys.int::+function-type-function+ 0)
(defconstant sys.int::+function-type-closure+ 1)
(defconstant sys.int::+function-type-funcallable-instance+ 3)

(defconstant sys.int::+symbol-mode-nil+ 0)
(defconstant sys.int::+symbol-mode-special+ 1)
(defconstant sys.int::+symbol-mode-constant+ 2)
(defconstant sys.int::+symbol-mode-symbol-macro+ 3)

(defvar *system-macros* (make-hash-table :test 'eq))
(defvar *system-compiler-macros* (make-hash-table :test 'equal))
(defvar *system-symbol-macros* (make-hash-table :test 'eq))
(defvar *system-symbol-declarations* (make-hash-table :test 'eq))

(defvar *setf-symbols* (make-hash-table :test 'eq))
(defvar *reverse-setf-symbols* (make-hash-table :test 'eq))

(defstruct (structure-type
             (:constructor sys.int::make-struct-type
                           (name slots parent area)))
  (name)
  (slots)
  (parent)
  (area))

(defvar *structure-types* (make-hash-table :test 'eq))

(defun sys.int::function-symbol (name)
  (cond ((symbolp name) name)
        ((and (= (list-length name) 2)
              (eql (first name) 'setf)
              (symbolp (second name)))
         (let ((x (gethash (second name) *setf-symbols*)))
           (unless x
             (setf x (make-symbol (format nil "~A" name)))
             (setf (gethash (second name) *setf-symbols*) x
                   (gethash x *reverse-setf-symbols*) (second name)))
           x))
        (t (error "Bad function name ~S~%" name))))

(defvar *cross-readtable* (copy-readtable nil))

(defun sys.int::symbol-macro-expansion (symbol &optional env)
  (dolist (e env
           (gethash symbol *system-symbol-macros*))
    (when (eql (first e) :symbol-macros)
      (let ((x (assoc symbol (rest e))))
        (when x (return (second x)))))))

(defun compiler-macro-function (name &optional env)
  (dolist (e env
           (gethash name *system-compiler-macros*))
    (when (eql (first e) :compiler-macros)
      (let ((x (assoc name (rest e) :test 'equal)))
        (when x (return (cdr x)))))))

(defun (setf compiler-macro-function) (value name &optional env)
  (declare (ignore env))
  (setf (gethash name *system-compiler-macros*) value))

(defun macro-function (symbol &optional env)
  (dolist (e env (gethash symbol *system-macros*))
    (when (eql (first e) :macros)
      (let ((x (assoc symbol (rest e))))
        (when x (return (cdr x)))))))

(defun macroexpand (form &optional env)
  (let ((did-expand nil))
    (loop (multiple-value-bind (expansion expanded-p)
              (macroexpand-1 form env)
            (unless expanded-p
              (return (values expansion did-expand)))
            (setf did-expand t
                  form expansion)))))

(defun macroexpand-1 (form &optional env)
  (cond ((symbolp form)
         (sys.int::symbol-macro-expansion form env))
        ((consp form)
         (let ((fn (macro-function (first form) env)))
           (if fn
               (values (funcall *macroexpand-hook* fn form env) t)
               (values form nil))))
        (t (values form nil))))

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
      (return (values lambda-list (second i))))))

(defmacro def-x-macro (name lambda-list &body body)
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

(defvar *macroexpand-hook* 'funcall)

(defun constantp (form &optional env)
  (if (or (eql form 'nil) (eql form 't)
          (keywordp form)
          (and (not (symbolp form))
               (not (consp form))))
      t
      nil))

(defun sys.int::variable-information (symbol)
  (cond ((or (member symbol '(nil t))
             (keywordp symbol)
             #+sbcl (eql (sb-cltl2:variable-information symbol) :constant))
         :constant)
        (t (gethash symbol *system-symbol-declarations*))))

(defvar *output-fasl*)
(defvar *output-map*)
(defvar *output-dry-run*)
(defvar *pending-llf-commands*)

(defun x-compile-top-level-implicit-progn (forms env mode)
  (dolist (f forms)
    (x-compile-top-level f env mode)))

(defun x-compile-top-level-lms-body (forms env mode)
  "Common code for handling the body of LOCALLY, MACROLET and SYMBOL-MACROLET forms at the top-level."
  (multiple-value-bind (body declares)
      (parse-declares forms)
    (dolist (dec declares)
      (when (eql 'special (first dec))
        (push (list* :special (rest dec)) env)))
    (x-compile-top-level-implicit-progn body env mode)))

(defun make-macrolet-env (definitions env)
  (list* (list* :macros (mapcar 'hack-macrolet-definition definitions)) env))

(defun x-compile-top-level (form env &optional (mode :not-compile-time))
  "Cross-compile a top-level form.
3.2.3.1 Processing of Top Level Forms."
  (let ((expansion (macroexpand form env)))
    (cond ((consp expansion)
           (case (first expansion)
             ;; 3. If the form is a progn form, each of its body forms is sequentially
             ;;    processed as a top level form in the same processing mode.
             ((progn)
              (x-compile-top-level-implicit-progn (rest expansion) env mode))
             ;; 4. If the form is a locally, macrolet, or symbol-macrolet, compile-file
             ;;    establishes the appropriate bindings and processes the body forms as
             ;;    top level forms with those bindings in effect in the same processing mode.
             ((locally)
              (x-compile-top-level-lms-body (rest expansion) env mode))
             ((macrolet)
              (destructuring-bind (definitions &body body) (rest expansion)
                (x-compile-top-level-lms-body body (make-macrolet-env definitions env) mode)))
             ((symbol-macrolet)
              (destructuring-bind (definitions &body body) (rest expansion)
                (x-compile-top-level-lms-body body (make-symbol-macrolet-env definitions env) mode)))
             ;; 5. If the form is an eval-when form, it is handled according to figure 3-7.
             ((eval-when)
              (destructuring-bind (situation &body body) (rest expansion)
                (multiple-value-bind (compile load eval)
                    (sys.int::parse-eval-when-situation situation)
                  ;; Figure 3-7. EVAL-WHEN processing
                  (cond
                    ;; Process as compile-time-too.
                    ((or (and compile load)
                         (and (not compile) load eval (eql mode :compile-time-too)))
                     (x-compile-top-level-implicit-progn body env :compile-time-too))
                    ;; Process as not-compile-time.
                    ((or (and (not compile) load eval (eql mode :not-compile-time))
                         (and (not compile) load (not eval)))
                     (x-compile-top-level-implicit-progn body env :not-compile-time))
                    ;; Evaluate.
                    ((or (and compile (not load))
                         (and (not compile) (not load) eval (eql mode :compile-time-too)))
                     (x-eval `(progn ,@body) env))
                    ;; Discard.
                    ((or (and (not compile) (not load) eval (eql mode :not-compile-time))
                         (and (not compile) (not load) (not eval)))
                     nil)
                    (t (error "Impossible!"))))))
             ;; 6. Otherwise, the form is a top level form that is not one of the
             ;;    special cases. In compile-time-too mode, the compiler first
             ;;    evaluates the form in the evaluation environment and then minimally
             ;;    compiles it. In not-compile-time mode, the form is simply minimally
             ;;    compiled. All subforms are treated as non-top-level forms.
             (t (when (eql mode :compile-time-too)
                  (x-eval expansion env))
                (when *output-fasl*
                  (x-compile expansion env)))))
          (t (when (eql mode :compile-time-too)
               (x-eval expansion env))
             (when *output-fasl*
               (x-compile expansion env))))))

;; TODO: Wrap form in a bunch of macrolets.
(defun x-eval (form env)
  (when *compile-print*
    (let ((*print-length* 3)
          (*print-level* 2))
      (format t ";; X-eval: ~S~%" form)))
  (eval form))

(defstruct cross-function
  mc
  constants
  fixups)

(defun sys.int::assemble-lap (code &optional name debug-info)
  (multiple-value-bind (mc constants fixups)
      (sys.lap-x86:assemble code
        :base-address 12
        :initial-symbols '((nil . :fixup)
                           (t . :fixup)
                           (undefined-function . :fixup))
        :info (list name debug-info))
    (make-cross-function :mc mc
                         :constants constants
                         :fixups fixups)))

(defconstant +llf-end-of-load+ #xFF)
(defconstant +llf-backlink+ #x01)
(defconstant +llf-function+ #x02)
(defconstant +llf-cons+ #x03)
(defconstant +llf-symbol+ #x04)
(defconstant +llf-uninterned-symbol+ #x05)
(defconstant +llf-unbound+ #x06)
(defconstant +llf-string+ #x07)
(defconstant +llf-setf-symbol+ #x08)
(defconstant +llf-integer+ #x09)
(defconstant +llf-invoke+ #x0A)
(defconstant +llf-funcall+ #x0A) ; same as invoke
(defconstant +llf-setf-fdefinition+ #x0B)
(defconstant +llf-simple-vector+ #x0C)
(defconstant +llf-character+ #x0D)
(defconstant +llf-structure-definition+ #x0E)
(defconstant +llf-defun+ #x0F)
(defconstant +llf-single-float+ #x10)
(defconstant +llf-proper-list+ #x11)
;; A vector consisting entirely of integers.
(defconstant +llf-integer-vector+ #x13)
(defconstant +llf-add-backlink+ #x14)

(defun write-llf-header (output-stream input-file)
  (declare (ignore input-file))
  ;; TODO: write the source file name out as well.
  (write-sequence #(#x4C #x4C #x46 #x00) output-stream))

(defun save-integer (integer stream)
  (let ((negativep (minusp integer)))
    (when negativep (setf integer (- integer)))
    (do ()
        ((zerop (logand integer (lognot #x3F)))
         (write-byte (logior integer (if negativep #x40 0)) stream))
      (write-byte (logior #x80 (logand integer #x7F))
                  stream)
      (setf integer (ash integer -7)))))

(defun char-bits (char)
  (declare (ignore char))
  0)

;;; FIXME: This should allow saving of all attributes and arbitrary codes.
(defun save-character (character stream)
  (let ((code (char-code character)))
    (assert (zerop (char-bits character)) (character))
    (assert (and (<= 0 code #x1FFFFF)
                 (not (<= #xD800 code #xDFFF)))
            (character))
    (cond ((<= code #x7F)
           (write-byte code stream))
          ((<= #x80 code #x7FF)
           (write-byte (logior (ash (logand code #x7C0) -6) #xC0) stream)
           (write-byte (logior (logand code #x3F) #x80) stream))
          ((or (<= #x800 code #xD7FF)
               (<= #xE000 code #xFFFF))
           (write-byte (logior (ash (logand code #xF000) -12) #xE0) stream)
           (write-byte (logior (ash (logand code #xFC0) -6) #x80) stream)
           (write-byte (logior (logand code #x3F) #x80) stream))
          ((<= #x10000 code #x10FFFF)
           (write-byte (logior (ash (logand code #x1C0000) -18) #xF0) stream)
           (write-byte (logior (ash (logand code #x3F000) -12) #x80) stream)
           (write-byte (logior (ash (logand code #xFC0) -6) #x80) stream)
           (write-byte (logior (logand code #x3F) #x80) stream))
          (t (error "TODO character ~S." character)))))

(defgeneric save-one-object (object object-map stream))

(defmethod save-one-object ((object cross-function) omap stream)
  (let ((constants (cross-function-constants object)))
    (dotimes (i (length constants))
      (save-object (aref constants i) omap stream))
    (save-object (cross-function-fixups object) omap stream)
    (write-byte +llf-function+ stream)
    (write-byte 0 stream) ; tag, normal function.
    (save-integer (length (cross-function-mc object)) stream)
    (save-integer (length constants) stream)
    (write-sequence (cross-function-mc object) stream)))

(defmethod save-one-object ((object cons) omap stream)
  (cond ((alexandria:proper-list-p object)
         (let ((len 0))
           (dolist (o object)
             (save-object o omap stream)
             (incf len))
           (write-byte +llf-proper-list+ stream)
           (save-integer len stream)))
        (t (save-object (cdr object) omap stream)
           (save-object (car object) omap stream)
           (write-byte +llf-cons+ stream))))

(defmethod save-one-object ((object symbol) omap stream)
  #+sbcl
  (setf object (case object
                 (sb-impl::backq-list 'list)
                 (sb-impl::backq-list* 'list*)
                 (sb-impl::backq-append 'append)
                 (sb-impl::backq-nconc 'nconc)
                 (sb-impl::backq-cons 'cons)
                 (t object)))
  (cond ((symbol-package object)
         (write-byte +llf-symbol+ stream)
         (save-integer (length (symbol-name object)) stream)
         (dotimes (i (length (symbol-name object)))
           (save-character (char (symbol-name object) i) stream))
         (let ((package (symbol-package object)))
           (when (eql package (find-package :cross-cl))
             (setf package (find-package :cl)))
           (save-integer (length (package-name package)) stream)
           (dotimes (i (length (package-name package)))
             (save-character (char (package-name package) i) stream))))
        ((gethash object *reverse-setf-symbols*)
         (save-object (gethash object *reverse-setf-symbols*) omap stream)
         (write-byte +llf-setf-symbol+ stream))
        (t (save-object (symbol-name object) omap stream)
           ;; Should save flags?
           (if (boundp object)
               (save-object (symbol-value object) omap stream)
               (write-byte +llf-unbound+ stream))
           (if (fboundp object)
               (save-object (symbol-function object) omap stream)
               (write-byte +llf-unbound+ stream))
           (save-object (symbol-plist object) omap stream)
           (write-byte +llf-uninterned-symbol+ stream))))

(defmethod save-one-object ((object string) omap stream)
  (write-byte +llf-string+ stream)
  (save-integer (length object) stream)
  (dotimes (i (length object))
    (save-character (char object i) stream)))

(defmethod save-one-object ((object integer) omap stream)
  (write-byte +llf-integer+ stream)
  (save-integer object stream))

(defmethod save-one-object ((object vector) omap stream)
  (cond ((every #'integerp object)
         (write-byte +llf-integer-vector+ stream)
         (save-integer (length object) stream)
         (dotimes (i (length object))
           (save-integer (aref object i) stream)))
        (t (dotimes (i (length object))
             (save-object (aref object i) omap stream))
           (write-byte +llf-simple-vector+ stream)
           (save-integer (length object) stream))))

(defmethod save-one-object ((object character) omap stream)
  (write-byte +llf-character+ stream)
  (save-character object stream))

(defmethod save-one-object ((object structure-type) omap stream)
  (save-object (structure-type-name object) omap stream)
  (save-object (structure-type-slots object) omap stream)
  (save-object (structure-type-parent object) omap stream)
  (save-object (structure-type-area object) omap stream)
  (write-byte +llf-structure-definition+ stream))

(defun %single-float-as-integer (value)
  (check-type value single-float)
  #+sbcl (ldb (byte 32 0) (sb-kernel:single-float-bits value))
  #-(or sbcl) (error "Not implemented on this platform!"))

(defmethod save-one-object ((object float) omap stream)
  (write-byte +llf-single-float+ stream)
  (save-integer (%single-float-as-integer object) stream))

(defun save-object (object omap stream)
  (let ((info (alexandria:ensure-gethash object omap (list (hash-table-count omap) 0 nil))))
    (cond (*output-dry-run*
           (incf (second info))
           (when (eql (second info) 1)
             (save-one-object object omap stream)))
          (t (when (not (third info))
               (save-one-object object omap stream)
               (setf (third info) t)
               (unless (eql (second info) 1)
                 (write-byte +llf-add-backlink+ stream)
                 (save-integer (first info) stream)))
             (unless (eql (second info) 1)
                 (write-byte +llf-backlink+ stream)
                 (save-integer (first info) stream))))))

(defun add-to-llf (action &rest objects)
  (push (list* action objects) *pending-llf-commands*))

(defun x-compile (form env)
  ;; Special case (%defun 'name (lambda ...)) forms.
  (cond ((and (consp form)
              (eql (first form) 'sys.int::%defun)
              (= (list-length form) 3)
              (consp (second form))
              (eql (first (second form)) 'quote)
              (= (list-length (second form)) 2)
              (consp (third form))
              (eql (first (third form)) 'lambda))
         (let* ((name (second (second form)))
                (lambda (third form))
                (fn (compile-lambda lambda (cons env nil))))
           #+nil(add-to-llf +llf-defun+ name fn)
           (add-to-llf +llf-setf-fdefinition+ fn name)))
        ;; And (define-lap-function name (options...) code...)
        ((and (consp form)
              (eql (first form) 'sys.int::define-lap-function)
              (>= (length form) 3))
         (let ((name (second form))
               (options (third form))
               (code (cdddr form)))
           (assert (null options) () "No DEFINE-LAP-FUNCTION options supported yet.")
           (add-to-llf +llf-setf-fdefinition+ (sys.int::assemble-lap code name) name)))
        ;; And (quote form)
        ((and (consp form)
              (eql (first form) 'quote)
              (= (length form) 2)))
        ;; Convert other forms to zero-argument functions and
        ;; add it to the fasl as an eval node.
        ;; Progn to avoid problems with DECLARE.
        (t (let ((fn (compile-lambda `(lambda () (progn ,form)) (cons env nil))))
             (add-to-llf +llf-funcall+ fn)))))

(defun cross-compile-file (input-file &key
                           (output-file (make-pathname :type "llf" :defaults input-file))
                           (verbose *compile-verbose*)
                           (print *compile-print*)
                           (external-format :default))
  (with-open-file (input input-file :external-format external-format)
    (with-open-file (*output-fasl* output-file
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede
                     :direction :output)
      (write-llf-header *output-fasl* input-file)
      (let* ((*readtable* (copy-readtable *cross-readtable*))
             (*output-map* (make-hash-table))
             (*pending-llf-commands* nil)
             (*package* (find-package "CL-USER"))
             (*compile-print* print)
             (*compile-verbose* verbose)
             (*compile-file-pathname* (pathname (merge-pathnames input-file)))
             (*compile-file-truename* (truename *compile-file-pathname*))
             (*gensym-counter* 0)
             (sys.int::*top-level-form-number* 0))
        (when *compile-verbose*
          (format t ";; Cross-compiling ~S~%" input-file))
        (iter:iter (iter:for form = (read input nil input))
              (when (eql form input)
                (return))
              (when *compile-print*
                (let ((*print-length* 3)
                      (*print-level* 2))
                  (format t ";; X-compiling: ~S~%" form)))
              (x-compile-top-level form nil)
              (incf sys.int::*top-level-form-number*))
        ;; Now write everything to the fasl.
        ;; Do two passes to detect circularity.
        (let ((commands (reverse *pending-llf-commands*)))
          (let ((*output-dry-run* t))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (save-object o *output-map* (make-broadcast-stream)))))
          (let ((*output-dry-run* nil))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (save-object o *output-map* *output-fasl*))
              (write-byte (car cmd) *output-fasl*))))
        (write-byte +llf-end-of-load+ *output-fasl*))))
  output-file)

(defun load-for-cross-compiler (input-file &key
                           (verbose *compile-verbose*)
                           (print *compile-print*)
                           (external-format :default))
  (with-open-file (input input-file :external-format external-format)
    (let* ((*readtable* (copy-readtable *cross-readtable*))
           (*package* (find-package "CL-USER"))
           (*compile-print* print)
           (*compile-verbose* verbose)
           (*compile-file-pathname* (pathname (merge-pathnames input-file)))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (*output-fasl* nil)
           (*gensym-counter* 0))
      (when *compile-verbose*
        (format t ";; Cross-loading ~S~%" input-file))
      (iter:iter (iter:for form = (read input nil input))
            (when (eql form input)
              (return))
            (when *compile-print*
              (let ((*print-length* 3)
                    (*print-level* 2))
                (format t ";; X-loading: ~S~%" form)))
            (x-compile-top-level form nil :not-compile-time))))
  t)

(defparameter *cross-source-files*
  '("basic-macros.lisp"
    "defmacro.lisp"
    "backquote.lisp"
    "setf.lisp"
    "defstruct.lisp"
    "cons-compiler-macros.lisp"
    "condition.lisp"
    "restarts.lisp"
    "error.lisp"
    "type.lisp"
    "array.lisp"
    "sequence.lisp"
    "hash-table.lisp"
    "packages.lisp"
    "stream.lisp"
    "reader.lisp"
    "printer.lisp"
    "numbers.lisp"
    "character.lisp"
    "closette.lisp"
    "data-types.lisp"
    "gc.lisp"
    "process.lisp"
    "interrupt.lisp"
    "cold-start.lisp"
)
  "These files are loaded into the compiler environment so other source
files will be compiled correctly.")

(defun set-up-cross-compiler ()
  (mapc 'load-for-cross-compiler *cross-source-files*))

(defun save-compiler-builtins (path)
  (with-open-file (*output-fasl* path
                   :element-type '(unsigned-byte 8)
                   :if-exists :supersede
                   :direction :output)
    (format t ";; Writing compiler builtins to ~A.~%" path)
    (write-llf-header *output-fasl* path)
    (let* ((builtins (generate-builtin-functions))
           (*readtable* (copy-readtable *cross-readtable*))
           (*output-map* (make-hash-table))
           (*pending-llf-commands* nil)
           (*package* (find-package "CL-USER"))
           (*compile-print* *compile-print*)
           (*compile-verbose* *compile-verbose*)
           (*compile-file-pathname* (pathname (merge-pathnames path)))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (*gensym-counter* 0))
      (dolist (b builtins)
        (let ((form `(sys.int::%defun ',(first b) ,(second b))))
          (let ((*print-length* 3)
                (*print-level* 3))
            (format t ";; Compiling form ~S.~%" form))
          (x-compile form nil)))
      ;; Now write everything to the fasl.
      ;; Do two passes to detect circularity.
      (let ((commands (reverse *pending-llf-commands*)))
        (let ((*output-dry-run* t))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (save-object o *output-map* (make-broadcast-stream)))))
        (let ((*output-dry-run* nil))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (save-object o *output-map* *output-fasl*))
            (write-byte (car cmd) *output-fasl*))))
      (write-byte +llf-end-of-load+ *output-fasl*))))

#+sbcl
(sb-ext:with-unlocked-packages (#:cl)
(defun (setf get) (value symbol indicator &optional default)
  (declare (ignore default))
  (setf (get symbol indicator) value))
)
