;;;; COMPILE-FILE and related functions

(in-package :mezzano.internals)

(defgeneric make-load-form (object &optional environment))

(defmethod make-load-form (object &optional environment)
  (declare (ignore environment))
  (error "Cannot save ~S, no specialized MAKE-LOAD-FORM method" object))

(defmethod make-load-form ((object class) &optional environment)
  (declare (ignore environment))
  (when (not (eql (find-class (class-name object)) object))
    (error "Cannot serialize class ~S, has no name in the current environment."))
  `(find-class ',(class-name object)))

(defvar *compile-parallel* nil)
(defvar *top-level-form-number* nil)

(defvar *fixup-table* nil)

(defun expand-macrolet-function (function)
  (destructuring-bind (name lambda-list &body body) function
    (let ((whole (gensym "WHOLE"))
          (env (gensym "ENV")))
      (multiple-value-bind (new-lambda-list env-binding)
          (fix-lambda-list-environment lambda-list)
        `(lambda (,whole ,env)
           (declare (lambda-name (macrolet ,name))
                    (ignorable ,whole ,env))
           ,(expand-destructuring-lambda-list new-lambda-list name body
                                              whole `(cdr ,whole)
                                              (when env-binding
                                                (list `(,env-binding ,env)))
                                              :permit-docstring t))))))

(defun make-macrolet-env (defs env)
  "Return a new environment containing the macro definitions."
  ;; FIXME: Outer macrolets & symbol macrolets should be visible in the macrolet's body.
  (let ((macro-bindings (loop
                           for def in defs
                           collect (list (first def)
                                         (eval (expand-macrolet-function def))))))
    (mezzano.compiler::extend-environment env :functions macro-bindings)))

(defun make-symbol-macrolet-env (defs env)
  (let ((defs (loop
                 for (name expansion) in defs
                 collect (make-instance 'mezzano.compiler::symbol-macro
                                        :name name
                                        :expansion expansion))))
    (mezzano.compiler::extend-environment env :variables defs)))

(defun handle-top-level-implicit-progn (forms load-fn eval-fn mode env)
  (dolist (f forms)
    (handle-top-level-form f load-fn eval-fn mode env)))

(defun handle-top-level-lms-body (forms load-fn eval-fn mode env)
  "Common code for handling the body of LOCALLY, MACROLET and SYMBOL-MACROLET forms at the top-level."
  (multiple-value-bind (body declares)
      (parse-declares forms)
    (handle-top-level-implicit-progn
     body load-fn eval-fn mode
     (mezzano.compiler::extend-environment env :declarations declares))))

(defun macroexpand-top-level-form (form env)
  (cond ((and (listp form)
              (>= (list-length form) 3)
              (eql (first form) 'define-lap-function)
              (listp (third form)))
         ;; Don't expand DEFINE-LAP-FUNCTION.
         (values form nil))
        (t
         ;; Preserve the above behaviour when recursively macroexpanding.
         (multiple-value-bind (expansion expandedp)
             (macroexpand-1 form env)
           (cond (expandedp
                  (values (macroexpand-top-level-form expansion env)
                          t))
                 (t
                  (values expansion nil)))))))

(defun handle-top-level-form (form load-fn eval-fn &optional (mode :not-compile-time) env)
  "Handle top-level forms. If the form should be evaluated at compile-time
then it is evaluated using EVAL-FN, if it should be loaded then
it is passed to LOAD-FN.
LOAD-FN is must be a function designator for a two-argument function. The
first argument is the form to load and the second is the environment.
NOTE: Non-compound forms (after macro-expansion) are ignored."
  ;; 3.2.3.1 Processing of Top Level Forms
  ;; 2. If the form is a macro form, its macro expansion is computed and processed
  ;;    as a top level form in the same processing mode.
  (let ((expansion (macroexpand-top-level-form form env)))
    ;; Symbol-macros, etc will have been expanded by this point
    ;; Normal symbols and self-evaluating objects will not have side-effects and
    ;; can be ignored.
    (when (consp expansion)
      (case (first expansion)
        ;; 3. If the form is a progn form, each of its body forms is sequentially
        ;;    processed as a top level form in the same processing mode.
        ((progn) (handle-top-level-implicit-progn (rest expansion) load-fn eval-fn mode env))
        ;; 4. If the form is a locally, macrolet, or symbol-macrolet, compile-file
        ;;    establishes the appropriate bindings and processes the body forms as
        ;;    top level forms with those bindings in effect in the same processing mode.
        ((locally)
         (handle-top-level-lms-body (rest expansion) load-fn eval-fn mode env))
        ((macrolet)
         (destructuring-bind (definitions &body body) (rest expansion)
           (handle-top-level-lms-body body load-fn eval-fn mode (make-macrolet-env definitions env))))
        ((symbol-macrolet)
         (destructuring-bind (definitions &body body) (rest expansion)
           (handle-top-level-lms-body body load-fn eval-fn mode (make-symbol-macrolet-env definitions env))))
        ;; 5. If the form is an eval-when form, it is handled according to figure 3-7.
        ((eval-when)
         (destructuring-bind (situation &body body) (rest expansion)
           (multiple-value-bind (compile load eval)
               (parse-eval-when-situation situation)
             ;; Figure 3-7. EVAL-WHEN processing
             (cond
               ;; Process as compile-time-too.
               ((or (and compile load)
                    (and (not compile) load eval (eql mode :compile-time-too)))
                (handle-top-level-implicit-progn body load-fn eval-fn :compile-time-too env))
               ;; Process as not-compile-time.
               ((or (and (not compile) load eval (eql mode :not-compile-time))
                    (and (not compile) load (not eval)))
                (handle-top-level-implicit-progn body load-fn eval-fn :not-compile-time env))
               ;; Evaluate.
               ((or (and compile (not load))
                    (and (not compile) (not load) eval (eql mode :compile-time-too)))
                (funcall eval-fn `(progn ,@body) env))
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
             (funcall eval-fn expansion env))
           (funcall load-fn expansion env))))))

(defvar *compile-verbose* t)
(defvar *compile-print* t)

(defvar *compile-file-pathname* nil)
(defvar *compile-file-truename* nil)

(defun compile-file-pathname (input-file &key output-file &allow-other-keys)
  (if output-file
      output-file
      (make-pathname :type "llf" :defaults input-file)))

(defvar *llf-forms*)
(defvar *llf-dry-run*)

(defun write-llf-header (output-stream input-file)
  (declare (ignore input-file))
  ;; TODO: write the source file name out as well.
  (write-sequence #(#x4C #x4C #x46 #x01) output-stream) ; LLF\x01
  (save-integer *llf-version* output-stream)
  (save-integer #+x86-64 +llf-arch-x86-64+
                #+arm64 +llf-arch-arm64+
                output-stream))

(defun save-integer (integer stream)
  (let ((negativep (minusp integer)))
    (when negativep (setf integer (- integer)))
    (do ()
        ((zerop (logand integer (lognot #x3F)))
         (write-byte (logior integer (if negativep #x40 0)) stream))
      (write-byte (logior #x80 (logand integer #x7F))
                  stream)
      (setf integer (ash integer -7)))))

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

(defmethod save-one-object ((object function) omap stream)
  (when (not (eql (function-tag object) +object-tag-function+))
    (error "Cannot save complicated function ~S" object))
  (dotimes (i (function-pool-size object))
    (save-object (function-pool-object object i) omap stream))
  (multiple-value-bind (fixups validp)
      (gethash object *fixup-table*)
    (when (not validp)
      (error "Missing fixups for function ~S" object))
    (save-object fixups omap stream))
  (write-byte +llf-function+ stream)
  (write-byte (function-tag object) stream)
  (save-integer (- (function-code-size object) 16) stream)
  (save-integer (function-pool-size object) stream)
  (multiple-value-bind (gc-info-address gc-info-length)
      (function-gc-info object)
    (declare (ignore gc-info-address))
    (save-integer gc-info-length stream)
    ;; Accumulate the code & gc metadata into a single big octet vector
    ;; and blat that out in one go. Faster than calling write-byte a
    ;; bunch of time.
    (when (not *llf-dry-run*)
      (let* ((n-code-bytes (- (function-code-size object) 16))
             (data (make-array (+ n-code-bytes gc-info-length) :element-type '(unsigned-byte 8))))
        (dotimes (i n-code-bytes)
          (setf (aref data i) (function-code-byte object (+ i 16))))
        (dotimes (i gc-info-length)
          (setf (aref data (+ n-code-bytes i)) (function-gc-metadata-byte object i)))
        (write-sequence data stream)))))

;;; From Alexandria.
(defun proper-list-p (object)
  "Returns true if OBJECT is a proper list."
  (cond ((not object)
         t)
        ((consp object)
         (do ((fast object (cddr fast))
              (slow (cons (car object) (cdr object)) (cdr slow)))
             (nil)
           (unless (and (listp fast) (consp (cdr fast)))
             (return (and (listp fast) (not (cdr fast)))))
           (when (eq fast slow)
             (return nil))))
        (t
         nil)))

(defmethod save-one-object ((object cons) omap stream)
  ;; FIXME: This has issues with circularities.
  (cond ((proper-list-p object)
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
  (cond ((symbol-package object)
         (write-byte +llf-symbol+ stream)
         (save-integer (length (symbol-name object)) stream)
         (dotimes (i (length (symbol-name object)))
           (save-character (char (symbol-name object) i) stream))
         (let ((package (symbol-package object)))
           (save-integer (length (package-name package)) stream)
           (dotimes (i (length (package-name package)))
             (save-character (char (package-name package) i) stream))))
        (t (save-object (symbol-name object) omap stream)
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
  (cond ((eql (array-element-type object) 't)
         (write-byte +llf-simple-vector+ stream)
         (save-integer (length object) stream)
         (write-object-backlink object omap stream :keep t)
         (dotimes (i (length object))
           (save-object (aref object i) omap stream))
         (write-byte +llf-initialize-array+ stream)
         (save-integer (length object) stream))
        (t
         ;; Save the vector with the appropriate element-type,
         ;; trimming it down based on the fill-pointer (if any).
         ;; Objects saved to a file are EQUAL to their original objects, not
         ;; EQL, and EQUAL respects the fill-pointer when comparing vectors.
         ;; Non-T vectors can't contain any complex Lisp objects, so
         ;; circularity isn't an issue.
         (dotimes (i (length object))
           (save-object (aref object i) omap stream))
         (save-object (array-element-type object) omap stream)
         (write-byte +llf-typed-array+ stream)
         (save-integer 1 stream)
         (save-integer (length object) stream))))

(defmethod save-one-object ((object character) omap stream)
  (cond ((zerop (char-bits object))
         (write-byte +llf-character+ stream)
         (save-character object stream))
        (t
         (write-byte +llf-character-with-bits+ stream)
         (save-character (code-char (char-code object)) stream)
         (save-integer (char-bits object) stream))))

(defun convert-structure-class-to-structure-definition (class)
  (make-struct-definition
   (class-name class)
   (loop
      for slot in (mezzano.clos:class-slots class)
      collect (make-struct-slot-definition
               (mezzano.clos:slot-definition-name slot)
               nil
               (mezzano.clos:slot-definition-initform slot)
               (mezzano.clos:slot-definition-type slot)
               (mezzano.clos:structure-slot-definition-read-only slot)
               (mezzano.clos:slot-definition-location slot)
               (mezzano.clos:structure-slot-definition-fixed-vector slot)
               (mezzano.clos:structure-slot-definition-align slot)
               (mezzano.clos:structure-slot-definition-dcas-sibling slot)
               (documentation slot t)))
   (if (eql class (find-class 'structure-object))
       nil
       (let ((parent (second (mezzano.clos:class-precedence-list class))))
         (if (eql parent (find-class 'structure-object))
             nil
             parent)))
   (mezzano.clos:class-allocation-area class)
   (layout-heap-size (mezzano.clos:class-layout class))
   (layout-heap-layout (mezzano.clos:class-layout class))
   (mezzano.clos:class-sealed class)
   (documentation class t)
   (slot-value class 'mezzano.clos::has-standard-constructor)))

(defmethod save-one-object ((object structure-class) omap stream)
  (save-one-object (convert-structure-class-to-structure-definition object)
                   omap stream))

(defmethod save-one-object ((object layout) omap stream)
  (save-one-object (layout-class object) omap stream)
  (write-byte +llf-layout+ stream))

(defmethod save-one-object ((object structure-definition) omap stream)
  (save-object (structure-definition-name object) omap stream)
  (save-object (structure-definition-slots object) omap stream)
  (save-object (structure-definition-parent object) omap stream)
  (save-object (structure-definition-area object) omap stream)
  (save-object (structure-definition-size object) omap stream)
  (save-object (layout-heap-layout (structure-definition-layout object)) omap stream)
  (save-object (structure-definition-sealed object) omap stream)
  (save-object (structure-definition-docstring object) omap stream)
  (save-object (structure-definition-has-standard-constructor object) omap stream)
  (write-byte +llf-structure-definition+ stream))

(defmethod save-one-object ((object structure-slot-definition) omap stream)
  (save-object (structure-slot-definition-name object) omap stream)
  (save-object (structure-slot-definition-accessor object) omap stream)
  (save-object (structure-slot-definition-initform object) omap stream)
  (save-object (structure-slot-definition-type object) omap stream)
  (save-object (structure-slot-definition-read-only object) omap stream)
  (save-object (structure-slot-definition-location object) omap stream)
  (save-object (structure-slot-definition-fixed-vector object) omap stream)
  (save-object (structure-slot-definition-align object) omap stream)
  (save-object (structure-slot-definition-dcas-sibling object) omap stream)
  (save-object (structure-slot-definition-documentation object) omap stream)
  (write-byte +llf-structure-slot-definition+ stream))

(defmethod save-one-object ((object float) omap stream)
  (declare (ignore omap))
  (etypecase object
    (short-float
     (write-byte +llf-short-float+ stream)
     (save-integer (%short-float-as-integer object) stream))
    (single-float
     (write-byte +llf-single-float+ stream)
     (save-integer (%single-float-as-integer object) stream))
    (double-float
     (write-byte +llf-double-float+ stream)
     (save-integer (%double-float-as-integer object) stream))))

(defmethod save-one-object ((object package) omap stream)
  (write-byte +llf-package+ stream)
  (let ((name (package-name object)))
    (save-integer (length name) stream)
    (dotimes (i (length name))
      (save-character (char name i) stream))))

(defmethod save-one-object ((object ratio) omap stream)
  (write-byte +llf-ratio+ stream)
  (save-integer (numerator object) stream)
  (save-integer (denominator object) stream))

(defmethod save-one-object ((object array) omap stream)
  (dotimes (i (array-total-size object))
    (save-object (row-major-aref object i) omap stream))
  (cond ((eql (array-element-type object) 't)
         (write-byte +llf-array+ stream))
        (t
         (save-object (array-element-type object) omap stream)
         (write-byte +llf-typed-array+ stream)))
  (save-integer (array-rank object) stream)
  (dolist (dim (array-dimensions object))
    (save-integer dim stream)))

(defmethod save-one-object ((object bit-vector) omap stream)
  (write-byte +llf-bit-vector+ stream)
  (save-integer (length object) stream)
  (dotimes (i (ceiling (length object) 8))
    (let ((octet 0))
      (dotimes (j 8)
        (let ((idx (+ (* i 8) j)))
          (when (>= idx (length object)) (return))
          (setf (ldb (byte 1 j) octet) (bit object idx))))
      (write-byte octet stream))))

(defmethod save-one-object ((object function-reference) omap stream)
  (save-object (function-reference-name object) omap stream)
  (write-byte +llf-function-reference+ stream))

(defmethod save-one-object ((object mezzano.runtime::symbol-value-cell) omap stream)
  (assert (mezzano.runtime::symbol-global-value-cell-p object))
  (save-object (mezzano.runtime::symbol-value-cell-symbol object) omap stream)
  (write-byte +llf-symbol-global-value-cell+ stream))

(defmethod save-one-object ((object byte) omap stream)
  (write-byte sys.int::+llf-byte+ stream)
  (save-integer (byte-size object) stream)
  (save-integer (byte-position object) stream))

(defmethod save-one-object ((object complex) omap stream)
  (etypecase (realpart object)
    (rational
     (write-byte sys.int::+llf-complex-rational+ stream)
     (save-integer (numerator (realpart object)) stream)
     (save-integer (denominator (realpart object)) stream)
     (save-integer (numerator (imagpart object)) stream)
     (save-integer (denominator (imagpart object)) stream))
    (short-float
     (write-byte sys.int::+llf-complex-short-float+ stream)
     (save-integer (%short-float-as-integer (realpart object)) stream)
     (save-integer (%short-float-as-integer (imagpart object)) stream))
    (single-float
     (write-byte sys.int::+llf-complex-single-float+ stream)
     (save-integer (%single-float-as-integer (realpart object)) stream)
     (save-integer (%single-float-as-integer (imagpart object)) stream))
    (double-float
     (write-byte sys.int::+llf-complex-double-float+ stream)
     (save-integer (%double-float-as-integer (realpart object)) stream)
     (save-integer (%double-float-as-integer (imagpart object)) stream))))

(defun load-hash-table-entries (hash-table entries)
  (loop
     for i below (length entries) by 2
     for key = (svref entries i)
     for value = (svref entries (1+ i))
     do (setf (gethash key hash-table) value)))

(defmethod make-load-form ((object hash-table) &optional environment)
  (declare (ignore environment))
  (values `(make-hash-table :test ',(hash-table-test object)
                            :rehash-size ',(hash-table-rehash-size object)
                            :rehash-threshold ',(hash-table-rehash-threshold object)
                            :synchronized ',(hash-table-synchronized object)
                            :enforce-gc-invariant-keys ',(hash-table-enforce-gc-invariant-keys object)
                            :weakness ',(hash-table-weakness object))
          (if (not (zerop (hash-table-count object)))
              `(load-hash-table-entries
                ',object
                ',(loop
                     with entries = (make-array (* (hash-table-count object) 2))
                     for i from 0 by 2
                     for key being the hash-keys in object using (hash-value value)
                     do (setf (svref entries i) key
                              (svref entries (1+ i)) value)
                     finally (return entries))))))

(defmethod save-one-object ((object instance-header) omap stream)
  (save-object (sys.int::layout-class (mezzano.runtime::%unpack-instance-header object))
               omap stream)
  (write-byte +llf-instance-header+ stream))

(defvar *compiling-make-load-form* nil)

(defmethod save-one-object (object omap stream)
  ;; Use COMPILE-LAMBDA here instead of COMPILE to get the correct L-T-V behaviour.
  (let* ((*compiling-make-load-form* (cons omap stream))
         (mezzano.compiler::*load-time-value-hook*
          (lambda (form read-only-p)
            (declare (ignore read-only-p))
            (let ((ltv-sym (gensym "LOAD-TIME-VALUE-CELL")))
              (compile-top-level-form `(locally
                                           (declare (special ,ltv-sym))
                                         (setq ,ltv-sym ,form))
                                      nil)
              `(sys.int::symbol-global-value ',ltv-sym)))))
    (multiple-value-bind (creation-form initialization-form)
        (make-load-form object)
      (compile-top-level-form-for-value creation-form nil)
      (when initialization-form
        (write-object-backlink object omap stream :keep t)
        (compile-top-level-form initialization-form nil)))))

(defun write-object-backlink (object omap stream &key keep)
  (when (not *llf-dry-run*)
    (let ((info (gethash object omap)))
      (assert (eql (third info) :save-in-progress))
      (setf (third info) t)
      (unless (eql (second info) 1)
        (write-byte +llf-add-backlink+ stream)
        (save-integer (first info) stream)
        (when keep
          (write-byte +llf-backlink+ stream)
          (save-integer (first info) stream))))))

(defun save-object (object omap stream)
  (when (typep object 'deferred-function)
    (assert (deferred-function-function object))
    ;; Perform any additional commands for this deferred function.
    (when (deferred-function-additional-commands object)
      (dolist (cmd (reverse (deferred-function-additional-commands object)))
        (dolist (o (cdr cmd))
          (save-object o omap stream))
        (when (and (not *llf-dry-run*)
                   (car cmd))
          (write-byte (car cmd) stream))))
    (return-from save-object
      (save-object (deferred-function-function object) omap stream)))
  (when (null (gethash object omap))
    ;; Backlink id, reference count, has-been-emitted
    (setf (gethash object omap) (list (hash-table-count omap) 0 nil)))
  (let ((info (gethash object omap)))
    (cond (*llf-dry-run*
           (incf (second info))
           (when (eql (second info) 1)
             (save-one-object object omap stream)))
          (t
           (ecase (third info)
             ((nil)
              ;; Emit object
              (setf (third info) :save-in-progress)
              (save-one-object object omap stream)
              (when (eql (third info) :save-in-progress)
                (write-object-backlink object omap stream :keep t)))
             (:save-in-progress
              (error "Unbroken circularity detected when saving object ~S. This can happen when an object's allocation form contains the object, possibly indirectly." object))
             ((t)
              ;; object saved ok
              (unless (eql (second info) 1)
                (write-byte +llf-backlink+ stream)
                (save-integer (first info) stream))))))))

(defun make-load-form-saving-slots (object &key (slot-names nil slot-names-p) environment)
  (declare (ignore environment))
  (etypecase object
    (standard-object
     (values `(allocate-instance ',(class-of object))
             `(progn
                ,@(loop
                     with class = (class-of object)
                     for slot in (mezzano.clos:class-slots class)
                     when (and (eql (mezzano.clos:slot-definition-allocation slot) :instance)
                               (or (not slot-names-p)
                                   (member (mezzano.clos:slot-definition-name slot) slot-names))
                               (slot-boundp object (mezzano.clos:slot-definition-name slot)))
                     collect `(setf (slot-value ',object ',(mezzano.clos:slot-definition-name slot))
                                    ',(slot-value object (mezzano.clos:slot-definition-name slot)))))))
    (structure-object
     ;; Must use alternative setters, (SETF SLOT-VALUE-USING-CLASS) on structures respects read-only-p.
     (values `(%allocate-struct ',(class-name (class-of object)))
             `(progn
                ,@(loop
                     with class = (class-of object)
                     with class-name = (class-name class)
                     for slot in (mezzano.clos:class-slots class)
                     when (and (eql (mezzano.clos:slot-definition-allocation slot) :instance)
                               (or (not slot-names-p)
                                   (member (mezzano.clos:slot-definition-name slot) slot-names)))
                     collect `(setf (%struct-slot ',object ',class-name ',(mezzano.clos:slot-definition-name slot))
                                    ',(slot-value object (mezzano.clos:slot-definition-name slot)))))))))

(defvar *current-deferred-function* nil)

(defun add-to-llf (action &rest objects)
  (cond (*current-deferred-function*
         ;; Running in a worker thread to compile a function.
         (push (list* action objects)
               (deferred-function-additional-commands
                   *current-deferred-function*)))
        (*compiling-make-load-form*
         ;; Compiling a call to make-load-form.
         ;; This occurs when saving objects, so the work must not be deferred.
         (dolist (o objects)
           (save-object o (car *compiling-make-load-form*) (cdr *compiling-make-load-form*)))
         (when (and action (not *llf-dry-run*))
           (write-byte action (cdr *compiling-make-load-form*))))
        (t
         ;; Normal operation, save the forms in the command list.
         (push (list* action objects) *llf-forms*))))

(defun compile-file-load-time-value (form read-only-p)
  (declare (ignore read-only-p))
  (let ((ltv-sym (gensym "LOAD-TIME-VALUE-CELL")))
    (compile-top-level-form `(locally
                                 (declare (special ,ltv-sym))
                               (setq ,ltv-sym ,form))
                            nil)
    `(sys.int::symbol-global-value ',ltv-sym)))

(defun compile-top-level-form (form env)
  (cond
    ;; Special case (define-lap-function name (options...) code...)
    ;; Don't macroexpand this, as it expands into a bunch of difficult to
    ;; recognize nonsense.
    ((and (listp form)
          (>= (list-length form) 3)
          (eql (first form) 'define-lap-function)
          (listp (third form)))
     (destructuring-bind (name (&optional lambda-list frame-layout environment-vector-offset environment-vector-layout) &body code)
         (cdr form)
       (let ((docstring nil))
         (when (stringp (first code))
           (setf docstring (pop code)))
         (compile-top-level-form
          `(sys.int::%defun ',name
                            ',(assemble-lap
                               code
                               name
                               (list :debug-info
                                     name
                                     frame-layout
                                     environment-vector-offset
                                     environment-vector-layout
                                     (when *compile-file-pathname*
                                       (princ-to-string *compile-file-pathname*))
                                     sys.int::*top-level-form-number*
                                     lambda-list
                                     docstring
                                     nil)
                               nil
                               #+x86-64 :x86-64
                               #+arm64 :arm64))
          env))))
    (t
     (compile-top-level-form-for-value form env)
     (add-to-llf sys.int::+llf-drop+))))

;; One of:
;;   'symbol
;;   #'function-name
(defun valid-funcall-function-p (form)
  (and (consp form)
       (consp (cdr form))
       (null (cddr form))
       (or (and (eql (first form) 'quote)
                (symbolp (second form)))
           (and (eql (first form) 'function)
                (valid-function-name-p (second form))))))

;; Convert a valid funcall function to the function name.
(defun funcall-function-name (form)
  (second form))

(defun compile-top-level-form-for-value (form env)
  ;; FIXME: This should probably use compiler-macroexpand.
  (let ((expansion (macroexpand form env)))
    (cond
      ((symbolp expansion)
       (if (or (keywordp expansion) (member expansion '(nil t)))
           (add-to-llf nil expansion)
           (add-to-llf sys.int::+llf-funcall-n+ expansion 'symbol-value 1)))
      ((not (consp expansion))
       ;; Self-evaluating form.
       (add-to-llf nil expansion))
      ((eql (first expansion) 'quote)
       (add-to-llf nil (second expansion)))
      ((and (eql (first expansion) 'function)
            (consp (second expansion))
            (eql (first (second expansion)) 'lambda))
       (add-to-llf nil (add-deferred-lambda (second expansion) env)))
      ((eql (first expansion) 'function)
       (compile-top-level-form-for-value `(fdefinition ',(second expansion))
                                         env))
      ((eql (first expansion) 'setq)
       (loop
          with first-iteration = t
          for (symbol value) on (rest form) by #'cddr
          do
            (assert (typep symbol 'symbol))
            (cond (first-iteration
                   (setf first-iteration nil))
                  (t
                   (add-to-llf sys.int::+llf-drop+)))
            (if (nth-value 1 (macroexpand-1 symbol))
                ;; This is a symbol macro, defer to SETF.
                (compile-top-level-form-for-value `(setf ,symbol ,value) env)
                ;; Regular symbol.
                (compile-top-level-form-for-value `(funcall #'(setf symbol-value)
                                                            ,value
                                                            ',symbol)
                                                  env))))
      ((eql (first expansion) 'if)
       (destructuring-bind (test then &optional else)
           (rest expansion)
         (compile-top-level-form-for-value test env)
         (add-to-llf sys.int::+llf-if+)
         (compile-top-level-form-for-value then env)
         (add-to-llf sys.int::+llf-else+)
         (compile-top-level-form-for-value else env)
         (add-to-llf sys.int::+llf-fi+)))
      ((and (eql (first expansion) 'progn)
            (cdr expansion)
            (endp (cddr expansion)))
       ;; Only (progn foo) supported currently.
       (compile-top-level-form-for-value (second expansion) env))
      ((special-operator-p (first expansion))
       ;; Can't convert this, convert it to a zero-argument function and
       ;; call that. PROGN to avoid problems with DECLARE.
       (add-to-llf sys.int::+llf-funcall-n+
                   (add-deferred-lambda
                    `(lambda ()
                       (declare (lambda-name
                                 (sys.int::toplevel ,(when *compile-file-pathname*
                                                           (princ-to-string *compile-file-pathname*))
                                                    ,sys.int::*top-level-form-number*)))
                       (progn ,expansion))
                    env)
                   0))
      (t
       ;; That should just leave ordinary calls.
       (let ((name (first expansion))
             (args (rest expansion)))
         ;; Unpeel funcall forms.
         (loop
            (cond ((and (eql name 'funcall)
                        (consp args)
                        (valid-funcall-function-p (first args)))
                   (setf name (funcall-function-name (first args))
                         args (rest args)))
                  (t
                   (return))))
         (dolist (arg args)
           (compile-top-level-form-for-value arg env))
         (add-to-llf sys.int::+llf-funcall-n+ name (length args)))))))

(defvar *deferred-functions*)

(defclass deferred-function ()
  ((%ast :initarg :ast :reader deferred-function-ast)
   (%tlf-number :initarg :tlf-number :reader deferred-function-tlf-number)
   (%compiled-function :initarg :compiled-function :accessor deferred-function-function)
   (%additional-commands :initarg :additional-commands :accessor deferred-function-additional-commands))
  (:default-initargs
   :tlf-number nil
   :compiled-function nil
   :additional-commands nil))

(defun add-deferred-lambda (lambda env)
  (if (and *compile-parallel*
           ;; If true, then this is a worker thread compiling something.
           ;; Don't schedule more work, just do it here.
           (not *current-deferred-function*)
           ;; If true then a make-load-form is actively being compiled
           ;; and is running in the context of save-object, now long past
           ;; the time that parallel compilation can be done.
           (not *compiling-make-load-form*))
      (let ((fn (make-instance 'deferred-function
                               :ast (mezzano.compiler::pass1-lambda lambda env)
                               :tlf-number *top-level-form-number*)))
        (push fn *deferred-functions*)
        fn)
      (mezzano.compiler::compile-lambda lambda env)))

(defun compile-file-worker (work-fifo return-fifo)
  (unwind-protect
       (loop
          (let ((work (mezzano.supervisor:fifo-pop work-fifo)))
            (when (eql work :finished)
              (return))
            (handler-bind
                ((error (lambda (c)
                          (mezzano.supervisor:fifo-push `(:error ,work ,c) return-fifo))))
              (let* ((*current-deferred-function* work)
                     (*top-level-form-number* (deferred-function-tlf-number work)))
                (setf (deferred-function-function work)
                      (mezzano.compiler::compile-ast (deferred-function-ast work))))
              (mezzano.supervisor:fifo-push work return-fifo))))
    (mezzano.supervisor:fifo-push :exit return-fifo)))

(defparameter *parallel-compile-file-dots-per-line* 72)

(defun compile-file (input-file &key
                                  (output-file (compile-file-pathname input-file))
                                  (verbose *compile-verbose*)
                                  (print *compile-print*)
                                  (external-format :default)
                                  (parallel *compile-parallel*))
  (with-compilation-unit ()
    (with-open-file (input-stream input-file :external-format external-format)
      (when verbose
        (format t ";; Compiling file ~S.~%" input-file))
      (let* ((start-time (get-internal-run-time))
             (*package* *package*)
             (*readtable* *readtable*)
             (*compile-verbose* verbose)
             (*compile-print* print)
             ;; Only perform parallel compilation when there are multiple cores.
             (*compile-parallel* (and parallel
                                      (or (eql parallel :force)
                                          (> mezzano.supervisor::*n-up-cpus* 1))))
             (*deferred-functions* '())
             (*llf-forms* nil)
             (omap (make-hash-table))
             (eof-marker (cons nil nil))
             (*compile-file-pathname* (pathname (merge-pathnames input-file)))
             (*compile-file-truename* (truename *compile-file-pathname*))
             (*top-level-form-number* 0)
             (mezzano.compiler::*load-time-value-hook* 'compile-file-load-time-value)
             ;; Don't persist optimize proclaimations outside COMPILE-FILE.
             (mezzano.compiler::*optimize-policy* (copy-list mezzano.compiler::*optimize-policy*))
             (*gensym-counter* 0)
             ;; During parallel compilation this is written to by the workers.
             (*fixup-table* (make-hash-table :synchronized (if *compile-parallel* t nil)))
             (location-stream (make-instance 'sys.int::location-tracking-stream
                                             :stream input-stream
                                             :namestring (ignore-errors (namestring *compile-file-pathname*)))))
        (sys.int::with-reader-location-tracking
          (loop
             for form = (read location-stream nil eof-marker)
             until (eql form eof-marker)
             do
               (when *compile-print*
                 (let ((*print-length* 3)
                       (*print-level* 3))
                   (declare (special *print-length* *print-level*))
                   (format t ";; ~A form ~S.~%"
                           (if *compile-parallel* "Processing" "Compiling")
                           form)))
             ;; TODO: Deal with lexical environments.
               (handle-top-level-form form
                                      (lambda (f env)
                                        (compile-top-level-form f env))
                                      (lambda (f env)
                                        (eval-in-lexenv f env)))
               (incf *top-level-form-number*)))
        (when *deferred-functions*
          (let* ((n-functions (length *deferred-functions*))
                 (n-workers mezzano.supervisor::*n-up-cpus*)
                 (work-fifo (mezzano.supervisor:make-fifo (* (max n-workers n-functions) 2)))
                 (return-fifo (mezzano.supervisor:make-fifo (* (max n-workers n-functions) 2)))
                 (workers '()))
            (unwind-protect
                 (progn
                   (loop
                      repeat n-workers
                      do (push (mezzano.supervisor:make-thread
                                (lambda () (compile-file-worker work-fifo return-fifo))
                                :name (format nil "Compile file worker")
                                :initial-bindings `((*terminal-io* ,*terminal-io*)
                                                    (*debug-io* ,*debug-io*)
                                                    (*error-output* ,*error-output*)
                                                    (*query-io* ,*query-io*)
                                                    (*standard-input* ,*standard-input*)
                                                    (*standard-output* ,*standard-output*)
                                                    (*trace-output* ,*trace-output*)
                                                    (*fixup-table* ,*fixup-table*)
                                                    (*compile-verbose* ,*compile-verbose*)
                                                    (*compile-print* ,*compile-print*)
                                                    (*compile-file-pathname* ,*compile-file-pathname*)
                                                    (*compile-file-truename* ,*compile-file-truename*)
                                                    ,@(mezzano.compiler::compiler-state-bindings))
                                :priority :low)
                               workers))
                   (loop
                      for fn in *deferred-functions*
                      do (mezzano.supervisor:fifo-push fn work-fifo))
                   (when *compile-print*
                     (format t ";; Parallel compiling ~:D functions over ~D workers...~%" n-functions n-workers))
                   (let ((start-time (get-internal-run-time)))
                     (loop
                        with i = 0
                        for total-compiled from 0
                        repeat n-functions
                        do
                          (when *compile-print*
                            (when (eql i 0)
                              (write-string ";; ")
                              (finish-output)))
                          (let ((status (mezzano.supervisor:fifo-pop return-fifo)))
                            (when (and (consp status)
                                       (eql (first status) :error))
                              (format t "Error while compiling ~S.~%" (second status))
                              (loop
                                 for worker in workers
                                 do (mezzano.supervisor:terminate-thread worker))
                              (error (third status))))
                          (when *compile-print*
                            (write-char #\.)
                            (finish-output)
                            (incf i)
                            (when (eql i *parallel-compile-file-dots-per-line*)
                              (format t " ~D%~%" (truncate (* total-compiled 100) n-functions))
                              (setf i 0))))
                     (when *compile-print*
                       (fresh-line)
                       (format t ";; Compiled ~:D functions in ~D seconds.~%"
                               n-functions
                               (float (/ (- (get-internal-run-time) start-time)
                                         internal-time-units-per-second)))))
                   (loop
                      repeat n-workers
                      do (mezzano.supervisor:fifo-push :finished work-fifo)))
              (loop
                 for worker in workers
                 do (mezzano.supervisor:terminate-thread worker)))))
        ;; Now write everything to the fasl.
        ;; Do two passes to detect circularity.
        (let ((commands (reverse *llf-forms*)))
          (let ((*llf-dry-run* t))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (save-object o omap (make-broadcast-stream)))))
          (with-open-file (output-stream output-file
                                         :element-type '(unsigned-byte 8)
                                         :if-exists :supersede
                                         :direction :output)
            (write-llf-header output-stream input-file)
            (let ((*llf-dry-run* nil))
              (dolist (cmd commands)
                (dolist (o (cdr cmd))
                  (save-object o omap output-stream))
                (when (car cmd)
                  (write-byte (car cmd) output-stream))))
            (write-byte +llf-end-of-load+ output-stream)
            (when *compile-print*
              (format t ";; Compile-file took ~D seconds.~%"
                      (float (/ (- (get-internal-run-time) start-time)
                                internal-time-units-per-second))))
            (values (truename output-stream) nil nil)))))))

(defun assemble-lap (code &optional name debug-info wired architecture)
  (multiple-value-bind (mc constants fixups symbols gc-data)
      (mezzano.lap:perform-assembly-using-target
       (mezzano.compiler::canonicalize-target architecture)
       code
       :base-address 16
       :initial-symbols '((nil . :fixup)
                          (t . :fixup)
                          (:unbound-value . :fixup)
                          (:symbol-binding-cache-sentinel . :fixup)
                          (:layout-instance-header . :fixup))
       :info (list name debug-info))
    (declare (ignore symbols))
    (let ((fn (make-function sys.int::+object-tag-function+
                             mc fixups constants gc-data wired)))
      (when *fixup-table*
        (setf (gethash fn *fixup-table*) fixups))
      fn)))
