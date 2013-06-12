(in-package :sys.int)

(defvar *top-level-form-number* nil)

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
                                                (list `(,env-binding ,env)))))))))

(defun make-macrolet-env (defs env)
  "Return a new environment containing the macro definitions."
  (list* (list* :macros (mapcar (lambda (def)
                                  (cons (first def)
                                        (eval (expand-macrolet-function def))))
                                defs))
         env))

(defun make-symbol-macrolet-env (defs env)
  (cons (list* :symbol-macros defs) env))

(defun handle-top-level-implicit-progn (forms load-fn eval-fn mode env)
  (dolist (f forms)
    (handle-top-level-form f load-fn eval-fn mode env)))

(defun handle-top-level-lms-body (forms load-fn eval-fn mode env)
  "Common code for handling the body of LOCALLY, MACROLET and SYMBOL-MACROLET forms at the top-level."
  (multiple-value-bind (body declares)
      (parse-declares forms)
    (dolist (dec declares)
      (when (eql 'special (first dec))
        (push (list* :special (rest dec)) env)))
    (handle-top-level-implicit-progn body load-fn eval-fn mode env)))

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
  (let ((expansion (macroexpand form env)))
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
  ;; TODO: write the source file name out as well.
  (write-sequence #(#x4C #x4C #x46 #x00) output-stream)) ; LLF\x00

(defun check-llf-header (stream)
  (assert (and (eql (read-byte stream) #x4C)
               (eql (read-byte stream) #x4C)
               (eql (read-byte stream) #x46)
               (eql (read-byte stream) #x00))))

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
  (dotimes (i (function-pool-size object))
    (save-object (function-pool-object object i) omap stream))
  ;; FIXME: This should be the fixup list.
  (save-object nil omap stream)
  (write-byte +llf-function+ stream)
  (write-byte (function-tag object) stream)
  (save-integer (- (function-code-size object) 12) stream)
  (save-integer (function-pool-size object) stream)
  (dotimes (i (- (function-code-size object) 12))
    (write-byte (function-code-byte object (+ i 12)) stream)))

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
        ((get object 'setf-symbol-backlink)
         (save-object (get object 'setf-symbol-backlink) omap stream)
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
  (dotimes (i (length object))
    (save-object (aref object i) omap stream))
  (write-byte +llf-simple-vector+ stream)
  (save-integer (length object) stream))

(defmethod save-one-object ((object character) omap stream)
  (write-byte +llf-character+ stream)
  (save-character object stream))

(defmethod save-one-object ((object structure-definition) omap stream)
  (save-object (structure-name object) omap stream)
  (save-object (structure-slots object) omap stream)
  (save-object (structure-parent object) omap stream)
  (save-object (structure-area object) omap stream)
  (write-byte +llf-structure-definition+ stream))

(defmethod save-one-object ((object float) omap stream)
  (write-byte +llf-single-float+ stream)
  (save-integer (%single-float-as-integer object) stream))

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
  (dotimes (i (array-total-size array))
    (save-object (row-major-aref array i) omap stream))
  (write-byte +llf-array+ stream)
  (save-integer (array-rank object) stream)
  (dolist (dim (array-dimensions object))
    (save-integer dim stream)))

(defun save-object (object omap stream)
  (when (null (gethash object omap))
    (setf (gethash object omap) (list (hash-table-count omap) 0 nil)))
  (let ((info (gethash object omap)))
    (cond (*llf-dry-run*
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
  (push (list* action objects) *llf-forms*))

(defun fastload-form (form omap stream)
  (cond ((and (listp form)
              (= (list-length form) 4)
              (eql (first form) 'funcall)
              (equal (second form) '(function (setf fdefinition)))
              (listp (third form))
              (= (list-length (third form)) 2)
              (eql (first (third form)) 'function)
              (listp (second (third form)))
              (eql (first (second (third form))) 'lambda)
              (listp (third form))
              (= (list-length (fourth form)) 2)
              (eql (first (fourth form)) 'quote))
         ;; FORM looks like (FUNCALL #'(SETF FDEFINITION) #'(LAMBDA ...) 'name)
         (add-to-llf +llf-setf-fdefinition+
                     (compile nil (second (third form)))
                     (second (fourth form)))
         t)
        ((and (listp form)
              (>= (list-length form) 3)
              (eql (first form) 'define-lap-function)
              (listp (third form)))
         ;; FORM looks like (DEFINE-LAP-FUNCTION name (options) code...)
         (unless (= (list-length (third form)) 0)
           (error "TODO: DEFINE-LAP-FUNCTION with options."))
         (add-to-llf +llf-setf-fdefinition+
                     (assemble-lap (cdddr form) (second form))
                     (second form))
         t)
        ((and (listp form)
              (= (list-length form) 2)
              (eql (first form) 'quote))
         t)))

(defun compile-file (input-file &key
                     (output-file (compile-file-pathname input-file))
                     (verbose *compile-verbose*)
                     (print *compile-print*)
                     (external-format :default))
  (with-open-file (input-stream input-file :external-format external-format)
    (with-open-file (output-stream output-file
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede
                     :direction :output)
      (format t ";; Compiling file ~S.~%" input-file)
      (write-llf-header output-stream input-file)
      (let* ((*package* *package*)
             (*readtable* *readtable*)
             (*compile-verbose* verbose)
             (*compile-print* print)
             (*llf-forms* nil)
             (omap (make-hash-table))
             (eof-marker (cons nil nil))
             (*compile-file-pathname* (pathname (merge-pathnames input-file)))
             (*compile-file-truename* (truename *compile-file-pathname*))
             (*top-level-form-number* 0))
        (do ((form (read input-stream nil eof-marker)
                   (read input-stream nil eof-marker)))
            ((eql form eof-marker))
          (let ((*print-length* 3)
                (*print-level* 3))
            (declare (special *print-length* *print-level*))
            (format t ";; Compiling form ~S.~%" form))
          ;; TODO: Deal with lexical environments.
          (handle-top-level-form form
                                 (lambda (f env)
                                   (or (fastload-form f omap output-stream)
                                       (add-to-llf +llf-invoke+
                                                   (compile nil `(lambda () (progn ,f))))))
                                 (lambda (f env)
                                   (sys.eval::eval-in-lexenv f env)))
          (incf *top-level-form-number*))
        ;; Now write everything to the fasl.
        ;; Do two passes to detect circularity.
        (let ((commands (reverse *llf-forms*)))
          (let ((*llf-dry-run* t))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (save-object o omap (make-broadcast-stream)))))
          (let ((*llf-dry-run* nil))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (save-object o omap output-stream))
              (write-byte (car cmd) output-stream))))
        (write-byte +llf-end-of-load+ output-stream))
      (values (truename output-stream) nil nil))))

(defmacro with-compilation-unit ((&key override) &body body)
  `(progn ,override ,@body))

(defun save-compiler-builtins (path)
  (with-open-file (output-stream path
                   :element-type '(unsigned-byte 8)
                   :if-exists :supersede
                   :direction :output)
    (format t ";; Writing compiler builtins to ~A.~%" path)
    (write-llf-header output-stream path)
    (let ((omap (make-hash-table))
          (builtins (sys.c::generate-builtin-functions)))
      (dolist (b builtins)
        (let ((form `(funcall #'(setf fdefinition) #',(second b) ',(first b))))
          (let ((*print-length* 3)
                (*print-level* 3))
            (declare (special *print-length* *print-level*))
            (format t ";; Compiling form ~S.~%" form))
          (or (fastload-form form omap output-stream)
              (error "Could not fastload builtin."))))
      (write-byte +llf-end-of-load+ output-stream))))
