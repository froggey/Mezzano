(in-package #:system.internals)

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

;; TODO: Create macrofunctions correctly, instead of using the host's destructuring-bind
(defun make-macrolet-env (defs env)
  "Return a new environment containing the macro definitions."
  (dolist (d defs)
    (push (list :macro (first d) (eval (expand-macrolet-function d))) env))
  env)

(defun make-symbol-macrolet-env (defs env)
  (dolist (d defs)
    (destructuring-bind (symbol expansion) d
      (push (list :symbol-macro symbol (lambda (whole env)
                                         (declare (ignore whole env))
                                         expansion))
            env)))
  env)

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

(defun compile-file-default-output (input-file)
  (make-pathname :type "llf" :defaults input-file))

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
(defconstant +llf-setf-fdefinition+ #x0B)
(defconstant +llf-simple-vector+ #x0C)
(defconstant +llf-character+ #x0D)
(defconstant +llf-structure-definition+ #x0E)

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
          (t (error "TODO character ~S." character)))))

(defgeneric save-one-object (object object-map stream))

(defmethod save-one-object ((object function) omap stream)
  (write-byte +llf-function+ stream)
  (write-byte (function-tag object) stream)
  (save-integer (function-code-size object) stream)
  (dotimes (i (function-code-size object))
    (write-byte (function-code-byte object i) stream))
  (save-object (function-fixups object) omap stream)
  (save-integer (function-pool-size object) stream)
  (dotimes (i (function-pool-size object))
    (save-object (function-pool-object object i) omap stream)))

(defmethod save-one-object ((object cons) omap stream)
  (write-byte +llf-cons+ stream)
  (save-object (cdr object) omap stream)
  (save-object (car object) omap stream))

(defmethod save-one-object ((object symbol) omap stream)
  (cond ((symbol-package object)
         (write-byte +llf-symbol+ stream)
         (save-integer (length (symbol-name object)) stream)
         (dotimes (i (length (symbol-name object)))
           (save-character (char (symbol-name object) i) stream))
         (save-integer (length (package-name (symbol-package object))) stream)
         (dotimes (i (length (package-name (symbol-package object))))
           (save-character (char (package-name (symbol-package object)) i) stream)))
        ((get object 'setf-symbol-backlink)
         (write-byte +llf-setf-symbol+ stream)
         (save-object (get object 'setf-symbol-backlink) omap stream))
        (t (write-byte +llf-uninterned-symbol+ stream)
           (save-integer (length (symbol-name object)) stream)
           (dotimes (i (length (symbol-name object)))
             (save-character (char (symbol-name object) i) stream))
           ;; Should save flags?
           (if (boundp object)
               (save-object (symbol-value object) omap stream)
               (write-byte +llf-unbound+ stream))
           (if (fboundp object)
               (save-object (symbol-function object) omap stream)
               (write-byte +llf-unbound+ stream))
           (save-object (symbol-plist object) omap stream))))

(defmethod save-one-object ((object string) omap stream)
  (write-byte +llf-string+ stream)
  (save-integer (length object) stream)
  (dotimes (i (length object))
    (save-character (char object i) stream)))

(defmethod save-one-object ((object integer) omap stream)
  (write-byte +llf-integer+ stream)
  (save-integer object stream))

(defmethod save-one-object ((object vector) omap stream)
  (write-byte +llf-simple-vector+ stream)
  (save-integer (length object) stream)
  (dotimes (i (length object))
    (save-object (aref object i) omap stream)))

(defmethod save-one-object ((object character) omap stream)
  (write-byte +llf-character+ stream)
  (save-character object stream))

(defmethod save-one-object ((object structure-definition) omap stream)
  (write-byte +llf-structure-definition+ stream)
  (save-object (structure-name object) omap stream)
  (save-object (structure-slots object) omap stream))

(defun save-object (object omap stream)
  (let ((id (gethash object omap)))
    (when id
      (write-byte +llf-backlink+ stream)
      (save-integer id stream)
      (return-from save-object))
    (setf (gethash object omap) (hash-table-count omap))
    (save-one-object object omap stream)))

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
         (write-byte +llf-setf-fdefinition+ stream)
         (save-object (compile nil (second (third form))) omap stream)
         (save-object (second (fourth form)) omap stream)
         t)
        ((and (listp form)
              (>= (list-length form) 3)
              (eql (first form) 'define-lap-function)
              (listp (third form)))
         ;; FORM looks like (DEFINE-LAP-FUNCTION name (options) code...)
         (unless (= (list-length (third form)) 0)
           (error "TODO: DEFINE-LAP-FUNCTION with options."))
         (write-byte +llf-setf-fdefinition+ stream)
         (save-object (assemble-lap (cdddr form) (second form)) omap stream)
         (save-object (second form) omap stream)
         t)
        ((and (listp form)
              (= (list-length form 2))
              (eql (first form) 'quote))
         t)))

(defun compile-file (input-file &key
                     (output-file (compile-file-default-output input-file))
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
      (let ((*package* *package*)
            (*readtable* *readtable*)
            (*compile-verbose* verbose)
            (*compile-print* print)
            (omap (make-hash-table))
            (eof-marker (cons nil nil)))
        (do ((form (read input-stream nil eof-marker)
                   (read input-stream nil eof-marker)))
            ((eql form eof-marker))
          (format t ";; Compiling form ~S.~%" form)
          ;; TODO: Deal with lexical environments.
          (handle-top-level-form form
                                 (lambda (f env)
                                   (assert (not env))
                                   (or (fastload-form f omap output-stream)
                                       (progn (write-byte +llf-invoke+ output-stream)
                                              (save-object (compile nil `(lambda () (progn ,f)))
                                                           omap output-stream))))
                                 (lambda (f env)
                                   (assert (not env))
                                   (eval f))))
        (write-byte +llf-end-of-load+ output-stream)))))

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
    (code-char value)))

(defun load-ub8-vector (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type '(unsigned-byte 8))))
    (read-sequence seq stream)
    seq))

(defun load-vector (stream omap)
  (let* ((len (load-integer stream))
         (seq (make-array len)))
    (dotimes (i len)
      (setf (aref seq i) (load-object stream omap)))
    seq))

(defun load-string (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type 'character)))
    (dotimes (i len)
      (setf (aref seq i) (load-character stream)))
    seq))

(defun llf-next-is-unbound-p (stream)
  (let ((current-position (file-position stream)))
    (cond ((eql (read-byte stream) +llf-unbound+)
           t)
          (t (file-position stream current-position)
             nil))))

(defun load-one-object (command stream omap)
  (ecase command
    (#.+llf-function+
     (let* ((tag (read-byte stream))
            (mc (load-ub8-vector stream))
            (fixups (load-object stream omap))
            (constants (load-vector stream omap)))
       (make-function-with-fixups tag mc fixups constants)))
    (#.+llf-cons+
     (let* ((cdr (load-object stream omap))
            (car (load-object stream omap)))
       (cons car cdr)))
    (#.+llf-symbol+
     (let* ((name (load-string stream))
            (package (load-string stream)))
       (intern name package)))
    (#.+llf-uninterned-symbol+
     (let* ((name (load-string stream))
            (sym (make-symbol name)))
       (unless (llf-next-is-unbound-p stream)
         (setf (symbol-value sym) (load-object stream omap)))
       (unless (llf-next-is-unbound-p stream)
         (setf (symbol-function sym) (load-object stream omap)))
       (setf (symbol-plist sym) (load-object stream omap))
       sym))
    (#.+llf-string+ (load-string stream))
    (#.+llf-setf-symbol+
     (let ((symbol (load-object stream omap)))
       (function-symbol `(setf ,symbol))))
    (#.+llf-integer+ (load-integer stream))
    (#.+llf-simple-vector+ (load-vector stream omap))
    (#.+llf-character+ (load-character stream))
    (#.+llf-structure-definition+
     (make-struct-type (load-object stream omap)
                       (load-object stream omap)))))

(defun load-object (stream omap)
  (let ((command (read-byte stream)))
    (case command
      (#.+llf-end-of-load+
       (values nil t))
      (#.+llf-backlink+
       (let ((id (load-integer stream)))
         (assert (< id (hash-table-count omap)) () "Object id ~S out of bounds." id)
         (values (gethash id omap) nil)))
      (#.+llf-invoke+
       (values (funcall (load-object stream omap)) nil))
      (#.+llf-setf-fdefinition+
       (let ((fn (load-object stream omap))
             (name (load-object stream omap)))
         (funcall #'(setf fdefinition) fn name)))
      (#.+llf-unbound+ (error "Should not seen UNBOUND here."))
      (t (let ((id (hash-table-count omap)))
           (setf (gethash id omap) '"!!!LOAD PLACEHOLDER!!!")
           (let ((obj (load-one-object command stream omap)))
             (setf (gethash id omap) obj)
             (values obj nil)))))))

(defun load-llf (stream)
  (check-llf-header stream)
  (let ((object-map (make-hash-table))
        (*package* *package*))
    (loop (multiple-value-bind (object stop)
              (load-object stream object-map)
            (declare (ignore object))
            (when stop
              (return))))))

(defvar *load-verbose* t)
(defvar *load-print* nil)

(defun load-from-stream (stream)
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
           (load-from-stream filespec))
          (t (with-open-file (stream filespec
                                     :if-does-not-exist if-does-not-exist
                                     :external-format external-format)
               (load-from-stream stream))))))
