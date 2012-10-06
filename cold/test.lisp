(in-package #:sys.int)

(declaim (special *cold-toplevel-forms*
                  *initial-obarray*
                  *initial-keyword-obarray*
                  *initial-setf-obarray*
                  *initial-structure-obarray*)
         (special *terminal-io*
                  *standard-output*
                  *standard-input*
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

(defun raise-undefined-function (invoked-through &rest args)
  (declare (ignore args))
  (write-char #\!)
  (write-char #\#)
  (when (symbolp invoked-through)
    (write-string (symbol-name invoked-through)))
  (loop (%hlt)))

(defun raise-unbound-error (symbol)
  (write-char #\!)
  (write-char #\*)
  (write-string (symbol-name symbol))
  (loop (%hlt)))

(defun raise-type-error (datum expected-type)
  (write-char #\!)
  (write-char #\$)
  (write expected-type)
  (write-char #\Space)
  (write datum)
  (loop (%hlt)))

(defun error (datum &rest arguments)
  (write-char #\!)
  (write datum)
  (write-char #\Space)
  (write arguments)
  (loop (%hlt)))

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
	  (equal (cdr x) (cdr y))))))

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

(defun endp (list)
  (cond ((null list) t)
        ((consp list) nil)
        (t (error 'type-error
                  :datum list
                  :expected-type 'list))))

(defun find-package-or-die (name)
  nil)
(defun find-package (name)
  nil)
(defun keywordp (object)
  (find object *initial-keyword-obarray*))
(defun find-symbol (name &optional package)
  (let ((pkg (if (string= package "KEYWORD")
                 *initial-keyword-obarray*
                 *initial-obarray*)))
    (dotimes (i (length pkg) (values nil nil))
      (when (string= name (symbol-name (aref pkg i)))
        (return (values (aref pkg i) :internal))))))
(defun intern (name &optional package)
  (let ((pkg (if (string= package "KEYWORD")
                 *initial-keyword-obarray*
                 *initial-obarray*)))
    (dotimes (i (length pkg)
              (let ((sym (make-symbol (string name))))
                (setf (symbol-package sym) (if (string= package "KEYWORD") :keyword t))
                (vector-push-extend sym pkg)
                (values sym nil)))
      (when (string= name (symbol-name (aref pkg i)))
        (return (values (aref pkg i) :internal))))))

;;; TODO: This requires a considerably more flexible mechanism.
;;; 12 is where the TLS slots in a stack group start.
(defparameter *next-symbol-tls-slot* 12)
(defconstant +maximum-tls-slot+ 512)
(defun %allocate-tls-slot (symbol)
  (when (>= *next-symbol-tls-slot* +maximum-tls-slot+)
    (error "Critial error! TLS slots exhausted!"))
  (let ((slot *next-symbol-tls-slot*))
    (incf *next-symbol-tls-slot*)
    (setf (ldb (byte 16 8) (%symbol-flags symbol)) slot)
    slot))

(defun proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (special (dolist (var (rest declaration-specifier))
               (setf (system:symbol-mode var) :special)))
    (constant (dolist (var (rest declaration-specifier))
                (setf (system:symbol-mode var) :constant)))))

(defun system:symbol-mode (symbol)
  (svref #(nil :special :constant :symbol-macro)
         (ldb (byte 2 0) (%symbol-flags symbol))))

(defun (setf system:symbol-mode) (value symbol)
  (setf (ldb (byte 2 0) (%symbol-flags symbol))
        (ecase value
          ((nil) +symbol-mode-nil+)
          ((:special) +symbol-mode-special+)
          ((:constant) +symbol-mode-constant+)
          ((:symbol-macro) +symbol-mode-symbol-macro+)))
  value)

(defun %defmacro (name function)
  (funcall #'(setf macro-function) function name))

(defun macro-function (symbol &optional env)
  (dolist (e env
           (get symbol '%macro-function))
    (when (eql (first e) :macros)
      (let ((fn (assoc symbol (rest e))))
        (when fn (return (cdr fn)))))))

(defun (setf macro-function) (value symbol &optional env)
  (when env
    (error "TODO: (Setf Macro-function) in environment."))
  (setf (symbol-function symbol) (lambda (&rest r)
                                   (declare (ignore r))
                                   (error 'undefined-function :name symbol))
        (get symbol '%macro-function) value))

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

(defun list (&rest args)
  args)

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
        *terminal-io* nil
        *standard-output* nil
        *standard-input* nil
        *screen-offset* 0
        *keyboard-shifted* nil
        *early-initialize-hook* '()
        *initialize-hook* '())
  ;; Initialize defstruct and patch up all the structure types.
  (bootstrap-defstruct)
  (dotimes (i (length *initial-structure-obarray*))
    (setf (%struct-slot (svref *initial-structure-obarray* i) 0) *structure-type-type*))
  (write-line "Hello, world.")
  (setf *print-base* 10.
        *print-escape* t
        *print-readably* nil)
  ;; Hook SETF symbols up.
  (dotimes (i (length *initial-setf-obarray*))
    (let* ((sym (svref *initial-setf-obarray* i))
           (other (find (symbol-name sym) *initial-obarray* :test 'string= :key 'symbol-name)))
      (assert other)
      (setf (get sym 'setf-symbol-backlink) other
            (get other 'setf-symbol) sym)))
  (dotimes (i (length *cold-toplevel-forms*))
    (funcall (svref *cold-toplevel-forms* i)))
  (setf *initial-obarray* (make-array (length *initial-obarray*)
                                      :fill-pointer t :adjustable t
                                      :initial-contents *initial-obarray*))
  (setf *initial-keyword-obarray* (make-array (length *initial-keyword-obarray*)
                                              :fill-pointer t :adjustable t
                                              :initial-contents *initial-keyword-obarray*))
  (terpri)
  (write-char #\*)
  (write-char #\O)
  (write-char #\K)
  (repl)
  (loop (%hlt)))
