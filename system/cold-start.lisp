;;;; Cold initialization
;;;;
;;;; This code is run when an image is booted for the first time

(in-package :mezzano.internals)

(declaim (special *cold-toplevel-forms*
                  *package-system*
                  *additional-cold-toplevel-forms*
                  *initial-obarray*
                  *initial-keyword-obarray*
                  *initial-fref-obarray*
                  *initial-function-docstrings*)
         (special *terminal-io*
                  *standard-output*
                  *standard-input*
                  *error-output*
                  *trace-output*
                  *debug-io*
                  *query-io*))
(declaim (special *features* *macroexpand-hook*))

;;; Stuff duplicated/reimplemented from stream.lisp.
;;; stream.lisp builds on CLOS, which is not present in the cold image.

(defun write-char (character &optional stream)
  (cold-write-char character stream))

(defun write-string (string &optional stream &key (start 0) end)
  (unless end (setf end (length string)))
  (dotimes (i (- end start))
    (write-char (char string (+ start i)) stream))
  string)

(defun terpri (&optional stream)
  (write-char #\Newline stream)
  nil)

(defun fresh-line (&optional stream)
  (cond ((start-line-p stream)
         nil)
        (t (terpri stream)
           t)))

(defun start-line-p (&optional stream)
  (cold-start-line-p stream))

(defun read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore eof-error-p eof-value recursive-p))
  (cold-read-char stream))

(defun unread-char (character &optional stream)
  (cold-unread-char character stream))

(defun peek-char (&optional peek-type s (eof-error-p t) eof-value recursive-p)
  (declare (ignore eof-error-p eof-value recursive-p))
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

(defun streamp (object)
  (eql object :cold-stream))

(defun %with-stream-editor (stream recursive-p function)
  (declare (ignore stream recursive-p))
  (funcall function))

(defun make-case-correcting-stream (stream case)
  (declare (ignore case))
  stream)

;;; Initial PRINT-OBJECT, replaced when CLOS is loaded.
(defun print-object (object stream)
  (print-unreadable-object (object stream :type t :identity t)))

;;; Pathname stuff before pathnames exist (file.lisp defines real pathnames).

(defun pathnamep (x)
  (declare (ignore x))
  nil)
(defun pathnames-equal (x y)
  (declare (ignore x y))
  nil)
(defun hash-pathname (pathname depth)
  (declare (ignore pathname depth))
  (error "Early call to hash-pathname"))

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

;;; Fake streams & fake stream functions, used by the mini loader to load
;;; multiboot/kboot modules.
(defstruct (mini-vector-stream
             (:constructor mini-vector-stream (vector)))
  vector
  (offset 0))

(defun %read-byte (stream)
  (if (mini-vector-stream-p stream)
      (prog1 (aref (mini-vector-stream-vector stream) (mini-vector-stream-offset stream))
        (incf (mini-vector-stream-offset stream)))
      (read-byte stream)))

(defun %read-sequence (seq stream)
  (cond ((mini-vector-stream-p stream)
         (replace seq (mini-vector-stream-vector stream)
                  :start2 (mini-vector-stream-offset stream)
                  :end2 (+ (mini-vector-stream-offset stream) (length seq)))
         (incf (mini-vector-stream-offset stream) (length seq)))
        (t (read-sequence seq stream))))

;;;; Simple EVAL for use in cold images.
(defun eval-cons (form)
  (case (first form)
    ((if) (if (eval (second form))
              (eval (third form))
              (eval (fourth form))))
    ((function) (if (and (consp (second form)) (eql (first (second form)) 'lambda))
                    (let ((lambda (second form)))
                      (when (second lambda)
                        (error "Not supported: Lambdas with arguments."))
                      (lambda ()
                        (eval `(progn ,@(cddr lambda)))))
                    (fdefinition (second form))))
    ((quote) (second form))
    ((progn) (do ((f (rest form) (cdr f)))
                 ((null (cdr f))
                  (eval (car f)))
               (eval (car f))))
    ((setq) (do ((f (rest form) (cddr f)))
                ((null (cddr f))
                 (setf (symbol-value (car f)) (eval (cadr f))))
              (setf (symbol-value (car f)) (eval (cadr f)))))
    (t (multiple-value-bind (expansion expanded-p)
           (macroexpand form)
         (if expanded-p
             (eval expansion)
             (apply (first form) (mapcar 'eval (rest form))))))))

(defun eval (form)
  (typecase form
    (cons (eval-cons form))
    (symbol (symbol-value form))
    (t form)))

;;; Used during cold-image bringup, various files will redefine packages before
;;; the package system is loaded.

(defvar *deferred-%defpackage-calls*)

(defun %defpackage (&rest arguments)
  (push arguments *deferred-%defpackage-calls*))

(defun keywordp (object)
  (and (symbolp object)
       (eql (symbol-package object) :keyword)))

;;; Needed for IN-PACKAGE before the package system is bootstrapped.
(defun find-package-or-die (name)
  (declare (ignore name))
  t)

;;; Early FIND-CLASS, needed for typep.
(defun find-class (name &optional (errorp t))
  (when errorp
    (error "Early call to FIND-CLASS for ~S" name))
  nil)

;;; Early handler bind
(defun %handler-bind (bindings thunk)
  (declare (ignore bindings))
  (funcall thunk))

(defvar *warm-llf-files*)

(defvar *cold-start-start-time*)
(defvar *cold-start-end-time*)

(defun initialize-lisp ()
  "A grab-bag of things that must be done before Lisp will work properly.
Cold-generator sets up just enough stuff for functions to be called, for
structures to exist, and for memory to be allocated, but not much beyond that."
  (setf *cold-start-start-time* (get-internal-run-time))
  (cold-array-initialization)
  (setf *package* nil
        *terminal-io* :cold-stream
        *standard-output* :cold-stream
        *standard-input* :cold-stream
        *debug-io* :cold-stream
        * nil
        ** nil
        *** nil
        /// nil
        // nil
        / nil
        +++ nil
        ++ nil
        + nil)
  (setf *print-base* 10.
        *print-escape* t
        *print-readably* nil
        *print-safe* nil)
  (setf *features* '(:short-float-is-ieee-half-float
                     :package-local-nicknames
                     :unicode
                     :little-endian
                     #+x86-64 :x86-64
                     #+arm64 :arm64
                     :mezzano
                     :ieee-floating-point
                     :ansi-cl
                     :common-lisp)
        *macroexpand-hook* 'funcall
        most-positive-fixnum #.(- (expt 2 (- 64 +n-fixnum-bits+ 1)) 1)
        most-negative-fixnum #.(- (expt 2 (- 64 +n-fixnum-bits+ 1)))
        *gc-epoch* 0
        *hash-table-unbound-value* (list "unbound hash-table entry")
        *hash-table-tombstone* (list "hash-table tombstone")
        *deferred-%defpackage-calls* '())
  ;; System tables.
  (setf *macros* (make-hash-table :test #'eq :synchronized t :weakness :key))
  (setf *symbol-function-info* (make-hash-table :test #'eq :enforce-gc-invariant-keys t :weakness :key)
        *setf-function-info* (make-hash-table :test #'eq :enforce-gc-invariant-keys t :weakness :key)
        *cas-function-info* (make-hash-table :test #'eq :enforce-gc-invariant-keys t :weakness :key)
        *function-info-lock* (mezzano.supervisor:make-rw-lock '*function-info-lock*))
  (setf *setf-expanders* (make-hash-table :test #'eq :synchronized t :weakness :key))
  (setf *type-info* (make-hash-table :test #'eq :enforce-gc-invariant-keys t :weakness :key)
        *type-info-lock* (mezzano.supervisor:make-rw-lock '*type-info*))
  ;; Put initial classes into the class table.
  (setf mezzano.clos::*class-reference-table* (make-hash-table :test #'eq :enforce-gc-invariant-keys t :weakness :key)
        mezzano.clos::*class-reference-table-lock* (mezzano.supervisor:make-rw-lock 'mezzano.clos::*class-reference-table*))
  (loop
     for (name . class) across mezzano.clos::*initial-class-table*
     do (setf (find-class name) class))
  (write-line "Cold image coming up...")
  ;; Hook FREFs up where required.
  (setf *setf-fref-table* (make-hash-table :synchronized t :weakness :key))
  (setf *cas-fref-table* (make-hash-table :synchronized t :weakness :key))
  (dotimes (i (length *initial-fref-obarray*))
    (let* ((fref (svref *initial-fref-obarray* i))
           (name (function-reference-name fref)))
      (when (consp name)
        (ecase (first name)
          ((setf)
           (setf (gethash (second name) *setf-fref-table*) fref))
          ((cas)
           (setf (gethash (second name) *cas-fref-table*) fref))))))
  ;; Create documentation hash tables.
  ;; FIXME: These should be weak but have structured keys. Need separate hash tables for setf/cas
  (setf *function-documentation* (make-hash-table :test #'equal :synchronized t))
  (setf *compiler-macro-documentation* (make-hash-table :test #'equal :synchronized t))
  (setf *setf-documentation* (make-hash-table :synchronized t :weakness :key))
  (setf *variable-documentation* (make-hash-table :synchronized t :weakness :key))
  (setf *variable-source-locations* (make-hash-table :test 'eq :synchronized t :weakness :key))
  ;; Transfer the initial function documentation over.
  (loop
     for (name doc) in *initial-function-docstrings*
     do (set-function-docstring name doc))
  (makunbound '*initial-function-docstrings*)
  ;; Run toplevel forms.
  (let ((*package* *package*))
    (dotimes (i (length *cold-toplevel-forms*))
      (eval (svref *cold-toplevel-forms* i))))
  ;; Constantify every keyword.
  (dotimes (i (length *initial-obarray*))
    (when (eql (symbol-package (aref *initial-obarray* i)) :keyword)
      (setf (symbol-mode (aref *initial-obarray* i)) :constant)))
  (dolist (sym '(nil t most-positive-fixnum most-negative-fixnum))
    (setf (symbol-mode sym) :constant))
  (mezzano.clos::initialize-clos)
  ;; Pull in the real package system.
  ;; If anything goes wrong before init-package-sys finishes then things
  ;; break in terrible ways.
  (dotimes (i (length *package-system*))
    (eval (svref *package-system* i)))
  (initialize-package-system)
  (dolist (args (reverse *deferred-%defpackage-calls*))
    (apply #'%defpackage args))
  (makunbound '*deferred-%defpackage-calls*)
  (let ((*package* *package*))
    (dotimes (i (length *additional-cold-toplevel-forms*))
      (eval (svref *additional-cold-toplevel-forms* i))))
  ;; Flush the bootstrap stuff.
  (makunbound '*initial-obarray*)
  (makunbound '*package-system*)
  (makunbound '*additional-cold-toplevel-forms*)
  (makunbound '*cold-toplevel-forms*)
  (makunbound '*initial-fref-obarray*)
  (write-line "First GC.")
  (room)
  (gc :full t)
  (room)
  (write-line "Cold load complete.")
  (mezzano.supervisor:snapshot)
  (write-line "Loading warm modules.")
  (let ((*terminal-io* *terminal-io*))
    (dotimes (i (length *warm-llf-files*))
      (write-string "Loading ")
      (write-line (car (aref *warm-llf-files* i)))
      (load-llf (mini-vector-stream (cdr (aref *warm-llf-files* i)))))
    (makunbound '*warm-llf-files*)
    (write-line "Post load GC.")
    (room)
    (gc)
    (room)
    (mezzano.supervisor:snapshot)
    (setf *cold-start-end-time* (get-internal-run-time))
    (format t "Hello, world.~%Cold start took ~:D seconds (~:D seconds of GC time).~%"
            (float (/ (- *cold-start-end-time*
                         *cold-start-start-time*)
                      internal-time-units-per-second))
            *gc-time*)))
