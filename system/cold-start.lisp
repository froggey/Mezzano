;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(declaim (special *cold-toplevel-forms*
                  *package-system*
                  *additional-cold-toplevel-forms*
                  *initial-obarray*
                  *initial-keyword-obarray*
                  *initial-fref-obarray*
                  *initial-structure-obarray*)
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

(defun %with-stream-editor (stream recursive-p function)
  (funcall function))

(defun make-case-correcting-stream (stream case)
  stream)

;;; Initial PRINT-OBJECT, replaced when CLOS is loaded.
(defun print-object (object stream)
  (print-unreadable-object (object stream :type t :identity t)))

;;; Pathname stuff before pathnames exist (file.lisp defines real pathnames).

(defun pathnamep (x) nil)
(defun pathnames-equal (x y) nil)

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

(defvar *deferred-%defpackage-calls* '())

(defun %defpackage (&rest arguments)
  (push arguments *deferred-%defpackage-calls*))

(defun keywordp (object)
  (and (symbolp object)
       (eql (symbol-package object) :keyword)))

;;; Needed for IN-PACKAGE before the package system is bootstrapped.
(defun find-package-or-die (name)
  t)

(defvar *warm-llf-files*)

(defun initialize-lisp ()
  "A grab-bag of things that must be done before Lisp will work properly.
Cold-generator sets up just enough stuff for functions to be called, for
structures to exist, and for memory to be allocated, but not much beyond that."
  (cold-array-initialization)
  (setf *package* nil
        *cold-stream* (make-cold-stream)
        *terminal-io* *cold-stream*
        *standard-output* *cold-stream*
        *standard-input* *cold-stream*
        *debug-io* *cold-stream*
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
  (setf *features* '(:package-local-nicknames
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
        most-negative-fixnum #.(- (expt 2 (- 64 +n-fixnum-bits+ 1))))
  ;; Wire up all the structure types.
  (setf (get 'sys.int::structure-definition 'sys.int::structure-type) sys.int::*structure-type-type*)
  (setf (get 'sys.int::structure-slot-definition 'sys.int::structure-type) sys.int::*structure-slot-type*)
  (dotimes (i (length *initial-structure-obarray*))
    (let ((defn (svref *initial-structure-obarray* i)))
      (setf (get (structure-name defn) 'structure-type) defn)))
  (write-line "Cold image coming up...")
  ;; Hook FREFs up where required.
  (dotimes (i (length *initial-fref-obarray*))
    (let* ((fref (svref *initial-fref-obarray* i))
           (name (%object-ref-t fref +fref-name+)))
      (when (consp name)
        (ecase (first name)
          ((setf)
           (setf (get (second name) 'setf-fref) fref))
          ((cas)
           (setf (get (second name) 'cas-fref) fref))))))
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
  (makunbound '*initial-structure-obarray*)
  (write-line "First GC.")
  (room)
  (gc)
  (room)
  (write-line "Cold load complete.")
  (mezzano.supervisor:snapshot)
  (write-line "Loading warm modules.")
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
  (write-line "Hello, world.")
  (terpri))
