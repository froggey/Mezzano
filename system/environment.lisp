;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; 25. Environment

(in-package :sys.int)

(defvar *site-info* nil
  "Site information, returned by SHORT- and LONG-SITE-NAME.
A list of two elements, the short & long name." )

(defvar *machine-info* nil "Value returned by MACHINE-INSTANCE.")

;;; 25.1.2 Debugging Utilities.

(defgeneric documentation (object doc-type)
  (:argument-precedence-order doc-type object))
(defgeneric (setf documentation) (new-value object doc-type)
  (:argument-precedence-order new-value doc-type object))

(defmethod documentation (x doc-type) nil)
(defmethod (setf documentation) (new-value x doc-type) new-value)

(defun map-apropos (fn string package)
  (setf string (string string))
  (cond (package
         (do-symbols (sym package)
           (when (search string (symbol-name sym) :test #'string-equal)
             (funcall fn sym))))
        (t
         (do-all-symbols (sym)
           (when (search string (symbol-name sym) :test #'string-equal)
             (funcall fn sym))))))

(defun apropos (string &optional package)
  (map-apropos (lambda (sym)
                 (let ((info '()))
                   (when (boundp sym) (push "bound" info))
                   (when (fboundp sym) (push "fbound" info))
                   (if info
                       (format t "~S ~A~%" sym info)
                       (format t "~S~%" sym))))
               string package)
  (values))

(defun apropos-list (string &optional package)
  (let ((syms '()))
    (map-apropos (lambda (sym)
                   (pushnew sym syms))
                 string package)
    syms))

(defun top-level-form-position (pathname tlf)
  (ignore-errors
    (with-open-file (s pathname)
      (loop
         repeat tlf
         do (with-standard-io-syntax
              (let ((*read-suppress* t)
                    (*read-eval* nil))
                (read s nil))))
      (1+ (file-position s)))))

(defun ed-function-location (function))

(defparameter *ed-hook* nil)

(defun ed (&optional x)
  "ED is the standard editor."
  (assert *ed-hook* (*ed-hook*) "No editor configured")
  (when (typep x 'function-name)
    (setf x (fdefinition x)))
  (etypecase x
    (null
     (funcall *ed-hook*))
    ((or pathname string)
     (funcall *ed-hook* :initial-pathname (pathname x)))
    (function
     (let* ((info (function-debug-info x))
            (pathname (debug-info-source-pathname info))
            (tlf (debug-info-source-top-level-form-number info)))
       (funcall *ed-hook*
                :initial-pathname (if pathname
                                      (translate-logical-pathname pathname)
                                      nil)
                :initial-position (top-level-form-position pathname tlf)))))
  (values))

(defparameter *inspect-hook* nil)

(defun inspect (object)
  (assert *inspect-hook* (*inspect-hook*) "No inspector configured")
  (funcall *inspect-hook* object))

(defun dribble (&optional pathname)
  (if pathname
      (with-open-stream (stream (open pathname :direction :io :if-exists :new-version))
        (with-simple-restart (finish-dribble "Exit DRIBBLE")
          (let ((*standard-output* (make-broadcast-stream stream *standard-output*))
                (*standard-input* (make-echo-stream *standard-input* stream)))
            (repl))))
      (let ((restart (find-restart 'finish-dribble)))
        (when restart
          (invoke-restart restart)))))

;;; 25.1.1 Top Level Loop.

(declaim (special * ** ***
                  + ++ +++
                  / // ///
                  -))

(defun repl ()
  (let ((* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil)
        (+ nil) (++ nil) (+++ nil)
        (- nil))
    (loop
       (with-simple-restart (abort "Return to READ-EVAL-PRINT loop.")
         (fresh-line)
         (format t "~A> " (package-shortest-name *package*))
         (let ((form (read)))
           (fresh-line)
           (let ((result (multiple-value-list (let ((- form))
                                                (eval form)))))
             (setf *** **
                   ** *
                   * (first result)
                   /// //
                   // /
                   / result
                   +++ ++
                   ++ +
                   + form)
             (when result
               (dolist (v result)
                 (fresh-line)
                 (write v)))))))))

;;; 25.1.3 Environment Inquiry.

(defun lisp-implementation-type ()
  "Mezzano")

(defvar *git-revision*) ; Set by the cold generator.
(defvar *lisp-implementation-version* "devel")

(defun lisp-implementation-version ()
  (if *git-revision*
      (format nil "~A ~A" *lisp-implementation-version* *git-revision*)
      *lisp-implementation-version*))

(defun short-site-name () (first *site-info*))
(defun long-site-name () (second *site-info*))

;; (instance)
(defun machine-instance () *machine-info*)
(defun machine-type ()
  #+x86-64 "x86-64"
  #+arm64 "arm64")
(defun machine-version ()
  (multiple-value-bind (cpuid-max vendor-1 vendor-3 vendor-2)
      (cpuid 0)
    (declare (ignore cpuid-max))
    (decode-cpuid-vendor vendor-1 vendor-2 vendor-3)))

;;; Mezzano uses no supporting software.
(defun software-type () nil)
(defun software-version () nil)
