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

(defvar *lisp-implementation-version* "devel")

(defun lisp-implementation-version ()
  *lisp-implementation-version*)

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
