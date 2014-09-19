;;; Low-level debugging code for the supervisor.

(in-package :mezzanine.supervisor)

;; The debug stream functions use line-buffered output, but output can
;; be forced using the DEBUG-FORCE-OUTPUT function.

;; The debug pesudostream must be a function that takes one or two arguments.
;; The first argument is a keyword specifying the operation,
;; the second argument is the data for the operation.
;; Operations:
;;     :read-char     Read a character from the stream.
;;     :clear-input   Clear any available input.
;;     :write-char    Write a character to the stream.
;;     :write-string  Write a string to the stream.
;;     :force-output  Same as CL:FORCE-OUTPUT.
(defvar *debug-pesudostream*)

(defun debug-set-output-pesudostream (pesudostream)
  (setf *debug-pesudostream* pesudostream))

(defun debug-read-char ()
  (when (boundp '*debug-pesudostream*)
    (funcall *debug-pesudostream* :read-char)))

(defun debug-clear-input ()
  (when (boundp '*debug-pesudostream*)
    (funcall *debug-pesudostream* :clear-input)))

(defun debug-write-string (string)
  (when (boundp '*debug-pesudostream*)
    (funcall *debug-pesudostream* :write-string string)))

(defun debug-write-char (char)
  (when (boundp '*debug-pesudostream*)
    (funcall *debug-pesudostream* :write-char char)))

(defun debug-write-line (string)
  (when (boundp '*debug-pesudostream*)
    (funcall *debug-pesudostream* :write-string string)
    (funcall *debug-pesudostream* :write-char #\Newline)
    (funcall *debug-pesudostream* :force-output)))

;;; Print a negative fixnum. Use negative numbers to avoid problems
;;; near most-negative-fixnum.
(defun debug-write-fixnum-1 (fixnum base)
  (unless (zerop fixnum)
    (multiple-value-bind (quot rem)
        (truncate fixnum base)
      (debug-write-fixnum-1 quot base)
      (debug-write-char (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" (- rem))))))

(defun debug-write-fixnum (fixnum &optional (base 16))
  (cond ((plusp fixnum)
         (debug-write-fixnum-1 (- fixnum) base))
        ((minusp fixnum)
         (debug-write-char #\-)
         (debug-write-fixnum-1 fixnum base))
        (t (debug-write-char #\0))))

(defun debug-print-line (&rest things)
  (declare (dynamic-extent things))
  (dolist (thing things)
    (cond ((sys.int::character-array-p thing)
           (debug-write-string thing))
          ((sys.int::fixnump thing)
           (debug-write-fixnum thing))
          (t (debug-write-string "#<")
             (debug-write-fixnum (sys.int::lisp-object-address thing))
             (debug-write-string ">"))))
  (when (boundp '*debug-pesudostream*)
    (funcall *debug-pesudostream* :write-char #\Newline)
    (funcall *debug-pesudostream* :force-output)))

(defun debug-start-line-p ()
  (when (boundp '*debug-pesudostream*)
    (funcall *debug-pesudostream* :start-line-p)))

(defun debug-force-output ()
  (when (boundp '*debug-pesudostream*)
    (funcall *debug-pesudostream* :force-output)))
