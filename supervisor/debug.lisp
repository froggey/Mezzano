;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Low-level debugging output for the supervisor.

(in-package :mezzano.supervisor)

;; The debug stream functions use line-buffered output, but output can
;; be forced using the DEBUG-FORCE-OUTPUT function.

;; The debug pseudostream must be a function that takes one or two arguments.
;; The first argument is a keyword specifying the operation,
;; the second argument is the data for the operation.
;; Operations:
;;     :read-char     Read a character from the stream.
;;     :clear-input   Clear any available input.
;;     :write-char    Write a character to the stream.
;;     :write-string  Write a string to the stream.
;;     :force-output  Same as CL:FORCE-OUTPUT.
(defvar *debug-pseudostream*)

(defun debug-set-output-pseudostream (pseudostream)
  (setf *debug-pseudostream* pseudostream))

(defun debug-read-char ()
  (when (boundp '*debug-pseudostream*)
    (funcall *debug-pseudostream* :read-char)))

(defun debug-clear-input ()
  (when (boundp '*debug-pseudostream*)
    (funcall *debug-pseudostream* :clear-input)))

(defun debug-write-string (string)
  (when (boundp '*debug-pseudostream*)
    (funcall *debug-pseudostream* :write-string string)))

(defun debug-write-char (char)
  (when (boundp '*debug-pseudostream*)
    (funcall *debug-pseudostream* :write-char char)))

(defun debug-write-line (string)
  (when (boundp '*debug-pseudostream*)
    (funcall *debug-pseudostream* :write-string string)
    (funcall *debug-pseudostream* :write-char #\Newline)
    (funcall *debug-pseudostream* :force-output)))

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

(defun debug-write (thing)
  (cond ((sys.int::character-array-p thing)
         (debug-write-string thing))
        ((symbolp thing)
         (debug-write-string (symbol-name thing)))
        ((sys.int::fixnump thing)
         (debug-write-fixnum thing))
        (t (debug-write-string "#<")
           (debug-write-fixnum (sys.int::lisp-object-address thing))
           (debug-write-string ">"))))

(defun debug-print-line-1 (things)
  (dolist (thing things)
    (debug-write thing))
  (when (boundp '*debug-pseudostream*)
    (funcall *debug-pseudostream* :write-char #\Newline)
    (funcall *debug-pseudostream* :force-output)))

(defun debug-print-line (&rest things)
  (declare (dynamic-extent things))
  (debug-print-line-1 things))

(defun debug-start-line-p ()
  (when (boundp '*debug-pseudostream*)
    (funcall *debug-pseudostream* :start-line-p)))

(defun debug-force-output ()
  (when (boundp '*debug-pseudostream*)
    (funcall *debug-pseudostream* :force-output)))

(defvar *panic-in-progress* nil)

(defun panic-print-backtrace (initial-frame-pointer)
  (let ((*pagefault-hook* (dx-lambda (&rest stuff)
                            (declare (ignore stuff))
                            (debug-print-line "#<truncated>")
                            (return-from panic-print-backtrace))))
    (do ((fp initial-frame-pointer
             (sys.int::memref-signed-byte-64 fp 0)))
        ((eql fp 0))
      (let ((return-address (sys.int::memref-signed-byte-64 fp 1)))
        (when (eql return-address 0)
          (return))
        (debug-write fp)
        (debug-write-char #\Space)
        (debug-write return-address)
        (debug-write-char #\Space)
        ;; Writing the name itself is fraught with danger. Print it in a seperate
        ;; call from the frame-pointer & return address so that a page-fault
        ;; won't abort the whole entry.
        (block nil
          (let* ((*pagefault-hook* (dx-lambda (&rest stuff)
                                     (declare (ignore stuff))
                                     (debug-write-string "#<unknown>")
                                     (return)))
                 (function (sys.int::return-address-to-function return-address))
                 (info (sys.int::%object-header-data function))
                 (mc-size (ldb (byte sys.int::+function-machine-code-size+
                                     sys.int::+function-machine-code-position+)
                               info))
                 ;; First entry in the constant pool.
                 (address (logand (sys.int::lisp-object-address function) -16))
                 (name (sys.int::memref-t address (* mc-size 2))))
            (debug-write name)))
        (debug-print-line)))))

(defun panic (&rest things)
  (declare (dynamic-extent things))
  (panic-1 things nil))

(defun panic-1 (things extra)
  (sys.int::%cli)
  (when (and (boundp '*panic-in-progress*)
             *panic-in-progress*)
    (loop (sys.int::%hlt)))
  ;; Stop the world, just in case printing the backtrace requires paging stuff in.
  (setf *world-stopper* (current-thread)
        *panic-in-progress* t)
  (when (eql *debug-pseudostream* 'debug-serial-stream)
    (setf *debug-pseudostream* 'debug-early-serial-stream))
  (set-panic-light)
  (debug-print-line-1 things)
  (when extra
    (funcall extra))
  (panic-print-backtrace (sys.int::read-frame-pointer))
  (loop (sys.int::%hlt)))

(defmacro ensure (condition &rest things)
  "A simple supervisor-safe ASSERT-like macro."
  `(when (not ,condition)
     (panic ,@things)))

(in-package :sys.int)

(defstruct (cold-stream (:area :wired)))

(in-package :mezzano.supervisor)

(defvar *cold-unread-char*)

(defun sys.int::cold-write-char (c stream)
  (declare (ignore stream))
  (debug-write-char c))

(defun sys.int::cold-start-line-p (stream)
  (declare (ignore stream))
  (debug-start-line-p))

(defun sys.int::cold-read-char (stream)
  (declare (ignore stream))
  (cond (*cold-unread-char*
         (prog1 *cold-unread-char*
           (setf *cold-unread-char* nil)))
        (t (debug-read-char))))

(defun sys.int::cold-unread-char (character stream)
  (declare (ignore stream))
  (when *cold-unread-char*
    (error "Multiple unread-char!"))
  (setf *cold-unread-char* character))

;;; Early error functions, replaced later as part of cold load.

(defun sys.int::assert-error (test-form datum &rest arguments)
  (declare (dynamic-extent arguments))
  (panic "Assert error " datum " " arguments))

(defun sys.int::raise-undefined-function (fref)
  (let ((name (sys.int::%object-ref-t fref sys.int::+fref-name+)))
    (cond ((consp name)
           (panic "Undefined function (" (symbol-name (car name)) " " (symbol-name (car (cdr name))) ")"))
          (t (panic "Undefined function " (symbol-name name))))))

(defun sys.int::raise-unbound-error (symbol)
  (panic "Unbound symbol " (symbol-name symbol)))

(defun error (datum &rest arguments)
  (declare (dynamic-extent arguments))
  (panic "Early ERROR. " datum " " arguments))

(defun enter-debugger (condition)
  (panic "Early enter debugger. " condition))

(defun invoke-debugger (condition)
  (panic "Early invoke debugger. " condition))
