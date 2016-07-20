;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
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

(defmacro call-debug-pseudostream (command &rest arguments)
  `(when (sys.int::symbol-global-boundp '*debug-pseudostream*)
     (funcall (sys.int::symbol-global-value '*debug-pseudostream*)
              ,command ,@arguments)))

(defun debug-read-char ()
  (call-debug-pseudostream :read-char))

(defun debug-clear-input ()
  (call-debug-pseudostream :clear-input))

(defun debug-write-string (string)
  (call-debug-pseudostream :write-string string))

(defun debug-write-char (char)
  (call-debug-pseudostream :write-char char))

(defun debug-write-line (string)
  (call-debug-pseudostream :write-string string)
  (call-debug-pseudostream :write-char #\Newline)
  (call-debug-pseudostream :force-output))

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

(defun debug-write (thing &optional (depth 0))
  (when (> depth 10)
    (debug-write-string "#<recursion-limit-exceeded>")
    (return-from debug-write))
  (cond ((sys.int::character-array-p thing)
         (debug-write-string thing))
        ((symbolp thing)
         (debug-write-string (symbol-name thing)))
        ((sys.int::fixnump thing)
         (debug-write-fixnum thing))
        ((consp thing)
         (debug-write-string "(")
         (debug-write (first thing) (1+ depth))
         (do ((i (rest thing) (rest i)))
             ((not (consp i))
              (when i
                (debug-write-string " . ")
                (debug-write i (1+ depth)))
              (debug-write-string ")"))
           (debug-write-string " ")
           (debug-write (first i) (1+ depth))))
        (t (debug-write-string "#<")
           (debug-write-fixnum (sys.int::lisp-object-address thing))
           (debug-write-string ">"))))

(defun debug-print-line-1 (things)
  (dolist (thing things)
    (debug-write thing))
  (call-debug-pseudostream :write-char #\Newline)
  (call-debug-pseudostream :force-output))

(defun debug-print-line (&rest things)
  (declare (dynamic-extent things))
  (debug-print-line-1 things))

(defun debug-start-line-p ()
  (call-debug-pseudostream :start-line-p))

(defun debug-force-output ()
  (call-debug-pseudostream :force-output))

(defvar *panic-in-progress* nil)

(defun panic-print-backtrace (initial-frame-pointer)
  (with-page-fault-hook
      (()
       (debug-print-line "#<truncated>")
       (return-from panic-print-backtrace))
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
          (with-page-fault-hook
              (()
               (debug-write-string "#<unknown>")
               (return))
            (let* ((function (sys.int::return-address-to-function return-address))
                   (info (sys.int::%object-header-data function))
                   (mc-size (ldb (byte sys.int::+function-machine-code-size+
                                       sys.int::+function-machine-code-position+)
                                 info))
                   ;; First entry in the constant pool.
                   (address (logand (sys.int::lisp-object-address function) -16))
                   (name (sys.int::memref-t address (* mc-size 2))))
              (debug-write name))))
        (debug-print-line)))))

(defun panic (&rest things)
  (declare (dynamic-extent things))
  (panic-1 things nil))

(defun panic-1 (things extra)
  (sys.int::%cli)
  (when (and (sys.int::symbol-global-boundp '*panic-in-progress*)
             (sys.int::symbol-global-value '*panic-in-progress*))
    (loop (sys.int::%hlt)))
  ;; Stop the world, just in case printing the backtrace requires paging stuff in.
  (setf (sys.int::symbol-global-value '*world-stopper*) (current-thread)
        (sys.int::symbol-global-value '*panic-in-progress*) t)
  (debug-force-output)
  (set-panic-light)
  (disable-page-fault-ist)
  (debug-print-line-1 things)
  (when extra
    (funcall extra))
  (panic-print-backtrace (sys.int::read-frame-pointer))
  (do ((thread (sys.int::symbol-global-value '*all-threads*)
               (thread-global-next thread)))
      ((null thread))
    (when (not (eql thread (current-thread)))
      (debug-print-line "----------")
      (debug-print-line "Thread " thread)
      (panic-print-backtrace (thread-frame-pointer thread))))
  (loop (sys.int::%hlt)))

(defun debug-dump-threads ()
  (debug-print-line "Thread " (current-thread))
  (panic-print-backtrace (sys.int::read-frame-pointer))
  (do ((thread (sys.int::symbol-global-value '*all-threads*)
               (thread-global-next thread)))
      ((null thread))
    (when (not (eql thread (current-thread)))
      (debug-print-line "----------")
      (debug-print-line "Thread " thread)
      (panic-print-backtrace (thread-frame-pointer thread)))))

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

(defun sys.int::cold-clear-input (stream)
  (declare (ignore stream))
  (debug-clear-input))

(defun sys.int::cold-listen (stream)
  (declare (ignore stream))
  ;; Read is not currently implemented for any debug stream.
  nil)

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

(defun sys.int::raise-type-error (datum expected-type)
  (panic "Type error. Expected " expected-type " got " datum))

(defun error (datum &rest arguments)
  (declare (dynamic-extent arguments))
  (panic "Early ERROR. " datum " " arguments))

(defun cerror (continue-message datum &rest arguments)
  (declare (dynamic-extent arguments))
  (panic "Early CERROR. " continue-message " " datum " " arguments))

(defun enter-debugger (condition)
  (panic "Early enter debugger. " condition))

(defun invoke-debugger (condition)
  (panic "Early invoke debugger. " condition))
