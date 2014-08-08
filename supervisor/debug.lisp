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
  (funcall *debug-pesudostream* :read-char))

(defun debug-clear-input ()
  (funcall *debug-pesudostream* :clear-input))

(defun debug-write-string (string)
  (funcall *debug-pesudostream* :write-string string))

(defun debug-write-char (char)
  (funcall *debug-pesudostream* :write-char char))

(defun debug-write-line (string)
  (funcall *debug-pesudostream* :write-string string)
  (funcall *debug-pesudostream* :write-char #\Newline)
  (funcall *debug-pesudostream* :force-output))

(defun debug-force-output ()
  (funcall *debug-pesudostream* :force-output))
