;;;; ---------------------------------------------------------------------------
;;;; Utilities related to streams

(uiop/package:define-package :uiop/stream
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/os :uiop/pathname :uiop/filesystem)
  (:export
   #:*default-stream-element-type*
   #:*stdin* #:setup-stdin #:*stdout* #:setup-stdout #:*stderr* #:setup-stderr
   #:detect-encoding #:*encoding-detection-hook* #:always-default-encoding
   #:encoding-external-format #:*encoding-external-format-hook* #:default-encoding-external-format
   #:*default-encoding* #:*utf-8-external-format*
   #:with-safe-io-syntax #:call-with-safe-io-syntax #:safe-read-from-string
   #:with-output #:output-string #:with-input #:input-string
   #:with-input-file #:call-with-input-file #:with-output-file #:call-with-output-file
   #:null-device-pathname #:call-with-null-input #:with-null-input
   #:call-with-null-output #:with-null-output
   #:finish-outputs #:format! #:safe-format!
   #:copy-stream-to-stream #:concatenate-files #:copy-file
   #:slurp-stream-string #:slurp-stream-lines #:slurp-stream-line
   #:slurp-stream-forms #:slurp-stream-form
   #:read-file-string #:read-file-line #:read-file-lines #:safe-read-file-line
   #:read-file-forms #:read-file-form #:safe-read-file-form
   #:eval-input #:eval-thunk #:standard-eval-thunk
   #:println #:writeln
   #:file-stream-p #:file-or-synonym-stream-p
   ;; Temporary files
   #:*temporary-directory* #:temporary-directory #:default-temporary-directory
   #:setup-temporary-directory
   #:call-with-temporary-file #:with-temporary-file
   #:add-pathname-suffix #:tmpize-pathname
   #:call-with-staging-pathname #:with-staging-pathname))
(in-package :uiop/stream)

(with-upgradability ()
  (defvar *default-stream-element-type*
    (or #+(or abcl cmucl cormanlisp scl xcl) 'character
        #+lispworks 'lw:simple-char
        :default)
    "default element-type for open (depends on the current CL implementation)")

  (defvar *stdin* *standard-input*
    "the original standard input stream at startup")

  (defun setup-stdin ()
    (setf *stdin*
          #.(or #+clozure 'ccl::*stdin*
                #+(or cmucl scl) 'system:*stdin*
                #+(or clasp ecl) 'ext::+process-standard-input+
                #+sbcl 'sb-sys:*stdin*
                '*standard-input*)))

  (defvar *stdout* *standard-output*
    "the original standard output stream at startup")

  (defun setup-stdout ()
    (setf *stdout*
          #.(or #+clozure 'ccl::*stdout*
                #+(or cmucl scl) 'system:*stdout*
                #+(or clasp ecl) 'ext::+process-standard-output+
                #+sbcl 'sb-sys:*stdout*
                '*standard-output*)))

  (defvar *stderr* *error-output*
    "the original error output stream at startup")

  (defun setup-stderr ()
    (setf *stderr*
          #.(or #+allegro 'excl::*stderr*
                #+clozure 'ccl::*stderr*
                #+(or cmucl scl) 'system:*stderr*
                #+(or clasp ecl) 'ext::+process-error-output+
                #+sbcl 'sb-sys:*stderr*
                '*error-output*)))

  ;; Run them now. In image.lisp, we'll register them to be run at image restart.
  (setup-stdin) (setup-stdout) (setup-stderr))


;;; Encodings (mostly hooks only; full support requires asdf-encodings)
(with-upgradability ()
  (defparameter *default-encoding*
    ;; preserve explicit user changes to something other than the legacy default :default
    (or (if-let (previous (and (boundp '*default-encoding*) (symbol-value '*default-encoding*)))
          (unless (eq previous :default) previous))
        :utf-8)
    "Default encoding for source files.
The default value :utf-8 is the portable thing.
The legacy behavior was :default.
If you (asdf:load-system :asdf-encodings) then
you will have autodetection via *encoding-detection-hook* below,
reading emacs-style -*- coding: utf-8 -*- specifications,
and falling back to utf-8 or latin1 if nothing is specified.")

  (defparameter *utf-8-external-format*
    (if (featurep :asdf-unicode)
        (or #+clisp charset:utf-8 :utf-8)
        :default)
    "Default :external-format argument to pass to CL:OPEN and also
CL:LOAD or CL:COMPILE-FILE to best process a UTF-8 encoded file.
On modern implementations, this will decode UTF-8 code points as CL characters.
On legacy implementations, it may fall back on some 8-bit encoding,
with non-ASCII code points being read as several CL characters;
hopefully, if done consistently, that won't affect program behavior too much.")

  (defun always-default-encoding (pathname)
    "Trivial function to use as *encoding-detection-hook*,
always 'detects' the *default-encoding*"
    (declare (ignore pathname))
    *default-encoding*)

  (defvar *encoding-detection-hook* #'always-default-encoding
    "Hook for an extension to define a function to automatically detect a file's encoding")

  (defun detect-encoding (pathname)
    "Detects the encoding of a specified file, going through user-configurable hooks"
    (if (and pathname (not (directory-pathname-p pathname)) (probe-file* pathname))
        (funcall *encoding-detection-hook* pathname)
        *default-encoding*))

  (defun default-encoding-external-format (encoding)
    "Default, ignorant, function to transform a character ENCODING as a
portable keyword to an implementation-dependent EXTERNAL-FORMAT specification.
Load system ASDF-ENCODINGS to hook in a better one."
    (case encoding
      (:default :default) ;; for backward-compatibility only. Explicit usage discouraged.
      (:utf-8 *utf-8-external-format*)
      (otherwise
       (cerror "Continue using :external-format :default" (compatfmt "~@<Your ASDF component is using encoding ~S but it isn't recognized. Your system should :defsystem-depends-on (:asdf-encodings).~:>") encoding)
       :default)))

  (defvar *encoding-external-format-hook*
    #'default-encoding-external-format
    "Hook for an extension (e.g. ASDF-ENCODINGS) to define a better mapping
from non-default encodings to and implementation-defined external-format's")

  (defun encoding-external-format (encoding)
    "Transform a portable ENCODING keyword to an implementation-dependent EXTERNAL-FORMAT,
going through all the proper hooks."
    (funcall *encoding-external-format-hook* (or encoding *default-encoding*))))


;;; Safe syntax
(with-upgradability ()
  (defvar *standard-readtable* (with-standard-io-syntax *readtable*)
    "The standard readtable, implementing the syntax specified by the CLHS.
It must never be modified, though only good implementations will even enforce that.")

  (defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
    "Establish safe CL reader options around the evaluation of BODY"
    `(call-with-safe-io-syntax #'(lambda () (let ((*package* (find-package ,package))) ,@body))))

  (defun call-with-safe-io-syntax (thunk &key (package :cl))
    (with-standard-io-syntax
      (let ((*package* (find-package package))
            (*read-default-float-format* 'double-float)
            (*print-readably* nil)
            (*read-eval* nil))
        (funcall thunk))))

  (defun safe-read-from-string (string &key (package :cl) (eof-error-p t) eof-value (start 0) end preserve-whitespace)
    "Read from STRING using a safe syntax, as per WITH-SAFE-IO-SYNTAX"
    (with-safe-io-syntax (:package package)
      (read-from-string string eof-error-p eof-value :start start :end end :preserve-whitespace preserve-whitespace))))

;;; Output helpers
(with-upgradability ()
  (defun call-with-output-file (pathname thunk
                                &key
                                  (element-type *default-stream-element-type*)
                                  (external-format *utf-8-external-format*)
                                  (if-exists :error)
                                  (if-does-not-exist :create))
    "Open FILE for input with given recognizes options, call THUNK with the resulting stream.
Other keys are accepted but discarded."
    (with-open-file (s pathname :direction :output
                                :element-type element-type
                                :external-format external-format
                                :if-exists if-exists
                                :if-does-not-exist if-does-not-exist)
      (funcall thunk s)))

  (defmacro with-output-file ((var pathname &rest keys
                               &key element-type external-format if-exists if-does-not-exist)
                              &body body)
    (declare (ignore element-type external-format if-exists if-does-not-exist))
    `(call-with-output-file ,pathname #'(lambda (,var) ,@body) ,@keys))

  (defun call-with-output (output function &key keys)
    "Calls FUNCTION with an actual stream argument,
behaving like FORMAT with respect to how stream designators are interpreted:
If OUTPUT is a STREAM, use it as the stream.
If OUTPUT is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OUTPUT is T, use *STANDARD-OUTPUT* as the stream.
If OUTPUT is a STRING with a fill-pointer, use it as a string-output-stream.
If OUTPUT is a PATHNAME, open the file and write to it, passing KEYS to WITH-OUTPUT-FILE
-- this latter as an extension since ASDF 3.1.
Otherwise, signal an error."
    (etypecase output
      (null
       (with-output-to-string (stream) (funcall function stream)))
      ((eql t)
       (funcall function *standard-output*))
      (stream
       (funcall function output))
      (string
       (assert (fill-pointer output))
       (with-output-to-string (stream output) (funcall function stream)))
      (pathname
       (apply 'call-with-output-file output function keys))))

  (defmacro with-output ((output-var &optional (value output-var)) &body body)
    "Bind OUTPUT-VAR to an output stream, coercing VALUE (default: previous binding of OUTPUT-VAR)
as per FORMAT, and evaluate BODY within the scope of this binding."
    `(call-with-output ,value #'(lambda (,output-var) ,@body)))

  (defun output-string (string &optional output)
    "If the desired OUTPUT is not NIL, print the string to the output; otherwise return the string"
    (if output
        (with-output (output) (princ string output))
        string)))


;;; Input helpers
(with-upgradability ()
  (defun call-with-input-file (pathname thunk
                               &key
                                 (element-type *default-stream-element-type*)
                                 (external-format *utf-8-external-format*)
                                 (if-does-not-exist :error))
    "Open FILE for input with given recognizes options, call THUNK with the resulting stream.
Other keys are accepted but discarded."
    (with-open-file (s pathname :direction :input
                                :element-type element-type
                                :external-format external-format
                                :if-does-not-exist if-does-not-exist)
      (funcall thunk s)))

  (defmacro with-input-file ((var pathname &rest keys
                              &key element-type external-format if-does-not-exist)
                             &body body)
    (declare (ignore element-type external-format if-does-not-exist))
    `(call-with-input-file ,pathname #'(lambda (,var) ,@body) ,@keys))

  (defun call-with-input (input function &key keys)
    "Calls FUNCTION with an actual stream argument, interpreting
stream designators like READ, but also coercing strings to STRING-INPUT-STREAM,
and PATHNAME to FILE-STREAM.
If INPUT is a STREAM, use it as the stream.
If INPUT is NIL, use a *STANDARD-INPUT* as the stream.
If INPUT is T, use *TERMINAL-IO* as the stream.
If INPUT is a STRING, use it as a string-input-stream.
If INPUT is a PATHNAME, open it, passing KEYS to WITH-INPUT-FILE
-- the latter is an extension since ASDF 3.1.
Otherwise, signal an error."
    (etypecase input
      (null (funcall function *standard-input*))
      ((eql t) (funcall function *terminal-io*))
      (stream (funcall function input))
      (string (with-input-from-string (stream input) (funcall function stream)))
      (pathname (apply 'call-with-input-file input function keys))))

  (defmacro with-input ((input-var &optional (value input-var)) &body body)
    "Bind INPUT-VAR to an input stream, coercing VALUE (default: previous binding of INPUT-VAR)
as per CALL-WITH-INPUT, and evaluate BODY within the scope of this binding."
    `(call-with-input ,value #'(lambda (,input-var) ,@body)))

  (defun input-string (&optional input)
    "If the desired INPUT is a string, return that string; otherwise slurp the INPUT into a string
and return that"
    (if (stringp input)
        input
        (with-input (input) (funcall 'slurp-stream-string input)))))

;;; Null device
(with-upgradability ()
  (defun null-device-pathname ()
    "Pathname to a bit bucket device that discards any information written to it
and always returns EOF when read from"
    (os-cond
      ((os-unix-p) #p"/dev/null")
      ((os-windows-p) #p"NUL") ;; Q: how many Lisps accept the #p"NUL:" syntax?
      (t (error "No /dev/null on your OS"))))
  (defun call-with-null-input (fun &rest keys &key element-type external-format if-does-not-exist)
    "Call FUN with an input stream from the null device; pass keyword arguments to OPEN."
    (declare (ignore element-type external-format if-does-not-exist))
    (apply 'call-with-input-file (null-device-pathname) fun keys))
  (defmacro with-null-input ((var &rest keys
                              &key element-type external-format if-does-not-exist)
                             &body body)
    (declare (ignore element-type external-format if-does-not-exist))
    "Evaluate BODY in a context when VAR is bound to an input stream accessing the null device.
Pass keyword arguments to OPEN."
    `(call-with-null-input #'(lambda (,var) ,@body) ,@keys))
  (defun call-with-null-output (fun
                                &key (element-type *default-stream-element-type*)
                                  (external-format *utf-8-external-format*)
                                  (if-exists :overwrite)
                                  (if-does-not-exist :error))
    "Call FUN with an output stream to the null device; pass keyword arguments to OPEN."
    (call-with-output-file
     (null-device-pathname) fun
     :element-type element-type :external-format external-format
     :if-exists if-exists :if-does-not-exist if-does-not-exist))
  (defmacro with-null-output ((var &rest keys
                              &key element-type external-format if-does-not-exist if-exists)
                              &body body)
    "Evaluate BODY in a context when VAR is bound to an output stream accessing the null device.
Pass keyword arguments to OPEN."
    (declare (ignore element-type external-format if-exists if-does-not-exist))
    `(call-with-null-output #'(lambda (,var) ,@body) ,@keys)))

;;; Ensure output buffers are flushed
(with-upgradability ()
  (defun finish-outputs (&rest streams)
    "Finish output on the main output streams as well as any specified one.
Useful for portably flushing I/O before user input or program exit."
    ;; CCL notably buffers its stream output by default.
    (dolist (s (append streams
                       (list *stdout* *stderr* *error-output* *standard-output* *trace-output*
                             *debug-io* *terminal-io* *query-io*)))
      (ignore-errors (finish-output s)))
    (values))

  (defun format! (stream format &rest args)
    "Just like format, but call finish-outputs before and after the output."
    (finish-outputs stream)
    (apply 'format stream format args)
    (finish-outputs stream))

  (defun safe-format! (stream format &rest args)
    "Variant of FORMAT that is safe against both
dangerous syntax configuration and errors while printing."
    (with-safe-io-syntax ()
      (ignore-errors (apply 'format! stream format args))
      (finish-outputs stream)))) ; just in case format failed


;;; Simple Whole-Stream processing
(with-upgradability ()
  (defun copy-stream-to-stream (input output &key element-type buffer-size linewise prefix)
    "Copy the contents of the INPUT stream into the OUTPUT stream.
If LINEWISE is true, then read and copy the stream line by line, with an optional PREFIX.
Otherwise, using WRITE-SEQUENCE using a buffer of size BUFFER-SIZE."
    (with-open-stream (input input)
      (if linewise
          (loop* :for (line eof) = (multiple-value-list (read-line input nil nil))
                 :while line :do
                 (when prefix (princ prefix output))
                 (princ line output)
                 (unless eof (terpri output))
                 (finish-output output)
                 (when eof (return)))
          (loop
            :with buffer-size = (or buffer-size 8192)
            :with buffer = (make-array (list buffer-size) :element-type (or element-type 'character))
            :for end = (read-sequence buffer input)
            :until (zerop end)
            :do (write-sequence buffer output :end end)
                (when (< end buffer-size) (return))))))

  (defun concatenate-files (inputs output)
    "create a new OUTPUT file the contents of which a the concatenate of the INPUTS files."
    (with-open-file (o output :element-type '(unsigned-byte 8)
                              :direction :output :if-exists :rename-and-delete)
      (dolist (input inputs)
        (with-open-file (i input :element-type '(unsigned-byte 8)
                                 :direction :input :if-does-not-exist :error)
          (copy-stream-to-stream i o :element-type '(unsigned-byte 8))))))

  (defun copy-file (input output)
    "Copy contents of the INPUT file to the OUTPUT file"
    ;; Not available on LW personal edition or LW 6.0 on Mac: (lispworks:copy-file i f)
    #+allegro
    (excl.osi:copy-file input output)
    #+ecl
    (ext:copy-file input output)
    #-(or allegro ecl)
    (concatenate-files (list input) output))

  (defun slurp-stream-string (input &key (element-type 'character) stripped)
    "Read the contents of the INPUT stream as a string"
    (let ((string
            (with-open-stream (input input)
              (with-output-to-string (output)
                (copy-stream-to-stream input output :element-type element-type)))))
      (if stripped (stripln string) string)))

  (defun slurp-stream-lines (input &key count)
    "Read the contents of the INPUT stream as a list of lines, return those lines.

Note: relies on the Lisp's READ-LINE, but additionally removes any remaining CR
from the line-ending if the file or stream had CR+LF but Lisp only removed LF.

Read no more than COUNT lines."
    (check-type count (or null integer))
    (with-open-stream (input input)
      (loop :for n :from 0
            :for l = (and (or (not count) (< n count))
                          (read-line input nil nil))
            ;; stripln: to remove CR when the OS sends CRLF and Lisp only remove LF
            :while l :collect (stripln l))))

  (defun slurp-stream-line (input &key (at 0))
    "Read the contents of the INPUT stream as a list of lines,
then return the ACCESS-AT of that list of lines using the AT specifier.
PATH defaults to 0, i.e. return the first line.
PATH is typically an integer, or a list of an integer and a function.
If PATH is NIL, it will return all the lines in the file.

The stream will not be read beyond the Nth lines,
where N is the index specified by path
if path is either an integer or a list that starts with an integer."
    (access-at (slurp-stream-lines input :count (access-at-count at)) at))

  (defun slurp-stream-forms (input &key count)
    "Read the contents of the INPUT stream as a list of forms,
and return those forms.

If COUNT is null, read to the end of the stream;
if COUNT is an integer, stop after COUNT forms were read.

BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (check-type count (or null integer))
    (loop :with eof = '#:eof
          :for n :from 0
          :for form = (if (and count (>= n count))
                          eof
                          (read-preserving-whitespace input nil eof))
          :until (eq form eof) :collect form))

  (defun slurp-stream-form (input &key (at 0))
    "Read the contents of the INPUT stream as a list of forms,
then return the ACCESS-AT of these forms following the AT.
AT defaults to 0, i.e. return the first form.
AT is typically a list of integers.
If AT is NIL, it will return all the forms in the file.

The stream will not be read beyond the Nth form,
where N is the index specified by path,
if path is either an integer or a list that starts with an integer.

BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (access-at (slurp-stream-forms input :count (access-at-count at)) at))

  (defun read-file-string (file &rest keys)
    "Open FILE with option KEYS, read its contents as a string"
    (apply 'call-with-input-file file 'slurp-stream-string keys))

  (defun read-file-lines (file &rest keys)
    "Open FILE with option KEYS, read its contents as a list of lines
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file 'slurp-stream-lines keys))

  (defun read-file-line (file &rest keys &key (at 0) &allow-other-keys)
    "Open input FILE with option KEYS (except AT),
and read its contents as per SLURP-STREAM-LINE with given AT specifier.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-line input :at at))
           (remove-plist-key :at keys)))

  (defun read-file-forms (file &rest keys &key count &allow-other-keys)
    "Open input FILE with option KEYS (except COUNT),
and read its contents as per SLURP-STREAM-FORMS with given COUNT.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-forms input :count count))
           (remove-plist-key :count keys)))

  (defun read-file-form (file &rest keys &key (at 0) &allow-other-keys)
    "Open input FILE with option KEYS (except AT),
and read its contents as per SLURP-STREAM-FORM with given AT specifier.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-form input :at at))
           (remove-plist-key :at keys)))

  (defun safe-read-file-line (pathname &rest keys &key (package :cl) &allow-other-keys)
    "Reads the specified line from the top of a file using a safe standardized syntax.
Extracts the line using READ-FILE-LINE,
within an WITH-SAFE-IO-SYNTAX using the specified PACKAGE."
    (with-safe-io-syntax (:package package)
      (apply 'read-file-line pathname (remove-plist-key :package keys))))

  (defun safe-read-file-form (pathname &rest keys &key (package :cl) &allow-other-keys)
    "Reads the specified form from the top of a file using a safe standardized syntax.
Extracts the form using READ-FILE-FORM,
within an WITH-SAFE-IO-SYNTAX using the specified PACKAGE."
    (with-safe-io-syntax (:package package)
      (apply 'read-file-form pathname (remove-plist-key :package keys))))

  (defun eval-input (input)
    "Portably read and evaluate forms from INPUT, return the last values."
    (with-input (input)
      (loop :with results :with eof ='#:eof
            :for form = (read input nil eof)
            :until (eq form eof)
            :do (setf results (multiple-value-list (eval form)))
            :finally (return (values-list results)))))

  (defun eval-thunk (thunk)
    "Evaluate a THUNK of code:
If a function, FUNCALL it without arguments.
If a constant literal and not a sequence, return it.
If a cons or a symbol, EVAL it.
If a string, repeatedly read and evaluate from it, returning the last values."
    (etypecase thunk
      ((or boolean keyword number character pathname) thunk)
      ((or cons symbol) (eval thunk))
      (function (funcall thunk))
      (string (eval-input thunk))))

  (defun standard-eval-thunk (thunk &key (package :cl))
    "Like EVAL-THUNK, but in a more standardized evaluation context."
    ;; Note: it's "standard-" not "safe-", because evaluation is never safe.
    (when thunk
      (with-safe-io-syntax (:package package)
        (let ((*read-eval* t))
          (eval-thunk thunk))))))

(with-upgradability ()
  (defun println (x &optional (stream *standard-output*))
    "Variant of PRINC that also calls TERPRI afterwards"
    (princ x stream) (terpri stream) (finish-output stream) (values))

  (defun writeln (x &rest keys &key (stream *standard-output*) &allow-other-keys)
    "Variant of WRITE that also calls TERPRI afterwards"
    (apply 'write x keys) (terpri stream) (finish-output stream) (values)))


;;; Using temporary files
(with-upgradability ()
  (defun default-temporary-directory ()
    "Return a default directory to use for temporary files"
    (os-cond
      ((os-unix-p)
       (or (getenv-pathname "TMPDIR" :ensure-directory t)
           (parse-native-namestring "/tmp/")))
      ((os-windows-p)
       (getenv-pathname "TEMP" :ensure-directory t))
      (t (subpathname (user-homedir-pathname) "tmp/"))))

  (defvar *temporary-directory* nil "User-configurable location for temporary files")

  (defun temporary-directory ()
    "Return a directory to use for temporary files"
    (or *temporary-directory* (default-temporary-directory)))

  (defun setup-temporary-directory ()
    "Configure a default temporary directory to use."
    (setf *temporary-directory* (default-temporary-directory))
    #+gcl (setf system::*tmp-dir* *temporary-directory*))

  (defun call-with-temporary-file
      (thunk &key
               (want-stream-p t) (want-pathname-p t) (direction :io) keep after
               directory (type "tmp" typep) prefix (suffix (when typep "-tmp"))
               (element-type *default-stream-element-type*)
               (external-format *utf-8-external-format*))
    "Call a THUNK with stream and/or pathname arguments identifying a temporary file.

The temporary file's pathname will be based on concatenating
PREFIX (or \"tmp\" if it's NIL), a random alphanumeric string,
and optional SUFFIX (defaults to \"-tmp\" if a type was provided)
and TYPE (defaults to \"tmp\", using a dot as separator if not NIL),
within DIRECTORY (defaulting to the TEMPORARY-DIRECTORY) if the PREFIX isn't absolute.

The file will be open with specified DIRECTION (defaults to :IO),
ELEMENT-TYPE (defaults to *DEFAULT-STREAM-ELEMENT-TYPE*) and
EXTERNAL-FORMAT (defaults to *UTF-8-EXTERNAL-FORMAT*).
If WANT-STREAM-P is true (the defaults to T), then THUNK will then be CALL-FUNCTION'ed
with the stream and the pathname (if WANT-PATHNAME-P is true, defaults to T),
and stream will be closed after the THUNK exits (either normally or abnormally).
If WANT-STREAM-P is false, then WANT-PATHAME-P must be true, and then
THUNK is only CALL-FUNCTION'ed after the stream is closed, with the pathname as argument.
Upon exit of THUNK, the AFTER thunk if defined is CALL-FUNCTION'ed with the pathname as argument.
If AFTER is defined, its results are returned, otherwise, the results of THUNK are returned.
Finally, the file will be deleted, unless the KEEP argument when CALL-FUNCTION'ed returns true."
    #+xcl (declare (ignorable typep))
    (check-type direction (member :output :io))
    (assert (or want-stream-p want-pathname-p))
    (loop
      :with prefix-pn = (ensure-absolute-pathname
                         (or prefix "tmp")
                         (or (ensure-pathname
                              directory
                              :namestring :native
                              :ensure-directory t
                              :ensure-physical t)
                             #'temporary-directory))
      :with prefix-nns = (native-namestring prefix-pn)
      :with results = (progn (ensure-directories-exist prefix-pn)
                             ())
      :for counter :from (random (expt 36 #-gcl 8 #+gcl 5))
      :for pathname = (parse-native-namestring
                       (format nil "~A~36R~@[~A~]~@[.~A~]"
                               prefix-nns counter suffix (unless (eq type :unspecific) type)))
      :for okp = nil :do
        ;; TODO: on Unix, do something about umask
        ;; TODO: on Unix, audit the code so we make sure it uses O_CREAT|O_EXCL
        ;; TODO: on Unix, use CFFI and mkstemp --
        ;; except UIOP is precisely meant to not depend on CFFI or on anything! Grrrr.
        ;; Can we at least design some hook?
        (unwind-protect
             (progn
               (ensure-directories-exist pathname)
               (with-open-file (stream pathname
                                       :direction direction
                                       :element-type element-type
                                       :external-format external-format
                                       :if-exists nil :if-does-not-exist :create)
                 (when stream
                   (setf okp pathname)
                   (when want-stream-p
                     ;; Note: can't return directly from within with-open-file
                     ;; or the non-local return causes the file creation to be undone.
                     (setf results (multiple-value-list
                                    (if want-pathname-p
                                        (funcall thunk stream pathname)
                                        (funcall thunk stream)))))))
               (cond
                 ((not okp) nil)
                 (after (return (call-function after okp)))
                 ((and want-pathname-p (not want-stream-p)) (return (call-function thunk okp)))
                 (t (return (values-list results)))))
          (when (and okp (not (call-function keep)))
            (ignore-errors (delete-file-if-exists okp))))))

  (defmacro with-temporary-file ((&key (stream (gensym "STREAM") streamp)
                                    (pathname (gensym "PATHNAME") pathnamep)
                                    directory prefix suffix type
                                    keep direction element-type external-format)
                                 &body body)
    "Evaluate BODY where the symbols specified by keyword arguments
STREAM and PATHNAME (if respectively specified) are bound corresponding
to a newly created temporary file ready for I/O, as per CALL-WITH-TEMPORARY-FILE.
At least one of STREAM or PATHNAME must be specified.
If the STREAM is not specified, it will be closed before the BODY is evaluated.
If STREAM is specified, then the :CLOSE-STREAM label if it appears in the BODY,
separates forms run before and after the stream is closed.
The values of the last form of the BODY (not counting the separating :CLOSE-STREAM) are returned.
Upon success, the KEEP form is evaluated and the file is is deleted unless it evaluates to TRUE."
    (check-type stream symbol)
    (check-type pathname symbol)
    (assert (or streamp pathnamep))
    (let* ((afterp (position :close-stream body))
           (before (if afterp (subseq body 0 afterp) body))
           (after (when afterp (subseq body (1+ afterp))))
           (beforef (gensym "BEFORE"))
           (afterf (gensym "AFTER")))
      `(flet (,@(when before
                  `((,beforef (,@(when streamp `(,stream)) ,@(when pathnamep `(,pathname)))
                       ,@(when after `((declare (ignorable ,pathname))))
                       ,@before)))
              ,@(when after
                  (assert pathnamep)
                  `((,afterf (,pathname) ,@after))))
         #-gcl (declare (dynamic-extent ,@(when before `(#',beforef)) ,@(when after `(#',afterf))))
         (call-with-temporary-file
          ,(when before `#',beforef)
          :want-stream-p ,streamp
          :want-pathname-p ,pathnamep
          ,@(when direction `(:direction ,direction))
          ,@(when directory `(:directory ,directory))
          ,@(when prefix `(:prefix ,prefix))
          ,@(when suffix `(:suffix ,suffix))
          ,@(when type `(:type ,type))
          ,@(when keep `(:keep ,keep))
          ,@(when after `(:after #',afterf))
          ,@(when element-type `(:element-type ,element-type))
          ,@(when external-format `(:external-format ,external-format))))))

  (defun get-temporary-file (&key directory prefix suffix type)
    (with-temporary-file (:pathname pn :keep t
                          :directory directory :prefix prefix :suffix suffix :type type)
      pn))

  ;; Temporary pathnames in simple cases where no contention is assumed
  (defun add-pathname-suffix (pathname suffix &rest keys)
    "Add a SUFFIX to the name of a PATHNAME, return a new pathname.
Further KEYS can be passed to MAKE-PATHNAME."
    (apply 'make-pathname :name (strcat (pathname-name pathname) suffix)
                          :defaults pathname keys))

  (defun tmpize-pathname (x)
    "Return a new pathname modified from X by adding a trivial random suffix.
A new empty file with said temporary pathname is created, to ensure there is no
clash with any concurrent process attempting the same thing."
    (let* ((px (ensure-pathname x :ensure-physical t))
           (prefix (if-let (n (pathname-name px)) (strcat n "-tmp") "tmp"))
           (directory (pathname-directory-pathname px)))
      (get-temporary-file :directory directory :prefix prefix :type (pathname-type px))))

  (defun call-with-staging-pathname (pathname fun)
    "Calls FUN with a staging pathname, and atomically
renames the staging pathname to the PATHNAME in the end.
NB: this protects only against failure of the program, not against concurrent attempts.
For the latter case, we ought pick a random suffix and atomically open it."
    (let* ((pathname (pathname pathname))
           (staging (tmpize-pathname pathname)))
      (unwind-protect
           (multiple-value-prog1
               (funcall fun staging)
             (rename-file-overwriting-target staging pathname))
        (delete-file-if-exists staging))))

  (defmacro with-staging-pathname ((pathname-var &optional (pathname-value pathname-var)) &body body)
    "Trivial syntax wrapper for CALL-WITH-STAGING-PATHNAME"
    `(call-with-staging-pathname ,pathname-value #'(lambda (,pathname-var) ,@body))))

(with-upgradability ()
  (defun file-stream-p (stream)
    (typep stream 'file-stream))
  (defun file-or-synonym-stream-p (stream)
    (or (file-stream-p stream)
        (and (typep stream 'synonym-stream)
             (file-or-synonym-stream-p
              (symbol-value (synonym-stream-symbol stream)))))))
