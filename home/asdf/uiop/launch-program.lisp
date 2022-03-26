;;;; -------------------------------------------------------------------------
;;;; launch-program - semi-portably spawn asynchronous subprocesses

(uiop/package:define-package :uiop/launch-program
  (:use :uiop/common-lisp :uiop/package :uiop/utility
   :uiop/pathname :uiop/os :uiop/filesystem :uiop/stream)
  (:export
   ;;; Escaping the command invocation madness
   #:easy-sh-character-p #:escape-sh-token #:escape-sh-command
   #:escape-windows-token #:escape-windows-command
   #:escape-shell-token #:escape-shell-command
   #:escape-token #:escape-command

   ;;; launch-program
   #:launch-program
   #:close-streams #:process-alive-p #:terminate-process #:wait-process
   #:process-info-error-output #:process-info-input #:process-info-output #:process-info-pid))
(in-package :uiop/launch-program)

;;;; ----- Escaping strings for the shell -----
(with-upgradability ()
  (defun requires-escaping-p (token &key good-chars bad-chars)
    "Does this token require escaping, given the specification of
either good chars that don't need escaping or bad chars that do need escaping,
as either a recognizing function or a sequence of characters."
    (some
     (cond
       ((and good-chars bad-chars)
        (parameter-error "~S: only one of good-chars and bad-chars can be provided"
                         'requires-escaping-p))
       ((typep good-chars 'function)
        (complement good-chars))
       ((typep bad-chars 'function)
        bad-chars)
       ((and good-chars (typep good-chars 'sequence))
        #'(lambda (c) (not (find c good-chars))))
       ((and bad-chars (typep bad-chars 'sequence))
        #'(lambda (c) (find c bad-chars)))
       (t (parameter-error "~S: no good-char criterion" 'requires-escaping-p)))
     token))

  (defun escape-token (token &key stream quote good-chars bad-chars escaper)
    "Call the ESCAPER function on TOKEN string if it needs escaping as per
REQUIRES-ESCAPING-P using GOOD-CHARS and BAD-CHARS, otherwise output TOKEN,
using STREAM as output (or returning result as a string if NIL)"
    (if (requires-escaping-p token :good-chars good-chars :bad-chars bad-chars)
        (with-output (stream)
          (apply escaper token stream (when quote `(:quote ,quote))))
        (output-string token stream)))

  (defun escape-windows-token-within-double-quotes (x &optional s)
    "Escape a string token X within double-quotes
for use within a MS Windows command-line, outputing to S."
    (labels ((issue (c) (princ c s))
             (issue-backslash (n) (loop :repeat n :do (issue #\\))))
      (loop
        :initially (issue #\") :finally (issue #\")
        :with l = (length x) :with i = 0
        :for i+1 = (1+ i) :while (< i l) :do
          (case (char x i)
            ((#\") (issue-backslash 1) (issue #\") (setf i i+1))
            ((#\\)
             (let* ((j (and (< i+1 l) (position-if-not
                                       #'(lambda (c) (eql c #\\)) x :start i+1)))
                    (n (- (or j l) i)))
               (cond
                 ((null j)
                  (issue-backslash (* 2 n)) (setf i l))
                 ((and (< j l) (eql (char x j) #\"))
                  (issue-backslash (1+ (* 2 n))) (issue #\") (setf i (1+ j)))
                 (t
                  (issue-backslash n) (setf i j)))))
            (otherwise
             (issue (char x i)) (setf i i+1))))))

  (defun easy-windows-character-p (x)
    "Is X an \"easy\" character that does not require quoting by the shell?"
    (or (alphanumericp x) (find x "+-_.,@:/=")))

  (defun escape-windows-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a MS Windows command-line, outputing to S."
    (escape-token token :stream s :good-chars #'easy-windows-character-p :quote nil
                        :escaper 'escape-windows-token-within-double-quotes))

  (defun escape-sh-token-within-double-quotes (x s &key (quote t))
    "Escape a string TOKEN within double-quotes
for use within a POSIX Bourne shell, outputing to S;
omit the outer double-quotes if key argument :QUOTE is NIL"
    (when quote (princ #\" s))
    (loop :for c :across x :do
      (when (find c "$`\\\"") (princ #\\ s))
      (princ c s))
    (when quote (princ #\" s)))

  (defun easy-sh-character-p (x)
    "Is X an \"easy\" character that does not require quoting by the shell?"
    (or (alphanumericp x) (find x "+-_.,%@:/=")))

  (defun escape-sh-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a POSIX Bourne shell, outputing to S."
    (escape-token token :stream s :quote #\" :good-chars #'easy-sh-character-p
                        :escaper 'escape-sh-token-within-double-quotes))

  (defun escape-shell-token (token &optional s)
    "Escape a token for the current operating system shell"
    (os-cond
      ((os-unix-p) (escape-sh-token token s))
      ((os-windows-p) (escape-windows-token token s))))

  (defun escape-command (command &optional s
                                  (escaper 'escape-shell-token))
    "Given a COMMAND as a list of tokens, return a string of the
spaced, escaped tokens, using ESCAPER to escape."
    (etypecase command
      (string (output-string command s))
      (list (with-output (s)
              (loop :for first = t :then nil :for token :in command :do
                (unless first (princ #\space s))
                (funcall escaper token s))))))

  (defun escape-windows-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by CommandLineToArgv in MS Windows"
    ;; http://msdn.microsoft.com/en-us/library/bb776391(v=vs.85).aspx
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft(v=vs.85).aspx
    (escape-command command s 'escape-windows-token))

  (defun escape-sh-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by /bin/sh in POSIX"
    (escape-command command s 'escape-sh-token))

  (defun escape-shell-command (command &optional stream)
    "Escape a command for the current operating system's shell"
    (escape-command command stream 'escape-shell-token)))


(with-upgradability ()
  ;;; Internal helpers for run-program
  (defun %normalize-io-specifier (specifier &optional role)
    "Normalizes a portable I/O specifier for LAUNCH-PROGRAM into an implementation-dependent
argument to pass to the internal RUN-PROGRAM"
    (declare (ignorable role))
    (typecase specifier
      (null (or #+(or allegro lispworks) (null-device-pathname)))
      (string (parse-native-namestring specifier))
      (pathname specifier)
      (stream specifier)
      ((eql :stream) :stream)
      ((eql :interactive)
       #+(or allegro lispworks) nil
       #+clisp :terminal
       #+(or abcl clozure cmucl ecl mkcl sbcl scl) t
       #-(or abcl clozure cmucl ecl mkcl sbcl scl allegro lispworks clisp)
       (not-implemented-error :interactive-output
                              "On this lisp implementation, cannot interpret ~a value of ~a"
                              specifier role))
      ((eql :output)
       (cond ((eq role :error-output)
              #+(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl scl)
              :output
              #-(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl scl)
              (not-implemented-error :error-output-redirect
                                     "Can't send ~a to ~a on this lisp implementation."
                                     role specifier))
             (t (parameter-error "~S IO specifier invalid for ~S" specifier role))))
      (otherwise
       (parameter-error "Incorrect I/O specifier ~S for ~S"
                        specifier role))))

  (defun %interactivep (input output error-output)
    (member :interactive (list input output error-output)))

  (defun %signal-to-exit-code (signum)
    (+ 128 signum))

  (defun %code-to-status (exit-code signal-code)
    (cond ((null exit-code) :running)
          ((null signal-code) (values :exited exit-code))
          (t (values :signaled signal-code))))

  #+mkcl
  (defun %mkcl-signal-to-number (signal)
    (require :mk-unix)
    (symbol-value (find-symbol signal :mk-unix)))

  (defclass process-info ()
    (;; The process field is highly platform-, implementation-, and
     ;; even version-dependent.
     ;; Prior to LispWorks 7, the only information that
     ;; `sys:run-shell-command` with `:wait nil` was certain to return
     ;; is a PID (e.g. when all streams are nil), hence we stored it
     ;; and used `sys:pid-exit-status` to obtain an exit status
     ;; later. That is still what we do.
     ;; From LispWorks 7 on, if `sys:run-shell-command` does not
     ;; return a proper stream, we are instead given a dummy stream.
     ;; We can thus always store a stream and use
     ;; `sys:pipe-exit-status` to obtain an exit status later.
     ;; The advantage of dealing with streams instead of PID is the
     ;; availability of functions like `sys:pipe-kill-process`.
     (process :initform nil)
     (input-stream :initform nil)
     (output-stream :initform nil)
     (bidir-stream :initform nil)
     (error-output-stream :initform nil)
     ;; For backward-compatibility, to maintain the property (zerop
     ;; exit-code) <-> success, an exit in response to a signal is
     ;; encoded as 128+signum.
     (exit-code :initform nil)
     ;; If the platform allows it, distinguish exiting with a code
     ;; >128 from exiting in response to a signal by setting this code
     (signal-code :initform nil)))

;;;---------------------------------------------------------------------------
;;; The following two helper functions take care of handling the IF-EXISTS and
;;; IF-DOES-NOT-EXIST arguments for RUN-PROGRAM. In particular, they process the
;;; :ERROR, :APPEND, and :SUPERSEDE arguments *here*, allowing the master
;;; function to treat input and output files unconditionally for reading and
;;; writing.
;;;---------------------------------------------------------------------------

  (defun %handle-if-exists (file if-exists)
    (when (or (stringp file) (pathnamep file))
      (ecase if-exists
        ((:append :supersede :error)
         (with-open-file (dummy file :direction :output :if-exists if-exists)
           (declare (ignorable dummy)))))))

  (defun %handle-if-does-not-exist (file if-does-not-exist)
    (when (or (stringp file) (pathnamep file))
      (ecase if-does-not-exist
        ((:create :error)
         (with-open-file (dummy file :direction :probe
                                :if-does-not-exist if-does-not-exist)
           (declare (ignorable dummy)))))))

  (defun process-info-error-output (process-info)
    (slot-value process-info 'error-output-stream))
  (defun process-info-input (process-info)
    (or (slot-value process-info 'bidir-stream)
        (slot-value process-info 'input-stream)))
  (defun process-info-output (process-info)
    (or (slot-value process-info 'bidir-stream)
        (slot-value process-info 'output-stream)))

  (defun process-info-pid (process-info)
    (let ((process (slot-value process-info 'process)))
      (declare (ignorable process))
      #+abcl (symbol-call :sys :process-pid process)
      #+allegro process
      #+clozure (ccl:external-process-id process)
      #+ecl (ext:external-process-pid process)
      #+(or cmucl scl) (ext:process-pid process)
      #+lispworks7+ (sys:pipe-pid process)
      #+(and lispworks (not lispworks7+)) process
      #+mkcl (mkcl:process-id process)
      #+sbcl (sb-ext:process-pid process)
      #-(or abcl allegro clozure cmucl ecl mkcl lispworks sbcl scl)
      (not-implemented-error 'process-info-pid)))

  (defun %process-status (process-info)
    (if-let (exit-code (slot-value process-info 'exit-code))
      (return-from %process-status
        (if-let (signal-code (slot-value process-info 'signal-code))
          (values :signaled signal-code)
          (values :exited exit-code))))
    #-(or allegro clozure cmucl ecl lispworks mkcl sbcl scl)
    (not-implemented-error '%process-status)
    (if-let (process (slot-value process-info 'process))
      (multiple-value-bind (status code)
          (progn
            #+allegro (multiple-value-bind (exit-code pid signal-code)
                          (sys:reap-os-subprocess :pid process :wait nil)
                        (assert pid)
                        (%code-to-status exit-code signal-code))
            #+clozure (ccl:external-process-status process)
            #+(or cmucl scl) (let ((status (ext:process-status process)))
                               (if (member status '(:exited :signaled))
                                   ;; Calling ext:process-exit-code on
                                   ;; processes that are still alive
                                   ;; yields an undefined result
                                   (values status (ext:process-exit-code process))
                                   status))
            #+ecl (ext:external-process-status process)
            #+lispworks
            ;; a signal is only returned on LispWorks 7+
            (multiple-value-bind (exit-code signal-code)
                (symbol-call :sys
                             #+lispworks7+ :pipe-exit-status
                             #-lispworks7+ :pid-exit-status
                             process :wait nil)
              (%code-to-status exit-code signal-code))
            #+mkcl (let ((status (mk-ext:process-status process)))
                     (if (eq status :exited)
                         ;; Only call mk-ext:process-exit-code when
                         ;; necessary since it leads to another waitpid()
                         (let ((code (mk-ext:process-exit-code process)))
                           (if (stringp code)
                               (values :signaled (%mkcl-signal-to-number code))
                               (values :exited code)))
                         status))
            #+sbcl (let ((status (sb-ext:process-status process)))
                     (if (eq status :running)
                         :running
                         ;; sb-ext:process-exit-code can also be
                         ;; called for stopped processes to determine
                         ;; the signal that stopped them
                         (values status (sb-ext:process-exit-code process)))))
        (case status
          (:exited (setf (slot-value process-info 'exit-code) code))
          (:signaled (let ((%code (%signal-to-exit-code code)))
                       (setf (slot-value process-info 'exit-code) %code
                             (slot-value process-info 'signal-code) code))))
        (if code
            (values status code)
            status))))

  (defun process-alive-p (process-info)
    "Check if a process has yet to exit."
    (unless (slot-value process-info 'exit-code)
      #+abcl (sys:process-alive-p (slot-value process-info 'process))
      #+(or cmucl scl) (ext:process-alive-p (slot-value process-info 'process))
      #+sbcl (sb-ext:process-alive-p (slot-value process-info 'process))
      #-(or abcl cmucl sbcl scl) (find (%process-status process-info)
                                       '(:running :stopped :continued :resumed))))

  (defun wait-process (process-info)
    "Wait for the process to terminate, if it is still running.
Otherwise, return immediately. An exit code (a number) will be
returned, with 0 indicating success, and anything else indicating
failure. If the process exits after receiving a signal, the exit code
will be the sum of 128 and the (positive) numeric signal code. A second
value may be returned in this case: the numeric signal code itself.
Any asynchronously spawned process requires this function to be run
before it is garbage-collected in order to free up resources that
might otherwise be irrevocably lost."
    (if-let (exit-code (slot-value process-info 'exit-code))
      (if-let (signal-code (slot-value process-info 'signal-code))
        (values exit-code signal-code)
        exit-code)
      (let ((process (slot-value process-info 'process)))
        #-(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl scl)
        (not-implemented-error 'wait-process)
        (when process
          ;; 1- wait
          #+clozure (ccl::external-process-wait process)
          #+(or cmucl scl) (ext:process-wait process)
          #+sbcl (sb-ext:process-wait process)
          ;; 2- extract result
          (multiple-value-bind (exit-code signal-code)
              (progn
                #+abcl (sys:process-wait process)
                #+allegro (multiple-value-bind (exit-code pid signal)
                              (sys:reap-os-subprocess :pid process :wait t)
                            (assert pid)
                            (values exit-code signal))
                #+clozure (multiple-value-bind (status code)
                              (ccl:external-process-status process)
                            (if (eq status :signaled)
                                (values nil code)
                                code))
                #+(or cmucl scl) (let ((status (ext:process-status process))
                                       (code (ext:process-exit-code process)))
                                   (if (eq status :signaled)
                                       (values nil code)
                                       code))
                #+ecl (multiple-value-bind (status code)
                          (ext:external-process-wait process t)
                        (if (eq status :signaled)
                            (values nil code)
                            code))
                #+lispworks (symbol-call :sys
                                         #+lispworks7+ :pipe-exit-status
                                         #-lispworks7+ :pid-exit-status
                                         process :wait t)
                #+mkcl (let ((code (mkcl:join-process process)))
                         (if (stringp code)
                             (values nil (%mkcl-signal-to-number code))
                             code))
                #+sbcl (let ((status (sb-ext:process-status process))
                             (code (sb-ext:process-exit-code process)))
                         (if (eq status :signaled)
                             (values nil code)
                             code)))
            (if signal-code
                (let ((%exit-code (%signal-to-exit-code signal-code)))
                  (setf (slot-value process-info 'exit-code) %exit-code
                        (slot-value process-info 'signal-code) signal-code)
                  (values %exit-code signal-code))
                (progn (setf (slot-value process-info 'exit-code) exit-code)
                       exit-code)))))))

  ;; WARNING: For signals other than SIGTERM and SIGKILL this may not
  ;; do what you expect it to. Sending SIGSTOP to a process spawned
  ;; via LAUNCH-PROGRAM, e.g., will stop the shell /bin/sh that is used
  ;; to run the command (via `sh -c command`) but not the actual
  ;; command.
  #+os-unix
  (defun %posix-send-signal (process-info signal)
    #+allegro (excl.osi:kill (slot-value process-info 'process) signal)
    #+clozure (ccl:signal-external-process (slot-value process-info 'process)
                                           signal :error-if-exited nil)
    #+(or cmucl scl) (ext:process-kill (slot-value process-info 'process) signal)
    #+sbcl (sb-ext:process-kill (slot-value process-info 'process) signal)
    #-(or allegro clozure cmucl sbcl scl)
    (if-let (pid (process-info-pid process-info))
      (symbol-call :uiop :run-program
                   (format nil "kill -~a ~a" signal pid) :ignore-error-status t)))

  ;;; this function never gets called on Windows, but the compiler cannot tell
  ;;; that. [2016/09/25:rpg]
  #+os-windows
  (defun %posix-send-signal (process-info signal)
    (declare (ignore process-info signal))
    (values))

  (defun terminate-process (process-info &key urgent)
    "Cause the process to exit. To that end, the process may or may
not be sent a signal, which it will find harder (or even impossible)
to ignore if URGENT is T. On some platforms, it may also be subject to
race conditions."
    (declare (ignorable urgent))
    #+abcl (sys:process-kill (slot-value process-info 'process))
    ;; On ECL, this will only work on versions later than 2016-09-06,
    ;; but we still want to compile on earlier versions, so we use symbol-call
    #+ecl (symbol-call :ext :terminate-process (slot-value process-info 'process) urgent)
    #+lispworks7+ (sys:pipe-kill-process (slot-value process-info 'process))
    #+mkcl (mk-ext:terminate-process (slot-value process-info 'process)
                                     :force urgent)
    #-(or abcl ecl lispworks7+ mkcl)
    (os-cond
     ((os-unix-p) (%posix-send-signal process-info (if urgent 9 15)))
     ((os-windows-p) (if-let (pid (process-info-pid process-info))
                       (symbol-call :uiop :run-program
                                    (format nil "taskkill ~:[~;/f ~]/pid ~a" urgent pid)
                                    :ignore-error-status t)))
     (t (not-implemented-error 'terminate-process))))

  (defun close-streams (process-info)
    "Close any stream that the process might own. Needs to be run
whenever streams were requested by passing :stream to :input, :output,
or :error-output."
    (dolist (stream
              (cons (slot-value process-info 'error-output-stream)
                    (if-let (bidir-stream (slot-value process-info 'bidir-stream))
                      (list bidir-stream)
                      (list (slot-value process-info 'input-stream)
                            (slot-value process-info 'output-stream)))))
      (when stream (close stream))))

  (defun launch-program (command &rest keys
                         &key
                           input (if-input-does-not-exist :error)
                           output (if-output-exists :supersede)
                           error-output (if-error-output-exists :supersede)
                           (element-type #-clozure *default-stream-element-type*
                                         #+clozure 'character)
                           (external-format *utf-8-external-format*)
                           directory
                           #+allegro separate-streams
                           &allow-other-keys)
    "Launch program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on
Windows) _asynchronously_.

If OUTPUT is a pathname, a string designating a pathname, or NIL (the
default) designating the null device, the file at that path is used as
output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*, and
under SLIME will be on your *inferior-lisp* buffer.  If it's T, output
goes to your current *STANDARD-OUTPUT* stream.  If it's :STREAM, a new
stream will be made available that can be accessed via
PROCESS-INFO-OUTPUT and read from. Otherwise, OUTPUT should be a value
that the underlying lisp implementation knows how to handle.

IF-OUTPUT-EXISTS, which is only meaningful if OUTPUT is a string or a
pathname, can take the values :ERROR, :APPEND, and :SUPERSEDE (the
default). The meaning of these values and their effect on the case
where OUTPUT does not exist, is analogous to the IF-EXISTS parameter
to OPEN with :DIRECTION :OUTPUT.

ERROR-OUTPUT is similar to OUTPUT. T designates the *ERROR-OUTPUT*,
:OUTPUT means redirecting the error output to the output stream,
and :STREAM causes a stream to be made available via
PROCESS-INFO-ERROR-OUTPUT.

IF-ERROR-OUTPUT-EXISTS is similar to IF-OUTPUT-EXIST, except that it
affects ERROR-OUTPUT rather than OUTPUT.

INPUT is similar to OUTPUT, except that T designates the
*STANDARD-INPUT* and a stream requested through the :STREAM keyword
would be available through PROCESS-INFO-INPUT.

IF-INPUT-DOES-NOT-EXIST, which is only meaningful if INPUT is a string
or a pathname, can take the values :CREATE and :ERROR (the
default). The meaning of these values is analogous to the
IF-DOES-NOT-EXIST parameter to OPEN with :DIRECTION :INPUT.

ELEMENT-TYPE and EXTERNAL-FORMAT are passed on to your Lisp
implementation, when applicable, for creation of the output stream.

LAUNCH-PROGRAM returns a PROCESS-INFO object."
    #-(or abcl allegro clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)
    (progn command keys input output error-output directory element-type external-format
           if-input-does-not-exist if-output-exists if-error-output-exists ;; ignore
           (not-implemented-error 'launch-program))
    #+allegro
    (when (some #'(lambda (stream)
                    (and (streamp stream)
                         (not (file-stream-p stream))))
                (list input output error-output))
      (parameter-error "~S: Streams passed as I/O parameters need to be file streams on this lisp"
                       'launch-program))
    #+(or abcl clisp lispworks)
    (when (some #'streamp (list input output error-output))
      (parameter-error "~S: I/O parameters cannot be foreign streams on this lisp"
                       'launch-program))
    #+clisp
    (unless (eq error-output :interactive)
      (parameter-error "~S: The only admissible value for ~S is ~S on this lisp"
                       'launch-program :error-output :interactive))
    #+ecl
    (when (some #'(lambda (stream)
                    (and (streamp stream)
                         (not (file-or-synonym-stream-p stream))))
                (list input output error-output))
      (parameter-error "~S: Streams passed as I/O parameters need to be (synonymous with) file streams on this lisp"
                       'launch-program))
    #+(or abcl allegro clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)
    (nest
     (progn ;; see comments for these functions
       (%handle-if-does-not-exist input if-input-does-not-exist)
       (%handle-if-exists output if-output-exists)
       (%handle-if-exists error-output if-error-output-exists))
     #+ecl (let ((*standard-input* *stdin*)
                 (*standard-output* *stdout*)
                 (*error-output* *stderr*)))
     (let ((process-info (make-instance 'process-info))
           (input (%normalize-io-specifier input :input))
           (output (%normalize-io-specifier output :output))
           (error-output (%normalize-io-specifier error-output :error-output))
           #+(and allegro os-windows) (interactive (%interactivep input output error-output))
           (command
            (etypecase command
              #+os-unix (string `("/bin/sh" "-c" ,command))
              #+os-unix (list command)
              #+os-windows
              (string
               ;; NB: On other Windows implementations, this is utterly bogus
               ;; except in the most trivial cases where no quoting is needed.
               ;; Use at your own risk.
               #-(or allegro clisp clozure ecl)
               (nest
                #+(or ecl sbcl) (unless (find-symbol* :escape-arguments #+ecl :ext #+sbcl :sb-impl nil))
                (parameter-error "~S doesn't support string commands on Windows on this Lisp"
                                 'launch-program command))
               ;; NB: We add cmd /c here. Behavior without going through cmd is not well specified
               ;; when the command contains spaces or special characters:
               ;; IIUC, the system will use space as a separator,
               ;; but the C++ argv-decoding libraries won't, and
               ;; you're supposed to use an extra argument to CreateProcess to bridge the gap,
               ;; yet neither allegro nor clisp provide access to that argument.
               #+(or allegro clisp) (strcat "cmd /c " command)
               ;; On ClozureCL for Windows, we assume you are using
               ;; r15398 or later in 1.9 or later,
               ;; so that bug 858 is fixed http://trac.clozure.com/ccl/ticket/858
               ;; On ECL, commit 2040629 https://gitlab.com/embeddable-common-lisp/ecl/issues/304
               ;; On SBCL, we assume the patch from fcae0fd (to be part of SBCL 1.3.13)
               #+(or clozure ecl sbcl) (cons "cmd" (strcat "/c " command)))
              #+os-windows
              (list
               #+allegro (escape-windows-command command)
               #-allegro command)))))
     #+(or abcl (and allegro os-unix) clozure cmucl ecl mkcl sbcl)
     (let ((program (car command))
           #-allegro (arguments (cdr command))))
     #+(and (or ecl sbcl) os-windows)
     (multiple-value-bind (arguments escape-arguments)
         (if (listp arguments)
             (values arguments t)
             (values (list arguments) nil)))
     #-(or allegro mkcl sbcl) (with-current-directory (directory))
     (multiple-value-bind
       #+(or abcl clozure cmucl sbcl scl) (process)
       #+allegro (in-or-io out-or-err err-or-pid pid-or-nil)
       #+ecl (stream code process)
       #+lispworks (io-or-pid err-or-nil #-lispworks7+ pid-or-nil)
       #+mkcl (stream process code)
       #.`(apply
           #+abcl 'sys:run-program
           #+allegro ,@'('excl:run-shell-command
                         #+os-unix (coerce (cons program command) 'vector)
                         #+os-windows command)
           #+clozure 'ccl:run-program
           #+(or cmucl ecl scl) 'ext:run-program
           #+lispworks ,@'('system:run-shell-command `("/usr/bin/env" ,@command)) ; full path needed
           #+mkcl 'mk-ext:run-program
           #+sbcl 'sb-ext:run-program
           #+(or abcl clozure cmucl ecl mkcl sbcl) ,@'(program arguments)
           #+(and (or ecl sbcl) os-windows) ,@'(:escape-arguments escape-arguments)
           :input input :if-input-does-not-exist :error
           :output output :if-output-exists :append
           ,(or #+(or allegro lispworks) :error-output :error) error-output
           ,(or #+(or allegro lispworks) :if-error-output-exists :if-error-exists) :append
           :wait nil :element-type element-type :external-format external-format
           :allow-other-keys t
           #+allegro ,@`(:directory directory
                         #+os-windows ,@'(:show-window (if interactive nil :hide)))
           #+lispworks ,@'(:save-exit-status t)
           #+mkcl ,@'(:directory (native-namestring directory))
           #-sbcl keys ;; on SBCL, don't pass :directory nil but remove it from the keys
           #+sbcl ,@'(:search t (if directory keys (remove-plist-key :directory keys)))))
     (labels ((prop (key value) (setf (slot-value process-info key) value)))
       #+allegro
       (cond
         (separate-streams
          (prop 'process pid-or-nil)
          (when (eq input :stream) (prop 'input-stream in-or-io))
          (when (eq output :stream) (prop 'output-stream out-or-err))
          (when (eq error-output :stream) (prop 'error-stream err-or-pid)))
         (t
          (prop 'process err-or-pid)
          (ecase (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))
            (0)
            (1 (prop 'input-stream in-or-io))
            (2 (prop 'output-stream in-or-io))
            (3 (prop 'bidir-stream in-or-io)))
          (when (eq error-output :stream)
            (prop 'error-stream out-or-err))))
       #+(or abcl clozure cmucl sbcl scl)
       (progn
         (prop 'process process)
         (when (eq input :stream)
           (nest
            (prop 'input-stream)
            #+abcl (symbol-call :sys :process-input)
            #+clozure (ccl:external-process-input-stream)
            #+(or cmucl scl) (ext:process-input)
            #+sbcl (sb-ext:process-input)
            process))
         (when (eq output :stream)
           (nest
            (prop 'output-stream)
            #+abcl (symbol-call :sys :process-output)
            #+clozure (ccl:external-process-output-stream)
            #+(or cmucl scl) (ext:process-output)
            #+sbcl (sb-ext:process-output)
            process))
         (when (eq error-output :stream)
           (nest
            (prop 'error-output-stream)
            #+abcl (symbol-call :sys :process-error)
            #+clozure (ccl:external-process-error-stream)
            #+(or cmucl scl) (ext:process-error)
            #+sbcl (sb-ext:process-error)
            process)))
       #+(or ecl mkcl)
       (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
         code ;; ignore
         (unless (zerop mode)
           (prop (case mode (1 'input-stream) (2 'output-stream) (3 'bidir-stream)) stream))
         (prop 'process process))
       #+lispworks
       ;; See also the comments on the process-info class
       (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
         (cond
           ((or (plusp mode) (eq error-output :stream))
            (prop 'process #+lispworks7+ io-or-pid #-lispworks7+ pid-or-nil)
            (when (plusp mode)
              (prop (ecase mode (1 'input-stream) (2 'output-stream) (3 'bidir-stream))
                    io-or-pid))
            (when (eq error-output :stream)
              (prop 'error-stream err-or-nil)))
           ;; Prior to Lispworks 7, this returned (pid); now it
           ;; returns (io err pid) of which we keep io.
           (t (prop 'process io-or-pid)))))
     process-info)))

