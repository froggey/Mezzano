;;;; -------------------------------------------------------------------------
;;;; run-program initially from xcvb-driver.

(uiop/package:define-package :uiop/run-program
  (:nicknames :asdf/run-program) ; OBSOLETE. Used by cl-sane, printv.
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/version
   :uiop/pathname :uiop/os :uiop/filesystem :uiop/stream :uiop/launch-program)
  (:export
   #:run-program
   #:slurp-input-stream #:vomit-output-stream
   #:subprocess-error
   #:subprocess-error-code #:subprocess-error-command #:subprocess-error-process)
  (:import-from :uiop/launch-program
   #:%handle-if-does-not-exist #:%handle-if-exists #:%interactivep
   #:input-stream #:output-stream #:error-output-stream))
(in-package :uiop/run-program)

;;;; Slurping a stream, typically the output of another program
(with-upgradability ()
  (defun call-stream-processor (fun processor stream)
    "Given FUN (typically SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM,
a PROCESSOR specification which is either an atom or a list specifying
a processor an keyword arguments, call the specified processor with
the given STREAM as input"
    (if (consp processor)
        (apply fun (first processor) stream (rest processor))
        (funcall fun processor stream)))

  (defgeneric slurp-input-stream (processor input-stream &key)
    (:documentation
     "SLURP-INPUT-STREAM is a generic function with two positional arguments
PROCESSOR and INPUT-STREAM and additional keyword arguments, that consumes (slurps)
the contents of the INPUT-STREAM and processes them according to a method
specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the INPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.  It will be applied to a cons of the
  INPUT-STREAM and the rest of the list.  That is (x . y) will be treated as
    \(APPLY x <stream> y\)
* if PROCESSOR is an output-stream, the contents of INPUT-STREAM is copied to the output-stream,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is the symbol CL:STRING or the keyword :STRING, then the contents of INPUT-STREAM
  are returned as a string, as per SLURP-STREAM-STRING.
* if PROCESSOR is the keyword :LINES then the INPUT-STREAM will be handled by SLURP-STREAM-LINES.
* if PROCESSOR is the keyword :LINE then the INPUT-STREAM will be handled by SLURP-STREAM-LINE.
* if PROCESSOR is the keyword :FORMS then the INPUT-STREAM will be handled by SLURP-STREAM-FORMS.
* if PROCESSOR is the keyword :FORM then the INPUT-STREAM will be handled by SLURP-STREAM-FORM.
* if PROCESSOR is T, it is treated the same as *standard-output*. If it is NIL, NIL is returned.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod slurp-input-stream ((function function) input-stream &key)
    (funcall function input-stream))

  (defmethod slurp-input-stream ((list cons) input-stream &key)
    (apply (first list) input-stream (rest list)))

  #-genera
  (defmethod slurp-input-stream ((output-stream stream) input-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod slurp-input-stream ((x (eql 'string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :lines)) stream &key count)
    (slurp-stream-lines stream :count count))

  (defmethod slurp-input-stream ((x (eql :line)) stream &key (at 0))
    (slurp-stream-line stream :at at))

  (defmethod slurp-input-stream ((x (eql :forms)) stream &key count)
    (slurp-stream-forms stream :count count))

  (defmethod slurp-input-stream ((x (eql :form)) stream &key (at 0))
    (slurp-stream-form stream :at at))

  (defmethod slurp-input-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'slurp-input-stream *standard-output* stream keys))

  (defmethod slurp-input-stream ((x null) (stream t) &key)
    nil)

  (defmethod slurp-input-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod slurp-input-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((output-stream-p x)
       (copy-stream-to-stream
        stream x
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (parameter-error "Invalid ~S destination ~S" 'slurp-input-stream x)))))

;;;; Vomiting a stream, typically into the input of another program.
(with-upgradability ()
  (defgeneric vomit-output-stream (processor output-stream &key)
    (:documentation
     "VOMIT-OUTPUT-STREAM is a generic function with two positional arguments
PROCESSOR and OUTPUT-STREAM and additional keyword arguments, that produces (vomits)
some content onto the OUTPUT-STREAM, according to a method specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the OUTPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.
  It will be applied to a cons of the OUTPUT-STREAM and the rest of the list.
  That is (x . y) will be treated as \(APPLY x <stream> y\)
* if PROCESSOR is an input-stream, its contents will be copied the OUTPUT-STREAM,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is a string, its contents will be printed to the OUTPUT-STREAM.
* if PROCESSOR is T, it is treated the same as *standard-input*. If it is NIL, nothing is done.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod vomit-output-stream ((function function) output-stream &key)
    (funcall function output-stream))

  (defmethod vomit-output-stream ((list cons) output-stream &key)
    (apply (first list) output-stream (rest list)))

  #-genera
  (defmethod vomit-output-stream ((input-stream stream) output-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod vomit-output-stream ((x string) stream &key fresh-line terpri)
    (princ x stream)
    (when fresh-line (fresh-line stream))
    (when terpri (terpri stream))
    (values))

  (defmethod vomit-output-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'vomit-output-stream *standard-input* stream keys))

  (defmethod vomit-output-stream ((x null) (stream t) &key)
    (values))

  (defmethod vomit-output-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod vomit-output-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((input-stream-p x)
       (copy-stream-to-stream
        x stream
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (parameter-error "Invalid ~S source ~S" 'vomit-output-stream x)))))


;;;; Run-program: synchronously run a program in a subprocess, handling input, output and error-output.
(with-upgradability ()
  (define-condition subprocess-error (error)
    ((code :initform nil :initarg :code :reader subprocess-error-code)
     (command :initform nil :initarg :command :reader subprocess-error-command)
     (process :initform nil :initarg :process :reader subprocess-error-process))
    (:report (lambda (condition stream)
               (format stream "Subprocess ~@[~S~% ~]~@[with command ~S~% ~]exited with error~@[ code ~D~]"
                       (subprocess-error-process condition)
                       (subprocess-error-command condition)
                       (subprocess-error-code condition)))))

  (defun %check-result (exit-code &key command process ignore-error-status)
    (unless ignore-error-status
      (unless (eql exit-code 0)
        (cerror "IGNORE-ERROR-STATUS"
                'subprocess-error :command command :code exit-code :process process)))
    exit-code)

  (defun %active-io-specifier-p (specifier)
    "Determines whether a run-program I/O specifier requires Lisp-side processing
via SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM (return T),
or whether it's already taken care of by the implementation's underlying run-program."
    (not (typep specifier '(or null string pathname (member :interactive :output)
                            #+(or cmucl (and sbcl os-unix) scl) (or stream (eql t))
                            #+lispworks file-stream))))

  (defun %run-program (command &rest keys &key &allow-other-keys)
    "DEPRECATED. Use LAUNCH-PROGRAM instead."
    (apply 'launch-program command keys))

  (defun %call-with-program-io (gf tval stream-easy-p fun direction spec activep returner
                                &key
                                  (element-type #-clozure *default-stream-element-type* #+clozure 'character)
                                  (external-format *utf-8-external-format*) &allow-other-keys)
    ;; handle redirection for run-program and system
    ;; SPEC is the specification for the subprocess's input or output or error-output
    ;; TVAL is the value used if the spec is T
    ;; GF is the generic function to call to handle arbitrary values of SPEC
    ;; STREAM-EASY-P is T if we're going to use a RUN-PROGRAM that copies streams in the background
    ;; (it's only meaningful on CMUCL, SBCL, SCL that actually do it)
    ;; DIRECTION is :INPUT, :OUTPUT or :ERROR-OUTPUT for the direction of this io argument
    ;; FUN is a function of the new reduced spec and an activity function to call with a stream
    ;; when the subprocess is active and communicating through that stream.
    ;; ACTIVEP is a boolean true if we will get to run code while the process is running
    ;; ELEMENT-TYPE and EXTERNAL-FORMAT control what kind of temporary file we may open.
    ;; RETURNER is a function called with the value of the activity.
    ;; --- TODO (fare@tunes.org): handle if-output-exists and such when doing it the hard way.
    (declare (ignorable stream-easy-p))
    (let* ((actual-spec (if (eq spec t) tval spec))
           (activity-spec (if (eq actual-spec :output)
                              (ecase direction
                                ((:input :output)
                                 (parameter-error "~S does not allow ~S as a ~S spec"
                                                  'run-program :output direction))
                                ((:error-output)
                                 nil))
                              actual-spec)))
      (labels ((activity (stream)
                 (call-function returner (call-stream-processor gf activity-spec stream)))
               (easy-case ()
                 (funcall fun actual-spec nil))
               (hard-case ()
                 (if activep
                     (funcall fun :stream #'activity)
                     (with-temporary-file (:pathname tmp)
                       (ecase direction
                         (:input
                          (with-output-file (s tmp :if-exists :overwrite
                                               :external-format external-format
                                               :element-type element-type)
                            (activity s))
                          (funcall fun tmp nil))
                         ((:output :error-output)
                          (multiple-value-prog1 (funcall fun tmp nil)
                            (with-input-file (s tmp
                                               :external-format external-format
                                               :element-type element-type)
                              (activity s)))))))))
        (typecase activity-spec
          ((or null string pathname (eql :interactive))
           (easy-case))
          #+(or cmucl (and sbcl os-unix) scl) ;; streams are only easy on implementations that try very hard
          (stream
           (if stream-easy-p (easy-case) (hard-case)))
          (t
           (hard-case))))))

  (defmacro place-setter (place)
    (when place
      (let ((value (gensym)))
        `#'(lambda (,value) (setf ,place ,value)))))

  (defmacro with-program-input (((reduced-input-var
                                  &optional (input-activity-var (gensym) iavp))
                                 input-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'vomit-output-stream *standard-input* ,stream-easy-p
            #'(lambda (,reduced-input-var ,input-activity-var)
                ,@(unless iavp `((declare (ignore ,input-activity-var))))
                ,@body)
            :input ,input-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-output (((reduced-output-var
                                  &optional (output-activity-var (gensym) oavp))
                                  output-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *standard-output* ,stream-easy-p
            #'(lambda (,reduced-output-var ,output-activity-var)
                ,@(unless oavp `((declare (ignore ,output-activity-var))))
                ,@body)
            :output ,output-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-error-output (((reduced-error-output-var
                                         &optional (error-output-activity-var (gensym) eoavp))
                                        error-output-form &key setf stream-easy-p active keys)
                                       &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *error-output* ,stream-easy-p
            #'(lambda (,reduced-error-output-var ,error-output-activity-var)
                ,@(unless eoavp `((declare (ignore ,error-output-activity-var))))
                ,@body)
            :error-output ,error-output-form ,active (place-setter ,setf) ,keys))

  (defun %use-launch-program (command &rest keys
                           &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using LAUNCH-PROGRAM
    #+(or cormanlisp gcl (and lispworks os-windows) mcl xcl)
    (progn
      command keys input output error-output ignore-error-status ;; ignore
      (not-implemented-error '%use-launch-program))
    (when (member :stream (list input output error-output))
      (parameter-error "~S: ~S is not allowed as synchronous I/O redirection argument"
                       'run-program :stream))
    (let* ((active-input-p (%active-io-specifier-p input))
           (active-output-p (%active-io-specifier-p output))
           (active-error-output-p (%active-io-specifier-p error-output))
           (activity
             (cond
               (active-output-p :output)
               (active-input-p :input)
               (active-error-output-p :error-output)
               (t nil)))
           output-result error-output-result exit-code process-info)
      (with-program-output ((reduced-output output-activity)
                            output :keys keys :setf output-result
                            :stream-easy-p t :active (eq activity :output))
        (with-program-error-output ((reduced-error-output error-output-activity)
                                    error-output :keys keys :setf error-output-result
                                    :stream-easy-p t :active (eq activity :error-output))
          (with-program-input ((reduced-input input-activity)
                               input :keys keys
                               :stream-easy-p t :active (eq activity :input))
            (setf process-info
                  (apply 'launch-program command
                         :input reduced-input :output reduced-output
                         :error-output (if (eq error-output :output) :output reduced-error-output)
                         keys))
            (labels ((get-stream (stream-name &optional fallbackp)
                       (or (slot-value process-info stream-name)
                           (when fallbackp
                             (slot-value process-info 'bidir-stream))))
                     (run-activity (activity stream-name &optional fallbackp)
                       (if-let (stream (get-stream stream-name fallbackp))
                         (funcall activity stream)
                         (error 'subprocess-error
                                :code `(:missing ,stream-name)
                                :command command :process process-info))))
              (unwind-protect
                   (ecase activity
                     ((nil))
                     (:input (run-activity input-activity 'input-stream t))
                     (:output (run-activity output-activity 'output-stream t))
                     (:error-output (run-activity error-output-activity 'error-output-stream)))
                (close-streams process-info)
                (setf exit-code (wait-process process-info)))))))
      (%check-result exit-code
                     :command command :process process-info
                     :ignore-error-status ignore-error-status)
      (values output-result error-output-result exit-code)))

  (defun %normalize-system-command (command) ;; helper for %USE-SYSTEM
    (etypecase command
      (string command)
      (list (escape-shell-command
             (os-cond
              ((os-unix-p) (cons "exec" command))
              (t command))))))

  (defun %redirected-system-command (command in out err directory) ;; helper for %USE-SYSTEM
    (flet ((redirect (spec operator)
             (let ((pathname
                     (typecase spec
                       (null (null-device-pathname))
                       (string (parse-native-namestring spec))
                       (pathname spec)
                       ((eql :output)
                        (unless (equal operator " 2>>")
                          (parameter-error "~S: only the ~S argument can be ~S"
                                           'run-program :error-output :output))
                        (return-from redirect '(" 2>&1"))))))
               (when pathname
                 (list operator " "
                       (escape-shell-token (native-namestring pathname)))))))
      (let* ((redirections (append (redirect in " <") (redirect out " >>") (redirect err " 2>>")))
             (normalized (%normalize-system-command command))
             (directory (or directory #+(or abcl xcl) (getcwd)))
             (chdir (when directory
                      (let ((dir-arg (escape-shell-token (native-namestring directory))))
                        (os-cond
                         ((os-unix-p) `("cd " ,dir-arg " ; "))
                         ((os-windows-p) `("cd /d " ,dir-arg " & ")))))))
        (reduce/strcat
         (os-cond
          ((os-unix-p) `(,@(when redirections `("exec " ,@redirections " ; ")) ,@chdir ,normalized))
          ((os-windows-p) `(,@redirections " (" ,@chdir ,normalized ")")))))))

  (defun %system (command &rest keys &key directory
                                       input (if-input-does-not-exist :error)
                                       output (if-output-exists :supersede)
                                       error-output (if-error-output-exists :supersede)
                                       &allow-other-keys)
    "A portable abstraction of a low-level call to libc's system()."
    (declare (ignorable keys directory input if-input-does-not-exist output
                        if-output-exists error-output if-error-output-exists))
    (when (member :stream (list input output error-output))
      (parameter-error "~S: ~S is not allowed as synchronous I/O redirection argument"
                       'run-program :stream))
    #+(or abcl allegro clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)
    (let (#+(or abcl ecl mkcl)
            (version (parse-version
                      #-abcl
                      (lisp-implementation-version)
                      #+abcl
                      (second (split-string (implementation-identifier) :separator '(#\-))))))
      (nest
       #+abcl (unless (lexicographic< '< version '(1 4 0)))
       #+ecl (unless (lexicographic<= '< version '(16 0 0)))
       #+mkcl (unless (lexicographic<= '< version '(1 1 9)))
       (return-from %system
         (wait-process
          (apply 'launch-program (%normalize-system-command command) keys)))))
    #+(or abcl clasp clisp cormanlisp ecl gcl genera (and lispworks os-windows) mkcl xcl)
    (let ((%command (%redirected-system-command command input output error-output directory)))
      ;; see comments for these functions
      (%handle-if-does-not-exist input if-input-does-not-exist)
      (%handle-if-exists output if-output-exists)
      (%handle-if-exists error-output if-error-output-exists)
      #+abcl (ext:run-shell-command %command)
      #+(or clasp ecl) (let ((*standard-input* *stdin*)
                             (*standard-output* *stdout*)
                             (*error-output* *stderr*))
                         (ext:system %command))
      #+clisp
      (let ((raw-exit-code
             (or
              #.`(#+os-windows ,@'(ext:run-shell-command %command)
                  #+os-unix ,@'(ext:run-program "/bin/sh" :arguments `("-c" ,%command))
                  :wait t :input :terminal :output :terminal)
              0)))
        (if (minusp raw-exit-code)
            (- 128 raw-exit-code)
            raw-exit-code))
      #+cormanlisp (win32:system %command)
      #+gcl (system:system %command)
      #+genera (not-implemented-error '%system)
      #+(and lispworks os-windows)
      (system:call-system %command :current-directory directory :wait t)
      #+mcl (ccl::with-cstrs ((%%command %command)) (_system %%command))
      #+mkcl (mkcl:system %command)
      #+xcl (system:%run-shell-command %command)))

  (defun %use-system (command &rest keys
                      &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using %system
    (let (output-result error-output-result exit-code)
      (with-program-output ((reduced-output)
                            output :keys keys :setf output-result)
        (with-program-error-output ((reduced-error-output)
                                    error-output :keys keys :setf error-output-result)
          (with-program-input ((reduced-input) input :keys keys)
            (setf exit-code (apply '%system command
                                   :input reduced-input :output reduced-output
                                   :error-output reduced-error-output keys)))))
      (%check-result exit-code
                     :command command
                     :ignore-error-status ignore-error-status)
      (values output-result error-output-result exit-code)))

  (defun run-program (command &rest keys
                       &key ignore-error-status (force-shell nil force-shell-suppliedp)
                         input (if-input-does-not-exist :error)
                         output (if-output-exists :supersede)
                         error-output (if-error-output-exists :supersede)
                         (element-type #-clozure *default-stream-element-type* #+clozure 'character)
                         (external-format *utf-8-external-format*)
                       &allow-other-keys)
    "Run program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on Windows);
_synchronously_ process its output as specified and return the processing results
when the program and its output processing are complete.

Always call a shell (rather than directly execute the command when possible)
if FORCE-SHELL is specified.  Similarly, never call a shell if FORCE-SHELL is
specified to be NIL.

Signal a continuable SUBPROCESS-ERROR if the process wasn't successful (exit-code 0),
unless IGNORE-ERROR-STATUS is specified.

If OUTPUT is a pathname, a string designating a pathname, or NIL (the default)
designating the null device, the file at that path is used as output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*,
and under SLIME will be on your *inferior-lisp* buffer.
If it's T, output goes to your current *STANDARD-OUTPUT* stream.
Otherwise, OUTPUT should be a value that is a suitable first argument to
SLURP-INPUT-STREAM (qv.), or a list of such a value and keyword arguments.
In this case, RUN-PROGRAM will create a temporary stream for the program output;
the program output, in that stream, will be processed by a call to SLURP-INPUT-STREAM,
using OUTPUT as the first argument (or the first element of OUTPUT, and the rest as keywords).
The primary value resulting from that call (or NIL if no call was needed)
will be the first value returned by RUN-PROGRAM.
E.g., using :OUTPUT :STRING will have it return the entire output stream as a string.
And using :OUTPUT '(:STRING :STRIPPED T) will have it return the same string
stripped of any ending newline.

IF-OUTPUT-EXISTS, which is only meaningful if OUTPUT is a string or a
pathname, can take the values :ERROR, :APPEND, and :SUPERSEDE (the
default). The meaning of these values and their effect on the case
where OUTPUT does not exist, is analogous to the IF-EXISTS parameter
to OPEN with :DIRECTION :OUTPUT.

ERROR-OUTPUT is similar to OUTPUT, except that the resulting value is returned
as the second value of RUN-PROGRAM. T designates the *ERROR-OUTPUT*.
Also :OUTPUT means redirecting the error output to the output stream,
in which case NIL is returned.

IF-ERROR-OUTPUT-EXISTS is similar to IF-OUTPUT-EXIST, except that it
affects ERROR-OUTPUT rather than OUTPUT.

INPUT is similar to OUTPUT, except that VOMIT-OUTPUT-STREAM is used,
no value is returned, and T designates the *STANDARD-INPUT*.

IF-INPUT-DOES-NOT-EXIST, which is only meaningful if INPUT is a string
or a pathname, can take the values :CREATE and :ERROR (the
default). The meaning of these values is analogous to the
IF-DOES-NOT-EXIST parameter to OPEN with :DIRECTION :INPUT.

ELEMENT-TYPE and EXTERNAL-FORMAT are passed on
to your Lisp implementation, when applicable, for creation of the output stream.

One and only one of the stream slurping or vomiting may or may not happen
in parallel in parallel with the subprocess,
depending on options and implementation,
and with priority being given to output processing.
Other streams are completely produced or consumed
before or after the subprocess is spawned, using temporary files.

RUN-PROGRAM returns 3 values:
0- the result of the OUTPUT slurping if any, or NIL
1- the result of the ERROR-OUTPUT slurping if any, or NIL
2- either 0 if the subprocess exited with success status,
or an indication of failure via the EXIT-CODE of the process"
    (declare (ignorable input output error-output if-input-does-not-exist if-output-exists
                        if-error-output-exists element-type external-format ignore-error-status))
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl lispworks mcl mkcl sbcl scl xcl)
    (not-implemented-error 'run-program)
    (apply (if (or force-shell
                   ;; Per doc string, set FORCE-SHELL to T if we get command as a string.
                   ;; But don't override user's specified preference. [2015/06/29:rpg]
                   (and (stringp command)
                        (or (not force-shell-suppliedp)
                            #-(or allegro clisp clozure sbcl) (os-cond ((os-windows-p) t))))
                   #+(or clasp clisp cormanlisp gcl (and lispworks os-windows) mcl xcl) t
                   ;; A race condition in ECL <= 16.0.0 prevents using ext:run-program
                   #+ecl #.(if-let (ver (parse-version (lisp-implementation-version)))
                                   (lexicographic<= '< ver '(16 0 0)))
                   #+(and lispworks os-unix) (%interactivep input output error-output))
               '%use-system '%use-launch-program)
           command keys)))

