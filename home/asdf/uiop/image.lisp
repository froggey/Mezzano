;;;; -------------------------------------------------------------------------
;;;; Starting, Stopping, Dumping a Lisp image

(uiop/package:define-package :uiop/image
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/pathname :uiop/stream :uiop/os)
  (:export
   #:*image-dumped-p* #:raw-command-line-arguments #:*command-line-arguments*
   #:command-line-arguments #:raw-command-line-arguments #:setup-command-line-arguments #:argv0
   #:*lisp-interaction*
   #:fatal-condition #:fatal-condition-p
   #:handle-fatal-condition
   #:call-with-fatal-condition-handler #:with-fatal-condition-handler
   #:*image-restore-hook* #:*image-prelude* #:*image-entry-point*
   #:*image-postlude* #:*image-dump-hook*
   #:quit #:die #:raw-print-backtrace #:print-backtrace #:print-condition-backtrace
   #:shell-boolean-exit
   #:register-image-restore-hook #:register-image-dump-hook
   #:call-image-restore-hook #:call-image-dump-hook
   #:restore-image #:dump-image #:create-image
))
(in-package :uiop/image)

(with-upgradability ()
  (defvar *lisp-interaction* t
    "Is this an interactive Lisp environment, or is it batch processing?")

  (defvar *command-line-arguments* nil
    "Command-line arguments")

  (defvar *image-dumped-p* nil ; may matter as to how to get to command-line-arguments
    "Is this a dumped image? As a standalone executable?")

  (defvar *image-restore-hook* nil
    "Functions to call (in reverse order) when the image is restored")

  (defvar *image-restored-p* nil
    "Has the image been restored? A boolean, or :in-progress while restoring, :in-regress while dumping")

  (defvar *image-prelude* nil
    "a form to evaluate, or string containing forms to read and evaluate
when the image is restarted, but before the entry point is called.")

  (defvar *image-entry-point* nil
    "a function with which to restart the dumped image when execution is restored from it.")

  (defvar *image-postlude* nil
    "a form to evaluate, or string containing forms to read and evaluate
before the image dump hooks are called and before the image is dumped.")

  (defvar *image-dump-hook* nil
    "Functions to call (in order) when before an image is dumped"))

(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute)
  (deftype fatal-condition ()
    `(and serious-condition #+clozure (not ccl:process-reset))))

;;; Exiting properly or im-
(with-upgradability ()
  (defun quit (&optional (code 0) (finish-output t))
    "Quits from the Lisp world, with the given exit status if provided.
This is designed to abstract away the implementation specific quit forms."
    (when finish-output ;; essential, for ClozureCL, and for standard compliance.
      (finish-outputs))
    #+(or abcl xcl) (ext:quit :status code)
    #+allegro (excl:exit code :quiet t)
    #+(or clasp ecl) (si:quit code)
    #+clisp (ext:quit code)
    #+clozure (ccl:quit code)
    #+cormanlisp (win32:exitprocess code)
    #+(or cmucl scl) (unix:unix-exit code)
    #+gcl (system:quit code)
    #+genera (error "~S: You probably don't want to Halt Genera. (code: ~S)" 'quit code)
    #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
    #+mcl (progn code (ccl:quit)) ;; or should we use FFI to call libc's exit(3) ?
    #+mkcl (mk-ext:quit :exit-code code)
    #+sbcl #.(let ((exit (find-symbol* :exit :sb-ext nil))
                   (quit (find-symbol* :quit :sb-ext nil)))
               (cond
                 (exit `(,exit :code code :abort (not finish-output)))
                 (quit `(,quit :unix-status code :recklessly-p (not finish-output)))))
    #-(or abcl allegro clasp clisp clozure cmucl ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
    (not-implemented-error 'quit "(called with exit code ~S)" code))

  (defun die (code format &rest arguments)
    "Die in error with some error message"
    (with-safe-io-syntax ()
      (ignore-errors
       (format! *stderr* "~&~?~&" format arguments)))
    (quit code))

  (defun raw-print-backtrace (&key (stream *debug-io*) count condition)
    "Print a backtrace, directly accessing the implementation"
    (declare (ignorable stream count condition))
    #+abcl
    (loop :for i :from 0
          :for frame :in (sys:backtrace (or count most-positive-fixnum)) :do
            (safe-format! stream "~&~D: ~A~%" i frame))
    #+allegro
    (let ((*terminal-io* stream)
          (*standard-output* stream)
          (tpl:*zoom-print-circle* *print-circle*)
          (tpl:*zoom-print-level* *print-level*)
          (tpl:*zoom-print-length* *print-length*))
      (tpl:do-command "zoom"
        :from-read-eval-print-loop nil
        :count (or count t)
        :all t))
    #+(or clasp ecl mkcl)
    (let* ((top (si:ihs-top))
           (repeats (if count (min top count) top))
           (backtrace (loop :for ihs :from 0 :below top
                            :collect (list (si::ihs-fun ihs)
                                           (si::ihs-env ihs)))))
      (loop :for i :from 0 :below repeats
            :for frame :in (nreverse backtrace) :do
              (safe-format! stream "~&~D: ~S~%" i frame)))
    #+clisp
    (system::print-backtrace :out stream :limit count)
    #+(or clozure mcl)
    (let ((*debug-io* stream))
      #+clozure (ccl:print-call-history :count count :start-frame-number 1)
      #+mcl (ccl:print-call-history :detailed-p nil)
      (finish-output stream))
    #+(or cmucl scl)
    (let ((debug:*debug-print-level* *print-level*)
          (debug:*debug-print-length* *print-length*))
      (debug:backtrace (or count most-positive-fixnum) stream))
    #+gcl
    (let ((*debug-io* stream))
      (ignore-errors
       (with-safe-io-syntax ()
         (if condition
             (conditions::condition-backtrace condition)
             (system::simple-backtrace)))))
    #+lispworks
    (let ((dbg::*debugger-stack*
            (dbg::grab-stack nil :how-many (or count most-positive-fixnum)))
          (*debug-io* stream)
          (dbg:*debug-print-level* *print-level*)
          (dbg:*debug-print-length* *print-length*))
      (dbg:bug-backtrace nil))
    #+mezzano
    (mezzano.debug:backtrace :stream stream :limit count)
    #+sbcl
    (sb-debug:print-backtrace :stream stream :count (or count most-positive-fixnum))
    #+xcl
    (loop :for i :from 0 :below (or count most-positive-fixnum)
          :for frame :in (extensions:backtrace-as-list) :do
            (safe-format! stream "~&~D: ~S~%" i frame)))

  (defun print-backtrace (&rest keys &key stream count condition)
    "Print a backtrace"
    (declare (ignore stream count condition))
    (with-safe-io-syntax (:package :cl)
      (let ((*print-readably* nil)
            (*print-circle* t)
            (*print-miser-width* 75)
            (*print-length* nil)
            (*print-level* nil)
            (*print-pretty* t))
        (ignore-errors (apply 'raw-print-backtrace keys)))))

  (defun print-condition-backtrace (condition &key (stream *stderr*) count)
    "Print a condition after a backtrace triggered by that condition"
    ;; We print the condition *after* the backtrace,
    ;; for the sake of who sees the backtrace at a terminal.
    ;; It is up to the caller to print the condition *before*, with some context.
    (print-backtrace :stream stream :count count :condition condition)
    (when condition
      (safe-format! stream "~&Above backtrace due to this condition:~%~A~&"
                    condition)))

  (defun fatal-condition-p (condition)
    "Is the CONDITION fatal?"
    (typep condition 'fatal-condition))

  (defun handle-fatal-condition (condition)
    "Handle a fatal CONDITION:
depending on whether *LISP-INTERACTION* is set, enter debugger or die"
    (cond
      (*lisp-interaction*
       (invoke-debugger condition))
      (t
       (safe-format! *stderr* "~&Fatal condition:~%~A~%" condition)
       (print-condition-backtrace condition :stream *stderr*)
       (die 99 "~A" condition))))

  (defun call-with-fatal-condition-handler (thunk)
    "Call THUNK in a context where fatal conditions are appropriately handled"
    (handler-bind ((fatal-condition #'handle-fatal-condition))
      (funcall thunk)))

  (defmacro with-fatal-condition-handler ((&optional) &body body)
    "Execute BODY in a context where fatal conditions are appropriately handled"
    `(call-with-fatal-condition-handler #'(lambda () ,@body)))

  (defun shell-boolean-exit (x)
    "Quit with a return code that is 0 iff argument X is true"
    (quit (if x 0 1))))


;;; Using image hooks
(with-upgradability ()
  (defun register-image-restore-hook (hook &optional (call-now-p t))
    "Regiter a hook function to be run when restoring a dumped image"
    (register-hook-function '*image-restore-hook* hook call-now-p))

  (defun register-image-dump-hook (hook &optional (call-now-p nil))
    "Register a the hook function to be run before to dump an image"
    (register-hook-function '*image-dump-hook* hook call-now-p))

  (defun call-image-restore-hook ()
    "Call the hook functions registered to be run when restoring a dumped image"
    (call-functions (reverse *image-restore-hook*)))

  (defun call-image-dump-hook ()
    "Call the hook functions registered to be run before to dump an image"
    (call-functions *image-dump-hook*)))


;;; Proper command-line arguments
(with-upgradability ()
  (defun raw-command-line-arguments ()
    "Find what the actual command line for this process was."
    #+abcl ext:*command-line-argument-list* ; Use 1.0.0 or later!
    #+allegro (sys:command-line-arguments) ; default: :application t
    #+(or clasp ecl) (loop :for i :from 0 :below (si:argc) :collect (si:argv i))
    #+clisp (coerce (ext:argv) 'list)
    #+clozure ccl:*command-line-argument-list*
    #+(or cmucl scl) extensions:*command-line-strings*
    #+gcl si:*command-args*
    #+(or genera mcl mezzano) nil
    #+lispworks sys:*line-arguments-list*
    #+mkcl (loop :for i :from 0 :below (mkcl:argc) :collect (mkcl:argv i))
    #+sbcl sb-ext:*posix-argv*
    #+xcl system:*argv*
    #-(or abcl allegro clasp clisp clozure cmucl ecl gcl genera lispworks mcl mezzano mkcl sbcl scl xcl)
    (not-implemented-error 'raw-command-line-arguments))

  (defun command-line-arguments (&optional (arguments (raw-command-line-arguments)))
    "Extract user arguments from command-line invocation of current process.
Assume the calling conventions of a generated script that uses --
if we are not called from a directly executable image."
    (block nil
      #+abcl (return arguments)
      ;; SBCL and Allegro already separate user arguments from implementation arguments.
      #-(or sbcl allegro)
      (unless (eq *image-dumped-p* :executable)
        ;; LispWorks command-line processing isn't transparent to the user
        ;; unless you create a standalone executable; in that case,
        ;; we rely on cl-launch or some other script to set the arguments for us.
        #+lispworks (return *command-line-arguments*)
        ;; On other implementations, on non-standalone executables,
        ;; we trust cl-launch or whichever script starts the program
        ;; to use -- as a delimiter between implementation arguments and user arguments.
        #-lispworks (setf arguments (member "--" arguments :test 'string-equal)))
      (rest arguments)))

  (defun argv0 ()
    "On supported implementations (most that matter), or when invoked by a proper wrapper script,
return a string that for the name with which the program was invoked, i.e. argv[0] in C.
Otherwise, return NIL."
    (cond
      ((eq *image-dumped-p* :executable) ; yes, this ARGV0 is our argv0 !
       ;; NB: not currently available on ABCL, Corman, Genera, MCL
       (or #+(or allegro clisp clozure cmucl gcl lispworks sbcl scl xcl)
           (first (raw-command-line-arguments))
           #+(or clasp ecl) (si:argv 0) #+mkcl (mkcl:argv 0)))
      (t ;; argv[0] is the name of the interpreter.
       ;; The wrapper script can export __CL_ARGV0. cl-launch does as of 4.0.1.8.
       (getenvp "__CL_ARGV0"))))

  (defun setup-command-line-arguments ()
    (setf *command-line-arguments* (command-line-arguments)))

  (defun restore-image (&key
                          (lisp-interaction *lisp-interaction*)
                          (restore-hook *image-restore-hook*)
                          (prelude *image-prelude*)
                          (entry-point *image-entry-point*)
                          (if-already-restored '(cerror "RUN RESTORE-IMAGE ANYWAY")))
    "From a freshly restarted Lisp image, restore the saved Lisp environment
by setting appropriate variables, running various hooks, and calling any specified entry point.

If the image has already been restored or is already being restored, as per *IMAGE-RESTORED-P*,
call the IF-ALREADY-RESTORED error handler (by default, a continuable error), and do return
immediately to the surrounding restore process if allowed to continue.

Then, comes the restore process itself:
First, call each function in the RESTORE-HOOK,
in the order they were registered with REGISTER-IMAGE-RESTORE-HOOK.
Second, evaluate the prelude, which is often Lisp text that is read,
as per EVAL-INPUT.
Third, call the ENTRY-POINT function, if any is specified, with no argument.

The restore process happens in a WITH-FATAL-CONDITION-HANDLER, so that if LISP-INTERACTION is NIL,
any unhandled error leads to a backtrace and an exit with an error status.
If LISP-INTERACTION is NIL, the process also exits when no error occurs:
if neither restart nor entry function is provided, the program will exit with status 0 (success);
if a function was provided, the program will exit after the function returns (if it returns),
with status 0 if and only if the primary return value of result is generalized boolean true,
and with status 1 if this value is NIL.

If LISP-INTERACTION is true, unhandled errors will take you to the debugger, and the result
of the function will be returned rather than interpreted as a boolean designating an exit code."
    (when *image-restored-p*
      (if if-already-restored
          (call-function if-already-restored "Image already ~:[being ~;~]restored"
                         (eq *image-restored-p* t))
          (return-from restore-image)))
    (with-fatal-condition-handler ()
      (setf *lisp-interaction* lisp-interaction)
      (setf *image-restore-hook* restore-hook)
      (setf *image-prelude* prelude)
      (setf *image-restored-p* :in-progress)
      (call-image-restore-hook)
      (standard-eval-thunk prelude)
      (setf *image-restored-p* t)
      (let ((results (multiple-value-list
                      (if entry-point
                          (call-function entry-point)
                          t))))
        (if lisp-interaction
            (values-list results)
            (shell-boolean-exit (first results)))))))


;;; Dumping an image

(with-upgradability ()
  (defun dump-image (filename &key output-name executable
                                (postlude *image-postlude*)
                                (dump-hook *image-dump-hook*)
                                #+clozure prepend-symbols #+clozure (purify t)
                                #+sbcl compression
                                #+(and sbcl os-windows) application-type)
    "Dump an image of the current Lisp environment at pathname FILENAME, with various options.

First, finalize the image, by evaluating the POSTLUDE as per EVAL-INPUT, then calling each of
 the functions in DUMP-HOOK, in reverse order of registration by REGISTER-DUMP-HOOK.

If EXECUTABLE is true, create an standalone executable program that calls RESTORE-IMAGE on startup.

Pass various implementation-defined options, such as PREPEND-SYMBOLS and PURITY on CCL,
or COMPRESSION on SBCL, and APPLICATION-TYPE on SBCL/Windows."
    ;; Note: at least SBCL saves only global values of variables in the heap image,
    ;; so make sure things you want to dump are NOT just local bindings shadowing the global values.
    (declare (ignorable filename output-name executable))
    (setf *image-dumped-p* (if executable :executable t))
    (setf *image-restored-p* :in-regress)
    (setf *image-postlude* postlude)
    (standard-eval-thunk *image-postlude*)
    (setf *image-dump-hook* dump-hook)
    (call-image-dump-hook)
    (setf *image-restored-p* nil)
    #-(or clisp clozure (and cmucl executable) lispworks sbcl scl)
    (when executable
      (not-implemented-error 'dump-image "dumping an executable"))
    #+allegro
    (progn
      (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) ; :new 5000000
      (excl:dumplisp :name filename :suppress-allegro-cl-banner t))
    #+clisp
    (apply #'ext:saveinitmem filename
           :quiet t
           :start-package *package*
           :keep-global-handlers nil
           :executable (if executable 0 t) ;--- requires clisp 2.48 or later, still catches --clisp-x
           (when executable
             (list
              ;; :parse-options nil ;--- requires a non-standard patch to clisp.
              :norc t :script nil :init-function #'restore-image)))
    #+clozure
    (flet ((dump (prepend-kernel)
             (ccl:save-application filename :prepend-kernel prepend-kernel :purify purify
                                            :toplevel-function (when executable #'restore-image))))
      ;;(setf ccl::*application* (make-instance 'ccl::lisp-development-system))
      (if prepend-symbols
          (with-temporary-file (:prefix "ccl-symbols-" :direction :output :pathname path)
            (require 'elf)
            (funcall (fdefinition 'ccl::write-elf-symbols-to-file) path)
            (dump path))
          (dump t)))
    #+(or cmucl scl)
    (progn
      (ext:gc :full t)
      (setf ext:*batch-mode* nil)
      (setf ext::*gc-run-time* 0)
      (apply 'ext:save-lisp filename
             :allow-other-keys t ;; hush SCL and old versions of CMUCL
             #+(and cmucl executable) :executable #+(and cmucl executable) t
             (when executable '(:init-function restore-image :process-command-line nil
                                :quiet t :load-init-file nil :site-init nil))))
    #+gcl
    (progn
      (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t)
      (si::save-system filename))
    #+lispworks
    (if executable
        (lispworks:deliver 'restore-image filename 0 :interface nil)
        (hcl:save-image filename :environment nil))
    #+sbcl
    (progn
      ;;(sb-pcl::precompile-random-code-segments) ;--- it is ugly slow at compile-time (!) when the initial core is a big CLOS program. If you want it, do it yourself
      (setf sb-ext::*gc-run-time* 0)
      (apply 'sb-ext:save-lisp-and-die filename
             :executable t ;--- always include the runtime that goes with the core
             (append
              (when compression (list :compression compression))
              ;;--- only save runtime-options for standalone executables
              (when executable (list :toplevel #'restore-image :save-runtime-options t))
              #+(and sbcl os-windows) ;; passing :application-type :gui will disable the console window.
              ;; the default is :console - only works with SBCL 1.1.15 or later.
              (when application-type (list :application-type application-type)))))
    #-(or allegro clisp clozure cmucl gcl lispworks sbcl scl)
    (not-implemented-error 'dump-image))

  (defun create-image (destination lisp-object-files
                       &key kind output-name prologue-code epilogue-code extra-object-files
                         (prelude () preludep) (postlude () postludep)
                         (entry-point () entry-point-p) build-args no-uiop)
    (declare (ignorable destination lisp-object-files extra-object-files kind output-name
                        prologue-code epilogue-code prelude preludep postlude postludep
                        entry-point entry-point-p build-args no-uiop))
    "On ECL, create an executable at pathname DESTINATION from the specified OBJECT-FILES and options"
    ;; Is it meaningful to run these in the current environment?
    ;; only if we also track the object files that constitute the "current" image,
    ;; and otherwise simulate dump-image, including quitting at the end.
    #-(or clasp ecl mkcl) (not-implemented-error 'create-image)
    #+(or clasp ecl mkcl)
    (let ((epilogue-code
           (if no-uiop
               epilogue-code
               (let ((forms
                      (append
                       (when epilogue-code `(,epilogue-code))
                       (when postludep `((setf *image-postlude* ',postlude)))
                       (when preludep `((setf *image-prelude* ',prelude)))
                       (when entry-point-p `((setf *image-entry-point* ',entry-point)))
                       (case kind
                         ((:image)
                          (setf kind :program) ;; to ECL, it's just another program.
                          `((setf *image-dumped-p* t)
                            (si::top-level #+(or clasp ecl) t) (quit)))
                         ((:program)
                          `((setf *image-dumped-p* :executable)
                            (shell-boolean-exit
                             (restore-image))))))))
                 (when forms `(progn ,@forms))))))
      (check-type kind (member :dll :shared-library :lib :static-library
                               :fasl :fasb :program))
      (apply #+clasp 'cmp:builder #+clasp kind
             #+(or ecl mkcl)
             (ecase kind
               ((:dll :shared-library)
                #+ecl 'c::build-shared-library #+mkcl 'compiler:build-shared-library)
               ((:lib :static-library)
                #+ecl 'c::build-static-library #+mkcl 'compiler:build-static-library)
               ((:fasl #+ecl :fasb)
                #+ecl 'c::build-fasl #+mkcl 'compiler:build-fasl)
               #+mkcl ((:fasb) 'compiler:build-bundle)
               ((:program)
                #+ecl 'c::build-program #+mkcl 'compiler:build-program))
             (pathname destination)
             #+(or clasp ecl) :lisp-files #+mkcl :lisp-object-files
             (append lisp-object-files #+(or clasp ecl) extra-object-files)
             #+ecl :init-name
             #+ecl (getf build-args :init-name)
             (append
              (when prologue-code `(:prologue-code ,prologue-code))
              (when epilogue-code `(:epilogue-code ,epilogue-code))
              #+mkcl (when extra-object-files `(:object-files ,extra-object-files))
              build-args)))))


;;; Some universal image restore hooks
(with-upgradability ()
  (map () 'register-image-restore-hook
       '(setup-stdin setup-stdout setup-stderr
         setup-command-line-arguments setup-temporary-directory
         #+abcl detect-os)))
