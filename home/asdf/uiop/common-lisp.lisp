;;;; -------------------------------------------------------------------------
;;;; Handle compatibility with multiple implementations.
;;; This file is for papering over the deficiencies and peculiarities
;;; of various Common Lisp implementations.
;;; For implementation-specific access to the system, see os.lisp instead.
;;; A few functions are defined here, but actually exported from utility;
;;; from this package only common-lisp symbols are exported.

(uiop/package:define-package :uiop/common-lisp
  (:nicknames :uoip/cl)
  (:use :uiop/package)
  (:use-reexport #-genera :common-lisp #+genera :future-common-lisp)
  #+allegro (:intern #:*acl-warn-save*)
  #+cormanlisp (:shadow #:user-homedir-pathname)
  #+cormanlisp
  (:export
   #:logical-pathname #:translate-logical-pathname
   #:make-broadcast-stream #:file-namestring)
  #+genera (:shadowing-import-from :scl #:boolean)
  #+genera (:export #:boolean #:ensure-directories-exist #:read-sequence #:write-sequence)
  #+(or mcl cmucl) (:shadow #:user-homedir-pathname))
(in-package :uiop/common-lisp)

#-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl genera lispworks mcl mezzano mkcl sbcl scl xcl)
(error "ASDF is not supported on your implementation. Please help us port it.")

;; (declaim (optimize (speed 1) (debug 3) (safety 3))) ; DON'T: trust implementation defaults.


;;;; Early meta-level tweaks

#+(or allegro clasp clisp clozure cmucl ecl lispworks mezzano mkcl sbcl)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (when (and #+allegro (member :ics *features*)
             #+(or clasp clisp cmucl ecl lispworks mkcl) (member :unicode *features*)
             #+clozure (member :openmcl-unicode-strings *features*)
             #+sbcl (member :sb-unicode *features*))
    ;; Check for unicode at runtime, so that a hypothetical FASL compiled with unicode
    ;; but loaded in a non-unicode setting (e.g. on Allegro) won't tell a lie.
    (pushnew :asdf-unicode *features*)))

#+allegro
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; We need to disable autoloading BEFORE any mention of package ASDF.
  ;; In particular, there must NOT be a mention of package ASDF in the defpackage of this file
  ;; or any previous file.
  (setf excl::*autoload-package-name-alist*
        (remove "asdf" excl::*autoload-package-name-alist*
                :test 'equalp :key 'car))
  (defparameter *acl-warn-save*
    (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
      excl:*warn-on-nested-reader-conditionals*))
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* nil))
  (setf *print-readably* nil))

#+clasp
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf *load-verbose* nil)
  (defun use-ecl-byte-compiler-p () nil))

#+clozure (in-package :ccl)
#+(and clozure windows-target) ;; See http://trac.clozure.com/ccl/ticket/1117
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (fboundp 'external-process-wait)
    (in-development-mode
     (defun external-process-wait (proc)
       (when (and (external-process-pid proc) (eq (external-process-%status proc) :running))
         (with-interrupts-enabled
             (wait-on-semaphore (external-process-completed proc))))
       (values (external-process-%exit-code proc)
               (external-process-%status proc))))))
#+clozure (in-package :uiop/common-lisp) ;; back in this package.

#+cmucl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf ext:*gc-verbose* nil)
  (defun user-homedir-pathname ()
    (first (ext:search-list (cl:user-homedir-pathname)))))

#+cormanlisp
(eval-when (:load-toplevel :compile-toplevel :execute)
  (deftype logical-pathname () nil)
  (defun make-broadcast-stream () *error-output*)
  (defun translate-logical-pathname (x) x)
  (defun user-homedir-pathname (&optional host)
    (declare (ignore host))
    (parse-namestring (format nil "~A\\" (cl:user-homedir-pathname))))
  (defun file-namestring (p)
    (setf p (pathname p))
    (format nil "~@[~A~]~@[.~A~]" (pathname-name p) (pathname-type p))))

#+ecl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf *load-verbose* nil)
  (defun use-ecl-byte-compiler-p () (and (member :ecl-bytecmp *features*) t))
  (unless (use-ecl-byte-compiler-p) (require :cmp)))

#+gcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (member :ansi-cl *features*)
    (error "ASDF only supports GCL in ANSI mode. Aborting.~%"))
  (setf compiler::*compiler-default-type* (pathname "")
        compiler::*lsp-ext* "")
  #.(let ((code ;; Only support very recent GCL 2.7.0 from November 2013 or later.
            (cond
              #+gcl
              ((or (< system::*gcl-major-version* 2)
                   (and (= system::*gcl-major-version* 2)
                        (< system::*gcl-minor-version* 7)))
               '(error "GCL 2.7 or later required to use ASDF")))))
      (eval code)
      code))

#+genera
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (fboundp 'lambda)
    (defmacro lambda (&whole form &rest bvl-decls-and-body)
      (declare (ignore bvl-decls-and-body)(zwei::indentation 1 1))
      `#',(cons 'lisp::lambda (cdr form))))
  (unless (fboundp 'ensure-directories-exist)
    (defun ensure-directories-exist (path)
      (fs:create-directories-recursively (pathname path))))
  (unless (fboundp 'read-sequence)
    (defun read-sequence (sequence stream &key (start 0) end)
      (scl:send stream :string-in nil sequence start end)))
  (unless (fboundp 'write-sequence)
    (defun write-sequence (sequence stream &key (start 0) end)
      (scl:send stream :string-out sequence start end)
      sequence)))

#+lispworks
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; lispworks 3 and earlier cannot be checked for so we always assume
  ;; at least version 4
  (unless (member :lispworks4 *features*)
    (pushnew :lispworks5+ *features*)
    (unless (member :lispworks5 *features*)
      (pushnew :lispworks6+ *features*)
      (unless (member :lispworks6 *features*)
        (pushnew :lispworks7+ *features*)))))

#.(or #+mcl ;; the #$ doesn't work on other lisps, even protected by #+mcl, so we use this trick
      (read-from-string
       "(eval-when (:load-toplevel :compile-toplevel :execute)
          (ccl:define-entry-point (_getenv \"getenv\") ((name :string)) :string)
          (ccl:define-entry-point (_system \"system\") ((name :string)) :int)
          ;; Note: ASDF may expect user-homedir-pathname to provide
          ;; the pathname of the current user's home directory, whereas
          ;; MCL by default provides the directory from which MCL was started.
          ;; See http://code.google.com/p/mcl/wiki/Portability
          (defun user-homedir-pathname ()
            (ccl::findfolder #$kuserdomain #$kCurrentUserFolderType))
          (defun probe-posix (posix-namestring)
            \"If a file exists for the posix namestring, return the pathname\"
            (ccl::with-cstrs ((cpath posix-namestring))
              (ccl::rlet ((is-dir :boolean)
                          (fsref :fsref))
                (when (eq #$noerr (#_fspathmakeref cpath fsref is-dir))
                  (ccl::%path-from-fsref fsref is-dir))))))"))

#+mkcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :cmp)
  (setq clos::*redefine-class-in-place* t)) ;; Make sure we have strict ANSI class redefinition semantics


;;;; Looping
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro loop* (&rest rest)
    #-genera `(loop ,@rest)
    #+genera `(lisp:loop ,@rest))) ;; In genera, CL:LOOP can't destructure, so we use LOOP*. Sigh.


;;;; compatfmt: avoid fancy format directives when unsupported
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun frob-substrings (string substrings &optional frob)
    "for each substring in SUBSTRINGS, find occurrences of it within STRING
that don't use parts of matched occurrences of previous strings, and
FROB them, that is to say, remove them if FROB is NIL,
replace by FROB if FROB is a STRING, or if FROB is a FUNCTION,
call FROB with the match and a function that emits a string in the output.
Return a string made of the parts not omitted or emitted by FROB."
    (declare (optimize (speed 0) (safety #-gcl 3 #+gcl 0) (debug 3)))
    (let ((length (length string)) (stream nil))
      (labels ((emit-string (x &optional (start 0) (end (length x)))
                 (when (< start end)
                   (unless stream (setf stream (make-string-output-stream)))
                   (write-string x stream :start start :end end)))
               (emit-substring (start end)
                 (when (and (zerop start) (= end length))
                   (return-from frob-substrings string))
                 (emit-string string start end))
               (recurse (substrings start end)
                 (cond
                   ((>= start end))
                   ((null substrings) (emit-substring start end))
                   (t (let* ((sub-spec (first substrings))
                             (sub (if (consp sub-spec) (car sub-spec) sub-spec))
                             (fun (if (consp sub-spec) (cdr sub-spec) frob))
                             (found (search sub string :start2 start :end2 end))
                             (more (rest substrings)))
                        (cond
                          (found
                           (recurse more start found)
                           (etypecase fun
                             (null)
                             (string (emit-string fun))
                             (function (funcall fun sub #'emit-string)))
                           (recurse substrings (+ found (length sub)) end))
                          (t
                           (recurse more start end))))))))
        (recurse substrings 0 length))
      (if stream (get-output-stream-string stream) "")))

  (defmacro compatfmt (format)
    #+(or gcl genera)
    (frob-substrings format `("~3i~_" #+genera ,@'("~@<" "~@;" "~@:>" "~:>")))
    #-(or gcl genera) format))
