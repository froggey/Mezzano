;;;; ---------------------------------------------------------------------------
;;;; Access to the Operating System

(uiop/package:define-package :uiop/os
  (:use :uiop/common-lisp :uiop/package :uiop/utility)
  (:export
   #:featurep #:os-unix-p #:os-macosx-p #:os-windows-p #:os-genera-p #:detect-os ;; features
   #:os-cond
   #:getenv #:getenvp ;; environment variables
   #:implementation-identifier ;; implementation identifier
   #:implementation-type #:*implementation-type*
   #:operating-system #:architecture #:lisp-version-string
   #:hostname #:getcwd #:chdir
   ;; Windows shortcut support
   #:read-null-terminated-string #:read-little-endian
   #:parse-file-location-info #:parse-windows-shortcut))
(in-package :uiop/os)

;;; Features
(with-upgradability ()
  (defun featurep (x &optional (*features* *features*))
    "Checks whether a feature expression X is true with respect to the *FEATURES* set,
as per the CLHS standard for #+ and #-. Beware that just like the CLHS,
we assume symbols from the KEYWORD package are used, but that unless you're using #+/#-
your reader will not have magically used the KEYWORD package, so you need specify
keywords explicitly."
    (cond
      ((atom x) (and (member x *features*) t))
      ((eq :not (car x)) (assert (null (cddr x))) (not (featurep (cadr x))))
      ((eq :or (car x)) (some #'featurep (cdr x)))
      ((eq :and (car x)) (every #'featurep (cdr x)))
      (t (parameter-error "~S: malformed feature specification ~S" 'featurep x))))

  ;; Starting with UIOP 3.1.5, these are runtime tests.
  ;; You may bind *features* with a copy of what your target system offers to test its properties.
  (defun os-macosx-p ()
    "Is the underlying operating system MacOS X?"
    ;; OS-MACOSX is not mutually exclusive with OS-UNIX,
    ;; in fact the former implies the latter.
    (featurep '(:or :darwin (:and :allegro :macosx) (:and :clisp :macos))))

  (defun os-unix-p ()
    "Is the underlying operating system some Unix variant?"
    (or (featurep '(:or :unix :cygwin)) (os-macosx-p)))

  (defun os-windows-p ()
    "Is the underlying operating system Microsoft Windows?"
    (and (not (os-unix-p)) (featurep '(:or :win32 :windows :mswindows :mingw32 :mingw64))))

  (defun os-genera-p ()
    "Is the underlying operating system Genera (running on a Symbolics Lisp Machine)?"
    (featurep :genera))

  (defun os-oldmac-p ()
    "Is the underlying operating system an (emulated?) MacOS 9 or earlier?"
    (featurep :mcl))

  (defun os-haiku-p ()
    "Is the underlying operating system Haiku?"
    (featurep :haiku))

  (defun os-mezzano-p ()
    "Is the underlying operating system Mezzano?"
    (featurep :mezzano))

  (defun detect-os ()
    "Detects the current operating system. Only needs be run at compile-time,
except on ABCL where it might change between FASL compilation and runtime."
    (loop* :with o
           :for (feature . detect) :in '((:os-unix . os-unix-p) (:os-macosx . os-macosx-p)
                                         (:os-windows . os-windows-p)
                                         (:genera . os-genera-p) (:os-oldmac . os-oldmac-p)
                                         (:haiku . os-haiku-p)
                                         (:mezzano . os-mezzano-p))
           :when (and (or (not o) (eq feature :os-macosx)) (funcall detect))
           :do (setf o feature) (pushnew feature *features*)
           :else :do (setf *features* (remove feature *features*))
           :finally
           (return (or o (error "Congratulations for trying ASDF on an operating system~%~
that is neither Unix, nor Windows, nor Genera, nor even old MacOS.~%Now you port it.")))))

  (defmacro os-cond (&rest clauses)
    #+abcl `(cond ,@clauses)
    #-abcl (loop* :for (test . body) :in clauses :when (eval test) :return `(progn ,@body)))

  (detect-os))

;;;; Environment variables: getting them, and parsing them.
(with-upgradability ()
  (defun getenv (x)
    "Query the environment, as in C getenv.
Beware: may return empty string if a variable is present but empty;
use getenvp to return NIL in such a case."
    (declare (ignorable x))
    #+(or abcl clasp clisp ecl xcl) (ext:getenv x)
    #+allegro (sys:getenv x)
    #+clozure (ccl:getenv x)
    #+cmucl (unix:unix-getenv x)
    #+scl (cdr (assoc x ext:*environment-list* :test #'string=))
    #+cormanlisp
    (let* ((buffer (ct:malloc 1))
           (cname (ct:lisp-string-to-c-string x))
           (needed-size (win:getenvironmentvariable cname buffer 0))
           (buffer1 (ct:malloc (1+ needed-size))))
      (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
                 nil
                 (ct:c-string-to-lisp-string buffer1))
        (ct:free buffer)
        (ct:free buffer1)))
    #+gcl (system:getenv x)
    #+(or genera mezzano) nil
    #+lispworks (lispworks:environment-variable x)
    #+mcl (ccl:with-cstrs ((name x))
            (let ((value (_getenv name)))
              (unless (ccl:%null-ptr-p value)
                (ccl:%get-cstring value))))
    #+mkcl (#.(or (find-symbol* 'getenv :si nil) (find-symbol* 'getenv :mk-ext nil)) x)
    #+sbcl (sb-ext:posix-getenv x)
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl genera lispworks mcl mezzano mkcl sbcl scl xcl)
    (not-implemented-error 'getenv))

  (defsetf getenv (x) (val)
    "Set an environment variable."
      (declare (ignorable x val))
    #+allegro `(setf (sys:getenv ,x) ,val)
    #+clisp `(system::setenv ,x ,val)
    #+clozure `(ccl:setenv ,x ,val)
    #+cmucl `(unix:unix-setenv ,x ,val 1)
    #+ecl `(ext:setenv ,x ,val)
    #+lispworks `(hcl:setenv ,x ,val)
    #+mkcl `(mkcl:setenv ,x ,val)
    #+sbcl `(progn (require :sb-posix) (symbol-call :sb-posix :setenv ,x ,val 1))
    #-(or allegro clisp clozure cmucl ecl lispworks mkcl sbcl)
    '(not-implemented-error '(setf getenv)))

  (defun getenvp (x)
    "Predicate that is true if the named variable is present in the libc environment,
then returning the non-empty string value of the variable"
    (let ((g (getenv x))) (and (not (emptyp g)) g))))


;;;; implementation-identifier
;;
;; produce a string to identify current implementation.
;; Initially stolen from SLIME's SWANK, completely rewritten since.
;; We're back to runtime checking, for the sake of e.g. ABCL.

(with-upgradability ()
  (defun first-feature (feature-sets)
    "A helper for various feature detection functions"
    (dolist (x feature-sets)
      (multiple-value-bind (short long feature-expr)
          (if (consp x)
              (values (first x) (second x) (cons :or (rest x)))
              (values x x x))
        (when (featurep feature-expr)
          (return (values short long))))))

  (defun implementation-type ()
    "The type of Lisp implementation used, as a short UIOP-standardized keyword"
    (first-feature
     '(:abcl (:acl :allegro) (:ccl :clozure) :clisp (:corman :cormanlisp)
       (:cmu :cmucl :cmu) :clasp :ecl :gcl
       (:lwpe :lispworks-personal-edition) (:lw :lispworks)
       :mcl :mezzano :mkcl :sbcl :scl (:smbx :symbolics) :xcl)))

  (defvar *implementation-type* (implementation-type)
    "The type of Lisp implementation used, as a short UIOP-standardized keyword")

  (defun operating-system ()
    "The operating system of the current host"
    (first-feature
     '(:cygwin
       (:win :windows :mswindows :win32 :mingw32) ;; try cygwin first!
       (:linux :linux :linux-target) ;; for GCL at least, must appear before :bsd
       (:macosx :macosx :darwin :darwin-target :apple) ; also before :bsd
       (:solaris :solaris :sunos)
       (:bsd :bsd :freebsd :netbsd :openbsd :dragonfly)
       :unix
       :genera
       :mezzano)))

  (defun architecture ()
    "The CPU architecture of the current host"
    (first-feature
     '((:x64 :x86-64 :x86_64 :x8664-target :amd64 (:and :word-size=64 :pc386))
       (:x86 :x86 :i386 :i486 :i586 :i686 :pentium3 :pentium4 :pc386 :iapx386 :x8632-target)
       (:ppc64 :ppc64 :ppc64-target) (:ppc32 :ppc32 :ppc32-target :ppc :powerpc)
       :hppa64 :hppa :sparc64 (:sparc32 :sparc32 :sparc)
       :mipsel :mipseb :mips :alpha (:arm :arm :arm-target) :imach
       ;; Java comes last: if someone uses C via CFFI or otherwise JNA or JNI,
       ;; we may have to segregate the code still by architecture.
       (:java :java :java-1.4 :java-1.5 :java-1.6 :java-1.7))))

  #+clozure
  (defun ccl-fasl-version ()
    ;; the fasl version is target-dependent from CCL 1.8 on.
    (or (let ((s 'ccl::target-fasl-version))
          (and (fboundp s) (funcall s)))
        (and (boundp 'ccl::fasl-version)
             (symbol-value 'ccl::fasl-version))
        (error "Can't determine fasl version.")))

  (defun lisp-version-string ()
    "return a string that identifies the current Lisp implementation version"
    (let ((s (lisp-implementation-version)))
      (car ; as opposed to OR, this idiom prevents some unreachable code warning
       (list
        #+allegro
        (format nil "~A~@[~A~]~@[~A~]~@[~A~]"
                excl::*common-lisp-version-number*
                ;; M means "modern", as opposed to ANSI-compatible mode (which I consider default)
                (and (eq excl:*current-case-mode* :case-sensitive-lower) "M")
                ;; Note if not using International ACL
                ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
                (excl:ics-target-case (:-ics "8"))
                (and (member :smp *features*) "S"))
        #+armedbear (format nil "~a-fasl~a" s system::*fasl-version*)
        #+clisp
        (subseq s 0 (position #\space s)) ; strip build information (date, etc.)
        #+clozure
        (format nil "~d.~d-f~d" ; shorten for windows
                ccl::*openmcl-major-version*
                ccl::*openmcl-minor-version*
                (logand (ccl-fasl-version) #xFF))
        #+cmucl (substitute #\- #\/ s)
        #+scl (format nil "~A~A" s
                      ;; ANSI upper case vs lower case.
                      (ecase ext:*case-mode* (:upper "") (:lower "l")))
        #+ecl (format nil "~A~@[-~A~]" s
                      (let ((vcs-id (ext:lisp-implementation-vcs-id)))
                        (unless (equal vcs-id "UNKNOWN")
                          (subseq vcs-id 0 (min (length vcs-id) 8)))))
        #+gcl (subseq s (1+ (position #\space s)))
        #+genera
        (multiple-value-bind (major minor) (sct:get-system-version "System")
          (format nil "~D.~D" major minor))
        #+mcl (subseq s 8) ; strip the leading "Version "
        #+mezzano (mezzano.extensions:lisp-version-string)
        ;; seems like there should be a shorter way to do this, like ACALL.
        #+mkcl (or
                (let ((fname (find-symbol* '#:git-describe-this-mkcl :mkcl nil)))
                  (when (and fname (fboundp fname))
                    (funcall fname)))
                s)
        s))))

  (defun implementation-identifier ()
    "Return a string that identifies the ABI of the current implementation,
suitable for use as a directory name to segregate Lisp FASLs, C dynamic libraries, etc."
    (substitute-if
     #\_ #'(lambda (x) (find x " /:;&^\\|?<>(){}[]$#`'\""))
     (format nil "~(~a~@{~@[-~a~]~}~)"
             (or (implementation-type) (lisp-implementation-type))
             (lisp-version-string)
             (or (operating-system) (software-type))
             (or (architecture) (machine-type))))))


;;;; Other system information

(with-upgradability ()
  (defun hostname ()
    "return the hostname of the current host"
    #+(or abcl clasp clozure cmucl ecl genera lispworks mcl mezzano mkcl sbcl scl xcl) (machine-instance)
    #+cormanlisp "localhost" ;; is there a better way? Does it matter?
    #+allegro (symbol-call :excl.osi :gethostname)
    #+clisp (first (split-string (machine-instance) :separator " "))
    #+gcl (system:gethostname)))


;;; Current directory
(with-upgradability ()

  #+cmucl
  (defun parse-unix-namestring* (unix-namestring)
    "variant of LISP::PARSE-UNIX-NAMESTRING that returns a pathname object"
    (multiple-value-bind (host device directory name type version)
        (lisp::parse-unix-namestring unix-namestring 0 (length unix-namestring))
      (make-pathname :host (or host lisp::*unix-host*) :device device
                     :directory directory :name name :type type :version version)))

  (defun getcwd ()
    "Get the current working directory as per POSIX getcwd(3), as a pathname object"
    (or #+(or abcl genera mezzano xcl) (truename *default-pathname-defaults*) ;; d-p-d is canonical!
        #+allegro (excl::current-directory)
        #+clisp (ext:default-directory)
        #+clozure (ccl:current-directory)
        #+(or cmucl scl) (#+cmucl parse-unix-namestring* #+scl lisp::parse-unix-namestring
                        (strcat (nth-value 1 (unix:unix-current-directory)) "/"))
        #+cormanlisp (pathname (pl::get-current-directory)) ;; Q: what type does it return?
        #+(or clasp ecl) (ext:getcwd)
        #+gcl (let ((*default-pathname-defaults* #p"")) (truename #p""))
        #+lispworks (hcl:get-working-directory)
        #+mkcl (mk-ext:getcwd)
        #+sbcl (sb-ext:parse-native-namestring (sb-unix:posix-getcwd/))
        #+xcl (extensions:current-directory)
        (not-implemented-error 'getcwd)))

  (defun chdir (x)
    "Change current directory, as per POSIX chdir(2), to a given pathname object"
    (if-let (x (pathname x))
      #+(or abcl genera mezzano xcl) (setf *default-pathname-defaults* (truename x)) ;; d-p-d is canonical!
      #+allegro (excl:chdir x)
      #+clisp (ext:cd x)
      #+clozure (setf (ccl:current-directory) x)
      #+(or cmucl scl) (unix:unix-chdir (ext:unix-namestring x))
      #+cormanlisp (unless (zerop (win32::_chdir (namestring x)))
                     (error "Could not set current directory to ~A" x))
      #+(or clasp ecl) (ext:chdir x)
      #+gcl (system:chdir x)
      #+lispworks (hcl:change-directory x)
      #+mkcl (mk-ext:chdir x)
      #+sbcl (progn (require :sb-posix) (symbol-call :sb-posix :chdir (sb-ext:native-namestring x)))
      #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl genera lispworks mkcl sbcl scl xcl)
      (not-implemented-error 'chdir))))


;;;; -----------------------------------------------------------------
;;;; Windows shortcut support.  Based on:
;;;;
;;;; Jesse Hager: The Windows Shortcut File Format.
;;;; http://www.wotsit.org/list.asp?fc=13

#-(or clisp genera) ; CLISP doesn't need it, and READ-SEQUENCE annoys old Genera that doesn't need it
(with-upgradability ()
  (defparameter *link-initial-dword* 76)
  (defparameter *link-guid* #(1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70))

  (defun read-null-terminated-string (s)
    "Read a null-terminated string from an octet stream S"
    ;; note: doesn't play well with UNICODE
    (with-output-to-string (out)
      (loop :for code = (read-byte s)
            :until (zerop code)
            :do (write-char (code-char code) out))))

  (defun read-little-endian (s &optional (bytes 4))
    "Read a number in little-endian format from an byte (octet) stream S,
the number having BYTES octets (defaulting to 4)."
    (loop :for i :from 0 :below bytes
          :sum (ash (read-byte s) (* 8 i))))

  (defun parse-file-location-info (s)
    "helper to parse-windows-shortcut"
    (let ((start (file-position s))
          (total-length (read-little-endian s))
          (end-of-header (read-little-endian s))
          (fli-flags (read-little-endian s))
          (local-volume-offset (read-little-endian s))
          (local-offset (read-little-endian s))
          (network-volume-offset (read-little-endian s))
          (remaining-offset (read-little-endian s)))
      (declare (ignore total-length end-of-header local-volume-offset))
      (unless (zerop fli-flags)
        (cond
          ((logbitp 0 fli-flags)
           (file-position s (+ start local-offset)))
          ((logbitp 1 fli-flags)
           (file-position s (+ start
                               network-volume-offset
                               #x14))))
        (strcat (read-null-terminated-string s)
                (progn
                  (file-position s (+ start remaining-offset))
                  (read-null-terminated-string s))))))

  (defun parse-windows-shortcut (pathname)
    "From a .lnk windows shortcut, extract the pathname linked to"
    ;; NB: doesn't do much checking & doesn't look like it will work well with UNICODE.
    (with-open-file (s pathname :element-type '(unsigned-byte 8))
      (handler-case
          (when (and (= (read-little-endian s) *link-initial-dword*)
                     (let ((header (make-array (length *link-guid*))))
                       (read-sequence header s)
                       (equalp header *link-guid*)))
            (let ((flags (read-little-endian s)))
              (file-position s 76)        ;skip rest of header
              (when (logbitp 0 flags)
                ;; skip shell item id list
                (let ((length (read-little-endian s 2)))
                  (file-position s (+ length (file-position s)))))
              (cond
                ((logbitp 1 flags)
                 (parse-file-location-info s))
                (t
                 (when (logbitp 2 flags)
                   ;; skip description string
                   (let ((length (read-little-endian s 2)))
                     (file-position s (+ length (file-position s)))))
                 (when (logbitp 3 flags)
                   ;; finally, our pathname
                   (let* ((length (read-little-endian s 2))
                          (buffer (make-array length)))
                     (read-sequence buffer s)
                     (map 'string #'code-char buffer)))))))
        (end-of-file (c)
          (declare (ignore c))
          nil)))))


