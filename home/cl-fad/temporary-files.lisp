(in-package :cl-fad)

(defparameter *default-template* "TEMPORARY-FILES:TEMP-%")

(defparameter *max-tries* 10000)

(defvar *name-random-state* (make-random-state t))

;; from XCVB
(eval-when (:load-toplevel :execute)
  (defun getenv (x)
    "Query the libc runtime environment. See getenv(3)."
    (declare (ignorable x))
    #+(or abcl clisp xcl) (ext:getenv x)
    #+allegro (sys:getenv x)
    #+clozure (ccl:getenv x)
    #+(or cmu scl) (cdr (assoc x ext:*environment-list* :test #'string=))
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
    #+ecl (si:getenv x)
    #+gcl (system:getenv x)
    #+lispworks (lispworks:environment-variable x)
    #+mcl (ccl:with-cstrs ((name x))
            (let ((value (_getenv name)))
              (unless (ccl:%null-ptr-p value)
                (ccl:%get-cstring value))))
    #+sbcl (sb-ext:posix-getenv x)
    #+clasp (ext:getenv x)
    #+mezzano nil
    #-(or abcl allegro clisp clozure cmu cormanlisp ecl gcl lispworks mcl sbcl scl xcl clasp mezzano)
    (error "~S is not supported on your implementation" 'getenv))

  (defun directory-from-environment (environment-variable-name)
    (let ((string (getenv environment-variable-name)))
      (when (plusp (length string))
        (pathname-as-directory string))))

  #+win32
  (define-condition missing-temp-environment-variable (error)
    ()
    (:report (lambda (condition stream)
               (declare (ignore condition))
               (format stream "the TEMP environment variable has not been found, cannot continue"))))

  #+win32
  (defun get-default-temporary-directory ()
    (or (directory-from-environment "TEMP")
        (error 'missing-temp-environment-variable)))

  #-win32
  (defun get-default-temporary-directory ()
    (or (directory-from-environment "TMPDIR")
        (and #-clisp (probe-file #P"/tmp/")
             #+clisp (ext:probe-directory #P"/tmp/")
             #P"/tmp/")))

  (handler-case
      (logical-pathname-translations "TEMPORARY-FILES")
    (error ()
      (alexandria:if-let (default-temporary-directory (get-default-temporary-directory))
        (setf (logical-pathname-translations "TEMPORARY-FILES") `(("*.*.*" ,default-temporary-directory)))
        (warn "could not automatically determine a default mapping for TEMPORARY-FILES")))))

;; locking for multi-threaded operation with unsafe random function

(defvar *create-file-name-lock* (bordeaux-threads:make-lock "Temporary File Name Creation Lock"))

(defmacro with-file-name-lock-held (() &body body)
  `(bordeaux-threads:with-lock-held (*create-file-name-lock*)
     ,@body))

(defun generate-random-string ()
  (with-file-name-lock-held ()
    (format nil "~:@(~36,8,'0R~)" (random (expt 36 8) *name-random-state*))))

(define-condition invalid-temporary-pathname-template (error)
  ((string :initarg :string))
  (:report (lambda (condition stream)
             (with-slots (string) condition
               (format stream "invalid temporary file name template ~S, must contain a percent sign that is to be replaced by a random string" string)))))

(defun generate-random-pathname (template random-string-generator)
  (let ((percent-position (or (position #\% template)
                              (error 'invalid-temporary-pathname-template :string template))))
    (merge-pathnames (concatenate 'string
                                  (subseq template 0 percent-position)
                                  (funcall random-string-generator)
                                  (subseq template (1+ percent-position))))))

(define-condition cannot-create-temporary-file (error)
  ((template :initarg :template)
   (max-tries :initarg :max-tries))
  (:report (lambda (condition stream)
             (with-slots (template max-tries) condition
               (format stream "cannot create temporary file with template ~A, giving up after ~D attempt~:P"
                       template max-tries)))))

(defun open-temporary (&rest open-arguments
		       &key
                         (template *default-template*)
			 (generate-random-string 'generate-random-string)
                         (max-tries *max-tries*)
                         (direction :output)
			 &allow-other-keys)
  "Create a file with a randomly generated name and return the opened
   stream.  The resulting pathname is generated from TEMPLATE, which
   is a string representing a pathname template.  A percent sign (%)
   in that string is replaced by a randomly generated string to make
   the filename unique.  The default for TEMPLATE places temporary
   files in the TEMPORARY-FILES logical pathname host, which is
   automatically set up in a system specific manner.  The file name
   generated from TEMPLATE is merged with *DEFAULT-PATHNAME-DEFAULTS*,
   so random pathnames relative to that directory can be generated by
   not specifying a directory in TEMPLATE.

   GENERATE-RANDOM-STRING can be passed to override the default
   function that generates the random name component.  It should
   return a random string consisting of characters that are permitted
   in a pathname (logical or physical, depending on TEMPLATE).

   The name of the temporary file can be accessed calling the PATHNAME
   function on STREAM.  For convenience, the temporary file is opened
   on the physical pathname, i.e. if the TEMPLATE designate a logical
   pathname the translation to a physical pathname is performed before
   opening the stream.

   In order to create a unique file name, OPEN-TEMPORARY may loop
   internally up to MAX-TRIES times before giving up and signalling a
   CANNOT-CREATE-TEMPORARY-FILE condition."
  (loop thereis (apply #'open
                       (translate-logical-pathname (generate-random-pathname template generate-random-string))
                       :direction direction
                       :if-exists nil
                       (alexandria:remove-from-plist open-arguments :template :generate-random-string :max-tries))
        repeat max-tries
        finally (error 'cannot-create-temporary-file
                       :template template
                       :max-tries max-tries)))

(defmacro with-output-to-temporary-file ((stream &rest args) &body body)
  "Create a temporary file using OPEN-TEMPORARY with ARGS and run BODY
  with STREAM bound to the temporary file stream.  Returns the
  pathname of the file that has been created.  See OPEN-TEMPORARY for
  permitted options."
  `(with-open-stream (,stream (open-temporary ,@args))
     ,@body
     (pathname ,stream)))

(defmacro with-open-temporary-file ((stream &rest args &key keep &allow-other-keys) &body body)
  "Create a temporary file using OPEN-TEMPORARY with ARGS and run BODY
  with STREAM bound to the temporary file stream.  Returns the values
  returned by BODY.  By default, the file is deleted when BODY is
  exited. If a true value is passed in KEEP, the file is not deleted
  when the body is exited.  See OPEN-TEMPORARY for more permitted
  options."
  `(with-open-stream (,stream (open-temporary ,@(alexandria:remove-from-plist args :keep)))
     #+sbcl
     (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     ,(if (and (constantp keep)
               keep)
          `(progn ,@body)
          `(unwind-protect
                (progn ,@body)
             (unless ,keep
               (close ,stream)
               (delete-file (pathname ,stream)))))))
