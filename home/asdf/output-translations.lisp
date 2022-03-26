;;;; ---------------------------------------------------------------------------
;;;; asdf-output-translations

(uiop/package:define-package :asdf/output-translations
  (:recycle :asdf/output-translations :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade)
  (:export
   #:*output-translations* #:*output-translations-parameter*
   #:invalid-output-translation
   #:output-translations #:output-translations-initialized-p
   #:initialize-output-translations #:clear-output-translations
   #:disable-output-translations #:ensure-output-translations
   #:apply-output-translations
   #:validate-output-translations-directive #:validate-output-translations-form
   #:validate-output-translations-file #:validate-output-translations-directory
   #:parse-output-translations-string #:wrapping-output-translations
   #:user-output-translations-pathname #:system-output-translations-pathname
   #:user-output-translations-directory-pathname #:system-output-translations-directory-pathname
   #:environment-output-translations #:process-output-translations
   #:compute-output-translations
   #+abcl #:translate-jar-pathname
   ))
(in-package :asdf/output-translations)

;; (setf output-translations) between 2.27 and 3.0.3 was using a defsetf macro
;; for the sake of obsolete versions of GCL 2.6. Make sure it doesn't come to haunt us.
(when-upgrading (:version "3.1.2") (fmakunbound '(setf output-translations)))

(with-upgradability ()
  (define-condition invalid-output-translation (invalid-configuration warning)
    ((format :initform (compatfmt "~@<Invalid asdf output-translation ~S~@[ in ~S~]~@{ ~@?~}~@:>"))))

  (defvar *output-translations* ()
    "Either NIL (for uninitialized), or a list of one element,
said element itself being a sorted list of mappings.
Each mapping is a pair of a source pathname and destination pathname,
and the order is by decreasing length of namestring of the source pathname.")

  (defun output-translations ()
    "Return the configured output-translations, if any"
    (car *output-translations*))

  ;; Set the output-translations, by sorting the provided new-value.
  (defun set-output-translations (new-value)
    (setf *output-translations*
          (list
           (stable-sort (copy-list new-value) #'>
                        :key #'(lambda (x)
                                 (etypecase (car x)
                                   ((eql t) -1)
                                   (pathname
                                    (let ((directory
                                           (normalize-pathname-directory-component
                                            (pathname-directory (car x)))))
                                      (if (listp directory) (length directory) 0))))))))
    new-value)
  (defun (setf output-translations) (new-value) (set-output-translations new-value))

  (defun output-translations-initialized-p ()
    "Have the output-translations been initialized yet?"
    (and *output-translations* t))

  (defun clear-output-translations ()
    "Undoes any initialization of the output translations."
    (setf *output-translations* '())
    (values))
  (register-clear-configuration-hook 'clear-output-translations)


  ;;; Validation of the configuration directives...

  (defun validate-output-translations-directive (directive)
    (or (member directive '(:enable-user-cache :disable-cache nil))
        (and (consp directive)
             (or (and (length=n-p directive 2)
                      (or (and (eq (first directive) :include)
                               (typep (second directive) '(or string pathname null)))
                          (and (location-designator-p (first directive))
                               (or (location-designator-p (second directive))
                                   (location-function-p (second directive))))))
                 (and (length=n-p directive 1)
                      (location-designator-p (first directive)))))))

  (defun validate-output-translations-form (form &key location)
    (validate-configuration-form
     form
     :output-translations
     'validate-output-translations-directive
     :location location :invalid-form-reporter 'invalid-output-translation))

  (defun validate-output-translations-file (file)
    (validate-configuration-file
     file 'validate-output-translations-form :description "output translations"))

  (defun validate-output-translations-directory (directory)
    (validate-configuration-directory
     directory :output-translations 'validate-output-translations-directive
               :invalid-form-reporter 'invalid-output-translation))


  ;;; Parse the ASDF_OUTPUT_TRANSLATIONS environment variable and/or some file contents
  (defun parse-output-translations-string (string &key location)
    (cond
      ((or (null string) (equal string ""))
       '(:output-translations :inherit-configuration))
      ((not (stringp string))
       (error (compatfmt "~@<Environment string isn't: ~3i~_~S~@:>") string))
      ((eql (char string 0) #\")
       (parse-output-translations-string (read-from-string string) :location location))
      ((eql (char string 0) #\()
       (validate-output-translations-form (read-from-string string) :location location))
      (t
       (loop
         :with inherit = nil
         :with directives = ()
         :with start = 0
         :with end = (length string)
         :with source = nil
         :with separator = (inter-directory-separator)
         :for i = (or (position separator string :start start) end) :do
           (let ((s (subseq string start i)))
             (cond
               (source
                (push (list source (if (equal "" s) nil s)) directives)
                (setf source nil))
               ((equal "" s)
                (when inherit
                  (error (compatfmt "~@<Only one inherited configuration allowed: ~3i~_~S~@:>")
                         string))
                (setf inherit t)
                (push :inherit-configuration directives))
               (t
                (setf source s)))
             (setf start (1+ i))
             (when (> start end)
               (when source
                 (error (compatfmt "~@<Uneven number of components in source to destination mapping: ~3i~_~S~@:>")
                        string))
               (unless inherit
                 (push :ignore-inherited-configuration directives))
               (return `(:output-translations ,@(nreverse directives)))))))))


  ;; The default sources of configuration for output-translations
  (defparameter* *default-output-translations*
    '(environment-output-translations
      user-output-translations-pathname
      user-output-translations-directory-pathname
      system-output-translations-pathname
      system-output-translations-directory-pathname))

  ;; Compulsory implementation-dependent wrapping for the translations:
  ;; handle implementation-provided systems.
  (defun wrapping-output-translations ()
    `(:output-translations
    ;; Some implementations have precompiled ASDF systems,
    ;; so we must disable translations for implementation paths.
      #+(or clasp #|clozure|# ecl mkcl sbcl)
      ,@(let ((h (resolve-symlinks* (lisp-implementation-directory))))
          (when h `(((,h ,*wild-path*) ()))))
      #+mkcl (,(translate-logical-pathname "CONTRIB:") ())
      ;; All-import, here is where we want user stuff to be:
      :inherit-configuration
      ;; These are for convenience, and can be overridden by the user:
      #+abcl (#p"/___jar___file___root___/**/*.*" (:user-cache #p"**/*.*"))
      #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
      ;; We enable the user cache by default, and here is the place we do:
      :enable-user-cache))

  ;; Relative pathnames of output-translations configuration to XDG configuration directory
  (defparameter *output-translations-file* (parse-unix-namestring "common-lisp/asdf-output-translations.conf"))
  (defparameter *output-translations-directory* (parse-unix-namestring "common-lisp/asdf-output-translations.conf.d/"))

  ;; Locating various configuration pathnames, depending on input or output intent.
  (defun user-output-translations-pathname (&key (direction :input))
    (xdg-config-pathname *output-translations-file* direction))
  (defun system-output-translations-pathname (&key (direction :input))
    (find-preferred-file (system-config-pathnames *output-translations-file*)
                         :direction direction))
  (defun user-output-translations-directory-pathname (&key (direction :input))
    (xdg-config-pathname *output-translations-directory* direction))
  (defun system-output-translations-directory-pathname (&key (direction :input))
    (find-preferred-file (system-config-pathnames *output-translations-directory*)
                         :direction direction))
  (defun environment-output-translations ()
    (getenv "ASDF_OUTPUT_TRANSLATIONS"))


  ;;; Processing the configuration.

  (defgeneric process-output-translations (spec &key inherit collect))

  (defun inherit-output-translations (inherit &key collect)
    (when inherit
      (process-output-translations (first inherit) :collect collect :inherit (rest inherit))))

  (defun* (process-output-translations-directive) (directive &key inherit collect)
    (if (atom directive)
        (ecase directive
          ((:enable-user-cache)
           (process-output-translations-directive '(t :user-cache) :collect collect))
          ((:disable-cache)
           (process-output-translations-directive '(t t) :collect collect))
          ((:inherit-configuration)
           (inherit-output-translations inherit :collect collect))
          ((:ignore-inherited-configuration :ignore-invalid-entries nil)
           nil))
        (let ((src (first directive))
              (dst (second directive)))
          (if (eq src :include)
              (when dst
                (process-output-translations (pathname dst) :inherit nil :collect collect))
              (when src
                (let ((trusrc (or (eql src t)
                                  (let ((loc (resolve-location src :ensure-directory t :wilden t)))
                                    (if (absolute-pathname-p loc) (resolve-symlinks* loc) loc)))))
                  (cond
                    ((location-function-p dst)
                     (funcall collect
                              (list trusrc (ensure-function (second dst)))))
                    ((typep dst 'boolean)
                     (funcall collect (list trusrc t)))
                    (t
                     (let* ((trudst (resolve-location dst :ensure-directory t :wilden t)))
                       (funcall collect (list trudst t))
                       (funcall collect (list trusrc trudst)))))))))))

  (defmethod process-output-translations ((x symbol) &key
                                                       (inherit *default-output-translations*)
                                                       collect)
    (process-output-translations (funcall x) :inherit inherit :collect collect))
  (defmethod process-output-translations ((pathname pathname) &key inherit collect)
    (cond
      ((directory-pathname-p pathname)
       (process-output-translations (validate-output-translations-directory pathname)
                                    :inherit inherit :collect collect))
      ((probe-file* pathname :truename *resolve-symlinks*)
       (process-output-translations (validate-output-translations-file pathname)
                                    :inherit inherit :collect collect))
      (t
       (inherit-output-translations inherit :collect collect))))
  (defmethod process-output-translations ((string string) &key inherit collect)
    (process-output-translations (parse-output-translations-string string)
                                 :inherit inherit :collect collect))
  (defmethod process-output-translations ((x null) &key inherit collect)
    (inherit-output-translations inherit :collect collect))
  (defmethod process-output-translations ((form cons) &key inherit collect)
    (dolist (directive (cdr (validate-output-translations-form form)))
      (process-output-translations-directive directive :inherit inherit :collect collect)))


  ;;; Top-level entry-points to configure output-translations

  (defun compute-output-translations (&optional parameter)
    "read the configuration, return it"
    (remove-duplicates
     (while-collecting (c)
       (inherit-output-translations
        `(wrapping-output-translations ,parameter ,@*default-output-translations*) :collect #'c))
     :test 'equal :from-end t))

  ;; Saving the user-provided parameter to output-translations, if any,
  ;; so we can recompute the translations after code upgrade.
  (defvar *output-translations-parameter* nil)

  ;; Main entry-point for users.
  (defun initialize-output-translations (&optional (parameter *output-translations-parameter*))
    "read the configuration, initialize the internal configuration variable,
return the configuration"
    (setf *output-translations-parameter* parameter
          (output-translations) (compute-output-translations parameter)))

  (defun disable-output-translations ()
    "Initialize output translations in a way that maps every file to itself,
effectively disabling the output translation facility."
    (initialize-output-translations
     '(:output-translations :disable-cache :ignore-inherited-configuration)))

  ;; checks an initial variable to see whether the state is initialized
  ;; or cleared. In the former case, return current configuration; in
  ;; the latter, initialize.  ASDF will call this function at the start
  ;; of (asdf:find-system).
  (defun ensure-output-translations ()
    (if (output-translations-initialized-p)
        (output-translations)
        (initialize-output-translations)))


  ;; Top-level entry-point to _use_ output-translations
  (defun* (apply-output-translations) (path)
    (etypecase path
      (logical-pathname
       path)
      ((or pathname string)
       (ensure-output-translations)
       (loop* :with p = (resolve-symlinks* path)
              :for (source destination) :in (car *output-translations*)
              :for root = (when (or (eq source t)
                                    (and (pathnamep source)
                                         (not (absolute-pathname-p source))))
                            (pathname-root p))
              :for absolute-source = (cond
                                       ((eq source t) (wilden root))
                                       (root (merge-pathnames* source root))
                                       (t source))
              :when (or (eq source t) (pathname-match-p p absolute-source))
              :return (translate-pathname* p absolute-source destination root source)
              :finally (return p)))))


  ;; Hook into uiop's output-translation mechanism
  #-cormanlisp
  (setf *output-translation-function* 'apply-output-translations)


  ;;; Implementation-dependent hacks
  #+abcl ;; ABCL: make it possible to use systems provided in the ABCL jar.
  (defun translate-jar-pathname (source wildcard)
    (declare (ignore wildcard))
    (flet ((normalize-device (pathname)
             (if (find :windows *features*)
                 pathname
                 (make-pathname :defaults pathname :device :unspecific))))
      (let* ((jar
               (pathname (first (pathname-device source))))
             (target-root-directory-namestring
               (format nil "/___jar___file___root___/~@[~A/~]"
                       (and (find :windows *features*)
                            (pathname-device jar))))
             (relative-source
               (relativize-pathname-directory source))
             (relative-jar
               (relativize-pathname-directory (ensure-directory-pathname jar)))
             (target-root-directory
               (normalize-device
                (pathname-directory-pathname
                 (parse-namestring target-root-directory-namestring))))
             (target-root
               (merge-pathnames* relative-jar target-root-directory))
             (target
               (merge-pathnames* relative-source target-root)))
        (normalize-device (apply-output-translations target))))))

