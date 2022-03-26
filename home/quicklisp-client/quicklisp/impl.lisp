(in-package #:ql-impl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun error-unimplemented (&rest args)
    (declare (ignore args))
    (error "Not implemented")))

(defvar *interfaces* (make-hash-table)
  "A table of defined interfaces and their documentation.")

(defun show-interfaces ()
  "Display information about what interfaces are defined."
  (maphash (lambda (interface info)
             (destructuring-bind (arguments docstring)
                 info
               (let ((*package* (find-package :keyword)))
                 (format t "(~S ~:[()~;~:*~A~]~@[~% ~S~])~%"
                         interface arguments docstring))))
           *interfaces*))

(defmacro neuter-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((definition (fdefinition 'error-unimplemented)))
       (do-external-symbols (symbol ,(string name))
         (unless (fboundp symbol)
           (setf (fdefinition symbol) definition))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun feature-expression-passes-p (expression)
    (cond ((keywordp expression)
           (member expression *features*))
          ((consp expression)
           (case (first expression)
             (or
              (some 'feature-expression-passes-p (rest expression)))
             (and
              (every 'feature-expression-passes-p (rest expression)))))
          (t (error "Unrecognized feature expression -- ~S" expression)))))


(defmacro define-implementation-package (feature package-name &rest options)
  (let* ((output-options '((:use)
                           (:export #:lisp)))
         (prep (cdr (assoc :prep options)))
         (class-option (cdr (assoc :class options)))
         (class (first class-option))
         (superclasses (rest class-option))
         (import-options '())
         (effectivep (feature-expression-passes-p feature)))
    (dolist (option options)
      (ecase (first option)
        ((:prep :class))
        ((:import-from
          :import)
         (push option import-options))
        ((:export
          :shadow
          :intern
          :documentation)
         (push option output-options))
        ((:reexport-from)
         (push (cons :export (cddr option)) output-options)
         (push (cons :import-from (cdr option)) import-options))))
    `(progn
       ,@(when effectivep
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   ,@prep)))
       (defclass ,class ,superclasses ())
       (defpackage ,package-name ,@output-options
                   ,@(when effectivep
                           import-options))
       ,@(when effectivep
               `((setf *implementation* (make-instance ',class))))
       ,@(unless effectivep
                 `((neuter-package ,package-name))))))

(defmacro definterface (name lambda-list &body options)
  (let* ((doc-option (find :documentation options :key #'first))
         (doc (second doc-option)))
    (setf (gethash name *interfaces*) (list lambda-list doc)))
  (let* ((forbidden (intersection lambda-list lambda-list-keywords))
         (gf-options (remove :implementation options :key #'first))
         (implementations (set-difference options gf-options))
         (implementation-arg (copy-symbol '%implementation)))
    (when forbidden
      (error "~S not allowed in definterface lambda list" forbidden))
    (flet ((method-option (class body)
             `(:method ((,implementation-arg ,class) ,@lambda-list)
                ,@body)))
      (let ((generic-name (intern (format nil "%~A" name))))
        `(progn
           (defgeneric ,generic-name (lisp ,@lambda-list)
             ,@gf-options
             ,@(mapcan (lambda (implementation)
                         (destructuring-bind (class &rest body)
                             (rest implementation)
                           (mapcar (lambda (class)
                                     (method-option class body))
                                   (if (consp class)
                                       class
                                       (list class)))))
                       implementations))
           (defun ,name ,lambda-list
             (,generic-name *implementation* ,@lambda-list)))))))

(defmacro defimplementation (name-and-options
                             lambda-list &body body)
  (destructuring-bind (name &key (for t) qualifier)
      (if (consp name-and-options)
          name-and-options
          (list name-and-options))
    (unless for
      (error "You must specify an implementation name."))
    (let ((generic-name (find-symbol (format nil "%~A" name)))
          (implementation-arg (copy-symbol '%implementation)))
      (unless generic-name
        (error "~S does not name an implementation function" name))
      `(defmethod ,generic-name
           ,@(when qualifier (list qualifier))
         ,(list* `(,implementation-arg ,for) lambda-list) ,@body))))


;;; Bootstrap implementations

(defvar *implementation* nil)
(defclass lisp () ())


;;; Allegro Common Lisp

(define-implementation-package :allegro #:ql-allegro
  (:documentation
   "Allegro Common Lisp - http://www.franz.com/products/allegrocl/")
  (:class allegro)
  (:reexport-from #:socket
                  #:make-socket)
  (:reexport-from #:excl
                  #:file-directory-p
                  #:delete-directory
                  #:delete-directory-and-files
                  #:read-vector))


;;; Armed Bear Common Lisp

(define-implementation-package :abcl #:ql-abcl
  (:documentation
   "Armed Bear Common Lisp - http://common-lisp.net/project/armedbear/")
  (:class abcl)
  (:reexport-from #:ext
                  #:make-socket
                  #:get-socket-stream))

;;; Clozure CL

(define-implementation-package :ccl #:ql-ccl
  (:documentation
   "Clozure Common Lisp - http://www.clozure.com/clozurecl.html")
  (:class ccl)
  (:reexport-from #:ccl
                  #:delete-directory
                  #:make-socket
                  #:native-translated-namestring))

;;; CLASP

(define-implementation-package :clasp #:ql-clasp
  (:documentation "CLASP - http://github.com/drmeister/clasp")
  (:class clasp)
  (:prep
   (require 'sockets))
  (:intern #:host-network-address)
  (:reexport-from #:si
                  #:rmdir
                  #:file-kind)
  (:reexport-from #:sb-bsd-sockets
                  #:get-host-by-name
                  #:host-ent-address
                  #:inet-socket
                  #:socket-connect
                  #:socket-make-stream))


;;; GNU CLISP

(define-implementation-package :clisp #:ql-clisp
  (:documentation "GNU CLISP - http://clisp.cons.org/")
  (:class clisp)
  (:reexport-from #:socket
                  #:socket-connect)
  (:reexport-from #:ext
                  #:delete-directory
                  #:rename-directory
                  #:probe-directory
                  #:probe-pathname
                  #:read-byte-sequence))


;;; CMUCL

(define-implementation-package :cmu #:ql-cmucl
  (:documentation "CMU Common Lisp - http://www.cons.org/cmucl/")
  (:class cmucl)
  (:reexport-from #:system
                  #:make-fd-stream)
  (:reexport-from #:unix
                  #:unix-rmdir)
  (:reexport-from #:extensions
                  #:connect-to-inet-socket
                  #:*gc-verbose*))

(defvar ql-cmucl:*gc-verbose*)


;;; Scieneer CL

(define-implementation-package :scl #:ql-scl
  (:documentation "Scieneer Common Lisp - http://www.scieneer.com/scl/")
  (:class scl)
  (:reexport-from #:system
                  #:make-fd-stream)
  (:reexport-from #:unix
                  #:unix-rmdir)
  (:reexport-from #:extensions
                  #:connect-to-inet-socket
                  #:unix-namestring))


;;; LispWorks

(define-implementation-package :lispworks #:ql-lispworks
  (:documentation "LispWorks - http://www.lispworks.com/")
  (:class lispworks)
  (:prep
   (require "comm"))
  (:reexport-from #:lw
                  #:file-directory-p
                  #:delete-directory)
  (:reexport-from #:comm
                  #:open-tcp-stream
                  #:get-host-entry))


;;; ECL

(define-implementation-package :ecl #:ql-ecl
  (:documentation "ECL - http://ecls.sourceforge.net/")
  (:class ecl)
  (:prep
   (require 'sockets))
  (:intern #:host-network-address)
  (:reexport-from #:si
                  #:rmdir
                  #:file-kind)
  (:reexport-from #:sb-bsd-sockets
                  #:get-host-by-name
                  #:host-ent-address
                  #:inet-socket
                  #:socket-connect
                  #:socket-make-stream))

;;; Mezzano

(define-implementation-package :mezzano #:ql-mezzano
  (:documentation "Mezzano Lisp Operating System - https://github.com/froggey/Mezzano")
  (:class mezzano)
  (:reexport-from #:mezzano.network.tcp
                  #:tcp-stream-connect))

;;; MKCL

(define-implementation-package :mkcl #:ql-mkcl
  (:documentation "ManKai Common Lisp - http://common-lisp.net/project/mkcl/")
  (:class mkcl)
  (:prep
   (require 'sockets))
  (:intern #:host-network-address)
  (:reexport-from #:si
                  #:rmdir
                  #:file-kind)
  (:reexport-from #:sb-bsd-sockets
                  #:get-host-by-name
                  #:host-ent-address
                  #:inet-socket
                  #:socket-connect
                  #:socket-make-stream))


;;; SBCL

(define-implementation-package :sbcl #:ql-sbcl
  (:class sbcl)
  (:documentation
   "Steel Bank Common Lisp - http://www.sbcl.org/")
  (:prep
   (require 'sb-posix)
   (require 'sb-bsd-sockets))
  (:intern #:host-network-address)
  (:reexport-from #:sb-posix
                  #:rmdir)
  (:reexport-from #:sb-ext
                  #:compiler-note
                  #:native-namestring)
  (:reexport-from #:sb-bsd-sockets
                  #:get-host-by-name
                  #:inet-socket
                  #:host-ent-address
                  #:socket-connect
                  #:socket-make-stream))
