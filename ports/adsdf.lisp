;;;; A Dumb System Definition Facility
;;;; This is a somewhat ASDF compatible defsystem implementation
;;;; specifically for cross-compiling some ASDF systems for LispOS.

(defpackage #:adsdf
  (:use #:cl #:iter))

(in-package #:adsdf)

(defvar *system-database* (make-hash-table :test 'equal))

(defun system-name (system) (first system))
(defun system-source-location (system) (second system))
(defun system-components (system) (third system))

(defun canonicalize-source-location (path)
  "Turn PATH into an absolute path and remove any name components."
  (make-pathname :name nil :type nil :version nil :defaults (merge-pathnames path)))

(defun register-system (defsystem-form source-location)
  (destructuring-bind (name &body body) (rest defsystem-form)
    (let ((components '())
          (true-name (string name)))
      (format t "Registering system ~A. Source in ~S~%" true-name source-location)
      (do ((i body (cddr i)))
          ((endp i))
        (ecase (first i)
          ((:version :licence :description :long-description))
          (:components
           (assert (null components) (components) "Multiple :COMPONENTS options...")
           (setf components (second i)))))
      (setf (gethash true-name *system-database*)
            (list true-name source-location components)))))

(defun read-asd (path)
  "Read a file containing ASDF DEFSYSTEM forms and register them in
the system database."
  (let ((*package* (find-package '#:asdf))
        (*features* sys.int:*features*))
    (iter (for form in-file path)
          (when (and (listp form)
                     (eql (first form) 'asdf:defsystem))
            (register-system form (canonicalize-source-location path))))))

(defun find-system (system &optional (errorp t))
  (etypecase system
    (list system)
    ((or string character symbol)
     (or (gethash (string system) *system-database*)
         (and errorp (error "No such system ~S~%" system))))))

(defun component-type (component)
  (first component))

(defun component-name (component)
  (second component))

(defun component-dependencies (component)
  (let ((deps '()))
    (do ((i (cddr component) (cddr i)))
        ((endp i))
      (ecase (first i)
        (:depends-on
         (if (listp (second i))
             (push (second i) deps)
             (push (list (second i)) deps)))))
    (apply 'append deps)))

(defun compute-dependency-order (system)
  (let ((order '())
        (pending (remove :file (system-components system) :test-not 'eql :key 'component-type))
        (still-pending nil)
        (progress-made t))
    (iter (while pending)
          (unless progress-made
            (error "Cannot resolve dependencies for system ~S~%" (system-name system)))
          (setf progress-made nil)
          (dolist (c pending)
            (dolist (dep (component-dependencies c)
                     (progn (setf progress-made t)
                            (push c order)))
              (unless (member dep order :test 'equal :key 'component-name)
                (push c still-pending)
                (return))))
          (setf pending still-pending
                still-pending nil))
    (mapcar 'component-name (nreverse order))))

(defun compile-system (system &key
                                (single-file t)
                                force
                                (verbose *compile-verbose*)
                                (print *compile-print*)
                                (external-format :default))
  "Compile SYSTEM. When SINGLE-FILE is true, produce one output file
containing the entire system."
  (setf system (find-system system))
  (let ((compile-order (compute-dependency-order system)))
    (cond (single-file
           (with-open-file (sys.c::*output-fasl*
                            (if (or (stringp single-file) (pathnamep single-file))
                                single-file
                                (make-pathname :name (string-downcase (system-name system))
                                               :type "llf"
                                               :defaults (system-source-location system)))
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede
                            :direction :output)
             (sys.c::write-llf-header sys.c::*output-fasl* (string-downcase (system-name system)))
             (let ((*readtable* (copy-readtable sys.c::*cross-readtable*))
                   (*features* sys.int:*features*)
                   (sys.c::*output-map* (make-hash-table))
                   (*compile-print* print)
                   (*compile-verbose* verbose))
               (sys.c::x-compile `(pushnew ,(intern (string-upcase (system-name system)) '#:keyword)
                                           sys.int::*loaded-adsdf-systems*)
                                 nil)
               (dolist (input-name compile-order)
                 (let* ((input-file (merge-pathnames (make-pathname :type "lisp" :defaults (pathname input-name))
                                                     (system-source-location system)))
                        (*package* (find-package "SYS.INT"))
                        (*compile-file-pathname* (pathname input-file))
                        (*compile-file-truename* (truename *compile-file-pathname*))
                        (*gensym-counter* 0))
                   (with-open-file (input (merge-pathnames input-file (system-source-location system)) :external-format external-format)
                     (when *compile-verbose*
                       (format t ";; Cross-compiling ~S~%" input-file))
                     (iter (for form = (read input nil input))
                           (when (eql form input)
                             (return))
                           (when *compile-print*
                             (let ((*print-length* 3)
                                   (*print-level* 2))
                               (format t ";; X-compiling: ~S~%" form)))
                           (sys.c::x-compile-top-level form nil)))))
               (sys.c::x-compile `(pushnew ,(intern (string-upcase (system-name system)) '#:keyword)
                                           sys.int::*loaded-adsdf-systems*)
                                 nil)
               (write-byte sys.c::+llf-end-of-load+ sys.c::*output-fasl*))))
          (t (mapc 'sys.c::cross-compile-file
                   (mapcar (lambda (input-name)
                             (merge-pathnames (make-pathname :type "lisp" :defaults (pathname input-name))
                                              (system-source-location system)))
                           compile-order))))))
