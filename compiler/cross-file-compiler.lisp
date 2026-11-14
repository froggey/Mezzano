(in-package :mezzano.compiler)

(defvar *compile-verbose* t)
(defvar *compile-print* t)

(defvar *compile-file-pathname* nil)
(defvar *compile-file-truename* nil)

(defun cross-compile-file (input-file
                           &key
                             (output-file (make-pathname :type "llf" :defaults input-file))
                             (verbose *compile-verbose*)
                             (print *compile-print*)
                             (external-format :default)
                             package)
  (with-open-file (input-stream input-file :external-format external-format)
    (when verbose
      (format t ";; Cross-compiling ~S~%" input-file))
    (let* ((start-time (get-internal-run-time))
           (*package* (or (find-package (or package "CROSS-CL-USER"))
                          (error "Unknown package ~S" package)))
           (*readtable* (copy-readtable *readtable*))
           (cl:*features* *features*)
           (*compile-verbose* verbose)
           (*compile-print* print)
           (sys.int::*compile-parallel* nil)
           (sys.int::*llf-architecture* *target-architecture*)
           (sys.int::*llf-forms* nil)
           (omap (make-hash-table))
           (eof-marker (cons nil nil))
           (*compile-file-pathname* (pathname (merge-pathnames input-file)))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (sys.int::*top-level-form-number* 0)
           (mezzano.compiler::*load-time-value-hook* 'sys.int::compile-file-load-time-value)
           ;; Don't persist optimize proclaimations outside COMPILE-FILE.
           (mezzano.compiler::*optimize-policy* (copy-list mezzano.compiler::*optimize-policy*))
           (*gensym-counter* 0)
           (sys.int::*fixup-table* (make-hash-table))
           (location-stream (make-instance 'sys.int::location-tracking-stream
                                           :stream input-stream
                                           :namestring (ignore-errors (namestring *compile-file-pathname*))))
           (sys.int::*llf-architecture* *target-architecture*))
      (sys.int::with-reader-location-tracking
        (loop
          for form = (read location-stream nil eof-marker)
          until (eql form eof-marker)
          do
             (when *compile-print*
               (let ((*print-length* 3)
                     (*print-level* 2))
                 (format t ";; X-compiling: ~S~%" form)))
             (sys.int::handle-top-level-form
              form
              #'sys.int::compile-top-level-form
              #'sys.int::eval-in-lexenv)
             (incf sys.int::*top-level-form-number*)))
      ;; Now write everything to the fasl.
      ;; Do two passes to detect circularity.
      (let ((commands (reverse sys.int::*llf-forms*)))
        (let ((sys.int::*llf-dry-run* t))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (sys.int::save-object o omap (make-broadcast-stream)))))
        (with-open-file (output-stream output-file
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede
                                       :direction :output)
          (sys.int::write-llf-header output-stream input-file)
          (let ((sys.int::*llf-dry-run* nil))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (sys.int::save-object o omap output-stream))
              (when (car cmd)
                (write-byte (car cmd) output-stream))))
          (write-byte sys.int::+llf-end-of-load+ output-stream)
          (when *compile-print*
            (format t ";; Cross-compile-file took ~D seconds.~%"
                    (float (/ (- (get-internal-run-time) start-time)
                              internal-time-units-per-second))))
          (values (truename output-stream) nil nil))))))

(defun load-for-cross-compiler (input-file &key
                           (verbose *compile-verbose*)
                           (print *compile-print*)
                           (external-format :default))
  (with-open-file (input input-file :external-format external-format)
    (let* ((*readtable* (copy-readtable *readtable*))
           (*package* (find-package "CROSS-CL-USER"))
           (*compile-print* print)
           (*compile-verbose* verbose)
           (*compile-file-pathname* (pathname (merge-pathnames input-file)))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (*gensym-counter* 0))
      (when *compile-verbose*
        (format t ";; Cross-loading ~S~%" input-file))
      (loop for form = (read input nil input) do
           (when (eql form input)
             (return))
           (when *compile-print*
             (let ((*print-length* 3)
                   (*print-level* 2))
               (format t ";; X-loading: ~S~%" form)))
           (sys.int::handle-top-level-form
            form
            (lambda (form env) (declare (ignore form env)))
            #'sys.int::eval-in-lexenv
            :not-compile-time))))
  t)

(defun save-custom-compiled-file (path generator)
  (let* ((*readtable* (copy-readtable *readtable*))
         (*package* (find-package "CROSS-CL-USER"))
         (*compile-print* *compile-print*)
         (*compile-verbose* *compile-verbose*)
         (*compile-file-pathname* nil)
         (*compile-file-truename* nil)
         (sys.int::*compile-parallel* nil)
         (sys.int::*llf-forms* nil)
         (omap (make-hash-table))
         (*gensym-counter* 0)
         (sys.int::*top-level-form-number* 0)
         (mezzano.compiler::*load-time-value-hook* 'sys.int::compile-file-load-time-value)
         ;; Don't persist optimize proclaimations outside COMPILE-FILE.
         (mezzano.compiler::*optimize-policy* (copy-list mezzano.compiler::*optimize-policy*))
         (sys.int::*fixup-table* (make-hash-table))
         (sys.int::*llf-architecture* *target-architecture*))
    (loop
      for values = (multiple-value-list (funcall generator))
      for form = (first values)
      do
         (when (not values)
           (return))
         (when *compile-print*
           (let ((*print-length* 3)
                 (*print-level* 2))
             (format t ";; X-compiling: ~S~%" form)))
         (sys.int::handle-top-level-form
          form
          #'sys.int::compile-top-level-form
          #'sys.int::eval-in-lexenv))
    ;; Now write everything to the fasl.
    ;; Do two passes to detect circularity.
    (let ((commands (reverse sys.int::*llf-forms*)))
      (let ((sys.int::*llf-dry-run* t))
        (dolist (cmd commands)
          (dolist (o (cdr cmd))
            (sys.int::save-object o omap (make-broadcast-stream)))))
      (with-open-file (output-stream path
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede
                                     :direction :output)
        (sys.int::write-llf-header output-stream path)
        (let ((sys.int::*llf-dry-run* nil))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (sys.int::save-object o omap output-stream))
            (when (car cmd)
              (write-byte (car cmd) output-stream))))
        (write-byte sys.int::+llf-end-of-load+ output-stream)
        (values (truename output-stream) nil nil)))))

(defun save-compiler-builtins (path target-architecture)
  (format t ";; Writing compiler builtins to ~A.~%" path)
  (let* ((builtins (ecase target-architecture
                     (:x86-64 (mezzano.compiler.backend.x86-64::generate-builtin-functions))
                     (:arm64 (mezzano.compiler.backend.arm64::generate-builtin-functions))))
         (*target-architecture* target-architecture))
    (save-custom-compiled-file path
                               (lambda ()
                                 (if builtins
                                     (let ((b (pop builtins)))
                                       `(sys.int::%defun ',(first b) ,(second b)))
                                     (values))))))

(defmethod sys.int::save-one-object ((object sys.int::cross-function) omap stream)
  (let ((constants (sys.int::cross-function-constants object)))
    (dotimes (i (length constants))
      (sys.int::save-object (aref constants i) omap stream))
    (sys.int::save-object (sys.int::cross-function-fixups object) omap stream)
    (write-byte sys.int::+llf-function+ stream)
    (write-byte sys.int::+object-tag-function+ stream) ; tag, normal function.
    (sys.int::save-integer (length (sys.int::cross-function-mc object)) stream)
    (sys.int::save-integer (length constants) stream)
    (sys.int::save-integer (length (sys.int::cross-function-gc-info object)) stream)
    (write-sequence (sys.int::cross-function-mc object) stream)
    (write-sequence (sys.int::cross-function-gc-info object) stream)))

(defmethod sys.int::save-one-object ((object cross-support::cross-short-float) omap stream)
  (write-byte sys.int::+llf-short-float+ stream)
  (sys.int::save-integer (sys.int::%short-float-as-integer object) stream))

(defmethod sys.int::save-one-object ((object cross-support::cross-complex-short-float) omap stream)
  (write-byte sys.int::+llf-complex-short-float+ stream)
  (sys.int::save-integer (sys.int::%short-float-as-integer (cross-support::cross-complex-short-float-realpart object)) stream)
  (sys.int::save-integer (sys.int::%short-float-as-integer (cross-support::cross-complex-short-float-imagpart object)) stream))

;; Override the normal save-one-object method specifically so that the cross-cl package
;; gets translated properly.
(defmethod sys.int::save-one-object ((object symbol) omap stream)
  (cond ((symbol-package object)
         (write-byte sys.int::+llf-symbol+ stream)
         (sys.int::save-integer (length (symbol-name object)) stream)
         (dotimes (i (length (symbol-name object)))
           (sys.int::save-character (char (symbol-name object) i) stream))
         (let ((package (symbol-package object)))
           (when (eql package (find-package :cross-cl))
             (setf package (find-package :cl)))
           (sys.int::save-integer (length (package-name package)) stream)
           (dotimes (i (length (package-name package)))
             (sys.int::save-character (char (package-name package) i) stream))))
        (t
         (sys.int::save-object (symbol-name object) omap stream)
         (write-byte sys.int::+llf-uninterned-symbol+ stream))))

(defmethod sys.int::save-one-object ((object mezzano.runtime::inline-cache) omap stream)
  (sys.int::save-object (cross-support::inline-cache-proxy-name object) omap stream)
  (write-byte sys.int::+llf-inline-cache+ stream))
