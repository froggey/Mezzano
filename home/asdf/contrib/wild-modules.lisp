(in-package :asdf)

(defclass wild-module (module)
  ((component-class :accessor wild-module-component-class
                    :initform 'static-file :initarg :component-class)
   (component-options :accessor wild-module-component-options
                      :initform nil :initarg :component-options)))

(defmethod (setf component-children) (new-value (module wild-module))
  (when new-value
    (sysdef-error "Cannot explicitly set wild-module ~A's children components. Please ~
use a wild pathname instead." module)))

(defmethod reinitialize-instance :after ((self wild-module) &key)
  (let ((pathname (component-pathname self)))
    (unless (and pathname (wild-pathname-p pathname))
      (sysdef-error "Wild-module ~A specified with non-wild pathname ~A."
                    self pathname))
    (setf (slot-value self 'components)
          (let* ((files (directory* pathname))
                 (class (wild-module-component-class self))
                 (options (wild-module-component-options self)))
            (mapcar (lambda (file)
                      (apply #'make-instance class
                             :name (namestring file)
                             :pathname file
                             :parent self
                             options))
                    files)))
    (compute-children-by-name self)
    (values)))

(defmethod input-files ((o compile-op) (c wild-module)) ())
(defmethod input-files ((o load-op) (c wild-module)) ())

(export 'wild-module)
