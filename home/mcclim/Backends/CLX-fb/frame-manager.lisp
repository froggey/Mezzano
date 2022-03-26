(in-package :clim-clx-fb)


(defclass clx-fb-frame-manager (clim-clx::clx-frame-manager)
  ()
  (:default-initargs :mirroring (clim-clx::mirror-factory :single)
                     :class-gensym (gensym "CLXFB")))

;;; if the pane is a subclass of basic-pane and it is not mirrored we create a new class.
(defun maybe-mirroring (fm concrete-pane-class)
  (when (and (not (subtypep concrete-pane-class 'mirrored-sheet-mixin))
	     (funcall (clim-clx::mirroring-p fm) concrete-pane-class))
    (let* ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
                                          (class-name concrete-pane-class)
                                          concrete-pane-class)))
      (multiple-value-bind (class-symbol foundp)
          (alexandria:ensure-symbol
           (alexandria:symbolicate (clim-clx::class-gensym fm) "-"
                                   (symbol-name concrete-pane-class-symbol))
           :clim-clx-fb)
	(unless foundp
          (let ((superclasses (if (subtypep concrete-pane-class 'sheet-with-medium-mixin)
                                  (list 'clx-fb-mirrored-sheet-mixin
                                        'climi::always-repaint-background-mixin
                                        concrete-pane-class-symbol)
                                  (list 'clx-fb-mirrored-sheet-mixin
                                        'climi::always-repaint-background-mixin
                                        ;;'temporary-medium-sheet-output-mixin
                                        'permanent-medium-sheet-output-mixin
                                        concrete-pane-class-symbol))))
            (eval
             `(defclass ,class-symbol
                  ,superclasses
                ()
                (:metaclass ,(type-of (find-class concrete-pane-class-symbol)))))))
        (format *debug-io* "dummy class mirror ~A: ~A~%" concrete-pane-class-symbol class-symbol)
        (setf concrete-pane-class (find-class class-symbol)))))
  concrete-pane-class)

(defmethod find-concrete-pane-class ((fm clx-fb-frame-manager)
                                     pane-type &optional (errorp t))
  ;; This backend doesn't have any specialized pane implementations
  ;; but depending on circumstances it may add optional mirroring to
  ;; the class. Such automatically defined concrete class has the same
  ;; name but with a gensym prefix and symbol in the backend package.
  (maybe-mirroring fm (find-concrete-pane-class t pane-type errorp)))
