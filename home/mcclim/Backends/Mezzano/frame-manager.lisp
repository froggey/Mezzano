(in-package #:clim-mezzano)

(defclass mezzano-frame-manager (frame-manager)
  ((class-gensym :initarg :class-gensym
                 :initform (gensym "MEZZANO-")
                 :reader class-gensym)))

;;; if the pane is a subclass of basic-pane and it is not mirrored we
;;; create a new class.

(defun maybe-mirroring (fm concrete-pane-class)
  (when (subtypep concrete-pane-class '(and (not mirrored-sheet-mixin)
                                        top-level-sheet-pane))
    (let ((concrete-pane-class-symbol (if (typep concrete-pane-class 'class)
                                          (class-name concrete-pane-class)
                                          concrete-pane-class)))
      (multiple-value-bind (class-symbol foundp)
          (alexandria:ensure-symbol
           (alexandria:symbolicate (class-gensym fm) "-"
                                   (symbol-name concrete-pane-class-symbol))
           :clim-mezzano)
        (unless foundp
          (let ((superclasses
                  (if (subtypep concrete-pane-class 'sheet-with-medium-mixin)
                      (list 'mezzano-mirrored-sheet-mixin
                            'climi::always-repaint-background-mixin
                            concrete-pane-class-symbol)
                      (list 'mezzano-mirrored-sheet-mixin
                            'climi::always-repaint-background-mixin
                            'permanent-medium-sheet-output-mixin
                            concrete-pane-class-symbol))))
            (eval
             `(defclass ,class-symbol
                  ,superclasses
                ()
                (:metaclass
                 ,(type-of (find-class concrete-pane-class-symbol)))))))
        (setf concrete-pane-class (find-class class-symbol)))))
  concrete-pane-class)

(defmethod find-concrete-pane-class ((frame-manager mezzano-frame-manager)
                                     pane-type &optional (errorp t))
  (maybe-mirroring frame-manager (find-concrete-pane-class t pane-type errorp)))

(defmethod adopt-frame :before ((fm mezzano-frame-manager) (frame menu-frame))
  (multiple-value-bind (buttons mouse-x mouse-y)
      (mos:global-mouse-state)
    (declare (ignore buttons))
    (setf (slot-value frame 'climi::left) (+ mouse-x 5)
          (slot-value frame 'climi::top) (+ mouse-y 5))))

  ;; CLX code for adopt-frame :before
  ;; Temporary kludge.
  ;; (when (eq (slot-value frame 'climi::top) nil)
  ;;   (multiple-value-bind (x y)
  ;;       (xlib:query-pointer (clx-port-window (port fm)))
  ;;     (incf x 10)
  ;;     (setf (slot-value frame 'climi::left) x
  ;;           (slot-value frame 'climi::top) y)))


(defmethod adopt-frame :after ((fm mezzano-frame-manager) (frame menu-frame))
  ;; TODO not sure what to do here - maybe draw frame should be moved
  ;; here from create-mezzano-mirror? Then need additional cases:
  ;; application-frame
  ;; others?

  ;; (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
  ;;   (xlib:map-window (sheet-direct-xmirror (slot-value frame 'top-level-sheet))))
  )
