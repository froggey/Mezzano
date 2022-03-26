(in-package #:climi)

;;;;
;;;;  Mixin Classes for Concrete Gadgets
;;;;

;;;; Redrawing mixins

(defclass arm/disarm-repaint-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets, whose appearence depends on its armed state."))

(defmethod armed-callback :after ((gadget arm/disarm-repaint-mixin) client id)
  (declare (ignore client id))
  (dispatch-repaint gadget (or (pane-viewport-region gadget)
                               (sheet-region gadget))))

(defmethod disarmed-callback :after ((gadget arm/disarm-repaint-mixin) client id)
  (declare (ignore client id))
  (dispatch-repaint gadget (or (pane-viewport-region gadget)
                               (sheet-region gadget))))

(defclass activate/deactivate-repaint-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets, whose appearence depends on its activatated state."))

(defmethod activate-gadget :after ((gadget activate/deactivate-repaint-mixin))
  (dispatch-repaint gadget (or (pane-viewport-region gadget)
                               (sheet-region gadget))))

(defmethod deactivate-gadget :after ((gadget activate/deactivate-repaint-mixin))
  (dispatch-repaint gadget (or (pane-viewport-region gadget)
                               (sheet-region gadget))))

(defclass value-changed-repaint-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets, whose appearence depends on its value."))

(defmethod (setf gadget-value) :after (new-value (gadget value-changed-repaint-mixin)
                                       &key &allow-other-keys)
  (declare (ignore new-value))
  (dispatch-repaint gadget (or (pane-viewport-region gadget)
                               (sheet-region gadget))))

;;;; Event handling mixins

(defclass enter/exit-arms/disarms-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets which are armed when the mouse enters and
    disarmed when the mouse leaves."))

(defmethod handle-event :before ((pane enter/exit-arms/disarms-mixin) (event pointer-enter-event))
  (declare (ignorable event))
  (arm-gadget pane))

(defmethod handle-event :after ((pane enter/exit-arms/disarms-mixin) (event pointer-exit-event))
  (declare (ignorable event))
  (disarm-gadget pane))

;;; Multiple children

(defclass activate/deactivate-children-mixin ()
  ()
  (:documentation
   "Mixin class for composite gadgets which (de)activate their children."))

(defmethod activate-gadget :after ((gadget activate/deactivate-children-mixin))
  (mapc #'activate-gadget (sheet-children gadget)))

(defmethod deactivate-gadget :after ((gadget activate/deactivate-children-mixin))
  (mapc #'deactivate-gadget (sheet-children gadget)))

;;; Common behavior on `basic-gadget'

(defmethod handle-event :around ((pane basic-gadget) (event device-event))
  ;; When a gadget is not activated, it receives no device events.
  (when (gadget-active-p pane)
    (call-next-method)))

(defmethod note-gadget-deactivated :after (client (gadget basic-gadget))
  (declare (ignore client))
  ;; When a gadget is deactivated, it cannot be armed.

  ;; Glitch: upon re-activation the mouse might happen to be in the
  ;; gadget and thus re-arm it immediately, that is not implemented.
  (disarm-gadget gadget))
