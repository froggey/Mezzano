;;;; Settings
;;;;
;;;; Change basic system settings.

(defpackage :mezzano.gui.settings
  (:use :cl)
  (:local-nicknames (:gui :mezzano.gui)
                    (:comp :mezzano.gui.compositor)
                    (:font :mezzano.gui.font)
                    (:theme :mezzano.gui.theme)
                    (:widgets :mezzano.gui.widgets)
                    (:sync :mezzano.sync))
  (:export #:spawn))

(in-package :mezzano.gui.settings)

(defclass settings-window ()
  ((%window :initarg :window :reader window)
   (%frame :initarg :frame :reader frame)
   (%button-box :initarg :button-box :reader button-box)))

(defgeneric dispatch-event (app event)
  (:method (app event)))

(defmethod dispatch-event (app (event comp:window-activation-event))
  (let ((frame (frame app)))
    (setf (widgets:activep frame) (comp:state event))
    (widgets:draw-frame frame)))

(defmethod dispatch-event (app (event comp:mouse-event))
  (handler-case
      (widgets:frame-mouse-event (frame app) event)
    (widgets:close-button-clicked ()
      (throw 'quit nil)))
  (widgets:widget-mouse-event (button-box app) event))

(defmethod dispatch-event (app (event comp:window-close-event))
  (throw 'quit nil))

(defmethod dispatch-event (app (event comp:quit-event))
  (throw 'quit nil))

(defun draw-settings-window (settings)
  (widgets:redraw-widget (button-box settings)))

(defclass keymap-button (widgets:button-box-button)
  ((%keymap :initarg :keymap :reader button-keymap)))

(defun update-keymap-buttons (settings)
  (loop
     for button in (widgets:buttons (button-box settings))
     do
       (setf (widgets:activep button) (eql comp::*current-keymap* (button-keymap button))))
  (draw-settings-window settings))

(defun settings ()
  (with-simple-restart (abort "Close settings")
    (catch 'quit
      (let* ((font (font:open-font
                    font:*default-monospace-font*
                    font:*default-monospace-font-size*))
             (mbox (sync:make-mailbox :capacity 50)))
          (comp:with-window (window mbox 0 0)
            (let* ((frame (make-instance 'widgets:frame
                                         :title "Settings"
                                         :close-button-p t
                                         :damage-function (widgets:default-damage-function window)
                                         :set-cursor-function (widgets:default-cursor-function window)))
                   (settings (make-instance 'settings-window
                                            :window window
                                            :frame frame))
                   (button-box (make-instance 'widgets:vertical-button-box
                                              :damage-function (widgets:default-damage-function window)
                                              :font font
                                              :buttons (loop
                                                          for keymap in comp::*keymap-list*
                                                          collect (make-instance 'keymap-button
                                                                                 :text (comp:name keymap)
                                                                                 :activep (eql keymap comp::*current-keymap*)
                                                                                 :keymap keymap
                                                                                 :on-click (lambda (button)
                                                                                             (setf comp::*current-keymap* (button-keymap button))
                                                                                             (update-keymap-buttons settings)))))))
              (setf (comp:name window) settings)
              (setf (slot-value settings '%button-box) button-box)
              ;; Adjust the window size now that sizes are knowable.
              (multiple-value-bind (left right top bottom)
                  (widgets:frame-size frame)
                (multiple-value-bind (width height)
                    (widgets:compute-button-box-geometry button-box :minimum-width 200)
                  (let* ((new-width (+ left width right))
                         (new-height (+ top height bottom))
                         (new-fb (gui:make-surface new-width new-height)))
                    (comp:resize-window window new-fb :origin :midpoint)
                    (widgets:resize-frame frame new-fb)
                    (widgets:resize-widget button-box new-fb
                                           left top width height))))
              (widgets:draw-frame frame)
              (draw-settings-window settings)
              (comp:damage-window window
                                  0 0
                                  (comp:width window)
                                  (comp:height window))
              (loop
                 (dispatch-event settings (sync:mailbox-receive mbox)))))))))

(defun spawn ()
  (mezzano.supervisor:make-thread 'settings
                                  :name "Settings"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Settings console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
