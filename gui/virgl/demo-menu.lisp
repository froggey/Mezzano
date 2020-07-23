;;;; Virgl demo menu.
;;;;
;;;; Not an actual virgl demo, see test.lisp for them

(defpackage :mezzano.gui.virgl.demo-menu
  (:use :cl)
  (:local-nicknames (:gui :mezzano.gui)
                    (:comp :mezzano.gui.compositor)
                    (:font :mezzano.gui.font)
                    (:theme :mezzano.gui.theme)
                    (:widgets :mezzano.gui.widgets)
                    (:virgl :mezzano.gui.virgl)
                    (:sync :mezzano.sync))
  (:export #:spawn))

(in-package :mezzano.gui.virgl.demo-menu)

(defclass demo-menu-window ()
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

(defvar *button-width* 200)

(defun draw-demo-menu-window (demo-menu)
  (widgets:redraw-widget (button-box demo-menu)))

(defun get-virgl-demos ()
  (let ((demos '()))
    (do-external-symbols (sym :mezzano.gui.virgl.test)
      (push sym demos))
    (sort demos #'string< :key #'symbol-name)))

(defun run-demo (demo-menu fn)
  (let* ((win (window demo-menu))
         (fb (comp:window-buffer win)))
    (unwind-protect
         (with-simple-restart (abort "Close virgl demo ~S" fn)
           ;; Hide the menu window by resizing to 0
           (comp:resize-window win (gui:make-surface 0 0))
           (funcall fn))
      ;; Restore the window.
      (comp:resize-window win fb))))

(defun demo-menu ()
  (handler-case (virgl:get-virgl)
    (virgl:virgl-unsupported-error ()
      (format *error-output* "Virgl device not detected.~%")
      (format *error-output* "Virgl is only supported under qemu.~%")
      (format *error-output* "To enable virgl support, qemu must be start with the options~%\"-vga virtio -display sdl,gl=on\"~%")
      (return-from demo-menu)))
  (with-simple-restart (abort "Close virgl demos")
    (catch 'quit
      (let* ((font (font:open-font
                    font:*default-monospace-font*
                    font:*default-monospace-font-size*))
             (mbox (sync:make-mailbox :capacity 50)))
          (comp:with-window (window mbox 0 0)
            (let* ((frame (make-instance 'widgets:frame
                                         :title "Virgl Demos"
                                         :close-button-p t
                                         :damage-function (widgets:default-damage-function window)
                                         :set-cursor-function (widgets:default-cursor-function window)))
                   (demo-menu (make-instance 'demo-menu-window
                                            :window window
                                            :frame frame))
                   (button-box (make-instance 'widgets:vertical-button-box
                                              :damage-function (widgets:default-damage-function window)
                                              :font font
                                              :buttons (loop
                                                          for demo-fn in (get-virgl-demos)
                                                          collect (make-instance 'widgets:button-box-button
                                                                                 :text (format nil "~:(~A~)" demo-fn)
                                                                                 :on-click (let ((demo-fn demo-fn))
                                                                                             (lambda (button)
                                                                                               (declare (ignore button))
                                                                                               (run-demo demo-menu demo-fn))))))))
              (setf (comp:name window) demo-menu)
              (setf (slot-value demo-menu '%button-box) button-box)
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
              (draw-demo-menu-window demo-menu)
              (comp:damage-window window
                                  0 0
                                  (comp:width window)
                                  (comp:height window))
              (loop
                 (dispatch-event demo-menu (sync:mailbox-receive mbox)))))))))

(defun spawn ()
  (mezzano.supervisor:make-thread 'demo-menu
                                  :name "Virgl Demos"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Virgl Demos"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
