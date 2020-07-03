;;;; Spy
;;;;
;;;; Peek into the internal state of the compositor.

(defpackage :mezzano.gui.spy
  (:use :cl)
  (:export #:spy #:spawn)
  (:local-nicknames (:sync :mezzano.sync)
                    (:gui :mezzano.gui)
                    (:comp :mezzano.gui.compositor)
                    (:font :mezzano.gui.font)
                    (:widgets :mezzano.gui.widgets)
                    (:sup :mezzano.supervisor)))

(in-package :mezzano.gui.spy)

(defclass spy-window ()
  ((%window :initarg :window :reader window)
   (%redraw :initarg :redraw :accessor redraw)
   (%frame :initarg :frame :reader frame)
   (%text-pane :initarg :text-pane :reader text-pane))
  (:default-initargs :redraw t))

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
      (throw 'quit nil))))

(defmethod dispatch-event (app (event comp:resize-request-event))
  (let* ((win (window app))
         (old-width (comp:width win))
         (old-height (comp:height win))
         (new-width (max 100 (comp:width event)))
         (new-height (max 100 (comp:height event))))
    (when (or (not (eql old-width new-width))
              (not (eql old-height new-height)))
      (let ((new-framebuffer (mezzano.gui:make-surface
                              new-width new-height)))
        (widgets:resize-frame (frame app) new-framebuffer)
        (comp:resize-window
         win new-framebuffer
         :origin (comp:resize-origin event))))))

(defmethod dispatch-event (app (event comp:resize-event))
  (let* ((fb (comp:window-buffer (window app)))
         (new-width (mezzano.gui:surface-width fb))
         (new-height (mezzano.gui:surface-height fb)))
    (multiple-value-bind (left right top bottom)
        (widgets:frame-size (frame app))
      (widgets:resize-widget
       (text-pane app)
       fb
       left top
       (- new-width left right)
       (- new-height top bottom))))
  (setf (redraw app) t))

(defmethod dispatch-event (app (event comp:window-close-event))
  (throw 'quit nil))

(defmethod dispatch-event (app (event comp:quit-event))
  (throw 'quit nil))

(defparameter *spy-refresh-interval* 1/5)
(defparameter *spy-immediate-mode* nil
  "Enable instant updates whenever the compositor itself receives an event.")

(defparameter *spy-report-mouse-window* t
  "If true, describe the mouse window, not the active window.")

(defun update-spy (spy)
  (declare (ignore spy))
  (let ((*print-pretty* t))
    (format t "Mouse: ~Dx~D ~6,'0B  ptr: ~S~%"
            comp::*mouse-x* comp::*mouse-y* comp::*mouse-buttons*
            comp::*mouse-pointer*)
    (format t "Mouse win: ~S~%" comp::(window-at-point *mouse-x* *mouse-y*))
    (format t "Prev mouse win: ~S~%" comp::*previous-mouse-window*)
    (format t "Drag ~S~% ~Dx~D  origin: ~Dx~D  passive: ~S  resize: ~S ~Dx~D ~Dx~D~%"
            comp::*drag-window* comp::*drag-x* comp::*drag-y*
            comp::*drag-x-origin* comp::*drag-y-origin*
            comp::*passive-drag* comp::*resize-origin*
            comp::*prev-resize-rect-x* comp::*prev-resize-rect-y*
            comp::*prev-resize-rect-w* comp::*prev-resize-rect-h*)
    (format t "Keymap: ~S~%" comp::*current-keymap*)
    (format t "Key mods: ~:S~%" comp::*keyboard-modifier-state*)
    (format t "Windows: ~:S~%" comp::*window-list*)
    (format t "M-Tab ~S ~:S~%" comp::*m-tab-active* comp::*m-tab-list*)
    (format t "Postprocess: ~S~%" comp::*postprocess-matrix*)
    (format t "Active window: ~S~%" comp::*active-window*)
    (describe (if *spy-report-mouse-window*
                  comp::(window-at-point *mouse-x* *mouse-y*)
                  comp::*active-window*))))

(defun spy ()
  (with-simple-restart (abort "Close spy")
    (catch 'quit
      (let ((font (font:open-font
                   font:*default-monospace-font*
                   font:*default-monospace-font-size*))
            (mbox (sync:make-mailbox :capacity 50)))
        (comp:with-window (window mbox 640 700)
          (let* ((framebuffer (comp:window-buffer window))
                 (frame (make-instance 'widgets:frame
                                       :framebuffer framebuffer
                                       :title "Spy"
                                       :close-button-p t
                                       :resizablep t
                                       :damage-function (widgets:default-damage-function window)
                                       :set-cursor-function (widgets:default-cursor-function window)))
                 (spy (make-instance 'spy-window
                                     :window window
                                     :frame frame))
                 (text-pane (make-instance 'widgets:text-widget
                                           :font font
                                           :framebuffer framebuffer
                                           :x-position (nth-value 0 (widgets:frame-size frame))
                                           :y-position (nth-value 2 (widgets:frame-size frame))
                                           :width (- (comp:width window)
                                                     (nth-value 0 (widgets:frame-size frame))
                                                     (nth-value 1 (widgets:frame-size frame)))
                                           :height (- (comp:height window)
                                                      (nth-value 2 (widgets:frame-size frame))
                                                      (nth-value 3 (widgets:frame-size frame)))
                                           :damage-function (lambda (&rest args)
                                                              (declare (ignore args))
                                                              (loop
                                                                 (let ((ev (sync:mailbox-receive mbox :wait-p nil)))
                                                                   (when (not ev) (return))
                                                                   (dispatch-event spy ev)))))))
            (setf (comp:name window) spy)
            (setf (slot-value spy '%text-pane) text-pane)
            (widgets:draw-frame frame)
            (comp:damage-window window
                                0 0
                                (comp:width window)
                                (comp:height window))
            (loop
               (when (redraw spy)
                 (let ((*standard-output* text-pane))
                   (setf (redraw spy) nil)
                   (widgets:reset *standard-output*)
                   (ignore-errors
                     (update-spy spy))
                   (comp:damage-window window
                                       0 0
                                       (comp:width window)
                                       (comp:height window))))
               (when (not (redraw spy))
                 ;; Spy on the compositor's event queue too, so we refresh
                 ;; immediately when it gets an input or damage event.
                 (if *spy-immediate-mode*
                     (sync:wait-for-objects-with-timeout
                      *spy-refresh-interval*
                      comp::*event-queue*
                      mbox)
                     (sync:wait-for-objects-with-timeout
                      *spy-refresh-interval*
                      mbox))
                 (let ((evt (sync:mailbox-receive mbox :wait-p nil)))
                   (cond (evt
                          (dispatch-event spy evt))
                         (t (setf (redraw spy) t))))))))))))

(defun spawn ()
  (mezzano.supervisor:make-thread 'spy
                                  :name "Spy"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Spy console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
