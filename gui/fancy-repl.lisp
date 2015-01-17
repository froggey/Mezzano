(defpackage :mezzanine.gui.fancy-repl
  (:use :cl :mezzanine.gui.font)
  (:export #:spawn))

(in-package :mezzanine.gui.fancy-repl)

(defclass fancy-repl (sys.gray:unread-char-mixin
                      sys.int::simple-edit-mixin
                      sys.gray:fundamental-character-input-stream
                      mezzanine.gui.widgets:text-widget)
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%thread :initarg :thread :reader thread)
   (%input :initarg :input :reader input-buffer)
   (%closed :initarg :window-closed :accessor window-closed)
   (%frame :initarg :frame :reader frame))
  (:default-initargs :input (mezzanine.supervisor:make-fifo 500 :element-type 'character)
                     :window-closed nil))

(defgeneric dispatch-event (window event)
  ;; Eat unknown events.
  (:method (w e)))

(defmethod dispatch-event (window (event mezzanine.gui.compositor:window-activation-event))
  (setf (mezzanine.gui.widgets:activep (frame window)) (mezzanine.gui.compositor:state event))
  (mezzanine.gui.widgets:draw-frame (frame window)))

(defmethod dispatch-event (window (event mezzanine.gui.compositor:key-event))
  ;; should filter out strange keys?
  (when (not (mezzanine.gui.compositor:key-releasep event))
    (mezzanine.supervisor:fifo-push (mezzanine.gui.compositor:key-key event) (input-buffer window) nil)))

(defmethod dispatch-event (window (event mezzanine.gui.compositor:mouse-event))
  (mezzanine.gui.widgets:frame-mouse-event (frame window) event))

(defmethod dispatch-event (window (event mezzanine.gui.compositor:window-close-event))
  (throw 'mezzanine.supervisor::terminate-thread nil)
  (setf (window-closed window) t))

(defun pump-event-loop (window)
  "Read & dispatch window events until there are no more waiting events."
  (loop
     (let ((evt (mezzanine.supervisor:fifo-pop (fifo window) nil)))
       (when (not evt)
         (return))
       (dispatch-event window evt))))

(defmethod sys.gray:stream-read-char ((stream fancy-repl))
  (unwind-protect
       (progn
         (setf (mezzanine.gui.widgets:cursor-visible stream) t)
         (loop
            ;; Catch up with window manager events.
            (pump-event-loop stream)
            (when (window-closed stream)
              (return :eof))
            ;; Check for an available character.
            (let ((ch (mezzanine.supervisor:fifo-pop (input-buffer stream) nil)))
              (when ch
                (return ch)))
            ;; Block until the next window event.
            (dispatch-event stream (mezzanine.supervisor:fifo-pop (fifo stream)))))
    (setf (mezzanine.gui.widgets:cursor-visible stream) nil)))

(defmethod sys.gray:stream-read-char-no-hang ((stream fancy-repl))
  ;; Catch up with window manager events.
  (pump-event-loop stream)
  (cond ((window-closed stream)
         :eof)
        (t (mezzanine.supervisor:fifo-pop (input-buffer stream) nil))))

(defmethod sys.gray:stream-terpri :before ((stream fancy-repl))
  ;; Catch up with window manager events.
  (pump-event-loop stream))

(defmethod sys.gray:stream-write-char :before ((stream fancy-repl) character)
  ;; Catch up with window manager events.
  (pump-event-loop stream))

(defun repl-main (&optional initial-function title width height)
  (with-font (font *default-monospace-font* *default-monospace-font-size*)
    (let ((fifo (mezzanine.supervisor:make-fifo 50)))
      (mezzanine.gui.compositor:with-window (window fifo (or width 800) (or height 600))
        (let* ((framebuffer (mezzanine.gui.compositor:window-buffer window))
               (frame (make-instance 'mezzanine.gui.widgets:frame
                                     :framebuffer framebuffer
                                     :title (string (or title initial-function "REPL"))
                                     :close-button-p t
                                     :damage-function (mezzanine.gui.widgets:default-damage-function window)))
               (term (make-instance 'fancy-repl
                                    :fifo fifo
                                    :window window
                                    :thread (mezzanine.supervisor:current-thread)
                                    :font font
                                    :frame frame
                                    ;; text-widget stuff.
                                    :framebuffer framebuffer
                                    :x-position (nth-value 0 (mezzanine.gui.widgets:frame-size frame))
                                    :y-position (nth-value 2 (mezzanine.gui.widgets:frame-size frame))
                                    :width (- (mezzanine.gui.compositor:width window)
                                              (nth-value 0 (mezzanine.gui.widgets:frame-size frame))
                                              (nth-value 1 (mezzanine.gui.widgets:frame-size frame)))
                                    :height (- (mezzanine.gui.compositor:height window)
                                               (nth-value 2 (mezzanine.gui.widgets:frame-size frame))
                                               (nth-value 3 (mezzanine.gui.widgets:frame-size frame)))
                                    :damage-function (mezzanine.gui.widgets:default-damage-function window)))
               (*terminal-io* term)
               (*standard-input* (make-synonym-stream '*terminal-io*))
               (*standard-output* *standard-input*)
               (*error-output* *standard-input*)
               (*query-io* *standard-input*)
               (*trace-output* *standard-input*)
               (*debug-io* *standard-input*))
          (mezzanine.gui.widgets:draw-frame frame)
          (mezzanine.gui.compositor:damage-window window
                                                  0 0
                                                  (mezzanine.gui.compositor:width window)
                                                  (mezzanine.gui.compositor:height window))
          (handler-case
              (funcall (or initial-function #'sys.int::repl))
            ;; Exit when the close button is clicked.
            (mezzanine.gui.widgets:close-button-clicked ()
              (return-from repl-main))))))))

(defun spawn (&key initial-function title width height)
  (mezzanine.supervisor:make-thread (lambda () (repl-main initial-function title width height))
                                    :name (or title "Fancy Lisp Listener")))
