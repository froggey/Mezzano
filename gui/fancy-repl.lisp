(defpackage :mezzano.gui.fancy-repl
  (:use :cl :mezzano.gui.font)
  (:export #:spawn))

(in-package :mezzano.gui.fancy-repl)

(defclass fancy-repl (mezzano.line-editor:line-edit-mixin
                      sys.gray:fundamental-character-input-stream
                      mezzano.gui.widgets:text-widget)
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%thread :initarg :thread :reader thread)
   (%input :initarg :input :reader input-buffer)
   (%closed :initarg :window-closed :accessor window-closed)
   (%frame :initarg :frame :reader frame))
  (:default-initargs :input (mezzano.supervisor:make-fifo 500 :element-type 'character)
                     :window-closed nil))

(defgeneric dispatch-event (window event)
  ;; Eat unknown events.
  (:method (w e)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame window)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame window)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:key-event))
  ;; should filter out strange keys?
  (when (not (mezzano.gui.compositor:key-releasep event))
    (mezzano.supervisor:fifo-push (if (mezzano.gui.compositor:key-modifier-state event)
                                      ;; Force character to uppercase when a modifier key is active, gets
                                      ;; around weirdness in how character names are processed.
                                      ;; #\C-a and #\C-A both parse as the same character (C-LATIN_CAPITAL_LETTER_A).
                                      (sys.int::make-character (char-code (char-upcase (mezzano.gui.compositor:key-key event)))
                                                               :control (find :control (mezzano.gui.compositor:key-modifier-state event))
                                                               :meta (find :meta (mezzano.gui.compositor:key-modifier-state event))
                                                               :super (find :super (mezzano.gui.compositor:key-modifier-state event))
                                                               :hyper (find :hyper (mezzano.gui.compositor:key-modifier-state event)))
                                      (mezzano.gui.compositor:key-key event))
                                  (input-buffer window) nil)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:mouse-event))
  (mezzano.gui.widgets:frame-mouse-event (frame window) event))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-close-event))
  (throw 'mezzano.supervisor::terminate-thread nil)
  (setf (window-closed window) t))

(defun pump-event-loop (window)
  "Read & dispatch window events until there are no more waiting events."
  (loop
     (let ((evt (mezzano.supervisor:fifo-pop (fifo window) nil)))
       (when (not evt)
         (return))
       (dispatch-event window evt))))

(defmethod sys.gray:stream-read-char ((stream fancy-repl))
  (unwind-protect
       (progn
         (setf (mezzano.gui.widgets:cursor-visible stream) t)
         (loop
            ;; Catch up with window manager events.
            (pump-event-loop stream)
            (when (window-closed stream)
              (return :eof))
            ;; Check for an available character.
            (let ((ch (mezzano.supervisor:fifo-pop (input-buffer stream) nil)))
              (when ch
                (return ch)))
            ;; Block until the next window event.
            (dispatch-event stream (mezzano.supervisor:fifo-pop (fifo stream)))))
    (setf (mezzano.gui.widgets:cursor-visible stream) nil)))

(defmethod sys.gray:stream-read-char-no-hang ((stream fancy-repl))
  ;; Catch up with window manager events.
  (pump-event-loop stream)
  (cond ((window-closed stream)
         :eof)
        (t (mezzano.supervisor:fifo-pop (input-buffer stream) nil))))

(defmethod sys.gray:stream-terpri :before ((stream fancy-repl))
  ;; Catch up with window manager events.
  (pump-event-loop stream))

(defmethod sys.gray:stream-write-char :before ((stream fancy-repl) character)
  ;; Catch up with window manager events.
  (pump-event-loop stream))

(defun repl-main (&optional initial-function title width height)
  (with-font (font *default-monospace-font* *default-monospace-font-size*)
    (let ((fifo (mezzano.supervisor:make-fifo 50)))
      (mezzano.gui.compositor:with-window (window fifo (or width 800) (or height 600))
        (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
               (frame (make-instance 'mezzano.gui.widgets:frame
                                     :framebuffer framebuffer
                                     :title (string (or title initial-function "REPL"))
                                     :close-button-p t
                                     :damage-function (mezzano.gui.widgets:default-damage-function window)))
               (term (make-instance 'fancy-repl
                                    :fifo fifo
                                    :window window
                                    :thread (mezzano.supervisor:current-thread)
                                    :font font
                                    :frame frame
                                    ;; text-widget stuff.
                                    :framebuffer framebuffer
                                    :x-position (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                    :y-position (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                    :width (- (mezzano.gui.compositor:width window)
                                              (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                              (nth-value 1 (mezzano.gui.widgets:frame-size frame)))
                                    :height (- (mezzano.gui.compositor:height window)
                                               (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                               (nth-value 3 (mezzano.gui.widgets:frame-size frame)))
                                    :damage-function (mezzano.gui.widgets:default-damage-function window)))
               (*terminal-io* term)
               (*standard-input* (make-synonym-stream '*terminal-io*))
               (*standard-output* *standard-input*)
               (*error-output* *standard-input*)
               (*query-io* *standard-input*)
               (*trace-output* *standard-input*)
               (*debug-io* *standard-input*))
          (mezzano.gui.widgets:draw-frame frame)
          (mezzano.gui.compositor:damage-window window
                                                0 0
                                                (mezzano.gui.compositor:width window)
                                                (mezzano.gui.compositor:height window))
          (handler-case
              (funcall (or initial-function #'sys.int::repl))
            ;; Exit when the close button is clicked.
            (mezzano.gui.widgets:close-button-clicked ()
              (return-from repl-main))))))))

(defun spawn (&key initial-function title width height)
  (mezzano.supervisor:make-thread (lambda () (repl-main initial-function title width height))
                                  :name (or title "Fancy Lisp Listener")))
