;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.gui.basic-repl)

(defclass basic-repl (sys.gray:unread-char-mixin
                      sys.int::simple-edit-mixin
                      sys.gray:fundamental-character-input-stream
                      sys.gray:fundamental-character-output-stream)
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%thread :initarg :thread :reader thread)
   (%input :initarg :input :reader input-buffer)
   (%x :initarg :x :accessor cursor-x)
   (%y :initarg :y :accessor cursor-y)
   (%line :initarg :line :accessor cursor-line)
   (%background-colour :initarg :background-colour :accessor background-colour)
   (%foreground-colour :initarg :foreground-colour :accessor foreground-colour))
  (:default-initargs :input (mezzano.supervisor:make-fifo 500 :element-type 'character)
                     :x 0
                     :y 0
                     :line 0
                     :foreground-colour mezzano.gui:*default-foreground-colour*
                     :background-colour mezzano.gui:*default-background-colour*))

(defgeneric dispatch-event (window event)
  ;; Eat unknown events.
  (:method (w e)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:key-event))
  ;; should filter out strange keys?
  (when (not (mezzano.gui.compositor:key-releasep event))
    (mezzano.supervisor:fifo-push (mezzano.gui.compositor:key-key event) (input-buffer window) nil)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:quit-event))
  (throw 'mezzano.supervisor:terminate-thread nil))

(defun pump-event-loop (window)
  "Read & dispatch window events until there are no more waiting events."
  (loop
     (let ((evt (mezzano.supervisor:fifo-pop (fifo window) nil)))
       (when (not evt)
         (return))
       (dispatch-event window evt))))

(defmethod sys.gray:stream-read-char ((stream basic-repl))
  (loop
     ;; Catch up with window manager events.
     (pump-event-loop stream)
     ;; Check for an available character.
     (let ((ch (mezzano.supervisor:fifo-pop (input-buffer stream) nil)))
       (when ch
         (return ch)))
     ;; Block until the next window event.
     (dispatch-event stream (mezzano.supervisor:fifo-pop (fifo stream)))))

(defmethod sys.gray:stream-terpri ((stream basic-repl))
  ;; Catch up with window manager events.
  (pump-event-loop stream)
  (let* ((x (cursor-x stream))
         (y (cursor-y stream))
         (window (window stream))
         (fb (mezzano.gui::surface-pixels (mezzano.gui.compositor:window-buffer window)))
         (win-width (mezzano.gui.compositor:width window))
         (win-height (mezzano.gui.compositor:height window)))
    ;; Clear to the end of the current line.
    (mezzano.gui::2d-array-bitset 16 (- win-width x) (background-colour stream) fb y x)
    (mezzano.gui.compositor:damage-window window x y (- win-width x) 16)
    ;; Advance to the next line.
    (setf (cursor-x stream) 0)
    (cond ((> (+ y 16 16) win-height)
           ;; Off the end of the screen. Scroll!
           (incf (cursor-line stream) 16)
           (mezzano.gui::2d-array-bitblt (- win-height 16) win-width
                                         fb 16 0
                                         fb 0 0)
           ;; Clear line.
           (mezzano.gui::2d-array-bitset 16 win-width (background-colour stream) fb y 0)
           ;; Damage the whole window.
           (mezzano.gui.compositor:damage-window window 0 0 win-width win-height))
          (t (incf y 16)
             (setf (cursor-y stream) y)
             ;; Clear line.
             (mezzano.gui::2d-array-bitset 16 win-width (background-colour stream) fb y 0)
             (mezzano.gui.compositor:damage-window window 0 y win-width 16)))))

(defmethod sys.gray:stream-write-char ((stream basic-repl) character)
  ;; Catch up with window manager events.
  (pump-event-loop stream)
  (cond
    ((eql character #\Newline)
     (sys.gray:stream-terpri stream))
    (t (let* ((width (sys.int::unifont-glyph-width character))
              (window (window stream))
              (fb (mezzano.gui::surface-pixels (mezzano.gui.compositor:window-buffer window)))
              (win-width (mezzano.gui.compositor:width window))
              (win-height (mezzano.gui.compositor:height window)))
         (when (> (+ (cursor-x stream) width) win-width)
           (sys.gray:stream-terpri stream))
         (let ((x (cursor-x stream))
               (y (cursor-y stream))
               (glyph (sys.int::map-unifont-2d character)))
           (mezzano.gui::2d-array-bitset 16 width (background-colour stream) fb y x)
           (when glyph
             (mezzano.gui::2d-array-bitset-blend-mask-1 16 width (foreground-colour stream)
                                                        glyph 0 0
                                                        fb y x))
           (mezzano.gui.compositor:damage-window window x y width 16)
           (incf (cursor-x stream) width))))))

(defmethod sys.gray:stream-start-line-p ((stream basic-repl))
  (zerop (cursor-x stream)))

(defmethod sys.gray:stream-line-column ((stream basic-repl))
  (truncate (cursor-x stream) 8))

(defmethod sys.int::stream-cursor-pos ((stream basic-repl))
  (values (cursor-x stream)
          (+ (cursor-line stream)
             (cursor-y stream))))

(defmethod sys.int::stream-move-to ((stream basic-repl) x y)
  (check-type x integer)
  (check-type y integer)
  (setf (cursor-x stream) x
        (cursor-y stream) (max (- y (cursor-line stream)) 0)))

(defmethod sys.int::stream-character-width ((stream basic-repl) character)
  (sys.int::unifont-glyph-width character))

(defmethod sys.int::stream-compute-motion ((stream basic-repl) string &optional (start 0) end initial-x initial-y)
  (unless end (setf end (length string)))
  (unless initial-x (setf initial-x (cursor-x stream)))
  (unless initial-y (setf initial-y (+ (cursor-line stream)
                                       (cursor-y stream))))
  (do* ((window (window stream))
        (framebuffer (mezzano.gui.compositor:window-buffer window))
        (win-width (mezzano.gui.compositor:width window))
        (win-height (mezzano.gui.compositor:height window))
        (i start (1+ i)))
      ((>= i end)
       (values initial-x initial-y))
    (let* ((ch (char string i))
           (width (sys.int::stream-character-width stream ch)))
      (when (or (eql ch #\Newline)
                (> (+ initial-x width) win-width))
        (setf initial-x 0
              initial-y (if (>= (+ initial-y 16) win-height)
                            0
                            (+ initial-y 16))))
      (unless (eql ch #\Newline)
        (incf initial-x width)))))

(defmethod sys.int::stream-clear-between ((stream basic-repl) start-x start-y end-x end-y)
  (let* ((window (window stream))
         (framebuffer (mezzano.gui::surface-pixels (mezzano.gui.compositor:window-buffer window)))
         (win-width (mezzano.gui.compositor:width window))
         (win-height (mezzano.gui.compositor:height window))
         (colour (background-colour stream)))
    (setf start-y (- start-y (cursor-line stream))
          end-y (- end-y (cursor-line stream)))
    (cond ((eql start-y end-y)
           ;; Clearing one line.
           (mezzano.gui::2d-array-bitset 16 (- end-x start-x) colour framebuffer start-y start-x)
           (mezzano.gui.compositor:damage-window window start-x start-y (- end-x start-x) 16))
          (t ;; Clearing many lines.
           ;; Clear top line.
           (mezzano.gui::2d-array-bitset 16 (- win-width start-x) colour
                                         framebuffer start-y start-x)
           (mezzano.gui.compositor:damage-window window start-x start-y (- win-width start-x) 16)
           ;; Clear in-between.
           (when (> (- end-y start-y) 16)
             (mezzano.gui::2d-array-bitset (- end-y start-y 16) win-width colour
                                           framebuffer (+ start-y 16) 0)
             (mezzano.gui.compositor:damage-window window 0 (+ start-y 16) win-width (- end-y start-y 16)))
           ;; Clear bottom line.
           (mezzano.gui::2d-array-bitset 16 end-x colour
                                         framebuffer end-y 0)
           (mezzano.gui.compositor:damage-window window 0 end-y end-x 16)))))

(defun repl-main (&key width height)
  (let* ((fifo (mezzano.supervisor:make-fifo 50))
         (window (mezzano.gui.compositor:make-window fifo (or width 640) (or height 480)))
         (framebuffer (mezzano.gui::surface-pixels (mezzano.gui.compositor:window-buffer window)))
         (term (make-instance 'basic-repl
                              :fifo fifo
                              :window window
                              :thread (mezzano.supervisor:current-thread)))
         (*terminal-io* term)
         (*standard-input* (make-synonym-stream '*terminal-io*))
         (*standard-output* *standard-input*)
         (*error-output* *standard-input*)
         (*query-io* *standard-input*)
         (*trace-output* *standard-input*)
         (*debug-io* *standard-input*))
    (mezzano.gui::2d-array-bitset (mezzano.gui.compositor:height window)
                                  (mezzano.gui.compositor:width window)
                                  (background-colour term)
                                  framebuffer 0 0)
    (mezzano.gui.compositor:damage-window window
                                          0 0
                                          (mezzano.gui.compositor:width window)
                                          (mezzano.gui.compositor:height window))
    (unwind-protect
         (sys.int::repl)
      (mezzano.gui.compositor:close-window window))))

(defun spawn (&rest args)
  (mezzano.supervisor:make-thread (lambda () (apply #'repl-main args))
                                  :name "Lisp Listener"))

(spawn)
