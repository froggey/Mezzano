;;;; Window-related classes & methods.

(in-package #:sys.graphics)

(defclass window-with-chrome (window)
  ((chrome-drag-state :initform '())
   (has-close-button :initarg :close-button :initform 't)))

(defmethod compute-window-margins ((window window-with-chrome))
  (values 1 1 19 1))

(defvar *corner-mask*
  (2d-array '((0.0 0.0 0.0 0.2 0.5)
              (0.0 0.0 0.5 1.0 1.0)
              (0.0 0.5 1.0 1.0 1.0)
              (0.2 1.0 1.0 1.0 1.0)
              (0.5 1.0 1.0 1.0 1.0)))
  "Alpha values for the rounded window corners.")

(defvar *title-top-colour* :silver)
(defvar *title-text-colour* :black)
(defvar *title-text-drop-colour* :gray)
(defvar *border-colour* :gray)
(defvar *close-button-main* :red)
(defvar *close-button-drop* :maroon)

(defun lerp (v0 v1 a)
  (+ v0 (* (- v1 v0) a)))

(defun lerp-colour (colour1 colour2 a)
  (let ((c1 (make-colour colour1))
        (c2 (make-colour colour2)))
    (logior (ash (truncate (lerp (ldb (byte 8 24) c1)
                                 (ldb (byte 8 24) c2)
                                 a))
                 24)
            (ash (truncate (lerp (ldb (byte 8 16) c1)
                                 (ldb (byte 8 16) c2)
                                 a))
                 16)
            (ash (truncate (lerp (ldb (byte 8 8) c1)
                                 (ldb (byte 8 8) c2)
                                 a))
                 8)
            (truncate (lerp (ldb (byte 8 0) c1)
                            (ldb (byte 8 0) c2)
                            a)))))

(defun vertical-gradient (nrows ncols colour1 colour2 to-array to-row to-col)
  (dotimes (i nrows)
    (bitset 1 ncols
            (lerp-colour colour1 colour2 (/ i (1- nrows)))
            to-array (+ to-row i) to-col)))

(defun draw-string (string colour to-array to-row to-col)
  (let ((s (string string))
        (x 0))
    (dotimes (i (length s))
      (let* ((ch (char s i))
             (glyph (sys.int::map-unifont-2d ch))
             (width (sys.int::unifont-glyph-width ch)))
        (when glyph
          (bitset-argb-xrgb-mask-1 16 width colour
                                   glyph 0 0
                                   to-array to-row (+ to-col x)))
        (incf x width)))
    x))

(defmethod window-redraw :before ((window window-with-chrome))
  (let* ((fb (window-backbuffer window))
         (dims (array-dimensions fb))
         (border-colour (make-colour *border-colour*))
         (text-colour (make-colour *title-text-colour*))
         (text-drop-colour (make-colour *title-text-drop-colour*)))
    ;; Bottom.
    (bitset 1 (second dims) border-colour fb (1- (first dims)) 0)
    ;; Left.
    (bitset (first dims) 1 border-colour fb 0 0)
    ;; Right
    (bitset (first dims) 1 border-colour fb 0 (1- (second dims)))
    ;; Title.
    (let ((title (window-title window)))
      (vertical-gradient 19 (second dims)
                         *title-top-colour* *border-colour*
                         fb 0 0)
      (draw-string title text-drop-colour fb 2 6)
      (draw-string title text-colour fb 1 5))
    (when (slot-value window 'has-close-button)
      ;; Close button. Unifont uses a chunky double-wide X.
      (let ((ch (name-char "Multiplication-X"))
            (position (- (second dims) 1 16)))
        (draw-string ch (make-colour *close-button-drop*)
                     fb 2 (1+ position))
        (draw-string ch (make-colour *close-button-main*)
                     fb 2 position)))
    ;; Round off the corners.
    (let* ((corner-width (array-dimension *corner-mask* 1))
           (corner-height (array-dimension *corner-mask* 0)))
      (dotimes (y corner-height)
        (dotimes (x corner-width)
          (let ((alpha (truncate (* (aref *corner-mask* y x) 255))))
            (setf (ldb (byte 8 24) (aref fb y x)) alpha
                  (ldb (byte 8 24) (aref fb y (- (second dims) x 1))) alpha)))))))

(defmethod mouse-button-event :around ((window window-with-chrome) button-id clickedp mouse-x mouse-y)
  (let* ((fb (window-backbuffer window))
         (dims (array-dimensions fb)))
    (cond ((and (not (slot-value window 'chrome-drag-state))
                (slot-value window 'has-close-button)
                (<= 1 mouse-y 17)
                (<= (- (second dims) 1 16) mouse-x (- (second dims) 1)))
           ;; Close button.
           (when (and (eql button-id 0)
                      (not clickedp))
             (window-close-event window)))
          ((or (and (<= 1 mouse-y 17)
                    (<= 1 mouse-x (- (second dims) 2)))
               (slot-value window 'chrome-drag-state))
           ;; Clicking the titlebar.
           (when (eql button-id 0)
             ;; Engage or disengage window dragging.
             (cond (clickedp
                    (setf (slot-value window 'chrome-drag-state) (cons mouse-x mouse-y))
                    (window-begin-drag window mouse-x mouse-y))
                   (t (when (slot-value window 'chrome-drag-state)
                        (setf (slot-value window 'chrome-drag-state) nil)
                        (window-finish-drag window))))))
          ;; FIXME: Should make window border mouse-sensitive as well.
          (t (call-next-method)))))

(defmethod mouse-move-event :around ((window window-with-chrome) mouse-x mouse-y rel-x rel-y button-state)
  (let* ((fb (window-backbuffer window))
         (dims (array-dimensions fb)))
    (cond ((slot-value window 'chrome-drag-state)
           (multiple-value-bind (win-x win-y)
               (window-position window)
             (multiple-value-bind (screen-width screen-height)
                 (screen-size)
               (let* ((dims (array-dimensions (window-frontbuffer window)))
                      (width (second dims))
                      (height (first dims))
                      (new-x (clamp (+ win-x rel-x)
                                    (+ (- width) 2)
                                    (- screen-width 2)))
                      (new-y (clamp (+ win-y rel-y)
                                    0
                                    (- screen-height 2))))
                 (setf (window-position window) (cons new-x new-y)
                       *refresh-required* t)))))
          (t (call-next-method)))))

(defmethod window-redraw :after ((window window))
  (window-swap-buffers window))

(defclass text-widget (sys.gray:fundamental-character-output-stream)
  ((cursor-x :initarg :cursor-x :accessor cursor-x)
   (cursor-y :initarg :cursor-y :accessor cursor-y)
   (foreground :initarg :foreground :accessor window-foreground-colour)
   (background :initarg :background :accessor window-background-colour)
   (framebuffer :initarg :framebuffer :reader framebuffer)
   (x :initarg :x :accessor widget-x)
   (y :initarg :y :accessor widget-y)
   (width :initarg :width :accessor widget-width)
   (height :initarg :height :accessor widget-height))
  (:default-initargs :x 0 :y 0
                     :cursor-x 0 :cursor-y 0
                     :foreground (make-colour *default-foreground-colour*)
                     :background (make-colour *default-background-colour*)))

(defclass character-input-mixin ()
  ((character-buffer :initarg :buffer :reader character-buffer))
  (:default-initargs :buffer (make-fifo 500 'character)))

(defmethod key-press-event ((window character-input-mixin) character)
  (fifo-push character (character-buffer window)))

(defmethod sys.gray:stream-read-char ((stream character-input-mixin))
  (loop
     (let ((char (fifo-pop (character-buffer stream))))
       (when char (return char)))
     (sys.int::process-wait "User input"
                            (lambda ()
                              (not (fifo-emptyp (character-buffer stream)))))))

(defclass text-window (sys.gray:unread-char-mixin
                       sys.int::simple-edit-mixin
                       character-input-mixin
                       sys.gray:fundamental-character-output-stream
                       sys.gray:fundamental-character-input-stream
                       window)
  ((widget :reader text-window-display)))

(defmethod window-foreground-colour ((window text-window))
  (window-foreground-colour (text-window-display window)))

(defmethod (setf window-foreground-colour) (new-value (window text-window))
  (setf (window-foreground-colour (text-window-display window)) new-value))

(defmethod window-background-colour ((window text-window))
  (window-background-colour (text-window-display window)))

(defmethod (setf window-background-colour) (new-value (window text-window))
  (setf (window-background-colour (text-window-display window)) new-value))

(defmethod initialize-instance :after ((instance text-window))
  (multiple-value-bind (left right top bottom)
      (compute-window-margins instance)
    (setf (slot-value instance 'widget) (make-instance 'text-widget
                                                       :framebuffer (window-backbuffer instance)
                                                       :x left
                                                       :y top
                                                       :width (- (array-dimension (window-backbuffer instance) 1)
                                                                 left right)
                                                       :height (- (array-dimension (window-backbuffer instance) 0)
                                                                  top bottom)))))

(defclass text-window-with-chrome (text-window window-with-chrome) ())

(defmethod window-redraw ((window text-window))
  (multiple-value-bind (left right top bottom)
      (compute-window-margins window)
    (bitset (window-height window) (window-width window)
            (window-background-colour (text-window-display window))
            (window-backbuffer window) top left)))

(defmethod window-close-event ((window text-window))
  (close-window window))

(defmethod sys.gray:stream-write-char ((stream text-widget) character)
  (let ((fb (framebuffer stream))
        (x (cursor-x stream))
        (y (cursor-y stream))
        (win-width (widget-width stream))
        (win-height (widget-height stream)))
    (cond
      ((eql character #\Newline)
       ;; Clear the next line.
       (setf (cursor-x stream) 0
             y (if (> (+ y 16 16) win-height)
                   0
                   (+ y 16))
             (cursor-y stream) y)
       (bitset 16 win-width (window-background-colour stream) fb (+ (widget-y stream) y) (widget-x stream)))
      (t (let ((width (sys.int::unifont-glyph-width character)))
           (when (> (+ x width) win-width)
             ;; Advance to the next line.
             ;; Maybe should clear the end of the current line?
             (setf x 0
                   y (if (> (+ y 16 16) win-height)
                         0
                         (+ y 16))
                   (cursor-y stream) y)
             (bitset 16 win-width (window-background-colour stream) fb (+ (widget-y stream) y) (widget-x stream)))
           (bitset 16 width (window-background-colour stream) fb (+ (widget-y stream) y) (+ (widget-x stream) x))
           (let ((glyph (sys.int::map-unifont-2d character)))
             (when glyph
               (bitset-argb-xrgb-mask-1 16 width (window-foreground-colour stream)
                                        glyph 0 0
                                        fb (+ (widget-y stream) y) (+ (widget-x stream) x))))
           (incf x width)
           (setf (cursor-x stream) x))))))

(defmethod sys.gray:stream-write-char ((stream text-window) character)
  (sys.gray:stream-write-char (text-window-display stream) character)
  (setf *refresh-required* t))

(defmethod sys.gray:stream-start-line-p ((stream text-widget))
  (zerop (cursor-x stream)))

(defmethod sys.gray:stream-start-line-p ((stream text-window))
  (sys.gray:stream-start-line-p (text-window-display stream)))

(defmethod sys.gray:stream-line-column ((stream text-widget))
  (truncate (cursor-x stream) 8))

(defmethod sys.gray:stream-line-column ((stream text-window))
  (sys.gray:stream-line-column (text-window-display stream)))

(defmethod sys.int::stream-cursor-pos ((stream text-widget))
  (values (cursor-x stream) (cursor-y stream)))

(defmethod sys.int::stream-cursor-pos ((stream text-window))
  (sys.int::stream-cursor-pos (text-window-display stream)))

(defmethod sys.int::stream-move-to ((stream text-widget) x y)
  (check-type x integer)
  (check-type y integer)
  (setf (cursor-x stream) x
        (cursor-y stream) y))

(defmethod sys.int::stream-move-to ((stream text-window) x y)
  (sys.int::stream-move-to (text-window-display stream) x y))

(defmethod sys.int::stream-character-width ((stream text-widget) character)
  (sys.int::unifont-glyph-width character))

(defmethod sys.int::stream-character-width ((stream text-window) character)
  (sys.int::stream-character-width (text-window-display stream) character))

(defmethod sys.int::stream-compute-motion ((stream text-widget) string &optional (start 0) end initial-x initial-y)
  (unless end (setf end (length string)))
  (unless initial-x (setf initial-x (cursor-x stream)))
  (unless initial-y (setf initial-y (cursor-y stream)))
  (do ((framebuffer (framebuffer stream))
       (win-width (widget-width stream))
       (win-height (widget-height stream))
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

(defmethod sys.int::stream-compute-motion ((stream text-window) string &optional (start 0) end initial-x initial-y)
  (sys.int::stream-compute-motion (text-window-display stream) string start end initial-x initial-y))

(defmethod sys.int::stream-clear-between ((stream text-widget) start-x start-y end-x end-y)
  (let ((framebuffer (framebuffer stream))
        (win-width (widget-width stream))
        (win-height (widget-height stream))
        (colour (window-background-colour stream)))
    (cond ((eql start-y end-y)
           ;; Clearing one line.
           (bitset 16 (- end-x start-x) colour framebuffer (+ (widget-y stream) start-y) (+ (widget-x stream) start-x)))
          (t ;; Clearing many lines.
           ;; Clear top line.
           (bitset 16 (- win-width start-x) colour
                   framebuffer (+ (widget-y stream) start-y) (+ (widget-x stream) start-x))
           ;; Clear in-between.
           (when (> (- end-y start-y) 16)
             (bitset (- end-y start-y 16) win-width colour
                     framebuffer (+ (widget-y stream) start-y 16) (widget-x stream)))
           ;; Clear bottom line.
           (bitset 16 end-x colour
                   framebuffer (+ (widget-y stream) end-y) (widget-x stream))))))

(defmethod sys.int::stream-clear-between ((stream text-window) start-x start-y end-x end-y)
  (sys.int::stream-clear-between (text-window-display stream) start-x start-y end-x end-y)
  (setf *refresh-required* t))

(defclass lisp-listener (text-window-with-chrome)
  ((process :reader lisp-listener-process)))

(defmacro with-window-streams (window &body body)
  "Rebind all stream variables to WINDOW."
  `(let* ((*terminal-io* ,window)
          (*standard-input* (make-synonym-stream '*terminal-io*))
          (*standard-output* *standard-input*)
          (*error-output* *standard-input*)
          (*query-io* *standard-input*)
          (*debug-io* *standard-input*)
          (*trace-output* *standard-input*))
     ,@body))

(defun lisp-listener-top-level (window)
  (unwind-protect
       (with-window-streams window
         (handler-case (sys.int::repl)
           (sys.int::quit-lisp ())))
    (close-window window)))

(defmethod initialize-instance :after ((instance lisp-listener))
  (let ((process (sys.int::make-process "Lisp Listener REPL")))
    (setf (slot-value instance 'process) process)
    (sys.int::process-preset process 'lisp-listener-top-level instance)
    (sys.int::process-enable process)))

(defmethod window-close-event :before ((window lisp-listener))
  (sys.int::process-arrest-reason (lisp-listener-process window) :window-closed))

(defun create-lisp-listener ()
  (window-set-visibility (make-window "Lisp Listener" 640 400 'lisp-listener) t))

(defvar *console-window* (make-window "Console" 640 400 'text-window-with-chrome :close-button nil))
(window-set-visibility *console-window* t)
