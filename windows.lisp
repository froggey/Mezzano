;;;; Window-related classes & methods.

(in-package #:sys.graphics)

(defclass window-with-chrome (window)
  ((chrome-drag-state :initform '())
   (has-close-button :initarg :close-button :initform 't)))

(defmethod compute-window-margins ((window window-with-chrome))
  (values 1 1 19 1))

(defun 2d-array (data)
  (let* ((width (length (first data)))
         (height (length data))
         (a (make-array (list height width))))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref a y x) (elt (elt data y) x))))
    a))

(defvar *corner-mask*
  (2d-array '((t   t   t   t   t)
              (t   t   t nil nil)
              (t   t nil nil nil)
              (t nil nil nil nil)
              (t nil nil nil nil))))

(defvar *corner-border*
  (2d-array '((nil nil nil nil nil)
              (nil nil nil   t   t)
              (nil nil   t nil nil)
              (nil   t nil nil nil)
              (nil   t nil nil nil))))

(defmethod window-redraw :before ((window window-with-chrome))
  (let* ((fb (window-backbuffer window))
         (dims (array-dimensions fb))
         (border-colour (make-colour :gray))
         (titlebar-colour (make-colour :black))
         (text-colour (make-colour :white)))
    ;; Top.
    (bitset 1 (second dims) border-colour fb 0 0)
    ;; Top between title and main body.
    (bitset 1 (second dims) border-colour fb 18 0)
    ;; Bottom.
    (bitset 1 (second dims) border-colour fb (1- (first dims)) 0)
    ;; Left.
    (bitset (first dims) 1 border-colour fb 0 0)
    ;; Right
    (bitset (first dims) 1 border-colour fb 0 (1- (second dims)))
    ;; Title.
    (let ((x 5)
          (title (window-title window)))
      (bitset 17 (- (second dims) 2) titlebar-colour fb 1 1)
      (dotimes (i (length title))
        (let* ((character (char title i))
               (width (if (eql character #\Space) 8 (sys.int::unifont-glyph-width character))))
          (when (> (+ x width) (- (second dims) 2 16))
            (return))
          (unless (eql character #\Space)
            (let* ((glyph (sys.int::map-unifont-2d character)))
              (cond (glyph
                     (bitset-argb-xrgb-mask-1 16 width text-colour
                                              glyph 0 0
                                              fb 1 x))
                    (t))))
          (incf x width))))
    (when (slot-value window 'has-close-button)
      ;; Close button. Unifont uses a chunky double-wide X.
      (let* ((glyph (sys.int::map-unifont-2d (name-char "Multiplication-X")))
             (width (array-dimension glyph 1)))
        (cond (glyph
               (bitset-argb-xrgb-mask-1 16 width (make-colour :red)
                                        glyph 0 0
                                        fb 2 (- (second dims) 1 16)))
              (t))))
    ;; Round off the corners.
    (let* ((corner-width (array-dimension *corner-mask* 1))
           (corner-height (array-dimension *corner-mask* 0)))
      (dotimes (y corner-height)
        (dotimes (x corner-width)
          (when (aref *corner-mask* y x)
            (setf (aref fb y x) (make-colour '(0 0 0 0)))
            (setf (aref fb y (- (second dims) x 1)) (make-colour '(0 0 0 0))))
          (when (aref *corner-border* y x)
            (setf (aref fb y x) border-colour)
            (setf (aref fb y (- (second dims) x 1)) border-colour)))))))

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

(defclass text-window (sys.int::edit-stream sys.int::stream-object window)
  ((cursor-x :initarg :x :accessor cursor-x)
   (cursor-y :initarg :y :accessor cursor-y)
   (foreground :initarg :foreground :accessor window-foreground-colour)
   (background :initarg :background :accessor window-background-colour)
   (buffer :initarg :buffer :reader text-window-buffer))
  (:default-initargs :x 0 :y 0
                     :foreground (make-colour *default-foreground-colour*)
                     :background (make-colour *default-background-colour*)
                     :buffer (make-fifo 500 'character)))

(defclass text-window-with-chrome (text-window window-with-chrome) ())

(defmethod window-redraw ((window text-window))
  (multiple-value-bind (left right top bottom)
      (compute-window-margins window)
    (bitset (window-height window) (window-width window) (window-background-colour window) (window-backbuffer window) top left)))

(defmethod key-press-event ((window text-window) character)
  (fifo-push character (text-window-buffer window)))

(defmethod mouse-button-event ((window text-window) button-id state mouse-x mouse-y)
  (format t "CLICK ~D ~S (~D,~D)~%" button-id state mouse-x mouse-y))

(defmethod window-close-event ((window text-window))
  (close-window window))

(defmethod sys.int::stream-read-char ((stream text-window))
  (loop
     (let ((char (fifo-pop (text-window-buffer stream))))
       (when char (return char)))
     (sys.int::process-wait "User input"
                            (lambda ()
                              (not (fifo-emptyp (text-window-buffer stream)))))))

(defmethod sys.int::stream-write-char (character (stream text-window))
  (multiple-value-bind (left right top bottom)
      (compute-window-margins stream)
    (let ((fb (window-frontbuffer stream))
          (x (cursor-x stream))
          (y (cursor-y stream))
          (win-width (window-width stream))
          (win-height (window-height stream)))
      (cond
        ((eql character #\Newline)
         ;; Clear the next line.
         (setf (cursor-x stream) 0
               y (if (> (+ y 16 16) win-height)
                     0
                     (+ y 16))
               (cursor-y stream) y)
         (sys.int::%bitset 16 win-width (window-background-colour stream) fb (+ top y) left))
        (t (let ((width (if (eql character #\Space) 8 (sys.int::unifont-glyph-width character))))
             (when (> (+ x width) win-width)
               ;; Advance to the next line.
               ;; Maybe should clear the end of the current line?
               (setf x 0
                     y (if (> (+ y 16 16) win-height)
                           0
                           (+ y 16))
                     (cursor-y stream) y)
               (sys.int::%bitset 16 win-width (window-background-colour stream) fb (+ top y) left))
             (sys.int::%bitset 16 width (window-background-colour stream) fb (+ top y) (+ left x))
             (unless (eql character #\Space)
               (let* ((glyph (sys.int::map-unifont-2d character)))
                 (cond (glyph
                        (bitset-argb-xrgb-mask-1 16 width (window-foreground-colour stream)
                                                 glyph 0 0
                                                 fb (+ top y) (+ left x)))
                       (t))))
             (incf x width)
             (setf (cursor-x stream) x))))))
  (setf *refresh-required* t))

(defmethod sys.int::stream-start-line-p ((stream text-window))
  (zerop (cursor-x stream)))

(defmethod sys.int::stream-cursor-pos ((stream text-window))
  (values (cursor-x stream) (cursor-y stream)))

(defmethod sys.int::stream-move-to ((stream text-window) x y)
  (check-type x integer)
  (check-type y integer)
  (setf (cursor-x stream) x
        (cursor-y stream) y))

(defmethod sys.int::stream-character-width ((stream text-window) character)
  (if (eql character #\Space)
      8
      (sys.int::unifont-glyph-width character)))

(defmethod sys.int::stream-compute-motion ((stream text-window) string &optional (start 0) end initial-x initial-y)
  (unless end (setf end (length string)))
  (unless initial-x (setf initial-x (cursor-x stream)))
  (unless initial-y (setf initial-y (cursor-y stream)))
  (do ((framebuffer (window-frontbuffer stream))
       (win-width (window-width stream))
       (win-height (window-height stream))
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

(defmethod sys.int::stream-clear-between ((stream text-window) start-x start-y end-x end-y)
  (multiple-value-bind (left right top bottom)
      (compute-window-margins stream)
    (let ((framebuffer (window-frontbuffer stream))
          (win-width (window-width stream))
          (win-height (window-height stream))
          (colour (window-background-colour stream)))
      (cond ((eql start-y end-y)
             ;; Clearing one line.
             (sys.int::%bitset 16 (- end-x start-x) colour framebuffer (+ top start-y) (+ left start-x)))
            (t ;; Clearing many lines.
             ;; Clear top line.
             (sys.int::%bitset 16 (- win-width start-x) colour
                               framebuffer (+ top start-y) (+ left start-x))
             ;; Clear in-between.
             (when (> (- end-y start-y) 16)
               (sys.int::%bitset (- end-y start-y 16) win-width colour
                                 framebuffer (+ top start-y 16) left))
             ;; Clear bottom line.
             (sys.int::%bitset 16 end-x colour
                               framebuffer (+ top end-y) left)))))
  (setf *refresh-required* t))

(defmethod sys.int::stream-element-type* ((stream text-window))
  'character)

(defclass lisp-listener (text-window-with-chrome)
  ((process :reader lisp-listener-process)))

(defmacro with-window-streams (window &body body)
  "Rebind all stream variables to WINDOW."
  `(let* ((*terminal-io* ,window)
          (*standard-input* (make-synonym-stream '*terminal-io*))
          (*standard-output* *standard-input*)
          (*error-output* *standard-input*)
          (*query-io* *standard-input*)
          (*debug-io* *standard-input*))
     ,@body))

(defun lisp-listener-top-level (window)
  (unwind-protect
       (with-window-streams window
         (handler-case (sys.int::repl)
           (sys.int::quit-lisp ())))
    (close-window window)))

(defmethod initialize-instance :after ((instance lisp-listener))
  (let ((process (make-instance 'sys.int::process :name "Lisp Listener REPL")))
    (setf (slot-value instance 'process) process)
    (sys.int::process-preset process 'lisp-listener-top-level instance)
    (sys.int::process-enable process)))

(defmethod window-close-event :before ((window lisp-listener))
  (sys.int::process-arrest-reason (lisp-listener-process window) :window-closed))

(defun create-lisp-listener ()
  (window-set-visibility (make-window "Lisp Listener" 640 400 'lisp-listener) t))

(defvar *console-window* (make-window "Console" 640 400 'text-window-with-chrome :close-button nil))
(window-set-visibility *console-window* t)
