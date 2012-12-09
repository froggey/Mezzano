(defpackage #:sys.graphics
  (:use #:cl)
  (:export #:register-screen))

(in-package #:sys.int)

(defmacro with-saved-screen ((&optional framebuffer &rest options) &body body)
  (let ((fb-sym (or framebuffer (gensym))))
    `(%with-saved-screen (lambda (,fb-sym) (declare (ignorable ,fb-sym)) ,@body) ,@options)))

(defun framebuffer-from-stream (stream)
  (when (typep stream 'shadow-stream)
    (setf stream (shadow-stream-primary stream)))
  (when (typep stream 'framebuffer-stream)
    (slot-value stream 'framebuffer)))

(defun %with-saved-screen (fn)
  (let ((fb (framebuffer-from-stream *terminal-io*)))
    (if fb
        (let* ((dims (array-dimensions fb))
               (position (multiple-value-list (sys.int::stream-cursor-pos *terminal-io*)))
               (back-buffer (make-array (array-dimensions fb)
                                        :element-type (array-element-type fb))))
          (%bitblt (first dims) (second dims)
                            fb 0 0
                            back-buffer 0 0)
          (unwind-protect
               (funcall fn fb)
            (apply 'stream-move-to *terminal-io* position)
            (%bitblt (first dims) (second dims)
                              back-buffer 0 0
                              fb 0 0)))
        (funcall fn nil))))

(in-package #:sys.graphics)

(defstruct screen
  name
  framebuffer
  backbuffer
  flip-function
  relative-position)

(defvar *screens* '())
(defvar *refresh-required* t)
(defvar *window-list* nil)
(defvar *global-keybindings* (make-hash-table))

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defun restore-default-keybindings ()
  (clrhash *global-keybindings*)
  (setf (gethash (name-char "M-Tab") *global-keybindings*) 'next-window
        (gethash (name-char "Esc") *global-keybindings*) 'suspend-graphics
        (gethash (name-char "F1") *global-keybindings*) 'create-lisp-listener))
(when (zerop (hash-table-count *global-keybindings*))
  (restore-default-keybindings))

(defun next-window ()
  (when *window-list*
    (setf *window-list* (window-next *window-list*))
    (setf *refresh-required* t)))

(defvar *read-input* nil)

(defun invoke-graphics ()
  (setf *read-input* t)
  (let ((old-terminal-io *terminal-io*))
    (unwind-protect
         (progn (setf *terminal-io* *console-window*)
                (sys.int::process-wait "Graphics" (lambda () (null *read-input*))))
      (setf *terminal-io* old-terminal-io))))

(defun suspend-graphics ()
  (setf *read-input* nil))

(defun register-screen (name framebuffer backbuffer flip-function)
  (push (cond (*screens*
               (make-screen :name name
                            :framebuffer framebuffer
                            :backbuffer backbuffer
                            :flip-function flip-function
                            :relative-position (cons :left-of (first *screens*))))
              (t (make-screen :name name
                              :framebuffer framebuffer
                              :backbuffer backbuffer
                              :flip-function flip-function
                              :relative-position :centre)))
        *screens*))

(defun screen-size ()
  (when *screens*
    (let* ((screen (first *screens*))
           (fb (screen-backbuffer screen))
           (dims (array-dimensions fb))
           (width (second dims))
           (height (first dims)))
      (values width height))))

(defmacro with-window ((window title width height &optional (class ''window)) &body body)
  `(let ((,window nil))
     (unwind-protect
         (progn
           (setf ,window (make-window ,title ,width ,height ,class))
           ,@body)
       (when ,window
         (close-window ,window)))))

(defstruct (fifo (:constructor %make-fifo))
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (buffer (error "No buffer specified.")
          :type array))

(defun make-fifo (size &optional (type 't))
  (%make-fifo :buffer (make-array size :element-type type)))

(defun fifo-emptyp (fifo)
  (eql (fifo-head fifo) (fifo-tail fifo)))

(defun fifo-push (value fifo)
  (let ((x (1+ (fifo-tail fifo))))
    (when (>= x (length (fifo-buffer fifo)))
      (setf x 0))
    ;; When next reaches head, the buffer is full.
    (cond ((= x (fifo-head fifo))
           nil)
          (t
           (setf (aref (fifo-buffer fifo) (fifo-tail fifo)) value
                 (fifo-tail fifo) x)))))

(defun fifo-pop (fifo)
  "Pop a byte from FIFO. Returns NIL if FIFO is empty!"
  (unless (fifo-emptyp fifo)
    (prog1 (aref (fifo-buffer fifo) (fifo-head fifo))
      (incf (fifo-head fifo))
      (when (>= (fifo-head fifo) (length (fifo-buffer fifo)))
        (setf (fifo-head fifo) 0)))))

(defclass window ()
  ((title :initarg :title :accessor window-title)
   (width :initarg :width :reader window-width)
   (height :initarg :height :reader window-height)
   (pos :initarg :position :writer (setf window-position))
   (frontbuffer :initarg :frontbuffer :reader window-frontbuffer)
   (backbuffer :initarg :backbuffer :reader window-backbuffer)
   (visiblep :initarg :visiblep :reader window-visiblep)
   (next :accessor window-next)
   (prev :accessor window-prev))
  (:default-initargs :visiblep nil))

(defgeneric compute-window-margins (window)
  (:documentation "Return the size of the left, right, top and bottom window margins as values."))

(defmethod compute-window-margins ((window window))
  (values 0 0 0 0))

(defgeneric window-position (window))

(defmethod window-position ((window window))
  (let ((pos (slot-value window 'pos)))
    (values (car pos) (cdr pos))))

(defgeneric window-swap-buffers (window))

(defmethod window-swap-buffers ((window window))
  (rotatef (slot-value window 'frontbuffer) (slot-value window 'backbuffer))
  (setf *refresh-required* t)
  (values))

(defgeneric window-set-visibility (window visiblep))
(defmethod window-set-visibility ((window window) visiblep)
  (setf *refresh-required* t)
  (setf (slot-value window 'visiblep) visiblep))

(defvar *next-window-position* 0)

(defmethod initialize-instance :after ((instance window) &key width height)
  (multiple-value-bind (left right top bottom)
      (compute-window-margins instance)
    (setf (slot-value instance 'frontbuffer) (make-array (list (+ height top bottom) (+ width left right))
                                                         :element-type '(unsigned-byte 32))
          (slot-value instance 'backbuffer) (make-array (list (+ height top bottom) (+ width left right))
                                                        :element-type '(unsigned-byte 32)))))

(defvar *active-drag-window* nil)

(defun window-begin-drag (window mouse-x mouse-y)
  (assert (not *active-drag-window*) () "Drag already in progress!")
  (setf *active-drag-window* (list window mouse-x mouse-y)))

(defun window-finish-drag (window)
  (assert (and *active-drag-window* (eql (first *active-drag-window*) window))
          (window) "Not dragging window!")
  (setf *active-drag-window* nil))

(defclass window-with-chrome (window)
  ((chrome-drag-state :initform '())))

(defmethod compute-window-margins ((window window-with-chrome))
  (values 1 1 19 1))

(defgeneric window-redraw (window))

(defgeneric key-press-event (window character))
(defmethod key-press-event ((window window) character))
(defgeneric mouse-button-event (window button-id state mouse-x mouse-y))
(defmethod mouse-button-event ((window window) button-id state mouse-x mouse-y))
(defgeneric mouse-move-event (window mouse-x mouse-y rel-x rel-y button-state))
(defmethod mouse-move-event ((window window) mouse-x mouse-y rel-x rel-y button-state))

(defun window-to-front (window)
  (format t "Raising window ~S  (current is ~S)~%" window *window-list*)
  (unless (eql window *window-list*)
    (assert *window-list* () "No open windows?")
    (setf (slot-value (window-next window) 'prev) (window-prev window)
          (slot-value (window-prev window) 'next) (window-next window))
    (setf (slot-value window 'next) *window-list*
          (slot-value window 'prev) (window-prev *window-list*))
    (setf (slot-value (window-prev *window-list*) 'next) window)
    (setf (slot-value *window-list* 'prev) window)
    (setf *window-list* window)
    (setf *refresh-required* t)))

(defmethod window-redraw :before ((window window-with-chrome))
  (let* ((fb (window-backbuffer window))
         (dims (array-dimensions fb))
         (border-colour (make-colour :gray))
         (titlebar-colour (make-colour :black))
         #+nil(text-colour (make-colour :white)))
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
          (when (> (+ x width) (second dims))
            (return))
          (unless (eql character #\Space)
            (sys.int::render-char-at character fb x 1))
          (incf x width))))))

(defmethod mouse-button-event :around ((window window-with-chrome) button-id state mouse-x mouse-y)
  (let* ((fb (window-backbuffer window))
         (dims (array-dimensions fb)))
    (cond ((or (and (<= 1 mouse-y 17)
                    (<= 1 mouse-x (- (second dims) 2)))
               (slot-value window 'chrome-drag-state))
           ;; Clicking the titlebar.
           (when (eql button-id 0)
             ;; Engage or disengage window dragging.
             (cond (state
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

(defun make-window (title width height class &rest initargs)
  (when (> (+ *next-window-position* width) 1000)
    (setf *next-window-position* 0))
  (when (> (+ *next-window-position* height) 1000)
    (setf *next-window-position* 0))
  (let ((window (apply 'make-instance
                       class
                       :title title
                       :width width
                       :height height
                       :position (cons *next-window-position* *next-window-position*)
                       initargs)))
    (window-redraw window)
    (incf *next-window-position* 50)
    (cond (*window-list*
           (setf (slot-value window 'next) *window-list*
                 (slot-value window 'prev) (window-prev *window-list*))
           (setf (slot-value (window-prev *window-list*) 'next) window)
           (setf (slot-value *window-list* 'prev) window))
          (t (setf (slot-value window 'next) window
                   (slot-value window 'prev) window)))
    (setf *window-list* window)
    window))

(defun close-window (window)
  (setf (slot-value (window-next window) 'prev) (window-prev window))
  (setf (slot-value (window-prev window) 'next) (window-next window))
  (when (eql *window-list* window)
    (if (eql (window-next window) window)
        ;; Closing the last window.
        (setf *window-list* nil)
        (setf *window-list* (window-next window))))
  (setf *refresh-required* t)
  (values))

(defgeneric window-move-to (window x y))

(defmethod window-move-to ((window window) x y)
  (setf (slot-value window 'pos) (cons x y))
  (setf *refresh-required* t))

(defun window-test (&optional (bgcolour (make-colour 0.5 0 0.5)))
  (with-window (window "TEST WINDOW" 640 400 'text-window)
    (bitset 400 640 bgcolour (window-frontbuffer window) 0 0)
    (window-set-visibility window t)
    (window-move-to window 300 300)
    (let ((*standard-input* window)
          (*standard-output* window))
      (with-simple-restart (abort "Leave REPL window")
        (sys.int::repl)))))

(defun clamp (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(declaim (special *colour-names*))

(defun make-colour (colour)
  (if (listp colour)
      (destructuring-bind (r g b &optional (a 1)) colour
        (logior (ash (truncate (* (clamp a 0 1) 255)) 24)
                (ash (truncate (* (clamp r 0 1) 255)) 16)
                (ash (truncate (* (clamp g 0 1) 255)) 8)
                (truncate (* (clamp b 0 1) 255))))
      (let ((c (assoc colour *colour-names*)))
        (unless c (error "Unknown colour name ~S." colour))
        (cdr c))))

(defvar *colour-names*
  `((:white   . ,(make-colour '(1.0  1.0  1.0)))
    (:silver  . ,(make-colour '(0.75 0.75 0.75)))
    (:gray    . ,(make-colour '(0.5  0.5  0.5)))
    (:black   . ,(make-colour '(0.0  0.0  0.0)))
    (:red     . ,(make-colour '(1.0  0.0  0.0)))
    (:maroon  . ,(make-colour '(0.5  0.0  0.0)))
    (:yellow  . ,(make-colour '(1.0  1.0  0.0)))
    (:olive   . ,(make-colour '(0.5  0.5  0.0)))
    (:lime    . ,(make-colour '(0.0  1.0  0.0)))
    (:green   . ,(make-colour '(0.0  0.5  0.0)))
    (:aqua    . ,(make-colour '(0.0  1.0  1.0)))
    (:teal    . ,(make-colour '(0.0  0.5  0.5)))
    (:blue    . ,(make-colour '(0.0  0.0  1.0)))
    (:navy    . ,(make-colour '(0.0  0.0  0.5)))
    (:fuchsia . ,(make-colour '(1.0  0.0  1.0)))
    (:purple  . ,(make-colour '(0.5  0.0  0.5))))
  "HTML colour names.")

(defun name-colour (name)
  (cdr (assoc name *colour-names*)))

(defun bitblt (nrows ncols from-array from-row from-col to-array to-row to-col)
  "Like %BITBLT, but clamps to the array dimensions."
  (let ((from-width (array-dimension from-array 1))
        (from-height (array-dimension from-array 0))
        (to-width (array-dimension to-array 1))
        (to-height (array-dimension to-array 0)))
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (decf from-row to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (decf from-col to-col)
      (setf to-col 0))
    ;; Clamp from row/column.
    (when (< from-row 0)
      (incf nrows from-row)
      (decf to-row from-row)
      (setf from-row 0))
    (when (< from-col 0)
      (incf ncols from-col)
      (decf to-col from-col)
      (setf from-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (min nrows (- to-height to-row) (- from-height from-row)))
    (setf ncols (min ncols (- to-width to-col) (- from-width from-col)))
    (when (and (> nrows 0)
               (> ncols 0))
      (sys.int::%bitblt nrows ncols from-array from-row from-col to-array to-row to-col))))

(defun bitset (nrows ncols val to-array to-row to-col)
  "Like %BITSET, but clamps to the array dimensions."
  (let ((to-width (array-dimension to-array 1))
        (to-height (array-dimension to-array 0)))
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (setf to-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (min nrows (- to-height to-row)))
    (setf ncols (min ncols (- to-width to-col)))
    (when (and (> nrows 0)
               (> ncols 0))
      (sys.int::%bitset nrows ncols val to-array to-row to-col))))

(defun bitblt-test ()
  (let ((src (make-array (list 64 32) :element-type '(unsigned-byte 32))))
    (sys.int::%bitset 64 32 (make-colour '(0.8 0.5 0.2)) src 0 0)
    (bitblt 64 32 src 0 0 sys.int::*bochs-framebuffer* 500 500)
    (bitblt 64 32 src 0 0 sys.int::*bochs-framebuffer* -16 500)
    (bitblt 64 32 src 0 0 sys.int::*bochs-framebuffer* 500 -16)
    (bitblt 64 32 src 0 0 sys.int::*bochs-framebuffer* 300 1000)
    (bitblt 64 32 src 0 0 sys.int::*bochs-framebuffer* 750 300)
    (bitblt 128 64 src 0 0 sys.int::*bochs-framebuffer* 600 600)
    (bitblt -64 -32 src 0 0 sys.int::*bochs-framebuffer* 700 700)
    (bitblt 64 32 src 20 10 sys.int::*bochs-framebuffer* 400 400)))

(defgeneric blit-window (framebuffer window))

(defmethod blit-window (fb (window window))
  (multiple-value-bind (x y)
      (window-position window)
    (let* ((src-buffer (window-frontbuffer window))
           (dims (array-dimensions src-buffer))
           (width (second dims))
           (height (first dims)))
      (bitblt height width src-buffer 0 0 fb y x))))

(defun compose-all-displays ()
  (when *screens*
    (let* ((screen (first *screens*))
           (fb (screen-backbuffer screen))
           (dims (array-dimensions fb))
           (width (second dims))
           (height (first dims)))
      (sys.int::%bitset height width (name-colour :blue) fb 0 0)
      (when *window-list*
        (do ((window (window-prev *window-list*) (window-prev window)))
            ((eql window *window-list*))
          (when (window-visiblep window)
            (blit-window fb window)))
        (when (window-visiblep *window-list*)
          (blit-window fb *window-list*)))
      (bitset 16 16 (name-colour :white) fb *mouse-y* *mouse-x*)
      (bitset 14 14 (name-colour :black) fb (1+ *mouse-y*) (1+ *mouse-x*))
      (bitset 4 4 (name-colour :white) fb *mouse-y* *mouse-x*)
      (rotatef (screen-backbuffer screen) (screen-framebuffer screen))
      (funcall (screen-flip-function screen)))))

(defun handle-keypress (key)
  (let ((command (gethash key *global-keybindings*)))
    (when command
      (funcall command)
      (return-from handle-keypress)))
  (when *window-list*
    (key-press-event *window-list* key)))

(defun decode-mouse-packet (byte-1 byte-2 byte-3)
  (values (logand byte-1 #b111) ; buttons 1 to 3
          (logior byte-2 (if (logtest byte-1 #b00010000) -256 0)) ; x-motion
          (- (logior byte-3 (if (logtest byte-1 #b00100000) -256 0))) ; y-motion
          0)) ; z-motion

(defvar *mouse-button-state* 0
  "Bits representing the state of each button. 1 is pressed, 0 is released.")

(defun map-windows (fn &optional reverse)
  "Invoke FN on each window. If REVERSE is true, then windows are visited in
reverse Z-order."
  (when *window-list*
    (if reverse
        (do ((window (window-prev *window-list*) (window-prev window)))
            ((eql window *window-list*)
             (funcall fn *window-list*))
          (funcall fn window))
        (progn
          (funcall fn *window-list*)
          (do ((window (window-next *window-list*) (window-next window)))
              ((eql window *window-list*))
            (funcall fn window))))))

(defun window-at (x y)
  "Find the highest window at (X,Y), NIL if no window there."
  (let ((result nil))
    (tagbody
       (map-windows (lambda (win)
                      (multiple-value-bind (win-x win-y)
                          (window-position win)
                        (let* ((dims (array-dimensions (window-frontbuffer win)))
                               (width (second dims))
                               (height (first dims)))
                          (when (and (<= win-x x (+ win-x width -1))
                                     (<= win-y y (+ win-y height -1)))
                            (setf result win)
                            (go end))))))
     end)
    result))

(defun process-mouse-packet (byte-1 byte-2 byte-3)
  (multiple-value-bind (buttons x-motion y-motion z-motion)
      (decode-mouse-packet byte-1 byte-2 byte-3)
    (when *screens*
      (let* ((screen (first *screens*))
             (fb (screen-backbuffer screen))
             (dims (array-dimensions fb))
             (width (second dims))
             (height (first dims)))
        (setf *mouse-x* (clamp (+ *mouse-x* x-motion) 0 (1- width)))
        (setf *mouse-y* (clamp (+ *mouse-y* y-motion) 0 (1- height)))))
    (let ((window (or (when *active-drag-window* (first *active-drag-window*))
                      (window-at *mouse-x* *mouse-y*))))
      (when window
        (multiple-value-bind (win-x win-y)
            (window-position window)
          (cond ((eql window *window-list*)
                 ;; Window is at front.
                 ;; Send mouse button events for each changing button.
                 (do ((button 0 (1+ button))
                      (current buttons (ash current -1))
                      (changed (logxor buttons *mouse-button-state*) (ash changed -1)))
                     ((zerop changed))
                   (when (logtest changed 1)
                     (mouse-button-event window button (logtest current 1) (- *mouse-x* win-x) (- *mouse-y* win-y)))))
                (t ;; Window is behind, raise it up.
                 (unless (zerop (logxor buttons *mouse-button-state*))
                   (window-to-front window))))
          ;; Send mouse move event when the mouse moves.
          (when (or (not (zerop x-motion))
                    (not (zerop y-motion)))
            (mouse-move-event window
                              (- *mouse-x* win-x) (- *mouse-y* win-y)
                              x-motion y-motion
                              buttons)))))
    (setf *mouse-button-state* buttons)
    (when (or (not (zerop x-motion))
              (not (zerop y-motion)))
      (setf *refresh-required* t))))

(defun graphics-worker ()
  (let ((mouse-state 0)
        (mouse-1 0)
        (mouse-2 0))
    (loop (sys.int::process-wait "Input and display update"
                                 (lambda ()
                                   (or *refresh-required*
                                       (and *read-input*
                                            (not (sys.int::ps/2-fifo-empty sys.int::*ps/2-key-fifo*)))
                                       (not (sys.int::ps/2-fifo-empty sys.int::*ps/2-aux-fifo*)))))
       (let ((*terminal-io* (sys.int::make-cold-stream)))
         (when *refresh-required*
           (setf *refresh-required* nil)
           (with-simple-restart (abort "Cancel screen update.")
             (compose-all-displays)))
         (when *read-input*
           (loop
              (let ((byte (sys.int::ps/2-pop-fifo sys.int::*ps/2-key-fifo*)))
                (when (not byte) (return))
                (let ((key (sys.int::ps/2-translate-scancode byte)))
                  (when key
                    (handle-keypress key))))))
         (loop
            (let ((byte (sys.int::ps/2-pop-fifo sys.int::*ps/2-aux-fifo*)))
              (when (not byte) (return))
              (ecase mouse-state
                (0 (when (logtest byte #b00001000)
                     (setf mouse-1 byte)
                     (incf mouse-state)))
                (1 (setf mouse-2 byte)
                   (incf mouse-state))
                (2 (process-mouse-packet mouse-1 mouse-2 byte)
                   (setf mouse-state 0)))))))))

(defvar *graphics-process* (make-instance 'sys.int::process :name "Graphics manager"))
(sys.int::process-preset *graphics-process* #'graphics-worker)
(sys.int::process-enable *graphics-process*)

(defun graphics-early-initialize ()
  ;; Forget all screens.
  (setf *screens* '())
  (setf *refresh-required* t)
  ;; Reinitialize the worker process.
  (sys.int::process-reset *graphics-process*))
(sys.int::add-hook '*early-initialize-hook* 'graphics-early-initialize)

(defclass text-window (sys.int::edit-stream sys.int::stream-object window)
  ((cursor-x :initarg :x :accessor cursor-x)
   (cursor-y :initarg :y :accessor cursor-y)
   (buffer :initarg :buffer :reader text-window-buffer))
  (:default-initargs :x 0 :y 0
                     :buffer (make-fifo 500 'character)))

(defclass text-window-with-chrome (text-window window-with-chrome) ())

(defmethod window-redraw ((window text-window))
  (multiple-value-bind (left right top bottom)
      (compute-window-margins window)
    (bitset (window-height window) (window-width window) (make-colour :black) (window-backbuffer window) top left)))

(defmethod key-press-event ((window text-window) character)
  (fifo-push character (text-window-buffer window)))

(defmethod mouse-button-event ((window text-window) button-id state mouse-x mouse-y)
  (format t "CLICK ~D ~S (~D,~D)~%" button-id state mouse-x mouse-y))

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
         (sys.int::%bitset 16 win-width #xFF000000 fb (+ top y) left))
        (t (let ((width (if (eql character #\Space) 8 (sys.int::unifont-glyph-width character))))
             (when (> (+ x width) win-width)
               ;; Advance to the next line.
               ;; Maybe should clear the end of the current line?
               (setf x 0
                     y (if (> (+ y 16 16) win-height)
                           0
                           (+ y 16))
                     (cursor-y stream) y)
               (sys.int::%bitset 16 win-width #xFF000000 fb (+ top y) left))
             (if (eql character #\Space)
                 (sys.int::%bitset 16 8 #xFF000000 fb (+ top y) (+ left x))
                 (sys.int::render-char-at character fb (+ left x) (+ top y)))
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
          (win-height (window-height stream)))
      (cond ((eql start-y end-y)
             ;; Clearing one line.
             (sys.int::%bitset 16 (- end-x start-x) #xFF000000 framebuffer (+ top start-y) (+ left start-x)))
            (t ;; Clearing many lines.
             ;; Clear top line.
             (sys.int::%bitset 16 (- win-width start-x) #xFF000000
                               framebuffer (+ top start-y) (+ left start-x))
             ;; Clear in-between.
             (when (> (- end-y start-y) 16)
               (sys.int::%bitset (- end-y start-y 16) win-width #xFF000000
                                 framebuffer (+ top start-y 16) left))
             ;; Clear bottom line.
             (sys.int::%bitset 16 end-x #xFF000000
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

(define-condition sys.int::quit-lisp () ())
(defun sys.int::quit () (signal 'sys.int::quit-lisp))

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

(defun create-lisp-listener ()
  (window-set-visibility (make-window "Lisp Listener" 640 400 'lisp-listener) t))

(defvar *console-window* (make-window "Console" 640 400 'text-window-with-chrome))
(window-set-visibility *console-window* t)
