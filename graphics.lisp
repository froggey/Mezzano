(defpackage :sys.graphics
  (:use :cl)
  (:export :register-screen))

(in-package :sys.graphics)

;;; Colours from Zenburn.
(defvar *default-foreground-colour* '(0.863 0.863 0.8))
(defvar *default-background-colour* '(0.247 0.247 0.247))
(defvar *default-background-darker-colour* '(0.169 0.169 0.169))

(defvar *background* nil)

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

(defun 2d-array (data &optional (element-type 't))
  (let* ((width (length (first data)))
         (height (length data)))
    (make-array (list height width)
                :element-type element-type
                :initial-contents data)))

(defvar *mouse-pointer*
  (2d-array '((#xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFFFFFFFF #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFFFFFFFF #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFF000000 #xFF000000 #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF #xFFFFFFFF)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFF000000 #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
              (#xFFFFFFFF #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000))
            '(unsigned-byte 32)))

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

(defvar *console-window*)

(defun invoke-graphics ()
  (setf *read-input* t
        *refresh-required* t)
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

(defmacro with-window ((window title width height &optional (class ''window) &rest initargs) &body body)
  `(let ((,window nil))
     (unwind-protect
         (progn
           (setf ,window (make-window ,title ,width ,height ,class ,@initargs))
           ,@body)
       (when ,window
         (close-window ,window)))))

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
                                                         :element-type '(unsigned-byte 32)
                                                         :area :static)
          (slot-value instance 'backbuffer) (make-array (list (+ height top bottom) (+ width left right))
                                                        :element-type '(unsigned-byte 32)
                                                        :area :static))))

(defvar *active-drag-window* nil)

(defun window-begin-drag (window mouse-x mouse-y)
  (assert (not *active-drag-window*) () "Drag already in progress!")
  (setf *active-drag-window* (list window mouse-x mouse-y)))

(defun window-finish-drag (window)
  (assert (and *active-drag-window* (eql (first *active-drag-window*) window))
          (window) "Not dragging window!")
  (setf *active-drag-window* nil))

(defgeneric window-redraw (window))

(defgeneric key-press-event (window character))
(defmethod key-press-event ((window window) character))
(defgeneric mouse-button-event (window button-id state mouse-x mouse-y))
(defmethod mouse-button-event ((window window) button-id state mouse-x mouse-y))
(defgeneric mouse-move-event (window mouse-x mouse-y rel-x rel-y button-state))
(defmethod mouse-move-event ((window window) mouse-x mouse-y rel-x rel-y button-state))
(defgeneric window-close-event (window))
(defmethod window-close-event ((window window)))

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
  (etypecase colour
    (list
     (destructuring-bind (r g b &optional (a 1)) colour
       (logior (ash (truncate (* (clamp a 0 1) 255)) 24)
               (ash (truncate (* (clamp r 0 1) 255)) 16)
               (ash (truncate (* (clamp g 0 1) 255)) 8)
               (truncate (* (clamp b 0 1) 255)))))
    (symbol
     (let ((c (assoc colour *colour-names*)))
       (unless c (error "Unknown colour name ~S." colour))
       (cdr c)))
    ((unsigned-byte 32) colour)))

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
      (bitblt-argb-xrgb height width src-buffer 0 0 fb y x))))

(defun compose-all-displays ()
  (when *screens*
    (let* ((screen (first *screens*))
           (fb (screen-backbuffer screen))
           (dims (array-dimensions fb))
           (width (second dims))
           (height (first dims)))
      (typecase *background*
        ((unsigned-byte 32)
         (sys.int::%bitset height width *background* fb 0 0))
        ((array (unsigned-byte 32) (* *))
         (bitblt (array-dimension *background* 0) (array-dimension *background* 1)
                 *background* 0 0
                 fb 0 0))
        (t (sys.int::%bitset height width (name-colour :blue) fb 0 0)))
      (when *window-list*
        (do ((window (window-prev *window-list*) (window-prev window)))
            ((eql window *window-list*))
          (when (window-visiblep window)
            (blit-window fb window)))
        (when (window-visiblep *window-list*)
          (blit-window fb *window-list*)))
      (bitblt-argb-xrgb (array-dimension *mouse-pointer* 0) (array-dimension *mouse-pointer* 1)
                        *mouse-pointer* 0 0
                        fb *mouse-y* *mouse-x*)
      (when (screen-flip-function screen)
        (rotatef (screen-backbuffer screen) (screen-framebuffer screen))
        (funcall (screen-flip-function screen))))))

(defun handle-keypress (key)
  (let ((command (gethash key *global-keybindings*)))
    (when command
      (ignore-errors
        (funcall command))
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
          (unless (eql window *window-list*)
            ;; Window is behind, raise it up.
            (unless (zerop (logxor buttons *mouse-button-state*))
              (window-to-front window)))
          ;; Send mouse button events for each changing button.
          (do ((button 0 (1+ button))
               (current buttons (ash current -1))
               (changed (logxor buttons *mouse-button-state*) (ash changed -1)))
              ((zerop changed))
            (when (logtest changed 1)
              (mouse-button-event window button (logtest current 1) (- *mouse-x* win-x) (- *mouse-y* win-y))))
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
        (mouse-2 0)
        (*terminal-io* (sys.int::make-cold-stream)))
    (loop (sys.int::process-wait "Input and display update"
                                 (lambda ()
                                   (or *refresh-required*
                                       (and *read-input*
                                            (or (not (sys.int::ps/2-fifo-empty sys.int::*ps/2-key-fifo*))
                                                (not (sys.int::ps/2-fifo-empty sys.int::*ps/2-aux-fifo*)))))))
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
                  (handle-keypress key)))))
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

(defvar *graphics-process* (sys.int::make-process "Graphics manager"))
(sys.int::process-preset *graphics-process* #'graphics-worker)
(sys.int::process-enable *graphics-process*)

(defun graphics-early-initialize ()
  ;; Forget all screens.
  (setf *screens* '())
  (setf *refresh-required* t)
  ;; Reinitialize the worker process.
  (sys.int::process-reset *graphics-process*))
(sys.int::add-hook '*early-initialize-hook* 'graphics-early-initialize)

(define-condition sys.int::quit-lisp () ())
(defun sys.int::quit () (signal 'sys.int::quit-lisp))

(defun update-window (window)
  "Notify the compositor that WINDOW's front buffer has changed and the screen should be updated."
  (setf *refresh-required* t))
