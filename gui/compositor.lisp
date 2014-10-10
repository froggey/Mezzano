(in-package :mezzanine.gui.compositor)

(defun clamp (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defvar *compositor* nil "Compositor thread.")
(defvar *compositor-heartbeat* nil "Compositor heartbeat thread. Drives redisplay.")

(defvar *event-queue* (mezzanine.supervisor:make-fifo 50)
  "Internal FIFO used to submit events to the compositor.")

(defclass window ()
  ((%x :initarg :x :accessor window-x)
   (%y :initarg :y :accessor window-y)
   (%fifo :initarg :fifo :reader fifo)
   (%buffer :initarg :buffer :reader window-buffer)))

(defgeneric width (thing))
(defgeneric height (thing))

(defmethod width ((window window))
  (array-dimension (window-buffer window) 1))

(defmethod height ((window window))
  (array-dimension (window-buffer window) 0))

(defvar *window-list* '())
(defvar *active-window* nil)

(defvar *main-screen* nil)
(defvar *screen-backbuffer* nil)

(defvar *clip-rect-x* 0)
(defvar *clip-rect-y* 0)
(defvar *clip-rect-width* 0)
(defvar *clip-rect-height* 0)

(defun expand-clip-rectangle (x y w h)
  (when (or (zerop *clip-rect-width*)
            (zerop *clip-rect-height*))
    (setf *clip-rect-x* x
          *clip-rect-y* y
          *clip-rect-width* w
          *clip-rect-height* h)
    (return-from expand-clip-rectangle))
  (let ((x2 (max (+ x w) (+ *clip-rect-x* *clip-rect-width*)))
        (y2 (max (+ y h) (+ *clip-rect-y* *clip-rect-height*))))
    #+debug-dirty-rects(format t "Expanding clip rect from ~D,~D ~D,~D~%"
            *clip-rect-x* *clip-rect-y* *clip-rect-width* *clip-rect-height*)
    (setf *clip-rect-x* (min *clip-rect-x* x)
          *clip-rect-y* (min *clip-rect-y* x)
          *clip-rect-width* (- x2 *clip-rect-x*)
          *clip-rect-height* (- y2 *clip-rect-y*))
    #+debug-dirty-rects(format t "Expanded clip rect to ~D,~D ~D,~D~%"
            *clip-rect-x* *clip-rect-y* *clip-rect-width* *clip-rect-height*)))

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

;;; Position of the "hot" pixel in *MOUSE-POINTER*, this is where clicks actually occur.
(defvar *mouse-hot-x* 0)
(defvar *mouse-hot-y* 0)

(defgeneric process-event (event))

;;;; Keyboard events, including translation from HID scancode.

(defclass key-event ()
  ((%scancode :initarg :scancode :reader key-scancode)
   (%releasep :initarg :releasep :reader key-releasep)
   (%key :initarg :key :reader key-key)
   (%modifier-state :initarg :modifier-state :reader key-modifier-state))
  (:default-initargs :scancode nil
                     :releasep nil
                     :key nil
                     :modifier-state '()))

(defvar *keyboard-modifier-state* '())
(defvar *keyboard-modifiers*
  '((#\Caps-Lock :caps-lock t)
    (#\Left-Shift :shift)
    (#\Right-Shift :shift)
    (#\Left-Control :control)
    (#\Right-Control :control)
    (#\Left-Meta :meta)
    (#\Right-Meta :meta)
    (#\Left-Super :super)
    (#\Right-Super :super)
    (#\Left-Hyper :hyper)
    (#\Right-Hyper :hyper)))

(defvar *keyboard-unshifted-map*
  '((#\0 . #\0)
    (#\1 . #\1)
    (#\2 . #\2)
    (#\3 . #\3)
    (#\4 . #\4)
    (#\5 . #\5)
    (#\6 . #\6)
    (#\7 . #\7)
    (#\8 . #\8)
    (#\9 . #\9)
    (#\A . #\a)
    (#\B . #\b)
    (#\C . #\c)
    (#\D . #\d)
    (#\E . #\e)
    (#\F . #\f)
    (#\G . #\g)
    (#\H . #\h)
    (#\I . #\i)
    (#\J . #\j)
    (#\K . #\k)
    (#\L . #\l)
    (#\M . #\m)
    (#\N . #\n)
    (#\O . #\o)
    (#\P . #\p)
    (#\Q . #\q)
    (#\R . #\r)
    (#\S . #\s)
    (#\T . #\t)
    (#\U . #\u)
    (#\V . #\v)
    (#\W . #\w)
    (#\X . #\x)
    (#\Y . #\y)
    (#\Z . #\z)
    (#\- . #\-)
    (#\= . #\=)
    (#\[ . #\[)
    (#\] . #\])
    (#\; . #\;)
    (#\' . #\')
    (#\` . #\`)
    (#\# . #\#)
    (#\, . #\,)
    (#\. . #\.)
    (#\/ . #\/)
    (#\\ . #\\)
    (#\Esc . #\Esc)
    (#\Backspace . #\Backspace)
    (#\Tab . #\Tab)
    (#\Newline . #\Newline)
    (#\Space . #\Space)
    (#\F1 . #\F1)
    (#\F2 . #\F2)
    (#\F3 . #\F3)
    (#\F4 . #\F4)
    (#\F5 . #\F5)
    (#\F6 . #\F6)
    (#\F7 . #\F7)
    (#\F8 . #\F8)
    (#\F9 . #\F9)
    (#\F10 . #\F10)
    (#\F11 . #\F11)
    (#\F12 . #\F12)
    (#\KP-0 . #\0)
    (#\KP-1 . #\1)
    (#\KP-2 . #\2)
    (#\KP-3 . #\3)
    (#\KP-4 . #\4)
    (#\KP-5 . #\5)
    (#\KP-6 . #\6)
    (#\KP-7 . #\7)
    (#\KP-8 . #\8)
    (#\KP-9 . #\9)
    (#\KP-Minus . #\-)
    (#\KP-Plus . #\+)
    (#\KP-Period . #\.)
    (#\KP-Multiply . #\*)))

(defvar *keyboard-shifted-map*
  '((#\0 . #\))
    (#\1 . #\!)
    (#\2 . #\")
    (#\3 . #\£)
    (#\4 . #\$)
    (#\5 . #\%)
    (#\6 . #\^)
    (#\7 . #\&)
    (#\8 . #\*)
    (#\9 . #\()
    (#\A . #\A)
    (#\B . #\B)
    (#\C . #\C)
    (#\D . #\D)
    (#\E . #\E)
    (#\F . #\F)
    (#\G . #\G)
    (#\H . #\H)
    (#\I . #\I)
    (#\J . #\J)
    (#\K . #\K)
    (#\L . #\L)
    (#\M . #\M)
    (#\N . #\N)
    (#\O . #\O)
    (#\P . #\P)
    (#\Q . #\Q)
    (#\R . #\R)
    (#\S . #\S)
    (#\T . #\T)
    (#\U . #\U)
    (#\V . #\V)
    (#\W . #\W)
    (#\X . #\X)
    (#\Y . #\Y)
    (#\Z . #\Z)
    (#\- . #\_)
    (#\= . #\+)
    (#\[ . #\{)
    (#\] . #\})
    (#\; . #\:)
    (#\' . #\@)
    (#\` . #\¬)
    (#\# . #\~)
    (#\, . #\>)
    (#\. . #\<)
    (#\/ . #\?)
    (#\\ . #\|)
    (#\Esc . #\Esc)
    (#\Backspace . #\Backspace)
    (#\Tab . #\Tab)
    (#\Newline . #\Newline)
    (#\Space . #\Space)
    (#\F1 . #\F1)
    (#\F2 . #\F2)
    (#\F3 . #\F3)
    (#\F4 . #\F4)
    (#\F5 . #\F5)
    (#\F6 . #\F6)
    (#\F7 . #\F7)
    (#\F8 . #\F8)
    (#\F9 . #\F9)
    (#\F10 . #\F10)
    (#\F11 . #\F11)
    (#\F12 . #\F12)
    (#\KP-0 . #\0)
    (#\KP-1 . #\1)
    (#\KP-2 . #\2)
    (#\KP-3 . #\3)
    (#\KP-4 . #\4)
    (#\KP-5 . #\5)
    (#\KP-6 . #\6)
    (#\KP-7 . #\7)
    (#\KP-8 . #\8)
    (#\KP-9 . #\9)
    (#\KP-Minus . #\-)
    (#\KP-Plus . #\+)
    (#\KP-Period . #\.)
    (#\KP-Multiply . #\*)))

(defun convert-scancode-to-key (scancode modifier-state)
  (let* ((caps (member :caps-lock modifier-state))
         (shift (member :shift modifier-state))
         (key (cdr (assoc scancode (if shift
                                       *keyboard-shifted-map*
                                       *keyboard-unshifted-map*)))))
    (if (and key (member :caps-lock modifier-state))
        (if shift
            (char-downcase key)
            (char-upcase key))
        key)))

(defmethod process-event ((event key-event))
  (let* ((scancode (key-scancode event))
         (modifier (assoc scancode *keyboard-modifiers*)))
    (cond ((and modifier (third modifier))
           ;; Locking modifier.
           (when (not (key-releasep event))
             (setf *keyboard-modifier-state*
                   (if (member (second modifier) *keyboard-modifier-state*)
                       (remove (second modifier) *keyboard-modifier-state*)
                       (list* (second modifier) *keyboard-modifier-state*)))))
          (modifier
           ;; Regular modifier.
           (setf *keyboard-modifier-state*
                 (if (key-releasep event)
                     (remove (second modifier) *keyboard-modifier-state*)
                     (list* (second modifier) *keyboard-modifier-state*))))
          (t ;; Normal key, try to translate it.
           (let ((translated (convert-scancode-to-key scancode *keyboard-modifier-state*)))
             (when (and translated *active-window*)
               (send-event *active-window*
                           (make-instance 'key-event
                                          :scancode (key-scancode event)
                                          :releasep (key-releasep event)
                                          :key translated
                                          :modifier-state (copy-list *keyboard-modifier-state*)))))))))

(defun submit-key (scancode releasep)
  "Submit a key event into the input system."
  (mezzanine.supervisor:fifo-push (make-instance 'key-event
                                                 :scancode scancode
                                                 :releasep releasep)
                                  *event-queue*))

;;;; Mouse events

(defclass mouse-event ()
  ((%button-state :initarg :button-state :reader mouse-button-state)
   (%button-change :initarg :button-change :reader mouse-button-change)
   (%x-position :initarg :x-position :reader mouse-x-position)
   (%y-position :initarg :y-position :reader mouse-y-position)
   (%x-motion :initarg :x-motion :reader mouse-x-motion)
   (%y-motion :initarg :y-motion :reader mouse-y-motion))
  (:default-initargs :button-state nil
                     :button-change 0
                     :x-position nil
                     :y-position nil
                     :x-motion 0
                     :y-motion 0))

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *mouse-buttons* 0)

(defmethod process-event ((event mouse-event))
  ;; Update positions and buttons
  (let* ((buttons (mouse-button-state event))
         (changes (logxor *mouse-buttons* buttons))
         (x-motion (mouse-x-motion event))
         (old-x *mouse-x*)
         (new-x (+ *mouse-x* x-motion))
         (y-motion (mouse-y-motion event))
         (old-y *mouse-y*)
         (new-y (+ *mouse-y* y-motion)))
    (multiple-value-bind (width height)
        (screen-dimensions)
      (setf *mouse-x* (clamp new-x 0 width)
            *mouse-y* (clamp new-y 0 height)))
    (setf *mouse-buttons* buttons)
    ;; Dispatch events and stuff...
    ;; Raise windows when clicked on, deal with drag, etc.
    #+(or)(do-something)
    (when (or (not (zerop x-motion))
              (not (zerop y-motion)))
      ;; Mouse position changed, redraw the screen.
      (expand-clip-rectangle old-x old-y (array-dimension *mouse-pointer* 1) (array-dimension *mouse-pointer* 0))
      (expand-clip-rectangle new-x new-y (array-dimension *mouse-pointer* 1) (array-dimension *mouse-pointer* 0)))))

(defun submit-mouse (buttons x-motion y-motion)
  "Submit a mouse event into the input system."
  (mezzanine.supervisor:fifo-push (make-instance 'mouse-event
                                                 :button-state buttons
                                                 :x-motion x-motion
                                                 :y-motion y-motion)
                                  *event-queue*))

(defun global-mouse-state ()
  "Fetch the current mouse state."
  (values *mouse-button-state* *mouse-x* *mouse-y*))

;;;; Window creation event.
;;;; From clients to the compositor.

(defclass window-create-event ()
  ((%window :initarg :window :reader window)))

(defmethod process-event ((event window-create-event))
  (let ((win (window event)))
    (format t "Registered new ~Dx~D window ~S, attached to FIFO ~S.~%"
            (width win) (height win) win (fifo win))
    (setf (window-x win) 0
          (window-y win) 0)
    (push win *window-list*)
    (when (not *active-window*)
      (setf *active-window* win)
      (send-event win (make-instance 'window-activation-event
                                     :window win
                                     :state t)))))

(defun make-window (fifo width height)
  (let ((window (make-instance 'window
                               :width width
                               :height height
                               :fifo fifo
                               :buffer (make-array (list height width)
                                                   :element-type '(unsigned-byte 32)
                                                   :initial-element 0))))
    (mezzanine.supervisor:fifo-push (make-instance 'window-create-event
                                                   :window window)
                                    *event-queue*)
    window))

;;;; Window close event.

(defclass window-close-event ()
  ((%window :initarg :window :reader window)))

(defmethod process-event ((event window-close-event))
  (format t "Closing window ~S. Goodbye!~%" (window event))
  (when (eql *active-window* (window event))
    (setf *active-window* nil))
  (setf *window-list* (remove (window event) *window-list*))
  (send-event (window event) event))

(defun close-window (window)
  (mezzanine.supervisor:fifo-push (make-instance 'window-close-event
                                                 :window window)
                                  *event-queue*))

;;;; Window activation changed.

(defclass window-activation-event ()
  ((%window :initarg :window :reader window)
   (%state :initarg :state :reader state)))

;;;; Window content updates.

(defclass damage-event ()
  ((%window :initarg :window :reader window)
   (%x :initarg :x :reader x)
   (%y :initarg :y :reader y)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

(defmethod process-event ((event damage-event))
  (let ((window (window event)))
    (multiple-value-bind (x y w h)
        (clip-rectangle (x event) (y event) (width event) (height event)
                        0 0 (width window) (height window))
      (expand-clip-rectangle (+ x (window-x window)) (+ y (window-y window))
                             w h))))

(defun damage-window (window x y width height)
  "Send a window damage event to the compositor."
  (mezzanine.supervisor:fifo-push (make-instance 'damage-event
                                                 :window window
                                                 :x x
                                                 :y y
                                                 :width width
                                                 :height height)
                                  *event-queue*))

;;;; Internal redisplay timer event.

(defclass redisplay-time-event () ())

(defmethod process-event ((event redisplay-time-event))
  (recompose-windows))

;;;; Other event stuff.

(defun send-event (window event)
  "Send an event to a window."
  (unless (mezzanine.supervisor:fifo-push event (fifo window) nil)
    (format t "Window ~S/FIFO ~S has stopped accepting events.~%"
            window (fifo window))))

;;;; Main body of the compositor.

(defun screen-dimensions ()
  (values (mezzanine.supervisor:framebuffer-width *main-screen*)
          (mezzanine.supervisor:framebuffer-height *main-screen*)))

(defun clip-rectangle (x y w h clip-x clip-y clip-w clip-h)
  "Clip the rectangle X,Y,W,H so it falls entirely within the clip rectangle."
  #+(or)(format t "Clipping ~D,~D ~D,~D to rect ~D,~D ~D,~D~%"
          x y w h clip-x clip-y clip-w clip-h)
  ;; Trim origin.
  (when (< x clip-x)
    (decf w (- clip-x x))
    (setf x clip-x))
  (when (< y clip-y)
    (decf h (- clip-y y))
    (setf y clip-y))
  ;; Trim size.
  (when (> (+ x w) (+ clip-x clip-w))
    (setf w (- (+ clip-x clip-w) x)))
  (when (> (+ y h) (+ clip-y clip-h))
    (setf h (- (+ clip-y clip-h) y)))
  (values x y (max w 0) (max h 0)))

(defun blit-with-clip (nrows ncols buffer row col)
  "Blit BUFFER to the screen backbuffer, obeying the global clip region."
  (multiple-value-bind (c-col c-row c-ncols c-nrows)
      (clip-rectangle col row ncols nrows
                      *clip-rect-x* *clip-rect-y*
                      *clip-rect-width* *clip-rect-height*)
    (when (and (not (zerop c-ncols))
               (not (zerop c-nrows)))
      #+(or)(format t "Blitting with clip. ~D,~D ~D,~D ~D,~D~%"
              c-nrows c-ncols (- c-row row) (- c-col col) c-row c-col)
      (bitblt-argb-xrgb c-nrows c-ncols
                        buffer (- c-row row) (- c-col col)
                        *screen-backbuffer* c-row c-col))))

(defun recompose-windows (&optional full)
  (when (and (not full)
             (or (zerop *clip-rect-width*)
                 (zerop *clip-rect-height*)))
    (return-from recompose-windows))
  ;; Draw windows back-to-front.
  (dolist (window (reverse *window-list*))
    (if full
        (bitblt-argb-xrgb (height window) (width window)
                          (window-buffer window) 0 0
                          *screen-backbuffer* (window-y window) (window-x window))
        (blit-with-clip (height window) (width window)
                        (window-buffer window)
                        (window-y window) (window-x window))))
  ;; Then the mouse pointer on top.
  (if full
      (bitblt-argb-xrgb (array-dimension *mouse-pointer* 0) (array-dimension *mouse-pointer* 1)
                        *mouse-pointer* 0 0
                        *screen-backbuffer* (- *mouse-y* *mouse-hot-y*) (- *mouse-x* *mouse-hot-x*))
      (blit-with-clip (array-dimension *mouse-pointer* 0) (array-dimension *mouse-pointer* 1)
                      *mouse-pointer*
                      (- *mouse-y* *mouse-hot-y*) (- *mouse-x* *mouse-hot-x*)))
  ;; Update the actual screen.
  (if full
      (mezzanine.supervisor:framebuffer-blit *main-screen*
                                             (mezzanine.supervisor:framebuffer-height *main-screen*)
                                             (mezzanine.supervisor:framebuffer-width *main-screen*)
                                             *screen-backbuffer* 0 0
                                             0 0)
      (mezzanine.supervisor:framebuffer-blit *main-screen*
                                             *clip-rect-height*
                                             *clip-rect-width*
                                             *screen-backbuffer* *clip-rect-y* *clip-rect-x*
                                             *clip-rect-y* *clip-rect-x*))
  ;; Reset clip region.
  (setf *clip-rect-x* 0
        *clip-rect-y* 0
        *clip-rect-width* 0
        *clip-rect-height* 0))

(defun compositor-thread ()
  (loop
     (when (not (eql *main-screen* (mezzanine.supervisor:current-framebuffer)))
       ;; Framebuffer changed. Rebuild the screen.
       (setf *main-screen* (mezzanine.supervisor:current-framebuffer)
             *screen-backbuffer* (make-array (list (mezzanine.supervisor:framebuffer-height *main-screen*)
                                                   (mezzanine.supervisor:framebuffer-width *main-screen*))
                                             :element-type '(unsigned-byte 32)))
       (recompose-windows t))
     (handler-case
         (process-event (mezzanine.supervisor:fifo-pop *event-queue*))
       (error (c)
         (ignore-errors
           (format t "~&Error ~A in compositor.~%" c))))))

(defun compositor-heartbeat-thread ()
  (loop
     (mezzanine.supervisor:wait-for-heartbeat)
     (when (and (not (zerop *clip-rect-width*))
                (not (zerop *clip-rect-height*)))
       (mezzanine.supervisor:fifo-push (make-instance 'redisplay-time-event)
                                       *event-queue*))))

(when *compositor*
  (format t "Restarting compositor thread.")
  (mezzanine.supervisor:destroy-thread *compositor*))
(setf *compositor* (mezzanine.supervisor:make-thread 'compositor-thread
                                                     :name "Compositor"))

(when *compositor-heartbeat*
  (format t "Restarting compositor heartbeat thread.")
  (mezzanine.supervisor:destroy-thread *compositor-heartbeat*))
(setf *compositor-heartbeat* (mezzanine.supervisor:make-thread 'compositor-heartbeat-thread
                                                               :name "Compositor Heartbeat"))
