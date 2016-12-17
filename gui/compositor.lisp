;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.gui.compositor)

(defvar *default-foreground-colour* (mezzano.gui:make-colour-from-octets #xDC #xDC #xCC))
(defvar *default-background-colour* (mezzano.gui:make-colour-from-octets #x3E #x3E #x3E #xD8))

(defun clamp (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defvar *compositor* nil "Compositor thread.")
(defvar *compositor-heartbeat* nil "Compositor heartbeat thread. Drives redisplay.")

(defvar *event-queue* (mezzano.supervisor:make-fifo 50)
  "Internal FIFO used to submit events to the compositor.")

(defclass window ()
  ((%x :initarg :x :accessor window-x)
   (%y :initarg :y :accessor window-y)
   (%thread :initarg :thread :accessor window-thread)
   (%fifo :initarg :fifo :reader fifo)
   (%buffer :initarg :buffer :reader window-buffer)
   (%layer :initarg :layer :reader layer)
   (%subscribed-notifications :initarg :notifications :reader subscribed-notifications)
   (%unresponsive :initarg :unresponsive :accessor window-unresponsive)
   (%kind :initarg :kind :reader kind))
  (:default-initargs :layer nil
                     :notifications '()
                     :unresponsive nil
                     :thread nil))

(defgeneric width (thing))
(defgeneric height (thing))

(defmethod width ((window window))
  (mezzano.gui:surface-width (window-buffer window)))

(defmethod height ((window window))
  (mezzano.gui:surface-height (window-buffer window)))

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
          *clip-rect-y* (min *clip-rect-y* y)
          *clip-rect-width* (- x2 *clip-rect-x*)
          *clip-rect-height* (- y2 *clip-rect-y*))
    #+debug-dirty-rects(format t "Expanded clip rect to ~D,~D ~D,~D~%"
            *clip-rect-x* *clip-rect-y* *clip-rect-width* *clip-rect-height*)))

(defun expand-clip-rectangle-by-window (window)
  (expand-clip-rectangle (window-x window) (window-y window)
                         (width window) (height window)))

(defun screen-to-window-coordinates (window x y)
  "Convert the screen-relative X,Y coordinates to window-relative coordinates."
  (values (- x (window-x window))
          (- y (window-y window))))

(defun window-to-screen-coordinates (window x y)
  "Convert the window-relative X,Y coordinates to screen-relative coordinates."
  (values (+ x (window-x window))
          (+ y (window-y window))))

(defun 2d-array (data &optional (element-type 't))
  (let* ((width (length (first data)))
         (height (length data)))
    (make-array (list height width)
                :element-type element-type
                :initial-contents data)))

(defvar *mouse-pointer*
  (mezzano.gui:make-surface-from-array
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
            '(unsigned-byte 32))))

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
(defparameter *keyboard-modifiers*
  '((#\Caps-Lock :caps-lock t)
    (#\Left-Shift :shift)
    (#\Right-Shift :shift)
    (#\Left-Control :control)
    (#\Right-Control :control)
    (#\Left-Meta :meta)
    (#\Right-Meta :shift2)
    (#\Left-Super :super)
    (#\Right-Super :super)
    (#\Left-Hyper :hyper)
    (#\Right-Hyper :hyper)))

(defvar *current-keymap* *engb-keymap*)

(defgeneric convert-scancode-to-key (keymap scancode modifier-state))

(defmethod convert-scancode-to-key ((keymap simple-keymap) scancode modifier-state)
  (let* ((caps (member :caps-lock modifier-state))
         (command-char (intersection '(:control :meta :super :hyper) modifier-state))
         (shift (member :shift modifier-state))
         (shift2 (member :shift2 modifier-state))
         (key (if (and command-char (untranslated-modifiers keymap))
                  (return-from convert-scancode-to-key scancode)
                  (cdr (assoc scancode (cond (shift2 (keymap-shifted2-map keymap))
                                             (shift (keymap-shifted-map keymap))
                                             (t (keymap-unshifted-map keymap))))))))
    (when (and key shift2 shift)
      (setf key (char-upcase key)))
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
           (let ((translated (convert-scancode-to-key *current-keymap* scancode *keyboard-modifier-state*)))
             (when translated
               (cond
                 ;; Global shortcuts.
                 ((and (member :meta *keyboard-modifier-state*)
                       (eql translated #\F12))
                  (when (key-releasep event)
                    ;; Switch keymaps.
                    (let ((index (1+ (or (position *current-keymap* *keymap-list*) -1))))
                      (when (>= index (length *keymap-list*))
                        (setf index 0))
                      (setf *current-keymap* (elt *keymap-list* index)))))
                 ((and (member :meta *keyboard-modifier-state*)
                       (eql translated #\Esc))
                  (when (and (key-releasep event)
                             *active-window*
                             (window-thread *active-window*))
                    ;; BREAK into the thread associated with the current window.
                    (mezzano.supervisor:establish-thread-foothold
                     (window-thread *active-window*)
                     (if (member :control *keyboard-modifier-state*)
                         (lambda () (cerror "Continue" "Keyboard break"))
                         (lambda () (break))))))
                 ((and (member :meta *keyboard-modifier-state*)
                       (eql translated #\F1))
                  (when (key-releasep event)
                    ;; Redraw the whole screen
                    (setf *clip-rect-width* (mezzano.supervisor:framebuffer-width *main-screen*)
                          *clip-rect-height* (mezzano.supervisor:framebuffer-height *main-screen*)
                          *clip-rect-x* 0
                          *clip-rect-y* 0)))
                 ;; Otherwise, dispatch to active window.
                 (*active-window*
                  (send-event *active-window*
                              (make-instance 'key-event
                                             :scancode (key-scancode event)
                                             :releasep (key-releasep event)
                                             :key translated
                                             :modifier-state (copy-list *keyboard-modifier-state*)))))))))))

(defun submit-key (scancode releasep)
  "Submit a key event into the input system."
  (mezzano.supervisor:fifo-push (make-instance 'key-event
                                               :scancode scancode
                                               :releasep releasep)
                                *event-queue*))

;;;; Mouse events

(defclass mouse-event ()
  ((%window :initarg :window :reader window)
   (%button-state :initarg :button-state :reader mouse-button-state)
   (%button-change :initarg :button-change :reader mouse-button-change)
   (%x-position :initarg :x-position :reader mouse-x-position)
   (%y-position :initarg :y-position :reader mouse-y-position)
   (%x-motion :initarg :x-motion :reader mouse-x-motion)
   (%y-motion :initarg :y-motion :reader mouse-y-motion))
  (:default-initargs :window nil
                     :button-state nil
                     :button-change 0
                     :x-position nil
                     :y-position nil
                     :x-motion 0
                     :y-motion 0))

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)
(defvar *mouse-buttons* 0)

(defvar *drag-window* nil)
(defvar *drag-x-origin* nil)
(defvar *drag-y-origin* nil)
(defvar *drag-x* nil)
(defvar *drag-y* nil)
(defvar *passive-drag* nil
  "Set to true when the drag was compositor-initiated.
A passive drag sends no drag events to the window.")

(defun activate-window (window)
  "Make WINDOW the active window."
  (when (not (eql window *active-window*))
    (when *active-window*
      (send-event *active-window* (make-instance 'window-activation-event
                                                 :window *active-window*
                                                 :state nil)))
    (setf *active-window* window)
    (send-event window (make-instance 'window-activation-event
                                      :window window
                                      :state t))
    (when (not (eql (layer window) :bottom))
      (setf *window-list* (remove window *window-list*))
      (push window *window-list*)
      ;; Layering change, redraw the whole window.
      (expand-clip-rectangle-by-window window))))

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
    (let ((win (window-at-point *mouse-x* *mouse-y*)))
      (when win
        (when (and (logbitp 0 buttons)
                   (logbitp 0 changes)
                   (member :meta *keyboard-modifier-state*)
                   (not (eql (layer win) :bottom)))
          ;; Meta + left down, start a passive drag.
          (setf *drag-x-origin* *mouse-x*
                *drag-y-origin* *mouse-y*
                *drag-window* win
                (values *drag-x* *drag-y*) (screen-to-window-coordinates win *mouse-x* *mouse-y*)
                *passive-drag* t)
          (activate-window win))
        (when (and (not (logbitp 0 buttons))
                   (logbitp 0 changes)
                   (not (eql win *active-window*)))
          ;; On left click release, select window.
          (activate-window win))
        (when (and win
                   (or (not (zerop x-motion))
                       (not (zerop y-motion))
                       (not (zerop changes))))
          (multiple-value-bind (win-x win-y)
              (screen-to-window-coordinates win *mouse-x* *mouse-y*)
            (send-event win (make-instance 'mouse-event
                                           :window win
                                           :button-state *mouse-buttons*
                                           :button-change changes
                                           :x-position win-x
                                           :y-position win-y
                                           :x-motion x-motion
                                           :y-motion y-motion))))))
    (when (or (not (zerop x-motion))
              (not (zerop y-motion)))
      (when *drag-window*
        (expand-clip-rectangle-by-window *drag-window*)
        ;; Use clipped mouse coordinates instead of relative motion.
        (setf (window-x *drag-window*) (- *mouse-x* *drag-x*))
        (setf (window-y *drag-window*) (- *mouse-y* *drag-y*))
        (expand-clip-rectangle-by-window *drag-window*))
      ;; Mouse position changed, redraw the screen.
      (expand-clip-rectangle old-x old-y (mezzano.gui:surface-width *mouse-pointer*) (mezzano.gui:surface-height *mouse-pointer*))
      (expand-clip-rectangle new-x new-y (mezzano.gui:surface-width *mouse-pointer*) (mezzano.gui:surface-height *mouse-pointer*)))
    (when (and (not (logbitp 0 buttons))
               (logbitp 0 changes)
               *drag-window*)
      ;; Left click release, stop drag.
      (setf *drag-window* nil))))

(defun submit-mouse (buttons x-motion y-motion)
  "Submit a mouse event into the input system."
  (mezzano.supervisor:fifo-push (make-instance 'mouse-event
                                               :button-state buttons
                                               :x-motion x-motion
                                               :y-motion y-motion)
                                *event-queue*))

(defun global-mouse-state ()
  "Fetch the current mouse state."
  (values *mouse-buttons* *mouse-x* *mouse-y*))

;;;; Window creation event.
;;;; From clients to the compositor.

(defclass window-create-event ()
  ((%window :initarg :window :reader window)
   (%initial-z-order :initarg :initial-z-order :reader initial-z-order)))

(defmethod process-event ((event window-create-event))
  (let ((win (window event)))
    (format t "Registered new ~Dx~D window ~S, attached to FIFO ~S.~%"
            (width win) (height win) win (fifo win))
    (setf (window-x win) 0
          (window-y win) 0)
    (case (layer win)
      (:bottom
       (setf *window-list* (append *window-list* (list win))))
      (:top
       (push win *window-list*))
      (t (setf (slot-value win '%layer) nil)
         (cond ((and (eql (initial-z-order event) :below-current)
                     *window-list*)
                (setf *window-list* (list* (first *window-list*)
                                           win
                                           (rest *window-list*))))
               (t (push win *window-list*)))))
    (when (and *active-window*
               (eql (initial-z-order event) :top))
      (send-event *active-window* (make-instance 'window-activation-event
                                                 :window *active-window*
                                                 :state nil)))
    (when (or (not *active-window*)
              (eql (initial-z-order event) :top))
      (setf *active-window* win)
      (send-event win (make-instance 'window-activation-event
                                     :window win
                                     :state t)))
    (setf *clip-rect-width* (mezzano.supervisor:framebuffer-width *main-screen*)
          *clip-rect-height* (mezzano.supervisor:framebuffer-height *main-screen*)
          *clip-rect-x* 0
          *clip-rect-y* 0)))

(defmacro with-window ((window fifo width height &rest options) &body body)
  `(let (,window)
     (unwind-protect
          (progn (setf ,window (mezzano.gui.compositor:make-window ,fifo ,width ,height ,@options))
                 ,@body)
       (when ,window
         (mezzano.gui.compositor:close-window ,window)))))

(defun make-window (fifo width height &key layer initial-z-order kind)
  (let ((window (make-instance 'window
                               :width width
                               :height height
                               :fifo fifo
                               :thread (mezzano.supervisor:current-thread)
                               :buffer (mezzano.gui:make-surface width height)
                               :layer layer
                               :kind kind)))
    (mezzano.supervisor:fifo-push (make-instance 'window-create-event
                                                 :window window
                                                 :initial-z-order (or initial-z-order :top))
                                  *event-queue*)
    window))

;;;; Window close event.

(defclass window-close-event ()
  ((%window :initarg :window :reader window)))

(defmethod process-event ((event window-close-event))
  (format t "Closing window ~S. Goodbye!~%" (window event))
  (setf *window-list* (remove (window event) *window-list*))
  (when (eql *active-window* (window event))
    (setf *active-window* (first *window-list*))
    (when *active-window*
      (send-event *active-window* (make-instance 'window-activation-event
                                                 :window *active-window*
                                                 :state t))))
  (setf *clip-rect-width* (mezzano.supervisor:framebuffer-width *main-screen*)
        *clip-rect-height* (mezzano.supervisor:framebuffer-height *main-screen*)
        *clip-rect-x* 0
        *clip-rect-y* 0)
  (send-event (window event) event))

(defun close-window (window)
  (mezzano.supervisor:fifo-push (make-instance 'window-close-event
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
    (multiple-value-bind (win-x win-y)
        (window-to-screen-coordinates window (x event) (y event))
      (multiple-value-bind (clip-x clip-y clip-w clip-h)
          (clip-rectangle win-x win-y (width event) (height event)
                          (window-x window) (window-y window) (width window) (height window))
        (expand-clip-rectangle clip-x clip-y clip-w clip-h)))))

(defun damage-window (window x y width height)
  "Send a window damage event to the compositor."
  (mezzano.supervisor:fifo-push (make-instance 'damage-event
                                               :window window
                                               :x x
                                               :y y
                                               :width width
                                               :height height)
                                *event-queue*))

;;;; Internal redisplay timer event.

(defclass redisplay-time-event ()
  ((%fullp :initarg :full :reader redisplay-time-event-fullp))
  (:default-initargs :full nil))

(defmethod process-event ((event redisplay-time-event))
  (recompose-windows (redisplay-time-event-fullp event)))

(defun force-redisplay (&optional full)
  (mezzano.supervisor:fifo-push (make-instance 'redisplay-time-event :full full)
                                *event-queue*))

;;;; Notifications.

(defclass subscribe-event ()
  ((%window :initarg :window :reader window)
   (%category :initarg :category :reader category)))

(defun subscribe-notification (window category)
  (mezzano.supervisor:fifo-push (make-instance 'subscribe-event
                                               :window window
                                               :category category)
                                *event-queue*))

(defmethod process-event ((event subscribe-event))
  (pushnew (category event) (slot-value (window event) '%subscribed-notifications))
  (case (category event)
    (:screen-geometry
     (send-event (window event) (make-instance 'screen-geometry-update
                                              :width (mezzano.supervisor:framebuffer-width *main-screen*)
                                              :height (mezzano.supervisor:framebuffer-height *main-screen*))))))

(defclass unsubscribe-event ()
  ((%window :initarg :window :reader window)
   (%category :initarg :category :reader category)))

(defun unsubscribe-notification (window category)
  (mezzano.supervisor:fifo-push (make-instance 'unsubscribe-event
                                               :window window
                                               :category category)
                                *event-queue*))

(defmethod process-event ((event unsubscribe-event))
  (setf (slot-value (window event) '%subscribed-notifications)
        (remove (category event) (subscribed-notifications (window event)))))

(defun broadcast-notification (category payload)
  (dolist (win *window-list*)
    (when (member category (subscribed-notifications win))
      ;; Maybe copy the payload?
      (send-event win payload))))

;;;; Screen geometry change notification.

(defclass screen-geometry-update ()
  ((%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

;;;; Other event stuff.

(defun send-event (window event)
  "Send an event to a window."
  (cond ((mezzano.supervisor:fifo-push event (fifo window) nil)
         (when (window-unresponsive window)
           (format t "Window ~S/FIFO ~S is accepting events again.~%"
                   window (with-output-to-string (s)
                            (print-unreadable-object ((fifo window) s :type t :identity t))))
           (setf *clip-rect-width* (mezzano.supervisor:framebuffer-width *main-screen*)
                 *clip-rect-height* (mezzano.supervisor:framebuffer-height *main-screen*)
                 *clip-rect-x* 0
                 *clip-rect-y* 0)
           (setf (window-unresponsive window) nil)))
        ((not (window-unresponsive window))
         (setf (window-unresponsive window) t)
         (format t "Window ~S/FIFO ~A has stopped accepting events.~%"
                 window (with-output-to-string (s)
                          (print-unreadable-object ((fifo window) s :type t :identity t))))
         (setf *clip-rect-width* (mezzano.supervisor:framebuffer-width *main-screen*)
               *clip-rect-height* (mezzano.supervisor:framebuffer-height *main-screen*)
               *clip-rect-x* 0
               *clip-rect-y* 0))))

;; FIXME: This really shouldn't be synchronous.
(defun get-window-by-kind (kind)
  (dolist (win *window-list*)
    (when (eql (kind win) kind)
      (return win))))

;;;; Main body of the compositor.

(defun screen-dimensions ()
  (values (mezzano.supervisor:framebuffer-width *main-screen*)
          (mezzano.supervisor:framebuffer-height *main-screen*)))

(defun window-at-point (x y)
  (dolist (win *window-list*)
    (when (and (>= x (window-x win))
               (< x (+ (window-x win) (width win)))
               (>= y (window-y win))
               (< y (+ (window-y win) (height win))))
      (return win))))

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

(defun blit-with-clip (width height buffer x y)
  "Blit BUFFER to the screen backbuffer, obeying the global clip region."
  (multiple-value-bind (c-x c-y c-width c-height)
      (clip-rectangle x y width height
                      *clip-rect-x* *clip-rect-y*
                      *clip-rect-width* *clip-rect-height*)
    (when (and (not (zerop c-width))
               (not (zerop c-height)))
      #+(or)(format t "Blitting with clip. ~D,~D ~D,~D ~D,~D~%"
              c-width c-height (- c-x x) (- c-y y) x y)
      (bitblt :blend
              c-width c-height
              buffer (- c-x x) (- c-y y)
              *screen-backbuffer* c-x c-y))))

(defun fill-with-clip (width height colour x y)
  "Fill a rectangle with COLOUR on the screen backbuffer, obeying the global clip region."
  (multiple-value-bind (c-x c-y c-width c-height)
      (clip-rectangle x y width height
                      *clip-rect-x* *clip-rect-y*
                      *clip-rect-width* *clip-rect-height*)
    (when (and (not (zerop c-width))
               (not (zerop c-height)))
      #+(or)(format t "Blitting with clip. ~D,~D ~D,~D ~D,~D~%"
              c-width c-height (- c-x x) (- c-y y) x y)
      (bitset :blend
              c-width c-height
              colour
              *screen-backbuffer* c-x c-y))))

(defun recompose-windows (&optional full)
  (when full
    ;; Expand the cliprect to the entire screen.
    (setf *clip-rect-width* (mezzano.supervisor:framebuffer-width *main-screen*)
          *clip-rect-height* (mezzano.supervisor:framebuffer-height *main-screen*)
          *clip-rect-x* 0
          *clip-rect-y* 0))
  (when (or (zerop *clip-rect-width*)
            (zerop *clip-rect-height*))
    (return-from recompose-windows))
  ;; Draw windows back-to-front.
  (dolist (window (reverse *window-list*))
    (blit-with-clip (width window) (height window)
                    (window-buffer window)
                    (window-x window) (window-y window))
    (when (window-unresponsive window)
      (fill-with-clip (width window) (height window)
                      (mezzano.gui:make-colour 0 0 0 0.5)
                      (window-x window) (window-y window))))
  ;; Then the mouse pointer on top.
  (blit-with-clip (surface-width *mouse-pointer*) (surface-height *mouse-pointer*)
                  *mouse-pointer*
                  (- *mouse-x* *mouse-hot-x*) (- *mouse-y* *mouse-hot-y*))
  ;; Update the actual screen.
  (mezzano.supervisor:framebuffer-blit *main-screen*
                                       *clip-rect-height*
                                       *clip-rect-width*
                                       (mezzano.gui::surface-pixels *screen-backbuffer*)
                                       *clip-rect-y* *clip-rect-x*
                                       *clip-rect-y* *clip-rect-x*)
  ;; Reset clip region.
  (setf *clip-rect-x* 0
        *clip-rect-y* 0
        *clip-rect-width* 0
        *clip-rect-height* 0))

(defun compositor-thread ()
  (loop
     (when (not (eql *main-screen* (mezzano.supervisor:current-framebuffer)))
       ;; Framebuffer changed. Rebuild the screen.
       (setf *main-screen* (mezzano.supervisor:current-framebuffer)
             *screen-backbuffer* (mezzano.gui:make-surface
                                  (mezzano.supervisor:framebuffer-width *main-screen*)
                                  (mezzano.supervisor:framebuffer-height *main-screen*)))
       (recompose-windows t)
       (broadcast-notification :screen-geometry
                               (make-instance 'screen-geometry-update
                                              :width (mezzano.supervisor:framebuffer-width *main-screen*)
                                              :height (mezzano.supervisor:framebuffer-height *main-screen*))))
     (sys.int::log-and-ignore-errors
       (process-event (mezzano.supervisor:fifo-pop *event-queue*)))))

(defvar *compositor-update-interval* 1/60)

(defun compositor-heartbeat-thread ()
  (loop
     (sleep *compositor-update-interval*)
     ;; Redisplay only when the system framebuffer changes or when there's
     ;; nonempty clip rect.
     (when (or (not (eql *main-screen* (mezzano.supervisor:current-framebuffer)))
               (and (not (zerop *clip-rect-width*))
                    (not (zerop *clip-rect-height*))))
       (mezzano.supervisor:fifo-push (make-instance 'redisplay-time-event)
                                     *event-queue*))))

(when (not *compositor*)
  (setf *compositor* (mezzano.supervisor:make-thread 'compositor-thread
                                                     :name "Compositor")))

(when (not *compositor-heartbeat*)
  (setf *compositor-heartbeat* (mezzano.supervisor:make-thread 'compositor-heartbeat-thread
                                                               :name "Compositor Heartbeat")))
