;;;; Display compositor and window manager.

(in-package :mezzano.gui.compositor)

(defparameter *enable-live-resize* nil
  "When true, windows will continously redraw their contents when resizing.")

(defun clamp (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defvar *compositor* nil "Compositor thread.")
(defvar *compositor-debug-enable* nil)

(defvar *event-queue* (mezzano.sync:make-mailbox :name 'compositor-event-queue)
  "Internal mailbox used to submit events to the compositor.")

(defun submit-compositor-event (event)
  (mezzano.sync:mailbox-send event *event-queue*))

(defclass window ()
  ((%name :initarg :name :accessor name)
   (%x :initarg :x :accessor window-x)
   (%y :initarg :y :accessor window-y)
   (%thread :initarg :thread :accessor window-thread)
   (%mailbox :initarg :mailbox :reader mailbox)
   (%buffer :initarg :buffer :reader window-buffer)
   (%layer :initarg :layer :reader layer)
   (%subscribed-notifications :initarg :notifications :reader subscribed-notifications)
   (%unresponsive :initarg :unresponsive :accessor window-unresponsive)
   (%kind :initarg :kind :reader kind)
   (%cursor :initarg :cursor :accessor cursor)
   (%grabp :initarg :grabp :accessor grabp)
   (%grab-x1 :initarg :grab-x1 :accessor grab-x1)
   (%grab-y1 :initarg :grab-y1 :accessor grab-y1)
   (%grab-x2 :initarg :grab-x2 :accessor grab-x2)
   (%grab-y2 :initarg :grab-y2 :accessor grab-y2))
  (:default-initargs
    :name nil
    :layer nil
    :notifications '()
    :unresponsive nil
    :thread nil
    :cursor :default
    :grabp nil))

(defmethod print-object ((instance window) stream)
  (let ((name (name instance)))
    (if name
        (print-unreadable-object (instance stream :type t :identity t)
          (format stream "~S" name))
        (print-unreadable-object (instance stream :type t :identity t)))))

(defclass event ()
  ((%window :initarg :window :reader window))
  (:default-initargs :window nil))

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

(defparameter *idle-time* 0)

(defun reset-idle-time ()
  (setf *idle-time* (get-internal-run-time)))

(defun screensaver-running-p ()
  (get-window-by-kind :screensaver))

(defun damage-whole-screen ()
  (setf *clip-rect-width* (mezzano.supervisor:framebuffer-width *main-screen*)
        *clip-rect-height* (mezzano.supervisor:framebuffer-height *main-screen*)
        *clip-rect-x* 0
        *clip-rect-y* 0))

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

(defclass mouse-cursor ()
  ((%name :initarg :name :accessor name)
   (%surface :initarg :surface :reader mouse-cursor-surface)
   (%hot-x :initarg :hot-x :reader mouse-cursor-hot-x)
   (%hot-y :initarg :hot-y :reader mouse-cursor-hot-y))
  (:default-initargs :name nil))

(defmethod print-object ((instance mouse-cursor) stream)
  (let ((name (name instance)))
    (if name
        (print-unreadable-object (instance stream :type t :identity t)
          (format stream "~S" name))
        (print-unreadable-object (instance stream :type t :identity t)))))

(defvar *mouse-cursor-library* (make-hash-table :synchronized t))

(defun make-mouse-cursor (surface &key name (hot-x 0) (hot-y 0))
  (assert (eql (mezzano.gui:surface-format surface) :argb32))
  (make-instance 'mouse-cursor
                 :name name
                 :surface surface
                 :hot-x hot-x
                 :hot-y hot-y))

(defun register-mouse-cursor (cursor name)
  (when (not (name cursor)) (setf (name cursor) name))
  (setf (gethash name *mouse-cursor-library*) cursor))

(defvar *default-mouse-pointer-surface*
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

(defvar *default-mouse-pointer*
  (make-instance 'mouse-cursor
                 :name :default
                 :surface *default-mouse-pointer-surface*
                 :hot-x 0
                 :hot-y 0))

(defvar *none-mouse-pointer-surface*
  (mezzano.gui:make-surface-from-array
   (2d-array '(())
             '(unsigned-byte 32))))

(defvar *none-mouse-pointer*
  (make-instance 'mouse-cursor
                 :name :none
                 :surface *none-mouse-pointer-surface*
                 :hot-x 0
                 :hot-y 0))

(defvar *mouse-pointer* *default-mouse-pointer*)

(defgeneric process-event (event))

;;;; Keyboard events, including translation from HID scancode.

(defclass key-event (event)
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

(defvar *m-tab-active* nil)
(defvar *m-tab-list* nil)

(defun allow-m-tab (window)
  (not (and (zerop (width window))
            (zerop (height window)))))

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
    (if (and key caps)
        (if shift
            (char-downcase key)
            (char-upcase key))
        key)))

(defun command-next-keymap ()
  ;; Switch keymaps.
  (let ((index (1+ (or (position *current-keymap* *keymap-list*) -1))))
    (when (>= index (length *keymap-list*))
      (setf index 0))
    (setf *current-keymap* (elt *keymap-list* index))))

(defun command-prev-keymap ()
  ;; Switch keymaps.
  (let ((index (1- (or (position *current-keymap* *keymap-list*)
                       (length *keymap-list*)))))
    (when (< index 0)
      (setf index (1- (length *keymap-list*))))
    (setf *current-keymap* (elt *keymap-list* index))))

(defun command-open-fancy-repl ()
  (eval (read-from-string "(mezzano.gui.fancy-repl:spawn)")))

(defun command-open-basic-repl ()
  (eval (read-from-string "(mezzano.gui.basic-repl:spawn)")))

(defun command-interrupt-active-window ()
  (when (and *active-window*
             (window-thread *active-window*))
    ;; BREAK into the thread associated with the current window.
    (mezzano.supervisor:establish-thread-foothold
     (window-thread *active-window*)
     (lambda () (break "Keyboard break")))))

(defun command-interrupt-active-window-cerror ()
  (when (and *active-window*
             (window-thread *active-window*))
    (mezzano.supervisor:establish-thread-foothold
     (window-thread *active-window*)
     (lambda () (cerror "Continue" "Keyboard break")))))

(defun command-quit-active-window ()
  (when *active-window*
    ;; Send a quit request.
    (send-event *active-window*
                (make-instance 'quit-event :window *active-window*))))

(defun command-force-quit-active-window ()
  (when *active-window*
    ;; Zap the window.
    (process-event (make-instance 'window-close-event
                                  :window *active-window*))))

(defparameter *global-shortcuts*
  '((#\M-F12 command-next-keymap)
    (#\C-M-F12 command-prev-keymap)
    (#\M-F10 command-open-fancy-repl)
    (#\C-M-F10 command-open-basic-repl)
    (#\M-Esc command-interrupt-active-window)
    (#\C-M-Esc command-interrupt-active-window-cerror)
    (#\M-F1 damage-whole-screen)
    (#\M-F4 command-quit-active-window)
    (#\C-M-F4 command-force-quit-active-window)))

(defparameter *shift-meta-is-ctrl-meta* t
  "When running in a VM under Linux, ctrl + alt + a function key will switch VTs.
This treats alt + shift as an alternative to ctrl + alt.")

(defun apply-key-modifiers (key)
  (if (intersection '(:control :meta :super :hyper) *keyboard-modifier-state*)
      ;; Force character to uppercase when a modifier key is active, gets
      ;; around weirdness in how character names are processed.
      ;; #\C-a and #\C-A both parse as the same character (C-LATIN_CAPITAL_LETTER_A).
      (mezzano.internals::make-character
       (char-code (char-upcase key))
       :control (or (find :control *keyboard-modifier-state*)
                    (and *shift-meta-is-ctrl-meta*
                         (find :meta *keyboard-modifier-state*)
                         (find :shift *keyboard-modifier-state*)))
       :meta (find :meta *keyboard-modifier-state*)
       :super (find :super *keyboard-modifier-state*)
       :hyper (find :hyper *keyboard-modifier-state*))
      key))

(defmethod process-event ((event key-event))
  (reset-idle-time)
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
           (cond ((key-releasep event)
                  (setf *keyboard-modifier-state*
                        (remove (second modifier) *keyboard-modifier-state*)))
                 (t
                  (pushnew (second modifier) *keyboard-modifier-state*)))
           (when (and (key-releasep event)
                      (eql (second modifier) :meta))
             (setf *m-tab-active* nil
                   *m-tab-list* '())))
          (t ;; Normal key, try to translate it.
           (let ((translated (or (key-key event)
                                 (and scancode
                                      (convert-scancode-to-key *current-keymap* scancode *keyboard-modifier-state*)))))
             (when translated
               (let* ((modified-key (apply-key-modifiers translated))
                      (shortcut (second (assoc modified-key *global-shortcuts*))))
                 (cond
                   (shortcut
                    (when (key-releasep event)
                      (mezzano.internals::log-and-ignore-errors
                        (funcall shortcut))))
                   ;; ALt-tab handling.
                   ((and (member :meta *keyboard-modifier-state*)
                         (eql translated #\Tab))
                    (when (not (key-releasep event))
                      (when (not *m-tab-active*)
                        (setf *m-tab-active* t
                              *m-tab-list* (remove-if-not #'allow-m-tab (copy-list *window-list*))))
                      (let* ((p (position *active-window* *m-tab-list*))
                             (n-windows (length *m-tab-list*))
                             (next (if p
                                       (elt *m-tab-list* (mod (+ p
                                                                 (if (member :shift *keyboard-modifier-state*)
                                                                     -1
                                                                     +1))
                                                              n-windows))
                                       (first *m-tab-list*))))
                        (activate-window next))))
                   ;; Otherwise, dispatch to active window.
                   (*active-window*
                    (send-event *active-window*
                                (make-instance 'key-event
                                               :window *active-window*
                                               :scancode (key-scancode event)
                                               :releasep (key-releasep event)
                                               :key translated
                                               :modifier-state (copy-list *keyboard-modifier-state*))))))))))))

(defun submit-key (scancode releasep &key key)
  "Submit a key event into the input system."
  (submit-compositor-event (make-instance 'key-event
                                          :scancode scancode
                                          :releasep releasep
                                          :key key)))

;;;; Mouse events

(defclass mouse-event (event)
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

(defvar *drag-window* nil)
(defvar *drag-x-origin* nil)
(defvar *drag-y-origin* nil)
(defvar *drag-x* nil)
(defvar *drag-y* nil)
(defvar *passive-drag* nil
  "Set to true when the drag was compositor-initiated.
A passive drag sends no drag events to the window.")
(defvar *resize-origin* nil)

(defvar *previous-mouse-window* nil
  "Window the mouse was over when the last mouse event was sent.
Mouse events are sent to both the current mouse window and the previous window
so that windows can notice when they lose their mouse visibility.")

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

(defun compute-resized-window-geometry ()
  (let* ((win *drag-window*)
         (old-width (width win))
         (old-height (height win)))
    (ecase *resize-origin*
      (:top-left
       (values :bottom-right
               (max 1 (+ old-width (- *drag-x-origin* *mouse-x*)))
               (max 1 (+ old-height (- *drag-y-origin* *mouse-y*)))))
      (:top-right
       (values :bottom-left
               (max 1 (- old-width (- *drag-x-origin* *mouse-x*)))
               (max 1 (+ old-height (- *drag-y-origin* *mouse-y*)))))
      (:bottom-right
       (values :top-left
               (max 1 (- old-width (- *drag-x-origin* *mouse-x*)))
               (max 1 (- old-height (- *drag-y-origin* *mouse-y*)))))
      (:bottom-left
       (values :top-right
               (max 1 (+ old-width (- *drag-x-origin* *mouse-x*)))
               (max 1 (- old-height (- *drag-y-origin* *mouse-y*)))))
      (:left
       (values :bottom-right
               (max 1 (+ old-width (- *drag-x-origin* *mouse-x*)))
               old-height))
      (:right
       (values :top-left
               (max 1 (- old-width (- *drag-x-origin* *mouse-x*)))
                old-height))
      (:top
       (values :bottom-right
               old-width
               (max 1 (+ old-height (- *drag-y-origin* *mouse-y*)))))
      (:bottom
       (values :top-left
               old-width
               (max 1 (- old-height (- *drag-y-origin* *mouse-y*))))))))

(defmethod process-event ((event mouse-event))
  (reset-idle-time)
  ;; Update positions and buttons
  (let* ((buttons (or (mouse-button-state event)
                      *mouse-buttons*))
         (changes (logxor *mouse-buttons* buttons))
         (old-x *mouse-x*)
         (x-motion (or (and (mouse-x-position event)
                            (- (mouse-x-position event) old-x))
                       (mouse-x-motion event)))
         (new-x (+ *mouse-x* x-motion))
         (old-y *mouse-y*)
         (y-motion (or (and (mouse-y-position event)
                            (- (mouse-y-position event) old-y))
                       (mouse-y-motion event)))
         (new-y (+ *mouse-y* y-motion)))
    (when (and *active-window*
               (grabp *active-window*))
      (multiple-value-bind (x1 y1)
          (window-to-screen-coordinates *active-window* (grab-x1 *active-window*) (grab-y1 *active-window*))
        (multiple-value-bind (x2 y2)
            (window-to-screen-coordinates *active-window* (grab-x2 *active-window*) (grab-y2 *active-window*))
          (setf new-x (clamp new-x x1 x2)
                new-y (clamp new-y y1 y2)))))
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
                *passive-drag* t
                *resize-origin* nil)
          (activate-window win))
        (when (and (logbitp 0 buttons)
                   (logbitp 0 changes)
                   (not (eql win *active-window*)))
          ;; On left click press, select window.
          (activate-window win))
        (when (and (not (logbitp 0 buttons))
                   (logbitp 0 changes)
                   (not (eql win *active-window*))
                   (not *drag-window*))
          ;; On left click release, select window, unless dragging.
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
                                           :y-motion y-motion)))
          (when (not (eql win *previous-mouse-window*))
            (when *previous-mouse-window*
              (multiple-value-bind (win-x win-y)
                  (screen-to-window-coordinates *previous-mouse-window* *mouse-x* *mouse-y*)
                (send-event *previous-mouse-window*
                            (make-instance 'mouse-event
                                           :window *previous-mouse-window*
                                           :button-state *mouse-buttons*
                                           :button-change changes
                                           :x-position win-x
                                           :y-position win-y
                                           :x-motion x-motion
                                           :y-motion y-motion))))
            (setf *previous-mouse-window* win)))))
    (when (or (not (zerop x-motion))
              (not (zerop y-motion)))
      (when *drag-window*
        (cond (*resize-origin*
               (when *enable-live-resize*
                 (multiple-value-bind (origin new-width new-height)
                     (compute-resized-window-geometry)
                   (send-event *drag-window* (make-instance 'resize-request-event
                                                            :window *drag-window*
                                                            :origin origin
                                                            :width new-width
                                                            :height new-height)))))
              (t
               (expand-clip-rectangle-by-window *drag-window*)
               ;; Use clipped mouse coordinates instead of relative motion.
               (setf (window-x *drag-window*) (- *mouse-x* *drag-x*))
               (setf (window-y *drag-window*) (- *mouse-y* *drag-y*))
               (expand-clip-rectangle-by-window *drag-window*))))
      ;; Mouse position changed, redraw the screen.
      ;; Make sure to damage the old cursor before changing the cursor style,
      ;; otherwise it'll use the shape of the new cursor and potentially miss bits.
      (damage-mouse-region old-x old-y)
      (update-mouse-cursor)
      (damage-mouse-region new-x new-y))
    (when (and (not (logbitp 0 buttons))
               (logbitp 0 changes)
               *drag-window*)
      ;; Left click release, stop drag and finish resize.
      (when (and *resize-origin*
                 (not *enable-live-resize*))
        ;; Issue the real resize request now.
        (multiple-value-bind (origin new-width new-height)
            (compute-resized-window-geometry)
          (send-event *drag-window* (make-instance 'resize-request-event
                                                   :window *drag-window*
                                                   :origin origin
                                                   :width new-width
                                                   :height new-height)))
        (expand-clip-rectangle-by-window *drag-window*)
        (setf *resize-origin* nil))
      (setf *drag-window* nil))))

(defun submit-mouse (buttons x-motion y-motion)
  "Submit a mouse event into the input system."
  (submit-compositor-event (make-instance 'mouse-event
                                          :button-state buttons
                                          :x-motion x-motion
                                          :y-motion y-motion)))

(defun submit-mouse-absolute (x-position y-position &key buttons)
  "Submit a mouse event into the input system."
  (submit-compositor-event (make-instance 'mouse-event
                                          :button-state buttons
                                          :x-position x-position
                                          :y-position y-position)))

(defun global-mouse-state ()
  "Fetch the current mouse state."
  (values *mouse-buttons* *mouse-x* *mouse-y*))

(defvar *prev-resize-rect-x* nil)
(defvar *prev-resize-rect-y* nil)
(defvar *prev-resize-rect-w* nil)
(defvar *prev-resize-rect-h* nil)

(defun damage-mouse-region (x y)
  (let* ((cursor-surface (mouse-cursor-surface *mouse-pointer*))
         (hot-x (mouse-cursor-hot-x *mouse-pointer*))
         (hot-y (mouse-cursor-hot-y *mouse-pointer*))
         (cursor-width (mezzano.gui:surface-width cursor-surface))
         (cursor-height (mezzano.gui:surface-height cursor-surface)))
    (expand-clip-rectangle (- x hot-x) (- y hot-y) cursor-width cursor-height)))

(defun update-mouse-cursor ()
  ;; Get cursor style under mouse.
  (let* ((mwin (window-at-point *mouse-x* *mouse-y*))
         (style (if mwin
                    (or (cursor mwin) :default)
                    :default)))
    (when (eql style :default)
      (setf style *default-mouse-pointer*))
    (when (eql style :none)
      (setf style *none-mouse-pointer*))
    (when (not (eql style *mouse-pointer*))
      (damage-mouse-region *mouse-x* *mouse-y*)
      (setf *mouse-pointer* style)
      (damage-mouse-region *mouse-x* *mouse-y*)))
  (cond ((and (not *enable-live-resize*)
              (resize-in-progress-p))
         ;; Active resize, redraw the entire resize rect.
         (when *prev-resize-rect-x*
           ;; Zap the old one.
           (expand-clip-rectangle *prev-resize-rect-x*
                                  *prev-resize-rect-y*
                                  *prev-resize-rect-w*
                                  *prev-resize-rect-h*))
         (multiple-value-bind (x y w h)
             (resize-rect-geom)
           (expand-clip-rectangle x y w h)
           (setf *prev-resize-rect-x* x
                 *prev-resize-rect-y* y
                 *prev-resize-rect-w* w
                 *prev-resize-rect-h* h)))
        (*prev-resize-rect-x*
         ;; Resize completed, but there's still a rect on-screen.
         ;; Zap it.
         (expand-clip-rectangle *prev-resize-rect-x*
                                *prev-resize-rect-y*
                                *prev-resize-rect-w*
                                *prev-resize-rect-h*)
           (setf *prev-resize-rect-x* nil
                 *prev-resize-rect-y* nil
                 *prev-resize-rect-w* nil
                 *prev-resize-rect-h* nil))))

;;;; Window creation event.
;;;; From clients to the compositor.

(defclass window-create-event (event)
  ((%initial-z-order :initarg :initial-z-order :reader initial-z-order)))

(defmethod process-event ((event window-create-event))
  (let ((win (window event)))
    (when *compositor-debug-enable*
      (format t "Registered new ~Dx~D window ~S.~%"
              (width win) (height win) win))
    (setf (window-x win) 0
          (window-y win) 0)
    (case (layer win)
      (:bottom
       (setf *window-list* (append *window-list* (list win))))
      (:top
       (push win *window-list*))
      (t
       (setf (slot-value win '%layer) nil)
       (cond ((and (eql (initial-z-order event) :below-current)
                   *window-list*
                   (not (eql (layer (first *window-list*)) :bottom)))
              (setf *window-list* (list* (first *window-list*)
                                         win
                                         (rest *window-list*))))
             (t (push win *window-list*)))
       (setf (window-x win) (- (truncate (mezzano.supervisor:framebuffer-width *main-screen*) 2)
                               (truncate (width win) 2))
             (window-y win) (- (truncate (mezzano.supervisor:framebuffer-height *main-screen*) 2)
                               (truncate (height win) 2)))))
    ;; Send the event back to the window to indicate that the window has
    ;; been created.
    (send-event win event)
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
    (when (and *m-tab-active*
               (allow-m-tab win))
      (push win *m-tab-list*))
    (expand-clip-rectangle-by-window win)))

(defmacro with-window ((window mailbox width height &rest options) &body body)
  `(let (,window)
     (unwind-protect
          (progn (setf ,window (mezzano.gui.compositor:make-window ,mailbox ,width ,height ,@options))
                 ,@body)
       (when ,window
         (mezzano.gui.compositor:close-window ,window)))))

(defun make-window (mailbox width height &key name layer initial-z-order kind)
  (let ((window (make-instance 'window
                               :name name
                               :mailbox mailbox
                               :thread (mezzano.supervisor:current-thread)
                               :buffer (mezzano.gui:make-surface width height)
                               :layer layer
                               :kind kind)))
    (submit-compositor-event (make-instance 'window-create-event
                                            :window window
                                            :initial-z-order (or initial-z-order :top)))
    window))

;;;; Window close event.

(defclass window-close-event (event)
  ())

(defmethod process-event ((event window-close-event))
  (let ((win (window event))
        (old-mwin (window-at-point *mouse-x* *mouse-y*)))
    (when *compositor-debug-enable*
      (format t "Closing window ~S. Goodbye!~%" win))
    (setf *window-list* (remove win *window-list*))
    (when (eql *drag-window* win)
      (setf *drag-window* nil))
    (when (eql *active-window* win)
      (setf *active-window* (first *window-list*))
      (when *active-window*
        (send-event *active-window* (make-instance 'window-activation-event
                                                   :window *active-window*
                                                   :state t))))
    (expand-clip-rectangle-by-window win)
    (setf *m-tab-list* (remove win *window-list*))
    (send-event win event)
    (let ((new-mwin (window-at-point *mouse-x* *mouse-y*)))
      (when (not (eql old-mwin new-mwin))
        ;; Mouse is now directly over a different window.
        (multiple-value-bind (win-x win-y)
            (screen-to-window-coordinates new-mwin *mouse-x* *mouse-y*)
          (send-event new-mwin (make-instance 'mouse-event
                                              :window win
                                              :button-state *mouse-buttons*
                                              :button-change 0
                                              :x-position win-x
                                              :y-position win-y
                                              :x-motion 0
                                              :y-motion 0)))))))

(defun close-window (window)
  (submit-compositor-event (make-instance 'window-close-event
                                          :window window)))

;;;; Window activation changed.

(defclass window-activation-event (event)
  ((%state :initarg :state :reader state)))

;;;; Window content updates.

(defclass damage-event (event)
  ((%x :initarg :x :reader x)
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
  (submit-compositor-event (make-instance 'damage-event
                                          :window window
                                          :x x
                                          :y y
                                          :width width
                                          :height height)))

;;;; Window dragging.

(defclass begin-drag-event (event)
  ((%mode :initarg :mode :reader mode)))

(defmethod process-event ((event begin-drag-event))
  (let ((window (window event))
        (mode (mode event)))
    (when (and (eql window *active-window*)
               (not *drag-window*)
               (member mode '(:move
                              :top-left
                              :top-right
                              :bottom-right
                              :bottom-left
                              :left
                              :right
                              :top
                              :bottom)))
      (setf *drag-x-origin* *mouse-x*
            *drag-y-origin* *mouse-y*
            *drag-window* window
            (values *drag-x* *drag-y*) (screen-to-window-coordinates window *mouse-x* *mouse-y*)
            *passive-drag* nil
            *resize-origin* (if (eql mode :move)
                                nil
                                mode))
      (unless (eql mode :move)
        (expand-clip-rectangle-by-window window)))))

(defun begin-window-drag (window &key (mode :move))
  "Send a begin drag event to the compositor."
  (submit-compositor-event (make-instance 'begin-drag-event
                                          :window window
                                          :mode mode)))

;;;; Window resizing.

(defclass resize-request-event (event)
  ((%origin :initarg :origin :reader resize-origin)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

(defclass resize-event (event)
  ((%origin :initarg :origin :reader resize-origin)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)
   (%new-fb :initarg :new-fb :reader resize-new-fb)))

(defun updated-window-origin-for-resize (window origin new-width new-height)
  (let ((delta-w (- (width window) new-width))
        (delta-h (- (height window) new-height)))
    (ecase origin
      (:bottom-right
       (values (+ (window-x window) delta-w)
               (+ (window-y window) delta-h)))
      (:top-right
       (values (+ (window-x window) delta-w)
               (window-y window)))
      (:bottom-left
       (values (window-x window)
               (+ (window-y window) delta-h)))
      (:top-left
       (values (window-x window)
               (window-y window)))
      (:midpoint
       (values (+ (window-x window) (truncate delta-w 2))
               (+ (window-y window) (truncate delta-h 2)))))))

(defmethod process-event ((event resize-event))
  (let* ((window (window event))
         (origin (resize-origin event))
         (old-framebuffer (window-buffer window))
         (new-framebuffer (resize-new-fb event))
         (delta-w (- (mezzano.gui:surface-width old-framebuffer)
                     (mezzano.gui:surface-width new-framebuffer)))
         (delta-h (- (mezzano.gui:surface-height old-framebuffer)
                     (mezzano.gui:surface-height new-framebuffer))))
    (assert (eql (mezzano.gui:surface-format new-framebuffer) :argb32))
    ;; Window size and position is going to change.
    (expand-clip-rectangle-by-window window)
    ;; Move window and update drag variables based on origin.
    (when (eql *drag-window* window)
      (case origin
        (:top-left
         (decf *drag-x-origin* delta-w)
         (decf *drag-y-origin* delta-h))
        (:bottom-left
         (decf *drag-x-origin* delta-w)
         (incf *drag-y-origin* delta-h))
        (:bottom-right
         (incf *drag-x-origin* delta-w)
         (incf *drag-y-origin* delta-h))
        (:top-right
         (incf *drag-x-origin* delta-w)
         (decf *drag-y-origin* delta-h))))
    (setf (values (window-x window) (window-y window))
          (updated-window-origin-for-resize window origin
                                            (mezzano.gui:surface-width new-framebuffer)
                                            (mezzano.gui:surface-height new-framebuffer)))
    ;; Switch over to the new framebuffer.
    (setf (slot-value window '%buffer) new-framebuffer)
    ;; Update display based on new position & geometry.
    (expand-clip-rectangle-by-window window)
    ;; Notify the client that the resize has completed here.
    (send-event window event)
    ;; Adjust the window's grab region, keeping it clamped to the
    ;; window geometry.
    ;; TODO: Make use of origin when adjusting it?
    (when (grabp window)
      (setf (grab-x1 window) (min (grab-x1 window) (width window))
            (grab-y1 window) (min (grab-y1 window) (height window))
            (grab-x2 window) (min (grab-x2 window) (width window))
            (grab-y2 window) (min (grab-y2 window) (height window))))
    ;; Mouse cursor may or may not be over a different window now.
    (update-mouse-cursor)))

(defun resize-window (window new-framebuffer &key (origin :top-left))
  (assert (eql (mezzano.gui:surface-format new-framebuffer) :argb32))
  (submit-compositor-event (make-instance 'resize-event
                                          :window window
                                          :origin origin
                                          :new-fb new-framebuffer)))

;;;; Window move event.

(defclass move-event ()
  ((%window :initarg :window :reader window)
   (%x      :initarg :x      :reader new-x)
   (%y      :initarg :y      :reader new-y)))

(defmethod process-event ((event move-event))
  (let ((window (window event)))
    (expand-clip-rectangle-by-window window)
    (setf (window-x window) (new-x event)
          (window-y window) (new-y event))
    (expand-clip-rectangle-by-window window)))

(defun move-window (window new-x new-y)
  (submit-compositor-event (make-instance 'move-event
                                          :window window
                                          :x new-x
                                          :y new-y)))

;;;; Window data.

(defclass set-window-data-event (event)
  ((%data :initarg :data :reader window-data)))

(defun lookup-cursor (cursor)
  (cond ((typep cursor 'mouse-cursor)
         cursor)
        (t (case cursor
             (:default :default)
             (:none :none)
             (t (gethash cursor *mouse-cursor-library*))))))

(defun real-set-window-data (window &key cursor &allow-other-keys)
  (when cursor
    (setf (cursor window) (or (lookup-cursor cursor)
                              :default))
    (update-mouse-cursor)))

(defmethod process-event ((event set-window-data-event))
  (apply #'real-set-window-data (window event) (window-data event)))

(defun set-window-data (window &rest data &key &allow-other-keys)
  (submit-compositor-event (make-instance 'set-window-data-event
                                          :window window
                                          :data data)))

;;;; Cursor control.

(defclass grab-cursor-event (event)
  ((%grabp :initarg :grabp :reader grabp)
   (%x :initarg :x :reader x)
   (%y :initarg :y :reader y)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

(defmethod process-event ((event grab-cursor-event))
  (let* ((win (window event))
         (x1 (or (x event) 0))
         (y1 (or (y event) 0))
         (x2 (+ x1 (or (width event) (width win))))
         (y2 (+ y1 (or (height event) (height win)))))
    (setf (grabp win) (grabp event)
          (grab-x1 win) (clamp x1 0 (width win))
          (grab-y1 win) (clamp y1 0 (height win))
          (grab-x2 win) (clamp x2 0 (width win))
          (grab-y2 win) (clamp y2 0 (height win)))))

(defun grab-cursor (window grabp &optional x y w h)
  "Grab or release the cursor.
Prevents the cursor from leaving the window boundary.
Only works when the window is active."
  (submit-compositor-event (make-instance 'grab-cursor-event
                                          :window window
                                          :grabp grabp
                                          :x x
                                          :y y
                                          :width w
                                          :height h)))

;;;; Quit event, sent by the compositor when the user wants to close the window.

(defclass quit-event (event)
  ())

;;;; Internal redisplay timer event.

(defvar *redisplay-pending* nil)

(defclass redisplay-event (event)
  ((%fullp :initarg :full :reader redisplay-event-fullp))
  (:default-initargs :full nil))

(defmethod process-event ((event redisplay-event))
  (when (not (eql *redisplay-pending* :full))
    (setf *redisplay-pending* (if (redisplay-event-fullp event) :full t))))

(defun force-redisplay (&optional full)
  (submit-compositor-event (make-instance 'redisplay-event :full full)))

;;;; Notifications.

(defclass subscribe-event (event)
  ((%category :initarg :category :reader category)))

(defun subscribe-notification (window category)
  (submit-compositor-event (make-instance 'subscribe-event
                                          :window window
                                          :category category)))

(defmethod process-event ((event subscribe-event))
  (pushnew (category event) (slot-value (window event) '%subscribed-notifications))
  (case (category event)
    (:screen-geometry
     (send-event (window event) (make-instance 'screen-geometry-update
                                              :width (mezzano.supervisor:framebuffer-width *main-screen*)
                                              :height (mezzano.supervisor:framebuffer-height *main-screen*))))))

(defclass unsubscribe-event (event)
  ((%category :initarg :category :reader category)))

(defun unsubscribe-notification (window category)
  (submit-compositor-event (make-instance 'unsubscribe-event
                                          :window window
                                          :category category)))

(defmethod process-event ((event unsubscribe-event))
  (setf (slot-value (window event) '%subscribed-notifications)
        (remove (category event) (subscribed-notifications (window event)))))

(defun broadcast-notification (category payload)
  (dolist (win *window-list*)
    (when (member category (subscribed-notifications win))
      ;; Maybe copy the payload?
      (send-event win payload))))

;;;; Screen geometry change notification.

(defclass screen-geometry-update (event)
  ((%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

;;;; Screen update notification.

(defclass screen-update (event)
  ((%x :initarg :x :reader x)
   (%y :initarg :y :reader y)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

;;;; Other event stuff.

(defun send-event (window event)
  "Send an event to a window."
  (cond ((mezzano.sync:mailbox-send event (mailbox window) :wait-p nil)
         (when (window-unresponsive window)
           (expand-clip-rectangle-by-window window)
           (setf (window-unresponsive window) nil)))
        ((not (window-unresponsive window))
         (setf (window-unresponsive window) t)
         (expand-clip-rectangle-by-window window))))

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

(defvar *postprocess-matrix* nil)
(defvar *prev-postprocess-matrix* nil)

(defun postprocess-matrix ()
  *postprocess-matrix*)

(defun (setf postprocess-matrix) (matrix)
  (check-type matrix (or null colour-matrix))
  (setf *postprocess-matrix* matrix)
  (force-redisplay t)
  matrix)

(defun postprocess-screen (clip-x clip-y clip-w clip-h)
  (bitblt *postprocess-matrix* ; matrix mode
          clip-w clip-h
          *screen-backbuffer* clip-x clip-y
          *screen-backbuffer* clip-x clip-y))

(defun resize-in-progress-p ()
  (and *drag-window*
       *resize-origin*))

(defun resize-rect-geom ()
  (multiple-value-bind (origin new-width new-height)
      (compute-resized-window-geometry)
    (multiple-value-bind (real-x real-y)
        (updated-window-origin-for-resize *drag-window* origin
                                          new-width new-height)
      (values real-x real-y new-width new-height))))

(defparameter *resize-rect-colour* (mezzano.gui:make-colour 0.75 1.0 1.0 0.5))
(defparameter *resize-rect-border-colour* (mezzano.gui:make-colour 0 1.0 1.0 0.5))

(defun draw-resize-rect ()
  (multiple-value-bind (x y w h)
      (resize-rect-geom)
    ;; Draw a rectangle with a nice border.
    ;; Top
    (fill-with-clip w 1 *resize-rect-border-colour* x y)
    ;; Bottom
    (fill-with-clip w 1 *resize-rect-border-colour* x (+ y (- h 1)))
    ;; Don't let the sides overdraw the top/bottom.
    ;; Left
    (fill-with-clip 1 (- h 2) *resize-rect-border-colour* x (+ y 1))
    ;; Right
    (fill-with-clip 1 (- h 2) *resize-rect-border-colour* (+ x (- w 1)) (+ y 1))
    ;; Body
    (fill-with-clip (- w 2) (- h 2) *resize-rect-colour* (+ x 1) (+ y 1))))

(defun recompose-windows (&optional full)
  (when full
    (damage-whole-screen))
  (when (not (eql *prev-postprocess-matrix* *postprocess-matrix*))
    (damage-whole-screen)
    (setf *prev-postprocess-matrix* *postprocess-matrix*))
  (when (or (zerop *clip-rect-width*)
            (zerop *clip-rect-height*))
    (return-from recompose-windows))
  (broadcast-notification :screen-updates
                          (make-instance 'screen-update
                                         :x *clip-rect-x*
                                         :y *clip-rect-y*
                                         :width *clip-rect-width*
                                         :height *clip-rect-height*))
  ;; Draw windows back-to-front.
  (dolist (window (reverse *window-list*))
    (blit-with-clip (width window) (height window)
                    (window-buffer window)
                    (window-x window) (window-y window))
    (when (window-unresponsive window)
      (fill-with-clip (width window) (height window)
                      (mezzano.gui:make-colour 0 0 0 0.5)
                      (window-x window) (window-y window))))
  ;; Resize rect.
  (when (and (not *enable-live-resize*)
             (resize-in-progress-p))
    (draw-resize-rect))
  ;; Then the mouse pointer on top.
  (let ((mouse-surface (mouse-cursor-surface *mouse-pointer*))
        (hot-x (mouse-cursor-hot-x *mouse-pointer*))
        (hot-y (mouse-cursor-hot-y *mouse-pointer*)))
    (blit-with-clip (surface-width mouse-surface) (surface-height mouse-surface)
                    mouse-surface
                    (- *mouse-x* hot-x) (- *mouse-y* hot-y)))
  (when *postprocess-matrix*
    (postprocess-screen *clip-rect-x* *clip-rect-y* *clip-rect-width* *clip-rect-height*))
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

(defvar *compositor-update-interval* 1/60)
(defparameter *last-frame-timestamp* 0)
(defparameter *screensaver-spawn-function* nil)
(defparameter *screensaver-time* (* 5 60))

(defun screen-dirty-p ()
  (or (and (not (zerop *clip-rect-width*))
           (not (zerop *clip-rect-height*)))
      ;; Changing the postprocess matrix implies redrawing the screen.
      (not (eql *prev-postprocess-matrix* *postprocess-matrix*))))

(defun compositor-thread-body ()
  (when (not (eql *main-screen* (mezzano.supervisor:current-framebuffer)))
    ;; Framebuffer changed. Rebuild the screen.
    (reset-idle-time)
    (setf *main-screen* (mezzano.supervisor:current-framebuffer)
          *screen-backbuffer* (mezzano.gui:make-surface
                               (mezzano.supervisor:framebuffer-width *main-screen*)
                               (mezzano.supervisor:framebuffer-height *main-screen*)))
    (setf *redisplay-pending* :full)
    (broadcast-notification :screen-geometry
                            (make-instance 'screen-geometry-update
                                           :width (mezzano.supervisor:framebuffer-width *main-screen*)
                                           :height (mezzano.supervisor:framebuffer-height *main-screen*))))
  ;; Process all pending events, then update the screen if required.
  ;; This folds multiple redisplay events into a single screen update,
  ;; preventing other events from being held up by slow redisplays.
  (loop
     (multiple-value-bind (event validp)
         (mezzano.sync:mailbox-receive *event-queue* :wait-p nil)
       (when (not validp)
         (return))
       (mezzano.internals::log-and-ignore-errors
         (process-event event))))
  (when (and *screensaver-spawn-function*
             (not (screensaver-running-p)))
    (let ((idle-time (- (get-internal-run-time) *idle-time*)))
      (when (>= idle-time (* *screensaver-time* internal-time-units-per-second))
        ;; Set the idle time to a point shortly in the future so the screensaver has
        ;; time to spawn and doesn't get reopened next frame.
        (setf *idle-time* (+ (get-internal-run-time)
                             (* *compositor-update-interval* 2
                                internal-time-units-per-second)))
        (ignore-errors
          (funcall *screensaver-spawn-function*)))))
  (when (or *redisplay-pending*
            (and (screen-dirty-p)
                 (>= (- (get-internal-run-time) *last-frame-timestamp*)
                     (* *compositor-update-interval* internal-time-units-per-second))))
    (setf *last-frame-timestamp* (get-internal-run-time))
    (recompose-windows (eql *redisplay-pending* :full))
    (setf *redisplay-pending* nil))
  (cond ((screen-dirty-p)
         ;; Only bother arming the frame timer if there's outstanding stuff to draw.
         (mezzano.supervisor:with-timer (frame-timer
                                         ;; Use an absolute time since the last frame instead of a
                                         ;; relative time to try to get a more consistent frame rate.
                                         :absolute (+ *last-frame-timestamp*
                                                       (truncate
                                                        (* *compositor-update-interval* internal-time-units-per-second)))
                                         :name 'compositor-redisplay)
           (mezzano.sync:wait-for-objects frame-timer
                                          *event-queue*
                                          (mezzano.supervisor:framebuffer-boot-id *main-screen*))))
        ((and *screensaver-spawn-function*
              (not (screensaver-running-p)))
         ;; Screensaver enabled, nothing to draw,
         ;; figure out how long until the screensaver needs to open.
         (mezzano.supervisor:with-timer (idle-timer
                                         :absolute (truncate
                                                    (+ *idle-time*
                                                       (* *screensaver-time* internal-time-units-per-second)))
                                         :name 'compositor-screensaver-idle)
           (mezzano.sync:wait-for-objects idle-timer
                                          *event-queue*
                                          (mezzano.supervisor:framebuffer-boot-id *main-screen*))))
        (t
         (mezzano.sync:wait-for-objects *event-queue*
                                         (mezzano.supervisor:framebuffer-boot-id *main-screen*)))))

(defun compositor-thread ()
  (loop (compositor-thread-body)))

(when (not *compositor*)
  (setf *compositor* (mezzano.supervisor:make-thread 'compositor-thread
                                                     :name "Compositor")))
