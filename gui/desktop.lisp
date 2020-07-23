;;;; Desktop application.
;;;;
;;;; Displays the GUI background and icons for launching programs.

(in-package :mezzano.gui.desktop)

(defvar *icons* '(("LOCAL:>Icons>Terminal.png" "Lisp REPL" "(mezzano.gui.fancy-repl:spawn)")
                  ("LOCAL:>Icons>Chat.png" "IRC" "(mezzano.irc-client:spawn)")
                  ("LOCAL:>Icons>Editor.png" "Editor" "(med:spawn)")
                  ("LOCAL:>Icons>Mandelbrot.png" "Mandelbrot" "(mezzano.mandelbrot:spawn)")
                  ("LOCAL:>Icons>Peek.png" "Keymap" "(mezzano.gui.settings:spawn)")
                  ("LOCAL:>Icons>Peek.png" "Peek" "(mezzano.gui.peek:spawn)")
                  ("LOCAL:>Icons>Peek.png" "Spy" "(mezzano.gui.spy:spawn)")
                  ("LOCAL:>Icons>Peek.png" "Memory Monitor" "(mezzano.gui.memory-monitor:spawn)")
                  ("LOCAL:>Icons>Filer.png" "Filer" "(mezzano.gui.filer:spawn)")
                  ("LOCAL:>Icons>Telnet.png" "Telnet" "(mezzano.telnet:spawn)")))

;;; Events for modifying the desktop.

(defclass set-background-colour ()
  ((%colour :initarg :colour :reader colour)))

(defclass set-background-image ()
  ((%image-pathname :initarg :image-pathname :reader image-pathname)))

(defclass set-text-colour ()
  ((%colour :initarg :colour :reader colour)))

;;; Desktop object.

(defclass desktop ()
  ((%fifo :initarg :fifo :reader fifo)
   (%font :initarg :font :accessor font)
   (%window :initarg :window :reader window)
   (%colour :initarg :colour :reader colour)
   (%image :initarg :image :reader image)
   (%image-pathname :initarg :image-pathname :reader image-pathname)
   (%clicked-icon :initarg :clicked-icon :accessor clicked-icon)
   (%text-cache :initform (make-hash-table :test 'equal) :reader text-cache))
  (:default-initargs :window nil :colour #xFF00FF00 :image nil :image-pathname nil :clicked-icon nil))

(defgeneric dispatch-event (desktop event)
  ;; Eat unknown events.
  (:method (w e)))

(defmethod dispatch-event (desktop (event set-background-colour))
  (check-type (colour event) (unsigned-byte 32))
  (setf (slot-value desktop '%colour) (colour event))
  (redraw-desktop-window desktop))

(defmethod dispatch-event (desktop (event set-background-image))
  (cond ((not (image-pathname event))
         (setf (slot-value desktop '%image) nil
               (slot-value desktop '%image-pathname) nil))
        (t (let* ((path (image-pathname event))
                  (image (load-image path)))
             (when image
               (setf (slot-value desktop '%image) image
                     (slot-value desktop '%image-pathname) path)))))
  (redraw-desktop-window desktop))

(defmethod dispatch-event (desktop (event set-text-colour))
  (check-type (colour event) (unsigned-byte 32))
  (setf theme:*desktop-text* (colour event))
  (redraw-desktop-window desktop))

(defmethod dispatch-event (desktop (event comp:screen-geometry-update))
  (let ((new-framebuffer (mezzano.gui:make-surface (comp:width event) (comp:height event))))
    (comp:resize-window (window desktop) new-framebuffer)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-event))
  (redraw-desktop-window app))

(defmethod dispatch-event (desktop (event comp:window-close-event))
  (when (eql (comp:window event) (window desktop))
    ;; Either the desktop window or the notification window was closed. Exit.
    (throw 'quit nil)))

(defun rasterize-string (string font colour)
  (let* ((width (font:string-display-width string font))
         (stroke-width 3)
         (result (gui:make-surface (+ width (* (ceiling stroke-width) 2)) (+ (font:line-height font) (* (ceiling stroke-width) 2)))))
    (font:draw-stroked-string
     string font result (ceiling stroke-width) (+ (ceiling stroke-width) (font:ascender font)) colour #xFF000000 stroke-width)
    result))

(defun get-cached-text-surface (desktop text)
  (or (gethash text (text-cache desktop))
      (setf (gethash text (text-cache desktop))
            (rasterize-string text (font desktop) theme:*desktop-text*))))

(defparameter *icon-vertical-space* 20)
(defparameter *icon-horizontal-offset* 20)
(defparameter *icon-image/text-space* 10)

(defun icon-geometry (desktop icon-data)
  (let* ((icon (first icon-data))
         (name (second icon-data))
         (image (load-image icon))
         (text (get-cached-text-surface desktop name)))
    (values (+ (gui:surface-width image)
               *icon-image/text-space*
               (gui:surface-width text))
            (gui:surface-height image))))

(defun get-icon-at-point (desktop x y)
  (let* ((font (font desktop))
         (window (window desktop))
         (desktop-height (comp:height window)))
    (loop
       with icon-pen = 0
       with column = *icon-horizontal-offset*
       with widest = 0
       for icon-repr in *icons*
       do
         (cond ((consp icon-repr)
                (incf icon-pen *icon-vertical-space*)
                (multiple-value-bind (width height)
                    (icon-geometry desktop icon-repr)
                  (when (> (+ icon-pen height) desktop-height)
                    (incf column widest)
                    (setf widest 0)
                    (setf icon-pen *icon-vertical-space*))
                  (when (and (<= column x (1- (+ column width)))
                             (<= icon-pen y (1- (+ icon-pen height))))
                    (return icon-repr))
                  (incf icon-pen height)
                  (setf widest (max widest width))))
               ((eql icon-repr :next-column)
                (incf column widest)
                (setf widest 0)
                (setf icon-pen 0))))))

(defmethod dispatch-event (desktop (event comp:mouse-event))
  (when (logbitp 0 (comp:mouse-button-change event))
    (let ((x (comp:mouse-x-position event))
          (y (comp:mouse-y-position event)))
      (cond ((logbitp 0 (comp:mouse-button-state event))
             ;; Mouse down, begin click. Highlight the clicked thing.
             (setf (clicked-icon desktop) (get-icon-at-point desktop x y))
             (when (clicked-icon desktop)
               (redraw-desktop-window desktop)))
            (t ;; Mouse up, finishing click. If unclicked thing is clicked thing, then run program.
             (let ((unclicked-thing (get-icon-at-point desktop x y))
                   (clicked-thing (clicked-icon desktop)))
               ;; Unclick the icon before doing stuff.
               (when (clicked-icon desktop)
                 (setf (clicked-icon desktop) nil)
                 (redraw-desktop-window desktop))
               (when (and unclicked-thing (eql unclicked-thing clicked-thing))
                 (let ((*package* (find-package :cl-user)))
                   (eval (read-from-string (third unclicked-thing)))))))))))

(defun redraw-desktop-window (desktop)
  (let* ((window (window desktop))
         (desktop-width (comp:width window))
         (desktop-height (comp:height window))
         (framebuffer (comp:window-buffer window))
         (font (font desktop)))
    (gui:bitset :set
                desktop-width desktop-height
                (colour desktop)
                framebuffer 0 0)
    (when (image desktop)
      (let* ((image (image desktop))
             (image-width (gui:surface-width image))
             (image-height (gui:surface-height image)))
        (gui:bitblt :blend
                    image-width image-height
                    image 0 0
                    framebuffer
                    (- (truncate desktop-width 2) (truncate image-width 2))
                    (- (truncate desktop-height 2) (truncate image-height 2)))))
    (loop
       with icon-pen = 0
       with column = *icon-horizontal-offset*
       with widest = 0
       for icon-repr in *icons*
       do
         (cond ((consp icon-repr)
                (incf icon-pen *icon-vertical-space*)
                (multiple-value-bind (width height)
                    (icon-geometry desktop icon-repr)
                  (when (> (+ icon-pen height) desktop-height)
                    (incf column (+ widest *icon-horizontal-offset*))
                    (setf widest 0)
                    (setf icon-pen *icon-vertical-space*))
                  (render-icon desktop icon-pen column icon-repr)
                  (incf icon-pen height)
                  (setf widest (max widest width))))
               ((eql icon-repr :next-column)
                (incf column (+ widest *icon-horizontal-offset*))
                (setf widest 0)
                (setf icon-pen 0))))
    (comp:damage-window window 0 0 (comp:width window) (comp:height window))))

(defun render-icon (desktop icon-pen column-offset icon-data)
  (let* ((window (window desktop))
         (font (font desktop))
         (framebuffer (comp:window-buffer window))
         (icon (first icon-data))
         (name (second icon-data))
         (image (load-image icon))
         (text (get-cached-text-surface desktop name)))
    (gui:bitblt :blend
                (gui:surface-width image) (gui:surface-height image)
                image 0 0
                framebuffer column-offset icon-pen)
    (when (eql icon (clicked-icon desktop))
      (gui:bitset :xor
                  (gui:surface-width image) (gui:surface-height image)
                  #x00FFFFFF
                  framebuffer
                  column-offset icon-pen))
    (gui:bitblt :blend
                (gui:surface-width text) (gui:surface-height text)
                text 0 0
                framebuffer
                (+ column-offset (gui:surface-width image) *icon-image/text-space*)
                (- (+ icon-pen (truncate (gui:surface-height image) 2) (font:ascender font))
                   (gui:surface-height text)))))

(defun desktop-main (desktop)
  (let* ((font (font:open-font
                font:*default-font*
                (* font:*default-font-size* 2)))
         (fifo (fifo desktop)))
    (setf (font desktop) font)
    ;; And a dummy window before we know the screen geometry.
    (setf (slot-value desktop '%window) (comp:make-window fifo 0 0
                                                          :layer :bottom
                                                          :initial-z-order :below-current
                                                          :kind :desktop))
    (setf (mezzano.gui.compositor:name (slot-value desktop '%window)) desktop)
    ;; Subscribe to screen geometry change notifications.
    (comp:subscribe-notification (window desktop) :screen-geometry)
    (unwind-protect
         (catch 'quit
           (loop
              (mezzano.internals::log-and-ignore-errors
               (dispatch-event desktop (mezzano.supervisor:fifo-pop fifo)))))
      (comp:close-window (window desktop)))))

(defun spawn (&key (colour #xFF011172) image)
  (let* ((fifo (mezzano.supervisor:make-fifo 50))
         (desktop (make-instance 'desktop :fifo fifo)))
    ;; Submit initial property set events.
    (when colour
      (mezzano.supervisor:fifo-push (make-instance 'set-background-colour :colour colour) fifo))
    (when image
      (mezzano.supervisor:fifo-push (make-instance 'set-background-image :image-pathname image) fifo))
    (mezzano.supervisor:make-thread (lambda () (desktop-main desktop))
                                    :name "Desktop"
                                    :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                       :title "Desktop console"))
                                                        (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                        (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                        (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                        (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                        (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                        (*query-io* ,(make-synonym-stream '*terminal-io*))))
    fifo))
