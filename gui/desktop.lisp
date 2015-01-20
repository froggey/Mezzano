(defpackage :mezzanine.gui.desktop
  (:use :cl)
  (:export #:spawn))

(in-package :mezzanine.gui.desktop)

(defvar *image-cache* (make-hash-table :test 'equal))

(defvar *icons* '(("LOCAL:>Icons>Terminal.png" "Lisp REPL" "(mezzanine.gui.fancy-repl:spawn)")
                  ("LOCAL:>Icons>Chat.png" "IRC" "(irc-client:spawn)")
                  ("LOCAL:>Icons>Editor.png" "Editor" "(mezzanine.editor:spawn)")
                  ("LOCAL:>Icons>Mandelbrot.png" "Mandelbrot" "(mandelbrot:spawn)")
                  ("LOCAL:>Icons>Peek.png" "Peek" "(mezzanine.gui.peek:spawn)")
                  ("LOCAL:>Icons>Telnet.png" "Nethack" "(telnet:spawn-nao)")
                  ("LOCAL:>Icons>Telnet.png" "Nyan Cat" "(telnet:spawn-nyan)")))

(defun load-jpeg (path)
  (handler-bind
      ((error (lambda (c)
                (ignore-errors (let ((*standard-output* *debug-io*))
                                 (format t "Error ~A while loading ~S.~%" c path)
                                 (sys.int::backtrace)
                                 (fresh-line)))
                (return-from load-jpeg nil))))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (multiple-value-bind (data height width channels)
          (jpeg:decode-stream stream)
        (when (not (eql channels 3))
          (error "Unsupported JPEG image, too many or too few channels."))
        ;; Transcode the image to a proper ARGB array.
        (let ((array (make-array (list height width) :element-type '(unsigned-byte 32))))
          (dotimes (y height)
            (dotimes (x width)
              (setf (aref array y x) (logior #xFF000000
                                             (ash (aref data (+ (* (+ (* y width) x) 3) 2)) 16)
                                             (ash (aref data (+ (* (+ (* y width) x) 3) 1)) 8)
                                             (aref data (* (+ (* y width) x) 3))))))
          array)))))

(defun load-png (path)
  (handler-bind ((error
                  (lambda (c)
                    (ignore-errors (let ((*standard-output* *debug-io*))
                                     (format t "Error ~A while loading ~S.~%" c path)
                                     (sys.int::backtrace)
                                     (fresh-line)))
                    (return-from load-png nil))))
    (let* ((png (png-read:read-png-file path))
           (data (png-read:image-data png))
           (width (png-read:width png))
           (height (png-read:height png))
           (array (make-array (list height width) :element-type '(unsigned-byte 32))))
      (ecase (png-read:colour-type png)
        (:truecolor-alpha
         (dotimes (y height)
           (dotimes (x width)
             (setf (aref array y x) (logior (ash (aref data x y 0) 16)
                                            (ash (aref data x y 1) 8)
                                            (aref data x y 2)
                                            (ash (aref data x y 3) 24))))))
        (:truecolor
         (dotimes (y height)
           (dotimes (x width)
             (setf (aref array y x) (logior (ash (aref data x y 0) 16)
                                            (ash (aref data x y 1) 8)
                                            (aref data x y 2)
                                            (ash #xFF 24)))))))
      array)))

(defun load-image (path)
  (let* ((truename (truename path))
         (image (gethash truename *image-cache*)))
    (unless image
      (setf image (or (load-jpeg truename)
                      (load-png truename)
                      (error "Unable to load ~S." path)))
      (setf (gethash truename *image-cache*) image))
    image))

;;; Events for modifying the desktop.

(defclass set-background-colour ()
  ((%colour :initarg :colour :reader colour)))

(defclass set-background-image ()
  ((%image-pathname :initarg :image-pathname :reader image-pathname)))

;;; Desktop object.

(defclass desktop ()
  ((%fifo :initarg :fifo :reader fifo)
   (%font :initarg :font :accessor font)
   (%window :initarg :window :reader window)
   (%notification-window :initarg :notification-window :reader notification-window)
   (%colour :initarg :colour :reader colour)
   (%image :initarg :image :reader image)
   (%image-pathname :initarg :image-pathname :reader image-pathname)
   (%clicked-icon :initarg :clicked-icon :accessor clicked-icon))
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

(defmethod dispatch-event (desktop (event mezzanine.gui.compositor:screen-geometry-update))
  (let ((new-window (mezzanine.gui.compositor:make-window (fifo desktop)
                                                          (mezzanine.gui.compositor:width event)
                                                          (mezzanine.gui.compositor:height event)
                                                          :layer :bottom
                                                          :initial-z-order :below-current))
        (old-window (window desktop)))
    (setf (slot-value desktop '%window) new-window)
    (redraw-desktop-window desktop)
    (mezzanine.gui.compositor:close-window old-window)))

(defmethod dispatch-event (desktop (event mezzanine.gui.compositor:window-close-event))
  (when (or (eql (mezzanine.gui.compositor:window event) (window desktop))
            (eql (mezzanine.gui.compositor:window event) (notification-window desktop)))
    ;; Either the desktop window or the notification window was closed. Exit.
    (throw 'quitting-time nil)))

(defun get-icon-at-point (desktop x y)
  (loop
     with icon-pen = 0
     for icon-repr in *icons*
     for (icon name fn) in *icons*
     do (progn ;ignore-errors
          (incf icon-pen 20)
          (let* ((image (load-image icon))
                 (width (array-dimension image 1))
                 (height (array-dimension image 0)))
            (when (and (<= 20 x (1- (+ 20 width)))
                       (<= icon-pen y (1- (+ icon-pen height))))
              (return icon-repr))
            (incf icon-pen (array-dimension image 0))))))

(defmethod dispatch-event (desktop (event mezzanine.gui.compositor:mouse-event))
  (when (logbitp 0 (mezzanine.gui.compositor:mouse-button-change event))
    (let ((x (mezzanine.gui.compositor:mouse-x-position event))
          (y (mezzanine.gui.compositor:mouse-y-position event)))
      (cond ((logbitp 0 (mezzanine.gui.compositor:mouse-button-state event))
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
         (desktop-width (mezzanine.gui.compositor:width window))
         (desktop-height (mezzanine.gui.compositor:height window))
         (framebuffer (mezzanine.gui.compositor:window-buffer window))
         (font (font desktop)))
    (mezzanine.gui:bitset desktop-height desktop-width
                          (colour desktop)
                          framebuffer 0 0)
    (when (image desktop)
      (let* ((image (image desktop))
             (image-width (array-dimension image 1))
             (image-height (array-dimension image 0)))
        (mezzanine.gui:bitblt-argb-xrgb image-height image-width
                                        image 0 0
                                        framebuffer (- (truncate desktop-height 2) (truncate image-height 2)) (- (truncate desktop-width 2) (truncate image-width 2)))))
    (loop
       with icon-pen = 0
       for icon-repr in *icons*
       for (icon name fn) in *icons*
       do (progn ;ignore-errors
            (incf icon-pen 20)
            (let ((image (load-image icon)))
              (mezzanine.gui:bitblt-argb-xrgb (array-dimension image 0) (array-dimension image 1)
                                              image 0 0
                                              framebuffer icon-pen 20)
              (when (eql icon-repr (clicked-icon desktop))
                (mezzanine.gui:bitxor (array-dimension image 0) (array-dimension image 1)
                                      #x00FFFFFF
                                      framebuffer icon-pen 20))
              (loop
                 with pen = 0
                 for ch across name
                 for glyph = (mezzanine.gui.font:character-to-glyph font ch)
                 for mask = (mezzanine.gui.font:glyph-mask glyph)
                 do
                   (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1) #xFFFFFFFF
                                                          mask 0 0
                                                          framebuffer
                                                          (- (+ icon-pen (truncate (array-dimension image 1) 2) (mezzanine.gui.font:ascender font))
                                                             (mezzanine.gui.font:glyph-yoff glyph))
                                                          (+ 20 (array-dimension image 0) 10 pen (mezzanine.gui.font:glyph-xoff glyph)))
                   (incf pen (mezzanine.gui.font:glyph-advance glyph)))
              (incf icon-pen (array-dimension image 0)))))
    (mezzanine.gui.compositor:damage-window window 0 0 (mezzanine.gui.compositor:width window) (mezzanine.gui.compositor:height window))))

(defun desktop-main (desktop)
  (mezzanine.gui.font:with-font (font
                                 mezzanine.gui.font:*default-font*
                                 (* mezzanine.gui.font:*default-font-size* 2))
    (let* ((fifo (fifo desktop)))
      (setf (font desktop) font)
      ;; Create a zero-size window for listening on system notifications.
      (setf (slot-value desktop '%notification-window) (mezzanine.gui.compositor:make-window fifo 0 0))
      ;; And a dummy window before we know the screen geometry.
      (setf (slot-value desktop '%window) (mezzanine.gui.compositor:make-window fifo 0 0))
      ;; Subscribe to screen geometry change notifications.
      (mezzanine.gui.compositor:subscribe-notification (notification-window desktop) :screen-geometry)
      (unwind-protect
           (catch 'quitting-time
             (loop
                (handler-case
                    (dispatch-event desktop (mezzanine.supervisor:fifo-pop fifo))
                  (error (c)
                    (ignore-errors
                      (format t "~&Error ~A in desktop.~%" c))))))
        (mezzanine.gui.compositor:close-window notification-window)
        (mezzanine.gui.compositor:close-window (window desktop))))))

(defun spawn (&key (colour #xFF011172) image)
  (let* ((fifo (mezzanine.supervisor:make-fifo 50))
         (desktop (make-instance 'desktop :fifo fifo)))
    ;; Submit initial property set events.
    (when colour
      (mezzanine.supervisor:fifo-push (make-instance 'set-background-colour :colour colour) fifo))
    (when image
      (mezzanine.supervisor:fifo-push (make-instance 'set-background-image :image-pathname image) fifo))
    (mezzanine.supervisor:make-thread (lambda () (desktop-main desktop))
                                      :name "Desktop"
                                      :initial-bindings `((*terminal-io* ,(make-instance 'mezzanine.gui.popup-io-stream:popup-io-stream
                                                                                         :title "Desktop console"))
                                                          (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                          (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                          (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                          (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                          (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                          (*query-io* ,(make-synonym-stream '*terminal-io*))))
    fifo))
