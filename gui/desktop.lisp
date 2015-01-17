(defpackage :mezzanine.gui.desktop
  (:use :cl)
  (:export #:spawn))

(in-package :mezzanine.gui.desktop)

(defun load-image (path)
  (handler-bind
      ((error (lambda (c)
                (ignore-errors (format *debug-io* "Error ~A while loading ~S.~%" c path)
                               (sys.int::backtrace))
                (return-from load-image nil))))
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

;;; Events for modifying the desktop.

(defclass set-background-colour ()
  ((%colour :initarg :colour :reader colour)))

(defclass set-background-image ()
  ((%image-pathname :initarg :image-pathname :reader image-pathname)))

;;; Desktop object.

(defclass desktop ()
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%notification-window :initarg :notification-window :reader notification-window)
   (%colour :initarg :colour :reader colour)
   (%image :initarg :image :reader image)
   (%image-pathname :initarg :image-pathname :reader image-pathname))
  (:default-initargs :window nil :colour #xFF00FF00 :image nil :image-pathname nil))

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
                                                          :layer :bottom))
        (old-window (window desktop)))
    (setf (slot-value desktop '%window) new-window)
    (redraw-desktop-window desktop)
    (mezzanine.gui.compositor:close-window old-window)))

(defmethod dispatch-event (desktop (event mezzanine.gui.compositor:window-close-event))
  (when (or (eql (mezzanine.gui.compositor:window event) (window desktop))
            (eql (mezzanine.gui.compositor:window event) (notification-window desktop)))
    ;; Either the desktop window or the notification window was closed. Exit.
    (throw 'quitting-time nil)))

(defun redraw-desktop-window (desktop)
  (let* ((window (window desktop))
         (desktop-width (mezzanine.gui.compositor:width window))
         (desktop-height (mezzanine.gui.compositor:height window))
         (framebuffer (mezzanine.gui.compositor:window-buffer window)))
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
    (mezzanine.gui.compositor:damage-window window 0 0 (mezzanine.gui.compositor:width window) (mezzanine.gui.compositor:height window))))

(defun desktop-main (desktop)
  (let* ((fifo (fifo desktop)))
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
      (mezzanine.gui.compositor:close-window (window desktop)))))

(defun spawn (&key (colour #xFF909090) image)
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
