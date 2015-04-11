;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.gui.desktop
  (:use :cl)
  (:export #:spawn))

(in-package :mezzano.gui.desktop)

(defvar *image-cache* (make-hash-table :test 'equal))

(defvar *icons* '(("LOCAL:>Icons>Terminal.png" "Lisp REPL" "(mezzano.gui.fancy-repl:spawn)")
                  ("LOCAL:>Icons>Chat.png" "IRC" "(irc-client:spawn)")
                  ("LOCAL:>Icons>Editor.png" "Editor" "(mezzano.editor:spawn)")
                  ("LOCAL:>Icons>Mandelbrot.png" "Mandelbrot" "(mandelbrot:spawn)")
                  ("LOCAL:>Icons>Peek.png" "Peek" "(mezzano.gui.peek:spawn)")
                  ("LOCAL:>Icons>FS-Viewer.png" "FS Viewer" "(mezzano.gui.fs-viewer:spawn)")
                  ("LOCAL:>Icons>Telnet.png" "Nethack" "(telnet:spawn-nao)")
                  ("LOCAL:>Icons>Telnet.png" "Nyan Cat" "(telnet:spawn-nyan)")))

(defun load-jpeg (path)
  (ignore-errors
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
  (ignore-errors
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

(defmethod dispatch-event (desktop (event mezzano.gui.compositor:screen-geometry-update))
  (let ((new-window (mezzano.gui.compositor:make-window (fifo desktop)
                                                        (mezzano.gui.compositor:width event)
                                                        (mezzano.gui.compositor:height event)
                                                        :layer :bottom
                                                        :initial-z-order :below-current
                                                        :kind :desktop))
        (old-window (window desktop)))
    (setf (slot-value desktop '%window) new-window)
    (redraw-desktop-window desktop)
    (mezzano.gui.compositor:close-window old-window)))

(defmethod dispatch-event (desktop (event mezzano.gui.compositor:window-close-event))
  (when (or (eql (mezzano.gui.compositor:window event) (window desktop))
            (eql (mezzano.gui.compositor:window event) (notification-window desktop)))
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

(defmethod dispatch-event (desktop (event mezzano.gui.compositor:mouse-event))
  (when (logbitp 0 (mezzano.gui.compositor:mouse-button-change event))
    (let ((x (mezzano.gui.compositor:mouse-x-position event))
          (y (mezzano.gui.compositor:mouse-y-position event)))
      (cond ((logbitp 0 (mezzano.gui.compositor:mouse-button-state event))
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
         (desktop-width (mezzano.gui.compositor:width window))
         (desktop-height (mezzano.gui.compositor:height window))
         (framebuffer (mezzano.gui.compositor:window-buffer window))
         (font (font desktop)))
    (mezzano.gui:bitset desktop-height desktop-width
                        (colour desktop)
                        framebuffer 0 0)
    (when (image desktop)
      (let* ((image (image desktop))
             (image-width (array-dimension image 1))
             (image-height (array-dimension image 0)))
        (mezzano.gui:bitblt-argb-xrgb image-height image-width
                                      image 0 0
                                      framebuffer (- (truncate desktop-height 2) (truncate image-height 2)) (- (truncate desktop-width 2) (truncate image-width 2)))))
    (loop
       with icon-pen = 0
       for icon-repr in *icons*
       for (icon name fn) in *icons*
       do (progn ;ignore-errors
            (incf icon-pen 20)
            (let ((image (load-image icon)))
              (mezzano.gui:bitblt-argb-xrgb (array-dimension image 0) (array-dimension image 1)
                                            image 0 0
                                            framebuffer icon-pen 20)
              (when (eql icon-repr (clicked-icon desktop))
                (mezzano.gui:bitxor (array-dimension image 0) (array-dimension image 1)
                                    #x00FFFFFF
                                    framebuffer icon-pen 20))
              (loop
                 with pen = 0
                 for ch across name
                 for glyph = (mezzano.gui.font:character-to-glyph font ch)
                 for mask = (mezzano.gui.font:glyph-mask glyph)
                 do
                   (mezzano.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1) #xFFFFFFFF
                                                        mask 0 0
                                                        framebuffer
                                                        (- (+ icon-pen (truncate (array-dimension image 1) 2) (mezzano.gui.font:ascender font))
                                                           (mezzano.gui.font:glyph-yoff glyph))
                                                        (+ 20 (array-dimension image 0) 10 pen (mezzano.gui.font:glyph-xoff glyph)))
                   (incf pen (mezzano.gui.font:glyph-advance glyph)))
              (incf icon-pen (array-dimension image 0)))))
    (mezzano.gui.compositor:damage-window window 0 0 (mezzano.gui.compositor:width window) (mezzano.gui.compositor:height window))))

(defun desktop-main (desktop)
  (mezzano.gui.font:with-font (font
                               mezzano.gui.font:*default-font*
                               (* mezzano.gui.font:*default-font-size* 2))
    (let* ((fifo (fifo desktop)))
      (setf (font desktop) font)
      ;; Create a zero-size window for listening on system notifications.
      (setf (slot-value desktop '%notification-window) (mezzano.gui.compositor:make-window fifo 0 0))
      ;; And a dummy window before we know the screen geometry.
      (setf (slot-value desktop '%window) (mezzano.gui.compositor:make-window fifo 0 0))
      ;; Subscribe to screen geometry change notifications.
      (mezzano.gui.compositor:subscribe-notification (notification-window desktop) :screen-geometry)
      (unwind-protect
           (catch 'quitting-time
             (loop
                (sys.int::log-and-ignore-errors
                  (dispatch-event desktop (mezzano.supervisor:fifo-pop fifo)))))
        (mezzano.gui.compositor:close-window notification-window)
        (mezzano.gui.compositor:close-window (window desktop))))))

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
