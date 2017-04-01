;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.gui.filer
  (:use :cl)
  (:export #:spawn))

(in-package :mezzano.gui.filer)

(defvar *up-icon* (mezzano.gui.image:load-image "LOCAL:>Icons>16x16 Up.png"))
(defvar *file-icon* (mezzano.gui.image:load-image "LOCAL:>Icons>16x16 File.png"))
(defvar *folder-icon* (mezzano.gui.image:load-image "LOCAL:>Icons>16x16 Folder.png"))

(defvar *directory-colour* mezzano.gui:*default-foreground-colour*)

(defun colourise-path (path)
  (case (canonical-type-from-pathname-type (pathname-type path))
    (:lisp-source-code (mezzano.gui:make-colour-from-octets #x94 #xBF #xF3))
    (:compiled-lisp-code (mezzano.gui:make-colour-from-octets #xF0 #xAF #x8F))
    (:text (mezzano.gui:make-colour-from-octets #xCC #x93 #x93))
    (:font (mezzano.gui:make-colour-from-octets #x7F #x9F #x7F))
    (:image (mezzano.gui:make-colour-from-octets #xDC #x8C #xC3))
    (:video (mezzano.gui:make-colour-from-octets #xDC #x8C #xC3))
    (:audio (mezzano.gui:make-colour-from-octets #xDC #x8C #xC3))
    (t mezzano.gui:*default-foreground-colour*)))

(defclass filer ()
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%thread :initarg :thread :reader thread)
   (%font :initarg :font :reader font)
   (%frame :initarg :frame :reader frame)
   (%path :initarg :path :accessor path)
   (%clickables :initarg :clickables :accessor clickables))
  (:default-initargs :clickables '()))

(defgeneric dispatch-event (viewer event))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame window)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame window)))

(defvar *type-registry*
  '((:lisp-source-code "lisp" "lsp" "asd" "lisp-expr")
    (:compiled-lisp-code "llf")
    (:text "text" "txt" "html" "css" "texinfo" "tex" "sh" "markdown" "md" "el")
    (:font "ttf")
    (:image "png" "jpeg" "jpg")
    (:video "avi" "gif")
    (:audio "wav")))

(defun canonical-type-from-pathname-type (type-string)
  (when (or (not type-string)
            (equal type-string ""))
    (return-from canonical-type-from-pathname-type :text))
  (when (eql type-string :wild)
    (return-from canonical-type-from-pathname-type :wild))
  (dolist (type *type-registry* (intern (format nil "UNKNOWN-~A" type-string) "KEYWORD"))
    (when (member type-string (rest type) :test #'string-equal)
      (return (first type)))))

(defgeneric view (type path)
  (:method (type path)
    (declare (ignore path))
    (error "No way to view files of type ~A." type)))

(defun view-in-editor (path)
  ;; Ech, the terrible groveling.
  (let ((existing (mezzano.gui.compositor:get-window-by-kind :editor)))
    (cond (existing
           (mezzano.supervisor:fifo-push (make-instance (read-from-string "med:open-file-request") :path path)
                                           (mezzano.gui.compositor::fifo existing)
                                           nil))
          (t (funcall (read-from-string "med:spawn") :initial-file path)))))

(defmethod view ((type (eql :lisp-source-code)) path)
  (view-in-editor path))

(defmethod view ((type (eql :text)) path)
  (view-in-editor path))

(defmethod view ((type (eql :image)) path)
  (funcall (read-from-string "mezzano.gui.image-viewer:spawn") path))

(defmethod view ((type (eql :video)) path)
  (funcall (read-from-string "mezzano.gui.trentino:spawn") path))

(defmethod view ((type (eql :audio)) path)
  (funcall (read-from-string "mezzano.gui.music-player:spawn") path))

(defun click (viewer thing)
  (cond ((functionp thing)
         (funcall thing viewer))
        ((not (pathnamep thing)))
        ((pathname-name thing)
         (view (canonical-type-from-pathname-type (pathname-type thing)) thing))
        (t ;; Directory
         (change-path viewer thing))))

(defmethod dispatch-event (window (event mezzano.gui.compositor:mouse-event))
  (mezzano.gui.widgets:frame-mouse-event (frame window) event)
  (when (and (not (logbitp 0 (mezzano.gui.compositor:mouse-button-state event)))
             (logbitp 0 (mezzano.gui.compositor:mouse-button-change event)))
    (let ((mx (mezzano.gui.compositor:mouse-x-position event))
          (my (mezzano.gui.compositor:mouse-y-position event)))
      (loop for (x1 y1 x2 y2 thing) in (clickables window) do
           (when (and (<= x1 mx) (< mx x2)
                      (<= y1 my) (< my y2))
             (return (click window thing)))))))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-close-event))
  (throw 'mezzano.supervisor::terminate-thread nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:quit-event))
  (throw 'mezzano.supervisor::terminate-thread nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:key-event))
  (declare (ignore window event)))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-request-event))
  (let ((old-width (mezzano.gui.compositor:width (window app)))
        (old-height (mezzano.gui.compositor:height (window app)))
        (new-width (max 100 (mezzano.gui.compositor:width event)))
        (new-height (max 100 (mezzano.gui.compositor:height event))))
    (when (or (not (eql old-width new-width))
              (not (eql old-height new-height)))
      (let ((new-framebuffer (mezzano.gui:make-surface
                              new-width new-height)))
        (mezzano.gui.widgets:resize-frame (frame app) new-framebuffer)
        (mezzano.gui.compositor:resize-window
         (window app) new-framebuffer
         :origin (mezzano.gui.compositor:resize-origin event))))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-event))
  (change-path app (path app)))

(defun draw-string (string font framebuffer x y colour)
  (loop
     with pen = x
     for ch across string
     for glyph = (mezzano.gui.font:character-to-glyph font ch)
     for mask = (mezzano.gui.font:glyph-mask glyph)
     do
       (mezzano.gui:bitset :blend
                           (mezzano.gui:surface-width mask) (mezzano.gui:surface-height mask)
                           colour
                           framebuffer
                           (+ pen (mezzano.gui.font:glyph-xoff glyph))
                           (- y (mezzano.gui.font:glyph-yoff glyph))
                           mask 0 0)
       (incf pen (mezzano.gui.font:glyph-advance glyph))
     finally (return pen)))

(defun change-path (viewer new-path)
  (setf (path viewer) new-path
        ;; Grumble
        (mezzano.gui.widgets:frame-title (frame viewer)) (concatenate 'string
                                                                      (string (mezzano.file-system:host-name (pathname-host new-path)))
                                                                      ":"
                                                                      (mezzano.file-system:unparse-pathname new-path (pathname-host new-path))))
  (let* ((window (window viewer))
         (framebuffer (mezzano.gui.compositor:window-buffer window))
         (font (font viewer))
         (width (mezzano.gui.compositor:width window))
         (height (mezzano.gui.compositor:height window))
         (stuff (directory (make-pathname :name :wild
                                          :type :wild
                                          :defaults new-path)))
         (files (sort (remove-if-not (lambda (x) (pathname-name x)) stuff)
                      #'string-lessp
                      :key #'pathname-name))
         (dirs (sort (set-difference stuff files)
                     #'string-lessp
                     :key (lambda (x) (first (last (pathname-directory x)))))))
    (multiple-value-bind (left right top bottom)
        (mezzano.gui.widgets:frame-size (frame viewer))
      (mezzano.gui:bitset :set
                          (- width left right)
                          (- height top bottom)
                          mezzano.gui:*default-background-colour*
                          framebuffer
                          left top)
      (let ((y top)
            (left-margin left)
            (next-left-margin 0)
            (column-y))
        (flet ((wr (string &optional (offset 0) (min-line-height 0))
                 (draw-string string
                              font
                              framebuffer
                              (+ left offset) (+ y (mezzano.gui.font:ascender font))
                              mezzano.gui:*default-foreground-colour*)
                 (incf y (max min-line-height (mezzano.gui.font:line-height font))))
               (seperator ()
                 (mezzano.gui:bitset :set
                                     (- width left right) 1
                                     mezzano.gui:*default-foreground-colour*
                                     framebuffer
                                     left y)
                 (incf y))
               (clickable (icon name thing colour)
                 (mezzano.gui:bitblt :blend
                                     16 16
                                     icon 0 0
                                     framebuffer
                                     (1+ left-margin) y)
                 (let ((end (draw-string name
                                         font
                                         framebuffer
                                         (+ left-margin 16 2) (+ y 2 (mezzano.gui.font:ascender font))
                                         colour)))
                   (setf next-left-margin (max next-left-margin end))
                   (push (list left-margin y end (+ y (max 16 (mezzano.gui.font:line-height font)))
                               thing)
                         (clickables viewer)))
                 (incf y (max 16 (mezzano.gui.font:line-height font)))
                 (when (> (+ y 16) height)
                   ;; Move to the next column.
                   (setf y column-y)
                   (setf left-margin (+ next-left-margin 8)))))
          (setf (clickables viewer) '())
          (let ((pen left))
            (dolist (host (remove-if (lambda (host)
                                       (or (string-equal (mezzano.file-system:host-name host) :http)
                                           (typep host 'mezzano.file-system:logical-host)))
                                     (mezzano.file-system:list-all-hosts)))
              (let ((before pen))
                (incf pen 10)
                (cond ((eql host (pathname-host new-path))
                       (let ((text-width (draw-string (mezzano.file-system:host-name host)
                                                      font
                                                      framebuffer
                                                      0 0
                                                      #x00000000)))
                         (mezzano.gui:bitset :set
                                             (+ 10 text-width 10)
                                             (mezzano.gui.font:line-height font)
                                             mezzano.gui:*default-foreground-colour*
                                             framebuffer
                                             (- pen 10) y)
                         (setf pen (draw-string (mezzano.file-system:host-name host)
                                                font
                                                framebuffer
                                                pen (+ y (mezzano.gui.font:ascender font))
                                                mezzano.gui:*default-background-colour*))))
                      (t (setf pen (draw-string (mezzano.file-system:host-name host)
                                                font
                                                framebuffer
                                                pen (+ y (mezzano.gui.font:ascender font))
                                                mezzano.gui:*default-foreground-colour*))))
                (incf pen 10)
                (push (list before y pen (+ y (mezzano.gui.font:line-height font))
                            (make-pathname :host host
                                           :directory '(:absolute)
                                           :name nil
                                           :type nil
                                           :version :newest))
                      (clickables viewer))))
            (incf y (mezzano.gui.font:line-height font)))
          (seperator)
          (wr (namestring new-path))
          (seperator)
          (setf column-y y)
          (when (not (= (length (pathname-directory new-path)) 1))
            (clickable *up-icon*
                       "Parent"
                       (make-pathname :directory (butlast (pathname-directory new-path))
                                      :defaults new-path)
                       mezzano.gui:*default-foreground-colour*))
          (dolist (d dirs)
            (clickable *folder-icon*
                       (format nil "~A" (first (last (pathname-directory d))))
                       d
                       *directory-colour*))
          (dolist (f files)
            (clickable *file-icon*
                       (file-namestring f)
                       f
                       (colourise-path f)))))
      (mezzano.gui.widgets:draw-frame (frame viewer))
      (mezzano.gui.compositor:damage-window window
                                            0 0
                                            width height))))

(defun main (default-path width height)
  (with-simple-restart (abort "Close filer")
    (let ((font (mezzano.gui.font:open-font
                 mezzano.gui.font:*default-monospace-font*
                 mezzano.gui.font:*default-monospace-font-size*))
          (fifo (mezzano.supervisor:make-fifo 50)))
      (mezzano.gui.compositor:with-window (window fifo (or width 640) (or height 480))
        (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
               (frame (make-instance 'mezzano.gui.widgets:frame
                                     :framebuffer framebuffer
                                     :title (namestring default-path)
                                     :close-button-p t
                                     :resizablep t
                                     :damage-function (mezzano.gui.widgets:default-damage-function window)
                                     :set-cursor-function (mezzano.gui.widgets:default-cursor-function window)))
               (filer (make-instance 'filer
                                     :fifo fifo
                                     :window window
                                     :thread (mezzano.supervisor:current-thread)
                                     :font font
                                     :frame frame
                                     :path default-path)))
          (change-path filer default-path)
          (loop
             (handler-case
                 (dispatch-event filer (mezzano.supervisor:fifo-pop fifo))
               (error (c)
                 (ignore-errors
                   (format t "Error: ~A~%" c)))
               ;; Exit when the close button is clicked.
               (mezzano.gui.widgets:close-button-clicked ()
                 (return-from main)))))))))

(defun spawn (&key (initial-path *default-pathname-defaults*) width height)
  (setf initial-path (merge-pathnames initial-path))
  (mezzano.supervisor:make-thread (lambda () (main initial-path width height))
                                  :name "Filer"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Filer console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
