;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :med)

(defclass editor ()
  ((%fifo :initarg :fifo :reader fifo)
   (%pending-event :initarg :pending-event :accessor pending-event)
   (%pending-redisplay :initarg :pending-redisplay :accessor pending-redisplay)
   (%window :initarg :window :accessor window)
   (%frame :initarg :frame :accessor frame)
   (%buffer :initarg :buffer :accessor current-buffer)
   (%last-buffer :initarg :last-buffer :accessor last-buffer)
   (%font :initarg :font :reader font)
   (%font-bold :initarg :font-bold :reader font-bold)
   (%pre-command-hooks :initarg :pre-command-hooks :accessor pre-command-hooks)
   (%post-command-hooks :initarg :post-command-hooks :accessor post-command-hooks)
   (%foreground-colour :initarg :foreground-colour :accessor foreground-colour)
   (%background-colour :initarg :background-colour :accessor background-colour)
   ;; Redisplay state.
   (%current-screen :initarg :screen :accessor editor-current-screen)
   (%line-cache :initarg :display-line-cache :accessor display-line-cache))
  (:default-initargs :pending-event nil
                     :pending-redisplay t
                     :foreground-colour mezzano.gui.theme:*foreground*
                     :background-colour mezzano.gui.theme:*background*
                     :last-buffer '()
                     :pre-command-hooks '()
                     :post-command-hooks '()
                     :screen nil
                     :display-line-cache '()))

(defclass open-file-request ()
  ((%path :initarg :path :reader path)
   (%position :initarg :position :reader initial-position))
  (:default-initargs :position nil))

(defvar *last-command*)
(defvar *this-command*)
(defvar *last-character*)
(defvar *this-character*)
(defvar *last-chord*)
(defvar *this-chord*)
(defvar *isearch-string* (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
(defvar *last-isearch-string* *isearch-string*)
(defvar *buffer-list* '())
(defvar *killed-region* '())
(defvar *global-key-map* (make-hash-table))

(defvar *editor*)

(defun buffer-list () *buffer-list*)
(defun (setf buffer-list) (value) (setf *buffer-list* value))

(defun killed-region () *killed-region*)
(defun (setf killed-region) (value) (setf *killed-region* value))

(defun global-key-map () *global-key-map*)
(defun (setf global-key-map) (value) (setf *global-key-map* value))

(defgeneric dispatch-event (editor event)
  (:method (editor event)))

(defmethod dispatch-event (editor (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame editor)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame editor)))

(defmethod dispatch-event (editor (event mezzano.gui.compositor:mouse-event))
  (handler-case
      (mezzano.gui.widgets:frame-mouse-event (frame editor) event)
    (mezzano.gui.widgets:close-button-clicked ()
      (throw 'quit nil))))

(defmethod dispatch-event (editor (event mezzano.gui.compositor:window-close-event))
  (declare (ignore editor event))
  (throw 'quit nil))

(defmethod dispatch-event (editor (event mezzano.gui.compositor:quit-event))
  (declare (ignore editor event))
  (throw 'quit nil))

(defmethod dispatch-event (editor (event mezzano.gui.compositor:key-event))
  (when (not (mezzano.gui.compositor:key-releasep event))
    (throw 'next-character
      (if (mezzano.gui.compositor:key-modifier-state event)
          ;; Force character to uppercase when a modifier key is active, gets
          ;; around weirdness in how character names are processed.
          ;; #\C-a and #\C-A both parse as the same character (C-LATIN_CAPITAL_LETTER_A).
          (mezzano.internals::make-character
           (char-code (char-upcase (mezzano.gui.compositor:key-key event)))
           :control (find :control (mezzano.gui.compositor:key-modifier-state event))
           :meta (find :meta (mezzano.gui.compositor:key-modifier-state event))
           :super (find :super (mezzano.gui.compositor:key-modifier-state event))
           :hyper (find :hyper (mezzano.gui.compositor:key-modifier-state event)))
          (mezzano.gui.compositor:key-key event)))))

(defmethod dispatch-event (editor (event open-file-request))
  (let ((*editor* editor))
    (find-file (path event))
    (when (initial-position event)
      (move-beginning-of-buffer (current-buffer *editor*))
      (move-char (current-buffer *editor*) (initial-position event))
      (force-redisplay))))

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
  (redraw-screen)
  (force-redisplay))

(defun editor-read-char-1 ()
  (catch 'next-character
    (when (pending-event *editor*)
      (let ((event (pending-event *editor*)))
        (setf (pending-event *editor*) nil)
      (dispatch-event *editor* event)))
    (loop
       (when (pending-redisplay *editor*)
         (throw 'next-character nil))
       (dispatch-event *editor* (mezzano.supervisor:fifo-pop (fifo *editor*))))))

(defun editor-read-char ()
  (loop
     (let ((ch (editor-read-char-1)))
       (when ch
         (return ch)))
     (setf (pending-redisplay *editor*) (not (redisplay)))))

(define-condition pending-input () ())

(defun check-pending-input ()
  (cond ((pending-event *editor*)
         (signal 'pending-input))
        (t (let ((event (mezzano.supervisor:fifo-pop (fifo *editor*) nil)))
             (when event
               (setf (pending-event *editor*) event)
               (signal 'pending-input))))))

(defun refresh-title ()
  (let ((buffer (current-buffer *editor*)))
    (setf (mezzano.gui.widgets:frame-title (frame *editor*))
          (format nil "MED - ~A~A"
                  (or (buffer-property buffer 'path)
                      (buffer-property buffer 'title)
                      "Untitled")
                  (cond ((buffer-property buffer 'new-file)
                         " (New file)")
                        (t ""))))
    (mezzano.gui.widgets:draw-frame (frame *editor*))))

(defun switch-to-buffer (buffer)
  (setf (current-buffer *editor*) buffer
        (pending-redisplay *editor*) t)
  (refresh-title))
