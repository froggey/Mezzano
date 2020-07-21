;;;; A console stream for GUI applications

(defpackage :mezzano.gui.popup-io-stream
  (:use :cl :mezzano.gui.font)
  (:local-nicknames (:sys.int :mezzano.internals))
  (:export #:popup-io-stream
           #:lazy-popup-io-stream))

(in-package :mezzano.gui.popup-io-stream)

(defclass popup-io-stream (mezzano.line-editor:line-edit-mixin
                           mezzano.gray:fundamental-character-input-stream
                           mezzano.gray:fundamental-character-output-stream)
  ((%fifo :reader fifo)
   (%framebuffer :reader framebuffer)
   (%closed :accessor window-closed)
   (%break-requested :initform nil :accessor break-requested)
   ;; Transient window stuff.
   (%window :reader window)
   (%thread :reader thread)
   (%frame-dirty-p :initform nil :accessor frame-dirty-p)
   ;; The read-char typeahead buffer.
   (%input :reader input-buffer)
   (%waiting-input-count :accessor waiting-input-count)
   ;; Frame and display.
   (%frame :reader frame)
   (%text-widget :reader display)
   ;; Protecting stuff.
   (%lock :reader lock)
   (%cvar :reader cvar)))

(defclass damage ()
  ((%x :initarg :x :reader x)
   (%y :initarg :y :reader y)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

(defmethod initialize-instance :after ((instance popup-io-stream) &key (width 640) (height 480) title)
  ;; Do this early so the initial text-widget damage even won't open the window.
  (setf (window-closed instance) nil
        (slot-value instance '%fifo) (mezzano.supervisor:make-fifo 50)
        (slot-value instance '%lock) (mezzano.supervisor:make-mutex "Popup Stream Lock")
        (slot-value instance '%cvar) (mezzano.supervisor:make-condition-variable "Popup Stream Cvar"))
  (let* ((framebuffer (mezzano.gui:make-surface width height))
         (frame (make-instance 'mezzano.gui.widgets:frame
                               :framebuffer framebuffer
                               :title (string (or title (format nil "~S" instance)))
                               :close-button-p t
                               :damage-function (lambda (&rest args)
                                                  (declare (ignore args))
                                                  (setf (frame-dirty-p instance) t))))
         (term (make-instance 'mezzano.gui.widgets:text-widget
                              :font (mezzano.gui.font:open-font
                                     mezzano.gui.font:*default-monospace-font*
                                     mezzano.gui.font:*default-monospace-font-size*)
                              :framebuffer framebuffer
                              :x-position (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                              :y-position (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                              :width (- width
                                        (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                        (nth-value 1 (mezzano.gui.widgets:frame-size frame)))
                              :height (- height
                                         (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                         (nth-value 3 (mezzano.gui.widgets:frame-size frame)))
                              :damage-function (lambda (&rest args) (apply #'damage instance args)))))
    (setf (slot-value instance '%framebuffer) framebuffer
          (slot-value instance '%closed) t
          (slot-value instance '%window) nil
          (slot-value instance '%thread) nil
          ;; Use a supervisor FIFO for this because I'm too lazy to implement a normal FIFO.
          (slot-value instance '%input) (mezzano.supervisor:make-fifo 500 :element-type 'character)
          (slot-value instance '%frame) frame
          (slot-value instance '%text-widget) term
          (slot-value instance '%waiting-input-count) 0)))

(defgeneric dispatch-event (stream event)
  (:method (viewer event) nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame window)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame window))
  (dispatch-event window
                  (make-instance 'damage
                                 :x 0
                                 :y 0
                                 :width (mezzano.gui.compositor:width (window window))
                                 :height (mezzano.gui.compositor:height (window window)))))

(defmethod dispatch-event (window (event mezzano.gui.compositor:key-event))
  ;; should filter out strange keys?
  (when (not (mezzano.gui.compositor:key-releasep event))
    (mezzano.supervisor:with-mutex ((lock window))
      (cond ((and (eql (mezzano.gui.compositor:key-key event) #\Esc)
                  (member :control (mezzano.gui.compositor:key-modifier-state event)))
             (setf (break-requested window) t))
            (t (mezzano.supervisor:fifo-push (if (mezzano.gui.compositor:key-modifier-state event)
                                                 ;; Force character to uppercase when a modifier key is active, gets
                                                 ;; around weirdness in how character names are processed.
                                                 ;; #\C-a and #\C-A both parse as the same character (C-LATIN_CAPITAL_LETTER_A).
                                                 (sys.int::make-character (char-code (char-upcase (mezzano.gui.compositor:key-key event)))
                                                                          :control (find :control (mezzano.gui.compositor:key-modifier-state event))
                                                                          :meta (find :meta (mezzano.gui.compositor:key-modifier-state event))
                                                                          :super (find :super (mezzano.gui.compositor:key-modifier-state event))
                                                                          :hyper (find :hyper (mezzano.gui.compositor:key-modifier-state event)))
                                                 (mezzano.gui.compositor:key-key event))
                                             (input-buffer window) nil)))
      (mezzano.supervisor:condition-notify (cvar window)))))

(defmethod dispatch-event (window (event mezzano.gui.compositor:mouse-event))
  (mezzano.gui.widgets:frame-mouse-event (frame window) event)
  (when (frame-dirty-p window)
    (setf (frame-dirty-p window) nil)
    (dispatch-event window
                    (make-instance 'damage
                                   :x 0
                                   :y 0
                                   :width (mezzano.gui.compositor:width (window window))
                                   :height (mezzano.gui.compositor:height (window window))))))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-close-event))
  (when (eql (window window) (mezzano.gui.compositor:window event))
    (throw 'mezzano.supervisor::terminate-thread nil)))

(defmethod dispatch-event (window (event damage))
  (mezzano.gui:bitblt :set
                      (width event) (height event)
                      (framebuffer window)
                      (x event) (y event)
                      (mezzano.gui.compositor:window-buffer (window window))
                      (x event) (y event))
  (mezzano.gui.compositor:damage-window (window window)
                                        (x event) (y event)
                                        (width event) (height event)))


(defun window-thread (stream)
  (let ((*terminal-io* sys.int::*cold-stream*))
    (mezzano.gui.compositor:with-window (window (fifo stream) (mezzano.gui:surface-width (framebuffer stream)) (mezzano.gui:surface-height (framebuffer stream))
                                                :initial-z-order :below-current
                                                :name stream)
      (setf (slot-value stream '%window) window
            (mezzano.gui.widgets:activep (frame stream)) nil)
      (mezzano.gui.widgets:draw-frame (frame stream))
      (dispatch-event stream
                      (make-instance 'damage
                                     :x 0
                                     :y 0
                                     :width (mezzano.gui.compositor:width window)
                                     :height (mezzano.gui.compositor:height window)))
      (unwind-protect
           (loop
              (handler-case
                  (dispatch-event stream (mezzano.supervisor:fifo-pop (fifo stream)))
                (mezzano.gui.widgets:close-button-clicked ()
                  (mezzano.supervisor:with-mutex ((lock stream))
                    (when (zerop (waiting-input-count stream))
                      (setf (slot-value stream '%window) nil
                            (slot-value stream '%thread) nil
                            (window-closed stream) t)
                      (return))))
                (error (c)
                  (ignore-errors
                    (format t "~&Error ~A in popup stream.~%" c)))))
        (mezzano.supervisor:with-mutex ((lock stream))
          (when (eql (slot-value stream '%window) window)
            (setf (slot-value stream '%window) nil
                  (slot-value stream '%thread) nil
                  (window-closed stream) t)))))))

(defun open-window (stream)
  (when (window-closed stream)
    (setf (window-closed stream) nil
          (slot-value stream '%thread) (mezzano.supervisor:make-thread (lambda () (window-thread stream))
                                                                       :name (format nil "~S Window Thread" stream)))))

(defun damage (stream x y width height)
  (open-window stream)
  (mezzano.supervisor:fifo-push (make-instance 'damage
                                               :x x
                                               :y y
                                               :width width
                                               :height height)
                                (fifo stream)))

(defun check-for-break (stream)
  (when (break-requested stream)
    (setf (break-requested stream) nil)
    (break)))

(defmethod mezzano.gray:stream-read-char ((stream popup-io-stream))
  (loop
     (check-for-break stream)
     (unwind-protect
          (progn
            (setf (mezzano.gui.widgets:cursor-visible (display stream)) t)
            (mezzano.supervisor:with-mutex ((lock stream))
              ;; Examine input stream.
              (let ((ch (mezzano.supervisor:fifo-pop (input-buffer stream) nil)))
                (when ch
                  (return ch)))
              ;; No characters ready, open the window and wait for one.
              (open-window stream)
              ;; Wait.
              (incf (waiting-input-count stream))
              (mezzano.supervisor:condition-wait (cvar stream) (lock stream))
              (decf (waiting-input-count stream))))
       (setf (mezzano.gui.widgets:cursor-visible (display stream)) nil))))

(defmethod mezzano.gray:stream-read-char-no-hang ((stream popup-io-stream))
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    ;; Examine input stream.
    (mezzano.supervisor:fifo-pop (input-buffer stream) nil)))

;;; Forward stream stuff on to the display.

(defmethod mezzano.gray:stream-terpri ((stream popup-io-stream))
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    (mezzano.gray:stream-terpri (display stream))))

(defmethod mezzano.gray:stream-write-char ((stream popup-io-stream) character)
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    (mezzano.gray:stream-write-char (display stream) character)))

(defmethod mezzano.gray:stream-start-line-p ((stream popup-io-stream))
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    (mezzano.gray:stream-start-line-p (display stream))))

(defmethod mezzano.gray:stream-line-column ((stream popup-io-stream))
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    (mezzano.gray:stream-line-column (display stream))))

(defmethod sys.int::stream-cursor-pos ((stream popup-io-stream))
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    (sys.int::stream-cursor-pos (display stream))))

(defmethod sys.int::stream-move-to ((stream popup-io-stream) x y)
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    (sys.int::stream-move-to (display stream) x y)))

(defmethod sys.int::stream-character-width ((stream popup-io-stream) character)
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    (sys.int::stream-character-width (display stream) character)))

(defmethod sys.int::stream-compute-motion ((stream popup-io-stream) string &optional (start 0) end initial-x initial-y)
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    (sys.int::stream-compute-motion (display stream) string start end initial-x initial-y)))

(defmethod sys.int::stream-clear-between ((stream popup-io-stream) start-x start-y end-x end-y)
  (check-for-break stream)
  (mezzano.supervisor:with-mutex ((lock stream))
    (sys.int::stream-clear-between (display stream) start-x start-y end-x end-y)))

(defvar *instances* (make-hash-table :weakness :key))
(defvar *instances-lock* (mezzano.supervisor:make-mutex "Popup instances lock"))

(defclass lazy-popup-io-stream (mezzano.gray:fundamental-character-input-stream
                                mezzano.gray:fundamental-character-output-stream)
  ())

(defun get-thread-popup-io-stream ()
  (mezzano.supervisor:with-mutex (*instances-lock*)
    (let* ((thread (mezzano.supervisor:current-thread))
           (entry (gethash thread *instances*)))
      (when (not entry)
        (setf entry (make-instance 'popup-io-stream
                                   :title (format nil "Console for ~S" thread))
              (gethash thread *instances*) entry))
      entry)))

(defmethod mezzano.gray:stream-read-char ((stream lazy-popup-io-stream))
  (mezzano.gray:stream-read-char (get-thread-popup-io-stream)))

(defmethod mezzano.gray:stream-read-char-no-hang ((stream lazy-popup-io-stream))
  (mezzano.gray:stream-read-char-no-hang (get-thread-popup-io-stream)))

(defmethod mezzano.gray:stream-unread-char ((stream lazy-popup-io-stream) character)
  (mezzano.gray:stream-unread-char (get-thread-popup-io-stream) character))

(defmethod mezzano.gray:stream-terpri ((stream lazy-popup-io-stream))
  (mezzano.gray:stream-terpri (get-thread-popup-io-stream)))

(defmethod mezzano.gray:stream-write-char ((stream lazy-popup-io-stream) character)
  (mezzano.gray:stream-write-char (get-thread-popup-io-stream) character))

(defmethod mezzano.gray:stream-start-line-p ((stream lazy-popup-io-stream))
  (mezzano.gray:stream-start-line-p (get-thread-popup-io-stream)))

(defmethod mezzano.gray:stream-line-column ((stream lazy-popup-io-stream))
  (mezzano.gray:stream-line-column (get-thread-popup-io-stream)))

(defmethod sys.int::stream-cursor-pos ((stream lazy-popup-io-stream))
  (sys.int::stream-cursor-pos (get-thread-popup-io-stream)))

(defmethod sys.int::stream-move-to ((stream lazy-popup-io-stream) x y)
  (sys.int::stream-move-to (get-thread-popup-io-stream) x y))

(defmethod sys.int::stream-character-width ((stream lazy-popup-io-stream) character)
  (sys.int::stream-character-width (get-thread-popup-io-stream) character))

(defmethod sys.int::stream-compute-motion ((stream lazy-popup-io-stream) string &optional (start 0) end initial-x initial-y)
  (sys.int::stream-compute-motion (get-thread-popup-io-stream) string start end initial-x initial-y))

(defmethod sys.int::stream-clear-between ((stream lazy-popup-io-stream) start-x start-y end-x end-y)
  (sys.int::stream-clear-between (get-thread-popup-io-stream) start-x start-y end-x end-y))
