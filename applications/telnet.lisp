;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :telnet
  (:use :cl)
  (:export #:spawn))

(in-package :telnet)

(defconstant +command-se+ 240
  "End of subnegotiation parameters.")
(defconstant +command-nop+ 241
  "No operation.")
(defconstant +command-data-mark+ 242
  "The data stream portion of a Synch.")
(defconstant +command-break+ 243
  "NVT character BRK.")
(defconstant +command-interrupt-process+ 244
  "The function IP.")
(defconstant +command-Abort-output+ 245
  "The function AO.")
(defconstant +command-Are-You-There+ 246
  "The function AYT.")
(defconstant +command-Erase-character+ 247
  "The function EC.")
(defconstant +command-Erase-Line+ 248
  "The function EL.")
(defconstant +command-Go-ahead+ 249
  "The GA signal.")
(defconstant +command-SB+ 250
  "Indicates that what follows is subnegotiation of the indicated option.")
(defconstant +command-WILL+ 251
  "Indicates the desire to begin performing, or confirmation
that you are now performing, the indicated option.")
(defconstant +command-WONT+ 252
  "Indicates the refusal to perform, or continue performing,
the indicated option.")
(defconstant +command-DO+ 253
  "Indicates the request that the other party perform, or
confirmation that you are expecting the other party to
perform, the indicated option.")
(defconstant +command-DONT+ 254
  "Indicates the demand that the other party stop performing,
or confirmation that you are no longer expecting the other
party to perform, the indicated option.")
(defconstant +command-IAC+ 255 "Interpret as Command.")

(defconstant +option-transmit-binary+ 0)
(defconstant +option-echo+ 1)
(defconstant +option-suppress-go-ahead+ 3)
(defconstant +option-status+ 5)
(defconstant +option-window-size+ 31)
(defconstant +option-terminal-speed+ 32)
(defconstant +option-terminal-type+ 24)
(defconstant +option-x-display-location+ 35)
(defconstant +option-new-environ+ 39)

(defconstant +subnegotiation-is+ 0)
(defconstant +subnegotiation-send+ 1)
(defconstant +subnegotiation-info+ 2)

(defun read-subnegotiation (connection)
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (byte nil))
    (loop (setf byte (read-byte connection))
       (cond ((eql byte +command-iac+)
              (case (read-byte connection)
                (#.+command-iac+
                 (vector-push-extend +command-iac+ bytes))
                (#.+command-se+
                 (return bytes))))
             (t (vector-push-extend byte bytes))))))

(defun telnet-command (telnet command)
  (let ((connection (connection telnet)))
    (case command
      (#.+command-sb+
       (let ((option (read-byte connection))
             (data (read-subnegotiation connection)))
         (case option
           (#.+option-terminal-type+
            (write-sequence (apply 'vector
                                   (append (list +command-iac+ +command-sb+ +option-terminal-type+ +subnegotiation-is+)
                                           (map 'list 'char-code (terminal-type telnet))
                                           (list +command-iac+ +command-se+)))
                            connection))
           (t (write-sequence (vector +command-iac+ +command-sb+ option +subnegotiation-is+
                                      +command-iac+ +command-se+)
                              connection)))))
      (#.+command-do+
       (let ((option (read-byte connection)))
         (case option
           (#.+option-terminal-type+
            (write-sequence (vector +command-iac+ +command-will+
                                    +option-terminal-type+)
                            connection))
           (#.+option-window-size+
            (when (eql option +option-window-size+)
              (setf (do-window-size-updates telnet) t))
            (send-window-size telnet))
           (t (write-sequence (vector +command-iac+ +command-wont+ option)
                              connection)))))
      (#.+command-dont+
       (let ((option (read-byte connection)))
         (when (eql option +option-window-size+)
           (setf (do-window-size-updates telnet) nil))
         (write-sequence (vector +command-iac+ +command-wont+
                                 option)
                         connection)))
      (#.+command-will+
       (read-byte connection)
       #+nil(write-sequence (vector +command-iac+ +command-wont+
                                    (read-byte connection))))
      (#.+command-wont+
       (read-byte connection)
       #+nil(write-sequence (vector +command-iac+ +command-wont+
                                    (read-byte connection)))))))

(defun send-window-size (telnet)
  (write-sequence (apply 'vector
                         (append (list +command-iac+ +command-sb+ +option-window-size+)
                                 (let ((width (mezzano.gui.xterm:terminal-width (xterm telnet)))
                                       (height (mezzano.gui.xterm:terminal-height (xterm telnet))))
                                   (list (ldb (byte 8 8) width) (ldb (byte 8 0) width)
                                         (ldb (byte 8 8) height) (ldb (byte 8 0) height)))
                                 (list +command-iac+ +command-se+)))
                  (connection telnet)))

(defclass telnet-client ()
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%font :initarg :font :reader font)
   (%frame :initarg :frame :reader frame)
   (%xterm :initarg :xterm :reader xterm)
   (%terminal-type :initarg :terminal-type :reader terminal-type)
   (%connection :initarg :connection :accessor connection)
   (%receive-thread :initarg :receive-thread :accessor receive-thread)
   (%do-window-size-updates :initarg :do-window-size-updates :accessor do-window-size-updates))
  (:default-initargs :connection nil))

(defclass server-disconnect-event ()
  ())

(defun compute-telnet-window-size (font cwidth cheight)
  ;; Make a fake frame to get the frame size.
  (let ((frame (make-instance 'mezzano.gui.widgets:frame)))
    (multiple-value-bind (left right top bottom)
        (mezzano.gui.widgets:frame-size frame)
      (let ((xterm-width (* cwidth (mezzano.gui.font:glyph-advance
                                    (mezzano.gui.font:character-to-glyph font #\M))))
            (xterm-height (* cheight (mezzano.gui.font:line-height font))))
        (values (+ left right xterm-width)
                (+ top bottom xterm-height)
                xterm-width xterm-height)))))

(defun telnet-write-string (telnet string)
  (loop
     with xterm = (xterm telnet)
     for ch across (string string)
     do (mezzano.gui.xterm:receive-char xterm ch)
     when (eql ch #\Newline)
     do (mezzano.gui.xterm:receive-char xterm #\Return)))

(defun telnet-read-line (telnet)
  (let ((line (make-array 100 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
       (handler-case
           (dispatch-event telnet (mezzano.supervisor:fifo-pop (fifo telnet)))
         (typed-key (c)
           (let ((key (typed-key c)))
             (telnet-write-string telnet key)
             (when (eql key #\Newline)
               (return line))
             (vector-push-extend key line)))))))

(defgeneric dispatch-event (telnet event)
  (:method (t e)))

(defmethod dispatch-event (telnet (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame telnet)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame telnet)))

(defmethod dispatch-event (telnet (event mezzano.gui.compositor:mouse-event))
  (handler-case
      (mezzano.gui.widgets:frame-mouse-event (frame telnet) event)
    (mezzano.gui.widgets:close-button-clicked ()
      (throw 'quit nil))))

(defmethod dispatch-event (telnet (event mezzano.gui.compositor:window-close-event))
  (throw 'quit nil))

(defmethod dispatch-event (telnet (event mezzano.gui.compositor:quit-event))
  (throw 'quit nil))

(define-condition typed-key ()
  ((%key :initarg :key :reader typed-key)))

(defmethod dispatch-event (telnet (event mezzano.gui.compositor:key-event))
  (when (not (mezzano.gui.compositor:key-releasep event))
    (signal 'typed-key :key (mezzano.gui.compositor:key-key event))
    (mezzano.gui.xterm:input-translate (xterm telnet)
                                       (mezzano.gui.compositor:key-key event)
                                       (lambda (ch)
                                         ;; FIXME: Translate to UTF-8 here.
                                         (when (eql ch (code-char +command-iac+))
                                           (write-byte +command-iac+ (connection telnet)))
                                         (write-byte (char-code ch) (connection telnet))))))

(defmethod dispatch-event (telnet (event server-disconnect-event))
  (setf (connection telnet) nil)
  (telnet-write-string telnet (format nil "~%Disconnected from server")))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-request-event))
  (let ((old-width (mezzano.gui.compositor:width (window app)))
        (old-height (mezzano.gui.compositor:height (window app)))
        (new-width (max 100 (mezzano.gui.compositor:width event)))
        (new-height (max 100 (mezzano.gui.compositor:height event))))
    ;; Snap dimensions to multiples of the font size.
    (multiple-value-bind (left right top bottom)
        (mezzano.gui.widgets:frame-size (frame app))
      (let* ((font (font app))
             (ch-width (mezzano.gui.font:glyph-advance
                        (mezzano.gui.font:character-to-glyph font #\M)))
             (ch-height (mezzano.gui.font:line-height font))
             (visible-width (truncate (- new-width left right) ch-width))
             (visible-height (truncate (- new-height top bottom) ch-height))
             (real-new-width (+ left right (* visible-width ch-width)))
             (real-new-height (+ top bottom (* visible-height ch-height))))
        (when (or (not (eql old-width real-new-width))
                  (not (eql old-height real-new-height)))
          (let ((new-framebuffer (mezzano.gui:make-surface
                                  real-new-width real-new-height)))
            (mezzano.gui.xterm:xterm-resize (xterm app)
                                            new-framebuffer
                                            left top
                                            visible-width visible-height)
            (mezzano.gui.widgets:resize-frame (frame app) new-framebuffer)
            (mezzano.gui.compositor:resize-window
             (window app) new-framebuffer
             :origin (mezzano.gui.compositor:resize-origin event))))))))

(defmethod dispatch-event (app (event mezzano.gui.compositor:resize-event))
  (when (do-window-size-updates app)
    (send-window-size app)))

(defun telnet-receive (telnet)
  (handler-case
      (let ((connection (connection telnet))
            (xterm (xterm telnet))
            (last-was-cr nil))
        ;; Announce capabilities.
        (write-sequence (vector +command-iac+ +command-do+ +option-suppress-go-ahead+
                                +command-iac+ +command-will+ +option-window-size+)
                        connection)
        ;; FIXME: Translate from UTF-8 here. Can't use read-char on the tcp-stream because
        ;; terminal IO happens on top of the binary telnet layer.
        (loop
           (when (not (connection telnet))
             (return))
           (let ((byte (read-byte connection)))
             (cond ((eql byte +command-iac+)
                    (let ((command (read-byte connection)))
                      (if (eql command +command-iac+)
                          (mezzano.gui.xterm:receive-char (xterm telnet) (code-char +command-iac+))
                          (telnet-command telnet command))))
                   ((eql byte #x0D) ; CR
                    (setf last-was-cr t)
                    (mezzano.gui.xterm:receive-char xterm (code-char byte)))
                   ((and last-was-cr (eql byte #x00))
                    (setf last-was-cr nil))
                   (t (setf last-was-cr nil)
                      (mezzano.gui.xterm:receive-char xterm (code-char byte)))))))
    (end-of-file ()
      (mezzano.supervisor:fifo-push (make-instance 'server-disconnect-event)
                                    (fifo telnet)))))

(defvar *server-shortcuts*
  '(("nao" "nethack.alt.org")
    ("nyan" "nyancat.dakko.us")))

(defun find-server (telnet server)
  (cond
    ((string= server "")
     ;; Prompting for the server.
     (telnet-write-string telnet (format nil "Known servers:~%"))
     (loop
        for (name address) in *server-shortcuts*
        do (telnet-write-string telnet (format nil " ~A: ~A~%" name address)))
     (telnet-write-string telnet "Server> ")
     (let ((s (telnet-read-line telnet)))
       (telnet-write-string telnet (format nil "Connecting to ~A~%" s))
       (find-server telnet s)))
    (t
     ;; Lookup by shortcut name.
     (dolist (s *server-shortcuts*
              ;; Or just pick that server.
              server)
       (when (string-equal (first s) server)
         (return (second s)))))))

(defun telnet-main (server port terminal-type cwidth cheight)
  (with-simple-restart (abort "Close telnet")
    (catch 'quit
      (let ((font (mezzano.gui.font:open-font
                   mezzano.gui.font:*default-monospace-font*
                   mezzano.gui.font:*default-monospace-font-size*))
            (fifo (mezzano.supervisor:make-fifo 50)))
        (multiple-value-bind (window-width window-height xterm-width xterm-height)
            (compute-telnet-window-size font cwidth cheight)
          (mezzano.gui.compositor:with-window (window fifo window-width window-height)
            (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
                   (frame (make-instance 'mezzano.gui.widgets:frame
                                         :framebuffer framebuffer
                                         :title "Telnet"
                                         :close-button-p t
                                         :resizablep t
                                         :damage-function (mezzano.gui.widgets:default-damage-function window)
                                         :set-cursor-function (mezzano.gui.widgets:default-cursor-function window)))
                   (xterm (make-instance 'mezzano.gui.xterm:xterm-terminal
                                         :framebuffer framebuffer
                                         :font font
                                         :x (nth-value 0 (mezzano.gui.widgets:frame-size frame))
                                         :y (nth-value 2 (mezzano.gui.widgets:frame-size frame))
                                         :width xterm-width
                                         :height xterm-height
                                         :damage-function (mezzano.gui.widgets:default-damage-function window)))
                   (telnet (make-instance 'telnet-client
                                          :fifo fifo
                                          :window window
                                          :font font
                                          :frame frame
                                          :xterm xterm
                                          :terminal-type terminal-type)))
              (mezzano.gui.widgets:draw-frame frame)
              (mezzano.gui.compositor:damage-window window
                                                    0 0
                                                    (mezzano.gui.compositor:width window)
                                                    (mezzano.gui.compositor:height window))
              (unwind-protect
                   (progn
                     (setf server (find-server telnet server))
                     (setf (connection telnet) (mezzano.network.tcp:tcp-stream-connect server port)
                           (receive-thread telnet) (mezzano.supervisor:make-thread (lambda () (telnet-receive telnet))
                                                                                   :name "Telnet receive"))
                     (setf (mezzano.gui.widgets:frame-title frame) (format nil "Telnet - ~A:~D" server port))
                     (mezzano.gui.widgets:draw-frame frame)
                     (loop
                        (dispatch-event telnet (mezzano.supervisor:fifo-pop fifo))))
                (when (connection telnet)
                  (close (connection telnet))
                  (setf (connection telnet) nil))))))))))

(defun spawn (&key (server "") (port 23) (terminal-type "xterm-color") (width 80) (height 24))
  (mezzano.supervisor:make-thread (lambda () (telnet-main server port terminal-type width height))
                                  :name "Telnet"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Telnet console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
