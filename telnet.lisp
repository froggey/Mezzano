(defpackage :telnet
  (:use :cl))

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

(defun telnet-command (telnet connection command)
  (case command
    (#.+command-sb+
     (let ((option (read-byte connection))
           (data (read-subnegotiation connection)))
       (case option
         (#.+option-terminal-type+
          (write-sequence (apply 'vector
                                 (append (list +command-iac+ +command-sb+ +option-terminal-type+ +subnegotiation-is+)
                                         (map 'list 'char-code (telnet-terminal-type telnet))
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
          (write-sequence (apply 'vector
                                 (append (list +command-iac+ +command-sb+ +option-window-size+)
                                         (list #x00 80 #x00 24)
                                         (list +command-iac+ +command-se+)))
                          connection))
         (t (write-sequence (vector +command-iac+ +command-wont+ option)
                            connection)))))
    (#.+command-dont+
     (write-sequence (vector +command-iac+ +command-wont+
                             (read-byte connection))
                     connection))
    (#.+command-will+
     (read-byte connection)
     #+nil(write-sequence (vector +command-iac+ +command-wont+
                                  (read-byte connection))))
    (#.+command-wont+
     (read-byte connection)
     #+nil(write-sequence (vector +command-iac+ +command-wont+
                                  (read-byte connection))))))

(defun telnet-rx (telnet)
  (handler-case
      (with-simple-restart (abort "Give up")
        (sys.int::process-wait "Awaiting connection" (lambda () (telnet-connection telnet)))
        ;; Announce capabilities.
        (write-sequence #(#.+command-iac+ #.+command-do+ #.+option-suppress-go-ahead+
                          #.+command-iac+ #.+command-will+ #.+option-window-size+)
                        (telnet-connection telnet))
        (let ((last-was-cr nil))
          (loop (let ((byte (read-byte (telnet-connection telnet))))
                  (cond ((eql byte +command-iac+)
                         (let ((command (read-byte (telnet-connection telnet))))
                           (if (eql command +command-iac+)
                               (write-byte +command-iac+ (telnet-terminal telnet))
                               (telnet-command telnet (telnet-connection telnet) command))))
                        ((eql byte #x0D) ; CR
                         (setf last-was-cr t)
                         (write-byte byte (telnet-terminal telnet)))
                        ((and last-was-cr (eql byte #x00))
                         (setf last-was-cr nil))
                        (t (setf last-was-cr nil)
                           (write-byte byte (telnet-terminal telnet)))))
             (setf sys.graphics::*refresh-required* t))))
    (end-of-file ()
      (let ((msg "Connection closed by remote host."))
        (dotimes (i (length msg))
          (write-byte (char-code (char msg i)) (telnet-terminal telnet))))
      (setf sys.graphics::*refresh-required* t))))

(defun telnet-top-level (window)
  (multiple-value-bind (left right top bottom)
      (sys.graphics::compute-window-margins window)
    (unwind-protect
         (let* ((fb (sys.graphics::window-frontbuffer window))
                (dims (array-dimensions fb))
                (terminal (make-instance 'sys.xterm:xterm-terminal
                                         :framebuffer fb
                                         :x left
                                         :y top
                                         :width (- (array-dimension (sys.graphics::window-backbuffer window) 1)
                                                   left right)
                                         :height (- (array-dimension (sys.graphics::window-backbuffer window) 0)
                                                    top bottom)
                                         :input window
                                         :interrupt-character (name-char "C-["))))
           (setf (slot-value window 'terminal) terminal)
           (setf sys.graphics::*refresh-required* t)
           (with-open-stream (connection (sys.net::tcp-stream-connect (telnet-server window) (telnet-port window)))
             (setf (telnet-connection window) connection)
             (handler-case
                 (loop (let ((byte (read-byte terminal)))
                         (when (eql byte +command-iac+)
                           (write-byte +command-iac+ connection))
                         (write-byte byte connection)))
               (terminal-interrupt ()))))
      (sys.graphics::close-window window)
      (sys.int::process-disable (receive-process window)))))

(defclass telnet-client (sys.gray:fundamental-character-input-stream
                         sys.gray:fundamental-character-output-stream
                         sys.gray:unread-char-mixin
                         sys.graphics::window-with-chrome)
  ((command-process :reader command-process)
   (receive-process :reader receive-process)
   (buffer :initarg :buffer :reader window-buffer)
   (connection :initform nil :accessor telnet-connection)
   (terminal :reader telnet-terminal)
   (server :initarg :server :reader telnet-server)
   (port :initarg :port :reader telnet-port)
   (terminal-type :initarg :terminal-type :reader telnet-terminal-type))
  (:default-initargs :buffer (sys.graphics::make-fifo 500 'character)))

(defmethod sys.gray:stream-read-char ((stream telnet-client))
  (loop
     (let ((char (sys.graphics::fifo-pop (window-buffer stream))))
       (when char (return char)))
     (sys.int::process-wait "User input"
                            (lambda ()
                              (not (sys.graphics::fifo-emptyp (window-buffer stream)))))))

(defmethod sys.graphics::key-press-event ((window telnet-client) character)
  (sys.graphics::fifo-push character (window-buffer window)))

(defmethod sys.graphics::window-close-event ((window telnet-client))
  (sys.int::process-disable (command-process window))
  (sys.int::process-disable (receive-process window))
  (when (telnet-connection window)
    (close (telnet-connection window)))
  (sys.graphics::close-window window))

(defmethod initialize-instance :after ((instance telnet-client))
  (let ((cmd (sys.int::make-process "Telnet command"))
        (rcv (sys.int::make-process "Telnet receive")))
    (setf (slot-value instance 'command-process) cmd)
    (setf (slot-value instance 'receive-process) rcv)
    (sys.int::process-preset cmd 'telnet-top-level instance)
    (sys.int::process-preset rcv 'telnet-rx instance)
    (sys.int::process-enable cmd)
    (sys.int::process-enable rcv)))

(defmethod sys.graphics::window-redraw ((window telnet-client)))

(defun create-telnet-client (server &key (port 23) title (terminal-type "xterm-color"))
  "Open a telnet window."
  (sys.graphics::window-set-visibility (sys.graphics::make-window (if title (format nil "Telnet - ~A" title) "Telnet")
                                                                  640 400
                                                                  'telnet-client
                                                                  :server server
                                                                  :port port
                                                                  :terminal-type terminal-type) t))

(setf (gethash (name-char "F3") sys.graphics::*global-keybindings*)
      (lambda () (create-telnet-client '(204 236 130 210) :title "nethack.alt.org")))
(setf (gethash (name-char "F7") sys.graphics::*global-keybindings*)
      ;; This server is dumb and treats xterm-color as xterm-256color. Jerk.
      (lambda () (create-telnet-client '(128 174 251 59) :title "nyancat")))
(setf (gethash (name-char "F8") sys.graphics::*global-keybindings*)
      (lambda () (create-telnet-client '(173 164 225 201) :title "magic-1.org")))
