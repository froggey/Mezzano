(defpackage #:telnet
  (:use #:cl))

(in-package #:telnet)

(defmacro with-process ((name function &rest arguments) &body body)
  (let ((x (gensym)))
    `(let ((,x (make-instance 'sys.int::process :name ,name)))
       (unwind-protect (progn
                         (sys.int::process-preset ,x ,function ,@arguments)
                         (sys.int::process-enable ,x)
                         ,@body)
         (sys.int::process-disable ,x)))))

(defmacro with-saved-screen (options &body body)
  `(%with-saved-screen (lambda () ,@body) ,@options))

(defun framebuffer-from-stream (stream)
  (when (typep stream 'sys.int::shadow-stream)
    (setf stream (sys.int::shadow-stream-primary stream)))
  (when (typep stream 'sys.int::framebuffer-stream)
    (slot-value stream 'sys.int::framebuffer)))

(defun %with-saved-screen (fn)
  (let ((fb (framebuffer-from-stream *terminal-io*)))
    (if fb
        (let* ((dims (array-dimensions fb))
               (position (multiple-value-list (sys.int::stream-cursor-pos *terminal-io*)))
               (back-buffer (make-array (array-dimensions fb)
                                        :element-type (array-element-type fb))))
          (sys.int::%bitblt (first dims) (second dims)
                            fb 0 0
                            back-buffer 0 0)
          (unwind-protect
               (funcall fn)
            (apply 'sys.int::stream-move-to *terminal-io* position)
            (sys.int::%bitblt (first dims) (second dims)
                              back-buffer 0 0
                              fb 0 0)))
        (funcall fn))))

(defun parse-integer (string &key (start 0) end (radix 10))
  (setf end (or end (length string)))
  (let ((negativep nil)
        (n 0))
    ;; Eat leading/trailing whitespace.
    (do () ((or (>= start end)
                (and (not (member (char string start) '(#\Space #\Newline #\Tab))))))
      (incf start))
    (when (>= start end)
      (error "No non-whitespace characters in ~S." string))
    (cond ((eql (char string start) #\+)
           (incf start))
          ((eql (char string start) #\-)
           (setf negativep t)
           (incf start)))
    (do ((offset start (1+ offset)))
        ((or (>= offset end)
             (member (char string offset) '(#\Space #\Newline #\Tab))))
      (let ((weight (digit-char-p (char string offset) radix)))
        (when (not weight)
          (error "Not a parseable integer ~S." string))
        (setf n (+ (* n radix) weight))))
    (when negativep
      (setf n (- n)))
    (values n end)))

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
(defconstant +option-terminal-speed+ 32)
(defconstant +option-terminal-type+ 24)
(defconstant +option-x-display-location+ 35)
(defconstant +option-new-environ+ 39)

(defconstant +subnegotiation-is+ 0)
(defconstant +subnegotiation-send+ 1)
(defconstant +subnegotiation-info+ 2)

(define-condition terminal-interrupt () ())

(defclass xterm-terminal (sys.int::stream-object)
  ((framebuffer :initarg :framebuffer :reader terminal-framebuffer)
   (input :initarg :input)
   (queued-bytes :initarg :queued-bytes)
   (interrupt-character :initarg :interrupt-character :accessor interrupt-character)
   (state :initarg :state :accessor terminal-state)
   (current-number)
   (parameters)
   (escape-sequence)
   (width :reader terminal-width)
   (height :reader terminal-height)
   (x-pos :initform 0 :accessor x-pos)
   (y-pos :initform 0 :accessor y-pos))
  (:default-initargs
   :interrupt-character nil
   :queued-bytes '()
   :state nil))

(defmethod initialize-instance :after ((instance xterm-terminal))
  (let* ((fb (terminal-framebuffer instance))
         (dims (array-dimensions fb)))
    (setf (slot-value instance 'width) (truncate (second dims) 8)
          (slot-value instance 'height) (truncate (second dims) 16))))

(defun give-up-parsing-escape (term)
  (format t "Failed to parse escape sequence ~S in state ~S.~%"
          (mapcar 'code-char (nreverse (slot-value term 'escape-sequence)))
          (terminal-state term))
  (setf (terminal-state term) nil))

(defun finish-parsing-escape (term)
  (setf (terminal-state term) nil))

(defun write-terminal (term char)
  (let ((x (x-pos term)) (y (y-pos term)))
    (when (and (<= 0 x (1- (terminal-width term)))
               (<= 0 y (1- (terminal-height term))))
      (if (eql char #\Space)
          (sys.int::%bitset 16 8 #xFF000000 (terminal-framebuffer term) (* y 16) (* x 8))
          (sys.int::render-char-at char (terminal-framebuffer term) (* x 8) (* y 16))))
    (incf (x-pos term))))

(defun clamp (value min max)
  (cond ((> value max) max)
        ((< value min) min)
        (t value)))

(defun clear (term top bottom)
  (setf top (clamp top 0 (terminal-height term)))
  (setf bottom (clamp bottom 0 (terminal-height term)))
  (when (< top bottom)
    (sys.int::%bitset (* (- bottom top) 16) (* (terminal-width term) 8)
                      #xFF000000 (terminal-framebuffer term)
                      (* top 16) 0)))

(defun erase (term left right)
  (setf left (clamp left 0 (terminal-width term)))
  (setf right (clamp right 0 (terminal-width term)))
  (when (and (<= 0 (y-pos term) (1- (terminal-height term)))
             (< left right))
    (sys.int::%bitset 16 (* (- right left) 8)
                      #xFF000000 (terminal-framebuffer term)
                      (* (y-pos term) 16) (* left 8))))

(defmethod sys.int::stream-write-byte (byte (stream xterm-terminal))
  (ecase (terminal-state stream)
    ((nil)
     (case (code-char byte)
       (#\Escape
        (setf (terminal-state stream) :saw-escape
              (slot-value stream 'current-number) nil
              (slot-value stream 'parameters) '()
              (slot-value stream 'escape-sequence) '()))
       (#\Cr
        (setf (x-pos stream) 0))
       (#\Lf
        (incf (y-pos stream)))
       (#\Bs
        (decf (x-pos stream)))
       (t (write-terminal stream (code-char byte)))))
    (:saw-escape
     (push byte (slot-value stream 'escape-sequence))
     ;; Saw escape byte.
     (case (code-char byte)
       (#\[ (setf (terminal-state stream) :saw-bracket))
       (#\( (setf (terminal-state stream) :saw-paren))
       (#\> (finish-parsing-escape stream))
       (#\= (finish-parsing-escape stream))
       (#\M (finish-parsing-escape stream)) ; Reverse index?
       (t (give-up-parsing-escape stream))))
    (:saw-bracket ;; Saw "\e[" and maybe some digits and semicolons.
     (push byte (slot-value stream 'escape-sequence))
     (cond ((eql (code-char byte) #\;)
            (cond ((slot-value stream 'current-number)
                   (push (slot-value stream 'current-number) (slot-value stream 'parameters))
                   (setf (slot-value stream 'current-number) nil))
                  (t (push 0 (slot-value stream 'parameters)))))
           ((digit-char-p (code-char byte))
            (unless (slot-value stream 'current-number)
              (setf (slot-value stream 'current-number) 0))
            (setf (slot-value stream 'current-number) (+ (* (slot-value stream 'current-number) 10)
                                                         (- byte (char-code #\0)))))
           (t (when (slot-value stream 'current-number)
                (push (slot-value stream 'current-number) (slot-value stream 'parameters))
                (setf (slot-value stream 'current-number) nil))
              (case (code-char byte)
                (#\A
                 (decf (y-pos stream) (or (first (slot-value stream 'parameters)) 1))
                 (finish-parsing-escape stream))
                (#\B
                 (incf (y-pos stream) (or (first (slot-value stream 'parameters)) 1))
                 (finish-parsing-escape stream))
                (#\C
                 (incf (x-pos stream) (or (first (slot-value stream 'parameters)) 1))
                 (finish-parsing-escape stream))
                (#\D
                 (decf (x-pos stream) (or (first (slot-value stream 'parameters)) 1))
                 (finish-parsing-escape stream))
                (#\H
                 (let* ((params (nreverse (slot-value stream 'parameters)))
                        (row (or (first params) 1))
                        (column (or (second params) 1)))
                   (setf (y-pos stream) (1- row)
                         (x-pos stream) (1- column)))
                 (finish-parsing-escape stream))
                (#\G
                 (setf (x-pos stream) (1- (or (first (slot-value stream 'parameters)) 1)))
                 (finish-parsing-escape stream))
                (#\J
                 (case (first (slot-value stream 'parameters))
                   ((0 nil) (clear stream 0 (y-pos stream))) ; clear above
                   (1 (clear stream (y-pos stream) (terminal-height stream))) ; clear below
                   (2 (clear stream 0 (terminal-height stream)))
                   (t (give-up-parsing-escape stream)))
                 (finish-parsing-escape stream))
                (#\K
                 (case (first (slot-value stream 'parameters))
                   ((0 nil) (erase stream (x-pos stream) (terminal-width stream))) ; erase to right
                   (1 (erase stream 0 (x-pos stream))) ; erase to left
                   (2 (erase stream 0 (terminal-width stream))) ; erase all
                   (t (give-up-parsing-escape stream)))
                 (finish-parsing-escape stream))
                (#\d
                 (setf (y-pos stream) (1- (or (first (slot-value stream 'parameters)) 1)))
                 (finish-parsing-escape stream))
                (#\m (finish-parsing-escape stream)) ; character attributes **
                (#\r (finish-parsing-escape stream)) ; set scroll region. **
                (#\h (finish-parsing-escape stream)) ; set mode **
                (#\l (finish-parsing-escape stream)) ; clear mode **
                (#\?
                 (if (or (slot-value stream 'parameters)
                         (slot-value stream 'current-number))
                     (give-up-parsing-escape stream)
                     (setf (terminal-state stream) :saw-bracket-question)))
                (t (give-up-parsing-escape stream))))))
    (:saw-bracket-question
     ;; Saw "\e[?"
     (push byte (slot-value stream 'escape-sequence))
     (cond ((eql (code-char byte) #\;)
            (cond ((slot-value stream 'current-number)
                   (push (slot-value stream 'current-number) (slot-value stream 'parameters))
                   (setf (slot-value stream 'current-number) nil))
                  (t (push 0 (slot-value stream 'parameters)))))
           ((digit-char-p (code-char byte))
            (unless (slot-value stream 'current-number)
              (setf (slot-value stream 'current-number) 0))
            (setf (slot-value stream 'current-number) (+ (* (slot-value stream 'current-number) 10)
                                                         (- byte (char-code #\0)))))
           (t (when (slot-value stream 'current-number)
                (push (slot-value stream 'current-number) (slot-value stream 'parameters))
                (setf (slot-value stream 'current-number) nil))
              (case (code-char byte)
                (#\h (finish-parsing-escape stream)) ; DEC private mode set **
                (#\l (finish-parsing-escape stream)) ; DEC private mode clear **
                (t (give-up-parsing-escape stream))))))
    (:saw-paren
     ;; Saw "\e(", expecting a character set identifier. **
     (push byte (slot-value stream 'escape-sequence))
     (finish-parsing-escape stream))))

(defvar *xterm-translations*
  (list (list (name-char "Up-Arrow")    '(#\Esc #\[ #\A))
        (list (name-char "Down-Arrow")  '(#\Esc #\[ #\B))
        (list (name-char "Right-Arrow") '(#\Esc #\[ #\C))
        (list (name-char "Left-Arrow")  '(#\Esc #\[ #\D))
        (list (name-char "Home")        '(#\Esc #\[ #\H))
        (list (name-char "End")         '(#\Esc #\[ #\F))
        (list (name-char "KP-Multiply") '(#\*))
        (list (name-char "KP-Divide")   '(#\/))
        (list (name-char "KP-Plus")     '(#\+))
        (list (name-char "KP-Minus")    '(#\-))
        (list (name-char "KP-Period")   '(#\.))
        (list (name-char "KP-0")        '(#\0))
        (list (name-char "KP-1")        '(#\1))
        (list (name-char "KP-2")        '(#\2))
        (list (name-char "KP-3")        '(#\3))
        (list (name-char "KP-4")        '(#\4))
        (list (name-char "KP-5")        '(#\5))
        (list (name-char "KP-6")        '(#\6))
        (list (name-char "KP-7")        '(#\7))
        (list (name-char "KP-8")        '(#\8))
        (list (name-char "KP-9")        '(#\9))))

(defmethod sys.int::stream-read-byte ((stream xterm-terminal))
  (cond ((slot-value stream 'queued-bytes)
         (pop (slot-value stream 'queued-bytes)))
        (t (let ((ch (read-char (slot-value stream 'input))))
             (when (eql ch (interrupt-character stream))
               (signal 'terminal-interrupt))
             (cond ((logtest (system:char-bits ch)
                             (logior sys.int::+char-meta-bit+
                                     sys.int::+char-super-bit+
                                     sys.int::+char-hyper-bit+))
                    ;; Ignore weird characters.
                    (stream-read-byte stream))
                   ((logtest (system:char-bits ch) sys.int::+char-control-bit+)
                    ;; Control character. Translate to ASCII.
                    (if (<= #x40 (char-code ch) #x7E)
                        (logxor (logand (char-code ch) #b01011111) #b00100000)
                        (stream-read-byte stream)))
                   (t ;; TODO: UTF-8 translation, etc.
                    (let ((translated (assoc ch *xterm-translations*)))
                      (cond (translated
                             (dolist (c (rest (second translated)))
                               (push (char-code c) (slot-value stream 'queued-bytes)))
                             (char-code (first (second translated))))
                            (t (logand (char-code ch) #xFF))))))))))

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

(defun telnet-command (connection command)
  (case command
    (#.+command-sb+
     (let ((option (read-byte connection))
           (data (read-subnegotiation connection)))
       (case option
         (#.+option-terminal-type+
          (write-sequence (vector +command-iac+ +command-sb+ +option-terminal-type+ +subnegotiation-is+
                                  (char-code #\x) (char-code #\t) (char-code #\e) (char-code #\r) (char-code #\m)
                                  +command-iac+ +command-se+)
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

(defun telnet-rx (connection terminal)
  (handler-case
      (with-simple-restart (abort "Give up")
        ;; Announce capabilities.
        (write-sequence #(#.+command-iac+ #.+command-do+ #.+option-suppress-go-ahead+)
                        connection)
        (let ((last-was-cr nil))
          (loop (let ((byte (read-byte connection)))
                  (cond ((eql byte +command-iac+)
                         (let ((command (read-byte connection)))
                           (if (eql command +command-iac+)
                               (write-byte +command-iac+ terminal)
                               (telnet-command connection command))))
                        ((eql byte #x0D) ; CR
                         (setf last-was-cr t)
                         (write-byte byte terminal))
                        ((and last-was-cr (eql byte #x00))
                         (setf last-was-cr nil))
                        (t (setf last-was-cr nil)
                           (write-byte byte terminal)))))))
    (end-of-file ()
      (let ((msg "Connection closed by remote host. C-[ to exit."))
        (dotimes (i (length msg))
          (write-byte (char-code (char msg i)) terminal))))))

(defun telnet (&optional (server '(204 236 130 210)) (port 23))
  (with-saved-screen ()
    (let* ((fb (framebuffer-from-stream *terminal-io*))
           (dims (array-dimensions fb))
           (terminal (make-instance 'xterm-terminal
                                    :framebuffer fb
                                    ;; Avoid the edit & echo behaviour of the framebuffer stream.
                                    :input (make-instance 'sys.int::ps/2-keyboard-stream)
                                    :interrupt-character (name-char "C-["))))
      (sys.int::%bitset (first dims) (second dims) 0 fb 0 0)
      (with-open-stream (connection (sys.net::tcp-stream-connect server port))
        (with-process ("TELNET receiver" #'telnet-rx connection terminal)
          (handler-case
              (loop (let ((byte (read-byte terminal)))
                      (when (eql byte +command-iac+)
                        (write-byte +command-iac+ connection))
                      (write-byte byte connection)))
            (terminal-interrupt ())))))))
