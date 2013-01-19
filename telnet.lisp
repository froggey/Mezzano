(defpackage #:telnet
  (:use #:cl))

(in-package #:telnet)

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
   ;; Offsets in the framebuffer.
   (x-offs :initarg :x :reader x-offset)
   (y-offs :initarg :y :reader y-offset)
   (input :initarg :input)
   (queued-bytes :initarg :queued-bytes)
   (interrupt-character :initarg :interrupt-character :accessor interrupt-character)
   (state :initform 'xterm-initial :accessor terminal-state)
   (current-number :initform nil)
   (parameters :initform '())
   (escape-sequence :initform '())
   (width :reader terminal-width)
   (height :reader terminal-height)
   (x-pos :initform 0 :accessor x-pos)
   (y-pos :initform 0 :accessor y-pos)
   (foreground :initarg :foreground :accessor foreground-colour)
   (background :initarg :background :accessor background-colour)
   (bold :initarg :bold :accessor bold)
   (inverse :initarg :inverse :accessor inverse)
   (underline :initarg :underline :accessor underline)
   (character-set :initarg :charset :accessor charset))
  (:default-initargs
   :interrupt-character nil
   :queued-bytes '()
   :foreground nil
   :background nil
   :bold nil
   :inverse nil
   :underline nil
   :charset :usascii
   :x 0 :y 0))

(defparameter *xterm-colours*
  #(#x000000 ; 0 Black
    #x800000 ; 1 Red
    #x008000 ; 2 Green
    #x808000 ; 3 Yellow
    #x000080 ; 4 Blue
    #x800080 ; 5 Magenta
    #x008080 ; 6 Cyan
    #xC0C0C0 ; 7 "White"
    #x808080 ; 0 Bright Black
    #xFF0000 ; 1 Bright Red
    #x00FF00 ; 2 Bright Green
    #xFFFF00 ; 3 Bright Yellow
    #x0000FF ; 4 Bright Blue
    #xFF00FF ; 5 Bright Magenta
    #x00FFFF ; 6 Bright Cyan
    #xFFFFFF ; 7 Bright White
    ))

(defparameter *dec-special-characters-and-line-drawing*
  #(#\BLACK_LOZENGE
    #\MEDIUM_SHADE
    #\Tab
    #\u000C ; form feed
    #\Return
    #\Newline
    #\DEGREE_SIGN
    #\PLUS-MINUS_SIGN
    #\Newline
    #\u000B ; vertical tab
    #\BOX_DRAWINGS_LIGHT_UP_AND_LEFT
    #\BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT
    #\BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT
    #\BOX_DRAWINGS_LIGHT_UP_AND_RIGHT
    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL
    #\Space ; "SCAN 1"?
    #\Space ; "SCAN 3"?
    #\BOX_DRAWINGS_LIGHT_HORIZONTAL ; "SCAN 5"?
    #\Space ; "SCAN 7"?
    #\Space ; "SCAN 9"?
    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT
    #\BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL
    #\BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL
    #\BOX_DRAWINGS_LIGHT_VERTICAL
    #\LESS-THAN_OR_SLANTED_EQUAL_TO
    #\GREATER-THAN_OR_SLANTED_EQUAL_TO
    #\GREEK_SMALL_LETTER_PI
    #\NOT_EQUAL_TO
    #\£
    #\MIDDLE_DOT))

(defun true-foreground-colour (terminal)
  (let ((colour (or (if (inverse terminal)
                        (background-colour terminal)
                        (foreground-colour terminal))
                    (if (inverse terminal) 0 7))))
    (when (and (bold terminal)
               (not (inverse terminal))
               (< colour 8))
      (incf colour 8))
    (logior #xFF000000
            (elt *xterm-colours* colour))))

(defun true-background-colour (terminal)
  (logior #xFF000000
          (elt *xterm-colours* (or (if (inverse terminal)
                                       (foreground-colour terminal)
                                       (background-colour terminal))
                                   (if (inverse terminal) 7 0)))))

(defmethod initialize-instance :after ((term xterm-terminal) &key width height)
  (let* ((fb (terminal-framebuffer term))
         (dims (array-dimensions fb)))
    (setf (slot-value term 'width) (truncate (or width (- (second dims) (x-offset term))) 8)
          (slot-value term 'height) (truncate (or height (- (first dims) (y-offset term))) 16))
    (sys.graphics::bitset (* (terminal-height term) 16) (* (terminal-width term) 8)
                          (true-background-colour term) fb
                          (y-offset term) (x-offset term))))

(defun report-unknown-escape (term)
  (format t "Failed to parse escape sequence ~S in state ~S.~%"
          (mapcar 'code-char (nreverse (slot-value term 'escape-sequence)))
          (terminal-state term)))


(defun write-terminal (term char)
  (let ((x (x-pos term)) (y (y-pos term)))
    (when (and (<= 0 x (1- (terminal-width term)))
               (<= 0 y (1- (terminal-height term))))
      (sys.graphics::bitset 16 8
                            (true-background-colour term) (terminal-framebuffer term)
                            (+ (* y 16) (y-offset term)) (+ (* x 8) (x-offset term)))
      (unless (eql char #\Space)
        (let* ((glyph (sys.int::map-unifont-2d char)))
          (when glyph
            (sys.graphics::bitset-argb-xrgb-mask-1 16 8 (true-foreground-colour term)
                                                   glyph 0 0
                                                   (terminal-framebuffer term)
                                                   (+ (* y 16) (y-offset term))
                                                   (+ (* x 8) (x-offset term)))
            (when (bold term)
              (sys.graphics::bitset-argb-xrgb-mask-1 16 7 (true-foreground-colour term)
                                                   glyph 0 0
                                                   (terminal-framebuffer term)
                                                   (+ (* y 16) (y-offset term))
                                                   (+ (* x 8) (x-offset term) 1))))))
      (when (underline term)
        (sys.graphics::bitset 1 8 (true-foreground-colour term) (terminal-framebuffer term)
                              (+ (* y 16) (y-offset term)) (+ (* x 8) (x-offset term) 7))))
    (incf (x-pos term))))

(defun clamp (value min max)
  (cond ((> value max) max)
        ((< value min) min)
        (t value)))

(defun clear (term top bottom)
  (setf top (clamp top 0 (terminal-height term)))
  (setf bottom (clamp bottom 0 (terminal-height term)))
  (when (< top bottom)
    (sys.graphics::bitset (* (- bottom top) 16) (* (terminal-width term) 8)
                          (true-background-colour term) (terminal-framebuffer term)
                          (+ (* top 16) (y-offset term)) (x-offset term))))

(defun erase (term left right)
  (setf left (clamp left 0 (terminal-width term)))
  (setf right (clamp right 0 (terminal-width term)))
  (when (and (<= 0 (y-pos term) (1- (terminal-height term)))
             (< left right))
    (sys.graphics::bitset 16 (* (- right left) 8)
                          (true-background-colour term) (terminal-framebuffer term)
                          (+ (* (y-pos term) 16) (y-offset term)) (+ (* left 8) (x-offset term)))))

(defun set-character-attributes (terminal attributes)
  (when (endp attributes) (setf attributes '(0)))
  (dolist (attr attributes)
    (case attr
      (0 ;; Normal (reset to defaults).
       (setf (bold terminal) nil
             (inverse terminal) nil
             (underline terminal) nil
             (foreground-colour terminal) nil
             (background-colour terminal) nil))
      (1 ;; Bold.
       (setf (bold terminal) t))
      (4 ;; Underlined
       (setf (underline terminal) t))
      (5 ;; Blink.
       (setf (bold terminal) t))
      (7 ;; Inverse.
       (setf (inverse terminal) t))
      (8 ;; Invisible.
       )
      (22 ;; Not bold.
       (setf (bold terminal) nil))
      (24 ;; Not underlined.
       (setf (underline terminal) nil))
      (25 ;; Not blink.
       )
      (27 ;; Not inverse.
       (setf (inverse terminal) nil))
      (28 ;; Not invisible.
       )
      (39 ;; Set foreground to default
       (setf (foreground-colour terminal) nil))
      (49 ;; Set background to default
       (setf (background-colour terminal) nil))
      ((2 3 6 9 38 38)) ; ???
      (t (cond
           ((<= 30 attr 37)
            (setf (foreground-colour terminal) (- attr 30)))
           ((<= 40 attr 47)
            (setf (background-colour terminal) (- attr 40)))
           ((<= 90 attr 97)
            (setf (foreground-colour terminal) (+ 8 (- attr 90))))
           ((<= 100 attr 107)
            (setf (background-colour terminal) (+ 8 (- attr 100))))
           (t (report-unknown-escape terminal)))))))

(defun xterm-initial (terminal byte)
  "Initial state."
  (case (code-char byte)
    (#\Escape
     (return-from xterm-initial
       'xterm-saw-escape))
    (#\Cr
     (setf (x-pos terminal) 0))
    (#\Lf
     (incf (y-pos terminal)))
    (#\Bs
     (decf (x-pos terminal)))
    (t (let ((char (ecase (charset terminal)
                     (:usascii (code-char byte)) ; No translation.
                     (:dec ;; Translate some characters.
                      (if (<= #x60 byte #x7E)
                          (aref *dec-special-characters-and-line-drawing*
                                (- byte #x60))
                          (code-char byte))))))
         (write-terminal terminal char))))
  nil)

(defun xterm-saw-escape (terminal byte)
  "Saw escape byte."
  (case (code-char byte)
    (#\[ 'xterm-saw-bracket)
    (#\( 'xterm-saw-paren)
    (#\> nil) ; ???
    (#\= nil) ; ???
    (#\M nil) ; Reverse index?
    (t (report-unknown-escape terminal) nil)))

(defun xterm-saw-bracket (terminal byte)
  "Saw '<Esc>[' and maybe some digits and semicolons."
  (cond ((eql (code-char byte) #\;)
         (cond ((slot-value terminal 'current-number)
                (push (slot-value terminal 'current-number) (slot-value terminal 'parameters))
                (setf (slot-value terminal 'current-number) nil))
               (t (push 0 (slot-value terminal 'parameters))))
         (return-from xterm-saw-bracket 'xterm-saw-bracket))
        ((digit-char-p (code-char byte))
         (unless (slot-value terminal 'current-number)
           (setf (slot-value terminal 'current-number) 0))
         (setf (slot-value terminal 'current-number) (+ (* (slot-value terminal 'current-number) 10)
                                                        (- byte (char-code #\0))))
         (return-from xterm-saw-bracket 'xterm-saw-bracket))
        (t (when (slot-value terminal 'current-number)
             (push (slot-value terminal 'current-number) (slot-value terminal 'parameters))
             (setf (slot-value terminal 'current-number) nil))
           (case (code-char byte)
             (#\A
              (decf (y-pos terminal) (or (first (slot-value terminal 'parameters)) 1)))
             (#\B
              (incf (y-pos terminal) (or (first (slot-value terminal 'parameters)) 1)))
             (#\C
              (incf (x-pos terminal) (or (first (slot-value terminal 'parameters)) 1)))
             (#\D
              (decf (x-pos terminal) (or (first (slot-value terminal 'parameters)) 1)))
             (#\H
              (let* ((params (nreverse (slot-value terminal 'parameters)))
                     (row (or (first params) 1))
                     (column (or (second params) 1)))
                (setf (y-pos terminal) (1- row)
                      (x-pos terminal) (1- column))))
             (#\G
              (setf (x-pos terminal) (1- (or (first (slot-value terminal 'parameters)) 1))))
             (#\J
              (case (first (slot-value terminal 'parameters))
                ((0 nil) (clear terminal 0 (y-pos terminal))) ; clear above
                (1 (clear terminal (y-pos terminal) (terminal-height terminal))) ; clear below
                (2 (clear terminal 0 (terminal-height terminal)))
                (t (report-unknown-escape terminal))))
             (#\K
              (case (first (slot-value terminal 'parameters))
                ((0 nil) (erase terminal (x-pos terminal) (terminal-width terminal))) ; erase to right
                (1 (erase terminal 0 (x-pos terminal))) ; erase to left
                (2 (erase terminal 0 (terminal-width terminal))) ; erase all
                (t (report-unknown-escape terminal))))
             (#\d
              (setf (y-pos terminal) (1- (or (first (slot-value terminal 'parameters)) 1))))
             (#\m (set-character-attributes terminal (nreverse (slot-value terminal 'parameters))))
             (#\r) ; set scroll region. **
             (#\h) ; set mode **
             (#\l) ; clear mode **
             (#\?
              (if (or (slot-value terminal 'parameters)
                      (slot-value terminal 'current-number))
                  (report-unknown-escape terminal)
                  (return-from xterm-saw-bracket 'xterm-saw-bracket-question)))
             (t (report-unknown-escape terminal)))))
  nil)

(defun xterm-saw-bracket-question (terminal byte)
  "Saw '<Esc>[?'"
  (cond ((eql (code-char byte) #\;)
         (cond ((slot-value terminal 'current-number)
                (push (slot-value terminal 'current-number) (slot-value terminal 'parameters))
                (setf (slot-value terminal 'current-number) nil))
               (t (push 0 (slot-value terminal 'parameters))))
         'xterm-saw-bracket-question)
        ((digit-char-p (code-char byte))
         (unless (slot-value terminal 'current-number)
           (setf (slot-value terminal 'current-number) 0))
         (setf (slot-value terminal 'current-number) (+ (* (slot-value terminal 'current-number) 10)
                                                      (- byte (char-code #\0))))
         'xterm-saw-bracket-question)
        (t (when (slot-value terminal 'current-number)
             (push (slot-value terminal 'current-number) (slot-value terminal 'parameters))
             (setf (slot-value terminal 'current-number) nil))
           (case (code-char byte)
             (#\h) ; DEC private mode set **
             (#\l) ; DEC private mode clear **
             (t (report-unknown-escape terminal)))
           nil)))

(defun xterm-saw-paren (terminal byte)
  "Saw '<Esc>(', expecting a character set identifier."
  (case (code-char byte)
    (#\B (setf (charset terminal) :usascii))
    (#\0 (setf (charset terminal) :dec))
    (t (setf (charset terminal) :usascii)
       (report-unknown-escape terminal)))
  nil)

(defmethod sys.int::stream-write-byte (byte (stream xterm-terminal))
  (push byte (slot-value stream 'escape-sequence))
  (let ((new-state (funcall (terminal-state stream) stream byte)))
    (cond (new-state
           (setf (terminal-state stream) new-state))
          (t (setf (terminal-state stream) 'xterm-initial
                   (slot-value stream 'current-number) nil
                   (slot-value stream 'parameters) '()
                   (slot-value stream 'escape-sequence) '())))))

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
          (write-sequence (apply 'vector
                                 (append (list +command-iac+ +command-sb+ +option-terminal-type+ +subnegotiation-is+)
                                         (map 'list 'char-code "xterm-color")
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
        (write-sequence #(#.+command-iac+ #.+command-do+ #.+option-suppress-go-ahead+)
                        (telnet-connection telnet))
        (let ((last-was-cr nil))
          (loop (let ((byte (read-byte (telnet-connection telnet))))
                  (cond ((eql byte +command-iac+)
                         (let ((command (read-byte (telnet-connection telnet))))
                           (if (eql command +command-iac+)
                               (write-byte +command-iac+ (telnet-terminal telnet))
                               (telnet-command (telnet-connection telnet) command))))
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
                (terminal (make-instance 'xterm-terminal
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
           ;; Hard-code NAO for now.
           (with-open-stream (connection (sys.net::tcp-stream-connect '(204 236 130 210) 23))
             (setf (telnet-connection window) connection)
             (handler-case
                 (loop (let ((byte (read-byte terminal)))
                         (when (eql byte +command-iac+)
                           (write-byte +command-iac+ connection))
                         (write-byte byte connection)))
               (terminal-interrupt ()))))
      (sys.graphics::close-window window)
      (sys.int::process-disable (receive-process window)))))

(defclass telnet-client (sys.int::stream-object sys.graphics::window-with-chrome)
  ((command-process :reader command-process)
   (receive-process :reader receive-process)
   (buffer :initarg :buffer :reader window-buffer)
   (connection :initform nil :accessor telnet-connection)
   (terminal :reader telnet-terminal))
  (:default-initargs :buffer (sys.graphics::make-fifo 500 'character)))

(defmethod sys.int::stream-read-char ((stream telnet-client))
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

(defun create-telnet-client ()
  "Open a telnet window."
  (sys.graphics::window-set-visibility (sys.graphics::make-window "Telnet" 640 400 'telnet-client) t))

(setf (gethash (name-char "F3") sys.graphics::*global-keybindings*) 'create-telnet-client)
