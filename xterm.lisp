;;;; An XTerm emulator widget.

(cl:defpackage :sys.xterm
  (:use :cl)
  (:export #:xterm-terminal
           #:terminal-interrupt))

(cl:in-package :sys.xterm)

(define-condition terminal-interrupt () ())

(defclass xterm-terminal (sys.gray:fundamental-binary-input-stream
                          sys.gray:fundamental-binary-output-stream)
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
   (character-set :initarg :charset :accessor charset)
   (saved-x :initform 0 :accessor saved-x)
   (saved-y :initform 0 :accessor saved-y)
   (scroll-start :initarg :scroll-start :accessor scroll-start)
   (scroll-end :initarg :scroll-end :accessor scroll-end)
   (utf-8-state :initform nil :accessor utf-8-state)
   (utf-8-length :initform nil :accessor utf-8-length)
   (utf-8-accumulator :initform nil :accessor utf-8-accumulator)

   (autowrap :initarg :autowrap :accessor autowrap) ; DECAWM (7)
)
  (:default-initargs
   :interrupt-character nil
   :queued-bytes '()
   :foreground nil
   :background nil
   :bold nil
   :inverse nil
   :underline nil
   :charset :us-ascii
   :scroll-start 0
   :scroll-end nil
   :x 0 :y 0))

(defun soft-reset (terminal)
  "Reset the terminal to the default state."
  (setf (autowrap terminal) nil))

(defmethod sys.gray:stream-element-type ((stream xterm-terminal))
  '(unsigned-byte 8))

(defun generate-xterm-colour-table ()
  (let ((colours (make-array 256 :element-type '(unsigned-byte 32))))
    (setf (subseq colours 0) #(#x000000 ; 0 Black
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
    ;; 16-231 are a 6x6x6 colour cube.
    (dotimes (red 6)
      (dotimes (green 6)
        (dotimes (blue 6)
          (setf (aref colours (+ 16 (* red 36) (* green 6) blue))
                (logior (ash (if (zerop red)   0 (+ (* red   40) 55)) 16)
                        (ash (if (zerop green) 0 (+ (* green 40) 55)) 8)
                             (if (zerop blue)  0 (+ (* blue  40) 55)))))))
    ;; 232-255 area a greyscale ramp, leaving out black & white.
    (dotimes (grey 24)
      (let ((level (+ (* grey 10) 8)))
        (setf (aref colours (+ 232 grey))
              (logior (ash level 16)
                      (ash level 8)
                      level))))
    colours))

(defparameter *xterm-colours* (generate-xterm-colour-table))

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
    (soft-reset term)
    (sys.graphics::bitset (* (terminal-height term) 16) (* (terminal-width term) 8)
                          (true-background-colour term) fb
                          (y-offset term) (x-offset term))))

(defun report-unknown-escape (term)
  (format t "Failed to parse escape sequence ~S in state ~S.~%"
          (reverse (slot-value term 'escape-sequence))
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
                              (+ (* y 16) (y-offset term) 15) (+ (* x 8) (x-offset term)))))
    (incf (x-pos term))
    (when (autowrap term)
      (when (>= (x-pos term) (terminal-width term))
        (setf (x-pos term) 0)
        (incf (y-pos term)))
      (when (>= (y-pos term) (terminal-height term))
        (scroll-terminal term 0 (1- (terminal-height term)) 1)
        (setf (y-pos term) (1- (terminal-height term)))))))

(defun scroll-terminal (term rstart rend lines)
  (setf rstart (max 0 rstart)
        rend (min rend (1- (terminal-height term))))
  (when (> rstart rend)
    (return-from scroll-terminal))
  (let ((rsize (- rend rstart)))
    (cond ((> lines 0)
           ;; Screen goes up.
           (cond ((>= lines rsize)
                  ;; All the way up.
                  (sys.graphics::bitset (* rsize 16) (* (terminal-width term) 8)
                                        (true-background-colour term) (terminal-framebuffer term)
                                        (+ (* rstart 16) (y-offset term)) (x-offset term)))
                 (t (sys.graphics::bitblt (* (1+ (- rsize lines)) 16) (* (terminal-width term) 8)
                                          (terminal-framebuffer term)
                                          (+ (* (+ rstart lines) 16) (y-offset term))
                                          (x-offset term)
                                          (terminal-framebuffer term)
                                          (+ (* rstart 16) (y-offset term))
                                          (x-offset term))
                    (sys.graphics::bitset (* lines 16) (* (terminal-width term) 8)
                                          (true-background-colour term) (terminal-framebuffer term)
                                          (+ (* (1+ (- rend lines)) 16) (y-offset term)) (x-offset term)))))
          ((< lines 0)
           ;; Screen goes down.
           (setf lines (- lines))
           (cond ((>= lines rsize)
                  ;; All the way down.
                  (sys.graphics::bitset (* rsize 16) (* (terminal-width term) 8)
                                        (true-background-colour term) (terminal-framebuffer term)
                                        (+ (* rstart 16) (y-offset term)) (x-offset term)))
                 (t (sys.graphics::bitblt (* (1+ (- rsize lines)) 16) (* (terminal-width term) 8)
                                          (terminal-framebuffer term)
                                          (+ (* rstart 16) (y-offset term))
                                          (x-offset term)
                                          (terminal-framebuffer term)
                                          (+ (* (+ rstart lines) 16) (y-offset term))
                                          (x-offset term))
                    (sys.graphics::bitset (* lines 16) (* (terminal-width term) 8)
                                          (true-background-colour term) (terminal-framebuffer term)
                                          (+ (* rstart 16) (y-offset term)) (x-offset term))))))))

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
  (cond ((and (eql (first attributes) 38)
              (eql (second attributes) 5)
              (integerp (third attributes))
              (null (fourth attributes))
              (<= 0 (third attributes) 255))
         ;; \e[38;5;${N}m  Set foreground to 256-colour ${N}.
         (setf (foreground-colour terminal) (third attributes)))
        ((and (eql (first attributes) 48)
              (eql (second attributes) 5)
              (integerp (third attributes))
              (null (fourth attributes))
              (<= 0 (third attributes) 255))
         ;; \e[48;5;${N}m  Set background to 256-colour ${N}.
         (setf (background-colour terminal) (third attributes)))
        (t (dolist (attr attributes)
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
               ((2 3 6 9)) ; ???
               (t (cond
                    ((<= 30 attr 37)
                     (setf (foreground-colour terminal) (- attr 30)))
                    ((<= 40 attr 47)
                     (setf (background-colour terminal) (- attr 40)))
                    ((<= 90 attr 97)
                     (setf (foreground-colour terminal) (+ 8 (- attr 90))))
                    ((<= 100 attr 107)
                     (setf (background-colour terminal) (+ 8 (- attr 100))))
                    (t (report-unknown-escape terminal)))))))))

(defun xterm-initial (terminal char)
  "Initial state."
  (ecase (charset terminal)
    (:us-ascii) ; No translation.
    (:dec ;; Translate some characters.
     (when (<= #x60 (char-int char) #x7E)
       (setf char (aref *dec-special-characters-and-line-drawing*
                        (- (char-int char) #x60))))))
  (case char
    (#\Escape
     (return-from xterm-initial
       'xterm-saw-escape))
    (#\Cr
     (setf (x-pos terminal) 0))
    (#\Lf
     (cond ((eql (y-pos terminal)
                 (or (scroll-end terminal)
                     (terminal-height terminal)))
            (scroll-terminal terminal
                             (scroll-start terminal)
                             (or (scroll-end terminal)
                                 (terminal-height terminal))
                             1))
           (t (incf (y-pos terminal)))))
    (#\Bs
     (decf (x-pos terminal)))
    (t (write-terminal terminal char)))
  nil)

(defun save-cursor (terminal)
  (setf (saved-x terminal) (x-pos terminal)
        (saved-y terminal) (y-pos terminal)))

(defun restore-cursor (terminal)
  (setf (x-pos terminal) (saved-x terminal)
        (y-pos terminal) (saved-y terminal)))

(defun xterm-saw-escape (terminal char)
  "Saw escape byte."
  (case char
    (#\[ 'xterm-saw-bracket)
    (#\] 'xterm-saw-close-bracket)
    (#\( 'xterm-saw-paren)
    (#\) 'xterm-saw-close-paren)
    (#\> nil) ; DECKPNM
    (#\= nil) ; DECKPAM
    (#\M nil) ; Reverse index?
    ;; Save cursor position.
    (#\7 (save-cursor terminal)
         nil)
    ;; Restore cursor position.
    (#\8 (restore-cursor terminal)
         nil)
    (t (report-unknown-escape terminal) nil)))

(defun xterm-saw-bracket (terminal char)
  "Saw '<Esc>[' and maybe some digits and semicolons."
  (cond ((eql char #\;)
         (cond ((slot-value terminal 'current-number)
                (push (slot-value terminal 'current-number) (slot-value terminal 'parameters))
                (setf (slot-value terminal 'current-number) nil))
               (t (push 0 (slot-value terminal 'parameters))))
         (return-from xterm-saw-bracket 'xterm-saw-bracket))
        ((digit-char-p char)
         (unless (slot-value terminal 'current-number)
           (setf (slot-value terminal 'current-number) 0))
         (setf (slot-value terminal 'current-number) (+ (* (slot-value terminal 'current-number) 10)
                                                        (- (char-code char) (char-code #\0))))
         (return-from xterm-saw-bracket 'xterm-saw-bracket))
        (t (when (slot-value terminal 'current-number)
             (push (slot-value terminal 'current-number) (slot-value terminal 'parameters))
             (setf (slot-value terminal 'current-number) nil))
           (case char
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
                ((0 nil)
                 ;; clear below
                 (erase terminal (x-pos terminal) (terminal-width terminal))
                 (clear terminal (1+ (y-pos terminal)) (terminal-height terminal)))
                (1 (clear terminal 0 (y-pos terminal))) ; clear above
                (2 (clear terminal 0 (terminal-height terminal)))
                (t (report-unknown-escape terminal))))
             (#\K
              (case (first (slot-value terminal 'parameters))
                ((0 nil) (erase terminal (x-pos terminal) (terminal-width terminal))) ; erase to right
                (1 (erase terminal 0 (x-pos terminal))) ; erase to left
                (2 (erase terminal 0 (terminal-width terminal))) ; erase all
                (t (report-unknown-escape terminal))))
             (#\L
              ;; Insert Pn lines, starting at the cursor. Lines move down.
              (when (<= (scroll-start terminal)
                        (y-pos terminal)
                        (1- (or (scroll-end terminal)
                                (terminal-height terminal))))
                (scroll-terminal terminal
                                 (y-pos terminal)
                                 (or (scroll-end terminal)
                                     (terminal-height terminal))
                                 (- (or (first (slot-value terminal 'parameters)) 1)))))
             (#\M
              ;; Delete Pn lines, starting at the cursor. Lines move up.
              (when (<= (scroll-start terminal)
                        (y-pos terminal)
                        (1- (or (scroll-end terminal)
                                (terminal-height terminal))))
                  (scroll-terminal terminal
                                   (y-pos terminal)
                                   (or (scroll-end terminal)
                                       (terminal-height terminal))
                                   (or (first (slot-value terminal 'parameters)) 1))))
             (#\d
              (setf (y-pos terminal) (1- (or (first (slot-value terminal 'parameters)) 1))))
             (#\m (set-character-attributes terminal (nreverse (slot-value terminal 'parameters))))
             (#\r ; set scroll region.
              (let ((params (nreverse (slot-value terminal 'parameters))))
                (setf (scroll-start terminal) (1- (or (first params) 1))
                      (scroll-end terminal) (when (second params)
                                              (1- (second params))))
                (format t "Scroll region: ~D ~D~%" (scroll-start terminal) (scroll-end terminal))))
             #+nil(#\h) ; set mode **
             #+nil(#\l) ; clear mode **
             (#\!
              (if (or (slot-value terminal 'parameters)
                      (slot-value terminal 'current-number))
                  (report-unknown-escape terminal)
                  (return-from xterm-saw-bracket 'xterm-saw-bracket-exclaim)))
             (#\?
              (if (or (slot-value terminal 'parameters)
                      (slot-value terminal 'current-number))
                  (report-unknown-escape terminal)
                  (return-from xterm-saw-bracket 'xterm-saw-bracket-question)))
             (t (report-unknown-escape terminal)))))
  nil)

(defun adjust-dec-private-mode (terminal mode value)
  "Set a DEC private mode. '<Esc>[?Pn;Pm...h' or '<Esc>[?Pn;Pm...l'"
  (case mode
    (1 ;; DECCKM.
     ;; Send application sequences to the host when set, ANSI cursor sequences when reset.
     )
    (3 ;; DECCOLM.
     ;; Select 132 (true) or 80 (false) columns per page.
     ;; Don't care about the value now, because there's no way to resize.
     ;; Changing DECCOLM resets the scrolling margins, erases all data in page memory,
     ;; resets the vertical split mode (DECLRMM) to unavailable,
     ;; and clears data from the status line if set to host-writable.
     (setf (scroll-start terminal) 0
           (scroll-end terminal) nil)
     (clear terminal 0 (terminal-height terminal)))
    (4 ;; DECSCLM. Select scrolling mode.
     ;; Smooth scrolling (true, default) or jump scrolling (false).
     )
    (12 ;; att610. Start/stop blinking cursor.
     )
    (25 ;; DECTCEM. Show/hide cursor.
     )
    (1049 ;; Save or restore cursor & switch screens.
     (cond (value
            ;; Save the cursor. Switch to the alternate screen and clear it.
            (save-cursor terminal))
           (t ;; Restore the cursor and switch to the normal screen.
            (restore-cursor terminal))))
    (t (format t "Unsupported DEC private mode ~D.~%" mode))))

(defun xterm-saw-bracket-exclaim (terminal char)
  "Saw '<Esc>[!'"
  (case char
    (#\p ; DECSTR.
     (soft-reset terminal))
    (t (report-unknown-escape terminal)))
  nil)

(defun xterm-saw-bracket-question (terminal char)
  "Saw '<Esc>[?'"
  (cond ((eql char #\;)
         (cond ((slot-value terminal 'current-number)
                (push (slot-value terminal 'current-number) (slot-value terminal 'parameters))
                (setf (slot-value terminal 'current-number) nil))
               (t (push 0 (slot-value terminal 'parameters))))
         'xterm-saw-bracket-question)
        ((digit-char-p char)
         (unless (slot-value terminal 'current-number)
           (setf (slot-value terminal 'current-number) 0))
         (setf (slot-value terminal 'current-number) (+ (* (slot-value terminal 'current-number) 10)
                                                      (- (char-code char) (char-code #\0))))
         'xterm-saw-bracket-question)
        (t (when (slot-value terminal 'current-number)
             (push (slot-value terminal 'current-number) (slot-value terminal 'parameters))
             (setf (slot-value terminal 'current-number) nil))
           (case char
             (#\h
              (dolist (p (nreverse (slot-value terminal 'parameters)))
                (adjust-dec-private-mode terminal p t)))
             (#\l
              (dolist (p (nreverse (slot-value terminal 'parameters)))
                (adjust-dec-private-mode terminal p nil)))
             (t (report-unknown-escape terminal)))
           nil)))

(defun xterm-saw-close-bracket (terminal char)
  "Saw '<Esc>]'/OSC and maybe some digits and semicolons."
  (cond ((eql char #\;)
         (cond ((slot-value terminal 'current-number)
                (push (slot-value terminal 'current-number) (slot-value terminal 'parameters))
                (setf (slot-value terminal 'current-number) nil))
               (t (push 0 (slot-value terminal 'parameters))))
         (return-from xterm-saw-close-bracket 'xterm-saw-close-bracket))
        ((digit-char-p char)
         (unless (slot-value terminal 'current-number)
           (setf (slot-value terminal 'current-number) 0))
         (setf (slot-value terminal 'current-number) (+ (* (slot-value terminal 'current-number) 10)
                                                        (- (char-code char) (char-code #\0))))
         (return-from xterm-saw-close-bracket 'xterm-saw-close-bracket))
        (t (report-unknown-escape terminal)))
  nil)

(defun xterm-saw-paren (terminal char)
  "Saw '<Esc>(', expecting a character set identifier."
  ;; Set G0 character set.
  (case char
    (#\B (setf (charset terminal) :us-ascii))
    (#\0 (setf (charset terminal) :dec))
    (t (setf (charset terminal) :us-ascii)
       (report-unknown-escape terminal)))
  nil)

(defun xterm-saw-close-paren (terminal char)
  "Saw '<Esc>)', expecting a character set identifier."
  ;; Set G1 character set.
  (case char
    (#\B #+nil(setf (charset terminal) :us-ascii))
    (#\0 #+nil(setf (charset terminal) :dec))
    (t #+nil(setf (charset terminal) :us-ascii)
       (report-unknown-escape terminal)))
  nil)

(defun utf-8-code-point-length (code-point)
  "Return the number of bytes required to encode CODE-POINT or NIL if it can't be encoded."
  (cond ((<= code-point #x7F)
         1)
        ((<= #x80 code-point #x7FF)
         2)
        ;; UTF-16 surrogates can't be encoded.
        ((or (<= #x800 code-point #xD7FF)
             (<= #xE000 code-point #xFFFF))
         3)
        ((<= #x10000 code-point #x10FFFF)
         4)))

(defmethod sys.gray:stream-write-byte ((terminal xterm-terminal) byte)
  (with-simple-restart (continue "Ignore this byte.")
    (cond ((utf-8-state terminal)
           ;; Some bytes to go.
           ;; Each remaining byte must have the top two bits set to #b10.
           (when (not (eql (ldb (byte 2 6) byte) #b10))
             (write-char #\REPLACEMENT_CHARACTER terminal)
             ;; Restart decode from this byte.
             (setf (utf-8-state terminal) nil)
             (return-from sys.gray:stream-write-byte
               (sys.gray:stream-write-char terminal byte)))
           (setf (utf-8-accumulator terminal) (logior (ash (utf-8-accumulator terminal) 6)
                                                      (ldb (byte 6 0) byte)))
           (when (zerop (decf (utf-8-state terminal)))
             ;; Finish up.
             (let ((code-point (utf-8-accumulator terminal)))
               (if (or (> code-point #x0010FFFF)
                       (<= #xD800 code-point #xDFFF)
                       (not (eql (utf-8-length terminal)
                                 (utf-8-code-point-length code-point))))
                   (write-char #\REPLACEMENT_CHARACTER terminal)
                   (write-char (code-char code-point) terminal)))
             (setf (utf-8-state terminal) nil)))
          (t ;; Starting a UTF-8 sequence.
           (multiple-value-bind (remaining-bytes partial-code-point)
               (sys.net::utf-8-decode-leader byte)
             (case remaining-bytes
               (0 (write-char (code-char partial-code-point) terminal))
               ((1 2 3)
                (setf (utf-8-state terminal) remaining-bytes
                      (utf-8-length terminal) (1+ remaining-bytes)
                      (utf-8-accumulator terminal) partial-code-point))
               (t (write-char #\REPLACEMENT_CHARACTER terminal))))))))

(defmethod sys.gray:stream-write-char ((stream xterm-terminal) char)
  (push char (slot-value stream 'escape-sequence))
  (with-simple-restart (continue "Ignore this character.")
    (let ((new-state (funcall (terminal-state stream) stream char)))
      (cond (new-state
             (setf (terminal-state stream) new-state))
            (t (setf (terminal-state stream) 'xterm-initial
                     (slot-value stream 'current-number) nil
                     (slot-value stream 'parameters) '()
                     (slot-value stream 'escape-sequence) '()))))))

(defvar *xterm-translations*
  (list (list (name-char "Up-Arrow")    '(#\Esc #\[ #\A))
        (list (name-char "Down-Arrow")  '(#\Esc #\[ #\B))
        (list (name-char "Right-Arrow") '(#\Esc #\[ #\C))
        (list (name-char "Left-Arrow")  '(#\Esc #\[ #\D))
        (list (name-char "Home")        '(#\Esc #\[ #\H))
        (list (name-char "End")         '(#\Esc #\[ #\F))
        (list (name-char "Page-Up")     '(#\Esc #\[ #\5 #\~))
        (list (name-char "Page-Down")   '(#\Esc #\[ #\6 #\~))
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

(defmethod sys.gray:stream-read-byte ((stream xterm-terminal))
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
                    (sys.gray:stream-read-byte stream))
                   ((logtest (system:char-bits ch) sys.int::+char-control-bit+)
                    ;; Control character. Translate to ASCII.
                    (if (<= #x40 (char-code ch) #x7E)
                        (logxor (logand (char-code ch) #b01011111) #b00100000)
                        (sys.gray:stream-read-byte stream)))
                   (t ;; TODO: UTF-8 translation, etc.
                    (let ((translated (assoc ch *xterm-translations*)))
                      (cond (translated
                             (dolist (c (rest (second translated)))
                               (push (char-code c) (slot-value stream 'queued-bytes)))
                             (char-code (first (second translated))))
                            (t (logand (char-code ch) #xFF))))))))))
