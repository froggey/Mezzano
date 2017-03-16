;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; An XTerm emulator widget.

(defpackage :mezzano.gui.xterm
  (:use :cl)
  (:export #:xterm-terminal
           #:xterm-resize
           #:terminal-width
           #:terminal-height
           #:input-translate
           #:receive-char))

(in-package :mezzano.gui.xterm)

(defclass xterm-terminal ()
  ((framebuffer :initarg :framebuffer :reader terminal-framebuffer)
   ;; Offsets in the framebuffer.
   (x-offs :initarg :x :reader x-offset)
   (y-offs :initarg :y :reader y-offset)
   (damage-function :initarg :damage-function :reader damage-function)

   ;; Parser state.
   (state :initform 'xterm-ground :accessor terminal-state)
   (intermediate-characters :initform '())
   (parameters :initform (make-array 16 :initial-element nil))
   (n-parameters :initform 0)
   (escape-sequence :initform '())
   (osc-buffer :initform (make-array 100 :fill-pointer 0 :element-type 'character))

   ;; Terminal state.
   (font :initarg :font :reader font)
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

   (autowrap :initarg :autowrap :accessor autowrap) ; DECAWM (7)
   )
  (:default-initargs
   :queued-bytes '()))

(defvar *xterm-translations*
  '((#\Up-Arrow    (#\Esc #\[ #\A))
    (#\Down-Arrow  (#\Esc #\[ #\B))
    (#\Right-Arrow (#\Esc #\[ #\C))
    (#\Left-Arrow  (#\Esc #\[ #\D))
    (#\Home        (#\Esc #\[ #\H))
    (#\End         (#\Esc #\[ #\F))
    (#\Page-Up     (#\Esc #\[ #\5 #\~))
    (#\Page-Down   (#\Esc #\[ #\6 #\~))
    (#\KP-Multiply (#\*))
    (#\KP-Divide   (#\/))
    (#\KP-Plus     (#\+))
    (#\KP-Minus    (#\-))
    (#\KP-Period   (#\.))
    (#\KP-0        (#\0))
    (#\KP-1        (#\1))
    (#\KP-2        (#\2))
    (#\KP-3        (#\3))
    (#\KP-4        (#\4))
    (#\KP-5        (#\5))
    (#\KP-6        (#\6))
    (#\KP-7        (#\7))
    (#\KP-8        (#\8))
    (#\KP-9        (#\9))))

(defun input-translate (terminal character fn)
  "Translate a character into a form suitable for consumption by a terminal client.
Calls FN with each output character."
  (declare (ignore terminal))
  (cond ((or (sys.int::char-bit character :meta)
             (sys.int::char-bit character :super)
             (sys.int::char-bit character :hyper))
         ;; Ignore weird characters.
         ;; FIXME: Do stuff with META.
         )
        ((sys.int::char-bit character :control)
         ;; Control character. Translate to C0 control set or ignore.
         ;; Wonder how to type the C1 control characters...
         (when (<= #x3F (char-code (char-upcase character)) #x5F)
           (funcall fn (code-char (logand (- (char-code (char-upcase character)) 64) #x7F)))))
        (t
         (let ((translated (assoc character *xterm-translations*)))
           (cond (translated
                  (mapc fn (second translated)))
                 (t (funcall fn character)))))))

(defun soft-reset (terminal)
  "Reset the terminal to the default state."
  (setf (autowrap terminal) nil
        (foreground-colour terminal) nil
        (background-colour terminal) nil
        (bold terminal) nil
        (inverse terminal) nil
        (underline terminal) nil
        (charset terminal) :us-ascii
        (scroll-start terminal) 0
        (scroll-end terminal) nil
        (x-pos terminal) 0
        (y-pos terminal) 0))

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
    ;; 232-255 are a greyscale ramp, leaving out black & white.
    (dotimes (grey 24)
      (let ((level (+ (* grey 10) 8)))
        (setf (aref colours (+ 232 grey))
              (logior (ash level 16)
                      (ash level 8)
                      level))))
    colours))

(defparameter *xterm-colours* (generate-xterm-colour-table))
(defparameter *xterm-default-background-colour* (mezzano.gui:make-colour 0 0 0 0.85)
  "Use this colour for the background when no background colour has been specified.")

(defparameter *dec-special-characters-and-line-drawing*
  #(#\BLACK_LOZENGE
    #\MEDIUM_SHADE
    #\SYMBOL_FOR_HORIZONTAL_TABULATION
    #\SYMBOL_FOR_FORM_FEED
    #\SYMBOL_FOR_CARRIAGE_RETURN
    #\SYMBOL_FOR_LINE_FEED
    #\DEGREE_SIGN
    #\PLUS-MINUS_SIGN
    #\SYMBOL_FOR_NEWLINE
    #\SYMBOL_FOR_VERTICAL_TABULATION
    #\BOX_DRAWINGS_LIGHT_UP_AND_LEFT
    #\BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT
    #\BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT
    #\BOX_DRAWINGS_LIGHT_UP_AND_RIGHT
    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_HORIZONTAL
    #\HORIZONTAL_SCAN_LINE-1
    #\HORIZONTAL_SCAN_LINE-3
    #\BOX_DRAWINGS_LIGHT_HORIZONTAL
    #\HORIZONTAL_SCAN_LINE-7
    #\HORIZONTAL_SCAN_LINE-9
    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_LEFT
    #\BOX_DRAWINGS_LIGHT_UP_AND_HORIZONTAL
    #\BOX_DRAWINGS_LIGHT_DOWN_AND_HORIZONTAL
    #\BOX_DRAWINGS_LIGHT_VERTICAL
    #\LESS-THAN_OR_SLANTED_EQUAL_TO
    #\GREATER-THAN_OR_SLANTED_EQUAL_TO
    #\GREEK_SMALL_LETTER_PI
    #\NOT_EQUAL_TO
    #\POUND_SIGN
    #\MIDDLE_DOT))

(defun true-foreground-colour (terminal)
  (let ((colour-index (or (if (inverse terminal)
                              (background-colour terminal)
                              (foreground-colour terminal))
                          (if (inverse terminal) 0 7))))
    (when (and (bold terminal)
               (not (inverse terminal))
               (< colour-index 8))
      (incf colour-index 8))
    (let ((colour (elt *xterm-colours* colour-index)))
      (mezzano.gui:make-colour-from-octets
       (ldb (byte 8 16) colour) ; Red
       (ldb (byte 8 8) colour) ; Green
       (ldb (byte 8 0) colour))))) ; Blue

(defun true-background-colour (terminal)
  (let ((colour-index (or (if (inverse terminal)
                              (foreground-colour terminal)
                              (background-colour terminal))
                          (if (inverse terminal) 7 nil))))
    (if colour-index
        (let ((colour (elt *xterm-colours* colour-index)))
          (mezzano.gui:make-colour-from-octets
           (ldb (byte 8 16) colour) ; Red
           (ldb (byte 8 8) colour) ; Green
           (ldb (byte 8 0) colour))) ; Blue
        *xterm-default-background-colour*)))

(defmethod initialize-instance :after ((term xterm-terminal) &key width height font &allow-other-keys)
  (let* ((fb (terminal-framebuffer term)))
    (setf (slot-value term 'width) (truncate width (cell-pixel-width term))
          (slot-value term 'height) (truncate height (cell-pixel-height term)))
    (soft-reset term)
    (mezzano.gui:bitset :set
                        (* (terminal-width term) (cell-pixel-width term))
                        (* (terminal-height term) (cell-pixel-height term))
                        (true-background-colour term)
                        fb
                        (x-offset term) (y-offset term))
    (funcall (damage-function term)
             (x-offset term) (y-offset term)
             (* (terminal-width term) (cell-pixel-width term))
             (* (terminal-height term) (cell-pixel-height term)))))

(defun xterm-resize (term new-framebuffer x y width height)
  (mezzano.gui:bitset :set
                        (* width (cell-pixel-width term))
                        (* height (cell-pixel-height term))
                        (true-background-colour term)
                        new-framebuffer
                        x y)
  (mezzano.gui:bitblt :set
                      (* (min (terminal-width term) width)
                         (cell-pixel-width term))
                      (* (min (terminal-height term) height)
                         (cell-pixel-height term))
                      (terminal-framebuffer term)
                      (x-offset term) (y-offset term)
                      new-framebuffer
                      x y)
  (setf (slot-value term 'width) width
        (slot-value term 'height) height
        (slot-value term 'x-offs) x
        (slot-value term 'y-offs) y
        (slot-value term 'framebuffer) new-framebuffer))

(defun report-unknown-escape (term)
  (format t "Failed to parse escape sequence ~S in state ~S.~%"
          (reverse (slot-value term 'escape-sequence))
          (terminal-state term)))

(defun cell-pixel-width (term)
  ;; Use the width of 'M', not the em-square-width.
  ;; DejaVu Sans Mono's em-square-width is full-width!
  (mezzano.gui.font:glyph-advance
   (mezzano.gui.font:character-to-glyph (font term) #\M)))

(defun cell-pixel-height (term)
  (mezzano.gui.font:line-height (font term)))

(defun x-pixel-position (term x)
  (+ (x-offset term) (* x (cell-pixel-width term))))

(defun y-pixel-position (term y)
  (+ (y-offset term) (* y (cell-pixel-height term))))

(defun write-terminal-at (term char x y)
  (when (and (<= 0 x (1- (terminal-width term)))
             (<= 0 y (1- (terminal-height term))))
    (let* ((glyph (mezzano.gui.font:character-to-glyph (font term) char))
           (mask (mezzano.gui.font:glyph-mask glyph))
           (fb (terminal-framebuffer term)))
      (mezzano.gui:bitset :set
                          (cell-pixel-width term) (cell-pixel-height term)
                          (true-background-colour term)
                          fb
                          (x-pixel-position term x) (y-pixel-position term y))
      (mezzano.gui:bitset :blend
                          (mezzano.gui:surface-width mask) (mezzano.gui:surface-height mask)
                          (true-foreground-colour term)
                          fb
                          (+ (x-pixel-position term x) (mezzano.gui.font:glyph-xoff glyph))
                          (- (+ (y-pixel-position term y) (mezzano.gui.font:ascender (font term))) (mezzano.gui.font:glyph-yoff glyph))
                          mask 0 0)
      #+nil(when (bold term)
             (mezzano.gui:bitset-blend-mask-1 16 7 (true-foreground-colour term)
                                                  glyph 0 0
                                                  (terminal-framebuffer term)
                                                  (y-pixel-position term y)
                                                  (1+ (x-pixel-position term x))))
      #+nil(when (underline term)
             (mezzano.gui:bitset 1 8 (true-foreground-colour term) (terminal-framebuffer term)
                                 (+ (* y 16) (y-offset term) 15) (+ (* x 8) (x-offset term)))))
    (funcall (damage-function term)
             (x-pixel-position term x) (y-pixel-position term y)
             (cell-pixel-width term) (cell-pixel-height term))))

(defun write-terminal (term char)
  (let ((x (x-pos term)) (y (y-pos term)))
    (write-terminal-at term char x y)
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
  (let ((rsize (- rend rstart))
        (fb (terminal-framebuffer term)))
    (cond ((> lines 0)
           ;; Screen goes up.
           (cond ((>= lines rsize)
                  ;; All the way up.
                  (mezzano.gui:bitset :set
                                      (* (terminal-width term) (cell-pixel-width term))
                                      (* rsize (cell-pixel-height term))
                                      (true-background-colour term)
                                      fb
                                      (x-offset term)
                                      (+ (* rstart (cell-pixel-height term)) (y-offset term))))
                 (t (mezzano.gui:bitblt :set
                                        (* (terminal-width term) (cell-pixel-width term))
                                        (* (1+ (- rsize lines)) (cell-pixel-height term))
                                        fb
                                        (x-offset term)
                                        (+ (* (+ rstart lines) (cell-pixel-height term)) (y-offset term))
                                        fb
                                        (x-offset term)
                                        (+ (* rstart (cell-pixel-height term)) (y-offset term)))
                    (mezzano.gui:bitset :set
                                        (* (terminal-width term) (cell-pixel-width term))
                                        (* lines (cell-pixel-height term))
                                        (true-background-colour term)
                                        fb
                                        (x-offset term)
                                        (+ (* (1+ (- rend lines)) (cell-pixel-height term)) (y-offset term))))))
          ((< lines 0)
           ;; Screen goes down.
           (setf lines (- lines))
           (cond ((>= lines rsize)
                  ;; All the way down.
                  (mezzano.gui:bitset :set
                                      (* (terminal-width term) (cell-pixel-width term))
                                      (* rsize (cell-pixel-height term))
                                      (true-background-colour term)
                                      fb
                                      (x-offset term)
                                      (+ (* rstart (cell-pixel-height term)) (y-offset term))))
                 (t (mezzano.gui:bitblt :set
                                        (* (terminal-width term) (cell-pixel-width term))
                                        (* (1+ (- rsize lines)) (cell-pixel-height term))
                                        fb
                                        (x-offset term)
                                        (+ (* rstart (cell-pixel-height term)) (y-offset term))
                                        fb
                                        (x-offset term)
                                        (+ (* (+ rstart lines) (cell-pixel-height term)) (y-offset term)))
                    (mezzano.gui:bitset :set
                                        (* (terminal-width term) (cell-pixel-width term))
                                        (* lines (cell-pixel-height term))
                                        (true-background-colour term)
                                        fb
                                        (x-offset term)
                                        (+ (* rstart (cell-pixel-height term)) (y-offset term)))))))
    ;; Wow. Such lazy. Much damage.
    (funcall (damage-function term)
             (x-offset term) (y-offset term)
             (* (terminal-width term) (cell-pixel-width term))
             (* (terminal-height term) (cell-pixel-height term)))))

(defun clamp (value min max)
  (cond ((> value max) max)
        ((< value min) min)
        (t value)))

(defun clear (term top bottom)
  (setf top (clamp top 0 (terminal-height term)))
  (setf bottom (clamp bottom 0 (terminal-height term)))
  (when (< top bottom)
    (mezzano.gui:bitset :set
                        (* (terminal-width term) (cell-pixel-width term))
                        (* (- bottom top) (cell-pixel-height term))
                        (true-background-colour term)
                        (terminal-framebuffer term)
                        (x-offset term)
                        (+ (* top (cell-pixel-height term)) (y-offset term)))
    (funcall (damage-function term)
             (x-offset term) (+ (* top (cell-pixel-height term)) (y-offset term))
             (* (terminal-width term) (cell-pixel-width term)) (* (- bottom top) (cell-pixel-height term)))))

(defun erase (term left right)
  (setf left (clamp left 0 (terminal-width term)))
  (setf right (clamp right 0 (terminal-width term)))
  (when (and (<= 0 (y-pos term) (1- (terminal-height term)))
             (< left right))
    (mezzano.gui:bitset :set
                        (* (- right left) (cell-pixel-width term))
                        (cell-pixel-height term)
                        (true-background-colour term)
                        (terminal-framebuffer term)
                        (+ (* left (cell-pixel-width term)) (x-offset term))
                        (+ (* (y-pos term) (cell-pixel-height term)) (y-offset term)))
    (funcall (damage-function term)
             (+ (* left (cell-pixel-width term)) (x-offset term)) (+ (* (y-pos term) (cell-pixel-height term)) (y-offset term))
             (* (- right left) (cell-pixel-width term)) (cell-pixel-height term))))

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
               ((0 nil) ;; Normal (reset to defaults).
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

(defun save-cursor (terminal)
  (setf (saved-x terminal) (x-pos terminal)
        (saved-y terminal) (y-pos terminal)))

(defun restore-cursor (terminal)
  (setf (x-pos terminal) (saved-x terminal)
        (y-pos terminal) (saved-y terminal)))

(defun adjust-ansi-mode (terminal mode value)
  "Set an ANSI mode. '<Esc>[Pn;Pm...h' or '<Esc>[Pn;Pm...l'"
  (case mode
    (t (format t "Unsupported ANSI mode ~D.~%" mode))))

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

;;;; Control sequence parser derived from http://vt100.net/emu/dec_ansi_parser

(defun default-action (terminal char)
  "This function implements the Anywhere state transitions."
  (cond ((eql char #\Escape)
         (xterm-clear terminal)
         'xterm-escape)
        ((eql char #\String-Terminator)
         'xterm-ground)
        ((member char '(#\Start-String
                        #\Privacy-Message
                        #\Application-Program-Command))
         'xterm-sos/pm/apc-string)
        ((eql char #\Device-Control-String)
         (xterm-clear terminal)
         'xterm-dcs-entry)
        ((eql char #\Operating-System-Command)
         (xterm-osc-start terminal)
         'xterm-osc-string)
        ((eql char #\Control-Sequence-Introducer)
         (xterm-clear terminal)
         'xterm-csi-entry)
        ((or (eql char #\Can)
             (eql char #\Sub)
             ;; Remaining C1 control codes.
             (<= #x80 (char-code char) #x9F))
         (xterm-execute terminal char)
         'xterm-ground)))

;;; States.

(defun xterm-ground (terminal char)
  (cond ((default-action terminal char))
        ;; C0 control codes are executed, some are handled by the default-action.
        ((<= (char-code char) #x1F)
         (xterm-execute terminal char)
         'xterm-ground)
        (t (xterm-print terminal char)
           'xterm-ground)))

(defun xterm-escape (terminal char)
  (cond ((default-action terminal char))
        ;; C0 control codes are executed without a state change,
        ;;some are handled by the default-action.
        ((<= (char-code char) #x1F)
         (xterm-execute terminal char)
         'xterm-escape)
        ((eql char #\Del)
         'xterm-escape)
        ((<= #x20 (char-code char) #x2F)
         (xterm-collect terminal char)
         'xterm-escape-intermediate)
        ((member char '(#\X #\^ #\_))
         'xterm-sos/pm/apc-string)
        ((eql char #\P)
         (xterm-clear terminal)
         'dcs-entry)
        ((eql char #\])
         (xterm-osc-start terminal)
         'xterm-osc-string)
        ((eql char #\[)
         (xterm-clear terminal)
         'xterm-csi-entry)
        (t (xterm-esc-dispatch terminal char)
           'xterm-ground)))

(defun xterm-escape-intermediate (terminal char)
  (cond ((default-action terminal char))
        ;; C0 control codes are executed without a state change,
        ;;some are handled by the default-action.
        ((<= (char-code char) #x1F)
         (xterm-execute terminal char)
         'xterm-escape-intermediate)
        ((<= #x20 (char-code char) #x2F)
         (xterm-collect terminal char)
         'xterm-escape-intermediate)
        ((eql char #\Del)
         'xterm-escape-intermediate)
        (t (xterm-esc-dispatch terminal char)
           'xterm-ground)))

(defun xterm-csi-entry (terminal char)
  (cond ((default-action terminal char))
        ;; C0 control codes are executed without a state change,
        ;;some are handled by the default-action.
        ((<= (char-code char) #x1F)
         (xterm-execute terminal char)
         'xterm-csi-entry)
        ((eql char #\Del)
         'xterm-csi-entry)
        ((or (<= #x30 (char-code char) #x39)
             (eql char #\;))
         (xterm-param terminal char)
         'xterm-csi-param)
        ((<= #x3C (char-code char) #x3F)
         (xterm-collect terminal char)
         'xterm-csi-param)
        ((eql char #\:)
         'xterm-csi-ignore)
        ((<= #x20 (char-code char) #x2F)
         (xterm-collect terminal char)
         'xterm-csi-intermediate)
        (t (xterm-csi-dispatch terminal char)
           'xterm-ground)))

(defun xterm-csi-param (terminal char)
  (cond ((default-action terminal char))
        ;; C0 control codes are executed without a state change,
        ;;some are handled by the default-action.
        ((<= (char-code char) #x1F)
         (xterm-execute terminal char)
         'xterm-csi-param)
        ((or (<= #x30 (char-code char) #x39)
             (eql char #\;))
         (xterm-param terminal char)
         'xterm-csi-param)
        ((eql char #\Del)
         'xterm-csi-param)
        ((<= #x20 (char-code char) #x2F)
         (xterm-collect terminal char)
         'xterm-csi-intermediate)
        ((or (eql char #\:)
             (<= #x3C (char-code char) #x3F))
         'xterm-csi-ignore)
        (t (xterm-csi-dispatch terminal char)
           'xterm-ground)))

(defun xterm-csi-ignore (terminal char)
  (cond ((default-action terminal char))
        ;; C0 control codes are executed without a state change,
        ;;some are handled by the default-action.
        ((<= (char-code char) #x1F)
         (xterm-execute terminal char)
         'xterm-csi-ignore)
        ((or (<= #x20 (char-code char) #x3F)
             (eql char #\Del))
         'xterm-csi-ignore)
        (t 'xterm-ground)))

(defun xterm-csi-intermediate (terminal char)
  (cond ((default-action terminal char))
        ;; C0 control codes are executed without a state change,
        ;;some are handled by the default-action.
        ((<= (char-code char) #x1F)
         (xterm-execute terminal char)
         'xterm-csi-intermediate)
        ((<= #x20 (char-code char) #x2F)
         (xterm-collect terminal char)
         'xterm-csi-intermediate)
        ((eql char #\Del)
         'xterm-csi-intermediate)
        ((<= #x30 (char-code char) #x3F)
         'xterm-csi-ignore)
        (t (xterm-csi-dispatch terminal char)
           'xterm-ground)))

(defun xterm-sos/pm/apc-string (terminal char)
  (cond ((default-action terminal char))
        ((eql char #\String-Terminator)
         'xterm-ground)
        (t 'xterm-sos/pm/apc-string)))

(defun xterm-osc-string (terminal char)
  (let ((default (default-action terminal char)))
    (cond (default
           (xterm-osc-end terminal)
              default)
          ((eql char #\String-Terminator)
           (xterm-osc-end terminal)
           'xterm-ground)
          ((<= (char-code char) #x1F)
           'xterm-osc-string)
          (t (xterm-osc-put terminal char)
             'xterm-osc-string))))

(defun xterm-dcs-entry (terminal char)
  (cond ((default-action terminal char))
        ((or (<= (char-code char) #x1F)
             (eql char #\Del))
         'xterm-dcs-entry)
        ((<= #x20 (char-code char) #x2F)
         (xterm-collect terminal char)
         'xterm-dcs-intermediate)
        ((eql char #\:)
         'xterm-dcs-ignore)
        ((<= #x30 (code-char char) #x3B)
         (xterm-param terminal char)
         'xterm-dcs-param)
        ((<= #x3C (code-char char) #x3F)
         (xterm-collect terminal char)
         'xterm-dcs-param)
        (t (xterm-hook terminal char)
           'xterm-dcs-passthrough)))

(defun xterm-dcs-param (terminal char)
  (cond ((default-action terminal char))
        ((or (<= (char-code char) #x1F)
             (eql char #\Del))
         'xterm-dcs-param)
        ((or (<= #x30 (char-code char) #x39)
             (eql char #\;))
         (xterm-param terminal char)
         'xterm-dcs-param)
        ((or (eql char #\:)
             (<= #x3C (code-char char) #x3F))
         'xterm-dcs-ignore)
        ((<= #x20 (char-code char) #x2F)
         (xterm-collect terminal char)
         'xterm-dcs-intermediate)
        (t (xterm-hook terminal char)
           'xterm-dcs-passthrough)))

(defun xterm-dcs-intermediate (terminal char)
  (cond ((default-action terminal char))
        ((or (<= (char-code char) #x1F)
             (eql char #\Del))
         'xterm-dcs-intermediate)
        ((<= #x30 (char-code char) #x3F)
         'xterm-dcs-ignore)
        (t (xterm-hook terminal char)
           'xterm-dcs-passthrough)))

(defun xterm-dcs-ignore (terminal char)
  (cond ((default-action terminal char))
        ((eql char #\String-Terminator)
         'xterm-ground)
        (t 'xterm-dcs-ignore)))

(defun xterm-dcs-passthrough (terminal char)
  (let ((default (default-action terminal char)))
    (cond (default
           (xterm-unhook terminal)
              default)
          ((eql char #\String-Terminator)
           (xterm-unhook terminal)
           'xterm-ground)
          ((eql char #\Del)
           'xterm-dcs-passthrough)
          (t (xterm-put terminal char)
             'xterm-dcs-passthrough))))

;;; Actions.

(defun xterm-print (terminal char)
  (ecase (charset terminal)
    (:us-ascii) ; No translation.
    (:dec ;; Translate some characters.
     (when (<= #x60 (char-int char) #x7E)
       (setf char (aref *dec-special-characters-and-line-drawing*
                        (- (char-int char) #x60))))))
  (write-terminal terminal char))

(defun xterm-execute (terminal control-code)
  (case control-code
    (#\Cr
     (setf (x-pos terminal) 0))
    (#\Lf
     (cond ((eql (1+ (y-pos terminal))
                 (or (scroll-end terminal)
                     (terminal-height terminal)))
            (scroll-terminal terminal
                             (scroll-start terminal)
                             (or (scroll-end terminal)
                                 (terminal-height terminal))
                             1))
           (t (incf (y-pos terminal)))))
    (#\Bs
     (decf (x-pos terminal)))))

(defun xterm-clear (terminal)
  (setf (slot-value terminal 'n-parameters) 0
        (slot-value terminal 'intermediate-characters) '())
  (fill (slot-value terminal 'parameters) nil))

(defun xterm-collect (terminal char)
  (when (listp (slot-value terminal 'intermediate-characters))
    (if (>= (length (slot-value terminal 'intermediate-characters)) 2)
        (setf (slot-value terminal 'intermediate-characters) :invalid)
        (push char (slot-value terminal 'intermediate-characters)))))

(defun xterm-param (terminal char)
  (when (< (slot-value terminal 'n-parameters) 16)
    (cond ((eql char #\;)
           (incf (slot-value terminal 'n-parameters)))
          (t
           (unless (aref (slot-value terminal 'parameters)
                         (slot-value terminal 'n-parameters))
             (setf (aref (slot-value terminal 'parameters)
                         (slot-value terminal 'n-parameters))
                   0))
           (setf (aref (slot-value terminal 'parameters)
                       (slot-value terminal 'n-parameters))
                 (min 16383
                      (+ (* (aref (slot-value terminal 'parameters)
                                  (slot-value terminal 'n-parameters))
                            10)
                         (- (char-code char) (char-code #\0)))))))))

(defun xterm-esc-dispatch (terminal char)
  (push char (slot-value terminal 'intermediate-characters))
  (let ((intermediates (reverse (slot-value terminal 'intermediate-characters))))
    (cond ((equal intermediates '(#\D)) ; Index (IND).
           (xterm-execute terminal #\Lf))
          ((equal intermediates '(#\E)) ; Next Line (NEL).
           (xterm-execute terminal #\Lf)
           (xterm-execute terminal #\Cr))
          ((equal intermediates '(#\M)) ; Reverse Index (RI).
           (cond ((eql (y-pos terminal)
                       (or (scroll-start terminal)
                           0))
                  (scroll-terminal terminal
                                   (scroll-start terminal)
                                   (or (scroll-end terminal)
                                       (terminal-height terminal))
                                   -1))
                 (t (decf (y-pos terminal)))))
          ((equal intermediates '(#\7)) ; Save Cursor (DECSC).
           (save-cursor terminal))
          ((equal intermediates '(#\8)) ; Restore Cursor (DECRC).
           (restore-cursor terminal))
          ((equal intermediates '(#\( #\B))
           ;; Set G0 character set.
           (setf (charset terminal) :us-ascii))
          ((equal intermediates '(#\( #\0))
           ;; Set G0 character set.
           (setf (charset terminal) :dec))
          ((equal intermediates '(#\# #\8))
           ;; Screen Alignment Display (DECALN).
           ;; Fill the screen with #\E.
           (dotimes (y (terminal-height terminal))
             (dotimes (x (terminal-width terminal))
               (write-terminal-at terminal #\E x y))))
          (t (report-unknown-escape terminal)))))

(defun xterm-csi-dispatch (terminal char)
  (when (and (< (slot-value terminal 'n-parameters) 16)
             (aref (slot-value terminal 'parameters)
                   (slot-value terminal 'n-parameters)))
    (incf (slot-value terminal 'n-parameters)))
  (let ((params (coerce (subseq (slot-value terminal 'parameters)
                                0 (slot-value terminal 'n-parameters))
                        'list))
        (intermediates (reverse (slot-value terminal 'intermediate-characters))))
    (cond ((endp intermediates)
           (case char
             (#\c ; Device Attributes (DA).
              (cond ((or (null params)
                         (eql (first params) 0))
                     ;; FIXME: Respond with something.
                     ;; \e[?1;2c  "I am a VT100 terminal with AVO."
                     )
                    (t (report-unknown-escape terminal))))
             (#\d ; Line Position Absolute (VPA).
              (setf (y-pos terminal) (1- (or (first params) 1))))
             (#\h ; Set Mode (SM).
              (dolist (p params)
                (adjust-ansi-mode terminal p t)))
             (#\l ; Reset Mode (RM).
              (dolist (p params)
                (adjust-ansi-mode terminal p nil)))
             (#\m ; Select Graphics Rendition (SGR).
              (set-character-attributes terminal params))
             (#\r ; Set Top and Bottom Margins (DECSTBM).
              (setf (scroll-start terminal) (1- (or (first params) 1))
                    (scroll-end terminal) (when (second params)
                                            (1- (second params))))
              (format t "Scroll region: ~D ~D~%" (scroll-start terminal) (scroll-end terminal)))
             (#\A ; Cursor Up (CUU).
              (setf (y-pos terminal) (max 0
                                          (- (y-pos terminal)
                                             (max (or (first params) 1) 1)))))
             (#\B ; Cursor Down (CUD).
              (setf (y-pos terminal) (min (1- (terminal-height terminal))
                                          (+ (y-pos terminal)
                                             (max (or (first params) 1) 1)))))
             (#\C ; Cursor Forwards (CUF).
              (setf (x-pos terminal) (min (1- (terminal-width terminal))
                                          (+ (x-pos terminal)
                                             (max (or (first params) 1) 1)))))
             (#\D ; Cursor Backwards (CUB).
              (setf (x-pos terminal) (max 0
                                          (- (x-pos terminal)
                                             (max (or (first params) 1) 1)))))
             ((#\f #\H) ; Horizontal and Vertical Position (HVP) and Cursor Position (CUP).
              (let* ((row (max (or (first params) 1) 1))
                     (column (max (or (second params) 1) 1)))
                (setf (y-pos terminal) (1- row)
                      (x-pos terminal) (1- column))))
             (#\G ; Cursor Character Absolute (CHA).
              (setf (x-pos terminal) (1- (or (first params) 1))))
             (#\J ; Erase In Display (ED).
              (case (first params)
                ((0 nil)
                 ;; clear below
                 (erase terminal (x-pos terminal) (terminal-width terminal))
                 (clear terminal (1+ (y-pos terminal)) (terminal-height terminal)))
                (1 (clear terminal 0 (y-pos terminal))) ; clear above
                (2 (clear terminal 0 (terminal-height terminal))) ; clear screen
                (t (report-unknown-escape terminal))))
             (#\K ; Erase In Line (EL).
              (case (first params)
                ((0 nil) (erase terminal (x-pos terminal) (terminal-width terminal))) ; erase to right
                (1 (erase terminal 0 (x-pos terminal))) ; erase to left
                (2 (erase terminal 0 (terminal-width terminal))) ; erase line
                (t (report-unknown-escape terminal))))
             (#\L ; Insert Line (IL).
              ;; Insert Pn lines, starting at the cursor. Lines move down.
              (when (<= (scroll-start terminal)
                        (y-pos terminal)
                        (1- (or (scroll-end terminal)
                                (terminal-height terminal))))
                (scroll-terminal terminal
                                 (y-pos terminal)
                                 (or (scroll-end terminal)
                                     (terminal-height terminal))
                                 (- (or (first params) 1)))))
             (#\M ; Delete Line (DL).
              ;; Delete Pn lines, starting at the cursor. Lines move up.
              (when (<= (scroll-start terminal)
                        (y-pos terminal)
                        (1- (or (scroll-end terminal)
                                (terminal-height terminal))))
                  (scroll-terminal terminal
                                   (y-pos terminal)
                                   (or (scroll-end terminal)
                                       (terminal-height terminal))
                                   (or (first params) 1))))
             (t (report-unknown-escape terminal))))
          ((equal intermediates '(#\?))
           (case char
             (#\h ; Set Mode (SM), DEC private modes.
              (dolist (p params)
                (adjust-dec-private-mode terminal p t)))
             (#\l ; Reset Mode (SM), DEC private modes.
              (dolist (p params)
                (adjust-dec-private-mode terminal p nil)))
             (t (report-unknown-escape terminal))))
          (t (report-unknown-escape terminal)))))

(defun xterm-hook (terminal char)
  (when (and (< (slot-value terminal 'n-parameters) 16)
             (aref (slot-value terminal 'parameters)
                   (slot-value terminal 'n-parameters)))
    (incf (slot-value terminal 'n-parameters)))
  (let ((params (subseq (slot-value terminal 'parameters)
                        0 (slot-value terminal 'n-parameters)))
        (intermediates (reverse (slot-value terminal 'intermediate-characters))))
    (report-unknown-escape terminal)))

(defun xterm-put (terminal char))

(defun xterm-unhook (terminal)
  (format t "Unhook~%"))

(defun xterm-osc-start (terminal)
  (setf (fill-pointer (slot-value terminal 'osc-buffer)) 0))

(defun xterm-osc-put (terminal char)
  (vector-push char (slot-value terminal 'osc-buffer)))

(defun xterm-osc-end (terminal)
  ;; Find the end of the command parameter.
  ;; The buffer will look like: P s ; P t
  (let* ((buf (slot-value terminal 'osc-buffer))
         (parameter-end (dotimes (i (length buf)
                                  (progn ;; No #\;, give up.
                                    (report-unknown-escape terminal)
                                    (return-from xterm-osc-end)))
                          (when (eql (aref buf i) #\;)
                            (return i))))
         (pt (subseq buf (1+ parameter-end)))
         (parameter 0))
    (dotimes (i parameter-end)
      (let ((weight (digit-char-p (aref buf i))))
        (when (not weight)
          (report-unknown-escape terminal)
          (return-from xterm-osc-end))
        (setf parameter (+ (* parameter 10) weight))))
    (case parameter
      (0 (format t "Set iconified name and window title to ~S~%" pt))
      (1 (format t "Set iconified name to ~S~%" pt))
      (2 (format t "Set window title to ~S~%" pt))
      (3 (format t "Set X property ~S~%" pt))
      (4 (format t "Set colour ~S~%" pt))
      ((10 11 12 13 14 15 16 17 18)
       (format t "Set dynamic colour ~S~%" buf))
      (46 (format t "Set log file to ~S~%" pt))
      (50 (format t "Set font to ~S~%" pt))
      (51) ; Reserved for Emacs shell.
      (52 (format t "Manipulate selection data: ~S~%" pt)))))

(defun receive-char (xterm char)
  (push char (slot-value xterm 'escape-sequence))
  (with-simple-restart (continue "Ignore this character.")
    (let ((new-state (funcall (terminal-state xterm) xterm char)))
      (cond (new-state
             (when (eql new-state 'xterm-ground)
               (setf (slot-value xterm 'escape-sequence) '()))
             (setf (terminal-state xterm) new-state))
            (t (setf (terminal-state xterm) 'xterm-ground))))))
