(defpackage :mezzanine.gui.widgets
  (:use :cl :mezzanine.gui.font)
  (:import-from :mezzanine.gui.compositor
                #:width #:height
                #:mouse-x-position #:mouse-y-position
                #:mouse-button-state #:mouse-button-change)
  (:export #:default-damage-function
           #:frame
           #:frame-title
           #:close-button-p
           #:close-button-hover
           #:activep
           #:frame-mouse-event
           #:close-button-clicked
           #:draw-frame
           #:frame-size
           #:text-widget
           #:reset))

(in-package :mezzanine.gui.widgets)

(defgeneric draw-frame (frame))
(defgeneric frame-size (frame))
(defgeneric frame-mouse-event (frame mouse-event))
(defgeneric reset (object))

(defun default-damage-function (window)
  (lambda (&rest args)
    (apply #'mezzanine.gui.compositor:damage-window window args)))

(define-condition close-button-clicked () ())

(defclass frame ()
  ((%framebuffer :initarg :framebuffer :reader framebuffer)
   (%damage-function :initarg :damage-function :reader damage-function)
   (%title :initarg :title :accessor frame-title)
   (%close-button-p :initarg :close-button-p :accessor close-button-p)
   (%close-button-hover :initarg :close-button-hover :accessor close-button-hover)
   (%activep :initarg :activep :accessor activep))
  (:default-initargs :title "" :close-button-p nil :close-button-hover nil :activep nil))

(defvar *frame-title-font* (open-font *default-font* *default-font-size*))

(defvar *corner-mask*
  #2A((0.0  0.0  0.25 0.7 0.9)
      (0.0  0.47 1.0  1.0 1.0)
      (0.25 1.0  1.0  1.0 1.0)
      (0.75 1.0  1.0  1.0 1.0)
      (0.94 1.0  1.0  1.0 1.0))
  "Alpha values for the rounded window corners.")

;; TODO: Load these from a file, maybe.
(defvar *close-button*
  (mezzanine.gui.compositor::2d-array
   '((#x00000000 #x00000000 #x00000000 #x00000202 #x472B0C0E #x93650E13 #xB58F141B #xB48E1219 #x8F650A0F #x412B0709 #x00000101 #x00000000 #x00000000 #x00000000)
     (#x00000000 #x00000000 #x24130A0A #xBC84161D #xFFCD353E #xFFE5737A #xFFEA949A #xFFE9949A #xFFE5727A #xFFCC313B #xB2830A11 #x1A130404 #x00000000 #x00000000)
     (#x00000000 #x2213090A #xE1A51D25 #xFFE76F76 #xFFF9DFE1 #xFFFDF6F6 #xFFFDF5F5 #xFFFDF5F5 #xFFFDF6F6 #xFFF9E0E2 #xFFE66F77 #xD7A40E16 #x17140202 #x00000000)
     (#x02000201 #xBD861A20 #xFFE25059 #xFFF5D1D3 #xFFF3C9CB #xFFF2C4C7 #xFFF2C4C7 #xFFF2C4C7 #xFFF2C4C7 #xFFF3C9CB #xFFF5D1D3 #xFFE15059 #xAD880A10 #x00000000)
     (#x432B0E10 #xFFCE1F29 #xFFE3787F #xFFEBA5A9 #xFFEA9DA2 #xFFEA9EA2 #xFFEA9EA2 #xFFEA9EA2 #xFFEA9EA2 #xFFEA9DA2 #xFFEBA5A9 #xFFE37980 #xFFD01C26 #x355F393B)
     (#x906D1C20 #xFFDB242E #xFFDD565E #xFFE47D82 #xFFE3787E #xFFE3787E #xFFE3787E #xFFE3787E #xFFE3787E #xFFE3787E #xFFE47D82 #xFFDD565E #xFFCE1621 #x85CA7377)
     (#xB29A252B #xFFDB2B35 #xFFD8313A #xFFDE5259 #xFFDF585F #xFFDF575E #xFFDF575E #xFFDF575E #xFFDF575E #xFFDF585F #xFFDE5259 #xFFD8313A #xFFD3212B #xA5CE6268)
     (#xB09A2228 #xFFE0323B #xFFDA3039 #xFFDB323B #xFFDC3B44 #xFFDD3E46 #xFFDD3E46 #xFFDD3E46 #xFFDD3E46 #xFFDC3B44 #xFFDB323B #xFFDA3039 #xFFD82832 #xA6CC6368)
     (#x897A2125 #xFFE5343C #xFFDE363F #xFFDE353E #xFFDD343D #xFFDE353E #xFFDE353E #xFFDE353E #xFFDE353E #xFFDD343D #xFFDE353E #xFFDE363F #xFFD7262F #x8AD28588)
     (#x37AF8B8C #xFFCB2129 #xFFE33C44 #xFFE03A42 #xFFE03A42 #xFFE03A42 #xFFE03A42 #xFFE03A42 #xFFE03A42 #xFFE03A42 #xFFE03A42 #xFFE33A43 #xFFCC2830 #x41E6CBCC)
     (#x00F5F6F5 #xACC44E53 #xFFDF2E37 #xFFE33D45 #xFFE23C44 #xFFE23C44 #xFFE23C44 #xFFE23C44 #xFFE23C44 #xFFE23C44 #xFFE43D45 #xFFDC2C34 #xBBC8676B #x02FDFEFE)
     (#x00FFFFFF #x16F5E3E3 #xD5CC4248 #xFFDC2C35 #xFFE43C45 #xFFE33D45 #xFFE23C44 #xFFE23C44 #xFFE33D45 #xFFE43C44 #xFFDD2C34 #xDFC2474C #x20F0E7E8 #x00FFFFFF)
     (#x00FFFFFF #x00FFFFFF #x19F2E5E5 #xAFCE6368 #xFFCF2B32 #xFFDB2C34 #xFFE0323B #xFFE0323B #xFFDB2C34 #xFFCC2B32 #xB9C6666A #x22EFE7E8 #x00FFFFFF #x00FFFFFF)
     (#x00FFFFFF #x00FFFFFF #x00FFFFFF #x00FDFFFF #x3EE6C8C9 #x88D3878A #xA7CB676C #xA8CA686B #x8CD0888B #x45E2C9CA #x00FDFFFF #x00FFFFFF #x00FFFFFF #x00FFFFFF))
   '(unsigned-byte 32)))

(defvar *close-button-hover*
  (mezzanine.gui.compositor::2d-array
   '((#x00000000 #x00000000 #x00000000 #x00000202 #x472B0C0E #x93650E13 #xB58F141B #xB48E1219 #x8F650A0F #x412B0709 #x00000101 #x00000000 #x00000000 #x00000000)
     (#x00000000 #x00000000 #x24130A0A #xBC84161D #xFFCD353F #xFFE5737A #xFFE9949A #xFFE9949A #xFFE5727A #xFFCD313B #xB2830A11 #x1A130404 #x00000000 #x00000000)
     (#x00000000 #x2213090A #xE1A51D25 #xFFE76E76 #xFFF6DFE1 #xFFFDF6F6 #xFFFEF5F6 #xFFFEF5F6 #xFFFEF6F7 #xFFF6E0E2 #xFFE56F77 #xD7A40E16 #x17140202 #x00000000)
     (#x02000201 #xBD861A20 #xFFE25059 #xFFF7D1D3 #xFFD2C4C5 #xFFC0BDBD #xFFEFC4C7 #xFFF6C5C8 #xFFCCBFC0 #xFFC5C2C3 #xFFF4D1D3 #xFFE15159 #xAD880A10 #x00000000)
     (#x432B0E10 #xFFCE1F29 #xFFE3787F #xFFEEA5A9 #xFFDD9C9F #xFF8B8E8E #xFFBB9698 #xFFD59A9E #xFF868D8D #xFFC9989B #xFFEFA6AA #xFFE37980 #xFFD01C26 #x355F393B)
     (#x906D1C20 #xFFDB242E #xFFDD565E #xFFE47D82 #xFFEB7980 #xFFA06C6E #xFF5F605F #xFF6A6262 #xFF7D6566 #xFFE6787F #xFFE57D83 #xFFDD565E #xFFCE1621 #x85CA7377)
     (#xB29A252B #xFFDB2B35 #xFFD8313A #xFFDE5259 #xFFE15860 #xFFDB565E #xFF4A3939 #xFF2D3332 #xFFBE5056 #xFFE65961 #xFFDE5259 #xFFD8313A #xFFD3212B #xA5CE6268)
     (#xB09A2228 #xFFE0323B #xFFDA3039 #xFFDB323B #xFFE13C45 #xFFD03B43 #xFF211413 #xFF050D0C #xFFA73237 #xFFE73D47 #xFFDA323B #xFFDA3039 #xFFD82832 #xA6CC6368)
     (#x897A2125 #xFFE5343C #xFFDE363F #xFFDD353E #xFFEB3841 #xFF5B1619 #xFF0A0202 #xFF220809 #xFF250809 #xFFDF353E #xFFE0363F #xFFDE363F #xFFD7262F #x8AD28588)
     (#x37AF8B8C #xFFCB2129 #xFFE33C44 #xFFE83C45 #xFFB62F35 #xFF000000 #xFF7C2024 #xFFB93036 #xFF000000 #xFF7E2125 #xFFED3D46 #xFFE33A43 #xFFCC2830 #x41E6CBCC)
     (#x00F5F6F5 #xACC44E53 #xFFDF2F37 #xFFEA4047 #xFF491416 #xFF260A0C #xFFDD3B43 #xFFF04048 #xFF55171A #xFF1D0809 #xFFD63A41 #xFFE02D35 #xBBC8676B #x02FDFEFE)
     (#x00FFFFFF #x16F5E3E3 #xD5CD4248 #xFFDB2C34 #xFFCF373E #xFFDA3B42 #xFFE63D45 #xFFE43D45 #xFFE13C44 #xFFD1373E #xFFD62A32 #xDFC3474C #x20F0E7E8 #x00FFFFFF)
     (#x00FFFFFF #x00FFFFFF #x19F2E5E5 #xAFCE6368 #xFFD32B33 #xFFDD2D35 #xFFE0323A #xFFE0323A #xFFDC2C34 #xFFD02C33 #xB9C7666A #x22EEE7E8 #x00FFFFFF #x00FFFFFF)
     (#x00FFFFFF #x00FFFFFF #x00FFFFFF #x00FDFFFF #x3EE6C8C9 #x88D3878A #xA7CB676C #xA8CA686B #x8CD0888B #x45E2C9CA #x00FDFFFF #x00FFFFFF #x00FFFFFF #x00FFFFFF))
   '(unsigned-byte 32)))

(defvar *close-button-x* 5)
(defvar *close-button-y* 3)

(defun lerp (v0 v1 a)
  (+ v0 (* (- v1 v0) a)))

(defun lerp-colour (c1 c2 a)
  (logior (ash (truncate (lerp (ldb (byte 8 24) c1)
                               (ldb (byte 8 24) c2)
                               a))
               24)
          (ash (truncate (lerp (ldb (byte 8 16) c1)
                               (ldb (byte 8 16) c2)
                               a))
               16)
          (ash (truncate (lerp (ldb (byte 8 8) c1)
                               (ldb (byte 8 8) c2)
                               a))
               8)
          (truncate (lerp (ldb (byte 8 0) c1)
                          (ldb (byte 8 0) c2)
                          a))))

(defun vertical-gradient (nrows ncols colour1 colour2 to-array to-row to-col)
  (dotimes (i nrows)
    (mezzanine.gui:bitset 1 ncols
                          (lerp-colour colour1 colour2 (/ i (1- nrows)))
                          to-array (+ to-row i) to-col)))

(defmethod draw-frame ((frame frame))
  (let* ((framebuffer (framebuffer frame))
         (win-width (array-dimension framebuffer 1))
         (win-height (array-dimension framebuffer 0))
         (title (frame-title frame))
         (colour (if (activep frame) #xFF808080 #xFF404040))
         (top-colour (if (activep frame) #xFFFFFFFF #xFF808080)))
    ;; Top.
    (vertical-gradient 19 win-width
                       top-colour colour
                       framebuffer 0 0)
    ;; Bottom.
    (mezzanine.gui:bitset 1 win-width colour framebuffer (1- win-height) 0)
    ;; Left.
    (mezzanine.gui:bitset win-height 1 colour framebuffer 19 0)
    ;; Right.
    (mezzanine.gui:bitset win-height 1 colour framebuffer 19 (1- win-width))
    ;; Round off the corners.
    (dotimes (y (array-dimension *corner-mask* 0))
      (dotimes (x (array-dimension *corner-mask* 1))
        (let ((alpha (truncate (* (aref *corner-mask* y x) 255))))
          (setf (ldb (byte 8 24) (aref framebuffer y x)) alpha
                (ldb (byte 8 24) (aref framebuffer y (- win-width x 1))) alpha))))
    ;; Close button.
    (when (close-button-p frame)
      (mezzanine.gui:bitblt-argb-xrgb (array-dimension *close-button* 0) (array-dimension *close-button* 1)
                                      (if (close-button-hover frame) *close-button-hover* *close-button*) 0 0
                                      framebuffer *close-button-y* *close-button-x*))
    ;; Title.
    (when title
      (let ((width 0))
        ;; How wide is the title text?
        (dotimes (i (length title))
          (incf width (glyph-advance (character-to-glyph *frame-title-font* (char title i)))))
        ;; Clamp it, corner elements and buttons.
        (setf width (mezzanine.gui:clamp width 0 (- win-width (+ 16 (* (array-dimension *corner-mask* 1) 2)))))
        ;; Find leftmost position.
        (let ((origin (- (truncate win-width 2) (truncate width 2)))
              (pen 0))
          ;; Write characters.
          (dotimes (i (length title))
            (let* ((glyph (character-to-glyph *frame-title-font* (char title i)))
                   (mask (glyph-mask glyph)))
              (when (> pen width)
                (return))
              (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1) #xFF3F3F3F
                                                     mask 0 0
                                                     framebuffer (- (+ 4 (ascender *frame-title-font*)) (glyph-yoff glyph)) (+ origin pen (glyph-xoff glyph)))
              (incf pen (glyph-advance glyph)))))))
    ;; Damage the whole window.
    (funcall (damage-function frame) 0 0 win-width win-height)))

(defmethod frame-size ((frame frame))
  ;; left, right, top, bottom.
  (values 1 1 19 1))

(defun in-frame-close-button (frame x y)
  (and (close-button-p frame)
       (>= x *close-button-x*)
       (< x (+ *close-button-x* (array-dimension *close-button* 1)))
       (>= y *close-button-y*)
       (< y (+ *close-button-y* (array-dimension *close-button* 0)))
       ;; Alpha test.
       (> (ldb (byte 8 24) (aref *close-button* (- y *close-button-y*) (- x *close-button-x*))) 128)))

(defmethod frame-mouse-event ((frame frame) event)
  (cond ((in-frame-close-button frame
                                (mouse-x-position event)
                                (mouse-y-position event))
         (when (not (close-button-hover frame))
           (setf (close-button-hover frame) t)
           (draw-frame frame)
           (funcall (damage-function frame)
                    0 0
                    (array-dimension (framebuffer frame) 1) 19))
         ;; Check for close button click.
         (when (and (logbitp 0 (mouse-button-change event))
                    ;; Mouse1 up
                    (not (logbitp 0 (mouse-button-state event))))
           (signal 'close-button-clicked)))
        ((close-button-hover frame)
         (setf (close-button-hover frame) nil)
         (draw-frame frame)
         (funcall (damage-function frame)
                  0 0
                  (array-dimension (framebuffer frame) 1) 19))))

(defclass text-widget (sys.gray:fundamental-character-output-stream)
  ((%framebuffer :initarg :framebuffer :reader framebuffer)
   (%x-position :initarg :x-position :reader x-position)
   (%y-position :initarg :y-position :reader y-position)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)
   (%damage-fn :initarg :damage-function :reader damage-function)
   (%x :accessor cursor-x)
   (%y :accessor cursor-y)
   (%column :accessor cursor-column)
   (%line :accessor cursor-line)
   (%background-colour :initarg :background-colour :reader background-colour)
   (%foreground-colour :initarg :foreground-colour :reader foreground-colour)
   (%font :initarg :font :reader font))
  (:default-initargs :foreground-colour #xFFDCDCCC
                     :background-colour #xFF3E3E3E))

(defmethod initialize-instance :after ((widget text-widget) &key &allow-other-keys)
  (reset widget))

(defmethod sys.gray:stream-terpri ((stream text-widget))
  (let* ((x (cursor-x stream))
         (y (cursor-y stream))
         (fb (framebuffer stream))
         (win-width (width stream))
         (win-height (height stream))
         (line-height (line-height (font stream)))
         (left (x-position stream))
         (top (y-position stream)))
    ;; Clear to the end of the current line.
    (mezzanine.gui:bitset line-height (- win-width x) (background-colour stream) fb (+ top y) (+ left x))
    (funcall (damage-function stream) (+ left x) (+ top y) (- win-width x) line-height)
    ;; Advance to the next line.
    (setf (cursor-x stream) 0
          (cursor-column stream) 0)
    (cond ((> (+ y (* line-height 2)) win-height)
           ;; Off the end of the screen. Scroll!
           (incf (cursor-line stream) line-height)
           (mezzanine.gui:bitblt (- win-height line-height) win-width
                                 fb (+ top line-height) left
                                 fb top left)
           ;; Clear line.
           (mezzanine.gui:bitset line-height win-width (background-colour stream) fb (+ top y) left)
           ;; Damage the whole area.
           (funcall (damage-function stream) left top win-width win-height))
          (t (incf y line-height)
             (setf (cursor-y stream) y)
             ;; Clear line.
             (mezzanine.gui:bitset line-height win-width (background-colour stream) fb (+ top y) left)
             (funcall (damage-function stream) left (+ top y) win-width line-height)))))

(defmethod sys.gray:stream-write-char ((stream text-widget) character)
  (cond
    ((eql character #\Newline)
     (sys.gray:stream-terpri stream))
    (t (let* ((glyph (character-to-glyph (font stream) character))
              (mask (glyph-mask glyph))
              (advance (glyph-advance glyph))
              (fb (framebuffer stream))
              (win-width (width stream))
              (line-height (line-height (font stream)))
              (left (x-position stream))
              (top (y-position stream)))
         (when (> (+ (cursor-x stream) advance) win-width)
           (sys.gray:stream-terpri stream))
         ;; Fetch x/y after terpri.
         (let ((x (cursor-x stream))
               (y (cursor-y stream)))
           (mezzanine.gui:bitset line-height advance (background-colour stream) fb (+ top y) (+ left x))
           (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1) (foreground-colour stream)
                                                  mask 0 0
                                                  fb (+ top (- (+ y (ascender (font stream))) (glyph-yoff glyph))) (+ left x (glyph-xoff glyph)))
           (funcall (damage-function stream) (+ left x) (+ top y) advance line-height)
           (incf (cursor-x stream) advance)
           (incf (cursor-column stream)))))))

(defmethod sys.gray:stream-start-line-p ((stream text-widget))
  (zerop (cursor-x stream)))

(defmethod sys.gray:stream-line-column ((stream text-widget))
  (cursor-column stream))

(defmethod sys.int::stream-cursor-pos ((stream text-widget))
  (values (cursor-x stream)
          (+ (cursor-line stream)
             (cursor-y stream))))

(defmethod sys.int::stream-move-to ((stream text-widget) x y)
  (check-type x integer)
  (check-type y integer)
  (setf (cursor-x stream) x
        (cursor-y stream) (max (- y (cursor-line stream)) 0)))

(defmethod sys.int::stream-character-width ((stream text-widget) character)
  (glyph-advance (character-to-glyph (font stream) character)))

(defmethod sys.int::stream-compute-motion ((stream text-widget) string &optional (start 0) end initial-x initial-y)
  (unless end (setf end (length string)))
  (unless initial-x (setf initial-x (cursor-x stream)))
  (unless initial-y (setf initial-y (+ (cursor-line stream)
                                       (cursor-y stream))))
  (do* ((framebuffer (framebuffer stream))
        (win-width (width stream))
        (win-height (height stream))
        (line-height (line-height (font stream)))
        (left (x-position stream))
        (top (y-position stream))
        (i start (1+ i)))
       ((>= i end)
        (values initial-x initial-y))
    (let* ((ch (char string i))
           (advance (sys.int::stream-character-width stream ch)))
      (when (or (eql ch #\Newline)
                (> (+ initial-x advance) win-width))
        (setf initial-x 0
              initial-y (if (>= (+ initial-y line-height) win-height)
                            0
                            (+ initial-y line-height))))
      (unless (eql ch #\Newline)
        (incf initial-x advance)))))

(defmethod sys.int::stream-clear-between ((stream text-widget) start-x start-y end-x end-y)
  (let* ((framebuffer (framebuffer stream))
         (win-width (width stream))
         (win-height (height stream))
         (colour (background-colour stream))
         (line-height (line-height (font stream)))
         (left (x-position stream))
         (top (y-position stream)))
    (setf start-y (- start-y (cursor-line stream))
          end-y (- end-y (cursor-line stream)))
    (cond ((eql start-y end-y)
           ;; Clearing one line.
           (mezzanine.gui:bitset line-height (- end-x start-x) colour framebuffer (+ top start-y) (+ left start-x))
           (funcall (damage-function stream) (+ left start-x) (+ top start-y) (- end-x start-x) line-height))
          (t ;; Clearing many lines.
           ;; Clear top line.
           (mezzanine.gui:bitset line-height (- win-width start-x) colour
                                 framebuffer (+ top start-y) (+ left start-x))
           (funcall (damage-function stream) (+ left start-x) (+ top start-y) (- win-width start-x) line-height)
           ;; Clear in-between.
           (when (> (- end-y start-y) line-height)
             (mezzanine.gui:bitset (- end-y start-y line-height) win-width colour
                                   framebuffer (+ top start-y line-height) left)
             (funcall (damage-function stream) left (+ top start-y line-height) win-width (- end-y start-y line-height)))
           ;; Clear bottom line.
           (mezzanine.gui:bitset line-height end-x colour
                                 framebuffer (+ top end-y) left)
           (funcall (damage-function stream) left (+ top end-y) end-x line-height)))))

(defmethod reset ((widget text-widget))
  (setf (cursor-x widget) 0
        (cursor-y widget) 0
        (cursor-column widget) 0
        (cursor-line widget) 0)
  (mezzanine.gui:bitset (height widget) (width widget)
                        (background-colour widget)
                        (framebuffer widget) (y-position widget) (x-position widget))
  (funcall (damage-function widget) (x-position widget) (y-position widget) (width widget) (height widget)))
