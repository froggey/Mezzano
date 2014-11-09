(defpackage :mezzanine.gui.widgets
  (:use :cl :mezzanine.gui.font)
  (:import-from :mezzanine.gui.compositor
                #:width #:height)
  (:export #:frame
           #:frame-title
           #:close-button-p
           #:activep
           #:in-frame-close-button
           #:draw-frame
           #:frame-size
           #:text-widget
           #:reset))

(in-package :mezzanine.gui.widgets)

(defgeneric draw-frame (frame))
(defgeneric frame-size (frame))
(defgeneric in-frame-close-button (frame x y))
(defgeneric reset (object))

(defclass frame ()
  ((%framebuffer :initarg :framebuffer :reader framebuffer)
   (%title :initarg :title :accessor frame-title)
   (%close-button-p :initarg :close-button-p :accessor close-button-p)
   (%activep :initarg :activep :accessor activep))
  (:default-initargs :title "" :close-button-p nil :activep nil))

(defvar *frame-title-font* (open-font "Helvetica" 12))

(defvar *corner-mask*
  #2A((0.0 0.0 0.0 0.2 0.5)
      (0.0 0.0 0.5 1.0 1.0)
      (0.0 0.5 1.0 1.0 1.0)
      (0.2 1.0 1.0 1.0 1.0)
      (0.5 1.0 1.0 1.0 1.0))
  "Alpha values for the rounded window corners.")

(defmethod draw-frame ((frame frame))
  (let* ((framebuffer (framebuffer frame))
         (win-width (array-dimension framebuffer 1))
         (win-height (array-dimension framebuffer 0))
         (title (frame-title frame))
         (colour (if (activep frame) #xFF8CD0D3 #xFF366060)))
    ;; Top.
    (mezzanine.gui:bitset 19 win-width colour framebuffer 0 0)
    ;; Bottom.
    (mezzanine.gui:bitset 1 win-width colour framebuffer (1- win-height) 0)
    ;; Left.
    (mezzanine.gui:bitset win-height 1 colour framebuffer 0 0)
    ;; Right.
    (mezzanine.gui:bitset win-height 1 colour framebuffer 0 (1- win-width))
    ;; Round off the corners.
    (dotimes (y (array-dimension *corner-mask* 0))
      (dotimes (x (array-dimension *corner-mask* 1))
        (let ((alpha (truncate (* (aref *corner-mask* y x) 255))))
          (setf (ldb (byte 8 24) (aref framebuffer y x)) alpha
                (ldb (byte 8 24) (aref framebuffer y (- win-width x 1))) alpha))))
    ;; Quit button.
    (when (close-button-p frame)
      (mezzanine.gui:bitset 13 13 #xFFBC8383 framebuffer 3 6))
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
              (incf pen (glyph-advance glyph)))))))))

(defmethod frame-size ((frame frame))
  ;; left, right, top, bottom.
  (values 1 1 19 1))

(defmethod in-frame-close-button ((frame frame) x y)
  (and (close-button-p frame)
       (< 5 x 19)
       (< 2 y 16)))

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
