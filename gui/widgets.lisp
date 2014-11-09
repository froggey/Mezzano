(defpackage :mezzanine.gui.widgets
  (:use :cl :mezzanine.gui.font)
  (:export #:frame
           #:frame-title
           #:close-button-p
           #:activep
           #:draw-frame
           #:frame-size))

(in-package :mezzanine.gui.widgets)

(defgeneric draw-frame (frame))
(defgeneric frame-size (frame))

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
