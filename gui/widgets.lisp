;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.gui.widgets
  (:use :cl :mezzano.gui.font)
  (:import-from :mezzano.gui.compositor
                #:width #:height
                #:mouse-x-position #:mouse-y-position
                #:mouse-button-state #:mouse-button-change)
  (:export #:default-damage-function
           #:default-cursor-function
           #:frame
           #:frame-title
           #:close-button-p
           #:close-button-hover
           #:activep
           #:frame-mouse-event
           #:close-button-clicked
           #:draw-frame
           #:frame-size
           #:resize-frame
           #:text-widget
           #:resize-text-widget
           #:reset
           #:cursor-visible))

(in-package :mezzano.gui.widgets)

(defgeneric draw-frame (frame))
(defgeneric resize-frame (frame new-framebuffer))
(defgeneric frame-size (frame))
(defgeneric frame-mouse-event (frame mouse-event))
(defgeneric reset (object))

(defun default-damage-function (window)
  (lambda (&rest args)
    (apply #'mezzano.gui.compositor:damage-window window args)))

(defun default-cursor-function (window)
  (lambda (cursor)
    (mezzano.gui.compositor:set-window-data window :cursor cursor)))

(define-condition close-button-clicked () ())

(defclass frame ()
  ((%framebuffer :initarg :framebuffer :reader framebuffer)
   (%damage-function :initarg :damage-function :reader damage-function)
   (%set-cursor-function :initarg :set-cursor-function :reader set-cursor-function)
   (%title :initarg :title :accessor frame-title)
   (%close-button-p :initarg :close-button-p :accessor close-button-p)
   (%close-button-hover :initarg :close-button-hover :accessor close-button-hover)
   (%activep :initarg :activep :accessor activep)
   (%resizablep :initarg :resizablep :accessor resizablep))
  (:default-initargs
    :title ""
    :close-button-p nil
    :close-button-hover nil
    :activep nil
    :resizablep nil))

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
  (mezzano.gui:make-surface-from-array
   (mezzano.gui.compositor::2d-array
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
    '(unsigned-byte 32))))

(defvar *close-button-hover*
  (mezzano.gui:make-surface-from-array
   (mezzano.gui.compositor::2d-array
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
    '(unsigned-byte 32))))

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

(defun vertical-gradient (width height colour1 colour2 to-array to-x to-y)
  (dotimes (i height)
    (mezzano.gui:bitset :set
                        width 1
                        (lerp-colour colour1 colour2 (/ i (1- height)))
                        to-array to-x (+ to-y i))))

(defvar *active-frame-colour* (mezzano.gui:make-colour-from-octets #x80 #x80 #x80))
(defvar *active-frame-top-colour* (mezzano.gui:make-colour-from-octets #xFF #xFF #xFF))
(defvar *inactive-frame-colour* (mezzano.gui:make-colour-from-octets #x40 #x40 #x40))
(defvar *inactive-frame-top-colour* (mezzano.gui:make-colour-from-octets #x80 #x80 #x80))

(defvar *frame-title-text-colour* (mezzano.gui:make-colour-from-octets #x3F #x3F #x3F))

(defmethod draw-frame ((frame frame))
  (let* ((framebuffer (framebuffer frame))
         (win-width (mezzano.gui:surface-width framebuffer))
         (win-height (mezzano.gui:surface-height framebuffer))
         (title (frame-title frame))
         (colour (if (activep frame) *active-frame-colour* *inactive-frame-colour*))
         (top-colour (if (activep frame) *active-frame-top-colour* *inactive-frame-top-colour*)))
    ;; Top.
    (vertical-gradient win-width 19
                       top-colour colour
                       framebuffer 0 0)
    ;; Bottom.
    (mezzano.gui:bitset :set
                        win-width 1
                        colour
                        framebuffer 0 (1- win-height))
    ;; Left.
    (mezzano.gui:bitset :set
                        1 win-height
                        colour
                        framebuffer 0 19)
    ;; Right.
    (mezzano.gui:bitset :set
                        1 win-height
                        colour
                        framebuffer (1- win-width) 19)
    ;; Round off the corners.
    (dotimes (y (array-dimension *corner-mask* 0))
      (let ((line-colour (lerp-colour top-colour colour (/ y 19))))
        (dotimes (x (array-dimension *corner-mask* 1))
          (let* ((alpha (aref *corner-mask* y x))
                 (real-colour (mezzano.gui:make-colour (* (mezzano.gui:colour-red line-colour)   alpha)
                                                       (* (mezzano.gui:colour-green line-colour) alpha)
                                                       (* (mezzano.gui:colour-blue line-colour)  alpha)
                                                       (* (mezzano.gui:colour-alpha line-colour) alpha)
                                                       t)))
            (setf (mezzano.gui:surface-pixel framebuffer x y) real-colour
                  (mezzano.gui:surface-pixel framebuffer (- win-width x 1) y) real-colour)))))
    ;; Close button.
    (when (close-button-p frame)
      (mezzano.gui:bitblt :blend
                          (mezzano.gui:surface-width *close-button*) (mezzano.gui:surface-height *close-button*)
                          (if (close-button-hover frame) *close-button-hover* *close-button*)
                          0 0
                          framebuffer *close-button-x* *close-button-y*))
    ;; Title.
    (when title
      (let ((width 0))
        ;; How wide is the title text?
        (dotimes (i (length title))
          (incf width (glyph-advance (character-to-glyph *frame-title-font* (char title i)))))
        ;; Clamp it, corner elements and buttons.
        (setf width (mezzano.gui:clamp width 0 (- win-width (+ 16 (* (array-dimension *corner-mask* 1) 2)))))
        ;; Find leftmost position.
        (let ((origin (- (truncate win-width 2) (truncate width 2)))
              (pen 0))
          ;; Write characters.
          (dotimes (i (length title))
            (let* ((glyph (character-to-glyph *frame-title-font* (char title i)))
                   (mask (glyph-mask glyph)))
              (when (> pen width)
                (return))
              (mezzano.gui:bitset :blend
                                  (mezzano.gui:surface-width mask) (mezzano.gui:surface-height mask)
                                  *frame-title-text-colour*
                                  framebuffer
                                  (+ origin pen (glyph-xoff glyph))
                                  (- (+ 4 (ascender *frame-title-font*)) (glyph-yoff glyph))
                                  mask 0 0)
              (incf pen (glyph-advance glyph)))))))
    ;; Damage the whole window.
    (funcall (damage-function frame) 0 0 win-width win-height)))

(defmethod resize-frame ((frame frame) new-framebuffer)
  (setf (slot-value frame '%framebuffer) new-framebuffer)
  (draw-frame frame))

(defmethod frame-size ((frame frame))
  ;; left, right, top, bottom.
  (values 1 1 19 1))

(defun in-frame-close-button (frame x y)
  (and (close-button-p frame)
       (>= x *close-button-x*)
       (< x (+ *close-button-x* (mezzano.gui:surface-width *close-button*)))
       (>= y *close-button-y*)
       (< y (+ *close-button-y* (mezzano.gui:surface-height *close-button*)))
       ;; Alpha test.
       (> (mezzano.gui:colour-alpha
           (mezzano.gui:surface-pixel *close-button* (- x *close-button-x*) (- y *close-button-y*)))
          0.5)))

(defun in-frame-header-p (frame x y)
  (declare (ignore x))
  (multiple-value-bind (left right top bottom)
      (frame-size frame)
    (declare (ignore left right bottom))
    (< y top)))

(defparameter *border-thickness* 3)
(defparameter *border-corner-size* 10)

(defun in-frame-border-p (frame x y)
  (let* ((framebuffer (framebuffer frame))
         (win-width (mezzano.gui:surface-width framebuffer))
         (win-height (mezzano.gui:surface-height framebuffer)))
    (labels ((near (coord size dist)
               (declare (ignore size))
               (< coord dist))
             (far (coord size dist)
               (>= coord (- size dist)))
             (corner (x y width-fn height-fn)
               (or
                (and (funcall width-fn x win-width *border-thickness*)
                     (funcall height-fn y win-height *border-corner-size*))
                (and (funcall height-fn y win-height *border-thickness*)
                     (funcall width-fn x win-width *border-corner-size*)))))
      (cond
        ;; Corners first.
        ((corner x y #'near #'near)
         :top-left)
        ((corner x y #'far #'near)
         :top-right)
        ((corner x y #'far #'far)
         :bottom-right)
        ((corner x y #'near #'far)
         :bottom-left)
        ((near x win-width *border-thickness*)
         :left)
        ((far x win-width *border-thickness*)
         :right)
        ((near y win-height *border-thickness*)
         :top)
        ((far y win-height *border-thickness*)
         :bottom)))))

(defun border-to-cursor (border)
  (case border
    (:top-left     :arrow-up-left)
    (:top-right    :arrow-up-right)
    (:bottom-right :arrow-down-right)
    (:bottom-left  :arrow-down-left)
    (:left         :arrow-left)
    (:right        :arrow-right)
    (:top          :arrow-up)
    (:bottom       :arrow-down)
    (t             :default)))

(defmethod frame-mouse-event ((frame frame) event)
  (cond ((in-frame-close-button frame
                                (mouse-x-position event)
                                (mouse-y-position event))
         (when (resizablep frame)
           (funcall (set-cursor-function frame) :default))
         (when (not (close-button-hover frame))
           (setf (close-button-hover frame) t)
           (draw-frame frame)
           (funcall (damage-function frame)
                    0 0
                    (mezzano.gui:surface-width (framebuffer frame)) 19))
         ;; Check for close button click.
         (when (and (logbitp 0 (mouse-button-change event))
                    ;; Mouse1 up
                    (not (logbitp 0 (mouse-button-state event))))
           (signal 'close-button-clicked)))
        (t
         (when (close-button-hover frame)
           (setf (close-button-hover frame) nil)
           (draw-frame frame)
           (funcall (damage-function frame)
                    0 0
                    (mezzano.gui:surface-width (framebuffer frame)) 19))
         ;; Check for drag start.
         (let ((border (in-frame-border-p frame
                                          (mouse-x-position event)
                                          (mouse-y-position event)))
               (win (mezzano.gui.compositor:window event)))
           (when (resizablep frame)
             (funcall (set-cursor-function frame)
                      (if border
                          (border-to-cursor border)
                          :default)))
           (cond ((not win))
                 ((not (and (logbitp 0 (mouse-button-change event))
                            ;; Mouse1 down
                            (logbitp 0 (mouse-button-state event)))))
                 ((and (resizablep frame)
                       border)
                  (mezzano.gui.compositor:begin-window-drag
                   win
                   :mode border))
                 ((in-frame-header-p frame
                                     (mouse-x-position event)
                                     (mouse-y-position event))
                  (mezzano.gui.compositor:begin-window-drag
                   win
                   :mode :move)))))))

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
   (%font :initarg :font :reader font)
   (%cursor-visible :initform nil :reader cursor-visible))
  (:default-initargs :foreground-colour mezzano.gui:*default-foreground-colour*
                     :background-colour mezzano.gui:*default-background-colour*))

(defmethod initialize-instance :after ((widget text-widget) &key &allow-other-keys)
  (reset widget))

(defgeneric (setf cursor-visible) (value object))

(defmethod (setf cursor-visible) (value (stream text-widget))
  (flet ((doit ()
           (let ((line-height (line-height (font stream)))
                 (x (+ (x-position stream) (cursor-x stream)))
                 (y (+ (y-position stream) (cursor-y stream))))
             (mezzano.gui:bitset :xor
                                 1 line-height
                                 #x00FFFFFF
                                 (framebuffer stream)
                                 x y)
             (funcall (damage-function stream)
                      x y
                      1 line-height))))
    (cond ((cursor-visible stream)
           (when (not value)
             ;; Cursor was visible, now invisible.
             (doit)))
          (t (when value
               (doit))))
    (setf (slot-value stream '%cursor-visible) value)))

(defmacro with-cursor-hidden ((stream) &body body)
  (let ((stream-sym (gensym))
        (state (gensym)))
    `(let* ((,stream-sym ,stream)
            (,state (cursor-visible ,stream-sym)))
       (unwind-protect
            (progn
              (when ,state
                (setf (cursor-visible ,stream-sym) nil))
              ,@body)
         (when ,state
           (setf (cursor-visible ,stream-sym) t))))))

(defmethod sys.gray:stream-terpri ((stream text-widget))
  (with-cursor-hidden (stream)
    (let* ((x (cursor-x stream))
           (y (cursor-y stream))
           (fb (framebuffer stream))
           (win-width (width stream))
           (win-height (height stream))
           (line-height (line-height (font stream)))
           (left (x-position stream))
           (top (y-position stream)))
      ;; Clear to the end of the current line.
      (mezzano.gui:bitset :set
                          (- win-width x) line-height
                          (background-colour stream)
                          fb (+ left x) (+ top y))
      (funcall (damage-function stream) (+ left x) (+ top y) (- win-width x) line-height)
      ;; Advance to the next line.
      (setf (cursor-x stream) 0
            (cursor-column stream) 0)
      (cond ((> (+ y (* line-height 2)) win-height)
             ;; Off the end of the screen. Scroll!
             (incf (cursor-line stream) line-height)
             (mezzano.gui:bitblt :set
                                 win-width (- win-height line-height)
                                 fb left (+ top line-height)
                                 fb left top)
             ;; Clear line.
             (mezzano.gui:bitset :set
                                 win-width line-height
                                 (background-colour stream)
                                 fb left (+ top y))
             ;; Damage the whole area.
             (funcall (damage-function stream) left top win-width win-height))
            (t (incf y line-height)
               (setf (cursor-y stream) y)
               ;; Clear line.
               (mezzano.gui:bitset :set
                                   win-width line-height
                                   (background-colour stream)
                                   fb left (+ top y))
               (funcall (damage-function stream) left (+ top y) win-width line-height))))))

(defmethod sys.gray:stream-write-char ((stream text-widget) character)
  (cond
    ((eql character #\Newline)
     (sys.gray:stream-terpri stream))
    (t (with-cursor-hidden (stream)
         (let* ((glyph (character-to-glyph (font stream) character))
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
             (mezzano.gui:bitset :set
                                 advance line-height
                                 (background-colour stream)
                                 fb (+ left x) (+ top y))
             (mezzano.gui:bitset :blend
                                 (mezzano.gui:surface-width mask) (mezzano.gui:surface-height mask)
                                 (foreground-colour stream)
                                 fb
                                 (+ left x (glyph-xoff glyph))
                                 (+ top (- (+ y (ascender (font stream))) (glyph-yoff glyph)))
                                 mask 0 0)
             (funcall (damage-function stream) (+ left x) (+ top y) advance line-height)
             (incf (cursor-x stream) advance)
             (incf (cursor-column stream))))))))

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
  (with-cursor-hidden (stream)
    (setf (cursor-x stream) x
          (cursor-y stream) (max (- y (cursor-line stream)) 0))))

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
  (with-cursor-hidden (stream)
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
             (mezzano.gui:bitset :set
                                 (- end-x start-x) line-height
                                 colour
                                 framebuffer (+ left start-x) (+ top start-y))
             (funcall (damage-function stream) (+ left start-x) (+ top start-y) (- end-x start-x) line-height))
            (t ;; Clearing many lines.
             ;; Clear top line.
             (mezzano.gui:bitset :set
                                 (- win-width start-x) line-height
                                 colour
                                 framebuffer (+ left start-x) (+ top start-y))
             (funcall (damage-function stream) (+ left start-x) (+ top start-y) (- win-width start-x) line-height)
             ;; Clear in-between.
             (when (> (- end-y start-y) line-height)
               (mezzano.gui:bitset :set
                                   win-width (- end-y start-y line-height)
                                   colour
                                   framebuffer left (+ top start-y line-height))
               (funcall (damage-function stream) left (+ top start-y line-height) win-width (- end-y start-y line-height)))
             ;; Clear bottom line.
             (mezzano.gui:bitset :set
                                 end-x line-height
                                 colour
                                 framebuffer left (+ top end-y))
             (funcall (damage-function stream) left (+ top end-y) end-x line-height))))))

(defun align-down (value alignment)
  (- value (rem value alignment)))

(defmethod sys.gray:stream-display ((stream text-widget) (object mezzano.gui:surface))
  (with-cursor-hidden (stream)
    (let* ((framebuffer (framebuffer stream))
           (win-width (width stream))
           (win-height (height stream))
           (colour (background-colour stream))
           (line-height (line-height (font stream)))
           (left (x-position stream))
           (top (y-position stream)))
      (cond ((> (mezzano.gui:surface-height object) line-height)
             ;; Object exceeds the height of a line, render it on it's own line.
             (let ((n-lines (ceiling (mezzano.gui:surface-height object) line-height)))
               (fresh-line stream)
               ;; Make space.
               (dotimes (i (1- n-lines))
                 (terpri stream))
               ;; Draw.
               (let* ((draw-width (min win-width (mezzano.gui:surface-width object)))
                      (draw-height (min (align-down win-height line-height)
                                        (mezzano.gui:surface-height object)))
                      (ypos (- (+ top (cursor-y stream) line-height) draw-height)))
                 (mezzano.gui:bitblt :blend draw-width draw-height
                                     object
                                     0
                                     (- (mezzano.gui:surface-height object) draw-height)
                                     framebuffer
                                     left
                                     ypos)
                 (funcall (damage-function stream) left ypos draw-width draw-height)
                 (setf (cursor-x stream) draw-width)
                 (terpri stream))))
            (t
             ;; Fits on one line, render it like a character.
             (let ((object-width (min win-width (mezzano.gui:surface-width object)))
                   (object-height (mezzano.gui:surface-height object)))
               (when (> (+ (cursor-x stream) object-width) win-width)
                 (sys.gray:stream-terpri stream))
               ;; Fetch x/y after terpri.
               (let ((x (cursor-x stream))
                     (y (cursor-y stream)))
                 (mezzano.gui:bitset :set
                                     object-width line-height
                                     (background-colour stream)
                                     framebuffer (+ left x) (+ top y))
                 (mezzano.gui:bitblt :blend
                                     object-width object-height
                                     object 0 0
                                     framebuffer
                                     (+ left x)
                                     (+ top y (- line-height object-height)))
                 (funcall (damage-function stream) (+ left x) (+ top y) object-width line-height)
                 (incf (cursor-x stream) object-width)
                 (incf (cursor-column stream)))))))))

(defgeneric resize-text-widget (widget framebuffer x-position y-position width height))

(defmethod resize-text-widget ((widget text-widget) framebuffer x-position y-position width height)
  ;; Fill the new framebuffer with the background colour.
  ;; The copy from the old framebuffer might not cover the entire new area.
  (mezzano.gui:bitset :set
                      width height
                      (background-colour widget)
                      framebuffer
                      x-position y-position)
  ;; Copy the old framebuffer to the new framebuffer,
  ;; and adjust the cursor position to be within limits.
  (let* ((y (cursor-y widget))
         (line-height (line-height (font widget)))
         (adjusted-y (min y (* (1- (truncate height line-height)) line-height)))
         (y-delta (- y adjusted-y)))
    (setf (cursor-y widget) adjusted-y)
    (incf (cursor-line widget) y-delta)
    (mezzano.gui:bitblt :set
                        (min width (width widget)) (min height (- (height widget) y-delta))
                        (framebuffer widget)
                        (x-position widget) (+ (y-position widget) y-delta)
                        framebuffer
                        x-position y-position))
  (setf (slot-value widget '%framebuffer) framebuffer
        (slot-value widget '%x-position) x-position
        (slot-value widget '%y-position) y-position
        (slot-value widget '%width) width
        (slot-value widget '%height) height)
  (funcall (damage-function widget) (x-position widget) (y-position widget) (width widget) (height widget)))

(defmethod reset ((widget text-widget))
  (setf (cursor-x widget) 0
        (cursor-y widget) 0
        (cursor-column widget) 0
        (cursor-line widget) 0
        (cursor-visible widget) nil)
  (mezzano.gui:bitset :set
                      (width widget) (height widget)
                      (background-colour widget)
                      (framebuffer widget)
                      (x-position widget) (y-position widget))
  (funcall (damage-function widget) (x-position widget) (y-position widget) (width widget) (height widget)))
