(defpackage :mezzanine.gui.fancy-repl
  (:use :cl)
  (:export #:spawn))

(in-package :mezzanine.gui.fancy-repl)

(defvar *default-font* "Monaco")

(defclass fancy-repl (sys.gray:unread-char-mixin
                      sys.int::simple-edit-mixin
                      sys.gray:fundamental-character-input-stream
                      sys.gray:fundamental-character-output-stream)
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%thread :initarg :thread :reader thread)
   (%input :initarg :input :reader input-buffer)
   (%x :initarg :x :accessor cursor-x)
   (%y :initarg :y :accessor cursor-y)
   (%column :initarg :column :accessor cursor-column)
   (%line :initarg :line :accessor cursor-line)
   (%background-colour :initarg :background-colour :accessor background-colour)
   (%foreground-colour :initarg :foreground-colour :accessor foreground-colour)
   (%font :initarg :font :reader font)
   (%closed :initarg :window-closed :accessor window-closed))
  (:default-initargs :input (mezzanine.supervisor:make-fifo 500 :element-type 'character)
                     :x 0
                     :y 0
                     :column 0
                     :line 0
                     :foreground-colour #xFFDCDCCC
                     :background-colour #xFF3E3E3E
                     :window-closed nil))

(defstruct glyph
  ;; Lisp character this glyph represents.
  character
  ;; 8-bit alpha mask for this glyph.
  mask
  ;; Y offset from baseline.
  yoff
  ;; X offset from pen position.
  xoff
  ;; Total width of this character.
  advance)

(defun path-map-line (path function)
  "Iterate over all the line on the contour of the path."
  (loop with iterator = (paths:path-iterator-segmented path)
     for previous-knot = nil then knot
     for (interpolation knot end-p) = (multiple-value-list (paths:path-iterator-next iterator))
     while knot
     when previous-knot
     do (funcall function previous-knot knot)
     until end-p
     finally (when knot
               (funcall function knot (nth-value 1 (paths:path-iterator-next iterator))))))

(defun rasterize-paths (paths sweep-function &optional (scale 1.0))
  (let ((state (aa:make-state)))
    (flet ((do-line (p1 p2)
             (aa:line-f state
                        (* scale (paths:point-x p1)) (* scale (paths:point-y p1))
                        (* scale (paths:point-x p2)) (* scale (paths:point-y p2)))))
      (loop for path in paths
         do (path-map-line path #'do-line)))
    (aa:cells-sweep state sweep-function)))

(defun normalize-alpha (alpha)
  (min 255 (abs alpha)))

(defun scale-bb (bb scale)
  (vector (floor (* (zpb-ttf:xmin bb) scale)) (floor (* (zpb-ttf:ymin bb) scale))
          (ceiling (* (zpb-ttf:xmax bb) scale)) (ceiling (* (zpb-ttf:ymax bb) scale))))

(defun rasterize-glyph (glyph scale)
  (let* ((bb (scale-bb (zpb-ttf:bounding-box glyph) scale))
         (width (- (zpb-ttf:xmax bb) (zpb-ttf:xmin bb)))
         (height (- (zpb-ttf:ymax bb) (zpb-ttf:ymin bb)))
         (array (make-array (list height width)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (paths (paths-ttf:paths-from-glyph glyph
                                            :scale-x scale
                                            :scale-y (- scale)
                                            :offset (paths:make-point 0 (zpb-ttf:ymax bb))
                                            :auto-orient :cw)))
    (rasterize-paths paths (lambda (x y alpha)
                             (setf (aref array y (- x (zpb-ttf:xmin bb)))
                                   (normalize-alpha alpha))))
    array))

(defun expand-bit-mask-to-ub8-mask (mask)
  (let ((new (make-array (array-dimensions mask) :element-type '(unsigned-byte 8))))
    (dotimes (y (array-dimension mask 0))
      (dotimes (x (array-dimension mask 1))
        (setf (aref new y x) (* #xFF (aref mask y x)))))
    new))

(defclass font ()
  ((%font-loader :initarg :loader :reader font-loader)
   (%font-size :initarg :size :reader font-size)
   (%font-scale :reader font-scale)
   (%line-height :reader line-height)
   (%font-ascender :reader font-ascender)
   (%glyph-cache :reader glyph-cache)))

(defmethod initialize-instance :after ((font font) &key loader size &allow-other-keys)
  (setf (slot-value font '%font-scale) (/ size (float (zpb-ttf:units/em loader)))
        (slot-value font '%line-height) (round (* (+ (zpb-ttf:ascender loader)
                                                     (- (zpb-ttf:descender loader))
                                                     (zpb-ttf:line-gap loader))
                                                  (font-scale font)))
        (slot-value font '%font-ascender) (round (* (zpb-ttf:ascender loader)
                                                    (font-scale font)))
        (slot-value font '%glyph-cache) (make-array 17 :initial-element nil)))

(defun find-font (name &optional (errorp t))
  "Return the truename of the font named NAME"
  (truename (make-pathname :name name :type "ttf" :defaults "LOCAL:>Fonts>" #+(or)"SYS:FONTS;")))

(defun open-font (name size)
  (make-instance 'font
                 :loader (zpb-ttf:open-font-loader (find-font name))
                 :size size))

(defun close-font (font)
  (zpb-ttf:close-font-loader (font-loader font)))

(defmacro with-font ((font name size) &body body)
  `(let (,font)
    (unwind-protect
         (progn
           (setf ,font (open-font ,name ,size))
           ,@body)
      (when ,font
        (close-font ,font)))))

(defgeneric character-to-glyph (font character))

(defmethod character-to-glyph ((font font) character)
  ;; TODO: char-bits
  (let* ((code (char-code character))
         (plane (ash code -16))
         (cell (logand code #xFFFF))
         (main-cache (glyph-cache font))
         (cell-cache (aref main-cache plane)))
    (when (not cell-cache)
      (setf cell-cache (make-array (expt 2 16) :initial-element nil)
            (aref main-cache plane) cell-cache))
    (let ((glyph (aref cell-cache cell)))
      (when (not glyph)
        ;; Glyph does not exist in the cache, rasterize it.
        (cond ((zpb-ttf:glyph-exists-p code (font-loader font))
               (let* ((ttf-glyph (zpb-ttf:find-glyph code (font-loader font)))
                      (scale (font-scale font))
                      (bb (scale-bb (zpb-ttf:bounding-box ttf-glyph) scale))
                      (advance (round (* (zpb-ttf:advance-width ttf-glyph) scale))))
                 (setf glyph (make-glyph :character (code-char code)
                                         :mask (rasterize-glyph ttf-glyph scale)
                                         :yoff (zpb-ttf:ymax bb)
                                         :xoff (zpb-ttf:xmin bb)
                                         :advance advance)
                       (aref cell-cache cell) glyph)))
              (t ;; Use Unifont fallback.
               (let ((mask (or (sys.int::map-unifont-2d (code-char code))
                               (sys.int::map-unifont-2d #\WHITE_VERTICAL_RECTANGLE))))
                 (setf glyph (make-glyph :character (code-char code)
                                         :mask (expand-bit-mask-to-ub8-mask mask)
                                         :yoff 14
                                         :xoff 0
                                         :advance (array-dimension mask 1))
                       (aref cell-cache cell) glyph)))))
      glyph)))

(defgeneric dispatch-event (window event)
  ;; Eat unknown events.
  (:method (w e)))

(defmethod dispatch-event (window (event mezzanine.gui.compositor:window-activation-event))
  (draw-window-frame (window window) (mezzanine.gui.compositor:state event) "REPL" (font window))
  (mezzanine.gui.compositor:damage-window (window window)
                                          0 0
                                          (mezzanine.gui.compositor:width (window window))
                                          (mezzanine.gui.compositor:height (window window))))

(defmethod dispatch-event (window (event mezzanine.gui.compositor:key-event))
  ;; should filter out strange keys?
  (when (not (mezzanine.gui.compositor:key-releasep event))
    (mezzanine.supervisor:fifo-push (mezzanine.gui.compositor:key-key event) (input-buffer window) nil)))

(defmethod dispatch-event (window (event mezzanine.gui.compositor:mouse-event))
  ;; Check for close button click.
  (when (and (< 5 (mezzanine.gui.compositor:mouse-x-position event) 19)
             (< 2 (mezzanine.gui.compositor:mouse-y-position event) 16)
             ;; Mouse1 up
             (logbitp 0 (mezzanine.gui.compositor:mouse-button-change event))
             (not (logbitp 0 (mezzanine.gui.compositor:mouse-button-state event))))
    (mezzanine.gui.compositor:close-window (window window))))

(defmethod dispatch-event (window (event mezzanine.gui.compositor:window-close-event))
  (throw 'mezzanine.supervisor::terminate-thread nil)
  (setf (window-closed window) t))

(defun pump-event-loop (window)
  "Read & dispatch window events until there are no more waiting events."
  (loop
     (let ((evt (mezzanine.supervisor:fifo-pop (fifo window) nil)))
       (when (not evt)
         (return))
       (dispatch-event window evt))))

(defmethod sys.gray:stream-read-char ((stream fancy-repl))
  (loop
     ;; Catch up with window manager events.
     (pump-event-loop stream)
     (when (window-closed stream)
       (return :eof))
     ;; Check for an available character.
     (let ((ch (mezzanine.supervisor:fifo-pop (input-buffer stream) nil)))
       (when ch
         (return ch)))
     ;; Block until the next window event.
     (dispatch-event stream (mezzanine.supervisor:fifo-pop (fifo stream)))))

(defmethod sys.gray:stream-terpri ((stream fancy-repl))
  ;; Catch up with window manager events.
  (pump-event-loop stream)
  (let* ((x (cursor-x stream))
         (y (cursor-y stream))
         (window (window stream))
         (fb (mezzanine.gui.compositor:window-buffer window))
         (win-width (mezzanine.gui.compositor:width window))
         (win-height (mezzanine.gui.compositor:height window))
         (line-height (line-height (font stream))))
    (multiple-value-bind (left right top bottom)
        (window-frame-size window)
      ;; Clear to the end of the current line.
      (mezzanine.gui:bitset line-height (- win-width left right x) (background-colour stream) fb (+ top y) (+ left x))
      (mezzanine.gui.compositor:damage-window window (+ left x) (+ top y) (- win-width left right x) line-height)
      ;; Advance to the next line.
      (setf (cursor-x stream) 0
            (cursor-column stream) 0)
      (cond ((> (+ y (* line-height 2)) (- win-height top bottom))
             ;; Off the end of the screen. Scroll!
             (incf (cursor-line stream) line-height)
             (mezzanine.gui:bitblt (- win-height top bottom line-height) (- win-width left right)
                                   fb (+ top line-height) left
                                   fb top left)
             ;; Clear line.
             (mezzanine.gui:bitset line-height (- win-width left right) (background-colour stream) fb (+ top y) left)
             ;; Damage the whole window.
             (mezzanine.gui.compositor:damage-window window left top (- win-width left right) (- win-height top bottom)))
            (t (incf y line-height)
               (setf (cursor-y stream) y)
               ;; Clear line.
               (mezzanine.gui:bitset line-height (- win-width left right) (background-colour stream) fb (+ top y) left)
               (mezzanine.gui.compositor:damage-window window left (+ top y) (- win-width left right) line-height))))))

(defmethod sys.gray:stream-write-char ((stream fancy-repl) character)
  ;; Catch up with window manager events.
  (pump-event-loop stream)
  (cond
    ((eql character #\Newline)
     (sys.gray:stream-terpri stream))
    (t (let* ((glyph (character-to-glyph (font stream) character))
              (mask (glyph-mask glyph))
              (width (glyph-advance glyph))
              (window (window stream))
              (fb (mezzanine.gui.compositor:window-buffer window))
              (win-width (mezzanine.gui.compositor:width window))
              (line-height (line-height (font stream))))
         (multiple-value-bind (left right top bottom)
             (window-frame-size window)
           (when (> (+ (cursor-x stream) width) (- win-width left right))
             (sys.gray:stream-terpri stream))
           ;; Fetch x/y after terpri.
           (let ((x (cursor-x stream))
                 (y (cursor-y stream)))
             (mezzanine.gui:bitset line-height width (background-colour stream) fb (+ top y) (+ left x))
             (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1) (foreground-colour stream)
                                                    mask 0 0
                                                    fb (+ top (- (+ y (font-ascender (font stream))) (glyph-yoff glyph))) (+ left x (glyph-xoff glyph)))
             (mezzanine.gui.compositor:damage-window window (+ left x) (+ top y) width line-height)
             (incf (cursor-x stream) width)
             (incf (cursor-column stream))))))))

(defmethod sys.gray:stream-start-line-p ((stream fancy-repl))
  (zerop (cursor-x stream)))

(defmethod sys.gray:stream-line-column ((stream fancy-repl))
  (cursor-column stream))

(defmethod sys.int::stream-cursor-pos ((stream fancy-repl))
  (values (cursor-x stream)
          (+ (cursor-line stream)
             (cursor-y stream))))

(defmethod sys.int::stream-move-to ((stream fancy-repl) x y)
  (check-type x integer)
  (check-type y integer)
  (setf (cursor-x stream) x
        (cursor-y stream) (max (- y (cursor-line stream)) 0)))

(defmethod sys.int::stream-character-width ((stream fancy-repl) character)
  (glyph-advance (character-to-glyph (font stream) character)))

(defmethod sys.int::stream-compute-motion ((stream fancy-repl) string &optional (start 0) end initial-x initial-y)
  (unless end (setf end (length string)))
  (unless initial-x (setf initial-x (cursor-x stream)))
  (unless initial-y (setf initial-y (+ (cursor-line stream)
                                       (cursor-y stream))))
  (multiple-value-bind (left right top bottom)
      (window-frame-size (window stream))
    (do* ((window (window stream))
          (framebuffer (mezzanine.gui.compositor:window-buffer window))
          (win-width (mezzanine.gui.compositor:width window))
          (win-height (mezzanine.gui.compositor:height window))
          (line-height (line-height (font stream)))
          (i start (1+ i)))
         ((>= i end)
          (values initial-x initial-y))
      (let* ((ch (char string i))
             (width (sys.int::stream-character-width stream ch)))
        (when (or (eql ch #\Newline)
                  (> (+ initial-x width) (- win-width left right)))
          (setf initial-x 0
                initial-y (if (>= (+ initial-y line-height) (- win-height top bottom))
                              0
                              (+ initial-y line-height))))
        (unless (eql ch #\Newline)
          (incf initial-x width))))))

(defmethod sys.int::stream-clear-between ((stream fancy-repl) start-x start-y end-x end-y)
  (let* ((window (window stream))
         (framebuffer (mezzanine.gui.compositor:window-buffer window))
         (win-width (mezzanine.gui.compositor:width window))
         (win-height (mezzanine.gui.compositor:height window))
         (colour (background-colour stream))
         (line-height (line-height (font stream))))
    (multiple-value-bind (left right top bottom)
        (window-frame-size window)
      (setf start-y (- start-y (cursor-line stream))
            end-y (- end-y (cursor-line stream)))
      (cond ((eql start-y end-y)
             ;; Clearing one line.
             (mezzanine.gui:bitset line-height (- end-x start-x) colour framebuffer (+ top start-y) (+ left start-x))
             (mezzanine.gui.compositor:damage-window window (+ left start-x) (+ top start-y) (- end-x start-x) line-height))
            (t ;; Clearing many lines.
             ;; Clear top line.
             (mezzanine.gui:bitset line-height (- win-width left right start-x) colour
                                   framebuffer (+ top start-y) (+ left start-x))
             (mezzanine.gui.compositor:damage-window window (+ left start-x) (+ top start-y) (- win-width left right start-x) line-height)
             ;; Clear in-between.
             (when (> (- end-y start-y) line-height)
               (mezzanine.gui:bitset (- end-y start-y line-height) (- win-width left right) colour
                                     framebuffer (+ top start-y line-height) left)
               (mezzanine.gui.compositor:damage-window window left (+ top start-y line-height) win-width (- end-y start-y line-height)))
             ;; Clear bottom line.
             (mezzanine.gui:bitset line-height end-x colour
                                   framebuffer (+ top end-y) left)
             (mezzanine.gui.compositor:damage-window window left (+ top end-y) end-x line-height))))))

(defvar *corner-mask*
  #2A((0.0 0.0 0.0 0.2 0.5)
      (0.0 0.0 0.5 1.0 1.0)
      (0.0 0.5 1.0 1.0 1.0)
      (0.2 1.0 1.0 1.0 1.0)
      (0.5 1.0 1.0 1.0 1.0))
  "Alpha values for the rounded window corners.")

(defun draw-window-frame (window active title font)
  (let ((framebuffer (mezzanine.gui.compositor:window-buffer window))
        (win-width (mezzanine.gui.compositor:width window))
        (win-height (mezzanine.gui.compositor:height window))
        (colour (if active #xFF8CD0D3 #xFF366060)))
    ;; Top.
    (mezzanine.gui:bitset 19 win-width colour framebuffer 0 0)
    ;; Bottom.
    (mezzanine.gui:bitset 1 win-width colour framebuffer (1- win-height) 0)
    ;; Left.
    (mezzanine.gui:bitset win-height 1 colour framebuffer 0 0)
    ;; Right.
    (mezzanine.gui:bitset win-height 1 colour framebuffer 0 (1- win-width))
    ;; Round off the corners.
    (let* ((corner-width (array-dimension *corner-mask* 1))
           (corner-height (array-dimension *corner-mask* 0)))
      (dotimes (y corner-height)
        (dotimes (x corner-width)
          (let ((alpha (truncate (* (aref *corner-mask* y x) 255))))
            (setf (ldb (byte 8 24) (aref framebuffer y x)) alpha
                  (ldb (byte 8 24) (aref framebuffer y (- win-width x 1))) alpha)))))
    ;; Quit button.
    (mezzanine.gui:bitset 13 13 #xFFBC8383 framebuffer 3 6)
    ;; Title.
    (when title
      (let ((width 0))
        ;; How wide is the title text?
        (dotimes (i (length title))
          (incf width (glyph-advance (character-to-glyph font (char title i)))))
        ;; Clamp it, corner elements and buttons.
        (setf width (mezzanine.gui:clamp width 0 (- win-width (+ 16 (* (array-dimension *corner-mask* 1) 2)))))
        ;; Find leftmost position.
        (let ((origin (- (truncate win-width 2) (truncate width 2)))
              (pen 0))
          ;; Write characters.
          (dotimes (i (length title))
            (let* ((glyph (character-to-glyph font (char title i)))
                   (mask (glyph-mask glyph)))
              (when (> pen width)
                (return))
              (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1) #xFF3F3F3F
                                                     mask 0 0
                                                     framebuffer (- (+ 3 (font-ascender font)) (glyph-yoff glyph)) (+ origin pen (glyph-xoff glyph)))
              (incf pen (glyph-advance glyph)))))))))

(defun window-frame-size (window)
  ;; left, right, top, bottom.
  (values 1 1 19 1))

(defun repl-main ()
  (with-font (font *default-font* 12)
    (let* ((fifo (mezzanine.supervisor:make-fifo 50))
           (window (mezzanine.gui.compositor:make-window fifo 800 300))
           (framebuffer (mezzanine.gui.compositor:window-buffer window))
           (term (make-instance 'fancy-repl
                                :fifo fifo
                                :window window
                                :thread (mezzanine.supervisor:current-thread)
                                :font font))
           (*standard-input* term)
           (*standard-output* term)
           ;(*terminal-io* term)
           ;(*standard-input* (make-synonym-stream '*terminal-io*))
           ;(*standard-output* *standard-input*)
           ;(*error-output* *standard-input*)
           ;(*query-io* *standard-input*)
           ;(*trace-output* *standard-input*)
           ;(*debug-io* *standard-input*)
           )
      (multiple-value-bind (left right top bottom)
          (window-frame-size window)
        (mezzanine.gui:bitset (- (mezzanine.gui.compositor:height window) top bottom)
                              (- (mezzanine.gui.compositor:width window) left right)
                              (background-colour term)
                              framebuffer top left))
      (draw-window-frame window nil "REPL" (font term))
      (mezzanine.gui.compositor:damage-window window
                                              0 0
                                              (mezzanine.gui.compositor:width window)
                                              (mezzanine.gui.compositor:height window))
      (unwind-protect
           (sys.int::repl)
        (mezzanine.gui.compositor:close-window window)))))

(defun spawn ()
  (mezzanine.supervisor:make-thread 'repl-main
                                    :name "Fancy Lisp Listener"))
