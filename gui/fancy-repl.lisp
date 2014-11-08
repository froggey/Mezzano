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
   (%font :initarg :font :reader font))
  (:default-initargs :input (mezzanine.supervisor:make-fifo 500 :element-type 'character)
                     :x 0
                     :y 0
                     :column 0
                     :line 0
                     :foreground-colour #xFFDCDCCC
                     :background-colour #xFF3E3E3E))

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

(defmethod dispatch-event (window (event mezzanine.gui.compositor:key-event))
  ;; should filter out strange keys?
  (when (not (mezzanine.gui.compositor:key-releasep event))
    (mezzanine.supervisor:fifo-push (mezzanine.gui.compositor:key-key event) (input-buffer window) nil)))

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
    ;; Clear to the end of the current line.
    (mezzanine.gui:bitset line-height (- win-width x) (background-colour stream) fb y x)
    (mezzanine.gui.compositor:damage-window window x y (- win-width x) line-height)
    ;; Advance to the next line.
    (setf (cursor-x stream) 0
          (cursor-column stream) 0)
    (cond ((> (+ y (* line-height 2)) win-height)
           ;; Off the end of the screen. Scroll!
           (incf (cursor-line stream) line-height)
           (mezzanine.gui:bitblt (- win-height line-height) win-width
                                 fb line-height 0
                                 fb 0 0)
           ;; Clear line.
           (mezzanine.gui:bitset line-height win-width (background-colour stream) fb y 0)
           ;; Damage the whole window.
           (mezzanine.gui.compositor:damage-window window 0 0 win-width win-height))
          (t (incf y line-height)
             (setf (cursor-y stream) y)
             ;; Clear line.
             (mezzanine.gui:bitset line-height win-width (background-colour stream) fb y 0)
             (mezzanine.gui.compositor:damage-window window 0 y win-width line-height)))))

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
              (win-height (mezzanine.gui.compositor:height window))
              (line-height (line-height (font stream))))
         (when (> (+ (cursor-x stream) width) win-width)
           (sys.gray:stream-terpri stream))
         (let ((x (cursor-x stream))
               (y (cursor-y stream)))
           (mezzanine.gui:bitset line-height width (background-colour stream) fb y x)
           (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1) (foreground-colour stream)
                                                  mask 0 0
                                                  fb (- (+ y (font-ascender (font stream))) (glyph-yoff glyph)) (+ x (glyph-xoff glyph)))
           (mezzanine.gui.compositor:damage-window window x y width line-height)
           (incf (cursor-x stream) width)
           (incf (cursor-column stream)))))))

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
                (> (+ initial-x width) win-width))
        (setf initial-x 0
              initial-y (if (>= (+ initial-y line-height) win-height)
                            0
                            (+ initial-y line-height))))
      (unless (eql ch #\Newline)
        (incf initial-x width)))))

(defmethod sys.int::stream-clear-between ((stream fancy-repl) start-x start-y end-x end-y)
  (let* ((window (window stream))
         (framebuffer (mezzanine.gui.compositor:window-buffer window))
         (win-width (mezzanine.gui.compositor:width window))
         (win-height (mezzanine.gui.compositor:height window))
         (colour (background-colour stream))
         (line-height (line-height (font stream))))
    (setf start-y (- start-y (cursor-line stream))
          end-y (- end-y (cursor-line stream)))
    (cond ((eql start-y end-y)
           ;; Clearing one line.
           (mezzanine.gui:bitset line-height (- end-x start-x) colour framebuffer start-y start-x)
           (mezzanine.gui.compositor:damage-window window start-x start-y (- end-x start-x) line-height))
          (t ;; Clearing many lines.
           ;; Clear top line.
           (mezzanine.gui:bitset line-height (- win-width start-x) colour
                                 framebuffer start-y start-x)
           (mezzanine.gui.compositor:damage-window window start-x start-y (- win-width start-x) line-height)
           ;; Clear in-between.
           (when (> (- end-y start-y) line-height)
             (mezzanine.gui:bitset (- end-y start-y line-height) win-width colour
                                   framebuffer (+ start-y line-height) 0)
             (mezzanine.gui.compositor:damage-window window 0 (+ start-y line-height) win-width (- end-y start-y line-height)))
           ;; Clear bottom line.
           (mezzanine.gui:bitset line-height end-x colour
                                 framebuffer end-y 0)
           (mezzanine.gui.compositor:damage-window window 0 end-y end-x line-height)))))

(defun repl-main ()
  (with-font (font *default-font* 12)
    (let* ((fifo (mezzanine.supervisor:make-fifo 50))
           (window (mezzanine.gui.compositor:make-window fifo 800 600))
           (framebuffer (mezzanine.gui.compositor:window-buffer window))
           (term (make-instance 'fancy-repl
                                :fifo fifo
                                :window window
                                :thread (mezzanine.supervisor:current-thread)
                                :font font))
           ;(*standard-input* term)
           ;(*standard-output* term)
           (*terminal-io* term)
           (*standard-input* (make-synonym-stream '*terminal-io*))
           (*standard-output* *standard-input*)
           (*error-output* *standard-input*)
           (*query-io* *standard-input*)
           (*trace-output* *standard-input*)
           (*debug-io* *standard-input*)
           )
      (mezzanine.gui:bitset (mezzanine.gui.compositor:height window)
                            (mezzanine.gui.compositor:width window)
                            (background-colour term)
                            framebuffer 0 0)
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
