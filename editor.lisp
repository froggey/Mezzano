(defpackage :mezzanine.editor
  (:use :cl)
  (:export #:spawn))

(in-package :mezzanine.editor)

(defclass line ()
  ((%next :initarg :next :accessor next)
   (%prev :initarg :prev :accessor prev)
   (%data :initarg :data :accessor data))
  (:default-initargs :next nil
                     :prev nil
                     :data (make-array 50 :element-type 'character :adjustable t :fill-pointer 0)))

(defclass editor ()
  ((%fifo :initarg :fifo :reader fifo)
   (%pending-event :initarg :pending-event :accessor pending-event)
   (%pending-redisplay :initarg :pending-redisplay :accessor pending-redisplay)
   (%font :initarg :font :reader font)
   (%window :initarg :window :accessor window)
   (%frame :initarg :frame :accessor frame)
   (%first-line :initarg :first-line :accessor first-line)
   (%last-line :initarg :last-line :accessor last-line)
   (%point-line :initarg :point-line :accessor point-line)
   (%point-offset :initarg :point-offset :accessor point-offset)
   (%point-column-hint :initarg :point-column-hint :accessor point-column-hint)
   (%foreground-colour :initarg :foreground-colour :accessor foreground-colour)
   (%background-colour :initarg :background-colour :accessor background-colour)
   (%window-top :initarg :window-top :accessor window-top))
  (:default-initargs :pending-event nil
                     :pending-redisplay nil
                     :csr-x 0 :csr-y 0 :csr-char #\Space
                     :foreground-colour #xFFDCDCCC
                     :background-colour #xFF3E3E3E))

(defvar *editor*)

(defgeneric dispatch-event (editor event)
  (:method (editor event)))

(defmethod dispatch-event (editor (event mezzanine.gui.compositor:window-activation-event))
  (setf (mezzanine.gui.widgets:activep (frame editor)) (mezzanine.gui.compositor:state event))
  (mezzanine.gui.widgets:draw-frame (frame editor)))

(defmethod dispatch-event (editor (event mezzanine.gui.compositor:mouse-event))
  (handler-case
      (mezzanine.gui.widgets:frame-mouse-event (frame editor) event)
    (mezzanine.gui.widgets:close-button-clicked ()
      (throw 'quit nil))))

(defmethod dispatch-event (editor (event mezzanine.gui.compositor:window-close-event))
  (declare (ignore editor event))
  (throw 'quit nil))

(defmethod dispatch-event (editor (event mezzanine.gui.compositor:key-event))
  (when (not (mezzanine.gui.compositor:key-releasep event))
    (throw 'next-character
      (if (mezzanine.gui.compositor:key-modifier-state event)
          (sys.int::make-character (char-code (char-upcase (mezzanine.gui.compositor:key-key event)))
                                   :control (find :control (mezzanine.gui.compositor:key-modifier-state event))
                                   :meta (find :meta (mezzanine.gui.compositor:key-modifier-state event))
                                   :super (find :super (mezzanine.gui.compositor:key-modifier-state event))
                                   :hyper (find :hyper (mezzanine.gui.compositor:key-modifier-state event)))
          (mezzanine.gui.compositor:key-key event)))))

(defun editor-read-char ()
  (catch 'next-character
    (when (pending-event *editor*)
      (let ((event (pending-event *editor*)))
        (setf (pending-event *editor*) nil)
      (dispatch-event *editor* event)))
    (when (pending-redisplay *editor*)
      (throw 'next-character nil))
    (loop
       (dispatch-event *editor* (mezzanine.supervisor:fifo-pop (fifo *editor*))))))

(defun input-waiting ()
  (when (pending-event *editor*)
    (return-from input-waiting t))
  (let ((event (mezzanine.supervisor:fifo-pop (fifo *editor*) nil)))
    (when event
      (setf (pending-event *editor*) event)
      (return-from input-waiting t))
    nil))

(defgeneric line-length (line))

(defmethod line-length ((line line))
  (length (data line)))

;;; Sub-editor. Buffer manipulation.

(defun insert-line ()
  "Insert a new line after the point, splitting the current line if needed.
The point is positioned at the start of the new line."
  (let* ((current-line (point-line *editor*))
         (new-line (make-instance 'line
                                  :next (next current-line)
                                  :prev current-line
                                  :data (make-array (- (line-length current-line)
                                                       (point-offset *editor*))
                                                    :element-type 'character
                                                    :adjustable t
                                                    :fill-pointer t))))
    (replace (data new-line) (data current-line)
             :start2 (point-offset *editor*))
    (cond ((next current-line)
           (setf (prev (next current-line)) new-line))
          (t (setf (last-line *editor*) new-line)))
    (setf (next current-line) new-line
          (fill-pointer (data current-line)) (point-offset *editor*))
    (setf (point-line *editor*) new-line
          (point-offset *editor*) 0
          (point-column-hint *editor*) 0))
  (values))

(defun insert-char (ch)
  "Insert CH after point, then advance point."
  (let ((current-line (point-line *editor*)))
    (cond ((eql (line-length current-line) (point-offset *editor*))
           ;; Inserting at end.
           (vector-push-extend ch (data current-line)))
          (t ;; Inserting in the middle or at the start.
           ;; Make sure the vector is long enough.
           (vector-push-extend ch (data current-line))
           (replace (data current-line) (data current-line)
                    :start1 (1+ (point-offset *editor*))
                    :start2 (point-offset *editor*))
           (setf (aref (data current-line) (point-offset *editor*)) ch))))
  (forward-char))

(defun kill-line ()
  (let ((current-line (point-line *editor*)))
    (cond ((eql (line-length current-line) (point-offset *editor*))
           ;; At end of line. Merge current line with next.
           (when (next current-line)
             (let ((next-line (next current-line))
                   (len (line-length current-line)))
               (adjust-array (data current-line)
                             (+ len
                                (line-length next-line))
                             :fill-pointer t)
               (replace (data current-line) (data next-line)
                        :start1 len)
               (cond ((next next-line)
                      (setf (prev (next next-line)) current-line))
                     (t (setf (last-line *editor*) current-line)))
               (setf (next current-line) (next next-line)))))
          (t ;; At start or middle of line. Trim line.
           (setf (fill-pointer (data current-line)) (point-offset *editor*)))))
  (values))

(defun delete-one-char-forward ()
  (let ((current-line (point-line *editor*)))
    (cond ((eql (point-offset *editor*) (line-length current-line))
           (delete-line))
          (t
           (replace (data current-line) (data current-line)
                    :start1 (point-offset *editor*)
                    :start2 (1+ (point-offset *editor*)))
           (decf (fill-pointer (data current-line)))))))

(defun delete-forward-char (&optional (n 1))
  "Delete the following N characters (previous if N is negative)."
  (cond ((minusp n)
         (setf n (- n))
         (dotimes (i n)
           (when (and (eql (point-line *editor*) (first-line *editor*))
                      (eql (point-offset *editor*) 0))
             (return))
           (backward-char)
           (delete-one-char-forward)))
        (t
         (dotimes (i n)
           (when (and (eql (point-line *editor*) (last-line *editor*))
                      (eql (point-offset *editor*) (line-length (last-line *editor*))))
             (return))
           (delete-one-char-forward))))
  (values))

(defun delete-backward-char (&optional (n 1))
  (delete-forward-char (- n)))

;;; Point motion.

(defun move-beginning-of-line ()
  (setf (point-offset *editor*) 0
        (point-column-hint *editor*) 0)
  (values))

(defun move-end-of-line ()
  (setf (point-offset *editor*) (line-length (point-line *editor*))
        (point-column-hint *editor*) (line-length (point-line *editor*)))
  (values))

(defun forward-char (&optional (n 1))
  "Move point forward by N characters. N may be negative."
  (cond ((minusp n)
         (setf n (- n))
         (dotimes (i n)
           (let ((current-line (point-line *editor*)))
             (cond ((eql 0 (point-offset *editor*))
                    ;; At start of line.
                    (when (prev current-line)
                      (setf (point-line *editor*) (prev current-line)
                            (point-offset *editor*) (line-length (prev current-line)))))
                   (t ;; Moving within a line.
                    (decf (point-offset *editor*)))))))
        (t
         (dotimes (i n)
           (let ((current-line (point-line *editor*)))
             (cond ((eql (line-length current-line) (point-offset *editor*))
                    ;; At end of line.
                    (when (next current-line)
                      (setf (point-offset *editor*) 0
                            (point-line *editor*) (next current-line))))
                   (t ;; Moving within a line.
                    (incf (point-offset *editor*))))))))
  (setf (point-column-hint *editor*) (point-offset *editor*))
  (values))

(defun backward-char (&optional (n 1))
  (forward-char (- n)))

(defun next-line (&optional (n 1))
  "Move point down by N lines. N may be negative.
Tries to stay as close to the current column as possible."
  (let ((accessor #'next))
    (when (minusp n)
      (setf n (- n)
            accessor #'prev))
    (dotimes (i n)
      (let* ((current-line (point-line *editor*))
             (new-line (funcall accessor current-line)))
        (cond (new-line
               (setf (point-offset *editor*) (min (point-column-hint *editor*)
                                                  (line-length new-line))
                     (point-line *editor*) new-line))
              (t (return))))))
  (values))

(defun previous-line (&optional (n 1))
  (next-line (- n)))

;;; Redisplay.

;; Lines are currently fixed-height.
(defun window-rows ()
  (multiple-value-bind (left right top bottom)
      (mezzanine.gui.widgets:frame-size (frame *editor*))
    (truncate (- (mezzanine.gui.compositor:height (window *editor*)) top bottom)
              (mezzanine.gui.font:line-height (font *editor*)))))

(defun put-string (string pen y start end)
  (multiple-value-bind (left right top bottom)
      (mezzanine.gui.widgets:frame-size (frame *editor*))
    (loop
       with font = (font *editor*)
       with y-pos = (+ (* y (mezzanine.gui.font:line-height font)) (mezzanine.gui.font:ascender font))
       with fb = (mezzanine.gui.compositor:window-buffer (window *editor*))
       with colour = (foreground-colour *editor*)
       for ch-offset from start below end
       for glyph = (mezzanine.gui.font:character-to-glyph font (aref string ch-offset))
       for mask = (mezzanine.gui.font:glyph-mask glyph)
       for advance = (mezzanine.gui.font:glyph-advance glyph)
       do
         (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1)
                                                colour
                                                mask 0 0
                                                fb
                                                (+ top (- y-pos (mezzanine.gui.font:glyph-yoff glyph)))
                                                (+ left pen (mezzanine.gui.font:glyph-xoff glyph)))
         (incf pen advance)
       finally (return pen))))

(defun put-char (character x y &key invert bold underline)
  (multiple-value-bind (left right top bottom)
      (mezzanine.gui.widgets:frame-size (frame *editor*))
    (let* ((glyph (mezzanine.gui.font:character-to-glyph (font *editor*) character))
           (mask (mezzanine.gui.font:glyph-mask glyph))
           (advance (mezzanine.gui.font:glyph-advance glyph))
           (fb (mezzanine.gui.compositor:window-buffer (window *editor*)))
           (win-width (- (mezzanine.gui.compositor:width (window *editor*)) left right))
           (line-height (mezzanine.gui.font:line-height (font *editor*)))
           (y (* y line-height)))
      (when (<= (+ x advance) win-width)
        (when invert
          (mezzanine.gui:bitset line-height advance
                                (foreground-colour *editor*)
                                fb (+ top y) (+ left x)))
        (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1)
                                               (if invert
                                                   (background-colour *editor*)
                                                   (foreground-colour *editor*))
                                               mask 0 0
                                               fb (+ top (- (+ y (mezzanine.gui.font:ascender (font *editor*))) (mezzanine.gui.font:glyph-yoff glyph))) (+ left x (mezzanine.gui.font:glyph-xoff glyph)))
        (when bold
          (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1)
                                                 (if invert
                                                     (background-colour *editor*)
                                                     (foreground-colour *editor*))
                                                 mask 0 0
                                                 fb (+ top (- (+ y (mezzanine.gui.font:ascender (font *editor*))) (mezzanine.gui.font:glyph-yoff glyph))) (+ left x (mezzanine.gui.font:glyph-xoff glyph))))
        (when underline
          (mezzanine.gui:bitset-argb-xrgb 1 advance
                                          (if invert
                                              (background-colour *editor*)
                                              (foreground-colour *editor*))
                                          fb (+ top y (mezzanine.gui.font:ascender (font *editor*))) (+ left x))))
      (+ x advance))))

(defun build-screen-line-list (top-line win-width)
  (let ((screen (make-array (window-rows) :initial-element nil :adjustable t :fill-pointer 0))
        (point-screen-line nil)
        (font (font *editor*)))
    (block done-line-counting
      (do ((line top-line (next line))
           (seen-point nil)
           (lines-since-point 0))
          ((null line))
        (when (input-waiting)
          (throw 'input-waiting nil))
        (let ((pen-offset 0)
              (current-screen-line (list line 0 nil)))
          (flet ((finish-line (end)
                   (setf (third current-screen-line) end)
                   (vector-push-extend current-screen-line screen)
                   (when seen-point
                     (incf lines-since-point)
                     (when (> lines-since-point (window-rows))
                       (return-from done-line-counting)))
                   (format t "Line ~S ~S  ~S ~S ~S~%"
                           (point-line *editor*) line
                           (second current-screen-line) (point-offset *editor*) (third current-screen-line))
                   (when (and (eql (point-line *editor*) line)
                              (<= (second current-screen-line) (point-offset *editor*) (third current-screen-line)))
                     ;; This screen line contains the point.
                     (setf point-screen-line (1- (length screen)))
                     (setf seen-point t))
                   (setf pen-offset 0
                         current-screen-line (list line end nil))))
            (dotimes (i (line-length line))
              (let* ((glyph (mezzanine.gui.font:character-to-glyph (font *editor*) (aref (data line) i)))
                     (advance (mezzanine.gui.font:glyph-advance glyph)))
                (format t "Pen-offset: ~S  Advance ~S  WWidth ~S~%" pen-offset advance win-width)
                (incf pen-offset advance)
                (when (>= pen-offset win-width)
                  (format t "Wrap line. ~S ~S~%" pen-offset win-width)
                  (finish-line i))))
            (finish-line (line-length line))))))
    (values screen point-screen-line)))

(defun redisplay ()
  (catch 'input-waiting
    (multiple-value-bind (left right top bottom)
        (mezzanine.gui.widgets:frame-size (frame *editor*))
      (let ((top-line (point-line *editor*)))
        ;; Starting from point, count back a bunch of physical lines.
        (dotimes (i (window-rows))
          (when (not (prev top-line))
            (return))
          (setf top-line (prev top-line)))
        ;; Figure out how much screen each line covers.
        (multiple-value-bind (screen point-screen-line)
            (build-screen-line-list top-line (- (mezzanine.gui.compositor:width (window *editor*)) left right))
          (format t "Screen layout: ~S~%" screen)
          (format t "Point is ~S ~S~%" (point-line *editor*) (point-offset *editor*))
          (format t "Point screen line is ~S ~S~%" point-screen-line (aref screen point-screen-line))
          (format t "Previous top of window is ~S~%" (window-top *editor*))
          (let ((window-top (position-if (lambda (x) (and (eql (first x) (first (window-top *editor*)))
                                                          (eql (second x) (second (window-top *editor*)))
                                                          (eql (third x) (third (window-top *editor*)))))
                                         screen)))
            ;; If the point is above (lt) the old window top, or below (gte) the window bottom, then recenter.
            (when (or (not window-top)
                      (< point-screen-line window-top)
                      (>= point-screen-line (+ window-top (window-rows))))
              (setf window-top (max 0 (- point-screen-line (truncate (window-rows) 2)))
                    (window-top *editor*) (aref screen window-top)))
            (format t "Window top is ~S~%" window-top)
            ;; Clear screen.
            (mezzanine.gui:bitset (- (mezzanine.gui.compositor:height (window *editor*)) top bottom)
                                  (- (mezzanine.gui.compositor:width (window *editor*)) left right)
                                  (background-colour *editor*)
                                  (mezzanine.gui.compositor:window-buffer (window *editor*))
                                  top left)
            ;; Now draw!
            (dotimes (y (window-rows))
              (when (input-waiting)
                (return-from redisplay nil))
              (when (>= (+ window-top y) (length screen))
                (return))
              (let* ((line (aref screen (+ window-top y)))
                     (data (data (first line)))
                     (point-at-endp (eql (point-offset *editor*) (line-length (point-line *editor*)))))
                (cond
                  ((not (eql (+ window-top y) point-screen-line))
                   (put-string (data (first line)) 0 y (second line) (third line)))
                  ;; Point is on this line. Highlight it.
                  ;; This does't work too well if the point is at the end of both a physical & a virtual line.
                  ;; Adding a right margin with wrap indicator would provide a place for the point to sit.
                  (point-at-endp
                   (let ((pen 0))
                     (setf pen (put-string (data (first line)) pen y (second line) (third line)))
                     (setf pen (put-char #\Space pen y :invert t :underline t :bold t))))
                  (t
                   (let ((pen 0))
                     (setf pen (put-string (data (first line)) pen y (second line) (point-offset *editor*)))
                     (setf pen (put-char (aref (data (first line)) (point-offset *editor*)) pen y :invert t :underline t :bold t))
                     (setf pen (put-string (data (first line)) pen y (1+ (point-offset *editor*)) (third line))))))))
            (mezzanine.gui.compositor:damage-window (window *editor*)
                                                    left top
                                                    (- (mezzanine.gui.compositor:width (window *editor*)) left right)
                                                    (- (mezzanine.gui.compositor:height (window *editor*)) top bottom))))))
    t))

(defun editor-loop ()
  (loop
     (handler-case
         (let ((ch (editor-read-char)))
           (case ch
             ((nil)) ; Happens when there's a redisplay pending.
             (#\Newline
              (insert-line))
             (#\C-F (forward-char))
             (#\C-B (backward-char))
             (#\C-N (next-line))
             (#\C-P (previous-line))
             (#\C-A (move-beginning-of-line))
             (#\C-E (move-end-of-line))
             (#\C-K (kill-line))
             (#\C-Q (insert-char (editor-read-char)))
             (#\Backspace (delete-backward-char))
             (#\Delete (delete-forward-char))
             (t (cond ((find ch "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789!\"£$%^&*()_+-=[]{};'#:@~,./<>?`~|\\"
                             :test #'char=)
                       (insert-char ch)))))
           (setf (pending-redisplay *editor*) (not (redisplay))))
       (error (c)
         (ignore-errors
           (format t "Editor error: ~A~%" c))))))

(defun editor-main (width height)
  (mezzanine.gui.font:with-font (font mezzanine.gui.font:*default-monospace-font* mezzanine.gui.font:*default-monospace-font-size*)
    (let ((fifo (mezzanine.supervisor:make-fifo 50)))
      (mezzanine.gui.compositor:with-window (window fifo (or width 640) (or height 700))
        (let* ((framebuffer (mezzanine.gui.compositor:window-buffer window))
               (frame (make-instance 'mezzanine.gui.widgets:frame
                                     :framebuffer framebuffer
                                     :title "Editor"
                                     :close-button-p t
                                     :damage-function (mezzanine.gui.widgets:default-damage-function window)))
               (line (make-instance 'line))
               (*editor* (make-instance 'editor
                                        :fifo fifo
                                        :pending-redisplay t
                                        :font font
                                        :window window
                                        :frame frame
                                        :first-line line
                                        :last-line line
                                        :point-line line
                                        :point-offset 0
                                        :point-column-hint 0
                                        :window-top (list line 0 0))))
          (mezzanine.gui.widgets:draw-frame frame)
          (catch 'quit
            (editor-loop)))))))

(defun spawn (&key width height)
  (mezzanine.supervisor:make-thread (lambda () (editor-main width height))
                                    :name "Editor"))
