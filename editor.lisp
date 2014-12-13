(defpackage :mezzanine.editor
  (:use :cl)
  (:export #:spawn))

(in-package :mezzanine.editor)

(defclass line ()
  ((%next :initarg :next :accessor next)
   (%prev :initarg :prev :accessor prev)
   (%data :initarg :data :accessor data)
   (%version :initarg :version :accessor version))
  (:default-initargs :next nil
                     :prev nil
                     :data (make-array 50 :element-type 'character :adjustable t :fill-pointer 0)
                     :version 0))

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
   ;; Redisplay state.
   (%window-top-line :initarg :window-top-line :accessor window-top-line)
   (%window-top :initarg :window-top :accessor window-top)
   (%current-screen :initarg :current-screen :accessor current-screen)
   (%current-point-logical-line :initarg :current-point-screen-line :accessor current-point-logical-line))
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

(define-condition pending-input () ())

(defun check-pending-input ()
  (cond ((pending-event *editor*)
         (signal 'pending-input))
        (t (let ((event (mezzanine.supervisor:fifo-pop (fifo *editor*) nil)))
             (when event
               (setf (pending-event *editor*) event)
               (signal 'pending-input))))))

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
    (incf (version current-line))
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
           (setf (aref (data current-line) (point-offset *editor*)) ch)))
    (incf (version current-line)))
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
           (setf (fill-pointer (data current-line)) (point-offset *editor*))))
    (incf (version current-line)))
  (values))

(defun delete-one-char-forward ()
  (let ((current-line (point-line *editor*)))
    (cond ((eql (point-offset *editor*) (line-length current-line))
           (kill-line))
          (t
           (replace (data current-line) (data current-line)
                    :start1 (point-offset *editor*)
                    :start2 (1+ (point-offset *editor*)))
           (decf (fill-pointer (data current-line)))
           (incf (version current-line))))))

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

(defun put-string (string pen y start end &key bold invert underline)
  (multiple-value-bind (left right top bottom)
      (mezzanine.gui.widgets:frame-size (frame *editor*))
    (loop
       with pen-start = pen
       with font = (font *editor*)
       with y-pos = (* y (mezzanine.gui.font:line-height font))
       with baseline = (+ y-pos (mezzanine.gui.font:ascender font))
       with fb = (mezzanine.gui.compositor:window-buffer (window *editor*))
       with colour = (foreground-colour *editor*)
       with line-height = (mezzanine.gui.font:line-height font)
       with foreground = (if invert
                             (background-colour *editor*)
                             (foreground-colour *editor*))
       with background = (if invert
                             (foreground-colour *editor*)
                             (background-colour *editor*))
       for ch-offset from start below end
       for glyph = (mezzanine.gui.font:character-to-glyph font (aref string ch-offset))
       for mask = (mezzanine.gui.font:glyph-mask glyph)
       for advance = (mezzanine.gui.font:glyph-advance glyph)
       do
         (when invert
           (mezzanine.gui:bitset line-height advance
                                 background
                                 fb (+ top y-pos) (+ left pen)))
         (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1)
                                                foreground
                                                mask 0 0
                                                fb
                                                (+ top (- baseline (mezzanine.gui.font:glyph-yoff glyph)))
                                                (+ left pen (mezzanine.gui.font:glyph-xoff glyph)))
         (when bold
           (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1)
                                                  foreground
                                                  mask 0 0
                                                  fb
                                                  (+ top (- baseline (mezzanine.gui.font:glyph-yoff glyph)))
                                                  (+ left pen (mezzanine.gui.font:glyph-xoff glyph))))
         (when underline
           (mezzanine.gui:bitset-argb-xrgb 1 advance
                                           foreground
                                           fb (+ top baseline) (+ left pen)))
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
                                               fb
                                               (+ top (- (+ y (mezzanine.gui.font:ascender (font *editor*))) (mezzanine.gui.font:glyph-yoff glyph)))
                                               (+ left x (mezzanine.gui.font:glyph-xoff glyph)))
        (when bold
          (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1)
                                                 (if invert
                                                     (background-colour *editor*)
                                                     (foreground-colour *editor*))
                                                 mask 0 0
                                                 fb
                                                 (+ top (- (+ y (mezzanine.gui.font:ascender (font *editor*))) (mezzanine.gui.font:glyph-yoff glyph)))
                                                 (+ left x (mezzanine.gui.font:glyph-xoff glyph))))
        (when underline
          (mezzanine.gui:bitset-argb-xrgb 1 advance
                                          (if invert
                                              (background-colour *editor*)
                                              (foreground-colour *editor*))
                                          fb
                                          (+ top y (mezzanine.gui.font:ascender (font *editor*)))
                                          (+ left x))))
      (+ x advance))))

(defclass logical-line-segment ()
  ((%logical-line :initarg :logical-line :accessor logical-line)
   (%start :initarg :start :accessor line-start)
   (%end :initarg :start :accessor line-end)))

(defclass logical-line ()
  ((%physical-line :initarg :physical-line :accessor physical-line)
   (%version :initarg :version :accessor version)
   (%segments :initarg :segments :accessor segments))
  (:default-initargs :segments (make-array 1 :adjustable t :fill-pointer 0)))

(defmethod physical-line ((line logical-line-segment))
  (physical-line (logical-line line)))

(defun physical-line-to-logical-line-1 (line win-width)
  (let* ((font (font *editor*))
         (pen-offset 0)
         (logical-line (make-instance 'logical-line
                                      :physical-line line
                                      :version (version line)))
         (segs (segments logical-line))
         (current-segment (make-instance 'logical-line-segment
                                         :logical-line logical-line
                                         :start 0)))
    (flet ((finish-segment (end)
             (check-pending-input)
             (setf (line-end current-segment) end)
             (vector-push-extend current-segment segs)
             (setf pen-offset 0)
                   current-segment (make-instance 'logical-line-segment
                                                  :logical-line logical-line
                                                  :start end)))
      (dotimes (i (line-length line))
        (let* ((glyph (mezzanine.gui.font:character-to-glyph (font *editor*) (aref (data line) i)))
               (advance (mezzanine.gui.font:glyph-advance glyph)))
          ;;(format t "Pen-offset: ~S  Advance ~S  Width ~S~%" pen-offset advance win-width)
          (incf pen-offset advance)
          (when (>= pen-offset win-width)
            ;;(format t "Wrap line. ~S ~S~%" pen-offset win-width)
            (finish-segment i))))
      (finish-segment (line-length line)))
    logical-line))

(defun physical-line-to-logical-line (current-screen line win-width)
  ;; Convert a physical line to a logical line, possibly reusing previously displayed logical lines.
  (cond
    (current-screen
     (loop for seg across current-screen do
          (when (and (zerop (line-start seg))
                     (eql (physical-line seg) line)
                     (eql (version (logical-line seg)) (version (physical-line seg))))
            #+(or)(format t "Reused logical line ~S for physical line ~S.~%" (logical-line seg) line)
            (return (logical-line seg)))
        finally (return (physical-line-to-logical-line-1 line win-width))))
    (t (physical-line-to-logical-line-1 line win-width))))

(defun build-screen-line-list (current-screen top-line win-width)
  (do ((screen (make-array (length current-screen) :adjustable t :fill-pointer 0))
       (point-screen-line nil)
       (lines-since-point 0)
       (line top-line (next line)))
      ((or (null line)
           (> lines-since-point (window-rows)))
       (values screen point-screen-line))
    (let ((logical-line (physical-line-to-logical-line current-screen line win-width)))
      (loop for seg across (segments logical-line) do
           (when point-screen-line
             (incf lines-since-point))
           #+(or)(format t "Line ~S ~S  ~S ~S ~S~%"
                   (point-line *editor*) line
                   (line-start seg) (point-offset *editor*) (line-end seg))
           (when (and (eql (point-line *editor*) line)
                      (<= (line-start seg) (point-offset *editor*) (line-end seg)))
             ;; This screen line contains the point.
             (setf point-screen-line (length screen)))
           (vector-push-extend seg screen)))))

(defun put-line (seg y window-top point-screen-line)
  (let* ((data (data (physical-line seg)))
         (point-at-endp (eql (point-offset *editor*) (line-length (point-line *editor*)))))
    (cond
      ((not (eql (+ window-top y) point-screen-line))
       (put-string data 0 y (line-start seg) (line-end seg)))
      ;; Point is on this line. Highlight it.
      ;; This does't work too well if the point is at the end of both a physical & a virtual line.
      ;; Adding a right margin with wrap indicator would provide a place for the point to sit.
      (point-at-endp
       (let ((pen 0))
         (setf pen (put-string data pen y (line-start seg) (line-end seg)))
         (setf pen (put-char #\Space pen y :invert t))))
      (t
       (let ((pen 0))
         (setf pen (put-string data pen y (line-start seg) (point-offset *editor*)))
         (setf pen (put-char (aref data (point-offset *editor*)) pen y :invert t))
         (setf pen (put-string data pen y (1+ (point-offset *editor*)) (line-end seg))))))))

(defun incremental-redisplay ()
  (multiple-value-bind (left right top bottom)
      (mezzanine.gui.widgets:frame-size (frame *editor*))
    (let* ((top-line (point-line *editor*))
           (current-screen (current-screen *editor*))
           (full-redisplayp (not current-screen))
           (win-width (- (mezzanine.gui.compositor:width (window *editor*)) left right))
           (line-height (mezzanine.gui.font:line-height (font *editor*))))
      ;; Starting from point, count back a bunch of physical lines.
      (dotimes (i (window-rows))
        (when (not (prev top-line))
          (return))
        (setf top-line (prev top-line)))
      ;; Rebuild the screen array.
      (multiple-value-bind (screen point-screen-line)
          (build-screen-line-list current-screen top-line win-width)
        #+(or)(format t "Incr Screen layout: ~S~%" screen)
        #+(or)(format t "Incr Point is ~S ~S~%" (point-line *editor*) (point-offset *editor*))
        #+(or)(format t "Incr Point screen line is ~S ~S~%" point-screen-line (aref screen point-screen-line))
        #+(or)(format t "Incr Previous top of window is ~S~%" (window-top-line *editor*))
        ;; Find the new top screen line, based on the current top screen line.
        (let ((window-top (when (window-top-line *editor*)
                            (position-if (lambda (x) (or (eql x (window-top-line *editor*))
                                                         (and (eql (physical-line x) (physical-line (window-top-line *editor*)))
                                                              (eql (line-start x) (line-start (window-top-line *editor*)))
                                                              (eql (line-end x) (line-end (window-top-line *editor*))))))
                                         screen))))
          ;; If the point is above (lt) the old window top, or below (gte) the window bottom, then recenter.
          (when (or (not window-top)
                    (< point-screen-line window-top)
                    (>= point-screen-line (+ window-top (window-rows))))
            (setf window-top (max 0 (- point-screen-line (truncate (window-rows) 2)))))
          #+(or)(format t "Incr Window top is ~S~%" window-top)
          (when full-redisplayp
            ;; Actually a full redisplay, clear the whole screen.
            (mezzanine.gui:bitset (- (mezzanine.gui.compositor:height (window *editor*)) top bottom)
                                  win-width
                                  (background-colour *editor*)
                                  (mezzanine.gui.compositor:window-buffer (window *editor*))
                                  top left))
          ;; Walk the screen line array, copying lines when possible or redrawing them when not.
          (dotimes (y (window-rows))
            (check-pending-input)
            (when (>= (+ window-top y) (length screen))
              (return))
            (let* ((seg (aref screen (+ window-top y)))
                   (position (when (not full-redisplayp)
                               (position seg
                                         current-screen
                                         :start (window-top *editor*)
                                         :end (min (+ (window-top *editor*) (window-rows))
                                                   (length current-screen))))))
              (cond ((or full-redisplayp
                         (eql y (- point-screen-line window-top))
                         (eql y (current-point-logical-line *editor*))
                         (not position))
                     (when (not full-redisplayp)
                       (mezzanine.gui:bitset line-height win-width
                                             (background-colour *editor*)
                                             (mezzanine.gui.compositor:window-buffer (window *editor*))
                                             (+ top (* y line-height)) left))
                     (put-line seg y window-top point-screen-line)
                     (when (not full-redisplayp)
                       (mezzanine.gui.compositor:damage-window (window *editor*)
                                                               left (+ top (* y line-height))
                                                               win-width line-height)
                       (when (< (+ (window-top *editor*) y) (length current-screen))
                         (setf (aref current-screen (+ (window-top *editor*) y)) seg))))
                    ((not (eql y (- position (window-top *editor*))))
                     #+(or)(format t "Copy old line ~S ~S to new line ~S ~S.~%" position (* (- position (window-top *editor*)) line-height) y (* y line-height))
                     (mezzanine.gui:bitblt line-height win-width
                                           (mezzanine.gui.compositor:window-buffer (window *editor*))
                                           (+ top (* (- position (window-top *editor*)) line-height)) left
                                           (mezzanine.gui.compositor:window-buffer (window *editor*))
                                           (+ top (* y line-height)) left)
                     (mezzanine.gui.compositor:damage-window (window *editor*)
                                                             left (+ top (* y line-height))
                                                             win-width line-height)
                     (when (< (+ (window-top *editor*) y) (length current-screen))
                       (setf (aref current-screen (+ (window-top *editor*) y)) seg)))
                    (t #+(or)(format t "Keep old line ~S ~S~%" position y)))))
          ;; If the new screen is shorter than the old screen, then fill
          ;; any newly nonexistent screen lines.
          (when (and (not full-redisplayp)
                     (> (- (length current-screen) (window-top *editor*))
                        (- (length screen) window-top)))
            (mezzanine.gui:bitset (* (- (- (length current-screen) (window-top *editor*))
                                        (- (length screen) window-top))
                                     line-height)
                                  win-width
                                  (background-colour *editor*)
                                  (mezzanine.gui.compositor:window-buffer (window *editor*))
                                  (+ top (* (- (length screen) window-top) line-height)) left)
            (mezzanine.gui.compositor:damage-window (window *editor*)
                                                    left
                                                    (+ top (* (- (length screen) window-top) line-height))
                                                    win-width
                                                    (* (- (- (length current-screen) (window-top *editor*))
                                                          (- (length screen) window-top))
                                                       line-height)))
          (when full-redisplayp
            (mezzanine.gui.compositor:damage-window (window *editor*)
                                                    left top
                                                    win-width
                                                    (- (mezzanine.gui.compositor:height (window *editor*)) top bottom)))
          (setf (current-screen *editor*) screen
                (window-top-line *editor*) (aref screen window-top)
                (window-top *editor*) window-top
                (current-point-logical-line *editor*) (- point-screen-line window-top)))))))

(defun redraw-screen ()
  (setf (current-screen *editor*) nil
        (window-top-line *editor*) nil))

(defun redisplay ()
  (handler-case
      (progn
        (check-pending-input)
        (incremental-redisplay)
        t)
    (pending-input ()
      nil)))

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
             (#\C-L (redraw-screen))
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
                                        :window-top-line nil
                                        :window-top 0
                                        :current-screen nil
                                        :current-point-logical-line nil)))
          (mezzanine.gui.widgets:draw-frame frame)
          (catch 'quit
            (editor-loop)))))))

(defun spawn (&key width height)
  (mezzanine.supervisor:make-thread (lambda () (editor-main width height))
                                    :name "Editor"))
