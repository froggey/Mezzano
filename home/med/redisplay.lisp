(in-package :med)

(defun redraw-screen ()
  "Redraw the whole screen. For use when the display is corrupted."
  ;; Flush the current screen and line cache.
  (setf (editor-current-screen *editor*) nil
        (display-line-cache *editor*) '()))

(defun pane-top-line (buffer)
  (let ((top-line (buffer-property buffer 'pane-top-line)))
    (when (not top-line)
      (setf top-line (make-mark (first-line buffer) 0 :left)
            (buffer-property buffer 'pane-top-line) top-line))
    top-line))

(defclass display-line ()
  ((%line :initarg :line :reader display-line-line)
   (%version :initarg :version :reader display-line-version)
   (%start :initarg :start :reader display-line-start)
   (%end :initarg :end :reader display-line-end)
   (%representation :initarg :representation :accessor display-line-representation)))

;; Lines are currently fixed-height.
(defun window-rows ()
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (frame *editor*))
    (- (truncate (- (mezzano.gui.compositor:height (window *editor*)) top bottom)
              (mezzano.gui.font:line-height (font *editor*))) 2)))

(defun flush-display-line (mark)
  "Flush the display line containing MARK."
  (setf (display-line-cache *editor*)
        (remove-if (lambda (line)
                     ;; Munch the entire line.
                     (eql (display-line-line line) (mark-line mark)))
                   (display-line-cache *editor*))))

(defun flush-display-lines-in-region (mark-1 mark-2)
  "Flush display lines containing the region specified by MARK-1 and MARK-2."
  (let ((first (min (line-number (mark-line mark-1))
                    (line-number (mark-line mark-2))))
        (last (max (line-number (mark-line mark-1))
                   (line-number (mark-line mark-2)))))
    (setf (display-line-cache *editor*)
          (remove-if (lambda (line)
                       (<= first (line-number (display-line-line line)) last))
                     (display-line-cache *editor*)))))

(defun flush-stale-lines ()
  "Flush any display lines with the wrong version."
  (setf (display-line-cache *editor*)
        (remove-if (lambda (line)
                     (not (eql (display-line-version line)
                               (line-version (display-line-line line)))))
                   (display-line-cache *editor*))))

(defun editor-width ()
  "Return the width of the display area in pixels."
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (frame *editor*))
    (- (mezzano.gui.compositor:width (window *editor*)) left right)))

(defun region-bounds (mark-1 mark-2)
  "Return a bunch of boundary information for the region."
  (cond ((eql (mark-line mark-1) (mark-line mark-2))
         ;; Same line.
         (when (> (mark-charpos mark-1) (mark-charpos mark-2))
           (rotatef mark-1 mark-2))
         (values (mark-line mark-1) (mark-charpos mark-1) nil
                 (mark-line mark-2) (mark-charpos mark-2) nil))
        (t ;; 2 or more lines.
         (when (> (line-number (mark-line mark-1)) (line-number (mark-line mark-2)))
           (rotatef mark-1 mark-2))
         (values (mark-line mark-1) (mark-charpos mark-1) (line-number (mark-line mark-1))
                 (mark-line mark-2) (mark-charpos mark-2) (line-number (mark-line mark-2))))))

(defun render-display-line-2 (line start &optional invert)
  (multiple-value-bind (line-1 line-1-charpos line-1-number line-2 line-2-charpos line-2-number)
      (region-bounds (buffer-point (current-buffer *editor*)) (buffer-mark (current-buffer *editor*)))
    (loop
       with pen = 0
       with font = (font *editor*)
       with font-bold = (font-bold *editor*)
       with baseline = (mezzano.gui.font:ascender font)
       with foreground = (if invert (background-colour *editor*) (foreground-colour *editor*))
       with background = (if invert (foreground-colour *editor*) (background-colour *editor*))
       with line-height = (mezzano.gui.font:line-height font)
       with win-width = (editor-width)
       with point = (buffer-point (current-buffer *editor*))
       with mark-active = (buffer-mark-active (current-buffer *editor*))
       with buffer = (mezzano.gui:make-surface
                      win-width line-height
                      :initial-colour background)
       for ch-position from start below (line-length line)
       for glyph = (mezzano.gui.font:character-to-glyph font (line-character line ch-position))
       for mask = (mezzano.gui.font:glyph-mask glyph)
       for advance = (mezzano.gui.font:glyph-advance glyph)
       do
         (when (> (+ pen advance) win-width)
           (return (values buffer ch-position)))
         (let ((at-point (and (eql line (mark-line point))
                              (eql ch-position (mark-charpos point))))
               (in-region (and mark-active
                               (or (if line-1-number
                                       (or (< line-1-number (line-number line) line-2-number)
                                           (and (eql line line-1)
                                                (<= line-1-charpos ch-position))
                                           (and (eql line line-2)
                                                (< ch-position line-2-charpos)))
                                       (and (eql line line-1)
                                            (<= line-1-charpos ch-position)
                                            (< ch-position line-2-charpos)))))))
           ;; Invert the point.
           (when at-point
             (mezzano.gui:bitset :set
                                 advance line-height
                                 foreground
                                 buffer
                                 pen 0))
           (mezzano.gui:bitset :blend
                               (mezzano.gui:surface-width mask)
                               (mezzano.gui:surface-height mask)
                               (if at-point
                                   background
                                   foreground)
                               buffer
                               (+ pen (mezzano.gui.font:glyph-xoff glyph))
                               (- baseline (mezzano.gui.font:glyph-yoff glyph))
                               mask 0 0)
           ;; Underline the region.
           (when in-region
             (mezzano.gui:bitset :blend
                                 advance 1
                                 (if at-point
                                     background
                                     foreground)
                                 buffer
                                 pen baseline))
           (incf pen advance))
       finally
       ;; Reached end of line, check for the point.
         (when (and (eql line (mark-line point))
                    (eql ch-position (mark-charpos point)))
           ;; Point is here, render it past the last character.
           (let* ((glyph (mezzano.gui.font:character-to-glyph font #\Space))
                  (advance (mezzano.gui.font:glyph-advance glyph)))
             (when (<= (+ pen advance) win-width) ; FIXME, how to display point at end of line & display line properly. also fix blit crash bug.
               (mezzano.gui:bitset :set
                                   advance line-height
                                   foreground
                                   buffer
                                   pen 0))))
       ;; TODO: Render underline to end of line region spans whole line.
         (return (values buffer ch-position)))))

(defun render-display-line-1 (line start &optional invert)
  (multiple-value-bind (buffer end)
      (render-display-line-2 line start invert)
    (let ((display-line (make-instance 'display-line
                                       :line line
                                       :version (line-version line)
                                       :start start
                                       :end end
                                       :representation buffer)))
      (push display-line (display-line-cache *editor*))
      display-line)))

(defun render-display-line (line fn &optional invert)
  "Render display lines for real line LINE, calling FN with each display line."
  (cond ((zerop (line-length line))
         (funcall fn (or (get-display-line-from-cache line 0)
                         (render-display-line-1 line 0 invert))))
        (t (do ((start 0))
               ((>= start (line-length line)))
             (let ((display-line (or (get-display-line-from-cache line start)
                                     (render-display-line-1 line start invert))))
               (funcall fn display-line)
               (setf start (display-line-end display-line)))))))

(defun get-display-line-from-cache (line start)
  (dolist (display-line (display-line-cache *editor*))
    (when (and (eql (display-line-line display-line) line)
               (eql (display-line-start display-line) start))
      ;; MRU cache.
      (setf (display-line-cache *editor*) (remove display-line (display-line-cache *editor*)))
      (push display-line (display-line-cache *editor*))
      (return display-line))))

(defun blit-display-line (line y)
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (frame *editor*))
    (let* ((fb (mezzano.gui.compositor:window-buffer (window *editor*)))
           (line-height (mezzano.gui.font:line-height (font *editor*)))
           (real-y (+ top (* y line-height)))
           (win-width (editor-width)))
      (if line
          ;; Blitting line.
          (mezzano.gui:bitblt :set
                              win-width line-height
                              (display-line-representation line)
                              0 0
                              fb
                              left real-y)
          ;; Line is empty.
          (mezzano.gui:bitset :set
                              win-width line-height
                              (background-colour *editor*)
                              fb
                              left real-y))
      (mezzano.gui.compositor:damage-window (window *editor*)
                                            left real-y
                                            win-width line-height))))

(defun recenter (buffer)
  "Move BUFFER's top line so that the point is displayed."
  (let* ((point (buffer-point buffer))
         (top-line (mark-line point))
         (rendered-lines (make-array (ceiling (window-rows) 2) :fill-pointer 0 :adjustable t))
         (point-display-line nil))
    ;; Move (window-rows)/2 lines up from point.
    (dotimes (i (ceiling (window-rows) 2))
      (when (not (previous-line top-line))
        (return))
      (setf top-line (previous-line top-line)))
    ;; Render display lines until point is reached.
    (do ((line top-line (next-line line)))
        ;; Should always top when the point's line has been reached.
        ()
      (render-display-line line
                           (lambda (display-line)
                             (vector-push-extend display-line rendered-lines)
                             (when (and (eql (mark-line point) (display-line-line display-line))
                                        (<= (display-line-start display-line) (mark-charpos point))
                                        (or (and (eql (display-line-end display-line) (line-length (display-line-line display-line)))
                                                 (eql (display-line-end display-line) (mark-charpos point)))
                                            (< (mark-charpos point) (display-line-end display-line))))
                               ;; This is point line, stop here.
                               (setf point-display-line (1- (length rendered-lines)))
                               (return)))))
    ;; Walk (window-rows)/2 display lines backwards from point. This is the new top-line.
    (let ((new-top-line (aref rendered-lines (max 0 (- point-display-line (truncate (window-rows) 2)))))
          (top-line-mark (buffer-property buffer 'pane-top-line)))
      (setf (mark-line top-line-mark) (display-line-line new-top-line))
            (mark-charpos top-line-mark) (display-line-start new-top-line))))

(defun minibuffer-rows ()
  (if (eql (current-buffer *editor*) *minibuffer*)
    (1+ (truncate (line-number (last-line *minibuffer*)) 10000))
    1))

(defvar *mode-line-buffer* (make-instance 'buffer))
(defun render-mode-line ()
  (let* ((buffer (current-buffer *editor*)))
    (unless (eql buffer *minibuffer*)
      (insert *mode-line-buffer*
        (format nil " [~A] ~A L~S C~S    (~A)"
           (if (buffer-modified buffer) "*" " ")
           (buffer-property buffer 'name)
           (1+ (truncate (line-number (mark-line (buffer-point buffer))) 10000))
           (1+ (mark-charpos (buffer-point buffer)))
           ;;(buffer-current-package buffer)
           *package* ; TODO: uncomment above when buffer-current-package is faster
           ))
      (render-display-line (first-line *mode-line-buffer*)
         (lambda (l) (blit-display-line l (- (window-rows) (1- (minibuffer-rows))))) t)
      (with-mark (point (buffer-point *mode-line-buffer*))
        (move-beginning-of-buffer *mode-line-buffer*)
        (delete-region *mode-line-buffer* point (buffer-point *mode-line-buffer*))))))

(defun redisplay ()
  "Perform an incremental redisplay cycle.
Returns true when the screen is up-to-date, false if the screen is dirty and there is pending input."
  (handler-case
      (progn
        (when (not (eql (length (editor-current-screen *editor*)) (window-rows)))
          (setf (editor-current-screen *editor*) (make-array (window-rows) :initial-element t)))
        (check-pending-input)
        (let* ((buffer (current-buffer *editor*))
               (current-screen (editor-current-screen *editor*))
               (new-screen (make-array (window-rows) :fill-pointer 0 :initial-element nil))
               (point-line nil)
               (top-line (pane-top-line buffer))
               (point (buffer-point buffer))
               (previous-point-position (buffer-property buffer 'pane-previous-point-position))
               (mark (buffer-mark buffer))
               (previous-mark-position (buffer-property buffer 'pane-previous-mark-position)))
      (mezzano.supervisor::with-mutex ((buffer-lock buffer))
      (when (not previous-point-position)
        (setf previous-point-position (copy-mark point :right)
        (buffer-property buffer 'pane-previous-point-position) previous-point-position))
      (when (not previous-mark-position)
        (setf previous-mark-position (copy-mark mark :left)
        (buffer-property buffer 'pane-previous-mark-position) previous-mark-position))
      ;; If the point has moved, then invalidate the line that contained the point and the line that
      ;; now holds the point.
      (when (not (mark= point previous-point-position))
        (flush-display-line previous-point-position)
        (flush-display-line point))
      ;; If the mark changes state, flush lines within the region.
      (when (or (and (not (buffer-mark-active buffer))
                     (buffer-property buffer 'pane-mark-was-active))
                (and (buffer-mark-active buffer)
                     (not (buffer-property buffer 'pane-mark-was-active))))
         (flush-display-lines-in-region point mark))
      ;; If the mark is active and the point moves, flush lines between the old point position
      ;; and the new position.
      ;; FIXME: This will cause a bunch of lines to be redrawn when the point & mark are exchanged.
      (when (and (buffer-mark-active buffer)
                 (not (mark= point previous-point-position)))
        (flush-display-lines-in-region point previous-point-position))
        ;; If the mark is or was active and moves, flush lines between the old mark position
        ;; and the new position.
        ;; FIXME: This will cause a bunch of lines to be redrawn when the point & mark are exchanged.
      (when (and (or (buffer-mark-active buffer)
                     (buffer-property buffer 'pane-mark-was-active))
                 (not (mark= mark previous-mark-position)))
        (flush-display-lines-in-region mark previous-mark-position))
      ;; Finally, flush any stale lines.
      (flush-stale-lines)
      ;; Update tracking properties.
      (setf (buffer-property buffer 'pane-mark-was-active) (buffer-mark-active buffer))
      (move-mark-to-mark previous-point-position point)
      (move-mark-to-mark previous-mark-position mark)
      ;; Generate WINDOW-ROWS display lines, starting at TOP-LINE.
      ;; TODO: Don't start from the beginning of the top-line, use the charpos instead.
      (setf (mark-charpos top-line) 0)
      (do ((line (mark-line top-line) (next-line line)))
           ;; Stop when there are no more lines or the screen has been filled up.
           ((null line))
        (render-display-line line
                             (lambda (display-line)
                             (check-pending-input)
                             (vector-push display-line new-screen)
        (when (and (eql (mark-line point) (display-line-line display-line))
                   (<= (display-line-start display-line) (mark-charpos point))
                   (or (and (eql (display-line-end display-line) (line-length (display-line-line display-line)))
                   (eql (display-line-end display-line) (mark-charpos point)))
                   (< (mark-charpos point) (display-line-end display-line))))
          (setf point-line display-line))
          (when (eql (fill-pointer new-screen) (window-rows))
            (return)))))
      (setf (fill-pointer new-screen) (window-rows))
      ;; If the point is not within the screen bounds, then recenter and retry.
      (when (and (eql *current-editor* *editor*)
                 (not point-line))
        (recenter buffer)
        (return-from redisplay nil))
      ;; Compare against the current screen, blitting when needed.
      (if (eql buffer *minibuffer*)
     (let ((minibuffer-rows (minibuffer-rows)))
       (do ((y 0 (incf y)))
            ((= y minibuffer-rows))
         (let ((line (aref new-screen y)))
           (unless (eql (aref current-screen y) line)
         (blit-display-line line (+ y (- (window-rows) minibuffer-rows) 2))
         (setf (aref current-screen y) line)
         (check-pending-input)))))
    (progn
      (dotimes (y (window-rows))
        (let ((line (aref new-screen y)))
          (unless (eql (aref current-screen y) line)
        (blit-display-line line y)
        (setf (aref current-screen y) line)
        (check-pending-input))))
        ;; render the messages line TODO: long message line output
        (let ((line (let ((line (last-line *messages*)))
                       (if (zerop (line-length line))
                          (previous-line (last-line *messages*))
                          line))))
          (when line
            (render-display-line line
                                 (lambda (l) (blit-display-line l (1+ (window-rows)))))))))
        (render-mode-line)
        ;; Prune the cache.
        (setf (display-line-cache *editor*) (subseq (display-line-cache *editor*) 0 (* (window-rows) 4))))
      t))
    (pending-input ()
      nil)))

(defclass force-redisplay () ())

(defmethod dispatch-event (editor (event force-redisplay))
  (setf (pending-redisplay editor) t))

(defparameter *force-redisplay-event* (make-instance 'force-redisplay))

(defun force-redisplay ()
  (dolist (editor *editors*)
    (cond ((eql editor *editor*)
           (setf (pending-redisplay editor) t))
          (t (mezzano.supervisor::fifo-push *force-redisplay-event* (fifo editor))))))
