(defpackage :mezzanine.editor
  (:use :cl)
  (:export #:spawn))

(in-package :mezzanine.editor)

;;; Lines.

(defclass line ()
  ((%next :initarg :next :accessor next-line)
   (%prev :initarg :prev :accessor previous-line)
   (%data :initarg :data :accessor data)
   (%version :initarg :version :accessor line-version)
   (%number :initarg :number :accessor line-number)
   (%mark-list :initarg :mark-list :accessor line-mark-list)
   (%buffer :initarg :buffer :accessor line-buffer))
  (:default-initargs :next nil
                     :prev nil
                     :data (make-array 50 :element-type 'character :adjustable t :fill-pointer 0)
                     :version 0
                     :number 0
                     :mark-list '()
                     :buffer nil))

(defgeneric line-character (line charpos))
(defgeneric (setf line-character) (value line charpos))
(defgeneric line-length (line))

(defmethod line-character ((line line) charpos)
  (aref (data line) charpos))

(defmethod (setf line-character) (value (line line) charpos)
  (setf (aref (data line) charpos) value))

(defmethod line-length ((line line))
  (length (data line)))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "N:~D V:~D" (line-number object) (line-version object))))

;;; Marks.

(defclass mark ()
  ((%line :initarg :line :reader mark-line)
   (%charpos :initarg :charpos :reader mark-charpos)
   ;; :left, :right or :temporary.
   (%kind :initarg :kind :reader mark-kind)))

(defgeneric (setf mark-line) (value mark))
(defgeneric (setf mark-charpos) (value mark))
(defgeneric (setf mark-kind) (value mark))

(defmethod print-object ((object mark) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S:~D ~S" (mark-line object) (mark-charpos object) (mark-kind object))))

;;; Buffers.

(defclass buffer ()
  ((%first-line :accessor first-line)
   (%last-line :accessor last-line)
   (%point :reader buffer-point)
   (%point-column-hint :initarg :point-column-hint :accessor point-column-hint)
   (%mark :reader buffer-mark)
   (%mark-active :initarg :mark-active :accessor buffer-mark-active)
   (%properties))
  (:default-initargs :mark-active nil :point-column-hint 0))

(defgeneric buffer-property (buffer property-name &optional default))
(defgeneric (setf buffer-property) (value buffer property-name &optional default))

(defmethod buffer-property ((buffer buffer) property-name &optional default)
  (gethash property-name (slot-value buffer '%properties) default))

(defmethod (setf buffer-property) (value (buffer buffer) property-name &optional default)
  (setf (gethash property-name (slot-value buffer '%properties) default) value))

(defmethod initialize-instance :after ((instance buffer) &key &allow-other-keys)
  (let ((line (make-instance 'line :buffer instance)))
    (setf (first-line instance) line
          (last-line instance) line
          (slot-value instance '%properties) (make-hash-table)
          (slot-value instance '%point) (make-mark line 0 :right)
          (slot-value instance '%mark) (make-mark line 0 :left))))

;;; The editor itself.

(defclass editor ()
  ((%fifo :initarg :fifo :reader fifo)
   (%pending-event :initarg :pending-event :accessor pending-event)
   (%pending-redisplay :initarg :pending-redisplay :accessor pending-redisplay)
   (%window :initarg :window :accessor window)
   (%frame :initarg :frame :accessor frame)
   (%buffer :initarg :buffer :accessor current-buffer)
   (%font :initarg :font :reader font)
   (%foreground-colour :initarg :foreground-colour :accessor foreground-colour)
   (%background-colour :initarg :background-colour :accessor background-colour)
   (%killed-region :initarg :killed-region :accessor killed-region)
   ;; Redisplay state.
   (%current-screen :initarg :screen :accessor editor-current-screen)
   (%line-cache :initarg :display-line-cache :accessor display-line-cache))
  (:default-initargs :pending-event nil
                     :pending-redisplay t
                     :foreground-colour #xFFDCDCCC
                     :background-colour #xFF3E3E3E
                     :killed-region nil
                     :screen nil
                     :display-line-cache '()))

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
          ;; Force character to uppercase when a modifier key is active, gets
          ;; around weirdness in how character names are processed.
          ;; #\C-a and #\C-A both parse as the same character (C-LATIN_CAPITAL_LETTER_A).
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

;;; Mark management.

(defun make-mark (line charpos &optional kind)
  (setf kind (or kind :temporary))
  (check-type kind (member :left :right :temporary))
  (let ((mark (make-instance 'mark
                             :line line
                             :charpos charpos
                             :kind kind)))
    (unless (eql kind :temporary)
      (push mark (line-mark-list line)))
    mark))

(defmethod (setf mark-line) (value (mark mark))
  (unless (eql (mark-kind mark) :temporary)
    (setf (line-mark-list (mark-line mark)) (remove mark (line-mark-list (mark-line mark))))
    (push mark (line-mark-list value)))
  (setf (slot-value mark '%charpos) (min (line-length value) (mark-charpos mark))
        (slot-value mark '%line) value))

(defmethod (setf mark-charpos) (value (mark mark))
  (check-type value (integer 0))
  (assert (<= value (line-length (mark-line mark))) (value) "Tried to move mark past end of line.")
  (setf (slot-value mark '%charpos) value))

(defmethod (setf mark-kind) (value (mark mark))
  (check-type value (member :temporary :left :right))
  (unless (eql (mark-kind mark) :temporary)
    ;; Remove from existing mark list.
    (setf (line-mark-list (mark-line mark)) (remove mark (line-mark-list (mark-line mark)))))
  (unless (eql value :temporary)
    ;; Add to mark list.
    (push mark (line-mark-list (mark-line mark))))
  (setf (slot-value mark '%kind) value))

(defun copy-mark (mark &optional kind)
  (make-mark (mark-line mark) (mark-charpos mark) kind))

(defun delete-mark (mark)
  (setf (line-mark-list (mark-line mark)) (remove mark (line-mark-list (mark-line mark)))))

(defmacro with-mark ((name where &optional kind) &body body)
  `(let ((,name nil))
     (unwind-protect
          (progn
            (setf ,name (copy-mark ,where ,kind))
            ,@body)
       (when ,name
         (delete-mark ,name)))))

(defun move-mark-to-mark (move-this-one here)
  (setf (mark-line move-this-one) (mark-line here)
        (mark-charpos move-this-one) (mark-charpos here)))

(defun mark-equal-p (a b)
  (and (eql (mark-line a) (mark-line b))
       (eql (mark-charpos a) (mark-charpos b))))

(defun point-to-mark (buffer mark)
  (move-mark-to-mark (buffer-point buffer) mark))

(defun mark-to-point (buffer mark)
  (move-mark-to-mark mark (buffer-point buffer)))

(defun mark-at-point-p (buffer mark)
  (mark-equal-p mark (buffer-point buffer)))

(defun start-of-line-p (mark)
  (eql (mark-charpos mark) 0))

(defun end-of-line-p (mark)
  (eql (mark-charpos mark) (line-length (mark-line mark))))

;;; Sub-editor. Buffer manipulation.

(defconstant +line-number-increment+ 10000)

(defun fully-renumber-lines-from (line)
  (do ((l line (next l)))
      ((null l))
    (setf (line-number line) (+ (line-number (prev line)) +line-number-increment+))))

(defun update-point-column-hint (buffer)
  (setf (point-column-hint buffer) (mark-charpos (buffer-point buffer))))

(defun insert-line (buffer)
  "Insert a new line after the point, splitting the current line if needed.
The point is positioned at the start of the new line."
  (let* ((point (buffer-point buffer))
         (current-line (mark-line point))
         (current-charpos (mark-charpos point))
         (new-line (make-instance 'line
                                  :buffer buffer
                                  :next (next-line current-line)
                                  :prev current-line
                                  :data (make-array (- (line-length current-line)
                                                       current-charpos)
                                                    :element-type 'character
                                                    :adjustable t
                                                    :fill-pointer t))))
    ;; Update line contents.
    (replace (data new-line) (data current-line)
             :start2 current-charpos)
    (setf (fill-pointer (data current-line)) current-charpos)
    (incf (line-version current-line))
    ;; Link into the line list.
    (cond ((next-line current-line)
           (setf (previous-line (next-line current-line)) new-line))
          (t (setf (last-line buffer) new-line)))
    (setf (next-line current-line) new-line)
    ;; Ensure coherent numbering.
    (cond ((and (next-line new-line)
                (eql (1+ (line-number current-line)) (line-number (next-line new-line))))
           ;; No numbers between. Give up and renumber everything from the new line forward.
           ;; Could be smarter.
           (fully-renumber-lines-from new-line))
          ((next-line new-line)
           ;; Midway between the previous (current) and the next line.
           (setf (line-number new-line) (+ (line-number current-line)
                                           (truncate (- (line-number (next-line new-line)) (line-number current-line)) 2))))
          (t (setf (line-number new-line) (+ (line-number current-line) +line-number-increment+))))
    ;; Update marks.
    (dolist (mark (line-mark-list current-line))
      (when (or (and (eql (mark-kind mark) :right)
                     (eql (mark-charpos mark) current-charpos))
                (> (mark-charpos mark) current-charpos))
        (let ((real-pos (- (line-length current-line) (mark-charpos mark))))
          (setf (mark-line mark) new-line
                (mark-charpos mark) real-pos))))
    (update-point-column-hint buffer))
  (values))

(defun insert-char (buffer character)
  "Insert CHARACTER after point, then advance point."
  (let* ((point (buffer-point buffer))
         (current-line (mark-line point))
         (current-charpos (mark-charpos point)))
    (cond ((eql (line-length current-line) current-charpos)
           ;; Inserting at end.
           (vector-push-extend character (data current-line)))
          (t ;; Inserting in the middle or at the start.
           ;; Make sure the vector is long enough.
           (vector-push-extend character (data current-line))
           (replace (data current-line) (data current-line)
                    :start1 (1+ current-charpos)
                    :start2 current-charpos)
           (setf (aref (data current-line) current-charpos) character)))
    (incf (line-version current-line))
    ;; Update marks.
    (dolist (mark (line-mark-list current-line))
      (when (or (and (eql (mark-kind mark) :right)
                     (eql (mark-charpos mark) current-charpos))
                (> (mark-charpos mark) current-charpos))
        (incf (mark-charpos mark)))))
  (update-point-column-hint buffer)
  (values))

(defun insert-string (buffer string &optional (translate-newlines t))
  (loop for ch across (string string)
     if (and translate-newlines (char= ch #\Newline))
     do (insert-line buffer)
     else do (insert-char buffer ch)))

(defun order-marks (mark-1 mark-2)
  (let ((line-1 (mark-line mark-1))
        (line-2 (mark-line mark-2)))
    (cond ((eql line-1 line-2)
           (if (> (mark-charpos mark-1) (mark-charpos mark-2))
               (values mark-2 mark-1)
               (values mark-1 mark-2)))
          ((> (line-number line-1)
              (line-number line-2))
           (values mark-2 mark-1))
          (t (values mark-1 mark-2)))))

(defun insert-region (buffer mark-1 mark-2)
  (setf (values mark-1 mark-2) (order-marks mark-1 mark-2))
  (do ((m1 (copy-mark mark-1))
       (m2 (copy-mark mark-2)))
      ((mark-equal-p m1 m2))
    (if (end-of-line-p m1)
        (insert-line buffer)
        (insert-char buffer (line-character (mark-line m1) (mark-charpos m1))))
    (move-mark m1)))

(defun yank-region (buffer)
  (when (killed-region *editor*)
    (insert-region buffer (car (killed-region *editor*)) (cdr (killed-region *editor*)))))

(defun delete-region (buffer mark-1 mark-2)
  "Delete region designated by MARK-1 and MARK-2 from buffer.
Returns the deleted region as a pair of marks into a disembodied line."
  (setf (values mark-1 mark-2) (order-marks mark-1 mark-2))
  (cond ((eql (mark-line mark-1) (mark-line mark-2))
         ;; Same line.
         (let* ((line (mark-line mark-1))
                (start (mark-charpos mark-1))
                (end (mark-charpos mark-2))
                (data (make-array (- end start)
                                  :element-type 'character
                                  :adjustable t
                                  :fill-pointer t)))
           ;; Extract deleted data.
           (replace data (data line)
                    :start2 start
                    :end2 end)
           ;; Delete data.
           (replace (data line) (data line)
                    :start1 start
                    :start2 end)
           (decf (fill-pointer (data line)) (- end start))
           ;; Update version.
           (incf (line-version line))
           ;; Update marks.
           (dolist (mark (line-mark-list line))
             (when (> (mark-charpos mark) start)
               (decf (mark-charpos mark) (- end start))))
           ;; Done.
           (let ((new-line (make-instance 'line :data data)))
             (values (make-mark new-line 0 :left)
                     (make-mark new-line (length data) :right)))))
        (t ;; Different lines.
         (let* ((first-line (mark-line mark-1))
                (first-chpos (mark-charpos mark-1))
                (next-line (next-line first-line))
                (last-line (mark-line mark-2))
                (last-chpos (mark-charpos mark-2))
                (data (make-array (- (line-length first-line) first-chpos)
                                  :element-type 'character
                                  :adjustable t
                                  :fill-pointer t)))
           (replace data (data first-line) :start2 first-chpos)
           ;; Join lines together.
           (adjust-array (data first-line)
                         (+ first-chpos
                            (- (line-length last-line) last-chpos))
                         :fill-pointer t)
           (replace (data first-line) (data last-line)
                    :start1 first-chpos
                    :start2 last-chpos)
           (incf (line-version first-line))
           (incf (line-version last-line))
           ;; Unlink intermediate lines & the last line from the line list.
           (cond ((next-line last-line)
                  (setf (previous-line (next-line last-line)) first-line))
                 (t (setf (last-line buffer) first-line)))
           (setf (next-line first-line) (next-line last-line))
           (setf (next-line last-line) nil
                 (line-buffer last-line) nil
                 (fill-pointer (data last-line)) last-chpos)
           ;; Adjust first-line marks.
           (dolist (mark (line-mark-list first-line))
             (when (> (mark-charpos mark) first-chpos)
               (setf (mark-charpos mark) first-chpos)))
           ;; Adjust last-line marks.
           (dolist (mark (line-mark-list last-line))
             (let ((new-pos (+ first-chpos (max 0 (- (mark-charpos mark) last-chpos)))))
               (setf (mark-line mark) first-line
                     (mark-charpos mark) new-pos)))
           ;; Adjust middle marks and fix lines.
           (do ((line next-line (next-line line)))
               ((eql line last-line))
             (incf (line-version line))
             (setf (line-buffer line) nil)
             (dolist (mark (line-mark-list line))
               (setf (mark-line mark) first-line
                     (mark-charpos mark) first-chpos)))
           ;; Done.
           (let ((new-line (make-instance 'line
                                          :data data
                                          :next next-line)))
             (setf (previous-line next-line) new-line)
             (values (make-mark new-line 0 :left)
                     (make-mark last-line last-chpos :right)))))))

(defun kill-region (buffer mark-1 mark-2)
  (multiple-value-bind (first-mark last-mark)
      (delete-region buffer mark-1 mark-2)
    (setf (killed-region *editor*) (cons first-mark last-mark))))

(defun kill-line (buffer)
  "Kill from point to the end of the line. If the point is at the end of the line,
then merge the current line and next line."
  (let ((point (buffer-point buffer)))
    (with-mark (here point :left)
      (if (end-of-line-p point)
          (move-mark point)
          (move-end-of-line buffer))
      (unwind-protect
           (delete-region buffer here point)
        (point-to-mark buffer here))))
  (values))

(defun delete-forward-char (buffer &optional (n 1))
  "Delete the following N characters (previous if N is negative)."
  (let ((point (buffer-point buffer)))
    (with-mark (here point :left)
      (move-mark point n)
      (unwind-protect
           (delete-region buffer here point)
        (point-to-mark buffer here))))
  (values))

(defun delete-backward-char (buffer &optional (n 1))
  (delete-forward-char buffer (- n)))

;;; Point motion.

(defun move-beginning-of-line (buffer)
  (setf (mark-charpos (buffer-point buffer)) 0)
  (update-point-column-hint buffer)
  (values))

(defun move-end-of-line (buffer)
  (let ((point (buffer-point buffer)))
    (setf (mark-charpos point) (line-length (mark-line point))))
  (update-point-column-hint buffer)
  (values))

(defun move-mark (mark &optional (n 1))
  "Move MARK forward by N character. Move backwards if N is negative."
  (cond ((minusp n)
         (setf n (- n))
         (dotimes (i n)
           (let ((current-line (mark-line mark)))
             (cond ((zerop (mark-charpos mark))
                    ;; At start of line.
                    (when (previous-line current-line)
                      (setf (mark-line mark) (previous-line current-line)
                            (mark-charpos mark) (line-length (previous-line current-line)))))
                   (t ;; Moving within a line.
                    (decf (mark-charpos mark)))))))
        (t
         (dotimes (i n)
           (let ((current-line (mark-line mark)))
             (cond ((eql (line-length current-line) (mark-charpos mark))
                    ;; At end of line.
                    (when (next-line current-line)
                      (setf (mark-line mark) (next-line current-line)
                            (mark-charpos mark) 0)))
                   (t ;; Moving within a line.
                    (incf (mark-charpos mark))))))))
  (values))

(defun forward-char (buffer &optional (n 1))
  "Move point forward by N characters. Move backwards if N is negative."
  (move-mark (buffer-point buffer) n)
  (update-point-column-hint buffer)
  (values))

(defun backward-char (buffer &optional (n 1))
  (forward-char buffer (- n)))

(defun forward-line (buffer &optional (n 1))
  "Move point down by N lines. N may be negative.
Tries to stay as close to the hint column as possible."
  (let ((accessor #'next-line)
        (point (buffer-point buffer)))
    (when (minusp n)
      (setf n (- n)
            accessor #'previous-line))
    (dotimes (i n)
      (let* ((current-line (mark-line point))
             (new-line (funcall accessor current-line)))
        (cond (new-line
               (setf (mark-line point) new-line
                     (mark-charpos point) (min (point-column-hint buffer)
                                               (line-length new-line))))
              (t (return))))))
  (values))

(defun backward-line (buffer &optional (n 1))
  (forward-line buffer (- n)))

(defun test-fill (buffer)
  (let ((width (1- (truncate (editor-width)
                             (mezzanine.gui.font:glyph-advance (mezzanine.gui.font:character-to-glyph (font *editor*) #\M))))))
    (let ((mark (copy-mark (buffer-point buffer) :left)))
      (dotimes (i (* (window-rows) 2))
        (dotimes (j width)
          (insert-char buffer (code-char (+ #x20 i))))
        (insert-line buffer))
      (point-to-mark buffer mark))))

;;; Mark stuff.

(defun set-mark (buffer)
  (cond
    ;; If the mark is active and the point is at mark, then
    ;; deactivate the mark.
    ((and (buffer-mark-active buffer)
          (mark-at-point-p buffer (buffer-mark buffer)))
     (setf (buffer-mark-active buffer) nil))
    ;; If the mark is not active, then activate it.
    ((not (buffer-mark-active buffer))
     (setf (buffer-mark-active buffer) t)))
  ;; Always move the mark to point.
  (mark-to-point buffer (buffer-mark buffer)))

(defun exchange-point-and-mark (buffer)
  (let ((saved (copy-mark (buffer-mark buffer))))
    (move-mark-to-mark (buffer-mark buffer) (buffer-point buffer))
    (move-mark-to-mark (buffer-point buffer) saved)
    (setf (buffer-mark-active buffer) t)
    (update-point-column-hint buffer)))

;;; Redisplay.

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
      (mezzanine.gui.widgets:frame-size (frame *editor*))
    (truncate (- (mezzanine.gui.compositor:height (window *editor*)) top bottom)
              (mezzanine.gui.font:line-height (font *editor*)))))

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
      (mezzanine.gui.widgets:frame-size (frame *editor*))
    (- (mezzanine.gui.compositor:width (window *editor*)) left right)))

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

(defun render-display-line-2 (line start)
  (multiple-value-bind (line-1 line-1-charpos line-1-number line-2 line-2-charpos line-2-number)
      (region-bounds (buffer-point (current-buffer *editor*)) (buffer-mark (current-buffer *editor*)))
    (loop
       with pen = 0
       with font = (font *editor*)
       with baseline = (mezzanine.gui.font:ascender font)
       with foreground = (foreground-colour *editor*)
       with background = (background-colour *editor*)
       with line-height = (mezzanine.gui.font:line-height font)
       with win-width = (editor-width)
       with point = (buffer-point (current-buffer *editor*))
       with mark-active = (buffer-mark-active (current-buffer *editor*))
       with buffer = (make-array (list line-height win-width)
                                 :element-type '(unsigned-byte 32)
                                 :initial-element background)
       for ch-position from start below (line-length line)
       for glyph = (mezzanine.gui.font:character-to-glyph font (line-character line ch-position))
       for mask = (mezzanine.gui.font:glyph-mask glyph)
       for advance = (mezzanine.gui.font:glyph-advance glyph)
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
             (mezzanine.gui:bitset line-height advance
                                   foreground
                                   buffer 0 pen))
           (mezzanine.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1)
                                                  (if at-point
                                                      background
                                                      foreground)
                                                  mask 0 0
                                                  buffer
                                                  (- baseline (mezzanine.gui.font:glyph-yoff glyph))
                                                  (+ pen (mezzanine.gui.font:glyph-xoff glyph)))
           ;; Underline the region.
           (when in-region
             (mezzanine.gui:bitset-argb-xrgb 1 advance
                                             (if at-point
                                                 background
                                                 foreground)
                                             buffer baseline pen))
           (incf pen advance))
       finally
       ;; Reached end of line, check for the point.
         (when (and (eql line (mark-line point))
                    (eql ch-position (mark-charpos point)))
           ;; Point is here, render it past the last character.
           (let* ((glyph (mezzanine.gui.font:character-to-glyph font #\Space))
                  (advance (mezzanine.gui.font:glyph-advance glyph)))
             (when (<= (+ pen advance) win-width) ; FIXME, how to display point at end of line & display line properly. also fix blit crash bug.
               (mezzanine.gui:bitset line-height advance
                                     foreground
                                     buffer 0 pen))))
       ;; TODO: Render underline to end of line region spans whole line.
         (return (values buffer ch-position)))))

(defun render-display-line-1 (line start)
  (multiple-value-bind (buffer end)
      (render-display-line-2 line start)
    (let ((display-line (make-instance 'display-line
                                       :line line
                                       :version (line-version line)
                                       :start start
                                       :end end
                                       :representation buffer)))
      (push display-line (display-line-cache *editor*))
      display-line)))

(defun render-display-line (line fn)
  "Render display lines for real line LINE, calling FN with each display line."
  (cond ((zerop (line-length line))
         (funcall fn (or (get-display-line-from-cache line 0)
                         (render-display-line-1 line 0))))
        (t (do ((start 0))
               ((>= start (line-length line)))
             (let ((display-line (or (get-display-line-from-cache line start)
                                     (render-display-line-1 line start))))
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
      (mezzanine.gui.widgets:frame-size (frame *editor*))
    (let* ((fb (mezzanine.gui.compositor:window-buffer (window *editor*)))
           (line-height (mezzanine.gui.font:line-height (font *editor*)))
           (real-y (+ top (* y line-height)))
           (win-width (editor-width)))
      (if line
          ;; Blitting line.
          (mezzanine.gui:bitblt line-height win-width
                                (display-line-representation line)
                                0 0
                                fb
                                real-y left)
          ;; Line is empty.
          (mezzanine.gui:bitset line-height win-width
                                (background-colour *editor*)
                                fb
                                real-y left))
      (mezzanine.gui.compositor:damage-window (window *editor*)
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
          (when (not previous-point-position)
            (setf previous-point-position (copy-mark point :right)
                  (buffer-property buffer 'pane-previous-point-position) previous-point-position))
          (when (not previous-mark-position)
            (setf previous-mark-position (copy-mark mark :left)
                  (buffer-property buffer 'pane-previous-mark-position) previous-mark-position))
          ;; If the point has moved, then invalidate the line that contained the point and the line that
          ;; now holds the point.
          (when (not (mark-equal-p point previous-point-position))
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
                     (not (mark-equal-p point previous-point-position)))
            (flush-display-lines-in-region point previous-point-position))
          ;; If the mark is or was active and moves, flush lines between the old mark position
          ;; and the new position.
          ;; FIXME: This will cause a bunch of lines to be redrawn when the point & mark are exchanged.
          (when (and (or (buffer-mark-active buffer)
                         (buffer-property buffer 'pane-mark-was-active))
                     (not (mark-equal-p mark previous-mark-position)))
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
          (when (not point-line)
            (recenter buffer)
            (return-from redisplay nil))
          ;; Compare against the current screen, blitting when needed.
          (dotimes (y (window-rows))
            (let ((line (aref new-screen y)))
              (unless (eql (aref current-screen y) line)
                (blit-display-line line y)
                (setf (aref current-screen y) line)
                (check-pending-input))))
          ;; Prune the cache.
          (setf (display-line-cache *editor*) (subseq (display-line-cache *editor*) 0 (* (window-rows) 4))))
        t)
    (pending-input ()
      nil)))

(defun editor-loop ()
  (loop
     (handler-case
         (let ((ch (editor-read-char))
               (buffer (current-buffer *editor*)))
           (case ch
             ((nil)) ; Happens when there's a redisplay pending.
             (#\Newline (insert-line buffer))
             (#\C-F (forward-char buffer))
             (#\C-B (backward-char buffer))
             (#\C-N (forward-line buffer))
             (#\C-P (backward-line buffer))
             (#\C-A (move-beginning-of-line buffer))
             (#\C-E (move-end-of-line buffer))
             (#\C-K (kill-line buffer))
             (#\C-Q (insert-char buffer (editor-read-char)))
             (#\C-L (recenter buffer))
             (#\M-L (redraw-screen))
             (#\C-Space (set-mark buffer))
             (#\C-X (exchange-point-and-mark buffer))
             (#\Backspace (delete-backward-char buffer))
             (#\C-D (delete-forward-char buffer))
             (#\Delete (delete-forward-char buffer))
             (#\C-T (test-fill buffer))
             (#\C-W (kill-region buffer (buffer-point buffer) (buffer-mark buffer)))
             (#\C-Y (yank-region buffer))
             (t (cond ((find ch "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789!\"£$%^&*()_+-=[]{};'#:@~,./<>?`~|\\"
                             :test #'char=)
                       (insert-char buffer ch)))))
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
               (*editor* (make-instance 'editor
                                        :fifo fifo
                                        :font font
                                        :window window
                                        :frame frame
                                        :buffer (make-instance 'buffer))))
          (mezzanine.gui.widgets:draw-frame frame)
          (catch 'quit
            (editor-loop)))))))

(defun spawn (&key width height)
  (mezzanine.supervisor:make-thread (lambda () (editor-main width height))
                                    :name "Editor"))
