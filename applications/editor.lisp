;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.editor
  (:use :cl)
  (:export #:spawn #:open-file-request))

(in-package :mezzano.editor)

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
   (%mark :reader buffer-mark)
   (%mark-active :initarg :mark-active :accessor buffer-mark-active)
   (%properties))
  (:default-initargs :mark-active nil))

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
   (%buffer-list :initarg :buffer-list :accessor buffer-list)
   (%buffer :initarg :buffer :accessor current-buffer)
   (%last-buffer :initarg :last-buffer :accessor last-buffer)
   (%font :initarg :font :reader font)
   (%foreground-colour :initarg :foreground-colour :accessor foreground-colour)
   (%background-colour :initarg :background-colour :accessor background-colour)
   (%killed-region :initarg :killed-region :accessor killed-region)
   (%global-key-map :initarg :global-key-map :accessor global-key-map)
   (%post-command-hooks :initarg :post-command-hooks :accessor post-command-hooks)
   ;; Redisplay state.
   (%current-screen :initarg :screen :accessor editor-current-screen)
   (%line-cache :initarg :display-line-cache :accessor display-line-cache))
  (:default-initargs :pending-event nil
                     :pending-redisplay t
                     :foreground-colour mezzano.gui:*default-foreground-colour*
                     :background-colour mezzano.gui:*default-background-colour*
                     :buffer-list '()
                     :last-buffer '()
                     :killed-region nil
                     :global-key-map (make-hash-table)
                     :post-command-hooks '()
                     :screen nil
                     :display-line-cache '()))

(defclass open-file-request ()
  ((%path :initarg :path :reader path)))

(defvar *last-command*)
(defvar *this-command*)
(defvar *last-character*)
(defvar *this-character*)
(defvar *last-chord*)
(defvar *this-chord*)
(defvar *minibuffer*)
(defvar *minibuffer-key-map*)

(defvar *editor*)

(defgeneric dispatch-event (editor event)
  (:method (editor event)))

(defmethod dispatch-event (editor (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame editor)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame editor)))

(defmethod dispatch-event (editor (event mezzano.gui.compositor:mouse-event))
  (handler-case
      (mezzano.gui.widgets:frame-mouse-event (frame editor) event)
    (mezzano.gui.widgets:close-button-clicked ()
      (throw 'quit nil))))

(defmethod dispatch-event (editor (event mezzano.gui.compositor:window-close-event))
  (declare (ignore editor event))
  (throw 'quit nil))

(defmethod dispatch-event (editor (event mezzano.gui.compositor:key-event))
  (when (not (mezzano.gui.compositor:key-releasep event))
    (throw 'next-character
      (if (mezzano.gui.compositor:key-modifier-state event)
          ;; Force character to uppercase when a modifier key is active, gets
          ;; around weirdness in how character names are processed.
          ;; #\C-a and #\C-A both parse as the same character (C-LATIN_CAPITAL_LETTER_A).
          (sys.int::make-character (char-code (char-upcase (mezzano.gui.compositor:key-key event)))
                                   :control (find :control (mezzano.gui.compositor:key-modifier-state event))
                                   :meta (find :meta (mezzano.gui.compositor:key-modifier-state event))
                                   :super (find :super (mezzano.gui.compositor:key-modifier-state event))
                                   :hyper (find :hyper (mezzano.gui.compositor:key-modifier-state event)))
          (mezzano.gui.compositor:key-key event)))))

(defmethod dispatch-event (editor (event open-file-request))
  (let ((*editor* editor))
    (find-file (path event))))

(defun editor-read-char-1 ()
  (catch 'next-character
    (when (pending-event *editor*)
      (let ((event (pending-event *editor*)))
        (setf (pending-event *editor*) nil)
      (dispatch-event *editor* event)))
    (loop
       (when (pending-redisplay *editor*)
         (throw 'next-character nil))
       (dispatch-event *editor* (mezzano.supervisor:fifo-pop (fifo *editor*))))))

(defun editor-read-char ()
  (loop
     (let ((ch (editor-read-char-1)))
       (when ch
         (return ch)))
     (setf (pending-redisplay *editor*) (not (redisplay)))))

(define-condition pending-input () ())

(defun check-pending-input ()
  (cond ((pending-event *editor*)
         (signal 'pending-input))
        (t (let ((event (mezzano.supervisor:fifo-pop (fifo *editor*) nil)))
             (when event
               (setf (pending-event *editor*) event)
               (signal 'pending-input))))))

(defun refresh-title ()
  (let ((buffer (current-buffer *editor*)))
    (setf (mezzano.gui.widgets:frame-title (frame *editor*))
          (format nil "Editor - ~A~A"
                  (or (buffer-property buffer 'name) "Untitled")
                  (cond ((buffer-property buffer 'new-file)
                         " (New file)")
                        ((buffer-modified buffer)
                         " (Modified)")
                        (t ""))))
    (mezzano.gui.widgets:draw-frame (frame *editor*))))

(defun switch-to-buffer (buffer)
  (setf (current-buffer *editor*) buffer
        (pending-redisplay *editor*) t)
  (refresh-title))

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

(defun mark= (a b)
  (and (eql (mark-line a) (mark-line b))
       (eql (mark-charpos a) (mark-charpos b))))

(defun mark< (a b)
  (or (< (line-number (mark-line a)) (line-number (mark-line b)))
      (and (eql (line-number (mark-line a)) (line-number (mark-line b)))
           (< (mark-charpos a) (mark-charpos b)))))

(defun mark> (a b)
  (mark< b a))

(defun mark<= (a b)
  (not (mark> a b)))

(defun mark>= (a b)
  (not (mark< a b)))

(defun point-to-mark (buffer mark)
  (move-mark-to-mark (buffer-point buffer) mark))

(defun mark-to-point (buffer mark)
  (move-mark-to-mark mark (buffer-point buffer)))

(defun mark-at-point-p (buffer mark)
  (mark= mark (buffer-point buffer)))

(defun start-of-line-p (mark)
  (eql (mark-charpos mark) 0))

(defun end-of-line-p (mark)
  (eql (mark-charpos mark) (line-length (mark-line mark))))

;;; Sub-editor. Buffer manipulation.

(defun buffer-modified (buffer)
  (buffer-property buffer 'modified))

(defun (setf buffer-modified) (value buffer)
  (when (not (eql (buffer-property buffer 'modified) value))
    (setf (buffer-property buffer 'modified) value)
    (refresh-title))
  value)

(defconstant +line-number-increment+ 10000)

(defun fully-renumber-lines-from (line)
  (do ((l line (next-line l)))
      ((null l))
    (setf (line-number line) (+ (line-number (previous-line line)) +line-number-increment+))))

(defun insert-line (point)
  "Insert a new line at POINT, splitting the current line if needed.
Don't use this, use INSERT instead."
  (let* ((current-line (mark-line point))
         (current-charpos (mark-charpos point))
         (new-line (make-instance 'line
                                  :buffer (line-buffer current-line)
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
          ((line-buffer current-line)
           (setf (last-line (line-buffer current-line)) new-line)))
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
    ;; Mark buffer modified (if any).
    (when (line-buffer current-line)
      (setf (buffer-modified (line-buffer current-line)) t)))
  (values))

(defun insert-char (point character)
  "Insert CHARACTER at POINT.
Don't use this directly, use INSERT instead."
  (let* ((current-line (mark-line point))
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
        (incf (mark-charpos mark))))
    ;; Mark buffer modified (if any).
    (when (line-buffer current-line)
      (setf (buffer-modified (line-buffer current-line)) t)))
  (values))

(defun insert (buffer string)
  "Insert STRING into BUFFER at point. STRING is a string-designator, so can be a character."
  (loop for ch across (string string)
     if (char= ch #\Newline)
     do (insert-line (buffer-point buffer))
     else do (insert-char (buffer-point buffer) ch)))

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

(defun insert-region-at-mark (point mark-1 mark-2)
  (setf (values mark-1 mark-2) (order-marks mark-1 mark-2))
  (let ((line-1 (mark-line mark-1))
        (chpos-1 (mark-charpos mark-1))
        (line-2 (mark-line mark-2))
        (chpos-2 (mark-charpos mark-2))
        (insert-line (mark-line point))
        (insert-chpos (mark-charpos point)))
    (cond ((eql line-1 line-2)
           ;; Not inserting any newlines, just make the line bigger.
           (when (not (eql chpos-1 chpos-2))
             (adjust-array (data insert-line)
                           (+ (line-length insert-line) (- chpos-2 chpos-1))
                           :fill-pointer t)
             (when (not (eql (line-length insert-line) insert-chpos))
               ;; Inserting in the middle, need to shuffle data up.
               (replace (data insert-line) (data insert-line)
                        :start1 (+ insert-chpos (- chpos-2 chpos-1))
                        :start2 insert-chpos))
             ;; Insert new data into the hole.
             (replace (data insert-line) (data line-1)
                      :start1 insert-chpos
                      :start2 chpos-1
                      :end2 chpos-2)
             (incf (line-version insert-line))
             ;; Update marks.
             (dolist (mark (line-mark-list insert-line))
               (when (or (and (eql (mark-kind mark) :right)
                              (eql (mark-charpos mark) insert-chpos))
                         (> (mark-charpos mark) insert-chpos))
                 (incf (mark-charpos mark) (- chpos-2 chpos-1))))
             ;; Mark buffer modified (if any).
             (when (line-buffer line-1)
               (setf (buffer-modified (line-buffer line-1)) t))))
          (t ;; Inserting multiple lines.
           ;; todo properly...
           (do ((m1 (copy-mark mark-1))
                (m2 (copy-mark mark-2)))
               ((mark= m1 m2))
             (if (end-of-line-p m1)
                 (insert-line point)
                 (insert-char point (line-character (mark-line m1) (mark-charpos m1))))
             (move-mark m1))))))

(defun insert-region (buffer mark-1 mark-2)
  (insert-region-at-mark (buffer-point buffer)
                         mark-1 mark-2))

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
           ;; Mark buffer modified (if any).
           (when (line-buffer line)
             (setf (buffer-modified (line-buffer line)) t))
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
           ;; Mark buffer modified (if any).
           (when (line-buffer first-line)
             (setf (buffer-modified (line-buffer first-line)) t))
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
    (when (or (not (mark= first-mark last-mark))
              (eql *last-command* 'kill-region))
      (setf *this-command* 'kill-region))
    (cond ((and (killed-region *editor*)
                (eql *last-command* 'kill-region))
           ;; Append to killed region.
           (insert-region-at-mark (cdr (killed-region *editor*))
                                  first-mark last-mark))
          (t ;; New killed region.
           (setf (killed-region *editor*) (cons first-mark last-mark))))))

(defun kill-line (buffer)
  "Kill from point to the end of the line. If the point is at the end of the line,
then merge the current line and next line."
  (let ((point (buffer-point buffer)))
    (with-mark (here point :left)
      (if (end-of-line-p point)
          (move-mark point)
          (move-end-of-line buffer))
      (unwind-protect
           (kill-region buffer here point)
        (point-to-mark buffer here))))
  (values))

(defun delete-char (buffer &optional (n 1))
  "Delete the following N characters (previous if N is negative)."
  (let ((point (buffer-point buffer)))
    (with-mark (here point :left)
      (move-mark point n)
      (unwind-protect
           (delete-region buffer here point)
        (point-to-mark buffer here))))
  (values))

(defun buffer-string (buffer mark-1 mark-2)
  (setf (values mark-1 mark-2) (order-marks mark-1 mark-2))
  (with-output-to-string (str)
    (do ((m1 (copy-mark mark-1))
         (m2 (copy-mark mark-2)))
        ((mark= m1 m2))
      (if (end-of-line-p m1)
          (terpri str)
          (write-char (line-character (mark-line m1) (mark-charpos m1)) str))
      (move-mark m1))))

;;; Point motion.

(defun move-beginning-of-line (buffer)
  (setf (mark-charpos (buffer-point buffer)) 0)
  (values))

(defun move-end-of-line (buffer)
  (let ((point (buffer-point buffer)))
    (setf (mark-charpos point) (line-length (mark-line point))))
  (values))

(defun move-beginning-of-buffer (buffer)
  (setf (mark-line (buffer-point buffer)) (first-line buffer)
        (mark-charpos (buffer-point buffer)) 0)
  (values))

(defun move-end-of-buffer (buffer)
  (let ((point (buffer-point buffer)))
    (setf (mark-line point) (last-line buffer)
          (mark-charpos point) (line-length (mark-line point))))
  (values))

(defun move-mark (mark &optional (n 1))
  "Move MARK forward by N character. Move backwards if N is negative.
Returns false when the mark reaches the start or end of the buffer, true otherwise."
  (cond ((minusp n)
         (setf n (- n))
         (dotimes (i n)
           (let ((current-line (mark-line mark)))
             (cond ((zerop (mark-charpos mark))
                    (cond ((previous-line current-line)
                           ;; At start of line.
                           (setf (mark-line mark) (previous-line current-line)
                                 (mark-charpos mark) (line-length (previous-line current-line))))
                          (t ;; At start of buffer.
                           (return-from move-mark nil))))
                   (t ;; Moving within a line.
                    (decf (mark-charpos mark)))))))
        (t
         (dotimes (i n)
           (let ((current-line (mark-line mark)))
             (cond ((eql (line-length current-line) (mark-charpos mark))
                    (cond ((next-line current-line)
                           ;; At end of line.
                           (setf (mark-line mark) (next-line current-line)
                                 (mark-charpos mark) 0))
                          (t (return-from move-mark nil))))
                   (t ;; Moving within a line.
                    (incf (mark-charpos mark))))))))
  t)

(defun move-char (buffer &optional (n 1))
  "Move point forward by N characters. Move backwards if N is negative."
  (move-mark (buffer-point buffer) n)
  (values))

(defun move-line (buffer &optional (n 1))
  "Move point down by N lines. N may be negative.
Tries to stay as close to the hint column as possible."
  (let* ((accessor #'next-line)
         (point (buffer-point buffer)))
    (when (not (eql *last-command* 'next-line))
      (setf (buffer-property buffer 'column-hint 0) (mark-charpos point)))
    (setf *this-command* 'next-line)
    (when (minusp n)
      (setf n (- n)
            accessor #'previous-line))
    (dotimes (i n)
      (let* ((current-line (mark-line point))
             (new-line (funcall accessor current-line)))
        (cond (new-line
               (setf (mark-line point) new-line
                     (mark-charpos point) (min (buffer-property buffer 'column-hint)
                                               (line-length new-line))))
              (t (return))))))
  (values))

(defun character-right-of (mark)
  (cond ((end-of-line-p mark)
         (cond
           ((next-line (mark-line mark))
            ;; At end of line.
            #\Newline)
           (t ;; At end of buffer.
            nil)))
        (t (line-character (mark-line mark) (mark-charpos mark)))))

(defun character-left-of (mark)
  (cond ((start-of-line-p mark)
         (cond
           ((previous-line (mark-line mark))
            ;; At start of line.
            #\Newline)
           (t ;; At start of buffer.
            nil)))
        (t (line-character (mark-line mark) (1- (mark-charpos mark))))))

(defun scan (mark predicate jump key)
  (loop
     (let ((ch (funcall key mark)))
       (when (not ch)
         (return nil))
       (when (funcall predicate ch)
         (return t))
       (when (not (move-mark mark jump))
         (return nil)))))

(defun scan-forward (mark predicate)
  (scan mark predicate 1 #'character-right-of))

(defun scan-backward (mark predicate)
  (scan mark predicate -1 #'character-left-of))

(defun move-word (buffer &optional (n 1))
  "Move point forward by N words. N may be negative."
  (let ((point (buffer-point buffer))
        (fn #'scan-forward))
    (when (minusp n)
      (setf n (- n)
            fn #'scan-backward))
    (dotimes (i n)
      ;; Forward past leading non-alphanumberic characters.
      (funcall fn point #'alphanumericp)
      ;; And now past alphanumeric characters.
      (funcall fn point (complement #'alphanumericp)))))

(defun scan-sexp-forward (mark)
  (let ((pair-stack '())
        (first-char t))
    (flet ((whitespacep (ch)
             (eql (sys.int::readtable-syntax-type ch nil) :whitespace)))
      ;; Skip past any leading whitespace.
      (scan-forward mark (complement #'whitespacep))
      (loop
         (let ((ch (character-right-of mark)))
           (when (not ch)
             (return nil))
           (when (and (whitespacep ch) (not pair-stack))
             (return t))
           (cond ((eql ch (first pair-stack))
                  (pop pair-stack)
                  (when (not pair-stack)
                    ;; Found last match, finished.
                    (move-mark mark 1)
                    (return t)))
                 ((eql ch #\))
                  (if first-char
                      (error "Unmatched ~C." ch)
                      (return t)))
                 ((eql ch #\")
                  (push #\" pair-stack))
                 ((eql ch #\()
                  (push #\) pair-stack)))
           (move-mark mark 1))
         (setf first-char nil)))))

(defun scan-sexp-backward (mark)
  (let ((pair-stack '())
        (first-char t))
    (flet ((whitespacep (ch)
             (eql (sys.int::readtable-syntax-type ch nil) :whitespace)))
      ;; Skip past any leading whitespace.
      (scan-backward mark (complement #'whitespacep))
      (loop
         (let ((ch (character-left-of mark)))
           (when (not ch)
             (return nil))
           (when (and (whitespacep ch) (not pair-stack))
             (return t))
           (cond ((eql ch (first pair-stack))
                  (pop pair-stack)
                  (when (not pair-stack)
                    ;; Found last match, finished.
                    (move-mark mark -1)
                    (return t)))
                 ((eql ch #\()
                  (if first-char
                      (error "Unmatched ~C." ch)
                      (return t)))
                 ((eql ch #\")
                  (push #\" pair-stack))
                 ((eql ch #\))
                  (push #\( pair-stack)))
           (move-mark mark -1))
         (setf first-char nil)))))

(defun move-sexp (buffer &optional (n 1))
  "Move point forward by N s-expressions. N may be negative."
  (let ((point (buffer-point buffer))
        (fn #'scan-sexp-forward))
    (when (minusp n)
      (setf n (- n)
            fn #'scan-sexp-backward))
    (dotimes (i n)
      (funcall fn point))))

(defun test-fill (buffer)
  (let ((width (1- (truncate (editor-width)
                             (mezzano.gui.font:glyph-advance (mezzano.gui.font:character-to-glyph (font *editor*) #\M))))))
    (with-mark (mark point :left)
      (dotimes (i (* (window-rows) 2))
        (dotimes (j width)
          (insert buffer (code-char (+ #x20 i))))
        (insert buffer #\Newline))
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
    (setf (buffer-mark-active buffer) t)))

;;; Minibuffer stuff.

(defun fix-minibuffer-point-position-hook ()
  (when (mark< (buffer-point *minibuffer*)
               (buffer-property *minibuffer* 'minibuffer-prompt-end))
    (point-to-mark *minibuffer*
                   (buffer-property *minibuffer* 'minibuffer-prompt-end)))
  (when (mark< (buffer-mark *minibuffer*)
               (buffer-property *minibuffer* 'minibuffer-prompt-end))
    (move-mark-to-mark (buffer-mark *minibuffer*)
                       (buffer-property *minibuffer* 'minibuffer-prompt-end))))

(defun minibuffer-finish-input-command ()
  (move-end-of-buffer *minibuffer*)
  (throw 'minibuffer-result
    (buffer-string *minibuffer*
                   (buffer-property *minibuffer* 'minibuffer-prompt-end)
                   (buffer-point *minibuffer*))))

(defun read-from-minibuffer (prompt)
  "Read a string from the minibuffer."
  (let ((old-key-map (global-key-map *editor*))
        (old-buffer (current-buffer *editor*))
        (old-post-command-hooks (post-command-hooks *editor*)))
    (when (eql old-buffer *minibuffer*)
      (error "Recursive minibuffer read!"))
    (unwind-protect
         (progn
           (setf *minibuffer* (make-instance 'buffer))
           (setf (global-key-map *editor*) *minibuffer-key-map*
                 (buffer-property *minibuffer* 'name) "*Minibuffer*")
           (push 'fix-minibuffer-point-position-hook (post-command-hooks *editor*))
           (switch-to-buffer *minibuffer*)
           (insert *minibuffer* prompt)
           (setf (buffer-property *minibuffer* 'minibuffer-prompt-end) (copy-mark (buffer-point *minibuffer*) :left))
           (catch 'minibuffer-result
             (editor-loop)))
      (switch-to-buffer old-buffer)
      (setf (global-key-map *editor*) old-key-map
            (post-command-hooks *editor*) old-post-command-hooks))))

(defun minibuffer-yes-or-no-p (&optional control &rest arguments)
  (let ((prompt (apply 'format nil control arguments)))
    (loop
       (let ((line (read-from-minibuffer (format nil "~A (Yes or No) " prompt))))
         (cond ((string-equal line "yes")
                (return t))
               ((string-equal line "no")
                (return nil)))))))

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
      (mezzano.gui.widgets:frame-size (frame *editor*))
    (truncate (- (mezzano.gui.compositor:height (window *editor*)) top bottom)
              (mezzano.gui.font:line-height (font *editor*)))))

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

(defun render-display-line-2 (line start)
  (multiple-value-bind (line-1 line-1-charpos line-1-number line-2 line-2-charpos line-2-number)
      (region-bounds (buffer-point (current-buffer *editor*)) (buffer-mark (current-buffer *editor*)))
    (loop
       with pen = 0
       with font = (font *editor*)
       with baseline = (mezzano.gui.font:ascender font)
       with foreground = (foreground-colour *editor*)
       with background = (background-colour *editor*)
       with line-height = (mezzano.gui.font:line-height font)
       with win-width = (editor-width)
       with point = (buffer-point (current-buffer *editor*))
       with mark-active = (buffer-mark-active (current-buffer *editor*))
       with buffer = (make-array (list line-height win-width)
                                 :element-type '(unsigned-byte 32)
                                 :initial-element background)
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
             (mezzano.gui:bitset line-height advance
                                   foreground
                                   buffer 0 pen))
           (mezzano.gui:bitset-argb-xrgb-mask-8 (array-dimension mask 0) (array-dimension mask 1)
                                                (if at-point
                                                    background
                                                    foreground)
                                                mask 0 0
                                                buffer
                                                (- baseline (mezzano.gui.font:glyph-yoff glyph))
                                                (+ pen (mezzano.gui.font:glyph-xoff glyph)))
           ;; Underline the region.
           (when in-region
             (mezzano.gui:bitset-argb-xrgb 1 advance
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
           (let* ((glyph (mezzano.gui.font:character-to-glyph font #\Space))
                  (advance (mezzano.gui.font:glyph-advance glyph)))
             (when (<= (+ pen advance) win-width) ; FIXME, how to display point at end of line & display line properly. also fix blit crash bug.
               (mezzano.gui:bitset line-height advance
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
      (mezzano.gui.widgets:frame-size (frame *editor*))
    (let* ((fb (mezzano.gui.compositor:window-buffer (window *editor*)))
           (line-height (mezzano.gui.font:line-height (font *editor*)))
           (real-y (+ top (* y line-height)))
           (win-width (editor-width)))
      (if line
          ;; Blitting line.
          (mezzano.gui:bitblt line-height win-width
                              (display-line-representation line)
                              0 0
                              fb
                              real-y left)
          ;; Line is empty.
          (mezzano.gui:bitset line-height win-width
                              (background-colour *editor*)
                              fb
                              real-y left))
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

;;;; Begin command wrappers.

;;; Motion & mark commands.

(defun forward-char-command ()
  (move-char (current-buffer *editor*)))

(defun backward-char-command ()
  (move-char (current-buffer *editor*) -1))

(defun next-line-command ()
  (move-line (current-buffer *editor*)))

(defun previous-line-command ()
  (move-line (current-buffer *editor*) -1))

(defun forward-word-command ()
  (move-word (current-buffer *editor*)))

(defun backward-word-command ()
  (move-word (current-buffer *editor*) -1))

(defun forward-sexp-command ()
  (move-sexp (current-buffer *editor*)))

(defun backward-sexp-command ()
  (move-sexp (current-buffer *editor*) -1))

(defun move-beginning-of-line-command ()
  (move-beginning-of-line (current-buffer *editor*)))

(defun move-end-of-line-command ()
  (move-end-of-line (current-buffer *editor*)))

(defun move-beginning-of-buffer-command ()
  (move-beginning-of-buffer (current-buffer *editor*)))

(defun move-end-of-buffer-command ()
  (move-end-of-buffer (current-buffer *editor*)))

(defun set-mark-command ()
  (set-mark (current-buffer *editor*)))

(defun exchange-point-and-mark-command ()
  (exchange-point-and-mark (current-buffer *editor*)))

;;; Editing commands.

(defun self-insert-command ()
  (insert (current-buffer *editor*) *this-character*))

(defun quoted-insert-command ()
  (insert (current-buffer *editor*) (editor-read-char)))

(defun delete-forward-char-command ()
  (delete-char (current-buffer *editor*)))

(defun delete-backward-char-command ()
  (delete-char (current-buffer *editor*) -1))

(defun kill-line-command ()
  (kill-line (current-buffer *editor*)))

(defun kill-region-command ()
  (let ((buffer (current-buffer *editor*)))
    (kill-region buffer (buffer-point buffer) (buffer-mark buffer))))

(defun kill-sexp-command ()
  (let* ((buffer (current-buffer *editor*))
         (point (buffer-point buffer)))
    (with-mark (current point)
      (move-sexp buffer 1)
      (kill-region buffer current point))))

(defun forward-kill-word-command ()
  (let* ((buffer (current-buffer *editor*))
         (point (buffer-point buffer)))
    (with-mark (current point)
      (move-word buffer 1)
      (kill-region buffer current point))))

(defun backward-kill-word-command ()
  (let* ((buffer (current-buffer *editor*))
         (point (buffer-point buffer)))
    (with-mark (current point)
      (move-word buffer -1)
      (kill-region buffer current point))))

(defun yank-command ()
  (yank-region (current-buffer *editor*)))

;;; Display commands.

(defun recenter-command ()
  (recenter (current-buffer *editor*)))

(defun redraw-screen-command ()
  (redraw-screen))

(defun scroll-up-command ()
  ;; Find the display line at the bottom of the screen and recenter on that.
  (let ((current-screen (editor-current-screen *editor*))
        (point (buffer-point (current-buffer *editor*))))
    (dotimes (i (length current-screen))
      (let ((line (aref current-screen (- (length current-screen) i 1))))
        (when line
          (setf (mark-line point) (display-line-line line)
                (mark-charpos point) (display-line-start line))
          (recenter (current-buffer *editor*))
          (return))))))

(defun scroll-down-command ()
  ;; Recenter on the topmost display line.
  (let* ((current-screen (editor-current-screen *editor*))
         (line (aref current-screen 0))
         (point (buffer-point (current-buffer *editor*))))
    (setf (mark-line point) (display-line-line line)
          (mark-charpos point) (display-line-start line))
    (recenter (current-buffer *editor*))))

;;; Other commands.

(defun keyboard-quit-command ()
  (error "Keyboard quit."))

(defun find-file (path)
  (setf path (merge-pathnames path))
  (dolist (buffer (buffer-list *editor*))
    (when (equal (buffer-property buffer 'path) path)
      (switch-to-buffer buffer)
      (setf *default-pathname-defaults* (make-pathname :name nil :type nil :version :newest :defaults path))
      (return-from find-file)))
  (let ((buffer (make-instance 'buffer)))
    (with-open-file (s path :if-does-not-exist nil)
      (cond (s
             (loop
                (multiple-value-bind (line missing-newline-p)
                    (read-line s nil)
                  (when (not line)
                    (return))
                  (insert buffer line)
                  (when (not missing-newline-p)
                    (insert buffer #\Newline)))))
            (t (setf (buffer-property buffer 'new-file) t))))
    (push buffer (buffer-list *editor*))
    (setf (buffer-property buffer 'path) path)
    (rename-buffer buffer (file-namestring path))
    (move-beginning-of-buffer buffer)
    ;; Loading the file will set the modified flag.
    (setf (buffer-modified buffer) nil)
    (switch-to-buffer buffer)
    (setf *default-pathname-defaults* (make-pathname :name nil :type nil :version :newest :defaults path))))

(defun find-file-command ()
  (find-file (read-from-minibuffer (format nil "Find file (default ~S): " *default-pathname-defaults*))))

(defun save-buffer-command ()
  (let ((buffer (current-buffer *editor*)))
    (when (not (buffer-property buffer 'path))
      (let* ((path (read-from-minibuffer (format nil "Write file (default ~S): " *default-pathname-defaults*)))
             (filespec (merge-pathnames path)))
        (rename-buffer buffer (file-namestring filespec))
        (setf (buffer-property buffer 'path) filespec)))
    ;; Make a backup of the file before writing, if it already exists
    (when (and *save-backup-files*
               (probe-file (buffer-property buffer 'path)))
      (rename-file (buffer-property buffer 'path)
                   (pathname (concatenate 'string
                                          (namestring (buffer-property buffer 'path))
                                          "~"))))
    (with-open-file (s (buffer-property buffer 'path)
                       :direction :output
                       :if-exists :new-version
                       :if-does-not-exist :create)
      (do ((line (first-line buffer) (next-line line)))
          ((not line))
        (write-sequence (data line) s)
        (when (next-line line)
          (terpri s))))
    (setf (buffer-property buffer 'new-file) nil
          (buffer-modified buffer) nil)))

(defun write-file-command ()
  (let* ((buffer (current-buffer *editor*))
         (*default-pathname-defaults* (or (buffer-property buffer 'path)
                                          *default-pathname-defaults*))
         (path (read-from-minibuffer (format nil "Write file (default ~S): " *default-pathname-defaults*)))
         (filespec (merge-pathnames path)))
    (rename-buffer buffer (file-namestring filespec))
    (setf (buffer-property buffer 'path) filespec)
    (with-open-file (s (buffer-property buffer 'path)
                       :direction :output
                       :if-exists :new-version
                       :if-does-not-exist :create)
      (do ((line (first-line buffer) (next-line line)))
          ((not line))
        (write-sequence (data line) s)
        (terpri s)))
    (setf (buffer-property buffer 'new-file) nil
          (buffer-modified buffer) nil)))

(defun list-buffers-command ()
  (let ((buffer (get-buffer-create "*Buffers*")))
    (switch-to-buffer buffer)
    ;; Clear the whole buffer.
    (delete-region buffer
                   (make-mark (first-line buffer) 0)
                   (make-mark (last-line buffer) (line-length (last-line buffer))))
    (dolist (b (buffer-list *editor*))
      (insert buffer (buffer-property b 'name))
      (insert buffer #\Newline))
    (setf (buffer-modified buffer) nil)))

(defun switch-to-buffer-command ()
  (let* ((default-buffer (or (last-buffer *editor*)
                             (current-buffer *editor*)))
         (name (string-trim " " (read-from-minibuffer (format nil "Buffer (default ~A): " (buffer-property default-buffer 'name)))))
         (other-buffer (if (zerop (length name))
                           default-buffer
                           (get-buffer-create name))))
    (when (not (eql (current-buffer *editor*) other-buffer))
      (setf (last-buffer *editor*) (current-buffer *editor*))
      (switch-to-buffer other-buffer))))

(defun kill-buffer-command ()
  (let* ((name (read-from-minibuffer (format nil "Buffer (default ~A): " (buffer-property (current-buffer *editor*) 'name))))
         (buffer (if (zerop (length name))
                     (current-buffer *editor*)
                     (or (get-buffer name)
                         (error "No buffer named ~S" name)))))
    (when (buffer-modified buffer)
      (when (not (minibuffer-yes-or-no-p "Buffer ~S modified, kill anyway?" (buffer-property buffer 'name)))
        (return-from kill-buffer-command)))
    (kill-buffer buffer)))

(defun get-buffer-create (name)
  (setf name (string name))
  (or (get-buffer name)
      (let ((buffer (make-instance 'buffer)))
        (setf (buffer-property buffer 'name) name)
        (push buffer (buffer-list *editor*))
        buffer)))

(defun get-buffer (name)
  (dolist (b (buffer-list *editor*))
    (when (string-equal (buffer-property b 'name) name)
      (return b))))

(defun kill-buffer (buffer)
  (setf (buffer-list *editor*) (remove buffer (buffer-list *editor*)))
  (when (eql buffer (last-buffer *editor*))
    (setf (last-buffer *editor*) nil))
  (when (eql buffer (current-buffer *editor*))
    (switch-to-buffer
     (if (buffer-list *editor*)
         (first (buffer-list *editor*))
         (get-buffer-create "*Scratch*")))))

(defun unique-name (name &optional version)
  (let ((actual-name (if version
                         (format nil "~A <~D>" name version)
                         name)))
    (if (get-buffer actual-name)
        (unique-name name (if version
                              (1+ version)
                              1))
        actual-name)))

(defun rename-buffer (buffer new-name)
  (unless (string-equal (buffer-property buffer 'name) new-name)
    (setf (buffer-property buffer 'name) (unique-name new-name))
    (refresh-title)))

;;; Lisp commands.

(defun beginning-of-top-level-form (buffer)
  "Move to the start of a top-level form.
A top-level form is designated by an open parenthesis at the start of a line."
  (let ((point (buffer-point buffer)))
    (setf (mark-charpos point) 0)
    (loop
       (when (eql (character-right-of point) #\()
         (return))
       (when (not (previous-line (mark-line point)))
         (error "Can't find start of top-level form."))
       (setf (mark-line point) (previous-line (mark-line point))))))

(defmacro save-excursion ((buffer) &body body)
  "Save the point & mark in buffer, execute body, then restore the saved point
and mark."
  `(call-with-save-excursion ,buffer (lambda () ,@body)))

(defun call-with-save-excursion (buffer fn)
  (let ((previous-point (copy-mark (buffer-point buffer) :right))
        (previous-mark (copy-mark (buffer-mark buffer) :left))
        (previous-mark-active (buffer-mark-active buffer)))
    (unwind-protect
         (funcall fn)
      (move-mark-to-mark (buffer-point buffer) previous-point)
      (move-mark-to-mark (buffer-mark buffer) previous-mark)
      (setf (buffer-mark-active buffer) previous-mark-active))))

(defun buffer-current-package (buffer)
  "From point, search backwards for a top-level IN-PACKAGE form.
If no such form is found, then return the CL-USER package."
  (let ((point (current-buffer *editor*))
        (temporary-package (make-package (gensym))))
    (import 'in-package temporary-package)
    (export 'in-package temporary-package)
    (unwind-protect
         (or (ignore-errors
               (save-excursion (buffer)
                  (let ((point (copy-mark (buffer-point buffer))))
                    (move-beginning-of-buffer buffer)
                    (let* ((str (buffer-string buffer (buffer-point buffer) point))
                           (pos (search (format nil "~A~A" #\( "in-package ") str :from-end t)))
                      (when (and pos (or (= 0 pos)
                                         (char= (char str (1- pos)) #\Newline)))
                        (let ((form (let ((*package* temporary-package)
                                          (*read-eval* nil))
                                      (ignore-errors
                                        (read-from-string (subseq str pos))))))
                          (when (and (listp form)
                                     (eql (first form) 'in-package)
                                     (= (list-length form) 2))
                            (return-from buffer-current-package (find-package (second form))))))))))
             (find-package :cl-user))
      (delete-package temporary-package))))

(defun eval-top-level-form-command ()
  (let ((buffer (current-buffer *editor*)))
    (save-excursion (buffer)
      (beginning-of-top-level-form buffer)
      (mark-to-point buffer (buffer-mark buffer))
      (move-sexp buffer 1)
      (let ((str (buffer-string buffer
                                (buffer-point buffer)
                                (buffer-mark buffer)))
            (package (buffer-current-package buffer)))
        (format t "Read ~S in package ~S~%" str package)
        (let ((form (let ((*package* package))
                      (read-from-string str))))
          (format t "Eval ~S~%" form)
          (eval form))))))

(defun beginning-of-top-level-form-command ()
  (beginning-of-top-level-form (current-buffer *editor*)))

;;;; End command wrappers.

(defun translate-command (editor character)
  "Translate a character to a command."
  (gethash character (global-key-map editor)))

(defun editor-loop ()
  (loop
     (let* ((*this-character* (editor-read-char))
            (*this-chord* (list *this-character*))
            (*this-command* (translate-command *editor* *this-character*)))
       (cond ((hash-table-p *this-command*)
              (loop
                 (setf *this-character* (editor-read-char)
                       *this-command* (gethash *this-character* *this-command*))
                 (push *this-character* *this-chord*)
                 (when (not (hash-table-p *this-command*))
                   (setf *this-chord* (reverse *this-chord*))
                   (cond (*this-command*
                          (funcall *this-command*)
                          (mapc 'funcall (post-command-hooks *editor*)))
                         (t (format t "Unknown command ~S~%" *this-chord*)))
                   (return))))
             (*this-command*
              (funcall *this-command*)
              (mapc 'funcall (post-command-hooks *editor*)))
             (t (format t "Unknown command ~S~%" *this-character*)))
       (setf *last-command* *this-command*
             *last-character* *this-character*
             *last-chord* *this-chord*)
       (setf (pending-redisplay *editor*) (not (redisplay))))))

(defun set-key (key fn map)
  (cond ((not (consp key))
         (set-key (list key) fn map))
        ((rest key)
         (let ((next (gethash (first key) map)))
           (when (not (hash-table-p next))
             (setf next (make-hash-table)
                   (gethash (first key) map) next))
           (set-key (rest key) fn next)))
        (t (setf (gethash (first key) map) fn))))

(defun initialize-key-map (key-map)
  (set-key #\Newline 'self-insert-command key-map)
  ;; ASCII printable characters.
  (loop for i from #x20 to #x7E
     do (set-key (code-char i) 'self-insert-command key-map))
  ;; Latin 1 printable characters.
  (loop for i from #xA0 to #xFF
     do (set-key (code-char i) 'self-insert-command key-map))
  (set-key #\C-F 'forward-char-command key-map)
  (set-key #\Right-Arrow 'forward-char-command key-map)
  (set-key #\C-B 'backward-char-command key-map)
  (set-key #\Left-Arrow 'backward-char-command key-map)
  (set-key #\C-N 'next-line-command key-map)
  (set-key #\Down-Arrow 'next-line-command key-map)
  (set-key #\C-P 'previous-line-command key-map)
  (set-key #\Up-Arrow 'previous-line-command key-map)
  (set-key #\M-F 'forward-word-command key-map)
  (set-key #\M-B 'backward-word-command key-map)
  (set-key #\C-M-F 'forward-sexp-command key-map)
  (set-key #\C-M-B 'backward-sexp-command key-map)
  (set-key #\C-A 'move-beginning-of-line-command key-map)
  (set-key #\C-E 'move-end-of-line-command key-map)
  (set-key #\C-K 'kill-line-command key-map)
  (set-key #\C-M-K 'kill-sexp-command key-map)
  (set-key #\C-Q 'quoted-insert-command key-map)
  (set-key #\C-L 'recenter-command key-map)
  (set-key #\M-L 'redraw-screen-command key-map)
  (set-key #\C-Space 'set-mark-command key-map)
  (set-key '(#\C-X #\C-X) 'exchange-point-and-mark-command key-map)
  (set-key #\Backspace 'delete-backward-char-command key-map)
  (set-key #\C-D 'delete-forward-char-command key-map)
  (set-key #\Delete 'delete-forward-char-command key-map)
  (set-key #\C-Backspace 'backward-kill-word-command key-map)
  (set-key #\M-D 'forward-kill-word-command key-map)
  (set-key #\C-W 'kill-region-command key-map)
  (set-key #\C-Y 'yank-command key-map)
  (set-key '(#\C-X #\C-F) 'find-file-command key-map)
  (set-key '(#\C-X #\C-S) 'save-buffer-command key-map)
  (set-key '(#\C-X #\C-W) 'write-file-command key-map)
  (set-key '(#\C-X #\k) 'kill-buffer-command key-map)
  (set-key '(#\C-X #\b) 'switch-to-buffer-command key-map)
  (set-key '(#\C-X #\C-B) 'list-buffers-command key-map)
  (set-key #\C-G 'keyboard-quit-command key-map)
  (set-key #\M-< 'move-beginning-of-buffer-command key-map)
  (set-key #\Home 'move-beginning-of-buffer-command key-map)
  (set-key #\M-> 'move-end-of-buffer-command key-map)
  (set-key #\End 'move-end-of-buffer-command key-map)
  (set-key #\C-V 'scroll-up-command key-map)
  (set-key #\Page-Down 'scroll-up-command key-map)
  (set-key #\M-V 'scroll-down-command key-map)
  (set-key #\Page-Up 'scroll-down-command key-map)
  (set-key '(#\C-C #\C-C) 'eval-top-level-form-command key-map)
  (set-key '(#\C-C #\C-A) 'beginning-of-top-level-form-command key-map))

(defun initialize-minibuffer-key-map (key-map)
  (initialize-key-map key-map)
  (set-key #\Newline 'minibuffer-finish-input-command key-map)
  (set-key '(#\C-X #\C-F) nil key-map)
  (set-key '(#\C-X #\C-S) nil key-map)
  (set-key '(#\C-X #\C-W) nil key-map)
  (set-key '(#\C-X #\k) nil key-map)
  (set-key '(#\C-X #\b) nil key-map)
  (set-key '(#\C-X #\C-B) nil key-map)
  (set-key #\C-C nil key-map))

(defun editor-main (width height initial-file)
  (mezzano.gui.font:with-font (font mezzano.gui.font:*default-monospace-font* mezzano.gui.font:*default-monospace-font-size*)
    (let ((fifo (mezzano.supervisor:make-fifo 50)))
      (mezzano.gui.compositor:with-window (window fifo (or width 640) (or height 700) :kind :editor)
        (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
               (frame (make-instance 'mezzano.gui.widgets:frame
                                     :framebuffer framebuffer
                                     :title "Editor"
                                     :close-button-p t
                                     :damage-function (mezzano.gui.widgets:default-damage-function window)))
               (*editor* (make-instance 'editor
                                        :fifo fifo
                                        :font font
                                        :window window
                                        :frame frame
                                        :buffer (make-instance 'buffer)))
               (*last-command* nil)
               (*last-character* nil)
               (*last-chord* nil)
               (*minibuffer* (make-instance 'buffer))
               (*minibuffer-key-map* (make-hash-table))
               (*default-pathname-defaults* *default-pathname-defaults*))
          (initialize-key-map (global-key-map *editor*))
          (initialize-minibuffer-key-map *minibuffer-key-map*)
          (mezzano.gui.widgets:draw-frame frame)
          (multiple-value-bind (left right top bottom)
              (mezzano.gui.widgets:frame-size (frame *editor*))
            (mezzano.gui:bitset (- (mezzano.gui.compositor:height window) top bottom)
                                (- (mezzano.gui.compositor:width window) left right)
                                (background-colour *editor*)
                                framebuffer
                                top left)
            (mezzano.gui.compositor:damage-window window
                                                  left top
                                                  (- (mezzano.gui.compositor:width window) left right)
                                                  (- (mezzano.gui.compositor:height window) top bottom)))
          (switch-to-buffer (get-buffer-create "*Scratch*"))
          (ignore-errors
            (when initial-file
              (find-file initial-file)))
          (catch 'quit
            (loop
               (handler-case
                   (editor-loop)
                 (error (c)
                   (ignore-errors
                     (format t "Editor error: ~A~%" c)
                     (setf (pending-redisplay *editor*) t)))))))))))

(defun spawn (&key width height initial-file)
  (mezzano.supervisor:make-thread (lambda () (editor-main width height initial-file))
                                  :name "Editor"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Editor console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
