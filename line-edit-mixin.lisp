;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.line-editor
  (:use :cl)
  (:export #:line-edit-mixin
           #:*line-editor-command-table*
	   #:global-set-key
           #:define-command
           #:buffer
           #:cursor-position
           #:last-command
           #:compute-completions
           #:history-table
           #:history-position
           ;; History table protocol.
           #:history-reset
           #:history-newest
           #:history-oldest
           #:history-next
           #:history-previous
           #:history-data
           #:history-add))

(in-package :mezzano.line-editor)

(defgeneric history-reset (history))
(defgeneric history-newest (history))
(defgeneric history-oldest (history))
(defgeneric history-next (history entry))
(defgeneric history-previous (history entry))
(defgeneric history-data (history entry))
(defgeneric history-add (history data))

(defclass history-table ()
  ((%lock :initform (mezzano.supervisor:make-mutex "History table lock") :reader lock)
   (%history-data)))

(defmethod initialize-instance :after ((instance history-table) &key &allow-other-keys)
  (history-reset instance))

(defmethod history-reset ((history history-table))
  (mezzano.supervisor:with-mutex ((lock history))
    (setf (slot-value history '%history-data) (make-array 0 :fill-pointer 0 :adjustable t))))

(defmethod history-newest ((history history-table))
  (mezzano.supervisor:with-mutex ((lock history))
    (when (not (zerop (length (slot-value history '%history-data))))
      (1- (length (slot-value history '%history-data))))))

(defmethod history-oldest ((history history-table))
  (mezzano.supervisor:with-mutex ((lock history))
    (when (not (zerop (length (slot-value history '%history-data))))
      0)))

(defmethod history-previous ((history history-table) entry)
  (mezzano.supervisor:with-mutex ((lock history))
    (when (not (zerop entry))
      (1- entry))))

(defmethod history-next ((history history-table) entry)
  (mezzano.supervisor:with-mutex ((lock history))
    (when (not (eql entry (1- (length (slot-value history '%history-data)))))
      (1+ entry))))

(defmethod history-data ((history history-table) entry)
  (check-type entry (integer 0))
  (or (mezzano.supervisor:with-mutex ((lock history))
        (when (< entry (length (slot-value history '%history-data)))
          (aref (slot-value history '%history-data) entry)))
      (error "Invalid history entry ~D." entry)))

(defmethod history-add ((history history-table) data)
  (mezzano.supervisor:with-mutex ((lock history))
    (cond ((or (zerop (length (slot-value history '%history-data)))
               (not (equal (aref (slot-value history '%history-data)
                                 (1- (length (slot-value history '%history-data))))
                           data)))
           (vector-push-extend data (slot-value history '%history-data)))
          (t (1- (length (slot-value history '%history-data)))))))

(defvar *line-editor-history* (make-instance 'history-table))

(defclass line-edit-mixin ()
  ((%line-buffer :initform nil :accessor buffer)
   (%cursor-position :accessor cursor-position)
   (%output-progress :initform nil :accessor output-progress)
   (%line-start-position :accessor line-start-position)
   (%line-end-position :accessor line-end-position)
   (%history-table :initarg :history-table :reader history-table)
   (%history-position :initarg :history-position :accessor history-position)
   (%history-search-fragment :initarg :history-search-fragment :accessor history-search-fragment)
   (%last-command :initarg :last-command :accessor last-command)
   (%completions :initarg :completions :accessor completions)
   (%current-completion :initarg :current-completion :accessor current-completion)
   (%current-completion-start :initarg :current-completion :accessor current-completion-start)
   (%current-completion-end :initarg :current-completion :accessor current-completion-end))
  (:default-initargs :history-table *line-editor-history*
                     :history-position nil
                     :history-search-fragment nil
                     :last-command nil
                     :current-completion nil))

(defvar *line-editor-command-table* (make-hash-table))

(defun global-set-key (keys command)
  "Create keyboard shortcut to any command."
  (when (not (listp keys))
    (setf keys (list keys)))
  (loop for key in keys
     collect (setf (gethash key *line-editor-command-table*) command)))

(defmacro define-command (name (stream keys) &body body)
  "Define command and create keyboard shortcut."
  `(progn
     (defun ,name (,stream)
       ,@body)
     (global-set-key ',keys ',name)))

(define-command forward-char (stream (#\C-F #\Right-Arrow))
  "Move forward one character."
  (unless (eql (cursor-position stream) (length (buffer stream)))
    (incf (cursor-position stream))))

(define-command backward-char (stream (#\C-B #\Left-Arrow))
  "Move backward one character."
  (unless (zerop (cursor-position stream))
    (decf (cursor-position stream))))

(defun next-word (stream position)
  "Starting from POSITION, return the end of the next word."
  ;; Move past non-alphanumeric characters.
  (loop
     (when (or (eql position (length (buffer stream)))
               (alphanumericp (char (buffer stream) position)))
       (return))
     (incf position))
  ;; Now past alphanumeric characters.
  (loop
     (when (or (eql position (length (buffer stream)))
               (not (alphanumericp (char (buffer stream) position))))
       (return))
     (incf position))
  position)

(defun previous-word (stream position)
  "Starting from POSITION, return the start of the previous word."
  ;; Move past non-alphanumeric characters.
  (loop
     (when (or (zerop position)
               (alphanumericp (char (buffer stream) (1- position))))
       (return))
     (decf position))
  ;; Now past alphanumeric characters.
  (loop
     (when (or (zerop position)
               (not (alphanumericp (char (buffer stream) (1- position)))))
       (return))
     (decf position))
  position)

(define-command forward-word (stream (#\M-F #\M-Right-Arrow))
  "Move forward one word."
  (setf (cursor-position stream) (next-word stream (cursor-position stream))))

(define-command backward-word (stream (#\M-B #\M-Left-Arrow))
  "Move backward one word."
  (setf (cursor-position stream) (previous-word stream (cursor-position stream))))

(define-command move-end-of-line (stream (#\C-E #\End))
  "Move to end of line."
  (setf (cursor-position stream) (length (buffer stream))))

(define-command move-beginning-of-line (stream (#\C-A #\Home))
  "Move the start of line."
  (setf (cursor-position stream) 0))

(defun set-from-history (stream)
  (let ((data (history-data (history-table stream) (history-position stream))))
    (setf (buffer stream) (make-array (length data)
                                      :element-type 'character
                                      :initial-contents data
                                      :fill-pointer t
                                      :adjustable t)
          (cursor-position stream) (length data))))

(defun history-search (history-table position direction-function term)
  (loop
     (let ((next (case position
                   (:newest (history-newest history-table))
                   (:oldest (history-oldest history-table))
                   (t (funcall direction-function history-table position)))))
       (when (not next)
         (return nil))
       (let ((data (history-data history-table next)))
         (when (and (<= (length term) (length data))
                    (string= term data :end2 (length term)))
           (return (values next data))))
       (setf position next))))

(defun history-search-in-progress (stream)
  (member (last-command stream) '(previous-history next-history)))

(defun current-history-pattern (stream)
  (cond ((history-search-in-progress stream)
         (history-search-fragment stream))
        ((zerop (length (buffer stream)))
         nil)
        (t (copy-seq (buffer stream)))))

(defun move-history (stream direction pattern)
  (setf (history-search-fragment stream) (or pattern ""))
  (let ((pos (history-search (history-table stream)
                             (or (when (history-search-in-progress stream)
                                   (history-position stream))
                                 (ecase direction
                                   (:forward :oldest)
                                   (:backward :newest)))
                             (ecase direction
                               (:forward #'history-next)
                               (:backward #'history-previous))
                             (history-search-fragment stream))))
    (when pos
      (let ((data (history-data (history-table stream) pos)))
        (setf (buffer stream) (make-array (length data)
                                          :element-type 'character
                                          :initial-contents data
                                          :fill-pointer t
                                          :adjustable t)
              (cursor-position stream) (length data))))
    (setf (history-position stream) pos)))

(define-command previous-history (stream (#\M-P #\Up-Arrow))
  (move-history stream :backward (current-history-pattern stream)))

(define-command next-history (stream (#\M-N #\Down-Arrow))
  (move-history stream :forward (current-history-pattern stream)))

(define-command forward-delete-char (stream (#\Delete #\C-D))
  "Delete one character forward."
  (unless (eql (cursor-position stream) (length (buffer stream)))
    (replace (buffer stream) (buffer stream)
             :start1 (cursor-position stream)
             :start2 (1+ (cursor-position stream)))
    (decf (fill-pointer (buffer stream)))))

(define-command backward-delete-char (stream #\Backspace)
  "Delete one character backward."
  (unless (zerop (cursor-position stream))
    ;; Shuffle characters backward.
    (decf (cursor-position stream))
    (replace (buffer stream) (buffer stream)
             :start1 (cursor-position stream)
             :start2 (1+ (cursor-position stream)))
    (decf (fill-pointer (buffer stream)))))

(define-command forward-delete-word (stream #\M-D)
  "Delete one word forward."
  (let ((end (next-word stream (cursor-position stream))))
    (replace (buffer stream) (buffer stream)
             :start1 (cursor-position stream)
             :start2 end)
    (decf (fill-pointer (buffer stream)) (- end (cursor-position stream)))))

(define-command backward-delete-word (stream (#\C-Backspace #\C-W))
  "Delete one word backward."
  (let ((start (previous-word stream (cursor-position stream))))
    (replace (buffer stream) (buffer stream)
             :start1 start
             :start2 (cursor-position stream))
    (decf (fill-pointer (buffer stream)) (- (cursor-position stream) start))
    (setf (cursor-position stream) start)))

(define-command delete-to-end-of-line (stream #\C-K)
  "Delete characters from the cursor to the end of the line."
  (unless (eql (cursor-position stream) (length (buffer stream)))
    (setf (fill-pointer (buffer stream)) (cursor-position stream))))

(define-command break-into-debugger (stream #\C-C)
  "Enter the debugger using BREAK."
  (break))

(define-command abort-input (stream #\C-G)
  "Abort input, call the nearest ABORT restart."
  (when (find-restart 'abort)
    (setf (buffer stream) nil)
    (abort)))

;; Returns the start & end position of the string in buffer to be replaced with the completions
;; and a sequence of completions.
(defgeneric compute-completions (stream buffer cursor-position))

(defmethod compute-completions ((stream line-edit-mixin) buffer cursor-position)
  (declare (ignore stream buffer cursor-position))
  nil)

(define-command complete (stream #\Tab)
  (when (not (eql (last-command stream) 'complete))
    (setf (values (current-completion-start stream)
                  (current-completion-end stream)
                  (completions stream))
          (compute-completions stream (buffer stream) (cursor-position stream)))
    (when (or (not (current-completion-start stream))
              (zerop (length (completions stream))))
      (setf (completions stream) #("")
            (current-completion-start stream) (cursor-position stream)
            (current-completion-end stream) (cursor-position stream)))
    (setf (current-completion stream) -1))
  (setf (current-completion stream) (rem (1+ (current-completion stream))
                                         (length (completions stream))))
  ;; Do the buffer shuffle. Move anything past the end of the completable thing
  ;; up or down so there is exactly enough space for the next completion.
  (let* ((next-completion (elt (completions stream) (current-completion stream)))
         (chars-to-remove (- (current-completion-end stream)
                             (current-completion-start stream)))
         (buffer (buffer stream))
         (new-size (+ (- (length buffer) chars-to-remove) (length next-completion)))
         (new-end (+ (current-completion-start stream) (length next-completion))))
    ;; Only grow the buffer, never shrink.
    (when (< (length buffer) new-size)
      (adjust-array buffer new-size))
    (setf (fill-pointer buffer) (max (length buffer) new-size))
    (replace buffer buffer
             :start1 new-end
             :start2 (current-completion-end stream))
    (setf (fill-pointer buffer) new-size)
    (replace buffer next-completion
             :start1 (current-completion-start stream))
    (setf (current-completion-end stream) new-end)
    (setf (cursor-position stream) (current-completion-end stream))))

(defmethod sys.gray:stream-unread-char ((stream line-edit-mixin) character)
  (declare (ignore character))
  (decf (output-progress stream)))

(defun redraw-line (stream)
  (destructuring-bind (x y)
      (line-start-position stream)
    (sys.int::stream-clear-between stream x y (first (line-end-position stream)) (second (line-end-position stream)))
    (sys.int::stream-move-to stream x y)
    (dotimes (i (cursor-position stream))
      (write-char (char (buffer stream) i) stream))
    (multiple-value-bind (cx cy)
        (sys.int::stream-cursor-pos stream)
      (dotimes (i (- (length (buffer stream)) (cursor-position stream)))
        (write-char (char (buffer stream) (+ (cursor-position stream) i)) stream))
      (setf (line-end-position stream) (multiple-value-list (sys.int::stream-cursor-pos stream)))
      (sys.int::stream-move-to stream cx cy))))

(defmethod sys.gray:stream-read-char :around ((stream line-edit-mixin))
  (cond ((and (output-progress stream)
              (not (eql (output-progress stream) (length (buffer stream)))))
         (prog1
             (char (buffer stream) (output-progress stream))
           (incf (output-progress stream))))
        (t (when (or (not (buffer stream))
                     (eql (output-progress stream) (length (buffer stream))))
             ;; Starting or restarting a line read.
             (setf (output-progress stream) nil
                   (cursor-position stream) 0
                   (buffer stream) (make-array 50 :element-type 'character :fill-pointer 0 :adjustable t)
                   (line-start-position stream) (multiple-value-list (sys.int::stream-cursor-pos stream))
                   (line-end-position stream) (multiple-value-list (sys.int::stream-cursor-pos stream))
                   (history-position stream) nil))
           (loop
              (let* ((ch (call-next-method))
                     (fn (gethash ch *line-editor-command-table*)))
                (cond ((eql ch #\Newline) ;; Submit the line.
                       ;; ### Maybe turn this into a proper command.
                       (when (not (eql (cursor-position stream) (length (buffer stream))))
                         (setf (cursor-position stream) (length (buffer stream)))
                         (redraw-line stream))
                       (when (not (zerop (length (buffer stream))))
                         (history-add (history-table stream) (copy-seq (buffer stream))))
                       (vector-push-extend #\Newline (buffer stream))
                       (write-char #\Newline stream)
                       (setf (output-progress stream) 0)
                       (setf (last-command stream) #\Newline)
                       (return (sys.gray:stream-read-char stream)))
                      (fn
                       (funcall fn stream)
                       (setf (last-command stream) fn)
                       (redraw-line stream))
                      ((not (graphic-char-p ch))) ; ignore non-graphics characters.
                      ((eql (cursor-position stream) (length (buffer stream)))
                       ;; At end of line, don't need to redraw.
                       (vector-push-extend ch (buffer stream))
                       (write-char ch stream)
                       (incf (cursor-position stream))
                       (setf (line-end-position stream) (multiple-value-list (sys.int::stream-cursor-pos stream)))
                       (setf (last-command stream) ch))
                      (t ;; Writing into the middle of the line requires a redraw.
                       ;; Ensure space.
                       (vector-push-extend ch (buffer stream))
                       ;; Shuffle.
                       (replace (buffer stream) (buffer stream)
                                :start1 (1+ (cursor-position stream))
                                :start2 (cursor-position stream))
                       ;; Insert character.
                       (setf (char (buffer stream) (cursor-position stream)) ch)
                       ;; Advance cursor.
                       (incf (cursor-position stream))
                       ;; Redraw.
                       (redraw-line stream)
                       (setf (last-command stream) ch))))))))

(defmethod sys.gray:stream-clear-input :around ((stream line-edit-mixin))
  (when (output-progress stream)
    (setf (output-progress stream) (length (buffer stream)))))
