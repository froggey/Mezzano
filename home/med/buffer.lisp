(in-package :med)

;;; Buffers.

(defclass buffer ()
  ((%first-line :accessor first-line)
   (%last-line :accessor last-line)
   (%point :reader buffer-point)
   (%mark :reader buffer-mark)
   (%mark-active :initarg :mark-active :accessor buffer-mark-active)
   (%key-map :initarg :key-map :accessor buffer-key-map)
   (%pre-command-hooks :initarg :pre-command-hooks :accessor buffer-pre-command-hooks)
   (%post-command-hooks :initarg :post-command-hooks :accessor buffer-post-command-hooks)
   (%lock :initarg :lock :reader buffer-lock)
   (%properties))
  (:default-initargs
     :mark-active nil
     :key-map (make-hash-table)
     :pre-command-hooks '()
     :post-command-hooks '()
     :lock (mezzano.supervisor::make-mutex "Buffer") ; TODO: Buffer Name Lock
   ))

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

(defmethod print-object ((object buffer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (buffer-property object 'name))))

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
                                                    :element-type 'cons
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
           (vector-push-extend (list character) (data current-line)))
          (t ;; Inserting in the middle or at the start.
           ;; Make sure the vector is long enough.
           (vector-push-extend (list character) (data current-line))
           (replace (data current-line) (data current-line)
                    :start1 (1+ current-charpos)
                    :start2 current-charpos)
           (setf (aref (data current-line) current-charpos) (list character))))
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
  (mezzano.supervisor::with-mutex ((buffer-lock buffer))
    (loop for ch across (string string)
       if (char= ch #\Newline)
       do (insert-line (buffer-point buffer))
       else do (insert-char (buffer-point buffer) ch))))

(defun order-marks (mark-1 mark-2)
  (assert (eql (line-buffer (mark-line mark-1))
               (line-buffer (mark-line mark-2))))
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
               ((mark>= m1 m2)) ; make sure we terminate
             (if (end-of-line-p m1)
                 (insert-line point)
                 (insert-char point (line-character (mark-line m1) (mark-charpos m1))))
             (move-mark m1))))))

(defun insert-region (buffer mark-1 mark-2)
  (insert-region-at-mark (buffer-point buffer)
                         mark-1 mark-2))

(defun yank-region (buffer)
  (when (killed-region)
    (insert-region buffer (car (killed-region)) (cdr (killed-region)))))

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
                                  :element-type 'cons
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
                                  :element-type 'cons
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
    (cond ((and (killed-region)
                (eql *last-command* 'kill-region))
           ;; Append to killed region.
           (insert-region-at-mark (cdr (killed-region))
                                  first-mark last-mark))
          (t ;; New killed region.
           (setf (killed-region) (cons first-mark last-mark))))))

(defun copy-region (buffer mark-1 mark-2)
   (declare (ignore buffer))
   (setf (killed-region) (cons mark-1 mark-2)))

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
        (unwind-protect
          (point-to-mark buffer here)
          t))))
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
  (let ((string (make-array 0 :element-type 'character :fill-pointer t :adjustable t)))
    (do ((m1 (copy-mark mark-1))
         (m2 (copy-mark mark-2)))
        ((mark= m1 m2))
      (if (end-of-line-p m1)
          (vector-push-extend #\Newline string)
          (vector-push-extend (line-character (mark-line m1) (mark-charpos m1)) string))
      (move-mark m1))
    string))
