(in-package :med)

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
                     (mark-charpos point) (min (buffer-property buffer 'column-hint 0)
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

(defun nth-character-left-of (mark nth)
  (let ((buffer (line-buffer (mark-line mark))))
    (save-excursion (buffer)
      (dotimes (i (1- nth))
        (move-mark mark -1))
      (character-left-of mark))))

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
             (cond
               ((eql (mezzano.internals::readtable-syntax-type ch nil) :whitespace) t)
               ((eql ch #\SEMICOLON) (scan-forward mark (lambda (c) (eql c #\Newline)))
                                     t))))
      ;; Skip past any leading whitespace.
      (scan-forward mark (complement #'whitespacep))
      (loop
         (let* ((ch (character-right-of mark))
                (chl (character-left-of mark))
                (chl2 (when (eql chl #\\) (nth-character-left-of mark 2))))
           (when (not ch)
             (return nil))
           (when (and (whitespacep ch) (not pair-stack))
             (return t))
           (unless (and (eql chl #\\)
                        (eql chl2 #\#))
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
           (push #\) pair-stack))))
           (move-mark mark 1))
         (setf first-char nil)))))

(defun scan-sexp-backward (mark)
  (let ((pair-stack '())
        (first-char t))
    (flet ((whitespacep (ch)
             (eql (mezzano.internals::readtable-syntax-type ch nil) :whitespace)))
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
                             (mezzano.gui.font:glyph-advance
                                (mezzano.gui.font:character-to-glyph
                                  (font *editor*) #\M))))))
    (with-mark (mark point :left)
      (dotimes (i (* (window-rows) 2))
        (dotimes (j width)
          (insert buffer (code-char (+ #x20 i))))
        (insert buffer #\Newline))
      (point-to-mark buffer mark))))
