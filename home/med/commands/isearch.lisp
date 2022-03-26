(in-package :med)

(defun search-forward (buffer string)
  "From point, search forwards for string in buffer."
  (with-mark (point (buffer-point buffer))
    ;; Search to the end of the buffer
    (save-excursion (buffer)
       (move-end-of-buffer buffer)
        (setf pos (search string (buffer-string buffer point
                                                (buffer-point buffer)))))
    (if pos
       ;; Found the string, go there
       (move-char buffer (+ pos (length string)))
       ;; Didn't find it, wrap around and search from the beginning
       (progn
         (save-excursion (buffer)
           (move-beginning-of-buffer buffer)
           (setf pos (search string (buffer-string buffer (buffer-point buffer) point))))
         (when pos
           (move-beginning-of-buffer buffer)
           (move-char buffer (+ pos (length string))))))))

(defun cancel-isearch ()
  (format t "~%Cancelling isearch.~%")
  (let ((buffer (current-buffer *editor*)))
    (setf (buffer-pre-command-hooks buffer)
          (remove 'isearch-pre-command-hook (buffer-pre-command-hooks buffer)))
    (setf (buffer-post-command-hooks buffer)
          (remove 'isearch-post-command-hook (buffer-post-command-hooks buffer)))))

(defun isearch-pre-command-hook ()
  (unless (or (eq *this-command* 'self-insert-command)
              (eq *this-command* 'isearch-command))
    (cancel-isearch)))
  
(defun isearch-post-command-hook ()
  (flet ((char-at-point (point)
           (line-character (mark-line point) (mark-charpos point))))
    (let* ((buffer (current-buffer *editor*))
           (point (buffer-point buffer)))
      (if (eql *this-command* 'self-insert-command)
        (progn
          (delete-backward-char-command)
          (insert *messages* *this-character*)
          (force-redisplay)
          (setf (buffer-modified buffer) (buffer-property buffer 'isearch-buffer-modified))
          (if (= 0 (length *isearch-string*))           
            (progn
              (scan-forward point (lambda  (c) (char= c *this-character*)))
              (let ((char-at-point (char-at-point point)))
                (when (char= *this-character* char-at-point)
                  (vector-push-extend *this-character* *isearch-string*))))
            (let ((char-at-point (char-at-point point))
                  (next-char (progn (move-mark point 1)
                                    (character-right-of point)))) ;; FIXME: Hebrew
              (vector-push-extend *this-character* *isearch-string*)
              (unless (char= *this-character* char-at-point)
                (move-mark point -1)
                (search-forward buffer *isearch-string*)))))
        (if (null *isearch-string*)
          (setf *isearch-string* (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
          (if (= 0 (length *isearch-string*))
            (search-forward buffer *last-isearch-string*) 
            (search-forward buffer *isearch-string*)))))))

(defun isearch-command ()
  (let ((buffer (current-buffer *editor*)))
    (unless (member 'isearch-post-command-hook (buffer-post-command-hooks buffer))
      (if (< 0 (length *isearch-string*))
        (setf *last-isearch-string* *isearch-string*))
      (format t "Isearch (Default: ~S): " (coerce *last-isearch-string* 'string))
      (setf *isearch-string* nil)
      (push 'isearch-pre-command-hook (buffer-pre-command-hooks buffer))
      (push 'isearch-post-command-hook (buffer-post-command-hooks buffer))
      (setf (buffer-property buffer 'isearch-buffer-modified) (buffer-modified buffer)))))
