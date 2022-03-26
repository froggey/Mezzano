(in-package :med)

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

(defun copy-region-command ()
  (let ((buffer (current-buffer *editor*)))
    (copy-region buffer (buffer-point buffer) (buffer-mark buffer))))

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

;;; Other commands.

(defun keyboard-quit-command ()
  (error "Keyboard quit."))

;;; Lisp commands.

(defun newline-command ()
  (insert (current-buffer *editor*) #\Newline))

(defun open-line-command ()
  (let ((buffer (current-buffer *editor*)))
    (move-end-of-line buffer)
    (newline-command)))

(defun execute-extended-command ()
  (let ((command (concatenate 'string (read-from-minibuffer "M-x ") "-command")))
    (format t "Executing extended command: ~A~%" command)
    (funcall (let ((*package* (find-package :med)))
               (read-from-string command)))))

(defun new-frame-command ()
  (spawn))

(defun repl-command ()
  (start-repl))

(defun grep-command ()
  (grep))

(defun cd-command ()
  (let* ((buffer (current-buffer *editor*))
         (dir (read-from-minibuffer "Directory: " 
                                    :default (namestring 
                                               (buffer-property buffer 
                                                               'default-pathname-defaults)))))
    (setf (buffer-property buffer 'default-pathname-defaults) (pathname dir))))

(defun compile-buffer-command ()
  (save-buffer-command)
  (mezzano.supervisor::make-thread
    (lambda () (cal (buffer-property (current-buffer *editor*) 'path)))
    :name "compile-file"
    :initial-bindings `((*editor* ,*editor*) 
                        (*standard-output* ,*standard-output*))))