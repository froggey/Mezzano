(in-package :med)

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
  (set-key #\M-X 'execute-extended-command key-map)      
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
  (set-key '(#\C-X #\s) 'save-some-buffers-command key-map)
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
  (set-key '(#\C-C #\C-A) 'beginning-of-top-level-form-command key-map)
  (set-key #\C-S 'isearch-command key-map)
  (set-key #\M-W 'copy-region-command key-map)
  (set-key #\C-M 'newline-command key-map)
  (set-key #\C-J 'newline-command key-map)
  (set-key #\C-O 'open-line-command key-map)
  (set-key #\M-Backspace 'backward-kill-word-command key-map)
  (set-key #\M-Colon 'eval-expression-command key-map)
  (set-key '(#\C-C #\C-K) 'compile-buffer-command key-map)
  (set-key '(#\C-X #\C-E) 'eval-last-sexp-command key-map)
  (set-key #\M-O 'find-matching-paren-command key-map)
  (set-key #\M-FULL_STOP 'find-definition-command key-map))


