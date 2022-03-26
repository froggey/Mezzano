(in-package :med)

(defun find-file (path)
  (setf path (merge-pathnames path))
  (dolist (buffer (buffer-list))
    (when (equal (buffer-property buffer 'path) path)
      (setf (last-buffer *editor*) (current-buffer *editor*))
      (switch-to-buffer buffer)
      (setf (buffer-property buffer 'default-pathname-defaults)
            (make-pathname :name nil :type nil :version :newest :defaults path))
      (return-from find-file buffer)))
  (let ((buffer (make-instance 'buffer)))
    (if (pathname-name path)
      ;; read file
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
              (t (setf (buffer-property buffer 'new-file) t)))
         (rename-buffer buffer (file-namestring path)))
      ;; read directory
      (progn
        (insert buffer (format nil "Directory: ~A~%~%" path))
        (mapc (lambda (file)
                (let* ((file-name (file-namestring file))
                       (name (if file-name file-name (directory-namestring file))))
                  (insert buffer name)
                  (insert buffer #\Newline)))
          (directory (merge-pathnames "*.*" path)))
        (setf (buffer-property buffer 'new-file) t)
        (rename-buffer buffer (directory-namestring path))))
    (push buffer (buffer-list))
    (setf (buffer-property buffer 'path) path)
    (move-beginning-of-buffer buffer)
    ;; Loading the file will set the modified flag.
    (setf (last-buffer *editor*) (current-buffer *editor*))
    (setf (buffer-modified buffer) nil)
    (switch-to-buffer buffer)
    (setf (buffer-property buffer 'default-pathname-defaults)
          (make-pathname :name nil :type nil :version :newest :defaults path))
    buffer))

;; FIME: should be regexes and use ppcre to search the 
;;       list rather then just strings and search
(defvar *file-completion-ignore-filetype-list* '(".llf" "~"))

(defun any (&rest args)
  (dolist (a args)
    (when a (return-from any t))))

(defun file-completer (text)
  (let (results)
    (dolist (path (directory (merge-pathnames "*.*" (pathname text))))
      (let ((file (namestring path)))
        (when (and (search text file)
                   (not (apply #'any
                                (mapcar (lambda (ignore) (search ignore file))
                                        *file-completion-ignore-filetype-list*))))
          (push file results))))
  results))

(defun find-file-command ()
  (find-file (read-from-minibuffer "Find file: " 
                                   :default (namestring 
                                              (or (buffer-property (current-buffer *editor*)                                                                  'default-pathname-defaults)
                                                   *default-pathname-defaults*))
                                   :completer #'file-completer)))

;; TODO: factor out the buffer saving from the below 3 functions into defun save-buffer

(defun save-buffer-command ()
  (let ((buffer (current-buffer *editor*)))
    (when (not (buffer-property buffer 'path))
      (let* ((path (read-from-minibuffer (format nil "Write file (default ~S): " 
                                                 :default (buffer-property buffer 'default-pathname-defaults))))
             (filespec (merge-pathnames path)))
        (rename-buffer buffer (file-namestring filespec))
        (setf (buffer-property buffer 'path) filespec)))
    (with-open-file (s (buffer-property buffer 'path)
                       :direction :output
                       :if-exists :new-version
                       :if-does-not-exist :create)
      (do ((line (first-line buffer) (next-line line)))
          ((not line))
        (write-sequence (map 'string #'car (data line)) s)
        (when (next-line line)
          (terpri s))))
    (setf (buffer-property buffer 'new-file) nil
          (buffer-modified buffer) nil)
    (format t "Wrote ~S~%" (buffer-property buffer 'path))))

(defun save-some-buffers-command ()
  (dolist (buffer (buffer-list))
    (when (and (buffer-modified buffer)
               (minibuffer-y-or-n-p 
                 (format nil "Save buffer ~A?" (buffer-property buffer 'name)))
               (buffer-property buffer 'path))
      (with-open-file (s (buffer-property buffer 'path)
                         :direction :output
                         :if-exists :new-version
                         :if-does-not-exist :create)
        (do ((line (first-line buffer) (next-line line)))
            ((not line))
          (write-sequence (map 'string #'car (data line)) s)
          (when (next-line line)
            (terpri s))))
        (setf (buffer-property buffer 'new-file) nil
              (buffer-modified buffer) nil)
        (format t "Wrote ~S~%" (buffer-property buffer 'path)))))

(defun write-file-command ()
  (let* ((buffer (current-buffer *editor*))
         (*default-pathname-defaults* (or (buffer-property buffer 'path)
                                          (buffer-property buffer 'default-pathname-defaults)
                                          *default-pathname-defaults*))
         (path (read-from-minibuffer "Write file: " 
                                     :default (namestring *default-pathname-defaults*)))
         (filespec (merge-pathnames path)))
    (rename-buffer buffer (file-namestring filespec))
    (setf (buffer-property buffer 'path) filespec)
    (with-open-file (s (buffer-property buffer 'path)
                       :direction :output
                       :if-exists :new-version
                       :if-does-not-exist :create)
      (do ((line (first-line buffer) (next-line line)))
          ((not line))
        (write-sequence (map 'string #'car (data line)) s)
        (terpri s)))
    (setf (buffer-property buffer 'new-file) nil
          (buffer-modified buffer) nil)
    (format t "Wrote ~S~%" (buffer-property buffer 'path))))

