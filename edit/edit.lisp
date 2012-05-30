(defpackage #:edit
  (:use #:cl))

(in-package #:edit)

(declaim (special *buffer* *all-buffers*))

(defmacro with-saved-screen ((&optional framebuffer &rest options) &body body)
  (let ((fb-sym (or framebuffer (gensym))))
    `(%with-saved-screen (lambda (,fb-sym) (declare (ignorable ,fb-sym)) ,@body) ,@options)))

(defun framebuffer-from-stream (stream)
  (when (typep stream 'sys.int::shadow-stream)
    (setf stream (sys.int::shadow-stream-primary stream)))
  (when (typep stream 'sys.int::framebuffer-stream)
    (slot-value stream 'sys.int::framebuffer)))

(defun %with-saved-screen (fn)
  (let ((fb (framebuffer-from-stream *terminal-io*)))
    (if fb
        (let* ((dims (array-dimensions fb))
               (position (multiple-value-list (sys.int::stream-cursor-pos *terminal-io*)))
               (back-buffer (make-array (array-dimensions fb)
                                        :element-type (array-element-type fb))))
          (sys.int::%bitblt (first dims) (second dims)
                            fb 0 0
                            back-buffer 0 0)
          (unwind-protect
               (funcall fn fb)
            (apply 'sys.int::stream-move-to *terminal-io* position)
            (sys.int::%bitblt (first dims) (second dims)
                              back-buffer 0 0
                              fb 0 0)))
        (funcall fn nil))))

(define-condition exit-editor () ())
(define-condition simple-editor-error (simple-error) ())
(define-condition minibuffer-complete () ())

(defvar *minibuffer* nil)
(defvar *delayed-minibuffer-clear* nil)
(defvar *dispatch-table* (make-hash-table))
(defvar *last-key-pressed* nil)
(defvar *last-command* nil)
(defvar *input-stream* nil)
(defvar *main-screen* nil)
(defvar *main-screen-buffer* nil)

(defun global-key (character)
  (gethash character *dispatch-table*))

(defun (setf global-key) (value character)
  (setf (gethash character *dispatch-table*) value))

(defun buffer-key (buffer character)
  (gethash character (buffer-keymap buffer)))

(defun (setf buffer-key) (value buffer character)
  (setf (gethash character (buffer-keymap buffer)) value))

(defun self-insert-command ()
  (insert-char *last-key-pressed*))

(defun quit ()
  (signal 'exit-editor))

(defun exit-minibuffer ()
  (signal 'minibuffer-complete))

(defun draw-cursor (fb x y &optional (active t))
  (dotimes (i 16)
    (setf (aref fb (+ y i) x) (if active
                                  (logior #xFF000000
                                          (ldb (byte 24 0) (lognot (aref fb (+ y i) x))))
                                  #xFFFF0000)))) ; red

(defmacro with-current-buffer (buffer-or-name &body body)
  `(let ((*buffer* (get-buffer ,buffer-or-name)))
     ,@body))

(defun display-buffer (buffer fb x y w h)
  (sys.int::%bitset h w #xFF000000 fb y x)
  (do ((lines-written 0)
       (line (buffer-first-line buffer)))
      ((or (>= lines-written h)
	   (null line)))
    (let ((contents (slot-value line 'contents))
          (next (slot-value line 'next)))
      (do ((i 0 (1+ i))
	   (ofs 0))
	  ((or (>= i (length contents))
	       (> lines-written h))
	   (when (with-current-buffer buffer
                   (and (eql (point-line) line)
                        (eql (point-character) i)))
	     (draw-cursor fb (+ x ofs) (+ y lines-written) (eql *buffer* buffer))))
	(let* ((ch (char contents i))
               (width (if (eql ch #\Space) 8 (sys.int::unifont-glyph-width ch))))
          (unless (>= (+ ofs width) w)
            (if (eql ch #\Space)
                (sys.int::%bitset 16 8 #xFF000000 fb (+ y lines-written) (+ x ofs))
                (sys.int::render-char-at ch fb (+ x ofs) (+ y lines-written))))
	  (when (with-current-buffer buffer
                  (and (eql (point-line) line)
                       (eql (point-character) i)))
            (draw-cursor fb (+ x ofs) (+ y lines-written) (eql *buffer* buffer)))
          (incf ofs width)))
      (incf lines-written 16)
      (setf line next))))

(defun redraw-screen ()
  (let* ((dims (array-dimensions *main-screen*))
         (width (second dims))
         (height (first dims)))
    (display-buffer *main-screen-buffer* *main-screen*
                    0 0
                    width (- height 16 16 2))
    (display-buffer *minibuffer* *main-screen*
                    0 (- height 16)
                    width 16)
    (sys.int::%bitset 16 width #xFF000000 *main-screen* (- height 16 16 1) 0)
    (let ((name (buffer-name *main-screen-buffer*))
          (ofs 0))
      (dotimes (i (length name))
        (let* ((ch (char name i))
               (char-width (if (eql ch #\Space) 8 (sys.int::unifont-glyph-width ch))))
          (when (< (+ ofs char-width) width)
            (if (eql ch #\Space)
                (sys.int::%bitset 16 8 #xFF000000 *main-screen* (- height 16 16 1) ofs)
                (sys.int::render-char-at ch *main-screen* ofs (- height 16 16 1))))
          (incf ofs char-width))))
    (sys.int::%bitset 1 width #xFFD0D0D0 *main-screen* (- height 16 1) 0)
    (sys.int::%bitset 1 width #xFFD0D0D0 *main-screen* (- height 16 16 2) 0)))

(defun read-from-minibuffer (prompt-string &key read)
  (let ((*buffer* *minibuffer*)
        (bloop nil))
    (clear-buffer)
    (insert prompt-string)
    (setf bloop (point-character))
    (handler-case (editor-loop)
      (minibuffer-complete ()
        (let ((string (subseq (slot-value (buffer-first-line *minibuffer*) 'contents)
                              bloop)))
          (setf *delayed-minibuffer-clear* t)
          (if read
              (let ((*package* (find-package :edit)))
                (read-from-string string))
              string))))))

(defun switch-to-buffer (&optional (buffer (read-from-minibuffer "Buffer: ")))
  (setf buffer (get-buffer buffer))
  (set-buffer buffer)
  (setf *main-screen-buffer* buffer))

(defun open-file (&optional (path (read-from-minibuffer "Path: ")))
  (let* ((path (merge-pathnames path))
         (file-name (file-namestring path)))
    (unless file-name
      (error 'simple-editor-error :format-control "Cannot open directory ~A." path))
    (let ((buffer (get-buffer-create file-name)))
      (switch-to-buffer buffer)
      (clear-buffer)
      (setf (buffer-local 'pathname) path)
      (with-open-file (s path :if-does-not-exist nil)
        (if s
            (loop
               (multiple-value-bind (line at-eof)
                   (read-line s nil)
                 (when (null line)
                   (return))
                 (insert line)
                 (if at-eof
                     (return)
                     (newline))))
            (with-current-buffer *minibuffer*
              (clear-buffer)
              (insert "(new file)"))))
      (beginning-of-buffer))))

(defun save-file ()
  (let ((path (buffer-local 'pathname)))
    (unless path
      (error 'simple-editor-error :format-control "Buffer has no path associated."))
    (with-open-file (stream path :direction :output :if-exists :new-version)
      (do ((line (buffer-first-line *buffer*)
                 (slot-value line 'next)))
          ((null line))
        (write-string (slot-value line 'contents) stream)
        (when (slot-value line 'next)
          (terpri stream))))))

(defun extended-command ()
  (let ((form (read-from-minibuffer "Extended command: " :read t)))
    (if (symbolp form)
        (funcall form)
        (eval form))))

(defun list-buffers ()
  (let ((buffer (get-buffer-create "*Buffer List*")))
    (switch-to-buffer buffer)
    (clear-buffer)
    (maphash (lambda (name v)
               (declare (ignore v))
               (unless (char= (char name 0) #\Space)
                 (insert name)
                 (newline)))
             *all-buffers*)))

(defun kill-buffer ()
  (remhash (buffer-name *buffer*) *all-buffers*)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defvar *extended-command-table* (make-hash-table))

(dotimes (i 95)
  (setf (global-key (code-char (+ #x20 i))) 'self-insert-command))
(dotimes (i 96)
  (setf (global-key (code-char (+ #xA0 i))) 'self-insert-command))
;; Using NAME-CHAR instead of #\ syntax to prevent genesis from seeing fancy characters.
(setf (global-key #\Newline) 'newline
      (global-key #\Backspace) 'backward-delete-char
      (global-key (name-char "C-d")) 'delete-char
      (global-key (name-char "C-b")) 'backward-char
      (global-key (name-char "C-f")) 'forward-char
      (global-key (name-char "C-p")) 'previous-line
      (global-key (name-char "C-n")) 'next-line
      (global-key (name-char "C-a")) 'move-beginning-of-line
      (global-key (name-char "C-e")) 'move-end-of-line
      (global-key (name-char "C-r")) 'redraw-screen
      (global-key (name-char "M-x")) 'extended-command
      (global-key (name-char "C-x")) *extended-command-table*
      (global-key (name-char "C-q")) 'quit
      (gethash (name-char "b") *extended-command-table*) 'switch-to-buffer
      (gethash (name-char "k") *extended-command-table*) 'kill-buffer
      (gethash (name-char "C-b") *extended-command-table*) 'list-buffers
      (gethash (name-char "C-f") *extended-command-table*) 'open-file
      (gethash (name-char "C-s") *extended-command-table*) 'save-file)

(defun editor-loop ()
  (loop
     (with-simple-restart (continue "Ignore the error.")
       (redraw-screen)
       (let* ((ch (read-char *input-stream*))
              (fn (or (buffer-key *buffer* ch)
                      (global-key ch))))
         (when (hash-table-p fn)
           (let* ((*minibuffer* (get-buffer-create " *minibuffer<2>*"))
                  (*buffer* *minibuffer*))
             (clear-buffer)
             (insert (char-name ch))
             (redraw-screen))
           (setf ch (read-char *input-stream*))
           (setf fn (gethash ch fn)))
         (setf *last-key-pressed* ch)
         (when *delayed-minibuffer-clear*
           (setf *delayed-minibuffer-clear* nil)
           (with-current-buffer *minibuffer*
             (clear-buffer)))
         (when fn
           (funcall fn))
         (setf *last-command* fn)))))

(defun ed (&optional thing)
  (unless *buffer*
    (set-buffer (get-buffer-create "*scratch*"))
    (setf *main-screen-buffer* (get-buffer-create "*scratch*"))
    (setf *minibuffer* (get-buffer-create " *minibuffer*")
          (buffer-key *minibuffer* #\Newline) 'exit-minibuffer))
  ;; Avoid the echo behaviour of framebuffer streams.
  (let ((*input-stream* (make-instance 'sys.int::ps/2-keyboard-stream)))
    (with-saved-screen (fb) ; binding special variables using lambda seems to be astonishingly broken.
      (let ((*main-screen* fb))
        (handler-case
            (editor-loop)
          (exit-editor ()))))))
