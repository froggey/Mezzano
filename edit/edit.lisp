(defpackage #:edit
  (:use #:cl))

(in-package #:edit)

(declaim (special *buffer* *all-buffers*))

(define-condition exit-editor () ())
(define-condition simple-editor-error (simple-error) ())
(define-condition minibuffer-complete () ())

(defvar *editor-window* nil)
(defvar *frame* nil)
(defvar *minibuffer* nil)
(defvar *delayed-minibuffer-clear* nil)
(defvar *dispatch-table* (make-hash-table))
(defvar *last-key-pressed* nil)
(defvar *last-command* nil)
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
  (sys.graphics::bitset h w (sys.graphics::make-colour sys.graphics::*default-background-colour*) fb y x)
  (do ((lines-written 0)
       (line (buffer-first-line buffer)))
      ((or (>= lines-written h)
	   (null line)))
    (let ((contents (slot-value line 'contents))
          (next (slot-value line 'next))
          (text-colour (sys.graphics::make-colour sys.graphics::*default-foreground-colour*)))
      (do ((i 0 (1+ i))
	   (ofs 0))
	  ((or (>= i (length contents))
	       (> lines-written h))
	   (when (with-current-buffer buffer
                   (and (eql (point-line) line)
                        (eql (point-character) i)))
	     (draw-cursor fb (+ x ofs) (+ y lines-written) (eql *buffer* buffer))))
	(let* ((ch (char contents i))
               (width (sys.int::unifont-glyph-width ch)))
          (when (< (+ ofs width) w)
            (let ((glyph (sys.int::map-unifont-2d ch)))
              (when glyph
                (sys.graphics::bitset-argb-xrgb-mask-1 16 width text-colour
                                                       glyph 0 0
                                                       fb (+ y lines-written) (+ x ofs)))))
	  (when (with-current-buffer buffer
                  (and (eql (point-line) line)
                       (eql (point-character) i)))
            (draw-cursor fb (+ x ofs) (+ y lines-written) (eql *buffer* buffer)))
          (incf ofs width)))
      (incf lines-written 16)
      (setf line next))))

(defun redraw-screen ()
  (sys.graphics::window-redraw *frame*))

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
       (let* ((ch (read-editor-character))
              (fn (or (buffer-key *buffer* ch)
                      (global-key ch))))
         (when (hash-table-p fn)
           (let* ((*minibuffer* (get-buffer-create " *minibuffer<2>*"))
                  (*buffer* *minibuffer*))
             (clear-buffer)
             (insert (char-name ch))
             (redraw-screen))
           (setf ch (read-editor-character))
           (setf fn (gethash ch fn)))
         (setf *last-key-pressed* ch)
         (when *delayed-minibuffer-clear*
           (setf *delayed-minibuffer-clear* nil)
           (with-current-buffer *minibuffer*
             (clear-buffer)))
         (when fn
           (funcall fn))
         (setf *last-command* fn)))))

(defclass editor-window (sys.graphics::window-with-chrome)
  ((input-buffer :initarg :input-buffer)
   (process :reader window-process))
  (:default-initargs :input-buffer (sys.int::make-fifo 500 "User Input" 'character)))

(defmethod initialize-instance :after ((instance editor-window))
  (unless *buffer*
    (set-buffer (get-buffer-create "*scratch*"))
    (setf *main-screen-buffer* (get-buffer-create "*scratch*"))
    (setf *minibuffer* (get-buffer-create " *minibuffer*")
          (buffer-key *minibuffer* #\Newline) 'exit-minibuffer))
  (let ((process (sys.int::make-process "Edit")))
    (setf (slot-value instance 'process) process)
    (sys.int::process-preset process 'editor-top-level instance)
    (sys.int::process-enable process)))

(defmethod sys.graphics::window-close-event :before ((window editor-window))
  (sys.int::process-arrest-reason (window-process window) :window-closed))

(defmethod sys.graphics::key-press-event ((window editor-window) character)
  (sys.int::fifo-push character (slot-value window 'input-buffer)))

(defmethod sys.graphics::window-redraw ((window editor-window))
  (let* ((fb (sys.graphics::window-backbuffer window))
         (dims (array-dimensions fb))
         (width (second dims))
         (height (first dims))
         (text-colour (sys.graphics::make-colour sys.graphics::*default-foreground-colour*)))
    (multiple-value-bind (left right top bottom)
        (sys.graphics::compute-window-margins window)
      (display-buffer *main-screen-buffer* fb
                      left top
                      (- width left right) (- height 16 16 2 top bottom))
      (display-buffer *minibuffer* fb
                      left (- height 16 bottom)
                      (- width left right) 16)
      (sys.graphics::bitset 16 (- width left right)
                            (sys.graphics::make-colour sys.graphics::*default-background-darker-colour*)
                            fb
                            (- height 16 16 1 bottom) left)
      (let ((name (buffer-name *main-screen-buffer*))
            (ofs left))
        (dotimes (i (length name))
          (let* ((ch (char name i))
                 (char-width (sys.int::unifont-glyph-width ch))
                 (glyph (sys.int::map-unifont-2d ch)))
            (when (and (< (+ ofs char-width) width)
                       glyph)
              (sys.graphics::bitset-argb-xrgb-mask-1 16 char-width text-colour
                                                     glyph 0 0
                                                     fb (- height 16 16 1 bottom) ofs))
            (incf ofs char-width))))
      (sys.graphics::bitset 1 (- width left right) #xFFD0D0D0 fb (- height 16 1 bottom) left)
      (sys.graphics::bitset 1 (- width left right) #xFFD0D0D0 fb (- height 16 16 2 bottom) left))))

(defun read-editor-character ()
  (sys.int::fifo-pop (slot-value *frame* 'input-buffer)))

(defun editor-top-level (window)
  (let ((*buffer* (get-buffer-create "*scratch*"))
        (*last-key-pressed* nil)
        (*frame* window))
    (unwind-protect
         (handler-case (editor-loop)
           (exit-editor ()))
      (sys.graphics::close-window window))))

(defun ed (&optional thing)
  (unless *editor-window*
    (setf *editor-window* (sys.graphics::make-window "Edit.com" 600 600 'editor-window)))
  (sys.graphics::window-set-visibility *editor-window* t))
(defun create-or-show-editor-window ()
  (unless *editor-window*
    (setf *editor-window* (sys.graphics::make-window "Edit.com" 600 600 'editor-window)))
  (sys.graphics::window-set-visibility *editor-window* t))
(defun create-or-show-editor-window ()
  (let ((win (sys.graphics::make-window "Edit" 600 600 'editor-window)))
    (sys.graphics::window-set-visibility win t)))

(setf (gethash (name-char "F6") sys.graphics::*global-keybindings*) 'create-or-show-editor-window)
(setf (gethash (name-char "M-F6") sys.graphics::*global-keybindings*) 'create-editor-window)
