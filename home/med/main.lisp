(in-package :med)

;;; testing

(defvar *break-on-command-signals* nil)

(defun translate-command (character)
  "Translate a character to a command."
  (multiple-value-bind (command found-p)
    (gethash character (buffer-key-map (current-buffer *editor*)))
    (unless found-p
      (setf command (gethash character (global-key-map))))
    command))

(defun editor-loop ()
  (flet ((call-command (command)
           (let ((buffer (current-buffer *editor*))
                 (*break-on-signals* (or *break-on-signals*
                                         *break-on-command-signals*)))
             (with-simple-restart (abort "Abort command ~S" command)
               (mapc 'funcall (buffer-pre-command-hooks buffer))
               (funcall command)
               (mapc 'funcall (buffer-post-command-hooks buffer))))))
  (loop
     (force-redisplay)
     (let* ((*this-character* (editor-read-char))
            (*this-chord* (list *this-character*))
            (*this-command* (translate-command *this-character*)))
       (cond ((hash-table-p *this-command*)
              (loop
                 (setf *this-character* (editor-read-char)
                       *this-command* (gethash *this-character* *this-command*))
                 (push *this-character* *this-chord*)
                 (when (not (hash-table-p *this-command*))
                   (setf *this-chord* (reverse *this-chord*))
                   (cond (*this-command*
                           (call-command *this-command*))
                         (t (format t "Unknown command ~S~%" *this-chord*)))
                   (return))))
             (*this-command*
               (call-command *this-command*))
             (t (format t "Unknown command ~S~%" *this-character*)))
       (setf *last-command* *this-command*
             *last-character* *this-character*
             *last-chord* *this-chord*)
       (setf *current-editor* *editor*)
       (setf (pending-redisplay *editor*) (not (redisplay)))))))

(defvar *editors* ())
(defvar *current-editor* ())

(defun editor-main (width height initial-file initial-position)
  (let ((font (mezzano.gui.font:open-font mezzano.gui.font:*default-monospace-font* mezzano.gui.font:*default-monospace-font-size*))
        (font-bold (mezzano.gui.font:open-font mezzano.gui.font::*default-monospace-bold-font* mezzano.gui.font:*default-monospace-font-size*))
        (fifo (mezzano.supervisor:make-fifo 50)))
  (mezzano.gui.compositor:with-window (window fifo (or width 640) (or height 700) :kind :editor)
    (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
      (frame (make-instance 'mezzano.gui.widgets:frame
                            :framebuffer framebuffer
                            :title "Editor"
                            :close-button-p t
                            :resizablep t
                            :damage-function (mezzano.gui.widgets:default-damage-function
                                                window)
                            :set-cursor-function (mezzano.gui.widgets:default-cursor-function window)))
      (*editor* (make-instance 'editor
                               :fifo fifo
                               :font font
                               :font-bold font-bold
                               :window window
                               :frame frame
                               :buffer (make-instance 'buffer)))
      (*last-command* nil)
      (*last-character* nil)
      (*last-chord* nil)
      (*default-pathname-defaults* *default-pathname-defaults*))
         (initialize-key-map (global-key-map))
         (mezzano.gui.widgets:draw-frame frame)
         (multiple-value-bind (left right top bottom)
           (mezzano.gui.widgets:frame-size (frame *editor*))
        (mezzano.gui:bitset :set
                            (- (mezzano.gui.compositor:width window) left right)
                            (- (mezzano.gui.compositor:height window) top bottom)
                            (background-colour *editor*)
                            framebuffer
                            left top)
        (mezzano.gui.compositor:damage-window window
                                              left top
                                              (- (mezzano.gui.compositor:width window)
                                                 left right)
                                              (- (mezzano.gui.compositor:height window)
                                                 top bottom)))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (let ((buffer (get-buffer-create "*Messages*")))
      (unless *editors*
        (format t "Welcome to the Mezzano EDitor. Happy Hacking!~%"))
      (push *editor* *editors*)
      (ignore-errors
        (when initial-file
          (find-file initial-file)
          (when initial-position
            (move-beginning-of-buffer (current-buffer *editor*))
            (move-char (current-buffer *editor*) initial-position)
            (force-redisplay))))
      (unwind-protect
           (catch 'quit
             (loop
                (handler-case
                    (editor-loop)
                  (error (c)
                    (ignore-errors
                      (format t "Editor error: ~A~%" c)
                      (setf (pending-redisplay *editor*) t))))))
        (setf *editors* (remove *editor* *editors*))))))))

(defvar *messages* (make-instance 'buffer))

(defun spawn (&key width height initial-file initial-position)
  (pushnew *messages* (buffer-list))
  (setf (buffer-property *messages* 'name) "*Messages*")
  (mezzano.supervisor:make-thread
    (lambda () (editor-main width height initial-file initial-position))
    :name "Editor"
    :initial-bindings `((*terminal-io* ,(make-instance
                                           'mezzano.gui.popup-io-stream:popup-io-stream
                                           :title "Editor console"))
                        (*standard-input* ,(make-synonym-stream '*terminal-io*))
                        (*standard-output* ,(make-instance 'buffer-stream
                                                           :buffer *messages*))
                        (*error-output* ,(make-synonym-stream '*terminal-io*))
                        (*trace-output* ,(make-synonym-stream '*terminal-io*))
                        (*debug-io* ,(make-synonym-stream '*terminal-io*))
                        (*query-io* ,(make-synonym-stream '*terminal-io*)))))

#+(or)
(defun spawn (&key width height initial-file initial-position)
  (mezzano.supervisor:make-thread (lambda () (editor-main width height initial-file initial-position))
                                  :name "Editor"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Editor console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))

(defun med-ed-hook (&key initial-pathname initial-position)
  (let ((existing (mezzano.gui.compositor:get-window-by-kind :editor)))
    (cond (existing
           ;; FIXME: Bring existing editor window to front.
           (when initial-pathname
             (mezzano.supervisor:fifo-push
              (make-instance 'open-file-request :path initial-pathname :position initial-position)
              (mezzano.gui.compositor::fifo existing)
              nil)))
          (t
           (spawn :initial-file initial-pathname :initial-position initial-position)))))

(when (not mezzano.extensions:*ed-hook*)
  (setf mezzano.extensions:*ed-hook* 'med-ed-hook))
