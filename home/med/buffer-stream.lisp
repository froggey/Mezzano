(in-package :med)

(defclass buffer-stream (mezzano.gray:fundamental-character-output-stream)
  ((buffer :initarg :buffer :reader buffer-stream-buffer)
   (filter :initarg :filter :reader buffer-stream-filter :initform nil)))

(defclass buffer-input-stream (mezzano.gray:fundamental-character-input-stream)
  ((buffer :initarg :buffer :reader buffer-stream-buffer)))

(defmethod mezzano.gray:stream-write-char ((stream buffer-stream) char)
  (let ((buffer (buffer-stream-buffer stream))
        (filter (buffer-stream-filter stream)))
    (move-end-of-buffer buffer)
    (insert buffer char)
    (let ((input-start (buffer-property buffer 'input-start)))
      (if input-start
          (move-mark-to-mark (buffer-property buffer 'input-start) (buffer-point buffer))
          (setf (buffer-property buffer 'input-start) (copy-mark (buffer-point buffer) :left))))
    (when filter
      (funcall filter buffer char))
    (when (or (char= char #\Newline) (char= char #\Space))
      (force-redisplay))))

(defmethod mezzano.gray:stream-read-char-no-hang ((stream buffer-stream))
  (let* ((buffer (buffer-stream-buffer stream))
         (point (buffer-point buffer))
         (input-start (buffer-property buffer 'input-start)))
    (when (mark> point input-start)
      (let* ((line (mark-line input-start))
             (c (handler-case (line-character line (mark-charpos input-start))
                (error () #\Newline))))
        (move-mark input-start)
        c))))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))

(defmethod mezzano.gray:stream-read-char ((stream buffer-stream))
  (loop
     (awhen (mezzano.gray:stream-read-char-no-hang stream)
       (return it))
     (mezzano.supervisor::fifo-push (mezzano.supervisor::fifo-pop (fifo *editor*))
                                    (fifo *editor*))))

(defmethod mezzano.gray:stream-unread-char ((stream buffer-stream) char)
  (let ((buffer (buffer-stream-buffer stream)))
    (move-mark (buffer-property buffer 'input-start) -1)))

(defmethod mezzano.gray:stream-start-line-p ((stream buffer-stream))
  (start-of-line-p (buffer-point (buffer-stream-buffer stream))))
