(in-package #:sys.int)

(defclass framebuffer-output-stream (sys.gray:fundamental-character-output-stream)
  ((framebuffer :initarg :framebuffer :accessor fbstream-framebuffer)
   (line :initarg :line :accessor fbstream-line)
   (x :initarg :x :accessor fbstream-x)
   (y :initarg :y :accessor fbstream-y))
  (:default-initargs :x 0 :y 0 :line 0))

(defclass framebuffer-stream (simple-edit-mixin
                              ps/2-keyboard-stream
                              framebuffer-output-stream)
  ())

(defun framebuffer-newline (stream)
  (let ((fb (fbstream-framebuffer stream))
        (y (fbstream-y stream)))
    ;; Clear to the end of the current line.
    (%bitset 16 (- (array-dimension fb 1) (fbstream-x stream)) #xFF000000 fb y (fbstream-x stream))
    ;; Advance to the next line.
    (setf (fbstream-x stream) 0)
    (cond ((> (+ y 16 16) (array-dimension fb 0))
           ;; Off the end of the screen. Scroll!
           (incf (fbstream-line stream) 16)
           (%bitblt (- (array-dimension fb 0) 16) (array-dimension fb 1)
                    fb 16 0
                    fb 0 0))
          (t (incf y 16)
             (setf (fbstream-y stream) y)))
    ;; Clear line.
    (%bitset 16 (array-dimension fb 1) #xFF000000 fb y 0)))

(defmethod sys.gray:stream-write-char ((stream framebuffer-output-stream) character)
  (cond
    ((eql character #\Newline)
     (framebuffer-newline stream))
    (t (let ((width (unifont-glyph-width character))
             (fb (fbstream-framebuffer stream)))
         (when (> (+ (fbstream-x stream) width) (array-dimension fb 1))
           (framebuffer-newline stream))
         (let ((x (fbstream-x stream))
               (y (fbstream-y stream)))
           (render-char-at character fb x y)
           (incf x width)
           (setf (fbstream-x stream) x))))))

(defmethod sys.gray:stream-start-line-p ((stream framebuffer-output-stream))
  (zerop (fbstream-x stream)))

(defmethod sys.gray:stream-line-column ((stream framebuffer-output-stream))
  (truncate (fbstream-x stream) 8))

(defmethod stream-cursor-pos ((stream framebuffer-output-stream))
  (values (fbstream-x stream)
          (+ (fbstream-line stream)
             (fbstream-y stream))))

(defmethod stream-move-to ((stream framebuffer-output-stream) x y)
  #+nil(check-type x integer)
  #+nil(check-type y integer)
  (setf (fbstream-x stream) x
        (fbstream-y stream) (max (- y (fbstream-line stream)) 0)))

(defmethod stream-character-width ((stream framebuffer-output-stream) character)
  (unifont-glyph-width character))

(defmethod stream-compute-motion ((stream framebuffer-output-stream) string &optional (start 0) end initial-x initial-y)
  (unless end (setf end (length string)))
  (unless initial-x (setf initial-x (fbstream-x stream)))
  (unless initial-y (setf initial-y (+ (fbstream-line stream)
                                       (fbstream-y stream))))
  (do ((framebuffer (fbstream-framebuffer stream))
       (i start (1+ i)))
      ((>= i end)
       (values initial-x initial-y))
    (let* ((ch (char string i))
	   (width (stream-character-width stream ch)))
      (when (or (eql ch #\Newline)
                (> (+ initial-x width) (array-dimension framebuffer 1)))
        (setf initial-x 0
              initial-y (+ initial-y 16)))
      (unless (eql ch #\Newline)
        (incf initial-x width)))))

(defmethod stream-clear-between ((stream framebuffer-output-stream) start-x start-y end-x end-y)
  (let ((framebuffer (fbstream-framebuffer stream)))
    (setf start-y (- start-y (fbstream-line stream))
          end-y (- end-y (fbstream-line stream)))
    (cond ((> start-y end-y)) ; Not clearing any lines.
          ((eql start-y end-y)
           ;; Clearing one line.
           (%bitset 16 (- end-x start-x) #xFF000000 framebuffer start-y start-x))
          (t ;; Clearing many lines.
           ;; Clear top line.
           (%bitset 16 (- (array-dimension framebuffer 1) start-x) #xFF000000
                    framebuffer start-y start-x)
           ;; Clear in-between.
           (when (> (- end-y start-y) 16)
             (%bitset (- end-y start-y 16) (array-dimension framebuffer 1) #xFF000000
                      framebuffer (+ start-y 16) 0))
           ;; Clear bottom line.
           (%bitset 16 end-x #xFF000000
                    framebuffer end-y 0)))))
