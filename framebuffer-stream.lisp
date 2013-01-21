(in-package #:sys.int)

(defclass framebuffer-output-stream (sys.gray:fundamental-character-output-stream)
  ((framebuffer :initarg :framebuffer :accessor fbstream-framebuffer)
   (x :initarg :x :accessor fbstream-x)
   (y :initarg :y :accessor fbstream-y))
  (:default-initargs :x 0 :y 0))

(defclass framebuffer-stream (simple-edit-mixin
                              ps/2-keyboard-stream
                              framebuffer-output-stream)
  ())

(defun framebuffer-write-char (character stream)
  (let ((fb (fbstream-framebuffer stream))
        (x (fbstream-x stream))
        (y (fbstream-y stream)))
    (cond
      ((eql character #\Newline)
       ;; Clear the next line.
       (setf (fbstream-x stream) 0
             y (if (> (+ y 16 16) (array-dimension fb 0))
                   0
                   (+ y 16))
             (fbstream-y stream) y)
       (%bitset 16 (array-dimension fb 1) #xFF000000 fb y 0))
      (t (let ((width (if (eql character #\Space) 8 (unifont-glyph-width character))))
           (when (> (+ x width) (array-dimension fb 1))
             ;; Advance to the next line.
             ;; Maybe should clear the end of the current line?
             (setf x 0
                   y (if (> (+ y 16 16) (array-dimension fb 0))
                         0
                         (+ y 16))
                   (fbstream-y stream) y)
             (%bitset 16 (array-dimension fb 1) #xFF000000 fb y 0))
           (if (eql character #\Space)
               (%bitset 16 8 #xFF000000 fb y x)
               (render-char-at character fb x y))
           (incf x width)
           (setf (fbstream-x stream) x))))))

(defmethod sys.gray:stream-write-char ((stream framebuffer-output-stream) character)
  (framebuffer-write-char character stream))

(defmethod sys.gray:stream-start-line-p ((stream framebuffer-output-stream))
  (zerop (fbstream-x stream)))

(defmethod stream-cursor-pos ((stream framebuffer-output-stream))
  (values (fbstream-x stream) (fbstream-y stream)))

(defmethod stream-move-to ((stream framebuffer-output-stream) x y)
  #+nil(check-type x integer)
  #+nil(check-type y integer)
  (setf (fbstream-x stream) x
        (fbstream-y stream) y))

(defmethod stream-character-width ((stream framebuffer-output-stream) character)
  (if (eql character #\Space)
      8
      (unifont-glyph-width character)))

(defun framebuffer-compute-motion (stream string start end initial-x initial-y)
  (unless end (setf end (length string)))
  (unless initial-x (setf initial-x (fbstream-x stream)))
  (unless initial-y (setf initial-y (fbstream-y stream)))
  (do ((framebuffer (fbstream-framebuffer stream))
       (i start (1+ i)))
      ((>= i end)
       (values initial-x initial-y))
    (let* ((ch (char string i))
	   (width (stream-character-width stream ch)))
      (when (or (eql ch #\Newline)
                (> (+ initial-x width) (array-dimension framebuffer 1)))
        (setf initial-x 0
              initial-y (if (>= (+ initial-y 16) (array-dimension framebuffer 0))
                            0
                            (+ initial-y 16))))
      (unless (eql ch #\Newline)
        (incf initial-x width)))))

(defmethod stream-compute-motion ((stream framebuffer-output-stream) string &optional (start 0) end initial-x initial-y)
  (framebuffer-compute-motion stream string start end initial-x initial-y))

(defmethod stream-clear-between ((stream framebuffer-output-stream) start-x start-y end-x end-y)
  (let ((framebuffer (fbstream-framebuffer stream)))
    (cond ((eql start-y end-y)
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
