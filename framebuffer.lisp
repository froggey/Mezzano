(in-package #:sys.int)

(declaim (special *unifont-bmp*))

(defparameter *unifont-glyph-cache* (make-array 256 :initial-element nil))

(defun map-unifont (c)
  (let ((code (char-code c)))
    ;; Unifont only covers plane 0
    (when (and (<= code #xffff)
               (zerop (char-bits c)))
      (let ((row (aref *unifont-bmp* (ash code -8))))
	(when row
	  (aref row (logand code #xff)))))))

(defun unifont-glyph-width (character)
  (let ((glyph (map-unifont character)))
    (cond ((null glyph)
           (* (length (char-name character)) 8))
          ((eql (array-dimension glyph 0) 32)
           16)
          (t 8))))

(defun render-unifont-glyph (c)
  "Produce an array containing the glyph for C that can be blitted directly to the screen."
  (let ((uniglyph (map-unifont c)))
    (cond (uniglyph
           (let* ((is-fullwidth (= (length uniglyph) 32))
                  (screen-glyph (make-array (list 16 (if is-fullwidth 16 8))
                                            :element-type '(unsigned-byte 32))))
             (if is-fullwidth
                 ;; Fullwidth.
                 (dotimes (i 16)
                   (dotimes (j 8)
                     (setf (aref screen-glyph i j)
                           (if (= 0 (logand (ash 1 (- 7 j)) (aref uniglyph (* i 2))))
                               #xFF000000
                               #xFFB2B2B2)
                           (aref screen-glyph i (+ j 8))
                           (if (= 0 (logand (ash 1 (- 7 j)) (aref uniglyph (1+ (* i 2)))))
                               #xFF000000
                               #xFFB2B2B2))))
                 ;; Halfwidth.
                 (dotimes (i 16)
                   (dotimes (j 8)
                     (setf (aref screen-glyph i j)
                           (if (= 0 (logand (ash 1 (- 7 j)) (aref uniglyph i)))
                               #xFF000000
                               #xFFB2B2B2)))))
             screen-glyph))
          (t ;; Render the character name surounded by a box.
           (let* ((name (char-name c))
                  (screen-glyph (make-array (list 16 (* (length name) 8))
                                            :element-type '(unsigned-byte 32))))
             (dotimes (i (length name))
               (let ((foo (get-unifont-glyph (char name i))))
                 (%bitblt 16 8 foo 0 0
                          screen-glyph 0 (* i 8))))
             ;; Draw box.
             (%bitset 1 (* (length name) 8) #xFFB2B2B2 screen-glyph 0 0)
             (%bitset 1 (* (length name) 8) #xFFB2B2B2 screen-glyph 15 0)
             (dotimes (i 16)
               (setf (aref screen-glyph i 0) #xFFB2B2B2
                     (aref screen-glyph i (1- (* (length name) 8))) #xFFB2B2B2))
             screen-glyph)))))

(defun get-unifont-glyph (c)
  ;; Only use the cache for characters in the BMP.
  (if (and (<= (char-code c) #xFFFF)
           (zerop (char-bits c)))
      (let* ((row (ash (char-code c) -8))
	     (cell (logand (char-code c) #xFF))
	     (cache-row (svref *unifont-glyph-cache* row)))
	(unless cache-row
	  (setf cache-row (make-array 256 :initial-element nil)
		(svref *unifont-glyph-cache* row) cache-row))
	(let ((glyph (svref cache-row cell)))
	  (unless glyph
	    (setf glyph (render-unifont-glyph c)
		  (svref cache-row cell) glyph))
	  glyph))
      (render-unifont-glyph c)))

(defun render-char-at (c framebuffer x y)
  (let ((glyph (get-unifont-glyph c)))
    (%bitblt 16 (array-dimension glyph 1) glyph 0 0
             framebuffer y x)))

(defclass framebuffer-stream (edit-stream ps/2-keyboard-stream stream-object)
  ((framebuffer :initarg :framebuffer :accessor fbstream-framebuffer)
   (x :initarg :x :accessor fbstream-x)
   (y :initarg :y :accessor fbstream-y))
  (:default-initargs :x 0 :y 0))

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

(defmethod stream-write-char (character (stream framebuffer-stream))
  (framebuffer-write-char character stream))

(defmethod stream-start-line-p ((stream framebuffer-stream))
  (zerop (fbstream-x stream)))

(defmethod stream-cursor-pos ((stream framebuffer-stream))
  (values (fbstream-x stream) (fbstream-y stream)))

(defmethod stream-move-to ((stream framebuffer-stream) x y)
  #+nil(check-type x integer)
  #+nil(check-type y integer)
  (setf (fbstream-x stream) x
        (fbstream-y stream) y))

(defmethod stream-character-width ((stream framebuffer-stream) character)
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

(defmethod stream-compute-motion ((stream framebuffer-stream) string &optional (start 0) end initial-x initial-y)
  (framebuffer-compute-motion stream string start end initial-x initial-y))

(defmethod stream-clear-between ((stream framebuffer-stream) start-x start-y end-x end-y)
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

(defmethod stream-element-type* ((stream framebuffer-stream))
  'character)
