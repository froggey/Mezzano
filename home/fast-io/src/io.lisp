(in-package :fast-io)

 ;; Vector buffer

(defvar *default-output-buffer-size* 16)

(declaim (inline output-buffer-vector output-buffer-fill output-buffer-len))
(defstruct output-buffer
  (vector (make-octet-vector *default-output-buffer-size*)
   :type octet-vector)
  (fill 0 :type index)
  (len 0 :type index)
  (queue nil :type list)
  (last nil :type list)
  (output nil))

(defstruct input-buffer
  (vector nil :type (or null octet-vector))
  (pos 0 :type index)
  (stream nil))

(defun buffer-position (buffer)
  "Return the number of bytes read (for an INPUT-BUFFER) or written
(for an OUTPUT-BUFFER)"
  (etypecase buffer
    (input-buffer (input-buffer-pos buffer))
    (output-buffer (output-buffer-len buffer))))

(declaim (ftype (function (index) octet-vector) make-octet-vector)
         (inline make-octet-vector))
(defun make-octet-vector (len)
  (make-array (the index len) :element-type 'octet))

(defun octets-from (sequence)
  (let ((vec (make-octet-vector (length sequence))))
    (replace vec sequence)
    vec))

(defun concat-buffer (buffer)
  (let* ((len (output-buffer-len buffer))
         (array
           #+fast-io-sv
           (if (eq :static (output-buffer-output buffer))
               (static-vectors:make-static-vector (the index len))
               (make-octet-vector len))
           #-fast-io-sv
           (make-octet-vector len)))
    (loop as i = 0 then (+ i (length a))
          for a in (output-buffer-queue buffer) do
            (replace (the octet-vector array)
                     (the octet-vector a) :start1 i)
          finally
             (replace (the octet-vector array)
                      (output-buffer-vector buffer)
                      :start1 i
                      :end2 (output-buffer-fill buffer)))
    array))

(defun flush (output-buffer)
  (when (> (output-buffer-fill output-buffer) 0)
    (write-sequence (output-buffer-vector output-buffer)
                    (output-buffer-output output-buffer)
                    :start 0 :end (output-buffer-fill output-buffer))
    (prog1 (output-buffer-fill output-buffer)
      (setf (output-buffer-fill output-buffer) 0))))

(defun extend (buffer &optional (min 1))
  (let ((vector (output-buffer-vector buffer)))
    (setf (output-buffer-last buffer)
          (nconc (output-buffer-last buffer)
                 (cons vector nil))
          (output-buffer-vector buffer)
          (make-octet-vector (max min (1+ (* 2 (length vector)))))
          (output-buffer-fill buffer) 0)
    (unless (output-buffer-queue buffer)
      (setf (output-buffer-queue buffer)
            (output-buffer-last buffer)))))

(defun fast-write-byte (byte output-buffer)
  (declare (type octet byte)
           (type output-buffer output-buffer)
           (optimize (speed 3) (safety 1)))
  (when (= (output-buffer-fill output-buffer)
           (array-dimension (output-buffer-vector output-buffer) 0))
    (if (streamp (output-buffer-output output-buffer))
        (flush output-buffer)
        (extend output-buffer)))
  (prog1
      (setf (aref (output-buffer-vector output-buffer)
                  (output-buffer-fill output-buffer))
            byte)
    (incf (output-buffer-fill output-buffer))
    (incf (output-buffer-len output-buffer))))

(defun fast-read-byte (input-buffer &optional (eof-error-p t) eof-value)
  (declare (type input-buffer input-buffer))
  (when-let ((vec (input-buffer-vector input-buffer))
             (pos (input-buffer-pos input-buffer)))
    (when (< pos (length vec))
      (incf (input-buffer-pos input-buffer))
      (return-from fast-read-byte (aref vec pos))))
  (when-let (stream (input-buffer-stream input-buffer))
    (return-from fast-read-byte (read-byte stream eof-error-p eof-value)))
  (if eof-error-p
      (error 'end-of-file :stream input-buffer)
      eof-value))

(defun fast-write-sequence (sequence output-buffer &optional (start 0) end)
  (if (streamp (output-buffer-output output-buffer))
      (progn
        (flush output-buffer)
        (write-sequence sequence (output-buffer-output output-buffer) :start start :end end))
      (progn
        (let* ((start2 start)
               (len (if end
                        (- end start)
                        (- (length sequence) start)))
               (buffer-remaining
                 (- (length (output-buffer-vector output-buffer))
                    (output-buffer-fill output-buffer))))
          (when (> buffer-remaining 0)
            (replace (output-buffer-vector output-buffer)
                     (the octet-vector sequence)
                     :start1 (output-buffer-fill output-buffer)
                     :start2 start2
                     :end2 end)
            (incf start2 buffer-remaining)
            (incf (output-buffer-fill output-buffer)
                  (min buffer-remaining len)))
          (let ((sequence-remaining (- (or end (length sequence)) start2)))
            (when (> sequence-remaining 0)
              (extend output-buffer sequence-remaining)
              (replace (output-buffer-vector output-buffer)
                       (the octet-vector sequence)
                       :start2 start2
                       :end2 end)
              (incf (output-buffer-fill output-buffer) sequence-remaining)))
          (incf (output-buffer-len output-buffer) len)
          len))))

(defun fast-read-sequence (sequence input-buffer &optional (start 0) end)
  (declare (type octet-vector sequence)
           (type input-buffer input-buffer))
  (let ((start1 start)
        (total-len (if end
                       (- end start)
                       (- (length sequence) start))))
    (when-let ((vec (input-buffer-vector input-buffer))
               (pos (input-buffer-pos input-buffer)))
      (when (< pos (length vec))
        (let ((len (min total-len (- (length vec) pos))))
          (replace sequence vec
                   :start1 start1
                   :start2 pos
                   :end2 (+ pos len))
          (incf (input-buffer-pos input-buffer) len)
          (incf start1 len))))
    (when (< start1 total-len)
      (when-let (stream (input-buffer-stream input-buffer))
        (return-from fast-read-sequence
          (read-sequence sequence stream :start start1
                                         :end (+ total-len start1)))))
    start1))

(defun finish-output-buffer (output-buffer)
  "Finish an output buffer. If it is backed by a vector (static or otherwise)
it returns the final octet vector. If it is backed by a stream it ensures that
all data has been flushed to the stream."
  (if (streamp (output-buffer-output output-buffer))
      (flush output-buffer)
      (concat-buffer output-buffer)))

(defmacro with-fast-output ((buffer &optional output) &body body)
  "Create `BUFFER`, optionally outputting to `OUTPUT`."
  `(let ((,buffer (make-output-buffer :output ,output)))
     ,@body
     (if (streamp (output-buffer-output ,buffer))
         (flush ,buffer)
         (finish-output-buffer ,buffer))))

(defmacro with-fast-input ((buffer vector &optional stream (offset 0)) &body body)
  `(let ((,buffer (make-input-buffer :vector ,vector :stream ,stream :pos ,offset)))
     ,@body))

 ;; READx and WRITEx
;;; WRITE-UNSIGNED-BE, READ-UNSIGNED-BE, etc taken from PACK, which is
;;; in the public domain.

(defmacro write-unsigned-be (value size buffer)
  (once-only (value buffer)
    `(progn
       ,@(loop for i from (* (1- size) 8) downto 0 by 8
               collect `(fast-write-byte (ldb (byte 8 ,i) ,value) ,buffer)))))

(defmacro read-unsigned-be (size buffer)
  (with-gensyms (value)
    (once-only (buffer)
      `(let ((,value 0))
         ,@(loop for i from (* (1- size) 8) downto 0 by 8
                 collect `(setf (ldb (byte 8 ,i) ,value) (fast-read-byte ,buffer)))
         ,value))))

(defmacro write-unsigned-le (value size buffer)
  (once-only (value buffer)
    `(progn
       ,@(loop for i from 0 below (* 8 size) by 8
               collect `(fast-write-byte (ldb (byte 8 ,i) ,value) ,buffer)))))

(defmacro read-unsigned-le (size buffer)
  (with-gensyms (value)
    (once-only (buffer)
      `(let ((,value 0))
         ,@(loop for i from 0 below (* 8 size) by 8
                 collect `(setf (ldb (byte 8 ,i) ,value) (fast-read-byte ,buffer)))
         ,value))))

(declaim (inline unsigned-to-signed))
(defun unsigned-to-signed (value size)
  (let ((max-signed (expt 2 (1- (* 8 size))))
        (to-subtract (expt 2 (* 8 size))))
    (if (>= value max-signed)
        (- value to-subtract)
        value)))

(declaim (inline signed-to-unsigned))
(defun signed-to-unsigned (value size)
  (if (minusp value)
      (+ value (expt 2 (* 8 size)))
      value))

(defmacro make-readers (&rest bitlens)
  (let ((names (mapcar (lambda (n)
                         (mapcar (lambda (m) (symbolicate (format nil m n)))
                                 '("READ~A-BE" "READU~A-BE"
                                   "READ~A-LE" "READU~A-LE")))
                       bitlens)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (inline ,@(flatten names)))
       ,@(loop for fun in names
               for bits in bitlens
               as bytes = (truncate bits 8)
               collect
               `(progn
                  (defun ,(first fun) (buffer)
                    (unsigned-to-signed (read-unsigned-be ,bytes buffer) ,bytes))
                  (defun ,(second fun) (buffer)
                    (read-unsigned-be ,bytes buffer))
                  (defun ,(third fun) (buffer)
                    (unsigned-to-signed (read-unsigned-le ,bytes buffer) ,bytes))
                  (defun ,(fourth fun) (buffer)
                    (read-unsigned-le ,bytes buffer)))))))

(defmacro make-writers (&rest bitlens)
  (let ((names (mapcar (lambda (n)
                         (mapcar (lambda (m) (symbolicate (format nil m n)))
                                 '("WRITE~A-BE" "WRITEU~A-BE"
                                   "WRITE~A-LE" "WRITEU~A-LE")))
                       bitlens)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (notinline ,@(flatten names)))
       ,@(loop for fun in names
               for bits in bitlens
               as bytes = (truncate bits 8)
               collect
               `(progn
                  (defun ,(first fun) (value buffer)
                    (declare (type (signed-byte ,bits) value))
                    (write-unsigned-be (the (unsigned-byte ,bits)
                                            (signed-to-unsigned value ,bytes)) ,bytes buffer))
                  (defun ,(second fun) (value buffer)
                    (declare (type (unsigned-byte ,bits) value))
                    (write-unsigned-be (the (unsigned-byte ,bits) value)
                                       ,bytes buffer))
                  (defun ,(third fun) (value buffer)
                    (declare (type (signed-byte ,bits) value))
                    (write-unsigned-le (the (unsigned-byte ,bits)
                                            (signed-to-unsigned value ,bytes)) ,bytes buffer))
                  (defun ,(fourth fun) (value buffer)
                    (declare (type (unsigned-byte ,bits) value))
                    (write-unsigned-le (the (unsigned-byte ,bits) value)
                                       ,bytes buffer)))))))

(make-writers 16 24 32 64 128)
(make-readers 16 24 32 64 128)

(declaim (inline write8 writeu8 read8 readu8))
(defun write8 (value buffer)
  (declare (type (signed-byte 8) value))
  (fast-write-byte (signed-to-unsigned value 1) buffer))

(defun writeu8 (value buffer)
  (declare (type (unsigned-byte 8) value))
  (fast-write-byte value buffer))


(defun read8 (buffer)
  (unsigned-to-signed (fast-read-byte buffer) 1))

(defun readu8 (buffer)
  (fast-read-byte buffer))

(setf (symbol-function 'write8-le) #'write8)
(setf (symbol-function 'write8-be) #'write8)
(setf (symbol-function 'writeu8-le) #'writeu8)
(setf (symbol-function 'writeu8-be) #'writeu8)

(setf (symbol-function 'read8-le) #'read8)
(setf (symbol-function 'read8-be) #'read8)
(setf (symbol-function 'readu8-le) #'readu8)
(setf (symbol-function 'readu8-be) #'readu8)
