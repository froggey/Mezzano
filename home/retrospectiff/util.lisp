
(in-package :retrospectiff.util)

(defun string-contents-of-stream (in)
  "Returns a string with the entire contents of the specified file."
  (with-output-to-string (contents)
    (let* ((buffer-size 4096)
           (buffer (make-string buffer-size)))
      (labels ((read-chunks ()
                 (let ((size (read-sequence buffer in)))
                   (if (< size buffer-size)
                       (princ (subseq buffer 0 size) contents)
                       (progn
                         (princ buffer contents)
                         (read-chunks))))))
        (read-chunks)))))

(defun string-contents-of-file (pathname)
  (with-open-file (in pathname :direction :input)
    (string-contents-of-stream in)))

(defun vector-contents-of-stream (in)
  "Returns a string with the entire contents of the specified file."
  (let ((contents (make-array 4096
                              :element-type '(unsigned-byte 8)
                              :fill-pointer 0
                              :adjustable t)))
    (let* ((buffer-size 4096)
           (buffer (make-array buffer-size
                               :element-type '(unsigned-byte 8)
                               :adjustable nil)))
      (labels ((read-chunks ()
                 (let ((size (read-sequence buffer in))
                       (old-fill-pointer (fill-pointer contents)))
                   (ensure-array-size-and-set-fill-pointer
                    contents (+ old-fill-pointer
                                size))
                   (loop for i below size
                      for k from old-fill-pointer
                      do (setf (aref contents k)
                               (aref buffer i)))
                   (unless (< size buffer-size)
                     (read-chunks)))))
        (read-chunks)))
    contents))

(defun vector-contents-of-file (pathname)
  (with-open-file (in pathname 
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (vector-contents-of-stream in)))

(defun ensure-array-size-and-set-fill-pointer (array fill-pointer)
  (let ((length (array-dimension array 0)))
    (when (>= fill-pointer length)
      (adjust-array array (max 256
                               (+ length (ash length -1))
                               fill-pointer)))
    (setf (fill-pointer array) fill-pointer)))

(defun remove-keyword-args (keywords list)
  (if (listp keywords)
      (loop for (x y) on list by #'cddr
         append (unless (member x keywords)
                  (list x y)))
      (loop for (x y) on list by #'cddr
         append (unless (eq x keywords)
                  (list x y)))))
