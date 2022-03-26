(in-package :fast-io.test)

 ;; Naive

(bench (50000)
  (let ((vec (make-array 16 :element-type 'octet
                            :adjustable t
                            :fill-pointer 0)))
    (dotimes (i 50)
      (vector-push-extend 0 vec))))

 ;; Flexi-streams

#+flexi-streams
(bench (50000)
  (flexi-streams:with-output-to-sequence (stream)
    (dotimes (i 50)
      (write-byte 0 stream))))

#+flexi-streams
(bench (50000)
  (flexi-streams:with-output-to-sequence (stream)
    (let ((vec (make-octet-vector 50)))
      (write-sequence vec stream))))

 ;; Fast-io

(bench (50000)
  (with-fast-output (buffer)
    (dotimes (i 50)
      (fast-write-byte 0 buffer))))

(defun test ()
  (with-fast-output (buffer)
    (dotimes (i 50)
      (fast-write-byte 0 buffer))))

(bench (50000) (test))

(bench (1000000)
  (let ((vec (make-octet-vector 50)))
   (with-fast-output (buffer)
     (fast-write-sequence vec buffer))))

(bench (50000)
  (static-vectors:free-static-vector
   (with-fast-output (buffer :static)
     (dotimes (i 50)
       (fast-write-byte 0 buffer)))))

 ;; Fast-io streams

(bench (1000000)
  (let ((stream (make-instance 'fast-output-stream))
        (vec (make-octet-vector 50)))
    (write-sequence vec stream)))
