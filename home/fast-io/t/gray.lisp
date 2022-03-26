(in-package :fast-io.test)

(defmacro wos (seq &body body)
  `(let ((s (make-instance 'fast-output-stream))
         (vec (octets-from ,seq)))
     ,@body
     (finish-output-stream s)))

(defmacro wis (seq vec-len &body body)
  `(let ((s (make-instance 'fast-input-stream
                           :vector (octets-from ,seq)))
         (vec (make-octet-vector ,vec-len)))
     ,@body))

(check (:name :write-stream)
  (results
   (wos #(1 2 3) (write-sequence vec s))
   (wos #(1 2 3 4 5)
     (write-sequence vec s :start 1 :end 4))))

(check (:name :read-stream)
  (results
   (wis #(1 2 3) 2
     (results (read-sequence vec s) vec))
   (wis #(1 2 3 4 5) 5
     (results (read-sequence vec s :start 1 :end 4) vec))))
