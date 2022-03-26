(in-package :fast-io.test)

(defmacro wo (&body body)
  `(with-fast-output (b)
     ,@body))

(defmacro wi (seq &body body)
  `(with-fast-input (b (octets-from ,seq))
     ,@body))

(defmacro write-all (bits)
  (let ((fun (mapcar (lambda (m) (symbolicate (format nil m bits)))
                     '("WRITE~A-BE" "WRITEU~A-BE"
                       "WRITE~A-LE" "WRITEU~A-LE")))
        (unsigned (- (expt 2 bits) 2))
        (signed (- (truncate (expt 2 bits) 2))))
    `(values
      (wo (,(first fun) ,signed b))
      (wo (,(second fun) ,unsigned b))
      (wo (,(third fun) ,signed b))
      (wo (,(fourth fun) ,unsigned b)))))

(defmacro read-all (bits)
  (let ((fun (mapcar (lambda (m) (symbolicate (format nil m bits)))
                     '("READ~A-BE" "READU~A-BE"
                       "READ~A-LE" "READU~A-LE"))))
    `(let ((bytes (multiple-value-list (write-all ,bits))))
         (values
          (wi (first bytes) (,(first fun) b))
          (wi (second bytes) (,(second fun) b))
          (wi (third bytes) (,(third fun) b))
          (wi (fourth bytes) (,(fourth fun) b))))))

(check (:name :octets)
  (results
   (make-octet-vector 4)
   (octets-from '(1 2 3 4))
   (octets-from #(4 3 2 1))))

(check (:name :write-bytes :output-p t)
  (results
   (wo (fast-write-byte 1 b))
   (wo (fast-write-sequence (octets-from '(1 2 3 4)) b))))

(check (:name :write-endian :output-p t)
  (results
   (write-all 8)
   (write-all 16)
   (write-all 32)
   (write-all 64)
   (write-all 128)))

(check (:name :read-bytes :output-p t)
  (results
   (wi '(1) (fast-read-byte b))
   (wi '(1 2 3 4)
     (let ((vec (make-octet-vector 4)))
       (fast-read-sequence vec b)
       vec))))

(check (:name :read-endian :output-p t)
  (results
   (read-all 8)
   (read-all 16)
   (read-all 32)
   (read-all 64)
   (read-all 128)))
