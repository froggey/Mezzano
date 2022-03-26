;;; adler32.lisp -- computing adler32 checksums (rfc1950)

(in-package :chipz)

(defstruct (adler32
             (:copier copy-adler32))
  (s1 1 :type fixnum)
  (s2 0 :type fixnum))

(defun update-adler32 (state vector start end)
  (declare (type simple-octet-vector vector))
  (declare (type index start end))
  ;; many thanks to Xach for his code from Salza.
  (let ((length (- end start))
        (i 0)
        (k 0)
        (s1 (adler32-s1 state))
        (s2 (adler32-s2 state)))
    (declare (type index i k length)
             (type fixnum s1 s2))
    (unless (zerop length)
      (tagbody
       loop
         (setf k (min 16 length))
         (decf length k)
       sum
         (setf s1 (+ (aref vector (+ start i)) s1))
         (setf s2 (+ s1 s2))
         (decf k)
         (incf i)
         (unless (zerop k)
           (go sum))
         (setf s1 (mod s1 adler32-modulo))
         (setf s2 (mod s2 adler32-modulo))
         (unless (zerop length)
           (go loop))
       end
         (setf (adler32-s1 state) s1
               (adler32-s2 state) s2)
         (return-from update-adler32 state)))))

(defun produce-adler32 (state)
  (logior (ash (adler32-s2 state) 16)
          (adler32-s1 state)))
