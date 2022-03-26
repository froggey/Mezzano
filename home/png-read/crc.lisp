(in-package :png-read)

(defun make-crc-array ()
  (let ((crc-array (make-array 256 :initial-element 0)))
   (iter (for n from 0 below 256)
         (let ((c n))
           (iter (for k from 0 below 8)
                 (if (not (zerop (logand c 1)))
                     (setf c (logxor #xedb88320 (ash c -1)))
                     (setf c (ash c -1))))
           (setf (aref crc-array n) c)))
   crc-array))

(defvar *crc-array* (make-crc-array))

(defun updated-crc (crc data)
  (reduce #'(lambda (c d)
              (logxor (aref *crc-array* (logand (logxor c d) #xff)) (ash c -8)))
          data :initial-value crc))

(defun start-crc (data)
  (updated-crc #xffffffff data))

(defun finish-crc (crc)
  (logxor crc #xffffffff))

(defun crc (data)
  (finish-crc (start-crc data)))