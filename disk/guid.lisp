;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

(in-package :mezzano.disk)

(defun get-guid (buf &optional (base 0))
  (let ((result (make-string 36 :initial-element #\X)))
    (setf (char result  8) #\-
          (char result 13) #\-
          (char result 18) #\-
          (char result 23) #\-)
    (loop
       for buf-idx from 3 downto 0
       for result-start from 0 upto 6 by 2
       for result-end from 2 upto 8 by 2
       do (setf (subseq result result-start result-end)
                (format nil "~2,'0X" (aref buf (+ base buf-idx)))))
    (loop
       for buf-idx from 5 downto 4
       for result-start from 9 upto 11 by 2
       for result-end from 11 upto 13 by 2
       do (setf (subseq result result-start result-end)
                (format nil "~2,'0X" (aref buf (+ base buf-idx)))))
    (loop
       for buf-idx from 7 downto 6
       for result-start from 14 upto 16 by 2
       for result-end from 16 upto 18 by 2
       do (setf (subseq result result-start result-end)
                (format nil "~2,'0X" (aref buf (+ base buf-idx)))))
    (loop
       for buf-idx from 8 upto 9
       for result-start from 19 upto 21 by 2
       for result-end from 21 upto 23 by 2
       do (setf (subseq result result-start result-end)
                (format nil "~2,'0X" (aref buf (+ base buf-idx)))))
    (loop
       for buf-idx from 10 upto 15
       for result-start from 24 upto 34 by 2
       for result-end from 26 upto 36 by 2
       do (setf (subseq result result-start result-end)
                (format nil "~2,'0X" (aref buf (+ base buf-idx)))))
    result))

(defun (setf get-guid) (value buf &optional (base 0))
  (flet ((hex-to-byte (c1 c2)
           (dpb (digit-char-p c1 16)
                (byte 4 4)
                (digit-char-p c2 16))))
    (loop
       for buf-idx from 3 downto 0
       for start from 0 upto 6 by 2
       do (setf (aref buf (+ base buf-idx))
                (hex-to-byte (char value start) (char value (1+ start)))))
    (loop
       for buf-idx from 5 downto 4
       for start from 9 upto 11 by 2
       do (setf (aref buf (+ base buf-idx))
                (hex-to-byte (char value start) (char value (1+ start)))))
    (loop
       for buf-idx from 7 downto 6
       for start from 14 upto 16 by 2
       do (setf (aref buf (+ base buf-idx))
                (hex-to-byte (char value start) (char value (1+ start)))))

    (loop
       for buf-idx from 8 upto 9
       for start from 19 upto 21 by 2
       do (setf (aref buf (+ base buf-idx))
                (hex-to-byte (char value start) (char value (1+ start)))))
    (loop
       for buf-idx from 10 upto 15
       for start from 24 upto 34 by 2
       do (setf (aref buf (+ base buf-idx))
                (hex-to-byte (char value start) (char value (1+ start))))))
  value
  buf)
