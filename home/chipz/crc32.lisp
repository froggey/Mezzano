;;;; crc32.lisp -- implementation of the CRC32 checksum

(in-package :chipz)

(defstruct (crc32
             (:copier copy-crc32))
  (crc #xffffffff :type (unsigned-byte 32)))

(defun update-crc32 (state vector start end)
  (declare (type simple-octet-vector vector))
  (declare (type index start end))
  (do ((crc (crc32-crc state))
       (i start (1+ i))
       (table +crc32-table+))
      ((>= i end)
       (setf (crc32-crc state) crc)
       state)
    (declare (type (unsigned-byte 32) crc))
    (setf crc (logxor (aref table
                            (logand (logxor crc (aref vector i)) #xff))
                      (ash crc -8)))))

(defun produce-crc32 (state)
  (logxor #xffffffff (crc32-crc state)))
