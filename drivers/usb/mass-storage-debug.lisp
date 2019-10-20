(in-package :mezzano.driver.usb.mass)

(defun get-be-unsigned-word/16 (buf offset)
  (dpb (aref buf offset) (byte 8 8) (aref buf (1+ offset))))

(defun get-be-unsigned-word/32 (buf offset)
  (dpb (aref buf offset)
       (byte 8 24)
       (dpb (aref buf (+ offset 1))
            (byte 8 16)
            (dpb (aref buf (+ offset 2))
                 (byte 8 8)
                 (aref buf (+ offset 3))))))


(defun print-cb (stream buf length &key (offset 0) (indent ""))
  (flet ((check-length (value)
           (when (/= length value)
             (format stream "~AWarning unexpected length ~D expected ~D~%"
                     indent
                     length
                     value))
           (>= length value)))
    (cond ((and (= (aref buf offset) #x00) (check-length 6))
           ;; Test unit ready command
           (format stream "~AUnit Ready?" indent))
          ((and (= (aref buf offset) #x28) (check-length 10))
           ;; Read (10) command
           (let ((address (get-be-unsigned-word/32 buf (+ offset 2)))
                 (xfer-length (get-be-unsigned-word/16 buf (+ offset 7))))
             (format stream "~ARead Address: #x~8,'0X (~D), blocks #x~4,'0X (~D)"
                     indent
                     address address
                     xfer-length xfer-length)
             (format stream "~%")))
          (T
           (format stream "~ALength: ~D~%" indent length)
           (format stream "~A" indent)
           (dotimes (i length)
             (format stream "~2,'0X " (aref buf (+ i offset)))
             (when (= (mod i 16) 15)
               (format stream "~%~A" indent)))
           (when (/= (mod length 16) 0)
             (format stream "~%"))))))

(defun print-cbw (stream buf &key (offset 0) (indent ""))
  (let ((signature (get-unsigned-word/32 buf (+ +cbw-signature+ offset)))
        (tag (get-unsigned-word/32 buf (+ +cbw-tag+ offset)))
        (xfer-length (get-unsigned-word/32 buf (+ +cbw-transfer-length+ offset)))
        (flags (aref buf (+ +cbw-flags+ offset)))
        (lun (aref buf (+ +cbw-lun+ offset)))
        (cb-length (aref buf (+ +cbw-cb-length+ offset))))
    (when (/= signature +cbw-signature-value+)
      (format stream
              "~AWarning, invalid signature: #x~8,'0X instead of #x~8,'0X~%"
              indent
              signature
              +cbw-signature-value+))
    (format stream "~ATag: #x~8,'0X, Dir: ~A, Xfer Length: #x~8,'0X, LUN: ~D~%"
            indent
            tag
            (case flags
              (#.+cbw-flags-data-out+ :out)
              (#.+cbw-flags-data-in+ :in)
              (T (format nil "#x~2,'0X (invalid)" flags)))
            xfer-length
            lun)
    (print-cb stream buf cb-length
              :offset (+ offset +cbw-control-block+)
              :indent (increase-indent indent))))

(defun print-cbw/32 (stream buf &key (offset 0) (indent ""))
  (let ((buf/8 (make-array 32 :initial-element 0)))
    (dotimes (i (min (length buf) 8))
      (let ((value (aref buf i))
            (idx (* 4 i)))
        (setf (aref buf/8 idx)       (ldb (byte 8 24) value)
              (aref buf/8 (+ idx 1)) (ldb (byte 8 16) value)
              (aref buf/8 (+ idx 2)) (ldb (byte 8  8) value)
              (aref buf/8 (+ idx 3)) (ldb (byte 8  0) value))))
    (print-cbw stream buf/8 :offset offset :indent indent)))

(defun print-csw (stream buf &key (offset 0) (indent ""))
  (let ((signature (get-unsigned-word/32 buf (+ +csw-signature+ offset)))
        (tag (get-unsigned-word/32 buf (+ +csw-tag+ offset)))
        (residue (get-unsigned-word/32 buf (+ +csw-data-residue+ offset)))
        (status (aref buf (+ +cs2-status+ offset))))
    (when (/= signature +csw-signature-value+)
      (format stream
              "~AWarning, invalid signature: #x~8,'0X instead of #x~8,'0X~%"
              indent
              signature
              +csw-signature-value+))
    (format stream "~ATag: #x~8,'0X, Data Residue: #x~8,'0X (~D), Status: ~A~%"
            indent
            tag
            residue residue
            (case status
              (#.+csw-status-success+ :success)
              (#.+csw-status-cmd-failed+ :failed)
              (#.+csw-phase-error+ :phase-error)
              (T (format nil "#x~2,'0X (invalid)" status))))))

(defun print-csw/32 (stream buf &key (offset 0) (indent ""))
  (let ((buf/8 (make-array 16 :initial-element 0)))
    (dotimes (i (min (length buf) 4))
      (let ((value (aref buf i))
            (idx (* 4 i)))
        (setf (aref buf/8 idx)       (ldb (byte 8 24) value)
              (aref buf/8 (+ idx 1)) (ldb (byte 8 16) value)
              (aref buf/8 (+ idx 2)) (ldb (byte 8  8) value)
              (aref buf/8 (+ idx 3)) (ldb (byte 8  0) value))))
    (print-csw stream buf/8 :offset offset :indent indent)))
