(require :nibbles)

(defun find-file (name image-data)
  (let* ((name-bytes (map 'list
                          #'char-code
                          (concatenate 'string
                                       (string-upcase name)
                                       (if (find #\. name)
                                           ""
                                           ".")
                                       ";1")))
         (name-field (coerce (list* (length name-bytes) name-bytes)
                             '(simple-array (unsigned-byte 8) (*))))
         (name-offset (search name-field image-data)))
    (values (* (nibbles:ub32ref/le image-data (+ (- name-offset 32) 2)) 2048)
            (nibbles:ub32ref/le image-data (+ (- name-offset 32) 10)))))

(defun patch-partition-entry (image entry address length)
  (file-position image (+ #x1be (* entry 16) 8))
  (nibbles:write-ub32/le (truncate address 512) image)
  (nibbles:write-ub32/le (ceiling length 512) image))

(defun update-image (image-path)
  (with-open-file (image image-path :direction :io :element-type '(unsigned-byte 8) :if-does-not-exist :error :if-exists :overwrite)
    ;; Assume all the ISO9660 directory entries are somewhere in the first MB.
    (let ((header-data (make-array #x100000 :element-type '(unsigned-byte 8))))
      (read-sequence header-data image)
      (multiple-value-bind (addr size)
          (find-file "efiusb.img" header-data)
        (patch-partition-entry image 0 addr size))
      (multiple-value-bind (addr size)
          (find-file "bootfs.img" header-data)
        (patch-partition-entry image 1 addr size)
        (sb-ext:run-program "kboot-install"
                            (list "--target=bios"
                                  (format nil "--image=~A" image-path)
                                  (format nil "--offset=~D" addr)
                                  "--path=boot/kboot.bin")
                             :search t :output *standard-output*))
      (multiple-value-bind (addr size)
          (find-file "image" header-data)
        (patch-partition-entry image 2 addr size)))))
