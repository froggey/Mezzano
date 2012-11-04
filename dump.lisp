(in-package #:sys.int)

(defconstant +ata-status-err+ #b00000001)
(defconstant +ata-status-drq+ #b00001000)
(defconstant +ata-status-bsy+ #b10000000)

(defun dump-image ()
  (when (not (yes-or-no-p "Danger! This will overwrite the primary master hard disk. Continue?"))
    (return-from dump-image))
  (format t "Saving image... ")
  *bump-pointer*
  ;; Write all memory from #x200000 up to *BUMP-POINTER*.
  ;; the primary master.
  (let* ((start #x200000)
         (end (* (ceiling (- *bump-pointer* #x8000000000) 512) 512))
         (count (truncate (- end start) 512)))
    ;; Reconfigure the multiboot header for the larger image.
    (setf (aref *multiboot-header* 5) (- *bump-pointer* #x8000000000)
          (aref *multiboot-header* 6) (- *bump-pointer* #x8000000000))
    (dotimes (sector count)
      (setf (io-port/8 #x1F6) (logior #xE0 (ldb (byte 4 24) sector));drive and high bits of the lba
            (io-port/8 #x1F1) 0 ; ???
            (io-port/8 #x1F2) 1 ; sector count
            (io-port/8 #x1F3) (ldb (byte 8 0) sector)
            (io-port/8 #x1F4) (ldb (byte 8 8) sector)
            (io-port/8 #x1F5) (ldb (byte 8 16) sector)
            (io-port/8 #x1F7) #x30) ;command write sectors
      ;; Wait for BSY to clear and DRQ or ERR to set.
      ;; Assumes that BSY will clear and DRQ will set at the same time.
      (tagbody
       top (let ((status (io-port/8 #x1F7)))
             (when (logtest status +ata-status-bsy+)
               (go top))
             (when (or (logtest status +ata-status-err+) ;ERR set.
                       (not (logtest status +ata-status-drq+))) ;DRQ not set.
               (error "Failed write command for sector ~S. Status: ~X." sector status))))
      ;; HERE WE GO! HERE WE GO!
      (dotimes (i 256)
        (setf (io-port/16 #x1F0) (memref-unsigned-byte-16 (+ #x8000000000 start
                                                             (* sector 512))
                                                          i))))
    (format t "Wrote ~S sectors (~DMiB) to disk.~%" count (truncate (truncate (* count 512) 1024) 1024))
    count))
