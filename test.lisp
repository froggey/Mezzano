(in-package #:sys.int)

;;; Initialize the ISA PIC.
(setf (io-port/8 #x20) #x11
      (io-port/8 #xA0) #x11
      (io-port/8 #x21) #x30
      (io-port/8 #xA1) #x38
      (io-port/8 #x21) #x04
      (io-port/8 #xA1) #x02
      (io-port/8 #x21) #x01
      (io-port/8 #xA1) #x01
      ;; Mask all IRQs except for the cascade IRQ (2).
      (io-port/8 #x21) #b11111011
      (io-port/8 #xA1) #b11111111)

(defun sys.int::simplify-string (string)
  (if (simple-string-p string)
      string
      (make-array (length string)
                  :element-type (if (every 'sys.int::base-char-p string)
                                    'base-char
                                    'character)
                  :initial-contents string)))

(defconstant +ata-status-err+ #b00000001)
(defconstant +ata-status-drq+ #b00001000)
(defconstant +ata-status-bsy+ #b10000000)

(defun dump-image ()
  (when (not (yes-or-no-p "Danger! This will overwrite the primary master hard disk. Continue?"))
    (return-from dump-image))
  (format t "Saving image... ")
  ;; Rebind *INITIAL-FUNCTION* using unwind-protect/symbol-value
  ;; to avoid the TLS mechanism.
  (let ((old-initial *initial-function*)
        (old-data-end (aref *multiboot-header* 5))
        (old-bss-size (aref *multiboot-header* 6)))
    (unwind-protect
         ;; Write all memory from #x1FF000 up to *BUMP-POINTER* to
         ;; the primary master.
         (let* ((start #x1FF000)
                (end (logand (+ (- *bump-pointer* #x8000000000) #x1000)
                             (lognot #xFFF)))
                (count (truncate (- end start) 512)))
           ;; Boot directly to the REPL.
           (setf (symbol-value '*initial-function*) #'initialize-lisp)
           ;; Adjust the sizes in the multiboot header.
           (setf (aref *multiboot-header* 5) end
                 (aref *multiboot-header* 6) 0)
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
           count)
      (setf (symbol-value '*initial-function*) old-initial
            (aref *multiboot-header* 5) old-data-end
            (aref *multiboot-header* 6) old-bss-size))))

(defun repl ()
  (loop
     (with-simple-restart (abort "Return to READ-EVAL-PRINT loop.")
       (fresh-line)
       (write-char #\>)
       (let ((form (read)))
         (fresh-line)
         (let ((result (multiple-value-list (eval form))))
           (if result
               (dolist (v result)
                 (fresh-line)
                 (write v))
               (progn
                 (fresh-line)
                 (write-string "; No values."))))))))

(defvar *initialize-hooks* '())

(defun add-hook (hook function)
  (unless (boundp hook)
    (setf (symbol-value hook) '()))
  (pushnew function (symbol-value hook)))

(defun initialize-lisp ()
  (setf *package* (find-package "CL-USER"))
  (pci-init)
  (dolist (i *initialize-hooks*)
    (funcall i))
  (write-string "Hello, World!")
  (repl))

(initialize-lisp)
