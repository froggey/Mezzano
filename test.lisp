(in-package #:sys.int)

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

(defun emergency-halt (message)
  (%cli)
  (dotimes (i (%simple-array-length message))
    (let ((code (logand (char-code (schar message i)) #xFF)))
      (setf (io-port/8 #xE9) code)
      (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 i)
            (logior code #x7000))))
  (loop (%hlt)))

(defun mumble (message)
  (mumble-string message)
  (setf (io-port/8 #xE9) #x0A))

;;; Used while the GC is copying, so no lookup tables.
(defun hexify (nibble)
  (cond ((<= 0 nibble 9)
         (+ nibble (char-code #\0)))
        (t (+ (- nibble 10) (char-code #\A)))))

(defun mumble-string (message)
  (dotimes (i (%simple-array-length message))
    (let ((code (logand (char-code (schar message i)) #xFF)))
      (setf (io-port/8 #xE9) code)
      (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 i)
            (logior code #x7000)))))
(defun mumble-hex (number)
  (dotimes (i 16)
    (setf (io-port/8 #xE9) (hexify (logand (ash number (* -4 (- 15 i))) #b1111)))))

(defun gc-trace (object direction prefix)
  (setf (io-port/8 #xE9) (char-code direction))
  (setf (io-port/8 #xE9) (logand (char-code prefix) #xFF))
  (let ((pointer (%pointer-field object))
        (tag (%tag-field object)))
    (dotimes (i 15)
      (setf (io-port/8 #xE9) (hexify (logand (ash pointer (* -4 (- 14 i))) #b1111))))
    (setf (io-port/8 #xE9) (hexify tag))
    (setf (io-port/8 #xE9) #x0A)))

(declaim (special * ** ***))

(defun repl ()
  (let ((* nil) (** nil) (*** nil))
    (loop
       (with-simple-restart (abort "Return to READ-EVAL-PRINT loop.")
         (fresh-line)
         (write-char #\>)
         (let ((form (read)))
           (fresh-line)
           (let ((result (multiple-value-list (eval form))))
             (setf *** **
                   ** *
                   * (first result))
             (when result
               (dolist (v result)
                 (fresh-line)
                 (write v)))))))))

(defvar *early-initialize-hook* '())
(defvar *initialize-hook* '())

(defun add-hook (hook function)
  (unless (boundp hook)
    (setf (symbol-value hook) '()))
  (pushnew function (symbol-value hook)))

(defun multiboot-module-count ()
  (if (logtest (memref-unsigned-byte-32 *multiboot-info* 0) +multiboot-flag-modules+)
      (memref-unsigned-byte-32 *multiboot-info* 5)
      0))

(defun multiboot-module-info ()
  (if (logtest (memref-unsigned-byte-32 *multiboot-info* 0) +multiboot-flag-modules+)
      (make-array (* (multiboot-module-count) 4)
                  :element-type '(unsigned-byte 32)
                  :memory (+ #x8000000000 (memref-unsigned-byte-32 *multiboot-info* 6)))
      (make-array 0 :element-type '(unsigned-byte 32))))

(defparameter *maximum-multiboot-command-line-length* 100)

(defun multiboot-module (id)
  (assert (< id (multiboot-module-count)))
  (let* ((info (multiboot-module-info))
         (base (aref info (+ (* id 4) 0)))
         (end (aref info (+ (* id 4) 1)))
         (size (- end base))
         (command-line (+ #x8000000000 (aref info (+ (* id 4) 2))))
         (cmdl-len (dotimes (i *maximum-multiboot-command-line-length*
                             *maximum-multiboot-command-line-length*)
                     (when (eql (memref-unsigned-byte-8 command-line i) 0)
                       (return i)))))
    (values (make-array size
                        :element-type '(unsigned-byte 8)
                        :memory (+ #x8000000000 base))
            (make-array cmdl-len
                        :element-type 'base-char
                        :memory command-line))))

(defun multiboot-module-data (id)
  (multiple-value-bind (data command-line)
      (multiboot-module id)
    (declare (ignore command-line))
    data))

(defun multiboot-module-command-line (id)
  (multiple-value-bind (data command-line)
      (multiboot-module id)
    (declare (ignore data))
    command-line))

(defclass multiboot-module-stream (stream-object file-stream)
  ((backing-array :initarg :backing-array)
   (command-line :initarg :command-line)
   (position :initarg :position))
  (:default-initargs :position 0))

(defmethod stream-read-byte ((stream multiboot-module-stream))
  (when (< (slot-value stream 'position) (length (slot-value stream 'backing-array)))
    (prog1 (aref (slot-value stream 'backing-array) (slot-value stream 'position))
      (incf (slot-value stream 'position)))))

(defmethod stream-file-position ((stream multiboot-module-stream))
  (slot-value stream 'position))

(defmethod stream-set-file-position ((stream multiboot-module-stream) new-position)
  (cond ((eql new-position :end)
         (setf (slot-value stream 'position) (length (slot-value stream 'backing-array))))
        (t (setf (slot-value stream 'position) new-position)))
  new-position)

(defmethod stream-element-type* ((stream multiboot-module-stream))
  '(unsigned-byte 8))

(defun make-multiboot-module-stream (id)
  (multiple-value-bind (data command-line)
      (multiboot-module id)
    (make-instance 'multiboot-module-stream
                   :backing-array data
                   :command-line command-line)))

(defun initialize-lisp ()
  (setf *package* (find-package "CL-USER"))
  (mapc 'funcall *early-initialize-hook*)
  (%sti)
  (pci-init)
  (mapc 'funcall *initialize-hook*)
  (format t "Hello, World!~%")
  (setf sys.clos::compile-methods t)
  (sys.clos:compile-methods 'sys.int::stream-read-byte)
  (sys.clos:compile-methods 'sys.int::stream-write-char)
  #+nil(sys.clos::compile-all-generic-functions)
  (with-simple-restart (abort "Stop loading modules.")
    (dotimes (i (multiboot-module-count))
      (with-simple-restart (continue "Skip loading this module.")
        (format t "Loading module ~S.~%" (multiboot-module-command-line i))
        (load (make-multiboot-module-stream i)))))
  (repl))

(initialize-lisp)
