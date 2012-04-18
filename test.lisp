(in-package #:sys.int)

(defvar *isa-pic-shadow-mask* #xFFFF)

(defun isa-pic-irq-mask (irq)
  (check-type irq (integer 0 16))
  (logtest (ash 1 irq) *isa-pic-shadow-mask*))

(defun (setf isa-pic-irq-mask) (value irq)
  (check-type irq (integer 0 16))
  (setf (ldb (byte 1 irq) *isa-pic-shadow-mask*)
        (if value 1 0))
  (if (< irq 8)
      ;; Master PIC.
      (setf (io-port/8 #x21) (ldb (byte 8 0) *isa-pic-shadow-mask*))
      ;; Slave PIC.
      (setf (io-port/8 #xA1) (ldb (byte 8 8) *isa-pic-shadow-mask*)))
  value)

(defvar *isa-pic-handlers* (make-array 16 :initial-element nil :area :static))
(defvar *isa-pic-base-handlers* (make-array 16 :initial-element nil))

(dotimes (i 16)
  (multiple-value-bind (mc pool)
      (sys.lap-x86:assemble `((sys.lap-x86:push :rax)
                              (sys.lap-x86:push :rcx)
                              (sys.lap-x86:push :rdx)
                              (sys.lap-x86:push :rbp)
                              (sys.lap-x86:push :rsi)
                              (sys.lap-x86:push :rdi)
                              (sys.lap-x86:mov64 :rax (:constant ,*isa-pic-handlers*))
                              (sys.lap-x86:cmp64 (:rax ,(+ (- +tag-array-like+) 8 (* i 8))) nil)
                              (sys.lap-x86:je over)
                              (sys.lap-x86:call (:rax ,(+ (- +tag-array-like+) 8 (* i 8))))
                              over
                              (sys.lap-x86:mov8 :al #x20)
                              (sys.lap-x86:out8 #x20)
                              ,@(when (>= i 8)
                                  `((sys.lap-x86:mov8 :al #x20)
                                    (sys.lap-x86:out8 #xA0)))
                              (sys.lap-x86:pop :rdi)
                              (sys.lap-x86:pop :rsi)
                              (sys.lap-x86:pop :rbp)
                              (sys.lap-x86:pop :rdx)
                              (sys.lap-x86:pop :rcx)
                              (sys.lap-x86:pop :rax)
                              (sys.lap-x86:iret))
        :base-address 12
        :initial-symbols (list (cons nil (lisp-object-address nil)))
        :info (list (list 'isa-pic-irq-handler i)))
    (setf (aref *isa-pic-base-handlers* i) (make-function mc pool))))

(defun isa-pic-interrupt-handler (irq)
  (aref *isa-pic-handlers* irq))

(defun (setf isa-pic-interrupt-handler) (value irq)
  (check-type value (or null function))
  (setf (aref *isa-pic-handlers* irq) value))

(defconstant +isa-pic-interrupt-base+ #x30)

(defun set-idt-entry (entry &key (offset 0) (segment #x0008)
                      (present t) (dpl 0) (ist nil)
                      (interrupt-gate-p t))
  (check-type entry (unsigned-byte 8))
  (check-type offset (signed-byte 64))
  (check-type segment (unsigned-byte 16))
  (check-type dpl (unsigned-byte 2))
  (check-type ist (or null (unsigned-byte 3)))
  (let ((value 0))
    (setf (ldb (byte 16 48) value) (ldb (byte 16 16) offset)
          (ldb (byte 1 47) value) (if present 1 0)
          (ldb (byte 2 45) value) dpl
          (ldb (byte 4 40) value) (if interrupt-gate-p
                                      #b1110
                                      #b1111)
          (ldb (byte 3 16) value) (or ist 0)
          (ldb (byte 16 16) value) segment
          (ldb (byte 16 0) value) (ldb (byte 16 0) offset))
    (setf (aref *idt* (* entry 2)) value
          (aref *idt* (1+ (* entry 2))) (ldb (byte 32 32) offset))))

(defun init-isa-pic ()
  ;; Hook into the IDT.
  (dotimes (i 16)
    (set-idt-entry (+ +isa-pic-interrupt-base+ i)
                   :offset (lisp-object-address (aref *isa-pic-base-handlers* i))))
  ;; Initialize the ISA PIC.
  (setf (io-port/8 #x20) #x11
        (io-port/8 #xA0) #x11
        (io-port/8 #x21) +isa-pic-interrupt-base+
        (io-port/8 #xA1) (+ +isa-pic-interrupt-base+ 8)
        (io-port/8 #x21) #x04
        (io-port/8 #xA1) #x02
        (io-port/8 #x21) #x01
        (io-port/8 #xA1) #x01
        ;; Mask all IRQs except for the cascade IRQ (2).
        (io-port/8 #x21) #xFF
        (io-port/8 #xA1) #xFF
        *isa-pic-shadow-mask* #xFFFF
        (isa-pic-irq-mask 2) nil))

(add-hook '*early-initialize-hook* 'init-isa-pic)

(defun ldb-exception (stack-frame)
  (mumble-string "In LDB.")
  (dotimes (i 32)
    (mumble-string " ")
    (mumble-hex (memref-unsigned-byte-64 stack-frame i)))
  (mumble-string ". Halted.")
  (loop (%hlt)))

(defvar *exception-base-handlers* (make-array 32 :initial-element nil))
(define-lap-function %%exception ()
  (sys.lap-x86:push :rax)
  (sys.lap-x86:push :rbx)
  (sys.lap-x86:push :rcx)
  (sys.lap-x86:push :rdx)
  (sys.lap-x86:push :rbp)
  (sys.lap-x86:push :rsi)
  (sys.lap-x86:push :rdi)
  (sys.lap-x86:push :r8)
  (sys.lap-x86:push :r9)
  (sys.lap-x86:push :r10)
  (sys.lap-x86:push :r11)
  (sys.lap-x86:push :r12)
  (sys.lap-x86:push :r13)
  (sys.lap-x86:push :r14)
  (sys.lap-x86:push :r15)
  (sys.lap-x86:mov64 :r8 :rsp)
  (sys.lap-x86:shl64 :r8 3)
  (sys.lap-x86:test64 :rsp #b1000)
  (sys.lap-x86:jz already-aligned)
  (sys.lap-x86:push 0)
  already-aligned
  (sys.lap-x86:mov32 :ecx 8)
  ;; FIXME: Should switch to a secondary data stack.
  (sys.lap-x86:mov64 :r13 (:constant ldb-exception))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :rsp :r8)
  (sys.lap-x86:pop :r15)
  (sys.lap-x86:pop :r14)
  (sys.lap-x86:pop :r13)
  (sys.lap-x86:pop :r12)
  (sys.lap-x86:pop :r11)
  (sys.lap-x86:pop :r10)
  (sys.lap-x86:pop :r9)
  (sys.lap-x86:pop :r8)
  (sys.lap-x86:pop :rdi)
  (sys.lap-x86:pop :rsi)
  (sys.lap-x86:pop :rbp)
  (sys.lap-x86:pop :rdx)
  (sys.lap-x86:pop :rcx)
  (sys.lap-x86:pop :rbx)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:add64 :rsp 16)
  (sys.lap-x86:iret))

(dotimes (i 32)
  (multiple-value-bind (mc pool)
      (sys.lap-x86:assemble `(,@(unless (member i '(8 10 11 12 13 14 17))
                                  `((sys.lap-x86:push 0)))
                              (sys.lap-x86:push ,i)
                              (sys.lap-x86:jmp (:constant ,#'%%exception)))
        :base-address 12
        :initial-symbols (list (cons nil (lisp-object-address nil)))
        :info (list (list 'exception-handler i)))
    (let ((fn (make-function mc pool)))
      (setf (aref *exception-base-handlers* i) fn)
      (set-idt-entry i :offset (lisp-object-address fn)))))

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

(defvar *early-initialize-hook* '())
(defvar *initialize-hook* '())

(defun add-hook (hook function)
  (unless (boundp hook)
    (setf (symbol-value hook) '()))
  (pushnew function (symbol-value hook)))

(defun initialize-lisp ()
  (setf *package* (find-package "CL-USER"))
  (mapc 'funcall *early-initialize-hook*)
  (%sti)
  (pci-init)
  (mapc 'funcall *initialize-hook*)
  (write-string "Hello, World!")
  (repl))

(initialize-lisp)
