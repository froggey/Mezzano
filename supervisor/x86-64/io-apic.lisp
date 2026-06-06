(in-package :mezzano.supervisor)

;; IO-APIC MMIO registers.
(defconstant +io-apic-reg-index+ 0)
(defconstant +io-apic-reg-data+  #x10)

;; IO-APIC register offsets.
(defconstant +io-apic-id+            #x00)
(defconstant +io-apic-version+       #x01)
(defconstant +io-apic-redirection+   #x10)

;; Redirection entry bit fields.
(defconstant +io-apic-entry-mask+            #x00010000)
(defconstant +io-apic-entry-trigger-mode+    #x00008000)
(defconstant +io-apic-entry-polarity+        #x00002000)
(defconstant +io-apic-entry-destination-mode+ #x00000800)
(defconstant +io-apic-entry-delIVery-mode+   #x00000700)
(defconstant +io-apic-entry-vector+          #x000000FF)

(defconstant +io-apic-base-vector+ 48)

(sys.int::defglobal *io-apics* nil)
(sys.int::defglobal *io-apic-active-p* nil)
(sys.int::defglobal *io-apic-irqs* nil)
(sys.int::defglobal *isa-irq-to-gsi* nil)
(sys.int::defglobal *gsi-flags* nil)

(defstruct (io-apic
             (:area :wired))
  (id 0 :type (unsigned-byte 8))
  (gsi-base 0 :type (unsigned-byte 32))
  (address 0 :type (unsigned-byte 64))
  (mmio-base 0)
  (max-redirection 0 :type (unsigned-byte 8)))

(defun io-apic-read (apic offset)
  (setf (physical-memref-unsigned-byte-32
         (+ (io-apic-mmio-base apic) +io-apic-reg-index+))
        offset)
  (physical-memref-unsigned-byte-32
   (+ (io-apic-mmio-base apic) +io-apic-reg-data+)))

(defun io-apic-write (apic offset value)
  (setf (physical-memref-unsigned-byte-32
         (+ (io-apic-mmio-base apic) +io-apic-reg-index+))
        offset)
  (setf (physical-memref-unsigned-byte-32
         (+ (io-apic-mmio-base apic) +io-apic-reg-data+))
        value))

(defun io-apic-read-redirection (apic entry)
  (let ((low (io-apic-read apic (+ +io-apic-redirection+ (* entry 2))))
        (high (io-apic-read apic (+ +io-apic-redirection+ (* entry 2) 1))))
    (logior low (ash high 32))))

(defun io-apic-write-redirection (apic entry value)
  (io-apic-write apic (+ +io-apic-redirection+ (* entry 2))
                 (ldb (byte 32 0) value))
  (io-apic-write apic (+ +io-apic-redirection+ (* entry 2) 1)
                 (ldb (byte 32 32) value)))

(defun io-apic-mask-irq (gsi)
  (dolist (apic *io-apics*)
    (let ((entry (- gsi (io-apic-gsi-base apic))))
      (when (and (<= 0 entry) (<= entry (io-apic-max-redirection apic)))
        (io-apic-write-redirection apic entry
                                   (logior (io-apic-read-redirection apic entry)
                                           +io-apic-entry-mask+))
        (return t)))))

(defun io-apic-unmask-irq (gsi)
  (dolist (apic *io-apics*)
    (let ((entry (- gsi (io-apic-gsi-base apic))))
      (when (and (<= 0 entry) (<= entry (io-apic-max-redirection apic)))
        (io-apic-write-redirection apic entry
                                   (logand (io-apic-read-redirection apic entry)
                                           (lognot +io-apic-entry-mask+)))
        (return t)))))

(defun io-apic-find-entry (gsi)
  (dolist (apic *io-apics*)
    (let ((entry (- gsi (io-apic-gsi-base apic))))
      (when (and (<= 0 entry) (<= entry (io-apic-max-redirection apic)))
        (return (values apic entry))))))

(defun io-apic-irq-spurious-p (gsi)
  (declare (ignore gsi))
  nil)

(defun gsi-flags-polarity (flags)
  (if (eql (ldb (byte 2 0) flags) 3) :low :high))

(defun gsi-flags-trigger (flags)
  (if (eql (ldb (byte 2 2) flags) 3) :level :edge))

(defun initialize-io-apics ()
  (setf *io-apic-active-p* nil)
  (unless (boundp '*io-apics*)
    (setf *io-apics* nil))
  (when *io-apics*
    (return-from initialize-io-apics))
  (setf *io-apic-irqs* nil)
  (setf *isa-irq-to-gsi* nil)
  (setf *gsi-flags* nil)
  (let ((madt (acpi-get-table 'acpi-madt-table-p)))
    (unless madt
      (debug-print-line "No MADT table, IO-APIC not available.")
      (return-from initialize-io-apics))
    (let ((isa-mapping (sys.int::make-simple-vector 16 :wired)))
      (setf *gsi-flags* (sys.int::make-simple-vector 256 :wired))
      (dotimes (i 16)
        (setf (svref isa-mapping i) i))
      (dotimes (i (sys.int::simple-vector-length
                   (acpi-madt-table-controllers madt)))
        (let ((entry (svref (acpi-madt-table-controllers madt) i)))
          (when (acpi-madt-interrupt-source-override-p entry)
            (when (and (eql (acpi-madt-interrupt-source-override-bus entry) 0)
                       (< (acpi-madt-interrupt-source-override-source entry) 16))
              (setf (svref isa-mapping
                           (acpi-madt-interrupt-source-override-source entry))
                    (acpi-madt-interrupt-source-override-global-system-interrupt entry))
              (let ((gsi (acpi-madt-interrupt-source-override-global-system-interrupt entry))
                    (flags (acpi-madt-interrupt-source-override-flags entry)))
                (when (< gsi 256)
                  (setf (svref *gsi-flags* gsi) flags)))
              (debug-print-line "MADT override: ISA IRQ "
                                (acpi-madt-interrupt-source-override-source entry)
                                " -> GSI "
                                (acpi-madt-interrupt-source-override-global-system-interrupt entry))))))
      (setf *isa-irq-to-gsi* isa-mapping))
    (setf *io-apic-irqs* (sys.int::make-simple-vector 256 :wired))
    (dotimes (i 256)
      (setf (svref *io-apic-irqs* i) (make-irq :platform-number i)))
    (setf *io-apics* '())
    (let ((bsp-apic-id (if (boundp '*bsp-cpu*)
                           (x86-64-cpu-apic-id *bsp-cpu*)
                           0)))
      (dotimes (i (sys.int::simple-vector-length
                   (acpi-madt-table-controllers madt)))
        (let ((entry (svref (acpi-madt-table-controllers madt) i)))
          (when (acpi-madt-ioapic-p entry)
            (let* ((phys-addr (acpi-madt-ioapic-address entry))
                   (page-base (align-down phys-addr +4k-page-size+)))
              (map-physical-memory-early page-base +4k-page-size+ "IO-APIC")
              (let ((io-apic (make-io-apic
                              :id (acpi-madt-ioapic-id entry)
                              :gsi-base (acpi-madt-ioapic-global-system-interrupt-base entry)
                              :address phys-addr
                              :mmio-base phys-addr)))
                (let* ((version-reg (io-apic-read io-apic +io-apic-version+))
                       (max-redir (ldb (byte 8 16) version-reg))
                       (n-entries (1+ max-redir))
                       (gsi-base (io-apic-gsi-base io-apic)))
                  (setf (io-apic-max-redirection io-apic) max-redir)
                  (push-wired io-apic *io-apics*)
                  (dotimes (e n-entries)
                    (io-apic-write-redirection io-apic e +io-apic-entry-mask+)
                     (let ((gsi (+ gsi-base e))
                           (flags (svref *gsi-flags* (+ gsi-base e))))
                       (io-apic-configure-entry gsi
                                                (+ +io-apic-base-vector+ gsi)
                                                bsp-apic-id
                                                :trigger-mode (gsi-flags-trigger flags)
                                                :polarity (gsi-flags-polarity flags)
                                                :masked t))))
                (debug-print-line "IO-APIC " (io-apic-id io-apic)
                                  " at " phys-addr
                                  " GSI base " (io-apic-gsi-base io-apic)
                                  " max redirect " (io-apic-max-redirection io-apic))))))))
    (when (not (null *io-apics*))
      (debug-print-line "IO-APIC init done. Hooking handlers...")
      (let ((max-gsi 0))
        (dolist (apic *io-apics*)
          (let ((top (+ (io-apic-gsi-base apic) (io-apic-max-redirection apic))))
            (setf max-gsi (max max-gsi top))))
        (dotimes (gsi (1+ max-gsi))
          (let ((vector (+ +io-apic-base-vector+ gsi)))
            (when (< vector 128)
              (hook-user-interrupt vector 'io-apic-interrupt-handler))))))
    (setf *io-apic-active-p* t)
    ;; Mask the i8259 PIC now that IO-APIC is handling interrupts.
    (when (boundp '*i8259-shadow-mask*)
      (setf (sys.int::io-port/8 #x21) #xFF
            (sys.int::io-port/8 #xA1) #xFF
            *i8259-shadow-mask* #xFFFF))
    nil))

(defun io-apic-interrupt-handler (interrupt-frame info)
  (let ((gsi (- info +io-apic-base-vector+)))
    (when (and (<= 0 gsi) (< gsi 256))
      (irq-deliver interrupt-frame (svref *io-apic-irqs* gsi)))
    (lapic-eoi)))

(defun io-apic-configure-entry (gsi vector destination-apic-id
                                &key (trigger-mode :edge) (polarity :high) (masked t))
  (multiple-value-bind (apic entry) (io-apic-find-entry gsi)
    (unless apic
      (debug-print-line "No IO-APIC for GSI " gsi)
      (return-from io-apic-configure-entry nil))
    (let ((entry-value (logior vector
                               (if masked +io-apic-entry-mask+ 0)
                               (ecase trigger-mode
                                 (:edge 0)
                                 (:level +io-apic-entry-trigger-mode+))
                               (ecase polarity
                                 (:high 0)
                                 (:low +io-apic-entry-polarity+)))))
      (if *lapic-x2apic-mode*
          (setf entry-value (logior entry-value (ash destination-apic-id 32)))
          (setf entry-value (logior entry-value (ash (ldb (byte 8 0) destination-apic-id) 56))))
      (io-apic-write-redirection apic entry entry-value)
      t)))
