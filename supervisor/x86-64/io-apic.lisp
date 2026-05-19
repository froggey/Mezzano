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

(sys.int::defglobal *io-apics* nil)
(sys.int::defglobal *io-apic-active-p* nil)

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
  "Read the two 32-bit halves of a redirection table entry."
  (let ((low (io-apic-read apic (+ +io-apic-redirection+ (* entry 2))))
        (high (io-apic-read apic (+ +io-apic-redirection+ (* entry 2) 1))))
    (logior low (ash high 32))))

(defun io-apic-write-redirection (apic entry value)
  "Write a 64-bit value to a redirection table entry."
  (io-apic-write apic (+ +io-apic-redirection+ (* entry 2))
                 (ldb (byte 32 0) value))
  (io-apic-write apic (+ +io-apic-redirection+ (* entry 2) 1)
                 (ldb (byte 32 32) value)))

(defun io-apic-mask-irq (gsi)
  "Mask an IO-APIC IRQ by GSI."
  (dolist (apic *io-apics*)
    (let ((entry (- gsi (io-apic-gsi-base apic))))
      (when (and (<= 0 entry) (< entry (io-apic-max-redirection apic)))
        (setf (io-apic-read-redirection apic entry)
              (logior (io-apic-read-redirection apic entry)
                      +io-apic-entry-mask+))
        (return t)))))

(defun io-apic-unmask-irq (gsi)
  "Unmask an IO-APIC IRQ by GSI."
  (dolist (apic *io-apics*)
    (let ((entry (- gsi (io-apic-gsi-base apic))))
      (when (and (<= 0 entry) (< entry (io-apic-max-redirection apic)))
        (setf (io-apic-read-redirection apic entry)
              (logand (io-apic-read-redirection apic entry)
                      (lognot +io-apic-entry-mask+)))
        (return t)))))

(defun io-apic-find-entry (gsi)
  "Find the IO-APIC and redirection entry index for a GSI."
  (dolist (apic *io-apics*)
    (let ((entry (- gsi (io-apic-gsi-base apic))))
      (when (and (<= 0 entry) (< entry (io-apic-max-redirection apic)))
        (return (values apic entry))))))

(defun io-apic-irq-spurious-p (gsi)
  (declare (ignore gsi))
  nil)

(defun initialize-io-apics ()
  "Initialize all IO-APICs from ACPI MADT entries."
  (when *io-apics*
    (return-from initialize-io-apics))
  (let ((madt (acpi-get-table 'acpi-madt-table-p)))
    (unless madt
      (debug-print-line "No MADT table, IO-APIC not available.")
      (return-from initialize-io-apics))
    (setf *io-apics* '())
    (dotimes (i (sys.int::simple-vector-length
                 (acpi-madt-table-controllers madt)))
      (let ((entry (svref (acpi-madt-table-controllers madt) i)))
        (when (acpi-madt-ioapic-p entry)
          (let* ((phys-addr (acpi-madt-ioapic-address entry))
                 (mmio-base (map-physical-memory-early
                             (align-down phys-addr +4k-page-size+)
                             +4k-page-size+
                             "IO-APIC"))
                 (io-apic (make-io-apic
                           :id (acpi-madt-ioapic-id entry)
                           :gsi-base (acpi-madt-ioapic-global-system-interrupt-base entry)
                           :address phys-addr
                           :mmio-base (+ mmio-base (- phys-addr (align-down phys-addr +4k-page-size+))))))
            ;; Read version to get max redirection entries.
            (let* ((version-reg (io-apic-read io-apic +io-apic-version+))
                   (max-redir (ldb (byte 8 16) version-reg)))
              (setf (io-apic-max-redirection io-apic) max-redir))
            ;; Mask all redirection entries.
            (dotimes (e (io-apic-max-redirection io-apic))
              (io-apic-write-redirection io-apic e +io-apic-entry-mask+))
            (push-wired io-apic *io-apics*)
            (debug-print-line "IO-APIC " (io-apic-id io-apic)
                              " at " phys-addr
                              " GSI base " (io-apic-gsi-base io-apic)
                              " max redirect " (io-apic-max-redirection io-apic))))))
    (setf *io-apic-active-p* (not (null *io-apics*)))
    (when *io-apic-active-p*
      (debug-print-line "IO-APIC initialized, " (length *io-apics*) " controller(s) active.")
      ;; Hook IO-APIC interrupt handler for all vectors 32-255.
      ;; i8259 hooks are still active for vectors 32-47, but the
      ;; IO-APIC's GSI routing determines which handler fires.
      (dotimes (v (- 256 32))
        (let ((vector (+ 32 v)))
          (hook-user-interrupt vector 'io-apic-interrupt-handler))))))

(defun io-apic-interrupt-handler (interrupt-frame info)
  (let ((gsi (- info 32)))
    (irq-deliver interrupt-frame (platform-irq gsi))
    (lapic-eoi)))

(defun io-apic-configure-entry (gsi vector destination-apic-id
                                &key (trigger-mode :edge) (polarity :high) (masked t))
  "Configure an IO-APIC redirection entry."
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
          ;; x2APIC: 32-bit APIC ID in bits 63-32.
          (setf entry-value (logior entry-value (ash destination-apic-id 32)))
          ;; xAPIC: 8-bit APIC ID in bits 63-56.
          (setf entry-value (logior entry-value (ash (ldb (byte 8 0) destination-apic-id) 56))))
      (io-apic-write-redirection apic entry entry-value)
      t)))
