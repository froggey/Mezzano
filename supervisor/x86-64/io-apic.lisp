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

;; Device interrupt vectors are allocated above +IO-APIC-BASE-VECTOR+.
;; The IPI vectors (#x80 wakeup, #x81 panic, #x82 quiesce, #x83 tlb-shootdown,
;; #x85 reschedule, ...) live in the gap below.  To keep the GSI->vector map
;; bijective (so the handler can recover the GSI from the vector) the range
;; #x80..#x8F is skipped: GSIs whose naive vector would land there are shifted
;; up past it.
(defconstant +io-apic-ipi-gap-start+ #x80)
(defconstant +io-apic-ipi-gap-end+   #x90)

(declaim (inline gsi->vector vector->gsi))
(defun gsi->vector (gsi)
  (let ((v (+ +io-apic-base-vector+ gsi)))
    (if (>= v +io-apic-ipi-gap-start+)
        (+ v (- +io-apic-ipi-gap-end+ +io-apic-ipi-gap-start+))
        v)))

(defun vector->gsi (vector)
  (cond ((>= vector +io-apic-ipi-gap-end+)
         (- vector +io-apic-base-vector+
            (- +io-apic-ipi-gap-end+ +io-apic-ipi-gap-start+)))
        ((< vector +io-apic-ipi-gap-start+)
         (- vector +io-apic-base-vector+))
        (t
         ;; Vector fell in the reserved IPI gap; no valid GSI.
         -1)))

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

(defun process-isa-overrides (controllers n-controllers)
  (let ((mapping (sys.int::make-simple-vector 16 :wired)))
    (dotimes (i 16) (setf (svref mapping i) i))
    (dotimes (i n-controllers)
      (let ((entry (svref controllers i)))
        (when (acpi-madt-interrupt-source-override-p entry)
          (let ((bus (acpi-madt-interrupt-source-override-bus entry))
                (source (acpi-madt-interrupt-source-override-source entry))
                (gsi (acpi-madt-interrupt-source-override-global-system-interrupt entry))
                (flags (acpi-madt-interrupt-source-override-flags entry)))
            (when (and (eql bus 0) (< source 16))
              (setf (svref mapping source) gsi)
              (when (< gsi 256)
                (setf (svref *gsi-flags* gsi) flags))
              (debug-print-line "MADT override: ISA IRQ " source " -> GSI " gsi))))))
    (setf *isa-irq-to-gsi* mapping)))

(defun init-one-io-apic (entry bsp-apic-id)
  (let* ((phys-addr (acpi-madt-ioapic-address entry))
         (id (acpi-madt-ioapic-id entry))
         (gsi-base (acpi-madt-ioapic-global-system-interrupt-base entry)))
    (map-physical-memory-early (align-down phys-addr +4k-page-size+)
                               +4k-page-size+ "IO-APIC")
    (let* ((apic (make-io-apic :id id :gsi-base gsi-base
                               :address phys-addr :mmio-base phys-addr))
           (max-redir (ldb (byte 8 16) (io-apic-read apic +io-apic-version+)))
           (n-entries (1+ max-redir)))
      (setf (io-apic-max-redirection apic) max-redir)
      (push-wired apic *io-apics*)
      (dotimes (e n-entries)
        (io-apic-write-redirection apic e +io-apic-entry-mask+)
        (let* ((gsi (+ gsi-base e))
               (vector (gsi->vector gsi))
               (flags (if (< gsi 256) (svref *gsi-flags* gsi) 0)))
          (when (< vector 256)
            (io-apic-configure-entry gsi vector bsp-apic-id
                                     :trigger-mode (gsi-flags-trigger flags)
                                     :polarity (gsi-flags-polarity flags)
                                     :masked t))))
      (debug-print-line "IO-APIC " id " at " phys-addr
                        " GSI base " gsi-base " max redirect " max-redir))))

(defun init-io-apic-controllers (controllers n-controllers)
  (setf *io-apic-irqs* (sys.int::make-simple-vector 256 :wired))
  (dotimes (i 256)
    (setf (svref *io-apic-irqs* i) (make-irq :platform-number i)))
  (setf *io-apics* '())
  (let ((bsp-id (if (boundp '*bsp-cpu*) (x86-64-cpu-apic-id *bsp-cpu*) 0)))
    (dotimes (i n-controllers)
      (let ((entry (svref controllers i)))
        (when (acpi-madt-ioapic-p entry)
          (init-one-io-apic entry bsp-id))))))

(defun hook-io-apic-interrupt-handlers ()
  (debug-print-line "IO-APIC init done. Hooking handlers...")
  (let ((max-gsi 0))
    (dolist (apic *io-apics*)
      (setf max-gsi (max max-gsi
                         (+ (io-apic-gsi-base apic)
                            (io-apic-max-redirection apic)))))
    (dotimes (gsi (1+ max-gsi))
      (let ((vector (gsi->vector gsi)))
        (when (< vector 256)
          (hook-user-interrupt vector 'io-apic-interrupt-handler))))))

(defun finalize-io-apic-init ()
  (setf *io-apic-active-p* t)
  (when (boundp '*i8259-shadow-mask*)
    (setf (sys.int::io-port/8 #x21) #xFF
          (sys.int::io-port/8 #xA1) #xFF
          *i8259-shadow-mask* #xFFFF))
  (write-lapic (logior (read-lapic +lapic-reg-lvt-lint0+) +lapic-lvt-mask+)
               +lapic-reg-lvt-lint0+)
  (write-lapic (logior (read-lapic +lapic-reg-lvt-lint1+) +lapic-lvt-mask+)
               +lapic-reg-lvt-lint1+))

(defun initialize-io-apics ()
  (setf *io-apic-active-p* nil)
  (unless (boundp '*io-apics*) (setf *io-apics* nil))
  (when *io-apics* (return-from initialize-io-apics))
  (setf *io-apic-irqs* nil *isa-irq-to-gsi* nil *gsi-flags* nil)
  (let ((madt (acpi-get-table 'acpi-madt-table-p)))
    (unless madt
      (debug-print-line "No MADT table, IO-APIC not available.")
      (return-from initialize-io-apics))
    (setf *gsi-flags* (sys.int::make-simple-vector 256 :wired))
    (let* ((controllers (acpi-madt-table-controllers madt))
           (n (sys.int::simple-vector-length controllers)))
      (process-isa-overrides controllers n)
      (init-io-apic-controllers controllers n)
      (when *io-apics*
        (hook-io-apic-interrupt-handlers)
        (finalize-io-apic-init)))))

(defun io-apic-interrupt-handler (interrupt-frame info)
  ;; If this CPU was idle during a TLB shootdown and missed the IPI,
  ;; flush now before the IRQ handler touches any pageable memory.
  (check-tlb-generation-consistency)
  (let ((gsi (vector->gsi info)))
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
      ;; The IO-APIC redirection-entry destination field is always bits
      ;; 63:56 regardless of whether the local APIC is in xAPIC or x2APIC
      ;; mode: the IO-APIC is a separate device that emits an MSI write
      ;; whose 8-bit physical destination is matched against the low 8
      ;; bits of each CPU's (x2)APIC ID.  Encoding the destination at bits
      ;; 39:32 (as an earlier version did in x2APIC mode) routes every
      ;; external interrupt to APIC ID 0.
      (setf entry-value (logior entry-value
                                (ash (ldb (byte 8 0) destination-apic-id) 56)))
      (io-apic-write-redirection apic entry entry-value)
      t)))
