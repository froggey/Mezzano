;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(sys.int::defglobal *acpi*)

(defconstant +acpi-rsdp-signature-offset+          0)
(defconstant +acpi-rsdp-checksum-offset+           8)
(defconstant +acpi-rsdp-oemid-offset+             10)
(defconstant +acpi-rsdp-revision-offset+          15)
(defconstant +acpi-rsdp-rsdt-address-offset+      16)
(defconstant +acpi-rsdp-length-offset+            20)
(defconstant +acpi-rsdp-xsdt-address-offset+      24)
(defconstant +acpi-rsdp-extended-checksum-offset+ 32)

(defconstant +acpi-header-signature-offset+         0)
(defconstant +acpi-header-length-offset+            4)
(defconstant +acpi-header-revision-offset+          8)
(defconstant +acpi-header-checksum-offset+          9)
(defconstant +acpi-header-oemid-offset+            10)
(defconstant +acpi-header-oem-table-id-offset+     16)
(defconstant +acpi-header-oem-revision-offset+     24)
(defconstant +acpi-header-creator-id-offset+       28)
(defconstant +acpi-header-creator-revision-offset+ 32)
(defconstant +acpi-header-length+ 36)

(defmacro define-acpi-table ((table-name signature) slots)
  `(progn
     ,@(loop
          for (name offset type) in slots
          collect `(defconstant ,(intern (format nil "+ACPI-~A-~A-OFFSET+" table-name name)
                                         (symbol-package table-name))
                     ,offset))
     (defstruct (,(intern (format nil "ACPI-~A-TABLE" table-name)
                          (symbol-package table-name))
             (:area :wired)
             (:include acpi-table-header))
       ,@(loop
            for (name offset type) in slots
            collect name))
     (defun ,(intern (format nil "READ-ACPI-~A-TABLE" table-name)
                     (symbol-package table-name))
         (address)
       (let ((table (,(intern (format nil "MAKE-ACPI-~A-TABLE" table-name)
                              (symbol-package table-name))
                      :address address)))
         (read-acpi-table-header table)
         (let ((table-length (acpi-table-header-length table)))
           ,@(loop
                for (name offset type) in slots
                for slot-accessor = (intern (format nil "ACPI-~A-TABLE-~A" table-name name)
                                            (symbol-package table-name))
                collect `(when (< ,offset table-length)
                           (setf (,slot-accessor table)
                                 (,(ecase type
                                     (:ub8 'physical-memref-unsigned-byte-8)
                                     (:ub16/le 'physical-memref-unsigned-byte-16)
                                     (:ub32/le 'physical-memref-unsigned-byte-32)
                                     (:ub64/le 'physical-memref-unsigned-byte-64)
                                     (:gas 'acpi-read-generic-address-structure))
                                   (+ address ,offset))))))
         table))))

(defun acpi-checksum-range (start size)
  (let ((sum 0))
    (dotimes (i size)
      (setf sum (ldb (byte 8 0) (+ sum (physical-memref-unsigned-byte-8 start i)))))
    (zerop sum)))

(defstruct (acpi-rsdp
             (:area :wired))
  oemid
  revision
  rsdt-address
  xsdt-address)

(defstruct (acpi-table-header
             (:area :wired))
  address
  signature
  length
  revision
  oemid
  oem-table-id-low
  oem-table-id-high
  oem-revision
  creator-id
  creator-revision)

(defstruct (acpi-generic-address
             (:area :wired))
  address-space-id
  register-bit-width
  register-bit-offset
  access-size
  address)

(defstruct (acpi-madt-table
             (:area :wired)
             (:include acpi-table-header))
  local-interrupt-controller-address
  flags
  controllers)

(define-acpi-table (fadt "FACP")
    ((firmware-ctrl               36 :ub32/le)
     (dsdt                        40 :ub32/le)
     (preferred-pm-profile        45 :ub8)
     (sci-int                     46 :ub16/le)
     (smi-cmd                     48 :ub32/le)
     (acpi-enable                 52 :ub8)
     (acpi-disable                53 :ub8)
     (s4bios-req                  54 :ub8)
     (pstate-cnt                  55 :ub8)
     (pm1a-evt-blk                56 :ub32/le)
     (pm1b-evt-blk                60 :ub32/le)
     (pm1a-cnt-blk                64 :ub32/le)
     (pm1b-cnt-blk                68 :ub32/le)
     (pm2-cnt-blk                 72 :ub32/le)
     (pm-tmr-blk                  76 :ub32/le)
     (gpe0-blk                    80 :ub32/le)
     (gpe1-blk                    84 :ub32/le)
     (pm1-evt-len                 88 :ub8)
     (pm1-cnt-len                 89 :ub8)
     (pm2-cnt-len                 90 :ub8)
     (pm-tmr-len                  91 :ub8)
     (gpe0-blk-len                92 :ub8)
     (gpe1-blk-len                93 :ub8)
     (gpe1-base                   94 :ub8)
     (cst-cnt                     95 :ub8)
     (p-lvl2-lat                  96 :ub16/le)
     (p-lvl3-lat                  98 :ub16/le)
     (flush-size                 100 :ub16/le)
     (flush-stride               102 :ub16/le)
     (duty-offset                104 :ub8)
     (duty-width                 105 :ub8)
     (day-alrm                   106 :ub8)
     (mon-alrm                   107 :ub8)
     (century                    108 :ub8)
     (iapc-boot-arch             109 :ub16/le)
     (flags                      112 :ub32/le)
     (reset-reg                  116 :gas)
     (reset-value                128 :ub8)
     (arm-boot-arch              129 :ub16/le)
     (fadt-minor-version         131 :ub8)
     (x-firmware-ctrl            132 :ub64/le)
     (x-dsdt                     140 :ub64/le)
     (x-pm1a-evt-blk             148 :gas)
     (x-pm1b-evt-blk             160 :gas)
     (x-pm1a-cnt-blk             172 :gas)
     (x-pm1b-cnt-blk             184 :gas)
     (x-pm2-cnt-blk              196 :gas)
     (x-pm-tmr-blk               208 :gas)
     (x-gpe0-blk                 220 :gas)
     (x-gpe1-blk                 232 :gas)
     (sleep-control-reg          244 :gas)
     (sleep-status-reg           256 :gas)
     (hypervisor-vendor-identity 268 :ub64/le)))

(defconstant +acpi-iapc-boot-arch-legacy-devices+       #x0001)
(defconstant +acpi-iapc-boot-arch-8042+                 #x0002)
(defconstant +acpi-iapc-boot-arch-vga-not-present+      #x0004)
(defconstant +acpi-iapc-boot-arch-msi-not-supported+    #x0008)
(defconstant +acpi-iapc-boot-arch-pcie-aspm-controls+   #x0010)
(defconstant +acpi-iapc-boot-arch-cmos-rtc-not-present+ #x0020)

(defun acpi-parse-rsdp (rsdp-address)
  (map-physical-memory (align-down rsdp-address +4k-page-size+)
                       (align-up (1+ (logand rsdp-address #xFFF))
                                 +4k-page-size+)
                       "ACPI")
  (let ((xsdt-address nil)
        (revision (physical-memref-unsigned-byte-8 (+ rsdp-address +acpi-rsdp-revision-offset+) 0)))
    (when (>= revision 2)
      (let ((length (physical-memref-unsigned-byte-32 (+ rsdp-address +acpi-rsdp-length-offset+) 0)))
        (cond ((acpi-checksum-range rsdp-address length)
               (setf xsdt-address (physical-memref-unsigned-byte-64 (+ rsdp-address +acpi-rsdp-xsdt-address-offset+) 0)))
              (t
               (debug-print-line "RSDP failed extended checksum?")))))
    ;; Read OEMID. (FIXME: Read as string here, not a vector of bytes.)
    (let ((oemid (sys.int::make-simple-vector 6 :wired)))
      (dotimes (i 6)
        (setf (svref oemid i) (physical-memref-unsigned-byte-8 (+ rsdp-address +acpi-rsdp-oemid-offset+) i)))
      (make-acpi-rsdp :oemid oemid
                      :revision revision
                      :rsdt-address (physical-memref-unsigned-byte-32 (+ rsdp-address +acpi-rsdp-rsdt-address-offset+) 0)
                      :xsdt-address xsdt-address))))

(defun ensure-acpi-table-accessible (address)
  ;; Make sure that the length is mapped.
  (let ((length-end (+ address +acpi-header-length-offset+ 4)))
    (map-physical-memory (align-down address +4k-page-size+)
                         (- (align-up length-end +4k-page-size+)
                            (align-down address +4k-page-size+))
                         "ACPI")
    ;; Read length and map the entire table in.
    (let* ((length (physical-memref-unsigned-byte-32
                    (+ address
                       +acpi-header-length-offset+)
                    0))
           (end (+ address length)))
      (map-physical-memory (align-down address +4k-page-size+)
                         (- (align-up end +4k-page-size+)
                            (align-down address +4k-page-size+))
                         "ACPI"))))

(defun acpi-read-generic-address-structure (address)
  (make-acpi-generic-address
   :address-space-id (physical-memref-unsigned-byte-8 (+ address 0))
   :register-bit-width (physical-memref-unsigned-byte-8 (+ address 1))
   :register-bit-offset (physical-memref-unsigned-byte-8 (+ address 2))
   :access-size (physical-memref-unsigned-byte-8 (+ address 3))
   :address (physical-memref-unsigned-byte-64 (+ address 4))))

(defun acpi-generic-address (gas)
  (let* ((address (acpi-generic-address-address gas))
         (value (ecase (acpi-generic-address-address-space-id gas)
                  (0 ; System memory space.
                   (ecase (acpi-generic-address-access-size gas)
                     (1 (physical-memref-unsigned-byte-8 address))
                     (2 (physical-memref-unsigned-byte-16 address))
                     (3 (physical-memref-unsigned-byte-32 address))
                     (4 (physical-memref-unsigned-byte-64 address))))
                  (1 ; System I/O space.
                   (ecase (acpi-generic-address-access-size gas)
                     (1 (sys.int::io-port/8 address))
                     (2 (sys.int::io-port/16 address))
                     (3 (sys.int::io-port/32 address)))))))
    (ldb (byte (acpi-generic-address-register-bit-width gas)
               (acpi-generic-address-register-bit-offset gas))
         value)))

(defun (setf acpi-generic-address) (value gas)
  (ensure (eql (acpi-generic-address-register-bit-offset gas) 0))
  (let ((width (acpi-generic-address-register-bit-width gas)))
    (ecase (acpi-generic-address-access-size gas)
      (1 (ensure (eql width 8)))
      (2 (ensure (eql width 16)))
      (3 (ensure (eql width 32)))
      (4 (ensure (eql width 64)))))
  (let ((address (acpi-generic-address-address gas)))
    (ecase (acpi-generic-address-address-space-id gas)
      (0 ; System memory space.
       (ecase (acpi-generic-address-access-size gas)
         (1 (setf (physical-memref-unsigned-byte-8 address) value))
         (2 (setf (physical-memref-unsigned-byte-16 address) value))
         (3 (setf (physical-memref-unsigned-byte-32 address) value))
         (4 (setf (physical-memref-unsigned-byte-64 address) value))))
      (1 ; System I/O space.
       (ecase (acpi-generic-address-access-size gas)
         (1 (setf (sys.int::io-port/8 address) value))
         (2 (setf (sys.int::io-port/16 address) value))
         (3 (setf (sys.int::io-port/32 address) value)))))))

(defun read-acpi-table-header (header)
  (let ((address (acpi-table-header-address header)))
    (setf (acpi-table-header-signature header)
          (physical-memref-unsigned-byte-32
           (+ address +acpi-header-signature-offset+)
           0))
    (setf (acpi-table-header-length header)
          (physical-memref-unsigned-byte-32
           (+ address +acpi-header-length-offset+)
           0))
    (setf (acpi-table-header-revision header)
          (physical-memref-unsigned-byte-8
           (+ address +acpi-header-revision-offset+)
           0))
    (setf (acpi-table-header-oemid header)
          (logior (physical-memref-unsigned-byte-32
                   (+ address +acpi-header-oemid-offset+)
                   0)
                  (ash (physical-memref-unsigned-byte-16
                        (+ address +acpi-header-oemid-offset+ 4)
                        0)
                       32)))
    (setf (acpi-table-header-oem-table-id-low header)
          (physical-memref-unsigned-byte-32
           (+ address +acpi-header-oem-table-id-offset+)
           0))
    (setf (acpi-table-header-oem-table-id-high header)
          (physical-memref-unsigned-byte-32
           (+ address +acpi-header-oem-table-id-offset+)
           1))
    (setf (acpi-table-header-oem-revision header)
          (physical-memref-unsigned-byte-32
           (+ address +acpi-header-oem-revision-offset+)
           0))
    (setf (acpi-table-header-creator-id header)
          (physical-memref-unsigned-byte-32
           (+ address +acpi-header-creator-id-offset+)
           0))
    (setf (acpi-table-header-creator-revision header)
          (physical-memref-unsigned-byte-32
           (+ address +acpi-header-creator-revision-offset+)
           0)))
  header)

(defun read-acpi-rsdt (address)
  (ensure-acpi-table-accessible address)
  (let ((header (make-acpi-table-header :address address)))
    (read-acpi-table-header header)
    (let* ((length (acpi-table-header-length header))
           (n-entries (truncate (- length +acpi-header-length+) 4))
           (entries (sys.int::make-simple-vector n-entries :wired)))
      (dotimes (i n-entries)
        (setf (svref entries i) (physical-memref-unsigned-byte-32 (+ address +acpi-header-length+) i)))
      (values header entries))))

(defun read-acpi-xsdt (address)
  (ensure-acpi-table-accessible address)
  (let ((header (make-acpi-table-header :address address)))
    (read-acpi-table-header header)
    (let* ((length (acpi-table-header-length header))
           (n-entries (truncate (- length +acpi-header-length+) 8))
           (entries (sys.int::make-simple-vector n-entries :wired)))
      (dotimes (i n-entries)
        (setf (svref entries i) (physical-memref-unsigned-byte-64 (+ address +acpi-header-length+) i)))
      (values header entries))))

(defstruct (acpi-madt-processor-lapic
             (:area :wired))
  acpi-processor-id
  apic-id
  flags)

(defstruct (acpi-madt-ioapic
             (:area :wired))
  id
  address
  global-system-interrupt-base)

(defstruct (acpi-madt-interrupt-source-override
             (:area :wired))
  bus
  source
  global-system-interrupt
  flags)

(defstruct (acpi-madt-nmi-source
             (:area :wired))
  flags
  global-system-interrupt)

(defstruct (acpi-madt-lapic-nmi
             (:area :wired))
  acpi-processor-id
  flags
  lapic-lintn)

(defstruct (acpi-madt-lapic-address-override
             (:area :wired))
  address)

(defun read-acpi-madt-table (address)
  (let ((table (make-acpi-madt-table :address address))
        (n-controller-entries 0))
    (read-acpi-table-header table)
    (setf (acpi-madt-table-local-interrupt-controller-address table)
          (physical-memref-unsigned-byte-32 (+ address 36)))
    (setf (acpi-madt-table-flags table)
          (physical-memref-unsigned-byte-32 (+ address 40)))
    (let ((offset 44)
          (total-length (acpi-table-header-length table)))
      (loop
         (when (>= offset total-length)
           (return))
         (when (<= (physical-memref-unsigned-byte-8 (+ address offset)) 5)
           ;; Ignore entries with an unknown type.
           (incf n-controller-entries))
         (incf offset (physical-memref-unsigned-byte-8 (+ address offset 1)))))
    (setf (acpi-madt-table-controllers table) (sys.int::make-simple-vector n-controller-entries :wired))
    (let ((offset 44)
          (total-length (acpi-table-header-length table))
          (current 0))
      (loop
         (when (>= offset total-length)
           (return))
         (let ((type (physical-memref-unsigned-byte-8 (+ address offset)))
               (len (physical-memref-unsigned-byte-8 (+ address offset 1))))
           (case type
             (0 ;; Processor local APIC.
              (setf (svref (acpi-madt-table-controllers table) current)
                    (make-acpi-madt-processor-lapic
                     :acpi-processor-id (physical-memref-unsigned-byte-8 (+ address offset 2))
                     :apic-id (physical-memref-unsigned-byte-8 (+ address offset 3))
                     :flags (physical-memref-unsigned-byte-32 (+ address offset 4)))))
             (1 ;; I/O APIC.
              (setf (svref (acpi-madt-table-controllers table) current)
                    (make-acpi-madt-ioapic
                     :id (physical-memref-unsigned-byte-8 (+ address offset 2))
                     :address (physical-memref-unsigned-byte-32 (+ address offset 4))
                     :global-system-interrupt-base (physical-memref-unsigned-byte-32 (+ address offset 8)))))
             (2 ;; Interrupt source override.
              (setf (svref (acpi-madt-table-controllers table) current)
                    (make-acpi-madt-interrupt-source-override
                     :bus (physical-memref-unsigned-byte-8 (+ address offset 2))
                     :source (physical-memref-unsigned-byte-8 (+ address offset 3))
                     :global-system-interrupt (physical-memref-unsigned-byte-32 (+ address offset 4))
                     :flags (physical-memref-unsigned-byte-16 (+ address offset 8)))))
             (3 ;; Non-maskable interrupt source.
              (setf (svref (acpi-madt-table-controllers table) current)
                    (make-acpi-madt-nmi-source
                     :flags (physical-memref-unsigned-byte-16 (+ address offset 2))
                     :global-system-interrupt (physical-memref-unsigned-byte-32 (+ address offset 4)))))
             (4 ;; Local APIC NMI.
              (setf (svref (acpi-madt-table-controllers table) current)
                    (make-acpi-madt-lapic-nmi
                     :acpi-processor-id (physical-memref-unsigned-byte-8 (+ address offset 2))
                     :flags (physical-memref-unsigned-byte-16 (+ address offset 3))
                     :lapic-lintn (physical-memref-unsigned-byte-8 (+ address offset 5)))))
             (5 ;; Local APIC address override.
              (setf (svref (acpi-madt-table-controllers table) current)
                    (make-acpi-madt-lapic-address-override
                     :address (physical-memref-unsigned-byte-64 (+ address offset 4))))))
           (incf current)
           (incf offset len))))
    table))

(defun read-acpi-table (address)
  (ensure-acpi-table-accessible address)
  (let ((sig (physical-memref-unsigned-byte-32 (+ address +acpi-header-signature-offset+))))
    (case sig
      (#x43495041 ; APIC
       (read-acpi-madt-table address))
      (#x50434146 ; FACP
       (read-acpi-fadt-table address))
      (t
       ;; Unknown table, just read the table header.
       (let ((header (make-acpi-table-header :address address)))
         (read-acpi-table-header header)
         header)))))

(defun acpi-rsdp-address ()
  ;; Fetch the RSDP address from somewhere.
  (let ((provided-rsdp (sys.int::memref-unsigned-byte-64
                        (+ *boot-information-page* +boot-information-acpi-rsdp+))))
    (or (if (zerop provided-rsdp)
            nil
            provided-rsdp)
        ;; Search the EFI system table.
        (efi-get-configuration-table #(#x30 #x2D #x9D #xEB #x88 #x2D #xD3 #x11
                                       #x9A #x16 #x00 #x90 #x27 #x3F #xC1 #x4D)))))

(defun initialize-acpi ()
  (setf *acpi* nil)
  (let ((rsdp-address (acpi-rsdp-address)))
    (when (not rsdp-address)
      (debug-print-line "No ACPI RSDP.")
      (return-from initialize-acpi))
    (let ((rsdp (acpi-parse-rsdp rsdp-address)))
      (debug-print-line "RSDP: " (acpi-rsdp-oemid rsdp) " " (acpi-rsdp-revision rsdp) " " (acpi-rsdp-rsdt-address rsdp) " " (acpi-rsdp-xsdt-address rsdp))
      (multiple-value-bind (root-header table-addresses)
          (if (acpi-rsdp-xsdt-address rsdp)
              (read-acpi-xsdt (acpi-rsdp-xsdt-address rsdp))
              (read-acpi-rsdt (acpi-rsdp-rsdt-address rsdp)))
        (debug-print-line "Read " (sys.int::simple-vector-length table-addresses) " tables.")
        (let ((tables (sys.int::make-simple-vector (sys.int::simple-vector-length table-addresses)
                                                   :wired)))
          (dotimes (i (sys.int::simple-vector-length tables))
            (setf (svref tables i) (read-acpi-table (svref table-addresses i)))
            (let ((header (make-acpi-table-header :address (svref table-addresses i))))
              (read-acpi-table-header header)
              (debug-print-line " Table " i " " (svref tables i) " " (acpi-table-header-signature header))))
          (setf *acpi* tables))))))

(defun acpi-get-table (predicate)
  (cond
    (*acpi*
     (dotimes (i (sys.int::simple-vector-length *acpi*)
               nil)
       (let ((table (svref *acpi* i)))
         (when (funcall predicate table)
           (return table)))))
    (t
     nil)))
