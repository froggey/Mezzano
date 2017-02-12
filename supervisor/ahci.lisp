;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; "Serial ATA Advancd Host Controller Interface (AHCI) 1.3"
;;; http://www.microbe.cz/docs/serial-ata-ahci-spec-rev1_3.pdf
;;; "Serial ATA: High Speed Serialized AT Attachment. Revision 1.0a"
;;; http://www.ece.umd.edu/courses/enee759h.S2003/references/serialata10a.pdf

;;; Register offsets in the HBA Memory Registers.

;; Generic Host Control.
(defconstant +ahci-register-CAP+ #x00 "Host Capabilities.")
(defconstant +ahci-register-GHC+ #x04 "Global Host Control.")
(defconstant +ahci-register-IS+ #x08 "Interrupt Status.")
(defconstant +ahci-register-PI+ #x0C "Ports Implemented.")
(defconstant +ahci-register-VS+ #x10 "Version.")
(defconstant +ahci-register-CCC_CTL+ #x14 "Command Completion Coalescing Control.")
(defconstant +ahci-register-CCC_PORTS+ #x18 "Command Completion Coalescing Ports.")
(defconstant +ahci-register-EM_LOC+ #x1C "Enclosure Management Location.")
(defconstant +ahci-register-EM_CTL+ #x20 "Enclosure Management Control.")
(defconstant +ahci-register-CAP2+ #x24 "Host Capabilities Extended.")
(defconstant +ahci-register-BOHC+ #x28 "BIOS/OS Handoff Control and Status.")

;; Port Control Registers.
(defconstant +ahci-register-PxCLB+ #x00 "Command List Base Address.")
(defconstant +ahci-register-PxCLBU+ #x04 "Command List Base Address Upper 32-bits.")
(defconstant +ahci-register-PxFB+ #x08 "FIS Base Address.")
(defconstant +ahci-register-PxFBU+ #x0C "FIS Base Address Upper 32-bits.")
(defconstant +ahci-register-PxIS+ #x10 "Interrupt Status.")
(defconstant +ahci-register-PxIE+ #x14 "Interrupt Enable.")
(defconstant +ahci-register-PxCMD+ #x18 "Command and Status.")
(defconstant +ahci-register-PxTFD+ #x20 "Task File Data.")
(defconstant +ahci-register-PxSIG+ #x24 "Signature.")
(defconstant +ahci-register-PxSSTS+ #x28 "SATA Status (SCR0: SStatus).")
(defconstant +ahci-register-PxSCTL+ #x2C "SATA Control (SCR2: SControl).")
(defconstant +ahci-register-PxSERR+ #x30 "SATA Error (SCR1: SError).")
(defconstant +ahci-register-PxSACT+ #x34 "SATA Active (SCR3: SActive).")
(defconstant +ahci-register-PxCI+ #x38 "Command Issue.")
(defconstant +ahci-register-PxSNTF+ #x3C "SATA Notification (SCR4: SNotification).")
(defconstant +ahci-register-PxFBS+ #x40 "FIS-based Switching Control.")
(defconstant +ahci-register-PxVS+ #x70 "Vendor Specific.")

;; Base address of Port 0's register set.
(defconstant +ahci-port0-base+ #x100)
;; Size of one port's register set.
(defconstant +ahci-port-control-size+ #x80)

(defconstant +ahci-maximum-n-ports+ 32
  "Maximum number of ports supported by one HBA.")

;;; Bits in CAP.
(defconstant +ahci-CAP-S46A+ 31 "Supports 64-bit Addressing.")
(defconstant +ahci-CAP-SNCQ+ 30 "Supports Native Command Queuing.")
(defconstant +ahci-CAP-SSNTF+ 29 "Supports SNotification Register.")
(defconstant +ahci-CAP-SMPS+ 28 "Supports Mechanical Presence Switch.")
(defconstant +ahci-CAP-SSS+ 27 "Supports Staggered Spin-up.")
(defconstant +ahci-CAP-SALP+ 26 "Supports Aggressive Link Power Management.")
(defconstant +ahci-CAP-SAL+ 25 "Support Activity LED.")
(defconstant +ahci-CAP-SCLO+ 24 "Supports Command List Override.")
(defconstant +ahci-CAP-ISS-position+ 20 "Interface Speed Support.")
(defconstant +ahci-CAP-ISS-size+ 4 "ISS field size.")
(defconstant +ahci-CAP-ISS-gen-1+ #b0001)
(defconstant +ahci-CAP-ISS-gen-2+ #b0010)
(defconstant +ahci-CAP-ISS-gen-3+ #b0011)
(defconstant +ahci-CAP-SAM+ 18 "Supports AHCI mode only.")
(defconstant +ahci-CAP-SPM+ 17 "Supports Port Multiplier.")
(defconstant +ahci-CAP-FBSS+ 16 "FIS-based Switching Supported.")
(defconstant +ahci-CAP-PMD+ 15 "PIO Multiple DRQ Block.")
(defconstant +ahci-CAP-SSC+ 14 "Slumber State Capable.")
(defconstant +ahci-CAP-PSC+ 13 "Partial State Capable.")
(defconstant +ahci-CAP-NCS-position+ 8 "Number of Command Slots.")
(defconstant +ahci-CAP-NCS-size+ 5 "NCS field size.")
(defconstant +ahci-CAP-CCCS+ 7 "Comand Completion Coalescing Supported.")
(defconstant +ahci-CAP-EMS+ 6 "Enclosure Management Supported.")
(defconstant +ahci-CAP-SXS+ 5 "Supports External SATA.")
(defconstant +ahci-CAP-NP-position+ 0 "Number of Ports.")
(defconstant +ahci-CAP-NP-size+ 5 "NP field size.")

;;; Bits in GHC.
(defconstant +ahci-GHC-AE+ 31 "AHCI Enable.")
(defconstant +ahci-GHC-MRSM+ 2 "MSI Revert to Single Message.")
(defconstant +ahci-GHC-IE+ 1 "Interrupt Enable.")
(defconstant +ahci-GHC-HR+ 0 "HBA Reset.")

;;; Bits in VS.
(defconstant +ahci-VS-MJR-position+ 16 "Major Version.")
(defconstant +ahci-VS-MJR-size+ 16 "MJR field size.")
(defconstant +ahci-VS-MNR-position+ 0 "Minor Version.")
(defconstant +ahci-VS-MNR-size+ 16 "MNR field size.")

;;; Bits in CCC_CTL.
(defconstant +ahci-CCC_CTL-TV-position+ 16 "Timeout Value.")
(defconstant +ahci-CCC_CTL-TV-size+ 16 "TV field size.")
(defconstant +ahci-CCC_CTL-CC-position+ 8 "Command Completions.")
(defconstant +ahci-CCC_CTL-CC-size+ 8 "CC field size.")
(defconstant +ahci-CCC_CTL-INT-position+ 3 "Interrupt.")
(defconstant +ahci-CCC_CTL-INT-size+ 5 "INT field size.")
(defconstant +ahci-CCC_CTL-EN+ 0 "Enable.")

;;; Bits in CAP2.
(defconstant +ahci-CAP2-APST+ 2 "Automatic Partial to Slumber Transitions.")
(defconstant +ahci-CAP2-NVMP+ 1 "NVMHCI Present.")
(defconstant +ahci-CAP2-BOH+ 0 "BIOS/OS Handoff.")

;;; Bits in BOHC.
(defconstant +ahci-BOHC-BB+ 4 "BIOS Busy.")
(defconstant +ahci-BOHC-OOC+ 3 "OS Ownership Change.")
(defconstant +ahci-BOHC-SOOE+ 2 "SMI on OS Ownership Change Enabled.")
(defconstant +ahci-BOHC-OOS+ 1 "OS Owned Semaphore.")
(defconstant +ahci-BOHC-BOS+ 0 "BIOS Owned Semaphore.")

;;; Bits in PxIS and PxIE.
(defconstant +ahci-PxIS-CPDS+ 31 "Cold Port Detect Status.")
(defconstant +ahci-PxIS-TFES+ 30 "Task File Error Status.")
(defconstant +ahci-PxIS-HBFS+ 29 "Host Bus Fatal Error Status.")
(defconstant +ahci-PxIS-HBDS+ 28 "Host Bus Data Error Status.")
(defconstant +ahci-PxIS-IFS+ 27 "Interface Fatal Error Status.")
(defconstant +ahci-PxIS-INFS+ 26 "Interface Non-fatal Error Status.")
(defconstant +ahci-PxIS-OFS+ 24 "Overflow Status.")
(defconstant +ahci-PxIS-IPMS+ 23 "Incorrect Port Multiplier Status.")
(defconstant +ahci-PxIS-PRCS+ 22 "PhyRdy Change Status.")
(defconstant +ahci-PxIS-DMPS+ 7 "Device Mechanical Presence Status.")
(defconstant +ahci-PxIS-PCS+ 6 "Port Connect Change Status.")
(defconstant +ahci-PxIS-DPS+ 5 "Descriptor Processed.")
(defconstant +ahci-PxIS-UFS+ 4 "Unknown FIS Interrupt.")
(defconstant +ahci-PxIS-SDBS+ 3 "Set Device Bits Interrupt.")
(defconstant +ahci-PxIS-DSS+ 2 "DMS Setup FIS Interrupt.")
(defconstant +ahci-PxIS-PSS+ 1 "PIO Setup FIS Interrupt.")
(defconstant +ahci-PxIS-DHRS+ 0 "Device to Host Register FIS Interrupt.")

;;; Bits in PxCMD.
(defconstant +ahci-PxCMD-ICC-position+ 28 "Interface Communication Control.")
(defconstant +ahci-PxCMD-ICC-size+ 4 "ICC field size.")
(defconstant +ahci-PxCMD-ICC-slumber+ 6)
(defconstant +ahci-PxCMD-ICC-partial+ 2)
(defconstant +ahci-PxCMD-ICC-active+ 1)
(defconstant +ahci-PxCMD-ICC-idle+ 0)
(defconstant +ahci-PxCMD-ASP+ 27 "Agressive Slumber/Partial.")
(defconstant +ahci-PxCMD-ALPE+ 26 "Aggressive Link Power Management Enable.")
(defconstant +ahci-PxCMD-DLAE+ 25 "Drive LED on ATAPI Enable.")
(defconstant +ahci-PxCMD-ATAPI+ 24 "Device is ATAPI.")
(defconstant +ahci-PxCMD-APSTE+ 23 "Automatic Partial to Slumber Transitions Enabled.")
(defconstant +ahci-PxCMD-FBSCP+ 22 "FIS-based Switching Capable Port.")
(defconstant +ahci-PxCMD-ESP+ 21 "External SATA Port.")
(defconstant +ahci-PxCMD-CPD+ 20 "Cold Presence Detection.")
(defconstant +ahci-PxCMD-MPSP+ 19 "Mechanical Presence Switch Attached to Port.")
(defconstant +ahci-PxCMD-HPCP+ 18 "Hot Plug Capable Port.")
(defconstant +ahci-PxCMD-PMA+ 17 "Port Multiplier Attached.")
(defconstant +ahci-PxCMD-CPS+ 16 "Cold Presence State.")
(defconstant +ahci-PxCMD-CR+ 15 "Command List Running.")
(defconstant +ahci-PxCMD-FR+ 14 "FIS Receive Running.")
(defconstant +ahci-PxCMD-MPSS+ 13 "Mechanical Presence Switch State.")
(defconstant +ahci-PxCMD-CCS-position+ 8 "Current Command Slot.")
(defconstant +ahci-PxCMD-CCS-size+ 5 "CCS field size.")
(defconstant +ahci-PxCMD-FRE+ 4 "FIS Receive Enable.")
(defconstant +ahci-PxCMD-CLO+ 3 "Command List Overide.")
(defconstant +ahci-PxCMD-POD+ 2 "Power On Device.")
(defconstant +ahci-PxCMD-SUD+ 1 "Spin-Up Device.")
(defconstant +ahci-PxCMD-ST+ 0 "Start.")

;;; Bits in PxTFD.
(defconstant +ahci-PxTFD-ERR-position+ 8)
(defconstant +ahci-PxTFD-ERR-size+ 8)
(defconstant +ahci-PxTFD-STS-position+ 0)
(defconstant +ahci-PxTFD-STS-size+ 8)

;;; Bits in PxSSTS.
(defconstant +ahci-PxSSTS-IPM-position+ 8 "Interface Power Management.")
(defconstant +ahci-PxSSTS-IPM-size+ 4 "IPM field size.")
(defconstant +ahci-PxSSTS-IPM-not-present+ 0)
(defconstant +ahci-PxSSTS-IPM-active+ 1)
(defconstant +ahci-PxSSTS-IPM-partial+ 2)
(defconstant +ahci-PxSSTS-IPM-slumber+ 6)
(defconstant +ahci-PxSSTS-SPD-position+ 4 "Current Interface Speed.")
(defconstant +ahci-PxSSTS-SPD-size+ 4 "SPD field size.")
(defconstant +ahci-PxSSTS-DET-position+ 0 "Device Detection.")
(defconstant +ahci-PxSSTS-DET-size+ 4 "DET field size.")
(defconstant +ahci-PxSSTS-DET-no-device+ 0)
(defconstant +ahci-PxSSTS-DET-no-phy+ 1)
(defconstant +ahci-PxSSTS-DET-ready+ 3)
(defconstant +ahci-PxSSTS-DET-offline+ 4)

;;; Bits in PxSCTL.
(defconstant +ahci-PxSCTL-IPM-position+ 8 "Interface Power Management Transitions Allowed.")
(defconstant +ahci-PxSCTL-IPM-size+ 4 "IPM field size.")
(defconstant +ahci-PxSCTL-IPM-no-restrictions+ 0)
(defconstant +ahci-PxSCTL-IPM-partial-disabled+ 1)
(defconstant +ahci-PxSCTL-IPM-slumber-disabled+ 2)
(defconstant +ahci-PxSCTL-IPM-partial-and-slumber-disabled+ 3)
(defconstant +ahci-PxSCTL-SPD-position+ 4 "Speed Allowed.")
(defconstant +ahci-PxSCTL-SPD-size+ 4 "SPD field size.")
(defconstant +ahci-PxSCTL-DET-position+ 0 "Device Detection Initialization.")
(defconstant +ahci-PxSCTL-DET-size+ 4 "DET field size.")

;;; Bits in PxSERR.
(defconstant +ahci-PxSERR-DIAG-X+ 26 "Exchanged.")
(defconstant +ahci-PxSERR-DIAG-F+ 25 "Unknown FIS Type.")
(defconstant +ahci-PxSERR-DIAG-T+ 24 "Transport State Transition Error.")
(defconstant +ahci-PxSERR-DIAG-S+ 23 "Link Sequence Error.")
(defconstant +ahci-PxSERR-DIAG-H+ 22 "Handshake Error.")
(defconstant +ahci-PxSERR-DIAG-C+ 21 "CRC Error.")
(defconstant +ahci-PxSERR-DIAG-D+ 20 "Disparity Error.")
(defconstant +ahci-PxSERR-DIAG-B+ 19 "10B to 8B Decode Error.")
(defconstant +ahci-PxSERR-DIAG-W+ 18 "Comm Wake.")
(defconstant +ahci-PxSERR-DIAG-I+ 17 "Phy Internal Error.")
(defconstant +ahci-PxSERR-DIAG-N+ 16 "PhyRdy Change.")
(defconstant +ahci-PxSERR-ERR-E+ 11 "Internal Error.")
(defconstant +ahci-PxSERR-ERR-P+ 10 "Protocol Error.")
(defconstant +ahci-PxSERR-ERR-C+ 9 "Persistent Communication or Data Integrity Error.")
(defconstant +ahci-PxSERR-ERR-T+ 8 "Transient Data Integrity Error.")
(defconstant +ahci-PxSERR-ERR-M+ 1 "Recovered Communications Error.")
(defconstant +ahci-PxSERR-ERR-I+ 0 "Recovered Data Integrity Error.")

;;; Bits in PxFBS.
(defconstant +ahci-PxFBS-DWE-position+ 16 "Device With Error.")
(defconstant +ahci-PxFBS-DWE-size+ 4 "DWE field size.")
(defconstant +ahci-PxFBS-ADO-position+ 12 "Active Device Optimization.")
(defconstant +ahci-PxFBS-ADO-size+ 4 "ADO field size.")
(defconstant +ahci-PxFBS-DEV-position+ 8 "Device To Issue.")
(defconstant +ahci-PxFBS-DEV-size+ 4 "DEV field size.")
(defconstant +ahci-PxFBS-SDE+ 2 "Single Device Error.")
(defconstant +ahci-PxFBS-DEC+ 1 "Device Error Clear.")
(defconstant +ahci-PxFBS-EN+ 0 "Enable.")

;;; Command Header.
(defconstant +ahci-ch-descriptor-information+ 0)
(defconstant +ahci-ch-PRDBC+ 1 "Physical Region Descriptor Byte Count.")
(defconstant +ahci-ch-CTBA+ 2 "Command Table Descriptor Base Address.")
(defconstant +ahci-ch-CTBAU+ 3 "Command Table Descriptor Base Address Upper 32-bits.")

(defconstant +ahci-command-header-size+ #x20)
(defconstant +ahci-maximum-n-command-headers+ 32)

;;; Bits in the Command Header Descriptor Information.
(defconstant +ahci-ch-di-PRDTL-position+ 16 "Physical Region Descriptor Table Length.")
(defconstant +ahci-ch-di-PRDTL-size+ 16 "PRDTL field size.")
(defconstant +ahci-ch-di-PMP-position+ 12 "Port Multiplier Port.")
(defconstant +ahci-ch-di-PMP-size+ 4 "PMP field size.")
(defconstant +ahci-ch-di-C+ 10 "Clear Busy Upon R_OK.")
(defconstant +ahci-ch-di-B+ 9 "BIST")
(defconstant +ahci-ch-di-R+ 8 "Reset.")
(defconstant +ahci-ch-di-P+ 7 "Prefetchable.")
(defconstant +ahci-ch-di-W+ 6 "Write.")
(defconstant +ahci-ch-di-A+ 5 "ATAPI.")
(defconstant +ahci-ch-di-CFL-position+ 0 "Command FIS Length.")
(defconstant +ahci-ch-di-CFL-size+ 5 "CFL field size.")

;;; Command Table Offsets.
(defconstant +ahci-ct-CFIS+ #x0 "Command FIS.")
(defconstant +ahci-ct-ACMD+ #x40 "ATAPI Command.")
(defconstant +ahci-ct-PRDT+ #x80 "Physical Region Descriptor Table.")

;;; Physical Region Descriptor Table Entry.
(defconstant +ahci-PRDT-DBA+ 0 "Data Base Address.")
(defconstant +ahci-PRDT-DBAU+ 1 "Data Base Address Upper 32-bits.")
(defconstant +ahci-PRDT-descriptor-information+ 3)

;;; Bits in the Physical Region Descriptor Table Descriptor Information.
(defconstant +ahci-PRDT-di-I+ 31 "Interrupt on completion.")
(defconstant +ahci-PRDT-di-DBC-position+ 0 "Data Byte Count.")
(defconstant +ahci-PRDT-di-DBC-size+ 22 "DBC field size.")

;;; Received FIS Structure offsets.
(defconstant +ahci-rfis-DSFIS+ #x00 "DMA Setup FIS.")
(defconstant +ahci-rfis-PSFIS+ #x20 "PIO Setup FIS.")
(defconstant +ahci-rfis-RFIS+ #x40 "D2H Register FIS.")
(defconstant +ahci-rfis-SDBFIS+ #x58 "Set Device Bits FIS.")
(defconstant +ahci-rfis-UFIS+ #x60 "Unknown FIS.")

(defconstant +ahci-rfis-size+ #x100)

;;; SATA FIS types.
(defconstant +sata-fis-register-h2d+ #x27)
(defconstant +sata-fis-register-d2h+ #x34)
(defconstant +sata-fis-set-device-bits-d2h+ #xA1)
(defconstant +sata-fis-dma-activate-d2h+ #x39)
(defconstant +sata-fis-dma-setup+ #x41)
(defconstant +sata-fis-bist-activate+ #x58)
(defconstant +sata-fis-pio-setup-d2h+ #x5F)
(defconstant +sata-fis-data+ #x46)

;;; ATA register offsets in the register FIS.
(defconstant +sata-register-fis-size+ 5) ; in dwords.
(defconstant +sata-register-fis-type+ 0)
(defconstant +sata-register-command-register-update-field+ 1)
(defconstant +sata-register-command-register-update-bit+ 7)
(defconstant +sata-register-command+ 2) ; h2d
(defconstant +sata-register-status+ 2) ; d2h
(defconstant +sata-register-features+ 3) ; h2d
(defconstant +sata-register-error+ 3) ; d2h
(defconstant +sata-register-lba-low+ 4)
(defconstant +sata-register-lba-mid+ 5)
(defconstant +sata-register-lba-high+ 6)
(defconstant +sata-register-device+ 7)
(defconstant +sata-register-lba-low-exp+ 8)
(defconstant +sata-register-lba-mid-exp+ 9)
(defconstant +sata-register-lba-high-exp+ 10)
(defconstant +sata-register-features-exp+ 11) ; h2d
(defconstant +sata-register-count+ 12)
(defconstant +sata-register-count-exp+ 13)
(defconstant +sata-register-control+ 15) ; h2d

(defstruct (ahci
             (:area :wired))
  location
  abar
  irq-handler-function
  (ports (sys.int::make-simple-vector +ahci-maximum-n-ports+ :wired)))

(defstruct (ahci-port
             (:area :wired))
  ahci
  id
  command-list
  received-fis
  command-table
  (irq-latch (make-latch "AHCI Port IRQ Notifier"))
  atapi-p
  cdb-size
  lba48-capable
  sector-size
  sector-count)

(defun ahci-port (ahci port)
  (svref (ahci-ports ahci) port))

(defun ahci-global-register (ahci register)
  (pci-io-region/32 (ahci-abar ahci) register))

(defun (setf ahci-global-register) (value ahci register)
  (setf (pci-io-region/32 (ahci-abar ahci) register) value))

(defun ahci-port-register (ahci port register)
  (pci-io-region/32 (ahci-abar ahci) (+ +ahci-port0-base+ (* port +ahci-port-control-size+) register)))

(defun (setf ahci-port-register) (value ahci port register)
  (setf (pci-io-region/32 (ahci-abar ahci) (+ +ahci-port0-base+ (* port +ahci-port-control-size+) register)) value))

(defun ahci-64-bit-p (ahci)
  (logbitp +ahci-CAP-S46A+
           (ahci-global-register ahci +ahci-register-CAP+)))

(defun ahci-dump-global-registers (ahci)
  (debug-print-line "Host Capabilities " (ahci-global-register ahci +ahci-register-CAP+))
  (debug-print-line "Global Host Control " (ahci-global-register ahci +ahci-register-GHC+))
  (debug-print-line "Interrupt Status " (ahci-global-register ahci +ahci-register-IS+))
  (debug-print-line "Ports Implemented " (ahci-global-register ahci +ahci-register-PI+))
  (debug-print-line "Version " (ahci-global-register ahci +ahci-register-VS+))
  (debug-print-line "Command Completion Coalescing Control " (ahci-global-register ahci +ahci-register-CCC_CTL+))
  (debug-print-line "Command Completion Coalescing Ports " (ahci-global-register ahci +ahci-register-CCC_PORTS+))
  (debug-print-line "Enclosure Management Location " (ahci-global-register ahci +ahci-register-EM_LOC+))
  (debug-print-line "Enclosure Management Control " (ahci-global-register ahci +ahci-register-EM_CTL+))
  (debug-print-line "Host Capabilities Extended " (ahci-global-register ahci +ahci-register-CAP2+))
  (debug-print-line "BIOS/OS Handoff Control and Status " (ahci-global-register ahci +ahci-register-BOHC+)))

(defun ahci-dump-port-registers (ahci port)
  (debug-print-line " Command List Base Address " (ahci-port-register ahci port +ahci-register-PxCLB+))
  (debug-print-line " Command List Base Address Upper 32-bits " (ahci-port-register ahci port +ahci-register-PxCLBU+))
  (debug-print-line " FIS Base Address " (ahci-port-register ahci port +ahci-register-PxFB+))
  (debug-print-line " FIS Base Address Upper 32-bits " (ahci-port-register ahci port +ahci-register-PxFBU+))
  (debug-print-line " Interrupt Status " (ahci-port-register ahci port +ahci-register-PxIS+))
  (debug-print-line " Interrupt Enable " (ahci-port-register ahci port +ahci-register-PxIE+))
  (debug-print-line " Command and Status " (ahci-port-register ahci port +ahci-register-PxCMD+))
  (debug-print-line " Task File Data " (ahci-port-register ahci port +ahci-register-PxTFD+))
  (debug-print-line " Signature " (ahci-port-register ahci port +ahci-register-PxSIG+))
  (debug-print-line " SATA Status (SCR0: SStatus) " (ahci-port-register ahci port +ahci-register-PxSSTS+))
  (debug-print-line " SATA Control (SCR2: SControl) " (ahci-port-register ahci port +ahci-register-PxSCTL+))
  (debug-print-line " SATA Error (SCR1: SError) " (ahci-port-register ahci port +ahci-register-PxSERR+))
  (debug-print-line " SATA Active (SCR3: SActive) " (ahci-port-register ahci port +ahci-register-PxSACT+))
  (debug-print-line " Command Issue " (ahci-port-register ahci port +ahci-register-PxCI+))
  (debug-print-line " SATA Notification (SCR4: SNotification) " (ahci-port-register ahci port +ahci-register-PxSNTF+))
  (debug-print-line " FIS-based Switching Control " (ahci-port-register ahci port +ahci-register-PxFBS+)))

(defun dump-mem (base count)
  (dotimes (i count)
    (debug-print-line (+ base (* i 16)) ":"
                      " " (physical-memref-unsigned-byte-32 base (+ (* i 4) 0))
                      " " (physical-memref-unsigned-byte-32 base (+ (* i 4) 1))
                      " " (physical-memref-unsigned-byte-32 base (+ (* i 4) 2))
                      " " (physical-memref-unsigned-byte-32 base (+ (* i 4) 3)))))

(defun ahci-port-reset (ahci port)
  (debug-print-line "Resetting port " port)
  ;; Stop command processing. Clear ST and wait for CR to clear.
  (setf (ldb (byte 1 +ahci-PxCMD-ST+)
             (ahci-port-register ahci port +ahci-register-PxCMD+))
        0)
  (loop
     (when (not (logbitp +ahci-PxCMD-CR+
                         (ahci-port-register ahci port +ahci-register-PxCMD+)))
       (return)))
  ;; Issue COMRESET.
  (debug-print-line "Issue COMRESET.")
  (setf (ldb (byte +ahci-PxSCTL-DET-size+ +ahci-PxSCTL-DET-position+)
             (ahci-port-register ahci port +ahci-register-PxSCTL+))
        1)
  (sleep 0.01) ; TODO: Figure out what timeout is actually required here.
  (setf (ldb (byte +ahci-PxSCTL-DET-size+ +ahci-PxSCTL-DET-position+)
             (ahci-port-register ahci port +ahci-register-PxSCTL+))
        0)
  ;; Wait for PHY communication to be reestablished.
  (debug-print-line "Waiting for PHY.")
  (loop
     (when (logbitp 0
                    (ldb (byte +ahci-PxSSTS-DET-size+ +ahci-PxSSTS-DET-position+)
                         (ahci-port-register ahci port +ahci-register-PxSSTS+)))
       (return)))
  ;; Clear errors.
  (setf (ahci-port-register ahci port +ahci-register-PxSERR+) #xFFFFFFFF)
  ;; Wait for BSY to clear and the device to come back up.
  (debug-print-line "Waiting for BSY to clear.")
  (loop
     (let* ((tfd (ahci-port-register ahci port +ahci-register-PxTFD+))
            (sts (ldb (byte +ahci-PxTFD-STS-size+ +ahci-PxTFD-STS-position+) tfd)))
       (when (not (logtest +ata-bsy+ sts))
         (return))))
  ;; Reenable command processing.
    ;; Stop command processing. Clear ST and wait for CR to clear.
  (setf (ldb (byte 1 +ahci-PxCMD-ST+)
             (ahci-port-register ahci port +ahci-register-PxCMD+))
        1)
  (debug-print-line "Port reset complete."))

(defun ahci-issue-packet-command (port-info cdb result-buffer result-len)
  (ensure (ahci-port-atapi-p port-info))
  ;; FIXME: Bounce buffer on non-64-bit capable HBAs
  (cond (result-buffer
         (setf result-buffer (- result-buffer +physical-map-base+)))
        (t
         (setf result-buffer 0)))
  (let ((ahci (ahci-port-ahci port-info))
        (port (ahci-port-id port-info)))
    (ahci-setup-buffer ahci port result-buffer result-len nil t)
    ;; Set registers in the FIS for an ATAPI command.
    ;; DMA, device-to-host direction.
    (setf (ahci-fis ahci port +sata-register-features+) #b0101)
    (setf (ahci-fis ahci port +sata-register-lba-mid+) (ldb (byte 8 0) result-len)
          (ahci-fis ahci port +sata-register-lba-high+) (ldb (byte 8 8) result-len))
    ;; Fill the ACMD field.
    (let* ((command-table (ahci-port-command-table port-info))
           (ct (+ command-table +ahci-ct-ACMD+)))
      (dotimes (i (ahci-port-cdb-size port-info))
        (setf (physical-memref-unsigned-byte-8 ct i) (svref cdb i))))
    (ahci-run-command ahci port +ata-command-packet+)
    (let* ((tfd (ahci-port-register ahci port +ahci-register-PxTFD+))
           (sts (ldb (byte +ahci-PxTFD-STS-size+ +ahci-PxTFD-STS-position+) tfd)))
      (cond ((logtest sts +ata-err+)
             (debug-print-line "PACKET command failed. TFD: " tfd)
             nil)
            (t result-len)))))

(defun ahci-rw-command (port-info lba count mem-addr command command-ext write)
  (ensure (not (ahci-port-atapi-p port-info)))
  ;; FIXME: Bounce buffer on non-64-bit capable HBAs
  (setf mem-addr (- mem-addr +physical-map-base+))
  (let ((ahci (ahci-port-ahci port-info))
        (port (ahci-port-id port-info)))
    (ahci-setup-buffer ahci port mem-addr (* count (ahci-port-sector-size port-info)) write nil)
    (cond ((ahci-port-lba48-capable port-info)
           (ahci-setup-lba48 ahci port lba count)
           (ahci-run-command ahci port command-ext))
          (t
           (ahci-setup-lba28 ahci port lba count)
           (ahci-run-command ahci port command)))
    (let* ((tfd (ahci-port-register ahci port +ahci-register-PxTFD+))
           (sts (ldb (byte +ahci-PxTFD-STS-size+ +ahci-PxTFD-STS-position+) tfd)))
      (cond ((logtest sts +ata-err+)
             (debug-print-line (if write "Write" "Read") " failed. TFD: " tfd)
             nil)
            (t t)))))

(defun ahci-read (port-info lba count mem-addr)
  (ahci-rw-command port-info
                   lba
                   count
                   mem-addr
                   +ata-command-read-dma+
                   +ata-command-read-dma-ext+
                   nil))

(defun ahci-write (port-info lba count mem-addr)
  (ahci-rw-command port-info
                   lba
                   count
                   mem-addr
                   +ata-command-write-dma+
                   +ata-command-write-dma-ext+
                   t))

(defun ahci-detect-atapi-drive (ahci port)
  ;; Issue IDENTIFY PACKET.
  ;; Dump the IDENTIFY PACKET data just after the command table.
  (let* ((port-info (ahci-port ahci port))
         (identify-data-phys (+ (ahci-port-command-table port-info) #x100))
         (identify-data (convert-to-pmap-address identify-data-phys)))
    (ahci-setup-buffer ahci port identify-data-phys 512 nil nil)
    (ahci-dump-port-registers ahci port)
    (ahci-run-command ahci port +ata-command-identify-packet+)
    (let* ((tfd (ahci-port-register ahci port +ahci-register-PxTFD+))
           (sts (ldb (byte +ahci-PxTFD-STS-size+ +ahci-PxTFD-STS-position+) tfd)))
      (when (logtest sts +ata-err+)
        (debug-print-line "IDENTIFY PACKET aborted by device. TFD: " tfd)
        (return-from ahci-detect-atapi-drive)))
    ;; Done!
    (debug-print-line "Command completed.")
    (dump-mem identify-data-phys 32)
    (let* ((general-config (memref-ub16/le identify-data 0))
           (cdb-size (case (ldb (byte 2 0) general-config)
                       (0 12)
                       (1 16))))
      (debug-print-line "PACKET device:")
      (debug-print-line " General configuration: " general-config)
      (debug-print-line " Specific configuration: " (memref-ub16/le identify-data 2))
      (debug-print-line " Capabilities: " (memref-ub16/le identify-data 49) " " (memref-ub16/le identify-data 50))
      (debug-print-line " Field validity: " (memref-ub16/le identify-data 53))
      (debug-print-line " DMADIR: " (memref-ub16/le identify-data 62))
      (debug-print-line " Multiword DMA transfer: " (memref-ub16/le identify-data 63))
      (debug-print-line " PIO transfer mode supported: " (memref-ub16/le identify-data 64))
      (debug-print-line " Features: " (memref-ub16/le identify-data 82) " " (memref-ub16/le identify-data 83) " " (memref-ub16/le identify-data 84) " " (memref-ub16/le identify-data 85) " " (memref-ub16/le identify-data 86) " " (memref-ub16/le identify-data 87))
      (debug-print-line " Removable Media Notification: " (memref-ub16/le identify-data 127))
      (when (or (not (logbitp 15 general-config))
                (logbitp 14 general-config))
        (debug-print-line "Device does not implement the PACKET command set.")
        (return-from ahci-detect-atapi-drive))
      (when (not (eql (ldb (byte 5 8) general-config) 5))
        (debug-print-line "PACKET device is not a CD-ROM drive.")
        (return-from ahci-detect-atapi-drive))
      (when (not cdb-size)
        (debug-print-line "PACKET device has unsupported CDB size " (ldb (byte 2 0) general-config))
        (return-from ahci-detect-atapi-drive))
      (setf (ahci-port-atapi-p port-info) t
            (ahci-port-cdb-size port-info) cdb-size)
      (cdrom-initialize-device port-info cdb-size 'ahci-issue-packet-command))))

(defun ahci-detect-drive (ahci port)
  ;; Issue IDENTIFY.
  ;; Dump the IDENTIFY data just after the command table.
  (let* ((port-info (ahci-port ahci port))
         (identify-data-phys (+ (ahci-port-command-table port-info) #x100))
         (identify-data (convert-to-pmap-address identify-data-phys)))
    (ahci-setup-buffer ahci port identify-data-phys 512 nil nil)
    (ahci-dump-port-registers ahci port)
    (ahci-run-command ahci port +ata-command-identify+)
    (let* ((tfd (ahci-port-register ahci port +ahci-register-PxTFD+))
           (sts (ldb (byte +ahci-PxTFD-STS-size+ +ahci-PxTFD-STS-position+) tfd)))
      (when (logtest sts +ata-err+)
        (debug-print-line "IDENTIFY aborted by device. TFD: " tfd)
        (return-from ahci-detect-drive)))
    ;; Done!
    (debug-print-line "Command completed.")
    (dump-mem identify-data-phys 32)
    (let* ((supported-command-sets (memref-ub16/le identify-data 83))
           (lba48-capable (logbitp 10 supported-command-sets))
           (sector-size (if (and (logbitp 14 (memref-ub16/le identify-data 106))
                                 (not (logbitp 13 (memref-ub16/le identify-data 106))))
                            ;; Data in logical sector size field valid.
                            (logior (memref-ub16/le identify-data 117)
                                    (ash (memref-ub16/le identify-data 118) 16))
                            ;; Not valid, use 512.
                            512))
           (sector-count (if lba48-capable
                             (logior (memref-ub16/le identify-data 100)
                                     (ash (memref-ub16/le identify-data 101) 16)
                                     (ash (memref-ub16/le identify-data 102) 32)
                                     (ash (memref-ub16/le identify-data 103) 48))
                             (logior (memref-ub16/le identify-data 60)
                                     (ash (memref-ub16/le identify-data 61) 16)))))
      (setf (ahci-port-lba48-capable port-info) lba48-capable
            (ahci-port-sector-size port-info) sector-size
            (ahci-port-sector-count port-info) sector-count)
      (debug-print-line "Features (83): " supported-command-sets)
      (debug-print-line "Sector size: " sector-size)
      (debug-print-line "Sector count: " sector-count)
      ;; FIXME: Can transfer more than 256 sectors at once...
      (register-disk port-info t sector-count sector-size 256 'ahci-read 'ahci-write))))

(defun (setf ahci-fis) (value ahci port offset)
  "Write an octet into the command FIS for PORT."
  (let* ((command-table (ahci-port-command-table (ahci-port ahci port)))
         (ct (+ command-table +ahci-ct-CFIS+)))
    (setf (physical-memref-unsigned-byte-8 ct offset) value)))

(defun ahci-setup-lba28 (ahci port lba count)
  "Set registers in the FIS for an LBA28 command."
  ;; FIXME: Limit checking LBA & COUNT and if COUNT = MAX, then handle that.
  ;; Count.
  (setf (ahci-fis ahci port +sata-register-count+) (ldb (byte 8 0) count))
  ;; LBA.
  (setf (ahci-fis ahci port +sata-register-lba-low+) (ldb (byte 8 0) lba)
        (ahci-fis ahci port +sata-register-lba-mid+) (ldb (byte 8 8) lba)
        (ahci-fis ahci port +sata-register-lba-high+) (ldb (byte 8 16) lba))
  ;; LBA bit and high bits of LBA.
  (setf (ahci-fis ahci port +sata-register-device+) (logior +ata-lba+
                                                            (ldb (byte 4 24) lba))))

(defun ahci-setup-lba48 (ahci port lba count)
  "Set registers in the FIS for an LBA48 command."
  ;; FIXME: Limit checking LBA & COUNT and if COUNT = MAX, then handle that.
  ;; Count.
  (setf (ahci-fis ahci port +sata-register-count+) (ldb (byte 8 0) count)
        (ahci-fis ahci port +sata-register-count-exp+) (ldb (byte 8 8) count))
  ;; LBA.
  (setf (ahci-fis ahci port +sata-register-lba-low+) (ldb (byte 8 0) lba)
        (ahci-fis ahci port +sata-register-lba-mid+) (ldb (byte 8 8) lba)
        (ahci-fis ahci port +sata-register-lba-high+) (ldb (byte 8 16) lba)
        (ahci-fis ahci port +sata-register-lba-low-exp+) (ldb (byte 8 24) lba)
        (ahci-fis ahci port +sata-register-lba-mid-exp+) (ldb (byte 8 32) lba)
        (ahci-fis ahci port +sata-register-lba-high-exp+) (ldb (byte 8 40) lba))
  ;; LBA bit.
  (setf (ahci-fis ahci port +sata-register-device+) +ata-lba+))

(defun ahci-setup-buffer (ahci port buffer length write atapi)
  "Configure the DMA buffer for this command."
  (let* ((ct (ahci-port-command-table (ahci-port ahci port)))
         (cl (ahci-port-command-list (ahci-port ahci port))))
    ;; Update write bit in the command header.
    (setf (ldb (byte 1 +ahci-ch-di-W+)
               (physical-memref-unsigned-byte-32 cl +ahci-ch-descriptor-information+))
          (if write 1 0))
    (setf (ldb (byte 1 +ahci-ch-di-A+)
               (physical-memref-unsigned-byte-32 cl +ahci-ch-descriptor-information+))
          (if atapi 1 0))
    ;; Point the PRDT 0 to the buffer.
    (setf (physical-memref-unsigned-byte-32 (+ ct +ahci-ct-PRDT+) +ahci-PRDT-DBA+) (ldb (byte 32 0) buffer)
          (physical-memref-unsigned-byte-32 (+ ct +ahci-ct-PRDT+) +ahci-PRDT-DBAU+) (ldb (byte 32 32) buffer))
    (setf (physical-memref-unsigned-byte-32 (+ ct +ahci-ct-PRDT+) +ahci-PRDT-descriptor-information+) (ash (1- length) +ahci-PRDT-di-DBC-position+))))

(defun ahci-run-command (ahci port command)
  (let* ((port-info (ahci-port ahci port))
         (irq-latch (ahci-port-irq-latch port-info)))
    ;; Reset PRDBC, stop it accumulating over commands. I'm not sure how the HBA actually uses this,
    ;; if it keeps track of DMA progress or if it's just for reporting.
    (setf (physical-memref-unsigned-byte-32 (ahci-port-command-list port-info) +ahci-ch-PRDBC+) 0)
    ;; Final FIS configuration.
    (setf (ahci-fis ahci port +sata-register-fis-type+) +sata-fis-register-h2d+
          (ahci-fis ahci port +sata-register-command-register-update-field+) (ash 1 +sata-register-command-register-update-bit+)
          (ahci-fis ahci port +sata-register-command+) command)
    ;; Reset latch before starting the command.
    (latch-reset irq-latch)
    ;; Start command 1.
    (setf (ahci-port-register ahci port +ahci-register-PxCI+) 1)
    ;; Wait for it... Wait for it...
    (latch-wait irq-latch)))

(defun ahci-initialize-port (ahci port)
  ;; Disable Command List processing and FIS RX.
  (let ((cmd (ahci-port-register ahci port +ahci-register-PxCMD+)))
    (setf (ldb (byte 1 +ahci-PxCMD-ST+) cmd) 0
          (ldb (byte 1 +ahci-PxCMD-FRE+) cmd) 0)
    (setf (ahci-port-register ahci port +ahci-register-PxCMD+) cmd))
  ;; Wait for CR and FR to clear before changing Command List/FIS base addresses.
  ;; TODO: Put a timeout on this and reset the port if needed.
  (debug-print-line "Waiting for CR/FR to stop.")
  (loop
     (let ((cmd (ahci-port-register ahci port +ahci-register-PxCMD+)))
       (when (and (not (logbitp +ahci-PxCMD-CR+ cmd))
                  (not (logbitp +ahci-PxCMD-FR+ cmd)))
         (return))))
  ;; Allocate the Command List, Received FIS and one Command Table.
  (let* ((port-data (allocate-physical-pages 1
                                             :mandatory-p "AHCI Port"
                                             :32-bit-only (not (ahci-64-bit-p ahci))))
         (port-data-phys (* port-data +4k-page-size+))
         (command-list port-data-phys)
         (received-fis (+ command-list (* +ahci-command-header-size+
                                          +ahci-maximum-n-command-headers+)))
         (command-table (+ received-fis +ahci-rfis-size+)))
    (debug-print-line "Allocated port data at " port-data-phys)
    (debug-print-line "Command List at " command-list)
    (debug-print-line "Received FIS at " received-fis)
    (debug-print-line "Command Tabl at " command-table)
    (setf (ahci-port-command-list (ahci-port ahci port)) command-list
          (ahci-port-received-fis (ahci-port ahci port)) received-fis
          (ahci-port-command-table (ahci-port ahci port)) command-table)
    ;; Clear Command List & RFIS before programming.
    (zeroize-physical-page port-data-phys)
    ;; Write addresses.
    (setf (ahci-port-register ahci port +ahci-register-PxCLB+)
          (ldb (byte 32 0) command-list))
    (setf (ahci-port-register ahci port +ahci-register-PxCLBU+)
          (ldb (byte 32 32) command-list))
    (setf (ahci-port-register ahci port +ahci-register-PxFB+)
          (ldb (byte 32 0) received-fis))
    (setf (ahci-port-register ahci port +ahci-register-PxFBU+)
          (ldb (byte 32 32) received-fis))
    ;; Disable power managment transitions.
    (setf (ldb (byte +ahci-PxSCTL-IPM-size+ +ahci-PxSCTL-IPM-position+)
               (ahci-port-register ahci port +ahci-register-PxSCTL+))
          +ahci-PxSCTL-IPM-partial-and-slumber-disabled+)
    (let ((cmd (ahci-port-register ahci port +ahci-register-PxCMD+))
          (cap (ahci-global-register ahci +ahci-register-CAP+)))
      ;; Power on and spin up the device.
      (when (logbitp +ahci-PxCMD-CPD+ cmd)
        (setf (ldb (byte 1 +ahci-PxCMD-POD+) cmd) 1))
      (when (logbitp +ahci-CAP-SSS+ cap)
        (setf (ldb (byte 1 +ahci-PxCMD-SUD+) cmd) 1))
      ;; Activate the port.
      (setf (ldb (byte +ahci-PxCMD-ICC-size+ +ahci-PxCMD-ICC-position+) cmd)
            +ahci-PxCMD-ICC-active+)
      ;; Enable FIS RX.
      (setf (ldb (byte 1 +ahci-PxCMD-FRE+) cmd) 1)
      ;; And command processing.
      (setf (ldb (byte 1 +ahci-PxCMD-ST+) cmd) 1)
      (setf (ahci-port-register ahci port +ahci-register-PxCMD+) cmd))
    ;; Flush PxSERR.
    (setf (ahci-port-register ahci port +ahci-register-PxSERR+) #xFFFFFFFF)
    ;; Clear any outstanding interrupts on this port.
    (setf (ahci-port-register ahci port +ahci-register-PxIS+)
          (ahci-port-register ahci port +ahci-register-PxIS+))
    (setf (ahci-global-register ahci +ahci-register-IS+) (ash 1 port))
    ;; Clear all the interrupt enable bits.
    ;; Keep PCS and PRCS disabled, I get a flood of these on my machine.
    (setf (ahci-port-register ahci port +ahci-register-PxIE+) (logior (ash 1 +ahci-PxIS-DHRS+)
                                                                      (ash 1 +ahci-PxIS-SDBS+)
                                                                      (ash 1 +ahci-PxIS-PSS+)
                                                                      (ash 1 +ahci-PxIS-DSS+)
                                                                      (ash 1 +ahci-PxIS-DPS+)
                                                                      (ash 1 +ahci-PxIS-UFS+)
                                                                      (ash 1 +ahci-PxIS-IPMS+)
                                                                      (ash 1 +ahci-PxIS-OFS+)
                                                                      (ash 1 +ahci-PxIS-INFS+)
                                                                      (ash 1 +ahci-PxIS-IFS+)
                                                                      (ash 1 +ahci-PxIS-HBDS+)
                                                                      (ash 1 +ahci-PxIS-HBFS+)
                                                                      (ash 1 +ahci-PxIS-TFES+)))
    (let ((cl command-list))
      ;; Prep command header 0.
      (setf (physical-memref-unsigned-byte-32 cl +ahci-ch-descriptor-information+)
            ;; Sending a register FIS (length 5) with one PRDT entry.
            (logior (ash +sata-register-fis-size+ +ahci-ch-di-CFL-position+)
                    (ash 1 +ahci-ch-di-PRDTL-position+)
                    (ash 1 +ahci-ch-di-P+)))
      (setf (physical-memref-unsigned-byte-32 cl +ahci-ch-CTBA+) (ldb (byte 32 0) command-table))
      (setf (physical-memref-unsigned-byte-32 cl +ahci-ch-CTBAU+) (ldb (byte 32 32) command-table)))))

(defun ahci-port-irq-handler (ahci port)
  (let ((state (ahci-port-register ahci port +ahci-register-PxIS+)))
    (when (not (zerop state))
      #+(or)(debug-print-line "AHCI IRQ for port " port ": " state)
      ;; Need to do something with error interrupts here as well.
      ;; Wake sleepers.
      (latch-trigger (ahci-port-irq-latch (ahci-port ahci port)))
      ;; Ack interrupts.
      (setf (ahci-port-register ahci port +ahci-register-PxIS+) state))))

(defun ahci-irq-handler (ahci)
  (let ((pending (ahci-global-register ahci +ahci-register-IS+))
        (ports (ahci-ports ahci)))
    #+(or)(debug-print-line "AHCI IRQ " pending)
    (dotimes (i 32)
      (when (and (svref ports i)
                 (logbitp i pending))
        (ahci-port-irq-handler ahci i)))
    ;; And clear any pending interrupts that were handled.
    (setf (ahci-global-register ahci +ahci-register-IS+) pending)))

(defun ahci-pci-register (location)
  ;; Wired allocation required for the IRQ handler closure.
  (declare (sys.c::closure-allocation :wired))
  (let ((ahci (make-ahci :location location
                         :abar (pci-io-region location 5 #x2000))))
    (setf (ahci-irq-handler-function ahci) (lambda (interrupt-frame irq)
                                             (declare (ignore interrupt-frame irq))
                                             (ahci-irq-handler ahci)))
    (debug-print-line "Detected AHCI ABAR at " (ahci-abar ahci))
    (debug-print-line "AHCI IRQ is " (pci-intr-line location))
    (ahci-dump-global-registers ahci)
    ;; Verify version.
    (let* ((version (ahci-global-register ahci +ahci-register-VS+))
           (major (ldb (byte +ahci-VS-MJR-size+ +ahci-VS-MJR-position+) version))
           (minor (ldb (byte +ahci-VS-MNR-size+ +ahci-VS-MNR-position+) version)))
      (debug-print-line "AHCI HBA version " version)
      (when (not (eql major 1))
        (debug-print-line "Major version " major " not supported.")
        (return-from ahci-pci-register)))
    ;; Clear IE and set AE in GHC.
    (setf (ahci-global-register ahci +ahci-register-GHC+) (ash 1 +ahci-GHC-AE+))
    ;; Attach interrupt handler.
    (debug-print-line "Handler: " (ahci-irq-handler ahci))
    (i8259-hook-irq (pci-intr-line location) (ahci-irq-handler-function ahci))
    ;; Make sure to enable PCI bus mastering for this device.
    (debug-print-line "Config register: " (pci-config/16 location +pci-config-command+))
    (setf (pci-config/16 location +pci-config-command+) (logior (pci-config/16 location +pci-config-command+)
                                                                (ash 1 +pci-command-bus-master+)))
    ;; Magic hacks for Intel devices?
    ;; Set port enable bits in Port Control and Status on Intel controllers.
    (when (eql (pci-config/16 location +pci-config-vendorid+) #x8086)
      (let* ((n-ports (1+ (ldb (byte +ahci-CAP-NP-size+ +ahci-CAP-NP-position+)
                               (ahci-global-register ahci +ahci-register-CAP+))))
             (pcs (pci-config/16 location #x92)))
        (setf (pci-config/16 location #x92) (logior pcs
                                                    (ash #xFF (- (- 8 n-ports)))))))
    ;; Initialize each port.
    (dotimes (i 32)
      (setf (svref (ahci-ports ahci) i) nil)
      (when (logbitp i (ahci-global-register ahci +ahci-register-PI+))
        (debug-print-line "Port " i)
        (setf (svref (ahci-ports ahci) i) (make-ahci-port :ahci ahci
                                                          :id i))
        (ahci-initialize-port ahci i)))
    ;; Port interrupts cleared, now clear HBA interrupts.
    (setf (ahci-global-register ahci +ahci-register-IS+) (ahci-global-register ahci +ahci-register-IS+))
    ;; Now safe to enable HBA interrupts.
    (setf (ldb (byte 1 +ahci-GHC-IE+) (ahci-global-register ahci +ahci-register-GHC+)) 1)
    (i8259-unmask-irq (pci-intr-line location))
    ;; Initialize devices attached to ports.
    (dotimes (port 32)
      (when (ahci-port ahci port)
        (let* ((tfd (ahci-port-register ahci port +ahci-register-PxTFD+))
               (sts (ldb (byte +ahci-PxTFD-STS-size+ +ahci-PxTFD-STS-position+) tfd))
               (sata-sts (ahci-port-register ahci port +ahci-register-PxSSTS+)))
          (cond ((and (not (logtest +ata-err+ sts))
                      (not (logtest +ata-drq+ sts))
                      (eql (ldb (byte +ahci-PxSSTS-DET-size+ +ahci-PxSSTS-DET-position+)
                                sata-sts)
                           +ahci-PxSSTS-DET-ready+))
                 ;; A device is attached and ready for use.
                 (debug-print-line "Initializing device on port " port)
                 (cond ((eql (ahci-port-register ahci port +ahci-register-PxSIG+)
                             #xEB140101)
                        (ahci-detect-atapi-drive ahci port))
                       (t
                        (ahci-detect-drive ahci port))))
                (t
                 (debug-print-line "No device present on port. TFD:" tfd " SSTS:" sata-sts))))))
    (ahci-dump-global-registers ahci)
    ))
