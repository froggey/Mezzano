(in-package :mezzano.supervisor)

(defun initialize-platform-early-console (boot-information-page)
  (declare (ignore boot-information-page))
  ;; TODO: This (along with the other serial settings) should be provided by the bootloader.
  (let ((serial-port-io-base #x3F8))
    (initialize-debug-serial serial-port-io-base 0 #'sys.int::io-port/8 #'(setf sys.int::io-port/8) 4 38400)))

(defun initialize-early-platform ()
  (initialize-interrupts)
  (initialize-i8259)
  (initialize-early-cpu))

(defun initialize-platform ()
  (let ((fadt (acpi-get-table 'acpi-fadt-table-p)))
    (cond
      (fadt
       (debug-print-line "ACPI FADT version: " (acpi-table-header-revision fadt))
       (when (> (acpi-table-header-revision fadt) 1)
         (debug-print-line "ACPI IA-PC boot flags: " (acpi-fadt-table-iapc-boot-arch fadt))))
      (t
       (debug-print-line "No ACPI FADT table detected.")))
    (initialize-cpu)
    (initialize-platform-time)
    (8042-probe)
    (initialize-pci)
    (when (not (boot-option +boot-option-no-detect+))
      (pci-detect))))

(defun platform-reboot ()
  (let ((fadt (acpi-get-table 'acpi-fadt-table-p)))
    (when fadt
      ;; ACPI reset.
      (let ((reset-address (acpi-fadt-table-reset-reg fadt))
            (reset-value (acpi-fadt-table-reset-value fadt)))
        (when (and reset-address
                   reset-value
                   (eql (acpi-generic-address-register-bit-width reset-address) 8)
                   (eql (acpi-generic-address-register-bit-offset reset-address) 0))
          (setf (acpi-generic-address reset-address) reset-value))))
    (when (or (not fadt)
              (< (acpi-table-header-revision fadt) 2)
              (logtest (acpi-fadt-table-iapc-boot-arch fadt)
                       +acpi-iapc-boot-arch-8042+))
      ;; Pulse the reset line via the PS/2 controller.
      (8042-wait-until-output-is-possible)
      ;; Pulse output line 0 low.
      (8042-register-write +8042-register-command+ +8042-command-reboot+)))
  ;; Give up. Trash the IDT and trigger a page-fault to triple-fault the CPU.
  (%lidt 0 0)
  (sys.int::memref-unsigned-byte-8 0 0))
