;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +efi-table-header-signature+ 0)
(defconstant +efi-table-header-revision+ 8)
(defconstant +efi-table-header-header-size+ 12)
(defconstant +efi-table-header-crc32+ 16)
(defconstant +efi-table-header-reserved+ 20)

(defconstant +efi-system-table-firmware-vendor+ 24)
(defconstant +efi-system-table-firmware-revision+ 32)
(defconstant +efi-system-table-console-in-handle+ 40)
(defconstant +efi-system-table-con-in+ 48)
(defconstant +efi-system-table-console-out-handle+ 56)
(defconstant +efi-system-table-con-out+ 64)
(defconstant +efi-system-table-standard-error-handle+ 72)
(defconstant +efi-system-table-std-err+ 80)
(defconstant +efi-system-table-runtime-services+ 88)
(defconstant +efi-system-table-boot-services+ 96)
(defconstant +efi-system-table-number-of-table-entries+ 104)
(defconstant +efi-system-table-configuration-table+ 112)

(defconstant +efi-runtime-services-get-time+ 24)
(defconstant +efi-runtime-services-set-time+ 32)
(defconstant +efi-runtime-services-get-wakeup-time+ 40)
(defconstant +efi-runtime-services-set-wakeup-time+ 48)
(defconstant +efi-runtime-services-set-virtual-address-map+ 56)
(defconstant +efi-runtime-services-convert-pointer+ 64)
(defconstant +efi-runtime-services-get-variable+ 72)
(defconstant +efi-runtime-services-get-next-variable-name+ 80)
(defconstant +efi-runtime-services-set-variable+ 88)
(defconstant +efi-runtime-services-get-next-high-monotonic-count+ 96)
(defconstant +efi-runtime-services-reset-system+ 104)
(defconstant +efi-runtime-services-update-capsule+ 112)
(defconstant +efi-runtime-services-query-capsule-capabilities+ 120)
(defconstant +efi-runtime-services-query-variable-info+ 128)

(defconstant +efi-configuration-table-vendor-guid+ 0)
(defconstant +efi-configuration-table-vendor-table+ 16)
(defconstant +efi-configuration-table-entry-size+ 24)

(sys.int::defglobal *efi-system-table*)

(defun efi-get-configuration-table (guid)
  (when *efi-system-table*
    (let ((config-table (physical-memref-unsigned-byte-64 (+ *efi-system-table* +efi-system-table-configuration-table+)))
          (n-config-table-entries (physical-memref-unsigned-byte-64 (+ *efi-system-table* +efi-system-table-number-of-table-entries+))))
      (dotimes (i n-config-table-entries nil)
        (flet ((guid (idx)
                 (physical-memref-unsigned-byte-8 (+ config-table
                                                     (* i +efi-configuration-table-entry-size+)
                                                     +efi-configuration-table-vendor-guid+)
                                                  idx)))
          (declare (dynamic-extent #'guid))
          (when (dotimes (j 16 t)
                  (when (not (eql (svref guid j) (guid j)))
                    (return nil)))
            (debug-print-line "Found at index " i)
            (return (physical-memref-unsigned-byte-64 (+ config-table
                                                         (* i +efi-configuration-table-entry-size+)
                                                         +efi-configuration-table-vendor-table+)))))))))

(defun initialize-efi ()
  (let ((sys-table (sys.int::memref-unsigned-byte-64
                    (+ *boot-information-page* +boot-information-efi-system-table+))))
    (setf *efi-system-table* (if (zerop sys-table)
                                 nil
                                 sys-table))
    (when *efi-system-table*
      (let ((config-table (physical-memref-unsigned-byte-64 (+ *efi-system-table* +efi-system-table-configuration-table+)))
            (n-config-table-entries (physical-memref-unsigned-byte-64 (+ *efi-system-table* +efi-system-table-number-of-table-entries+))))
        (debug-print-line "Booted from EFI, system table at " *efi-system-table*)
        (debug-print-line "  Firmware vendor: " (physical-memref-unsigned-byte-64 (+ *efi-system-table* +efi-system-table-firmware-vendor+)))
        (debug-print-line "  Firmware revision: " (physical-memref-unsigned-byte-32 (+ *efi-system-table* +efi-system-table-firmware-revision+)))
        (debug-print-line "  Runtime services: " (physical-memref-unsigned-byte-64 (+ *efi-system-table* +efi-system-table-runtime-services+)))
        (debug-print-line "  Configuration table: " config-table "  (" n-config-table-entries " entries)")
        (dotimes (i n-config-table-entries)
          (flet ((guid (idx)
                   (physical-memref-unsigned-byte-8 (+ config-table
                                                        (* i +efi-configuration-table-entry-size+)
                                                        +efi-configuration-table-vendor-guid+)
                                                    idx)))
            (declare (dynamic-extent #'guid))
            (debug-print-line "    Entry " i ": "
                              (guid 0) ":" (guid 1) ":" (guid 2) ":" (guid 3) ":"
                              (guid 4) ":" (guid 5) ":" (guid 6) ":" (guid 7) ":"
                              (guid 8) ":" (guid 9) ":" (guid 10) ":" (guid 11) ":"
                              (guid 12) ":" (guid 13) ":" (guid 14) ":" (guid 15)
                              " @ " (physical-memref-unsigned-byte-64 (+ config-table
                                                                         (* i +efi-configuration-table-entry-size+)
                                                                         +efi-configuration-table-vendor-table+)))))
))))
