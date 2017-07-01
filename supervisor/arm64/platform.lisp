;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defun initialize-fdt-dw-apb-uart-console (fdt-node address-cells size-cells)
  (let* ((reg (fdt-get-property fdt-node "reg"))
         (base-address (fdt-read-integer reg address-cells 0))
         (reg-shift-prop (fdt-get-property fdt-node "reg-shift"))
         (reg-shift (if reg-shift-prop
                        (fdt-read-u32 reg-shift-prop)
                        0)))
    (debug-print-line "DW-APB-UART at " base-address " reg-shift " reg-shift)
    (initialize-debug-serial base-address reg-shift
                             #'physical-memref-unsigned-byte-32
                             #'(setf physical-memref-unsigned-byte-32)
                             0 ; TODO: IRQ
                             115200 ; TODO: Get from console string.
                             nil))) ; TODO: reinit is buggy?

(defun initialize-platform-early-console (boot-information-page)
  (declare (ignore boot-information-page))
  (let* ((chosen (fdt-get-named-child-node (fdt-root) "chosen"))
         (stdout-path-prop (if chosen
                               (fdt-get-property chosen "stdout-path")
                               nil))
         (stdout-node (if stdout-path-prop
                          (fdt-resolve-prop-path stdout-path-prop)
                          nil)))
    (debug-print-line "stdout node is " stdout-node)
    (cond ((not stdout-node))
          ((fdt-compatible-p stdout-node "snps,dw-apb-uart")
           (initialize-fdt-dw-apb-uart-console stdout-node
                                               ;; FIXME!
                                               1 1))
          (t
           (debug-print-line "stdout node is an unsupported device")))))

(defun initialize-early-platform ()
  (when (not (fdt-present-p))
    (panic "No FDT provided"))
  (debug-print-line "Performing early FDT scan")
  (arm64-fdt-scan t))

(defun initialize-platform ()
  (debug-print-line "Performing FDT scan")
  (arm64-fdt-scan nil)
  ;; This is correct for qemu's cortex-a53 virt machine.
  #+(or)
  (dotimes (i 32)
    (virtio-mmio-register (+ #x0A000000 (* i #x200))
                          (+ 48 i))))

(defun arm64-fdt-scan (earlyp)
  (do-fdt-child-nodes (node (fdt-root))
    (cond ((fdt-compatible-p node "simple-bus")
           (debug-print-line "simple-bus at " node)
           (register-fdt-simple-bus node earlyp))
          ((fdt-compatible-p node "arm,armv8-timer")
           (when (not earlyp)
             (initialize-platform-time node)))
          (t
           (debug-print-line "unknown fdt node at " node)))))

(defun register-fdt-simple-bus (node earlyp)
  (let ((address-cells (fdt-address-cells node))
        (size-cells (fdt-size-cells node))
        (ranges (fdt-get-property node "ranges")))
    (when (not ranges)
      (debug-print-line "invalid simple-bus. missing ranges")
      (return-from register-fdt-simple-bus))
    (when (not (eql (fdt-property-length ranges) 0))
      ;; TODO.
      (debug-print-line "simple-bus with non-simple parent-child mapping, ignoring.")
      (return-from register-fdt-simple-bus))
    ;; Walk children, looking for thing.
    (do-fdt-child-nodes (child node)
      (cond ((fdt-compatible-p child "simple-bus")
             (debug-print-line "simple-bus at " child)
             (register-fdt-simple-bus child earlyp))
            ((fdt-compatible-p child "arm,gic-400")
             (when earlyp
               (initialize-fdt-gic-400 child address-cells size-cells)))
            #+(or) ; not implemented yet!
            ((fdt-compatible-p child "allwinner,sun4i-a10-timer")
             (when (not earlyp)
               (initialize-fdt-sun4i-a10-timer node address-cells size-cells)))
            (t
             (debug-print-line "unknown fdt node at " child " on simple-bus"))))))
