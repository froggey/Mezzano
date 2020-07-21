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

(defun initialize-fdt-pl011 (fdt-node address-cells size-cells)
  (let* ((reg (fdt-get-property fdt-node "reg"))
         (base-address (fdt-read-integer reg address-cells 0)))
    (initialize-debug-uart base-address)))

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
          ((fdt-compatible-p stdout-node "arm,pl011")
           (initialize-fdt-pl011 stdout-node
                                 ;; FIXME!
                                 2 2))
          (t
           (debug-print-line "stdout node is an unsupported device")))))

(defun initialize-early-platform ()
  (when (not (fdt-present-p))
    (panic "No FDT provided"))
  (debug-print-line "Performing early FDT scan")
  (arm64-fdt-scan t))

(defun initialize-platform ()
  (debug-print-line "Performing FDT scan")
  (arm64-fdt-scan nil))

(defun arm64-fdt-scan (earlyp)
  ;; qemu puts all devices in the root node, instead of under a simple-bus.
  ;; Treat the root node as a simple-bus to deal with this.
  (register-fdt-simple-bus (fdt-root) earlyp t))

(defun register-fdt-simple-bus (node earlyp &optional ignore-ranges)
  (let ((address-cells (fdt-address-cells node))
        (size-cells (fdt-size-cells node))
        (ranges (fdt-get-property node "ranges")))
    (when (not ignore-ranges)
      (when (not ranges)
        (debug-print-line "invalid simple-bus. missing ranges")
        (return-from register-fdt-simple-bus))
      (when (not (eql (fdt-property-length ranges) 0))
        ;; TODO.
        (debug-print-line "simple-bus with non-simple parent-child mapping, ignoring.")
        (return-from register-fdt-simple-bus)))
    ;; Walk children, looking for thing.
    (do-fdt-child-nodes (child node)
      (cond ((fdt-compatible-p child "simple-bus")
             (debug-print-line "simple-bus at " child)
             (register-fdt-simple-bus child earlyp))
            ((fdt-compatible-p child "arm,armv8-timer")
             (when (not earlyp)
               (initialize-platform-time child)))
            ((or (fdt-compatible-p child "arm,gic-400")
                 (fdt-compatible-p child "arm,cortex-a15-gic"))
             (when earlyp
               (initialize-fdt-gic-400 child address-cells size-cells)))
            ((fdt-compatible-p child "virtio,mmio")
             (when (not earlyp)
               (virtio-mmio-fdt-register child address-cells size-cells)))
            #+(or) ; not implemented yet!
            ((fdt-compatible-p child "allwinner,sun4i-a10-timer")
             (when (not earlyp)
               (initialize-fdt-sun4i-a10-timer node address-cells size-cells)))
            (t
             (debug-print-line "unknown fdt node at " child " on simple-bus"))))))
