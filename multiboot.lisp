(in-package :sys.int)

(defconstant +multiboot-flag-mem-info+        #b000000000001)
(defconstant +multiboot-flag-boot-device+     #b000000000010)
(defconstant +multiboot-flag-command-line+    #b000000000100)
(defconstant +multiboot-flag-modules+         #b000000001000)
(defconstant +multiboot-flag-aout-symbols+    #b000000010000)
(defconstant +multiboot-flag-elf-symbols+     #b000000100000)
(defconstant +multiboot-flag-memory-map+      #b000001000000)
(defconstant +multiboot-flag-drives+          #b000010000000)
(defconstant +multiboot-flag-config-table+    #b000100000000)
(defconstant +multiboot-flag-bootloader-name+ #b001000000000)
(defconstant +multiboot-flag-apm-table+       #b010000000000)
(defconstant +multiboot-flag-vbe+             #b100000000000)

(defconstant +multiboot-maximum-string-length+ 128)

;;; UB32 offsets into the multiboot info struct.
(defconstant +multiboot-info-flags+ 0)
(defconstant +multiboot-info-mem-lower+ 1)
(defconstant +multiboot-info-mem-upper+ 2)
(defconstant +multiboot-info-boot-device+ 3)
(defconstant +multiboot-info-command-line+ 4)
(defconstant +multiboot-info-module-count+ 5)
(defconstant +multiboot-info-module-address+ 6)
(defconstant +multiboot-info-mmap-length+ 11)
(defconstant +multiboot-info-mmap-address+ 12)
(defconstant +multiboot-info-drives-length+ 13)
(defconstant +multiboot-info-drives-address+ 14)
(defconstant +multiboot-info-config-table+ 15)
(defconstant +multiboot-info-boot-loader-name+ 16)
(defconstant +multiboot-info-apm-table+ 17)
(defconstant +multiboot-info-total-size+ 72
  "Total size of the multiboot info struct, in bytes.")

;;; UB32 offsets into the multiboot header.
(defconstant +multiboot-header-magic+ 0)
(defconstant +multiboot-header-flags+ 1)
(defconstant +multiboot-header-checksum+ 2)
(defconstant +multiboot-header-header-addr+ 3)
(defconstant +multiboot-header-load-addr+ 4)
(defconstant +multiboot-header-load-end-addr+ 5)
(defconstant +multiboot-header-bss-end-addr+ 6)
(defconstant +multiboot-header-entry-addr+ 7)
(defconstant +multiboot-header-mode-type+ 8)
(defconstant +multiboot-header-width+ 9)
(defconstant +multiboot-header-height+ 10)
(defconstant +multiboot-header-depth+ 11)

;;; UB32 offsets into the multiboot module structure.
(defconstant +multiboot-module-start+ 0)
(defconstant +multiboot-module-end+ 1)
(defconstant +multiboot-module-string+ 2)
(defconstant +multiboot-module-total-size+ 16
  "Total size in bytes of the multiboot module structure.")

;;; UB8 offsets into the multiboot-provided memory map.
(defconstant +multiboot-mmap-entry-size+ 0
  "A UB32/LE field. Contains the size of the current entry,
minus the size of the entry-size field.")
(defconstant +multiboot-mmap-base+ 4
  "A UB64/LE field. Contains the base address of the current entry.")
(defconstant +multiboot-mmap-length+ 12
  "A UB64/LE field. Contains the length of the current entry.")
(defconstant +multiboot-mmap-type+ 20
  "A UB32/LE field. Contains the type of the current entry.
Use MULTIBOOT-MMAP-MEMORY-TYPE to convert to the standard representation.")

(declaim (special *multiboot-info*
                  *multiboot-header*))

(defun multiboot-mmap-memory-type (type)
  "Convert a multiboot memory map type to a normal memory type."
  (case type
    (1 :free)
    (2 :reserved)
    (3 :acpi-reclaimable)
    (4 :acpi-nvs)
    (5 :bad)
    (t :unknown)))

(defun multiboot-memory-map (&optional suppress-memory-map suppress-memory-info)
  "Fetch the memory map from the multiboot header.
Returns NIL if the bootloader has not provided any memory information.
The returned memory map only describes physical memory and does not mark the kernel, modules
or other used regions as used."
  (let* ((memory-map (make-array 16 :fill-pointer 0 :adjustable t))
         (info (+ *multiboot-info* #x8000000000))
         (flags (memref-unsigned-byte-32 info +multiboot-info-flags+)))
    (cond ((and (logtest flags +multiboot-flag-memory-map+)
                (not suppress-memory-map))
           ;; Memory map provided.
           ;; Each entry looks like:
           ;;  +0  entry-size (ub32/le) (not including the entry-size field)
           ;;  +4  base (ub64/le)
           ;;  +12 length (ub64/le)
           ;;  +20 type (ub32/le)
           (do* ((length (memref-unsigned-byte-32 info +multiboot-info-mmap-length+))
                 (mmap-base (+ (memref-unsigned-byte-32 info +multiboot-info-mmap-address+) #x8000000000))
                 (mmap-end (+ mmap-base length))
                 (mmap mmap-base
                       (+ mmap (memref-unsigned-byte-32 (+ mmap +multiboot-mmap-entry-size+) 0) 4)))
                ((>= mmap (+ mmap-base length)))
             (let ((base (memref-unsigned-byte-64 (+ mmap +multiboot-mmap-base+) 0))
                   (length (memref-unsigned-byte-64 (+ mmap +multiboot-mmap-length+) 0))
                   (type (multiboot-mmap-memory-type (memref-unsigned-byte-32 (+ mmap +multiboot-mmap-type+) 0))))
               (vector-push-extend (make-memory-map-entry :base base
                                                          :length length
                                                          :type type)
                                   memory-map)))
           memory-map)
          ((and (logtest flags +multiboot-flag-mem-info+)
                (not suppress-memory-info))
           ;; No memory map, but the memory info fields are valid.
           ;; Create a single region using the memory above 1MB.
           ;; Ignore lowmem, too hairy.
           (vector-push-extend (make-memory-map-entry :base #x100000
                                                      :length (* (memref-unsigned-byte-32 info +multiboot-info-mem-upper+)
                                                                 1024)
                                                      :type :free)
                               memory-map)
           memory-map))))

(defun multiboot-cstring-length (address)
  "Return the length of the C string at ADDRESS, limited to +multiboot-maximum-string-length+."
  (do ((len 0 (1+ len)))
      ((or (zerop (memref-unsigned-byte-8 address len))
           (>= len +multiboot-maximum-string-length+))
       ;; Make sure the null byte is counted.
       (min (1+ len) +multiboot-maximum-string-length+))))

(defun multiboot-mmap-add-reserved-regions (mmap)
  "Add reserved regions to the multiboot memory map."
  (let* ((new-mmap (make-array (length mmap)
                               :initial-contents mmap
                               :fill-pointer t
                               :adjustable t))
         (info (+ *multiboot-info* #x8000000000))
         (flags (memref-unsigned-byte-32 info +multiboot-info-flags+)))
    (flet ((add (base length type)
             (vector-push-extend (make-memory-map-entry
                                  :base base
                                  :length length
                                  :type type)
                                 new-mmap)))
      ;; Figure out how large the kernel is by examining the multiboot header.
      (add (aref *multiboot-header* +multiboot-header-load-addr+)
           (- (cond ((not (zerop (aref *multiboot-header* +multiboot-header-bss-end-addr+)))
                     (aref *multiboot-header* +multiboot-header-bss-end-addr+))
                    (t (aref *multiboot-header* +multiboot-header-load-end-addr+)))
              (aref *multiboot-header* +multiboot-header-load-addr+))
           :kernel)
      ;; Add the multiboot info struct & associated memory regions.
      (add *multiboot-info*
           +multiboot-info-total-size+
           :multiboot-info)
      ;; Module stuff.
      (when (logtest flags +multiboot-flag-modules+)
        (let ((count (memref-unsigned-byte-32 info +multiboot-info-module-count+))
              (base (memref-unsigned-byte-32 info +multiboot-info-module-address+)))
          (add (memref-unsigned-byte-32 info +multiboot-info-module-address+)
               (* (memref-unsigned-byte-32 info +multiboot-info-module-count+)
                  +multiboot-module-total-size+)
               :multiboot-module-info)
          (dotimes (i count)
            (let* ((struct (+ #x8000000000 base (* i +multiboot-module-total-size+)))
                   (start (memref-unsigned-byte-32 struct +multiboot-module-start+))
                   (end (memref-unsigned-byte-32 struct +multiboot-module-end+))
                   (length (- end start))
                   (string (memref-unsigned-byte-32 struct +multiboot-module-string+))
                   (string-length (multiboot-cstring-length (+ #x8000000000 string))))
              ;; Add the module string.
              (add string
                   string-length
                   :multiboot-module-string)
              ;; And the module.
              (add start
                   length
                   :multiboot-module)))))
      ;; Memory map.
      (when (logtest flags +multiboot-flag-memory-map+)
        (add (memref-unsigned-byte-32 info +multiboot-info-mmap-address+)
             (memref-unsigned-byte-32 info +multiboot-info-mmap-length+)
             :multiboot-memory-map))
      ;; Command line.
      (when (logtest flags +multiboot-flag-command-line+)
        (let ((addr (memref-unsigned-byte-32 info +multiboot-info-command-line+)))
          (add addr
               (multiboot-cstring-length (+ #x8000000000 addr))
               :multiboot-command-line)))
      ;; Boot loader name.
      (when (logtest flags +multiboot-flag-bootloader-name+)
        (let ((addr (memref-unsigned-byte-32 info +multiboot-info-boot-loader-name+)))
          (add addr
               (multiboot-cstring-length (+ #x8000000000 addr))
               :multiboot-bootloader-name)))
      new-mmap)))
