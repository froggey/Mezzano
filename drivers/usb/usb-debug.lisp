(in-package :mezzano.driver.usb)

(defun pci-print/4 (stream text device address)
  (format stream "~A: ~8,'0X~%" text (pci:pci-config/32 device address)))

(defun pci-print/2 (stream text device address)
  (format stream "~A: ~4,'0X~%" text (pci:pci-config/16 device address)))

(defun pci-print/1 (stream text device address)
  (format stream "~A: ~2,'0x~%" text (pci:pci-config/8 device address)))

(defun global-print/32 (stream text base offset)
  (let ((value (pci:pci-io-region/32 base offset)))
    (format stream "~A: ~4,'0B ~4,'0B ~4,'0B ~4,'0B" text
            (ldb (byte 4 28) value)
            (ldb (byte 4 24) value)
            (ldb (byte 4 20) value)
            (ldb (byte 4 16) value))
    (format stream " ~4,'0B ~4,'0B ~4,'0B ~4,'0B~%"
            (ldb (byte 4 12) value)
            (ldb (byte 4  8) value)
            (ldb (byte 4  4) value)
            (ldb (byte 4  0) value))))

(defun global-print/16 (stream text base offset)
  (let ((value (pci:pci-io-region/32 base offset)))
    (format stream "~A: ~4,'0B ~4,'0B ~4,'0B ~4,'0B~%" text
            (ldb (byte 4 12) value)
            (ldb (byte 4  8) value)
            (ldb (byte 4  4) value)
            (ldb (byte 4  0) value))))

(defun global-print/8 (stream text base offset)
  (let ((value (pci:pci-io-region/32 base offset)))
    (format stream "~A: ~4,'0B ~4,'0B~%" text
            (ldb (byte 4  4) value)
            (ldb (byte 4  0) value))))

(defun global-print/4 (stream text base offset)
  (format stream "~A: ~8,'0X~%" text (pci:pci-io-region/32 base offset)))

(defun global-print/2 (stream text base offset)
  (format stream "~A: ~4,'0X~%" text (pci:pci-io-region/16 base offset)))

(defun global-print/1 (stream text base offset)
  (format stream "~A: ~2,'0x~%" text (pci:pci-io-region/8 base offset)))

(defconstant +standard-requests+
  '("Get Status"
    "Clear Feature"
    "Reserved (2)"
    "Set Feature"
    "Reserved (4)"
    "Set Address"
    "Get Descriptor"
    "Set Descriptor"
    "Get Configuration"
    "Set Configuration"
    "Get Interface"
    "Set Interface"
    "Sync Frame"))

(defconstant +descriptor-types+
  '("Undefined"
    "Device"
    "Configuration"
    "String"
    "Interface"
    "Endpoint"))

;; (define-constant +dev-req-get-status+         0)
;; (define-constant +dev-req-clear-feature+      1)
;; (define-constant +dev-req-reserved-2+         2)
;; (define-constant +dev-req-set-feature+        3)
;; (define-constant +dev-req-reserved-4+         4)
;; (define-constant +dev-req-set-address+        5)
;; (define-constant +dev-req-get-descriptor+     6)
;; (define-constant +dev-req-set-descriptor+     7)
;; (define-constant +dev-req-get-configuration+  8)
;; (define-constant +dev-req-set-configuration+  9)
;; (define-constant +dev-req-get-interface+     10)
;; (define-constant +dev-req-set-interface+     12)

(defun print-request (stream buf &key (indent ""))
  (let ((request-type (aref buf 0))
        (request (aref buf 1))
        (value  (dpb (aref buf 3) (byte 8 8) (aref buf 2)))
        (index  (dpb (aref buf 5) (byte 8 8) (aref buf 4)))
        (length (dpb (aref buf 7) (byte 8 8) (aref buf 6))))
    (format stream "~A~A: " indent (nth request +standard-requests+))
    (format stream "~[host->device~;device->host~]"
            (ldb (byte 1 7) request-type))
    (format stream ", Type: ~[Standard~;Class~;Vendor~;Reserved~]"
            (ldb (byte 2 5) request-type))
    (format stream ", Dest: ~[Device~;Interface~;~Endpoint~;Other~]~%"
            (ldb (byte 5 0) request-type))
    (case request
      (#.+dev-req-get-status+
       (format stream "~A  IFace/Endpt Num: ~D" indent index)
       (format stream ", Length: ~D~%" length))

      (#.+dev-req-clear-feature+
       (format stream "~A  Feature: ~D" indent value)
       (format stream ", IFace/Endpt Num: ~D" index)
       (format stream ", Length: ~D~%" length))

      (#.+dev-req-set-feature+
       (format stream "~A  Feature: ~D" indent value)
       (format stream ", IFace/Endpt Num: ~D" index)
       (format stream ", Length: ~D~%" length))

      (#.+dev-req-set-address+
       (format stream "~A  Address: ~D" indent value)
       (format stream ", Length: ~D~%" length))

      (#.+dev-req-get-descriptor+
       (format stream "~A  Type: ~A"
               indent
               (let ((type (aref buf 3)))
                 (cond ((< type (length +descriptor-types+))
                        (nth type +descriptor-types+))
                       ((<= #x21 type #x23)
                        (nth (- type #x21) '("HID" "Report" "Physical"))))))
       (format stream ", Idx: ~D" (aref buf 2))
       (format stream ", Lang: #x~4,'0X" index)
       (format stream ", Length: ~D~%" length))

      (#.+dev-req-set-descriptor+
       (format stream "~A  Type: ~A"
               indent
               (let ((type (aref buf 3)))
                 (cond ((< type (length +descriptor-types+))
                        (nth type +descriptor-types+))
                       ((<= #x21 type #x23)
                        (nth (- type #x21) '("HID" "Report" "Physical"))))))
       (format stream ", Idx: ~D" (aref buf 2))
       (format stream ", Lang: #x~4,'0X" index)
       (format stream ", Length: ~D~%" length))

      (#.+dev-req-get-configuration+
       (format stream ", Length: ~D~%" length))

      (#.+dev-req-set-configuration+
       (format stream "~A  Config: ~D" indent value)
       (format stream ", Length: ~D~%" length))

      (#.+dev-req-get-interface+
       (format stream "~A  IFace: ~D" indent index)
       (format stream ", Length: ~D~%" length))

      (#.+dev-req-set-interface+
       (format stream "~A  Alt setting: ~D" value)
       (format stream ", IFace: ~D" index)
       (format stream ", Length: ~D~%" length))
      )))

(defun print-usbmon-request (stream buf &key (indent ""))
  (let ((byte-buf (make-array 8)))
    (setf
     (aref byte-buf 0) (aref buf 0)
     (aref byte-buf 1) (aref buf 1)
     (aref byte-buf 2) (ldb (byte 8 0) (aref buf 2))
     (aref byte-buf 3) (ldb (byte 8 8) (aref buf 2))
     (aref byte-buf 4) (ldb (byte 8 0) (aref buf 3))
     (aref byte-buf 5) (ldb (byte 8 8) (aref buf 3))
     (aref byte-buf 6) (ldb (byte 8 0) (aref buf 4))
     (aref byte-buf 7) (ldb (byte 8 8) (aref buf 4)))
    (print-request stream byte-buf :indent indent)))

(defun increase-indent (indent)
  (concatenate 'string indent "  "))

(defun print-buffer (stream buf &key (indent ""))
  (let* ((buf (if (arrayp buf) buf (phys-addr->array buf)))
         (size (cadr (array-element-type buf))))
    (ecase size
      (8
       (format stream "~A" indent)
       (dotimes (i (length buf))
         (format stream "~2,'0X " (aref buf i))
         (when (= (mod i 16) 15)
           (format stream "~%~A" indent)))
       (when (/= (mod (length buf) 16) 0)
         (format stream "~%")))
      (16
       (format stream "~A" indent)
       (dotimes (i (length buf))
         (format stream "~4,'0X " (aref buf i))
         (when (= (mod i 8) 7)
           (format stream "~%~A" indent)))
       (when (/= (mod (length buf) 8) 0)
         (format stream "~%")))
      (32
       (format stream "~A" indent)
       (dotimes (i (length buf))
         (format stream "~8,'0X " (aref buf i))
         (when (= (mod i 4) 3)
           (format stream "~%~A" indent)))
       (when (/= (mod (length buf) 4) 0)
         (format stream "~%"))))))

(defun print-device-descriptor (stream buf offset indent)
  (cond ((>= (length buf) (+ offset (aref buf offset)))
         ;; full descriptor is available
         (format stream "~AUSB Rel: ~X.~2,'0X, Class: ~D, Subclass: ~D"
                 indent
                 (aref buf (+ offset 3))
                 (aref buf (+ offset 2))
                 (aref buf (+ offset 4))
                 (aref buf (+ offset 5)))
         (format stream ", Protocol: ~D, Max Packet: ~D, Num Configs: ~D~%"
                 (aref buf (+ offset 6))
                 (aref buf (+ offset 7))
                 (aref buf (+ offset 17)))
         (format stream "~AVendor: #x~4,'0X, Product: #x~4,'0X"
                 indent
                 (get-unsigned-word/16 buf (+ offset 8))
                 (get-unsigned-word/16 buf (+ offset 10)))
         (format stream ", Device Rel: ~X.~2,'0X~%"
                 (aref buf (+ offset 13))
                 (aref buf (+ offset 12)))
         (format stream "~AManuf. Idx: ~D Prod. Idx: ~D Serial Num Idx: ~D~%"
                 indent
                 (aref buf (+ offset 14))
                 (aref buf (+ offset 15))
                 (aref buf (+ offset 16))))
        (T
         ;; only partial descriptor is available
         (format stream "~A" indent)
         (loop for idx from offset to (1- (length buf))
            do (format stream "~2,'0X " (aref buf idx)))))
  (aref buf offset))

(defun print-configuration-descriptor (stream buf offset indent)
  (cond ((>= (length buf) (+ offset (aref buf offset)))
         ;; full descriptor is available
         (format stream "~ALength: ~D"
                 indent (get-unsigned-word/16 buf (+ offset 2)))
         (format stream ", Interfaces: ~D" (aref buf (+ offset 4)))
         (format stream ", Config Number: ~D" (aref buf (+ offset 5)))
         (format stream ", String Index: ~D" (aref buf (+ offset 6)))
         (let ((attr (aref buf (+ offset 7))))
           (when (ldb-test (byte 1 6) attr)
             (format stream ", Self Powered"))
           (when (ldb-test (byte 1 5) attr)
             (format stream ", Remote Wakeup")))
         (format stream ", Power: ~D~%" (* 2 (aref buf (+ offset 8)))))
        (T
         ;; only partial descriptor is available
         (format stream "~A" indent)
         (loop for idx from offset to (1- (length buf))
            do (format stream "~2,'0X " (aref buf idx)))))
  (aref buf offset))

(defun print-interface-descriptor (stream buf offset indent)
  (cond ((>= (length buf) (+ offset (aref buf offset)))
         (format stream "~AIface Number: ~D" indent (aref buf (+ offset 2)))
         (format stream ", Alternate: ~D" (aref buf (+ offset 3)))
         (format stream ", Num Endpts: ~D" (aref buf (+ offset 4)))
         (format stream ", Class: ~D" (aref buf (+ offset 5)))
         (format stream ", SubClass: ~D" (aref buf (+ offset 6)))
         (format stream ". Protocol: ~D" (aref buf (+ offset 7)))
         (format stream ", String Index: ~D~%" (aref buf (+ offset 8))))
        (T
         ;; only partial descriptor is available
         (format stream "~A" indent)
         (loop for idx from offset to (1- (length buf))
            do (format stream "~2,'0X " (aref buf idx)))))
  (aref buf offset))

(defun print-endpoint-descriptor (stream buf offset indent)
  (cond ((>= (length buf) (+ offset (aref buf offset)))
         (let ((addr (aref buf (+ offset 2))))
           (format stream "~AAddress: ~D" indent (ldb (byte 4 0) addr))
           (format stream ", ~[Out~;In~]" (ldb (byte 1 7) addr)))
         (format stream ", ~[Control~;Isochronous~;Bulk~;Interrupt~]"
                 (aref buf (+ offset 3)))
         (format stream ", Max Packet: ~D"
                 (get-unsigned-word/16 buf (+ offset 4)))
         (format stream ", Interval: ~D~%" (aref buf (+ offset 6))))
        (T
         ;; only partial descriptor is available
         (format stream "~A" indent)
         (loop for idx from offset to (1- (length buf))
            do (format stream "~2,'0X " (aref buf idx)))))
  (aref buf offset))

(defun print-hid-descriptor (stream buf offset indent)
  (cond ((>= (length buf) (+ offset (aref buf offset)))
         (format stream "~AHID Rel: ~X.~2'0X, Country Code: ~D, "
                 indent
                 (aref buf (+ offset 3))
                 (aref buf (+ offset 2))
                 (aref buf (+ offset 4)))
         (format stream "Num Descriptors: ~D, Type: ~D, Report size: ~D~%"
                 (aref buf (+ offset 5))
                 (aref buf (+ offset 6))
                 (get-unsigned-word/16 buf (+ offset 7)))
         (when (> (aref buf offset) 9)
           (loop for idx from (+ offset 9) to (+ offset (aref buf offset)) by 3
              do (format stream "~AOptional Type: ~D, Length: ~D~%"
                         indent
                         (aref buf idx)
                         (get-unsigned-word/16 buf (+ idx 1))))))
        (T
         ;; only partial descriptor is available
         (format stream "~A" indent)
         (loop for idx from offset to (1- (length buf))
            do (format stream "~2,'0X " (aref buf idx)))))
  (aref buf offset))

(defun print-descriptor (stream buf &key (offset 0) (indent ""))
  (loop
     with indent2 = (increase-indent indent)
     when (>= offset (length buf)) do (return)
     do
       (format stream "~%")
       (incf
        offset
        (case (aref buf (+ offset 1))
          (#.+desc-type-device+
           (format stream "~ADevice Descriptor~%" indent)
           (print-device-descriptor stream buf offset indent2))

          (#.+desc-type-configuration+
           (format stream "~AConfiguration Descriptor~%" indent)
           (print-configuration-descriptor stream buf offset indent2))

          (#.+desc-type-string+
           (format stream "~AString Descriptor: \"" indent)
           (loop for idx from 2 upto (1- (aref buf 0)) by 2
              do (format stream "~C" (code-char (aref buf idx))))
           (format stream "\"~%")
           (aref buf 0))

          (#.+desc-type-interface+
           (format stream "~AInterface Descriptor~%" indent)
           (print-interface-descriptor stream buf offset indent2))

          (#.+desc-type-endpoint+
           (format stream "~AEnd Point Descriptor~%" indent)
           (print-endpoint-descriptor stream buf offset indent2))

          (#.+desc-type-hid+
           (format stream "~AHID Descriptor~%" indent)
           (print-hid-descriptor stream buf offset indent2))

          #+nil (#.+desc-type-report+
                 )

          #+nil (#.+desc-type-physical+
                 )
          (T
           (when (>= (length buf) (+ offset 2))
             (format stream "~AUnsupported descriptor #x~2,'0X:"
                     indent
                     (aref buf (+ offset 1)))
             (let ((end (min (length buf) (+ offset (aref buf offset) -1))))
               (loop for idx from offset to end
                  do (format stream " ~2,'0X" (aref buf idx)))))
           (format stream "~%")
           (aref buf offset))))))
