(in-package :mezzano.driver.usb.ohci)

(defun print-interrupts (stream title base offset)
  (let ((interrupts (pci:pci-io-region/32 base offset)))
    (format stream "~A: " title)
    (format stream " ~[               ~; Master Enable ~] |"
            (ldb +interrupt-master-enable+ interrupts))
    (format stream " ~[              ~; Owner Change ~] |"
            (ldb +interrupt-ownership-change+ interrupts))
    (format stream " ~[          ~; Root Hub ~] |"
            (ldb +interrupt-root-hub-change+ interrupts))
    (format stream " ~[                ~; Frame Overflow ~] |"
            (ldb +interrupt-frame-number-overflow+ interrupts))
    (format stream " ~[       ~; Error ~] |"
            (ldb +interrupt-unrecoverable-error+ interrupts))
    (format stream " ~[        ~; Resume ~] |"
            (ldb +interrupt-resume-detect+ interrupts))
    (format stream " ~[             ~; Frame Start ~] |"
            (ldb +interrupt-start-of-frame+ interrupts))
    (format stream " ~[           ~; Done Head ~] |"
            (ldb +interrupt-done-head+ interrupts))
    (format stream " ~[         ~; Overrun ~] |~%"
            (ldb +interrupt-scheduling-overrun+ interrupts))))

(defun print-ohci (stream ohci)
  (let ((pci-device (pci-device ohci))
        (bar (bar ohci)))
    (pci-print/2 stream "Vendor ID" pci-device pci:+pci-config-vendorid+)
    (pci-print/2 stream "Device ID" pci-device pci:+pci-config-deviceid+)
    ;; 2 bytes: command
    ;; 2 bytes: status
    (pci-print/1 stream "Rev ID" pci-device pci:+pci-config-revid+)

    ;; CLASSC
    (pci-print/1 stream "Base Class" pci-device (+ pci:+pci-config-classcode+ 2))
    (pci-print/1 stream "Sub Class" pci-device (+ pci:+pci-config-classcode+ 1))
    (pci-print/1 stream "Prog IF" pci-device pci:+pci-config-classcode+)

    ;; 1 byte: +pci-config-cachelinesz+   #x0C
    ;; 1 byte: +pci-config-latency-timer+ #x0D
    ;; 1 byte: +pci-config-hdr-type+      #x0E
    ;; 1 byte: +pci-config-bist+          #x0F

    ;; USBBASE
    (pci-print/4 stream "BAR" pci-device pci:+pci-config-bar-start+)

    ;; Controller registers
    (global-print/4  stream "   Revision" bar +ohci-revision+)

    (let ((control (pci:pci-io-region/32 bar +ohci-control+)))
      (format stream "    Control: Remote Wakeup ~[disabled~;enabled~],"
              (ldb +control-remote-wakeup-enable+ control))
      (format stream " Remote Wakeup ~[not ~;~]suppported,"
              (ldb +control-remote-wakeup-connected+ control))
      (format stream " Interrupts ~[normal~;SMM~],"
              (ldb +control-interrupt-routing+ control))
      (format stream " USB ~[Reset~;Resume~;Operational~;Suspended~],"
              (ldb +control-functional-state+ control))
      (format stream " Bulk ~[Disabled~;Enabled~],"
              (ldb +control-bulk-list-enable+ control))
      (format stream " Control ~[Disabled~;Enabled~],"
              (ldb +control-control-list-enable+ control))
      (format stream " Isochronous ~[Disabled~;Enabled~],"
              (ldb +control-isochronous-enable+ control))
      (format stream " Periodic ~[Disabled~;Enabled~],"
              (ldb +control-periodic-list-enable+ control))
      (format stream " Service Ratio ~D:1~%"
              (1+ (ldb +control-bulk-service-ratio+ control))))

    (let ((command (pci:pci-io-region/32 bar +ohci-command-status+)))
      (format stream " Cmd Status: Overrun Count: ~D,"
              (ldb +command-schedule-overrun+ command))
      (format stream " ~[No~;~] Ownership Change Requested,"
              (ldb +command-ownership-change+ command))
      (format stream " Bulk ~[Not ~;~]Filled,"
              (ldb +command-bulk-list-filled+ command))
      (format stream " Control  ~[Not ~;~]Filled,"
              (ldb +command-control-list-filled+ command))
      (format stream " HC ~[Not ~;~]Reset~%"
              (ldb +command-controller-reset+ command)))

    (let ((interrupts (pci:pci-io-region/32 bar +ohci-interrupt-status+)))
      (format stream " Int Status:                 |")
      (format stream " ~[              ~; Owner Change ~] |"
              (ldb +interrupt-ownership-change+ interrupts))
      (format stream " ~[          ~; Root Hub ~] |"
              (ldb +interrupt-root-hub-change+ interrupts))
      (format stream " ~[                ~; Frame Overflow ~] |"
              (ldb +interrupt-frame-number-overflow+ interrupts))
      (format stream " ~[       ~; Error ~] |"
              (ldb +interrupt-unrecoverable-error+ interrupts))
      (format stream " ~[        ~; Resume ~] |"
              (ldb +interrupt-resume-detect+ interrupts))
      (format stream " ~[             ~; Frame Start ~] |"
              (ldb +interrupt-start-of-frame+ interrupts))
      (format stream " ~[           ~; Done Head ~] |"
              (ldb +interrupt-done-head+ interrupts))
      (format stream " ~[         ~; Overrun ~] |~%"
              (ldb +interrupt-scheduling-overrun+ interrupts)))

    (let ((interrupts (pci:pci-io-region/32 bar +ohci-interrupt-enable+)))
      (format stream " Int Enable: ~[(Master Enable)~; Master Enable ~] |"
              (ldb +interrupt-master-enable+ interrupts))
      (format stream " ~[(Owner Change)~; Owner Change ~] |"
              (ldb +interrupt-ownership-change+ interrupts))
      (format stream " ~[(Root Hub)~; Root Hub ~] |"
              (ldb +interrupt-root-hub-change+ interrupts))
      (format stream " ~[(Frame Overflow)~; Frame Overflow ~] |"
              (ldb +interrupt-frame-number-overflow+ interrupts))
      (format stream " ~[(Error)~; Error ~] |"
              (ldb +interrupt-unrecoverable-error+ interrupts))
      (format stream " ~[(Resume)~; Resume ~] |"
              (ldb +interrupt-resume-detect+ interrupts))
      (format stream " ~[(Frame Start)~; Frame Start ~] |"
              (ldb +interrupt-start-of-frame+ interrupts))
      (format stream " ~[(Done Head)~; Done Head ~] |"
              (ldb +interrupt-done-head+ interrupts))
      (format stream " ~[(Overrun)~; Overrun ~] |~%"
              (ldb +interrupt-scheduling-overrun+ interrupts)))

    (global-print/32 stream " Int Status" bar +ohci-interrupt-status+)
    (global-print/32 stream " Int Enable" bar +ohci-interrupt-enable+)
    (global-print/32 stream "Int Disable" bar +ohci-interrupt-disable+)
    (global-print/4  stream "       HCCA" bar +ohci-hcca+)
    (when (/= (hcca-phys-addr ohci) 0)
      (format stream "       HCCA Frame Num: ~D~%" (hcca-frame-number ohci))
      (format stream "       HCCA Done Head: ~8,'0X~%" (hcca-done-head ohci)))
    (global-print/4  stream " Current Period" bar +ohci-current-period-endpt+)
    (global-print/4  stream "   Control Head" bar +ohci-control-head-pointer+)
    (global-print/4  stream "Current Control" bar +ohci-current-control-pointer+)
    (global-print/4  stream "      Bulk Head" bar +ohci-bulk-head-pointer+)
    (global-print/4  stream "   Current Bulk" bar +ohci-current-bulk-pointer+)
    (global-print/4  stream "      Done Head" bar +ohci-done-head-pointer+)
    (global-print/4  stream " Frame Interval" bar +ohci-frame-interval+)
    (global-print/4  stream "Frame Remaining" bar +ohci-frame-remaining+)
    (global-print/4  stream "   Frame Number" bar +ohci-frame-number+)
    (global-print/4  stream " Periodic Start" bar +ohci-periodic-start+)
    (global-print/4  stream "   LS Threshold" bar +ohci-low-speed-threshold+)
    (global-print/4  stream "Root Hub Desc A" bar +ohci-root-hub-descriptor-a+)
    (global-print/4  stream "Root Hub Desc B" bar +ohci-root-hub-descriptor-b+)
    (global-print/32 stream "Root Hub Status" bar +ohci-root-hub-status+)
    (global-print/32 stream "    Port Status" bar +ohci-root-hub-port-status+)
    (loop for i from 1 to (1- (num-ports ohci))
       do (global-print/32 stream
                           (format nil "           ~4D" i)
                           bar
                           (+ +ohci-root-hub-port-status+ (* 4 i))))))

(defun debug-find-32ms-interrupt (32ms-interrupts ed-phys-addr)
  (loop for int-node across 32ms-interrupts
     when (= (array->phys-addr (int-node-ed int-node)) ed-phys-addr) do
       (return-from debug-find-32ms-interrupt int-node))
  (error "Unable to find ed corresponding to ~8,'0X" ed-phys-addr))

(defun print-interrupt-table (stream ohci)
  (with-slots (interrupt-table 32ms-interrupts 16ms-interrupts 8ms-interrupts
                               4ms-interrupts 2ms-interrupts 1ms-interrupts) ohci
    (dotimes (i 32)
      (let ((int-node (debug-find-32ms-interrupt
                       32ms-interrupts (aref interrupt-table i))))
        (format stream "~2D  "
                (round (log (1+ (int-node-bandwidth int-node)) 10))))
      (format stream "~A" (aref 32ms-interrupts i))
      (when (< i 16)
        (format stream "  ~A" (aref 16ms-interrupts i)))
      (when (< i 8)
        (format stream "  ~A" (aref 8ms-interrupts i)))
      (when (< i 4)
        (format stream "  ~A" (aref 4ms-interrupts i)))
      (when (= i 18)
        (format stream "                                               ")
        (format stream "  ~A  ~A"
                (aref 2ms-interrupts 0)
                (aref 1ms-interrupts 0)))
      (when (= i 19)
        (format stream "                                               ")
        (format stream "  ~A" (aref 2ms-interrupts 1)))
      (format stream "~%"))))

(defun print-td (stream td &key (indent "") (buffer nil))
  (let ((phys-addr (if (integerp td) td (array->phys-addr td)))
        (td (if (arrayp td) td (phys-addr->array td))))
    (format stream "~A~16,'0X transfer descriptor~%" indent phys-addr)
    (let ((header (td-header td)))
      (format stream "~A    Cond. Code: ~A (~D), Err Cnt: ~D,"
              indent
              (aref +condition-codes+ (ldb +td-condition-code+ header))
              (ldb +td-condition-code+ header)
              (ldb +td-error-count+ header))
      (format stream " Toggle: ~[ED~;ED~;0~;1~],"
              (ldb +td-data-toggle+ header))
      (format stream " interrupt delay: ~D, ~[setup~;out~;in~;reserved~],"
              (ldb +td-delay-interrupt+ header)
              (ldb +td-direction-pid+ header))
      (format stream " ~[full~;partial~] buffer, ~8,'0X~%"
              (ldb +td-buffer-rounding+ header) header))
    (format stream "~A    current buffer pointer: ~16,'0X~%"
            indent (td-buffer-pointer td))
    (format stream "~A           next TD pointer: ~16,'0X~%"
            indent (td-next-td td))
    (format stream "~A                buffer end: ~16,'0X~%"
            indent (td-buffer-end td))
    (when (and buffer (/= (td-buffer-pointer td) 0))
      (format stream "~A    ~16,'0X~%" indent (td-buffer-pointer td))
      (print-buffer stream (td-buffer-pointer td)
                    :indent (concatenate 'string indent "        ")))))

(defun print-ed (stream ed &key (indent "") (buffers nil))
  (let* ((phys-addr (if (integerp ed) ed (array->phys-addr ed)))
         (ed (if (arrayp ed) ed (phys-addr->array ed)))
         (td-head-phys (ed-tdq-head ed)))
    (format stream "~%~A~16,'0X endpt descriptor~%" indent phys-addr)
    (let ((ed-header (ed-header ed)))
      (format stream "~A    max packet: ~D, ~[General~;Isochronous~]"
              indent
              (ldb +endpt-max-packet-size+ ed-header)
              (ldb +endpt-format+ ed-header))
      (format stream ", ~[valid~;skip~]"
              (ldb +endpt-skip+ ed-header))
      (format stream ", ~[full~;low~] speed"
              (ldb +endpt-speed+ ed-header))
      (format stream ", ~[TD dir~;out~;in~;TD dir~], addr: ~D, end pt: ~D"
              (ldb +endpt-direction+ ed-header)
              (ldb +endpt-function-addr-field+ ed-header)
              (ldb +endpt-number+ ed-header))
      (format stream ", Toggle: ~D~%" (ldb +endpt-toggle-carry+ td-head-phys)))
    (when (ldb-test +endpt-halted+ td-head-phys)
      (format stream "~A    ED Halted~%" indent))
    (do ((td-phys (logandc2 td-head-phys #x0F)
                  (td-next-td (phys-addr->array td-phys))))
        ((or (= td-phys (ed-tdq-tail ed)) (= td-phys 0)))
      (format stream "~%")
      (print-td stream td-phys
                :indent (concatenate 'string indent "    ")
                :buffer buffers))
    (format stream "~%~A    next ed: ~16,'0X~%" indent (ed-next-ed ed))))

(defun print-done-queue (stream ohci)
  (do ((td (logandc2 (hcca-done-head ohci) #x0F)
           (td-next-td (phys-addr->array td))))
      ((= td 0))
    (format stream "~%")
    (print-td stream td)))
