(defpackage #:peek
  (:use #:cl))

(in-package #:peek)

(defclass peek-window (sys.graphics::text-window-with-chrome)
  ((process :reader window-process)))

(defmethod initialize-instance :after ((instance peek-window))
  (let ((process (sys.int::make-process "Peek")))
    (setf (slot-value instance 'process) process)
    (sys.int::process-preset process 'peek-top-level instance)
    (sys.int::process-enable process)))

(defmethod sys.graphics::window-close-event :before ((window peek-window))
  (sys.int::process-arrest-reason (window-process window) :window-closed))

(defun clear-window (window &optional (colour (sys.graphics::window-background-colour window)))
  (multiple-value-bind (left right top bottom)
      (sys.graphics::compute-window-margins window)
    (let* ((fb (sys.graphics::window-frontbuffer window))
           (dims (array-dimensions fb)))
      (sys.graphics::bitset (- (first dims) top bottom) (- (second dims) left right)
                            (sys.graphics::make-colour colour) fb top left)
      (sys.int::stream-move-to window 0 0)
      (setf sys.graphics::*refresh-required* t))))

(defvar *peek-commands*
  '((#\? "Help" peek-help "Show a help page.")
    (#\P "Process" peek-process "Show currently active processes.")
    (#\M "Memory" peek-memory "Show memory information.")
    (#\N "Network" peek-network "Show network information.")
    (#\D "Devices" peek-devices "Show device information.")
    (#\C "CPU" peek-cpu "Show CPU information.")
    (#\Q "Quit" nil "Quit Peek")))

(defun print-header ()
  (dolist (cmd *peek-commands*)
    (write-string (second cmd))
    (unless (char-equal (char (second cmd) 0) (first cmd))
      (format t "(~S)" (first cmd)))
    (write-char #\Space)))

(defun peek-help ()
  (format t "      Peek help~%")
  (format t "Char~6TCommand~20TInfo~%")
  (dolist (cmd *peek-commands*)
    (format t "~S~6T~A~20T~A~%" (first cmd) (second cmd) (fourth cmd))))

(defun peek-process ()
  (format t "Process Name~24TState~%")
  (dolist (process sys.int::*active-processes*)
    (format t " ~A~24T~A~%" (sys.int::process-name process) (sys.int::process-whostate process))))

(defun peek-memory ()
  (room))

(defun peek-network ()
  (format t "Network cards:~%")
  (dolist (card sys.net::*cards*)
    (let ((address (sys.net::ipv4-interface-address card nil)))
      (format t " ~S~%" card)
      (when address
        (format t "   IPv4 address: ~/sys.net::format-tcp4-address/~%" address))))
  (format t "Routing table:~%")
  (format t " Network~17TGateway~33TNetmask~49TInterface~%")
  (dolist (route sys.net::*routing-table*)
    (write-char #\Space)
    (if (first route)
        (sys.net::format-tcp4-address *standard-output* (first route))
        (write-string ":DEFAULT"))
    (format t "~17T")
    (if (second route)
        (sys.net::format-tcp4-address *standard-output* (second route))
        (write-string "N/A"))
    (format t "~33T~/sys.net::format-tcp4-address/~49T~S~%" (third route) (fourth route)))
  (format t "Servers:~%")
  (dolist (server sys.net::*server-alist*)
    (format t "~S  TCPv4 ~D~%" (second server) (first server)))
  (format t "TCPv4 connections:~%")
  (format t " Local~8TRemote~40TState~%")
  (dolist (conn sys.net::*tcp-connections*)
    (format t " ~D~8T~/sys.net::format-tcp4-address/:~D~40T~S~%"
            (sys.net::tcp-connection-local-port conn)
            (sys.net::tcp-connection-remote-ip conn) (sys.net::tcp-connection-remote-port conn)
            (sys.net::tcp-connection-state conn))))

(defun peek-devices ()
  (format t "PCI devices:~%")
  (dolist (dev sys.int::*pci-devices*)
    (multiple-value-bind (vname dname)
        (sys.int::pci-find-device (sys.int::pci-device-vendor-id dev)
                                  (sys.int::pci-device-device-id dev))
      (format t " ~2,'0X:~X:~X: ~4,'0X ~4,'0X ~S ~S~%"
              (sys.int::pci-device-bus dev) (sys.int::pci-device-device dev) (sys.int::pci-device-function dev)
              (sys.int::pci-device-vendor-id dev)
              (sys.int::pci-device-device-id dev)
              vname dname)))
  (format t "PCI drivers:~%")
  (dolist (drv sys.int::*pci-drivers*)
    (format t " ~S~%" (first drv))
    #+nil(dolist (id (second drv))
      (multiple-value-bind (vname dname)
          (sys.int::pci-find-device (first id) (second id))
        (format t "   ~4,'0X ~4,'0X ~S ~S~%" (first id) (second id) vname dname)))))

(defvar *cpuid-1-ecx-features*
  #("SSE3"
    nil
    nil
    "MONITOR"
    "DS-CPL"
    "VMX"
    "SMX"
    "EST"
    "TM2"
    "SSSE3"
    "CNXT-ID"
    nil
    nil
    "CMPXCHG16B"
    "xTPR"
    "PDCM"
    nil
    nil
    "DCA"
    "SSE4.1"
    "SSE4.2"
    nil
    nil
    "POPCNT"))

(defvar *cpuid-1-edx-features*
  #("FPU"
    "VME"
    "DE"
    "PSE"
    "TSC"
    "MSR"
    "PAE"
    "MCE"
    "CX8"
    "APIC"
    nil
    "SEP"
    "MTRR"
    "PGE"
    "MCA"
    "CMOV"
    "PAT"
    "PSE-36"
    "PSN"
    "CLFSH"
    nil
    "DS"
    "ACPI"
    "MMX"
    "FXSR"
    "SSE"
    "SSE2"
    "SS"
    "HTT"
    "TM"
    nil
    "PBE"))

(defvar *cpuid-ext-1-ecx-features*
  #("LAHF/SAHF"))

(defvar *cpuid-ext-1-edx-features*
  #(nil ; FPU
    nil ; VME
    nil ; DE
    nil ; PSE
    nil ; TSC
    nil ; MSR
    nil ; PAE
    nil ; MCE
    nil ; CMPXCHG8B
    nil ; APIC
    nil
    "SYSCALL"
    nil ; MTRR
    nil ; PGE
    nil ; MCA
    nil ; CMOV
    nil ; PAT
    nil ; PSE36
    nil
    nil
    "NX"
    nil
    "MmxExt"
    nil ; MMX
    nil ; FXSR
    "FFXSR"
    "Page1GB"
    "RDTSCP"
    nil
    "LM"
    "3DNowExt"
    "3DNow"))

(defun scan-feature-bits (feature-seq bits)
  (let ((features '()))
    (dotimes (i (length feature-seq) features)
      (when (and (elt feature-seq i)
                 (logbitp i bits))
        (push (elt feature-seq i) features)))))

(defun peek-cpu ()
  (let ((features '())
        (extended-cpuid-max nil))
    (multiple-value-bind (cpuid-max vendor-1 vendor-3 vendor-2)
        (sys.int::cpuid 0)
      (format t "Maximum CPUID level: ~X~%" cpuid-max)
      (format t "Vendor: ~A~%" (sys.int::decode-cpuid-vendor vendor-1 vendor-2 vendor-3))
      (setf extended-cpuid-max (sys.int::cpuid #x80000000))
      (if (logbitp 31 extended-cpuid-max)
          (format t "Maximum extended CPUID level: ~X~%" extended-cpuid-max)
          (format t "Extended CPUID not supported.~%"))
      (when (>= cpuid-max 1)
        (multiple-value-bind (a b c d)
            (sys.int::cpuid 1)
          (let* ((stepping-id (ldb (byte 4 0) a))
                 (model (ldb (byte 4 4) a))
                 (family-id (ldb (byte 4 8) a))
                 (processor-type (ldb (byte 2 12) a))
                 (extended-model-id (ldb (byte 4 16) a))
                 (extended-family-id (ldb (byte 8 20) a))
                 (display-family (if (= family-id #xF)
                                     (+ family-id extended-family-id)
                                     family-id))
                 (displayed-model (if (or (= family-id #x6) (= family-id #xF))
                                      (+ (ash extended-model-id 4) model)
                                      model))
                 (brand (ldb (byte 8 0) b)))
            (format t "Model: ~X  Family: ~X  Stepping: ~X  Processor type: ~X  Brand index: ~D~%"
                    displayed-model display-family stepping-id processor-type brand))
          (format t "CLFLUSH size: ~D bytes~%" (* (ldb (byte 8 8) b) 8))
          (format t "Local APIC ID: ~D~%" (ldb (byte 8 24) b))
          (setf features (nconc (scan-feature-bits *cpuid-1-ecx-features* c)
                                (scan-feature-bits *cpuid-1-edx-features* d)
                                features))))
      (when (>= extended-cpuid-max #x80000001)
        (multiple-value-bind (a b c d)
            (sys.int::cpuid #x80000001)
          (setf features (nconc (scan-feature-bits *cpuid-ext-1-ecx-features* c)
                                (scan-feature-bits *cpuid-ext-1-edx-features* d)
                                features)))))
    (format t "Features: ~A~%" features)))

(defun peek-top-level (window)
  (unwind-protect
       (let ((*standard-output* window)
             (*standard-input* window)
             (mode 'peek-help))
         (loop
            (restart-case
                (progn (clear-window window)
                       (print-header)
                       (fresh-line)
                       (funcall mode)
                       (setf sys.graphics::*refresh-required* t)
                       (let* ((ch (read-char window))
                              (cmd (assoc ch *peek-commands* :test 'char-equal)))
                         (cond ((char= ch #\Space)) ; refresh current window
                               ((char-equal ch #\Q)
                                (return))
                               (cmd (setf mode (third cmd))))))
              (abort () :report "Reset Peek."
                (setf mode 'peek-help)))))
    (sys.graphics::close-window window)))

(defun create-peek-window ()
  (sys.graphics::window-set-visibility (sys.graphics::make-window "Peek" 640 640 'peek-window) t))

(setf (gethash (name-char "F4") sys.graphics::*global-keybindings*) 'create-peek-window)
