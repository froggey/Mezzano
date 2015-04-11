;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; FIXME: Should not be here.
;;; >>>>>>

(defun stack-base (stack)
  (car stack))

(defun stack-size (stack)
  (cdr stack))

;; TODO: Actually allocate virtual memory.
(defun %allocate-stack (size &optional wired)
  ;; 4k align the size.
  (setf size (logand (+ size #xFFF) (lognot #xFFF)))
  (let* ((addr (safe-without-interrupts (size wired)
                 (with-symbol-spinlock (mezzano.runtime::*wired-allocator-lock*)
                   (prog1 (logior (+ (if wired sys.int::*wired-stack-area-bump* sys.int::*stack-area-bump*) #x200000)
                                  (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+))
                     ;; 2m align the memory region.
                     (if wired
                         (incf sys.int::*wired-stack-area-bump* (align-up size #x200000))
                         (incf sys.int::*stack-area-bump* (align-up size #x200000)))))))
         (stack (sys.int::cons-in-area addr size :wired)))
    ;; Allocate blocks.
    (allocate-memory-range addr size
                           (logior sys.int::+block-map-present+
                                   sys.int::+block-map-writable+
                                   sys.int::+block-map-zero-fill+))
    stack))

;; TODO.
(defun sleep (seconds)
  nil)

;;; <<<<<<

(defvar *boot-information-page*)

(defconstant +log2-4k-page+ 12)
(defconstant +n-32-bit-physical-buddy-bins+ (- 32 +log2-4k-page+)
  "Number of buddy bins for the below 4GB allocator.")
(defconstant +n-64-bit-physical-buddy-bins+ (- 36 +log2-4k-page+)
  "Number of buddy bins for the above 4GB allocator.")

(defconstant +buddy-bin-size+ 16
  "Size in bytes of one buddy bin.")

(defconstant +boot-information-boot-uuid-offset+                  0)
(defconstant +boot-information-32-bit-physical-buddy-bins-offset+ 16)
(defconstant +boot-information-64-bit-physical-buddy-bins-offset+ 336)
(defconstant +boot-information-video+                             768)
(defconstant +boot-information-framebuffer-physical-address+      (+ +boot-information-video+ 0))
(defconstant +boot-information-framebuffer-width+                 (+ +boot-information-video+ 8))
(defconstant +boot-information-framebuffer-pitch+                 (+ +boot-information-video+ 16))
(defconstant +boot-information-framebuffer-height+                (+ +boot-information-video+ 24))
(defconstant +boot-information-framebuffer-layout+                (+ +boot-information-video+ 32))
(defconstant +boot-information-n-memory-map-entries+              824)
(defconstant +boot-information-memory-map+                        832)

(defun boot-uuid (offset)
  (check-type offset (integer 0 15))
  (sys.int::memref-unsigned-byte-8 (+ +boot-information-boot-uuid-offset+ *boot-information-page*)
                                   offset))

;; This thunk exists purely so that the GC knows when to stop unwinding the initial process' stack.
;; I'd like to get rid of it somehow...
(sys.int::define-lap-function sys.int::%%bootloader-entry-point ()
  (:gc :no-frame)
  ;; Drop the bootloader's return address.
  (sys.lap-x86::add64 :rsp 8)
  ;; Call the real entry point.
  (sys.lap-x86:mov64 :r13 (:function sys.int::bootloader-entry-point))
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))

(defvar *boot-hook-lock* (make-mutex "Boot Hook Lock"))
(defvar *boot-hooks* '())

(defun add-boot-hook (fn)
  (with-mutex (*boot-hook-lock*)
    (push fn *boot-hooks*)))

(defun remove-boot-hook (fn)
  (with-mutex (*boot-hook-lock*)
    (setf *boot-hooks* (remove fn *boot-hooks*))))

(defun run-boot-hooks ()
  (dolist (hook *boot-hooks*)
    (sys.int::log-and-ignore-errors
      (format t "Run boot hook ~A~%" hook)
      (funcall hook))))

(defvar *boot-id*)

(defstruct (nic
             (:area :wired))
  device
  mac
  transmit-packet
  stats
  mtu)

(defvar *nics*)
(defvar *received-packets*)

(defun register-nic (device mac transmit-fn stats-fn mtu)
  (debug-print-line "Registered NIC " device " with MAC " mac)
  (push-wired (make-nic :device device
                        :mac mac
                        :transmit-packet transmit-fn
                        :stats stats-fn
                        :mtu mtu)
              *nics*))

(defun net-statistics (nic)
  "Get NIC statistics. Returns 7 values:
Bytes received.
Packets received.
Receive errors.
Bytes transmitted.
Packets transmitted.
Transmit errors.
Collisions."
  (funcall (nic-stats nic) (mezzano.supervisor::nic-device nic)))

(defun net-transmit-packet (nic pkt)
  (funcall (mezzano.supervisor::nic-transmit-packet nic)
           (mezzano.supervisor::nic-device nic)
           pkt))

(defun net-receive-packet ()
  "Wait for a packet to arrive.
Returns two values, the packet data and the receiving NIC."
  (let ((info (irq-fifo-pop *received-packets*)))
    (values (cdr info) (car info))))

(defun nic-received-packet (device pkt)
  (let ((nic (find device *nics* :key #'nic-device)))
    (when nic
      (irq-fifo-push (cons nic pkt) *received-packets*))))

(defvar *deferred-boot-actions*)

(defun add-deferred-boot-action (action)
  (push-wired action *deferred-boot-actions*))

(defun sys.int::bootloader-entry-point (boot-information-page)
  (let ((first-run-p nil)
        ;; TODO: This (along with the other serial settings) should be provided by the bootloader.
        (serial-port-io-base #x3F8))
    (initialize-early-debug-serial serial-port-io-base)
    (initialize-initial-thread)
    (setf *boot-information-page* boot-information-page
          *cold-unread-char* nil
          mezzano.runtime::*paranoid-allocation* nil
          *nics* '()
          *deferred-boot-actions* '()
          *paging-disk* nil)
    (initialize-physical-allocator)
    (initialize-early-video)
    (initialize-boot-cpu)
    (when (not (boundp 'mezzano.runtime::*tls-lock*))
      (setf first-run-p t)
      (mezzano.runtime::first-run-initialize-allocator)
      ;; FIXME: Should be done by cold generator
      (setf mezzano.runtime::*tls-lock* :unlocked
            mezzano.runtime::*active-catch-handlers* 'nil
            *pseudo-atomic* nil
            ;; FIXME: This should be a normal non-IRQ FIFO, but
            ;; creating a FIFO won't work until the cold load finishes.
            *received-packets* (make-irq-fifo 50)))
    (setf *boot-id* (sys.int::cons-in-area nil nil :wired))
    (initialize-interrupts)
    (initialize-i8259)
    (initialize-threads)
    (initialize-disk)
    (initialize-pager)
    (initialize-snapshot)
    (sys.int::%sti)
    (initialize-debug-serial serial-port-io-base 4 38400)
    ;;(debug-set-output-pseudostream (lambda (op &optional arg) (declare (ignore op arg))))
    (debug-write-line "Hello, Debug World!")
    (irq-fifo-reset *received-packets*)
    (initialize-time)
    (initialize-ata)
    (initialize-ahci)
    (initialize-virtio)
    (initialize-virtio-net)
    (initialize-ps/2)
    (initialize-video)
    (initialize-pci)
    (detect-disk-partitions)
    (detect-paging-disk)
    (when (not *paging-disk*)
      (panic "Could not find boot device. Sorry."))
    (dolist (action *deferred-boot-actions*)
      (funcall action))
    (makunbound '*deferred-boot-actions*)
    (cond (first-run-p
           (make-thread #'sys.int::initialize-lisp :name "Main thread"))
          (t (make-thread #'run-boot-hooks :name "Boot hook thread")))
    (finish-initial-thread)))
