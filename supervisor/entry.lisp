;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; FIXME: Should not be here.
;;; >>>>>>

;; fixme: multiple-evaluation of PLACE.
(defmacro push-wired-locked (item place mutex)
  (let ((new-cons (gensym)))
    `(let ((,new-cons (sys.int::cons-in-area ,item nil :wired)))
       (with-mutex (,mutex)
         (setf (cdr ,new-cons) ,place
               place ,new-cons)))))

;; fixme: multiple-evaluation of PLACE.
(defmacro push-wired (item place)
  `(setf ,place (sys.int::cons-in-area ,item ,place :wired)))

(defun string-length (string)
  (assert (sys.int::character-array-p string))
  (sys.int::%array-like-ref-t string 3))

(defun sys.int::assert-error (test-form datum &rest arguments)
  (declare (dynamic-extent arguments))
  (panic "Assert error " datum " " arguments))

;; Back-compat.
(defmacro with-gc-deferred (&body body)
  `(with-pseudo-atomic ,@body))

(defun call-with-gc-deferred (thunk)
  (call-with-pseudo-atomic thunk))

(defun find-extent-named (name largep)
  (cond ((store-extent-p name) name)
        (t (dolist (extent *extent-table*
                    (error "can't find extent..."))
             (when (and (or (eql (store-extent-type extent) name)
                            (and (eql name :wired)
                                 (eql (store-extent-type extent) :pinned)
                                 (store-extent-wired-p extent)))
                        (not (store-extent-finished-p extent))
                        (eql (store-extent-large-p extent) largep))
               (return extent))))))

(defun stack-base (stack)
  (car stack))

(defun stack-size (stack)
  (cdr stack))

;; TODO: Actually allocate virtual memory.
(defun %allocate-stack (size &optional wired)
  ;; 4k align the size.
  (setf size (logand (+ size #xFFF) (lognot #xFFF)))
  (let* ((addr (with-symbol-spinlock (mezzano.runtime::*wired-allocator-lock*)
                 (prog1 (logior (+ (if wired sys.int::*wired-stack-area-bump* sys.int::*stack-area-bump*) #x200000)
                                (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+))
                   ;; 2m align the memory region.
                   (if wired
                       (incf sys.int::*wired-stack-area-bump* (align-up size #x200000))
                       (incf sys.int::*stack-area-bump* (align-up size #x200000))))))
         (stack (sys.int::cons-in-area addr size :wired)))
    ;; Allocate blocks.
    (with-mutex (*vm-lock*)
      (dotimes (i (ceiling size #x1000))
        (allocate-new-block-for-virtual-address (+ addr (* i #x1000))
                                                (logior sys.int::+block-map-present+
                                                        sys.int::+block-map-writable+
                                                        sys.int::+block-map-zero-fill+))))
    stack))

;; TODO.
(defun sleep (seconds)
  nil)

(defun sys.int::raise-undefined-function (fref)
  (let ((name (sys.int::%array-like-ref-t fref sys.int::+fref-name+)))
    (cond ((consp name)
           (panic "Undefined function (" (symbol-name (car name)) " " (symbol-name (car (cdr name))) ")"))
          (t (panic "Undefined function " (symbol-name name))))))

(defun sys.int::raise-unbound-error (symbol)
  (panic "Unbound symbol " (symbol-name symbol)))

(in-package :sys.int)

(defstruct (cold-stream (:area :wired)))

(in-package :mezzano.supervisor)

(defvar *cold-unread-char*)

(defun sys.int::cold-write-char (c stream)
  (declare (ignore stream))
  (debug-write-char c))

(defun sys.int::cold-start-line-p (stream)
  (declare (ignore stream))
  (debug-start-line-p))

(defun sys.int::cold-read-char (stream)
  (declare (ignore stream))
  (cond (*cold-unread-char*
         (prog1 *cold-unread-char*
           (setf *cold-unread-char* nil)))
        (t (debug-read-char))))

(defun sys.int::cold-unread-char (character stream)
  (declare (ignore stream))
  (when *cold-unread-char*
    (error "Multiple unread-char!"))
  (setf *cold-unread-char* character))

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
(defconstant +boot-information-module-base+                       808)
(defconstant +boot-information-module-limit+                      816)
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
    (handler-case (funcall hook)
      (error (c)
        (format t "~&Error ~A while running boot hook ~S.~%" c hook)))))

(defun align-up (value power-of-two)
  "Align VALUE up to the nearest multiple of POWER-OF-TWO."
  (logand (+ value (1- power-of-two)) (lognot (1- power-of-two))))

(defvar *boot-id*)

(defmacro do-boot-modules ((data-address data-size name-address name-size &optional result) &body body)
  (let ((base-sym (gensym "base"))
        (limit-sym (gensym "limit"))
        (offset-sym (gensym "offset")))
    `(do ((,base-sym (+ +physical-map-base+ (sys.int::memref-t (+ *boot-information-page* +boot-information-module-base+) 0)))
          (,limit-sym (sys.int::memref-t (+ *boot-information-page* +boot-information-module-limit+) 0))
          (,offset-sym 0))
         ((>= ,offset-sym ,limit-sym)
          ,result)
       (let* ((,data-address (+ +physical-map-base+ (sys.int::memref-t (+ ,base-sym ,offset-sym) 0)))
              (,data-size (sys.int::memref-t (+ ,base-sym ,offset-sym) 1))
              (,name-address (+ ,base-sym ,offset-sym 24))
              (,name-size (sys.int::memref-t (+ ,base-sym ,offset-sym) 2)))
         (incf ,offset-sym (align-up (+ 24 ,name-size) 16))
         (tagbody ,@body)))))

;; This is pretty terrible, it's needed because the boot modules are transient
;; and it's not safe to return references to transient memory.
(defun fetch-boot-modules ()
  (let ((boot-id *boot-id*)
        (n-modules 0)
        module-size-vector
        name-size-vector
        module-vector
        name-vector)
    ;; Count modules.
    (with-pseudo-atomic
      (when (not (eql boot-id *boot-id*))
        (return-from fetch-boot-modules '()))
      (do-boot-modules (data-address data-size name-address name-size)
        (debug-print-line n-modules " " data-address " " data-size " " name-address " " name-size)
        (incf n-modules)))
    ;; Allocate vector to hold modules.
    (setf module-size-vector (make-array n-modules)
          module-vector (make-array n-modules)
          name-size-vector (make-array n-modules)
          name-vector (make-array n-modules))
    ;; Fetch sizes.
    (with-pseudo-atomic
      (let ((n 0))
        (when (not (eql boot-id *boot-id*))
          (return-from fetch-boot-modules '()))
        (do-boot-modules (data-address data-size name-address name-size)
          (setf (aref module-size-vector n) data-size
                (aref name-size-vector n) name-size)
          (incf n))))
    ;; Generate data and name vectors.
    (dotimes (i n-modules)
      (setf (aref module-vector i) (make-array (aref module-size-vector i) :element-type '(unsigned-byte 8))
            (aref name-vector i) (make-array (aref name-size-vector i) :element-type 'character)))
    ;; Copy modules & names out.
    (with-pseudo-atomic
      (let ((n 0))
        (when (not (eql boot-id *boot-id*))
          (return-from fetch-boot-modules '()))
        (do-boot-modules (data-address data-size name-address name-size)
          (let ((module (aref module-vector n))
                (name (aref name-vector n)))
            (dotimes (i data-size)
              (setf (aref module i) (sys.int::memref-unsigned-byte-8 data-address i)))
            (dotimes (i name-size)
              (setf (aref name i) (code-char (sys.int::memref-unsigned-byte-8 name-address i)))))
          (incf n))))
    ;; Build & return module list.
    (map 'list #'cons name-vector module-vector)))

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
  (let ((info (fifo-pop *received-packets*)))
    (values (cdr info) (car info))))

(defun nic-received-packet (device pkt)
  (let ((nic (find device *nics* :key #'nic-device)))
    (when nic
      (fifo-push (cons nic pkt) *received-packets* nil))))

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
          *block-cache* nil
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
            *received-packets* (make-fifo 50)))
    (fifo-reset *received-packets*)
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
    (initialize-time)
    (initialize-ata)
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
