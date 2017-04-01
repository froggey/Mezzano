;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; FIXME: Should not be here.
;;; >>>>>>

(defstruct (stack
             (:constructor %make-stack (base size))
             (:area :wired))
  base
  size)

(defun %allocate-stack-1 (aligned-size bump-sym)
  (mezzano.supervisor:without-footholds
    (mezzano.supervisor:with-mutex (mezzano.runtime::*allocator-lock*)
      (prog1 (logior (+ (symbol-value bump-sym) #x200000) ; + 2MB for guard page
                     (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+))
        (incf (symbol-value bump-sym) aligned-size)))))

;; TODO: Actually allocate virtual memory.
(defun %allocate-stack (size &optional wired)
  (declare (sys.c::closure-allocation :wired))
  ;; 4k align the size.
  (setf size (logand (+ size #xFFF) (lognot #xFFF)))
  ;; 2m align the memory region.
  (let* ((addr (%allocate-stack-1 (align-up size #x200000)
                                  (if wired
                                      'sys.int::*wired-stack-area-bump*
                                      'sys.int::*stack-area-bump*)))
         (stack (%make-stack addr size)))
    ;; Allocate blocks.
    (loop
       for i from 0 do
         (when (allocate-memory-range addr size
                                      (logior sys.int::+block-map-present+
                                              sys.int::+block-map-writable+
                                              sys.int::+block-map-zero-fill+
                                              (if wired
                                                  sys.int::+block-map-wired+
                                                  0)))
           (return))
         (when (> i mezzano.runtime::*maximum-allocation-attempts*)
           (error 'storage-condition))
         (debug-print-line "No memory for stack, calling GC.")
         (sys.int::gc))
    (sys.int::make-weak-pointer stack stack
                                (lambda ()
                                  (release-memory-range addr size))
                                :wired)
    stack))

(defun reboot ()
  ;; FIXME: Need to sync disks and wait until snapshotting finishes.
  (platform-reboot)
  (values))

;;; <<<<<<

(sys.int::defglobal *boot-information-page*)

(defconstant +virtual-address-bits+ 48)
(defconstant +log2-4k-page+ 12)
(defconstant +n-32-bit-physical-buddy-bins+ (- 32 +log2-4k-page+)
  "Number of buddy bins for the below 4GB allocator.")
(defconstant +n-64-bit-physical-buddy-bins+ (- 39 +log2-4k-page+)
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
(defconstant +boot-information-acpi-rsdp+                         808)
(defconstant +boot-information-options+                           816)
(defconstant +boot-information-n-memory-map-entries+              824)
(defconstant +boot-information-memory-map+                        832)
(defconstant +boot-information-efi-system-table+                 1344)
(defconstant +boot-information-fdt-address+                      1352)
(defconstant +boot-information-block-map+                        1360)

(defconstant +boot-option-force-read-only+ #x01)
(defconstant +boot-option-freestanding+ #x02)
(defconstant +boot-option-video-console+ #x04)
(defconstant +boot-option-no-detect+ #x08)

(defun boot-uuid (offset)
  (check-type offset (integer 0 15))
  (sys.int::memref-unsigned-byte-8 (+ +boot-information-boot-uuid-offset+ *boot-information-page*)
                                   offset))

(defun boot-field (field)
  (sys.int::memref-t (+ *boot-information-page* field)))

(defun boot-option (option)
  (logtest (boot-field +boot-information-options+) option))

(sys.int::defglobal *boot-hook-lock*)
(sys.int::defglobal *early-boot-hooks*)
(sys.int::defglobal *boot-hooks*)
(sys.int::defglobal *late-boot-hooks*)

(defun add-boot-hook (fn &optional when)
  (check-type when (member nil :late :early))
  (with-mutex (*boot-hook-lock*)
    (case when
      (:early
       (push fn *early-boot-hooks*))
      ((nil)
       (push fn *boot-hooks*))
      (:late
       (push fn *late-boot-hooks*)))))

(defun remove-boot-hook (fn)
  (with-mutex (*boot-hook-lock*)
    (setf *early-boot-hooks* (remove fn *early-boot-hooks*))
    (setf *boot-hooks* (remove fn *boot-hooks*))
    (setf *late-boot-hooks* (remove fn *late-boot-hooks*))))

(defun run-boot-hooks ()
  (dolist (hook *early-boot-hooks*)
    (sys.int::log-and-ignore-errors
      (format t "Run early boot hook ~A~%" hook)
      (funcall hook)))
  (dolist (hook *boot-hooks*)
    (sys.int::log-and-ignore-errors
      (format t "Run boot hook ~A~%" hook)
      (funcall hook)))
  (dolist (hook *late-boot-hooks*)
    (sys.int::log-and-ignore-errors
      (format t "Run late boot hook ~A~%" hook)
      (funcall hook))))

(sys.int::defglobal *boot-id*)

(defun current-boot-id ()
  *boot-id*)

(sys.int::defglobal *deferred-boot-actions*)

(defun add-deferred-boot-action (action)
  (if (boundp '*deferred-boot-actions*)
      (push-wired action *deferred-boot-actions*)
      (funcall action)))

(sys.int::defglobal *post-boot-worker-thread*)

(defun post-boot-worker ()
  (loop
     ;; Run deferred boot actions first.
     (dolist (action *deferred-boot-actions*)
       (funcall action))
     (makunbound '*deferred-boot-actions*)
     ;; Now normal boot hooks.
     (run-boot-hooks)
     ;; Sleep til next boot.
     (%run-on-wired-stack-without-interrupts (sp fp)
      (let ((self (current-thread)))
        (decf *snapshot-inhibit*)
        (setf (thread-wait-item self) "Next boot"
              (thread-state self) :sleeping)
        (%reschedule-via-wired-stack sp fp)))))

(defun sys.int::bootloader-entry-point (boot-information-page)
  (let ((first-run-p nil))
    (initialize-boot-cpu)
    (initialize-debug-log)
    (initialize-platform-early-console boot-information-page)
    (initialize-initial-thread)
    (setf *boot-information-page* boot-information-page
          *cold-unread-char* nil
          mezzano.runtime::*paranoid-allocation* nil
          *deferred-boot-actions* '()
          *paging-disk* nil
          *page-fault-hook* nil)
    (initialize-physical-allocator)
    (initialize-early-video)
    (when (boot-option +boot-option-video-console+)
      (debug-set-output-pseudostream #'debug-video-stream))
    (when (not (boundp 'mezzano.runtime::*active-catch-handlers*))
      (setf first-run-p t)
      (mezzano.runtime::first-run-initialize-allocator)
      ;; FIXME: Should be done by cold generator
      (setf mezzano.runtime::*active-catch-handlers* 'nil
            *pseudo-atomic* nil
            sys.int::*known-finalizers* nil))
    (setf *boot-id* (sys.int::cons-in-area nil nil :wired))
    (initialize-early-platform)
    (initialize-threads)
    (initialize-disk)
    (initialize-pager)
    (initialize-snapshot)
    (%enable-interrupts)
    ;;(debug-set-output-pseudostream #'debug-video-stream)
    ;;(debug-set-output-pseudostream (lambda (op &optional arg) (declare (ignore op arg))))
    (debug-write-line "Hello, Debug World!")
    (initialize-time)
    (initialize-video)
    (initialize-efi)
    (initialize-acpi)
    (initialize-virtio)
    (initialize-platform)
    (initialize-time-late)
    (when (not (boot-option +boot-option-no-detect+))
      (detect-disk-partitions))
    (initialize-paging-system)
    (cond (first-run-p
           (setf *post-boot-worker-thread* (make-thread #'post-boot-worker :name "Post-boot worker thread")
                 *boot-hook-lock* (make-mutex "Boot Hook Lock")
                 *early-boot-hooks* '()
                 *boot-hooks* '()
                 *late-boot-hooks* '())
           (make-thread #'sys.int::initialize-lisp :name "Main thread"))
          (t (wake-thread *post-boot-worker-thread*)))
    (finish-initial-thread)))
