;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Various packages.

(cl:in-package :cl-user)

(defpackage :mezzano.compiler
  (:nicknames :sys.c :system.compiler)
  (:export #:compile
           #:compiler-macro-function
           #:*macroexpand-hook*
           #:macroexpand
           #:macroexpand-1
           #:macro-function
           #:constantp
           #:fixnump

           #:quoted-form-p
           #:lambda-information
           #:lambda-information-p
           #:lambda-information-name
           #:lambda-information-docstring
           #:lambda-information-lambda-list
           #:lambda-information-body
           #:lambda-information-required-args
           #:lambda-information-optional-args
           #:lambda-information-rest-arg
           #:lambda-information-enable-keys
           #:lambda-information-key-args
           #:lambda-information-allow-other-keys
           #:lambda-information-environment-arg
           #:lambda-information-environment-layout
           #:lambda-information-fref-arg
           #:lambda-information-closure-arg
           #:lambda-information-count-arg
           #:lambda-information-plist
           #:lexical-variable
           #:lexical-variable-p
           #:lexical-variable-name
           #:lexical-variable-definition-point
           #:lexical-variable-ignore
           #:lexical-variable-dynamic-extent
           #:lexical-variable-use-count
           #:lexical-variable-write-count
           #:lexical-variable-used-in
           #:lexical-variable-plist
           #:localp
           #:special-variable
           #:block-information
           #:block-information-env-var
           #:block-information-count
           #:block-information-return-mode
           #:tagbody-information
           #:tagbody-information-go-tags
           #:go-tag
           #:go-tag-p
           #:go-tag-name
           #:go-tag-tagbody
           #:go-tag-use-count
           #:go-tag-used-in

           #:ast-block
           #:ast-function
           #:ast-go
           #:ast-if
           #:ast-let
           #:ast-multiple-value-bind
           #:ast-multiple-value-call
           #:ast-multiple-value-prog1
           #:ast-progn
           #:ast-quote
           #:ast-return-from
           #:ast-setq
           #:ast-tagbody
           #:ast-the
           #:ast-unwind-protect
           #:ast-call
           #:ast-jump-table

           #:ast-info
           #:ast-body
           #:ast-name
           #:ast-target
           #:ast-go-info
           #:ast-test
           #:ast-if-then
           #:ast-if-else
           #:ast-bindings
           #:ast-value-form
           #:ast-function-form
           #:ast-forms
           #:ast-value
           #:ast-setq-variable
           #:ast-statements
           #:ast-the-type
           #:ast-protected-form
           #:ast-cleanup-function
           #:ast-arguments
           #:ast-targets)
  (:use :cl))

(defpackage :mezzano.compiler.codegen.x86-64
  (:use :cl :mezzano.compiler)
  (:export #:codegen-lambda
           #:generate-builtin-functions))

(defpackage :mezzano.lap.arm64
  (:documentation "arm64 assembler for LAP.")
  (:use :cl)
  (:export #:assemble))

(defpackage :mezzano.compiler.codegen.arm64
  (:use :cl :mezzano.compiler)
  (:local-nicknames (:lap :mezzano.lap.arm64))
  (:export #:codegen-lambda
           #:generate-builtin-functions))

(defpackage :system.internals
  (:nicknames :sys.int)
  (:use :cl)
  (:export :allocate-std-instance
           :std-instance-p
           :std-instance-class
           :std-instance-slots
           :allocate-funcallable-std-instance
           :funcallable-std-instance-p
           :funcallable-std-instance-function
           :funcallable-std-instance-class
           :funcallable-std-instance-slots
           :funcallable-standard-object))

(defpackage :mezzano.clos
  (:use :cl))

;;; Supervisor manages the hardware, doing paging and memory management.
(defpackage :mezzano.supervisor
  (:use :cl)
  (:export #:current-thread
           #:with-symbol-spinlock
           #:with-pseudo-atomic
           #:with-snapshot-inhibited
           #:without-interrupts
           #:with-world-stopped
           #:make-thread
           #:thread
           #:threadp
           #:thread-name
           #:thread-state
           #:thread-lock
           #:thread-stack
           #:thread-wait-item
           #:thread-special-stack-pointer
           #:thread-full-save-p
           #:thread-%next
           #:thread-%prev
           #:thread-pending-footholds
           #:thread-mutex-stack
           #:thread-global-next
           #:thread-global-prev
           #:thread-priority
           #:thread-pager-argument-1
           #:thread-pager-argument-2
           #:thread-pager-argument-3
           #:thread-state-r15
           #:thread-state-r14
           #:thread-state-r13
           #:thread-state-r13-value
           #:thread-state-r12
           #:thread-state-r12-value
           #:thread-state-r11
           #:thread-state-r11-value
           #:thread-state-r10
           #:thread-state-r10-value
           #:thread-state-r9
           #:thread-state-r9-value
           #:thread-state-r8
           #:thread-state-r8-value
           #:thread-state-rdi
           #:thread-state-rsi
           #:thread-state-rbx
           #:thread-state-rbx-value
           #:thread-state-rdx
           #:thread-state-rdx-value
           #:thread-state-rcx
           #:thread-state-rcx-value
           #:thread-state-rax
           #:thread-state-rax-value
           #:thread-state-rbp
           #:thread-frame-pointer
           #:thread-state-rip
           #:thread-state-cs
           #:thread-state-rflags
           #:thread-state-rsp
           #:thread-stack-pointer
           #:thread-state-ss
           #:thread-yield
           #:all-threads
           #:without-footholds
           #:establish-thread-foothold
           #:terminate-thread
           #:mutex
           #:mutex-p
           #:make-mutex
           #:with-mutex
           #:mutex-held-p
           #:acquire-mutex
           #:release-mutex
           #:condition-variable
           #:condition-variable-p
           #:make-condition-variable
           #:condition-wait
           #:condition-notify
           #:latch
           #:latch-p
           #:make-latch
           #:latch-reset
           #:latch-wait
           #:latch-trigger
           #:snapshot
           #:allocate-memory-range
           #:protect-memory-range
           #:release-memory-range
           #:debug-print-line
           #:panic
           #:fifo
           #:fifo-p
           #:make-fifo
           #:fifo-push
           #:fifo-pop
           #:fifo-reset
           #:fifo-size
           #:fifo-element-type
           #:add-boot-hook
           #:remove-boot-hook
           #:store-statistics
           #:physical-memory-statistics
           #:reboot
           #:current-boot-id

           ;; Temporary drivers.
           #:ps/2-key-read
           #:ps/2-aux-read
           #:current-framebuffer
           #:framebuffer-blit
           #:framebuffer-width
           #:framebuffer-height
           ;; The heartbeat timer is wired directly to the PIT, and beats at 18Hz.
           #:wait-for-heartbeat
           #:read-rtc-time
           #:all-disks
           #:disk-writable-p
           #:disk-n-sectors
           #:disk-sector-size
           #:disk-read
           #:disk-write
           #:make-disk-request
           #:disk-submit-request
           #:disk-cancel-request
           #:disk-await-request
           #:disk-request-complete-p
           #:start-profiling
           #:stop-profiling
           #:*virtio-input-devices*
           #:read-virtio-input-device
           #:virtualbox-read-event
           #:virtualbox-graphics-update-framebuffer

           #:pci-device-location
           #:pci-config/8
           #:pci-config/16
           #:pci-config/32
           #:pci-base-class
           #:pci-sub-class
           #:pci-programming-interface
           #:pci-bar
           #:pci-io-region
           #:pci-io-region/8
           #:pci-io-region/16
           #:pci-io-region/32
           #:pci-intr-line
           #:pci-bus-master-enabled
           #:map-pci-devices
           #:pci-probe
           #:define-pci-driver

           #:make-simple-irq
           #:simple-irq-unmask
           #:simple-irq-mask
           #:simple-irq-attach

           #:+virtio-dev-id-invalid+
           #:+virtio-dev-id-net+
           #:+virtio-dev-id-block+
           #:+virtio-dev-id-console+
           #:+virtio-dev-id-entropy-src+
           #:+virtio-dev-id-mem-balloon+
           #:+virtio-dev-id-io-memory+
           #:+virtio-dev-id-rpmsg+
           #:+virtio-dev-id-scsi-host+
           #:+virtio-dev-id-9p-transport+
           #:+virtio-dev-id-mac80211-wlan+
           #:+virtio-dev-id-rproc-serial+
           #:+virtio-dev-id-caif+
           #:+virtio-dev-id-gpu+
           #:+virtio-dev-id-input+

           #:+virtio-status-reset+
           #:+virtio-status-acknowledge+
           #:+virtio-status-driver+
           #:+virtio-status-ok+
           #:+virtio-status-failed+

           #:+virtio-ring-desc-f-next+
           #:+virtio-ring-desc-f-write+
           #:+virtio-ring-desc-f-indirect+

           #:virtio-device
           #:virtqueue
           #:virtio-ring-size
           #:virtio-virtqueue
           #:virtqueue-index
           #:virtqueue-size
           #:virtqueue-avail-offset
           #:virtqueue-used-offset
           #:virtqueue-next-free-descriptor
           #:virtqueue-last-seen-used
           #:virtio-device-specific-header/8
           #:virtio-device-specific-header/16
           #:virtio-device-specific-header/32
           #:virtio-device-specific-header/64
           #:virtio-ring-desc-address
           #:virtio-ring-desc-length
           #:virtio-ring-desc-flags
           #:virtio-ring-desc-next
           #:virtio-ring-avail-flags
           #:virtio-ring-avail-idx
           #:virtio-ring-avail-ring
           #:virtio-ring-used-flags
           #:virtio-ring-used-idx
           #:virtio-ring-used-elem-id
           #:virtio-ring-used-elem-len
           #:virtio-ring-alloc-descriptor
           #:virtio-ring-free-descriptor
           #:virtio-ring-add-to-avail-ring
           #:virtio-pop-used-ring
           #:virtio-kick
           #:virtio-ring-disable-interrupts
           #:virtio-ring-enable-interrupts
           #:virtio-device-status
           #:virtio-device-features
           #:virtio-guest-features
           #:virtio-isr-status
           #:virtio-device-irq
           #:virtio-attach-irq
           #:virtio-ack-irq
           #:virtio-irq-mask
           #:virtio-configure-virtqueues
           #:virtio-driver-detached
           #:define-virtio-driver
           ))

;;; Runtime contains a bunch of low-level and common functions required to
;;; run the supervisor and the rest of the CL system.
(defpackage :mezzano.runtime
  (:use :cl))

(defpackage :sys.lap
  (:documentation "The system assembler.")
  (:use :cl)
  (:export #:perform-assembly
           #:emit
           #:immediatep
           #:resolve-immediate
           #:*current-address*
           #:note-fixup
           #:*function-reference-resolver*))

(defpackage :sys.lap-x86
  (:documentation "x86 assembler for LAP.")
  (:use :cl :sys.lap)
  (:export #:assemble))
