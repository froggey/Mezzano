;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Packages used for cross-compilation.

(defpackage :cross-cl
  (:use :cl)
  (:shadow :defconstant
           :proclaim
           :get-setf-expansion
           :macroexpand
           :macroexpand-1
           :compiler-macro-function
           :macro-function
           :most-positive-fixnum
           :most-negative-fixnum
           :lambda-list-keywords
           :*features*
           :compile
           :*macroexpand-hook*
           :constantp
           :array-rank-limit
           :array-dimension-limit
           :array-total-size-limit
           :char-code-limit
           :call-arguments-limit
           :lambda-parameters-limit
           :multiple-values-limit
           :most-negative-short-float
           :most-negative-single-float
           :most-negative-double-float
           :most-negative-long-float
           :most-positive-short-float
           :most-positive-single-float
           :most-positive-double-float
           :most-positive-long-float
           :boole-1
           :boole-2
           :boole-andc1
           :boole-andc2
           :boole-and
           :boole-c1
           :boole-c2
           :boole-clr
           :boole-eqv
           :boole-ior
           :boole-nand
           :boole-nor
           :boole-orc1
           :boole-orc2
           :boole-set
           :boole-xor
           :internal-time-units-per-second
           :pi
           :byte
           :byte-size
           :byte-position
           :ldb
           :dpb
           :subtypep
           :upgraded-array-element-type)
  (:export . #.(let ((symbols '()))
                 (do-external-symbols (sym :cl symbols)
                   (push sym symbols)))))

;;; Watch out, package exports below here must be kept in sync with package.lisp

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
  (:use :cross-cl))

(defpackage :mezzano.compiler.codegen.x86-64
  (:use :cross-cl :mezzano.compiler)
  (:export #:codegen-lambda
           #:generate-builtin-functions))

(defpackage :mezzano.lap.arm64
  (:documentation "arm64 assembler for LAP.")
  (:use :cross-cl)
  (:export #:assemble))

(defpackage :mezzano.compiler.codegen.arm64
  (:use :cross-cl :mezzano.compiler)
  (:local-nicknames (:lap :mezzano.lap.arm64))
  (:export #:codegen-lambda
           #:generate-builtin-functions))

(in-package :system.compiler)

(defpackage :system.internals
  (:nicknames :sys.int)
  (:use :cross-cl)
  (:export #:allocate-std-instance
           #:std-instance-p
           #:std-instance-class
           #:std-instance-slots
           #:std-instance-layout
           #:allocate-funcallable-std-instance
           #:funcallable-std-instance-p
           #:funcallable-std-instance-function
           #:funcallable-std-instance-class
           #:funcallable-std-instance-slots
           #:funcallable-std-instance-layout
           #:funcallable-standard-object))

(declaim (declaration sys.int::lambda-name))

(defpackage :mezzano.clos
  (:use :cross-cl)
  (:import-from :sys.int
                #:allocate-std-instance
                #:std-instance-p
                #:std-instance-class
                #:std-instance-slots
                #:allocate-funcallable-std-instance
                #:funcallable-std-instance-p
                #:funcallable-std-instance-function
                #:funcallable-std-instance-class
                #:funcallable-std-instance-slots)
  (:export #:defclass #:defgeneric #:defmethod
           #:find-class #:class-of
           #:call-next-method #:next-method-p
           #:slot-value #:slot-boundp #:slot-exists-p #:slot-makunbound
           #:make-instance #:change-class
           #:initialize-instance #:reinitialize-instance #:shared-initialize
           #:update-instance-for-different-class
           #:update-instance-for-redefined-class
           #:print-object
           #:set-funcallable-instance-function

           #:standard-object #:funcallable-standard-object
           #:standard-class #:funcallable-standard-class
           #:standard-generic-function #:standard-method
           #:standard-slot-definition
           #:standard-effective-slot-definition
           #:standard-direct-slot-definition

           #:class-name
           #:class-direct-superclasses #:class-direct-slots
           #:class-precedence-list #:class-slots #:class-direct-subclasses
           #:class-direct-methods
           #:class-finalized-p
           #:class-prototype
           #:generic-function-name #:generic-function-lambda-list
           #:generic-function-methods #:generic-function-discriminating-function
           #:generic-function-method-class
           #:generic-function-method-combination
           #:generic-function-argument-precedence-order
           #:method-lambda-list #:method-qualifiers #:method-specializers
           #:method-generic-function #:method-function
           #:slot-definition-name #:slot-definition-initfunction
           #:slot-definition-initform #:slot-definition-initargs
           #:slot-definition-readers #:slot-definition-writers
           #:slot-definition-allocation
           #:slot-definition-documentation
           #:slot-definition-type
           ;;
           ;; Class-related metaobject protocol
           ;;
           #:compute-class-precedence-list #:compute-slots
           #:compute-effective-slot-definition
           #:finalize-inheritance #:allocate-instance
           #:slot-value-using-class #:slot-boundp-using-class
           #:slot-exists-p-using-class #:slot-makunbound-using-class
           ;;
           ;; Generic function related metaobject protocol
           ;;
           #:compute-discriminating-function
           #:compute-applicable-methods-using-classes #:method-more-specific-p
           #:compute-applicable-methods
           #:compute-effective-method-function
           #:compute-effective-method
           #:compute-method-function
           #:apply-methods #:apply-method

           #:metaobject #:specializer #:class
           #:structure-class #:structure-object
           #:intern-eql-specializer #:eql-specializer #:eql-specializer-object

           #:slot-unbound #:no-applicable-method

           #:with-slots #:with-accessors

           #:extract-lambda-list
           #:extract-specializer-names

           #:validate-superclass

           #:ensure-class
           #:ensure-class-using-class
           ))

(defpackage :sys.format)

;;; Supervisor manages the hardware, doing paging and memory management.
(defpackage :mezzano.supervisor
  (:use :cross-cl)
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
  (:use :cross-cl))

(defpackage :sys.lap
  (:documentation "The system assembler.")
  (:use :cross-cl)
  (:export #:perform-assembly
           #:emit
           #:immediatep
           #:resolve-immediate
           #:*current-address*
           #:note-fixup
           #:*function-reference-resolver*))

(defpackage :sys.lap-x86
  (:documentation "x86 assembler for LAP.")
  (:use :cross-cl :sys.lap)
  (:export #:assemble))

(in-package :sys.c)

(defstruct (byte
             (:constructor byte (size position)))
  size
  position)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun loose-constant-equal (x y)
  (or (eql x y)
      (and (typep x 'byte)
           (typep y 'byte)
           (equalp x y))))
)

;; Super early definition until the real DEFCONSTANT is loaded.
(defmacro defconstant (name value &optional doc)
  `(alexandria:define-constant ,name ,value
     :test 'loose-constant-equal
     ,@(when doc (list :documentation doc))))
