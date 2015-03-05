;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Packages used for cross-compilation.

(defpackage :cross-cl
  (:use :cl)
  (:shadow :proclaim
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
           :pi)
  (:export . #.(let ((symbols '()))
                 (do-external-symbols (sym :cl symbols)
                   (push sym symbols)))))

;;; Watch out, package exports below here must be kept in sync with package.lisp

(defpackage :system.compiler
  (:nicknames :sys.c)
  (:export :compile
           :compiler-macro-function
           :*macroexpand-hook*
           :macroexpand
           :macroexpand-1
           :macro-function
           :constantp

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
           #:lambda-information-plist
           #:lexical-variable
           #:lexical-variable-p
           #:block-information-env-var
           #:block-information-count
           #:block-information-return-mode
           #:tagbody-information-go-tags
           #:go-tag-p)
  (:use :cross-cl))

(in-package :system.compiler)

(defpackage :system
  (:export :dotted-list-length
           :parse-ordinary-lambda-list
           :lambda-name
           :proclaimed-special-p
           :symbol-macro-function
           :variable-information
           :symbol-mode
           :io-port/8
           :io-port/16
           :io-port/32
           :char-bit
           :char-bits
           :fixnump))

(declaim (declaration system:lambda-name))

(defpackage :system.internals
  (:nicknames :sys.int)
  (:use :cross-cl :system)
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

(defpackage :system.closette
  (:nicknames :sys.clos :clos)
  (:use :cross-cl)
  (:import-from :sys.int
		:allocate-std-instance
		:std-instance-p
		:std-instance-class
		:std-instance-slots
                :allocate-funcallable-std-instance
                :funcallable-std-instance-p
                :funcallable-std-instance-function
                :funcallable-std-instance-class
                :funcallable-std-instance-slots))

(defpackage :sys.format)

;;; Supervisor manages the hardware, doing paging and memory management.
(defpackage :mezzano.supervisor
  (:use :cross-cl)
  (:export #:current-thread
           #:with-symbol-spinlock
           #:with-gc-deferred
           #:with-pseudo-atomic
           #:without-interrupts
           #:with-world-stopped
           #:make-thread
           #:thread
           #:threadp
           #:thread-name
           #:thread-state
           #:thread-lock
           #:thread-stack
           #:thread-stack-pointer
           #:thread-wait-item
           #:thread-special-stack-pointer
           #:thread-preemption-disable-depth
           #:thread-preemption-pending
           #:thread-%next
           #:thread-%prev
           #:thread-foothold-disable-depth
           #:thread-frame-pointer
           #:thread-mutex-stack
           #:thread-global-next
           #:thread-global-prev
           #:thread-yield
           #:all-threads
           #:establish-thread-foothold
           #:destroy-thread
           #:make-mutex
           #:with-mutex
           #:make-condition-variable
           #:condition-wait
           #:condition-notify
           #:snapshot
           #:protect-memory-range
           #:release-memory-range
           #:compact-block-freelist
           #:debug-print-line
           #:panic
           #:make-fifo
           #:fifo-push
           #:fifo-pop
           #:fifo-reset
           #:fifo-size
           #:fifo-element-type
           #:add-boot-hook
           #:remove-boot-hook
           #:fetch-boot-modules
           #:store-statistics

           ;; Temporary drivers.
           #:ps/2-key-read
           #:ps/2-aux-read
           #:current-framebuffer
           #:framebuffer-blit
           #:framebuffer-width
           #:framebuffer-height
           #:*nics*
           #:nic
           #:nic-mac
           #:nic-mtu
           #:net-statistics
           #:net-transmit-packet
           #:net-receive-packet
           ;; The heartbeat timer is wired directly to the PIT, and beats at 18Hz.
           #:wait-for-heartbeat
           #:read-rtc-time
           #:all-disks
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
           #:stop-profiling))

;;; Runtime contains a bunch of low-level and common functions required to
;;; run the supervisor and the rest of the CL system.
(defpackage :mezzano.runtime
  (:use :cross-cl))

(defpackage :sys.lap
  (:documentation "The system assembler.")
  (:use :cross-cl)
  (:export :perform-assembly :emit :immediatep :resolve-immediate :*current-address*
           :note-fixup))

(defpackage :sys.lap-x86
  (:documentation "x86 assembler for LAP.")
  (:use :cross-cl :sys.lap)
  (:export #:assemble
           #:*function-reference-resolver*))
