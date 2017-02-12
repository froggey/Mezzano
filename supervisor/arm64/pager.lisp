;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +arm64-tte-present+            #x001)
(defconstant +arm64-tte-not-block+          #x002
  "Set when page is either a page table or a final ttl3 page.")
(defconstant +arm64-tte-accessed+           #x400)
(defconstant +arm64-tte-ap+                 (byte 2 6))
(defconstant +arm64-tte-ap-prw-una+         0 "Prot RW, user not accessible")
(defconstant +arm64-tte-ap-prw-urw+         1 "Prot RW, user RW")
(defconstant +arm64-tte-ap-pro-una+         2 "Prot RO, user not accessible")
(defconstant +arm64-tte-ap-pro-uro+         3 "Prot RO, user RO")
(defconstant +arm64-tte-accessed+           #x400)
(defconstant +arm64-tte-sh+                 (byte 2 8))
(defconstant +arm64-tte-sh-non-shareable+   0)
(defconstant +arm64-tte-sh-outer-shareable+ 2)
(defconstant +arm64-tte-sh-inner-shareable+ 3)

(defconstant +arm64-tte-copy-on-write+ #x0080000000000000)
(defconstant +arm64-tte-writable+      #x0100000000000000)
(defconstant +arm64-tte-dirty+         #x0200000000000000)
(defconstant +arm64-tte-address-mask+  #x00007ffffffff000)

(sys.int::define-lap-function %ttbr0 (())
  (mezzano.lap.arm64:mrs :x9 :ttbr0-el1)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %ttbr1 (())
  (mezzano.lap.arm64:mrs :x9 :ttbr1-el1)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function flush-tlb-single ((address))
  ;; TODO.
  (mezzano.lap.arm64:tlbi :vmalle1)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function flush-tlb (())
  (mezzano.lap.arm64:tlbi :vmalle1)
  (mezzano.lap.arm64:ret))

(defun address-l4-bits (address) (ldb (byte 9 39) address))
(defun address-l3-bits (address) (ldb (byte 9 30) address))
(defun address-l2-bits (address) (ldb (byte 9 21) address))
(defun address-l1-bits (address) (ldb (byte 9 12) address))

(defun make-pte (frame &key writable (present t) block wired dirty copy-on-write)
  (logior (ash frame 12)
          (if present
              (logior +arm64-tte-present+
                      +arm64-tte-accessed+
                      (dpb +arm64-tte-sh-inner-shareable+
                           +arm64-tte-sh+
                           0))
              0)
          (if block
              0
              +arm64-tte-not-block+)
          (if writable
              (logior +arm64-tte-writable+
                      (if (or wired dirty)
                          (logior (dpb +arm64-tte-ap-prw-una+
                                       +arm64-tte-ap+
                                       0)
                                  +arm64-tte-dirty+)
                          (dpb +arm64-tte-ap-pro-una+
                               +arm64-tte-ap+
                               0)))
              (dpb +arm64-tte-ap-pro-una+
                   +arm64-tte-ap+
                   0))
          (if copy-on-write
              +arm64-tte-copy-on-write+
              0)))

(defun pte-physical-address (pte)
  (logand pte +arm64-tte-address-mask+))

(defun page-present-p (page-table &optional (index 0))
  (logtest +arm64-tte-present+
           (page-table-entry page-table index)))

(defun page-copy-on-write-p (page-table &optional (index 0))
  (logtest +arm64-tte-copy-on-write+
           (page-table-entry page-table index)))

(defun page-dirty-p (page-table &optional (index 0))
  (logtest +arm64-tte-dirty+
           (page-table-entry page-table index)))

(defun descend-page-table (page-table index allocate)
  (if (not (page-present-p page-table index))
      (when allocate
        ;; No PT. Allocate one.
        (let* ((frame (pager-allocate-page :page-table))
               (addr (convert-to-pmap-address (ash frame 12))))
          (zeroize-page addr)
          (setf (page-table-entry page-table index) (make-pte frame :writable t))
          addr))
      (convert-to-pmap-address (pte-physical-address (page-table-entry page-table index)))))

(defun get-pte-for-address (address &optional (allocate t))
  (let* ((ttl0 (convert-to-pmap-address (if (logbitp 63 address)
                                            (%ttbr1)
                                            (%ttbr0))))
         (ttl1           (descend-page-table ttl0 (address-l4-bits address) allocate))
         (ttl2 (and ttl1 (descend-page-table ttl1 (address-l3-bits address) allocate)))
         (ttl3 (and ttl2 (descend-page-table ttl2 (address-l2-bits address) allocate))))
    (and ttl3 (+ ttl3 (* 8 (address-l1-bits address))))))
