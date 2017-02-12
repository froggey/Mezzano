;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +x86-64-pte-present+        #x001)
(defconstant +x86-64-pte-write+          #x002)
(defconstant +x86-64-pte-user+           #x004)
(defconstant +x86-64-pte-write-through+  #x008)
(defconstant +x86-64-pte-cache-disabled+ #x010)
(defconstant +x86-64-pte-accessed+       #x020)
(defconstant +x86-64-pte-dirty+          #x040)
(defconstant +x86-64-pte-page-size+      #x080)
(defconstant +x86-64-pte-global+         #x100)
(defconstant +x86-64-pte-copy-on-write+  #x400)
(defconstant +x86-64-pte-address-mask+   #x000FFFFFFFFFF000)

(defun flush-tlb ()
  ;; Reloading CR3 on x86oids causes all TLBs to be marked invalid.
  (setf (sys.int::%cr3) (sys.int::%cr3)))

(defun flush-tlb-single (address)
  (sys.int::%invlpg address))

(defun page-present-p (page-table &optional (index 0))
  (logtest +x86-64-pte-present+
           (page-table-entry page-table index)))

(defun page-copy-on-write-p (page-table &optional (index 0))
  (logtest +x86-64-pte-copy-on-write+
           (page-table-entry page-table index)))

(defun page-dirty-p (page-table &optional (index 0))
  (logtest +x86-64-pte-dirty+
           (page-table-entry page-table index)))

(defun address-l4-bits (address) (ldb (byte 9 39) address))
(defun address-l3-bits (address) (ldb (byte 9 30) address))
(defun address-l2-bits (address) (ldb (byte 9 21) address))
(defun address-l1-bits (address) (ldb (byte 9 12) address))

(defun descend-page-table (page-table index allocate)
  (if (not (page-present-p page-table index))
      (when allocate
        ;; No PT. Allocate one.
        (let* ((frame (pager-allocate-page :page-table))
               (addr (convert-to-pmap-address (ash frame 12))))
          (zeroize-page addr)
          (setf (page-table-entry page-table index) (logior (ash frame 12)
                                                            +x86-64-pte-present+
                                                            +x86-64-pte-accessed+
                                                            +x86-64-pte-write+))
          addr))
      (convert-to-pmap-address (logand (page-table-entry page-table index)
                                       +x86-64-pte-address-mask+))))

(defun get-pte-for-address (address &optional (allocate t))
  (let* ((cr3 (convert-to-pmap-address (logand (sys.int::%cr3) (lognot #xFFF))))
         (pdp            (descend-page-table cr3  (address-l4-bits address) allocate))
         (pdir (and pdp  (descend-page-table pdp  (address-l3-bits address) allocate)))
         (pt   (and pdir (descend-page-table pdir (address-l2-bits address) allocate))))
    (and pt (+ pt (* 8 (address-l1-bits address))))))

(defun pte-physical-address (pte)
  (logand pte +x86-64-pte-address-mask+))

(defun make-pte (frame &key writable (present t) wired dirty copy-on-write)
  (declare (ignore wired))
  (logior (ash frame 12)
          (if present
              +x86-64-pte-present+
              0)
          (if writable
              +x86-64-pte-write+
              0)
          (if dirty
              (logior +x86-64-pte-dirty+
                      +x86-64-pte-accessed+)
              0)
          (if copy-on-write
              +x86-64-pte-copy-on-write+
              0)))
