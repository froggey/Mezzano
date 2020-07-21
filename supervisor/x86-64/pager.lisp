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

(defun pte-page-present-p (pte)
  (logtest +x86-64-pte-present+ pte))

(defun page-present-p (page-table &optional (index 0))
  (pte-page-present-p (page-table-entry page-table index)))

(defun pte-page-writable-p (pte)
  (logtest +x86-64-pte-write+ pte))

(defun page-writable-p (page-table &optional (index 0))
  (pte-page-writable-p (page-table-entry page-table index)))

(defun pte-page-copy-on-write-p (pte)
  (logtest +x86-64-pte-copy-on-write+ pte))

(defun page-copy-on-write-p (page-table &optional (index 0))
  (pte-page-copy-on-write-p (page-table-entry page-table index)))

(defun page-dirty-p (page-table &optional (index 0))
  (logtest +x86-64-pte-dirty+
           (page-table-entry page-table index)))

(defun page-frame (page-table &optional (index 0))
  (ash (logand +x86-64-pte-address-mask+
               (page-table-entry page-table index))
       -12))

(defun address-l4-bits (address) (ldb (byte 9 39) address))
(defun address-l3-bits (address) (ldb (byte 9 30) address))
(defun address-l2-bits (address) (ldb (byte 9 21) address))
(defun address-l1-bits (address) (ldb (byte 9 12) address))

(defun descend-page-table (page-table index allocate)
  (if (not (page-present-p page-table index))
      (when allocate
        ;; No PT. Allocate one.
        (let* ((frame (pager-allocate-page :new-type :page-table))
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

(defun update-pte (pte &key (writable nil writablep) (dirty nil dirtyp))
  (let ((current-entry (page-table-entry pte)))
    (when writablep
      (if writable
          (setf current-entry (logior current-entry +x86-64-pte-write+))
          (setf current-entry (logand current-entry (lognot +x86-64-pte-write+)))))
    (when dirtyp
      (if dirty
          (setf current-entry (logior current-entry +x86-64-pte-dirty+))
          (setf current-entry (logand current-entry (lognot +x86-64-pte-dirty+)))))
    (setf (page-table-entry pte) current-entry)))

(defun update-pte-atomic (pte pte-value &key (writable nil writablep) (dirty nil dirtyp))
  (let ((current-entry pte-value))
    (when writablep
      (if writable
          (setf current-entry (logior current-entry +x86-64-pte-write+))
          (setf current-entry (logand current-entry (lognot +x86-64-pte-write+)))))
    (when dirtyp
      (if dirty
          (setf current-entry (logior current-entry +x86-64-pte-dirty+))
          (setf current-entry (logand current-entry (lognot +x86-64-pte-dirty+)))))
    (eql (sys.int::cas (page-table-entry pte) pte-value current-entry) pte-value)))

(defun make-pte (frame &key writable (present t) wired dirty copy-on-write (cache-mode :normal))
  (declare (ignore wired cache-mode))
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

(defun map-ptes-pml2 (pml2 pml2e start end fn)
  (let ((entry (page-table-entry pml2 pml2e)))
    (when (logtest entry +x86-64-pte-present+)
      (loop
         with pml1 = (convert-to-pmap-address (logand entry +x86-64-pte-address-mask+))
         for i from (align-down start #x0000000000001000) below (align-up end #x0000000000001000) by #x0000000000001000
         for pml1e = (ldb (byte 9 12) i)
         do
           (funcall fn i (+ pml1 (* pml1e 8)))))))

(defun map-ptes-pml3 (pml3 pml3e start end fn)
  (let ((entry (page-table-entry pml3 pml3e)))
    (when (logtest entry +x86-64-pte-present+)
      (loop
         with pml2 = (convert-to-pmap-address (logand entry +x86-64-pte-address-mask+))
         for i from (align-down start #x0000000000200000) below (align-up end #x0000000000200000) by #x0000000000200000
         for pml2e = (ldb (byte 9 21) i)
         do
           (map-ptes-pml2 pml2 pml2e
                          (max start i)
                          (min end (+ i #x0000000000200000))
                          fn)))))

(defun map-ptes-pml4 (pml4 pml4e start end fn)
  (let ((entry (page-table-entry pml4 pml4e)))
    (when (logtest entry +x86-64-pte-present+)
      (loop
         with pml3 = (convert-to-pmap-address (logand entry +x86-64-pte-address-mask+))
         for i from (align-down start #x0000000040000000) below (align-up end #x0000000040000000) by #x0000000040000000
         for pml3e = (ldb (byte 9 30) i)
         do
           (map-ptes-pml3 pml3 pml3e
                          (max start i)
                          (min end (+ i #x0000000040000000))
                          fn)))))

(defun map-ptes (start end fn &key sparse)
  "Visit all visible page table entries from START to END.
If SPARSE is true, then PTEs that don't exist in the range won't be visited;
otherwise FN will be called with a NIL PTE for those entries."
  (cond (sparse
         (loop
            with start-pml4e = (ldb (byte 9 39) (align-down start #x0000008000000000))
            with end-pml4e = (ldb (byte 9 39) (align-up end #x0000008000000000))
            with pml4 = (convert-to-pmap-address (logand (sys.int::%cr3) (lognot #xFFF)))
            for pml4e from start-pml4e below end-pml4e
            for address from (align-down start #x0000008000000000) by #x0000008000000000
            do
              (map-ptes-pml4 pml4 pml4e
                             (max start address)
                             (min end (+ address #x0000008000000000))
                             fn)))
        (t
         (loop
            for page from start below end by #x1000
            for pte = (get-pte-for-address page nil)
            when (or (not sparse) pte)
            do (funcall fn page pte)))))
