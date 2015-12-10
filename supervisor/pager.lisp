;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defvar *vm-lock*)

(defvar *pager-fast-path-enabled*)

(defvar *pager-fast-path-hits*)
(defvar *pager-fast-path-misses*)

(defvar *pager-waiting-threads*)
(defvar *pager-current-thread*)
(defvar *pager-lock*)
(defvar *pager-disk-request*)

(defvar *paging-disk*)
(defvar *bml4*)

(defvar *page-replacement-list-head*)
(defvar *page-replacement-list-tail*)

(defvar *store-fudge-factor*)

(defconstant +page-table-present+        #x001)
(defconstant +page-table-write+          #x002)
(defconstant +page-table-user+           #x004)
(defconstant +page-table-write-through+  #x008)
(defconstant +page-table-cache-disabled+ #x010)
(defconstant +page-table-accessed+       #x020)
(defconstant +page-table-dirty+          #x040)
(defconstant +page-table-page-size+      #x080)
(defconstant +page-table-global+         #x100)
(defconstant +page-table-copy-on-write+  #x400)
(defconstant +page-table-address-mask+   #x000FFFFFFFFFF000)

(declaim (inline flush-tlb))
(defun flush-tlb ()
  ;; Reloading CR3 on x86oids causes all TLBs to be marked invalid.
  (setf (sys.int::%cr3) (sys.int::%cr3)))

(declaim (inline page-table-entry (setf page-table-entry)))
(defun page-table-entry (page-table index)
  (sys.int::memref-unsigned-byte-64 page-table index))
(defun (setf page-table-entry) (value page-table index)
  (setf (sys.int::memref-unsigned-byte-64 page-table index) value))

(declaim (inline page-present-p))
(defun page-present-p (page-table index)
  (logtest +page-table-present+
           (page-table-entry page-table index)))

(declaim (inline address-l4-bits address-l3-bits address-l2-bits address-l1-bits))
(defun address-l4-bits (address) (ldb (byte 9 39) address))
(defun address-l3-bits (address) (ldb (byte 9 30) address))
(defun address-l2-bits (address) (ldb (byte 9 21) address))
(defun address-l1-bits (address) (ldb (byte 9 12) address))

(declaim (inline zeroize-page))
(defun zeroize-page (addr)
  (sys.int::%fill-words addr 0 512))

(defun page-aligned-p (value)
  (zerop (logand value #xFFF)))

(defun allocate-page (&optional mandatory)
  (let ((frame (allocate-physical-pages 1 :mandatory-p mandatory)))
    (when frame
      (+ +physical-map-base+ (* frame +4k-page-size+)))))

(defun free-page (page)
  (let ((frame (truncate (- page +physical-map-base+) +4k-page-size+)))
    (release-physical-pages frame 1)))

(defmacro with-disk-block ((page block-id) &body body)
  `(let ((,page nil))
     (unwind-protect
          (progn
            (setf ,page (read-disk-block ,block-id))
            ,@body)
       (when ,page
         (free-page ,page)))))

(defun read-disk-block (block-id)
  "Read a block from the disk, returning a freshly allocated page containing
the data. Free the page with FREE-PAGE when done."
  (let ((page (allocate-page "Disk block")))
    ;; This is only used during boot to read the freelist & block map, so
    ;; reusing *PAGER-DISK-REQUEST* is ok.
    (disk-submit-request *pager-disk-request*
                         *paging-disk*
                         :read
                         (* block-id (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                         (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                         page)
    (unless (disk-await-request *pager-disk-request*)
      (panic "Unable to read page from disk"))
    page))

(defun initialize-block-map (bml4-block)
  (debug-print-line "Reading block map.")
  (labels ((one-level (block-id fn)
             ;; Process a non-leaf level of the block map.
             ;; FN is called on each present block id to produce the next level.
             ;; Generate a table of memory pointers to the next level.
             (let ((memory-repr (allocate-page "Block Map")))
               (zeroize-page memory-repr)
               (with-disk-block (disk-repr block-id)
                 (dotimes (i 512)
                   (let ((entry (sys.int::memref-unsigned-byte-64 disk-repr i)))
                     (when (logtest sys.int::+block-map-present+ entry)
                       (setf (sys.int::memref-signed-byte-64 memory-repr i)
                             (funcall fn
                                      (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+)
                                           entry)))))))
               memory-repr))
           (level-1 (block-id)
             (read-disk-block block-id))
           (level-2 (block-id)
             (one-level block-id #'level-1))
           (level-3 (block-id)
             (one-level block-id #'level-2))
           (level-4 (block-id)
             (one-level block-id #'level-3)))
    (setf *bml4* (level-4 bml4-block))
    (debug-print-line "BML4 at " *bml4*)))

(defconstant +image-header-block-map+ 96)
(defconstant +image-header-freelist+ 104)

(defun initialize-paging-system (disk header)
  (setf *paging-disk* disk)
  (initialize-block-map (sys.int::memref-unsigned-byte-64 (+ header +image-header-block-map+) 0))
  (initialize-store-freelist (truncate (* (disk-n-sectors *paging-disk*) (disk-sector-size *paging-disk*)) #x1000)
                             (sys.int::memref-unsigned-byte-64 (+ header +image-header-freelist+) 0))
  (multiple-value-bind (free-blocks total-blocks)
      (store-statistics)
    (setf *store-fudge-factor* (+ (- total-blocks free-blocks) 256))
    (debug-print-line "Set fudge factor to " *store-fudge-factor*))
  (debug-print-line "Waking pager thread.")
  (setf (thread-state sys.int::*pager-thread*) :runnable))

(defun detect-paging-disk ()
  (dolist (disk (all-disks))
    (let* ((sector-size (disk-sector-size disk))
           (page (allocate-physical-pages (ceiling (max +4k-page-size+ sector-size) +4k-page-size+)
                                          :mandatory-p "DETECT-PAGING-DISK disk buffer"))
           (page-addr (+ +physical-map-base+ (* page +4k-page-size+))))
      ;; Read first 4k, figure out what to do with it.
      (when (not (disk-read disk 0 (ceiling +4k-page-size+ sector-size) page-addr))
        (panic "Unable to read first block on disk " disk))
      ;; Search for a Mezzano header here.
      (unwind-protect
           (when (and
                  (not *paging-disk*)
                  ;; Match magic.
                  (loop
                     for byte in '(#x00 #x4D #x65 #x7A #x7A #x61 #x6E #x69 #x6E #x65 #x49 #x6D #x61 #x67 #x65 #x00)
                     for offset from 0
                     do (when (not (eql (sys.int::memref-unsigned-byte-8 page-addr offset) byte))
                          (return nil))
                     finally (return t))
                  ;; Match boot UUID.
                  (loop
                     for offset from 0 below 16
                     do (when (not (eql (sys.int::memref-unsigned-byte-8 page-addr (+ 16 offset))
                                        (boot-uuid offset)))
                          (return nil))
                     finally (return t)))
             (debug-print-line "Found boot image on disk " disk "!")
             (initialize-paging-system disk page-addr)
             (return))
        ;; Release the pages.
        (release-physical-pages page (ceiling (max +4k-page-size+ sector-size) +4k-page-size+))))))

(defun descend-page-table (page-table index allocate)
  (if (not (page-present-p page-table index))
      (when allocate
        ;; No PT. Allocate one.
        (let* ((frame (pager-allocate-page :page-table))
               (addr (+ +physical-map-base+ (ash frame 12))))
          (zeroize-page addr)
          (setf (page-table-entry page-table index) (logior (ash frame 12)
                                                            +page-table-present+
                                                            +page-table-write+))
          addr))
      (+ +physical-map-base+ (logand (page-table-entry page-table index) +page-table-address-mask+))))

(defun get-pte-for-address (address &optional (allocate t))
  (let* ((cr3 (+ +physical-map-base+ (logand (sys.int::%cr3) (lognot #xFFF))))
         (pdp            (descend-page-table cr3  (address-l4-bits address) allocate))
         (pdir (and pdp  (descend-page-table pdp  (address-l3-bits address) allocate)))
         (pt   (and pdir (descend-page-table pdir (address-l2-bits address) allocate))))
    (and pt (+ pt (* 8 (address-l1-bits address))))))

(defun block-info-for-virtual-address-1 (address &optional allocate)
  "Return the address (access with (memref-ub64 X 0)) of the block map entry for ADDRESS.
If ALLOCATE is true, then allocate intermediate block map levels as required.
Returns NIL if the entry is missing and ALLOCATE is false."
  (flet ((get-level (map entry)
           (let ((info (sys.int::memref-signed-byte-64 map entry)))
             (cond ((not (zerop info))
                    info)
                   ((not allocate)
                    (return-from block-info-for-virtual-address-1 nil))
                   (t (let* ((frame (pager-allocate-page :other))
                             (new-level (+ +physical-map-base+ (* frame +4k-page-size+))))
                        (zeroize-page new-level)
                        (setf (sys.int::memref-signed-byte-64 map entry) new-level)
                        new-level))))))
    (let* ((bml4e (address-l4-bits address))
           (bml3e (address-l3-bits address))
           (bml2e (address-l2-bits address))
           (bml1e (address-l1-bits address))
           (bml3 (get-level *bml4* bml4e))
           (bml2 (get-level bml3 bml3e))
           (bml1 (get-level bml2 bml2e)))
      (+ bml1 (* bml1e 8)))))

(defun block-info-for-virtual-address (address)
  (let ((bme (block-info-for-virtual-address-1 address)))
    (cond ((or (null bme)
               (zerop (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+)
                           (sys.int::memref-unsigned-byte-64 bme 0))))
           nil)
          (t (sys.int::memref-unsigned-byte-64 bme 0)))))

(defun allocate-new-block-for-virtual-address (address flags)
  (let ((new-block (or (store-alloc 1)
                       (panic "Aiiee, out of store.")))
        (bme (block-info-for-virtual-address-1 address t)))
    ;; Update the block info for this address.
    (when (not (zerop (sys.int::memref-unsigned-byte-64 bme 0)))
      (panic "Block " address " entry not zero!"))
    (setf (sys.int::memref-unsigned-byte-64 bme 0)
          (logior (ash new-block sys.int::+block-map-id-shift+)
                  sys.int::+block-map-committed+
                  flags))))

(defun release-block-at-virtual-address (address)
  ;; Update the block info for this address.
  (let ((bme-addr (block-info-for-virtual-address-1 address)))
    (when (or (not bme-addr)
              (zerop (sys.int::memref-unsigned-byte-64 bme-addr 0)))
      (panic "Block " address " entry is zero or not allocated!"))
    (let* ((bme (sys.int::memref-unsigned-byte-64 bme-addr 0))
           (block-id (ash bme (- sys.int::+block-map-id-shift+))))
      (cond ((logtest sys.int::+block-map-committed+ bme)
             (store-free block-id 1))
            (t #+(or)(debug-print-line "Block " (sys.int::memref-unsigned-byte-64 bme-addr 0) " vaddr " address " is uncommitted.")
               (store-deferred-free block-id 1)
               (decf *store-fudge-factor*)))
      (setf (sys.int::memref-unsigned-byte-64 bme-addr 0) 0))))

(defun set-address-flags (address flags)
  ;; Update the block info for this address.
  ;; Preserve the state of the committed flag.
  (let ((bme (block-info-for-virtual-address-1 address)))
    (when (or (not bme)
              (zerop (sys.int::memref-unsigned-byte-64 bme 0)))
      (panic "Block " address " entry is zero or not allocated!"))
    (setf (sys.int::memref-unsigned-byte-64 bme 0)
          (logior (logand (sys.int::memref-unsigned-byte-64 bme 0)
                          (logior (lognot sys.int::+block-map-flag-mask+)
                                  sys.int::+block-map-committed+))
                  flags))))

(defun release-vm-page (frame)
  (case (physical-page-frame-type frame)
    (:active
     (remove-from-page-replacement-list frame)
     (release-physical-pages frame 1))
    (:active-writeback
     (setf (physical-page-frame-type frame) :inactive-writeback))
    (t (panic "Releasing page " frame " with bad type " (physical-page-frame-type frame)))))

(defun pager-rpc (fn &optional arg1 arg2 arg3)
  (without-footholds
    (let ((self (current-thread)))
      (setf (thread-pager-argument-1 self) arg1
            (thread-pager-argument-2 self) arg2
            (thread-pager-argument-3 self) arg3))
    (%call-on-wired-stack-without-interrupts
     (lambda (sp fp fn)
       (let ((self (current-thread)))
         (with-symbol-spinlock (*pager-lock*)
           (%lock-thread self)
           (setf (thread-state self) :pager-request
                 (thread-wait-item self) fn
                 (thread-%next self) *pager-waiting-threads*
                 *pager-waiting-threads* self)
           (with-thread-lock (sys.int::*pager-thread*)
             (when (and (eql (thread-state sys.int::*pager-thread*) :sleeping)
                        (eql (thread-wait-item sys.int::*pager-thread*) '*pager-waiting-threads*))
               (setf (thread-state sys.int::*pager-thread*) :runnable)
               (with-symbol-spinlock (*global-thread-lock*)
                 (push-run-queue sys.int::*pager-thread*)))))
         (%reschedule-via-wired-stack sp fp)))
     nil fn)
    (thread-pager-argument-1 (current-thread))))

(defun allocate-memory-range (base length flags)
  (assert (and (page-aligned-p base)
               (page-aligned-p length))
          (base length)
          "Range not page aligned.")
  (pager-rpc 'allocate-memory-range-in-pager base length flags))

(defun allocate-memory-range-in-pager (base length flags)
  (debug-print-line "Allocate range " base "-" (+ base length) "  " flags)
  (with-mutex (*vm-lock*)
    ;; Ensure there's enough fudged memory before allocating.
    (when (< (- *store-freelist-n-free-blocks* (truncate length +4k-page-size+)) *store-fudge-factor*)
      (debug-print-line "Not allocating " length " bytes, too few blocks remaining. "
                        *store-freelist-n-free-blocks* " " *store-fudge-factor*)
      (return-from allocate-memory-range-in-pager nil))
    (dotimes (i (truncate length #x1000))
      (allocate-new-block-for-virtual-address
       (+ base (* i #x1000))
       flags)))
  t)

(defun release-memory-range (base length)
  (assert (and (page-aligned-p base)
               (page-aligned-p length))
          (base length)
          "Range not page aligned.")
  (pager-rpc 'release-memory-range-in-pager base length))

(defun release-memory-range-in-pager (base length ignore3)
  (declare (ignore ignore3))
  (debug-print-line "Release range " base "-" (+ base length))
  (with-mutex (*vm-lock*)
    (dotimes (i (truncate length #x1000))
      ;; Update block map.
      (release-block-at-virtual-address (+ base (* i #x1000)))
      ;; Update page tables and release pages if possible.
      (let ((pte (get-pte-for-address (+ base (* i #x1000)) nil)))
        (when (and pte (page-present-p pte 0))
          (release-vm-page (ash (page-table-entry pte 0) -12))
          (setf (page-table-entry pte 0) 0))))
    (flush-tlb)))

(defun protect-memory-range (base length flags)
  (assert (and (page-aligned-p base)
               (page-aligned-p length))
          (base length)
          "Range not page aligned.")
  (assert (>= (+ base length) (* 2 1024 1024 1024)) () "Wired area can't be protected.")
  ;; Implementing this is a litle complicated. It'll need to keep dirty pages in memory.
  ;; Actual possible implementation strategy: Commit if uncommitted, then back write to disk.
  (assert (or (logtest sys.int::+block-map-present+ flags)
              (logtest sys.int::+block-map-zero-fill+ flags))
          () "TODO: Cannot mark block not-present without zero-fill")
  (pager-rpc 'protect-memory-range-in-pager base length flags))

(defun protect-memory-range-in-pager (base length flags)
  (debug-print-line "Protect range " base "-" (+ base length) "  " flags)
  (with-mutex (*vm-lock*)
    (dotimes (i (truncate length #x1000))
      ;; Update block map.
      (set-address-flags (+ base (* i #x1000)) flags)
      ;; Update page tables and release pages if possible.
      (let ((pte (get-pte-for-address (+ base (* i #x1000)) nil)))
        (when (and pte (page-present-p pte 0))
          (cond ((or (not (logtest sys.int::+block-map-present+ flags))
                     (logtest sys.int::+block-map-zero-fill+ flags))
                 ;; Page going away, but it's ok. It'll be back, zero-filled.
                 #+(or)(debug-print-line "  flush page " (+ base (* i #x1000)) "  " (page-table-entry pte 0))
                 (release-vm-page (ash (page-table-entry pte 0) -12))
                 (setf (page-table-entry pte 0) 0))
                ((logtest sys.int::+block-map-writable+ flags)
                 ;; Mark writable.
                 (setf (page-table-entry pte 0) (logior (page-table-entry pte 0)
                                                        +page-table-write+)))
                (t
                 ;; Mark read-only.
                 (setf (page-table-entry pte 0) (logand (page-table-entry pte 0)
                                                        (lognot +page-table-write+))))))))
    (flush-tlb)))

(defun pager-allocate-page (&optional (new-type :active))
  (let ((frame (allocate-physical-pages 1 :type new-type)))
    (when (not frame)
      ;; TODO: Purge empty page table levels.
      #+(or)(debug-print-line "Pager out of memory, preparing to SWAP!")
      (when (not *page-replacement-list-head*)
        (panic "No pages available for page-out?"))
      (let* ((candidate *page-replacement-list-head*)
             (candidate-virtual (physical-page-virtual-address candidate))
             (pte-addr (get-pte-for-address candidate-virtual nil))
             (pte (sys.int::memref-unsigned-byte-64 pte-addr 0))
             (bme-addr (block-info-for-virtual-address-1 candidate-virtual nil))
             (bme (sys.int::memref-unsigned-byte-64 bme-addr 0)))
        (ensure (eql (physical-page-frame-type candidate) :active)
                "Page-out candidate has type " (physical-page-frame-type candidate) ", wanted :ACTIVE.")
        #+(or)(debug-print-line "Candidate " candidate ":" candidate-virtual
                          "  pte " pte
                          "  type " (physical-page-frame-type candidate)
                          "  block " (physical-page-frame-block-id candidate)
                          "  bme " bme)
        ;; Remove this page from the VM, but do not free it just yet.
        (remove-from-page-replacement-list candidate)
        (setf (sys.int::memref-unsigned-byte-64 pte-addr 0) 0)
        (sys.int::%invlpg candidate-virtual)
        ;; Maybe write it back to disk.
        (when (logtest pte +page-table-dirty+)
          (when (not (logtest bme sys.int::+block-map-committed+))
            #+(or)(debug-print-line "Candidate is dirty but part of on-disk snapshot, allocating new block.")
            (let ((new-block (or (store-alloc 1)
                                 (panic "Aiiee, out of store during swap-out.")))
                  (old-block (ash bme (- sys.int::+block-map-id-shift+))))
              (setf bme (logior (ash new-block sys.int::+block-map-id-shift+)
                                sys.int::+block-map-committed+
                                (logand bme #xFF))
                    (sys.int::memref-unsigned-byte-64 bme-addr 0) bme)
              #+(or)(debug-print-line "Replace old block " old-block " with " new-block " vaddr " candidate-virtual)
              (decf *store-fudge-factor*)
              (store-deferred-free old-block 1)
              #+(or)(debug-print-line "Old block: " old-block "  new-block: " new-block)))
          (let ((block-id (ash bme (- sys.int::+block-map-id-shift+))))
            #+(or)(debug-print-line "Candidate is dirty, writing back to block " block-id)
            (disk-submit-request *pager-disk-request*
                                 *paging-disk*
                                 :write
                                 (* block-id
                                    (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                                 (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                                 (+ +physical-map-base+ (ash candidate 12)))
            (unless (disk-await-request *pager-disk-request*)
              (panic "Unable to swap page to disk."))))
        ;; Now it can be reused.
        (setf frame candidate)
        (setf (physical-page-frame-type frame) new-type)))
    frame))

;;; This list is also modified by the page fault handler.
;;; No locking is required for this at the moment, as neither the pager nor the snapshotter
;;; ever cause page faults. Extra locking might be required in the future (SMP, preemption, etc).
(defun append-to-page-replacement-list (frame)
  (cond (*page-replacement-list-head*
         (setf (physical-page-frame-next *page-replacement-list-tail*) frame
               (physical-page-frame-prev frame) *page-replacement-list-tail*
               (physical-page-frame-next frame) nil
               *page-replacement-list-tail* frame))
        (t
         (setf *page-replacement-list-head* frame
               *page-replacement-list-tail* frame
               (physical-page-frame-next frame) nil
               (physical-page-frame-prev frame) nil))))

(defun remove-from-page-replacement-list (frame)
  (when (eql *page-replacement-list-head* frame)
    (setf *page-replacement-list-head* (physical-page-frame-next frame)))
  (when (physical-page-frame-next frame)
    (setf (physical-page-frame-prev (physical-page-frame-next frame)) (physical-page-frame-prev frame)))
  (when (eql *page-replacement-list-tail* frame)
    (setf *page-replacement-list-tail* (physical-page-frame-prev frame)))
  (when (physical-page-frame-prev frame)
    (setf (physical-page-frame-next (physical-page-frame-prev frame)) (physical-page-frame-next frame))))

(defun wait-for-page (address)
  (with-mutex (*vm-lock*)
    (let ((pte (get-pte-for-address address))
          (block-info (block-info-for-virtual-address address)))
      #+(or)(debug-print-line "WFP " address " block " block-info)
      ;; Examine the page table, if there's a present entry then the page
      ;; was mapped while acquiring the VM lock. Just return.
      (when (page-present-p pte 0)
        (when (logtest (sys.int::memref-unsigned-byte-64 pte 0) +page-table-copy-on-write+)
          (debug-print-line "Copying page " address " in WFP.")
          (snapshot-clone-cow-page (pager-allocate-page) address))
        #+(or)(debug-print-line "WFP " address " block " block-info " already mapped " (page-table-entry pte 0))
        (return-from wait-for-page t))
      ;; Note that this test is done after the pte test. this is a hack to make
      ;; runtime-allocated stacks work before allocate-stack actually modifies the
      ;; block map.
      (when (or (not block-info)
                (not (logtest sys.int::+block-map-present+ block-info)))
        #+(or)(debug-print-line "WFP " address " not present")
        (return-from wait-for-page nil))
      ;; No page allocated. Allocate a page and read the data.
      (let* ((frame (pager-allocate-page))
             (addr (+ +physical-map-base+ (ash frame 12)))
             (is-zero-page nil))
        (setf (physical-page-frame-block-id frame) (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) block-info)
              (physical-page-virtual-address frame) (logand address (lognot (1- +4k-page-size+))))
        (unless *page-replacement-list-head*
          (debug-print-line "addr " address " vaddr " (physical-page-virtual-address frame) " frame " frame " pteA " (get-pte-for-address address #+(or)(physical-page-virtual-address frame) nil) " pteB " (get-pte-for-address (physical-page-virtual-address frame) nil)))
        (append-to-page-replacement-list frame)
        (cond ((logtest sys.int::+block-map-zero-fill+ block-info)
               ;; Block is zero-filled.
               (setf is-zero-page t)
               (zeroize-page addr)
               ;; Clear the zero-fill flag.
               (set-address-flags address (logand block-info
                                                  sys.int::+block-map-flag-mask+
                                                  (lognot sys.int::+block-map-zero-fill+))))
              (t ;; Block must be read from disk.
               (disk-submit-request *pager-disk-request*
                                    *paging-disk*
                                    :read
                                    (* (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) block-info)
                                       (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                                    (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                                    addr)
               (unless (disk-await-request *pager-disk-request*)
                 (panic "Unable to read page from disk"))))
        (setf (page-table-entry pte 0) (logior (ash frame 12)
                                               +page-table-present+
                                               (if (logtest sys.int::+block-map-writable+ block-info)
                                                   +page-table-write+
                                                 0)))
        (sys.int::%invlpg address)
        (when is-zero-page
          ;; Touch the page to make sure the snapshotter & swap code know to swap it out.
          ;; The zero fill flag in the block map was cleared, but the on-disk data doesn't reflect that.
          ;; This sets the dirty bits in the page tables properly.
          (setf (sys.int::memref-unsigned-byte-8 address 0) 0))
        #+(or)(debug-print-line "WFP " address " block " block-info " mapped to " (page-table-entry pte 0)))))
  t)

(defun wait-for-page-fast-path (fault-address)
  (with-mutex (*vm-lock* nil)
    (let ((pte (get-pte-for-address fault-address nil))
          (block-info (block-info-for-virtual-address fault-address)))
      (when (and pte
                 (not (page-present-p pte 0))
                 block-info
                 (logtest sys.int::+block-map-present+ block-info)
                 (logtest sys.int::+block-map-zero-fill+ block-info))
        ;; Mapping a zero page into an existing PTE.
        (let ((frame (allocate-physical-pages 1 :type :active)))
          (when frame
            ;(debug-print-line "Zero page fast path for " fault-address)
            (let ((page-addr (+ +physical-map-base+ (ash frame 12))))
              (setf (physical-page-frame-block-id frame) (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) block-info)
                    (physical-page-virtual-address frame) (logand fault-address (lognot (1- +4k-page-size+))))
              (append-to-page-replacement-list frame)
              (zeroize-page page-addr)
              ;; Clear the zero-fill flag.
              (set-address-flags fault-address (logand block-info
                                                       sys.int::+block-map-flag-mask+
                                                       (lognot sys.int::+block-map-zero-fill+)))
              (setf (page-table-entry pte 0) (logior (ash frame 12)
                                                     +page-table-present+
                                                     (if (logtest sys.int::+block-map-writable+ block-info)
                                                         +page-table-write+
                                                         0)))
              (sys.int::%invlpg fault-address)
              ;; Touch the page to make sure the snapshotter & swap code know to swap it out.
              ;; The zero fill flag in the block map was cleared, but the on-disk data doesn't reflect that.
              ;; This sets the dirty bits in the page tables properly.
              (setf (sys.int::memref-unsigned-byte-8 fault-address 0) 0))
            t))))))

(defun wait-for-page-via-interrupt (interrupt-frame address)
  "Called by the page fault handler when a non-present page is accessed.
It will put the thread to sleep, while it waits for the page."
  (let ((self (current-thread)))
    (when (eql self sys.int::*pager-thread*)
      (panic "Page fault in pager!"))
    (when (and *pager-fast-path-enabled*
               (wait-for-page-fast-path address))
      (incf *pager-fast-path-hits*)
      (return-from wait-for-page-via-interrupt))
    (incf *pager-fast-path-misses*)
    (with-symbol-spinlock (*pager-lock*)
      (%lock-thread self)
      (setf (thread-state self) :waiting-for-page
            (thread-wait-item self) address
            (thread-%next self) *pager-waiting-threads*
            *pager-waiting-threads* self)
      (with-thread-lock (sys.int::*pager-thread*)
        (when (and (eql (thread-state sys.int::*pager-thread*) :sleeping)
                   (eql (thread-wait-item sys.int::*pager-thread*) '*pager-waiting-threads*))
          (setf (thread-state sys.int::*pager-thread*) :runnable)
          (with-symbol-spinlock (*global-thread-lock*)
            (push-run-queue sys.int::*pager-thread*)))))
    (%reschedule-via-interrupt interrupt-frame)))

(defun map-physical-memory (base size name)
  ;; Page alignment required.
  (assert (page-aligned-p base))
  (assert (page-aligned-p size))
  (with-mutex (*vm-lock*)
    (dotimes (i (truncate size #x1000))
      (let ((pte (get-pte-for-address (+ +physical-map-base+ base (* i #x1000)))))
        (when (not (page-present-p pte 0))
          (setf (page-table-entry pte 0) (logior (+ base (* i #x1000))
                                                 +page-table-present+
                                                 +page-table-write+)))))))

(defun initialize-pager ()
  (when (not (boundp '*pager-waiting-threads*))
    (setf *pager-waiting-threads* '()
          *pager-current-thread* nil
          *pager-lock* (place-spinlock-initializer)
          *pager-fast-path-enabled* t))
  (setf *page-replacement-list-head* nil
        *page-replacement-list-tail* nil)
  (setf *pager-fast-path-hits* 0
        *pager-fast-path-misses* 0)
  (when *pager-current-thread*
    ;; Push any current thread back on the waiting threads list.
    (setf (thread-%next *pager-current-thread*) *pager-waiting-threads*
          *pager-waiting-threads* *pager-current-thread*
          *pager-current-thread* nil))
  (setf *pager-disk-request* (make-disk-request))
  ;; Don't let the pager run until the paging disk has been found.
  (setf (thread-state sys.int::*pager-thread*) :sleeping
        (thread-wait-item sys.int::*pager-thread*) "Waiting for paging disk")
  ;; The VM lock is recreated each boot because it is only held by
  ;; the ephemeral pager and snapshot threads.
  ;; Big fat lie!!! Anything that calls PROTECT-MEMORY-RANGE/RELEASE-MEMORY-RANGE/etc holds this :|
  ;; Less of a big fat lie now. Only callers of MAP-PHYSICAL-MEMORY are a problem.
  (setf *vm-lock* (make-mutex "Global VM Lock")))

;;; When true, the system will panic if a thread touches a truely unmapped page.
;;; When false, the system will continue on and leave the thread in limbo.
(defvar *panic-on-unhandled-paging-requests* t)

(defun handle-pager-request ()
  (setf (thread-pager-argument-1 *pager-current-thread*)
        (funcall (thread-wait-item *pager-current-thread*)
                 (thread-pager-argument-1 *pager-current-thread*)
                 (thread-pager-argument-2 *pager-current-thread*)
                 (thread-pager-argument-3 *pager-current-thread*)))
  (wake-thread *pager-current-thread*))

(defun pager-thread ()
  (loop
     ;; Select a thread.
     (without-interrupts
       (with-symbol-spinlock (*pager-lock*)
         (loop
            (when *pager-waiting-threads*
              (setf *pager-current-thread* *pager-waiting-threads*
                    *pager-waiting-threads* (thread-%next *pager-current-thread*))
              (set-paging-light t)
              (return))
            (set-paging-light nil)
            ;; Manually sleep, don't use condition variables or similar within ephemeral threads.
            (%lock-thread sys.int::*pager-thread*)
            (release-place-spinlock *pager-lock*)
            (setf (thread-state sys.int::*pager-thread*) :sleeping
                  (thread-wait-item sys.int::*pager-thread*) '*pager-waiting-threads*)
            (%reschedule)
            (sys.int::%cli)
            (acquire-place-spinlock *pager-lock*))))
     (cond ((eql (thread-state *pager-current-thread*) :pager-request)
            (handle-pager-request))
           ;; Page it in
           ((wait-for-page (thread-wait-item *pager-current-thread*))
            ;; Release the thread.
            (wake-thread *pager-current-thread*))
           ;; TODO: Shouldn't panic at all, this should be dispatched to a debugger thread.
           (*panic-on-unhandled-paging-requests*
            ;; This strange contortion is here to get a dynamic-extent list that can be passed
            ;; to PANIC-1. Need to implement DX list allocation in the compiler.
            ((lambda (&rest stuff)
               (declare (dynamic-extent stuff))
               (panic-1 stuff (lambda ()
                                (panic-print-backtrace (thread-frame-pointer *pager-current-thread*))
                                (debug-print-line "-------"))))
             "page fault on unmapped page " (thread-wait-item *pager-current-thread*) " in thread " *pager-current-thread*))
           (t
            (debug-print-line "Thread " *pager-current-thread* " faulted on address " (thread-wait-item *pager-current-thread*))
            (panic-print-backtrace (thread-frame-pointer *pager-current-thread*))))
     (setf *pager-current-thread* nil)))
