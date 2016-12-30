;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(sys.int::defglobal *vm-lock*)

(sys.int::defglobal *pager-noisy*)

(sys.int::defglobal *pager-fast-path-enabled*)

(sys.int::defglobal *pager-fast-path-hits*)
(sys.int::defglobal *pager-fast-path-misses*)

(sys.int::defglobal *pager-waiting-threads*)
(sys.int::defglobal *pager-current-thread*)
(sys.int::defglobal *pager-lock*)
(sys.int::defglobal *pager-disk-request*)

(sys.int::defglobal *paging-disk*)
(sys.int::defglobal *paging-read-only*)
(sys.int::defglobal *bml4*)

(sys.int::defglobal *page-replacement-list-head*)
(sys.int::defglobal *page-replacement-list-tail*)

(sys.int::defglobal *pager-lazy-block-allocation-enabled*)

(sys.int::defglobal *store-fudge-factor*)

(defun pager-log (&rest things)
  (declare (dynamic-extent things))
  (when *pager-noisy*
    (debug-print-line-1 things)))

(declaim (inline page-table-entry (setf page-table-entry)))
(defun page-table-entry (page-table &optional (index 0))
  (sys.int::memref-unsigned-byte-64 page-table index))
(defun (setf page-table-entry) (value page-table &optional (index 0))
  (setf (sys.int::memref-unsigned-byte-64 page-table index) value))

(declaim (inline zeroize-page zeroize-physical-page))
(defun zeroize-page (addr)
  (sys.int::%fill-words addr 0 512))

(defun zeroize-physical-page (physical-addr)
  (sys.int::%fill-words (convert-to-pmap-address physical-addr) 0 512))

(declaim (inline block-info-present-p))
(defun block-info-present-p (block-info)
  (logtest sys.int::+block-map-present+ block-info))

(declaim (inline block-info-zero-fill-p))
(defun block-info-zero-fill-p (block-info)
  (logtest sys.int::+block-map-zero-fill+ block-info))

(declaim (inline block-info-writable-p))
(defun block-info-writable-p (block-info)
  (logtest sys.int::+block-map-writable+ block-info))

(declaim (inline block-info-committed-p))
(defun block-info-committed-p (block-info)
  (logtest sys.int::+block-map-committed+ block-info))

(declaim (inline block-info-block-id))
(defun block-info-block-id (block-info)
  (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+)
       block-info))

(declaim (inline block-info-lazy-block-p))
(defun block-info-lazy-block-p (block-info)
  (eql (block-info-block-id block-info) sys.int::+block-map-id-lazy+))

(defun page-aligned-p (value)
  (zerop (logand value #xFFF)))

(defun allocate-page (&optional mandatory)
  (let ((frame (allocate-physical-pages 1 :mandatory-p mandatory)))
    (when frame
      (convert-to-pmap-address (* frame +4k-page-size+)))))

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

(defconstant +image-header-block-map+ 96)
(defconstant +image-header-freelist+ 104)

(defun initialize-paging-system ()
  (cond ((boot-option +boot-option-freestanding+)
         (initialize-freestanding-paging-system))
        (t
         (detect-paging-disk)
         (when (not *paging-disk*)
           (panic "Could not find boot device. Sorry.")))))

(defun initialize-freestanding-paging-system ()
  (setf *paging-disk* :freestanding
        *paging-read-only* t)
  (debug-print-line "Running freestanding.")
  (setf *bml4* (sys.int::memref-signed-byte-64 (+ *boot-information-page* +boot-information-block-map+)))
  (debug-print-line "BML4 at " *bml4*)
  (initialize-freestanding-store)
  (setf *store-fudge-factor* (- (truncate (physical-memory-statistics) 1.1)))
  (debug-print-line "Set fudge factor to " *store-fudge-factor*)
  (debug-print-line "Waking pager thread.")
  (setf (thread-state sys.int::*pager-thread*) :runnable)
  (push-run-queue sys.int::*pager-thread*))

(defun initialize-hosted-paging-system (disk header)
  (setf *paging-disk* disk)
  (setf *paging-read-only* (or (not (disk-writable-p disk))
                               (boot-option +boot-option-force-read-only+)))
  (when *paging-read-only*
    (debug-print-line "Running read-only."))
  (setf *bml4* (sys.int::memref-signed-byte-64 (+ *boot-information-page* +boot-information-block-map+)))
  (debug-print-line "BML4 at " *bml4*)
  (initialize-store-freelist (truncate (* (disk-n-sectors *paging-disk*) (disk-sector-size *paging-disk*)) #x1000)
                             (sys.int::memref-unsigned-byte-64 (+ header +image-header-freelist+)))
  (multiple-value-bind (free-blocks total-blocks)
      (store-statistics)
    ;; This is a rough approximation of the total image size.
    (let ((allocated-blocks (- total-blocks free-blocks)))
      ;; When running read-only, we can only use main memory (minus the total used by the image).
      (cond (*paging-read-only*
             (setf *store-fudge-factor* (- (+ (truncate (physical-memory-statistics) 1.1)
                                              allocated-blocks))))
            (t
             (setf *store-fudge-factor* (+ allocated-blocks 256))))))
  (debug-print-line "Set fudge factor to " *store-fudge-factor*)
  (debug-print-line "Waking pager thread.")
  (setf (thread-state sys.int::*pager-thread*) :runnable)
  (push-run-queue sys.int::*pager-thread*))

(defun detect-paging-disk ()
  (debug-print-line "Looking for paging disk with UUID "
                    (boot-uuid 0) ":" (boot-uuid 1) ":"
                    (boot-uuid 2) ":" (boot-uuid 3) ":"
                    (boot-uuid 4) ":" (boot-uuid 5) ":"
                    (boot-uuid 6) ":" (boot-uuid 7) ":"
                    (boot-uuid 8) ":" (boot-uuid 9) ":"
                    (boot-uuid 10) ":" (boot-uuid 11) ":"
                    (boot-uuid 12) ":" (boot-uuid 13) ":"
                    (boot-uuid 14) ":" (boot-uuid 15))
  (dolist (disk (all-disks))
    (let* ((sector-size (disk-sector-size disk))
           (page (allocate-physical-pages (ceiling (max +4k-page-size+ sector-size) +4k-page-size+)
                                          :mandatory-p "DETECT-PAGING-DISK disk buffer"))
           (page-addr (convert-to-pmap-address (* page +4k-page-size+))))
      ;; Read first 4k, figure out what to do with it.
      (when (not (disk-read disk 0 (ceiling +4k-page-size+ sector-size) page-addr))
        (panic "Unable to read first block on disk " disk))
      ;; Search for a Mezzano header here.
      (unwind-protect
           (flet ((check-magic ()
                    (loop
                       for byte in '(#x00 #x4D #x65 #x7A #x7A #x61 #x6E #x69 #x6E #x65 #x49 #x6D #x61 #x67 #x65 #x00)
                       for offset from 0
                       do (when (not (eql (sys.int::memref-unsigned-byte-8 page-addr offset) byte))
                            (return nil))
                       finally (return t)))
                  (check-uuid ()
                    (loop
                       for offset from 0 below 16
                       do (when (not (eql (sys.int::memref-unsigned-byte-8 page-addr (+ 16 offset))
                                          (boot-uuid offset)))
                            (return nil))
                       finally (return t))))
             (declare (dynamic-extent #'check-magic #'check-uuid))
             (when (check-magic)
               (debug-print-line "Found image with UUID "
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 0)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 1)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 2)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 3)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 4)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 5)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 6)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 7)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 8)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 9)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 10)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 11)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 12)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 13)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 14)) ":"
                                 (sys.int::memref-unsigned-byte-8 page-addr (+ 16 15))
                                 " on disk " disk)
               (when (check-uuid)
                 (debug-print-line "Found boot image on disk " disk "!")
                 (initialize-hosted-paging-system disk page-addr)
                 (return))))
        ;; Release the pages.
        (release-physical-pages page (ceiling (max +4k-page-size+ sector-size) +4k-page-size+))))))

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
                             (new-level (convert-to-pmap-address (* frame +4k-page-size+))))
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
               (zerop (block-info-block-id (sys.int::memref-unsigned-byte-64 bme))))
           nil)
          (t (sys.int::memref-unsigned-byte-64 bme)))))

(defun allocate-new-block-for-virtual-address (address flags)
  (let ((new-block (if *pager-lazy-block-allocation-enabled*
                       sys.int::+block-map-id-lazy+
                       (or (store-alloc 1)
                           (panic "Unable to allocate new block!"))))
        (bme (block-info-for-virtual-address-1 address t)))
    (when *pager-lazy-block-allocation-enabled*
      (incf *store-fudge-factor*))
    ;; Update the block info for this address.
    (when (not (zerop (sys.int::memref-unsigned-byte-64 bme)))
      (panic "Block " address " entry not zero!"))
    (setf (sys.int::memref-unsigned-byte-64 bme)
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
           (block-id (block-info-block-id bme)))
      (cond ((block-info-committed-p bme)
             (cond ((not (eql block-id sys.int::+block-map-id-lazy+))
                    (store-free block-id 1))
                   (t
                    (decf *store-fudge-factor*))))
            (t
             #+(or)(debug-print-line "Block " (sys.int::memref-unsigned-byte-64 bme-addr 0) " vaddr " address " is uncommitted.")
             (ensure (not (eql block-id sys.int::+block-map-id-lazy+))
                     "Tried to release non-committed deferred block.")
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
    (%run-on-wired-stack-without-interrupts (sp fp fn)
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
    (thread-pager-argument-1 (current-thread))))

(defun allocate-memory-range (base length flags)
  (assert (and (page-aligned-p base)
               (page-aligned-p length))
          (base length)
          "Range not page aligned.")
  (pager-rpc 'allocate-memory-range-in-pager base length flags))

(defun map-new-wired-page (address)
  (let ((pte (get-pte-for-address address))
        (block-info (block-info-for-virtual-address address)))
    ;;(debug-print-line "MNWP " address " block " block-info)
    (when (page-present-p pte 0)
      (panic "Mapping new wired page at " address " but page already mapped!"))
    (when (or (not block-info)
              (not (block-info-present-p block-info)))
      (panic "Mapping new wired page at " address " but address not present!"))
    (when (not (block-info-zero-fill-p block-info))
      (panic "Not implemented! Mapping new wired page at " address " but not zero!"))
    ;; No page allocated. Allocate a page and read the data.
    (let* ((frame (pager-allocate-page :wired))
           (addr (convert-to-pmap-address (ash frame 12))))
      (setf (physical-page-frame-block-id frame) (block-info-block-id block-info)
            (physical-page-virtual-address frame) (logand address (lognot (1- +4k-page-size+))))
      ;; Block is zero-filled.
      (zeroize-page addr)
      ;; Clear the zero-fill flag.
      (set-address-flags address (logand block-info
                                         sys.int::+block-map-flag-mask+
                                         (lognot sys.int::+block-map-zero-fill+)))
      (setf (page-table-entry pte 0) (make-pte frame
                                               :writable (block-info-writable-p block-info)))
      ;; Don't need to dirty the page like in W-F-P, the snapshotter takes all wired pages.
      (flush-tlb-single address))))

(defun allocate-memory-range-in-pager (base length flags)
  (pager-log "Allocate range " base "-" (+ base length) "  " flags)
  (when (< base #x80000000)
    ;; This will require allocating new wired backing frames as well.
    (debug-print-line "TODO: Allocate pages in the wired area.")
    (return-from allocate-memory-range-in-pager nil))
  (when (logtest flags sys.int::+block-map-wired+)
    (ensure (or (< base #x80000000) ; wired area
                (and (<= #x200000000000 base) ; wired stack area.
                     (< base #x208000000000)))))
  (with-mutex (*vm-lock*)
    ;; Ensure there's enough fudged memory before allocating.
    (when (< (- *store-freelist-n-free-blocks* (truncate length +4k-page-size+)) *store-fudge-factor*)
      (pager-log "Not allocating " length " bytes, too few blocks remaining. "
                 *store-freelist-n-free-blocks* " " *store-fudge-factor*)
      (return-from allocate-memory-range-in-pager nil))
    (dotimes (i (truncate length #x1000))
      (allocate-new-block-for-virtual-address
       (+ base (* i #x1000))
       flags))
    (when (and (<= #x200000000000 base) ; wired stack area.
               (< base #x208000000000))
      (ensure (logtest flags sys.int::+block-map-wired+))
      ;; Pages in the wired stack area don't require backing frames.
      (dotimes (i (truncate length #x1000))
        (map-new-wired-page (+ base (* i #x1000))))))
  t)

(defun release-memory-range (base length)
  (assert (and (page-aligned-p base)
               (page-aligned-p length))
          (base length)
          "Range not page aligned.")
  (pager-rpc 'release-memory-range-in-pager base length))

(defun release-memory-range-in-pager (base length ignore3)
  (declare (ignore ignore3))
  (pager-log "Release range " base "-" (+ base length))
  (with-mutex (*vm-lock*)
    (dotimes (i (truncate length #x1000))
      ;; Update block map.
      (release-block-at-virtual-address (+ base (* i #x1000)))
      ;; Update page tables and release pages if possible.
      (let ((pte (get-pte-for-address (+ base (* i #x1000)) nil)))
        (when (and pte (page-present-p pte 0))
          (release-vm-page (ash (pte-physical-address (page-table-entry pte 0)) -12))
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
  (assert (or (block-info-present-p flags)
              (block-info-zero-fill-p flags))
          () "TODO: Cannot mark block not-present without zero-fill")
  (pager-rpc 'protect-memory-range-in-pager base length flags))

(defun protect-memory-range-in-pager (base length flags)
  (pager-log "Protect range " base "-" (+ base length) "  " flags)
  (with-mutex (*vm-lock*)
    (dotimes (i (truncate length #x1000))
      ;; Update block map.
      (set-address-flags (+ base (* i #x1000)) flags)
      ;; Update page tables and release pages if possible.
      (let ((pte (get-pte-for-address (+ base (* i #x1000)) nil)))
        (when (and pte (page-present-p pte 0))
          (cond ((or (not (block-info-present-p flags))
                     (block-info-zero-fill-p flags))
                 ;; Page going away, but it's ok. It'll be back, zero-filled.
                 #+(or)(debug-print-line "  flush page " (+ base (* i #x1000)) "  " (page-table-entry pte 0))
                 (release-vm-page (ash (pte-physical-address (page-table-entry pte 0)) -12))
                 (setf (page-table-entry pte 0) 0))
                ((block-info-writable-p flags)
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
      (when *paging-read-only*
        (panic "Out of memory when running read-only."))
      (when (not *page-replacement-list-head*)
        (panic "No pages available for page-out?"))
      (let* ((candidate *page-replacement-list-head*)
             (candidate-virtual (physical-page-virtual-address candidate))
             (pte-addr (get-pte-for-address candidate-virtual nil))
             (dirty-p (page-dirty-p pte-addr))
             (bme-addr (block-info-for-virtual-address-1 candidate-virtual nil))
             (bme (sys.int::memref-unsigned-byte-64 bme-addr 0)))
        (ensure (eql (physical-page-frame-type candidate) :active)
                "Page-out candidate has type " (physical-page-frame-type candidate) ", wanted :ACTIVE.")
        #+(or)(debug-print-line "Candidate " candidate ":" candidate-virtual
                          "  dirty-p " dirty-p
                          "  type " (physical-page-frame-type candidate)
                          "  block " (physical-page-frame-block-id candidate)
                          "  bme " bme)
        ;; Remove this page from the VM, but do not free it just yet.
        (remove-from-page-replacement-list candidate)
        (setf (page-table-entry pte-addr) (make-pte 0 :present nil))
        (flush-tlb-single candidate-virtual)
        ;; Maybe write it back to disk.
        (when dirty-p
          (when (not (block-info-committed-p bme))
            #+(or)(debug-print-line "Candidate is dirty but part of on-disk snapshot, allocating new block.")
            (let ((new-block (or (store-alloc 1)
                                 (panic "Aiiee, out of store during swap-out.")))
                  (old-block (block-info-block-id bme)))
              (ensure (not (eql old-block sys.int::+block-map-id-lazy+))
                      "Uncommitted lazy block")
              (setf bme (logior (ash new-block sys.int::+block-map-id-shift+)
                                sys.int::+block-map-committed+
                                (logand bme sys.int::+block-map-flag-mask+))
                    (sys.int::memref-unsigned-byte-64 bme-addr 0) bme)
              #+(or)(debug-print-line "Replace old block " old-block " with " new-block " vaddr " candidate-virtual)
              (decf *store-fudge-factor*)
              (store-deferred-free old-block 1)
              #+(or)(debug-print-line "Old block: " old-block "  new-block: " new-block)))
          (when (block-info-lazy-block-p bme)
            (let ((new-block (or (store-alloc 1)
                                 (panic "Unable to allocate lazy block!"))))
              (setf bme (logior (ash new-block sys.int::+block-map-id-shift+)
                                (logand bme sys.int::+block-map-flag-mask+))
                    (sys.int::memref-unsigned-byte-64 bme-addr 0) bme)))
          (let ((block-id (block-info-block-id bme)))
            #+(or)(debug-print-line "Candidate is dirty, writing back to block " block-id)
            (disk-submit-request *pager-disk-request*
                                 *paging-disk*
                                 :write
                                 (* block-id
                                    (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                                 (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                                 (convert-to-pmap-address (ash candidate 12)))
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
      (when (page-present-p pte)
        (when (page-copy-on-write-p pte)
          (pager-log "Copying page " address " in WFP.")
          (snapshot-clone-cow-page (pager-allocate-page) address))
        #+(or)(debug-print-line "WFP " address " block " block-info " already mapped " (page-table-entry pte 0))
        (return-from wait-for-page t))
      ;; Note that this test is done after the pte test. this is a hack to make
      ;; runtime-allocated stacks work before allocate-stack actually modifies the
      ;; block map.
      ;; ### must investigate if allocate-stack still needs this behaviour & why.
      (when (or (not block-info)
                (not (block-info-present-p block-info)))
        #+(or)(debug-print-line "WFP " address " not present")
        (return-from wait-for-page nil))
      ;; No page allocated. Allocate a page and read the data.
      (let* ((frame (pager-allocate-page))
             (addr (convert-to-pmap-address (ash frame 12)))
             (is-zero-page nil))
        (setf (physical-page-frame-block-id frame) (block-info-block-id block-info)
              (physical-page-virtual-address frame) (logand address (lognot (1- +4k-page-size+))))
        (when t;(not *page-replacement-list-head*)
          (pager-log "addr " address " vaddr " (physical-page-virtual-address frame) " frame " frame " pteA " (get-pte-for-address address #+(or)(physical-page-virtual-address frame) nil) " pteB " (get-pte-for-address (physical-page-virtual-address frame) nil)))
        (append-to-page-replacement-list frame)
        (cond ((block-info-zero-fill-p block-info)
               ;; Block is zero-filled.
               (setf is-zero-page t)
               (zeroize-page addr)
               ;; Clear the zero-fill flag.
               (set-address-flags address (logand block-info
                                                  sys.int::+block-map-flag-mask+
                                                  (lognot sys.int::+block-map-zero-fill+))))
              (t ;; Block must be read from disk.
               (when (eql *paging-disk* :freestanding)
                 (panic "Unable to satisfy paging request for page " address " while running freestanding"))
               (disk-submit-request *pager-disk-request*
                                    *paging-disk*
                                    :read
                                    (* (block-info-block-id block-info)
                                       (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                                    (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                                    addr)
               (unless (disk-await-request *pager-disk-request*)
                 (panic "Unable to read page from disk"))))
        (setf (page-table-entry pte) (make-pte frame
                                               :writable (block-info-writable-p block-info)))
        (flush-tlb-single address)
        (when is-zero-page
          ;; Touch the page to make sure the snapshotter & swap code know to swap it out.
          ;; The zero fill flag in the block map was cleared, but the on-disk data doesn't reflect that.
          ;; This sets the dirty bits in the page tables properly.
          (setf (sys.int::memref-unsigned-byte-8 address 0) 0))
        #+(or)
        (debug-print-line "WFP " address " block " block-info " mapped to " (page-table-entry pte 0)))))
  t)

;; Fast path, called from the page-fault handler.
;; If the *VM-LOCK* is not taken,
;; and the page tables for the address have already been allocated,
;; and the page for the address is a zero-page,
;; and there is a free page frame
;; then immediately allocate, zero, and map a frame, then return true.
;; Otherwise return false to punt back to w-f-p-via-interrupt.
;; Blocking is not permitted during a page-fault, which is why
;; this function can't block on the lock, or wait for a frame to be
;; swapped out, or wait for the new data to be swapped in.
(defun wait-for-page-fast-path (fault-address)
  (with-mutex (*vm-lock* nil)
    (let ((pte (get-pte-for-address fault-address nil))
          (block-info (block-info-for-virtual-address fault-address)))
      (when (and pte
                 (not (page-present-p pte 0))
                 block-info
                 (block-info-present-p block-info)
                 (block-info-zero-fill-p block-info))
        ;; Mapping a zero page into an existing PTE.
        (let ((frame (allocate-physical-pages 1 :type :active)))
          (when frame
            ;(debug-print-line "Zero page fast path for " fault-address)
            (let ((page-addr (convert-to-pmap-address (ash frame 12))))
              (setf (physical-page-frame-block-id frame) (block-info-block-id block-info)
                    (physical-page-virtual-address frame) (logand fault-address (lognot (1- +4k-page-size+))))
              (append-to-page-replacement-list frame)
              (zeroize-page page-addr)
              ;; Clear the zero-fill flag.
              (set-address-flags fault-address (logand block-info
                                                       sys.int::+block-map-flag-mask+
                                                       (lognot sys.int::+block-map-zero-fill+)))
              ;; Touch the page to make sure the snapshotter & swap code know to swap it out.
              ;; The zero fill flag in the block map was cleared, but the on-disk data doesn't reflect that.
              ;; This sets the dirty bits in the page tables properly.
              (setf (page-table-entry pte 0) (make-pte frame
                                                       :writable (block-info-writable-p block-info)
                                                       :dirty t))
              (flush-tlb-single fault-address))
            t))))))

(defun wait-for-page-via-interrupt (interrupt-frame address)
  "Called by the page fault handler when a non-present page is accessed.
It will put the thread to sleep, while it waits for the page."
  (let ((self (current-thread))
        (pager (sys.int::symbol-global-value 'sys.int::*pager-thread*)))
    (when (eql self pager)
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
      (with-thread-lock (pager)
        (when (and (eql (thread-state pager) :sleeping)
                   (eql (thread-wait-item pager) '*pager-waiting-threads*))
          (setf (thread-state pager) :runnable)
          (with-symbol-spinlock (*global-thread-lock*)
            (push-run-queue pager)))))
    (%reschedule-via-interrupt interrupt-frame)))

(defun map-physical-memory (base size name)
  ;; Page alignment required.
  (assert (page-aligned-p base))
  (assert (page-aligned-p size))
  (with-mutex (*vm-lock*)
    (dotimes (i (truncate size #x1000))
      (let ((pte (get-pte-for-address (convert-to-pmap-address (+ base (* i #x1000))))))
        (when (not (page-present-p pte 0))
          (setf (page-table-entry pte 0) (make-pte (+ (truncate base #x1000) i)
                                                   :writable t
                                                   :wired t)))))))

(defun initialize-pager ()
  (when (not (boundp '*pager-waiting-threads*))
    (setf *pager-noisy* nil
          *pager-waiting-threads* '()
          *pager-current-thread* nil
          *pager-lock* (place-spinlock-initializer)
          *pager-fast-path-enabled* t
          *pager-lazy-block-allocation-enabled* t))
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
            (release-place-spinlock (sys.int::symbol-global-value '*pager-lock*))
            (setf (thread-state sys.int::*pager-thread*) :sleeping
                  (thread-wait-item sys.int::*pager-thread*) '*pager-waiting-threads*)
            (%run-on-wired-stack-without-interrupts (sp fp)
             (%reschedule-via-wired-stack sp fp))
            (%disable-interrupts)
            (acquire-place-spinlock (sys.int::symbol-global-value '*pager-lock*)))))
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
