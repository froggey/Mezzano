;;;; Virtual memory manager

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

(sys.int::defglobal *page-replacement-list-lock*)
(sys.int::defglobal *page-replacement-list-head*)
(sys.int::defglobal *page-replacement-list-tail*)

(sys.int::defglobal *pager-lazy-block-allocation-enabled*)

(sys.int::defglobal *store-fudge-factor*)

(defun pager-log (&rest things)
  (declare (dynamic-extent things))
  (when (eql *pager-noisy* t)
    (debug-print-line-1 things)))

(defun pager-log-op (&rest things)
  (declare (dynamic-extent things))
  (when *pager-noisy*
    (debug-print-line-1 things)))

(declaim (inline page-table-entry (setf page-table-entry)))
(defun page-table-entry (page-table &optional (index 0))
  (sys.int::memref-unsigned-byte-64 page-table index))
(defun (setf page-table-entry) (value page-table &optional (index 0))
  (setf (sys.int::memref-unsigned-byte-64 page-table index) value))
(defun (sys.int::cas page-table-entry) (old new page-table &optional (index 0))
  (sys.int::cas (sys.int::memref-unsigned-byte-64 page-table index) old new))

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

(declaim (inline block-info-track-dirty-p))
(defun block-info-track-dirty-p (block-info)
  (logtest sys.int::+block-map-track-dirty+ block-info))

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

(defun stack-area-p (address)
  (eql (ldb sys.int::+address-tag+ address) sys.int::+address-tag-stack+))

(defun mark-bit-region-p (address)
  (and (<= sys.int::+mark-bit-region-base+ address)
       (< address (+ sys.int::+mark-bit-region-base+
                     sys.int::+mark-bit-region-size+))))

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
  (with-rw-lock-write (*vm-lock*)
    (cond ((boot-option +boot-option-freestanding+)
           (initialize-freestanding-paging-system))
          (t
           (detect-paging-disk)
           (when (not *paging-disk*)
             (panic "Could not find boot device. Sorry."))))))

(defun initialize-freestanding-paging-system ()
  (setf *paging-disk* :freestanding
        *paging-read-only* t)
  (debug-print-line "Running freestanding.")
  (debug-print-line "BML4 at " *bml4*)
  (initialize-freestanding-store)
  (setf *store-fudge-factor* (- (truncate (physical-memory-statistics) 1.1)))
  (debug-print-line "Set fudge factor to " *store-fudge-factor*)
  (debug-print-line "Waking pager thread.")
  (wake-thread sys.int::*pager-thread*))

(defun initialize-hosted-paging-system (disk header)
  (setf *paging-disk* disk)
  (setf *paging-read-only* (or (not (disk-writable-p disk))
                               (boot-option +boot-option-force-read-only+)))
  (when *paging-read-only*
    (debug-print-line "Running read-only."))
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
  (wake-thread sys.int::*pager-thread*))

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
                   (t (let* ((frame (pager-allocate-page :new-type :other))
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

(defun allocate-new-block-for-virtual-address (address flags &key eager)
  (let ((new-block (if (or (and (not eager)
                                *pager-lazy-block-allocation-enabled*)
                           *paging-read-only*)
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

(defun release-vm-page (frame &key allow-wired stackp)
  (case (physical-page-frame-type frame)
    (:active
     (remove-from-page-replacement-list frame)
     (release-physical-pages frame 1))
    (:active-writeback
     (setf (physical-page-frame-type frame) :inactive-writeback))
    (:wired
     (when (not allow-wired)
       (panic "Releasing page wired " frame))
     ;; Release the backing frame, if any.
     (when (not stackp)
       (let ((backing-frame (physical-page-frame-next frame)))
         (when backing-frame
           (ecase (physical-page-frame-type backing-frame)
             (:wired-backing
              (release-physical-pages backing-frame 1))
             (:wired-backing-writeback
              (setf (physical-page-frame-type backing-frame) :inactive-writeback))))))
     (release-physical-pages frame 1))
    (t
     (panic "Releasing page " frame " with bad type " (physical-page-frame-type frame)))))

(defun pager-rpc (fn &optional arg1 arg2 arg3)
  (without-footholds
    (let ((self (current-thread)))
      (setf (thread-pager-argument-1 self) arg1
            (thread-pager-argument-2 self) arg2
            (thread-pager-argument-3 self) arg3))
    (%run-on-wired-stack-without-interrupts (sp fp fn)
     (let ((self (current-thread)))
       (with-symbol-spinlock (*pager-lock*)
         (acquire-global-thread-lock)
         (setf (thread-state self) :pager-request
               (thread-wait-item self) fn
               (thread-queue-next self) *pager-waiting-threads*
               *pager-waiting-threads* self)
         (when (and (eql (thread-state sys.int::*pager-thread*) :sleeping)
                    (eql (thread-wait-item sys.int::*pager-thread*) '*pager-waiting-threads*))
           (setf (thread-state sys.int::*pager-thread*) :runnable)
           (push-run-queue sys.int::*pager-thread*)))
       (%reschedule-via-wired-stack sp fp)))
    (thread-pager-argument-1 (current-thread))))

(defun allocate-memory-range (base length flags)
  (cond ((or (stack-area-p base)
             (mark-bit-region-p base))
         (assert (and (page-aligned-p base)
                      (page-aligned-p length))
                 (base length)
                 "Range not aligned."))
        (t
         (assert (and (zerop (rem base sys.int::+allocation-minimum-alignment+))
                      (zerop (rem length sys.int::+allocation-minimum-alignment+)))
                 (base length)
                 "Range not aligned.")))
  (pager-rpc 'allocate-memory-range-in-pager base length flags))

(defun map-new-wired-page (address &key backing-frame)
  (let ((pte (get-pte-for-address address t))
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
    (let* ((frame (pager-allocate-page :new-type :wired))
           (addr (convert-to-pmap-address (ash frame 12))))
      (setf (physical-page-frame-block-id frame) (block-info-block-id block-info)
            (physical-page-virtual-address frame) (logand address (lognot (1- +4k-page-size+))))
      (cond (backing-frame
             ;; Include a backing frame.
             (let ((new-backing-frame (pager-allocate-page :new-type :wired-backing)))
               (setf (physical-page-frame-next frame) new-backing-frame)
               (setf (physical-page-virtual-address new-backing-frame) address)))
            (t
             (setf (physical-page-frame-next frame) nil)))
      ;; Block is zero-filled.
      (zeroize-page addr)
      ;; Clear the zero-fill flag.
      (set-address-flags address (logand block-info
                                         sys.int::+block-map-flag-mask+
                                         (lognot sys.int::+block-map-zero-fill+)))
      ;; Don't need to dirty the page like in W-F-P, the snapshotter takes all wired pages.
      (setf (page-table-entry pte 0) (make-pte frame
                                               :writable (and (block-info-writable-p block-info)
                                                              (not (block-info-track-dirty-p block-info)))
                                               :dirty t)))))

(defun allocate-memory-range-in-pager (base length flags)
  (pager-log-op "Allocate range " base "-" (+ base length) "  " flags)
  (when (logtest flags sys.int::+block-map-wired+)
    (ensure (or (< base #x80000000) ; wired area
                (and (<= #x200000000000 base) ; wired stack area.
                     (< base #x208000000000)))))
  (with-rw-lock-write (*vm-lock*)
    ;; Ensure there's enough fudged memory before allocating.
    (when (< (- *store-freelist-n-free-blocks* (truncate length +4k-page-size+)) *store-fudge-factor*)
      (pager-log "Not allocating " length " bytes, too few blocks remaining. "
                 *store-freelist-n-free-blocks* " " *store-fudge-factor*)
      (return-from allocate-memory-range-in-pager nil))
    (dotimes (i (truncate length #x1000))
      (allocate-new-block-for-virtual-address
       (+ base (* i #x1000))
       flags
       :eager (logtest flags sys.int::+block-map-wired+)))
    (when (not (or (stack-area-p base)
                   (mark-bit-region-p base)))
      ;; Allocate new card table pages.
      (let ((card-base (+ sys.int::+card-table-base+
                          (* (truncate base sys.int::+card-size+)
                             sys.int::+card-table-entry-size+)))
            (card-length (* (truncate length sys.int::+card-size+)
                            sys.int::+card-table-entry-size+)))
        (dotimes (i (truncate card-length #x1000))
          (allocate-new-block-for-virtual-address
           (+ card-base (* i #x1000))
           (logior sys.int::+block-map-present+
                   sys.int::+block-map-writable+
                   sys.int::+block-map-zero-fill+
                   sys.int::+block-map-wired+)
           :eager t)
          (map-new-wired-page (+ card-base (* i #x1000)) :backing-frame t))
        (begin-tlb-shootdown)
        (tlb-shootdown-range card-base card-length)
        (finish-tlb-shootdown)))
    (when (or (< base #x80000000)
              (and (<= #x200000000000 base) ; wired stack area.
                   (< base #x208000000000)))
      (ensure (logtest flags sys.int::+block-map-wired+))
      ;; Pages in the wired stack area don't require backing frames.
      (dotimes (i (truncate length #x1000))
        (map-new-wired-page (+ base (* i #x1000))
                            :backing-frame (stack-area-p base)))
      (begin-tlb-shootdown)
      (tlb-shootdown-range base length)
      (finish-tlb-shootdown)))
  t)

(defun release-memory-range (base length)
  (cond ((or (stack-area-p base)
             (mark-bit-region-p base))
         (assert (and (page-aligned-p base)
                      (page-aligned-p length))
                 (base length)
                 "Range not aligned."))
        (t
         (assert (and (zerop (rem base sys.int::+allocation-minimum-alignment+))
                      (zerop (rem length sys.int::+allocation-minimum-alignment+)))
                 (base length)
                 "Range not aligned.")))
  (pager-rpc 'release-memory-range-in-pager base length))

(defun release-memory-range-in-pager (base length ignore3)
  (declare (ignore ignore3))
  (pager-log-op "Release range " base "-" (+ base length))
  (with-rw-lock-write (*vm-lock*)
    (let ((stackp (stack-area-p base))
          (card-base (+ sys.int::+card-table-base+
                        (* (truncate base sys.int::+card-size+)
                           sys.int::+card-table-entry-size+)))
          (card-length (* (truncate length sys.int::+card-size+)
                          sys.int::+card-table-entry-size+)))
      (when (not (or stackp
                     (mark-bit-region-p base)))
        ;; Release card table pages.
        (dotimes (i (truncate card-length #x1000))
          ;; Update block map.
          (release-block-at-virtual-address (+ card-base (* i #x1000)))
          ;; Update page tables and release pages if possible.
          (let ((pte (get-pte-for-address (+ card-base (* i #x1000)) nil)))
            (when (and pte (page-present-p pte 0))
              (release-vm-page (ash (pte-physical-address (page-table-entry pte 0)) -12)
                               :allow-wired t)
              (setf (page-table-entry pte 0) 0)))))
      (dotimes (i (truncate length #x1000))
        ;; Update block map.
        (release-block-at-virtual-address (+ base (* i #x1000)))
        ;; Update page tables and release pages if possible.
        (let ((pte (get-pte-for-address (+ base (* i #x1000)) nil)))
          (when (and pte (page-present-p pte 0))
            (release-vm-page (ash (pte-physical-address (page-table-entry pte 0)) -12)
                             ;; Allow wired stacks to be freed.
                             :allow-wired stackp :stackp stackp)
            (setf (page-table-entry pte 0) 0))))
      (begin-tlb-shootdown)
      (flush-tlb)
      (when (not (or stackp
                     (mark-bit-region-p base)))
        (tlb-shootdown-range card-base card-length))
      (tlb-shootdown-range base length)
      (finish-tlb-shootdown))))

(defun protect-memory-range (base length flags)
  (assert (and (page-aligned-p base)
               (page-aligned-p length))
          (base length)
          "Range not page aligned.")
  (assert (>= (+ base length) (* 512 1024 1024 1024)) () "Wired area can't be protected.")
  ;; P-M-R only modifies the protection flags (present, writable and track-dirty).
  (assert (not (logtest flags (lognot (logior sys.int::+block-map-present+
                                              sys.int::+block-map-writable+
                                              sys.int::+block-map-track-dirty+)))))
  (pager-rpc 'protect-memory-range-in-pager base length flags))

(defun protect-memory-range-in-pager (base length flags)
  (pager-log-op "Protect range " base "-" (+ base length) "  " flags)
  (with-rw-lock-write (*vm-lock*)
    (begin-tlb-shootdown)
    (dotimes (i (truncate length #x1000))
      (let* ((address (+ base (* i #x1000)))
             (block-info (or (block-info-for-virtual-address address)
                             (panic "Virtual address " address " not mapped??")))
             (page-flags (logior (logand block-info
                                         ;; Clear bits.
                                         (lognot (logior sys.int::+block-map-present+
                                                         sys.int::+block-map-writable+
                                                         sys.int::+block-map-track-dirty+)))
                                 flags)))
        (set-address-flags address page-flags)
        ;; Update page tables and release pages if possible.
        (let ((pte (get-pte-for-address address nil)))
          (when (and pte (page-present-p pte 0))
            (cond ((or (not (block-info-present-p page-flags))
                       (block-info-zero-fill-p page-flags))
                   ;; Page going away, but it's ok. It'll be back, zero-filled.
                   #+(or)(debug-print-line "  flush page " (+ base (* i #x1000)) "  " (page-table-entry pte 0))
                   (release-vm-page (ash (pte-physical-address (page-table-entry pte 0)) -12))
                   (setf (page-table-entry pte 0) 0))
                  ((and (block-info-writable-p page-flags)
                        (not (block-info-track-dirty-p page-flags)))
                   ;; Mark writable.
                   (update-pte pte :writable t))
                  (t
                   ;; Mark read-only.
                   (update-pte pte :writable nil)))))))
    (flush-tlb)
    (tlb-shootdown-all)
    (finish-tlb-shootdown)))

(defun update-wired-dirty-bits ()
  (pager-rpc 'update-wired-dirty-bits-in-pager))

(defun update-wired-dirty-bits-in-pager (ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
  (with-rw-lock-write (*vm-lock*)
    (begin-tlb-shootdown)
    (map-ptes
     sys.int::*wired-area-base* sys.int::*wired-area-bump*
     (dx-lambda (wired-page pte)
       (when (not pte)
         (panic "Missing pte for wired page " wired-page))
       (when (page-dirty-p pte)
         (setf (sys.int::card-table-dirty-gen wired-page) 0)
         ;; ARM64's dirty bit emulation does not support emulating
         ;; dirty bits in the wired area yet.
         #-arm64
         (update-pte pte :dirty nil))))
    (map-ptes
     sys.int::*wired-function-area-limit* sys.int::*function-area-base*
     (dx-lambda (wired-page pte)
       (when (not pte)
         (panic "Missing pte for wired function page " wired-page))
       (when (page-dirty-p pte)
         (setf (sys.int::card-table-dirty-gen wired-page) 0)
         ;; ARM64's dirty bit emulation does not support emulating
         ;; dirty bits in the wired area yet.
         #-arm64
         (update-pte pte :dirty nil))))
    (flush-tlb)
    (tlb-shootdown-all)
    (finish-tlb-shootdown)))

(defun get-page-physical-address (virtual-address)
  "Return the physical page frame mapped for VIRTUAL-ADDRESS.
Returns NIL if the address is unmapped."
  (pager-rpc 'get-page-physical-address-in-pager virtual-address))

(defun get-page-physical-address-in-pager (virtual-address ignore2 ignore3)
  (declare (ignore ignore2 ignore3))
  (with-rw-lock-write (*vm-lock*)
    (let ((pte (get-pte-for-address virtual-address nil)))
      (when (and pte (page-present-p pte))
        (+ (ash (page-frame pte) 12)
           (logand virtual-address #xFFF))))))

(defun get-object-physical-addresses (object)
  "Return a simple vector containing all the physical page fors OBJECT.
The first entry in the vector maintains the object's page offset, while
the remaining entries start at offset 0 from the page. If a page is not
mapped, then the entry will be NIL."
  (let* ((address (sys.int::lisp-object-address object))
         (start (logand address (lognot #xFFF)))
         (end (logand (+ address (* (sys.int::object-size object) 8) #xFFF)
                      (lognot #xFFF)))
         (n-pages (truncate (- end start) #x1000))
         (frame-vector (make-array n-pages)))
    (dotimes (i n-pages)
      (setf (aref frame-vector i)
            (get-page-physical-address (logand (+ address (* i #x1000))
                                               ;; Maintain the object offset
                                               ;; for the first page, but set
                                               ;; the offset on the remaining
                                               ;; pages to zero.
                                               (if (zerop i)
                                                   -1
                                                   (lognot #xFFF))))))
    frame-vector))

(defun map-sg-vec (virtual-address sg-vec cache-mode)
  (cond (*paging-disk*
         (pager-rpc 'map-sg-vec-in-pager virtual-address sg-vec cache-mode))
        (t
         ;; Early boot, pager not running yet.
         (map-sg-vec-in-pager virtual-address sg-vec cache-mode))))

(defun map-sg-vec-in-pager (virtual-address sg-vec cache-mode)
  (pager-log-op "Map sg-vec " virtual-address " " sg-vec " " cache-mode)
  (with-rw-lock-write (*vm-lock*)
    (let ((virt-offset 0))
      (loop
         for i below (sys.int::simple-vector-length sg-vec) by 2
         for phys-addr = (svref sg-vec i)
         for sg-len = (svref sg-vec (1+ i))
         do
           (loop
              for page-offset below sg-len by #x1000
              for paddr = (+ phys-addr page-offset)
              for vaddr = (+ virtual-address virt-offset page-offset)
              for bme = (block-info-for-virtual-address-1 vaddr t)
              for pte = (get-pte-for-address vaddr)
              do
                (when (not (zerop (sys.int::memref-unsigned-byte-64 bme)))
                  (panic "Block " vaddr " entry not zero!"))
                (setf (sys.int::memref-unsigned-byte-64 bme)
                      (logior sys.int::+block-map-present+
                              sys.int::+block-map-writable+
                              sys.int::+block-map-transient+))
                (setf (page-table-entry pte) (make-pte (truncate paddr #x1000)
                                                       :writable t
                                                       :wired t
                                                       :dirty t
                                                       :cache-mode cache-mode)))
           (incf virt-offset sg-len))
      (begin-tlb-shootdown)
      (flush-tlb)
      (tlb-shootdown-range virtual-address virt-offset)
      (finish-tlb-shootdown)))
  t)

;; Also frees the memory associated with the sg-vec.
(defun unmap-sg-vec (virtual-address sg-vec)
  (cond (*paging-disk*
         (pager-rpc 'unmap-sg-vec-in-pager virtual-address sg-vec))
        (t
         ;; Early boot, pager not running yet.
         (unmap-sg-vec-in-pager virtual-address sg-vec nil))))

(defun unmap-sg-vec-in-pager (virtual-address sg-vec ignore3)
  (declare (ignore ignore3))
  (pager-log-op "Unmap sg-vec " virtual-address " " sg-vec)
  (with-rw-lock-write (*vm-lock*)
    (let ((virt-offset 0))
      (loop
         for i below (sys.int::simple-vector-length sg-vec) by 2
         for phys-addr = (svref sg-vec i)
         for sg-len = (svref sg-vec (1+ i))
         do
           (loop
              for page-offset below sg-len by #x1000
              for paddr = (+ phys-addr page-offset)
              for vaddr = (+ virtual-address virt-offset page-offset)
              for bme = (block-info-for-virtual-address-1 vaddr)
              for pte = (get-pte-for-address vaddr nil)
              do
                (when (not (eql (sys.int::memref-unsigned-byte-64 bme)
                                (logior sys.int::+block-map-present+
                                        sys.int::+block-map-writable+
                                        sys.int::+block-map-transient+)))
                  (panic "Block " vaddr " entry not correct for sg-vec!"))
                (setf (sys.int::memref-unsigned-byte-64 bme) 0)
                (when (and pte (page-present-p pte))
                  (setf (page-table-entry pte) 0)))
           (release-physical-pages (truncate phys-addr #x1000)
                                   (ceiling sg-len #x1000))
           (incf virt-offset sg-len))
      (begin-tlb-shootdown)
      (flush-tlb)
      (tlb-shootdown-range virtual-address virt-offset)
      (finish-tlb-shootdown)))
  t)

(defun pager-allocate-page (&key (new-type :active))
  (ensure (rw-lock-write-held-p *vm-lock*))
  (check-tlb-shootdown-not-in-progress)
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
        (begin-tlb-shootdown)
        (setf (page-table-entry pte-addr) (make-pte 0 :present nil))
        (flush-tlb-single candidate-virtual)
        (tlb-shootdown-single candidate-virtual)
        (finish-tlb-shootdown)
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

;;; WAIT-FOR-PAGE-FAST-PATH calls this concurrently with the *VM-LOCK*
;;; read-held, so append requires an additional lock.
;;; All other accesses to the replacement list are perfomed with the
;;; full *VM-LOCK* write-held so don't need to lock.
(defun append-to-page-replacement-list (frame)
  (safe-without-interrupts (frame)
    (with-place-spinlock (*page-replacement-list-lock*)
      (cond (*page-replacement-list-head*
             (setf (physical-page-frame-next *page-replacement-list-tail*) frame
                   (physical-page-frame-prev frame) *page-replacement-list-tail*
                   (physical-page-frame-next frame) nil
                   *page-replacement-list-tail* frame))
            (t
             (setf *page-replacement-list-head* frame
                   *page-replacement-list-tail* frame
                   (physical-page-frame-next frame) nil
                   (physical-page-frame-prev frame) nil))))))

(defun remove-from-page-replacement-list (frame)
  (ensure (rw-lock-write-held-p *vm-lock*))
  (when (eql *page-replacement-list-head* frame)
    (setf *page-replacement-list-head* (physical-page-frame-next frame)))
  (when (physical-page-frame-next frame)
    (setf (physical-page-frame-prev (physical-page-frame-next frame)) (physical-page-frame-prev frame)))
  (when (eql *page-replacement-list-tail* frame)
    (setf *page-replacement-list-tail* (physical-page-frame-prev frame)))
  (when (physical-page-frame-prev frame)
    (setf (physical-page-frame-next (physical-page-frame-prev frame)) (physical-page-frame-next frame))))

(defun address-in-non-faulting-range-p (address)
  ;; Pages below 512G are wired and should never be unmapped or protected.
  ;; Same for pages in the wired stack area.

  (or (<= 0 address (1- (* 512 1024 1024 1024)))
      (<= (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+)
          address
          (+ (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+)
             (* 512 1024 1024 1024)))))

(defun wait-for-page-unlocked (address writep)
  (when (address-in-non-faulting-range-p address)
    (debug-print-line "Fault on non-faulting address " address)
    (return-from wait-for-page-unlocked nil))
  (let ((pte (get-pte-for-address address))
        (block-info (block-info-for-virtual-address address)))
    #+(or)(debug-print-line "WFP " address " block " block-info)
    (when (and writep
               (not (block-info-writable-p block-info)))
      (debug-print-line "Write to read-only page " address)
      (return-from wait-for-page-unlocked nil))
    ;; Examine the page table, if there's a present entry then the page
    ;; was mapped while acquiring the VM lock. Just return.
    (when (page-present-p pte)
      (when (and block-info
                 (block-info-track-dirty-p block-info))
        ;; Probably a write fault. Set the dirty flag in the card table.
        ;; Leave the page read-only and the track bit set until after
        ;; clone-cow has finished.
        (setf (sys.int::card-table-dirty-gen address) 0))
      (when (page-copy-on-write-p pte)
        (pager-log "Copying page " address " in WFP.")
        (snapshot-clone-cow-page (pager-allocate-page) address))
      (when (and block-info
                 (block-info-track-dirty-p block-info))
        ;; Wipe the track flag.
        (set-address-flags address (logand block-info
                                           sys.int::+block-map-flag-mask+
                                           (lognot sys.int::+block-map-track-dirty+)))
        ;; Remap page read/write.
        (begin-tlb-shootdown)
        (setf (page-table-entry pte) (make-pte (ash (pte-physical-address (page-table-entry pte)) -12)
                                               :writable (block-info-writable-p block-info)))
        (flush-tlb-single address)
        (tlb-shootdown-single address)
        (finish-tlb-shootdown))
      #+(or)(debug-print-line "WFP " address " block " block-info " already mapped " (page-table-entry pte 0))
      (return-from wait-for-page-unlocked t))
    ;; Note that this test is done after the pte test. this is a hack to make
    ;; runtime-allocated stacks work before allocate-stack actually modifies the
    ;; block map.
    ;; ### must investigate if allocate-stack still needs this behaviour & why.
    (when (or (not block-info)
              (not (block-info-present-p block-info)))
      #+(or)(debug-print-line "WFP " address " not present")
      (return-from wait-for-page-unlocked nil))
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
      (begin-tlb-shootdown)
      (setf (page-table-entry pte) (make-pte frame
                                             :writable (and (block-info-writable-p block-info)
                                                            (not (block-info-track-dirty-p block-info)))
                                             ;; Mark the page dirty to make sure the snapshotter & swap code know to swap it out.
                                             ;; The zero fill flag in the block map was cleared, but the on-disk data doesn't reflect that.
                                             :dirty is-zero-page))
      (flush-tlb-single address)
      (tlb-shootdown-single address)
      (finish-tlb-shootdown)
      #+(or)
      (debug-print-line "WFP " address " block " block-info " mapped to " (page-table-entry pte 0))))
  t)

(defun wait-for-page (address writep)
  (with-rw-lock-write (*vm-lock*)
    (wait-for-page-unlocked address writep)))

;; Fast path, called from the page-fault handler.
;; If the *VM-LOCK* is not write-taken,
;; and the page tables for the address have already been allocated,
;; and the page for the address is a zero-page,
;; and there is a free page frame
;; then immediately allocate, zero, and map a frame, then return true.
;; Otherwise return false to punt back to w-f-p-via-interrupt.
;; Blocking is not permitted during a page-fault, which is why
;; this function can't block on the lock, or wait for a frame to be
;; swapped out, or wait for the new data to be swapped in.
(defun wait-for-page-fast-path (fault-address writep)
  (with-rw-lock-read (*vm-lock* :wait-p nil)
    (let ((pte (get-pte-for-address fault-address nil))
          (block-info (block-info-for-virtual-address fault-address)))
      ;; GC write barrier.
      ;; If a read-only non-COW page is written to then set the appropriate
      ;; card table entry and make the page writable again.
      ;; Other threads can change the page from read-only to read-write
      ;; under us, so check for that and assume success in that case.
      (when (and pte
                 (page-present-p pte)
                 (not (page-copy-on-write-p pte))
                 block-info
                 (block-info-writable-p block-info)
                 (block-info-track-dirty-p block-info))
        ;; CAS loop to attempt a PTE update.
        ;; The fast path will only ever change a page from
        ;; not-present to present or from read-only to read-write.
        ;; So this page will never disappear out from under us and
        ;; looping without repeating the above checks is safe.
        (loop
           (let ((pte-value (page-table-entry pte)))
             (when (pte-page-writable-p pte-value)
               ;; Page was writable all along. Possibly a stale
               ;; TLB entry or another thread hit the write barrier
               ;; at the same time.
               (flush-tlb-single fault-address)
               (return))
             (when (update-pte-atomic pte pte-value :writable t)
               ;; Page table updated successfully.
               (return))))
        ;; Tracking dirty bits for the GC.
        (setf (sys.int::card-table-dirty-gen fault-address) 0)
        (flush-tlb-single fault-address)
        (return-from wait-for-page-fast-path t))
      ;; Pager fast zero page mapping.
      ;; If a page is non-present in the page table and marked
      ;; as zero-fill in the block map, then allocate, zero and map it.
      ;; This is also where spurious faults are detected and ignored. These can
      ;; occur when another thread updates the page tables while we're
      ;; handling this fault.
      ;; Other threads will set the zero fill flag to 0 if they install a
      ;; page themselves.
      (when (and pte
                 block-info
                 (block-info-present-p block-info))
        ;; Optimistically allocate a frame to map.
        (let ((frame (allocate-physical-pages 1 :type :active)))
          (when frame
            ;;(debug-print-line "Optimistic zero page fast path for " fault-address)
            (let ((page-addr (convert-to-pmap-address (ash frame 12))))
              (zeroize-page page-addr)
              ;; CAS loop to try to install our frame.
              (loop
                 (let ((pte-value (page-table-entry pte)))
                   (when (or (pte-page-present-p pte-value)
                             (not (block-info-zero-fill-p block-info)))
                     ;; Give up.
                     (release-physical-pages frame 1)
                     ;; For a fault to be spurious the page must be present
                     ;; and one of the following must be true:
                     ;; 1) the access was a read
                     ;; 2) the page is writable
                     ;; 3) the page is not-cow, the block is
                     ;;    writable and track-dirty.
                     (cond ((and (pte-page-present-p pte-value)
                                 (or (not writep)
                                     (pte-page-writable-p pte-value)
                                     (and (not (pte-page-copy-on-write-p pte-value))
                                          (block-info-writable-p block-info)
                                          (block-info-track-dirty-p block-info))))
                            (flush-tlb-single fault-address)
                            (return-from wait-for-page-fast-path t))
                           (t
                            (return))))
                   ;; Stuff the page in now.
                   ;; Mark the page as dirty to make sure the snapshotter & swap code know to swap it out.
                   ;; The zero fill flag in the block map was cleared, but the on-disk data doesn't reflect that.
                   ;; This sets the dirty bits in the page tables properly.
                   (when (eql (sys.int::cas (page-table-entry pte)
                                            pte-value
                                            (make-pte frame
                                                      :writable (and (block-info-writable-p block-info)
                                                                     (not (block-info-track-dirty-p block-info)))
                                                      :dirty t))
                              pte-value)
                     ;; Success! Our page was mapped. Now finish up.
                     (setf (physical-page-frame-block-id frame) (block-info-block-id block-info)
                           (physical-page-virtual-address frame) (logand fault-address (lognot (1- +4k-page-size+))))
                     (append-to-page-replacement-list frame)
                     ;; Clear the zero-fill flag.
                     ;; We set the PTE so this is our page. Other threads won't write
                     ;; to the block map for this address so additional locking isn't needed.
                     (set-address-flags fault-address (logand block-info
                                                              sys.int::+block-map-flag-mask+
                                                              (lognot sys.int::+block-map-zero-fill+)))
                     ;; Play a little fast & loose with TLB shootdown here.
                     ;; The fast path runs on the exception stack with interrupts
                     ;; disabled. TLB shootdown requires interrupts to be enabled.
                     ;; At this point all other CPUs should know that this entry is non-present,
                     ;; so the worst case is that a spurious page fault is taken.
                     (flush-tlb-single fault-address)
                     (return-from wait-for-page-fast-path t)))))))))))

(defun wait-for-page-via-interrupt (interrupt-frame address writep ist-state)
  "Called by the page fault handler when a page fault occurs.
It will put the thread to sleep, while it waits for the page."
  (let ((self (current-thread))
        (pager (sys.int::symbol-global-value 'sys.int::*pager-thread*)))
    (when (eql self pager)
      (panic "Page fault in pager!"))
    (when (and *pager-fast-path-enabled*
               (wait-for-page-fast-path address writep))
      (sys.int::%atomic-fixnum-add-symbol
       '*pager-fast-path-hits* 1)
      (return-from wait-for-page-via-interrupt))
    (sys.int::%atomic-fixnum-add-symbol
     '*pager-fast-path-misses* 1)
    (with-symbol-spinlock (*pager-lock*)
      (acquire-global-thread-lock)
      (setf (thread-state self) (if writep
                                    :waiting-for-page-write
                                    :waiting-for-page-read)
            (thread-wait-item self) address
            (thread-queue-next self) *pager-waiting-threads*
            *pager-waiting-threads* self)
      (when (and (eql (thread-state pager) :sleeping)
                 (eql (thread-wait-item pager) '*pager-waiting-threads*))
        (setf (thread-state pager) :runnable)
        (push-run-queue pager)))
    (restore-page-fault-ist ist-state)
    (%reschedule-via-interrupt interrupt-frame)))

(defun map-physical-memory-in-pager (base size name)
  (declare (ignore name))
  ;; Page alignment required.
  (ensure (page-aligned-p base))
  (ensure (page-aligned-p size))
  (with-rw-lock-write (*vm-lock*)
    (dotimes (i (truncate size #x1000))
      (let ((pte (get-pte-for-address (convert-to-pmap-address (+ base (* i #x1000))))))
        (when (not (page-present-p pte 0))
          (setf (page-table-entry pte 0) (make-pte (+ (truncate base #x1000) i)
                                                   :writable t
                                                   :wired t
                                                   :cache-mode :uncached)))))))

(defun map-physical-memory-early (base size name)
  ;; TODO: Check that this really is being called early.
  ;; Page alignment required.
  (assert (page-aligned-p base))
  (assert (page-aligned-p size))
  (map-physical-memory-in-pager base size name))

(defun map-physical-memory (base size name)
  ;; Page alignment required.
  (assert (page-aligned-p base))
  (assert (page-aligned-p size))
  (pager-rpc 'map-physical-memory-in-pager base size name))

(defun pager-invoke (function arg _arg3)
  (declare (ignore _arg3))
  (when (not (pager-invoke-function-on-thread *pager-current-thread* function arg))
    (panic "PAGER-INVOKE " function " " arg " on " *pager-current-thread* " failed! Out of stack space?")))

(defun pager-invoke-via-interrupt (function interrupt-frame arg)
  "Arrange for FUNCTION to be called with ARG, for use in exception handlers."
  (let ((self (current-thread)))
    (setf (thread-pager-argument-1 self) function
          (thread-pager-argument-2 self) arg
          (thread-pager-argument-3 self) nil)
    (with-symbol-spinlock (*pager-lock*)
      (acquire-global-thread-lock)
      (setf (thread-state self) :pager-request
            (thread-wait-item self) 'pager-invoke
            (thread-queue-next self) *pager-waiting-threads*
            *pager-waiting-threads* self)
      (when (and (eql (thread-state sys.int::*pager-thread*) :sleeping)
                 (eql (thread-wait-item sys.int::*pager-thread*) '*pager-waiting-threads*))
        (setf (thread-state sys.int::*pager-thread*) :runnable)
        (push-run-queue sys.int::*pager-thread*)))
    (%reschedule-via-interrupt interrupt-frame)))

(defun initialize-pager ()
  (setf *bml4* (sys.int::memref-signed-byte-64 (+ *boot-information-page* +boot-information-block-map+)))
  (when (not (boundp '*pager-waiting-threads*))
    (setf *pager-noisy* nil
          *pager-waiting-threads* '()
          *pager-current-thread* nil
          *pager-lock* (place-spinlock-initializer)
          *pager-fast-path-enabled* t
          *pager-lazy-block-allocation-enabled* t))
  (setf *page-replacement-list-lock* (place-spinlock-initializer)
        *page-replacement-list-head* nil
        *page-replacement-list-tail* nil)
  (setf *pager-fast-path-hits* 0
        *pager-fast-path-misses* 0)
  (when *pager-current-thread*
    ;; Push any current thread back on the waiting threads list.
    (setf (thread-queue-next *pager-current-thread*) *pager-waiting-threads*
          *pager-waiting-threads* *pager-current-thread*
          *pager-current-thread* nil))
  (setf *pager-disk-request* (make-disk-request))
  ;; The VM lock is recreated each boot because it is only held by
  ;; the ephemeral pager & snapshot threads or by threads that have
  ;; inhibited snapshot (just callers of MAP-PHYSICAL-MEMORY).
  (setf *vm-lock* (make-rw-lock '*vm-lock*))
  ;; Set all the dirty bits for wired pages. They were not saved over snapshot.
  (map-ptes
   sys.int::*wired-area-base* sys.int::*wired-area-bump*
   (dx-lambda (wired-page pte)
     (when (not pte)
       (panic "Missing pte for wired page " wired-page))
     (update-pte pte
                 :dirty t)))
  (flush-tlb))

;;; When true, the system will panic if a thread touches a truely unmapped page.
;;; When false, the system will continue on and leave the thread in limbo.
(sys.int::defglobal *panic-on-unhandled-paging-requests* t)

(defun handle-pager-request ()
  (setf (thread-pager-argument-1 *pager-current-thread*)
        (funcall (thread-wait-item *pager-current-thread*)
                 (thread-pager-argument-1 *pager-current-thread*)
                 (thread-pager-argument-2 *pager-current-thread*)
                 (thread-pager-argument-3 *pager-current-thread*)))
  (wake-thread *pager-current-thread*))

(defun pager-invoke-function-on-thread (thread function &optional (arg nil arg-p))
  (with-rw-lock-write (*vm-lock*)
    ;; Ensure that enough of the stack is paged in to form a full
    ;; interrupt frame.
    (let* ((current-sp (thread-state-rsp thread))
           (target-sp (- current-sp (stack-space-required-for-force-call-on-thread thread))))
      (loop
         for address from (align-down target-sp #x1000) below (align-up current-sp #x1000) by #x1000
         do (when (not (wait-for-page-unlocked address t))
              (return-from pager-invoke-function-on-thread nil))))
    (if arg-p
        (force-call-on-thread thread function arg)
        (force-call-on-thread thread function))
    t))

;; These exist so that the function object exists in the wired area,
;; this is needed so the pager can safely read the entry point address.
(defun %raise-stack-overflow ()
  (sys.int::raise-stack-overflow))

(defun %raise-memory-fault (address)
  (sys.int::raise-memory-fault address))

(defun handle-fault-in-pager (thread writep)
  "Called when WAIT-FOR-PAGE is unable to handle a paging request."
  (let ((faulting-address (thread-wait-item thread)))
    (let* ((stack (thread-stack thread))
           (stack-base (stack-base stack))
           (stack-guard-top (+ stack-base +thread-stack-soft-guard-size+))
           (stack-return-top (+ stack-guard-top +thread-stack-guard-return-size+)))
      (when (and (<= stack-base faulting-address)
                 (< faulting-address stack-guard-top))
        (debug-print-line "Fault on soft stack guard page for thread " thread ", address " faulting-address)
        (debug-print-line "  Unprotecting guard area " stack-base "-" stack-guard-top)
        (protect-memory-range-in-pager
         stack-base +thread-stack-soft-guard-size+
         (logior sys.int::+block-map-present+
                 sys.int::+block-map-writable+))
        ;; Make the return area read-only. Writes to it will trigger a
        ;; reprotection of the guard page.
        (debug-print-line "  Protecting guard return area " stack-guard-top "-" stack-return-top)
        (protect-memory-range-in-pager
         stack-guard-top +thread-stack-guard-return-size+
         sys.int::+block-map-present+)
        (setf (thread-stack-guard-page-state thread) t)
        ;; Arrange for the thread to call the support function.
        (when (pager-invoke-function-on-thread thread #'%raise-stack-overflow)
          (debug-print-line "Pager invoked stack overflow handler.")
          ;; Return and let the thread redo the fault.
          (wake-thread thread)
          (return-from handle-fault-in-pager))
        (debug-print-line "Unable to invoke stack overflow handler - insufficient stack space?"))
      (when (and writep
                 (<= stack-guard-top faulting-address)
                 (< faulting-address stack-return-top))
        (debug-print-line "Fault on stack guard return page for thread " thread ",  address " faulting-address)
        ;; Write to the return area, make the guard area fully inaccessible
        ;; and make the return region writable again.
        (debug-print-line "  Protecting guard area " stack-base "-" stack-guard-top)
        ;; FIXME: This shouldn't use +B-M-ZERO-FILL+, it blows away the contents
        ;; of the old stack pages. Fixing this invokes modifying P-M-R and the
        ;; rest of the pager so it supports a protection flag value of 0.
        (protect-memory-range-in-pager
         stack-base +thread-stack-soft-guard-size+
         sys.int::+block-map-zero-fill+)
        ;; Make the return area read-only. Writes to it will trigger a
        ;; reprotection of the guard page.
        (debug-print-line "  Unprotecting guard return area " stack-guard-top "-" stack-return-top)
        (protect-memory-range-in-pager
         stack-guard-top +thread-stack-guard-return-size+
         (logior sys.int::+block-map-present+
                 sys.int::+block-map-writable+))
        (setf (thread-stack-guard-page-state thread) nil)
        ;; Return and let the thread redo the fault.
        (wake-thread thread)
        (return-from handle-fault-in-pager)))
    ;; Try to pass through to the support function.
    (when (pager-invoke-function-on-thread thread #'%raise-memory-fault faulting-address)
      (debug-print-line "Pager invoked memory fault handler on address " faulting-address)
      ;; Return and let the thread redo the fault.
      (wake-thread thread)
      (return-from handle-fault-in-pager))
    ;; TODO: Shouldn't panic at all, this should be dispatched to a debugger thread.
    (cond ((or (not (boundp '*panic-on-unhandled-paging-requests*))
               *panic-on-unhandled-paging-requests*)
           (let ((message (list "page fault on unmapped page " faulting-address " in thread " *pager-current-thread*)))
             (declare (dynamic-extent message))
             (panic-1 message (lambda ()
                                (dump-thread-saved-pc *pager-current-thread*)
                                (panic-print-backtrace (thread-frame-pointer *pager-current-thread*))
                                (debug-print-line "-------")))))
          (t
           (debug-print-line "Thread " *pager-current-thread* " faulted on address " faulting-address)
           (panic-print-backtrace (thread-frame-pointer *pager-current-thread*))))))

(defun pager-thread ()
  (loop
     ;; Select a thread.
     (without-interrupts
       (with-symbol-spinlock (*pager-lock*)
         (loop
            (when *pager-waiting-threads*
              (setf *pager-current-thread* *pager-waiting-threads*
                    *pager-waiting-threads* (thread-queue-next *pager-current-thread*))
              (set-paging-light t)
              (return))
            (set-paging-light nil)
            ;; Manually sleep, don't use condition variables or similar within ephemeral threads.
            (acquire-global-thread-lock)
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
           ((wait-for-page (thread-wait-item *pager-current-thread*)
                           (ecase (thread-state *pager-current-thread*)
                             (:waiting-for-page-read nil)
                             (:waiting-for-page-write t)))
            ;; Release the thread.
            (wake-thread *pager-current-thread*))
           (t
            (handle-fault-in-pager *pager-current-thread*
                                   (ecase (thread-state *pager-current-thread*)
                                     (:waiting-for-page-read nil)
                                     (:waiting-for-page-write t)))))
     (setf *pager-current-thread* nil)))
