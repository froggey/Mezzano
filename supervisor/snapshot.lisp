;;;; Whole-system transparent persistence support.

(in-package :mezzano.supervisor)

(sys.int::defglobal *snapshot-in-progress* nil)
(sys.int::defglobal *snapshot-state*)
(sys.int::defglobal *snapshot-inhibit*)

(sys.int::defglobal *snapshot-disk-request*)
(sys.int::defglobal *snapshot-bounce-buffer-page*)
(sys.int::defglobal *snapshot-pending-writeback-pages-count*)
(sys.int::defglobal *snapshot-pending-writeback-pages*)

(sys.int::defglobal *enable-snapshot-cow-fast-path* nil)

(sys.int::defglobal *snapshot-epoch*)
(sys.int::defglobal *snapshot-next-epoch*)

(declaim (inline %fast-page-copy))
(defun %fast-page-copy (destination source)
  (sys.int::%copy-words destination source 512))

(defun snapshot-add-to-writeback-list (frame)
  ;; Link frame into writeback list.
  (cond (*snapshot-pending-writeback-pages*
         (setf (physical-page-frame-prev *snapshot-pending-writeback-pages*) frame
               (physical-page-frame-next frame) *snapshot-pending-writeback-pages*
               (physical-page-frame-prev frame) nil
               *snapshot-pending-writeback-pages* frame))
        (t
         (setf *snapshot-pending-writeback-pages* frame
               (physical-page-frame-next frame) nil
               (physical-page-frame-prev frame) nil)))
  (incf *snapshot-pending-writeback-pages-count*))

(defun snapshot-add-writeback-frame (frame)
  (ensure (eql (physical-page-frame-type frame) :active)
          "Tried to writeback non-active frame of type " (physical-page-frame-type frame))
  (let* ((address (physical-page-virtual-address frame))
         (bme-addr (or (block-info-for-virtual-address-1 address nil)
                       (panic "No block map entry for page " address)))
         (bme (sys.int::memref-unsigned-byte-64 bme-addr 0)))
    #+(or)(debug-print-line "Add writeback frame " frame ":" address " " bme)
    ;; Commit if needed.
    (when (not (logtest bme sys.int::+block-map-committed+))
      (let ((new-block (or (store-alloc 1)
                           (panic "Aiiee, out of store during writeback page commit.")))
            (old-block (ash bme (- sys.int::+block-map-id-shift+))))
        #+(or)(debug-print-line "  committing " old-block " " new-block)
        (setf bme (logior (ash new-block sys.int::+block-map-id-shift+)
                          sys.int::+block-map-committed+
                          (logand bme #xFF))
              (sys.int::memref-unsigned-byte-64 bme-addr 0) bme)
        #+(or)(debug-print-line "Replace old block " old-block " with " new-block " vaddr " candidate-virtual)
        (decf *store-fudge-factor*)
        (store-deferred-free old-block 1)
        #+(or)(debug-print-line "Old block: " old-block "  new-block: " new-block)))
    ;; Set block number.
    (setf (physical-page-frame-block-id frame) (ash bme (- sys.int::+block-map-id-shift+)))
    (setf (physical-page-frame-type frame) :active-writeback)
    (remove-from-page-replacement-list frame)
    (snapshot-add-to-writeback-list frame)))

(defun snapshot-copy-wired-area ()
  ;; FIXME: Only copy dirty pages.
  ;; FIXME: See comment in INITIALIZE-SNAPSHOT about card-table sparseness
  ;; Walk through each wired page and allocate a new block for it.
  (flet ((alloc-blocks (start end &key sparse)
           (map-ptes
            start end
            (dx-lambda (wired-page pte)
              (when (not pte)
                (panic "No page table entry for wired-page " wired-page))
              (let ((page-frame (and (page-present-p pte 0)
                                     (ash (pte-physical-address (sys.int::memref-unsigned-byte-64 pte 0)) -12))))
                (when page-frame
                  (let* ((backing-frame (physical-page-frame-next page-frame))
                         (bme-addr (or (block-info-for-virtual-address-1 wired-page nil)
                                       (panic "No block map entry for wired-page " wired-page)))
                         (bme (sys.int::memref-unsigned-byte-64 bme-addr 0))
                         (new-block (or (store-alloc 1)
                                        (panic "Aiiee, out of store when copying wired area!")))
                         (old-block (ash bme (- sys.int::+block-map-id-shift+))))
                    (setf (physical-page-frame-block-id backing-frame) new-block)
                    (setf (physical-page-frame-type backing-frame) :wired-backing-writeback)
                    (setf bme (logior (ash new-block sys.int::+block-map-id-shift+)
                                      sys.int::+block-map-committed+
                                      (logand bme #xFF))
                          (sys.int::memref-unsigned-byte-64 bme-addr 0) bme)
                    (decf *store-fudge-factor*)
                    (store-deferred-free old-block 1)))))
            :sparse sparse)))
    (alloc-blocks sys.int::*wired-area-base* sys.int::*wired-area-bump*)
    (alloc-blocks sys.int::+card-table-base+
                  (+ sys.int::+card-table-base+ sys.int::+card-table-size+)
                  :sparse t)
    (alloc-blocks sys.int::*wired-function-area-limit* sys.int::*function-area-base*))
  ;; FIXME: This should completely suspend other CPUs for the duration. Stopping
  ;; the world is not enough.
  ;; I bet this could be partially done with CoW. Evil.
  ;; Copy without interrupts to avoid smearing.
  (without-interrupts
    (flet ((copy-pages (start end &key sparse)
             (map-ptes
              start end
              (dx-lambda (wired-page pte)
                (when (not pte)
                  (panic "No page table entry for " wired-page))
                (let* ((page-frame (and (page-present-p pte)
                                        (ash (pte-physical-address (sys.int::memref-unsigned-byte-64 pte 0)) -12)))
                       (other-frame (and page-frame
                                         (physical-page-frame-next page-frame))))
                  (when page-frame
                    (%fast-page-copy (convert-to-pmap-address (ash other-frame 12))
                                     wired-page)
                    (snapshot-add-to-writeback-list other-frame))))
              :sparse sparse)))
      (copy-pages sys.int::*wired-area-base* sys.int::*wired-area-bump*)
      (copy-pages sys.int::+card-table-base+
                  (+ sys.int::+card-table-base+ sys.int::+card-table-size+)
                  :sparse t)
      (copy-pages sys.int::*wired-function-area-limit* sys.int::*function-area-base*))))

(defun snapshot-clone-cow-page (new-frame fault-addr)
  (let* ((pte (or (get-pte-for-address fault-addr nil)
                  (panic "No PTE for CoW address?" fault-addr)))
         (block-info (or (block-info-for-virtual-address fault-addr)
                         (panic "No block info for CoW address?" fault-addr)))
         (old-frame (ash (pte-physical-address (sys.int::memref-unsigned-byte-64 pte 0)) -12)))
    (ensure (page-copy-on-write-p pte)
            "Copying non-CoW page?")
    (%fast-page-copy (convert-to-pmap-address (ash new-frame 12))
                     (logand fault-addr (lognot #xFFF)))
    (setf (physical-page-frame-block-id new-frame) (physical-page-frame-block-id old-frame)
          (physical-page-virtual-address new-frame) (physical-page-virtual-address old-frame))
    (setf (physical-page-frame-type new-frame) :active
          (physical-page-frame-type old-frame) :inactive-writeback)
    (append-to-page-replacement-list new-frame)
    ;; Point the PTE at the new page, disable copy on write and reenable write access.
    (begin-tlb-shootdown)
    (setf (sys.int::memref-unsigned-byte-64 pte 0)
          (make-pte new-frame
                    :writable (and (block-info-writable-p block-info)
                                   (not (block-info-track-dirty-p block-info)))))
    (flush-tlb-single fault-addr)
    (tlb-shootdown-single fault-addr)
    (finish-tlb-shootdown)
    #+(or)(debug-print-line "Copied page " fault-addr)))

(defun snapshot-clone-cow-page-via-page-fault (interrupt-frame fault-addr)
  (cond (*enable-snapshot-cow-fast-path*
         ;; Doesn't work on SMP, due to issues with TLB shootdown.
         (let ((new-frame (allocate-physical-pages 1)))
           (when (not new-frame)
             ;; No memory. Punt to the pager, does not return.
             (wait-for-page-via-interrupt interrupt-frame fault-addr))
           (snapshot-clone-cow-page new-frame fault-addr)))
        (t
         (wait-for-page-via-interrupt interrupt-frame fault-addr))))

(defun pop-pending-snapshot-page ()
  "Pop the next pending snapshot page.
Returns 4 values:
  FRAME - the frame id of the page. This frame is not in-use, and will either be
          a copied CoW frame, a wired backing frame, or the snapshot bounce buffer page.
  FREEP - True if FRAME should be freed after it has been written back.
  BLOCK-ID - ID of the block to write to. This will always be a valid block, not a deferred block.
  ADDRESS - Virtual address of the page to write back."
  (with-rw-lock-write (*vm-lock*)
    (begin-tlb-shootdown)
    (multiple-value-bind (frame freep block-id address)
        (pop-pending-snapshot-page-1)
      (tlb-shootdown-single address)
      (finish-tlb-shootdown)
      (values frame freep block-id address))))

(defun pop-pending-snapshot-page-1 ()
  (without-interrupts
    (let* ((frame *snapshot-pending-writeback-pages*)
           (address (physical-page-virtual-address frame))
           (block-id (physical-page-frame-block-id frame)))
      ;; Remove frame from list.
      (setf *snapshot-pending-writeback-pages* (physical-page-frame-next frame))
      (when *snapshot-pending-writeback-pages*
        (setf (physical-page-frame-prev *snapshot-pending-writeback-pages*) nil))
      (decf *snapshot-pending-writeback-pages-count*)
      (case (physical-page-frame-type frame)
        (:active-writeback
         ;; Page is mapped in memory.
         ;; Copy to the bounce buffer.
         (%fast-page-copy (convert-to-pmap-address (ash *snapshot-bounce-buffer-page* 12))
                          (convert-to-pmap-address (ash frame 12)))
         ;; Allow access to the page again.
         (let* ((pte (or (get-pte-for-address address nil)
                         (panic "No PTE for CoW address?")))
                (frame (ash (pte-physical-address (sys.int::memref-unsigned-byte-64 pte 0)) -12))
                ;; The block map should only be consulted for active pages.
                ;; Other kinds of pages indicate that the virtual memory was modified
                ;; and the page may no longer exist in the most recent block map.
                (block-info (or (block-info-for-virtual-address address)
                                (panic "No block info for CoW address?" address))))
           ;; Update PTE bits. Clear CoW bit, make writable.
           (setf (sys.int::memref-unsigned-byte-64 pte 0)
                 (make-pte frame
                           :writable (and (block-info-writable-p block-info)
                                          (not (block-info-track-dirty-p block-info)))
                           :dirty (page-dirty-p pte)))
           (flush-tlb-single address))
         ;; Return page to normal use.
         (setf (physical-page-frame-type frame) :active)
         (append-to-page-replacement-list frame)
         (values *snapshot-bounce-buffer-page*
                 ;; Don't free the bounce page.
                 nil
                 block-id
                 address))
        (:inactive-writeback
         ;; Page was copied.
         (values frame
                 t
                 block-id
                 address))
        (:wired-backing-writeback
         ;; Page is a wired backing page.
         ;; Copy to the bounce buffer.
         ;; Page may be freed during writeback if VM is modified.
         (%fast-page-copy (convert-to-pmap-address (ash *snapshot-bounce-buffer-page* 12))
                          (convert-to-pmap-address (ash frame 12)))
         ;; Return page to normal use.
         (setf (physical-page-frame-type frame) :wired-backing)
         (values *snapshot-bounce-buffer-page*
                 nil
                 block-id
                 address))
        (t (panic "Frame " frame " for address " address " has non-writeback type "
                  (physical-page-frame-type frame)))))))

(defun snapshot-write-back-pages ()
  ;; Write dirty/copied pages back.
  (let ((n 0))
    (loop
       (when (eql n 0)
         (setf n 100)
         (debug-print-line *snapshot-pending-writeback-pages-count* " dirty pages to write back"))
       (decf n)
       (when (not *snapshot-pending-writeback-pages*) (return))
       (multiple-value-bind (frame freep block-id address)
           (pop-pending-snapshot-page)
         (declare (ignorable address))
         #+(or)(debug-print-line "Writing back page " frame "/" block-id "/" address)
         (or (snapshot-write-disk block-id (convert-to-pmap-address (ash frame 12)))
             (panic "Unable to write page to disk!"))
         (when freep
           (release-physical-pages frame 1))))))

(defun snapshot-write-disk (block data)
  (disk-submit-request *snapshot-disk-request*
                       *paging-disk*
                       :write
                       (* block
                          (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                       (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                       data)
  (disk-await-request *snapshot-disk-request*))

(defun snapshot-freelist ()
  (values (regenerate-store-freelist)
          (prog1 *store-deferred-freelist-head*
            (setf *store-deferred-freelist-head* '()))))

(defun snapshot-bml1 (bml1 address-part)
  (let ((bml1-disk (or (store-alloc 1)
                       (panic "Unable to allocate disk space for new block map.")))
        ;; Update the bottom level of the block map in-place.
        (bml1-memory bml1)
        (bml1-count 0)
        (next-address-part (ash address-part 9)))
    (dotimes (i 512)
      (let ((entry (sys.int::memref-unsigned-byte-64 bml1 i))
            (address (* (logior next-address-part i) +4k-page-size+)))
        (when (not (zerop entry))
          ;; Allocate any lazy blocks.
          (when (block-info-lazy-block-p entry)
            (ensure (block-info-committed-p entry) "Uncommitted lazy block.")
            (let ((new-block (or (store-alloc 1)
                                 (panic "Unable to allocate lazy block!"))))
              (decf *store-fudge-factor*)
              (let ((pte (get-pte-for-address address nil)))
                (when (and pte
                           (page-present-p pte 0))
                  (setf (physical-page-frame-block-id (ash (pte-physical-address (page-table-entry pte 0)) -12)) new-block)))
              (setf entry (logior (ash new-block sys.int::+block-map-id-shift+)
                                  (logand entry sys.int::+block-map-flag-mask+))
                    (sys.int::memref-unsigned-byte-64 bml1 i) entry)))
          ;; Uncommit any committed pages.
          (when (block-info-committed-p entry)
            (setf (sys.int::memref-unsigned-byte-64 bml1 i) (logand entry (lognot sys.int::+block-map-committed+)))
            (incf *store-fudge-factor*))
          (incf bml1-count))))
    (cond ((zerop bml1-count)
           ;; No entries, don't bother with this level.
           (store-free bml1-disk 1)
           nil)
          (t (snapshot-write-disk bml1-disk bml1-memory)
             (ash bml1-disk sys.int::+block-map-id-shift+)))))

(defun snapshot-block-map-outer-level (bml next-fn address-part)
  (let ((bml-disk (or (store-alloc 1)
                      (panic "Unable to allocate disk space for new block map.")))
        (bml-memory (convert-to-pmap-address (* (pager-allocate-page :new-type :other) +4k-page-size+)))
        (bml-count 0)
        (next-address-part (ash address-part 9)))
    (dotimes (i 512)
      (let* ((entry (sys.int::memref-signed-byte-64 bml i))
             (disk-entry (if (zerop entry)
                             nil
                             (funcall next-fn entry (logior next-address-part i)))))
        (cond (disk-entry
               (setf (sys.int::memref-signed-byte-64 bml-memory i) disk-entry)
               (incf bml-count))
              (t
               (setf (sys.int::memref-signed-byte-64 bml-memory i) 0)))))
    (prog1
        (cond ((zerop bml-count)
               ;; No entries, don't bother with this level.
               (store-free bml-disk 1)
               nil)
              (t (snapshot-write-disk bml-disk bml-memory)
                 (ash bml-disk sys.int::+block-map-id-shift+)))
      (free-page bml-memory))))

(defun snapshot-bml2 (bml2 address-part)
  (snapshot-block-map-outer-level bml2 #'snapshot-bml1 address-part))

(defun snapshot-bml3 (bml3 address-part)
  (snapshot-block-map-outer-level bml3 #'snapshot-bml2 address-part))

(defun snapshot-bml4 (bml4)
  (snapshot-block-map-outer-level bml4 #'snapshot-bml3 0))

(defun snapshot-block-map ()
  (ash (snapshot-bml4 *bml4*) (- sys.int::+block-map-id-shift+)))

(defun take-snapshot ()
  (when *paging-read-only*
    (debug-print-line "Not taking snapshot, running read-only.")
    (return-from take-snapshot))
  ;; TODO: Ensure there is a free area of at least 64kb in the wired area.
  ;; That should be enough to boot the system.
  (set-snapshot-light t)
  (setf *snapshot-pending-writeback-pages* nil
        *snapshot-pending-writeback-pages-count* 0)
  (let ((freelist-block nil)
        (bml4-block nil)
        (previously-deferred-free-blocks nil))
    ;; Stop the world before taking the *VM-LOCK*. There may be PA threads waiting for pages.
    (with-world-stopped ()
      (when (not (zerop *snapshot-inhibit*))
        (set-snapshot-light nil)
        (return-from take-snapshot :retry))
      (debug-print-line "Begin snapshot.")
      (with-rw-lock-write (*vm-lock*)
        (debug-print-line "deferred blocks: " *store-freelist-n-deferred-free-blocks*)
        (debug-print-line "Copying wired area.")
        (snapshot-copy-wired-area)
        (debug-print-line "Marking dirty pages copy-on-write.")
        (snapshot-mark-cow-dirty-pages)
        ;; FIXME: Disk writes are slow and should be done outside WITH-WORLD-STOPPED.
        ;; FIXME!! Need to free the old block map & freelist.
        (debug-print-line "Updating block map.")
        (setf bml4-block (snapshot-block-map))
        (debug-print-line "Updating freelist.")
        (setf (values freelist-block previously-deferred-free-blocks)
              (snapshot-freelist))))
    (snapshot-write-back-pages)
    ;; Update the block map & freelist entries in the header.
    (debug-print-line "Updating disk header.")
    (let ((header (convert-to-pmap-address (* (with-rw-lock-write (*vm-lock*)
                                                (pager-allocate-page :new-type :other))
                                            +4k-page-size+))))
      (disk-submit-request *snapshot-disk-request*
                           *paging-disk*
                           :read
                           0
                           (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                           header)
      (unless (disk-await-request *snapshot-disk-request*)
        (panic "Unable to read header from disk"))
      (setf (sys.int::memref-unsigned-byte-64 (+ header +image-header-block-map+) 0) bml4-block)
      (setf (sys.int::memref-unsigned-byte-64 (+ header +image-header-freelist+) 0) freelist-block)
      (snapshot-write-disk 0 header)
      (free-page header))
    (with-rw-lock-write (*vm-lock*)
      (store-release-deferred-blocks previously-deferred-free-blocks)))
  (set-snapshot-light nil)
  (debug-print-line "End snapshot."))

(defun current-snapshot-epoch ()
  "Return an event that identifies the current snapshot epoch.
This event will be signalled when the epoch changes."
  *snapshot-epoch*)

(defun snapshot-thread ()
  ;; A preallocated timer is required here because the snapshot thread
  ;; runs at :supervisor priority and must not cons outside of boot.
  (with-timer (snapshot-retry-timer)
    (loop
       ;; Retry occurs when *SNAPSHOT-INHIBIT* is non-zero.
       (loop
          while (or (not (eql *snapshot-inhibit* 0))
                    (eql (take-snapshot) :retry))
          do (timer-sleep snapshot-retry-timer 0.1))
       ;; After taking a snapshot, clear *snapshot-in-progress*
       ;; and go back to sleep.
       ;; FIXME: There's a race between setting this event and the thread
       ;; going to sleep. (SETF EVENT-STATE) can't be called with the
       ;; global thread-lock held.
       (setf (event-state *snapshot-state*) t)
       ;; Move to the next epoch.
       (setf (event-state *snapshot-epoch*) t)
       (setf *snapshot-epoch* *snapshot-next-epoch*)
       (%disable-interrupts)
       (acquire-global-thread-lock)
       (setf *snapshot-in-progress* nil)
       (setf (thread-state sys.int::*snapshot-thread*) :sleeping
             (thread-wait-item sys.int::*snapshot-thread*) "Snapshot")
       (%run-on-wired-stack-without-interrupts (sp fp)
         (%reschedule-via-wired-stack sp fp)))))

(defun allocate-snapshot-wired-backing-pages (start end &key sparse)
  (map-ptes
   start end
   (dx-lambda (wired-page pte)
     (when (not pte)
       (panic "No page table entry for wired page " wired-page))
     (let* ((page-frame (and (page-present-p pte)
                             (ash (pte-physical-address (sys.int::memref-unsigned-byte-64 pte 0)) -12))))
       (when page-frame
         ;; ### Could disable snapshotting if this can't be allocated.
         (let ((frame (allocate-physical-pages 1
                                               :mandatory-p "wired backing pages"
                                               :type :wired-backing)))
           (setf (physical-page-frame-next page-frame) frame)
           (setf (physical-page-virtual-address frame) wired-page)))))
   :sparse sparse))

(defun initialize-snapshot ()
  (when (not (boundp '*snapshot-state*))
    (setf *snapshot-state* (make-event :name 'snapshot-not-in-progress)))
  (setf (event-state *snapshot-state*) nil)
  (cond ((boundp '*snapshot-epoch*)
         (setf (event-state *snapshot-epoch*) t)
         (setf *snapshot-epoch* *snapshot-next-epoch*))
        (t
         (setf *snapshot-epoch* (make-event :name 'snapshot-epoch))))
  (setf *snapshot-disk-request* (make-disk-request))
  (setf *snapshot-in-progress* nil)
  (setf *snapshot-inhibit* 1)
  (setf *enable-snapshot-cow-fast-path* nil)
  ;; Allocate pages to copy the wired area into.
  ;; TODO: Use 2MB pages when possible.
  ;; ### when the wired area expands this will need to be something...
  (allocate-snapshot-wired-backing-pages sys.int::*wired-area-base* sys.int::*wired-area-bump*)
  (allocate-snapshot-wired-backing-pages sys.int::*wired-function-area-limit* sys.int::*function-area-base*)
  ;; FIXME: The card table is mostly sparse. The system spends ages scanning
  ;; it during boot looking for allocated regions but mostly doing nothing.
  ;; Could modify the bootloader to allocate backing pages.
  (allocate-snapshot-wired-backing-pages sys.int::+card-table-base+
                                         (+ sys.int::+card-table-base+ sys.int::+card-table-size+)
                                         :sparse t)
  ;; ### same here.
  (setf *snapshot-bounce-buffer-page* (allocate-physical-pages 1
                                                               :mandatory-p "snapshot bounce page")))

(defun snapshot ()
  ;; Run a GC before snapshotting to reduce the amount of space required.
  (sys.int::gc :full t)
  ;; Attempt to wake the snapshot thread, only waking it if
  ;; there is not snapshot currently in progress.
  ;; FIXME: locking for SMP.
  (let* ((next-epoch (make-event :name 'snapshot-epoch))
         (did-wake (safe-without-interrupts ()
                     (let ((was-in-progress (sys.int::cas (sys.int::symbol-global-value '*snapshot-in-progress*) nil t)))
                       (when (eql was-in-progress nil)
                         (setf (event-state *snapshot-state*) nil)
                         (setf *snapshot-next-epoch* next-epoch)
                         (wake-thread sys.int::*snapshot-thread*)
                         t)))))
    (when did-wake
      (thread-yield))))

(defun wait-for-snapshot-completion ()
  "If a snapshot is currently being take, then wait for it to complete."
  (event-wait *snapshot-state*))

(defmacro with-snapshot-inhibited (options &body body)
  `(call-with-snapshot-inhibited (dx-lambda () ,@body) ,@options))

(defun call-with-snapshot-inhibited (fn)
  ;; FIXME: Switch to ATOMIC-INCF/-DECF. Needs xcompiler fixes for declaim.
  (sys.int::%atomic-fixnum-add-symbol '*snapshot-inhibit* 1)
  (unwind-protect
       (funcall fn)
    (sys.int::%atomic-fixnum-add-symbol '*snapshot-inhibit* -1)))
