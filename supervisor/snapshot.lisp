;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defvar *snapshot-in-progress* nil)

(defvar *snapshot-bounce-buffer-page*)
(defvar *snapshot-pending-writeback-pages-count*)
(defvar *snapshot-pending-writeback-pages*)

(defun snapshot-block-cache ()
  ;; Write all block metadata (block cache) back.
  ;; TODO: When cache dirtiness is implemented, only write back dirty pages.
  (do ((cache-page *block-cache* (sys.int::memref-t cache-page 511)))
      ((null cache-page))
    (dotimes (i 255)
      (let ((block-id (sys.int::memref-unsigned-byte-64 cache-page (* i 2)))
            (address (sys.int::memref-signed-byte-64 cache-page (1+ (* i 2)))))
        (when (zerop address)
          (return))
        #+(or)(debug-print-line "Writing back block " block-id "/" address)
        (or (disk-write *paging-disk*
                        (* block-id
                           (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                        (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                        address)
            (panic "Unable to write page to disk! Everything is fucked, sorry."))))))

;;; (destination source)
(sys.int::define-lap-function %fast-page-copy ()
  (sys.lap-x86:mov64 :rdi :r8)
  (sys.lap-x86:mov64 :rsi :r9)
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:sar64 :rsi #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov32 :ecx 512)
  (sys.lap-x86:rep)
  (sys.lap-x86:movs64)
  (sys.lap-x86:ret))

(defun snapshot-add-writeback-frame (frame)
  ;; Mark frame as pending writeback.
  (setf (ldb (byte 1 +page-frame-flag-writeback+) (physical-page-frame-flags frame)) 1)
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

(defun snapshot-copy-wired-area ()
  ;; FIXME: Only copy dirty pages.
  ;; I bet this could be partially done with CoW. Evil.
  (without-interrupts
    (loop
       for i from #x200000 below sys.int::*wired-area-bump* by #x1000
       for pte = (or (get-pte-for-address i nil)
                     (panic "No page table entry for " i))
       for page-frame = (ash (sys.int::memref-unsigned-byte-64 pte 0) -12)
       for other-frame = (physical-page-frame-next page-frame)
       do
         (%fast-page-copy (+ +physical-map-base+ (ash other-frame 12)) i)
         (snapshot-add-writeback-frame other-frame))))

(defun snapshot-mark-cow-dirty-pages ()
  ;; Mark all non-wired dirty pages as read-only and set their CoW bits.
  (let ((pml4 (+ +physical-map-base+ (logand (sys.int::%cr3) (lognot #xFFF)))))
    (labels ((mark-pml4e-cow (pml4e)
               ;(debug-print-line " PML4e " pml4e)
               (let* ((entry (sys.int::memref-unsigned-byte-64 pml4 pml4e))
                      (pml3 (+ +physical-map-base+
                               (logand entry +page-table-address-mask+))))
                 (when (and (logtest entry +page-table-present+)
                            (logtest entry +page-table-accessed+))
                   (dotimes (i 512)
                     (mark-pml3e-cow pml3 i))
                   ;; Clear accessed bit.
                   (setf (sys.int::memref-unsigned-byte-64 pml4 pml4e)
                         (logand entry (lognot +page-table-accessed+))))))
             (mark-pml3e-cow (pml3 pml3e)
               ;(debug-print-line "  PML3e " pml3e "  " pml3)
               (let* ((entry (sys.int::memref-unsigned-byte-64 pml3 pml3e))
                      (pml2 (+ +physical-map-base+
                               (logand entry +page-table-address-mask+))))
                 (when (and (logtest entry +page-table-present+)
                            (logtest entry +page-table-accessed+))
                   (dotimes (i 512)
                     (mark-pml2e-cow pml2 i))
                   ;; Clear accessed bit.
                   (setf (sys.int::memref-unsigned-byte-64 pml3 pml3e)
                         (logand entry (lognot +page-table-accessed+))))))
             (mark-pml2e-cow (pml2 pml2e)
               ;(debug-print-line "   PML2e " pml2e "  " pml2)
               (let* ((entry (sys.int::memref-unsigned-byte-64 pml2 pml2e))
                      (pml1 (+ +physical-map-base+
                               (logand entry +page-table-address-mask+))))
                 (when (and (logtest entry +page-table-present+)
                            (logtest entry +page-table-accessed+))
                   (dotimes (i 512)
                     (mark-pml1e-cow pml1 i))
                   ;; Clear accessed bit.
                   (setf (sys.int::memref-unsigned-byte-64 pml2 pml2e)
                         (logand entry (lognot +page-table-accessed+))))))
             (mark-pml1e-cow (pml1 pml1e)
               (let ((entry (sys.int::memref-unsigned-byte-64 pml1 pml1e)))
                 ;(debug-print-line "    PML1e " pml1e "  " pml1 "  " entry)
                 (when (logtest entry +page-table-copy-on-write+)
                   (panic "Page table entry marked CoW when it shouldn't be."))
                 (when (logtest entry +page-table-dirty+)
                   (snapshot-add-writeback-frame (ash (sys.int::memref-unsigned-byte-64 pml1 pml1e) -12))
                   ;; Clear dirty and writable bits, set copy-on-write bit.
                   (setf (sys.int::memref-unsigned-byte-64 pml1 pml1e)
                         (logand (logior entry
                                         +page-table-copy-on-write+)
                                 (lognot +page-table-write+)
                                 (lognot +page-table-dirty+)))))))
      ;; Skip wired area, entry 0.
      (loop for i from 1 below 64 ; pinned area to wired stack area.
         do (mark-pml4e-cow i))
      ;; Skip wired stack area, entry 64.
      (loop for i from 65 below 256 ; stack area to end of persistent memory.
         do (mark-pml4e-cow i))
      ;; Cover the part of the pinned area that got missed as well.
      (let ((pml3 (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 pml4 0) +page-table-address-mask+))))
        ;; Skip first 2 entries, the wired area.
        (loop for i from 2 below 512
           do (mark-pml3e-cow pml3 i)))))
  (flush-tlb))

(defun snapshot-clone-cow-page-via-page-fault (fault-addr)
  (let* ((new-frame (allocate-physical-pages 1 "CoW page"))
         (pte (or (get-pte-for-address fault-addr nil)
                  (panic "No PTE for CoW address?")))
         (old-frame (ash (sys.int::memref-unsigned-byte-64 pte 0) -12)))
    (%fast-page-copy (+ +physical-map-base+ (ash new-frame 12))
                     (logand fault-addr (lognot #xFFF)))
    ;; Replace the writeback-pages list entry with the newly copied page.
    ;; (Or modify the PTE instead?)
    (setf (physical-page-frame-next new-frame) (physical-page-frame-next old-frame)
          (physical-page-frame-prev new-frame) (physical-page-frame-prev old-frame))
    (when (physical-page-frame-next old-frame)
      (setf (physical-page-frame-prev (physical-page-frame-next old-frame)) new-frame))
    (when (physical-page-frame-prev old-frame)
      (setf (physical-page-frame-next (physical-page-frame-prev old-frame)) new-frame))
    (when (eql *snapshot-pending-writeback-pages* old-frame)
      (setf *snapshot-pending-writeback-pages* new-frame))
    ;; Flags update.
    (setf (physical-page-frame-flags new-frame) (logior (logand fault-addr (lognot #xFFF))
                                                        (ash 1 +page-frame-flag-writeback+)))
    (setf (physical-page-frame-block-id new-frame) (physical-page-frame-block-id old-frame))
    (setf (ldb (byte 1 +page-frame-flag-writeback+) (physical-page-frame-flags old-frame)) 0)
    ;; Update PTE bits. Clear CoW bit, make writable.
    (setf (sys.int::memref-unsigned-byte-64 pte 0)
          (logior (logand (sys.int::memref-unsigned-byte-64 pte 0)
                          (lognot +page-table-copy-on-write+))
                  +page-table-write+))
    (sys.int::%invlpg fault-addr)
    #+(or)(debug-print-line "Copied page " fault-addr)))

(defun pop-pending-snapshot-page ()
  (with-mutex (*vm-lock*)
    (without-interrupts
      (let* ((frame *snapshot-pending-writeback-pages*)
             (flags (physical-page-frame-flags frame))
             (address (logand flags (lognot #xFFF))))
        ;; Remove frame from list.
        (setf *snapshot-pending-writeback-pages* (physical-page-frame-next frame))
        (when *snapshot-pending-writeback-pages*
          (setf (physical-page-frame-prev *snapshot-pending-writeback-pages*) nil))
        (decf *snapshot-pending-writeback-pages-count*)
        (when (not (logbitp +page-frame-flag-writeback+ (physical-page-frame-flags frame)))
          (panic "Page " frame " for address " (logand (physical-page-frame-flags frame) (lognot #xFFF)) " not marked as writeback."))
        (setf (ldb (byte 1 +page-frame-flag-writeback+) (physical-page-frame-flags frame)) 0)
        (cond
          ((logbitp +page-frame-flag-cache+ flags)
           ;; If the page is in use as a cache page, copy it to the bounce buffer.
           (%fast-page-copy (+ +physical-map-base+ (ash *snapshot-bounce-buffer-page* 12))
                            (+ +physical-map-base+ (ash frame 12)))
           ;; Allow access to the page again.
           (let ((pte (or (get-pte-for-address address nil)
                          (panic "No PTE for CoW address?"))))
             ;; Update PTE bits. Clear CoW bit, make writable.
             (setf (sys.int::memref-unsigned-byte-64 pte 0)
                   (logior (logand (sys.int::memref-unsigned-byte-64 pte 0)
                                   (lognot +page-table-copy-on-write+))
                           +page-table-write+))
             (sys.int::%invlpg address))
           ;; Don't free the bounce page.
           (values *snapshot-bounce-buffer-page* nil (physical-page-frame-block-id frame) address))
          (t
           ;; Page is either a wired bounce page or a copied page.
           (values frame
                   ;; Don't free wired bounce pages.
                   (not (< address (* 2 1024 1024 1024)))
                   (physical-page-frame-block-id frame)
                   address)))))))

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
         #+(or)(debug-print-line "Writing back page " frame "/" block-id "/" address)
         (or (disk-write *paging-disk*
                         (* block-id
                            (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                         (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                         (+ +physical-map-base+ (ash frame 12)))
             (panic "Unable to write page to disk! Everything is fucked, sorry."))
         (when freep
           (release-physical-pages frame 1))))))

(defun take-snapshot ()
  (debug-write-line "Begin snapshot.")
  ;; TODO: Ensure there is a free area of at least 64kb in the wired area.
  ;; This'll be enough to boot the system.
  (set-snapshot-light t)
  (setf *snapshot-pending-writeback-pages* nil
        *snapshot-pending-writeback-pages-count* 0)
  (with-mutex (*vm-lock*)
    (with-world-stopped
      (regenerate-store-freelist)
      (snapshot-block-cache)
      (snapshot-copy-wired-area)
      (snapshot-mark-cow-dirty-pages)))
  (snapshot-write-back-pages)
  (set-snapshot-light nil)
  (debug-write-line "End snapshot."))

(defun snapshot-thread ()
  (loop
     (take-snapshot)
     ;; After taking a snapshot, clear *snapshot-in-progress*
     ;; and go back to sleep.
     (%lock-thread sys.int::*snapshot-thread*)
     (setf *snapshot-in-progress* nil)
     (setf (thread-state sys.int::*snapshot-thread*) :sleeping
           (thread-wait-item sys.int::*snapshot-thread*) "Snapshot")
     (%reschedule)))

(defun initialize-snapshot ()
  (setf *snapshot-in-progress* nil)
  ;; Allocate pages to copy the wired area into.
  ;; TODO: Use 2MB pages when possible.
  ;; ### when the wired area expands this will need to be something...
  (loop
     for i from #x200000 below sys.int::*wired-area-bump* by #x1000
     ;; ### Could disable snapshotting if this can't be allocated.
     do (let* ((frame (allocate-physical-pages 1 "wired backing pages"))
               (pte (or (get-pte-for-address i nil)
                        (panic "No page table entry for " i)))
               (page-frame (ash (sys.int::memref-unsigned-byte-64 pte 0) -12)))
          (setf (physical-page-frame-next page-frame) frame)
          (setf (physical-page-frame-flags frame) i)
          (setf (physical-page-frame-block-id frame) (physical-page-frame-block-id page-frame))))
  ;; ### same here.
  (setf *snapshot-bounce-buffer-page* (allocate-physical-pages 1 "snapshot bounce page")))

(defun snapshot ()
  ;; Attempt to wake the snapshot thread, only waking it if
  ;; there is not snapshot currently in progress.
  (let ((did-wake nil))
    (with-thread-lock (sys.int::*snapshot-thread*)
      (when (not *snapshot-in-progress*)
        (setf *snapshot-in-progress* t
              did-wake t)
        (setf (thread-state sys.int::*snapshot-thread*) :runnable)
        (with-symbol-spinlock (*global-thread-lock*)
          (push-run-queue sys.int::*snapshot-thread*))))
    (when did-wake
      (thread-yield))))
