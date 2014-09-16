(in-package :mezzanine.supervisor)

(defvar *snapshot-in-progress* nil)

(defun write-back-page (address)
  "Write pack the page at ADDRESS if it has a backing block."
  ;; Perform stack address aliasing.
  (when (and (logbitp sys.int::+address-mark-bit+ address)
             (eql (ldb (byte sys.int::+address-tag-size+ sys.int::+address-tag-shift+) address)
                  sys.int::+address-tag-stack+))
    (setf (ldb (byte 1 sys.int::+address-mark-bit+) address) 0))
  (let ((block-info (block-info-for-virtual-address address)))
    ;; Sanity checking
    (cond ((logbitp 47 address)
           (when block-info
             (debug-write-line "Blech, page has backing block but probably shouldn't have.")))
          (t
           (unless block-info
             (debug-write-line "Blech, page doesn't have backing block but probably should have."))))
    (when block-info
      (or (funcall (disk-write-fn *paging-disk*)
                   (disk-device *paging-disk*)
                   (* (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) block-info)
                      (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                   (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                   address)
          (progn (debug-write-line "Unable to write page to disk! Everything is fucked, sorry.")
                 (loop))))))

(defun take-snapshot ()
  (debug-write-line "Begin snapshot.")
  (with-mutex (*vm-lock*)
    (with-world-stopped
      ;; Write all dirty pages to disk.
      ;; FIXME: Should do this using CoW. Interrupts may modify state during writeback.
      (let ((pml4 (+ +physical-map-base+ (logand (sys.int::%cr3) (lognot #xFFF)))))
        (dotimes (pml4e 256) ; ### only the low half of the address space needs to be scanned atm.
          (when (and (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pml4 pml4e))
                     (logtest +page-table-accessed+ (sys.int::memref-unsigned-byte-64 pml4 pml4e)))
            (let ((pml3 (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 pml4 pml4e) +page-table-address-mask+))))
              (dotimes (pml3e 512)
                (when (and (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pml3 pml3e))
                           (logtest +page-table-accessed+ (sys.int::memref-unsigned-byte-64 pml3 pml3e)))
                  (let ((pml2 (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 pml3 pml3e) +page-table-address-mask+))))
                    (dotimes (pml2e 512)
                      (when (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pml2 pml2e))
                        (cond ((logtest +page-table-page-size+ (sys.int::memref-unsigned-byte-64 pml2 pml2e))
                               (when (logtest +page-table-dirty+ (sys.int::memref-unsigned-byte-64 pml2 pml2e))
                                 (dotimes (pml1e 512)
                                   (write-back-page (logior (ash pml4e 39)
                                                            (ash pml3e 30)
                                                            (ash pml2e 21)
                                                            (ash pml1e 12))))
                                 (setf (sys.int::memref-unsigned-byte-64 pml2 pml2e)
                                       (logand (sys.int::memref-unsigned-byte-64 pml2 pml2e) (lognot +page-table-dirty+)))))
                              ((logtest +page-table-accessed+ (sys.int::memref-unsigned-byte-64 pml2 pml2e))
                               (let ((pml1 (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 pml2 pml2e) +page-table-address-mask+))))
                                 (dotimes (pml1e 512)
                                   (when (and (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pml1 pml1e))
                                              (logtest +page-table-dirty+ (sys.int::memref-unsigned-byte-64 pml1 pml1e)))
                                     (write-back-page (logior (ash pml4e 39)
                                                              (ash pml3e 30)
                                                              (ash pml2e 21)
                                                              (ash pml1e 12)))
                                     (setf (sys.int::memref-unsigned-byte-64 pml1 pml1e)
                                           (logand (sys.int::memref-unsigned-byte-64 pml1 pml1e) (lognot +page-table-dirty+))))))
                               (setf (sys.int::memref-unsigned-byte-64 pml2 pml2e)
                                     (logand (sys.int::memref-unsigned-byte-64 pml2 pml2e) (lognot +page-table-accessed+))))))))
                  (setf (sys.int::memref-unsigned-byte-64 pml3 pml3e)
                        (logand (sys.int::memref-unsigned-byte-64 pml3 pml3e) (lognot +page-table-accessed+))))))
            (setf (sys.int::memref-unsigned-byte-64 pml4 pml4e)
                  (logand (sys.int::memref-unsigned-byte-64 pml4 pml4e) (lognot +page-table-accessed+))))))
      ;; Write all block metadata (block cache) back.
      ))
  (debug-write-line "End snapshot."))

(defun snapshot-thread ()
  (loop
     (take-snapshot)
     ;; After taking a snapshot, clear *snapshot-in-progress*
     ;; and go back to sleep.
     (%lock-thread sys.int::*snapshot-thread*)
     (setf *snapshot-in-progress* nil)
     (setf (thread-state sys.int::*snapshot-thread*) :sleeping)
     (%reschedule)))

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
