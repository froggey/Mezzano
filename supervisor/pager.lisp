(in-package :mezzanine.supervisor)

(defstruct (disk
             (:area :wired))
  device
  n-sectors
  sector-size
  max-transfer
  read-fn
  write-fn)

(defvar *vm-lock*)

(defvar *pager-waiting-threads*)
(defvar *pager-current-thread*)
(defvar *pager-lock*)

(defvar *disks*)

(defvar *paging-disk*)
(defvar *bml4-block*)

(defconstant +page-table-present+        #x001)
(defconstant +page-table-write+          #x002)
(defconstant +page-table-user+           #x004)
(defconstant +page-table-write-through+  #x008)
(defconstant +page-table-cache-disabled+ #x010)
(defconstant +page-table-accessed+       #x020)
(defconstant +page-table-dirty+          #x040)
(defconstant +page-table-page-size+      #x080)
(defconstant +page-table-global+         #x100)
(defconstant +page-table-address-mask+   #x000FFFFFFFFFF000)

(defun register-disk (device n-sectors sector-size max-transfer read-fn write-fn)
  (when (> sector-size +4k-page-size+)
    (debug-write-line "Ignoring device with sector size larger than 4k.")
    (return-from register-disk))
  (let ((disk (make-disk :device device
                         :sector-size sector-size
                         :n-sectors n-sectors
                         :max-transfer max-transfer
                         :read-fn read-fn
                         :write-fn write-fn)))
    (setf *disks* (sys.int::cons-in-area disk *disks* :wired))))

(defun detect-paging-disk ()
  (dolist (disk *disks*)
    (let* ((sector-size (disk-sector-size disk))
           (read-fn (disk-read-fn disk))
           (device (disk-device disk))
           (page (or (allocate-physical-pages (ceiling (max +4k-page-size+ sector-size) +4k-page-size+))
                     ;; I guess this could happen on strange devices with sector sizes > 4k.
                     (error "Unable to allocate memory when examining device ~S!" device)))
           (page-addr (+ +physical-map-base+ (* page +4k-page-size+))))
      (setf *disks* (sys.int::cons-in-area disk *disks* :wired))
      ;; Read first 4k, figure out what to do with it.
      (or (funcall read-fn device 0 (ceiling +4k-page-size+ sector-size) page-addr)
          (progn
            (release-physical-pages page (ceiling (max +4k-page-size+ sector-size) +4k-page-size+))
            (error "Unable to read first block on device ~S!" device)))
      ;; Search for a Mezzanine header here.
      ;; TODO: Scan for partition maps.
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
        (debug-write-line "Found boot image!")
        (setf *paging-disk* disk)
        (setf *bml4-block* (sys.int::memref-unsigned-byte-64 (+ page-addr 96) 0))
        (initialize-store-freelist (sys.int::memref-unsigned-byte-64 (+ page-addr 64) 0)
                                   (sys.int::memref-unsigned-byte-64 (+ page-addr 104) 0)))
      ;; Release the pages.
      (release-physical-pages page (ceiling (max +4k-page-size+ sector-size) +4k-page-size+)))))

(defun get-pte-for-address (address &optional (allocate t))
  (let ((cr3 (+ +physical-map-base+ (logand (sys.int::%cr3) (lognot #xFFF))))
        (pml4e (ldb (byte 9 39) address))
        (pdpe (ldb (byte 9 30) address))
        (pde (ldb (byte 9 21) address))
        (pte (ldb (byte 9 12) address)))
    (when (not (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 cr3 pml4e)))
      ;; No PDP. Allocate one.
      (when (not allocate)
        (return-from get-pte-for-address nil))
      (let* ((frame (or (allocate-physical-pages 1)
                        (panic "Aiee. No memory.")))
             (addr (+ +physical-map-base+ (ash frame 12))))
        (dotimes (i 512)
          (setf (sys.int::memref-unsigned-byte-64 addr i) 0))
        (setf (sys.int::memref-unsigned-byte-64 cr3 pml4e) (logior (ash frame 12)
                                                                   +page-table-present+
                                                                   +page-table-write+))))
    (let ((pdp (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 cr3 pml4e) +page-table-address-mask+))))
      (when (not (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pdp pdpe)))
        ;; No PDir. Allocate one.
        (when (not allocate)
          (return-from get-pte-for-address nil))
        (let* ((frame (or (allocate-physical-pages 1)
                          (panic "Aiee. No memory.")))
               (addr (+ +physical-map-base+ (ash frame 12))))
          (dotimes (i 512)
            (setf (sys.int::memref-unsigned-byte-64 addr i) 0))
          (setf (sys.int::memref-unsigned-byte-64 pdp pdpe) (logior (ash frame 12)
                                                                    +page-table-present+
                                                                    +page-table-write+))))
      (let ((pdir (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 pdp pdpe) +page-table-address-mask+))))
        (when (not (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pdir pde)))
          ;; No PT. Allocate one.
          (when (not allocate)
            (return-from get-pte-for-address nil))
          (let* ((frame (or (allocate-physical-pages 1)
                            (panic "Aiee. No memory.")))
                 (addr (+ +physical-map-base+ (ash frame 12))))
            (dotimes (i 512)
              (setf (sys.int::memref-unsigned-byte-64 addr i) 0))
            (setf (sys.int::memref-unsigned-byte-64 pdir pde) (logior (ash frame 12)
                                                                      +page-table-present+
                                                                      +page-table-write+))))
        (let ((pt (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 pdir pde) +page-table-address-mask+))))
          (+ pt (* pte 8)))))))

(defun insert-into-block-cache (block-id addr)
  ;; Insert it into the cache.
  (do ((cache-page *block-cache* (sys.int::memref-t cache-page 511)))
      ((null cache-page)
       ;; Expand the cache.
       (let* ((frame (or (allocate-physical-pages 1)
                         (panic "Aiee. No memory.")))
              (cache-addr (+ +physical-map-base+ (ash frame 12))))
         (dotimes (i 512)
           (setf (sys.int::memref-unsigned-byte-64 cache-addr i) 0))
         (setf (sys.int::memref-t cache-addr 511) *block-cache*
               *block-cache* cache-addr)
         (setf (sys.int::memref-unsigned-byte-64 cache-addr 0) block-id
               (sys.int::memref-signed-byte-64 cache-addr 1) addr)))
    (dotimes (i 255)
      (when (eql (sys.int::memref-unsigned-byte-64 cache-page (* i 2)) 0)
        (setf (sys.int::memref-unsigned-byte-64 cache-page (* i 2)) block-id
              (sys.int::memref-signed-byte-64 cache-page (1+ (* i 2))) addr)
        (return-from insert-into-block-cache)))))

(defun read-cached-block-from-disk (block-id)
  (let* ((frame (or (allocate-physical-pages 1)
                    (panic "Aiee. No memory.")))
         (addr (+ +physical-map-base+ (ash frame 12))))
    (unless (funcall (disk-read-fn *paging-disk*)
                     (disk-device *paging-disk*)
                     (* block-id (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                     (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                     addr)
      (panic "Unable to read page from disk"))
    (insert-into-block-cache block-id addr)
    ;; TODO: clear addr's dirty bit.
    addr))

(defun zero-cached-block (block-id)
  ;; Scan the block cache.
  (do ((cache-page *block-cache* (sys.int::memref-t cache-page 511)))
      ((null cache-page)
       ;; Not present in cache, allocate a new page and insert it.
       (let* ((frame (or (allocate-physical-pages 1)
                         (panic "Aiee. No memory.")))
              (data (+ +physical-map-base+ (ash frame 12))))
         (dotimes (i 512)
           (setf (sys.int::memref-unsigned-byte-64 data i) 0))
         (insert-into-block-cache block-id data)
         data))
    (dotimes (i 255)
      (when (eql (sys.int::memref-unsigned-byte-64 cache-page (* i 2)) block-id)
        (let ((data (sys.int::memref-signed-byte-64 cache-page (1+ (* i 2)))))
          (dotimes (i 512)
            (setf (sys.int::memref-unsigned-byte-64 data i) 0))
          (return-from zero-cached-block data))))))

(defun read-cached-block (block-id)
  ;; Scan the block cache.
  (do ((cache-page *block-cache* (sys.int::memref-t cache-page 511)))
      ((null cache-page)
       (read-cached-block-from-disk block-id))
    (dotimes (i 255)
      (when (eql (sys.int::memref-unsigned-byte-64 cache-page (* i 2)) block-id)
        (return-from read-cached-block (sys.int::memref-signed-byte-64 cache-page (1+ (* i 2))))))))

(defun block-info-for-virtual-address (address)
  (flet ((read-level (map entry)
           ;; Stop if there is no block, or if the level is not present.
           (let* ((info (sys.int::memref-unsigned-byte-64 map entry))
                  (id (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) info)))
             (when (or (zerop id)
                       (not (logtest sys.int::+block-map-present+ info)))
               (return-from block-info-for-virtual-address nil))
             (read-cached-block id))))
    (let* ((bml4e (ldb (byte 9 39) address))
           (bml3e (ldb (byte 9 30) address))
           (bml2e (ldb (byte 9 21) address))
           (bml1e (ldb (byte 9 12) address))
           (bml4 (read-cached-block *bml4-block*))
           (bml3 (read-level bml4 bml4e))
           (bml2 (read-level bml3 bml3e))
           (bml1 (read-level bml2 bml2e)))
      (when (zerop (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+)
                        (sys.int::memref-unsigned-byte-64 bml1 bml1e)))
        (return-from block-info-for-virtual-address nil))
      (sys.int::memref-unsigned-byte-64 bml1 bml1e))))

(defun allocate-new-block-for-virtual-address (address flags)
  (let ((new-block (store-alloc 1)))
    (when (not new-block)
      (panic "Aiiee, out of store."))
    ;; Update the block info for this address.
    (flet ((read-level (map entry)
             ;; Allocate a new block if there is no block.
             (let* ((info (sys.int::memref-unsigned-byte-64 map entry))
                    (id (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) info)))
               (when (zerop id)
                 (setf id (or (store-alloc 1)
                              (panic "Aiiee, out of store."))
                       (sys.int::memref-unsigned-byte-64 map entry) (logior (ash id sys.int::+block-map-id-shift+)
                                                                            sys.int::+block-map-present+))
                 (zero-cached-block id))
               (read-cached-block id))))
      (let* ((bml4e (ldb (byte 9 39) address))
             (bml3e (ldb (byte 9 30) address))
             (bml2e (ldb (byte 9 21) address))
             (bml1e (ldb (byte 9 12) address))
             (bml4 (read-cached-block *bml4-block*))
             (bml3 (read-level bml4 bml4e))
             (bml2 (read-level bml3 bml3e))
             (bml1 (read-level bml2 bml2e)))
        (when (not (zerop (sys.int::memref-unsigned-byte-64 bml1 bml1e)))
          (panic "Block " address " entry not zero!"))
        (setf (sys.int::memref-unsigned-byte-64 bml1 bml1e) (logior (ash new-block sys.int::+block-map-id-shift+)
                                                                    flags))))))

(defun release-block-at-virtual-address (address)
  ;; Update the block info for this address.
  (flet ((read-level (map entry)
           ;; Allocate a new block if there is no block.
           (let* ((info (sys.int::memref-unsigned-byte-64 map entry))
                  (id (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) info)))
             (when (zerop id)
               (setf id (or (store-alloc 1)
                            (panic "Aiiee, out of store."))
                     (sys.int::memref-unsigned-byte-64 map entry) (logior (ash id sys.int::+block-map-id-shift+)
                                                                          sys.int::+block-map-present+))
               (zero-cached-block id))
             (read-cached-block id))))
    (let* ((bml4e (ldb (byte 9 39) address))
           (bml3e (ldb (byte 9 30) address))
           (bml2e (ldb (byte 9 21) address))
           (bml1e (ldb (byte 9 12) address))
           (bml4 (read-cached-block *bml4-block*))
           (bml3 (read-level bml4 bml4e))
           (bml2 (read-level bml3 bml3e))
           (bml1 (read-level bml2 bml2e)))
      (when (zerop (sys.int::memref-unsigned-byte-64 bml1 bml1e))
        (panic "Block " address " entry is zero!"))
      (store-free (ash (sys.int::memref-unsigned-byte-64 bml1 bml1e) (- sys.int::+block-map-id-shift+)) 1)
      (setf (sys.int::memref-unsigned-byte-64 bml1 bml1e) 0))))

(defun set-address-flags (address flags)
  ;; Update the block info for this address.
  (flet ((read-level (map entry)
           ;; Allocate a new block if there is no block.
           (let* ((info (sys.int::memref-unsigned-byte-64 map entry))
                  (id (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) info)))
             (when (zerop id)
               (panic "Block " address " not present!"))
             (read-cached-block id))))
    (let* ((bml4e (ldb (byte 9 39) address))
           (bml3e (ldb (byte 9 30) address))
           (bml2e (ldb (byte 9 21) address))
           (bml1e (ldb (byte 9 12) address))
           (bml4 (read-cached-block *bml4-block*))
           (bml3 (read-level bml4 bml4e))
           (bml2 (read-level bml3 bml3e))
           (bml1 (read-level bml2 bml2e)))
      (when (zerop (sys.int::memref-unsigned-byte-64 bml1 bml1e))
        (panic "Block " address " not present!"))
      (setf (sys.int::memref-unsigned-byte-64 bml1 bml1e) (logior (logand (sys.int::memref-unsigned-byte-64 bml1 bml1e)
                                                                          (lognot sys.int::+block-map-flag-mask+))
                                                                  flags)))))

(defun release-memory-range (base length)
  (assert (zerop (logand (logior base length) #xFFF)) () "Range not page aligned.")
  (debug-print-line "Release range " base "-" (+ base length))
  (%stack-probe (* 32 1024))
  (with-mutex (*vm-lock*)
    (dotimes (i (truncate length #x1000))
      ;; Update block map.
      (release-block-at-virtual-address (+ base (* i #x1000)))
      ;; Update page tables and release pages if possible.
      (let ((pte (get-pte-for-address (+ base (* i #x1000)) nil)))
        (when (and pte (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pte 0)))
          (release-physical-pages (ash (sys.int::memref-unsigned-byte-64 pte 0) -12) 1)
          (setf (sys.int::memref-unsigned-byte-64 pte 0) 0))))
    ;; Flush TLB.
    (setf (sys.int::%cr3) (sys.int::%cr3))))

(defun protect-memory-range (base length flags)
  (assert (zerop (logand (logior base length) #xFFF)) () "Range not page aligned.")
  (assert (>= (+ base length) (* 2 1024 1024 1024)) () "Wired area can't be protected.")
  ;; Implementing this is a litle complicated. It'll need to keep dirty pages in memory.
  (assert (or (logtest sys.int::+block-map-present+ flags)
              (logtest sys.int::+block-map-zero-fill+ flags))
          () "TODO: Cannot mark block not-present without zero-fill")
  (debug-print-line "Protect range " base "-" (+ base length) "  " flags)
  (%stack-probe (* 32 1024))
  (with-mutex (*vm-lock*)
    (dotimes (i (truncate length #x1000))
      ;; Update block map.
      (set-address-flags (+ base (* i #x1000)) flags)
      ;; Update page tables and release pages if possible.
      (let ((pte (get-pte-for-address (+ base (* i #x1000)) nil)))
        (when (and pte (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pte 0)))
          (cond ((or (not (logtest sys.int::+block-map-present+ flags))
                     (logtest sys.int::+block-map-zero-fill+ flags))
                 ;; Page going away, but it's ok. It'll be back, zero-filled.
                 #+(or)(debug-print-line "  flush page " (+ base (* i #x1000)) "  " (sys.int::memref-unsigned-byte-64 pte 0))
                 (release-physical-pages (ash (sys.int::memref-unsigned-byte-64 pte 0) -12) 1)
                 (setf (sys.int::memref-unsigned-byte-64 pte 0) 0))
                ((logtest sys.int::+block-map-writable+ flags)
                 ;; Mark writable.
                 (setf (sys.int::memref-unsigned-byte-64 pte 0) (logior (sys.int::memref-unsigned-byte-64 pte 0)
                                                                        +page-table-write+)))
                (t
                 ;; Mark read-only.
                 (setf (sys.int::memref-unsigned-byte-64 pte 0) (logand (sys.int::memref-unsigned-byte-64 pte 0)
                                                                        (lognot +page-table-write+))))))))
    ;; Flush TLB.
    (setf (sys.int::%cr3) (sys.int::%cr3))))

(defun wait-for-page (address)
  (with-mutex (*vm-lock*)
    ;; Stack pages with the mark bit set are aliased to the same
    ;; address with the mark bit clear.
    (when (and (logbitp sys.int::+address-mark-bit+ address)
               (eql (ldb (byte sys.int::+address-tag-size+ sys.int::+address-tag-shift+) address)
                    sys.int::+address-tag-stack+))
      (setf address (logand address (lognot (ash 1 sys.int::+address-mark-bit+)))))
    (let ((pte (get-pte-for-address address))
          (block-info (block-info-for-virtual-address address)))
      ;; Examine the page table, if there's a present entry then the page
      ;; was mapped while acquiring the VM lock. Just return.
      (when (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pte 0))
        (return-from wait-for-page t))
      ;; Note that this test is done after the pte test. this is a hack to make
      ;; runtime-allocated stacks work before allocate-stack actually modifies the
      ;; block map.
      (when (or (not block-info)
                (not (logtest sys.int::+block-map-present+ block-info)))
        (return-from wait-for-page nil))
      ;; No page allocated. Allocate a page and read the data.
      (let* ((frame (or (allocate-physical-pages 1)
                        (progn (debug-write-line "Aiee. No memory.")
                               (loop))))
             (addr (+ +physical-map-base+ (ash frame 12))))
        (cond ((logtest sys.int::+block-map-zero-fill+ block-info)
               ;; Block is zero-filled.
               (dotimes (i 512)
                 (setf (sys.int::memref-unsigned-byte-64 addr i) 0))
               ;; Clear the zero-fill flag.
               (set-address-flags address (logand block-info
                                                  sys.int::+block-map-flag-mask+
                                                  (lognot sys.int::+block-map-zero-fill+))))
              (t ;; Block must be read from disk.
               (or (funcall (disk-read-fn *paging-disk*)
                            (disk-device *paging-disk*)
                            (* (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) block-info)
                               (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                            (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                            addr)
                   (panic "Unable to read page from disk"))))
        (setf (sys.int::memref-unsigned-byte-64 pte 0) (logior (ash frame 12)
                                                               +page-table-present+
                                                               (if (logtest sys.int::+block-map-writable+ block-info)
                                                                   +page-table-write+
                                                                   0)))
        ;; Perform stack aliasing at the PML4 level.
        (when (eql (ldb (byte sys.int::+address-tag-size+ sys.int::+address-tag-shift+) address)
                   sys.int::+address-tag-stack+)
          (let ((cr3 (+ +physical-map-base+ (logand (sys.int::%cr3) (lognot #xFFF))))
                (pml4e (ldb (byte 9 39) address))
                (other-pml4e (ldb (byte 9 39) (logior address (ash 1 sys.int::+address-mark-bit+)))))
            ;; Keep access bit set, if already set.
            (setf (sys.int::memref-unsigned-byte-64 cr3 other-pml4e) (logior (sys.int::memref-unsigned-byte-64 cr3 pml4e)
                                                                             (logand (sys.int::memref-unsigned-byte-64 cr3 other-pml4e)
                                                                                     +page-table-accessed+)))))))
    ;; Flush TLB.
    (setf (sys.int::%cr3) (sys.int::%cr3)))
  t)

(defun wait-for-page-via-interrupt (interrupt-frame address)
  "Called by the page fault handler when a non-present page is accessed.
It will put the thread to sleep, while it waits for the page."
  (let ((self (current-thread)))
    (acquire-mutex *pager-lock*)
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
          (push-run-queue sys.int::*pager-thread*))))
    (release-mutex *pager-lock*)
    (%reschedule-via-interrupt interrupt-frame)))

(defun map-physical-memory (base size name)
  ;; Page alignment required.
  (assert (zerop (logand base #xFFF)))
  (assert (zerop (logand size #xFFF)))
  (%stack-probe (* 32 1024))
  (with-mutex (*vm-lock*)
    (dotimes (i (truncate size #x1000))
      (let ((pte (get-pte-for-address (+ +physical-map-base+ base (* i #x1000)))))
        (when (not (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pte 0)))
          (setf (sys.int::memref-unsigned-byte-64 pte 0) (logior (+ base (* i #x1000))
                                                                 +page-table-present+
                                                                 +page-table-write+)))))))

(defun initialize-pager ()
  (when (not (boundp '*pager-waiting-threads*))
    (setf *pager-waiting-threads* '()
          *pager-current-thread* nil
          *pager-lock* (make-mutex "Pager lock" :spin)))
  (when *pager-current-thread*
    ;; Push any current thread back on the waiting threads list.
    (setf (thread-%next *pager-current-thread*) *pager-waiting-threads*
          *pager-waiting-threads* *pager-current-thread*
          *pager-current-thread* nil))
  ;; The VM lock is recreated each boot because it is only held by
  ;; the ephemeral pager and snapshot threads.
  (setf *vm-lock* (make-mutex "Global VM Lock")))

(defun pager-thread ()
  (loop
     ;; Select a thread.
     (with-mutex (*pager-lock*)
       (loop
          (when *pager-waiting-threads*
            (setf *pager-current-thread* *pager-waiting-threads*
                  *pager-waiting-threads* (thread-%next *pager-current-thread*))
            (return))
          ;; Manually sleep, don't use condition variables or similar within ephemeral threads.
          (%lock-thread sys.int::*pager-thread*)
          (release-mutex *pager-lock*)
          (setf (thread-state sys.int::*pager-thread*) :sleeping
                (thread-wait-item sys.int::*pager-thread*) '*pager-waiting-threads*)
          (%reschedule)
          (acquire-mutex *pager-lock*)))
     ;; Page it in
     (when (not (wait-for-page (thread-wait-item *pager-current-thread*)))
       ;; TODO: Dispatch this to a debugger thread.
       (panic "page fault on unmapped page " (thread-wait-item *pager-current-thread*) " in thread " *pager-current-thread*))
     ;; Release the thread.
     (wake-thread *pager-current-thread*)
     (setf *pager-current-thread* nil)))
