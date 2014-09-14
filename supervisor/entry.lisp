(in-package :mezzanine.supervisor)

;;; FIXME: Should not be here.
;;; >>>>>>

(defun string-length (string)
  (assert (sys.int::character-array-p string))
  (sys.int::%array-like-ref-t string 3))

(defmacro with-gc-deferred (&body body)
  `(call-with-gc-deferred (flet ((with-gc-deferred-thunk () ,@body))
                            (declare (dynamic-extent #'with-gc-deferred-thunk))
                            #'with-gc-deferred-thunk)))

;; TODO?
(defun call-with-gc-deferred (thunk)
  (funcall thunk))

(defun find-extent-named (name largep)
  (cond ((store-extent-p name) name)
        (t (dolist (extent *extent-table*
                    (error "can't find extent..."))
             (when (and (or (eql (store-extent-type extent) name)
                            (and (eql name :wired)
                                 (eql (store-extent-type extent) :pinned)
                                 (store-extent-wired-p extent)))
                        (not (store-extent-finished-p extent))
                        (eql (store-extent-large-p extent) largep))
               (return extent))))))

(defun stack-base (stack)
  (car stack))

(defun stack-size (stack)
  (cdr stack))

;; TODO: Actually allocate virtual memory & store.
(defun %allocate-stack (size)
  ;; 4k align the size.
  (setf size (logand (+ size #xFFF) (lognot #xFFF)))
  (let* ((addr (with-symbol-spinlock (mezzanine.runtime::*wired-allocator-lock*)
                 (prog1 (logior (+ sys.int::*stack-area-bump* #x200000)
                                (ash sys.int::+address-tag-stack+ sys.int::+address-tag-shift+))
                   ;; 2m align the memory region.
                   (incf sys.int::*stack-area-bump* (+ (logand (+ size #x1FFFFF) (lognot #x1FFFFF))
                                                       #x200000)))))
         (stack (sys.int::cons-in-area addr size :wired)))
    ;; Map the memory in, should allocate this from the store instead... sigh.
    (with-mutex (*vm-lock*)
      (dotimes (i (ceiling size #x1000))
        (let* ((address (+ addr (* i #x1000)))
               (pte (get-pte-for-address address)))
          (assert (not (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pte 0))))
          ;; No page allocated. Allocate a page and read the data.
          (let* ((frame (or (allocate-physical-pages 1)
                            (progn (debug-write-line "Aiee. No memory.")
                                   (loop))))
                 (addr (+ +physical-map-base+ (ash frame 12))))
            (setf (sys.int::memref-unsigned-byte-64 pte 0) (logior (ash frame 12)
                                                                   +page-table-present+
                                                                   +page-table-write+))))))
    stack))

;; TODO.
(defun sleep (seconds)
  nil)

(defun sys.int::raise-undefined-function (fref)
  (debug-write-string "Undefined function ")
  (let ((name (sys.int::%array-like-ref-t fref sys.int::+fref-name+)))
    (cond ((consp name)
           (debug-write-string "(")
           (debug-write-string (symbol-name (car name)))
           (debug-write-string " ")
           (debug-write-string (symbol-name (car (cdr name))))
           (debug-write-line ")"))
          (t (debug-write-line (symbol-name name)))))
  (sys.int::%sti)
  (loop))

(defun sys.int::raise-unbound-error (symbol)
  (debug-write-string "Unbound symbol ")
  (debug-write-line (symbol-name symbol))
  (sys.int::%sti)
  (loop))

(in-package :sys.int)

(defstruct (cold-stream (:area :wired)))

(in-package :mezzanine.supervisor)

(defvar *cold-unread-char*)

(defun sys.int::cold-write-char (c stream)
  (declare (ignore stream))
  (debug-write-char c))

(defun sys.int::cold-start-line-p (stream)
  (declare (ignore stream))
  (debug-start-line-p))

(defun sys.int::cold-read-char (stream)
  (declare (ignore stream))
  (cond (*cold-unread-char*
         (prog1 *cold-unread-char*
           (setf *cold-unread-char* nil)))
        (t (debug-read-char))))

(defun sys.int::cold-unread-char (character stream)
  (declare (ignore stream))
  (when *cold-unread-char*
    (error "Multiple unread-char!"))
  (setf *cold-unread-char* character))

;;; <<<<<<

(defvar *boot-information-page*)

(defstruct (disk
             (:area :wired))
  device
  n-sectors
  sector-size
  max-transfer
  read-fn
  write-fn)

(defvar *disks*)

(defvar *paging-disk*)
(defvar *bml4-block*)

(defconstant +n-physical-buddy-bins+ 32)
(defconstant +buddy-bin-size+ 16)

(defconstant +boot-information-boot-uuid-offset+ 0)
(defconstant +boot-information-physical-buddy-bins-offset+ 16)

(defun boot-uuid (offset)
  (check-type offset (integer 0 15))
  (sys.int::memref-unsigned-byte-8 *boot-information-page* offset))

(defun register-disk (device n-sectors sector-size max-transfer read-fn write-fn)
  (when (> sector-size +4k-page-size+)
    (debug-write-line "Ignoring device with sector size larger than 4k."))
  (let* ((disk (make-disk :device device
                          :sector-size sector-size
                          :n-sectors n-sectors
                          :max-transfer max-transfer
                          :read-fn read-fn
                          :write-fn write-fn))
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
      (setf *bml4-block* (sys.int::memref-unsigned-byte-64 (+ page-addr 96) 0)))
    ;; Release the pages.
    (release-physical-pages page (ceiling (max +4k-page-size+ sector-size) +4k-page-size+))))

(defvar *vm-lock*)

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
                        (progn (debug-write-line "Aiee. No memory.")
                               (loop))))
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
                          (progn (debug-write-line "Aiee. No memory.")
                                 (loop))))
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
                            (progn (debug-write-line "Aiee. No memory.")
                                   (loop))))
                 (addr (+ +physical-map-base+ (ash frame 12))))
            (dotimes (i 512)
              (setf (sys.int::memref-unsigned-byte-64 addr i) 0))
            (setf (sys.int::memref-unsigned-byte-64 pdir pde) (logior (ash frame 12)
                                                                      +page-table-present+
                                                                      +page-table-write+))))
        (let ((pt (+ +physical-map-base+ (logand (sys.int::memref-unsigned-byte-64 pdir pde) +page-table-address-mask+))))
          (+ pt (* pte 8)))))))

(defun read-cached-block-from-disk (block-id)
  (let* ((frame (or (allocate-physical-pages 1)
                    (progn (debug-write-line "Aiee. No memory.")
                           (loop))))
         (addr (+ +physical-map-base+ (ash frame 12))))
    (or (funcall (disk-read-fn *paging-disk*)
                 (disk-device *paging-disk*)
                 (* block-id (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                 (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                 addr)
        (progn (debug-write-line "Unable to read page from disk")
               (loop)))
    ;; Insert it into the cache.
    (block cache-insert
      (do ((cache-page *block-cache* (sys.int::memref-t cache-page 511)))
          ((null cache-page)
           ;; Expand the cache.
           (let* ((frame (or (allocate-physical-pages 1)
                             (progn (debug-write-line "Aiee. No memory.")
                                    (loop))))
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
            (return-from cache-insert)))))
    addr))

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

(defun wait-for-page-via-interrupt-1 (interrupt-frame address)
  (declare (ignore interrupt-frame))
  ;; Stack pages with the mark bit set are aliased to the same
  ;; address with the mark bit clear.
  (when (and (logbitp sys.int::+address-mark-bit+ address)
             (eql (ldb (byte sys.int::+address-tag-size+ sys.int::+address-tag-shift+) address)
                  sys.int::+address-tag-stack+))
    (return-from wait-for-page-via-interrupt-1
      (when (wait-for-page-via-interrupt-1 interrupt-frame
                                           (logand address (lognot (ash 1 sys.int::+address-mark-bit+))))
        ;; Page actually exists, fetch the pte for it and mash it into the pte for this address.
        ;; FIXME: Alias page tables at the PML4 level, not at the PTE level.
        (let ((other-pte (get-pte-for-address (logand address (lognot (ash 1 sys.int::+address-mark-bit+)))))
              (pte (get-pte-for-address address)))
          (when (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pte 0))
            (return-from wait-for-page-via-interrupt-1 t))
          (setf (sys.int::memref-unsigned-byte-64 pte 0) (sys.int::memref-unsigned-byte-64 other-pte 0))
          t))))
  (let ((block-info (block-info-for-virtual-address address)))
    (when (or (not block-info)
              (not (logtest sys.int::+block-map-present+ block-info)))
      (return-from wait-for-page-via-interrupt-1 nil))
    ;; Examine the page table, if there's a present entry then the page
    ;; was mapped while acquiring the VM lock. Just return.
    (let ((pte (get-pte-for-address address)))
      (when (logtest +page-table-present+ (sys.int::memref-unsigned-byte-64 pte 0))
        (return-from wait-for-page-via-interrupt-1 t))
      ;; No page allocated. Allocate a page and read the data.
      (let* ((frame (or (allocate-physical-pages 1)
                        (progn (debug-write-line "Aiee. No memory.")
                               (loop))))
             (addr (+ +physical-map-base+ (ash frame 12))))
        (cond ((logtest sys.int::+block-map-zero-fill+ block-info)
               ;; Block is zero-filled.
               (dotimes (i 512)
                 (setf (sys.int::memref-unsigned-byte-64 addr i) 0)))
              (t ;; Block must be read from disk.
               (or (funcall (disk-read-fn *paging-disk*)
                            (disk-device *paging-disk*)
                            (* (ldb (byte sys.int::+block-map-id-size+ sys.int::+block-map-id-shift+) block-info)
                               (ceiling +4k-page-size+ (disk-sector-size *paging-disk*)))
                            (ceiling +4k-page-size+ (disk-sector-size *paging-disk*))
                            addr)
                   (progn (debug-write-line "Unable to read page from disk")
                          (loop)))))
        (setf (sys.int::memref-unsigned-byte-64 pte 0) (logior (ash frame 12)
                                                               +page-table-present+
                                                               (if (logtest sys.int::+block-map-writable+ block-info)
                                                                   +page-table-write+
                                                                   0)))))
    t))

(defun wait-for-page-via-interrupt (interrupt-frame address)
  (with-mutex (*vm-lock*)
    (wait-for-page-via-interrupt-1 interrupt-frame address)))

(defun sys.int::bootloader-entry-point (boot-information-page)
  (initialize-initial-thread)
  (setf *boot-information-page* boot-information-page
        *block-cache* nil
        *cold-unread-char* nil)
  (when (not (boundp 'mezzanine.runtime::*tls-lock*))
    (mezzanine.runtime::first-run-initialize-allocator)
    ;; FIXME: Should be done by cold generator
    (setf mezzanine.runtime::*tls-lock* :unlocked
          mezzanine.runtime::*active-catch-handlers* 'nil)
    ;; Bootstrap the defstruct system.
    ;; 1) Initialize *structure-type-type* so make-struct-definition works.
    (setf sys.int::*structure-type-type* nil)
    ;; 2) Create the real definition, with broken type.
    (setf sys.int::*structure-type-type* (sys.int::make-struct-definition
                                          'sys.int::structure-definition
                                          ;; (name accessor initial-value type read-only atomic).
                                          '((sys.int::name sys.int::structure-name nil t t nil)
                                            (sys.int::slots sys.int::structure-slots nil t t nil)
                                            (sys.int::parent sys.int::structure-parent nil t t nil)
                                            (sys.int::area sys.int::structure-area nil t t nil)
                                            (sys.int::class sys.int::structure-class nil t nil nil))
                                          nil
                                          :wired))
    ;; 3) Patch up the broken structure type.
    (setf (sys.int::%struct-slot sys.int::*structure-type-type* 0) sys.int::*structure-type-type*))
  (initialize-interrupts)
  (initialize-i8259)
  (initialize-physical-allocator)
  (initialize-threads)
  (when (not (boundp '*vm-lock*))
    (setf *vm-lock* (make-mutex "Global VM Lock")))
  (sys.int::%sti)
  (initialize-debug-serial #x3F8 4 38400)
  ;;(debug-set-output-pesudostream (lambda (op &optional arg) (declare (ignore op arg))))
  (debug-write-line "Hello, Debug World!")
  (setf *disks* '()
        *paging-disk* nil)
  (initialize-ata)
  (when (not *paging-disk*)
    (debug-write-line "Could not find boot device. Sorry.")
    (loop))
  ;; Load the extent table.
  (make-thread #'sys.int::initialize-lisp :name "Main thread")
  (finish-initial-thread))
