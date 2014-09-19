;;;; Physical memory management.
;;;; Currently supports up to (expt 2 39) bytes of physical memory (512GB).

(in-package :mezzanine.supervisor)

;; Every page frame gets a structure allocated for it in a sparse array.
;; The bootloader will generate this for us.
;; +0 flags (fixnum)
;;    bit 0 - free
;;    bit 1 - is cache for store
;; +1 store block id (when allocated and used as cache)
;; +1 buddy bin (only when free)
;; +2 freelist next
;; +3 freelist prev

(defconstant +physical-map-base+
  (logior (ash -1 48) #x800000000000))

(defconstant +page-frame-information-vector+
  (logior (ash -1 48) #x808000000000))

(defconstant +4k-page-size+ #x1000)
(defconstant +2m-page-size+ #x200000)

(defconstant +page-frame-flag-free+ 0)
(defconstant +page-frame-flag-cache+ 1)

;;; Accessors into the page frame information vector.
(macrolet ((def (name offset)
             `(progn
                (defun ,name (page-number)
                  (sys.int::memref-t (+ +page-frame-information-vector+
                                        (* page-number 32))
                                     ,offset))
                (defun (setf ,name) (value page-number)
                  (setf (sys.int::memref-t (+ +page-frame-information-vector+
                                              (* page-number 32))
                                           ,offset)
                        value)))))
  (def physical-page-frame-flags 0)
  (def physical-page-frame-block-id 1)
  (def physical-page-frame-bin 1)
  (def physical-page-frame-next 2)
  (def physical-page-frame-prev 3))

;;; Accessors into the buddy bin heads.
(macrolet ((def (name offset)
             `(progn
                (defun ,name (bin)
                  (check-type bin (integer 0 (,+n-physical-buddy-bins+)))
                  (sys.int::memref-t (+ *boot-information-page*
                                        +boot-information-physical-buddy-bins-offset+
                                        (* bin +buddy-bin-size+))
                                     ,offset))
                (defun (setf ,name) (value bin)
                  (check-type bin (integer 0 (,+n-physical-buddy-bins+)))
                  (setf (sys.int::memref-t (+ *boot-information-page*
                                              +boot-information-physical-buddy-bins-offset+
                                              (* bin +buddy-bin-size+))
                                           ,offset)
                        value)))))
  (def physical-buddy-bin-head 0)
  (def physical-buddy-bin-count 1))

(defvar *physical-lock*)

;; Bootloader does everything for us. How kind.
(defun initialize-physical-allocator ()
  (when (not (boundp '*physical-lock*))
    ;; First boot.
    (setf *physical-lock* :unlocked)))

(defun allocate-physical-pages (n-pages)
  (assert (not (zerop n-pages)))
  (with-symbol-spinlock (*physical-lock*)
    ;; Find the bin that matches this page count and
    ;; the lowest bin with pages that is best-bin <= N < max-bin.
    (let* ((best-bin (integer-length (1- n-pages)))
           (avail-bin (loop
                         for bin from best-bin below +n-physical-buddy-bins+
                         when (physical-buddy-bin-head bin)
                         do (return bin))))
      (when avail-bin
        ;; Pages available.
        ;; Remove the page from the bin freelist list and mark as allocated.
        (let ((frame (physical-buddy-bin-head avail-bin)))
          (setf (physical-buddy-bin-head avail-bin) (physical-page-frame-next frame))
          (decf (physical-buddy-bin-count avail-bin))
          (when (physical-page-frame-next frame)
            (setf (physical-page-frame-prev (physical-page-frame-next frame)) nil))
          (when (zerop (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags frame)))
            (sys.int::%sti)
            (debug-print-line "Allocated allocated page " frame)
            (loop))
          (setf (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags frame)) 0)
          ;; Split block as required.
          (loop
             (when (eql avail-bin best-bin)
               (return))
             (decf avail-bin)
             (let ((p (+ frame (ash 1 avail-bin))))
               ;; Mark free, and set bin number.
               (setf (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags p)) 1
                     (physical-page-frame-bin p) avail-bin)
               ;; Attach to freelist.
               (when (physical-buddy-bin-head avail-bin)
                 (setf (physical-page-frame-prev (physical-buddy-bin-head avail-bin)) p))
               (setf (physical-page-frame-prev p) nil
                     (physical-page-frame-next p) (physical-buddy-bin-head avail-bin)
                     (physical-buddy-bin-head avail-bin) p)
               (incf (physical-buddy-bin-count avail-bin))))
          frame)))))

(defun release-physical-pages (page-number n-pages)
  (assert (not (zerop n-pages)))
  (assert (eql (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags page-number)) 0))
  (when (not (zerop (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags page-number))))
    (sys.int::%sti)
    (debug-print-line "Freed free page " page-number)
    (loop))
  (with-symbol-spinlock (*physical-lock*)
    (let ((bin (integer-length (1- n-pages))))
      (loop
         (let ((buddy (logxor page-number (ash 1 bin))))
           ;; Is buddy available?
           (when (or (eql bin (1- +n-physical-buddy-bins+))
                     (eql (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags buddy)) 0)
                     (and (eql (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags buddy)) 1)
                          (not (eql (physical-page-frame-bin buddy) bin))))
             (return))
           ;; Combine with buddy.
           ;; Remove buddy from freelist.
           (when (eql (physical-buddy-bin-head bin) buddy)
             (setf (physical-buddy-bin-head bin) (physical-page-frame-next buddy)))
           (when (physical-page-frame-next buddy)
             (setf (physical-page-frame-prev (physical-page-frame-next buddy)) (physical-page-frame-prev buddy)))
           (when (physical-page-frame-prev buddy)
             (setf (physical-page-frame-next (physical-page-frame-prev buddy)) (physical-page-frame-next buddy)))
           (decf (physical-buddy-bin-count bin))
           ;; Pages are merged.
           (incf bin)
           (when (< buddy page-number)
             (setf page-number buddy))))
      ;; Attach to freelist.
      (setf (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags page-number)) 1)
      (when (physical-buddy-bin-head bin)
        (setf (physical-page-frame-prev (physical-buddy-bin-head bin)) page-number))
      (setf (physical-page-frame-next page-number) (physical-buddy-bin-head bin)
            (physical-page-frame-prev page-number) nil)
      (setf (physical-page-frame-bin page-number) bin)
      (setf (physical-buddy-bin-head bin) page-number)
      (incf (physical-buddy-bin-count bin))))
  (values))
