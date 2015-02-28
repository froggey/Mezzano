;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Physical memory management.
;;;; Currently supports up to (expt 2 39) bytes of physical memory (512GB).

(in-package :mezzano.supervisor)

;; Every page frame gets a structure allocated for it in a sparse array.
;; The bootloader will generate this for us.
;; +0 flags (fixnum)
;;    bit 0 - free
;;    bit 1 - is cache for store
;;    bit 2 - page is waiting for writeback
;;    bits 12+ - virtual address. Only valid when bit 1 set.
;; +1 store block id (when allocated and used as cache or waiting for writeback)
;; +1 buddy bin (only when free)
;; +2 freelist next (only when free)
;; +2 Snapshot next (only when waiting for writeback)
;; +3 freelist prev
;; +3 snapshot prev (only when waiting for writeback)

(defconstant +physical-map-base+
  (logior (ash -1 48) #x800000000000))

(defconstant +physical-map-end+
  (logior (ash -1 48) #x808000000000))

(defconstant +page-frame-information-vector+
  (logior (ash -1 48) #x808000000000))

(defconstant +4k-page-size+ #x1000)
(defconstant +2m-page-size+ #x200000)

(defconstant +page-frame-flag-free+ 0)
(defconstant +page-frame-flag-cache+ 1)
(defconstant +page-frame-flag-writeback+ 2)

(defvar *verbose-physical-allocation*)

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
    (setf *physical-lock* :unlocked
          *verbose-physical-allocation* nil)))

(defun allocate-physical-pages (n-pages &optional mandatory-p)
  "Allocate N-PAGES of contiguous physical page frames.
If the allocation could not be satisfied then NIL will be returned
when MANDATORY-P is false, otherwise PANIC will be called.
If MANDATORY-P is non-NIL, it should be a string describing the allocation."
  (ensure (not (zerop n-pages)) "Tried to allocate 0 frames.")
  (with-symbol-spinlock (*physical-lock*)
    ;; Find the bin that matches this page count and
    ;; the lowest bin with pages that is best-bin <= N < max-bin.
    (let* ((best-bin (integer-length (1- n-pages)))
           (avail-bin (loop
                         for bin from best-bin below +n-physical-buddy-bins+
                         when (physical-buddy-bin-head bin)
                         do (return bin))))
      (cond (avail-bin
             ;; Pages available.
             ;; Remove the page from the bin freelist list and mark as allocated.
             (let ((frame (physical-buddy-bin-head avail-bin)))
               (setf (physical-buddy-bin-head avail-bin) (physical-page-frame-next frame))
               (decf (physical-buddy-bin-count avail-bin))
               (when (physical-page-frame-next frame)
                 (setf (physical-page-frame-prev (physical-page-frame-next frame)) nil))
               (when (zerop (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags frame)))
                 (panic "Allocated allocated page " frame))
               (setf (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags frame)) 0)
               ;; Split block as required.
               (loop
                  (when (eql avail-bin best-bin)
                    (return))
                  (decf avail-bin)
                  (let ((p (+ frame (ash 1 avail-bin))))
                    (when *verbose-physical-allocation*
                      (debug-print-line "Split frame " frame " buddy " p " order " avail-bin))
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
               (when mezzano.runtime::*paranoid-allocation*
                 (dotimes (i (* n-pages 512))
                   (setf (sys.int::memref-signed-byte-64 (+ +physical-map-base+ (ash frame 12)) i) -1)))
               (when *verbose-physical-allocation*
                 (debug-print-line "Allocated " n-pages " pages " frame))
               frame))
            (mandatory-p
             (panic "No physical memory: " mandatory-p))))))

(defun release-physical-pages (page-number n-pages)
  (ensure (not (zerop n-pages)) "Tried to free 0 frames.")
  (ensure (not (logbitp +page-frame-flag-free+ (physical-page-frame-flags page-number))) "Free bit set in frame flags.")
  (ensure (not (logbitp +page-frame-flag-cache+ (physical-page-frame-flags page-number))) "Cache bit set in frame flags.")
  (ensure (not (logbitp +page-frame-flag-writeback+ (physical-page-frame-flags page-number))) "Writeback bit set in frame flags.")
  (when mezzano.runtime::*paranoid-allocation*
    (dotimes (i (* n-pages 512))
      (setf (sys.int::memref-signed-byte-64 (+ +physical-map-base+ (ash page-number 12)) i) -1)))
  (when *verbose-physical-allocation*
    (debug-print-line "Freeing " n-pages " pages " page-number))
  (when (not (zerop (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags page-number))))
    (panic "Freed free page " page-number))
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
           (when *verbose-physical-allocation*
             (debug-print-line "Merge frame " page-number " buddy " buddy " order " bin " other " (physical-page-frame-bin buddy)))
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
