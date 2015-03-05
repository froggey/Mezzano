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
                (defun ,name (allocator bin)
                  (sys.int::memref-t (+ *boot-information-page*
                                        allocator
                                        (* bin +buddy-bin-size+))
                                     ,offset))
                (defun (setf ,name) (value allocator bin)
                  (setf (sys.int::memref-t (+ *boot-information-page*
                                              allocator
                                              (* bin +buddy-bin-size+))
                                           ,offset)
                        value)))))
  (def physical-buddy-bin-head 0)
  (def physical-buddy-bin-count 1))

;;; Accessors into the memory map.
(defun n-memory-map-entries ()
  (sys.int::memref-unsigned-byte-64
   (+ *boot-information-page*
      +boot-information-n-memory-map-entries+)
   0))

(defun memory-map-entry-start (entry)
  (sys.int::memref-unsigned-byte-64
   (+ *boot-information-page*
      +boot-information-memory-map+
      (* entry 16))
   0))

(defun memory-map-entry-end (entry)
  (sys.int::memref-unsigned-byte-64
   (+ *boot-information-page*
      +boot-information-memory-map+
      (* entry 16))
   1))

(defvar *physical-lock*)

;; Bootloader does everything for us. How kind.
(defun initialize-physical-allocator ()
  (when (not (boundp '*physical-lock*))
    ;; First boot.
    (setf *physical-lock* :unlocked
          *verbose-physical-allocation* nil)))

(defun allocate-physical-pages-1 (n-pages buddy-allocator-address max-order)
  ;; Find the bin that matches this page count and
  ;; the lowest bin with pages that is best-bin <= N < max-bin.
  (let* ((best-bin (integer-length (1- n-pages)))
         (avail-bin (loop
                       for bin from best-bin below max-order
                       when (physical-buddy-bin-head buddy-allocator-address bin)
                       do (return bin))))
    (when avail-bin
      ;; Pages available.
      ;; Remove the page from the bin freelist list and mark as allocated.
      (let ((frame (physical-buddy-bin-head buddy-allocator-address avail-bin)))
        (setf (physical-buddy-bin-head buddy-allocator-address avail-bin) (physical-page-frame-next frame))
        (decf (physical-buddy-bin-count buddy-allocator-address avail-bin))
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
             (when (physical-buddy-bin-head buddy-allocator-address avail-bin)
               (setf (physical-page-frame-prev (physical-buddy-bin-head buddy-allocator-address avail-bin)) p))
             (setf (physical-page-frame-prev p) nil
                   (physical-page-frame-next p) (physical-buddy-bin-head buddy-allocator-address avail-bin)
                   (physical-buddy-bin-head buddy-allocator-address avail-bin) p)
             (incf (physical-buddy-bin-count buddy-allocator-address avail-bin))))
        (when mezzano.runtime::*paranoid-allocation*
          (dotimes (i (* n-pages 512))
            (setf (sys.int::memref-signed-byte-64 (+ +physical-map-base+ (ash frame 12)) i) -1)))
        (when *verbose-physical-allocation*
          (debug-print-line "Allocated " n-pages " pages " frame))
        frame))))

(defun allocate-physical-pages (n-pages &optional mandatory-p 32-bit-only)
  "Allocate N-PAGES of contiguous physical page frames.
If the allocation could not be satisfied then NIL will be returned
when MANDATORY-P is false, otherwise PANIC will be called.
If MANDATORY-P is non-NIL, it should be a string describing the allocation."
  (ensure (not (zerop n-pages)) "Tried to allocate 0 frames.")
  (with-symbol-spinlock (*physical-lock*)
    (let ((frame (or (when (not 32-bit-only)
                       ;; Try 64-bit allocator first, save 32-bit pages for things that really need it.
                       (allocate-physical-pages-1 n-pages
                                                  +boot-information-64-bit-physical-buddy-bins-offset+
                                                  +n-64-bit-physical-buddy-bins+))
                     (allocate-physical-pages-1 n-pages
                                                +boot-information-32-bit-physical-buddy-bins-offset+
                                                +n-32-bit-physical-buddy-bins+))))
      (when (and (not frame)
                 mandatory-p)
        (panic "No physical memory: " mandatory-p))
      frame)))

(defun physical-page-exists (page-number)
  (let ((page (* page-number +4k-page-size+)))
    (dotimes (i (n-memory-map-entries) nil)
      (when (and (<= (memory-map-entry-start i) page)
                 (< page (memory-map-entry-end i)))
        (return t)))))

(defun release-physical-pages-1 (page-number n-pages buddy-allocator-address max-order)
  (let ((bin (integer-length (1- n-pages))))
    (loop
       (let ((buddy (logxor page-number (ash 1 bin))))
         ;; Is buddy available?
         (when (or (eql bin (1- max-order))
                   (not (physical-page-exists buddy))
                   (eql (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags buddy)) 0)
                   (and (eql (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags buddy)) 1)
                        (not (eql (physical-page-frame-bin buddy) bin))))
           (return))
         (when *verbose-physical-allocation*
           (debug-print-line "Merge frame " page-number " buddy " buddy " order " bin " other " (physical-page-frame-bin buddy)))
         ;; Combine with buddy.
         ;; Remove buddy from freelist.
         (when (eql (physical-buddy-bin-head buddy-allocator-address bin) buddy)
           (setf (physical-buddy-bin-head buddy-allocator-address bin) (physical-page-frame-next buddy)))
         (when (physical-page-frame-next buddy)
           (setf (physical-page-frame-prev (physical-page-frame-next buddy)) (physical-page-frame-prev buddy)))
         (when (physical-page-frame-prev buddy)
           (setf (physical-page-frame-next (physical-page-frame-prev buddy)) (physical-page-frame-next buddy)))
         (decf (physical-buddy-bin-count buddy-allocator-address bin))
         ;; Pages are merged.
         (incf bin)
         (when (< buddy page-number)
           (setf page-number buddy))))
    ;; Attach to freelist.
    (setf (ldb (byte 1 +page-frame-flag-free+) (physical-page-frame-flags page-number)) 1)
    (when (physical-buddy-bin-head buddy-allocator-address bin)
      (setf (physical-page-frame-prev (physical-buddy-bin-head buddy-allocator-address bin)) page-number))
    (setf (physical-page-frame-next page-number) (physical-buddy-bin-head buddy-allocator-address bin)
          (physical-page-frame-prev page-number) nil)
    (setf (physical-page-frame-bin page-number) bin)
    (setf (physical-buddy-bin-head buddy-allocator-address bin) page-number)
    (incf (physical-buddy-bin-count buddy-allocator-address bin))))

(defun release-physical-pages (page-number n-pages)
  (ensure (not (zerop n-pages)) "Tried to free 0 frames.")
  (ensure (physical-page-exists page-number) "Frame " page-number " does not exist.")
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
    (cond ((< page-number (truncate #x100000000 +4k-page-size+))
           (release-physical-pages-1 page-number
                                     n-pages
                                     +boot-information-32-bit-physical-buddy-bins-offset+
                                     +n-32-bit-physical-buddy-bins+))
          (t
           (release-physical-pages-1 page-number
                                     n-pages
                                     +boot-information-64-bit-physical-buddy-bins-offset+
                                     +n-64-bit-physical-buddy-bins+))))
  (values))
