;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Physical memory management.
;;;; Currently supports up to (expt 2 39) bytes of physical memory (512GB).

(in-package :mezzano.supervisor)

;; Every page frame gets a structure allocated for it in a sparse array.
;; The bootloader will generate this for us.
;; +0 type & extra data:
;;    (byte 8 0) - type
;;         0  Other
;;         1  Free
;;         2  Wired page
;;         3  Wired backing page
;;         4  Active
;;         5  Active, waiting for writeback
;;         6  Inactive, waiting for writeback
;;         7  Page table
;;    (byte 8 8) - Buddy bin, only when free
;;    (byte 52 8) - Virtual page, only when active or wired backing page.
;; +1 store block id (when inactive, waiting for writeback or wired backing page)
;; +2 freelist next (only when free)
;; +2 writeback next (only when waiting for writeback)
;; +2 lru next (only when active, not waiting for writeback)
;; +2 backing page (only when wired)
;; +3 freelist prev
;; +3 writeback prev
;; +3 lru prev

(defconstant +physical-map-base+
  (logior (ash -1 48) #x800000000000))

(defconstant +physical-map-end+
  (logior (ash -1 48) #x808000000000))

(defconstant +page-frame-information-vector+
  (logior (ash -1 48) #x808000000000))

(defconstant +4k-page-size+ #x1000)
(defconstant +2m-page-size+ #x200000)

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
  (def physical-page-frame-extra 1)
  (def physical-page-frame-next 2)
  (def physical-page-frame-prev 3))

(defun physical-page-frame-type (page-number)
  (ecase (ldb (byte 8 0) (physical-page-frame-flags page-number))
    (0 :other)
    (1 :free)
    (2 :wired)
    (3 :wired-backing)
    (4 :active)
    (5 :active-writeback)
    (6 :inactive-writeback)
    (7 :page-table)))

(defun (setf physical-page-frame-type) (value page-number)
  (setf (ldb (byte 8 0) (physical-page-frame-flags page-number))
        (ecase value
          (:other 0)
          (:free 1)
          (:wired 2)
          (:wired-backing 3)
          (:active 4)
          (:active-writeback 5)
          (:inactive-writeback 6)
          (:page-table 7))))

(defun physical-page-frame-bin (page-number)
  (ldb (byte 8 8) (physical-page-frame-flags page-number)))

(defun (setf physical-page-frame-bin) (value page-number)
  (setf (ldb (byte 8 8) (physical-page-frame-flags page-number)) value))

(defun physical-page-virtual-address (page-number)
  (ash (ldb (byte (- +virtual-address-bits+ +log2-4k-page+) 8)
            (physical-page-frame-flags page-number))
       +log2-4k-page+))

(defun (setf physical-page-virtual-address) (value page-number)
  (let ((abs-value (if (minusp value)
                       (- value)
                       value)))
    (ensure (eql (logand abs-value (logand (1- (ash 1 +virtual-address-bits+))
                                           (lognot (1- +4k-page-size+))))
                 abs-value)
            "Virtual address " value " not page aligned or outside limits."))
  (setf (ldb (byte (- +virtual-address-bits+ +log2-4k-page+) 8)
             (physical-page-frame-flags page-number))
        (ash value (- +log2-4k-page+)))
  value)

(defun physical-page-frame-block-id (page-number)
  (physical-page-frame-extra page-number))

(defun (setf physical-page-frame-block-id) (value page-number)
  (setf (physical-page-frame-extra page-number) value))

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

(defun allocate-physical-pages-1 (n-pages buddy-allocator-address max-order type)
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
        (when *verbose-physical-allocation*
          (debug-print-line "Considering frame " frame " type " (physical-page-frame-type frame)
                            " avail bin " avail-bin " best bin " best-bin))
        (when (not (eql (physical-page-frame-type frame) :free))
          (panic "Allocated allocated page " frame))
        (setf (physical-page-frame-type frame) type)
        ;; Split block as required.
        (loop
           (when (eql avail-bin best-bin)
             (return))
           (decf avail-bin)
           (let ((p (+ frame (ash 1 avail-bin))))
             (when *verbose-physical-allocation*
               (debug-print-line "Split frame " frame " buddy " p " order " avail-bin))
             ;; Mark free, and set bin number.
             (setf (physical-page-frame-type p) :free
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

(defun allocate-physical-pages (n-pages &key (type :other) mandatory-p 32-bit-only)
  "Allocate N-PAGES of contiguous physical page frames.
If the allocation could not be satisfied then NIL will be returned
when MANDATORY-P is false, otherwise PANIC will be called.
If MANDATORY-P is non-NIL, it should be a string describing the allocation."
  (ensure (not (zerop n-pages)) "Tried to allocate 0 frames.")
  (when *verbose-physical-allocation*
    (debug-print-line "Allocating " n-pages " of type " type))
  (let ((frame
         (safe-without-interrupts (n-pages type 32-bit-only)
           (with-symbol-spinlock (*physical-lock*)
             (or (when (not 32-bit-only)
                   ;; Try 64-bit allocator first, save 32-bit pages for things that really need it.
                   (allocate-physical-pages-1 n-pages
                                              +boot-information-64-bit-physical-buddy-bins-offset+
                                              +n-64-bit-physical-buddy-bins+
                                              type))
                 (allocate-physical-pages-1 n-pages
                                            +boot-information-32-bit-physical-buddy-bins-offset+
                                            +n-32-bit-physical-buddy-bins+
                                            type))))))
    (when *verbose-physical-allocation*
      (debug-print-line "Allocated frame " frame))
    (when (and (not frame)
               mandatory-p)
      (panic "No physical memory: " mandatory-p))
    frame))

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
                   (not (eql (physical-page-frame-type buddy) :free))
                   (and (eql (physical-page-frame-type buddy) :free)
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
    (setf (physical-page-frame-type page-number) :free)
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
  (ensure (not (eql (physical-page-frame-type page-number) :free)) "Tried to free free frame.")
  (when mezzano.runtime::*paranoid-allocation*
    (dotimes (i (* n-pages 512))
      (setf (sys.int::memref-signed-byte-64 (+ +physical-map-base+ (ash page-number 12)) i) -1)))
  (when *verbose-physical-allocation*
    (debug-print-line "Freeing " n-pages " pages " page-number))
  (safe-without-interrupts (page-number n-pages)
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
                                       +n-64-bit-physical-buddy-bins+)))))
  (values))

(defun physical-memory-statistics ()
  (safe-without-interrupts ()
    (let ((n-free-pages 0)
          (total-pages 0))
      (with-symbol-spinlock (*physical-lock*)
        (dotimes (i (n-memory-map-entries))
          (incf total-pages (truncate (- (memory-map-entry-end i)
                                         (memory-map-entry-start i))
                                      +4k-page-size+)))
        (dotimes (bin +n-32-bit-physical-buddy-bins+)
          (incf n-free-pages (* (physical-buddy-bin-count
                                 +boot-information-32-bit-physical-buddy-bins-offset+
                                 bin)
                                (ash 1 bin))))
        (dotimes (bin +n-64-bit-physical-buddy-bins+)
          (incf n-free-pages (* (physical-buddy-bin-count
                                 +boot-information-64-bit-physical-buddy-bins-offset+
                                 bin)
                                (ash 1 bin)))))
      (values n-free-pages total-pages))))
