;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defun snapshot-mark-cow-dirty-pages ()
  ;; Mark all non-wired dirty pages as read-only and set their CoW bits.
  (labels ((mark-level (pml index next-fn)
             ;;(debug-print-line " PML " pml " index " index)
             (let* ((entry (sys.int::memref-unsigned-byte-64 pml index))
                    (next-pml (convert-to-pmap-address
                               (logand entry +arm64-tte-address-mask+))))
               (when (logtest entry +arm64-tte-present+)
                 (dotimes (i 512)
                   (funcall next-fn next-pml i)))))
           (mark-pml4e-cow (pml4 pml4e)
             (mark-level pml4 pml4e #'mark-pml3e-cow))
           (mark-pml3e-cow (pml3 pml3e)
             (mark-level pml3 pml3e #'mark-pml2e-cow))
           (mark-pml2e-cow (pml2 pml2e)
             (mark-level pml2 pml2e #'mark-pml1e-cow))
           (mark-pml1e-cow (pml1 pml1e)
             (let ((entry (sys.int::memref-unsigned-byte-64 pml1 pml1e)))
               ;;(debug-print-line "    PML1e " pml1e "  " pml1 "  " entry)
               (when (logtest entry +arm64-tte-copy-on-write+)
                 (panic "Page table entry marked CoW when it shouldn't be."))
               (when (logtest entry +arm64-tte-dirty+)
                 (let ((frame (ash (pte-physical-address (sys.int::memref-unsigned-byte-64 pml1 pml1e)) -12)))
                   (snapshot-add-writeback-frame frame)
                   ;; Clear dirty and writable bits, set copy-on-write bit.
                   (setf (sys.int::memref-unsigned-byte-64 pml1 pml1e)
                         (make-pte frame
                                   :copy-on-write t)))))))
    (declare (dynamic-extent #'mark-level
                             #'mark-pml4e-cow #'mark-pml3e-cow
                             #'mark-pml2e-cow #'mark-pml1e-cow))
    (let ((pml4 (convert-to-pmap-address (logand (%ttbr0) (lognot #xFFF)))))
      ;; Skip wired area, entry 0.
      (loop for i from 1 below 64 ; pinned area to wired stack area.
         do (mark-pml4e-cow pml4 i))
      ;; Skip wired stack area, entry 64.
      (loop for i from 65 below 256 ; stack area to end of persistent memory.
         do (mark-pml4e-cow pml4 i))
      ;; Cover the part of the pinned area that got missed as well.
      (let ((pml3 (convert-to-pmap-address (logand (sys.int::memref-unsigned-byte-64 pml4 0) +arm64-tte-address-mask+))))
        ;; Skip first 2 entries, the wired area.
        (loop for i from 2 below 512
           do (mark-pml3e-cow pml3 i)))))
  (flush-tlb))
