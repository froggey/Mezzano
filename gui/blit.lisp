;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Optimized pluggable blitter functions.
;;;; The low-level pixel blending functions use a custom calling
;;;; convetion and must not be called directly from lisp.
;;;; RAX contains the source pixel.
;;;; R9 contains the destination simple UB32 vector.
;;;; RDI contains the index of the destination pixel.
;;;; RAX, RCX, RDX and MMX/SSE registers are caller save.
;;;; All other registers are callee save.

(in-package :mezzano.gui)

(defun compute-blit-info-dest-src (nrows ncols from-array from-row from-col to-array to-row to-col)
  "Clamp parameters to array boundaries, return the stride of both arrays and their undisplaced, non-complex base arrays."
  (let ((from-width (array-dimension from-array 1))
        (from-height (array-dimension from-array 0))
        (from-offset 0)
        (to-width (array-dimension to-array 1))
        (to-height (array-dimension to-array 0))
        (to-offset 0))
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (decf from-row to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (decf from-col to-col)
      (setf to-col 0))
    ;; Clamp from row/column.
    (when (< from-row 0)
      (incf nrows from-row)
      (decf to-row from-row)
      (setf from-row 0))
    (when (< from-col 0)
      (incf ncols from-col)
      (decf to-col from-col)
      (setf from-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (max (min nrows (- to-height to-row) (- from-height from-row)) 0))
    (setf ncols (max (min ncols (- to-width to-col) (- from-width from-col)) 0))
    ;; Undisplace displaced arrays
    (multiple-value-bind (from-displaced-to from-displaced-offset)
        (array-displacement from-array)
      (when from-displaced-to
        (setf from-array from-displaced-to
              from-offset from-displaced-offset)))
    (multiple-value-bind (to-displaced-to to-displaced-offset)
        (array-displacement to-array)
      (when to-displaced-to
        (setf to-array to-displaced-to
              to-offset to-displaced-offset)))
    (incf from-offset (+ (* from-row from-width) from-col))
    (incf to-offset (+ (* to-row to-width) to-col))
    (values nrows ncols
            (if (sys.int::%simple-1d-array-p from-array)
                from-array
                (sys.int::%complex-array-storage from-array))
            from-offset from-width
            (if (sys.int::%simple-1d-array-p to-array)
                to-array
                (sys.int::%complex-array-storage to-array))
            to-offset to-width)))

(defun compute-blit-info-dest (nrows ncols to-array to-row to-col)
  "Clamp parameters to array boundaries, return the stride of the array and its undisplaced, non-complex base array."
  (let ((to-width (array-dimension to-array 1))
        (to-height (array-dimension to-array 0))
        (to-offset 0))
    ;; Only need to clamp values below zero here. nrows/ncols will
    ;; end up negative if the source/target positions are too large.
    ;; Clamp to row/column.
    (when (< to-row 0)
      (incf nrows to-row)
      (setf to-row 0))
    (when (< to-col 0)
      (incf ncols to-col)
      (setf to-col 0))
    ;; Clamp nrows/ncols.
    (setf nrows (min nrows (- to-height to-row)))
    (setf ncols (min ncols (- to-width to-col)))
    ;; Undisplace displaced arrays
    (multiple-value-bind (to-displaced-to to-displaced-offset)
        (array-displacement to-array)
      (when to-displaced-to
        (setf to-array to-displaced-to
              to-offset to-displaced-offset)))
    (incf to-offset (+ (* to-row to-width) to-col))
    (values nrows ncols
            (if (sys.int::%simple-1d-array-p to-array)
                to-array
                (sys.int::%complex-array-storage to-array))
            to-offset to-width)))

(declaim (inline simple-ub32-vector-p simple-ub8-vector-p simple-ub1-vector-p))

(defun simple-ub32-vector-p (object)
  (typep object '(simple-array (unsigned-byte 32) (*))))

(defun simple-ub8-vector-p (object)
  (typep object '(simple-array (unsigned-byte 8) (*))))

(defun simple-ub1-vector-p (object)
  (typep object '(simple-array (unsigned-byte 1) (*))))

;;; High-level functions.

(defun 2d-array-bitblt (nrows ncols from-array from-row from-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols from from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols from-array from-row from-col to-array to-row to-col)
    (assert (simple-ub32-vector-p from))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitblt-line #'%%set-one-argb8888-argb8888
                      to to-offset
                      ncols
                      from from-offset)
        (incf from-offset from-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitblt-blend (nrows ncols from-array from-row from-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols from from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols from-array from-row from-col to-array to-row to-col)
    (assert (simple-ub32-vector-p from))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitblt-line #'%%alpha-blend-one-argb8888-argb8888
                      to to-offset
                      ncols
                      from from-offset)
        (incf from-offset from-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitblt-xor (nrows ncols from-array from-row from-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols from from-offset from-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols from-array from-row from-col to-array to-row to-col)
    (assert (simple-ub32-vector-p from))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitblt-line #'%%xor-one-argb8888-argb8888
                      to to-offset
                      ncols
                      from from-offset)
        (incf from-offset from-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset (nrows ncols colour to-array to-row to-col)
  (multiple-value-bind (nrows ncols to to-offset to-stride)
      (compute-blit-info-dest nrows ncols to-array to-row to-col)
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-line #'%%set-one-argb8888-argb8888
                      to to-offset
                      ncols
                      colour)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-blend (nrows ncols colour to-array to-row to-col)
  (when (eql (ldb (byte 8 24) colour) #xFF)
    ;; Fully opaque, just use BITSET.
    (return-from 2d-array-bitset-blend (2d-array-bitset nrows ncols colour to-array to-row to-col)))
  (multiple-value-bind (nrows ncols to to-offset to-stride)
      (compute-blit-info-dest nrows ncols to-array to-row to-col)
    (assert (simple-ub32-vector-p to))
    (when (and (not (eql (ldb (byte 8 24) colour) 0))
               (> ncols 0))
      (dotimes (y nrows)
        (%bitset-line #'%%alpha-blend-one-argb8888-argb8888
                      to to-offset
                      ncols
                      colour)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-xor (nrows ncols colour to-array to-row to-col)
  "Exclusive OR a rectangle with COLOUR."
  (multiple-value-bind (nrows ncols to to-offset to-stride)
      (compute-blit-info-dest nrows ncols to-array to-row to-col)
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-line #'%%xor-one-argb8888-argb8888
                      to to-offset
                      ncols
                      colour)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-mask-1 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub1-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-mask-1-line #'%%set-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-blend-mask-1 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub1-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (and (not (eql (ldb (byte 8 24) colour) 0))
               (> ncols 0))
      (dotimes (y nrows)
        (%bitset-mask-1-line #'%%alpha-blend-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-xor-mask-1 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub1-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-mask-1-line #'%%xor-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-mask-8 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub8-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-mask-8-line #'%%set-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-blend-mask-8 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub8-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (and (not (eql (ldb (byte 8 24) colour) 0))
               (> ncols 0))
      (dotimes (y nrows)
        (%bitset-mask-8-line #'%%alpha-blend-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))

(defun 2d-array-bitset-xor-mask-8 (nrows ncols colour mask-array mask-row mask-col to-array to-row to-col)
  (multiple-value-bind (nrows ncols mask mask-offset mask-stride to to-offset to-stride)
      (compute-blit-info-dest-src nrows ncols mask-array mask-row mask-col to-array to-row to-col)
    (assert (simple-ub8-vector-p mask))
    (assert (simple-ub32-vector-p to))
    (when (> ncols 0)
      (dotimes (y nrows)
        (%bitset-mask-8-line #'%%xor-one-argb8888-argb8888
                             to to-offset
                             ncols
                             mask mask-offset
                             colour)
        (incf mask-offset mask-stride)
        (incf to-offset to-stride)))))
