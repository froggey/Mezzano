;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defparameter *object-tags-to-basic-types*
  #(array-t                    ; #b000000
    array-fixnum               ; #b000001
    array-bit                  ; #b000010
    array-unsigned-byte-2      ; #b000011
    array-unsigned-byte-4      ; #b000100
    array-unsigned-byte-8      ; #b000101
    array-unsigned-byte-16     ; #b000110
    array-unsigned-byte-32     ; #b000111
    array-unsigned-byte-64     ; #b001000
    array-signed-byte-1        ; #b001001
    array-signed-byte-2        ; #b001010
    array-signed-byte-4        ; #b001011
    array-signed-byte-8        ; #b001100
    array-signed-byte-16       ; #b001101
    array-signed-byte-32       ; #b001110
    array-signed-byte-64       ; #b001111
    array-single-float         ; #b010000
    array-double-float         ; #b010001
    array-short-float          ; #b010010
    array-long-float           ; #b010011
    array-complex-single-float ; #b010100
    array-complex-double-float ; #b010101
    array-complex-short-float  ; #b010110
    array-complex-long-float   ; #b010111
    array-xmm-vector           ; #b011000
    invalid-011001             ; #b011001
    invalid-011010             ; #b011010
    invalid-011011             ; #b011011
    simple-string              ; #b011100
    string                     ; #b011101
    simple-array               ; #b011110
    array                      ; #b011111
    bignum                     ; #b100000
    double-float               ; #b100001
    short-float                ; #b100010
    long-float                 ; #b100011
    complex-rational           ; #b100100
    complex-single-float       ; #b100101
    complex-double-float       ; #b100110
    complex-short-float        ; #b100111
    complex-long-float         ; #b101000
    ratio                      ; #b101001
    invalid-101010             ; #b101010
    invalid-101011             ; #b101011
    invalid-101100             ; #b101100
    invalid-101101             ; #b101101
    invalid-101110             ; #b101110
    invalid-101111             ; #b101111
    symbol                     ; #b110000
    structure-object           ; #b110001
    std-instance               ; #b110010
    xmm-vector                 ; #b110011
    thread                     ; #b110100
    unbound-value              ; #b110101
    function-reference         ; #b110110
    interrupt-frame            ; #b110111
    cons                       ; #b111000
    freelist-entry             ; #b111001
    weak-pointer               ; #b111010
    invalid-111011             ; #b111011
    function                   ; #b111100
    closure                    ; #b111101
    funcallable-instance       ; #b111110
    invalid-111111             ; #b111111
    )
  "Mapping from object tags to more friendly names.
Should be kept in sync with data-types.")

(defun print-n-allocated-objects-table (n-allocated-objects allocated-object-sizes allocated-classes)
  (dotimes (i (length n-allocated-objects))
    (when (not (zerop (aref n-allocated-objects i)))
      (format t "  ~A:~35T~:D objects. ~:D words.~%"
              (aref *object-tags-to-basic-types* i)
              (aref n-allocated-objects i)
              (aref allocated-object-sizes i))))
  (loop for i below (length allocated-classes) by 2 do
       (let ((class (aref allocated-classes i))
             (count (aref allocated-classes (1+ i))))
         (format t "  ~A: ~:D objects.~%" class count))))

(defun room (&optional (verbosity :default))
  (let ((total-used 0)
        (total 0))
    (format t "General area: ~:D/~:D words used (~D%).~%"
            (truncate *general-area-bump* 8) (truncate *general-area-limit* 8)
            (truncate (* *general-area-bump* 100) *general-area-limit*))
    (incf total-used (truncate *general-area-bump* 8))
    (incf total (truncate *general-area-limit* 8))
    (when (eql verbosity t)
      (multiple-value-bind (allocated-words total-words largest-free-space n-allocated-objects allocated-object-sizes allocated-classes)
          (area-info :general)
        (declare (ignore allocated-words total-words largest-free-space))
        (print-n-allocated-objects-table n-allocated-objects allocated-object-sizes allocated-classes)))
    (format t "Cons area: ~:D/~:D words used (~D%).~%"
            (truncate *cons-area-bump* 8) (truncate *cons-area-limit* 8)
            (truncate (* *cons-area-bump* 100) *cons-area-limit*))
    (incf total-used (truncate *cons-area-bump* 8))
    (incf total (truncate *cons-area-limit* 8))
    (when (eql verbosity t)
      (multiple-value-bind (allocated-words total-words largest-free-space n-allocated-objects allocated-object-sizes allocated-classes)
          (area-info :cons)
        (declare (ignore allocated-words total-words largest-free-space))
        (print-n-allocated-objects-table n-allocated-objects allocated-object-sizes allocated-classes)))
    (multiple-value-bind (allocated-words total-words largest-free-space n-allocated-objects allocated-object-sizes allocated-classes)
        (area-info :wired)
      (format t "Wired area: ~:D/~:D words allocated (~D%).~%"
              allocated-words total-words
              (truncate (* allocated-words 100) total-words))
      (format t "  Largest free area: ~:D words.~%" largest-free-space)
      (incf total-used allocated-words)
      (incf total total-words)
      (when (eql verbosity t)
        (print-n-allocated-objects-table n-allocated-objects allocated-object-sizes allocated-classes)
        (print-fragment-counts (pinned-area-fragment-counts :wired))))
    (multiple-value-bind (allocated-words total-words largest-free-space n-allocated-objects allocated-object-sizes allocated-classes)
        (area-info :pinned)
      (format t "Pinned area: ~:D/~:D words allocated (~D%).~%"
              allocated-words total-words
              (truncate (* allocated-words 100) total-words))
      (format t "  Largest free area: ~:D words.~%" largest-free-space)
      (incf total-used allocated-words)
      (incf total total-words)
      (when (eql verbosity t)
        (print-n-allocated-objects-table n-allocated-objects allocated-object-sizes allocated-classes)
        (print-fragment-counts (pinned-area-fragment-counts :pinned))))
    (format t "Total ~:D/~:D words used (~D%).~%"
            total-used total
            (truncate (* total-used 100) total))
    (when (not (eql mezzano.supervisor::*paging-disk* :freestanding))
      (multiple-value-bind (n-free-blocks total-blocks)
          (mezzano.supervisor:store-statistics)
        (format t "~:D/~:D store blocks used (~D%).~%"
                (- total-blocks n-free-blocks) total-blocks
                (truncate (* (- total-blocks n-free-blocks) 100) total-blocks))))
    (multiple-value-bind (n-free-page-frames total-page-frames)
        (mezzano.supervisor:physical-memory-statistics)
      (format t "~:D/~:D physical pages used (~D%).~%"
              (- total-page-frames n-free-page-frames) total-page-frames
              (truncate (* (- total-page-frames n-free-page-frames) 100)
                        total-page-frames))))
  (when (eql verbosity t)
    (format t "Paging disk is ~S.~%" mezzano.supervisor::*paging-disk*)
    (format t "Fudge-factor is ~D.~%" mezzano.supervisor::*store-fudge-factor*)
    (when mezzano.supervisor::*paging-read-only*
      (format t "Running in read-only mode.~%")))
  (values))

(defun %walk-pinned-area (base limit fn)
  (let ((address base))
    (loop
       (when (>= address limit)
         (return))
       (let ((size (align-up (size-of-pinned-area-allocation address) 2))
             ;; Carefully read the type, avoid bignums.
             (type (ldb (byte +object-type-size+ +object-type-shift+)
                        (memref-unsigned-byte-8 address 0))))
         (funcall fn
                  (if (eql type +object-tag-cons+)
                      (%%assemble-value (+ address 16) +tag-cons+)
                      (%%assemble-value address +tag-object+))
                  address
                  size)
         (incf address (* size 8))))))

(defun %walk-general-area (fn)
  (let ((finger 0))
    (loop
       (when (eql finger *general-area-bump*)
         (return))
       (let* ((address (logior finger
                               (ash +address-tag-general+ +address-tag-shift+)
                               *dynamic-mark-bit*))
              (object (%%assemble-value address +tag-object+))
              (size (object-size object)))
         (funcall fn object address size)
         (when (oddp size)
           (incf size))
         (incf finger (* size 8))))))

(defun %walk-cons-area (fn)
  (let ((finger 0))
    (loop
       (when (eql finger *cons-area-bump*)
         (return))
       (let* ((address (logior finger
                               (ash +address-tag-cons+ +address-tag-shift+)
                               *dynamic-mark-bit*))
              (object (%%assemble-value address +tag-cons+)))
         (funcall fn object address 2)
         (incf finger 16)))))

(defun walk-area (area fn)
  "Call FN with the value, address and size of every object in AREA.
FN will be called with the world stopped, it must not allocate."
  (check-type area (member :pinned :wired :general :cons))
  (mezzano.supervisor:with-world-stopped ()
    (case area
      (:pinned (%walk-pinned-area (* 2 1024 1024 1024) *pinned-area-bump* fn))
      (:wired (%walk-pinned-area (* 2 1024 1024) *wired-area-bump* fn))
      (:general (%walk-general-area fn))
      (:cons (%walk-cons-area fn)))))

(defun area-info (area)
  (let ((largest-free-space 0)
        (allocated-words 0)
        (total-words 0)
        (n-allocated-objects (make-array (ash 1 +object-type-size+)
                                         :initial-element 0))
        (allocated-objects-sizes (make-array (ash 1 +object-type-size+)
                                             :initial-element 0))
        ;; TODO: Should keep this sorted by address for binary searching, and
        ;; need to expand it when needed. Not possible with-world-stopped.
        (allocated-classes (make-array 1000 :fill-pointer 0)))
    (flet ((add-class (class)
             (loop
                for i below (length allocated-classes) by 2
                when (eql (aref allocated-classes i) class)
                do
                  (incf (aref allocated-classes (1+ i)))
                  (return)
                finally
                  ;; Not seen yet.
                  (vector-push class allocated-classes)
                  (vector-push 1 allocated-classes))))
      (walk-area area
                 (lambda (object address size)
                   (declare (ignore address))
                   (let ((tag (if (consp object)
                                  +object-tag-cons+
                                  (%object-tag object))))
                     (incf (svref n-allocated-objects tag))
                     (incf (svref allocated-objects-sizes tag) size)
                     (incf total-words size)
                     (when (not (eql tag +object-tag-freelist-entry+))
                       (incf allocated-words size))
                     (case tag
                       (#.+object-tag-freelist-entry+
                        (setf largest-free-space (max largest-free-space size)))
                       (#.+object-tag-std-instance+
                        (add-class (std-instance-class object)))
                       (#.+object-tag-funcallable-instance+
                        (add-class (funcallable-std-instance-class object)))
                       (#.+object-tag-structure-object+
                        (add-class (%struct-slot object 0))))))))
    (values allocated-words total-words largest-free-space
            n-allocated-objects allocated-objects-sizes allocated-classes)))

(defun print-fragment-counts (counts)
  (format t "  Free fragment counts:~%")
  (dotimes (i (length counts))
    (let ((n (aref counts i)))
      (when (not (zerop n))
        (format t "    ~:D words:~35T~D~%" (ash 1 i) n)))))

(defun pinned-area-fragment-counts (area)
  (let ((counts (make-array 64 :initial-element 0)))
    (walk-area area
               (lambda (object address size)
                 (declare (ignore address))
                 (when (%object-of-type-p object +object-tag-freelist-entry+)
                   (incf (aref counts (integer-length (1- size)))))))
    counts))
