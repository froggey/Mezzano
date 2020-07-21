;;;; ROOM and other heap groveling functions

(in-package :mezzano.internals)

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
    invalid-011000             ; #b011000
    invalid-011001             ; #b011001
    invalid-011010             ; #b011010
    invalid-011011             ; #b011011
    simple-string              ; #b011100
    string                     ; #b011101
    simple-array               ; #b011110
    array                      ; #b011111
    bignum                     ; #b100000
    ratio                      ; #b100001
    double-float               ; #b100010
    short-float                ; #b100011
    long-float                 ; #b100100
    complex-rational           ; #b100101
    complex-single-float       ; #b100110
    complex-double-float       ; #b100111
    complex-short-float        ; #b101000
    complex-long-float         ; #b101001
    invalid-101010             ; #b101010
    invalid-101011             ; #b101011
    invalid-101100             ; #b101100
    invalid-101101             ; #b101101
    mezzano.runtime::symbol-value-cell ; #b101110
    mezzano.simd:mmx-vector    ; #b101111
    symbol                     ; #b110000
    invalid-110001             ; #b110001
    invalid-110010             ; #b110010
    mezzano.simd:sse-vector    ; #b110011
    invalid-110100             ; #b110100
    instance                   ; #b110101
    function-reference         ; #b110110
    interrupt-frame            ; #b110111
    cons                       ; #b111000
    freelist-entry             ; #b111001
    weak-pointer               ; #b111010
    mezzano.delimited-continuations:delimited-continuation ; #b111011
    function                   ; #b111100
    funcallable-instance       ; #b111101
    closure                    ; #b111110
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
  (let ((classes-and-counts
         (sort (loop
                  for i below (length allocated-classes) by 2
                  collect (cons (aref allocated-classes i)
                                (aref allocated-classes (1+ i))))
               #'>
               :key #'cdr)))
    (loop
       for (class . count) in classes-and-counts
       do (format t "  ~A: ~:D objects. ~:D words.~%"
                  class count
                  (* count (1+ (layout-heap-size
                                (mezzano.clos:class-layout class))))))))

(defun room-pinned-area (area verbosity)
  (multiple-value-bind (allocated-words total-words largest-free-space n-allocated-objects allocated-object-sizes allocated-classes)
      (area-info area)
    (format t "~:(~A~) area: ~:D/~:D words allocated (~D%).~%"
            area
            allocated-words total-words
            (truncate (* allocated-words 100) total-words))
    (format t "  Largest free area: ~:D words.~%" largest-free-space)
    (when (eql verbosity t)
      (print-n-allocated-objects-table n-allocated-objects allocated-object-sizes allocated-classes)
      (print-fragment-counts (pinned-area-fragment-counts area)))
    (values allocated-words total-words)))

(defun room (&optional (verbosity :default))
  (let ((total-used 0)
        (total 0))
    (let ((general-bump (+ *general-area-old-gen-bump* *general-area-young-gen-bump*))
          (general-limit (+ *general-area-old-gen-limit* *general-area-young-gen-limit*)))
      (format t "General area: ~:D/~:D words used (~D%).~%"
              (truncate general-bump 8) (truncate general-limit 8)
              (truncate (* general-bump 100) general-limit))
      (incf total-used (truncate general-bump 8))
      (incf total (truncate general-limit 8)))
    (when (eql verbosity t)
      (format t "  General area old gen: ~:D/~:D words used (~D%).~%"
              (truncate *general-area-old-gen-bump* 8) (truncate *general-area-old-gen-limit* 8)
              (truncate (* *general-area-old-gen-bump* 100) *general-area-old-gen-limit*))
      (format t "  General area young gen: ~:D/~:D words used (~D%).~%"
              (truncate *general-area-young-gen-bump* 8) (truncate *general-area-young-gen-limit* 8)
              (truncate (* *general-area-young-gen-bump* 100) (max 1 *general-area-young-gen-limit*)))
      (multiple-value-bind (allocated-words total-words largest-free-space n-allocated-objects allocated-object-sizes allocated-classes)
          (area-info :general)
        (declare (ignore allocated-words total-words largest-free-space))
        (print-n-allocated-objects-table n-allocated-objects allocated-object-sizes allocated-classes)))
    (let ((cons-bump (+ *cons-area-old-gen-bump* *cons-area-young-gen-bump*))
          (cons-limit (+ *cons-area-old-gen-limit* *cons-area-young-gen-limit*)))
      (format t "Cons area: ~:D/~:D words used (~D%).~%"
              (truncate cons-bump 8) (truncate cons-limit 8)
              (truncate (* cons-bump 100) cons-limit))
      (incf total-used (truncate cons-bump 8))
      (incf total (truncate cons-limit 8)))
    (when (eql verbosity t)
      (format t "  Cons area old gen: ~:D/~:D words used (~D%).~%"
              (truncate *cons-area-old-gen-bump* 8) (truncate *cons-area-old-gen-limit* 8)
              (truncate (* *cons-area-old-gen-bump* 100) *cons-area-old-gen-limit*))
      (format t "  Cons area young gen: ~:D/~:D words used (~D%).~%"
              (truncate *cons-area-young-gen-bump* 8) (truncate *cons-area-young-gen-limit* 8)
              (truncate (* *cons-area-young-gen-bump* 100) (max 1 *cons-area-young-gen-limit*)))
      (multiple-value-bind (allocated-words total-words largest-free-space n-allocated-objects allocated-object-sizes allocated-classes)
          (area-info :cons)
        (declare (ignore allocated-words total-words largest-free-space))
        (print-n-allocated-objects-table n-allocated-objects allocated-object-sizes allocated-classes)))
    (multiple-value-bind (allocated-words total-words)
        (room-pinned-area :wired verbosity)
      (incf total-used allocated-words)
      (incf total total-words))
    (multiple-value-bind (allocated-words total-words)
        (room-pinned-area :wired-function verbosity)
      (incf total-used allocated-words)
      (incf total total-words))
    (multiple-value-bind (allocated-words total-words)
        (room-pinned-area :function verbosity)
      (incf total-used allocated-words)
      (incf total total-words))
    (multiple-value-bind (allocated-words total-words)
        (room-pinned-area :pinned verbosity)
      (incf total-used allocated-words)
      (incf total total-words))
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

(defun %walk-general-area-1 (fn base length)
  (let ((finger 0))
    (loop
       (when (eql finger length)
         (return))
       (let* ((address (logior base finger))
              (object (%%assemble-value address +tag-object+))
              (size (object-size object)))
         (funcall fn object address size)
         (when (oddp size)
           (incf size))
         (incf finger (* size 8))))))

(defun %walk-general-area (fn)
  ;; Young gen.
  (%walk-general-area-1 fn
                        (logior (ash +address-tag-general+ +address-tag-shift+)
                                *young-gen-newspace-bit*)
                        *general-area-young-gen-bump*)
  ;; Old gen.
  (%walk-general-area-1 fn
                        (logior (ash +address-tag-general+ +address-tag-shift+)
                                +address-old-generation+
                                *old-gen-newspace-bit*)
                        *general-area-old-gen-bump*))

(defun %walk-cons-area-1 (fn base length)
  (let ((finger 0))
    (loop
       (when (eql finger length)
         (return))
       (let* ((address (logior base finger))
              (object (%%assemble-value address +tag-cons+)))
         (funcall fn object address 2)
         (incf finger 16)))))

(defun %walk-cons-area (fn)
  ;; Young gen.
  (%walk-cons-area-1 fn
                     (logior (ash +address-tag-cons+ +address-tag-shift+)
                             *young-gen-newspace-bit*)
                     *cons-area-young-gen-bump*)
  ;; Old gen.
  (%walk-cons-area-1 fn
                     (logior (ash +address-tag-cons+ +address-tag-shift+)
                             +address-old-generation+
                             *old-gen-newspace-bit*)
                     *cons-area-old-gen-bump*))

(defparameter *area-names* '(:pinned :wired :wired-function :function :general :cons))

(defun walk-all-areas (fn)
  (dolist (area *area-names*)
    (walk-area area fn)))

(defun walk-area (area fn)
  "Call FN with the value, address and size of every object in AREA.
FN will be called with the world stopped, it must not allocate."
  (check-type area (member :pinned :wired :wired-function :function :general :cons))
  (mezzano.supervisor:with-world-stopped ()
    (case area
      (:pinned (%walk-pinned-area *pinned-area-base* *pinned-area-bump* fn))
      (:wired (%walk-pinned-area *wired-area-base* *wired-area-bump* fn))
      (:wired-function (%walk-pinned-area *wired-function-area-limit* *function-area-base* fn))
      (:function (%walk-pinned-area *function-area-base* *function-area-limit* fn))
      (:general (%walk-general-area fn))
      (:cons (%walk-cons-area fn)))))

(defun area-usage (area)
  "Returns words used and committed."
  (ecase area
    (:pinned
     (values *pinned-area-usage* (/ (- *pinned-area-bump* *pinned-area-base*) 8)))
    (:wired
     (values *wired-area-usage* (/ (- *wired-area-bump* *wired-area-base*) 8)))
    (:wired-function
     (values *wired-function-area-usage* (/ (- *function-area-base* *wired-function-area-limit*) 8)))
    (:function
     (values *function-area-usage* (/ (- *function-area-limit* *function-area-base*) 8)))
    (:general
     (values (/ (+ *general-area-old-gen-bump* *general-area-young-gen-bump*) 8)
             (/ (+ *general-area-old-gen-limit* *general-area-young-gen-limit*) 8)))
    (:cons
     (values (/ (+ *cons-area-old-gen-bump* *cons-area-young-gen-bump*) 8)
             (/ (+ *cons-area-old-gen-limit* *cons-area-young-gen-limit*) 8)))))

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
                       ((#.+object-tag-instance+
                         #.+object-tag-funcallable-instance+)
                        (let ((layout (%instance-layout object)))
                          (add-class
                           (layout-class
                            (if (layout-p layout)
                                layout
                                (mezzano.runtime::obsolete-instance-layout-old-layout layout)))))))))))
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

(defun walk-object-references (object fn)
  "Walk an object and call FN with the object, the value of any unspecialized field, and the field index.
An unspecialized field is a field that may hold any Lisp value, not specialized
on floats or integers."
  (cond ((consp object)
         (funcall fn object (car object) :car)
         (funcall fn object (cdr object) :cdr))
        ((%value-has-tag-p object +tag-object+)
         (case (%object-tag object)
           (#.+object-tag-array-t+
            (dotimes (i (length object))
              (funcall fn object (svref object i) i)))
           ((#.+object-tag-simple-string+
             #.+object-tag-string+
             #.+object-tag-simple-array+
             #.+object-tag-array+)
            ;; Don't include the axes, they're not references.
            (funcall fn object (%complex-array-storage object) +complex-array-storage+)
            (funcall fn object (%complex-array-fill-pointer object) +complex-array-fill-pointer+)
            (funcall fn object (%complex-array-info object) +complex-array-info+))
           (#.+object-tag-ratio+
            (funcall fn object (numerator object) +ratio-numerator+)
            (funcall fn object (denominator object) +ratio-denominator+))
           (#.+object-tag-complex-rational+
            (funcall fn object (realpart object) +complex-realpart+)
            (funcall fn object (imagpart object) +complex-imagpart+))
           (#.+object-tag-symbol-value-cell+
            (dotimes (i (%object-header-data object))
              (funcall fn object (%object-ref-t object i) i)))
           (#.+object-tag-symbol+
            (dotimes (i 5)
              (funcall fn object (%object-ref-t object i) i)))
           (#.+object-tag-instance+
            ;; Gets a bit hairy with gc link-snapping...
            (let* ((direct-layout (%instance-layout object))
                   (layout (if (layout-p direct-layout)
                               direct-layout
                               (mezzano.runtime::obsolete-instance-layout-old-layout
                                direct-layout)))
                   (heap-layout (layout-heap-layout layout)))
              (funcall fn object layout -1)
              (cond ((eql heap-layout 't)
                     (dotimes (i (layout-heap-size layout))
                       (funcall fn object (%object-ref-t object i) i)))
                    (heap-layout
                     (dotimes (i (layout-heap-size layout))
                       (when (eql (bit heap-layout i) 1)
                         (funcall fn object (%object-ref-t object i) i)))))))
           (#.+object-tag-function-reference+
            (funcall fn object (%object-ref-t object +fref-name+) +fref-name+)
            (funcall fn object (%object-ref-t object +fref-function+) +fref-function+))
           ;;+object-tag-weak-pointer+
           (#.+object-tag-function+
            (let ((pool-base (function-pool-base object)))
              (dotimes (i (function-pool-size object))
                (funcall fn object (%object-ref-t object (+ pool-base i)) (+ pool-base i)))))
           (#.+object-tag-funcallable-instance+
            ;; Gets a bit hairy with gc link-snapping...
            (let* ((direct-layout (%instance-layout object))
                   (layout (if (layout-p direct-layout)
                               direct-layout
                               (mezzano.runtime::obsolete-instance-layout-old-layout
                                direct-layout)))
                   (heap-layout (layout-heap-layout layout)))
              (funcall fn object layout -1)
              (funcall fn object (%object-ref-t fn +funcallable-instance-function+) +funcallable-instance-function+)
              (cond ((eql heap-layout 't)
                     (dotimes (i (layout-heap-size layout))
                       (funcall fn object (%object-ref-t object i) i)))
                    (heap-layout
                     (dotimes (i (layout-heap-size layout))
                       (when (eql (bit heap-layout i) 1)
                         (funcall fn object (%object-ref-t object i) i)))))))
           (#.+object-tag-closure+
            (loop
               for i from 1 below (%object-header-data object)
               do (funcall fn object (%object-ref-t object i) i)))))))
