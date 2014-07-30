;;;; Memory Memory Memory handling.
;;;; Physical memory tracking and page tables fiddling.

(in-package :sys.int)

(defstruct memory-map-entry
  base
  length
  type)

#+nil(defmethod print-object ((entry memory-map-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format t "~X-~X ~S"
            (memory-map-entry-base entry)
            (+ (memory-map-entry-base entry)
               (memory-map-entry-length entry))
            (memory-map-entry-type entry))))

(defun merge-memory-map-types (type1 type2)
  (assert (or (and (eql type1 :free)
                   (eql type2 :free))
              (and (not (eql type1 :free))
                   (not (eql type2 :free))))
          (type1 type2)
          "Attempting to merge free & non-free types.")
  (unless (listp type1)
    (setf type1 (list type1)))
  (unless (listp type2)
    (setf type2 (list type2)))
  (let ((merged (union type1 type2)))
    (if (null (rest merged))
        (first merged)
        merged)))

(defun canonicalize-memory-map (mmap)
  "Page-align all entries, merge overlapping regions and remove empty regions."
  (let ((new-mmap (map 'vector #'copy-memory-map-entry mmap)))
    ;; Round addresses and lengths to page boundaries.
    ;; Free memory ranges get shrunk, other ranges are grown.
    (loop for entry across new-mmap do
         (case (memory-map-entry-type entry)
           (:free
            (setf (memory-map-entry-base entry)
                  (logand (+ (memory-map-entry-base entry) #xFFF) (lognot #xFFF)))
            (setf (memory-map-entry-length entry)
                  (logand (memory-map-entry-length entry) (lognot #xFFF))))
           (t
            (setf (memory-map-entry-base entry)
                  (logand (memory-map-entry-base entry) (lognot #xFFF)))
            (setf (memory-map-entry-length entry)
                  (logand (+ (memory-map-entry-length entry) #xFFF) (lognot #xFFF))))))
    ;; Split overlapping regions. The horrible part.
    (do ((saw-overlap t))
        ((not saw-overlap))
      (setf saw-overlap nil)
      ;; Remove zero-size areas.
      #+nil(delete-if #'zerop new-mmap :key #'memory-map-entry-length)
      (let ((new (make-array (length new-mmap) :adjustable t :fill-pointer 0)))
        (dotimes (i (length new-mmap))
          (unless (zerop (memory-map-entry-length (aref new-mmap i)))
            (vector-push-extend (aref new-mmap i) new)))
        (setf new-mmap new))
      ;; Sort before scanning.
      (sort new-mmap #'< :key #'memory-map-entry-base)
      (dotimes (i (1- (length new-mmap)))
        (let ((a (elt new-mmap i))
              (b (elt new-mmap (1+ i))))
          (when (< (memory-map-entry-base b) (+ (memory-map-entry-base a) (memory-map-entry-length a)))
            (setf saw-overlap t)
            ;; Overlap. Figure out which region to split.
            #+(or)(format t "~S overlaps with ~S~%" a b)
            (cond ((and (eql (memory-map-entry-type a) :free)
                        (eql (memory-map-entry-type b) :free))
                   ;; Both sides are free, merge them.
                   (setf (memory-map-entry-length a) (- (+ (memory-map-entry-base b)
                                                           (memory-map-entry-length b))
                                                        (memory-map-entry-base a))
                         (memory-map-entry-length b) 0))
                  ((eql (memory-map-entry-type a) :free)
                   (cond ((eql (memory-map-entry-base a) (memory-map-entry-base b))
                          (setf (memory-map-entry-length a) (max 0 (- (+ (memory-map-entry-base a)
                                                                         (memory-map-entry-length a))
                                                                      (+ (memory-map-entry-base b)
                                                                         (memory-map-entry-length b))))
                                (memory-map-entry-base a) (+ (memory-map-entry-base b)
                                                             (memory-map-entry-length b))))
                         (t (vector-push-extend (make-memory-map-entry
                                                 :base (+ (memory-map-entry-base b) (memory-map-entry-length b))
                                                 :length (max 0 (- (+ (memory-map-entry-base a) (memory-map-entry-length a))
                                                                   (+ (memory-map-entry-base b) (memory-map-entry-length b))))
                                                 :type :free)
                                                new-mmap)
                            (setf (memory-map-entry-length a) (- (memory-map-entry-base b)
                                                                 (memory-map-entry-base a))))))
                  ((eql (memory-map-entry-type b) :free)
                   (setf (memory-map-entry-length b) (max 0 (- (+ (memory-map-entry-base b)
                                                                  (memory-map-entry-length b))
                                                               (+ (memory-map-entry-base a)
                                                                  (memory-map-entry-length a))))
                         (memory-map-entry-base b) (+ (memory-map-entry-base a)
                                                      (memory-map-entry-length a))))
                  (t ;; Both sides are reserved, merge them.
                   (setf (memory-map-entry-type a) (merge-memory-map-types (memory-map-entry-type a)
                                                                           (memory-map-entry-type b)))
                   (setf (memory-map-entry-length a) (- (+ (memory-map-entry-base b)
                                                           (memory-map-entry-length b))
                                                        (memory-map-entry-base a))
                         (memory-map-entry-length b) 0))))
          ;; Merge adjacent free regions
          (when (and (eql (memory-map-entry-type a) :free)
                     (eql (memory-map-entry-type b) :free)
                     (eql (+ (memory-map-entry-base a)
                             (memory-map-entry-length a))
                          (memory-map-entry-base b)))
            (setf saw-overlap t)
            #+(or)(format t "Merging free regions ~S ~S.~%" a b)
            (setf (memory-map-entry-length a) (- (+ (memory-map-entry-base b)
                                                    (memory-map-entry-length b))
                                                 (memory-map-entry-base a))
                  (memory-map-entry-length b) 0))))
      #+(or)(format t "Iteration complete:~%")
      #+(or)(print-memory-map new-mmap))
    new-mmap))

(defun print-memory-map (mmap)
  "Print a memory map in a friendly format."
  (loop
     for entry across mmap
     for i from 0 do
       (format t "~3D: ~16,'0X ~16,'0X  ~:(~A~)~%" i
               (memory-map-entry-base entry)
               (+ (memory-map-entry-base entry)
                  (memory-map-entry-length entry))
               (memory-map-entry-type entry))))

(defun test-mmap (&optional suppress-memory-map)
  (let ((mmap (cond (*kboot-tag-list*
                     (kboot-memory-map))
                    (t #()))))
    (format t "Bootloader provided memory map:~%")
    (print-memory-map mmap)
    (setf mmap (canonicalize-memory-map mmap))
    (format t "In canonical form:~%")
    (print-memory-map mmap)
    mmap))

(defun test-bad-mmap ()
  (flet ((test (input expected)
           (format t "Test input:~%")
           (print-memory-map input)
           (format t "Test result:~%")
           (print-memory-map (canonicalize-memory-map input))
           (format t "Expected result:~%")
           (print-memory-map expected)))
    ;; #x1000-#x3000 Free
    ;; #x2000-#x3000 Reserved
    ;; =>
    ;; #x1000-#x2000 Free
    ;; #x2000-#x3000 Reserved
    (test (vector (make-memory-map-entry :base #x1000
                                         :length #x2000
                                         :type :free)
                  (make-memory-map-entry :base #x2000
                                         :length #x1000
                                         :type :reserved))
          (vector (make-memory-map-entry :base #x1000
                                         :length #x1000
                                         :type :free)
                  (make-memory-map-entry :base #x2000
                                         :length #x1000
                                         :type :reserved)))
    ;; #x1000-#x3000 Free
    ;; #x1000-#x2000 Reserved
    ;; =>
    ;; #x1000-#x2000 Reserved
    ;; #x2000-#x3000 Free
    (test (vector (make-memory-map-entry :base #x1000
                                         :length #x2000
                                         :type :free)
                  (make-memory-map-entry :base #x1000
                                         :length #x1000
                                         :type :reserved))
          (vector (make-memory-map-entry :base #x1000
                                         :length #x1000
                                         :type :reserved)
                  (make-memory-map-entry :base #x2000
                                         :length #x1000
                                         :type :free)))
    ;; #x1000-#x2000 Reserved
    ;; #x1000-#x3000 Free
    ;; =>
    ;; #x1000-#x2000 Reserved
    ;; #x2000-#x3000 Free
    (test (vector (make-memory-map-entry :base #x1000
                                         :length #x1000
                                         :type :reserved)
                  (make-memory-map-entry :base #x1000
                                         :length #x2000
                                         :type :free))
          (vector (make-memory-map-entry :base #x1000
                                         :length #x1000
                                         :type :reserved)
                  (make-memory-map-entry :base #x2000
                                         :length #x1000
                                         :type :free)))
    ;; #x1000-#x3000 Reserved
    ;; #x2000-#x4000 Free
    ;; =>
    ;; #x1000-#x3000 Reserved
    ;; #x3000-#x4000 Free
    (test (vector (make-memory-map-entry :base #x1000
                                         :length #x2000
                                         :type :reserved)
                  (make-memory-map-entry :base #x2000
                                         :length #x2000
                                         :type :free))
          (vector (make-memory-map-entry :base #x1000
                                         :length #x2000
                                         :type :reserved)
                  (make-memory-map-entry :base #x3000
                                         :length #x1000
                                         :type :free)))
    ))
