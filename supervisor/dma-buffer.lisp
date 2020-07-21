;;;; DMA buffers
;;;;
;;;; These provide access to physical memory in a safe and structured way.

(in-package :mezzano.supervisor)

(defstruct (dma-buffer
             (:area :wired)
             (:constructor %make-dma-buffer)
             (:copier nil))
  (name nil)
  (length -1 :read-only t :type fixnum)
  (boot-id nil :read-only t)
  (persistent-p nil :read-only t)
  (virtual-address -1 :read-only t :type fixnum)
  (scatter/gather-vector nil :type (or simple-vector null))
  (cache-mode :write-back :read-only t :type (member :write-back
                                                     :write-through
                                                     :write-combining
                                                     :uncached)))

(defconstant +dma-buffer-guard-size+ #x200000)
(sys.int::defglobal *dma-buffer-virtual-address-bump*)

(defun make-dma-buffer (length &key name persistent contiguous 32-bit (cache-mode :write-back) (errorp t))
  "Allocate a new DMA buffer of the given length.
NAME         - Name of the DMA buffer, for debugging.
PERSISTENT   - If true, the memory will be saved by a snapshot, though the
               physical addresses may change between boots.
CONTIGUOUS   - Allocate physical memory in a single contiguous chunk. The
               resulting DMA buffer will have exactly one scatter/gather entry.
               Allocation may fail and signal an error even if there is
               sufficient memory, as it might not be contiguous.
32-BIT       - If true, all allocated physical memory will be below the 32-bit
               boundary.
CACHE-MODE   - Select between :WRITE-BACK, :WRITE-THROUGH, :WRITE-COMBINING, and
               :UNCACHED caching modes. Defaults to :WRITE-BACK.
ERRORP       - If true, then a normal Lisp error will be signalled if memory
               can't be allocated. If false, NIL will be returned on allocation
               failure instead.

LENGTH is always internally rounded up to the nearest whole page and returned
physical memory will always be page-aligned.

If the DMA buffer is not persistent, then the memory will be lost and become
inaccessible when the machine is booted. Attempts to access it from Lisp (either
directly or via the buffer array) will signal a DMA-BUFFER-EXPIRED error."
  (declare (mezzano.compiler::closure-allocation :wired))
  (assert (not (and persistent (> length #x1000) contiguous))
          (persistent length contiguous)
          "Multi-page contiguous persistent dma-buffers are not implemented.")
  (assert (not (and persistent 32-bit))
          (persistent 32-bit)
          "32-bit-only persistent dma-buffers are not supported.")
  (assert (eql cache-mode :write-back)
          (cache-mode)
          "Non-write-back cache modes not implemented.")
  (check-type length (integer 0))
  (with-snapshot-inhibited ()
    (let* ((aligned-length (align-up (if (zerop length) 1 length) #x1000))
           (virtual-address (sys.int::atomic-incf
                             *dma-buffer-virtual-address-bump*
                             (+ (align-up aligned-length #x200000)
                                +dma-buffer-guard-size+)))
           (sg-vec (if persistent
                       (alloc-persistent-sg-vec length errorp)
                       (alloc-sg-vec length
                                     :contiguous contiguous
                                     :32-bit 32-bit
                                     :errorp errorp)))
           (did-map nil)
           (successp nil))
      (when (not sg-vec)
        ;; Allocation failure with false ERRORP.
        (return-from make-dma-buffer nil))
      ;; Be very careful to ensure that the physical memory is freed if something
      ;; goes wrong during creation.
      (unwind-protect
           (let* ((boot-id (current-boot-id))
                  (dma-buffer (%make-dma-buffer :name name
                                                :length length
                                                :boot-id boot-id
                                                :persistent-p (not (not persistent))
                                                :virtual-address virtual-address
                                                :scatter/gather-vector sg-vec
                                                :cache-mode cache-mode)))
             (if persistent
                 (map-persistent-sg-vec virtual-address sg-vec cache-mode)
                 (map-sg-vec virtual-address sg-vec cache-mode))
             (setf did-map t)
             (sys.int::make-weak-pointer
              dma-buffer
              :finalizer (lambda ()
                           (with-snapshot-inhibited ()
                             (when (and (svref sg-vec 0) ; Avoid released buffers
                                        (eql boot-id (current-boot-id)))
                               (unmap-sg-vec virtual-address sg-vec))))
              :area :wired)
             ;; Everything succeeded, don't free the SG-VEC.
             (setf successp t)
             dma-buffer)
        (when (not successp)
          (when did-map
            (release-memory-range virtual-address aligned-length))
          (when (not persistent)
            (dotimes (i (truncate (sys.int::simple-vector-length sg-vec) 2))
              (release-physical-pages (truncate (svref sg-vec (* i 2)) #x1000)
                                      (truncate (align-up (svref sg-vec (1+ (* i 2))) #x1000) #x1000)))))))))

(defun dma-buffer-expired-p (dma-buffer)
  "Return true if RELEASE-DMA-BUFFER was called on DMA-BUFFER."
  (or (not (svref (dma-buffer-scatter/gather-vector dma-buffer) 0))
      (not (eql (dma-buffer-boot-id dma-buffer) (current-boot-id)))))

(defun release-dma-buffer (dma-buffer)
  "Release the underlying memory associated with DMA-BUFFER.
After a call to this function, accesses to the memory area will signal
a DMA-BUFFER-EXPIRED error.
Calling this function is not strictly necessary, the GC will release the
associated memory eventually."
  (with-snapshot-inhibited ()
    (let ((sg-vec (dma-buffer-scatter/gather-vector dma-buffer)))
      (when (svref sg-vec 0)
        (when (eql (dma-buffer-boot-id dma-buffer) (current-boot-id))
          (unmap-sg-vec (dma-buffer-virtual-address dma-buffer) sg-vec))
        ;; Mark the buffer as released.
        (setf (svref sg-vec 0) nil))))
  (values))

(defun dma-buffer-contiguous-p (dma-buffer)
  "Returns true if DMA-BUFFER is contiguous in physical memory."
  (eql (dma-buffer-n-sg-entries dma-buffer) 1))

(defun dma-buffer-cache-flush (dma-buffer &optional (start 0) end)
  "Ensure that CPU caches are coherent with system memory for this DMA buffer."
  (declare (ignore start end)) ; flush the entire buffer for now.
  (ecase (dma-buffer-cache-mode dma-buffer)
    ((:write-back :write-through :write-combining)
     ;; TODO: do this properly. Use CLFLUSH, etc.
     (dma-write-barrier))
    (:uncached))
  (values))

(defun dma-buffer-physical-address (dma-buffer)
  "Return the physial address of DMA-BUFFER.
This is only valid on DMA buffers that are contiguous or <= one page in size."
  (assert (dma-buffer-contiguous-p dma-buffer))
  (values (dma-buffer-sg-entry dma-buffer 0)))

(defun dma-buffer-n-sg-entries (dma-buffer)
  "Return the number of scatter/gather entries in DMA-BUFFER.
For contiguous buffers this will always return 1."
  (when (dma-buffer-expired-p dma-buffer)
    (error "~S has expired" dma-buffer))
  (let ((sg-vec (dma-buffer-scatter/gather-vector dma-buffer)))
    (if (dma-buffer-persistent-p dma-buffer)
        (sys.int::simple-vector-length sg-vec)
        (truncate (sys.int::simple-vector-length sg-vec) 2))))

(defun dma-buffer-sg-entry (dma-buffer entry-index)
  "Return the physical address and length as values of specified scatter/gather entry."
  (when (dma-buffer-expired-p dma-buffer)
    (error "~S has expired" dma-buffer))
  (let ((sg-vec (dma-buffer-scatter/gather-vector dma-buffer)))
    (cond ((dma-buffer-persistent-p dma-buffer)
           (with-snapshot-inhibited ()
             (when (not (eql (dma-buffer-boot-id dma-buffer)
                             (current-boot-id)))
               ;; The SG vec is outdated, update it.
               (update-persistent-sg-vec (dma-buffer-virtual-address dma-buffer)
                                         sg-vec)
               (setf (dma-buffer-persistent-boot-id dma-buffer)
                     (current-boot-id)))
             ;; Entries in persistent sg-vecs are always page-sized.
             (values (svref sg-vec entry-index)
                     #x1000)))
          (t
           (let ((vec-index (* entry-index 2)))
             (values (svref sg-vec vec-index)
                     (svref sg-vec (1+ vec-index))))))))

(defun dma-buffer-sg-entry-list (dma-buffer &key area)
  "Return a freshly-consed list containing the scatter/gather address/length pairs.
Each list entry is a cons representing one SG entry, with the car being
the address and the cdr being the length.
This function allocates. The :AREA argument determines where the list is allocated."
  (let* ((result (cons nil nil))
         (tail result))
    (declare (dynamic-extent result))
    (loop
       for i below (dma-buffer-n-sg-entries dma-buffer)
       do (multiple-value-bind (address length)
              (dma-buffer-sg-entry dma-buffer i)
            (setf tail (setf (cdr tail)
                             (sys.int::cons-in-area
                              (sys.int::cons-in-area address length area)
                              nil
                              area)))))
    (cdr result)))

(declaim (inline map-dma-buffer-pages))
(defun map-dma-buffer-pages (fn dma-buffer)
  (loop
     for index below (dma-buffer-n-sg-entries dma-buffer)
     do
       (multiple-value-bind (page length)
           (dma-buffer-sg-entry dma-buffer index)
         (loop
            for i from page by #x1000
            repeat (truncate length #x1000)
            do (funcall fn i))))
  (values))

(defmacro do-dma-buffer-pages ((physical-page dma-buffer &optional result) &body body)
  `(block nil
     (map-dma-buffer-pages
      (lambda (,physical-page)
        (tagbody ,@body))
      ,dma-buffer)
     ,result))

(defun alloc-persistent-sg-vec (length errorp)
  (declare (ignore errorp))
  (sys.int::make-simple-vector (ceiling length #x1000) :wired))

(defun update-persistent-sg-vec (virtual-address sg-vec)
  (dotimes (i (sys.int::simple-vector-length sg-vec))
    (setf (svref sg-vec i) (get-page-physical-address (+ virtual-address (* i #x1000))))))

(defun map-persistent-sg-vec (virtual-address sg-vec cache-mode)
  (assert (eql cache-mode :write-back))
  (when (not (allocate-memory-range
              virtual-address
              (* (sys.int::simple-vector-length sg-vec)
                 #x1000)
              (logior sys.int::+block-map-present+
                      sys.int::+block-map-writable+
                      sys.int::+block-map-zero-fill+
                      sys.int::+block-map-wired+)))
    (error "Unable to allocate persistent dma buffer"))
  (update-persistent-sg-vec virtual-address sg-vec)
  (values))

(defun alloc-sg-vec (length &key contiguous 32-bit errorp)
  (cond ((or (<= length #x1000)
             contiguous)
         ;; This sg-vec will consist of a single entry, simple.
         (let* ((sg-vec (sys.int::make-simple-vector 2 :wired))
                (frame (allocate-physical-pages (if (eql length 0)
                                                    1
                                                    (ceiling length #x1000))
                                                :32-bit-only 32-bit
                                                :type :transient-dma-buffer)))
           (when (not frame)
             ;; FIXME: What kind of error to signal here?
             ;; TODO: Should this call into the pager to try to convince it
             ;; to free up some memory?
             (when errorp
               (error "Unable to allocate dma buffer of length ~D" length))
             (return-from alloc-sg-vec nil))
           (setf (svref sg-vec 0) (ash frame 12)
                 (svref sg-vec 1) length)
           sg-vec))
        (t
         ;; Non-contiguous allocations are a little bit tricky, as
         ;; it isn't known up-front exactly how many sg entries are actually
         ;; needed. This tries to minimize the number of entries needed.
         ;; This is written this way to minimize the amount of consing in the
         ;; wired area.
         (let ((frame-stack -1)
               (n-entries 0)
               (n-frames-remaining (ceiling length #x1000)))
           (unwind-protect
                (loop
                   (when (zerop n-frames-remaining)
                     ;; Physical memory allocation complete.
                     ;; Allocate & populate sg-vec.
                     ;; Allocation here can fail, which is why it is in the UNWIND-PROTECT.
                     (let ((sg-vec (sys.int::make-simple-vector (* n-entries 2) :wired)))
                       ;; Populating sv-vec will pop all entries from the frame stack
                       ;; and stop the UNWIND-PROTECT from freeing them on return.
                       (dotimes (i n-entries)
                         (let ((next (physical-memref-unsigned-byte-64 frame-stack 0))
                               (length (physical-memref-unsigned-byte-64 frame-stack 1)))
                           (setf (svref sg-vec (* i 2)) frame-stack
                                 (svref sg-vec (1+ (* i 2))) length)
                           (decf n-entries)
                           (setf frame-stack next)))
                       (return sg-vec)))
                   (let ((attempt n-frames-remaining)
                         (frames nil))
                     (loop
                        (setf frames (allocate-physical-pages attempt
                                                              :32-bit-only 32-bit
                                                              :type :transient-dma-buffer))
                        (when frames
                          (return))
                        (when (eql attempt 1)
                          ;; Can't get any smaller than this.
                          (when errorp
                            (error "Unable to allocate dma buffer of length ~D" length))
                          (return-from alloc-sg-vec nil))
                        (setf attempt (ceiling attempt 2)))
                     (decf n-frames-remaining attempt)
                     (incf n-entries)
                     (let ((paddr (* frames #x1000)))
                       (setf (physical-memref-unsigned-byte-64 paddr 0) frame-stack
                             (physical-memref-unsigned-byte-64 paddr 1) (* attempt #x1000))
                       (setf frame-stack paddr))))
             ;; Free the frames in the event that the sv-vec allocation fails (due to
             ;; lack of memory) but execution continues.
             (loop
                (when (eql n-entries 0)
                  (return))
                (let ((next (physical-memref-unsigned-byte-64 frame-stack 0))
                      (length (physical-memref-unsigned-byte-64 frame-stack 1)))
                  (release-physical-pages (truncate frame-stack #x1000)
                                          (truncate length #x1000))
                  (setf frame-stack next)
                  (decf n-entries))))))))
