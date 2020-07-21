;;;; Low-level support functions for functions.

(in-package :mezzano.internals)

(defun function-tag (function)
  (check-type function function)
  (%object-tag function))

(defun function-pool-size (function)
  (%type-check function +object-tag-function+ 'compiled-function)
  (ldb +function-header-pool-size+ (%object-header-data function)))

(defun function-code-size (function)
  (%type-check function +object-tag-function+ 'compiled-function)
  (* (ldb +function-header-code-size+ (%object-header-data function)) 16))

(defun function-gc-metadata-size (function)
  (%type-check function +object-tag-function+ 'compiled-function)
  (ldb +function-header-metadata-size+ (%object-header-data function)))

(defun function-pool-base (function)
  (%type-check function +object-tag-function+ 'compiled-function)
  (- (truncate (function-code-size function) 8) 1))

(defun function-pool-object (function offset)
  (check-type offset (integer 0))
  (assert (< offset (function-pool-size function)))
  (%object-ref-t function (+ (function-pool-base function) offset)))

(defun function-code-byte (function offset)
  (%type-check function +object-tag-function+ 'compiled-function)
  (check-type offset (integer 0))
  (assert (< offset (function-code-size function)))
  (%object-ref-unsigned-byte-8 function (- offset 8)))

(defun function-gc-metadata-byte (function offset)
  (%type-check function +object-tag-function+ 'compiled-function)
  (check-type offset (integer 0))
  (assert (< offset (function-gc-metadata-size function)))
  (let* ((mc-size (function-code-size function))
         (n-constants (function-pool-size function))
         (gcmd-base (+ -8 mc-size (* n-constants 8))))
    (%object-ref-unsigned-byte-8 function (+ gcmd-base offset))))

(defun function-gc-info (function)
  "Return the address of and the number of bytes in FUNCTION's GC info."
  (%type-check function +object-tag-function+ 'compiled-function)
  (let* ((address (logand (lisp-object-address function) -16))
         (gc-length (ldb +function-header-metadata-size+
                         (%object-header-data function)))
         (mc-size (function-code-size function))
         (n-constants (function-pool-size function)))
    (values (+ address mc-size (* n-constants 8)) ; Address.
            gc-length))) ; Length.

(defun map-function-gc-metadata (function function-to-inspect)
  "Call FUNCTION with every GC metadata entry in FUNCTION-TO-INSPECT.
Arguments to FUNCTION:
 start-offset
 framep
 interruptp
 pushed-values
 pushed-values-register
 layout-address
 layout-length
 multiple-values
 incoming-arguments
 block-or-tagbody-thunk
 extra-registers
 restart"
  (%type-check function-to-inspect +object-tag-function+ 'function)
  (let* ((fn-address (logand (lisp-object-address function-to-inspect) -16))
         (header-data (%object-header-data function-to-inspect))
         (mc-size (* (ldb +function-header-code-size+ header-data) 16))
         (n-constants (ldb +function-header-pool-size+ header-data))
         ;; Address of GC metadata & the length.
         (address (+ fn-address mc-size (* n-constants 8)))
         (length (ldb +function-header-metadata-size+ header-data))
         ;; Position within the metadata.
         (position 0))
    (flet ((consume (&optional (errorp t))
             (when (>= position length)
               (when errorp
                 (mezzano.supervisor:panic "Corrupt GC info in function " function-to-inspect))
               (return-from map-function-gc-metadata))
             (prog1 (memref-unsigned-byte-8 address position)
               (incf position))))
      (declare (dynamic-extent #'consume))
      (loop (let ((start-offset-in-function 0)
                  flags-and-pvr
                  mv-and-ia
                  (pv 0)
                  (n-layout-bits 0)
                  layout-address)
              ;; Read first byte of address, this is where we can terminate.
              (let ((byte (consume nil))
                    (offset 0))
                (setf start-offset-in-function (ldb (byte 7 0) byte)
                      offset 7)
                (when (logtest byte #x80)
                  ;; Read remaining bytes.
                  (loop (let ((byte (consume)))
                          (setf (ldb (byte 7 offset) start-offset-in-function)
                                (ldb (byte 7 0) byte))
                          (incf offset 7)
                          (unless (logtest byte #x80)
                            (return))))))
              ;; Read flag/pvr byte
              (setf flags-and-pvr (consume))
              ;; Read mv-and-ia
              (setf mv-and-ia (consume))
              ;; Read vs32 pv.
              (let ((shift 0))
                (loop
                   (let ((b (consume)))
                     (when (not (logtest b #x80))
                       (setf pv (logior pv (ash (logand b #x3F) shift)))
                       (when (logtest b #x40)
                         (setf pv (- pv)))
                       (return))
                     (setf pv (logior pv (ash (logand b #x7F) shift)))
                     (incf shift 7))))
              ;; Read vu32 n-layout bits.
              (let ((shift 0))
                (loop
                   (let ((b (consume)))
                     (setf n-layout-bits (logior n-layout-bits (ash (logand b #x7F) shift)))
                     (when (not (logtest b #x80))
                       (return))
                     (incf shift 7))))
              (setf layout-address (+ address position))
              ;; Consume layout bits.
              (incf position (ceiling n-layout-bits 8))
              ;; Decode this entry and do something else.
              (funcall function
                       ;; Start offset in the function.
                       start-offset-in-function
                       ;; Frame/no-frame.
                       (logbitp +gcmd-flag0-frame+ flags-and-pvr)
                       ;; Interrupt.
                       (logbitp +gcmd-flag0-interrupt+ flags-and-pvr)
                       ;; Pushed-values.
                       pv
                       ;; Pushed-values-register.
                       (if (logbitp +gcmd-flag0-pushed-values-register+ flags-and-pvr)
                           :rcx
                           nil)
                       ;; Layout-address. Fixnum pointer to virtual memory
                       ;; the inspected function must remain live to keep
                       ;; this valid.
                       layout-address
                       ;; Number of bits in the layout.
                       n-layout-bits
                       ;; Multiple-values.
                       (if (eql (ldb +gcmd-flag1-multiple-values+ mv-and-ia) 15)
                           nil
                           (ldb +gcmd-flag1-multiple-values+ mv-and-ia))
                       ;; Incoming-arguments.
                       (if (logbitp 3 flags-and-pvr)
                           (if (eql (ldb +gcmd-flag1-incoming-arguments-location+ mv-and-ia) 15)
                               :rcx
                               (ldb +gcmd-flag1-incoming-arguments-location+ mv-and-ia))
                           nil)
                       ;; Block-or-tagbody-thunk.
                       (if (logbitp +gcmd-flag0-block-or-tagbody-thunk+ flags-and-pvr)
                           :rax
                           nil)
                       ;; Extra-registers.
                       (case (ldb +gcmd-flag0-extra-registers+ flags-and-pvr)
                         (0 nil)
                         (1 :rax)
                         (2 :rax-rcx)
                         (3 :rax-rcx-rdx))
                       ;; Restart
                       (logbitp +gcmd-flag0-restart+ flags-and-pvr)))))))

(defun decode-function-gc-info (function)
  (let ((result '()))
    (map-function-gc-metadata
     (lambda (offset
              framep interruptp
              pushed-values pushed-values-register
              layout-address layout-length
              multiple-values incoming-arguments
              block-or-tagbody-thunk extra-registers
              restart)
       (let ((layout (make-array 32 :element-type 'bit :adjustable t :fill-pointer 0)))
         ;; Consume layout bits.
         (dotimes (i (ceiling layout-length 8))
           (let ((byte (memref-unsigned-byte-8 layout-address i)))
             (dotimes (j 8)
               (vector-push-extend (ldb (byte 1 j) byte) layout))))
         (setf (fill-pointer layout) layout-length)
         ;; Assemble something that looks like a LAP GC entry.
         (let ((entry '()))
           (unless (zerop layout-length)
             (setf (getf entry :layout) layout))
           (when block-or-tagbody-thunk
             (setf (getf entry :block-or-tagbody-thunk) block-or-tagbody-thunk))
           (when incoming-arguments
             (setf (getf entry :incoming-arguments) incoming-arguments))
           (when multiple-values
             (setf (getf entry :multiple-values) multiple-values))
           (when pushed-values-register
             (setf (getf entry :pushed-values-register) pushed-values-register))
           (when extra-registers
             (setf (getf entry :extra-registers) extra-registers))
           (unless (zerop pushed-values)
             (setf (getf entry :pushed-values) pushed-values))
           (when interruptp
             (setf (getf entry :interrupt) t))
           (when restart
             (setf (getf entry :restart) t))
           (push (list* offset
                        (if framep :frame :no-frame)
                        entry)
                 result))))
     function)
    (reverse result)))

(defun closure-p (object)
  (%object-of-type-p object +object-tag-closure+))

(defun %closure-function (closure)
  (assert (%object-of-type-p closure +object-tag-closure+))
  ;; Return the closed-over function associated with CLOSURE.
  (%object-ref-t closure +closure-function+))

(defun %closure-length (closure)
  (assert (%object-of-type-p closure +object-tag-closure+))
  (- (%object-header-data closure) 2))

(defun %closure-value (closure index)
  (assert (%object-of-type-p closure +object-tag-closure+))
  (assert (<= 0 index))
  (assert (< index (%closure-length closure)))
  (%object-ref-t closure (+ 1 +closure-function+ index)))

(defun (setf %closure-value) (value closure index)
  (assert (%object-of-type-p closure +object-tag-closure+))
  (assert (<= 0 index))
  (assert (< index (%closure-length closure)))
  (setf (%object-ref-t closure (+ 1 +closure-function+ index)) value))

(defun function-name (function)
  (check-type function function)
  (ecase (%object-tag function)
    (#.+object-tag-function+
     ;; Regular function. First entry in the constant pool.
     (function-pool-object function 0))
    (#.+object-tag-closure+
     ;; Closure. Return the name of the closed-over function.
     (function-name (%closure-function function)))
    (#.+object-tag-funcallable-instance+
     (multiple-value-bind (lambda closurep name)
         (funcallable-instance-lambda-expression function)
       (declare (ignore lambda closurep))
       name))
    (#.+object-tag-delimited-continuation+
     nil)))

(defun function-lambda-expression (function)
  (check-type function function)
  (ecase (%object-tag function)
    (#.+object-tag-function+
     (values nil nil (function-name function)))
    (#.+object-tag-closure+
     (values nil t (function-name function)))
    (#.+object-tag-funcallable-instance+
     (funcallable-instance-lambda-expression function))
    (#.+object-tag-delimited-continuation+
     (values nil nil nil))))

(defun function-debug-info (function)
  (check-type function function)
  (ecase (%object-tag function)
    (#.+object-tag-function+
     ;; Regular function. Second entry in the constant pool.
     (function-pool-object function 1))
    (#.+object-tag-closure+
     ;; Closure. Return the debug-info of the closed-over function.
     (function-debug-info (%closure-function function)))
    (#.+object-tag-funcallable-instance+
     (funcallable-instance-debug-info function))
    (#.+object-tag-delimited-continuation+
     nil)))

(defun funcallable-instance-function (funcallable-instance)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'funcallable-instance)
  (%object-ref-t funcallable-instance +funcallable-instance-function+))
(defun (setf funcallable-instance-function) (value funcallable-instance)
  (check-type value function)
  (%type-check funcallable-instance +object-tag-funcallable-instance+ 'funcallable-instance)
  ;; TODO: If the function is an +OBJECT-TAG-FUNCTION+, then the entry point could point directly at it.
  ;; Same as in ALLOCATE-FUNCALLABLE-INSTANCE.
  (setf (%object-ref-t funcallable-instance +funcallable-instance-function+) value))

(defun compiled-function-p (object)
  (when (functionp object)
    (ecase (%object-tag object)
      ((#.+object-tag-function+
        #.+object-tag-closure+)
       t)
      (#.+object-tag-funcallable-instance+
       (funcallable-instance-compiled-function-p object))
      (#.+object-tag-delimited-continuation+
       nil))))

;;; Not sure about the DEFTYPEs for COMPILED-FUNCTION and CLOSURE.
;;; They need to be subtypes of FUNCTION.

(deftype compiled-function ()
  '(satisfies compiled-function-p))

(defun sys.int::closure-p (object)
  (%object-of-type-p object sys.int::+object-tag-closure+))

(deftype sys.int::closure ()
  '(satisfies sys.int::closure-p))
