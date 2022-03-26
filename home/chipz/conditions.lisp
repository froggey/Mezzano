;;;; conditions.lisp -- errors that can be thrown by chipz

(in-package :chipz)

(define-condition chipz-error (simple-error)
  ()
  (:documentation "The base condition of the CHIPZ library.  All
other conditions inherit from this error."))

(define-condition decompression-error (chipz-error)
  ()
  (:documentation "The base condition of all conditions signaled during
decompression."))

(define-condition invalid-format-error (chipz-error)
  ((format :initarg :format :reader invalid-format))
  (:report (lambda (condition stream)
             (format stream "Invalid format ~S"
                     (invalid-format condition))))
  (:documentation "Signaled when an invalid format name is passed to
MAKE-DSTATE, MAKE-INFLATE-STATE, or DECOMPRESS."))

(define-condition invalid-checksum-error (decompression-error)
  ((expected-checksum :initarg :stored :reader expected-checksum)
   (actual-checksum :initarg :computed :reader actual-checksum)
   (kind :initarg :kind :reader checksum-kind))
  (:report (lambda (condition stream)
             (format stream "Invalid ~A checksum, expected ~X, got ~X"
                     (checksum-kind condition)
                     (expected-checksum condition)
                     (actual-checksum condition))))
  (:documentation "Signaled when the checksum of decompressed data does
not match the expected value."))

(define-condition premature-end-of-stream (decompression-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Unexpected EOF")))
  (:documentation "Signaled when FINISH-DSTATE is called on a state that
has not actually reached the end of the input being decompressed."))


;;; Errors related to inflate

(define-condition inflate-error (decompression-error)
  ()
  (:documentation "The base condition of conditions signaled when
decompressing DEFLATE-related formats."))

(define-condition invalid-zlib-header-error (inflate-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid zlib header")))
  (:documentation "Signaled when a zlib header does not pass the
consistency check."))

(define-condition invalid-gzip-header-error (inflate-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid gzip header")))
  (:documentation "Signaled when a gzip header does not have the proper ID."))

(define-condition reserved-block-type-error (inflate-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid deflate block")))
  (:documentation "Signaled when an invalid deflate block is found."))

(define-condition invalid-stored-block-length-error (inflate-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid stored block length")))
  (:documentation "Signaled when a stored block's length does not pass
the consistency check."))


;;; Errors related to bzip2

(define-condition bzip2-error (decompression-error)
  ()
  (:documentation "The base condition of conditions signaled when
decompressing BZIP2-related formats."))

(define-condition invalid-bzip2-data (bzip2-error)
  ()
  (:documentation "Signaled when invalid bzip2 data is found."))
