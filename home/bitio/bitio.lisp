(in-package :bitio)

(defclass bitio ()
  ((%octet-stream :initarg :octet-stream
                  :initform NIL
                  :reader octet-stream)

   ;; This is used to hold buffered data read using fast sequence operators
   ;; from the underlying octet. Octets are always written to aref 0 until
   ;; we reach the end of the required read. Then we read them from aref 0
   ;; forward until we're done processing them all. The constructor function
   ;; is responsible for making this.
   (%octet-read-buffer :initarg :octet-read-buffer
                       :accessor octet-read-buffer)

   ;; read-bit-stable is an unsigned integer that holds unprocessed
   ;; bits always in canonical ordering. However, depending on what
   ;; we're doing we might take bits from the MSB side or the LSB
   ;; side. The read-bit-stable is an octet worth of bits or less. Though
   ;; this may change in the future.
   (%read-bit-stable :initarg :read-bit-stable
                     :initform 0
                     :accessor read-bit-stable)
   ;; How many bits are currently in the stable.
   (%num-bits-in-stable :initarg :num-bits-in-stable
                        :initform 0
                        :accessor num-bits-in-stable)
   ;; The API to the octet-stream
   (%bitio/read-octet :initarg :bitio/read-octet
                      :initform NIL
                      :reader %bitio/read-octet)
   (%bitio/read-sequence :initarg :bitio/read-sequence
                         :initform NIL
                         :reader %bitio/read-sequence)
   (%default-bit-endian :initarg :default-bit-endian
                        :initform :be
                        :reader default-bit-endian)
   (%default-byte-endian :initarg :default-byte-endian
                         :initform :le
                         :reader default-byte-endian)
   (%default-bits-per-byte :initarg :default-bits-per-byte
                           :initform 8
                           :reader default-bits-per-byte)))

(defgeneric bitio/read-octet (bitio &optional eof-error-p eof-value)
  (:documentation "Read an octet from the funciton supplied with the stream
instance the bitio is wrapping."))

(defmethod bitio/read-octet (bitio &optional
                                     (eof-error-p T)
                                     (eof-value NIL))
  (funcall (%bitio/read-octet bitio)
           (octet-stream bitio)
           eof-error-p eof-value))


;; EXPORT
(defun make-bitio (octet-stream bitio/read-octet bitio/read-sequence
                   &key (bit-endian :be) (byte-endian :le) (bits-per-byte 8)
                     (octet-read-buffer-size 4096))
  "OCTET-STREAM must be a stream that is ready to read/write binary
octets of (unsigned-byte 8) type. BITIO/READ-OCTET is a function
associated with the OCTET-STREAM that reads a single octet from that
stream.  BITIO/READ-SEQUENCE is a function associated with the octet
stream that reads a vector of octets. The functions for
BITIO/READ-OCTET and BITIO/READ-SEQUENCE both expect the same oridnary
lambda list as CL's READ-BYTE and READ-SEQUENCE, respectively.
Returns aninstance of a BITIO class."
  (make-instance 'bitio
                 :octet-stream octet-stream
                 :bitio/read-octet bitio/read-octet
                 :bitio/read-sequence bitio/read-sequence
                 :octet-read-buffer (make-array octet-read-buffer-size
                                                :element-type '(unsigned-byte 8)
                                                :initial-element 0)
                 :default-bit-endian bit-endian
                 :default-byte-endian byte-endian
                 :default-bits-per-byte bits-per-byte))
