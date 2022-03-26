;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; streams.lisp --- Conversions between strings and UB8 vectors.
;;;
;;; Copyright (c) 2005-2007, Dr. Edmund Weitz. All rights reserved.
;;; Copyright (c) 2008, Attila Lendvai. All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; STATUS
;;;
;;; - in-memory output streams support binary/bivalent/character
;;;   element-types and file-position

;;; TODO
;;;
;;; - filter-stream types/mixins that can wrap a binary stream and
;;;   turn it into a bivalent/character stream
;;; - in-memory input streams with file-position similar to in-memory
;;;   output streams
;;; - in-memory input/output streams?

(in-package #:babel)

(defpackage #:babel-streams
  (:use #:common-lisp #:babel #:trivial-gray-streams #:alexandria)
  (:export
   #:in-memory-stream
   #:vector-output-stream
   #:vector-input-stream
   #:make-in-memory-output-stream
   #:make-in-memory-input-stream
   #:get-output-stream-sequence
   #:with-output-to-sequence
   #:with-input-from-sequence))

(in-package :babel-streams)

(declaim (inline check-if-open check-if-accepts-octets
                 check-if-accepts-characters stream-accepts-characters?
                 stream-accepts-octets? vector-extend
                 extend-vector-output-stream-buffer))

(defgeneric get-output-stream-sequence (stream &key &allow-other-keys))

;;;; Some utilities (on top due to inlining)

(defun vector-extend (extension vector &key (start 0) (end (length extension)))
  ;; copied over from cl-quasi-quote
  (declare (optimize speed)
           (type vector extension vector)
           (type array-index start end))
  (let* ((original-length (length vector))
         (extension-length (- end start))
         (new-length (+ original-length extension-length))
         (original-dimension (array-dimension vector 0)))
    (when (< original-dimension new-length)
      (setf vector
            (adjust-array vector (max (* 2 original-dimension) new-length))))
    (setf (fill-pointer vector) new-length)
    (replace vector extension :start1 original-length :start2 start :end2 end)
    vector))

(defclass in-memory-stream (trivial-gray-stream-mixin)
  ((element-type                  ; :default means bivalent
    :initform :default :initarg :element-type :accessor element-type-of)
   (external-format
    :initform (ensure-external-format *default-character-encoding*)
    :initarg :external-format :accessor external-format-of)
   #+cmu
   (open-p
    :initform t :accessor in-memory-stream-open-p
    :documentation "For CMUCL we have to keep track of this manually."))
  (:documentation "An IN-MEMORY-STREAM is a binary stream that reads octets
                   from or writes octets to a sequence in RAM."))

(defmethod stream-element-type ((self in-memory-stream))
  ;; stream-element-type is a CL symbol, we may not install an accessor on it.
  ;; so, go through this extra step.
  (element-type-of self))

(defun stream-accepts-octets? (stream)
  (let ((element-type (element-type-of stream)))
    (or (eq element-type :default)
        (equal element-type '(unsigned-byte 8))
        (subtypep element-type '(unsigned-byte 8)))))

(defun stream-accepts-characters? (stream)
  (let ((element-type (element-type-of stream)))
    (member element-type '(:default character base-char))))

(defclass in-memory-input-stream (in-memory-stream fundamental-binary-input-stream)
  ()
  (:documentation "An IN-MEMORY-INPUT-STREAM is a binary stream that reads
                   octets from a sequence in RAM."))

#+cmu
(defmethod output-stream-p ((stream in-memory-input-stream))
  "Explicitly states whether this is an output stream."
  (declare (optimize speed))
  nil)

(defclass in-memory-output-stream (in-memory-stream
                                   fundamental-binary-output-stream)
  ()
  (:documentation "An IN-MEMORY-OUTPUT-STREAM is a binary stream that
                   writes octets to a sequence in RAM."))

#+cmu
(defmethod input-stream-p ((stream in-memory-output-stream))
  "Explicitly states whether this is an input stream."
  (declare (optimize speed))
  nil)

(defun make-in-memory-output-stream (&key (element-type :default)
                                     external-format
                                     initial-buffer-size)
  "Returns a binary output stream which accepts objects of type
ELEMENT-TYPE \(a subtype of OCTET) and makes available a sequence that
contains the octes that were actually output."
  (declare (optimize speed))
  (unless external-format
    (setf external-format *default-character-encoding*))
  (when (eq element-type :bivalent)
    (setf element-type :default))
  (make-instance 'vector-output-stream
                 :vector (make-vector-stream-buffer
                          :element-type
                          (cond
                            ((or (eq element-type :default)
                                 (equal element-type '(unsigned-byte 8)))
                             '(unsigned-byte 8))
                            ((eq element-type 'character)
                             'character)
                            ((subtypep element-type '(unsigned-byte 8))
                             '(unsigned-byte 8))
                            (t (error "Illegal element-type ~S" element-type)))
                          :initial-size initial-buffer-size)
                 :element-type element-type
                 :external-format (ensure-external-format external-format)))

(defun make-in-memory-input-stream (data &key (element-type :default)
                                    external-format)
  "Returns a binary input stream which provides the elements of DATA when read."
  (declare (optimize speed))
  (unless external-format
    (setf external-format *default-character-encoding*))
  (when (eq element-type :bivalent)
    (setf element-type :default))
  (make-instance 'vector-input-stream
                 :vector data
                 :element-type element-type
                 :end (length data)
                 :external-format (ensure-external-format external-format)))

(defclass vector-stream ()
  ((vector
    :initarg :vector :accessor vector-stream-vector
    :documentation "The underlying vector of the stream which \(for output)
                    must always be adjustable and have a fill pointer.")
   (index
    :initform 0 :initarg :index :accessor vector-stream-index
    :type (integer 0 #.array-dimension-limit)
    :documentation "An index into the underlying vector denoting the
                    current position."))
  (:documentation
   "A VECTOR-STREAM is a mixin for IN-MEMORY streams where the underlying
    sequence is a vector."))

(defclass vector-input-stream (vector-stream in-memory-input-stream)
  ((end
    :initarg :end :accessor vector-stream-end
    :type (integer 0 #.array-dimension-limit)
    :documentation "An index into the underlying vector denoting the end
                    of the available data."))
  (:documentation "A binary input stream that gets its data from an
                   associated vector of octets."))

(defclass vector-output-stream (vector-stream in-memory-output-stream)
  ()
  (:documentation
   "A binary output stream that writes its data to an associated vector."))

(define-condition in-memory-stream-error (stream-error)
  ()
  (:documentation "Superclass for all errors related to IN-MEMORY streams."))

(define-condition in-memory-stream-closed-error (in-memory-stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~S is closed."
                     (stream-error-stream condition))))
  (:documentation "An error that is signalled when someone is trying to read
                   from or write to a closed IN-MEMORY stream."))

(define-condition wrong-element-type-stream-error (stream-error)
  ((expected-type :accessor expected-type-of :initarg :expected-type))
  (:report (lambda (condition output)
             (let ((stream (stream-error-stream condition)))
               (format output "The element-type of ~S is ~S while expecting ~
                               a stream that accepts ~S."
                       stream (element-type-of stream)
                       (expected-type-of condition))))))

(defun wrong-element-type-stream-error (stream expected-type)
  (error 'wrong-element-type-stream-error
         :stream stream :expected-type expected-type))

#+cmu
(defmethod open-stream-p ((stream in-memory-stream))
  "Returns a true value if STREAM is open.  See ANSI standard."
  (declare (optimize speed))
  (in-memory-stream-open-p stream))

#+cmu
(defmethod close ((stream in-memory-stream) &key abort)
  "Closes the stream STREAM.  See ANSI standard."
  (declare (ignore abort) (optimize speed))
  (prog1
      (in-memory-stream-open-p stream)
    (setf (in-memory-stream-open-p stream) nil)))

(defun check-if-open (stream)
  "Checks if STREAM is open and signals an error otherwise."
  (declare (optimize speed))
  (unless (open-stream-p stream)
    (error 'in-memory-stream-closed-error :stream stream)))

(defun check-if-accepts-octets (stream)
  (declare (optimize speed))
  (unless (stream-accepts-octets? stream)
    (wrong-element-type-stream-error stream '(unsigned-byte 8))))

(defun check-if-accepts-characters (stream)
  (declare (optimize speed))
  (unless (stream-accepts-characters? stream)
    (wrong-element-type-stream-error stream 'character)))

(defmethod stream-read-byte ((stream vector-input-stream))
  "Reads one byte and increments INDEX pointer unless we're beyond END pointer."
  (declare (optimize speed))
  (check-if-open stream)
  (let ((index (vector-stream-index stream)))
    (cond ((< index (vector-stream-end stream))
           (incf (vector-stream-index stream))
           (aref (vector-stream-vector stream) index))
          (t :eof))))

#+#:ignore
(defmethod stream-read-char ((stream vector-input-stream))
  ;; TODO
  )

(defmethod stream-listen ((stream vector-input-stream))
  "Checking whether INDEX is beyond END."
  (declare (optimize speed))
  (check-if-open stream)
  (< (vector-stream-index stream) (vector-stream-end stream)))

(defmethod stream-read-sequence ((stream vector-input-stream)
                                 sequence start end &key)
  (declare (optimize speed) (type array-index start end))
  ;; TODO check the sequence type, assert for the element-type and use
  ;; the external-format.
  (loop with vector-end of-type array-index = (vector-stream-end stream)
        with vector = (vector-stream-vector stream)
        for index from start below end
        for vector-index of-type array-index = (vector-stream-index stream)
        while (< vector-index vector-end)
        do (setf (elt sequence index)
                 (aref vector vector-index))
           (incf (vector-stream-index stream))
        finally (return index)))

(defmethod stream-write-byte ((stream vector-output-stream) byte)
  "Writes a byte \(octet) by extending the underlying vector."
  (declare (optimize speed))
  (check-if-open stream)
  (check-if-accepts-octets stream)
  (vector-push-extend byte (vector-stream-vector stream))
  (incf (vector-stream-index stream))
  byte)

(defun extend-vector-output-stream-buffer (extension stream &key (start 0)
                                           (end (length extension)))
  (declare (optimize speed)
           (type array-index start end)
           (type vector extension))
  (vector-extend extension (vector-stream-vector stream) :start start :end end)
  (incf (vector-stream-index stream) (- end start))
  (values))

(defmethod stream-write-char ((stream vector-output-stream) char)
  (declare (optimize speed))
  (check-if-open stream)
  (check-if-accepts-characters stream)
  ;; TODO this is naiive here, there's room for optimization
  (let ((octets (string-to-octets (string char)
                                  :encoding (external-format-of stream))))
    (extend-vector-output-stream-buffer octets stream))
  char)

(defmethod stream-write-sequence ((stream vector-output-stream)
                                  sequence start end &key)
  "Just calls VECTOR-PUSH-EXTEND repeatedly."
  (declare (optimize speed)
           (type array-index start end))
  (etypecase sequence
    (string
     (if (stream-accepts-octets? stream)
         ;; TODO this is naiive here, there's room for optimization
         (let ((octets (string-to-octets sequence
                                         :encoding (external-format-of stream)
                                         :start start
                                         :end end)))
           (extend-vector-output-stream-buffer octets stream))
         (progn
           (assert (stream-accepts-characters? stream))
           (extend-vector-output-stream-buffer sequence stream
                                               :start start :end end))))
    ((vector (unsigned-byte 8))
     ;; specialized branch to help inlining
     (check-if-accepts-octets stream)
     (extend-vector-output-stream-buffer sequence stream :start start :end end))
    (vector
     (check-if-accepts-octets stream)
     (extend-vector-output-stream-buffer sequence stream :start start :end end)))
  sequence)

(defmethod stream-write-string ((stream vector-output-stream)
                                string &optional (start 0) (end (length string)))
  (stream-write-sequence stream string start (or end (length string))))

(defmethod stream-line-column ((stream vector-output-stream))
  "Dummy line-column method that always returns NIL. Needed for
character output streams."
  nil)

(defmethod stream-file-position ((stream vector-stream))
  "Simply returns the index into the underlying vector."
  (declare (optimize speed))
  (vector-stream-index stream))

(defun make-vector-stream-buffer (&key (element-type '(unsigned-byte 8))
                                  initial-size)
  "Creates and returns an array which can be used as the underlying vector
   for a VECTOR-OUTPUT-STREAM."
  (declare (optimize speed)
           (type (or null array-index) initial-size))
  (make-array (the array-index (or initial-size 32))
              :adjustable t
              :fill-pointer 0
              :element-type element-type))

(defmethod get-output-stream-sequence ((stream in-memory-output-stream) &key (return-as 'vector))
  "Returns a vector containing, in order, all the octets that have
been output to the IN-MEMORY stream STREAM. This operation clears any
octets on STREAM, so the vector contains only those octets which have
been output since the last call to GET-OUTPUT-STREAM-SEQUENCE or since
the creation of the stream, whichever occurred most recently. If
AS-LIST is true the return value is coerced to a list."
  (declare (optimize speed))
  (prog1
      (ecase return-as
        (vector (vector-stream-vector stream))
        (string (octets-to-string (vector-stream-vector stream)
                                  :encoding (external-format-of stream)))
        (list (coerce (vector-stream-vector stream) 'list)))
    (setf (vector-stream-vector stream) (make-vector-stream-buffer))))

(defmacro with-output-to-sequence
    ((var &key (return-as ''vector) (element-type '':default)
          (external-format '*default-character-encoding*) initial-buffer-size)
     &body body)
  "Creates an IN-MEMORY output stream, binds VAR to this stream and
then executes the code in BODY. The stream stores data of type
ELEMENT-TYPE \(a subtype of OCTET). The stream is automatically closed
on exit from WITH-OUTPUT-TO-SEQUENCE, no matter whether the exit is
normal or abnormal. The return value of this macro is a vector \(or a
list if AS-LIST is true) containing the octets that were sent to the
stream within BODY."
  (multiple-value-bind (body declarations) (parse-body body)
    ;; this is here to stop SBCL complaining about binding them to NIL
    `(let ((,var (make-in-memory-output-stream
                  :element-type ,element-type
                  :external-format ,external-format
                  :initial-buffer-size ,initial-buffer-size)))
       ,@declarations
       (unwind-protect
            (progn
              ,@body
              (get-output-stream-sequence ,var :return-as ,return-as))
         (close ,var)))))

(defmacro with-input-from-sequence
    ((var data &key (element-type '':default)
          (external-format '*default-character-encoding*))
     &body body)
  "Creates an IN-MEMORY input stream that will return the values
available in DATA, binds VAR to this stream and then executes the code
in BODY. The stream stores data of type ELEMENT-TYPE \(a subtype of
OCTET). The stream is automatically closed on exit from
WITH-INPUT-FROM-SEQUENCE, no matter whether the exit is normal or
abnormal. The return value of this macro is the return value of BODY."
  (multiple-value-bind (body declarations) (parse-body body)
    ;; this is here to stop SBCL complaining about binding them to NIL
    `(let ((,var (make-in-memory-input-stream
                  ,data :element-type ,element-type
                  :external-format ,external-format)))
       ,@declarations
       (unwind-protect
            (progn
              ,@body)
         (close ,var)))))
