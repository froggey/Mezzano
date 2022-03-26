;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/in-memory.lisp,v 1.31 2008/05/19 07:57:07 edi Exp $

;;; Copyright (c) 2005-2008, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

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

(in-package :flexi-streams)

(defclass in-memory-stream (trivial-gray-stream-mixin)
  ((transformer :initarg :transformer
                :accessor in-memory-stream-transformer
                :documentation "A function used to transform the
written/read octet to the value stored/retrieved in/from the
underlying vector.")   
   #+:cmu
   (open-p :initform t
           :accessor in-memory-stream-open-p
           :documentation "For CMUCL we have to keep track of this
manually."))
  (:documentation "An IN-MEMORY-STREAM is a binary stream that reads
octets from or writes octets to a sequence in RAM."))

(defclass in-memory-input-stream (in-memory-stream fundamental-binary-input-stream)
  ()
  (:documentation "An IN-MEMORY-INPUT-STREAM is a binary stream that
reads octets from a sequence in RAM."))

#+:cmu
(defmethod output-stream-p ((stream in-memory-input-stream))
  "Explicitly states whether this is an output stream."
  (declare (optimize speed))
  nil)

(defclass in-memory-output-stream (in-memory-stream fundamental-binary-output-stream)
  ()
  (:documentation "An IN-MEMORY-OUTPUT-STREAM is a binary stream that
writes octets to a sequence in RAM."))

#+:cmu
(defmethod input-stream-p ((stream in-memory-output-stream))
  "Explicitly states whether this is an input stream."
  (declare (optimize speed))
  nil)

(defclass list-stream ()
  ((list :initarg :list
         :accessor list-stream-list
         :documentation "The underlying list of the stream."))
  (:documentation "A LIST-STREAM is a mixin for IN-MEMORY streams
where the underlying sequence is a list."))

(defclass vector-stream ()
  ((vector :initarg :vector
           :accessor vector-stream-vector
           :documentation "The underlying vector of the stream which
\(for output) must always be adjustable and have a fill pointer."))
  (:documentation "A VECTOR-STREAM is a mixin for IN-MEMORY streams
where the underlying sequence is a vector."))

(defclass list-input-stream (list-stream in-memory-input-stream)
  ()
  (:documentation "A binary input stream that gets its data from an
associated list of octets."))

(defclass vector-input-stream (vector-stream in-memory-input-stream)
  ((index :initarg :index
          :accessor vector-stream-index
          :type (integer 0 #.array-dimension-limit)
          :documentation "An index into the underlying vector denoting
the current position.")
   (end :initarg :end
        :accessor vector-stream-end
        :type (integer 0 #.array-dimension-limit)
        :documentation "An index into the underlying vector denoting
the end of the available data."))
  (:documentation "A binary input stream that gets its data from an
associated vector of octets."))

(defclass vector-output-stream (vector-stream in-memory-output-stream)
  ()
  (:documentation "A binary output stream that writes its data to an
associated vector."))

#+:cmu
(defmethod open-stream-p ((stream in-memory-stream))
  "Returns a true value if STREAM is open.  See ANSI standard."
  (declare #.*standard-optimize-settings*)
  (in-memory-stream-open-p stream))

#+:cmu
(defmethod close ((stream in-memory-stream) &key abort)
  "Closes the stream STREAM.  See ANSI standard."
  (declare #.*standard-optimize-settings*)
  (declare (ignore abort))
  (prog1
      (in-memory-stream-open-p stream)
    (setf (in-memory-stream-open-p stream) nil)))

(defmethod check-if-open ((stream in-memory-stream))
  "Checks if STREAM is open and signals an error otherwise."
  (declare #.*standard-optimize-settings*)
  (unless (open-stream-p stream)
    (error 'in-memory-stream-closed-error
           :stream stream)))

(defmethod stream-element-type ((stream in-memory-stream))
  "The element type is always OCTET by definition."
  (declare #.*standard-optimize-settings*)
  'octet)

(defmethod transform-octet ((stream in-memory-stream) octet)
  "Applies the transformer of STREAM to octet and returns the result."
  (declare #.*standard-optimize-settings*)
  (funcall (or (in-memory-stream-transformer stream)
               #'identity) octet))

(defmethod stream-read-byte ((stream list-input-stream))
  "Reads one byte by simply popping it off of the top of the list."
  (declare #.*standard-optimize-settings*)
  (check-if-open stream)
  (with-accessors ((list list-stream-list))
      stream
    (transform-octet stream (or (pop list) (return-from stream-read-byte :eof)))))

(defmethod stream-listen ((stream list-input-stream))
  "Checks whether list is not empty."
  (declare #.*standard-optimize-settings*)
  (check-if-open stream)
  (with-accessors ((list list-stream-list))
      stream
    list))

(defmethod stream-read-sequence ((stream list-input-stream) sequence start end &key)
  "Repeatedly pops elements from the list until it's empty."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (with-accessors ((list list-stream-list))
      stream
    (loop for index of-type fixnum from start below end
          while list
          do (setf (elt sequence index) (pop list))
          finally (return index))))

(defmethod stream-read-byte ((stream vector-input-stream))
  "Reads one byte and increments INDEX pointer unless we're beyond
END pointer."
  (declare #.*standard-optimize-settings*)
  (check-if-open stream)
  (with-accessors ((index vector-stream-index)
                   (end vector-stream-end)
                   (vector vector-stream-vector))
      stream
    (let ((current-index index))
      (declare (fixnum current-index))
      (cond ((< current-index (the fixnum end))
             (incf (the fixnum index))
             (transform-octet stream (aref vector current-index)))
            (t :eof)))))

(defmethod stream-listen ((stream vector-input-stream))
  "Checking whether INDEX is beyond END."
  (declare #.*standard-optimize-settings*)
  (check-if-open stream)
  (with-accessors ((index vector-stream-index)
                   (end vector-stream-end))
      stream
    (< (the fixnum index) (the fixnum end))))
  
(defmethod stream-read-sequence ((stream vector-input-stream) sequence start end &key)
  "Traverses both sequences in parallel until the end of one of them
is reached."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (loop with vector-end of-type fixnum = (vector-stream-end stream)
        with vector = (vector-stream-vector stream)
        for index of-type fixnum from start below end
        for vector-index of-type fixnum = (vector-stream-index stream)
        while (< vector-index vector-end)
        do (setf (elt sequence index)
                 (aref vector vector-index))
           (incf (the fixnum (vector-stream-index stream)))
        finally (return index)))

(defmethod stream-write-byte ((stream vector-output-stream) byte)
  "Writes a byte \(octet) by extending the underlying vector."
  (declare #.*standard-optimize-settings*)
  (check-if-open stream)
  (with-accessors ((vector vector-stream-vector))
      stream
    (vector-push-extend (transform-octet stream byte) vector)))

(defmethod stream-write-sequence ((stream vector-output-stream) sequence start end &key)
  "Just calls VECTOR-PUSH-EXTEND repeatedly."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (with-accessors ((vector vector-stream-vector))
      stream
    (loop for index of-type fixnum from start below end
          do (vector-push-extend (transform-octet stream (elt sequence index)) vector))
    sequence))

(defmethod stream-file-position ((stream vector-input-stream))
  "Simply returns the index into the underlying vector."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((index vector-stream-index))
      stream
    index))

(defmethod (setf stream-file-position) (position-spec (stream vector-input-stream))
  "Sets the index into the underlying vector if POSITION-SPEC is acceptable."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((index vector-stream-index)
                   (end vector-stream-end))
      stream
    (setq index
          (case position-spec
            (:start 0)
            (:end end)
            (otherwise
             (unless (integerp position-spec)
               (error 'in-memory-stream-position-spec-error
                      :format-control "Unknown file position designator: ~S."
                      :format-arguments (list position-spec)
                      :stream stream
                      :position-spec position-spec))
             (unless (<= 0 position-spec end)
               (error 'in-memory-stream-position-spec-error
                      :format-control "File position designator ~S is out of bounds."
                      :format-arguments (list position-spec)
                      :stream stream
                      :position-spec position-spec))
             position-spec)))
    position-spec))

(defmethod stream-file-position ((stream vector-output-stream))
  "Simply returns the fill pointer of the underlying vector."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((vector vector-stream-vector))
      stream
    (fill-pointer vector)))

(defmethod (setf stream-file-position) (position-spec (stream vector-output-stream))
  "Sets the fill pointer underlying vector if POSITION-SPEC is
acceptable.  Adjusts the vector if necessary."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((vector vector-stream-vector))
      stream
    (let* ((total-size (array-total-size vector))
           (new-fill-pointer
            (case position-spec
              (:start 0)
              (:end
               (warn "File position designator :END doesn't really make sense for an output stream.")
               total-size)
              (otherwise
               (unless (integerp position-spec)
                 (error 'in-memory-stream-position-spec-error
                        :format-control "Unknown file position designator: ~S."
                        :format-arguments (list position-spec)
                        :stream stream
                        :position-spec position-spec))
               (unless (<= 0 position-spec array-total-size-limit)
                 (error 'in-memory-stream-position-spec-error
                        :format-control "File position designator ~S is out of bounds."
                        :format-arguments (list position-spec)
                        :stream stream
                        :position-spec position-spec))
               position-spec))))
      (declare (fixnum total-size new-fill-pointer))
      (when (> new-fill-pointer total-size)
        (adjust-array vector new-fill-pointer))
      (setf (fill-pointer vector) new-fill-pointer)
      position-spec)))

(defmethod make-in-memory-input-stream ((vector vector) &key (start 0)
                                                             (end (length vector))
                                                             transformer)
  "Returns a binary input stream which will supply, in order, the
octets in the subsequence of VECTOR bounded by START and END.
Each octet returned will be transformed in turn by the optional
TRANSFORMER function."
  (declare #.*standard-optimize-settings*)
  (make-instance 'vector-input-stream
                 :vector vector
                 :index start
                 :end end
                 :transformer transformer))

(defmethod make-in-memory-input-stream ((list list) &key (start 0)
                                                         (end (length list))
                                                         transformer)
  "Returns a binary input stream which will supply, in order, the
octets in the subsequence of LIST bounded by START and END.  Each
octet returned will be transformed in turn by the optional
TRANSFORMER function."
  (declare #.*standard-optimize-settings*)
  (make-instance 'list-input-stream
                 :list (subseq list start end)
                 :transformer transformer))

(defun make-output-vector (&key (element-type 'octet))
  "Creates and returns an array which can be used as the underlying
vector for a VECTOR-OUTPUT-STREAM."
  (declare #.*standard-optimize-settings*)
  (make-array 0 :adjustable t
                :fill-pointer 0
                :element-type element-type))

(defun make-in-memory-output-stream (&key (element-type 'octet) transformer)
  "Returns a binary output stream which accepts objects of type
ELEMENT-TYPE \(a subtype of OCTET) and makes available a sequence
that contains the octes that were actually output.  The octets
stored will each be transformed by the optional TRANSFORMER
function."
  (declare #.*standard-optimize-settings*)
  (make-instance 'vector-output-stream
                 :vector (make-output-vector :element-type element-type)
                 :transformer transformer))

(defmethod get-output-stream-sequence ((stream in-memory-output-stream) &key as-list)
  "Returns a vector containing, in order, all the octets that have
been output to the IN-MEMORY stream STREAM. This operation clears any
octets on STREAM, so the vector contains only those octets which have
been output since the last call to GET-OUTPUT-STREAM-SEQUENCE or since
the creation of the stream, whichever occurred most recently.  If
AS-LIST is true the return value is coerced to a list."
  (declare #.*standard-optimize-settings*)
  (with-accessors ((vector vector-stream-vector))
      stream
    (prog1
        (if as-list
          (coerce vector 'list)
          vector)
      (setq vector
            (make-output-vector)))))

(defmethod output-stream-sequence-length ((stream in-memory-output-stream))
  "Returns the current length of the underlying vector of the
IN-MEMORY output stream STREAM."
  (declare (optimize speed))
  (length (the vector (vector-stream-vector stream))))

(defmacro with-input-from-sequence ((var sequence &key start end transformer) 
                                    &body body)
  "Creates an IN-MEMORY input stream from SEQUENCE using the
parameters START and END, binds VAR to this stream and then
executes the code in BODY.  A function TRANSFORMER may optionally
be specified to transform the returned octets.  The stream is
automatically closed on exit from WITH-INPUT-FROM-SEQUENCE, no
matter whether the exit is normal or abnormal.  The return value
of this macro is the return value of BODY."
  (with-rebinding (sequence)
    `(let (,var)
       (unwind-protect
           (progn
             (setq ,var (make-in-memory-input-stream ,sequence
                                                     :start (or ,start 0)
                                                     :end (or ,end (length ,sequence))
                                                     :transformer ,transformer))
             ,@body)
         (when ,var (close ,var))))))

(defmacro with-output-to-sequence ((var &key as-list (element-type ''octet) transformer)
                                   &body body)
  "Creates an IN-MEMORY output stream, binds VAR to this stream
and then executes the code in BODY.  The stream stores data of
type ELEMENT-TYPE \(a subtype of OCTET) which is \(optionally)
transformed by the function TRANSFORMER prior to storage.  The
stream is automatically closed on exit from
WITH-OUTPUT-TO-SEQUENCE, no matter whether the exit is normal or
abnormal.  The return value of this macro is a vector \(or a list
if AS-LIST is true) containing the octets that were sent to the
stream within BODY."
  `(let (,var)
     (unwind-protect
         (progn
           (setq ,var (make-in-memory-output-stream :element-type ,element-type
                                                    :transformer ,transformer))
           ,@body
           (get-output-stream-sequence ,var :as-list ,as-list))
       (when ,var (close ,var)))))
