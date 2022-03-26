;;; -*- Mode: Lisp; Package: DREI-CORE -*-

;;;  (c) copyright 2006-2007 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :drei-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (Gray) streams interface to buffers.

(defclass buffer-stream (fundamental-character-input-stream
                         fundamental-character-output-stream)
  ((%buffer :initarg :buffer
            :initform (error "A buffer must be provided")
            :reader buffer
            :documentation "The buffer from which this stream
will read data.")
   (%start-mark :initarg :start-mark
                :reader start-mark
                :documentation "A mark into the buffer of the
stream that indicates from which point on the stream will read
data from the buffer. By default, the beginning of the
buffer. This mark should not be changed.")
   (%end-mark :initarg :end-mark
              :reader end-mark
              :documentation "A mark into the buffer of the
stream that indicates the buffer position that the stream will
consider end-of-file. By default, the end of the buffer. This
mark should not be changed.")
   (%point :accessor point-of
           :documentation "A mark indicating the current position
in the buffer of the stream."))
  (:documentation "A bidirectional stream that performs I/O on an
underlying Drei buffer. Marks can be provided to let the stream
operate on only a specific section of the buffer."))

(defmethod initialize-instance :after
    ((stream buffer-stream) &key)
  (unless (slot-boundp stream '%start-mark)
    (setf (slot-value stream '%start-mark)
          (clone-mark (point (buffer stream)) :left))
    (beginning-of-buffer (start-mark stream)))
  (unless (slot-boundp stream '%end-mark)
    (setf (slot-value stream '%end-mark)
          (clone-mark (start-mark stream) :right))
    (end-of-buffer (end-mark stream)))
  (setf (point stream)
        (narrow-mark (clone-mark (start-mark stream) :right)
                     (start-mark stream)
                     (end-mark stream))))

;;; Input methods.

(defmethod stream-read-char ((stream buffer-stream))
  (if (end-of-buffer-p (point stream))
      :eof
      (prog1 (object-after (point stream))
        (forward-object (point stream)))))

(defmethod stream-unread-char ((stream buffer-stream) (char character))
  (unless (beginning-of-buffer-p (point stream))
    (backward-object (point stream))
    nil))

(defmethod stream-read-char-no-hang ((stream buffer-stream))
  (stream-read-char stream))

(defmethod stream-peek-char ((stream buffer-stream))
  (if (end-of-buffer-p (point stream))
      :eof
      (object-after (point stream))))

(defmethod stream-listen ((stream buffer-stream))
  (not (end-of-buffer-p (point stream))))

(defmethod stream-read-line ((stream buffer-stream))
  (let ((orig-offset (offset (point stream)))
        (end-of-line-offset (offset (end-of-line (point stream)))))
    (unless (end-of-buffer-p (point stream))
      (forward-object (point stream)))
    (values (buffer-substring (buffer stream)
                              orig-offset
                              end-of-line-offset)
            (end-of-buffer-p (point stream)))))

(defmethod stream-clear-input ((stream buffer-stream))
  nil)

;;; Output methods.

(defmethod stream-write-char ((stream buffer-stream) char)
  (insert-object (point stream) char))

(defmethod stream-line-column ((stream buffer-stream))
  (column-number (point stream)))

(defmethod stream-start-line-p ((stream buffer-stream))
  (or (mark= (point stream)
             (start-mark stream))
      (beginning-of-line-p (point stream))))

(defmethod stream-write-string ((stream buffer-stream) string &optional (start 0) end)
  (insert-sequence (point stream)
                   (subseq string start end)))

(defmethod stream-terpri ((stream buffer-stream))
  (insert-object (point stream) #\Newline))

(defmethod stream-fresh-line ((stream buffer-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)))

(defmethod stream-finish-output ((stream buffer-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-force-output ((stream buffer-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-clear-output ((stream buffer-stream))
  (declare (ignore stream))
  nil)

(defmethod stream-advance-to-column ((stream buffer-stream) (column integer))
  (call-next-method))

(defmethod interactive-stream-p ((stream buffer-stream))
  nil)

;;; Interface functions.

(defun make-buffer-stream (&key (buffer (current-buffer))
                           (start-mark nil start-mark-p)
                           (end-mark nil end-mark-p))
  "Create a buffer stream object reading data from `buffer'. By
default, the stream will read from the beginning of the buffer
and until the end of the buffer, but this can be changed via the
optional arguments `start-mark' and `end-mark'."
  (apply #'make-instance 'buffer-stream
         :buffer buffer
         (append (when start-mark-p
                   (list :start-mark (clone-mark start-mark :left)))
                 (when end-mark-p
                   (list :end-mark (clone-mark end-mark :right))))))
