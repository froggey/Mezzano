;;;
;;; A text progress bar
;;;

(in-package #:ql-progress)

(defclass progress-bar ()
  ((start-time
    :initarg :start-time
    :accessor start-time)
   (end-time
    :initarg :end-time
    :accessor end-time)
   (progress-character
    :initarg :progress-character
    :accessor progress-character)
   (character-count
    :initarg :character-count
    :accessor character-count
    :documentation "How many characters wide is the progress bar?")
   (characters-so-far
    :initarg :characters-so-far
    :accessor characters-so-far)
   (update-interval
    :initarg :update-interval
    :accessor update-interval
    :documentation "Update the progress bar display after this many
    internal-time units.")
   (last-update-time
    :initarg :last-update-time
    :accessor last-update-time
    :documentation "The display was last updated at this time.")
   (total
    :initarg :total
    :accessor total
    :documentation "The total number of units tracked by this progress bar.")
   (progress
    :initarg :progress
    :accessor progress
    :documentation "How far in the progress are we?")
   (pending
    :initarg :pending
    :accessor pending
    :documentation "How many raw units should be tracked in the next
    display update?"))
  (:default-initargs
   :progress-character #\=
   :character-count 50
   :characters-so-far 0
   :update-interval (floor internal-time-units-per-second 4)
   :last-update-time 0
   :total 0
   :progress 0
   :pending 0))

(defgeneric start-display (progress-bar))
(defgeneric update-progress (progress-bar unit-count))
(defgeneric update-display (progress-bar))
(defgeneric finish-display (progress-bar))
(defgeneric elapsed-time (progress-bar))
(defgeneric units-per-second (progress-bar))

(defmethod start-display (progress-bar)
  (setf (last-update-time progress-bar) (get-internal-real-time))
  (setf (start-time progress-bar) (get-internal-real-time))
  (fresh-line)
  (finish-output))

(defmethod update-display (progress-bar)
  (incf (progress progress-bar) (pending progress-bar))
  (setf (pending progress-bar) 0)
  (setf (last-update-time progress-bar) (get-internal-real-time))
  (let* ((showable (floor (character-count progress-bar)
                          (/ (total progress-bar) (progress progress-bar))))
         (needed (- showable (characters-so-far progress-bar))))
    (setf (characters-so-far progress-bar) showable)
    (dotimes (i needed)
      (write-char (progress-character progress-bar)))
    (finish-output)))

(defmethod update-progress (progress-bar unit-count)
  (incf (pending progress-bar) unit-count)
  (let ((now (get-internal-real-time)))
    (when (< (update-interval progress-bar)
             (- now (last-update-time progress-bar)))
      (update-display progress-bar))))

(defmethod finish-display (progress-bar)
  (update-display progress-bar)
  (setf (end-time progress-bar) (get-internal-real-time))
  (terpri)
  (format t "~:D bytes in ~$ seconds (~$KB/sec)~%"
          (total progress-bar)
          (elapsed-time progress-bar)
          (/  (units-per-second progress-bar) 1024))
  (finish-output))

(defmethod elapsed-time (progress-bar)
  (/ (- (end-time progress-bar) (start-time progress-bar))
     internal-time-units-per-second))

(defmethod units-per-second (progress-bar)
  (if (plusp (elapsed-time progress-bar))
      (/ (total progress-bar) (elapsed-time progress-bar))
      0))

(defun kb/sec (progress-bar)
  (/ (units-per-second progress-bar) 1024))



(defparameter *uncertain-progress-chars* "?")

(defclass uncertain-size-progress-bar (progress-bar)
  ((progress-char-index
    :initarg :progress-char-index
    :accessor progress-char-index)
   (units-per-char
    :initarg :units-per-char
    :accessor units-per-char))
  (:default-initargs
   :total 0
   :progress-char-index 0
   :units-per-char (floor (expt 1024 2) 50)))

(defmethod update-progress :after ((progress-bar uncertain-size-progress-bar)
                            unit-count)
  (incf (total progress-bar) unit-count))

(defmethod progress-character ((progress-bar uncertain-size-progress-bar))
  (let ((index (progress-char-index progress-bar)))
    (prog1
        (char *uncertain-progress-chars* index)
      (setf (progress-char-index progress-bar)
            (mod (1+ index) (length *uncertain-progress-chars*))))))

(defmethod update-display ((progress-bar uncertain-size-progress-bar))
  (setf (last-update-time progress-bar) (get-internal-real-time))
  (multiple-value-bind (chars pend)
      (floor (pending progress-bar) (units-per-char progress-bar))
    (setf (pending progress-bar) pend)
    (dotimes (i chars)
      (write-char (progress-character progress-bar))
      (incf (characters-so-far progress-bar))
      (when (<= (character-count progress-bar)
                (characters-so-far progress-bar))
        (terpri)
        (setf (characters-so-far progress-bar) 0)
        (finish-output)))
    (finish-output)))

(defun make-progress-bar (total)
  (if (or (not total) (zerop total))
      (make-instance 'uncertain-size-progress-bar)
      (make-instance 'progress-bar :total total)))

