(in-package :clim-user)

(defclass sample ()
  ((data :initarg :data
         :accessor data
         :type list
         :initform '()))
  (:documentation "A statistical sample"))

(defgeneric sample-size (sample)
  (:documentation "Return the size of a statistical sample"))

(defmethod sample-size ((sample sample))
  (length (data sample)))

(defmethod print-object ((object sample) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "n=~D" (sample-size object))))

(defparameter *my-sample*
  (make-instance 'sample
                 :data (map-into (make-list 1000) (lambda () (random 100.f0)))))

(defgeneric sum (sample)
  (:documentation "The sum of all numbers in a statistical
sample"))

(defmethod sum ((sample sample))
  (reduce #'+ (data sample)))

(defgeneric mean (sample)
  (:documentation "The mean of the numbers in a statistical
sample"))

(defmethod mean ((sample sample))
  (/ (sum sample)
     (sample-size sample)))

(defgeneric standard-deviation (sample)
  (:documentation "Find the standard deviation of the numbers
in a sample. This measures how spread out they are."))

(defmethod standard-deviation ((sample sample))
  (let ((mean (mean sample)))
    (sqrt (/ (loop for x in (data sample)
                   sum (expt (- x mean) 2))
             (1- (sample-size sample))))))

;;;; 1

(defmethod clouseau:inspect-object-using-state ((object sample)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-header))
                                                (stream t))
  (format stream "SAMPLE n=~D" (sample-size object)))

(defmethod clouseau:inspect-object-using-state ((object sample)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream t))
  (clouseau:formatting-place (object 'clouseau:reader-place 'mean
                                     present-place present-object)
    (write-string "mean" stream) ; label
    (present-place stream)       ; place indicator for the "slot"
    (present-object stream))     ; the value of the "slot" is the object
  (fresh-line stream)
  (clouseau:formatting-place (object 'clouseau:reader-place 'standard-deviation
                                     present-place present-object)
    (write-string "std. dev." stream)
    (present-place stream)
    (present-object stream)))

;;;; 2

(defmethod clouseau:inspect-object-using-state ((object sample)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream t))
  (formatting-table (stream)
    (clouseau:format-place-row stream object 'clouseau:reader-place 'mean
                                :label "mean")
    (clouseau:format-place-row stream object 'clouseau:reader-place 'standard-deviation
                                :label "std. dev.")))

;;;; 3

(defun xbar (stream)
  "Draw an x with a bar over it"
  (with-room-for-graphics (stream)
    (with-text-face (stream :italic)
      (princ #\x stream)
      (draw-line* stream 0 0 (text-size stream #\x) 0))))

(defmethod clouseau:inspect-object-using-state ((object sample)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream t))
  (formatting-table (stream)
    (clouseau:format-place-row stream object 'clouseau:reader-place 'mean
                                :label #'xbar)
    (clouseau:format-place-row stream object 'clouseau:reader-place 'standard-deviation
                                :label #\S)))

;;;; 4

(defmethod clouseau:inspect-object-using-state ((object sample)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-header))
                                                (stream t))
  (clouseau::inspect-class-as-name (class-of object) stream)
  (write-char #\Space stream)
  (with-drawing-options (stream :text-family :serif :text-face :italic)
    (write-char #\n stream))
  (format stream "=~D" (sample-size object)))

;;;; 5

(defmethod clouseau:inspect-object-using-state ((object sample)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream t))
  (formatting-table (stream)
    (clouseau:format-place-row stream object 'clouseau:reader-place 'mean
                               :label #'xbar)
    (clouseau:format-place-row stream object 'clouseau:reader-place 'standard-deviation
                               :label #\S :label-style '(:text-face :italic)))
  (fresh-line stream)
  (clouseau:inspect-object-using-state object state :histogram stream))

(defmethod clouseau:inspect-object-using-state ((object sample)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :histogram))
                                                (stream t))
  (let* ((data (data object))
         (buckets (make-array 10 :initial-element 0))
         (max (reduce #'max data)))
    (loop for x in data
          do (incf (aref buckets (floor x (/ (1+ max) (length buckets))))))
    (with-room-for-graphics (stream)
      (loop for i below (length buckets)
            for x = (* 20 i)
            for y = (* 1 (aref buckets i))
            do (draw-rectangle* stream x 0 (+ x 19) y
                                :ink (make-contrasting-inks 1 0))))))
