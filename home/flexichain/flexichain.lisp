;;; Flexichain
;;; Flexichain data structure definition
;;;
;;; Copyright (C) 2003-2004  Robert Strandh (strandh@labri.fr)
;;; Copyright (C) 2003-2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


(cl:in-package :flexichain)

(defclass flexichain ()
  ((element-type :initarg :element-type :initform t)
   (fill-element :initarg :fill-element)
   (expand-factor :initarg :expand-factor :initform 1.5)
   (min-size :initarg :min-size :initform 5))
  (:documentation "The protocol class for flexichains."))

(defmethod initialize-instance :after ((chain flexichain) &rest initargs
                                       &key initial-contents)
  (declare (ignore initargs initial-contents))
  (with-slots (expand-factor min-size) chain
    (assert (> expand-factor 1) ()
            'flexichain-initialization-error
            :cause "EXPAND-FACTOR should be greater than 1.")
    (assert (> min-size 0) ()
            'flexichain-initialization-error
            :cause "MIN-SIZE should be greater than 0."))
  (if (slot-boundp chain 'fill-element)
      (with-slots (element-type fill-element) chain
        (assert (typep fill-element element-type) ()
                'flexichain-initialization-error
                :cause (format nil "FILL-ELEMENT ~A not of type ~S."
                               fill-element element-type)))
      (multiple-value-bind (element foundp)
          (find-if-2 (lambda (x)
                       (typep x (slot-value chain 'element-type)))
                     '(nil 0 #\a))
        (if foundp
            (setf (slot-value chain 'fill-element) element)
            (error 'flexichain-initialization-error
                   :cause
                   "FILL-ELEMENT not provided, no default applicable.")))))

(define-condition flexi-error (simple-error)
  ())

(define-condition flexi-initialization-error (flexi-error)
  ((cause :reader flexi-initialization-error-cause
          :initarg :cause :initform ""))
  (:report (lambda (condition stream)
             (format stream "Error initializing FLEXICHAIN (~S)"
                     (flexi-initialization-error-cause condition)))))

(define-condition flexi-position-error (flexi-error)
  ((chain :reader flexi-position-error-chain
          :initarg :chain :initform nil)
   (position :reader flexi-position-error-position
             :initarg :position :initform nil))
  (:report (lambda (condition stream)
             (format stream "Position ~D out of bounds in ~A"
                     (flexi-position-error-position condition)
                     (flexi-position-error-chain condition)))))

(define-condition flexi-incompatible-type-error (flexi-error)
  ((chain :reader flexi-incompatible-type-error-chain
          :initarg :chain :initform nil)
   (element :reader flexi-incompatible-type-error-element
            :initarg :element :initform nil))
  (:report (lambda (condition stream)
             (let ((element (flexi-incompatible-type-error-element
                             condition)))
               (format stream "Element ~A of type ~A cannot be inserted in ~A"
                       element
                       (type-of element)
                       (flexi-incompatible-type-error-chain condition))))))

(defgeneric nb-elements (chain)
  (:documentation "Returns the number of elements in the flexichain."))

(defgeneric flexi-empty-p (chain)
  (:documentation "Checks whether CHAIN is empty or not."))

(defgeneric insert* (chain position object)
  (:documentation "Inserts an object before the element at POSITION
in the chain. If POSITION is out of range (less than 0 or greater
than the length of CHAIN, the FLEXI-POSITION-ERROR condition will be
signaled."))

(defgeneric insert-vector* (chain position vector)
  (:documentation "Inserts the elements of VECTOR before the
element at POSITION in the chain. If POSITION is out of
range (less than 0 or greater than the length of CHAIN, the
FLEXI-POSITION-ERROR condition will be signaled."))

(defgeneric delete* (chain position)
  (:documentation "Deletes an element at POSITION of the chain.
If POSITION is out of range (less than 0 or greater than or equal
to the length of CHAIN, the FLEXI-POSITION-ERROR condition
will be signaled."))

(defgeneric delete-elements* (chain position n)
  (:documentation "Delete N elements at POSITION of the chain. If
POSITION+N is out of range (less than 0 or greater than or equal
to the length of CHAIN, the FLEXI-POSITION-ERROR condition will
be signaled. N can be negative, in which case elements will be
deleted before POSITION."))

(defgeneric element* (chain position)
  (:documentation "Returns the element at POSITION of the chain.
If POSITION is out of range (less than 0 or greater than or equal
to the length of CHAIN, the FLEXI-POSITION-ERROR condition
will be signaled."))

(defgeneric (setf element*) (object chain position)
  (:documentation "Replaces the element at POSITION of CHAIN by OBJECT.
If POSITION if out of range (less than 0 or greater than or equal to
the length of CHAIN, the FLEXI-POSITION-ERROR condition will be signaled."))

(defgeneric push-start (chain object)
  (:documentation "Inserts an object at the beginning of CHAIN."))

(defgeneric push-end (chain object)
  (:documentation "Inserts an object at the end of CHAIN."))

(defgeneric pop-start (chain)
  (:documentation "Pops and returns the element at the beginning of CHAIN."))

(defgeneric pop-end (chain)
  (:documentation "Pops and returns the element at the end of CHAIN."))

(defgeneric rotate (chain &optional n)
  (:documentation "Rotates the elements of CHAIN so that the element
that used to be at position N is now at position 0. With a negative
value of N, rotates the elements so that the element that used to be
at position 0 is now at position N."))

(defclass standard-flexichain (flexichain)
  ((buffer)
   (gap-start)
   (gap-end)
   (data-start))
  (:documentation "The standard instantiable subclass of FLEXICHAIN."))

(defun required-space (chain nb-elements)
  (with-slots (min-size expand-factor) chain
     (+ 2 (max (ceiling (* nb-elements expand-factor))
               min-size))))

(defmethod initialize-instance :after ((chain standard-flexichain)
                                       &rest initargs
                                       &key
                                       initial-contents
                                       (initial-nb-elements 0)
                                       (initial-element nil))
  (declare (ignore initargs))
  ;; Check initial-contents if provided
  (unless (null initial-contents)
    (with-slots (element-type) chain
      (multiple-value-bind (offending-element foundp)
          (find-if-2 (lambda (x)
                       (not (typep x element-type)))
                     initial-contents)
        (assert (not foundp) ()
                'flexi-initialization-error
                :cause (format nil "Initial element ~A not of type ~S."
                               offending-element element-type)))))
  ;; Initialize slots
  (with-slots (element-type fill-element buffer) chain
     (let* ((data-length (if (> (length initial-contents) initial-nb-elements)
                             (length initial-contents)
                             initial-nb-elements))
            (size (required-space chain data-length))
            (fill-size (- size data-length 2))
            (sentinel-list (make-list 2 :initial-element fill-element))
            (fill-list (make-list fill-size :initial-element fill-element)))
       (setf buffer
             (if initial-contents
                 (make-array size
                             :element-type element-type
                             :initial-contents (concatenate 'list
                                                            sentinel-list
                                                            initial-contents
                                                            fill-list))
                 (let ((arr (make-array size
                                        :element-type element-type
                                        :initial-element initial-element)))
                   (fill arr fill-element :end (length sentinel-list))
                   (fill arr fill-element
                         :start (+ (length sentinel-list) initial-nb-elements)
                         :end size))))
       (with-slots (gap-start gap-end data-start) chain
         (setf gap-start (+ 2 data-length)
               gap-end 0
               data-start 1)))))

(defmacro with-virtual-gap ((bl ds gs ge) chain &body body)
  (let ((c (gensym)))
    `(let* ((,c ,chain)
            (,bl (length (slot-value ,c 'buffer)))
            (,ds (slot-value ,c 'data-start))
            (,gs (slot-value ,c 'gap-start))
            (,ge (slot-value ,c 'gap-end)))
       (declare (ignorable ,bl ,ds ,gs ,ge))
       (when (< ,gs ,ds) (incf ,gs ,bl))
       (when (< ,ge ,ds) (incf ,ge ,bl))
       ,@body)))

(defmethod nb-elements ((chain standard-flexichain))
  (with-virtual-gap (bl ds gs ge) chain
    (- bl (- ge gs) 2)))

(defmethod flexi-empty-p ((chain standard-flexichain))
  (zerop (nb-elements chain)))

(defun position-index (chain position)
  "Returns the (0 indexed) index of the POSITION-th element
of the CHAIN in the buffer."
  (with-virtual-gap (bl ds gs ge) chain
    (let ((index (+ ds position 1)))
      (when (>= index gs)
        (incf index (- ge gs)))
      (when (>= index bl)
        (decf index bl))
      index)))

(defun index-position (chain index)
  "Returns the position corresponding to the INDEX in the CHAIN.
Note: the result is undefined if INDEX is not the index of a valid
element of the CHAIN."
  (with-virtual-gap (bl ds gs ge) chain
    (when (< index ds)
      (incf index bl))
    (when (>= index ge)
      (decf index (- ge gs)))
    (- index ds 1)))

(defun ensure-gap-position (chain position)
  (move-gap chain (position-index chain position)))

(defun ensure-room (chain nb-elements)
  (with-slots (buffer) chain
     (when (> nb-elements (- (length buffer) 2))
       (increase-buffer-size chain nb-elements))))

(defmethod insert* ((chain standard-flexichain) position object)
  (with-slots (element-type buffer gap-start) chain
     (assert (<= 0 position (nb-elements chain)) ()
             'flexi-position-error :chain chain :position position)
     (assert (typep object element-type) ()
             'flexi-incompatible-type-error :element object :chain chain)
     (ensure-gap-position chain position)
     (ensure-room chain (1+ (nb-elements chain)))
     (setf (aref buffer gap-start) object)
     (incf gap-start)
     (when (= gap-start (length buffer))
       (setf gap-start 0))))

(defmethod insert-vector* ((chain standard-flexichain) position vector)
  (with-slots (element-type buffer gap-start) chain
     (assert (<= 0 position (nb-elements chain)) ()
             'flexi-position-error :chain chain :position position)
     (assert (subtypep (array-element-type vector)
                       (upgraded-array-element-type element-type))
             ()
             'flexi-incompatible-type-error :element vector :chain chain)
     (ensure-gap-position chain position)
     (ensure-room chain (+ (nb-elements chain) (length vector)))
     (loop for elem across vector
        do (setf (aref buffer gap-start) elem)
          (incf gap-start)
          (when (= gap-start (length buffer))
            (setf gap-start 0)))))

(defmethod delete* ((chain standard-flexichain) position)
  (with-slots (buffer expand-factor min-size fill-element gap-end) chain
    (assert (< -1 position (nb-elements chain)) ()
            'flexi-position-error :chain chain :position position)
    (ensure-gap-position chain position)
    (setf (aref buffer gap-end) fill-element)
    (incf gap-end)
    (when (= gap-end (length buffer))
      (setf gap-end 0))
    (when (and (> (length buffer) (+ min-size 2))
               (< (+ (nb-elements chain) 2) (/ (length buffer) (square expand-factor))))
      (decrease-buffer-size chain))))

(defmethod delete-elements* ((chain standard-flexichain) position n)
  (unless (zerop n)
    (with-slots (buffer expand-factor min-size gap-end data-start) chain
      (when (minusp n)
        (incf position n)
        (setf n (* -1 n)))
      (assert (<= 0 (+ position n) (nb-elements chain)) ()
              'flexi-position-error :chain chain :position position)
      (ensure-gap-position chain position)
      ;; Two cases to consider - one where position+n is wholly on
      ;; this side of the gap in buffer, and one where part of it is
      ;; "wrapped around" to the beginning of buffer.
      (cond ((>= (length buffer) (+ gap-end n))
             (fill-gap chain gap-end (+ gap-end n))
             (incf gap-end n))
            (t (let ((surplus-elements (- n (- (length buffer) gap-end))))
                 (fill-gap chain gap-end (length buffer))
                 (fill-gap chain 0 surplus-elements)
                 (setf gap-end surplus-elements))))
      (when (= gap-end (length buffer))
        (setf gap-end 0))
      (when (and (> (length buffer) (+ min-size 2))
                 (< (+ (nb-elements chain) 2) (/ (length buffer) (square expand-factor))))
        (decrease-buffer-size chain)))))

(defmethod element* ((chain standard-flexichain) position)
  (with-slots (buffer) chain
     (assert (< -1 position (nb-elements chain)) ()
             'flexi-position-error :chain chain :position position)
     (aref buffer (position-index chain position))))

(defmethod (setf element*) (object (chain standard-flexichain) position)
  (with-slots (buffer element-type) chain
     (assert (< -1 position (nb-elements chain)) ()
             'flexi-position-error :chain chain :position position)
     (assert (typep object element-type) ()
             'flexi-incompatible-type-error :chain chain :element object)
     (setf (aref buffer (position-index chain position)) object)))

(defmethod push-start ((chain standard-flexichain) object)
  (insert* chain 0 object))

(defmethod push-end ((chain standard-flexichain) object)
  (insert* chain (nb-elements chain) object))

(defmethod pop-start ((chain standard-flexichain))
  (prog1
      (element* chain 0)
    (delete* chain 0)))

(defmethod pop-end ((chain standard-flexichain))
  (let ((position (1- (nb-elements chain))))
    (prog1
        (element* chain position)
      (delete* chain position))))

(defmethod rotate ((chain standard-flexichain) &optional (n 1))
  (when (> (nb-elements chain) 1)
    (cond ((plusp n) (loop repeat n do (push-start chain (pop-end chain))))
          ((minusp n) (loop repeat (- n) do (push-end chain (pop-start chain))))
          (t nil))))

(defun move-gap (chain hot-spot)
  "Moves the elements and gap inside the buffer so that
the element currently at HOT-SPOT becomes the first element following
the gap, or does nothing if there are no elements."
  (with-slots (gap-start gap-end) chain
    (unless (= hot-spot gap-end)
      (case (gap-location chain)
        (:gap-empty (move-empty-gap chain hot-spot))
        (:gap-left (move-left-gap chain hot-spot))
        (:gap-right (move-right-gap chain hot-spot))
        (:gap-middle (move-middle-gap chain hot-spot))
        (:gap-non-contiguous (move-non-contiguous-gap chain hot-spot))))
    (values gap-start gap-end)))

(defun move-empty-gap (chain hot-spot)
  "Moves the gap. Handles the case where the gap is empty."
  (with-slots (gap-start gap-end) chain
    (setf gap-start hot-spot
          gap-end hot-spot)))

(defun move-left-gap (chain hot-spot)
  "Moves the gap. Handles the case where the gap is
on the left of the buffer."
  (with-slots (buffer gap-start gap-end data-start) chain
    (let ((buffer-size (length buffer)))
      (cond ((< (- hot-spot gap-end) (- buffer-size hot-spot))
             (push-elements-left chain (- hot-spot gap-end)))
            ((<= (- buffer-size hot-spot) gap-end)
             (hop-elements-left chain (- buffer-size hot-spot)))
            (t
             (hop-elements-left chain (- gap-end gap-start))
             (push-elements-right chain (- gap-start hot-spot)))))))

(defun move-right-gap (chain hot-spot)
  "Moves the gap. Handles the case where the gap is
on the right of the buffer."
  (with-slots (buffer gap-start gap-end) chain
    (let ((buffer-size (length buffer)))
      (cond ((< (- gap-start hot-spot) hot-spot)
             (push-elements-right chain (- gap-start hot-spot)))
            ((<= hot-spot (- buffer-size gap-start))
             (hop-elements-right chain hot-spot))
            (t
             (hop-elements-right chain (- buffer-size gap-start))
             (push-elements-left chain (- hot-spot gap-end)))))))

(defun move-middle-gap (chain hot-spot)
  "Moves the gap. Handles the case where the gap is
in the middle of the buffer."
  (with-slots (buffer gap-start gap-end) chain
    (let ((buffer-size (length buffer)))
      (cond ((< hot-spot gap-start)
             (cond ((<= (- gap-start hot-spot)
                        (+ (- buffer-size gap-end) hot-spot))
                    (push-elements-right chain (- gap-start hot-spot)))
                   (t
                    (push-elements-left chain (- buffer-size gap-end))
                    (move-right-gap chain hot-spot))))
            (t
             (cond ((< (- hot-spot gap-end)
                       (+ (- buffer-size hot-spot) gap-start))
                    (push-elements-left chain (- hot-spot gap-end)))
                   (t
                    (push-elements-right chain gap-start)
                    (move-left-gap chain hot-spot))))))))

(defun move-non-contiguous-gap (chain hot-spot)
  "Moves the gap. Handles the case where the gap is in 2 parts,
on both ends of the buffer."
  (with-slots (buffer gap-start gap-end) chain
    (let ((buffer-size (length buffer)))
      (cond ((< (- hot-spot gap-end) (- gap-start hot-spot))
             (hop-elements-right chain (min (- buffer-size gap-start)
                                           (- hot-spot gap-end)))
             (let ((nb-left (- hot-spot gap-end)))
               (unless (zerop nb-left)
                 (push-elements-left chain nb-left))))
            (t
             (hop-elements-left chain (min gap-end (- gap-start hot-spot)))
             (let ((nb-right (- gap-start hot-spot)))
               (unless (zerop nb-right)
                 (push-elements-right chain nb-right))))))))

(defgeneric move-elements (standard-flexichain to from start1 start2 end2)
  (:documentation "move elements of a flexichain and adjust data-start"))

(defmethod move-elements ((fc standard-flexichain) to from start1 start2 end2)
  (replace to from :start1 start1 :start2 start2 :end2 end2)
  (with-slots (data-start) fc
     (when (and (<= start2 data-start) (< data-start end2))
       (incf data-start (- start1 start2)))))

(defgeneric fill-gap (standard-flexichain start end)
  (:documentation "fill part of gap with the fill element"))

(defmethod fill-gap ((fc standard-flexichain) start end)
  (with-slots (buffer fill-element) fc
     (fill buffer fill-element :start start :end end)))

(defun push-elements-left (chain count)
  "Pushes the COUNT elements of CHAIN at the right of the gap,
to the beginning of the gap. The gap must be continuous. Example:
PUSH-ELEMENTS-LEFT abcd-----efghijklm 2  => abcdef-----ghijklm"
  (with-slots (buffer gap-start gap-end) chain
    (move-elements chain buffer buffer gap-start gap-end (+ gap-end count))
    (fill-gap chain (max gap-end (+ gap-start count)) (+ gap-end count))
    (incf gap-start count)
    (incf gap-end count)
    (normalize-indices chain)))

(defun push-elements-right (chain count)
  "Pushes the COUNT elements of CHAIN at the left of the gap,
to the end of the gap. The gap must be continuous. Example:
PUSH-ELEMENTS-RIGHT abcd-----efghijklm 2  =>  ab-----cdefghijklm"
  (with-slots (buffer gap-start gap-end) chain
    (let* ((buffer-size (length buffer))
           (rotated-gap-end (if (zerop gap-end) buffer-size gap-end)))
      (move-elements chain buffer buffer
                     (- rotated-gap-end count) (- gap-start count) gap-start)
      (fill-gap chain (- gap-start count) (min gap-start (- rotated-gap-end count)))
      (decf gap-start count)
      (setf gap-end (- rotated-gap-end count))
      (normalize-indices chain))))

(defun hop-elements-left (chain count)
  "Moves the COUNT rightmost elements to the end of the gap,
on the left of the data. Example:
HOP-ELEMENTS-LEFT ---abcdefghijklm--- 2  =>  -lmabcdefghijk-----"
  (with-slots (buffer gap-start gap-end) chain
    (let* ((buffer-size (length buffer))
           (rotated-gap-start (if (zerop gap-start) buffer-size gap-start)))
      (move-elements chain buffer buffer
                     (- gap-end count) (- rotated-gap-start count) rotated-gap-start)
      (fill-gap chain (- rotated-gap-start count) rotated-gap-start)
      (setf gap-start (- rotated-gap-start count))
      (decf gap-end count)
      (normalize-indices chain))))

(defun hop-elements-right (chain count)
  "Moves the COUNT leftmost elements to the beginning of the gap,
on the right of the data. Example:
HOP-ELEMENTS-RIGHT ---abcdefghijklm--- 2  =>  -----cdefghijklmab-"
  (with-slots (buffer gap-start gap-end) chain
    (move-elements chain buffer buffer gap-start gap-end (+ gap-end count))
    (fill-gap chain gap-end (+ gap-end count))
    (incf gap-start count)
    (incf gap-end count)
    (normalize-indices chain)))

(defun increase-buffer-size (chain nb-elements)
  (resize-buffer chain (required-space chain nb-elements)))

(defun decrease-buffer-size (chain)
  (resize-buffer chain (required-space chain (nb-elements chain))))

(defgeneric resize-buffer (standard-flexichain new-buffer-size)
  (:documentation "allocate a new buffer with the size indicated"))

(defmethod resize-buffer ((fc standard-flexichain) new-buffer-size)
  (with-slots (buffer gap-start gap-end
               fill-element element-type expand-factor) fc
    (let ((buffer-size (length buffer))
          (buffer-after (make-array new-buffer-size
                                    :element-type element-type
                                    :initial-element fill-element)))
      (case (gap-location fc)
        ((:gap-empty :gap-middle)
         (move-elements fc buffer-after buffer 0 0 gap-start)
         (let ((gap-end-after (- new-buffer-size (- buffer-size gap-end))))
           (move-elements fc buffer-after buffer gap-end-after gap-end buffer-size)
           (setf gap-end gap-end-after)))
        (:gap-right
         (move-elements fc buffer-after buffer 0 0 gap-start))
        (:gap-left
         (let ((gap-end-after (- new-buffer-size (+ 2 (nb-elements fc)))))
           (move-elements fc buffer-after buffer gap-end-after gap-end buffer-size)
           (setf gap-end gap-end-after)))
        (:gap-non-contiguous
         (move-elements fc buffer-after buffer 0 gap-end gap-start)
         (decf gap-start gap-end)
         (setf gap-end 0)))
      (setf buffer buffer-after)))
  (normalize-indices fc))

(defun normalize-indices (chain)
  "Sets gap limits to 0 if they are at the end of the buffer."
  (with-slots (buffer gap-start gap-end data-start) chain
    (let ((buffer-size (length buffer)))
      (when (>= data-start buffer-size)
        (setf data-start 0))
      (when (>= gap-start buffer-size)
        (setf gap-start 0))
      (when (>= gap-end buffer-size)
        (setf gap-end 0)))))

(defun gap-location (chain)
  "Returns a keyword indicating the general location of the gap."
  (with-slots (buffer gap-start gap-end) chain
    (cond ((= gap-start gap-end) :gap-empty)
          ((and (zerop gap-start) (>= gap-end 0)) :gap-left)
          ((and (zerop gap-end) (> gap-start 0)) :gap-right)
          ((> gap-end gap-start) :gap-middle)
          (t :gap-non-contiguous))))
