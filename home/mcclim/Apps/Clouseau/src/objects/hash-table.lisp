;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;; `hash-table-key-place'

(defclass hash-table-key-place (key-place)
  ())

(defmethod supportsp ((place     hash-table-key-place)
                      (operation (eql 'remove-value)))
  t)

(defmethod value ((place hash-table-key-place))
  (cell place))

(defmethod (setf value) (new-value (place hash-table-key-place))
  (let* ((hash-table (container place))
         (old-key    (cell place))
         (old-value  (gethash old-key hash-table)))
    (remhash old-key hash-table)
    (setf (gethash new-value hash-table) old-value)))

(defmethod remove-value ((place hash-table-key-place))
  (remhash (cell place) (container place)))

;; `hash-table-value-place'

(defclass hash-table-value-place (value-place)
  ())

(defmethod supportsp ((place     hash-table-value-place)
                      (operation (eql 'remove-value)))
  t)

(defmethod value ((place hash-table-value-place))
  (gethash (cell place) (container place)))

(defmethod (setf value) (new-value (place hash-table-value-place))
  (setf (gethash (cell place) (container place)) new-value))

(defmethod remove-value ((place hash-table-value-place))
  (remhash (cell place) (container place)))

;;; Object state

(defclass inspected-hash-table (inspected-instance)
  ()
  (:default-initargs
   :slot-style nil))

(defmethod object-state-class ((object hash-table) (place t))
  'inspected-hash-table)

;;; Object inspection methods

(defun draw-hash-table-diagram (stream hash-table)
  (let* ((width                  300)
         (height                 16)
         (size                   (hash-table-size hash-table))
         (rehash-size            (hash-table-rehash-size hash-table))
         (rehash-threshold       (hash-table-rehash-threshold hash-table))
         (max                    (max size
                                      (* size rehash-size)
                                      (* size rehash-threshold)))
         (size-ratio             (/ size max))
         (rehash-size-ratio      (/ (* size rehash-size) max))
         (rehash-threshold-ratio (/ (* size rehash-threshold) max))
         (count-ratio            (/ (hash-table-count hash-table) max)))
    (with-style (stream :hash-table-count)
      (draw-rectangle* stream 0 0 (* count-ratio width) height))
    (with-style (stream :hash-table-rehash-size)
      (draw-rectangle* stream 0 0 (* rehash-size-ratio width) height
                       :filled nil :line-dashes #(2 2)))
    (with-style (stream :hash-table-size)
      (draw-rectangle* stream 0 0 (* size-ratio width) height :filled nil))
    (with-style (stream :hash-table-rehash-threshold)
      (let ((x (* rehash-threshold-ratio width)))
        (draw-line* stream x 0 x height)))))

(defmethod inspect-object-using-state ((object hash-table)
                                       (state  inspected-hash-table)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (format-place-cells stream object 'reader-place 'hash-table-test
                            :label "Test")
        #+sbcl (format-place-cells stream object 'reader-place 'sb-ext:hash-table-synchronized-p
                                   :label "Synchronized")
        #+sbcl (format-place-cells stream object 'reader-place 'sb-ext:hash-table-weakness
                                   :label "Weakness"))
      (formatting-row (stream)
        (format-place-cells stream object 'reader-place 'hash-table-count
                            :label "Count" :object-style :hash-table-count)
        (format-place-cells stream object 'reader-place 'hash-table-size
                            :label "Size" :object-style :hash-table-size))
      (formatting-row (stream)
        (format-place-cells stream object 'reader-place 'hash-table-rehash-size
                            :label "Rehash Size"
                            :object-style :hash-table-rehash-size)
        (format-place-cells stream object 'reader-place 'hash-table-rehash-threshold
                            :label "Rehash Threshold"
                            :object-style :hash-table-rehash-threshold))))

  (with-room-for-graphics (stream)
    (draw-hash-table-diagram stream object))

  ;; Slots (not displayed by default)
  (call-next-method)

  (with-section (stream) "Entries"
    (with-placeholder-if-empty (stream)
      ((zerop (hash-table-count object)) ; TODO don't call count twice
       "No entries~%")
      (t
       (formatting-table (stream)
         (maphash
          (lambda (key value)
            (declare (ignore value))
            (formatting-row (stream)
              (format-place-cells stream object 'hash-table-key-place key)
              (format-place-cells stream object 'hash-table-value-place key)))
          object))))))

;;; Commands

(define-command (com-clear-hash-table :command-table inspector-command-table
                                      :name          "Clear Hash-table")
    ((object inspected-hash-table))
  (clrhash (object object)))

(define-presentation-to-command-translator inspected-hash-table->com-clear-hash-table
    (inspected-hash-table com-clear-hash-table inspector-command-table
     :tester ((object) (plusp (hash-table-count (object object))))
     :priority -1
     :documentation "Clear hash-table entries"
     :pointer-documentation ((object stream)
                             (format stream "~@<Clear all entries of ~A~@:>"
                                     (object object))))
    (object)
  (list object))
