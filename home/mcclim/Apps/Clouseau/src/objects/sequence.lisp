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

;;; Utilities

(defun end-or-length (end length)
  (if end
      (min end length)
      length))

(defun print-sequence-header (stream length start end)
  (format stream "of length ~:D" length)
  (let ((end (end-or-length end length)))
    (when (or (plusp start) (< end length))
      (write-char #\Space stream)
      (with-style (stream :note)
        (format stream "~:D … ~:D shown" start end)))))

(defun note-truncated (stream length shown-count)
  (with-style (stream :note)
    (format stream "~:D element~:P not shown" (- length shown-count))))

;;; Object states

(defclass inspected-sequence (inspected-identity-object-mixin
                              inspected-object)
  ((%start :initarg  :start
           :accessor start
           :initform 0)
   (%end   :initarg  :end
           :accessor end
           :initform 30)))

(defmethod effective-bounds ((state inspected-sequence) (length integer))
  (let ((start (start state))
        (end   (end-or-length (end state) length)))
    (values start end (or (plusp start) (< end length)))))

;;; Object inspection methods

(defmethod inspect-object-using-state ((object sequence)
                                       (state  inspected-sequence)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (inspect-class-as-name (class-of object) stream)
  (write-char #\Space stream)
  (with-output-as-presentation (stream state 'sequence-range)
    (print-sequence-header stream (length object) (start state) (end state))))

(defmethod inspect-object-using-state ((object sequence)
                                       (state  inspected-sequence)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (call-next-method)

  (let ((length (length object)))
    (with-section (stream) "Elements"
      (with-placeholder-if-empty (stream)
        ((zerop length)
         "no elements")
        (t
         (multiple-value-bind (start end truncated?)
             (effective-bounds state length)
           (with-preserved-cursor-x (stream)
             (formatting-table (stream)
               (loop :for i :from start :below end
                     :do (formatting-row (stream)
                           (formatting-place
                               (object 'sequence-element-place i present inspect)
                             (formatting-cell (stream :align-x :right)
                               (present stream))
                             (formatting-cell (stream)
                               (inspect stream)))))))
           (when truncated?
             (note-truncated stream length (- end start)))))))))

;;; Range presentation type

(define-presentation-type sequence-or-sequence-range ())

(define-presentation-type inspected-sequence ()
  :inherit-from '(sequence-or-sequence-range))

(define-presentation-type sequence-range ()
  :inherit-from '(sequence-or-sequence-range))

;;; Commands

(define-command (com-adjust-range :command-table inspector-command-table)
    ((object 'sequence-range :gesture (:select :priority 1)))
  (with-command-error-handling ("Could not adjust range of ~A" object)
      (let ((stream    *standard-output*)
            (old-start (start object))
            (old-end   (end object))
            old-x)
        (block nil
          (tracking-pointer (stream :transformp t :multiple-window t)
            (:pointer-button-press (x)
              (setf old-x x))
            (:pointer-motion (x)
              (unless old-x
                (setf old-x x))
              (let ((delta-x (clamp (floor (- x old-x) 2)
                                    (- old-start) most-positive-fixnum))) ; TODO length
                (setf (start object) (+ old-start delta-x)
                      (end   object) (+ old-end   delta-x)))
              (note-changed (place object)))
            (:pointer-button-release ()
              (when old-x
                (return))))))))

(define-command (com-show-all :command-table inspector-command-table
                              :name          "Show all elements")
    ((object 'sequence-or-sequence-range :gesture (:select :priority -1)))
  (setf (end object) nil))

(define-command (com-show-next :command-table inspector-command-table
                               :name          "Show next page of elements")
    ((object 'inspected-sequence :gesture (:select
                                        ; TODO :documentation
                                           :priority -1)))
  (let ((delta (- (end object) (start object))))
    (incf (start object) delta)
    (incf (end object)   delta)))

(define-command (com-show-twice-as-many :command-table inspector-command-table
                                        :name          "Show twice as many elements")
    ((object 'inspected-sequence :gesture (:select
                                        ; TODO :documentation
                                           :priority -1)))
  (setf (end object) (* 2 (end object))))
