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

;;; `inspector-view'
;;;
;;; Basically just something that is distinguishable from
;;; `textual-view'.

(defclass inspector-view (textual-view)
  ())

;;; Place presentations
;;;
;;; Such a presentation visually represents the place in which an
;;; inspected object resides. The presentation also indicates whether
;;; the place can be changed (by assigning a different value or
;;; removing the value).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type place ()))

(define-presentation-method present :around ((object basic-place)
                                             (type   place)
                                             (stream t)
                                             (view   inspector-view)
                                             &key)
  ;; Present the place with the "changable" style if its value can be
  ;; changed or removed.
  (if (or (supportsp object 'setf) (supportsp object 'remove-value))
      (with-style (stream :changable) (call-next-method))
      (call-next-method)))

(macrolet
    ((define (place-class indicator)
       `(define-presentation-method present ((object ,place-class)
                                             (type   place)
                                             (stream t)
                                             (view   inspector-view)
                                             &key)
          (write-char ,indicator stream))))
  (define basic-place            #\→)
  (define sequence-element-place #\⁃)
  (define key-place              #\•)
  (define value-place            #\→))

;;; `inspected-object'

(defclass inspected-object ()
  ((%place       :initarg  :place
                 :reader   place)
   (%style       :initarg  :style
                 :accessor style
                 :initform :collapsed)
   (%occurrences :accessor occurrences
                 :initform nil)))

(defmethod object ((object inspected-object))
  (value (place object)))

(defmethod state-applicable-p ((state t) (object t) (place t))
  (eq (class-name (class-of state)) (object-state-class object place))) ; TODO compare the metaobjects?

(defmethod object-state-class ((object t) (place t))
  'inspected-object)

(defmethod make-object-state ((object t) (place t))
  (let ((class (object-state-class object place)))
    (make-instance class :place place)))

;;; Indicating circular structure using presentation highlighting
;;;
;;; Other occurrences of the object underlying the presentation being
;;; highlighted are indicated using Bezier arrows pointing at them.
;;;
;;; *OCCURRENCE-HIGHLIGHT-LIMIT* controls how many occurrences are
;;; highlighted using Bezier arrows.
;;;
;;; *OCCURRENCE-HIGHLIGHT-DISABLE-THRESHOLD* controls at which number
;;; of occurrences highlighting using Bezier arrows is disabled
;;; entirely.

(defparameter *occurrence-highlight-limit* 10)

(defparameter *occurrence-highlight-disable-threshold* 10)

(labels ((map-other-occurrences (function presentation &key count)
           (when-let ((occurrences (cdr (occurrences
                                         (presentation-object presentation)))))
             (let ((i 0))
               (map nil (lambda (other)
                          (unless (eq other presentation)
                            (funcall function i other)
                            (incf i)
                            (when (and count (>= i count))
                              (return-from map-other-occurrences))))
                    occurrences))))
         (count-other-occurrences (presentation)
           (let ((count 0))
             (map-other-occurrences (lambda (i other)
                                      (declare (ignore i other))
                                      (incf count))
                                    presentation)
             count))
         (plan (record)
           (let* ((count      (count-other-occurrences record))
                  (highlightp
                    (when-let ((threshold *occurrence-highlight-disable-threshold*))
                      (< count threshold)))
                  (end        (if-let ((limit *occurrence-highlight-limit*))
                                limit
                                count))
                  (omitted    (if highlightp
                                  (- count end)
                                  count)))
             (values end omitted highlightp)))
         (draw-arc-arrow (stream from to ink)
           (multiple-value-bind (x1 y1) (bounding-rectangle-position from)
             (multiple-value-bind (x2 y2) (bounding-rectangle-position to)
               (let ((design (mcclim-bezier:make-bezier-curve*
                              (list x1              y1
                                    (lerp .3 x1 x2) y1
                                    x2              (lerp .7 y1 y2)
                                    x2              y2))))
                 (draw-design stream design :ink ink :line-thickness 2)
                 (draw-arrow* stream
                              x2 (lerp .99 y1 y2)
                              x2               y2
                              :ink ink :line-thickness 2)))))
         (draw-occurrence-count (stream record count)
           (let ((visible-region (or (pane-viewport-region stream)
                                     (sheet-region stream)))
                 (text           (format nil "~:D other occurrence~:P in ~
                                             current view"
                                         count)))
             ;; Put TEXT below RECORD if it is visible at that
             ;; position, otherwise put it into the top right corner of
             ;; the visible region of STREAM.
             (with-bounding-rectangle* (x1 y1 x2 y2) record
               (declare (ignore y1 x2))
               (multiple-value-bind (x y)
                   (if (region-contains-position-p
                        visible-region x1 (+ y2 8 16))
                       (values x1 (+ y2 8))
                       (with-bounding-rectangle* (x1 y1 x2 y2) visible-region
                         (declare (ignore x1 y2))
                         (values (- x2
                                    (text-size stream text
                                               :text-style *badge-text-style*)
                                    8)
                                 (+ y1 8))))
                 (setf (stream-cursor-position stream) (values x y))))
             (with-output-as-badge (stream)
               (write-string text stream)))))

  (define-presentation-method highlight-presentation
    :after ((type   inspected-object)
            (record t)
            (stream t)
            (state  (eql :highlight)))
    (multiple-value-bind (count omitted highlightp) (plan record)
      ;; Draw bezier arcs to other occurrences.
      (when highlightp
        (map-other-occurrences
         (lambda (i other-presentation)
           (let ((ink (make-contrasting-inks 8 (mod i 8))))
             (when (zerop i)
               (multiple-value-call #'draw-circle* stream
                 (bounding-rectangle-position record) 5 :ink ink))
             (draw-arc-arrow stream record other-presentation ink)))
         record :count count))
      ;; Indicate number of omitted (i.e. not highlighted)
      ;; occurrences.
      (when (plusp omitted)
        (draw-occurrence-count stream record omitted))))

  (define-presentation-method highlight-presentation
    :after ((type   inspected-object)
            (record t)
            (stream t)
            (state  (eql :unhighlight)))
    (multiple-value-bind (count omitted highlightp) (plan record)
      ;; Repaint a region that is the union of the bounding regions of
      ;; all bezier arcs.
      (when highlightp
        (let ((region +nowhere+))
          (map-other-occurrences
           (lambda (i other-presentation)
             (let ((new-region
                     (with-output-to-output-record (stream)
                       (when (zerop i)
                         (multiple-value-call #'draw-circle* stream
                           (bounding-rectangle-position record) 5))
                       (draw-arc-arrow
                        stream record other-presentation +black+))))
               (setf region (region-union region new-region))))
           record :count count)
          (unless (eq region +nowhere+)
            (repaint-sheet stream region))))
      ;; Repaint region occupied by textual note.
      (when (plusp omitted)
        (let ((region (with-output-to-output-record (stream)
                        (draw-occurrence-count stream record omitted))))
          (repaint-sheet stream region))))))
