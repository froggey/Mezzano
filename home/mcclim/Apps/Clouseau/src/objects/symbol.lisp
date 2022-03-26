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

(declaim (inline symbol-state))
(defun symbol-state (symbol package)
  (nth-value 1 (find-symbol (symbol-name symbol) package)))

(declaim (inline package-locked?))
(defun package-locked? (package)
  #+sbcl (sb-ext:package-locked-p package)
  #-sbcl nil)

;;; `symbol-slot-place'

(defclass symbol-slot-place (basic-place)
  ())

#+sbcl
(defmethod supportsp :around ((place symbol-slot-place) (operation t))
  (and (if-let ((package (symbol-package (container place))))
         (not (package-locked? package))
         t)
       (call-next-method)))

(defmethod supportsp ((place symbol-slot-place) (operation (eql 'remove-value)))
  t)

;;; `symbol-value-place'
;;;
;;; TODO show type
;;; TODO check value against type

(defclass symbol-value-place (symbol-slot-place)
  ())

(defmethod supportsp ((place symbol-value-place) (operation t))
  (and (not (constantp (container place)))
       (call-next-method)))

(defmethod valuep ((place symbol-value-place))
  (boundp (container place)))

(defmethod value ((place symbol-value-place))
  (symbol-value (container place)))

(defmethod (setf value) ((new-value t) (place symbol-value-place))
  (setf (symbol-value (container place)) new-value))

(defmethod remove-value ((place symbol-value-place))
  (makunbound (container place)))

;;; `symbol-function-place'

(defclass symbol-function-place (symbol-slot-place)
  ())

(defmethod accepts-value-p ((place symbol-function-place) (value t))
  nil)

(defmethod accepts-value-p ((place symbol-function-place) (value function))
  t)

(defmethod valuep ((place symbol-function-place))
  (fboundp (container place)))

(defmethod value ((place symbol-function-place))
  (fdefinition (container place)))

(defmethod (setf value) ((new-value function) (place symbol-function-place))
  (setf (fdefinition (container place)) new-value))

(defmethod remove-value ((place symbol-function-place))
  (fmakunbound (container place)))

;;; `symbol-type-place'

(defclass symbol-type-place (symbol-slot-place)
  ())

(defmethod accepts-value-p ((place symbol-type-place) (value t))
  nil)

(defmethod accepts-value-p ((place symbol-type-place) (value class))
  t)

(defmethod valuep ((place symbol-type-place))
  (find-class (container place) nil))

(defmethod value ((place symbol-type-place))
  (find-class (container place) nil))

(defmethod (setf value) ((new-value class) (place symbol-type-place))
  (setf (find-class (container place) nil) new-value))

(defmethod remove-value ((place symbol-type-place))
  (setf (find-class place nil) nil))

(defmethod make-object-state ((object class)
                              (place  symbol-type-place))
  (make-instance (object-state-class object place) :place place
                                                   :style :name-only))

;;; Object states

(defclass inspected-package (inspected-instance)
  ((%symbol-filter :initarg  :symbol-filter
                   :accessor symbol-filter
                   :initform nil))
  (:default-initargs
   :slot-style nil))

(defmethod (setf symbol-filter) :after ((new-value t)
                                        (object    inspected-package))
  ())

(defmethod object-state-class ((object package) (place t))
  'inspected-package)

;;; Object inspection methods

;;; Symbol

(defmethod inspect-object-using-state :after ((object symbol)
                                              (state  inspected-object)
                                              (style  (eql :badges))
                                              (stream t))
  (write-char #\Space stream)
  (if-let ((package (symbol-package object)))
    (badge stream "~(~A~)" (symbol-state object package))
    (badge stream "uninterned"))

  #+sbcl (when-let ((kind (sb-cltl2:variable-information object)))
           (write-char #\Space stream)
           (badge stream "~(~A~)" kind)))

(defmethod inspect-object-using-state ((object symbol)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (formatting-row (stream)
      (format-place-cells stream object 'reader-place 'symbol-name
                          :label "Name")
      (format-place-cells stream object 'reader-place 'symbol-package ; TODO should be mutable
                          :label "Package"))
    (formatting-row (stream)
      (format-place-cells stream object 'symbol-value-place nil
                          :label "Value")
      (format-place-cells stream object 'symbol-function-place nil
                          :label "Function"))
    (formatting-row (stream)
      (format-place-cells stream object 'symbol-type-place nil
                          :label "Type"))))

;;; Package

(defun package-symbols (package &key filter)
  (let ((result (make-array 100 :adjustable t :fill-pointer 0)))
    (do-external-symbols (symbol package)
      (when (and (eq (symbol-package symbol) package)
                 (or (not filter)
                     (funcall filter symbol)))
        (vector-push-extend symbol result)))
    (sort result #'string-lessp :key #'symbol-name)))

(defmethod inspect-object-using-state :after ((object package)
                                              (state  inspected-object)
                                              (style  (eql :badges))
                                              (stream t))
  (when (package-locked? object)
    (write-char #\Space stream)
    (badge stream "locked")))

;; TODO style symbols grouped by external etc.
(defmethod inspect-object-using-state ((object package)
                                       (state  inspected-package)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (format-place-cells stream object 'reader-place 'package-name
                            :label "Name")
        (format-place-cells stream object 'reader-place 'package-nicknames
                            :label "Nicknames")
        #+sbcl (format-place-cells stream object 'reader-place 'package-locked?
                                   :label "Locked"))
      (formatting-row (stream)
        (format-place-cells stream object 'reader-place 'package-use-list
                            :label "Uses")
        (format-place-cells stream object 'reader-place 'package-used-by-list
                            :label "Used by"))
      #+sbcl (format-place-row stream object 'reader-place 'sb-ext:package-local-nicknames
                               :label "Local nicknames")))

  (print-documentation object stream)

  ;; Slots (not displayed by default)
  (call-next-method)

  ;; Symbols
  (with-section (stream) "Symbols"
    (with-drawing-options (stream :text-size :smaller)
      (formatting-table (stream)
        (formatting-header (stream) "Symbol" "Value" "Function" "Type")

        (flet ((symbol-row (symbol)
                 (formatting-row (stream)
                   (formatting-place (object 'pseudo-place symbol nil inspect* :place-var place)
                     (formatting-cell (stream) (inspect* stream))
                     ;; Value slot
                     (formatting-place (symbol 'symbol-value-place nil present inspect)
                       (formatting-cell (stream) (present stream) (inspect stream)))
                     ;; Function slot
                     (formatting-place (symbol 'symbol-function-place nil present inspect)
                       (formatting-cell (stream) (present stream) (inspect stream)))
                     ;; Type slot
                     (formatting-place (symbol 'symbol-type-place nil present inspect)
                       (formatting-cell (stream) (present stream) (inspect stream)))))))
          (map nil #'symbol-row (package-symbols object :filter (symbol-filter state))))))))

;; TODO command: trace all symbols
