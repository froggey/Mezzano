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

;;; Always

(defmethod inspect-object-using-state :around ((object t)
                                               (state  t)
                                               (style  t)
                                               (stream t))
  (with-print-error-handling (stream)
    (call-next-method)))

;;; Collapsed

(defmethod inspect-object-using-state :around ((object t)
                                               (state  t)
                                               (style  (eql :collapsed))
                                               (stream t))
  (with-drawing-options (stream :text-family :fix)
    (call-next-method)))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :collapsed))
                                       (stream t))
  (with-safe-and-terse-printing (stream)
    (prin1 object stream)))

;;; Expanded

(defmethod inspect-object-using-state :around ((object t)
                                               (state  inspected-object)
                                               (style  (eql :expanded))
                                               (stream t))
  (with-object-border (stream *depth*)
    (call-next-method object state style stream)))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :expanded))
                                       (stream t))
  (formatting-table (stream)
    (formatting-column (stream)
      (formatting-row (stream)
        (formatting-cell (stream)
          (with-style (stream :header)
            (inspect-object-using-state object state :expanded-header stream))))
      (formatting-row (stream)
        (formatting-cell (stream)
          (inspect-object-using-state object state :expanded-body stream))))))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (with-safe-and-terse-printing (stream)
    (prin1 object stream)))

(defmethod inspect-object-using-state :after ((object t)
                                              (state  inspected-object)
                                              (style  (eql :expanded-header))
                                              (stream t))
  (inspect-object-using-state object state :badges stream))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :badges))
                                       (stream t)))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t)))

;;; Object identity mixin

(defclass inspected-identity-object-mixin ()
  ())

(defmethod inspect-object-using-state :after ((object t)
                                              (state  inspected-identity-object-mixin)
                                              (style  (eql :expanded-header))
                                              (stream t))
  (write-char #\Space stream)
  (inspect-object-using-state object state :object-identity stream))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-identity-object-mixin)
                                       (style  (eql :object-identity))
                                       (stream t))
  (print-object-identity object stream))

;;; Collapsed state mixin

(defclass remembered-collapsed-style-mixin ()
  ((%collapsed-style :initarg  :collapsed-style
                     :accessor collapsed-style)))

(defmethod initialize-instance :after
    ((instance remembered-collapsed-style-mixin)
     &key
     (collapsed-style nil collapsed-style-supplied-p))
  (declare (ignore collapsed-style))
  (unless collapsed-style-supplied-p
    (setf (collapsed-style instance) (style instance))))

(defmethod (setf style) :around ((new-value (eql :collapsed))
                                 (object    remembered-collapsed-style-mixin))
  (let ((collapsed-style (collapsed-style object)))
    (if (eq new-value collapsed-style)
        (call-next-method)
        (setf (style object) collapsed-style))))
