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

(defun safe-function-lambda-expression (function)
  ;; SBCL can signal UNBOUND-SLOT if FUNCTION is the prototype of a
  ;; funcallable instance class.
  #+sbcl (handler-case
             (function-lambda-expression function)
           (unbound-slot () nil))
  #-sbcl (function-lambda-expression function))

(defun function-name (function)
  (when-let ((name (nth-value 2 (safe-function-lambda-expression function))))
    (unless (typep name '(cons (eql lambda)))
      name)))

(defun function-lambda-list
    (function &optional (expression (safe-function-lambda-expression function)))
  #+sbcl (declare (ignore expression))
  (values #+sbcl (sb-introspect:function-lambda-list function) #+sbcl t
          #-sbcl (second expression) #-sbcl expression))

(defun function-closure-p (function)
  (and (nth-value 1 (safe-function-lambda-expression function))
       #+sbcl (sb-kernel:closurep function)))

(defun function-traced-p (function)
  #-sbcl nil
  #+sbcl (gethash function sb-debug::*traced-funs*))

#+sbcl
(defun trace-function (function)
  (sb-debug::trace-1 (function-name function)
                     (sb-debug::make-trace-info)
                     function))

#+sbcl
(defun untrace-function (function)
  (sb-debug::untrace-1 (function-name function)))

;;; `method-place'

(defclass method-place (read-only-place)
  ())

(defmethod supportsp ((place method-place) (operation (eql 'remove-value)))
  t)

(defmethod value ((place method-place))
  (cell place))

(defmethod remove-value ((place method-place))
  (remove-method (container place) (value place)))

(defmethod qualifiers ((place method-place))
  (method-qualifiers (cell place)))

(defmethod specializers ((place method-place))
  (c2mop:method-specializers (cell place)))

;;; `closed-over-place'

(defclass closed-over-place (read-only-place)
  ())

#+sbcl
(defmethod value ((place closed-over-place))
  (let ((closure (container place))
        (index   (cell place)))
    (sb-kernel:%closure-index-ref closure index)))

;;; Object states

(defclass inspected-function (inspected-identity-object-mixin
                              inspected-object)
  ((%disassembly-style :initarg  :disassembly-style
                       :type     (member nil t) ; could be text vs. graph later
                       :accessor disassembly-style
                       :initform nil)))

(defmethod object-state-class ((object function) (place t))
  'inspected-function)

(defclass inspected-funcallable-standard-object (inspected-function
                                                 inspected-instance)
  ())

(defmethod object-state-class ((object c2mop:funcallable-standard-object)
                               (place  t))
  'inspected-funcallable-standard-object)

(defclass inspected-generic-function (inspected-funcallable-standard-object)
  ()
  (:default-initargs
   :slot-style nil))

(defmethod object-state-class ((object generic-function) (place t))
  'inspected-generic-function)

(defclass inspected-method (inspected-instance
                            remembered-collapsed-style-mixin)
  ()
  (:default-initargs
   :slot-style nil))

(defmethod object-state-class ((object method) (place t))
  'inspected-method)

(defmethod make-object-state ((object method) (place method-place))
  (let ((class (object-state-class object place)))
    (make-instance class :place place :style :class-only)))

;;; Object inspection methods

;;; `function'

(defmethod inspect-object-using-state ((object function)
                                       (state  inspected-function)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (inspect-class-as-name (class-of object) stream)

  (write-char #\Space stream)
  (let ((name (function-name object)))
    (with-placeholder-if-empty (stream)
      ((not name) "no name")
      (t          (princ name stream)))))

(defmethod inspect-object-using-state :after ((object function)
                                              (state  inspected-function)
                                              (style  (eql :badges))
                                              (stream t))
  ;; Closure
  (when (function-closure-p object)
    (write-char #\Space stream)
    (badge stream "closure"))
  ;; Traced
  (when (function-traced-p object)
    (write-string " " stream)
    (badge stream "traced"))
  ;; Interpreted
  (unless (compiled-function-p object)
    (write-string " " stream)
    (badge stream "interpreted")))

(defun inspect-closure-cells (function stream)
  (formatting-table (stream)
    #+sbcl
    (loop :for i :below (1- (sb-kernel:get-closure-length function))
          :do (format-place-row stream function 'closed-over-place i))))

(defmethod inspect-object-using-state ((object function)
                                       (state  inspected-function)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (multiple-value-bind (expression closurep)
      (safe-function-lambda-expression object)
    (with-preserved-cursor-x (stream)
      (formatting-table (stream)
        (multiple-value-bind (lambda-list lambda-list-p)
            (function-lambda-list object expression)
          ;; Name
          (format-place-row stream object 'reader-place 'function-name
                            :label "Name")
          ;; Lambda list
          (formatting-row (stream)
            (formatting-place (object 'pseudo-place lambda-list present inspect)
              (with-style (stream :slot-like)
                (formatting-cell (stream) (write-string "Lambda list" stream))
                (formatting-cell (stream) (present stream)))
              (formatting-cell (stream)
                (with-placeholder-if-empty (stream)
                  ((not lambda-list-p)
                   "not available")
                  (t
                   (inspect stream)))))))
        ;; Type
        #+sbcl
        (format-place-row stream object 'reader-place 'sb-introspect:function-type
                          :label "Type")))
    ;; Documentation
    (print-documentation object stream)
    ;; Close cells
    (when (and closurep #+sbcl (sb-kernel:closurep object))
      (with-section (stream) "Closure cells"
        (inspect-closure-cells object stream)))
    ;; Disassembly
    (when (disassembly-style state)
      (display-disassembly object stream))))

;;; `funcallable-standard-object'

(defmethod inspect-object-using-state :after ((object function)
                                              (state  inspected-funcallable-standard-object)
                                              (style  (eql :badges))
                                              (stream t))
  (write-string " " stream)
  (badge stream "funcallable"))

(defmethod inspect-object-using-state ((object c2mop:funcallable-standard-object)
                                       (state  inspected-funcallable-standard-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  ;; Function
  (call-next-method)

  ;; Slots
  (with-section (stream) "Slots"
    (inspect-slots object (slot-style state) stream)))

;;; `generic-function'

(defun max-specializers (methods)
  (reduce #'max methods :key (compose #'length #'c2mop:method-specializers)))

(defun inspect-method-list (generic-function methods stream
                            &key generic-function-name
                                 (specializer-column-count
                                  (max-specializers methods)))
  (formatting-table (stream)
    (formatting-row (stream)
      (if generic-function-name
          (formatting-header (stream) "Generic function" "Qualifiers" "Specializers")
          (formatting-header (stream)                    "Qualifiers" "Specializers")))
    (map nil (lambda (method)
               (let ((generic-function (or generic-function
                                           (c2mop:method-generic-function method))))
                 (formatting-place (generic-function 'method-place method
                                    present inspect :place-var place)
                   (formatting-row (stream)
                     (with-style (stream :slot-like)
                       (when generic-function-name
                         (formatting-cell (stream)
                           (princ (c2mop:generic-function-name generic-function) ; TODO present
                                  stream)))
                       (formatting-cell (stream)
                         (format-items (qualifiers place) :stream stream))
                       (let* ((specializers (specializers place))
                              (missing      (- specializer-column-count
                                               (length specializers))))
                         (map nil (lambda (specializer)
                                    (formatting-cell (stream)
                                      (when specializer
                                        (with-print-error-handling (stream)
                                          (typecase specializer
                                            (class (inspect-class-as-name specializer stream))
                                            (t     (prin1 `(eql ,(c2mop:eql-specializer-object specializer)) stream))))))) ; TODO should be inspectable
                              (append specializers (make-list missing)))))
                     (formatting-cell (stream)
                       (present stream))
                     (formatting-cell (stream)
                       (inspect stream))))))
         methods)))

(defmethod inspect-object-using-state ((object generic-function)
                                       (state  inspected-generic-function)
                                       (style  (eql :expanded-body))
                                       (stream t))
  ;; Funcallable standard object
  (call-next-method)

  ;; method class
  ;; method combination
  ;; Methods
  (with-section (stream) "Methods"
    (let ((methods (c2mop:generic-function-methods object)))
      (with-placeholder-if-empty (stream)
        ((null methods)
         "No methods~%")
        (t
         (inspect-method-list object methods stream))))))

;; `method'

(defmethod inspect-object-using-state ((object method)
                                       (state  inspected-method)
                                       (style  (eql :class-only))
                                       (stream t))
  (princ (class-name (class-of object)) stream))

(defmethod inspect-object-using-state ((object method)
                                       (state  inspected-method)
                                       (style  (eql :expanded-body))
                                       (stream t))
  ;; Documentation
  (print-documentation object stream)
  ;; Slots
  (call-next-method))

;;; Commands

;;; Generic functions

(define-command (com-remove-methods :command-table inspector-command-table
                                    :name          "Remove all Methods")
    ((object 'inspected-generic-function))
  (with-command-error-handling ("Could not remove all methods")
      (let ((object (object object)))
        (map nil (curry #'remove-method object)
             (c2mop:generic-function-methods object)))))

(define-presentation-to-command-translator
    inspected-generic-function->com-remove-methods
    (inspected-generic-function com-remove-methods inspector-command-table
     :tester ((object)
              (not (null (ignore-errors
                          (c2mop:generic-function-methods (object object))))))
     :priority -1
     :documentation "Remove all methods"
     :pointer-documentation ((object stream)
                             (format stream "~@<Remove methods from ~
                                             the generic function ~A~@:>"
                                     (object object))))
    (object)
  (list object))

;;; Tracing

#+sbcl
(define-command (com-trace :command-table inspector-command-table
                           :name          "Trace Function")
    ((object 'inspected-function))
  (let ((function (object object)))
    (with-command-error-handling ("Could not trace ~A" function)
      (trace-function function))))

#+sbcl
(define-presentation-to-command-translator inspected-function->com-trace
    (inspected-function com-trace inspector-command-table
     :tester ((object)
              (let ((function (object object)))
                (and (function-name function)
                     (not (function-traced-p function)))))
     :priority -1
     :documentation "Trace function"
     :pointer-documentation ((object stream)
                             (format stream "~@<Trace the function ~A~@:>"
                                     (object object))))
    (object)
  (list object))

#+sbcl
(define-command (com-untrace :command-table inspector-command-table
                             :name          "Untrace Function")
    ((object 'inspected-function))
  (let ((function (object object)))
    (with-command-error-handling ("Could not untrace ~A" object)
      (untrace-function function))))

#+sbcl
(define-presentation-to-command-translator inspected-function->com-untrace
    (inspected-function com-untrace inspector-command-table
     :tester ((object)
              (let ((function (object object)))
                (and (function-name function)
                     (function-traced-p function))))
     :priority -1
     :documentation "Untrace function"
     :pointer-documentation ((object stream)
                             (format stream "~@<Untrace the function ~A~@:>"
                                     (object object))))
  (object)
  (list object))

;;; Disassembling

(define-command (com-show-disassembly :command-table inspector-command-table
                                      :name          t)
    ((object 'inspected-function))
  (setf (disassembly-style object) t))

(define-presentation-to-command-translator inspected-function->com-show-disassembly
    (inspected-function com-show-disassembly inspector-command-table
     :tester ((object)
              (and (eq (style object) :expanded)
                   (not (disassembly-style object))))
     :priority -1
     :documentation "Show disassembly"
     :pointer-documentation ((object stream)
                             (format stream "~@<Show disassembly for ~
                                             function ~A~@:>"
                                     (object object))))
    (object)
  (list object))

(define-command (com-hide-disassembly :command-table inspector-command-table
                                      :name          t)
    ((object 'inspected-function))
  (setf (disassembly-style object) nil))

(define-presentation-to-command-translator inspected-function->com-hide-disassembly
    (inspected-function com-hide-disassembly inspector-command-table
     :tester ((object)
              (and (eq (style object) :expanded)
                   (disassembly-style object)))
     :priority -1
     :documentation "Hide disassembly"
     :pointer-documentation ((object stream)
                             (format stream "~@<Hide disassembly for ~
                                             function ~A~@:>"
                                     (object object))))
    (object)
  (list object))
