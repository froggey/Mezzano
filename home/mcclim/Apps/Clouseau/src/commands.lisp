;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
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

(defun call-with-command-error-handling (do-thunk undo-thunk
                                         &optional format-control
                                         &rest format-arguments)
  (handler-case
      (funcall do-thunk)
    (error (condition)
      (funcall undo-thunk)
      (let ((stream (frame-standard-output *application-frame*)))
        (with-style (stream :error)
          (format stream "~&~@<~?: ~A~@:>~%"
                  (or format-control "Error executing command")
                  format-arguments
                  condition))))))

(defmacro with-command-error-handling ((&optional format-control
                                        &rest format-arguments)
                                       do-form &body undo-forms)
  `(call-with-command-error-handling
    (lambda () ,do-form) (lambda () ,@undo-forms)
    ,format-control ,@format-arguments))

(defun eval-with-bindings (form &key object-state place root-place)
  (let* ((place       (cond (object-state (place object-state))
                            (place)))
         (object      (when (and place (valuep place))
                        (value place)))
         (root-place  (cond (root-place)
                            (place (root place))))
         (root-object (value root-place))
         (output      (make-string-output-stream))
         (warnings    '()))
    (multiple-value-bind (function warningsp failurep)
        (let ((*standard-output* output)
              (*error-output*    output)
              (*trace-output*    output))
          (handler-bind ((style-warning #'muffle-warning)
                         (warning       (lambda (condition)
                                          (push condition warnings))))
            (compile nil `(lambda ()
                            (let (,@(when root-object
                                      `((** (quote ,root-object))))
                                  ,@(when object
                                      `((* (quote ,object)))))
                              ,form)))))
      (declare (ignore warningsp))
      (when failurep
        (error "~:[Invalid expression~;~:*~{~%~A~}~]" warnings))
      (funcall function))))

;;; Inspector command table
;;;
;;; Contains commands that should be available in any context (as
;;; opposed to, say, the standalone inspector application).

(define-command-table inspector-command-table)

;;; Commands on all inspected objects

(defun toggle-expand-documentation (which object stream)
  (format stream "~A " which)
  (with-print-error-handling (stream)
    (with-safe-and-terse-printing (stream)
      (princ (object object) stream))))

(define-command (com-expand :command-table inspector-command-table
                            :name          t)
    ((object inspected-object))
  (setf (style object) :expanded))

(define-presentation-to-command-translator object->expand
    (inspected-object com-expand inspector-command-table
     :tester ((object) (not (eq (style object) :expanded)))
     :documentation "Expand object"
     :pointer-documentation ((object stream)
                             (toggle-expand-documentation
                              "Expand" object stream)))
    (object)
  (list object))

(define-command (com-collapse :command-table inspector-command-table
                              :name          t)
    ((object inspected-object))
  (setf (style object) :collapsed))

(define-presentation-to-command-translator object->collapse
    (inspected-object com-collapse inspector-command-table
     :tester ((object) (eq (style object) :expanded))
     :documentation "Collapse object"
     :pointer-documentation ((object stream)
                             (toggle-expand-documentation
                              "Collapse" object stream)))
    (object)
  (list object))

(define-command (com-eval-with-context :command-table inspector-command-table
                                       :name          t)
    ((object 'inspected-object :prompt  "context object"
                               :default (state (root-place *application-frame*)))
     (form   'clim:form))
  (with-command-error-handling ("Error evaluating form")
      (let ((result (eval-with-bindings form :object-state object)))
        (present result 'clim:expression))))

(define-presentation-to-command-translator object->eval-with-context
    (inspected-object com-eval-with-context inspector-command-table
     :priority      -1
     :documentation "Evaluate a form in this context")
    (object)
  (list object (accept 'clim:form :prompt "form")))

;;; Commands on all places

(define-command (com-set-place :command-table inspector-command-table
                               :name          t)
    ((place 'place     :prompt "Place to set value of")
     (form  'clim:form :prompt "New place value (evaluated)"))
  (with-command-error-handling ("Error evaluating and setting value")
      (let ((new-value (eval-with-bindings form :place place)))
        (setf (value place) new-value))))

(define-presentation-to-command-translator place->com-set-place
    (place com-set-place inspector-command-table
     :gesture       :edit
     :tester        ((object) (supportsp object 'setf))
     :documentation "Set value of place")
    (object)
  (list object (accept 'clim:form :prompt "New place value (evaluated)")))

(define-command (com-remove-place-value :command-table inspector-command-table
                                        :name          t)
    ((place 'place :prompt "Place to remove value of"))
  (with-command-error-handling
      ("Could not remove value of place ~A" place)
      (remove-value place)))

(define-presentation-to-command-translator place->com-remove-place-value
    (place com-remove-place-value inspector-command-table
     :gesture :delete
     :tester ((object)
              (and (supportsp object 'remove-value)
                   (safe-valuep object)))
     :documentation "Remove value of place")
    (object)
  (list object))

(define-command (com-copy-place-value :command-table inspector-command-table
                                      :name          t)
    ((from-place 'place :prompt "From place")
     (to-place   'place :prompt "To place"))
  (with-command-error-handling
      ("Could not copy value from ~A to ~A" from-place to-place)
      (let ((new-value (value from-place)))
        (setf (value to-place) new-value
              (state to-place) (make-object-state new-value to-place)))))

(define-gesture-name :copy :pointer-button-press (:left :control))

(define-drag-and-drop-translator drag-copy-place-value
    (place command place inspector-command-table
     :gesture :copy
     :tester ((object from-object)
              (cond ((not from-object)
                     (safe-valuep object)) ; TODO should work for unbound?
                    ((eq from-object object)
                     nil)
                    ((safe-valuep from-object)
                     (ignore-errors ; TODO do this properly
                      (and (supportsp object 'setf)
                           (accepts-value-p object (value from-object)))))
                    (t
                     (supportsp object 'remove-value))))
     :pointer-documentation ((object destination-object stream)
                             (if destination-object
                                 (format stream "Copy value of ~A into ~A"
                                         object destination-object)
                                 (format stream "Drag onto place to ~
                                                 copy value of ~A"
                                         object))))
    (object destination-object)
  (list 'com-copy-place-value object destination-object))

(define-command (com-swap-place-values :command-table inspector-command-table
                                       :name          t)
    ((place-1 'place :prompt "First place")
     (place-2 'place :prompt "Second place"))
  ;; Attempt to change values (without children and states) first so
  ;; that fewer things need undoing if, for example, a slot type check
  ;; signals an error.
  (let ((old-value-1 (value place-1))
        (old-value-2 (value place-2)))
    (with-command-error-handling
        ("Could not swap ~A and ~A" place-1 place-2)
        (progn
          (setf (value place-1) old-value-2
                (value place-2) old-value-1)
          (rotatef (children place-1) (children place-2))
          (rotatef (state place-1)    (state place-2)))
      (setf (value place-1) old-value-1
            (value place-2) old-value-2))))

(define-drag-and-drop-translator drag-swap-place-values
    (place command place inspector-command-table
     :gesture :select
     :tester ((object from-object)
              (cond ((not from-object)
                     (safe-valuep object)) ; TODO should work for unbound?
                    ((eq from-object object)
                     nil)
                    ((safe-valuep from-object)
                     (ignore-errors ; TODO do this properly
                      (and (supportsp object 'setf)
                           (accepts-value-p object (value from-object)))))
                    (t
                     (supportsp object 'remove-value))))
     :documentation ((object stream)
                     (format stream "Drag ~A onto another slot to swap ~
                                     their contents."
                             object))
     :pointer-documentation ((object destination-object stream)
                             (if destination-object
                                 (format stream "Swap ~A and ~A"
                                         object destination-object)
                                 (format stream "Drag onto place to ~
                                                 swap with ~A"
                                         object))))
    (object destination-object)
  (list 'com-swap-place-values object destination-object))

;;; Commands on Boolean-valued places

(define-command (com-set-place-to-false :command-table inspector-command-table
                                        :name          t)
    ((place 'place))
  (with-command-error-handling ("Could not set value of ~A to false" place)
      (setf (value place) nil)))

(define-presentation-to-command-translator place->com-set-place-to-false
    (place com-set-place-to-false inspector-command-table
     :tester ((object)
              (and (supportsp object 'setf)
                   (accepts-value-p object t)
                   (safe-valuep object)
                   (eq (value object) t)))
     :priority 2
     :documentation "Set to false"
     :pointer-documentation
     ((object stream)
      (format stream "Set value of ~A to ~S" object nil)))
    (object)
  (list object))

(define-command (com-set-place-to-true :command-table inspector-command-table
                                       :name          t)
    ((place 'place))
  (with-command-error-handling ("Could not set value of ~A to true" place)
      (setf (value place) t)))

(define-presentation-to-command-translator place->com-set-place-to-true
    (place com-set-place-to-true inspector-command-table
     :tester ((object)
              (and (supportsp object 'setf)
                   (accepts-value-p object t)
                   (safe-valuep object)
                   (eq (value object) nil)))
     :priority 2
     :documentation "Set to true"
     :pointer-documentation
     ((object stream)
      (format stream "Set value of ~A to ~S" object t)))
    (object)
  (list object))

;;; Commands on real-valued places

(define-command (com-increment-place :command-table inspector-command-table
                                     :name          t)
    ((place 'place))
  (with-command-error-handling ("Could not increment the value of ~A" place)
      (incf (value place))))

(clim:define-gesture-name :increment :pointer-scroll (:wheel-up :control))

(define-presentation-to-command-translator place->com-inrement-place
    (place com-increment-place inspector-command-table
     :gesture :increment
     :tester ((object)
              (and (supportsp object 'setf)
                   (safe-valuep object)
                   (let ((value (value object)))
                     (and (typep value 'real)
                          (accepts-value-p object (1+ value))))))
     :documentation "Increment by 1"
     :pointer-documentation ((object stream)
                             (format stream "Increment ~A by 1" object)))
    (object)
  (list object))

(define-command (com-decrement-place :command-table inspector-command-table
                                     :name          t)
    ((place 'place))
  (with-command-error-handling ("Could not decrement the value of ~A" place)
      (decf (value place))))

(clim:define-gesture-name :decrement :pointer-scroll (:wheel-down :control))

(define-presentation-to-command-translator place->com-decrement-place
    (place com-decrement-place inspector-command-table
     :gesture :decrement
     :tester ((object)
              (and (supportsp object 'setf)
                   (safe-valuep object)
                   (let ((value (value object)))
                     (and (typep value 'real)
                          (accepts-value-p object (1- value))))))
     :documentation "Decrement by 1"
     :pointer-documentation ((object stream)
                             (format stream "Decrement ~A by 1" object)))
    (object)
  (list object))
