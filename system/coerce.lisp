;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(define-condition unknown-coercion (type-error)
  ((object :initarg :object
           :reader unknown-coercion-object)
   (type :initarg :type
         :reader unknown-coercion-type))
  (:report (lambda (condition stream)
             (format stream "Don't know how to coerce ~S to type ~S."
                     (unknown-coercion-object condition)
                     (unknown-coercion-type condition)))))

(defun coerce-vector-element-type (type)
  "Figure out the element type of the vector type TYPE."
  (cond
    ((or (subtypep type 'base-string)
         (subtypep type 'simple-base-string))
     'base-char)
    ((or (subtypep type 'string)
         (subtypep type 'simple-string))
     'character)
    ((or (subtypep type 'bit-vector)
         (subtypep type 'simple-bit-vector))
     'bit)
    (t (let* ((expanded-type (typeexpand type))
              (element-type (if (and (consp expanded-type)
                                     (member (first expanded-type) '(array simple-array)))
                                (parse-array-type expanded-type)
                                nil)))
         (if (eql element-type '*)
             't
             element-type)))))

(defun coerce (object result-type)
  (when (or (eql result-type 't)
            (typep object result-type))
    (return-from coerce object))
  (cond ((subtypep result-type 'list)
         (map 'list 'identity object))
        ((subtypep result-type 'vector)
         (check-type object sequence)
         (let ((element-type (coerce-vector-element-type result-type)))
           (if element-type
               (make-array (length object)
                           :element-type element-type
                           :initial-contents object)
               (error 'unknown-coercion :object object :type result-type))))
        ((subtypep result-type 'float)
         (float object))
        (t (error 'unknown-coercion :object object :type result-type))))
