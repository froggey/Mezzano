;;;; Copyright (c) 2019 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Syncronization primitives.
;;;;
;;;; This is the high-level side, expanding on what the supervisor provides.

(defpackage :mezzano.sync
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor))
  (:import-from :mezzano.supervisor
                #:wait-for-objects
                #:get-object-event)
  (:export #:wait-for-objects
           #:get-object-event))

(in-package :mezzano.sync)

(defgeneric get-object-event (object))

(defmethod get-object-event ((object sup:event))
  object)

(defmethod print-object ((object sup:event) stream)
  (let ((name (sup:event-name object)))
    (cond (name
           (print-unreadable-object (object stream :type t :identity t)
             (format stream "~A" name)))
          (t
           (print-unreadable-object (object stream :type t :identity t))))))
