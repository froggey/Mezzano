;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; Thread Creation

(defun %make-thread (function name)
  (declare (ignore name))
  (threads:create-thread function))

(defun current-thread ()
  threads:*current-thread*)

;;; Introspection/debugging

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (threads:terminate-thread thread))

(mark-supported)
