;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- CFFI type translator
;;;

(in-package :static-vectors)

(defctype static-vector
  (:wrapper :pointer :to-c static-vector-pointer))
