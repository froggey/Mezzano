;;;; Copyright (c) 2019 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.disk-file-system
  (:use :cl)
  (:export #:read-sector
           #:write-sector))

(in-package :mezzano.disk-file-system)

(defun read-sector (disk start-sector n-sectors)
  "Read n sectors from disk"
  (let* ((sector-size (mezzano.supervisor:disk-sector-size disk))
         (result (make-array (* sector-size n-sectors) :element-type '(unsigned-byte 8)))
         (temp-buf (make-array sector-size :element-type '(unsigned-byte 8) :area :wired)))
    (dotimes (offset n-sectors)
      (multiple-value-bind (successp error-reason)
          (mezzano.supervisor:disk-read disk (+ start-sector offset) 1 temp-buf)
        (when (not successp)
          (error "Disk read error: ~S" error-reason)))
      (replace result temp-buf :start1 (* offset sector-size)))
    result))

(defun write-sector (disk start-sector array n-sectors)
  "Write n sectors to disk"
  (let* ((sector-size (mezzano.supervisor:disk-sector-size disk))
         (temp-buf (make-array sector-size :element-type '(unsigned-byte 8) :area :wired)))
    (dotimes (offset n-sectors)
      (replace temp-buf array :start2 (* offset sector-size))
      (multiple-value-bind (successp error-reason)
          (mezzano.supervisor:disk-write disk (+ start-sector offset) 1 temp-buf)
        (when (not successp)
          (error "Disk write error: ~S" error-reason))))))
