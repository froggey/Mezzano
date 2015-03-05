;;;; Copyright (c) 2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; The high-level part of the profiler.
;;;; Takes raw samples and turns them into a more useful form.

(defpackage :mezzano.profiler
  (:use :cl)
  (:export #:decode-profile-buffer
           #:save-profile))

(in-package :mezzano.profiler)

(defun decode-profile-buffer (buffer)
  "Convert the buffer returned by STOP-PROFILING into a more useful format.
The original buffer is filled with return addresses, not with the actual functions.
The returned value is a sequence of samples, and each sample is a sequence that
begins with the thread that was active the time, followed by the functions that
were on the call-stack when the sample was taken. The functions are ordered from
innermost (most recently called) to outermost (the entry point) and the samples
are from oldest to newest."
  (let ((offset 0)
        (profile-entries (make-array 100 :adjustable t :fill-pointer 0)))
    (loop
       (when (>= offset (length buffer))
         (return))
       (cond ((mezzano.supervisor:threadp (svref buffer offset))
              ;; A sample is a thread object, followed by many fixnums.
              (let ((current (make-array 100 :adjustable t :fill-pointer 0)))
                (vector-push-extend (svref buffer offset) current)
                (incf offset)
                (loop
                   (when (or (>= offset (length buffer))
                             (not (integerp (svref buffer offset))))
                     (return))
                   ;; FIXME: Should check that this is actually a function.
                   ;; FIXME: This is probably wildly GC-unsafe.
                   (let* ((return-address (svref buffer offset))
                          (base-address (sys.int::base-address-of-internal-pointer return-address))
                          (fn (sys.int::%%assemble-value base-address sys.int::+tag-object+)))
                     (vector-push-extend fn current)
                     (incf offset)))
                (vector-push-extend current profile-entries)))
             (t (incf offset))))
    profile-entries))

(defun save-profile (path profile)
  "Convert a profile into an almost human-readable format."
  (with-open-file (s path :direction :output :if-exists :new-version :if-does-not-exist :create)
    (format s "#(~%")
    (loop
       for e across profile do
         (write e :stream s)
         (terpri s))
    (format s ")~%")))
