;;;; Copyright (c) 2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; The high-level part of the profiler.
;;;; Takes raw samples and turns them into a more useful form.

(defpackage :mezzano.profiler
  (:use :cl)
  (:export #:decode-profile-buffer
           #:save-profile))

(in-package :mezzano.profiler)

(defstruct thread-sample
  thread
  state
  wait-item
  call-stack)

(defun decode-profile-buffer (buffer)
  "Convert the buffer returned by STOP-PROFILING into a more useful format.
The returned value is a sequence of samples, and each sample is a sequence of
thread states & call-stacks."
  (let ((offset 0)
        (profile-entries (make-array 100 :adjustable t :fill-pointer 0)))
    ;; Due to wrapping, the start of the buffer might not be the start of a sample.
    ;; Skip forward to the first one, or the end of the buffer.
    (loop
       (when (or (>= offset (length buffer))
                 (eql (svref buffer offset) :start))
         (return))
       (incf offset))
    ;; Pull samples out.
    (flet ((next ()
             (if (< offset (length buffer))
                 (svref buffer offset)
                 nil))
           (consume ()
             (prog1
                 (svref buffer offset)
               (incf offset))))
      (loop
         (when (>= offset (length buffer))
           (return))
         (incf offset) ; Skip past :START
         (let ((sample (make-array 10 :adjustable t :fill-pointer 0)))
           (loop
              (when (not (mezzano.supervisor:threadp (next)))
                (return))
              (let ((thread (consume)) ; thread
                    (state (consume)) ; state
                    (wait-item (consume)) ; wait-item
                    (call-stack (make-array 10 :adjustable t :fill-pointer 0)))
                (loop
                   (typecase (next)
                     ((eql :truncated)
                      (consume)
                      (vector-push-extend (cons :truncated 0) call-stack))
                     (function
                      ;; Called function and offset into.
                      (vector-push-extend (cons (consume) (consume)) call-stack))
                     (t (return))))
                (vector-push-extend (make-thread-sample :thread thread
                                                        :state state
                                                        :wait-item wait-item
                                                        :call-stack call-stack)
                                    sample)))
           (vector-push-extend sample profile-entries))))
    profile-entries))

(defun save-profile (path profile)
  "Convert a profile into an almost human-readable format."
  (with-open-file (s path :direction :output :if-exists :new-version :if-does-not-exist :create)
    (loop
       for sample across profile do
         (format s "------------------------~%")
         (loop
            for thread across sample do
              (format s "Thread ~S~%" (thread-sample-thread thread))
              (format s " State ~S~%" (thread-sample-state thread))
              (format s " Wait-item ~S~%" (thread-sample-wait-item thread))
              (format s " Call-stack:~%")
              (loop
                 for (fn . offset) across (thread-sample-call-stack thread) do
                   (format s "  ~S + ~D~%" fn offset))))))

(defstruct tree-branch
  value
  (count 0)
  (branches (make-hash-table)))

(defun add-sample (current-branch call-stack &optional (position (length call-stack)))
  (cond ((zerop position)
         (incf (tree-branch-count current-branch)))
        (t
         (let* ((current (elt call-stack (1- position)))
                (fn (car current))
                (offset (cdr current)))
           ;; Ignore truncated or unmapped samples.
           (when (functionp fn)
             (let ((branch (gethash fn (tree-branch-branches current-branch))))
               (when (not branch)
                 (setf branch (make-tree-branch :value fn)
                       (gethash fn (tree-branch-branches current-branch)) branch))
               (add-sample branch call-stack (1- position))))))))

(defun treeify-profile (profile)
  (loop
     with threads = (make-hash-table)
     for sample across profile do
       (loop
          for thread across sample do
            (when (not (gethash (thread-sample-thread thread) threads))
              (setf (gethash (thread-sample-thread thread) threads)
                    (make-tree-branch :value (thread-sample-thread thread))))
            (let ((root (gethash (thread-sample-thread thread) threads)))
              (add-sample root (thread-sample-call-stack thread))))
     finally (return threads)))

(defun write-tree-1 (tree &optional (depth 0))
  (loop repeat depth do (write-char #\Space))
  (format t "~D ~S~%" (tree-branch-count tree) (tree-branch-value tree))
  (maphash (lambda (k branch)
             (declare (ignore k))
             (write-tree-1 branch (1+ depth)))
           (tree-branch-branches tree)))


(defun write-tree (tree &optional (depth 0))
  (maphash (lambda (k branch)
             (declare (ignore k))
             (write-tree-1 branch (1+ depth)))
           tree))
