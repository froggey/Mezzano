;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; The high-level part of the profiler.
;;;; Takes raw samples and turns them into a more useful form.

(defpackage :mezzano.profiler
  (:use :cl)
  (:export #:decode-profile-buffer
           #:save-profile
           #:with-profiling
           #:with-allocation-profiling))

(in-package :mezzano.profiler)

(defparameter *ignorable-function-names*
  '(mezzano.runtime::%apply
    apply
    (lambda :in mezzano.clos::compute-1-effective-discriminator)
    (lambda :in mezzano.clos::std-compute-effective-method-function)
    (lambda :in mezzano.clos::compute-n-effective-discriminator)
    (lambda :in mezzano.clos::std-compute-effective-method-function-with-standard-method-combination)
    (lambda :in mezzano.clos::compute-primary-emfun)
    (mezzano.clos::1-effective-discriminator 0 1 nil)
    (mezzano.clos::1-effective-discriminator 0 1 t)
    (mezzano.clos::1-effective-discriminator 0 2 nil)
    (mezzano.clos::1-effective-discriminator 0 2 t)
    (mezzano.clos::1-effective-discriminator 0 3 nil)
    (mezzano.clos::1-effective-discriminator 0 3 t)
    (mezzano.clos::1-effective-discriminator 0 4 nil)
    (mezzano.clos::1-effective-discriminator 0 4 t)
    (mezzano.clos::1-effective-discriminator 0 5 nil)
    (mezzano.clos::1-effective-discriminator 0 5 t)
    sys.int::%progv
    sys.int::%catch
    mezzano.supervisor::call-with-snapshot-inhibited))

(defstruct thread-sample
  thread
  state
  wait-item
  call-stack)

(defmacro with-profiling ((&whole options &key buffer-size path thread verbosity prune ignore-functions repeat order-by) &body body)
  "Profile BODY.
:THREAD - Thread to sample.
          If NIL, then sample all threads.
          If T, then sample the current thread.
          Can be a specific thread to sample.
:BUFFER-SIZE - Size of the profiler's sample buffer.
:PATH - Path to write th profiler report to, if NIL then the samples will be returned.
:PRUNE - When :THREAD is T, try to prune away stack frames above the WITH-PROFILING call.
:IGNORE-FUNCTIONS - A list of function names to be removed from the call stack."
  `(call-with-profiling (lambda () ,@body) ,@options))

(defun call-with-profiling (function &key buffer-size path (thread t) (verbosity :report) (prune (eql thread 't)) (ignore-functions *ignorable-function-names*) repeat order-by)
  (let* ((profile-buffer nil)
         (results (unwind-protect
                       (progn
                         (mezzano.supervisor:start-profiling
                          :buffer-size buffer-size
                          :thread (if (eq thread t)
                                      (mezzano.supervisor:current-thread)
                                      thread))
                         (if repeat
                             (dotimes (i repeat)
                               (funcall function))
                             (multiple-value-list (funcall function))))
                    (setf profile-buffer (mezzano.supervisor:stop-profiling)))))
    (setf profile-buffer (decode-profile-buffer profile-buffer (if prune #'call-with-profiling nil) ignore-functions))
    (cond (path
           (save-profile path profile-buffer :verbosity verbosity :order-by order-by)
           (values-list results))
          (t profile-buffer))))

(defun decode-profile-buffer (buffer &optional prune-function ignore-functions)
  "Convert the buffer returned by STOP-PROFILING into a more useful format.
The returned value is a sequence of samples, and each sample is a sequence of
thread states & call-stacks."
  (let ((offset 0)
        (profile-entries (make-array 100 :adjustable t :fill-pointer 0)))
    ;; Due to wrapping, the start of the buffer might not be the start of a sample.
    ;; Skip forward to the first one, or the end of the buffer.
    (loop
       (when (or (>= offset (length buffer))
                 (eql (aref buffer offset) :start))
         (return))
       (incf offset))
    ;; Pull samples out.
    (flet ((next ()
             (if (< offset (length buffer))
                 (aref buffer offset)
                 nil))
           (consume ()
             (prog1
                 (aref buffer offset)
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
                    (call-stack (make-array 10 :adjustable t :fill-pointer 0))
                    (stop nil))
                (loop
                   (typecase (next)
                     ((eql :truncated)
                      (consume)
                      (vector-push-extend (cons :truncated 0) call-stack))
                     (function
                      ;; Called function and offset into.
                      (let ((fn (consume))
                            (offset (consume)))
                        (unless stop
                          (when (not (find (sys.int::function-name fn) ignore-functions
                                           :test #'equal))
                            (vector-push-extend (cons fn offset) call-stack))
                          (when (eql fn prune-function)
                            (setf stop t)))))
                     (t (return))))
                (vector-push-extend (make-thread-sample :thread thread
                                                        :state state
                                                        :wait-item wait-item
                                                        :call-stack call-stack)
                                    sample)))
           (vector-push-extend sample profile-entries))))
    profile-entries))

(defmacro with-allocation-profiling ((&whole options &key path verbosity prune ignore-functions) &body body)
  "Profile BODY.
:THREAD - Thread to sample.
          If NIL, then sample all threads.
          If T, then sample the current thread.
          Can be a specific thread to sample.
:BUFFER-SIZE - Size of the profiler's sample buffer.
:PATH - Path to write th profiler report to, if NIL then the samples will be returned.
:PRUNE - When :THREAD is T, try to prune away stack frames above the WITH-PROFILING call."
  `(call-with-allocation-profiling (lambda () ,@body) ,@options))

(defun call-with-allocation-profiling (function &key path (verbosity :report) (prune t) (ignore-functions *ignorable-function-names*))
  (let* ((raw-buffer (make-array 0 :adjustable t :fill-pointer 0))
         (results (let* ((mezzano.runtime::*allocation-profile* raw-buffer)
                         (mezzano.runtime::*enable-allocation-profiling* t))
                    (multiple-value-list (funcall function))))
         (profile-buffer (decode-profile-buffer raw-buffer (if prune #'call-with-allocation-profiling nil) ignore-functions)))
    (cond (path
           (save-profile path profile-buffer :verbosity verbosity)
           (values-list results))
          (t profile-buffer))))

(defun save-profile (path profile &key (verbosity :report) order-by)
  "Convert a profile into an almost human-readable format."
  (with-open-file (s path :direction :output :if-exists :new-version :if-does-not-exist :create)
    (when (member verbosity '(:report :full))
      (let ((*standard-output* s))
        (generate-report profile order-by))
    (when (eql verbosity :full)
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
                     (format s "  ~S + ~D~%" fn offset))))))))

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

(defclass profile-entry ()
  ((%function :initarg :function :reader profile-entry-function)
   (%callers :initarg :callers :reader profile-entry-callers)
   (%callees :initarg :callees :reader profile-entry-callees)
   (%direct-count :initarg :direct-count :accessor profile-entry-direct-count)
   (%total-count :initarg :total-count :accessor profile-entry-total-count))
  (:default-initargs
   :direct-count 0
    :total-count 0
    :callers (make-hash-table)
    :callees (make-hash-table)))

(defun generate-report (profile order-by)
  (let ((threads (make-hash-table)))
    (labels ((entry-for (thread function)
               (let ((thread-samples (gethash thread threads)))
                 (when (not thread-samples)
                   (setf thread-samples (make-hash-table)
                         (gethash thread threads) thread-samples))
                 (let ((entry (gethash function thread-samples)))
                   (when (not entry)
                     (setf entry (make-instance 'profile-entry :function function)
                           (gethash function thread-samples) entry))
                   entry)))
             (add-sample (thread function directp previous)
               (when (eql function 0)
                 ;(warn "Ignoring zero function. bug?")
                 (return-from add-sample))
               (when (eql previous 0)
                 (error "zero previous?"))
               (let* ((fn (car function))
                      (entry (entry-for thread fn)))
                 (cond (directp
                        (incf (profile-entry-direct-count entry)))
                       (t
                        (when previous
                          (let* ((prev-fn (car previous))
                                 (prev-entry (entry-for thread prev-fn)))
                            (incf (gethash prev-fn (profile-entry-callees entry) 0))
                            (incf (gethash fn (profile-entry-callers prev-entry) 0))))
                        (incf (profile-entry-total-count entry)))))))
      (loop for sample across profile do
           (loop for thread across sample do
                (loop
                   for prev = nil then entry
                   for entry across (thread-sample-call-stack thread)
                   do
                     (add-sample (thread-sample-thread thread)
                                 entry
                                 nil
                                 prev))
                (add-sample (thread-sample-thread thread)
                            (elt (thread-sample-call-stack thread) 0)
                            t
                            nil))))
    (maphash (lambda (thread sample-table)
               ;; Convert sample hash table to array of samples.
               (let ((samples (make-array (hash-table-count sample-table)
                                          :fill-pointer 0))
                     (total-total 0)
                     (total-direct 0))
                 (maphash (lambda (fn sample)
                            (declare (ignore fn))
                            (vector-push sample samples))
                          sample-table)
                 (setf samples (sort samples #'>
                                     :key (ecase order-by
                                            ((nil :total) #'profile-entry-total-count)
                                            ((:direct) #'profile-entry-direct-count))))
                 (loop
                    for s across samples
                    do
                      (incf total-total (profile-entry-total-count s))
                      (incf total-direct (profile-entry-direct-count s)))
                 ;; Print.
                 (format t "~S:~%" thread)
                 (loop
                    for s across samples
                    do (format t "  ~D ~D%~10T~D ~D%~20T~S~%"
                               (profile-entry-direct-count s) (* (/ (profile-entry-direct-count s) total-direct) 100.0)
                               (profile-entry-total-count s) (* (/ (profile-entry-total-count s) total-total) 100.0)
                               (profile-entry-function s)))
                 ;; Print the callers/callees table too.
                 (loop
                    for s across samples
                    do
                      (format t "  --------------------~%")
                      (maphash (lambda (caller count)
                                 (format t "    ~D~10T~S~%" count caller))
                               (profile-entry-callers s))
                      (format t "  ~S~10T~S~%" (profile-entry-total-count s) (profile-entry-function s))
                      (maphash (lambda (callee count)
                                 (format t "    ~D~10T~S~%" count callee))
                               (profile-entry-callees s)))))
             threads)))
