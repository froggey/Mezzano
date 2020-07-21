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
    (mezzano.clos::1-effective-discriminator 0 1 nil nil)
    (mezzano.clos::1-effective-discriminator 0 1 t nil)
    (mezzano.clos::1-effective-discriminator 0 2 nil nil)
    (mezzano.clos::1-effective-discriminator 0 2 t nil)
    (mezzano.clos::1-effective-discriminator 0 3 nil nil)
    (mezzano.clos::1-effective-discriminator 0 3 t nil)
    (mezzano.clos::1-effective-discriminator 0 4 nil nil)
    (mezzano.clos::1-effective-discriminator 0 4 t nil)
    (mezzano.clos::1-effective-discriminator 0 5 nil nil)
    (mezzano.clos::1-effective-discriminator 0 5 t nil)
    (mezzano.clos::1-effective-discriminator 1 1 nil nil)
    (mezzano.clos::1-effective-discriminator 1 1 t nil)
    (mezzano.clos::1-effective-discriminator 1 2 nil nil)
    (mezzano.clos::1-effective-discriminator 1 2 t nil)
    (mezzano.clos::1-effective-discriminator 1 3 nil nil)
    (mezzano.clos::1-effective-discriminator 1 3 t nil)
    (mezzano.clos::1-effective-discriminator 1 4 nil nil)
    (mezzano.clos::1-effective-discriminator 1 4 t nil)
    (mezzano.clos::1-effective-discriminator 1 5 nil nil)
    (mezzano.clos::1-effective-discriminator 1 5 t nil)
    (mezzano.clos::1-effective-discriminator 2 1 nil nil)
    (mezzano.clos::1-effective-discriminator 2 1 t nil)
    (mezzano.clos::1-effective-discriminator 2 2 nil nil)
    (mezzano.clos::1-effective-discriminator 2 2 t nil)
    (mezzano.clos::1-effective-discriminator 2 3 nil nil)
    (mezzano.clos::1-effective-discriminator 2 3 t nil)
    (mezzano.clos::1-effective-discriminator 2 4 nil nil)
    (mezzano.clos::1-effective-discriminator 2 4 t nil)
    (mezzano.clos::1-effective-discriminator 2 5 nil nil)
    (mezzano.clos::1-effective-discriminator 2 5 t nil)
    (mezzano.clos::1-effective-discriminator 3 1 nil nil)
    (mezzano.clos::1-effective-discriminator 3 1 t nil)
    (mezzano.clos::1-effective-discriminator 3 2 nil nil)
    (mezzano.clos::1-effective-discriminator 3 2 t nil)
    (mezzano.clos::1-effective-discriminator 3 3 nil nil)
    (mezzano.clos::1-effective-discriminator 3 3 t nil)
    (mezzano.clos::1-effective-discriminator 3 4 nil nil)
    (mezzano.clos::1-effective-discriminator 3 4 t nil)
    (mezzano.clos::1-effective-discriminator 3 5 nil nil)
    (mezzano.clos::1-effective-discriminator 3 5 t nil)
    (mezzano.clos::1-effective-discriminator 4 1 nil nil)
    (mezzano.clos::1-effective-discriminator 4 1 t nil)
    (mezzano.clos::1-effective-discriminator 4 2 nil nil)
    (mezzano.clos::1-effective-discriminator 4 2 t nil)
    (mezzano.clos::1-effective-discriminator 4 3 nil nil)
    (mezzano.clos::1-effective-discriminator 4 3 t nil)
    (mezzano.clos::1-effective-discriminator 4 4 nil nil)
    (mezzano.clos::1-effective-discriminator 4 4 t nil)
    (mezzano.clos::1-effective-discriminator 4 5 nil nil)
    (mezzano.clos::1-effective-discriminator 4 5 t nil)
    (mezzano.clos::1-effective-discriminator 0 1 nil t)
    (mezzano.clos::1-effective-discriminator 0 1 t t)
    (mezzano.clos::1-effective-discriminator 0 2 nil t)
    (mezzano.clos::1-effective-discriminator 0 2 t t)
    (mezzano.clos::1-effective-discriminator 0 3 nil t)
    (mezzano.clos::1-effective-discriminator 0 3 t t)
    (mezzano.clos::1-effective-discriminator 0 4 nil t)
    (mezzano.clos::1-effective-discriminator 0 4 t t)
    (mezzano.clos::1-effective-discriminator 0 5 nil t)
    (mezzano.clos::1-effective-discriminator 0 5 t t)
    (mezzano.clos::1-effective-discriminator 1 1 nil t)
    (mezzano.clos::1-effective-discriminator 1 1 t t)
    (mezzano.clos::1-effective-discriminator 1 2 nil t)
    (mezzano.clos::1-effective-discriminator 1 2 t t)
    (mezzano.clos::1-effective-discriminator 1 3 nil t)
    (mezzano.clos::1-effective-discriminator 1 3 t t)
    (mezzano.clos::1-effective-discriminator 1 4 nil t)
    (mezzano.clos::1-effective-discriminator 1 4 t t)
    (mezzano.clos::1-effective-discriminator 1 5 nil t)
    (mezzano.clos::1-effective-discriminator 1 5 t t)
    (mezzano.clos::1-effective-discriminator 2 1 nil t)
    (mezzano.clos::1-effective-discriminator 2 1 t t)
    (mezzano.clos::1-effective-discriminator 2 2 nil t)
    (mezzano.clos::1-effective-discriminator 2 2 t t)
    (mezzano.clos::1-effective-discriminator 2 3 nil t)
    (mezzano.clos::1-effective-discriminator 2 3 t t)
    (mezzano.clos::1-effective-discriminator 2 4 nil t)
    (mezzano.clos::1-effective-discriminator 2 4 t t)
    (mezzano.clos::1-effective-discriminator 2 5 nil t)
    (mezzano.clos::1-effective-discriminator 2 5 t t)
    (mezzano.clos::1-effective-discriminator 3 1 nil t)
    (mezzano.clos::1-effective-discriminator 3 1 t t)
    (mezzano.clos::1-effective-discriminator 3 2 nil t)
    (mezzano.clos::1-effective-discriminator 3 2 t t)
    (mezzano.clos::1-effective-discriminator 3 3 nil t)
    (mezzano.clos::1-effective-discriminator 3 3 t t)
    (mezzano.clos::1-effective-discriminator 3 4 nil t)
    (mezzano.clos::1-effective-discriminator 3 4 t t)
    (mezzano.clos::1-effective-discriminator 3 5 nil t)
    (mezzano.clos::1-effective-discriminator 3 5 t t)
    (mezzano.clos::1-effective-discriminator 4 1 nil t)
    (mezzano.clos::1-effective-discriminator 4 1 t t)
    (mezzano.clos::1-effective-discriminator 4 2 nil t)
    (mezzano.clos::1-effective-discriminator 4 2 t t)
    (mezzano.clos::1-effective-discriminator 4 3 nil t)
    (mezzano.clos::1-effective-discriminator 4 3 t t)
    (mezzano.clos::1-effective-discriminator 4 4 nil t)
    (mezzano.clos::1-effective-discriminator 4 4 t t)
    (mezzano.clos::1-effective-discriminator 4 5 nil t)
    (mezzano.clos::1-effective-discriminator 4 5 t t)
    mezzano.internals::%progv
    mezzano.internals::%catch
    mezzano.supervisor::call-with-snapshot-inhibited
    mezzano.compiler::call-with-metering
    mezzano.supervisor::call-with-mutex
    mezzano.internals::%%funcallable-instance-trampoline%%
    mezzano.runtime::%allocate-object))

(defclass profile-data ()
  ((%data :initarg :data :reader profile-data)))

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
  (declare (ignore buffer-size path thread verbosity prune ignore-functions repeat order-by))
  `(call-with-profiling (lambda () ,@body) ,@options))

(defun call-with-profiling (function &key buffer-size path (thread t) (verbosity :report) (prune (eql thread 't)) (ignore-functions *ignorable-function-names*) repeat order-by)
  (let* ((profile-buffer nil)
         (results (cond (repeat
                         (let ((profiles '()))
                           (dotimes (i repeat)
                             ;; The buffer is fixed size and can only be so large due to wired
                             ;; area limits. Restart the profiler each run and combine the
                             ;; results after the fact to make better use of it.
                             (unwind-protect
                                  (progn
                                    (mezzano.supervisor:start-profiling
                                     :buffer-size buffer-size
                                     :thread (if (eq thread t)
                                                 (mezzano.supervisor:current-thread)
                                                 thread)
                                     ;; Disable sampling during GC when tracing a specific thread.
                                     :sample-during-gc (not thread))
                                    (funcall function))
                               (push (decode-profile-buffer (mezzano.supervisor:stop-profiling)
                                                            (if prune #'call-with-profiling nil)
                                                            ignore-functions)
                                     profiles)))
                           (setf profile-buffer (reduce #'(lambda (x y) (concatenate 'vector x y))
                                                        (reverse profiles)
                                                        :initial-value #()))
                           '()))
                        (t
                         (multiple-value-list
                          (unwind-protect
                               (progn
                                 (mezzano.supervisor:start-profiling
                                  :buffer-size buffer-size
                                  :thread (if (eq thread t)
                                              (mezzano.supervisor:current-thread)
                                              thread)
                                  ;; Disable sampling during GC when tracing a specific thread.
                                  :sample-during-gc (not thread))
                                 (funcall function))
                            (setf profile-buffer (decode-profile-buffer (mezzano.supervisor:stop-profiling)
                                                                        (if prune #'call-with-profiling nil)
                                                                        ignore-functions))))))))
    (let ((data (make-instance 'profile-data :data profile-buffer)))
      (cond (path
             (save-profile path data :verbosity verbosity :order-by order-by)
             (values-list results))
            (t
             data)))))

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
         (incf offset) ; Skip past timestamp
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
                          (when (not (find (mezzano.internals::function-name fn) ignore-functions
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

(defmacro with-allocation-profiling ((&whole options &key repeat) &body body)
  "Profile BODY."
  (declare (ignore repeat))
  `(call-with-allocation-profiling (lambda () ,@body) ,@options))

(defun log-allocation-profile-entry (buffer words)
  (let ((first nil)
        (tree '()))
    (block nil
      (mezzano.internals::%map-backtrace
       (lambda (i fp)
         (let* ((fn (mezzano.internals::function-from-frame (list nil fp nil))))
           (when (eql i 3)
             (setf first fn))
           (when (eql fn #'call-with-allocation-profiling)
             (return))
           (when (> i 3)
             (push fn tree))))))
    (dolist (fn tree)
      (let ((p (position fn buffer)))
        (cond (p
               (setf buffer (aref buffer (+ p 2))))
              (t
               (let ((new (make-array 3 :adjustable t :fill-pointer 0)))
                 (vector-push-extend fn buffer)
                 (vector-push-extend 0 buffer)
                 (vector-push-extend new buffer)
                 (setf buffer new))))))
    (let ((p (position first buffer)))
      (cond (p
             (incf (aref buffer (+ p 1)) words))
            (t
             (let ((new (make-array 3 :adjustable t :fill-pointer 0)))
               (vector-push-extend first buffer)
               (vector-push-extend words buffer)
               (vector-push-extend new buffer)))))))

(defun name-for-flame-graph (fn)
  (let ((*print-pretty* nil))
    (substitute #\_ #\Space
                (substitute #\L #\<
                            (substitute #\G #\>
                                        (format nil "~(~S~)" (mezzano.internals::function-name fn)))))))

(defun generate-allocation-flame-graph (profile stream threshold)
  (labels ((frob (level stack)
             (loop
                for i from 0 below (length level) by 3
                for fn = (aref level i)
                for count = (aref level (+ i 1))
                for next = (aref level (+ i 2))
                for name = (name-for-flame-graph fn)
                for stack2 = (list* name stack)
                for first = t
                do
                  (when (>= count threshold)
                    (dolist (entry (nreverse stack2))
                      (cond (first (setf first nil))
                            (t (write-char #\; stream)))
                      (write-string entry stream))
                    (write-char #\Space stream)
                    (write count :stream stream)
                    (terpri stream))
                  (frob next stack2))))
    (frob profile '()))
  profile)

(defun call-with-allocation-profiling (function &key repeat)
  (let ((raw-buffer (make-array 3 :adjustable t :fill-pointer 0)))
    (unwind-protect
         (let ((mezzano.runtime::*allocation-profile-hook*
                (lambda (words) (log-allocation-profile-entry raw-buffer words))))
           (setf mezzano.runtime::*enable-allocation-profiling* t)
           (dotimes (i (or repeat 1))
             (funcall function)))
      (setf mezzano.runtime::*enable-allocation-profiling* nil))
    ;; TODO: Unify format with normal profiling.
    raw-buffer))

(defun save-profile (path profile &key (verbosity :report) order-by)
  "Convert a profile into an almost human-readable format."
  (with-open-file (s path :direction :output :if-exists :new-version :if-does-not-exist :create)
    (cond ((eql verbosity :flame-graph)
           (generate-flame-graph profile s))
          (t
           (format s "Version ~A~%" (lisp-implementation-version))
           (when (member verbosity '(:report :full))
             (generate-report profile order-by s))
           (when (eql verbosity :full)
             (loop
                for sample across (profile-data profile) do
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
  profile)

(defstruct tree-branch
  value
  (count 0)
  (branches (make-hash-table)))

(defun add-sample (current-branch call-stack &optional (position (length call-stack)))
  (cond ((zerop position)
         (incf (tree-branch-count current-branch)))
        (t
         (let* ((current (elt call-stack (1- position)))
                (fn (car current)))
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

(defun generate-report (profile order-by stream)
  (setf profile (profile-data profile))
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
                 (format stream "~S:~%" thread)
                 (loop
                    for s across samples
                    do (format stream "  ~D ~D%~15T~D ~D%~30T~S~%"
                               (profile-entry-direct-count s) (* (/ (profile-entry-direct-count s) total-direct) 100.0)
                               (profile-entry-total-count s) (* (/ (profile-entry-total-count s) total-total) 100.0)
                               (profile-entry-function s)))
                 ;; Print the callers/callees table too.
                 (loop
                    for s across samples
                    do
                      (format stream "  --------------------~%")
                      (let ((callers (make-array 10 :adjustable t :fill-pointer 0)))
                        (maphash (lambda (caller count)
                                   (vector-push-extend (cons caller count) callers))
                                 (profile-entry-callers s))
                        (setf callers (sort callers #'> :key #'cdr))
                        (loop for (caller . count) across callers do
                             (format stream "    ~D~10T~S~%" count caller)))
                      (format stream "  ~S~10T~S~%" (profile-entry-total-count s) (profile-entry-function s))
                      (let ((callees (make-array 10 :adjustable t :fill-pointer 0)))
                        (maphash (lambda (callee count)
                                   (vector-push-extend (cons callee count) callees))
                                 (profile-entry-callees s))
                        (setf callees (sort callees #'> :key #'cdr))
                        (loop for (callee . count) across callees do
                             (format stream "    ~D~10T~S~%" count callee))))))
             threads)))

(defun generate-flame-graph (profile stream)
  (loop
     for sample across (profile-data profile)
     do
       (when (not (zerop (length sample)))
         (loop
            with first = t
            for (fn . offset) across (reverse (thread-sample-call-stack (elt sample 0)))
            do
              (when (not (eql fn 0))
                (cond (first (setf first nil))
                      (t (write-char #\; stream)))
                (let ((*print-pretty* nil))
                  (write-string (name-for-flame-graph fn) stream))))
         (write-char #\Space stream)
         (write 1 :stream stream)
         (terpri stream)))
  profile)
