;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; The low-level part of the profiler.
;;;; Deals with setting up/tearing down the profile buffer and actually
;;;; taking samples.

(in-package :mezzano.supervisor)

(sys.int::defglobal *enable-profiling* nil)
(sys.int::defglobal *profile-thread* nil
  "When non-nil, only this thread will be profiled.")

;; The profile buffer must be a wired simple-vector.
;; It is treated as a circular buffer, when it fill up newer entries
;; will replace older entries.
(sys.int::defglobal *profile-buffer*)
(sys.int::defglobal *profile-buffer-head*)
(sys.int::defglobal *profile-buffer-tail*)

(sys.int::defglobal *default-profile-buffer-size* (* 1024 1024))

(defun profile-append-entry (thing)
  "Append one THING to the profile buffer, wrapping around as required."
  (let ((buffer-len (sys.int::%object-header-data *profile-buffer*))
        (place *profile-buffer-head*))
    (incf *profile-buffer-head*)
    (when (eql *profile-buffer-head* buffer-len)
      (setf *profile-buffer-head* 0))
    (when (eql *profile-buffer-head* *profile-buffer-tail*)
      (incf *profile-buffer-tail*)
      (when (eql *profile-buffer-tail* buffer-len)
        (setf *profile-buffer-tail* 0)))
    (setf (svref *profile-buffer* place) thing)))

(defun profile-append-return-address (addr)
  "Append a return address to the profile buffer as a function + offset."
  (let* ((fn (with-page-fault-hook
                 (()
                  (profile-append-entry addr)
                  (profile-append-entry 0)
                  (return-from profile-append-return-address))
               ;; The function might be unmapped and in the pinned area, so it's possible
               ;; that this can fault.
               (sys.int::return-address-to-function addr)))
         (fn-address (logand (sys.int::lisp-object-address fn) -16))
         (offset (- addr fn-address)))
    (profile-append-entry fn)
    (profile-append-entry offset)))

(defun profile-append-call-stack (initial-frame-pointer)
  "Append a complete-as-possible call stack to the profile buffer."
  ;; If a thread's stack is not mapped in, then this may take a page-fault.
  ;; Produce a truncated sample if that happens.
  (with-page-fault-hook
      (()
       (profile-append-entry :truncated)
       (return-from profile-append-call-stack))
    (do ((fp initial-frame-pointer
             (sys.int::memref-unsigned-byte-64 fp 0)))
        ((eql fp 0))
      (let ((return-address (sys.int::memref-unsigned-byte-64 fp 1)))
        (when (zerop return-address)
          (return))
        (profile-append-return-address return-address)))))

;;; Watch out, this runs in an interrupt handler.
(defun profile-sample (interrupt-frame)
  (when (and (boundp '*enable-profiling*)
             *enable-profiling*)
    (profile-append-entry :start)
    (when (or (not *profile-thread*)
              (eql *profile-thread* (current-thread)))
      ;; Dump the current thread.
      (profile-append-entry (current-thread))
      (profile-append-entry :active)
      (profile-append-entry (thread-wait-item (current-thread)))
      (profile-append-return-address (interrupt-frame-raw-register interrupt-frame :rip))
      (profile-append-call-stack (interrupt-frame-raw-register interrupt-frame :rbp)))
    ;; And all the others.
    (cond (*profile-thread*
           (let ((thread *profile-thread*))
             (when (not (eql thread (current-thread)))
               (profile-append-entry thread)
               (profile-append-entry (thread-state thread))
               (profile-append-entry (thread-wait-item thread))
               (when (thread-full-save-p thread)
                 ;; RIP is valid in the save area.
                 (profile-append-return-address (thread-state-rip thread)))
               (profile-append-call-stack (thread-frame-pointer thread)))))
          (t
           (loop
              for thread = *all-threads* then (thread-global-next thread)
              until (not thread) do
                (when (not (eql thread (current-thread)))
                  (profile-append-entry thread)
                  (profile-append-entry (thread-state thread))
                  (profile-append-entry (thread-wait-item thread))
                  (when (thread-full-save-p thread)
                    ;; RIP is valid in the save area.
                    (profile-append-return-address (thread-state-rip thread)))
                  (profile-append-call-stack (thread-frame-pointer thread))))))))

(defun start-profiling (&key buffer-size thread (reset t))
  "Set up a profile sample buffer and enable sampling."
  (assert (not *enable-profiling*) ()
          "Profiling already started.")
  (setf buffer-size (or buffer-size *default-profile-buffer-size*))
  (when (or (not (boundp '*profile-buffer*))
            (not *profile-buffer*)
            (not (eql (length *profile-buffer*) buffer-size)))
    (setf reset t)
    (setf *profile-buffer* (make-array buffer-size :area :wired)))
  (when reset
    (setf *profile-buffer-head* 0
          *profile-buffer-tail* 0))
  (setf *profile-thread* thread)
  (setf *enable-profiling* t))

(defun stop-profiling ()
  "Stop sampling and retrieve the sample buffer."
  (assert *enable-profiling* ()
          "Profiling not started.")
  (setf *enable-profiling* nil)
  (setf *profile-thread* nil)
  (let* ((n-entries (if (< *profile-buffer-head* *profile-buffer-tail*)
                        (+ (- (length *profile-buffer*) *profile-buffer-tail*)
                           *profile-buffer-head*)
                        *profile-buffer-head*))
         (buffer (make-array n-entries)))
    (cond ((< *profile-buffer-head* *profile-buffer-tail*)
           (replace buffer *profile-buffer*
                    :start2 *profile-buffer-tail*)
           (replace buffer *profile-buffer*
                    :start1 (- (length *profile-buffer*) *profile-buffer-tail*)
                    :end2 *profile-buffer-head*))
          (t (replace buffer *profile-buffer*
                    :end2 *profile-buffer-head*)))
    buffer))
