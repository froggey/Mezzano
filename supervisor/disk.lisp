;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; External API:
;;; ALL-DISKS - Return a list of all disks in the systems. List might not be fresh.
;;; DISK-WRITABLE-P - Return true if a disk is writable.
;;; DISK-N-SECTORS - Number of sectors on the disk.
;;; DISK-SECTOR-SIZE - Size of a disk sector in octets.
;;; DISK-READ - Read sectors.
;;; DISK-WRITE - Write sectors.
;;; MAKE-DISK-REQUEST - Allocate a new request object for async operations.
;;; DISK-SUBMIT-REQUEST - Submit an async request.
;;; DISK-CANCEL-REQUEST - Abort an in-progress request.
;;; DISK-AWAIT-REQUEST - Wait for a request to complete.
;;; DISK-REQUEST-COMPLETE-P - Returns true if the request is not in-progress.

(sys.int::defglobal *log-disk-requests* nil)

(defstruct (disk
             (:area :wired))
  device
  writable-p
  n-sectors
  sector-size
  max-transfer
  read-fn
  write-fn
  (valid t))

(defstruct (partition
             (:area :wired))
  disk
  offset
  id)

(defstruct (disk-request
             (:constructor make-disk-request ())
             (:area :wired))
  (state :error :type (member :setup :waiting :in-progress :complete :error))
  (error-reason :uninitialized)
  disk
  direction
  lba
  n-sectors
  buffer
  (lock (place-spinlock-initializer))
  (latch (make-latch "Disk request notifier"))
  next)

(sys.int::defglobal *disks*)

(sys.int::defglobal *disk-request-current*)
(sys.int::defglobal *disk-request-queue-head*)
(sys.int::defglobal *disk-request-queue-lock*)
(sys.int::defglobal *disk-request-queue-latch*)

(defun all-disks ()
  ;; Would be nice to copy the list here, but this is called before the paging disk
  ;; is found, so no allocation outside the wired area.
  *disks*)

(defun register-disk (device writable-p n-sectors sector-size max-transfer read-fn write-fn)
  (when (> sector-size +4k-page-size+)
    (debug-print-line "Ignoring device " device " with overly-large sector size " sector-size " (should be <= than 4k).")
    (return-from register-disk))
  (let ((disk (make-disk :device device
                         :writable-p writable-p
                         :sector-size sector-size
                         :n-sectors n-sectors
                         :max-transfer max-transfer
                         :read-fn read-fn
                         :write-fn write-fn)))
    (debug-print-line "Registered new " (if writable-p "R/W" "R/O") " disk " disk " sectors:" n-sectors)
    (setf *disks* (sys.int::cons-in-area disk *disks* :wired))))

(defun initialize-disk ()
  (when (not (boundp '*disk-request-queue-head*))
    (setf *log-disk-requests* nil)
    (setf *disk-request-current* nil
          *disk-request-queue-head* nil
          *disk-request-queue-lock* (place-spinlock-initializer)
          *disk-request-queue-latch* (make-latch "Disk request queue notifier")
          *disks* '()))
  ;; Abort any queued or in-progress requests.
  (do ((request *disk-request-queue-head* (disk-request-next request)))
      ((null request))
    (setf (disk-request-state request) :error
          (disk-request-error-reason request) :system-reinitialized)
    (latch-trigger (disk-request-latch request)))
  (setf *disk-request-queue-head* nil)
  (when *disk-request-current*
    (setf (disk-request-state *disk-request-current*) :error
          (disk-request-error-reason *disk-request-current*) :system-reinitialized)
    (latch-trigger (disk-request-latch *disk-request-current*))
    (setf *disk-request-current* nil))
  ;; Clear the disk list.
  (dolist (disk *disks*)
    (setf (disk-valid disk) nil))
  (setf *disks* '()))

(defun pop-disk-request ()
  (loop
     (without-interrupts
       (with-symbol-spinlock (*disk-request-queue-lock*)
         (let ((req *disk-request-queue-head*))
           (when req
             (with-place-spinlock ((disk-request-lock req))
               (setf (disk-request-state req) :in-progress
                     *disk-request-current* req)
               (setf *disk-request-queue-head* (disk-request-next req)))
             (return req)))
         (latch-reset *disk-request-queue-latch*)))
     (latch-wait *disk-request-queue-latch*)))

(defun process-one-disk-request (request)
  (let* ((disk (disk-request-disk request))
         (buffer (disk-request-buffer request))
         (direction (disk-request-direction request))
         (bounce-buffer nil)
         (real-buffer nil))
    (when *log-disk-requests*
      (debug-print-line "Disk " (if (eql direction :read) "read" "write")
                        " on " disk
                        " " (disk-request-lba request)
                        " " (disk-request-n-sectors request)
                        " " buffer))
    (when (and (not (disk-writable-p disk))
               (eql direction :write))
       (return-from process-one-disk-request
         (values nil "Write to read-only disk.")))
    (case direction
      (:read (set-disk-read-light t))
      (:write (set-disk-write-light t)))
    (cond
      ((sys.int::fixnump buffer)
       (setf real-buffer buffer))
      ;; Must be a wired simple ub8 vector.
      ;; Be very careful when checking the type here.
      ;; Check that it is an object first, then wired,
      ;; then the exact type.
      ;; This stops the disk-thread from touching memory that might not be
      ;; wired.
      ((and (sys.int::%value-has-tag-p buffer sys.int::+tag-object+)
            (< (sys.int::lisp-object-address buffer) (* 2 1024 1024 1024))
            (eql (sys.int::%object-tag buffer) sys.int::+object-tag-array-unsigned-byte-8+))
       ;; Reading or writing into an array, allocate a bounce buffer.
       ;; TODO: Do this without the bounce buffer.
       (setf bounce-buffer (allocate-physical-pages
                            (ceiling (* (disk-request-n-sectors request)
                                        (disk-sector-size disk))
                                     +4k-page-size+)))
       (when (not bounce-buffer)
         (return-from process-one-disk-request
           (values nil "Unable to allocate disk bounce buffer.")))
       (setf real-buffer (convert-to-pmap-address (* bounce-buffer +4k-page-size+)))
       (when (eql direction :write)
         (dotimes (i (sys.int::%object-header-data buffer))
           (setf (sys.int::memref-unsigned-byte-8 real-buffer i)
                 (sys.int::%object-ref-unsigned-byte-8 buffer i)))))
      (t ;; Weird buffer type. Give up.
       (return-from process-one-disk-request
         (values nil "Bad disk buffer type."))))
    ;; Execute.
    (multiple-value-bind (successp error)
        (funcall (case direction
                   (:read (disk-read-fn disk))
                   (:write (disk-write-fn disk)))
                 (disk-device disk)
                 (disk-request-lba request)
                 ;; FIXME: Deal with n-sectors > disk max-sectors.
                 (disk-request-n-sectors request)
                 real-buffer)
      (when bounce-buffer
        (when (and successp
                   (eql direction :read))
          (dotimes (i (sys.int::%object-header-data buffer))
            (setf (sys.int::%object-ref-unsigned-byte-8 buffer i)
                  (sys.int::memref-unsigned-byte-8 real-buffer i))))
        (release-physical-pages bounce-buffer
                                (ceiling (* (disk-request-n-sectors request)
                                            (disk-sector-size disk))
                                         +4k-page-size+)))
      (case direction
        (:read (set-disk-read-light nil))
        (:write (set-disk-write-light nil)))
      (values successp error))))

(defun disk-thread ()
  (loop
     (let ((request (pop-disk-request)))
       (multiple-value-bind (successp reason)
           (process-one-disk-request request)
         (without-interrupts
           (with-place-spinlock ((disk-request-lock request))
             (cond (successp
                    (setf (disk-request-state request) :complete
                          (disk-request-error-reason request) nil))
                   (t
                    (setf (disk-request-state request) :error
                          (disk-request-error-reason request) reason)))
             (setf *disk-request-current* nil)
             (latch-trigger (disk-request-latch request))))))))

(defun disk-read (disk lba n-sectors buffer)
  "Synchronously read N-SECTORS sectors of data to BUFFER from DISK at sector offset LBA.
Returns true on success; false and an error on failure."
  (let ((request (make-disk-request)))
    (disk-submit-request request disk :read lba n-sectors buffer)
    (disk-await-request request)))

(defun disk-write (disk lba n-sectors buffer)
  "Synchronously write N-SECTORS sectors of data from BUFFER to DISK at sector offset LBA.
Returns true on success; false and an error on failure."
  (let ((request (make-disk-request)))
    (disk-submit-request request disk :write lba n-sectors buffer)
    (disk-await-request request)))

(defun disk-submit-request-1 (request disk direction lba n-sectors buffer)
  "Like DISK-SUBMIT-REQUEST but does not signal if the request is not complete.
Returns true on success; false on failure."
  ;; Wonder if these should fail async...
  (check-type direction (member :read :write))
  (assert (<= 0 lba (+ lba n-sectors) (disk-n-sectors disk)))
  ;; This terrible nonsense is because SAFE-WITHOUT-INTERRUPTS
  ;; can't pass more than 3 arguments to the lambda.
  ;; Check if the request has already been submitted.
  (when (safe-without-interrupts (request)
          (with-place-spinlock ((disk-request-lock request))
            (let ((state (disk-request-state request)))
              (case state
                ((:in-progress :waiting :setup)
                 t)
                (nil
                 ;; Transition to setup phase. This effectively locks the request,
                 ;; preventing anything else from modifying it.
                 (setf (disk-request-state request) :setup)
                 nil)))))
    ;; Request is already active.
    (return-from disk-submit-request-1 nil))
  ;; Setup.
  ;; Safe to do with interrupts enabled because the request is in :SETUP.
  (setf (disk-request-disk request) disk
        (disk-request-direction request) direction
        (disk-request-lba request) lba
        (disk-request-n-sectors request) n-sectors
        (disk-request-buffer request) buffer)
  ;; Now submit.
  (safe-without-interrupts (request disk)
    (with-symbol-spinlock (*disk-request-queue-lock*)
      (with-place-spinlock ((disk-request-lock request))
        (setf (disk-request-state request) :waiting)
        (latch-reset (disk-request-latch request))
        (cond ((disk-valid disk)
               ;; Attach to request list.
               (setf (disk-request-next request) *disk-request-queue-head*
                     *disk-request-queue-head* request)
               ;; Wake the disk thread.
               (latch-trigger *disk-request-queue-latch*))
              (t
               (latch-trigger (disk-request-latch request))
               (setf (disk-request-state request) :error
                     (disk-request-error-reason request) :system-reinitialized))))))
  ;; All done.
  t)

(defun disk-submit-request (request disk direction lba n-sectors buffer)
  "Submit a request to the disk. Safe to perform at any time.
DIRECTION is either :READ or :WRITE.
Signals an error if REQUEST is already queued or in-progress.
Resubmitting a completed or failed request treats the request object as though it had
been freshly allocated."
  ;; Avoid signalling with the lock held.
  (when (not (disk-submit-request-1 request disk direction lba n-sectors buffer))
    (error "Request already in progress.")))

(defun disk-cancel-request (request &optional (reason :cancelled))
  "Cancel a disk request.
If the request has completed or is freshly allocated, then it is not cancelled.
An in-progress request may be cancelled, or it may complete."
  (without-interrupts
    (with-symbol-spinlock (*disk-request-queue-lock*)
      (with-place-spinlock ((disk-request-lock request))
        ;; Easy case, just remove it from the queue.
        ;; Cancelling an in-progress request is trickier.
        (when (eql (disk-request-state request) :waiting)
          (cond ((eql request *disk-request-queue-head*)
                 (setf *disk-request-queue-head* (disk-request-next request)))
                (t (do ((prev *disk-request-queue-head* (disk-request-next prev)))
                       ((null prev))
                     (when (eql (disk-request-next prev) request)
                       (setf (disk-request-next prev) (disk-request-next request))))))
          (setf (disk-request-state request) :error
                (disk-request-error-reason request) reason)
          (latch-trigger (disk-request-latch request)))))))

(defun disk-await-request (request)
  "Wait for REQUEST to complete. Returns true if the request succeeded; false on failure.
Second value is the failure reason."
  (latch-wait (disk-request-latch request))
  (case (disk-request-state request)
    (:complete
     t)
    (:error
     (values nil (disk-request-error-reason request)))))

(defun disk-request-complete-p (request)
  "Test if REQUEST has completed. Use DISK-AWAIT-REQUEST to retrieve more detailed information."
  (without-interrupts
    (with-place-spinlock ((disk-request-lock request))
      (let ((state (disk-request-state request)))
        (or (eql state :complete) (eql state :error))))))
