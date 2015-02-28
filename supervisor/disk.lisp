(in-package :mezzano.supervisor)

;;; External API:
;;; ALL-DISKS - Return a list of all disks in the systems. List might not be fresh.
;;; DISK-N-SECTORS - Number of sectors on the disk.
;;; DISK-SECTOR-SIZE - Size of a disk sector in octets.
;;; DISK-READ - Read sectors.
;;; DISK-WRITE - Write sectors.
;;; MAKE-DISK-REQUEST - Allocate a new request object for async operations.
;;; DISK-SUBMIT-REQUEST - Submit an async request.
;;; DISK-CANCEL-REQUEST - Abort an in-progress request.
;;; DISK-AWAIT-REQUEST - Wait for a request to complete.
;;; DISK-REQUEST-COMPLETE-P - Returns true if the request is not in-progress.

(defvar *log-disk-requests* nil)

(defstruct (disk
             (:area :wired))
  device
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
  (state :error :type (member :waiting :in-progress :complete :error))
  (error-reason :uninitialized)
  disk
  direction
  lba
  n-sectors
  buffer
  (lock (make-mutex "Disk request lock" :spin))
  (cvar (make-condition-variable "Disk request notifier"))
  next)

(defvar *disks*)

(defvar *disk-request-current*)
(defvar *disk-request-queue-head*)
(defvar *disk-request-queue-lock*)
(defvar *disk-request-queue-cvar*)

(defun all-disks ()
  ;; Would be nice to copy the list here, but this is called before the paging disk
  ;; is found, so no allocation outside the wired area.
  *disks*)

(defun register-disk (device n-sectors sector-size max-transfer read-fn write-fn)
  (when (> sector-size +4k-page-size+)
    (debug-print-line "Ignoring device " device " with overly-large sector size " sector-size " (should be <= than 4k).")
    (return-from register-disk))
  (let ((disk (make-disk :device device
                         :sector-size sector-size
                         :n-sectors n-sectors
                         :max-transfer max-transfer
                         :read-fn read-fn
                         :write-fn write-fn)))
    (setf *disks* (sys.int::cons-in-area disk *disks* :wired))))

(defun initialize-disk ()
  (when (not (boundp '*disk-request-queue-head*))
    (setf *log-disk-requests* nil)
    (setf *disk-request-current* nil
          *disk-request-queue-head* nil
          *disk-request-queue-lock* (make-mutex "Disk request queue lock" :spin)
          *disk-request-queue-cvar* (make-condition-variable "Disk request queue notifier")
          *disks* '()))
  ;; Abort any queued or in-progress requests.
  (do ((request *disk-request-queue-head* (disk-request-next request)))
      ((null request))
    (setf (disk-request-state request) :error
          (disk-request-error-reason request) :system-reinitialized)
    (condition-notify (disk-request-cvar request) t))
  (setf *disk-request-queue-head* nil)
  (when *disk-request-current*
    (setf (disk-request-state *disk-request-current*) :error
          (disk-request-error-reason *disk-request-current*) :system-reinitialized)
    (condition-notify (disk-request-cvar *disk-request-current*) t)
    (setf *disk-request-current* nil))
  ;; Clear the disk list.
  (dolist (disk *disks*)
    (setf (disk-valid disk) nil))
  (setf *disks* '()))

(defun read-disk-partition (device lba n-sectors buffer)
  (funcall (disk-read-fn (partition-disk device))
           (disk-device (partition-disk device))
           (+ (partition-offset device) lba)
           n-sectors
           buffer))

(defun write-disk-partition (device lba n-sectors buffer)
  (funcall (disk-write-fn (partition-disk device))
           (disk-device (partition-disk device))
           (+ (partition-offset device) lba)
           n-sectors
           buffer))

(defun detect-disk-partitions ()
  (dolist (disk (all-disks))
    (let* ((sector-size (disk-sector-size disk))
           (page (allocate-physical-pages (ceiling sector-size +4k-page-size+)
                                          "DETECT-DISK disk buffer"))
           (page-addr (+ +physical-map-base+ (* page +4k-page-size+))))
      (when (not (disk-read disk 0 (ceiling +4k-page-size+ sector-size) page-addr))
        (panic "Unable to read first block on disk " disk))
      ;; Look for a PC partition table.
      ;; TODO: GPT detection must be done this, as it contains a protective MBR/partition table.
      (when (and (>= sector-size 512)
                 (eql (sys.int::memref-unsigned-byte-8 page-addr #x1FE) #x55)
                 (eql (sys.int::memref-unsigned-byte-8 page-addr #x1FF) #xAA))
        ;; Found, scan partitions.
        ;; TODO: Extended partitions.
        ;; TODO: Explict little endian reads.
        (dotimes (i 4)
          (let ((system-id (sys.int::memref-unsigned-byte-8 (+ page-addr #x1BE (* 16 i) 4) 0))
                (start-lba (sys.int::memref-unsigned-byte-32 (+ page-addr #x1BE (* 16 i) 8) 0))
                (size (sys.int::memref-unsigned-byte-32 (+ page-addr #x1BE (* 16 i) 12) 0)))
            (when (and (not (eql system-id 0))
                       (not (eql size 0)))
              (debug-print-line "Detected partition " i " on disk " disk ". Start: " start-lba " size: " size)
              (register-disk (make-partition :disk disk
                                             :offset start-lba
                                             :id i)
                             size
                             sector-size
                             (disk-max-transfer disk)
                             'read-disk-partition
                             'write-disk-partition)))))
      ;; Release the pages.
      (release-physical-pages page (ceiling sector-size +4k-page-size+)))))

(defun pop-disk-request ()
  (with-mutex (*disk-request-queue-lock*)
    (loop
       (let ((req *disk-request-queue-head*))
         (when req
           (with-mutex ((disk-request-lock req))
             (setf (disk-request-state req) :in-progress
                   *disk-request-current* req)
             (setf *disk-request-queue-head* (disk-request-next req)))
           (return req)))
       (condition-wait *disk-request-queue-cvar* *disk-request-queue-lock*))))

(defun disk-thread ()
  (loop
     ;; Grab a request.
     (let* ((request (pop-disk-request))
            (disk (disk-request-disk request)))
       (when *log-disk-requests*
         (debug-print-line "Disk " (if (eql (disk-request-direction request) :read) "read" "write")
                           " on " disk
                           " " (disk-request-lba request)
                           " " (disk-request-n-sectors request)
                           " " (disk-request-buffer request)))
       (case (disk-request-direction request)
         (:read (set-disk-read-light t))
         (:write (set-disk-write-light t)))
       ;; Execute.
       (multiple-value-bind (successp reason)
           (funcall (case (disk-request-direction request)
                      (:read (disk-read-fn disk))
                      (:write (disk-write-fn disk)))
                    (disk-device disk)
                    (disk-request-lba request)
                    ;; FIXME: Deal with n-sectors > disk max-sectors.
                    (disk-request-n-sectors request)
                    (disk-request-buffer request))
         (with-mutex ((disk-request-lock request))
           (cond (successp
                  (setf (disk-request-state request) :complete
                        (disk-request-error-reason request) nil))
                 (t
                  (setf (disk-request-state request) :error
                        (disk-request-error-reason request) reason)))
           (setf *disk-request-current* nil)
           (condition-notify (disk-request-cvar request) t)))
       (case (disk-request-direction request)
         (:read (set-disk-read-light nil))
         (:write (set-disk-write-light nil))))))

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
  (assert (<= 0 lba (+ lba n-sectors) (1- (disk-n-sectors disk))))
  (with-mutex (*disk-request-queue-lock*)
    (with-mutex ((disk-request-lock request))
      (let ((state (disk-request-state request)))
        (when (or (eql state :in-progress) (eql state :waiting))
          ;; Bad! Request has already been submitted, no resubmitting.
          (return-from disk-submit-request-1 nil)))
      (setf (disk-request-state request) :waiting
            (disk-request-disk request) disk
            (disk-request-direction request) direction
            (disk-request-lba request) lba
            (disk-request-n-sectors request) n-sectors
            (disk-request-buffer request) buffer)
      (when (not (disk-valid disk))
        (setf (disk-request-state request) :error
              (disk-request-error-reason request) :system-reinitialized)
        (return-from disk-submit-request-1 t))
      ;; Attach to request list.
      (setf (disk-request-next request) *disk-request-queue-head*
            *disk-request-queue-head* request)
      ;; Wake the disk thread.
      (condition-notify *disk-request-queue-cvar*)))
  ;; All good.
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
  (with-mutex (*disk-request-queue-lock*)
    (with-mutex ((disk-request-lock request))
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
        (condition-notify (disk-request-cvar request) t)))))

(defun disk-await-request (request)
  "Wait for REQUEST to complete. Returns true if the request succeeded; false on failure.
Second value is the failure reason."
  (with-mutex ((disk-request-lock request))
    (loop
       (case (disk-request-state request)
         (:complete
          (return t))
         (:error
          (return (values nil (disk-request-error-reason request)))))
       (condition-wait (disk-request-cvar request) (disk-request-lock request)))))

(defun disk-request-complete-p (request)
  "Test if REQUEST has completed. Use DISK-AWAIT-REQUEST to retrieve more detailed information."
  (with-mutex ((disk-request-lock request))
    (let ((state (disk-request-state request)))
      (or (eql state :complete) (eql state :error)))))
