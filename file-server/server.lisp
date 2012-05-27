(in-package #:file-server)

(defvar *commands* (make-hash-table))
(defvar *file-table*)
(defvar *client*)

(define-condition error-with-class (simple-error)
  ((error-class :initarg :class :reader error-class)))

(defmacro defcommand (name lambda-list &body body)
  (let ((args (gensym)))
    `(setf (gethash ',name *commands*)
           (alexandria:named-lambda ,name (,args)
             (destructuring-bind ,lambda-list (rest ,args)
               ,@body)))))

(defcommand :open (path &rest keys &key &allow-other-keys)
  (let ((fid (iter (for i from 0)
                   (for entry in-sequence *file-table*)
                   (when (eql entry nil)
                     (return i)))))
    (format t "Keys ~S~%" keys)
    (unless fid
      (error 'error-with-class
             :format-control "Too many open files."
             :class :too-many-open-files))
    (when (getf keys :element-type)
      (error 'error-with-class
             :format-control ":ELEMENT-TYPE is not permitted."
             :class :bad-argument))
    (setf (aref *file-table* fid)
          (apply 'open path
                 :element-type '(unsigned-byte 8)
                 keys))
    (format *client* "~D~%" fid)))

(defun get-stream (fid)
  (when (or (not (<= 0 fid (1- (length *file-table*))))
            (null (aref *file-table* fid)))
    (error 'error-with-class
           :format-control "Bad file ID."
           :class :bad-file-id))
  (aref *file-table* fid))

(defcommand :close (fid)
  (let ((stream (get-stream fid)))
    (close stream)
    (setf (aref *file-table* fid) nil)
    (format *client* ":ok~%")))

(defcommand :read (fid offset count)
  (assert (< count 1000000)) ; 1MB limit.
  (let ((stream (get-stream fid))
        (seq (make-array count :element-type '(unsigned-byte 8))))
    (file-position stream offset)
    (let ((count (read-sequence seq stream)))
      (format *client* "~D~%" count)
      (write-sequence seq *client* :end count)
      (finish-output *client*))))

(defcommand :write (fid offset count)
  (assert (< count 1000000)) ; 1MB limit.
  (let ((stream (get-stream fid))
        (seq (make-array count :element-type '(unsigned-byte 8))))
    (read-line *client*)
    (read-sequence seq *client*)
    (file-position stream offset)
    (write-sequence seq stream)
    (format *client* ":ok~%")))

(defcommand :size (fid)
  (let ((stream (get-stream fid)))
    (file-position stream :end)
    (format *client* "~D~%" (file-position stream))))

(defcommand :ping ()
  (format *client* ":pong~%"))

(defcommand :directory (path)
  (format *client* "~S~%"
          (mapcar (lambda (path)
                    (list (namestring path)))
                  (cl-fad:list-directory path))))

(defun handle-client (*client*)
  (let ((*file-table* (make-array 8 :initial-element nil)))
    (unwind-protect
         (loop
            (let ((form (read-preserving-whitespace *client*)))
              (format t "Command: ~S~%" form)
              (cond
                ((or (not (consp form))
                     (not (keywordp (first form))))
                 (format *client* "(:invalid-command \"Invalid command\")"))
                ((eql (first form) :quit)
                 (format *client* ":bye")
                 (return))
                (t (let ((fn (gethash (first form) *commands*)))
                     (if fn
                         (handler-case (funcall fn form)
                           (error-with-class (c)
                             (format *client* "(~S ~S)"
                                     (error-class c)
                                     (format nil "~A" c)))
                           (error (c)
                             (format *client* "(:error ~S)"
                                     (format nil "~A" c))))
                         (format *client* "(:invalid-command \"Invalid command\")"))))))
            (finish-output *client*))
      (iter
        (for file in-sequence *file-table*)
        (when file (close file)))
      (finish-output *client*))))

(defun run-file-server (&key (port 2599))
  (iolib:with-open-socket (server :connect :passive
                                  :address-family :internet
                                  :type :stream
                                  :external-format :ascii)
    (iolib:bind-address server iolib:+ipv4-unspecified+
                        :port port
                        :reuse-addr t)
    (iolib:listen-on server :backlog 5)
    (loop
       (iolib:with-accept-connection (client server :wait t)
         (multiple-value-bind (who rport)
             (iolib:remote-name client)
           (format t "Got a connection from ~A:~A!~%" who rport))
         (handler-case (handle-client client)
           (end-of-file ()))))
    (finish-output)
    t))

(defun spawn-file-server (&rest args)
  (bordeaux-threads:make-thread (lambda () (catch 'finished (apply 'run-file-server args)))))

(defun kill-server (thread)
  (bordeaux-threads:interrupt-thread thread (lambda () (throw 'finished nil))))