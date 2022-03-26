;;;
;;; A simple HTTP client
;;;

(in-package #:ql-http)

;;; Octet data

(deftype octet ()
  '(unsigned-byte 8))

(defun make-octet-vector (size)
  (make-array size :element-type 'octet
              :initial-element 0))

(defun octet-vector (&rest octets)
  (make-array (length octets) :element-type 'octet
              :initial-contents octets))

;;; ASCII characters as integers

(defun acode (char)
  (cond ((eql char :cr)
         13)
        ((eql char :lf)
         10)
        (t
         (let ((code (char-code char)))
           (if (<= 0 code 127)
               code
               (error "Character ~S is not in the ASCII character set"
                      char))))))

(defvar *whitespace*
  (list (acode #\Space) (acode #\Tab) (acode :cr) (acode :lf)))

(defun whitep (code)
  (member code *whitespace*))

(defun ascii-vector (string)
  (let ((vector (make-octet-vector (length string))))
    (loop for char across string
          for code = (char-code char)
          for i from 0
          if (< 127 code) do
          (error "Invalid character for ASCII -- ~A" char)
          else
          do (setf (aref vector i) code))
    vector))

(defun ascii-subseq (vector start end)
  "Return a subseq of octet-specialized VECTOR as a string."
  (let ((string (make-string (- end start))))
    (loop for i from 0
          for j from start below end
          do (setf (char string i) (code-char (aref vector j))))
    string))

(defun ascii-downcase (code)
  (if (<= 65 code 90)
      (+ code 32)
      code))

(defun ascii-equal (a b)
  (eql (ascii-downcase a) (ascii-downcase b)))

(defmacro acase (value &body cases)
  (flet ((convert-case-keys (keys)
           (mapcar (lambda (key)
                     (etypecase key
                       (integer key)
                       (character (char-code key))
                       (symbol
                        (ecase key
                          (:cr 13)
                          (:lf 10)
                          ((t) t)))))
                   (if (consp keys) keys (list keys)))))
    `(case ,value
       ,@(mapcar (lambda (case)
                   (destructuring-bind (keys &rest body)
                       case
                     `(,(if (eql keys t)
                            t
                            (convert-case-keys keys))
                        ,@body)))
                 cases))))

;;; Pattern matching (for finding headers)

(defclass matcher ()
  ((pattern
    :initarg :pattern
    :reader pattern)
   (pos
    :initform 0
    :accessor match-pos)
   (matchedp
    :initform nil
    :accessor matchedp)))

(defun reset-match (matcher)
  (setf (match-pos matcher) 0
        (matchedp matcher) nil))

(define-condition match-failure (error) ())

(defun match (matcher input &key (start 0) end error)
  (let ((i start)
        (end (or end (length input)))
        (match-end (length (pattern matcher))))
    (with-slots (pattern pos)
        matcher
      (loop
       (cond ((= pos match-end)
              (let ((match-start (- i pos)))
                (setf pos 0)
                (setf (matchedp matcher) t)
                (return (values match-start (+ match-start match-end)))))
             ((= i end)
              (return nil))
             ((= (aref pattern pos)
                 (aref input i))
              (incf i)
              (incf pos))
             (t
              (if error
                  (error 'match-failure)
                  (if (zerop pos)
                      (incf i)
                      (setf pos 0)))))))))

(defun ascii-matcher (string)
  (make-instance 'matcher
                 :pattern (ascii-vector string)))

(defun octet-matcher (&rest octets)
  (make-instance 'matcher
                 :pattern (apply 'octet-vector octets)))

(defun acode-matcher (&rest codes)
  (make-instance 'matcher
                 :pattern (make-array (length codes)
                                      :element-type 'octet
                                      :initial-contents
                                      (mapcar 'acode codes))))


;;; "Connection Buffers" are a kind of callback-driven,
;;; pattern-matching chunky stream. Callbacks can be called for a
;;; certain number of octets or until one or more patterns are seen in
;;; the input. cbufs automatically refill themselves from a
;;; connection as needed.

(defvar *cbuf-buffer-size* 8192)

(define-condition end-of-data (error) ())

(defclass cbuf ()
  ((data
    :initarg :data
    :accessor data)
   (connection
    :initarg :connection
    :accessor connection)
   (start
    :initarg :start
    :accessor start)
   (end
    :initarg :end
    :accessor end)
   (eofp
    :initarg :eofp
    :accessor eofp))
  (:default-initargs
   :data (make-octet-vector *cbuf-buffer-size*)
   :connection nil
   :start 0
   :end 0
   :eofp nil)
  (:documentation "A CBUF is a connection buffer that keeps track of
  incoming data from a connection. Several functions make it easy to
  treat a CBUF as a kind of chunky, callback-driven stream."))

(define-condition cbuf-progress ()
  ((size
    :initarg :size
    :accessor cbuf-progress-size
    :initform 0)))

(defun call-processor (fun cbuf start end)
  (signal 'cbuf-progress :size (- end start))
  (funcall fun (data cbuf) start end))

(defun make-cbuf (connection)
  (make-instance 'cbuf :connection connection))

(defun make-stream-writer (stream)
  "Create a callback for writing data to STREAM."
  (lambda (data start end)
    (write-sequence data stream :start start :end end)))

(defgeneric size (cbuf)
  (:method ((cbuf cbuf))
    (- (end cbuf) (start cbuf))))

(defgeneric emptyp (cbuf)
  (:method ((cbuf cbuf))
    (zerop (size cbuf))))

(defgeneric refill (cbuf)
  (:method ((cbuf cbuf))
    (when (eofp cbuf)
      (error 'end-of-data))
    (setf (start cbuf) 0)
    (setf (end cbuf)
          (read-octets (data cbuf)
                       (connection cbuf)))
    (cond ((emptyp cbuf)
           (setf (eofp cbuf) t)
           (error 'end-of-data))
          (t (size cbuf)))))

(defun process-all (fun cbuf)
  (unless (emptyp cbuf)
    (call-processor fun cbuf (start cbuf) (end cbuf))))

(defun multi-cmatch (matchers cbuf)
  (let (start end)
    (dolist (matcher matchers (values start end))
      (multiple-value-bind (s e)
          (match matcher (data cbuf)
                 :start (start cbuf)
                 :end (end cbuf))
        (when (and s (or (null start) (< s start)))
          (setf start s
                end e))))))

(defun cmatch (matcher cbuf)
  (if (consp matcher)
      (multi-cmatch matcher cbuf)
      (match matcher (data cbuf) :start (start cbuf) :end (end cbuf))))

(defun call-until-end (fun cbuf)
  (handler-case
      (loop
       (process-all fun cbuf)
       (refill cbuf))
    (end-of-data ()
      (return-from call-until-end))))

(defun show-cbuf (context cbuf)
  (format t "cbuf: ~A ~D - ~D~%" context (start cbuf) (end cbuf)))

(defun call-for-n-octets (n fun cbuf)
  (let ((remaining n))
    (loop
     (when (<= remaining (size cbuf))
       (let ((end (+ (start cbuf) remaining)))
         (call-processor fun cbuf (start cbuf) end)
         (setf (start cbuf) end)
         (return)))
     (process-all fun cbuf)
     (decf remaining (size cbuf))
     (refill cbuf))))

(defun call-until-matching (matcher fun cbuf)
  (loop
   (multiple-value-bind (start end)
       (cmatch matcher cbuf)
     (when start
       (call-processor fun cbuf (start cbuf) end)
       (setf (start cbuf) end)
       (return)))
   (process-all fun cbuf)
   (refill cbuf)))

(defun ignore-data (data start end)
  (declare (ignore data start end)))

(defun skip-until-matching (matcher cbuf)
  (call-until-matching matcher 'ignore-data cbuf))


;;; Creating HTTP requests as octet buffers

(defclass octet-sink ()
  ((storage
    :initarg :storage
    :accessor storage))
  (:default-initargs
   :storage (make-array 1024 :element-type 'octet
                        :fill-pointer 0
                        :adjustable t))
  (:documentation "A simple stream-like target for collecting
  octets."))

(defun add-octet (octet sink)
  (vector-push-extend octet (storage sink)))

(defun add-octets (octets sink &key (start 0) end)
  (setf end (or end (length octets)))
  (loop for i from start below end
        do (add-octet (aref octets i) sink)))

(defun add-string (string sink)
  (loop for char across string
        for code = (char-code char)
        do (add-octet code sink)))

(defun add-strings (sink &rest strings)
  (mapc (lambda (string) (add-string string sink)) strings))

(defun add-newline (sink)
  (add-octet 13 sink)
  (add-octet 10 sink))

(defun sink-buffer (sink)
  (subseq (storage sink) 0))

(defvar *proxy-url* (config-value "proxy-url"))

(defun full-proxy-path (host port path)
  (format nil "~:[http~;https~]://~A~:[:~D~;~*~]~A"
          (eql port 443)
          host
          (or (null port)
              (eql port 80)
              (eql port 443))
          port
          path))

(defun user-agent-string ()
  "Return a string suitable for using as the User-Agent value in HTTP
requests. Includes Quicklisp version and CL implementation and version
information."
  (labels ((requires-encoding (char)
             (not (or (alphanumericp char)
                      (member char '(#\. #\- #\_)))))
           (encode (string)
             (substitute-if #\_ #'requires-encoding string))
           (version-string (string)
             (if (string-equal string nil)
                 "unknown"
                 (let* ((length (length string))
                        (start (or (position-if #'digit-char-p string)
                                   0))
                        (space (or (position #\Space string :start start)
                                   length))
                        (limit (min space length (+ start 24))))
                   (encode (subseq string start limit))))))
    ;; FIXME: Be more configurable, and take/set the version from
    ;; somewhere else.
    (format nil "quicklisp-client/~A ~A/~A"
            ql-info:*version*
            (encode (lisp-implementation-type))
            (version-string (lisp-implementation-version)))))

(defun make-request-buffer (host port path &key (method "GET"))
  "Return an octet vector suitable for sending as an HTTP 1.1 request."
  (setf method (string method))
  (when *proxy-url*
    (setf path (full-proxy-path host port path)))
  (let ((sink (make-instance 'octet-sink)))
    (flet ((add-line (&rest strings)
             (apply #'add-strings sink strings)
             (add-newline sink)))
      (add-line method " " path " HTTP/1.1")
      (add-line "Host: " host (if (integerp port)
                                  (format nil ":~D" port)
                                  ""))
      (add-line "Connection: close")
      (add-line "User-Agent: " (user-agent-string))
      (add-newline sink)
      (sink-buffer sink))))

(defun sink-until-matching (matcher cbuf)
  (let ((sink (make-instance 'octet-sink)))
    (call-until-matching
     matcher
     (lambda (buffer start end)
       (add-octets buffer sink :start start :end end))
     cbuf)
    (sink-buffer sink)))


;;; HTTP headers

(defclass header ()
  ((data
    :initarg :data
    :accessor data)
   (status
    :initarg :status
    :accessor status)
   (name-starts
    :initarg :name-starts
    :accessor name-starts)
   (name-ends
    :initarg :name-ends
    :accessor name-ends)
   (value-starts
    :initarg :value-starts
    :accessor value-starts)
   (value-ends
    :initarg :value-ends
    :accessor value-ends)))

(defmethod print-object ((header header) stream)
  (print-unreadable-object (header stream :type t)
    (prin1 (status header) stream)))

(defun matches-at (pattern target pos)
  (= (mismatch pattern target :start2 pos) (length pattern)))

(defun header-value-indexes (field-name header)
  (loop with data = (data header)
        with pattern = (ascii-vector (string-downcase field-name))
        for start across (name-starts header)
        for i from 0
        when (matches-at pattern data start)
        return (values (aref (value-starts header) i)
                       (aref (value-ends header) i))))

(defun ascii-header-value (field-name header)
  (multiple-value-bind (start end)
      (header-value-indexes field-name header)
    (when start
      (ascii-subseq (data header) start end))))

(defun all-field-names (header)
  (map 'list
       (lambda (start end)
         (ascii-subseq (data header) start end))
       (name-starts header)
       (name-ends header)))

(defun headers-alist (header)
  (mapcar (lambda (name)
            (cons name (ascii-header-value name header)))
          (all-field-names header)))

(defmethod describe-object :after ((header header) stream)
  (format stream "~&Decoded headers:~%  ~S~%" (headers-alist header)))

(defun content-length (header)
  (let ((field-value (ascii-header-value "content-length" header)))
    (when field-value
      (let ((value (ignore-errors (parse-integer field-value))))
        (or value
            (error "Content-Length header field value is not a number -- ~A"
                   field-value))))))

(defun chunkedp (header)
  (string= (ascii-header-value "transfer-encoding" header) "chunked"))

(defun location (header)
  (ascii-header-value "location" header))

(defun status-code (vector)
  (let* ((space (position (acode #\Space) vector))
         (c1 (- (aref vector (incf space)) 48))
         (c2 (- (aref vector (incf space)) 48))
         (c3 (- (aref vector (incf space)) 48)))
    (+ (* c1 100)
       (* c2  10)
       (* c3   1))))

(defun force-downcase-field-names (header)
  (loop with data = (data header)
        for start across (name-starts header)
        for end across (name-ends header)
        do (loop for i from start below end
                 for code = (aref data i)
                 do (setf (aref data i) (ascii-downcase code)))))

(defun skip-white-forward (pos vector)
  (position-if-not 'whitep vector :start pos))

(defun skip-white-backward (pos vector)
  (let ((nonwhite (position-if-not 'whitep vector :end pos :from-end t)))
    (if nonwhite
        (1+ nonwhite)
        pos)))

(defun contract-field-value-indexes (header)
  "Header field values exclude leading and trailing whitespace; adjust
the indexes in the header accordingly."
  (loop with starts = (value-starts header)
        with ends = (value-ends header)
        with data = (data header)
        for i from 0
        for start across starts
        for end across ends
        do
        (setf (aref starts i) (skip-white-forward start data))
        (setf (aref ends i) (skip-white-backward end data))))

(defun next-line-pos (vector)
  (let ((pos 0))
    (labels ((finish (&optional (i pos))
               (return-from next-line-pos i))
             (after-cr (code)
               (acase code
                 (:lf (finish pos))
                 (t (finish (1- pos)))))
             (pending (code)
               (acase code
                 (:cr #'after-cr)
                 (:lf (finish pos))
                 (t #'pending))))
      (let ((state #'pending))
        (loop
         (setf state (funcall state (aref vector pos)))
         (incf pos))))))

(defun make-hvector ()
  (make-array 16 :fill-pointer 0 :adjustable t))

(defun process-header (vector)
  "Create a HEADER instance from the octet data in VECTOR."
  (let* ((name-starts (make-hvector))
         (name-ends (make-hvector))
         (value-starts (make-hvector))
         (value-ends (make-hvector))
         (header (make-instance 'header
                                :data vector
                                :status 999
                                :name-starts name-starts
                                :name-ends name-ends
                                :value-starts value-starts
                                :value-ends value-ends))
         (mark nil)
         (pos (next-line-pos vector)))
    (unless pos
      (error "Unable to process HTTP header"))
    (setf (status header) (status-code vector))
    (labels ((save (value vector)
               (vector-push-extend value vector))
             (mark ()
               (setf mark pos))
             (clear-mark ()
               (setf mark nil))
             (finish ()
               (if mark
                   (save mark value-ends)
                   (save pos value-ends))
              (force-downcase-field-names header)
              (contract-field-value-indexes header)
              (return-from process-header header))
             (in-new-line (code)
               (acase code
                 ((#\Tab #\Space) (setf mark nil) #'in-value)
                 (t
                  (when mark
                    (save mark value-ends))
                  (clear-mark)
                  (save pos name-starts)
                  (in-name code))))
             (after-cr (code)
               (acase code
                 (:lf #'in-new-line)
                 (t (in-new-line code))))
             (in-name (code)
               (acase code
                 (#\:
                  (save pos name-ends)
                  (save (1+ pos) value-starts)
                  #'in-value)
                 ((:cr :lf)
                  (finish))
                 ((#\Tab #\Space)
                  (error "Unexpected whitespace in header field name"))
                 (t
                  (unless (<= 0 code 127)
                    (error "Unexpected non-ASCII header field name"))
                  #'in-name)))
             (in-value (code)
               (acase code
                 (:lf (mark) #'in-new-line)
                 (:cr (mark) #'after-cr)
                 (t #'in-value))))
      (let ((state #'in-new-line))
        (loop
         (incf pos)
         (when (<= (length vector) pos)
           (error "No header found in response"))
         (setf state (funcall state (aref vector pos))))))))


;;; HTTP URL parsing

(defclass url ()
  ((scheme
    :initarg :scheme
    :accessor scheme
    :initform nil)
   (hostname
    :initarg :hostname
    :accessor hostname
    :initform nil)
   (port
    :initarg :port
    :accessor port
    :initform nil)
   (path
    :initarg :path
    :accessor path
    :initform "/")))

(defun parse-urlstring (urlstring)
  (setf urlstring (string-trim " " urlstring))
  (let* ((pos (position #\: urlstring))
         (scheme (or (and pos (subseq  urlstring 0 pos)) "http"))
         (pos (mismatch urlstring "://" :test 'char-equal :start1 pos))
         (mark pos)
         (url (make-instance 'url)))
    (setf (scheme url) scheme)
    (labels ((save ()
               (subseq urlstring mark pos))
             (mark ()
               (setf mark pos))
             (finish ()
               (return-from parse-urlstring url))
             (hostname-char-p (char)
               (position char "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_."
                         :test 'char-equal))
             (at-start (char)
               (case char
                 (#\/
                  (setf (port url) nil)
                  (mark)
                  #'in-path)
                 (t
                  #'in-host)))
             (in-host (char)
               (case char
                 ((#\/ :end)
                  (setf (hostname url) (save))
                  (mark)
                  #'in-path)
                 (#\:
                  (setf (hostname url) (save))
                  (mark)
                  #'in-port)
                 (t
                  (unless (hostname-char-p char)
                    (error "~S is not a valid URL" urlstring))
                  #'in-host)))
             (in-port (char)
               (case char
                 ((#\/ :end)
                  (setf (port url)
                        (parse-integer urlstring
                                       :start (1+ mark)
                                       :end pos))
                  (mark)
                  #'in-path)
                 (t
                  (unless (digit-char-p char)
                    (error "Bad port in URL ~S" urlstring))
                  #'in-port)))
             (in-path (char)
               (case char
                 ((#\# :end)
                  (setf (path url) (save))
                  (finish)))
               #'in-path))
      (let ((state #'at-start))
        (loop
         (when (<= (length urlstring) pos)
           (funcall state :end)
           (finish))
         (setf state (funcall state (aref urlstring pos)))
         (incf pos))))))

(defun url (thing)
  (if (stringp thing)
      (parse-urlstring thing)
      thing))

(defgeneric request-buffer (method url)
  (:method (method url)
    (setf url (url url))
    (make-request-buffer (hostname url) (or (port url) 80) (path url)
                         :method method)))

(defun urlstring (url)
  (format nil "~@[~A://~]~@[~A~]~@[:~D~]~A"
          (and (hostname url) (scheme url))
          (hostname url)
          (port url)
          (path url)))

(defmethod print-object ((url url) stream)
  (print-unreadable-object (url stream :type t)
    (prin1 (urlstring url) stream)))

(defun merge-urls (url1 url2)
  (setf url1 (url url1))
  (setf url2 (url url2))
  (make-instance 'url
                 :scheme (or (scheme url1)
                             (scheme url2))
                 :hostname (or (hostname url1)
                               (hostname url2))
                 :port (or (port url1)
                           (port url2))
                 :path (or (path url1)
                           (path url2))))


;;; Requesting an URL and saving it to a file

(defparameter *maximum-redirects* 10)
(defvar *default-url-defaults* (url "http://src.quicklisp.org/"))

(defun read-http-header (cbuf)
  (let ((header-data (sink-until-matching (list (acode-matcher :lf :lf)
                                                (acode-matcher :cr :cr)
                                                (acode-matcher :cr :lf :cr :lf))
                                 cbuf)))
    (process-header header-data)))

(defun read-chunk-header (cbuf)
  (let* ((header-data (sink-until-matching (acode-matcher :cr :lf) cbuf))
         (end (or (position (acode :cr) header-data)
                  (position (acode #\;) header-data))))
    (values (parse-integer (ascii-subseq header-data 0 end) :radix 16))))

(defun save-chunk-response (stream cbuf)
  "For a chunked response, read all chunks and write them to STREAM."
  (let ((fun (make-stream-writer stream))
        (matcher (acode-matcher :cr :lf)))
    (loop
     (let ((chunk-size (read-chunk-header cbuf)))
       (when (zerop chunk-size)
         (return))
       (call-for-n-octets chunk-size fun cbuf)
       (skip-until-matching matcher cbuf)))))

(defun save-response (file header cbuf &key (if-exists :rename-and-delete))
  (with-open-file (stream file
                          :direction :output
                          :if-exists if-exists
                          :element-type 'octet)
    (let ((content-length (content-length header)))
      (cond ((chunkedp header)
             (save-chunk-response stream cbuf))
            (content-length
             (call-for-n-octets content-length
                                (make-stream-writer stream)
                                cbuf))
            (t
             (call-until-end (make-stream-writer stream) cbuf))))))

(defun call-with-progress-bar (size fun)
  (let ((progress-bar (make-progress-bar size)))
    (start-display progress-bar)
    (flet ((update (condition)
             (update-progress progress-bar
                              (cbuf-progress-size condition))))
      (handler-bind ((cbuf-progress #'update))
        (funcall fun)))
    (finish-display progress-bar)))

(define-condition fetch-error (error) ())

(define-condition unexpected-http-status (fetch-error)
  ((status-code
    :initarg :status-code
    :reader unexpected-http-status-code)
   (url
    :initarg :url
    :reader unexpected-http-status-url))
  (:report
   (lambda (condition stream)
     (format stream "Unexpected HTTP status for ~A: ~A"
             (unexpected-http-status-url condition)
             (unexpected-http-status-code condition)))))

(define-condition too-many-redirects (fetch-error)
  ((url
    :initarg :url
    :reader too-many-redirects-url)
   (redirect-count
    :initarg :redirect-count
    :reader too-many-redirects-count))
  (:report
   (lambda (condition stream)
     (format stream "Too many redirects (~:D) for ~A"
             (too-many-redirects-count condition)
             (too-many-redirects-url condition)))))

(defvar *fetch-scheme-functions*
  '(("http" . http-fetch))
  "assoc list to decide which scheme-function are called by FETCH function.")

(defun fetch (url file &rest rest)
  "Request URL and write the body of the response to FILE."
  (let* ((url (merge-urls url *default-url-defaults*))
         (call (cdr (assoc (scheme url) *fetch-scheme-functions* :test 'equal))))
    (if call
        (apply call (urlstring url) file rest)
        (error "Unknown scheme ~S" url))))

(defun http-fetch (url file &key (follow-redirects t) quietly
              (if-exists :rename-and-delete)
              (maximum-redirects *maximum-redirects*))
  "default scheme-function for http protocol."
  (setf url (merge-urls url *default-url-defaults*))
  (setf file (merge-pathnames file))
  (let ((redirect-count 0)
        (original-url url)
        (connect-url (or (url *proxy-url*) url))
        (stream (if quietly
                    (make-broadcast-stream)
                    *trace-output*)))
    (loop
     (when (<= maximum-redirects redirect-count)
       (error 'too-many-redirects
              :url original-url
              :redirect-count redirect-count))
     (with-connection (connection (hostname connect-url) (or (port connect-url) 80))
       (let ((cbuf (make-instance 'cbuf :connection connection))
             (request (request-buffer "GET" url)))
         (write-octets request connection)
         (let ((header (read-http-header cbuf)))
           (loop while (= (status header) 100)
                 do (setf header (read-http-header cbuf)))
           (cond ((= (status header) 200)
                  (let ((size (content-length header)))
                    (format stream "~&; Fetching ~A~%" url)
                    (if (and (numberp size)
                             (plusp size))
                        (format stream "; ~$KB~%" (/ size 1024))
                        (format stream "; Unknown size~%"))
                    (if quietly
                        (save-response file header cbuf
                                       :if-exists if-exists)
                        (call-with-progress-bar
                         (content-length header)
                         (lambda ()
                           (save-response file header cbuf
                                          :if-exists if-exists))))))
                 ((not (<= 300 (status header) 399))
                  (error 'unexpected-http-status
                         :url url
                         :status-code (status header))))
           (if (and follow-redirects (<= 300 (status header) 399))
               (let ((new-urlstring (ascii-header-value "location" header)))
                 (when (not new-urlstring)
                   (error "Redirect code ~D received, but no Location: header"
                          (status header)))
                 (incf redirect-count)
                 (setf url (merge-urls new-urlstring
                                       url))
                 (format stream "~&; Redirecting to ~A~%" url))
               (return (values header (and file (probe-file file)))))))))))
