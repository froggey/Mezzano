;;;; utils.lisp

(in-package #:ql-util)

(defun write-line-to-file (string file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede)
    (write-line string stream)))

(defvar *do-not-prompt* nil
  "When *DO-NOT-PROMPT* is true, PRESS-ENTER-TO-CONTINUE returns true
  without user interaction.")

(defmacro without-prompting (&body body)
  "Evaluate BODY in an environment where PRESS-ENTER-TO-CONTINUE
 always returns true without prompting for the user to press enter."
  `(let ((*do-not-prompt* t))
     ,@body))

(defun press-enter-to-continue ()
  (when *do-not-prompt*
    (return-from press-enter-to-continue t))
  (format *query-io* "~&Press Enter to continue.~%")
  (let ((result (read-line *query-io*)))
    (zerop (length result))))

(defun replace-file (from to)
  "Like RENAME-FILE, but deletes TO if it exists, first."
  (when (probe-file to)
    (delete-file to))
  (rename-file from to))

(defun copy-file (from to &key (if-exists :rename-and-delete))
  "Copy the file FROM to TO."
  (let* ((buffer-size 8192)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (with-open-file (from-stream from :element-type '(unsigned-byte 8))
      (with-open-file (to-stream to :element-type '(unsigned-byte 8)
                                 :direction :output
                                 :if-exists if-exists)
        (let ((length (file-length from-stream)))
          (multiple-value-bind (full leftover)
              (floor length buffer-size)
            (dotimes (i full)
              (read-sequence buffer from-stream)
              (write-sequence buffer to-stream))
            (read-sequence buffer from-stream)
            (write-sequence buffer to-stream :end leftover)))))
    (probe-file to)))

(defun ensure-file-exists (pathname)
  (open pathname :direction :probe :if-does-not-exist :create))

(defun delete-file-if-exists (pathname)
  (when (probe-file pathname)
    (delete-file pathname)))

(defun split-spaces (line)
  (let ((words '())
        (mark 0)
        (pos 0))
    (labels ((finish ()
               (setf pos (length line))
               (save)
               (return-from split-spaces (nreverse words)))
             (save ()
               (when (< mark pos)
                 (push (subseq line mark pos) words)))
             (mark ()
               (setf mark pos))
             (in-word (char)
               (case char
                 (#\Space
                    (save)
                    #'in-space)
                 (t
                    #'in-word)))
             (in-space (char)
               (case char
                 (#\Space
                    #'in-space)
                 (t
                    (mark)
                    #'in-word))))
      (let ((state #'in-word))
        (dotimes (i (length line) (finish))
          (setf pos i)
          (setf state (funcall state (char line i))))))))

(defun first-line (file)
  (with-open-file (stream file)
    (values (read-line stream))))

(defun (setf first-line) (line file)
  (with-open-file (stream file :direction :output
                          :if-exists :rename-and-delete)
    (write-line line stream)))

(defun file-size (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun safely-read (stream)
  "Read one form from STREAM with *READ-EVAL* bound to NIL."
  (let ((*read-eval* nil))
    (read stream)))

(defun safely-read-file (file)
  "Read the first form from FILE with SAFELY-READ."
  (with-open-file (stream file)
    (safely-read stream)))

(defun make-versions-url (url)
  "Given an URL that looks like http://foo/bar.ext, return
http://foo/bar-versions.txt."
  (let ((suffix-pos (position #\. url :from-end t)))
    (unless suffix-pos
      (error "Can't make a versions URL from ~A" url))
    (let ((extension (subseq url suffix-pos)))
      (concatenate 'string
                   (subseq url 0 suffix-pos)
                   "-versions"
                   extension))))
