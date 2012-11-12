(in-package #:sys.int)

;;; Mini FASL loader.

(defconstant +llf-end-of-load+ #xFF)
(defconstant +llf-backlink+ #x01)
(defconstant +llf-function+ #x02)
(defconstant +llf-cons+ #x03)
(defconstant +llf-symbol+ #x04)
(defconstant +llf-uninterned-symbol+ #x05)
(defconstant +llf-unbound+ #x06)
(defconstant +llf-string+ #x07)
(defconstant +llf-setf-symbol+ #x08)
(defconstant +llf-integer+ #x09)
(defconstant +llf-invoke+ #x0A)
(defconstant +llf-setf-fdefinition+ #x0B)
(defconstant +llf-simple-vector+ #x0C)
(defconstant +llf-character+ #x0D)
(defconstant +llf-structure-definition+ #x0E)
(defconstant +llf-single-float+ #x10)

(defun check-llf-header (stream)
  (assert (and (eql (%read-byte stream) #x4C)
               (eql (%read-byte stream) #x4C)
               (eql (%read-byte stream) #x46)
               (eql (%read-byte stream) #x00))))

(defun load-integer (stream)
  (let ((value 0) (shift 0))
    (loop
         (let ((b (%read-byte stream)))
           (when (not (logtest b #x80))
             (setf value (logior value (ash (logand b #x3F) shift)))
             (if (logtest b #x40)
                 (return (- value))
                 (return value)))
           (setf value (logior value (ash (logand b #x7F) shift)))
           (incf shift 7)))))

(defun utf8-sequence-length (byte)
  (cond
    ((eql (logand byte #x80) #x00)
     (values 1 byte))
    ((eql (logand byte #xE0) #xC0)
     (values 2 (logand byte #x1F)))
    ((eql (logand byte #xF0) #xE0)
     (values 3 (logand byte #x0F)))
    ((eql (logand byte #xF8) #xF0)
     (values 4 (logand byte #x07)))
    (t (error "Invalid UTF-8 lead byte ~S." byte))))

(defun load-character (stream)
  (multiple-value-bind (length value)
      (utf8-sequence-length (%read-byte stream))
    ;; Read remaining bytes. They must all be continuation bytes.
    (dotimes (i (1- length))
      (let ((byte (%read-byte stream)))
        (unless (eql (logand byte #xC0) #x80)
          (error "Invalid UTF-8 continuation byte ~S." byte))
        (setf value (logior (ash value 6) (logand byte #x3F)))))
    (code-char value)))

(defun load-ub8-vector (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type '(unsigned-byte 8))))
    (%read-sequence seq stream)
    seq))

(defun load-vector (stream omap)
  (let* ((len (load-integer stream))
         (seq (make-array len)))
    (dotimes (i len)
      (setf (aref seq i) (load-object stream omap)))
    seq))

(defun load-string (stream)
  (let* ((len (load-integer stream))
         (seq (make-array len :element-type 'character)))
    (dotimes (i len)
      (setf (aref seq i) (load-character stream)))
    seq))

(defun llf-next-is-unbound-p (stream)
  (let ((current-position (%file-position stream)))
    (cond ((eql (%read-byte stream) +llf-unbound+)
           t)
          (t (%file-position stream current-position)
             nil))))

(defun load-one-object (command stream omap)
  (ecase command
    (#.+llf-function+
     (let* ((tag (%read-byte stream))
            (mc (load-ub8-vector stream))
            (fixups (load-object stream omap))
            (constants (load-vector stream omap)))
       (make-function-with-fixups tag mc fixups constants)))
    (#.+llf-cons+
     (let* ((cdr (load-object stream omap))
            (car (load-object stream omap)))
       (cons car cdr)))
    (#.+llf-symbol+
     (let* ((name (load-string stream))
            (package (load-string stream)))
       (intern name package)))
    (#.+llf-uninterned-symbol+
     (let* ((name (load-string stream))
            (sym (make-symbol name)))
       (unless (llf-next-is-unbound-p stream)
         (setf (symbol-value sym) (load-object stream omap)))
       (unless (llf-next-is-unbound-p stream)
         (setf (symbol-function sym) (load-object stream omap)))
       (setf (symbol-plist sym) (load-object stream omap))
       sym))
    (#.+llf-string+ (load-string stream))
    (#.+llf-setf-symbol+
     (let ((symbol (load-object stream omap)))
       (function-symbol `(setf ,symbol))))
    (#.+llf-integer+ (load-integer stream))
    (#.+llf-simple-vector+ (load-vector stream omap))
    (#.+llf-character+ (load-character stream))
    (#.+llf-structure-definition+
     (make-struct-type (load-object stream omap)
                       (load-object stream omap)))
    (#.+llf-single-float+
     (%integer-as-single-float (load-integer stream)))))

(defun load-object (stream omap)
  (let ((command (%read-byte stream)))
    (case command
      (#.+llf-end-of-load+
       (values nil t))
      (#.+llf-backlink+
       (let ((id (load-integer stream)))
         (assert (< id (hash-table-count omap)) () "Object id ~S out of bounds." id)
         (values (gethash id omap) nil)))
      (#.+llf-invoke+
       (values (funcall (load-object stream omap)) nil))
      (#.+llf-setf-fdefinition+
       (let ((fn (load-object stream omap))
             (name (load-object stream omap)))
         (funcall #'(setf fdefinition) fn name)))
      (#.+llf-unbound+ (error "Should not seen UNBOUND here."))
      (t (let ((id (hash-table-count omap)))
           (setf (gethash id omap) '"!!!LOAD PLACEHOLDER!!!")
           (let ((obj (load-one-object command stream omap)))
             (setf (gethash id omap) obj)
             (values obj nil)))))))

(defun mini-load-llf (stream)
  (check-llf-header stream)
  (let ((object-map (make-hash-table))
        (*package* *package*))
    (loop (multiple-value-bind (object stop)
              (load-object stream object-map)
            (declare (ignore object))
            (when stop
              (return))))))
