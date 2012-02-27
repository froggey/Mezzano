(in-package #:sys.int)

(defvar *print-array* t)
(defvar *print-base* 10)
(defvar *print-case* :upcase)
(defvar *print-circle* nil)
(defvar *print-escape* t)
(defvar *print-gensym* t)
(defvar *print-length* nil)
(defvar *print-level* nil)
(defvar *print-lines* nil)
(defvar *print-miser-width* nil)
(defvar *print-pprint-dispatch* nil)
(defvar *print-pretty* nil)
(defvar *print-radix* nil)
(defvar *print-readably* nil)
(defvar *print-right-margin* nil)

(defvar *print-safe* nil)
(defvar *print-space-char-ansi* nil)

(defun write-unsigned-integer (x base stream)
  (unless (= x 0)
    (write-unsigned-integer (truncate x base) base stream)
    (write-char (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                       (rem x base))
                stream)))

(defun write-integer (x &optional (base 10) stream)
  (cond ((= x 0)
         (write-char #\0 stream))
        ((< x 0)
         (write-char #\- stream)
         (write-unsigned-integer (- 0 x) base stream))
        (t (write-unsigned-integer x base stream))))

(defun terpri (&optional stream)
  (write-char #\Newline stream))

(defun fresh-line (&optional stream)
  (unless (start-line-p stream)
    (terpri stream)))

(defun write-string (string &optional stream)
  (dotimes (i (length string))
    (write-char (char string i) stream)))

(defun write-object (object stream)
  (typecase object
    (integer (write-integer object *print-base* stream))
    (cons
     (write-char #\( stream)
     (write (car object) :stream stream)
     (do ((i (cdr object) (cdr i)))
         ((atom i)
          (when i
            (write-string " . " stream)
            (write i :stream stream))
          (write-char #\) stream))
       (write-char #\Space stream)
       (write (car i) :stream stream)))
    (symbol
     (when (keywordp object)
       (write-char #\: stream))
     (write-string (symbol-name object) stream))
    (string
     (write-char #\" stream)
     (dotimes (i (length object))
       (let ((c (char object i)))
         (case c
           (#\\ (write-char #\\ stream) (write-char #\\ stream))
           (#\" (write-char #\\ stream) (write-char #\" stream))
           (t (write-char c stream)))))
     (write-char #\" stream))
    (character
     (write-char #\# stream)
     (write-char #\\ stream)
     (cond ((and (or *print-space-char-ansi* (not (eql object #\Space)))
                 (not (eql object #\Newline))
                 (standard-char-p object))
            (write-char object stream))
           (t (write-string (char-name object)))))
    (function
     (print-unreadable-object (object stream :type t :identity t)
       (write (function-name object) :stream stream)))
    (t (if *print-safe*
           (print-unreadable-object (object stream :type t :identity t))
           (print-object object stream))))
  object)

(defun write (object &key (stream t) (escape *print-escape*) (readably *print-readably*) &allow-other-keys)
  (let ((*print-escape* escape)
        (*print-readably* readably))
    (write-object object stream)))

(defmacro print-unreadable-object ((object stream &rest keys &key type identity) &body body)
  `(%print-unreadable-object ,(when body `(lambda () (progn ,@body))) ,object ,stream ,@keys))

(defun %print-unreadable-object (fn object stream &key type identity)
  (write-char #\# stream)
  (write-char #\< stream)
  (when type
    (write (type-of object) :stream (make-case-correcting-stream stream :titlecase)))
  (when fn
    (when type
      (write-char #\Space stream))
    (funcall fn))
  (when identity
    (when (or type fn)
      (write-char #\Space stream))
    (write-integer (sys.int::lisp-object-address object) 16))
  (write-char #\>))
