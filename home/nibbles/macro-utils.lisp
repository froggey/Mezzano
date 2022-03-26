;;;; macro-utils.lisp -- functions for compile-time macros

(cl:in-package :nibbles)

(defun byte-fun-name (bitsize signedp big-endian-p desc)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~A~D~A/~A"
                    (symbol-name (if signedp :sb :ub))
                    bitsize
                    (symbol-name desc)
                    (symbol-name (if big-endian-p :be :le))))))

(defun float-fun-name (float-type big-endian-p desc)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~A-~A-~A/~A"
                    (symbol-name :ieee)
                    (symbol-name float-type)
                    (symbol-name desc)
                    (symbol-name (if big-endian-p :be :le))))))

(defun byte-ref-fun-name (bitsize signedp big-endian-p)
  (byte-fun-name bitsize signedp big-endian-p :ref))

(defun float-ref-fun-name (float-type big-endian-p)
  (float-fun-name float-type big-endian-p :ref))

(defun byte-set-fun-name (bitsize signedp big-endian-p)
  (byte-fun-name bitsize signedp big-endian-p :set))

(defun float-set-fun-name (float-type big-endian-p)
  (float-fun-name float-type big-endian-p :set))

(defun stream-ref-fun-name (bitsize readp signedp big-endian-p)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~A-~A~D/~A"
                    (symbol-name (if readp :read :write))
                    (symbol-name (if signedp :sb :ub))
                    bitsize
                    (symbol-name (if big-endian-p :be :le))))))

(defun stream-float-ref-fun-name (float-type readp big-endian-p)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~A-~A-~A/~A"
                    (symbol-name (if readp :read :write))
                    (symbol-name :ieee)
                    (symbol-name float-type)
                    (symbol-name (if big-endian-p :be :le))))))

(defun stream-seq-fun-name (bitsize readp signedp big-endian-p)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~A-~A~D/~A-~A"
                    (symbol-name (if readp :read :write))
                    (symbol-name (if signedp :sb :ub))
                    bitsize
                    (symbol-name (if big-endian-p :be :le))
                    (symbol-name :sequence)))))

(defun stream-float-seq-fun-name (float-type readp big-endian-p)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~A-~A-~A/~A-~A"
                    (symbol-name (if readp :read :write))
                    (symbol-name :ieee)
                    (symbol-name float-type)
                    (symbol-name (if big-endian-p :be :le))
                    (symbol-name :sequence)))))

(defun stream-into-seq-fun-name (bitsize signedp big-endian-p)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~A-~A~D/~A-~A"
                    (symbol-name :read)
                    (symbol-name (if signedp :sb :ub))
                    bitsize
                    (symbol-name (if big-endian-p :be :le))
                    (symbol-name :into-sequence)))))

(defun stream-float-into-seq-fun-name (float-type big-endian-p)
  (let ((*package* (find-package :nibbles)))
    (intern (format nil "~A-~A/~A-~A"
                    (symbol-name :read-ieee)
                    (symbol-name float-type)
                    (symbol-name (if big-endian-p :be :le))
                    (symbol-name :into-sequence)))))

(defun internalify (s)
  (let ((*package* (find-package :nibbles)))
    (intern (concatenate 'string "%" (string s)))))

(defun format-docstring (&rest args)
  (loop with docstring = (apply #'format nil args)
	for start = 0 then (when pos (1+ pos))
	for pos = (and start (position #\Space docstring :start start))
	while start
	collect (subseq docstring start pos) into words
	finally (return (format nil "~{~<~%~1,76:;~A~>~^ ~}"
				words))))

(defun ref-form (vector-name index-name byte-size signedp big-endian-p)
  "Return a form that fetches a SIGNEDP BYTE-SIZE value from VECTOR-NAME,
starting at INDEX-NAME.  The value is stored in the vector according to
BIG-ENDIAN-P."
  (multiple-value-bind (low high increment compare)
      (if big-endian-p
          (values 0 (1- byte-size) 1 #'>)
          (values (1- byte-size) 0 -1 #'<))
    (do ((i (+ low increment) (+ i increment))
         (shift (* (- byte-size 2) 8) (- shift 8))
         (forms nil))
        ((funcall compare i high)
         `(let* ((high-byte (aref , vector-name
                                    (+ ,index-name ,low)))
                 ;; Would be great if we could just sign-extend along
                 ;; with the load, but this is as good as it gets in
                 ;; portable Common Lisp.
                 (signed-high ,(if signedp
                                   `(if (logbitp 7 high-byte)
                                        (- high-byte 256)
                                        high-byte)
                                   'high-byte))
                 (shifted-into-place
                  (ash signed-high ,(* (1- byte-size) 8))))
            (declare (type (unsigned-byte 8) high-byte))
            (declare (type (,(if signedp 'signed-byte 'unsigned-byte) 8)
                           signed-high))
            (logior shifted-into-place ,@(nreverse forms))))
      (push `(ash (aref ,vector-name
                        (+ ,index-name ,i))
                  ,shift)
            forms))))
(defun set-form (vector-name index-name value-name byte-size big-endian-p)
  "Return a form that stores a BYTE-SIZE VALUE-NAME into VECTOR-NAME,
starting at INDEX-NAME.  The value is stored in the vector according to
BIG-ENDIAN-P.  The form returns VALUE-NAME."
  `(progn
     ,@(loop for i from 1 to byte-size
             collect (let ((offset (if big-endian-p
                                       (- byte-size i)
                                       (1- i))))
                       `(setf (aref ,vector-name
                                    (+ ,index-name ,offset))
                              (ldb (byte 8 ,(* 8 (1- i))) ,value-name))))
     ,value-name))
