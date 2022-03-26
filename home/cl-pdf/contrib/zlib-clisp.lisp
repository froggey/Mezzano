;;;
;;; Placed into the Public Domain by Joerg-Cyril Hoehle, 2002.
;;;
;;; zlib-clisp.lisp - Code to compress a Lisp string to an array of
;;; bytes, using the zlib compression library (http://www.zlib.org).
;;; Suitable for use with CL-PDF by Marc Battyani.
;;; For use with CLISP > 2.28 which provides dynamic loading of
;;; libraries and an enhanced FFI.

;; Needs in pdf.lisp:
;; (defconstant +external-format+ #+CLISP (ext:make-encoding :charset 'charset:iso-8859-1 :line-terminator :unix))
;; :default or :unix is not enough, esp. if custom:*default-file-encoding* is
;; based on charset:cp1252 (as in MS-Windows implementation)

;;; Underspecified requirements about compress-string signature lead
;;; to undefined input and output types...
;; AllegroCL (cl-zlib-small.lisp): (array (unsigned-byte 8))
;; LispWorks (zlib-lw.lisp): string
;; CMUCL (zlib-cmucl.lisp): (values string len)
;; CLISP (here): (array (unsigned-byte 8))
;; NB: A second result value is not used by the calling code
;; (write-object pdf-stream) in pdf.lisp. It would be useful in
;; conjunction with :end to save one call to SUBSEQ.

;; From the source (pdf.lisp): input (source) is a string, and the
;; result must be suitable for use by WRITE-SEQUENCE on a stream with
;; element-type what with-open-file opens by default.

;; Returning a string is not acceptable however, in the presence of
;; Unicode and all kinds of character encodings.
;; The compressed bytes must be written out verbatim.


(in-package :pdf)

#-CLISP (error "This code should be loaded in CLISP.")

(use-package "FFI")

#|
Here is what I dream of:
(def-lib-call-out zlib-compress [eventually library]
  (:name "compress")
  (:arguments (dest (ffi:c-ptr (ffi:c-array ffi:uint8 is_len(destlen)))
                    :out :guard (zerop return))
              (destlen ffi:ulong :in-out)
              (source (ffi:c-ptr (ffi:c-array ffi:uint8 is_len(sourcelen))))
              (sourcelen ffi:ulong))
  (:return-type ffi:int))
|#

(defvar *zlib-path*
  (or #+WIN32 "zlib.dll"
      ;; TODO how to deal with cygwin (#+UNIX on MS-Windows)? -- it wants zlib.dll
      #+UNIX "libz.so"
      #+AMIGA "zlib.library"
  )
  "Set this variable to point to the location of the zlib library
(libz.so or zlib.dll) on your system.")

(defvar *zlib*)
(unless (and (boundp '*zlib*)
             (ffi:validp *zlib*))
  (setq *zlib* (user::foreign-library *zlib-path*)))


;; Notes: CL-PDF passes source as a string, not as an array of
;; :element-type (unsigned-byte 8).
;; Using ffi:c-string means custom:*foreign-encoding* comes into play...
;; TODO custom:*foreign-encoding* is not set.

;; The callee does not even see the trailing 0 that is appended by
;; ffi:c-string (or ffi:c-array-ptr character), since it receives the
;; buffer length as an extra argument.

(ffi:def-lib-call-out zlib-compress-string *zlib*
  (:name "compress")
  (:arguments (dest ffi:c-pointer :in)
              (destlen (ffi:c-ptr ffi:ulong) :in-out)
              (source ffi:c-string)
              (sourcelen ffi:ulong))
  (:return-type ffi:int)
  (:language :stdc))

(defun compress-string (source)
  "Compress the string SOURCE. Returns an array of bytes
representing the compressed data."
  (let* ((sourcelen (length source))
         (destlen (+ 12 (ceiling (* sourcelen 1.05)))))
    ;; Using CLISP's symbol-macro based interface
    (ffi:with-c-var (dest `(c-array uint8 ,destlen)) ; no init
      (multiple-value-bind (status actual)
          (zlib-compress-string (ffi:c-var-address dest) destlen source sourcelen)
        (if (zerop status)
            ;;(subseq dest 0 actual)
            ;;ffi:cast not usable because of different size...
            (ffi:offset dest 0 `(c-array uint8 ,actual))
          (error "zlib error, code ~d" status))))))


(defmethod write-object ((obj pdf-stream) &optional root-level &aux compressed)
  #+(or Lispworks allegro CMU CLISP)
  (when (and *compress-streams* (> (length (content obj)) *min-size-for-compression*))
    (setf (content obj) (setq compressed (compress-string (content obj))))
    (let ((filter (get-dict-value obj "/Filter")))
      (if filter
        (change-dict-value obj "/Filter" (vector "/FlateDecode" filter))
        (push (cons "/Filter" "/FlateDecode")(dict-values obj)))))
  (call-next-method)
  (write-line "stream" *pdf-stream*)
  #+(or CLISP)
  (if (typep (content obj) '(array (unsigned-byte 8)))
      (unwind-protect ;; a typical pattern for CLISP's LETF macro...
          (progn
            ;; stream is not bivalent, switch element-type
            (setf (stream-element-type *pdf-stream*) '(unsigned-byte 8))
            (write-sequence (content obj) *pdf-stream*))
        (setf (stream-element-type *pdf-stream*) 'character))
    (write-sequence (content obj) *pdf-stream*))
  #-(or CLISP)
  (write-sequence (content obj) *pdf-stream*)
  (write-char #\Newline *pdf-stream*)
  (write-line "endstream" *pdf-stream*))


