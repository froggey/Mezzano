;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; strings.lisp --- Conversions between strings and UB8 vectors.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:babel)

;;; The usefulness of this string/octets interface of Babel's is very
;;; limited on Lisps with 8-bit characters which will in effect only
;;; support the latin-1 subset of Unicode.  That is, all encodings are
;;; supported but we can only store the first 256 code points in Lisp
;;; strings.  Support for using other 8-bit encodings for strings on
;;; these Lisps could be added with an extra encoding/decoding step.
;;; Supporting other encodings with larger code units would be silly
;;; (it would break expectations about common string operations) and
;;; better done with something like Closure's runes.

;;; Can we handle unicode fully?
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The EVAL is just here to avoid warnings...
  (case (eval char-code-limit)
    (#x100 (pushnew '8-bit-chars *features*))
    (#x10000 (pushnew 'ucs-2-chars *features*))
    (#x110000 #| yay |#)
    ;; This is here mostly because if the CHAR-CODE-LIMIT is bigger
    ;; than #x11000, strange things might happen but we probably
    ;; shouldn't descriminate against other, smaller, values.
    (t (error "Strange CHAR-CODE-LIMIT (#x~X), bailing out."
              char-code-limit))))

;;; Adapted from Ironclad.  TODO: check if it's worthwhile adding
;;; implementation-specific accessors such as SAP-REF-* for SBCL.
(defmacro ub-get (vector index &optional (bytes 1) (endianness :ne))
  (let ((big-endian (member endianness
                            '(:be #+big-endian :ne #+little-endian :re))))
    (once-only (vector index)
      `(logand
        ,(1- (ash 1 (* 8 bytes)))
        (logior
         ,@(loop for i from 0 below bytes
                 for offset = (if big-endian i (- bytes i 1))
                 for shift = (if big-endian
                                 (* (- bytes i 1) 8)
                                 (* offset 8))
                 collect `(ash (aref ,vector (+ ,index ,offset)) ,shift)))))))

(defmacro ub-set (value vector index &optional (bytes 1) (endianness :ne))
  (let ((big-endian (member endianness
                            '(:be #+big-endian :ne #+little-endian :re))))
    `(progn
       ,@(loop for i from 1 to bytes
               for offset = (if big-endian (- bytes i) (1- i)) collect
               `(setf (aref ,vector (+ ,index ,offset))
                      (ldb (byte 8 ,(* 8 (1- i))) ,value)))
       (values))))

(defmacro string-get (string index)
  `(char-code (schar ,string ,index)))

(defmacro string-set (code string index)
  `(setf (schar ,string ,index) (code-char ,code)))

;;; SIMPLE-BASE-STRING would also be a subtype of SIMPLE-STRING so we
;;; don't use that because on SBCL BASE-CHARs can only hold ASCII.
;;; Also, with (> SPEED SAFETY) (setf (schar base-str n) big-char)
;;; will quietly work, sort of.
;;;
;;; XXX: test this on various lisps.

(defconstant unicode-char-code-limit
  char-code-limit
  "An alias for CL:CHAR-CODE-LIMIT which might be lower than
#x110000 on some Lisps.")

(deftype unicode-char ()
  "This character type can hold any characters whose CHAR-CODEs
are less than UNICODE-CHAR-CODE-LIMIT."
  #+lispworks 'lw:simple-char
  #-lispworks 'character)

(deftype simple-unicode-string ()
  "Alias for (SIMPLE-ARRAY UNICODE-CHAR (*))."
  '(simple-array unicode-char (*)))

(deftype unicode-string ()
  "Alias for (VECTOR UNICODE-CHAR *)."
  '(vector unicode-char *))

(defparameter *string-vector-mappings*
  (instantiate-concrete-mappings
   ;; :optimize ((speed 3) (safety 0) (debug 0) (compilation-speed 0))
   :octet-seq-setter ub-set
   :octet-seq-getter ub-get
   :octet-seq-type (simple-array (unsigned-byte 8) (*))
   :code-point-seq-setter string-set
   :code-point-seq-getter string-get
   :code-point-seq-type simple-unicode-string))

#+sbcl
(defparameter *simple-base-string-vector-mappings*
  (instantiate-concrete-mappings
   ;; :optimize ((speed 3) (safety 0) (debug 0) (compilation-speed 0))
   :instantiate-decoders nil
   :octet-seq-setter ub-set
   :octet-seq-getter ub-get
   :octet-seq-type (simple-array (unsigned-byte 8) (*))
   :code-point-seq-setter string-set
   :code-point-seq-getter string-get
   :code-point-seq-type simple-base-string))

;;; Do we want a more a specific error condition here?
(defun check-vector-bounds (vector start end)
  (unless (<= 0 start end (length vector))
    (error "Invalid start (~A) and end (~A) values for vector of length ~A."
           start end (length vector))))

(defmacro with-simple-vector (((v vector) (s start) (e end)) &body body)
  "If VECTOR is a displaced or adjustable array, binds V to the
underlying simple vector, adds an adequate offset to START and
END and binds those offset values to S and E.  Otherwise, if
VECTOR is already a simple array, it's simply bound to V with no
further changes.

START and END are unchecked and assumed to be within bounds.

Note that in some Lisps, a slow copying implementation is
necessary to obtain a simple vector thus V will be bound to a
copy of VECTOR coerced to a simple-vector.  Therefore, you
shouldn't attempt to modify V."
  #+sbcl
  `(sb-kernel:with-array-data ((,v ,vector) (,s ,start) (,e ,end))
     ,@body)
  #+(or cmu scl)
  `(lisp::with-array-data ((,v ,vector) (,s ,start) (,e ,end))
     ,@body)
  #+openmcl
  (with-unique-names (offset)
    `(multiple-value-bind (,v ,offset)
         (ccl::array-data-and-offset ,vector)
       (let ((,s (+ ,start ,offset))
             (,e (+ ,end ,offset)))
         ,@body)))
  #+allegro
  (with-unique-names (offset)
    `(excl::with-underlying-simple-vector (,vector ,v ,offset)
       (let ((,e (+ ,end ,offset))
             (,s (+ ,start ,offset)))
         ,@body)))
  ;; slow, copying implementation
  #-(or sbcl cmu scl openmcl allegro)
  (once-only (vector)
    `(funcall (if (adjustable-array-p ,vector)
                  #'call-with-array-data/copy
                  #'call-with-array-data/fast)
              ,vector ,start ,end
              (lambda (,v ,s ,e) ,@body))))

#-(or sbcl cmu scl openmcl allegro)
(progn
  ;; Stolen from f2cl.
  (defun array-data-and-offset (array)
    (loop with offset = 0 do
          (multiple-value-bind (displaced-to index-offset)
              (array-displacement array)
            (when (null displaced-to)
              (return-from array-data-and-offset
                (values array offset)))
            (incf offset index-offset)
            (setf array displaced-to))))

  (defun call-with-array-data/fast (vector start end fn)
    (multiple-value-bind (data offset)
        (array-data-and-offset vector)
      (funcall fn data (+ offset start) (+ offset end))))

  (defun call-with-array-data/copy (vector start end fn)
    (funcall fn (replace (make-array (- end start) :element-type
                                     (array-element-type vector))
                         vector :start2 start :end2 end)
             0 (- end start))))

(defmacro with-checked-simple-vector (((v vector) (s start) (e end)) &body body)
  "Like WITH-SIMPLE-VECTOR but bound-checks START and END."
  (once-only (vector start)
    `(let ((,e (or ,end (length ,vector))))
       (check-vector-bounds ,vector ,start ,e)
       (with-simple-vector ((,v ,vector) (,s ,start) (,e ,e))
         ,@body))))

;;; Future features these functions should have:
;;;
;;;   * null-terminate
;;;   * specify target vector/string + offset
;;;   * documentation :)

(declaim (inline octets-to-string string-to-octets string-size-in-octets
                 vector-size-in-chars concatenate-strings-to-octets
                 bom-vector))

(defun octets-to-string (vector &key (start 0) end
                         (errorp (not *suppress-character-coding-errors*))
                         (encoding *default-character-encoding*))
  (check-type vector (vector (unsigned-byte 8)))
  (with-checked-simple-vector ((vector vector) (start start) (end end))
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (let ((*suppress-character-coding-errors* (not errorp))
          (mapping (lookup-mapping *string-vector-mappings* encoding)))
      (multiple-value-bind (size new-end)
          (funcall (code-point-counter mapping) vector start end -1)
        ;; TODO we could optimize ASCII here: the result should
        ;; be a simple-base-string filled using code-char...
        (let ((string (make-string size :element-type 'unicode-char)))
          (funcall (decoder mapping) vector start new-end string 0)
          string)))))

(defun bom-vector (encoding use-bom)
  (check-type use-bom (member :default t nil))
  (the simple-vector
    (if (null use-bom)
        #()
        (let ((enc (typecase encoding
                     (external-format (external-format-encoding encoding))
                     (t (get-character-encoding encoding)))))
          (if (or (eq use-bom t)
                  (and (eq use-bom :default) (enc-use-bom enc)))
              ;; VALUES avoids a "type assertion too complex to check" note.
              (values (enc-bom-encoding enc))
              #())))))

(defun string-to-octets (string &key (encoding *default-character-encoding*)
                         (start 0) end (use-bom :default)
                         (errorp (not *suppress-character-coding-errors*)))
  (declare (optimize (speed 3) (safety 2)))
  (let ((*suppress-character-coding-errors* (not errorp)))
    (etypecase string
      ;; On some lisps (e.g. clisp and ccl) all strings are BASE-STRING and all
      ;; characters are BASE-CHAR. So, only enable this optimization for
      ;; selected targets.
      #+sbcl
      (simple-base-string
       (unless end
         (setf end (length string)))
       (check-vector-bounds string start end)
       (let* ((mapping (lookup-mapping *simple-base-string-vector-mappings*
                                       encoding))
              (bom (bom-vector encoding use-bom))
              (bom-length (length bom))
              ;; OPTIMIZE: we could use the (length string) information here
              ;; because it's a simple-base-string where each character <= 127
              (result (make-array
                       (+ (the array-index
                            (funcall (the function (octet-counter mapping))
                                     string start end -1))
                          bom-length)
                       :element-type '(unsigned-byte 8))))
         (replace result bom)
         (funcall (the function (encoder mapping))
                  string start end result bom-length)
         result))
      (string
       ;; FIXME: we shouldn't really need that coercion to UNICODE-STRING
       ;; but we kind of because it's declared all over.  To avoid that,
       ;; we'd need different types for input and output strings.  Or maybe
       ;; this is not a problem; figure that out.
       (with-checked-simple-vector ((string (coerce string 'unicode-string))
                                    (start start) (end end))
         (declare (type simple-unicode-string string))
         (let* ((mapping (lookup-mapping *string-vector-mappings* encoding))
                (bom (bom-vector encoding use-bom))
                (bom-length (length bom))
                (result (make-array
                         (+ (the array-index
                              (funcall (the function (octet-counter mapping))
                                       string start end -1))
                            bom-length)
                         :element-type '(unsigned-byte 8))))
           (replace result bom)
           (funcall (the function (encoder mapping))
                    string start end result bom-length)
           result))))))

(defun concatenate-strings-to-octets (encoding &rest strings)
  "Optimized equivalent of
\(string-to-octets \(apply #'concatenate 'string strings)
                  :encoding encoding)"
  (declare (dynamic-extent strings))
  (let* ((mapping (lookup-mapping *string-vector-mappings* encoding))
         (octet-counter (octet-counter mapping))
         (vector (make-array
                  (the array-index
                    (reduce #'+ strings
                            :key (lambda (string)
                                   (funcall octet-counter
                                            string 0 (length string) -1))))
                  :element-type '(unsigned-byte 8)))
         (current-index 0))
    (declare (type array-index current-index))
    (dolist (string strings)
      (check-type string string)
      (with-checked-simple-vector ((string (coerce string 'unicode-string))
                                   (start 0) (end (length string)))
        (declare (type simple-unicode-string string))
        (incf current-index
              (funcall (encoder mapping)
                       string start end vector current-index))))
    vector))

(defun string-size-in-octets (string &key (start 0) end (max -1 maxp)
                              (errorp (not *suppress-character-coding-errors*))
                              (encoding *default-character-encoding*))
  (check-type string string)
  (with-checked-simple-vector ((string (coerce string 'unicode-string))
                               (start start) (end end))
    (declare (type simple-unicode-string string))
    (let ((mapping (lookup-mapping *string-vector-mappings* encoding))
          (*suppress-character-coding-errors* (not errorp)))
      (when maxp (assert (plusp max)))
      (funcall (octet-counter mapping) string start end max))))

(defun vector-size-in-chars (vector &key (start 0) end (max -1 maxp)
                             (errorp (not *suppress-character-coding-errors*))
                             (encoding *default-character-encoding*))
  (check-type vector (vector (unsigned-byte 8)))
  (with-checked-simple-vector ((vector vector) (start start) (end end))
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (let ((mapping (lookup-mapping *string-vector-mappings* encoding))
          (*suppress-character-coding-errors* (not errorp)))
      (when maxp (assert (plusp max)))
      (funcall (code-point-counter mapping) vector start end max))))

(declaim (notinline octets-to-string string-to-octets string-size-in-octets
                    vector-size-in-chars concatenate-strings-to-octets))
