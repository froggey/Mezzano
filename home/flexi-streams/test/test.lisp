;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS-TEST; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/test/test.lisp,v 1.39 2008/05/30 09:10:55 edi Exp $

;;; Copyright (c) 2006-2008, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :flexi-streams-test)

(defmacro with-test-suite ((test-description &key show-progress-p) &body body)
  "Defines a test suite.  Three utilities are available inside of the
body of the macro: The function FAIL, and the macros CHECK and
WITH-EXPECTED-ERROR.  FAIL, the lowest level utility, marks the test
defined by WITH-TEST-SUITE as failed.  CHECK checks whether its argument is
true, otherwise it calls FAIL. If during evaluation of the specified
expression any condition is signalled, this is also considered a
failure.  WITH-EXPECTED-ERROR executes its body and considers the test
a success if the specified error was signalled, otherwise it calls
FAIL.

WITH-TEST-SUITE prints a simple progress report if SHOW-PROGRESS-P is true."
  (with-unique-names (successp testcount)
    (with-rebinding (show-progress-p)
      `(let ((,successp t)
             (,testcount 1))
         (when (and ,show-progress-p (not (numberp ,show-progress-p)))
           (setq ,show-progress-p 1))
         (flet ((fail (format-str &rest format-args)
                  (apply #'format t format-str format-args)
                  (setq ,successp nil))
                (maybe-show-progress ()
                  (when (and ,show-progress-p (zerop (mod ,testcount ,show-progress-p)))
                    (format t ".")
                    (when (zerop (mod ,testcount (* 10 ,show-progress-p)))
                      (terpri))
                    (force-output))
                  (incf ,testcount)))
           (macrolet ((check (expression)
                        `(progn
                           (maybe-show-progress)
                           (handler-case
                               (unless ,expression
                                 (fail "~&Test ~S failed.~%" ',expression))
                             (error (c)
                               (fail "~&Test ~S failed signalling error of type ~A: ~A.~%" 
                                     ',expression (type-of c) c)))))
                      (with-expected-error ((condition-type) &body body)
                        `(progn
                           (maybe-show-progress)
                           (handler-case (progn ,@body)
                             (,condition-type () t)
                             (:no-error (&rest args)
                               (declare (ignore args))                           
                               (fail "~&Expected condition ~S not signalled.~%"
                                     ',condition-type))))))
             (format t "~&Test suite: ~S~%" ,test-description)
             ,@body))
         ,successp))))

;; LW can't indent this correctly because it's in a MACROLET
#+:lispworks
(editor:setup-indent "with-expected-error" 1 2 4)

(defconstant +buffer-size+ 8192
  "Size of buffers for COPY-STREAM* below.")

(defvar *copy-function* nil
  "Which function to use when copying from one stream to the other -
see for example COPY-FILE below.")

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*))
  "The pathname of the file \(`test.lisp') where this variable was
defined.")

#+:lispworks
(defun get-env-variable-as-directory (name)
  (lw:when-let (string (lw:environment-variable name))
    (when (plusp (length string))
      (cond ((find (char string (1- (length string))) "\\/" :test #'char=) string)
            (t (lw:string-append string "/"))))))

(defvar *tmp-dir*
  (load-time-value
    (merge-pathnames "odd-streams-test/"
                     #+:allegro (system:temporary-directory)
                     #+:lispworks (pathname (or (get-env-variable-as-directory "TEMP")
                                                (get-env-variable-as-directory "TMP")
                                                #+:win32 "C:/"
                                                #-:win32 "/tmp/"))
                     #-(or :allegro :lispworks) #p"/tmp/"))
  "The pathname of a temporary directory used for testing.")

(defvar *test-files*
  '(("kafka" (:utf8 :latin1 :cp1252))
    ("tilton" (:utf8 :ascii))
    ("hebrew" (:utf8 :latin8))
    ("russian" (:utf8 :koi8r))
    ("unicode_demo" (:utf8 :ucs2 :ucs4)))
  "A list of test files where each entry consists of the name
prefix and a list of encodings.")

(defun create-file-variants (file-name symbol)
  "For a name suffix FILE-NAME and a symbol SYMBOL denoting an
encoding returns a list of pairs where the car is a full file
name and the cdr is the corresponding external format.  This list
contains all possible variants w.r.t. to line-end conversion and
endianness."
  (let ((args (ecase symbol
                (:ascii '(:ascii))
                (:latin1 '(:latin-1))
                (:latin8 '(:hebrew))
                (:cp1252 '(:code-page :id 1252))
                (:koi8r '(:koi8-r))
                (:utf8 '(:utf-8))
                (:ucs2 '(:utf-16))
                (:ucs4 '(:utf-32))))
        (endianp (member symbol '(:ucs2 :ucs4))))
    (loop for little-endian in (if endianp '(t nil) '(t))
          for endian-suffix in (if endianp '("_le" "_be") '(""))
          nconc (loop for eol-style in '(:lf :cr :crlf)
                      collect (cons (format nil "~A_~(~A~)_~(~A~)~A.txt"
                                            file-name symbol eol-style endian-suffix)
                                    (apply #'make-external-format
                                           (append args `(:eol-style ,eol-style
                                                          :little-endian ,little-endian))))))))

(defun create-test-combinations (file-name symbols &optional simplep)
  "For a name suffix FILE-NAME and a list of symbols SYMBOLS denoting
different encodings of the corresponding file returns a list of lists
which can be used as arglists by COMPARE-FILES.  If SIMPLEP is true, a
list which can be used for the string and sequence tests below is
returned."
  (let ((file-variants (loop for symbol in symbols
                             nconc (create-file-variants file-name symbol))))
    (loop for (name-in . external-format-in) in file-variants
          when simplep
          collect (list name-in external-format-in)
          else
          nconc (loop for (name-out . external-format-out) in file-variants
                      collect (list name-in external-format-in name-out external-format-out)))))
                      
(defun file-equal (file1 file2)
  "Returns a true value iff FILE1 and FILE2 have the same
contents \(viewed as binary files)."
  (with-open-file (stream1 file1 :element-type 'octet)
    (with-open-file (stream2 file2 :element-type 'octet)
      (and (= (file-length stream1) (file-length stream2))
           (loop for byte1 = (read-byte stream1 nil nil)
                 for byte2 = (read-byte stream2 nil nil)
                 while (and byte1 byte2)
                 always (= byte1 byte2))))))

(defun copy-stream (stream-in external-format-in stream-out external-format-out)
  "Copies the contents of the binary stream STREAM-IN to the
binary stream STREAM-OUT using flexi streams - STREAM-IN is read
with the external format EXTERNAL-FORMAT-IN and STREAM-OUT is
written with EXTERNAL-FORMAT-OUT."
  (let ((in (make-flexi-stream stream-in :external-format external-format-in))
        (out (make-flexi-stream stream-out :external-format external-format-out)))
    (loop for line = (read-line in nil nil)
          while line
          do (write-line line out))))

(defun copy-stream* (stream-in external-format-in stream-out external-format-out)
  "Like COPY-STREAM, but uses READ-SEQUENCE and WRITE-SEQUENCE instead
of READ-LINE and WRITE-LINE."
  (let ((in (make-flexi-stream stream-in :external-format external-format-in))
        (out (make-flexi-stream stream-out :external-format external-format-out))
        (buffer (make-array +buffer-size+ :element-type 'char*)))
    (loop
     (let ((position (read-sequence buffer in)))
       (when (zerop position) (return))
       (write-sequence buffer out :end position)))))

(defun copy-file (path-in external-format-in path-out external-format-out direction-out direction-in)
  "Copies the contents of the file denoted by the pathname
PATH-IN to the file denoted by the pathname PATH-OUT using flexi
streams - STREAM-IN is read with the external format
EXTERNAL-FORMAT-IN and STREAM-OUT is written with
EXTERNAL-FORMAT-OUT.  The input file is opened with
the :DIRECTION keyword argument DIRECTION-IN, the output file is
opened with the :DIRECTION keyword argument DIRECTION-OUT."
  (with-open-file (in path-in
                      :element-type 'octet
                      :direction direction-in
                      :if-does-not-exist :error
                      :if-exists :overwrite)
    (with-open-file (out path-out
                         :element-type 'octet
                         :direction direction-out
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (funcall *copy-function* in external-format-in out external-format-out))))

#+:lispworks
(defun copy-file-lw (path-in external-format-in path-out external-format-out direction-out direction-in)
  "Same as COPY-FILE, but uses character streams instead of
binary streams.  Only used to test LispWorks-specific behaviour."
  (with-open-file (in path-in
                      :external-format '(:latin-1 :eol-style :lf)
                      :element-type 'base-char
                      :direction direction-in
                      :if-does-not-exist :error
                      :if-exists :overwrite)
    (with-open-file (out path-out
                         :external-format '(:latin-1 :eol-style :lf)
                         :element-type 'base-char
                         :direction direction-out
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (funcall *copy-function* in external-format-in out external-format-out))))

(defun compare-files (&key verbose)
  "Each test in this suite copies the contents of one file \(in the
`test' directory) to another file \(in a temporary directory) using
flexi streams with different external formats.  The resulting file is
compared with an existing file in the `test' directory to check if the
outcome is as expected.  Uses various variants of the :DIRECTION
keyword when opening the files.

Returns a true value iff all tests succeeded.  Prints information
about each individual comparison if VERBOSE is true."
  (with-test-suite ("Reading/writing files" :show-progress-p (not verbose))      
    (flet ((one-comparison (path-in external-format-in path-out external-format-out verbose) 
             (when verbose
               (format t "~&File ~S, using copy function ~S" (file-namestring path-in) *copy-function*)
               (format t "~&  and external formats ~S --> ~S"
                       (normalize-external-format external-format-in)
                       (normalize-external-format external-format-out)))
             (let ((full-path-in (merge-pathnames path-in *this-file*))
                   (full-path-out (ensure-directories-exist
                                   (merge-pathnames path-out *tmp-dir*)))
                   (full-path-orig (merge-pathnames path-out *this-file*)))
               (dolist (direction-out '(:output :io))
                 (dolist (direction-in '(:input :io))
                   (when verbose
                     (format t "~&...directions ~S --> ~S" direction-in direction-out))
                   (copy-file full-path-in external-format-in
                              full-path-out external-format-out
                              direction-out direction-in)
                   (check (file-equal full-path-out full-path-orig))
                   #+:lispworks
                   (progn
                   (when verbose
                     (format t "~&...directions ~S --> ~S \(LispWorks)" direction-in direction-out))
                     (copy-file-lw full-path-in external-format-in
                                   full-path-out external-format-out
                                   direction-out direction-in)
                     (check (file-equal full-path-out full-path-orig))))))))
      (loop with compare-files-args-list = (loop for (file-name symbols) in *test-files*
                                                 nconc (create-test-combinations file-name symbols))
            for *copy-function* in '(copy-stream copy-stream*)
            do (loop for (path-in external-format-in path-out external-format-out) in compare-files-args-list
                     do (one-comparison path-in external-format-in path-out external-format-out verbose))))))

(defun file-as-octet-vector (pathspec)
  "Returns the contents of the file denoted by PATHSPEC as a vector of
octets."
  (with-open-file (in pathspec :element-type 'octet)
    (let ((vector (make-array (file-length in) :element-type 'octet)))
      (read-sequence vector in)
      vector)))

(defun file-as-string (pathspec external-format)
  "Reads the contents of the file denoted by PATHSPEC using the
external format EXTERNAL-FORMAT and returns the result as a string."
  (with-open-file (in pathspec :element-type 'octet)
    (let* ((number-of-octets (file-length in))
           (in (make-flexi-stream in :external-format external-format))
           (string (make-array number-of-octets
                               :element-type #+:lispworks 'lw:simple-char
                                             #-:lispworks 'character
                               :fill-pointer t)))
      (setf (fill-pointer string) (read-sequence string in))
      string)))

(defun old-string-to-octets (string &key
                                    (external-format (make-external-format :latin1))
                                    (start 0) end)
  "The old version of STRING-TO-OCTETS.  We can use it to test
in-memory streams."
  (declare (optimize speed))
  (with-output-to-sequence (out)
    (let ((flexi (make-flexi-stream out :external-format external-format)))
      (write-string string flexi :start start :end end))))

(defun old-octets-to-string (vector &key
                                    (external-format (make-external-format :latin1))
                                    (start 0) (end (length vector)))
  "The old version of OCTETS-TO-STRING.  We can use it to test
in-memory streams."
  (declare (optimize speed))
  (with-input-from-sequence (in vector :start start :end end)
    (let ((flexi (make-flexi-stream in :external-format external-format))
          (result (make-array (- end start)
                              :element-type #+:lispworks 'lw:simple-char
                                            #-:lispworks 'character
                              :fill-pointer t)))
      (setf (fill-pointer result)
            (read-sequence result flexi))
      result)))

(defun string-tests (&key verbose)
  "Tests whether conversion from strings to octets and vice versa
works as expected.  Also tests with the old versions of the conversion
functions in order to test in-memory streams."
  (with-test-suite ("String tests" :show-progress-p (and (not verbose) 10))
    (flet ((one-string-test (pathspec external-format verbose)
             (when verbose
               (format t "~&With external format ~S:" (normalize-external-format external-format)))
             (let* ((full-path (merge-pathnames pathspec *this-file*))
                    (octets-vector (file-as-octet-vector full-path))
                    (octets-list (coerce octets-vector 'list))
                    (string (file-as-string full-path external-format)))
               (when verbose
                 (format t "~&...testing OCTETS-TO-STRING"))
               (check (string= (octets-to-string octets-vector :external-format external-format) string))
               (check (string= (octets-to-string octets-list :external-format external-format) string))
               (when verbose
                 (format t "~&...testing STRING-TO-OCTETS"))
               (check (equalp (string-to-octets string :external-format external-format) octets-vector))
               (when verbose
                 (format t "~&...testing in-memory streams"))
               (check (string= (old-octets-to-string octets-vector :external-format external-format) string))
               (check (string= (old-octets-to-string octets-list :external-format external-format) string))
               (check (equalp (old-string-to-octets string :external-format external-format) octets-vector)))))
      (loop with simple-test-args-list = (loop for (file-name symbols) in *test-files*
                                               nconc (create-test-combinations file-name symbols t))
            for (pathspec external-format) in simple-test-args-list
            do (one-string-test pathspec external-format verbose)))))
      

(defun sequence-equal (seq1 seq2)
  "Whether the two sequences have the same elements."
  (and (= (length seq1) (length seq2))
       (loop for i below (length seq1)
             always (eql (elt seq1 i) (elt seq2 i)))))

(defun sequence-tests (&key verbose)
  "Several tests to confirm that READ-SEQUENCE and WRITE-SEQUENCE
behave as expected."
  (with-test-suite ("Sequence tests" :show-progress-p (and (not verbose) 10))
    (flet ((one-sequence-test (pathspec external-format verbose)
             (when verbose
               (format t "~&With external format ~S:" (normalize-external-format external-format)))
             (let* ((full-path (merge-pathnames pathspec *this-file*))
                    (file-string (file-as-string full-path external-format))
                    (string-length (length file-string))
                    (octets (file-as-octet-vector full-path))
                    (octet-length (length octets)))
               (when (external-format-equal external-format (make-external-format :utf8))
                 (when verbose
                   (format t "~&...reading octets"))
                 #-:openmcl
                 ;; FLEXI-STREAMS puts integers into the list, but OpenMCL
                 ;; thinks they are characters...
                 (with-open-file (in full-path :element-type 'octet)
                   (let* ((in (make-flexi-stream in :external-format external-format))
                          (list (make-list octet-length)))
                     (setf (flexi-stream-element-type in) 'octet)
                     #-:clisp
                     (read-sequence list in)
                     #+:clisp
                     (ext:read-byte-sequence list in)
                     (check (sequence-equal list octets))))
                 (with-open-file (in full-path :element-type 'octet)
                   (let* ((in (make-flexi-stream in :external-format external-format))
                          (third (floor octet-length 3))
                          (half (floor octet-length 2))
                          (vector (make-array half :element-type 'octet)))
                     (check (sequence-equal (loop repeat third
                                                  collect (read-byte in))
                                            (subseq octets 0 third)))
                     (read-sequence vector in)
                     (check (sequence-equal vector (subseq octets third (+ third half)))))))
               (when verbose
                 (format t "~&...reading characters"))
               (with-open-file (in full-path :element-type 'octet)
                 (let* ((in (make-flexi-stream in :external-format external-format))
                        (string (make-string (- string-length 10) :element-type 'char*)))
                   (setf (flexi-stream-element-type in) 'octet)
                   (check (sequence-equal (loop repeat 10
                                                collect (read-char in))
                                          (subseq file-string 0 10)))
                   (read-sequence string in)
                   (check (sequence-equal string (subseq file-string 10)))))
               (with-open-file (in full-path :element-type 'octet)
                 (let* ((in (make-flexi-stream in :external-format external-format))
                        (list (make-list (- string-length 100))))
                   (check (sequence-equal (loop repeat 50
                                                collect (read-char in))
                                          (subseq file-string 0 50)))
                   #-:clisp
                   (read-sequence list in)
                   #+:clisp
                   (ext:read-char-sequence list in)
                   (check (sequence-equal list (subseq file-string 50 (- string-length 50))))
                   (check (sequence-equal (loop repeat 50
                                                collect (read-char in))
                                          (subseq file-string (- string-length 50))))))
               (with-open-file (in full-path :element-type 'octet)
                 (let* ((in (make-flexi-stream in :external-format external-format))
                        (array (make-array (- string-length 50))))
                   (check (sequence-equal (loop repeat 25
                                                collect (read-char in))
                                          (subseq file-string 0 25)))
                   #-:clisp
                   (read-sequence array in)
                   #+:clisp
                   (ext:read-char-sequence array in)
                   (check (sequence-equal array (subseq file-string 25 (- string-length 25))))
                   (check (sequence-equal (loop repeat 25
                                                collect (read-char in))
                                          (subseq file-string (- string-length 25))))))
               (let ((path-out (ensure-directories-exist (merge-pathnames pathspec *tmp-dir*))))
                 (when verbose
                   (format t "~&...writing sequences"))
                 (with-open-file (out path-out
                                      :direction :output
                                      :if-exists :supersede
                                      :element-type 'octet)
                   (let ((out (make-flexi-stream out :external-format external-format)))
                     (write-sequence octets out)))
                 (check (file-equal full-path path-out))
                 (with-open-file (out path-out
                                      :direction :output
                                      :if-exists :supersede
                                      :element-type 'octet)
                   (let ((out (make-flexi-stream out :external-format external-format)))
                     (write-sequence file-string out)))
                 (check (file-equal full-path path-out))
                 (with-open-file (out path-out
                                      :direction :output
                                      :if-exists :supersede
                                      :element-type 'octet)
                   (let ((out (make-flexi-stream out :external-format external-format)))
                     (write-sequence file-string out :end 100)
                     (write-sequence octets out
                                     :start (length (string-to-octets file-string
                                                                      :external-format external-format
                                                                      :end 100)))))
                 (check (file-equal full-path path-out))))))

      (loop with simple-test-args-list = (loop for (file-name symbols) in *test-files*
                                               nconc (create-test-combinations file-name symbols t))
            for (pathspec external-format) in simple-test-args-list
            do (one-sequence-test pathspec external-format verbose)))))

(defmacro using-values ((&rest values) &body body)
  "Executes BODY and feeds an element from VALUES to the USE-VALUE
restart each time a EXTERNAL-FORMAT-ENCODING-ERROR is signalled.
Signals an error when there are more or less
EXTERNAL-FORMAT-ENCODING-ERRORs than there are elements in VALUES."
  (with-unique-names (value-stack condition-counter)
    `(let ((,value-stack ',values)
	   (,condition-counter 0))
       (handler-bind ((external-format-encoding-error
                       #'(lambda (c)
                           (declare (ignore c)) 
                           (unless ,value-stack
                             (error "Too many encoding errors signalled, expected only ~A."
                                    ,(length values)))
                           (incf ,condition-counter)
                           (use-value (pop ,value-stack)))))
         (prog1 (progn ,@body)
           (when ,value-stack
             (error "~A encoding errors signalled, but ~A were expected."
                    ,condition-counter ,(length values))))))))

(defun accept-overlong (octets code-point)
  "Converts the `overlong' UTF-8 sequence OCTETS to using
OCTETS-TO-STRINGS, accepts the expected error with the corresponding
restart and checks that the result is CODE-POINT."
  (handler-bind ((external-format-encoding-error
                  (lambda (c)
                    (declare (ignore c))
                    (invoke-restart 'accept-overlong-sequence))))
    (string= (octets-to-string octets :external-format :utf-8)
             (string (code-char code-point)))))

(defun read-flexi-line (sequence external-format)
  "Creates and returns a string from the octet sequence SEQUENCE using
the external format EXTERNAL-FORMAT."
  (with-input-from-sequence (in sequence)
    (setq in (make-flexi-stream in :external-format external-format))
    (read-line in)))

(defun read-flexi-line* (sequence external-format)
  "Like READ-FLEXI-LINE but uses OCTETS-TO-STRING internally."
  (octets-to-string sequence :external-format external-format))

(defun error-handling-tests (&key verbose)
  "Tests several possible errors and how they are handled."
  (with-test-suite ("Testing error handling" :show-progress-p (not verbose))
    (macrolet ((want-encoding-error (input format)
                 `(with-expected-error (external-format-encoding-error)
                    (read-flexi-line* ,input ,format))))
      (when verbose
        (format t "~&\"Overlong\" UTF-8 sequences"))
      (want-encoding-error #(#b11000000 #b10000000) :utf-8)
      (want-encoding-error #(#b11000001 #b10000000) :utf-8)
      (want-encoding-error #(#b11100000 #b10011111 #b10000000) :utf-8)
      (want-encoding-error #(#b11110000 #b10001111 #b10000000 #b10000000) :utf-8)
      (check (accept-overlong #(#b11000000 #b10000000) #b00000000))
      (check (accept-overlong #(#b11000001 #b10000000) #b01000000))
      (check (accept-overlong #(#b11100000 #b10011111 #b10000000) #b011111000000))
      (check (accept-overlong #(#b11110000 #b10001111 #b10000000 #b10000000)
                              #b1111000000000000))
      (when verbose
        (format t "~&Invalid lead octets in UTF-8"))
      (want-encoding-error #(#b11111000) :utf-8)
      (want-encoding-error #(#b11111001) :utf-8)
      (want-encoding-error #(#b11111100) :utf-8)
      (want-encoding-error #(#b11111101) :utf-8)
      (want-encoding-error #(#b11111110) :utf-8)
      (want-encoding-error #(#b11111111) :utf-8)
      (when verbose
        (format t "~&Illegal code points"))
      (want-encoding-error #(#x00 #x00 #x11 #x00) :utf-32le)
      (want-encoding-error #(#x00 #xd8) :utf-16le)
      (want-encoding-error #(#xff #xdf) :utf-16le))
    (macrolet ((want-encoding-error (input format)
                 `(with-expected-error (external-format-encoding-error)
                    (read-flexi-line* ,input ,format))))                 
      (when verbose
        (format t "~&UTF-8 sequences which are too short"))
      (want-encoding-error #(#xe4 #xf6 #xfc) :utf8)
      (want-encoding-error #(#xc0) :utf8)
      (want-encoding-error #(#xe0 #xff) :utf8)
      (want-encoding-error #(#xf0 #xff #xff) :utf8)
      (when verbose
        (format t "~&UTF-16 sequences with an odd number of octets"))
      (want-encoding-error #(#x01) :utf-16le)
      (want-encoding-error #(#x01 #x01 #x01) :utf-16le)
      (want-encoding-error #(#x01) :utf-16be)
      (want-encoding-error #(#x01 #x01 #x01) :utf-16be)
      (when verbose
        (format t "~&Missing words in UTF-16"))
      (want-encoding-error #(#x01 #xd8) :utf-16le)
      (want-encoding-error #(#xd8 #x01) :utf-16be)
      (when verbose
        (format t "~&Missing octets in UTF-32"))
      (want-encoding-error #(#x01) :utf-32le)
      (want-encoding-error #(#x01 #x01) :utf-32le)
      (want-encoding-error #(#x01 #x01 #x01) :utf-32le)
      (want-encoding-error #(#x01 #x01 #x01 #x01 #x01) :utf-32le)
      (want-encoding-error #(#x01) :utf-32be)
      (want-encoding-error #(#x01 #x01) :utf-32be)
      (want-encoding-error #(#x01 #x01 #x01) :utf-32be)
      (want-encoding-error #(#x01 #x01 #x01 #x01 #x01) :utf-32be))
    (when verbose
      (format t "~&Handling of EOF in the middle of CRLF"))
    (check (string= #.(string #\Return)
                    (read-flexi-line `(,(char-code #\Return)) '(:ascii :eol-style :crlf))))
    (let ((*substitution-char* #\?))
      (when verbose
        (format t "~&Fixed substitution character #\?")
        (format t "~&:ASCII doesn't have characters with char codes > 127"))
      (check (string= "a??" (read-flexi-line `(,(char-code #\a) 128 200) :ascii)))
      (check (string= "a??" (read-flexi-line* `#(,(char-code #\a) 128 200) :ascii)))
      (when verbose
        (format t "~&:WINDOWS-1253 doesn't have a characters with codes 170 and 210"))
      (check (string= "a??" (read-flexi-line `(,(char-code #\a) 170 210) :windows-1253)))
      (check (string= "a??" (read-flexi-line* `#(,(char-code #\a) 170 210) :windows-1253)))
      (when verbose
        (format t "~&Not a valid UTF-8 sequence"))
      (check (string= "??" (read-flexi-line '(#xe4 #xf6 #xfc) :utf8))))
    (let ((*substitution-char* nil))
      (when verbose
        (format t "~&Variable substitution using USE-VALUE restart")
        (format t "~&:ASCII doesn't have characters with char codes > 127"))
      (check (string= "abc" (using-values (#\b #\c)
                              (read-flexi-line `(,(char-code #\a) 128 200) :ascii))))
      (check (string= "abc" (using-values (#\b #\c)
                              (read-flexi-line* `#(,(char-code #\a) 128 200) :ascii))))
      (when verbose
        (format t "~&:WINDOWS-1253 doesn't have a characters with codes 170 and 210"))
      (check (string= "axy" (using-values (#\x #\y)
                              (read-flexi-line `(,(char-code #\a) 170 210) :windows-1253))))
      (check (string= "axy" (using-values (#\x #\y)
                              (read-flexi-line* `#(,(char-code #\a) 170 210) :windows-1253))))
      (when verbose
        (format t "~&Not a valid UTF-8 sequence"))
      (check (string= "QW" (using-values (#\Q #\W) (read-flexi-line '(#xe4 #xf6 #xfc) :utf8))))
      (when verbose
        (format t "~&UTF-8 can't start neither with #b11111110 nor with #b11111111"))
      (check (string= "QW" (using-values (#\Q #\W) (read-flexi-line '(#b11111110 #b11111111) :utf8))))
      (when verbose
        (format t "~&Only one octet in UTF-16 sequence"))
      (check (string= "E" (using-values (#\E) (read-flexi-line '(#x01) :utf-16le))))
      (when verbose
        (format t "~&Two octets in UTF-16, but value of resulting word suggests that another word follows"))
      (check (string= "R" (using-values (#\R) (read-flexi-line '(#x01 #xd8) :utf-16le))))
      (when verbose
        (format t "~&The second word must fit into the [#xdc00; #xdfff] interval, but it is #xdbff"))
      (check (string= "T" (using-values (#\T) (read-flexi-line '(#x01 #xd8 #xff #xdb) :utf-16le))))
      (check (string= "T" (using-values (#\T) (read-flexi-line* #(#x01 #xd8 #xff #xdb) :utf-16le))))
      (when verbose
        (format t "~&The same as for little endian above, but using inverse order of bytes in words"))
      (check (string= "E" (using-values (#\E) (read-flexi-line '(#x01) :utf-16be))))
      (check (string= "R" (using-values (#\R) (read-flexi-line '(#xd8 #x01) :utf-16be))))
      (check (string= "T" (using-values (#\T) (read-flexi-line '(#xd8 #x01 #xdb #xff) :utf-16be))))
      (check (string= "T" (using-values (#\T) (read-flexi-line* #(#xd8 #x01 #xdb #xff) :utf-16be))))
      (when verbose
        (format t "~&EOF in the middle of a 4-octet sequence in UTF-32"))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01) :utf-32le))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01 #x01) :utf-32le))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01 #x01 #x01) :utf-32le))))
      (check (string= "aY" (using-values (#\Y)
                             (read-flexi-line `(,(char-code #\a) #x00 #x00 #x00 #x01) :utf-32le))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01) :utf-32be))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01 #x01) :utf-32be))))
      (check (string= "Y" (using-values (#\Y) (read-flexi-line '(#x01 #x01 #x01) :utf-32be))))
      (check (string= "aY" (using-values (#\Y)
                             (read-flexi-line `(#x00 #x00 #x00 ,(char-code #\a) #x01) :utf-32be)))))))

(defun unread-char-tests (&key verbose)
  "Tests whether UNREAD-CHAR behaves as expected."
  (with-test-suite ("UNREAD-CHAR behaviour." :show-progress-p (and (not verbose) 100))
    (flet ((test-one-file (file-name external-format)
             (when verbose
               (format t "~&  ...and external format ~A" (normalize-external-format external-format)))
             (with-open-file (in (merge-pathnames file-name *this-file*)
                                 :element-type 'flex:octet)
               (let ((in (make-flexi-stream in :external-format external-format)))
                 (loop repeat 300
                       for char = (read-char in)
                       do (unread-char char in)
                          (check (char= (read-char in) char)))))))
      (loop for (file-name symbols) in *test-files*
            when verbose
            do (format t "~&With file ~S" file-name)
            do (loop for symbol in symbols
                     do (loop for (file-name . external-format) in (create-file-variants file-name symbol)
                              do (test-one-file file-name external-format)))))))

(defun column-tests (&key verbose)
  (with-test-suite ("STREAM-LINE-COLUMN tests" :show-progress-p (not verbose))
    (let* ((binary-stream (flexi-streams:make-in-memory-output-stream))
           (stream (flexi-streams:make-flexi-stream binary-stream :external-format :iso-8859-1)))
      (write-sequence "hello" stream)
      (format stream "~12Tworld")
      (finish-output stream)
      (check (string= "hello       world"
                      (flexi-streams:octets-to-string
                       (flexi-streams::vector-stream-vector binary-stream)
                       :external-format :iso-8859-1)))
      (terpri stream)
      (check (= 0 (flexi-stream-column stream)))
      (write-sequence "abc" stream)
      (check (= 3 (flexi-stream-column stream)))
      (terpri stream)
      (check (= 0 (flexi-stream-column stream))))))

(defun make-external-format-tests (&key verbose)
  (with-test-suite ("MAKE-EXTERNAL-FORMAT tests" :show-progress-p (not verbose))
    (flet ((make-case (real-name &key id name)
           (list real-name
                 :id id
                 :input-names (list name (string-upcase name) (string-downcase name)))))
      (let ((cases (append '((:utf-8 :id nil
                                     :input-names (:utf8 :utf-8 "utf8" "utf-8" "UTF8" "UTF-8")))
                           (loop for (name . real-name) in +name-map+
                                 unless (member :code-page (list name real-name))
                                   append (list (make-case real-name :name name)
                                                (make-case real-name :name real-name)))
                           (loop for (name . definition) in +shortcut-map+
                                 for key = (car definition)
                                 for id = (getf (cdr definition) :id)
                                 for expected = (or (cdr (assoc key +name-map+)) key)
                                 collect (make-case expected :id id :name name)))))

        (loop for (expected-name . kwargs) in cases
              for id = (getf kwargs :id)
              for input-names = (getf kwargs :input-names)
              do (loop for name in input-names
                       for ext-format = (make-external-format name)
                       do (check (eq (flex:external-format-name ext-format) expected-name))
                       when id
                         do (check (= (flex:external-format-id ext-format) id))))))

    (let ((error-cases '("utf-8 " " utf-8" "utf8 " " utf8" "utf89" :utf89 utf89 :code-page nil)))
      (loop for input-name in error-cases
            do (with-expected-error (external-format-error)
                 (make-external-format input-name))))))

(defun run-all-tests (&key verbose)
  "Runs all tests for FLEXI-STREAMS and returns a true value iff all
tests succeeded.  VERBOSE is interpreted by the individual test suites
above."
  (let ((successp t))
    (macrolet ((run-test-suite (&body body)
                 `(unless (progn ,@body)
                    (setq successp nil))))
      (run-test-suite (compare-files :verbose verbose))
      (run-test-suite (string-tests :verbose verbose))
      (run-test-suite (sequence-tests :verbose verbose))
      (run-test-suite (error-handling-tests :verbose verbose))
      (run-test-suite (unread-char-tests :verbose verbose))
      (run-test-suite (column-tests :verbose verbose))
      (run-test-suite (make-external-format-tests :verbose verbose))
      (format t "~2&~:[Some tests failed~;All tests passed~]." successp)
      successp)))
            
