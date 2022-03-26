;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; tests.lisp --- Unit and regression tests for Babel.
;;;
;;; Copyright (C) 2007-2009, Luis Oliveira  <loliveira@common-lisp.net>
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

(in-package #:cl-user)
(defpackage #:babel-tests
  (:use #:common-lisp #:babel #:babel-encodings #:hu.dwim.stefil)
  (:import-from #:alexandria #:ignore-some-conditions)
  (:export #:run))
(in-package #:babel-tests)

(defun indented-format (level stream format-control &rest format-arguments)
  (let ((line-prefix (make-string level :initial-element #\Space)))
    (let ((output (format nil "~?~%" format-control format-arguments)))
      (with-input-from-string (s output)
        (loop for line = (read-line s nil nil) until (null line)
              do (format stream "~A~A~%" line-prefix line))))))

;; adapted from https://github.com/luismbo/stefil/blob/master/source/suite.lisp
(defun describe-failed-tests (&key (result *last-test-result*) (stream t))
  "Prints out a report for RESULT in STREAM.

RESULT defaults to `*last-test-result*' and STREAM defaults to t"
  (let ((descs (hu.dwim.stefil::failure-descriptions-of result)))
    (cond ((zerop (length descs))
           (format stream "~&~%[no failures!]"))
          (t
           (format stream "~&~%Test failures:~%")
           (dotimes (i (length descs))
             (let ((desc (aref descs i))
                   format-control format-arguments)
               ;; XXX: most of Stefil's conditions specialise DESCRIBE-OBJECT
               ;; with nice human-readable messages. We should add any missing
               ;; ones (like UNEXPECTED-ERROR) and ditch this code.
               (etypecase desc
                 (hu.dwim.stefil::unexpected-error
                  (let ((c (hu.dwim.stefil::condition-of desc)))
                    (typecase c
                      (simple-condition
                       (setf format-control (simple-condition-format-control c))
                       (setf format-arguments
                             (simple-condition-format-arguments c)))
                      (t
                       (setf format-control "~S"
                             format-arguments (list c))))))
                 (hu.dwim.stefil::failed-assertion
                  (setf format-control (hu.dwim.stefil::format-control-of desc)
                        format-arguments (hu.dwim.stefil::format-arguments-of desc)))
                 (hu.dwim.stefil::missing-condition
                  (setf format-control "~A"
                        format-arguments (list (with-output-to-string (stream)
                                                 (describe desc stream)))))
                 (null
                  (setf format-control "Test succeeded!")))
               (format stream "~%Failure ~A: ~A when running ~S~%~%"
                       (1+ i)
                       (type-of desc)
                       (hu.dwim.stefil::name-of (hu.dwim.stefil::test-of (first (hu.dwim.stefil::test-context-backtrace-of desc)))))
               (indented-format 4 stream "~?" format-control format-arguments)))))))

(defun run ()
  (let ((test-run (without-debugging (babel-tests))))
    (print test-run)
    (describe-failed-tests :result test-run)
    (values (zerop (length (hu.dwim.stefil::failure-descriptions-of test-run)))
            test-run)))

(defsuite* (babel-tests :in root-suite))

(defun ub8v (&rest contents)
  (make-array (length contents) :element-type '(unsigned-byte 8)
              :initial-contents contents))

(defun make-ub8-vector (size)
  (make-array size :element-type '(unsigned-byte 8)
              :initial-element 0))

(defmacro returns (form &rest values)
  "Asserts, through EQUALP, that FORM returns VALUES."
  `(is (equalp (multiple-value-list ,form) (list ,@values))))

(defmacro defstest (name form &body return-values)
  "Similar to RT's DEFTEST."
  `(deftest ,name ()
     (returns ,form ,@(mapcar (lambda (x) `',x) return-values))))

(defun fail (control-string &rest arguments)
  (hu.dwim.stefil::record/failure 'hu.dwim.stefil::failed-assertion
                                  :format-control control-string
                                  :format-arguments arguments))

(defun expected (expected &key got)
  (fail "expected ~A, got ~A instead" expected got))

(enable-sharp-backslash-syntax)

;;;; Simple tests using ASCII

(defstest enc.ascii.1
    (string-to-octets "abc" :encoding :ascii)
  #(97 98 99))

(defstest enc.ascii.2
    (string-to-octets (string #\uED) :encoding :ascii :errorp nil)
  #(#x1a))

(deftest enc.ascii.3 ()
  (handler-case
      (string-to-octets (string #\uED) :encoding :ascii :errorp t)
    (character-encoding-error (c)
      (is (eql 0 (character-coding-error-position c)))
      (is (eq :ascii (character-coding-error-encoding c)))
      (is (eql #xed (character-encoding-error-code c))))
    (:no-error (result)
      (expected 'character-encoding-error :got result))))

(defstest dec.ascii.1
    (octets-to-string (ub8v 97 98 99) :encoding :ascii)
  "abc")

(deftest dec.ascii.2 ()
  (handler-case
      (octets-to-string (ub8v 97 128 99) :encoding :ascii :errorp t)
    (character-decoding-error (c)
      (is (equalp #(128) (character-decoding-error-octets c)))
      (is (eql 1 (character-coding-error-position c)))
      (is (eq :ascii (character-coding-error-encoding c))))
    (:no-error (result)
      (expected 'character-decoding-error :got result))))

(defstest dec.ascii.3
    (octets-to-string (ub8v 97 255 98 99) :encoding :ascii :errorp nil)
  #(#\a #\Sub #\b #\c))

(defstest oct-count.ascii.1
    (string-size-in-octets "abc" :encoding :ascii)
  3 3)

(defstest char-count.ascii.1
    (vector-size-in-chars (ub8v 97 98 99) :encoding :ascii)
  3 3)

;;;; UTF-8

(defstest char-count.utf-8.1
    ;; "ni hao" in hanzi with the last octet missing
    (vector-size-in-chars (ub8v 228 189 160 229 165) :errorp nil)
  2 5)

(deftest char-count.utf-8.2 ()
    ;; same as above with the last 2 octets missing
  (handler-case
      (vector-size-in-chars (ub8v 228 189 160 229) :errorp t)
    (end-of-input-in-character (c)
      (is (equalp #(229) (character-decoding-error-octets c)))
      (is (eql 3 (character-coding-error-position c)))
      (is (eq :utf-8 (character-coding-error-encoding c))))
    (:no-error (result)
      (expected 'end-of-input-in-character :got result))))

;;; Lispworks bug?
;; #+lispworks
;; (pushnew 'dec.utf-8.1 rtest::*expected-failures*)

(defstest dec.utf-8.1
    (octets-to-string (ub8v 228 189 160 229) :errorp nil)
  #(#\u4f60 #\ufffd))

(deftest dec.utf-8.2 ()
  (handler-case
      (octets-to-string (ub8v 228 189 160 229) :errorp t)
    (end-of-input-in-character (c)
      (is (equalp #(229) (character-decoding-error-octets c)))
      (is (eql 3 (character-coding-error-position c)))
      (is (eq :utf-8 (character-coding-error-encoding c))))
    (:no-error (result)
      (expected 'end-of-input-in-character :got result))))

;;;; UTF-16

;;; Test that the BOM is not being counted as a character.
(deftest char-count.utf-16.bom ()
  (is (eql (vector-size-in-chars (ub8v #xfe #xff #x00 #x55 #x00 #x54 #x00 #x46)
                                 :encoding :utf-16)
           3))
  (is (eql (vector-size-in-chars (ub8v #xff #xfe #x00 #x55 #x00 #x54 #x00 #x46)
                                 :encoding :utf-16)
           3)))

;;;; UTF-32

;;; RT: check that UTF-32 characters without a BOM are treated as
;;; little-endian.
(deftest endianness.utf-32.no-bom ()
  (is (string= "a" (octets-to-string (ub8v 0 0 0 97) :encoding :utf-32))))

;;;; MORE TESTS

(defparameter *standard-characters*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~")

;;; Testing consistency by encoding and decoding a simple string for
;;; all character encodings.
(deftest rw-equiv.1 ()
  (let ((compatible-encodings (remove :ebcdic-international (list-character-encodings))))
    (dolist (*default-character-encoding* compatible-encodings)
      (let ((octets (string-to-octets *standard-characters*)))
        (is (string= (octets-to-string octets) *standard-characters*))))))

;;; FIXME: assumes little-endianness.  Easily fixable when we
;;; implement the BE and LE variants of :UTF-16.
(deftest concatenate-strings-to-octets-equiv.1 ()
  (let ((foo (octets-to-string (ub8v 102 195 186 195 186)
                               :encoding :utf-8))
        (bar (octets-to-string (ub8v 98 195 161 114)
                               :encoding :utf-8)))
    ;; note: FOO and BAR are not ascii
    (is (equalp (concatenate-strings-to-octets :utf-8 foo bar)
                (ub8v 102 195 186 195 186 98 195 161 114)))
    (is (equalp (concatenate-strings-to-octets :utf-16 foo bar)
                (ub8v 102 0 250 0 250 0 98 0 225 0 114 0)))))

;;;; Testing against files generated by GNU iconv.

(defun test-file (name type)
  (let ((sys-pn (truename
                 (asdf:system-definition-pathname
                  (asdf:find-system 'babel-tests)))))
    (make-pathname :name name :type type
                   :directory (append (pathname-directory sys-pn)
                                      '("tests"))
                   :defaults sys-pn)))

(defun read-test-file (name type)
  (with-open-file (in (test-file name type) :element-type '(unsigned-byte 8))
    (let* ((data (loop for byte = (read-byte in nil nil)
                       until (null byte) collect byte)))
      (make-array (length data) :element-type '(unsigned-byte 8)
                  :initial-contents data))))

(deftest test-encoding (enc &optional input-enc-name)
  (let* ((*default-character-encoding* enc)
         (enc-name (string-downcase (symbol-name enc)))
         (utf8-octets (read-test-file enc-name "txt-utf8"))
         (foo-octets (read-test-file (or input-enc-name enc-name) "txt"))
         (utf8-string (octets-to-string utf8-octets :encoding :utf-8 :errorp t))
         (foo-string (octets-to-string foo-octets :errorp t)))
    (is (string= utf8-string foo-string))
    (is (= (length foo-string) (vector-size-in-chars foo-octets :errorp t)))
    (unless (member enc '(:utf-16 :utf-32))
      ;; FIXME: skipping UTF-16 and UTF-32 because of the BOMs and
      ;; because the input might not be in native-endian order so the
      ;; comparison will fail there.
      (let ((new-octets (string-to-octets foo-string :errorp t)))
        (is (equalp new-octets foo-octets))
        (is (eql (length foo-octets)
                 (string-size-in-octets foo-string :errorp t)))))))

(deftest iconv-test ()
  (dolist (enc '(:ascii :ebcdic-us :utf-8 :utf-16 :utf-32))
    (case enc
      (:utf-16 (test-encoding :utf-16 "utf-16-with-le-bom"))
      (:utf-32 (test-encoding :utf-32 "utf-32-with-le-bom")))
    (test-encoding enc)))

;;; RT: accept encoding objects in LOOKUP-MAPPING etc.
(defstest encoding-objects.1
    (string-to-octets "abc" :encoding (get-character-encoding :ascii))
  #(97 98 99))

(defmacro with-sharp-backslash-syntax (&body body)
  `(let ((*readtable* (copy-readtable *readtable*)))
     (set-sharp-backslash-syntax-in-readtable)
     ,@body))

(defstest sharp-backslash.1
    (with-sharp-backslash-syntax
      (loop for string in '("#\\a" "#\\u" "#\\ued")
            collect (char-code (read-from-string string))))
  (97 117 #xed))

(deftest sharp-backslash.2 ()
  (signals reader-error (with-sharp-backslash-syntax
                          (read-from-string "#\\u12zz"))))

(deftest test-read-from-string (string object position)
  "Test that (read-from-string STRING) returns values OBJECT and POSITION."
  (multiple-value-bind (obj pos)
      (read-from-string string)
    (is (eql object obj))
    (is (eql position pos))))

;;; RT: our #\ reader didn't honor *READ-SUPPRESS*.
(deftest sharp-backslash.3 ()
  (with-sharp-backslash-syntax
    (let ((*read-suppress* t))
      (test-read-from-string "#\\ujunk" nil 7)
      (test-read-from-string "#\\u12zz" nil 7))))

;;; RT: the slow implementation of with-simple-vector was buggy.
(defstest string-to-octets.1
    (code-char (aref (string-to-octets "abc" :start 1 :end 2) 0))
  #\b)

(defstest simple-base-string.1
    (string-to-octets (coerce "abc" 'base-string) :encoding :ascii)
  #(97 98 99))

;;; For now, disable this tests for Lisps that are strict about
;;; non-character code points. In the future, simply mark them as
;;; expected failures.
#-(or abcl ccl)
(progn
  (defstest utf-8b.1
      (string-to-octets (coerce #(#\a #\b #\udcf0) 'unicode-string)
                        :encoding :utf-8b)
    #(97 98 #xf0))

  #+#:temporarily-disabled
  (defstest utf-8b.2
      (octets-to-string (ub8v 97 98 #xcd) :encoding :utf-8b)
    #(#\a #\b #\udccd))

  (defstest utf-8b.3
      (octets-to-string (ub8v 97 #xf0 #xf1 #xff #x01) :encoding :utf-8b)
    #(#\a #\udcf0 #\udcf1 #\udcff #\udc01))

  (deftest utf-8b.4 ()
    (let* ((octets (coerce (loop repeat 8192 collect (random (+ #x82)))
                           '(array (unsigned-byte 8) (*))))
           (string (octets-to-string octets :encoding :utf-8b)))
      (is (equalp octets (string-to-octets string :encoding :utf-8b))))))

;;; The following tests have been adapted from SBCL's
;;; tests/octets.pure.lisp file.

(deftest ensure-roundtrip-ascii ()
  (let ((octets (make-ub8-vector 128)))
    (dotimes (i 128)
      (setf (aref octets i) i))
    (let* ((str (octets-to-string octets :encoding :ascii))
           (oct2 (string-to-octets str :encoding :ascii)))
      (is (= (length octets) (length oct2)))
      (is (every #'= octets oct2)))))

(deftest test-8bit-roundtrip (enc)
  (let ((octets (make-ub8-vector 256)))
    (dotimes (i 256)
      (setf (aref octets i) i))
    (let* ((str (octets-to-string octets :encoding enc)))
      ;; remove the undefined code-points because they translate
      ;; to #xFFFD and string-to-octets raises an error when
      ;; encoding #xFFFD
      (multiple-value-bind (filtered-str filtered-octets)
          (let ((s (make-array 0 :element-type 'character
                               :adjustable t :fill-pointer 0))
                (o (make-array 0 :element-type '(unsigned-byte 16)
                               :adjustable t :fill-pointer 0)))
            (loop for i below 256
                  for c = (aref str i)
                  when (/= (char-code c) #xFFFD)
                  do (vector-push-extend c s)
                     (vector-push-extend (aref octets i) o))
            (values s o))
        (let ((oct2 (string-to-octets filtered-str :encoding enc)))
          (is (eql (length filtered-octets) (length oct2)))
          (is (every #'eql filtered-octets oct2)))))))

(defparameter *iso-8859-charsets*
  '(:iso-8859-1 :iso-8859-2 :iso-8859-3 :iso-8859-4 :iso-8859-5 :iso-8859-6
    :iso-8859-7 :iso-8859-8 :iso-8859-9 :iso-8859-10 :iso-8859-11 :iso-8859-13
    :iso-8859-14 :iso-8859-15 :iso-8859-16))

;;; Don't actually see what comes out, but there shouldn't be any
;;; errors.
(deftest iso-8859-roundtrip-no-checking ()
  (loop for enc in *iso-8859-charsets* do (test-8bit-roundtrip enc)))

(deftest ensure-roundtrip-latin ()
  (loop for enc in '(:latin1 :latin9) do (test-8bit-roundtrip enc)))

;;; Latin-9 chars; the previous test checked roundtrip from
;;; octets->char and back, now test that the latin-9 characters did in
;;; fact appear during that trip.
(deftest ensure-roundtrip-latin9 ()
  (let ((l9c (map 'string #'code-char '(8364 352 353 381 382 338 339 376))))
    (is (string= (octets-to-string (string-to-octets l9c :encoding :latin9)
                                   :encoding :latin9)
                 l9c))))

;; Expected to fail on Lisps that are strict about non-character code
;; points. Mark this as an expected failure when Stefil supports such
;; a feature.
#-(or abcl ccl)
(deftest code-char-nilness ()
  (is (loop for i below unicode-char-code-limit
            never (null (code-char i)))))

(deftest test-unicode-roundtrip (enc)
  (let ((string (make-string unicode-char-code-limit)))
    (dotimes (i unicode-char-code-limit)
      (setf (char string i)
            (if (or (<= #xD800 i #xDFFF)
                    (<= #xFDD0 i #xFDEF)
                    (eql (logand i #xFFFF) #xFFFF)
                    (eql (logand i #xFFFF) #xFFFE))
                #\? ; don't try to encode non-characters.
                (code-char i))))
    (let ((string2 (octets-to-string
                    (string-to-octets string :encoding enc :errorp t)
                    :encoding enc :errorp t)))
      (is (eql (length string2) (length string)))
      (is (string= string string2)))))

(deftest ensure-roundtrip.utf8 ()
  (test-unicode-roundtrip :utf-8))

(deftest ensure-roundtrip.utf16 ()
  (test-unicode-roundtrip :utf-16))

(deftest ensure-roundtrip.utf32 ()
  (test-unicode-roundtrip :utf-32))

#+sbcl
(progn
  (deftest test-encode-against-sbcl (enc)
    (let ((string (make-string unicode-char-code-limit)))
      (dotimes (i unicode-char-code-limit)
        (setf (char string i) (code-char i)))
      (loop for ch across string
            for babel = (string-to-octets (string ch) :encoding enc)
            for sbcl = (sb-ext:string-to-octets (string ch)
                                                :external-format enc)
            do (is (equalp babel sbcl)))))

  ;; not run automatically because it's a bit slow (1114112 assertions)
  (deftest (test-encode-against-sbcl.utf-8 :auto-call nil) ()
    (test-encode-against-sbcl :utf-8)))

(deftest non-ascii-bytes ()
  (let ((octets (make-array 128
                            :element-type '(unsigned-byte 8)
                            :initial-contents (loop for i from 128 below 256
                                                    collect i))))
    (is (string= (octets-to-string octets :encoding :ascii :errorp nil)
                 (make-string 128 :initial-element #\Sub)))))

(deftest non-ascii-chars ()
  (let ((string (make-array 128
                            :element-type 'character
                            :initial-contents (loop for i from 128 below 256
                                                    collect (code-char i)))))
    (is (equalp (string-to-octets string :encoding :ascii :errorp nil)
                (make-array 128 :initial-element (char-code #\Sub))))))

;;;; The following UTF-8 decoding tests are adapted from
;;;; <http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt>.

(deftest utf8-decode-test (octets expected-results expected-errors)
  (let ((string (octets-to-string (coerce octets '(vector (unsigned-byte 8) *))
                                  :encoding :utf-8 :errorp nil)))
    (is (string= expected-results string))
    (is (= (count #\ufffd string) expected-errors))))

(deftest utf8-decode-tests (octets expected-results)
  (let ((expected-errors (count #\? expected-results))
        (expected-results (substitute #\ufffd #\? expected-results)))
    (utf8-decode-test octets expected-results expected-errors)
    (utf8-decode-test (concatenate 'vector '(34) octets '(34))
                      (format nil "\"~A\"" expected-results)
                      expected-errors)))

(deftest utf8-too-big-characters ()
  (utf8-decode-tests #(#xf4 #x90 #x80 #x80) "?")           ; #x110000
  (utf8-decode-tests #(#xf7 #xbf #xbf #xbf) "?")           ; #x1fffff
  (utf8-decode-tests #(#xf8 #x88 #x80 #x80 #x80) "?")      ; #x200000
  (utf8-decode-tests #(#xfb #xbf #xbf #xbf #xbf) "?")      ; #x3ffffff
  (utf8-decode-tests #(#xfc #x84 #x80 #x80 #x80 #x80) "?") ; #x4000000e
  (utf8-decode-tests #(#xfd #xbf #xbf #xbf #xbf #xbf) "?")) ; #x7fffffff

(deftest utf8-unexpected-continuation-bytes ()
  (utf8-decode-tests #(#x80) "?")
  (utf8-decode-tests #(#xbf) "?")
  (utf8-decode-tests #(#x80 #xbf) "??")
  (utf8-decode-tests #(#x80 #xbf #x80) "???")
  (utf8-decode-tests #(#x80 #xbf #x80 #xbf) "????")
  (utf8-decode-tests #(#x80 #xbf #x80 #xbf #x80) "?????")
  (utf8-decode-tests #(#x80 #xbf #x80 #xbf #x80 #xbf) "??????")
  (utf8-decode-tests #(#x80 #xbf #x80 #xbf #x80 #xbf #x80) "???????"))

;;; All 64 continuation bytes in a row.
(deftest utf8-continuation-bytes ()
  (apply #'utf8-decode-tests
         (loop for i from #x80 to #xbf
               collect i into bytes
               collect #\? into chars
               finally (return (list bytes
                                     (coerce chars 'string))))))

(deftest utf8-lonely-start-characters ()
  (flet ((lsc (first last)
           (apply #'utf8-decode-tests
                  (loop for i from first to last
                        nconc (list i 32) into bytes
                        nconc (list #\? #\Space) into chars
                        finally (return (list bytes (coerce chars 'string)))))
           (apply #'utf8-decode-tests
                  (loop for i from first to last
                        collect i into bytes
                        collect #\? into chars
                        finally (return
                                  (list bytes (coerce chars 'string)))))))
    (lsc #xc0 #xdf)                     ; 2-byte sequence start chars
    (lsc #xe0 #xef)                     ; 3-byte
    (lsc #xf0 #xf7)                     ; 4-byte
    (lsc #xf8 #xfb)                     ; 5-byte
    (lsc #xfc #xfd)))                   ; 6-byte

;;; Otherwise incomplete sequences (last continuation byte missing)
(deftest utf8-incomplete-sequences ()
  (utf8-decode-tests #0=#(#xc0) "?")
  (utf8-decode-tests #1=#(#xe0 #x80) "?")
  (utf8-decode-tests #2=#(#xf0 #x80 #x80) "?")
  (utf8-decode-tests #3=#(#xf8 #x80 #x80 #x80) "?")
  (utf8-decode-tests #4=#(#xfc #x80 #x80 #x80 #x80) "?")
  (utf8-decode-tests #5=#(#xdf) "?")
  (utf8-decode-tests #6=#(#xef #xbf) "?")
  (utf8-decode-tests #7=#(#xf7 #xbf #xbf) "?")
  (utf8-decode-tests #8=#(#xfb #xbf #xbf #xbf) "?")
  (utf8-decode-tests #9=#(#xfd #xbf #xbf #xbf #xbf) "?")
  ;; All ten previous tests concatenated
  (utf8-decode-tests (concatenate 'vector
                                  #0# #1# #2# #3# #4# #5# #6# #7# #8# #9#)
                     "??????????"))

(deftest utf8-random-impossible-bytes ()
  (utf8-decode-tests #(#xfe) "?")
  (utf8-decode-tests #(#xff) "?")
  (utf8-decode-tests #(#xfe #xfe #xff #xff) "????"))

(deftest utf8-overlong-sequences-/ ()
  (utf8-decode-tests #(#xc0 #xaf) "?")
  (utf8-decode-tests #(#xe0 #x80 #xaf) "?")
  (utf8-decode-tests #(#xf0 #x80 #x80 #xaf) "?")
  (utf8-decode-tests #(#xf8 #x80 #x80 #x80 #xaf) "?")
  (utf8-decode-tests #(#xfc #x80 #x80 #x80 #x80 #xaf) "?"))

(deftest utf8-overlong-sequences-rubout ()
  (utf8-decode-tests #(#xc1 #xbf) "?")
  (utf8-decode-tests #(#xe0 #x9f #xbf) "?")
  (utf8-decode-tests #(#xf0 #x8f #xbf #xbf) "?")
  (utf8-decode-tests #(#xf8 #x87 #xbf #xbf #xbf) "?")
  (utf8-decode-tests #(#xfc #x83 #xbf #xbf #xbf #xbf) "?"))

(deftest utf8-overlong-sequences-null ()
  (utf8-decode-tests #(#xc0 #x80) "?")
  (utf8-decode-tests #(#xe0 #x80 #x80) "?")
  (utf8-decode-tests #(#xf0 #x80 #x80 #x80) "?")
  (utf8-decode-tests #(#xf8 #x80 #x80 #x80 #x80) "?")
  (utf8-decode-tests #(#xfc #x80 #x80 #x80 #x80 #x80) "?"))

;;;; End of adapted SBCL tests.

;;; Expected to fail, for now.
#+#:ignore
(deftest utf8-illegal-code-positions ()
  ;; single UTF-16 surrogates
  (utf8-decode-tests #(#xed #xa0 #x80) "?")
  (utf8-decode-tests #(#xed #xad #xbf) "?")
  (utf8-decode-tests #(#xed #xae #x80) "?")
  (utf8-decode-tests #(#xed #xaf #xbf) "?")
  (utf8-decode-tests #(#xed #xb0 #x80) "?")
  (utf8-decode-tests #(#xed #xbe #x80) "?")
  (utf8-decode-tests #(#xed #xbf #xbf) "?")
  ;; paired UTF-16 surrogates
  (utf8-decode-tests #(ed a0 80 ed b0 80) "??")
  (utf8-decode-tests #(ed a0 80 ed bf bf) "??")
  (utf8-decode-tests #(ed ad bf ed b0 80) "??")
  (utf8-decode-tests #(ed ad bf ed bf bf) "??")
  (utf8-decode-tests #(ed ae 80 ed b0 80) "??")
  (utf8-decode-tests #(ed ae 80 ed bf bf) "??")
  (utf8-decode-tests #(ed af bf ed b0 80) "??")
  (utf8-decode-tests #(ed af bf ed bf bf) "??")
  ;; other illegal code positions
  (utf8-decode-tests #(#xef #xbf #xbe) "?")  ; #\uFFFE
  (utf8-decode-tests #(#xef #xbf #xbf) "?")) ; #\uFFFF

;;; A list of the ISO-8859 encodings where each element is a cons with
;;; the car being a keyword denoting the encoding and the cdr being a
;;; vector enumerating the corresponding character codes.
;;;
;;; It was auto-generated from files which can be found at
;;; <ftp://ftp.unicode.org/Public/MAPPINGS/ISO8859/>.
;;;
;;; Taken from flexi-streams.
(defparameter *iso-8859-tables*
  '((:iso-8859-1 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170
       171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188
       189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206
       207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224
       225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242
       243 244 245 246 247 248 249 250 251 252 253 254 255))

    (:iso-8859-2 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 260 728 321 164 317 346 167 168 352 350
       356 377 173 381 379 176 261 731 322 180 318 347 711 184 353 351 357 378
       733 382 380 340 193 194 258 196 313 262 199 268 201 280 203 282 205 206
       270 272 323 327 211 212 336 214 215 344 366 218 368 220 221 354 223 341
       225 226 259 228 314 263 231 269 233 281 235 283 237 238 271 273 324 328
       243 244 337 246 247 345 367 250 369 252 253 355 729))

    (:iso-8859-3 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 294 728 163 164 65533 292 167 168 304
       350 286 308 173 65533 379 176 295 178 179 180 181 293 183 184 305 351
       287 309 189 65533 380 192 193 194 65533 196 266 264 199 200 201 202 203
       204 205 206 207 65533 209 210 211 212 288 214 215 284 217 218 219 220
       364 348 223 224 225 226 65533 228 267 265 231 232 233 234 235 236 237
       238 239 65533 241 242 243 244 289 246 247 285 249 250 251 252 365 349
       729))

    (:iso-8859-4 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 260 312 342 164 296 315 167 168 352 274
       290 358 173 381 175 176 261 731 343 180 297 316 711 184 353 275 291 359
       330 382 331 256 193 194 195 196 197 198 302 268 201 280 203 278 205 206
       298 272 325 332 310 212 213 214 215 216 370 218 219 220 360 362 223 257
       225 226 227 228 229 230 303 269 233 281 235 279 237 238 299 273 326 333
       311 244 245 246 247 248 371 250 251 252 361 363 729))

    (:iso-8859-5 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 1025 1026 1027 1028 1029 1030 1031 1032
       1033 1034 1035 1036 173 1038 1039 1040 1041 1042 1043 1044 1045 1046
       1047 1048 1049 1050 1051 1052 1053 1054 1055 1056 1057 1058 1059 1060
       1061 1062 1063 1064 1065 1066 1067 1068 1069 1070 1071 1072 1073 1074
       1075 1076 1077 1078 1079 1080 1081 1082 1083 1084 1085 1086 1087 1088
       1089 1090 1091 1092 1093 1094 1095 1096 1097 1098 1099 1100 1101 1102
       1103 8470 1105 1106 1107 1108 1109 1110 1111 1112 1113 1114 1115 1116
       167 1118 1119))

    (:iso-8859-6 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 65533 65533 65533 164 65533 65533 65533
       65533 65533 65533 65533 1548 173 65533 65533 65533 65533 65533 65533
       65533 65533 65533 65533 65533 65533 65533 1563 65533 65533 65533 1567
       65533 1569 1570 1571 1572 1573 1574 1575 1576 1577 1578 1579 1580 1581
       1582 1583 1584 1585 1586 1587 1588 1589 1590 1591 1592 1593 1594 65533
       65533 65533 65533 65533 1600 1601 1602 1603 1604 1605 1606 1607 1608
       1609 1610 1611 1612 1613 1614 1615 1616 1617 1618 65533 65533 65533
       65533 65533 65533 65533 65533 65533 65533 65533 65533 65533))

    (:iso-8859-7 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 8216 8217 163 8364 8367 166 167 168 169
       890 171 172 173 65533 8213 176 177 178 179 900 901 902 183 904 905 906
       187 908 189 910 911 912 913 914 915 916 917 918 919 920 921 922 923 924
       925 926 927 928 929 65533 931 932 933 934 935 936 937 938 939 940 941
       942 943 944 945 946 947 948 949 950 951 952 953 954 955 956 957 958 959
       960 961 962 963 964 965 966 967 968 969 970 971 972 973 974 65533))

    (:iso-8859-8 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 65533 162 163 164 165 166 167 168 169
       215 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 247 187
       188 189 190 65533 65533 65533 65533 65533 65533 65533 65533 65533 65533
       65533 65533 65533 65533 65533 65533 65533 65533 65533 65533 65533 65533
       65533 65533 65533 65533 65533 65533 65533 65533 65533 65533 8215 1488
       1489 1490 1491 1492 1493 1494 1495 1496 1497 1498 1499 1500 1501 1502
       1503 1504 1505 1506 1507 1508 1509 1510 1511 1512 1513 1514 65533 65533
       8206 8207 65533))

    (:iso-8859-9 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170
       171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188
       189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206
       207 286 209 210 211 212 213 214 215 216 217 218 219 220 304 350 223 224
       225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 287 241 242
       243 244 245 246 247 248 249 250 251 252 305 351 255))

    (:iso-8859-10 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 260 274 290 298 296 310 167 315 272 352
       358 381 173 362 330 176 261 275 291 299 297 311 183 316 273 353 359 382
       8213 363 331 256 193 194 195 196 197 198 302 268 201 280 203 278 205 206
       207 208 325 332 211 212 213 214 360 216 370 218 219 220 221 222 223 257
       225 226 227 228 229 230 303 269 233 281 235 279 237 238 239 240 326 333
       243 244 245 246 361 248 371 250 251 252 253 254 312))

    (:iso-8859-11 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 3585 3586 3587 3588 3589 3590 3591 3592
       3593 3594 3595 3596 3597 3598 3599 3600 3601 3602 3603 3604 3605 3606
       3607 3608 3609 3610 3611 3612 3613 3614 3615 3616 3617 3618 3619 3620
       3621 3622 3623 3624 3625 3626 3627 3628 3629 3630 3631 3632 3633 3634
       3635 3636 3637 3638 3639 3640 3641 3642 65533 65533 65533 65533 3647
       3648 3649 3650 3651 3652 3653 3654 3655 3656 3657 3658 3659 3660 3661
       3662 3663 3664 3665 3666 3667 3668 3669 3670 3671 3672 3673 3674 3675
       65533 65533 65533 65533))

    (:iso-8859-13 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 8221 162 163 164 8222 166 167 216 169
       342 171 172 173 174 198 176 177 178 179 8220 181 182 183 248 185 343 187
       188 189 190 230 260 302 256 262 196 197 280 274 268 201 377 278 290 310
       298 315 352 323 325 211 332 213 214 215 370 321 346 362 220 379 381 223
       261 303 257 263 228 229 281 275 269 233 378 279 291 311 299 316 353 324
       326 243 333 245 246 247 371 322 347 363 252 380 382 8217))

    (:iso-8859-14 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 7682 7683 163 266 267 7690 167 7808 169
       7810 7691 7922 173 174 376 7710 7711 288 289 7744 7745 182 7766 7809
       7767 7811 7776 7923 7812 7813 7777 192 193 194 195 196 197 198 199 200
       201 202 203 204 205 206 207 372 209 210 211 212 213 214 7786 216 217 218
       219 220 221 374 223 224 225 226 227 228 229 230 231 232 233 234 235 236
       237 238 239 373 241 242 243 244 245 246 7787 248 249 250 251 252 253 375
       255))

    (:iso-8859-15 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 161 162 163 8364 165 352 167 353 169 170
       171 172 173 174 175 176 177 178 179 381 181 182 183 382 185 186 187 338
       339 376 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206
       207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224
       225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242
       243 244 245 246 247 248 249 250 251 252 253 254 255))

    (:iso-8859-16 .
     #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
       27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
       51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74
       75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
       99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
       117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
       135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
       153 154 155 156 157 158 159 160 260 261 321 8364 8222 352 167 353 169
       536 171 377 173 378 379 176 177 268 322 381 8221 182 183 382 269 537 187
       338 339 376 380 192 193 194 258 196 262 198 199 200 201 202 203 204 205
       206 207 272 323 210 211 212 336 214 346 368 217 218 219 220 280 538 223
       224 225 226 259 228 263 230 231 232 233 234 235 236 237 238 239 273 324
       242 243 244 337 246 347 369 249 250 251 252 281 539 255))))

(deftest iso-8859-decode-check ()
  (loop for enc in *iso-8859-charsets*
        for octets = (let ((octets (make-ub8-vector 256)))
                       (dotimes (i 256 octets)
                         (setf (aref octets i) i)))
        for string = (octets-to-string octets :encoding enc)
        do (is (equalp (map 'vector #'char-code string)
                       (cdr (assoc enc *iso-8859-tables*))))))

(deftest character-out-of-range.utf-32 ()
  (signals character-out-of-range
    (octets-to-string (ub8v 0 0 #xfe #xff 0 #x11 0 0)
                      :encoding :utf-32 :errorp t)))

;;; RT: encoders and decoders were returning bogus values.
(deftest encoder/decoder-retvals (encoding &optional (test-string "abc"))
  (let* ((mapping (lookup-mapping babel::*string-vector-mappings* encoding))
         (strlen (length test-string))
         ;; encoding
         (octet-precount (funcall (octet-counter mapping)
                                  test-string 0 strlen -1))
         (array (make-array octet-precount :element-type '(unsigned-byte 8)))
         (encoded-octet-count (funcall (encoder mapping)
                                       test-string 0 strlen array 0))
         ;; decoding
         (string (make-string strlen))
         (char-precount (funcall (code-point-counter mapping)
                                 array 0 octet-precount -1))
         (char-count (funcall (decoder mapping)
                              array 0 octet-precount string 0)))
    (is (= octet-precount encoded-octet-count))
    (is (= char-precount char-count))
    (is (string= test-string string))))

(deftest encoder-and-decoder-return-values ()
  (mapcar 'encoder/decoder-retvals
          (remove-if 'ambiguous-encoding-p
                     (list-character-encodings))))

(deftest code-point-sweep (encoding)
  (finishes
    (dotimes (i char-code-limit)
      (let ((char (ignore-errors (code-char i))))
        (when char
          (ignore-some-conditions (character-encoding-error)
            (string-to-octets (string char) :encoding encoding)))))))

#+enable-slow-babel-tests
(deftest code-point-sweep-all-encodings ()
  (mapc #'code-point-sweep (list-character-encodings)))

(deftest octet-sweep (encoding)
  (finishes
    (loop for b1 upto #xff do
      (loop for b2 upto #xff do
        (loop for b3 upto #xff do
          (loop for b4 upto #xff do
            (ignore-some-conditions (character-decoding-error)
              (octets-to-string (ub8v b1 b2 b3 b4) :encoding encoding))))))))

#+enable-slow-babel-tests
(deftest octet-sweep-all-encodings ()
  (mapc #'octet-sweep (list-character-encodings)))
