;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(cl:in-package :drei-tests)

(def-suite buffer-streams-tests :description "The test suite for
buffer-streams related tests." :in drei-tests)

(in-suite buffer-streams-tests)

(defun whole-buffer-stream (buffer)
  (let ((mark1 (clone-mark (make-buffer-mark buffer)))
        (mark2 (clone-mark (make-buffer-mark buffer))))
    (beginning-of-buffer mark1)
    (end-of-buffer mark2)
    (make-buffer-stream :buffer buffer
                        :start-mark mark1
                        :end-mark mark2)))

(defun delimited-buffer-stream (buffer start-offset end-offset)
  (let ((mark1 (clone-mark (make-buffer-mark buffer)))
        (mark2 (clone-mark (make-buffer-mark buffer))))
    (setf (offset mark1) start-offset)
    (setf (offset mark2) end-offset)
    (make-buffer-stream :buffer buffer
                        :start-mark mark1
                        :end-mark mark2)))

(test stream-creation
  (with-drei-environment (:initial-contents "foo bar baz")
    (let ((stream (make-buffer-stream
                   :buffer (current-buffer)
                   :start-mark (clone-mark (point) :right)
                   :end-mark (clone-mark (point) :left))))
      (is (typep (start-mark stream) 'left-sticky-mark))
      (is (typep (end-mark stream) 'right-sticky-mark)))))

(test stream-read-char
  (with-drei-environment (:initial-contents "foo bar baz")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is (char= (read-char stream) #\f))
      (is (char= (read-char stream) #\o))
      (is (char= (read-char stream) #\o))
      (is (char= (read-char stream) #\Space))
      (is (char= (read-char stream) #\b))
      (is (char= (read-char stream) #\a))
      (is (char= (read-char stream) #\r))
      (is (char= (read-char stream) #\Space))
      (is (char= (read-char stream) #\b))
      (is (char= (read-char stream) #\a))
      (is (char= (read-char stream) #\z))
      (signals end-of-file
        (read-char stream))
      (is (eq (read-char stream nil :eof) :eof)))
    (let ((stream (delimited-buffer-stream (current-buffer) 4 7)))
      (is (char= (read-char stream) #\b))
      (is (char= (read-char stream) #\a))
      (is (char= (read-char stream) #\r))
      (signals end-of-file
        (read-char stream))
      (is (eq (read-char stream nil :eof) :eof)))))

(test stream-unread-char
  (with-drei-environment (:initial-contents "foo bar baz")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is (char= (read-char stream) #\f))
      (unread-char #\f stream)
      (is (char= (read-char stream) #\f)))
    (let ((stream (delimited-buffer-stream (current-buffer) 4 7)))
      (is (char= (read-char stream) #\b))
      (unread-char #\b stream)
      (is (char= (read-char stream) #\b))
      (is (char= (read-char stream) #\a))
      (is (char= (read-char stream) #\r))
      (signals end-of-file
        (read-char stream))
      (is (eq (read-char stream nil :eof) :eof))
      (unread-char #\r stream)
      (is (char= (read-char stream) #\r))
      (signals end-of-file
        (read-char stream))
      (is (eq (read-char stream nil :eof) :eof)))))

;; Effectively the same as `read-char' for us.
(test stream-read-char-no-hang
  (with-drei-environment (:initial-contents "foo bar baz")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is (char= (read-char-no-hang stream) #\f))
      (is (char= (read-char-no-hang stream) #\o))
      (is (char= (read-char-no-hang stream) #\o))
      (is (char= (read-char-no-hang stream) #\Space))
      (is (char= (read-char-no-hang stream) #\b))
      (is (char= (read-char-no-hang stream) #\a))
      (is (char= (read-char-no-hang stream) #\r))
      (is (char= (read-char-no-hang stream) #\Space))
      (is (char= (read-char-no-hang stream) #\b))
      (is (char= (read-char-no-hang stream) #\a))
      (is (char= (read-char-no-hang stream) #\z))
      (signals end-of-file
        (read-char-no-hang stream))
      (is (eq (read-char-no-hang stream nil :eof) :eof)))
    (let ((stream (delimited-buffer-stream (current-buffer) 4 7)))
      (is (char= (read-char-no-hang stream) #\b))
      (is (char= (read-char-no-hang stream) #\a))
      (is (char= (read-char-no-hang stream) #\r))
      (signals end-of-file
        (read-char-no-hang stream))
      (is (eq (read-char-no-hang stream nil :eof) :eof)))))

(test stream-peek-char
  (with-drei-environment (:initial-contents "foo bar baz")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is (char= (peek-char nil stream) #\f))
      (read-char stream)
      (is (char= (peek-char nil stream) #\o))
      (read-char stream)
      (is (char= (peek-char nil stream) #\o))
      (read-char stream))
    (let ((stream (delimited-buffer-stream (current-buffer) 3 6)))
      (is (char= (peek-char nil stream) #\Space)))
    (let ((stream (delimited-buffer-stream (current-buffer) 3 6)))
      (is (char= (peek-char t stream) #\b)))
    (let ((stream (delimited-buffer-stream (current-buffer) 3 7)))
      (is (char= (peek-char #\r stream) #\r)))
    (let ((stream (delimited-buffer-stream (current-buffer) 0 0)))
      (signals end-of-file
        (peek-char t stream))
      (is (eq (peek-char t stream nil :eof) :eof)))))

(test stream-listen
  (with-drei-environment (:initial-contents "foo bar baz")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is-true (stream-listen stream))
      (dotimes (i 11)
        (finishes (read-char stream)))
      (is-false (stream-listen stream))
      (unread-char #\z stream)
      (is-true (stream-listen stream)))
    (let ((stream (delimited-buffer-stream (current-buffer) 3 6)))
      (is-true (stream-listen stream))
      (dotimes (i 3)
        (finishes (read-char stream)))
      (is-false (stream-listen stream))
      (unread-char #\r stream)
      (is-true (stream-listen stream)))
    (let ((stream (delimited-buffer-stream (current-buffer) 0 0)))
      (is-false (stream-listen stream)))))

(test stream-read-line
  (with-drei-environment (:initial-contents "line 1
line 2
line 3")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is (string= (read-line stream) "line 1"))
      (is (string= (read-line stream) "line 2"))
      (is (char= (read-char stream) #\l))
      (is (string= (read-line stream) "ine 3"))
      (signals end-of-file
        (read-line stream))
      (is (eq (read-line stream nil :eof) :eof)))))

(test stream-write-char
  (with-drei-environment (:initial-contents "piece of text")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is (char= (write-char #\a stream) #\a))
      (buffer-is "apiece of text")
      (is (char= (read-char stream) #\p))
      (is (string= (read-line stream) "iece of text"))
      (signals end-of-file
        (read-char stream))
      (is (char= (write-char #\a stream) #\a))
      (buffer-is "apiece of texta")
      (signals end-of-file
        (read-char stream)))))

(test stream-line-column
  (with-drei-environment (:initial-contents "abcde")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is (= (stream-line-column stream) 0))
      (is (char= (read-char stream) #\a))
      (is (= (stream-line-column stream) 1))
      (is (char= (read-char stream) #\b))
      (is (= (stream-line-column stream) 2))
      (is (char= (read-char stream) #\c))
      (is (= (stream-line-column stream) 3))
      (is (char= (read-char stream) #\d))
      (is (= (stream-line-column stream) 4))
      (is (char= (write-char #\a stream) #\a))
      (is (= (stream-line-column stream) 5))
      (is (char= (read-char stream) #\e))
      (signals end-of-file
        (read-char stream))
      (is (= (stream-line-column stream) 6)))))

(test stream-start-line-p
  (with-drei-environment (:initial-contents "foobar")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is-true (stream-start-line-p stream))
      (is (char= (read-char stream) #\f))
      (is-false (stream-start-line-p stream))
      (unread-char #\f stream)
      (is-true (stream-start-line-p stream)))
    (let ((stream (delimited-buffer-stream (current-buffer) 3 6)))
      (is-true (stream-start-line-p stream))
      (is (char= (read-char stream) #\b))
      (is-false (stream-start-line-p stream))
      (unread-char #\b stream)
      (is-true (stream-start-line-p stream)))))

(test stream-write-string
  (with-drei-environment (:initial-contents "contents")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (write-string "foobar" stream)
      (buffer-is "foobarcontents")
      (is-false (stream-start-line-p stream))
      (write-string #.(format nil "~%") stream)
      (buffer-is #.(format nil "foobar~%contents"))
      (is-true (stream-start-line-p stream))
      (is (char= (read-char stream) #\c)))))

(test stream-terpri
  (with-drei-environment (:initial-contents "contents")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is-true (stream-start-line-p stream))
      (is (char= (read-char stream) #\c))
      (is-false (stream-start-line-p stream))
      (terpri stream)
      (is-true (stream-start-line-p stream))
      (terpri stream)
      (is-true (stream-start-line-p stream))
      (buffer-is #.(format nil "c~%~%ontents")))))

(test stream-fresh-line
  (with-drei-environment (:initial-contents "contents")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (is-true (stream-start-line-p stream))
      (is (char= (read-char stream) #\c))
      (is-false (stream-start-line-p stream))
      (fresh-line stream)
      (is-true (stream-start-line-p stream))
      (fresh-line stream)
      (is-true (stream-start-line-p stream))
      (buffer-is #.(format nil "c~%ontents")))))

(test stream-advance-to-column
  (with-drei-environment (:initial-contents "")
    (let ((stream (whole-buffer-stream (current-buffer))))
      (write-string "foobar" stream)
      (stream-advance-to-column stream 3)
      (buffer-is "foobar")
      (fresh-line stream)
      (stream-advance-to-column stream 3)
      (buffer-is #.(format nil "foobar~%   ")))))
