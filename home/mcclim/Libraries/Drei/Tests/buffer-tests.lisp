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
;;;
;;; The test cases in this files test the functions of the buffer
;;; protocol and the implementations provided by Drei. The test cases
;;; are divided into two FiceAM suites - one for functionality
;;; regression testing and one for performance testing.

(cl:in-package :drei-tests)

(def-suite buffer-tests :description "The test suite for
buffer-protocol related tests." :in drei-tests)

(in-suite buffer-tests)

(buffer-test buffer-make-instance
  "Test that the various types of buffers can be created, and
that they are initialized properly."
  (let* ((buffer (make-instance %%buffer)))
    (signals motion-after-end
      (make-instance %%left-sticky-mark :buffer buffer :offset 1))
    (handler-case
        (progn (make-instance %%right-sticky-mark
                :buffer buffer
                :offset 1)
               (fail))
      (motion-after-end (c)
        (is (= (drei-buffer::condition-offset c) 1))))))

(buffer-test clone-mark
  "Test that marks really are cloned."
  (let* ((buffer (make-instance %%buffer))
         (low (make-buffer-mark buffer))
         (high (make-buffer-mark buffer))
         (low2 (clone-mark low))
         (high2 (clone-mark high))
         (low3 (clone-mark high :left))
         (high3 (clone-mark low :right)))
    ;; They must be of the same class...
    (is (class-of low) (class-of low2))
    (is (class-of low2) (class-of low3))
    (is (class-of high) (class-of high2))
    (is (class-of high2) (class-of high3))
    ;; And have the same offset.
    (is (= (offset low) (offset low2) (offset low3)))
    (is (= (offset high) (offset high2) (offset high3) 0))))

;;; NOTE: the current implementation uses vectors wherever sequences
;;; are expected (and strings are vectors of characters)

(buffer-test insert-buffer-object
  "Test that insertion of buffer objects happen in the right
order, and that the buffer is updated appropriately."
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-object buffer 0 #\a)
    ;; The buffer should have increased in size to accomodate the
    ;; object.
    (is (= (size buffer) 1))
    ;; The contents should be what we put in there.
    (is (string= (buffer-substring buffer 0 1) "a"))
    (insert-buffer-object buffer 0 #\b)
    (insert-buffer-object buffer 0 #\a)
    (is (= (size buffer) 3))
    (is (equal (buffer-substring buffer 0 3) "aba"))
    ;; We should be able to insert an object in the middle of the
    ;; buffer.
    (insert-buffer-object buffer 2 #\b)
    (is (equal (buffer-contents buffer) "abba"))
    ;; Attempting to insert an object after buffer end should cause an
    ;; error.
    (handler-case
        (progn (insert-buffer-object buffer 5 #\a)
               (fail "Failed to signal during insertion of object
after buffer end"))
      (offset-after-end (c)
        (is (= (condition-offset c) 5))))
    ;; Attempting to insert an object before buffer start should cause
    ;; an error.
    (handler-case
        (progn (insert-buffer-object buffer -1 #\a)
               (fail "Failed to signal during insertion of object
before buffer start"))
      (offset-before-beginning (c)
        (= (condition-offset c) -1)))))

(buffer-test insert-buffer-sequence
  "Test that we are able to insert sequences, and that the buffer
size and contents are updated appropriately."
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (is (= (size buffer) 7))
    (is (string= (buffer-substring buffer 0 7) "climacs"))
    (insert-buffer-sequence buffer 0 "climacs")
    ;; Inserting a sequence in the middle should work.
    (insert-buffer-sequence buffer 3 "ClimacS")
    (is (= (size buffer) 21))
    (is (string= (buffer-substring buffer 0 14) "cliClimacSmacs"))
    (insert-buffer-sequence buffer 0 "climacs")
    (insert-buffer-sequence buffer 0 "ClimacS")
    (is (= (size buffer) 35))
    (is (string= (buffer-substring buffer 0 14) "ClimacSclimacs"))
    (handler-case
        (progn (insert-buffer-sequence buffer 37 "climacs")
               (fail "Failed to signal during insertion of
sequence after buffer end"))
      (offset-after-end (c)
        (= (condition-offset c) 37)))
    (handler-case
        (progn (insert-buffer-sequence buffer -1 "climacs")
               (fail "Failed to signal during insertion of
sequence before buffer start"))
      (offset-before-beginning (c)
        (is (= (condition-offset c) -1))))))

(buffer-test delete-buffer-range
  "Test whether deletion of multiple objects in the buffer
works."
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 7)
    (is (= 0 (size buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 0 3)
    (is (= 4 (size buffer)))
    (is (string= "macs" (buffer-substring buffer 0 4)))
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 4)
    (is (= 7 (size buffer)))
    (is (string= "cli" (buffer-substring buffer 0 3)))
    ;; Deleting 0 characters should work (and leave the buffer
    ;; untouched.)
    (insert-buffer-sequence buffer 0 "climacs")
    (delete-buffer-range buffer 3 0)
    (is (= 14 (size buffer)))
    (is (string= "climacs" (buffer-substring buffer 0 7)))
    (handler-case
        (progn
          (delete-buffer-range buffer -1 0)
          (fail "Failed to signal during deletion of range with
negative offset"))
      (offset-before-beginning (c)
        (is (= -1 (condition-offset c)))))
    (handler-case
        (progn
          (delete-buffer-range buffer 12 3)
          (fail "Failed to signal during deletion of range that
exceeded buffer end."))
      (offset-after-end (c)
        (is (= 15 (condition-offset c)))))))

(buffer-test insert-object
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :left)))
      (insert-object m #\X)
      (is (= 8 (size buffer)))
      (is (= 3 (offset m)))
      (is (string= "cliXmacs" (buffer-substring buffer 0 8))))
    (let ((m (make-buffer-mark buffer 2 :right)))
      (insert-object m #\X)
      (is (= 9 (size buffer)))
      (is (= 3 (offset m)))
      (is (string= "clXiXmacs" (buffer-substring buffer 0 9))))))

(buffer-test insert-sequence
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :left))
	  (m2 (make-buffer-mark buffer 5 :left)))
      (insert-sequence m "ClimacS")
      (is (= (size buffer) 14))
      (is (eq (buffer m) (buffer m2)))
      (is (= (offset m) 3))
      (is (= (offset m2) 12))
      (is (string= (buffer-substring buffer 0 14) "cliClimacSmacs"))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :right))
          (m2 (make-buffer-mark buffer 5 :right)))
      (insert-sequence m "ClimacS")
      (is (= (size buffer) 14))
      (is (eq (buffer m) (buffer m2)))
      (is (= (offset m) 10))
      (is (= (offset m2) 12))
      (is (string= (buffer-substring buffer 0 14) "cliClimacSmacs")))))

(buffer-test delete-range
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :left))
	  (m2 (make-buffer-mark buffer 5 :left)))
      (delete-range m 2)
      (is (= (size buffer) 5))
      (is (eq (buffer m) (buffer m2)))
      (is (= (offset m) 3))
      (is (= (offset m2) 3))
      (is (string= (buffer-substring buffer 0 5) "clics"))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :right))
	  (m2 (make-buffer-mark buffer 5 :right)))
      (delete-range m -2)
      (is (= (size buffer) 5))
      (is (eq (buffer m) (buffer m2)))
      (is (= (offset m) 1))
      (is (= (offset m2) 3))
      (is (string= (buffer-substring buffer 0 5) "cmacs")))))

(buffer-test delete-region
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :left))
	  (m2 (make-buffer-mark buffer 5 :left)))
      (delete-region m m2)
      (is (= 5 (size buffer)))
      (is (eq (buffer m) (buffer m2)))
      (is (= 3 (offset m)))
      (is (= 3 (offset m2)))
      (is (string= (buffer-substring buffer 0 5) "clics"))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :right))
	  (m2 (make-buffer-mark buffer 5 :right)))
      (delete-region m m2)
      (is (= 5 (size buffer)))
      (is (eq (buffer m) (buffer m2)))
      (is (= 3 (offset m)))
      (is (= 3 (offset m2)))
      (is (string= (buffer-substring buffer 0 5) "clics"))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :left))
          (m2 (make-buffer-mark buffer 5 :left)))
      (delete-region m2 m)
      (is (= 5 (size buffer)))
      (is (eq (buffer m) (buffer m2)))
      (is (= 3 (offset m)))
      (is (= 3 (offset m2)))
      (is (string= (buffer-substring buffer 0 5) "clics"))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :right))
	  (m2 (make-buffer-mark buffer 5 :right)))
      (delete-region m2 m)
      (is (= 5 (size buffer)))
      (is (eq (buffer m) (buffer m2)))
      (is (= 3 (offset m)))
      (is (= 3 (offset m2)))
      (is (string= (buffer-substring buffer 0 5) "clics"))))
  (signals error
    (let ((buffer (make-instance %%buffer))
          (buffer2 (make-instance %%buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (insert-buffer-sequence buffer2 0 "climacs")
      (let ((m (make-buffer-mark buffer 3 :right))
            (m2 (make-buffer-mark buffer2 5 :right)))
        (delete-region m2 m))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m (make-buffer-mark buffer 3 :left))
	  (m2 (make-buffer-mark buffer 5 :left)))
      (delete-region m 5)
      (delete-region 1 m2)
      (is (= 3 (size buffer)))
      (is (eq (buffer m) (buffer m2)))
      (is (= 1 (offset m)))
      (is (= 1 (offset m2)))
      (is (string= "ccs" (buffer-substring buffer 0 3))))))

(buffer-test number-of-lines
  (let ((buffer (make-instance %%buffer)))
    (is (= (number-of-lines buffer) 0))
    (insert-buffer-sequence buffer 0 "climacs
climacs
")
    (is (= (number-of-lines buffer) 2))))

(buffer-test mark-relations
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let ((m0 (make-buffer-mark buffer 0 :right))
          (m1 (make-buffer-mark buffer 3 :left))
          (m1a (make-buffer-mark buffer 3 :right))
          (m2 (make-buffer-mark buffer 5 :right))
          (m2a (make-buffer-mark buffer 5 :left))
          (m3 (make-buffer-mark buffer 7 :left)))
      (is-true (mark< m0 m1)) (not (mark> m0 m1)) (not (mark>= m0 m1))
      (is-true (mark< m0 m2)) (not (mark> m0 m2)) (not (mark>= m0 m2))
      (is-true (mark< m0 m3)) (not (mark> m0 m3)) (not (mark>= m0 m3))
      (is-true (mark< m1 m2)) (not (mark> m1 m2)) (not (mark>= m1 m2))
      (is-true (mark< m1 m3)) (not (mark> m1 m3)) (not (mark>= m1 m3))
      (is-true (mark< m2 m3)) (not (mark> m2 m3)) (not (mark>= m2 m3))
      (is-true (mark<= m1 m1a)) (not (mark> m1 m1a))
      (is-true (mark>= m1 m1a)) (not (mark< m1 m1a))
      (is-true (mark> m3 m2)) (not (mark< m3 m2)) (not (mark<= m3 m2))
      (is-true (mark> m3 m1)) (not (mark< m3 m1)) (not (mark<= m3 m1))
      (is-true (mark> m3 m0)) (not (mark< m3 m0)) (not (mark<= m3 m0))
      (is-true (mark> m2 m1)) (not (mark< m2 m1)) (not (mark<= m2 m1))
      (is-true (mark> m2 m0)) (not (mark< m2 m0)) (not (mark<= m2 m0))
      (is-true (mark>= m2 m2a)) (not (mark> m2 m2a))
      (is-true (mark>= m2 m2a)) (not (mark< m2 m2a))
      (is-true (mark= m1 m1a))
      (is-true (mark= m2 m2a))
      (is-true (beginning-of-buffer-p m0)) (not (beginning-of-buffer-p m3))
      (is-true (end-of-buffer-p m3)) (not (end-of-buffer-p m0))
      (is-true (beginning-of-line-p m0)) (not (beginning-of-line-p m3))
      (is-true (end-of-line-p m3)) (not (end-of-line-p m0))
      (is-true (every #'(lambda (m) (zerop (line-number m)))
                      (list m0 m1 m1a m2 m2a m3))))))

(buffer-test setf-offset
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(make-buffer-mark buffer -1 :left)
        (fail "Failed to signal when setting offset of mark to
negative value"))
    (motion-before-beginning (c)
      (is (= (condition-offset c) -1))))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
        (make-buffer-mark buffer 8 :left)
        (fail "Failed to signal when setting offset of mark to
a value too large for the buffer"))
    (motion-after-end (c)
      (is (= (condition-offset c) 8)))))

(buffer-test backward-object
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let* ((m1 (make-buffer-mark buffer 4 :left))
	   (m2 (clone-mark m1)))
      (backward-object m1 2)
      (is (string= (region-to-string m1 m2) "im"))))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let* ((m1 (make-buffer-mark buffer 2 :right))
	       (m2 (clone-mark m1)))
	  (backward-object m1 3)
	  (region-to-sequence m1 m2)
          (fail "Failed to signal when moving mark to before
buffer start.")))
    (motion-before-beginning (c)
      (is (= (condition-offset c) -1)))))

(buffer-test forward-object
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (let* ((m1 (make-buffer-mark buffer 4 :left))
	   (m2 (clone-mark m1)))
      (forward-object m1 2)
      (is (string= (region-to-string m1 m2) "ac"))))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(let* ((m1 (make-buffer-mark buffer 0 :right))
	       (m2 (clone-mark m1)))
	  (setf (offset m1) 6
		(offset m2) 6)
	  (forward-object m1 3)
	  (region-to-sequence m1 m2)
          (fail "Failed to signal when moving mark past end of
buffer.")))
    (motion-after-end (c)
      (is (= (condition-offset c) 9)))))

(buffer-test setf-buffer-object
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (setf (buffer-object buffer 0) #\C)
    (is (string= (buffer-contents buffer)  "Climacs")))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(setf (buffer-object buffer 0) #\a)
        (fail "Failed to signal when setting buffer object at
offset 0"))
    (offset-after-end (c)
      (is (= (condition-offset c) 0))))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(setf (buffer-object buffer -1) #\a)
        (fail "Failed to signal when setting buffer object at
negative offset"))
    (offset-before-beginning (c)
      (is (= (condition-offset c) -1)))))

(buffer-test mark<
  (signals error
    (let ((buffer (make-instance %%buffer))
          (buffer2 (make-instance %%buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (insert-buffer-sequence buffer2 0 "climacs")
      (let ((m1 (make-buffer-mark buffer))
            (m2 (make-buffer-mark buffer2)))
        (mark< m1 m2)))))

(buffer-test mark>
  (signals error
    (let ((buffer (make-instance %%buffer))
          (buffer2 (make-instance %%buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (insert-buffer-sequence buffer2 0 "climacs")
      (let ((m1 (make-buffer-mark buffer))
            (m2 (make-buffer-mark buffer2)))
        (mark> m1 m2)))))

(buffer-test mark<=
  (signals error
    (let ((buffer (make-instance %%buffer))
          (buffer2 (make-instance %%buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (insert-buffer-sequence buffer2 0 "climacs")
      (let ((m1 (make-buffer-mark buffer))
            (m2 (make-buffer-mark buffer2)))
        (mark<= m1 m2)))))

(buffer-test mark>=
  (signals error
    (let ((buffer (make-instance %%buffer))
          (buffer2 (make-instance %%buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (insert-buffer-sequence buffer2 0 "climacs")
      (let ((m1 (make-buffer-mark buffer))
            (m2 (make-buffer-mark buffer2)))
        (mark>= m1 m2)))))

(buffer-test mark=
  (signals error
    (let ((buffer (make-instance %%buffer))
          (buffer2 (make-instance %%buffer)))
      (insert-buffer-sequence buffer 0 "climacs")
      (insert-buffer-sequence buffer2 0 "climacs")
      (let ((m1 (make-buffer-mark buffer))
            (m2 (make-buffer-mark buffer2)))
        (mark= m1 m2)))))

(buffer-test line-number
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-buffer-mark buffer 0 :left))
	  (m2 (make-buffer-mark buffer 0 :right)))
      (setf (offset m1) 3
	    (offset m2) 11)
      (is (= 0 (line-number m1) (1- (line-number m2)))))))

(buffer-test buffer-column-number
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "~A~Aclimacs" #\Tab #\Tab))
    (is (char= (buffer-object buffer 2) #\c))
    (is (= (buffer-column-number buffer 2) 2))))

(buffer-test buffer-column-number
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 #.(format nil "~%~A~Aclimacs" #\Tab #\Tab))
    (is (char= (buffer-object buffer 3) #\c))
    (is (= (buffer-column-number buffer 3) 2))))

(buffer-test column-number
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m1 (make-buffer-mark buffer 0 :left))
	  (m2 (make-buffer-mark buffer 0 :right)))
      (setf (offset m1) 3
	    (offset m2) 11)
      (is (= 3 (column-number m1) (column-number m2)))))t)

(buffer-test beginning-of-line
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-buffer-mark buffer 0 :left)))
      (setf (offset m) 11)
      (is-true (not (beginning-of-line-p m)))
      (beginning-of-line m)
      (is-true (beginning-of-line-p m)))))

(buffer-test end-of-line
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-buffer-mark buffer 0 :left)))
      (setf (offset m) 11)
      (is-true (not (end-of-line-p m)))
      (end-of-line m)
      (is-true (end-of-line-p m))))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
")
    (let ((m (make-buffer-mark buffer 0 :left)))
      (setf (offset m) 1)
      (is-true (not (end-of-line-p m)))
      (end-of-line m)
      (is (= (offset m) 7))
      (is-true (char= (buffer-object (buffer m) (offset m)) #\Newline)))))

(buffer-test beginning-of-buffer
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-buffer-mark buffer 0 :left)))
      (setf (offset m) 11)
      (is-true (not (beginning-of-buffer-p m)))
      (beginning-of-buffer m)
      (is-true (beginning-of-buffer-p m)))))

(buffer-test end-of-buffer
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs
climacs")
    (let ((m (make-buffer-mark buffer 0 :left)))
      (setf (offset m) 11)
      (is-true (not (end-of-buffer-p m)))
      (end-of-buffer m)
      (is-true (end-of-buffer-p m)))))

(buffer-test buffer-object
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (is (char= (buffer-object buffer 3) #\m)))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer -1)
        (fail "Failed to signal when requesting object with
negative offset"))
    (no-such-offset (c)
      (is (= (condition-offset c) -1))))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(buffer-object buffer 7)
        (fail "Failed to signal when requesting object past end
of buffer"))
    (no-such-offset (c)
      (is (= (condition-offset c) 7)))))

(buffer-test buffer-sequence
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(buffer-sequence buffer -1 0)
        (fail "Failed to signal when requesting buffer seuqnce
with negative offset"))
    (no-such-offset (c)
      (is (= (condition-offset c) -1))))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(buffer-sequence buffer 0 1)
        (fail "Failed to signal when requesting buffer sequence
too big for buffer"))
    (no-such-offset (c)
      (= (condition-offset c) 1)))
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (is (equalp (buffer-sequence buffer 5 3) #()))))

(buffer-test object-before
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (is (char= (object-before (make-buffer-mark buffer 7)) #\s)))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-before (make-buffer-mark buffer))
        (fail "Failed to signal when requesting object before
buffer start"))
    (no-such-offset (c)
      (= (condition-offset c) -1))))

(buffer-test object-after
  (let ((buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 "climacs")
    (is (char= (object-after (make-buffer-mark buffer)) #\c)))
  (handler-case
      (let ((buffer (make-instance %%buffer)))
	(insert-buffer-sequence buffer 0 "climacs")
	(object-after (make-buffer-mark buffer 7))
        (fail "Failed to signal when requesting object past
buffer end"))
    (no-such-offset (c)
      (= (condition-offset c) 7))))

(buffer-test region-to-sequence
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-string (make-buffer-mark buffer) (make-buffer-mark buffer 7))))
      (is-true (not (eq seq seq2)))
      (is (string= seq2 "climacs"))))
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-string 0 (make-buffer-mark buffer 7))))
      (is-true (not (eq seq seq2)))
      (is (string= seq2 "climacs"))))
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-string (make-buffer-mark buffer 7) 0)))
      (is-true (not (eq seq seq2)))
      (is (string= seq2 "climacs"))))
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-string (make-buffer-mark buffer) 7)))
      (is-true (not (eq seq seq2)))
      (is (string= seq2 "climacs"))))
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (let ((seq2 (region-to-string 7 (make-buffer-mark buffer))))
      (is-true (not (eq seq seq2)))
      (is (string= seq2 "climacs"))))
  (let ((seq "climacs")
	(buffer (make-instance %%buffer)))
    (insert-buffer-sequence buffer 0 seq)
    (is (string= (region-to-string (make-buffer-mark buffer 7) (make-buffer-mark buffer))
                 "climacs")))
  (signals error
    (let ((buffer1 (make-instance %%buffer))
          (buffer2 (make-instance %%buffer)))
      (region-to-string (make-buffer-mark buffer1) (make-buffer-mark buffer2)))))

;;;; performance tests

(def-suite buffer-performance-tests :description "The test suite for
buffer-protocol implementation performance tests.")

(in-suite buffer-performance-tests)

(buffer-test performance-test-1
  (is (= (time
          (loop with b = (make-instance %%buffer)
             for i from 0 below 100000
             do (insert-buffer-object b 0 #\a)
             finally (return (size b))))
         100000)))

(buffer-test performance-test-1a
  (is (= (time
          (let ((b (loop with b = (make-instance %%buffer)
                      for i from 0 below 100000
                      do (insert-buffer-object b 0 #\a)
                      finally (return b))))
            (loop for i from 0 below 100000
               do (delete-buffer-range b 0 1)
               finally (return (size b)))))
         0)))

(buffer-test performance.test-1b
  (is (= (time
          (loop with b = (make-instance %%buffer)
             for i from 0 below 100000
             do (insert-buffer-object b (size b) #\a)
             finally (return (size b))))
         100000)))

(buffer-test performance.test-1ba
  (is (= (time
          (let ((b (loop with b = (make-instance %%buffer)
                      for i from 0 below 100000
                      do (insert-buffer-object b (size b) #\a)
                      finally (return b))))
            (loop for i from 0 below 100000
               do (delete-buffer-range b 0 1)
               finally (return (size b)))))
         0)))

(buffer-test performance.test-1c
  (is (= (time
          (loop with b = (make-instance %%buffer)
             for i from 0 below 100000
             do (insert-buffer-object b (floor (size b) 2) #\a)
             finally (return (size b))))
         100000)))

(buffer-test performance.test-1ca
  (is (= (time
          (let ((b (loop with b = (make-instance %%buffer)
                      for i from 0 below 100000
                      do (insert-buffer-object b (floor (size b) 2) #\a)
                      finally (return b))))
            (loop for i from 0 below 100000
               do (delete-buffer-range b 0 1)
               finally (return (size b)))))
         0)))

(buffer-test performance.test-1cb
  (is (= (time
          (let ((b (loop with b = (make-instance %%buffer)
                      for i from 0 below 100000
                      do (insert-buffer-object b (floor (size b) 2) #\a)
                      finally (return b))))
            (loop for i from 0 below 100000
               do (delete-buffer-range b (floor (size b) 2) 1)
               finally (return (size b)))))
         0)))

(buffer-test performance.test-2
  (is (= (time
          (loop with b = (make-instance %%buffer)
             for i from 0 below 100000
             do (insert-buffer-sequence b 0 "a")
             finally (return (size b))))
         100000)))

(buffer-test performance.test-2b
  (is (= (time
          (loop with b = (make-instance %%buffer)
             for i from 0 below 100000
             do (insert-buffer-sequence b (size b) "a")
             finally (return (size b))))
         100000)))

(buffer-test performance.test-2c
  (is (= (time
          (loop with b = (make-instance %%buffer)
             for i from 0 below 100000
             do (insert-buffer-sequence b (floor (size b) 2) "a")
             finally (return (size b))))
         100000)))

(buffer-test performance.test-3
  (is (= (time
          (loop with b = (make-instance %%buffer)
             for i from 0 below 100000
             do (insert-buffer-sequence b 0 "abcdefghij")
             finally (return (size b))))
         1000000)))

(buffer-test performance.test-3b
  (is (= (time
          (loop with b = (make-instance %%buffer)
             for i from 0 below 100000
             do (insert-buffer-sequence b (size b) "abcdefghij")
             finally (return (size b))))
         1000000)))

(buffer-test performance.test-3c
  (is (= (time
          (loop with b = (make-instance %%buffer)
             for i from 0 below 100000
             do (insert-buffer-sequence b (floor (size b) 2) "abcdefghij")
             finally (return (size b))))
         1000000)))

(buffer-test performance.test-4
  (is-false (time
             (let ((b (make-instance %%buffer)))
               (insert-buffer-sequence b 0 (make-array '(100000) :initial-element #\a))
               (let ((m (clone-mark (make-buffer-mark b))))
                 (loop
                    for i from 0 below 1000
                    for f = t then (not b)
                    do (if f
                           (end-of-line m)
                           (beginning-of-line m))))))))

(buffer-test performance.test-4b
  (is-false (time
             (let ((b (make-instance %%buffer)))
               (insert-buffer-object b 0 #\Newline)
               (insert-buffer-sequence b 0 (make-array '(100000) :initial-element #\a))
               (insert-buffer-object b 0 #\Newline)
               (let ((m (clone-mark (make-buffer-mark b))))
                 (loop
                    for i from 0 below 1000
                    for f = t then (not b)
                    do (if f
                           (end-of-line m)
                           (beginning-of-line m))))))))

(buffer-test performance.test-4c
  (is-false (time
             (let ((b (make-instance %%buffer)))
               (insert-buffer-object b 0 #\Newline)
               (insert-buffer-sequence b 0 (make-array '(100000) :initial-element #\a))
               (insert-buffer-object b 0 #\Newline)
               (let ((m (clone-mark (make-buffer-mark b))))
                 (incf (offset m))
                 (loop
                    for i from 0 below 1000
                    for f = t then (not b)
                    do (if f
                           (end-of-line m)
                           (beginning-of-line m))))))))

(buffer-test performance.test-4d
  (is (equal (time
              (let ((b (make-instance %%buffer)))
                (insert-buffer-object b 0 #\Newline)
                (insert-buffer-sequence b 0 (make-array '(100000) :initial-element #\a))
                (insert-buffer-object b 0 #\Newline)
                (let ((m (clone-mark (make-buffer-mark b))))
                  (setf (offset m) (floor (size b) 2))
                  (loop
                     for i from 0 below 10
                     collect (list (line-number m) (column-number m))))))
             '((1 50000) (1 50000) (1 50000) (1 50000) (1 50000) (1 50000)
               (1 50000) (1 50000) (1 50000) (1 50000)))))

(buffer-test performance.test-4e
  (is (= (time
          (let ((b (make-instance %%buffer)))
            (insert-buffer-sequence
             b 0 (make-array '(100000) :initial-element #\Newline))
            (let ((m (clone-mark (make-buffer-mark b))))
              (loop
                 for i from 0 below 1000
                 for f = t then (not b)
                 do (if f
                        (forward-line m 0 100000)
                        (previous-line m 0 100000))
                 finally (return (number-of-lines b))))))
         100000)))
