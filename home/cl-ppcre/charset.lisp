;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/charset.lisp,v 1.10 2009/09/17 19:17:30 edi Exp $

;;; A specialized set implementation for characters by Nikodemus Siivola.

;;; Copyright (c) 2008, Nikodemus Siivola. All rights reserved.
;;; Copyright (c) 2008-2009, Dr. Edmund Weitz. All rights reserved.

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

(in-package :cl-ppcre)

(defconstant +probe-depth+ 3
  "Maximum number of collisions \(for any element) we accept before we
allocate more storage.  This is now fixed, but could be made to vary
depending on the size of the storage vector \(e.g. in the range of
1-4).  Larger probe-depths mean more collisions are tolerated before
the table grows, but increase the constant factor.")

(defun make-char-vector (size)
  "Returns a vector of size SIZE to hold characters.  All elements are
initialized to #\Null except for the first one which is initialized to
#\?."
  (declare #.*standard-optimize-settings*)
  (declare (type (integer 2 #.(1- array-total-size-limit)) size))
  ;; since #\Null always hashes to 0, store something else there
  ;; initially, and #\Null everywhere else
  (let ((result (make-array size
                            :element-type #-:lispworks 'character #+:lispworks 'lw:simple-char
                            :initial-element (code-char 0))))
    (setf (char result 0) #\?)
    result))

(defstruct (charset (:constructor make-charset ()))
  ;; this is set to 0 when we stop hashing and just use a CHAR-CODE
  ;; indexed vector
  (depth +probe-depth+ :type fixnum)
  ;; the number of characters in this set
  (count 0 :type fixnum)
  ;; the storage vector
  (vector (make-char-vector 12) :type (simple-array character (*))))

;; seems to be necessary for some Lisps like ClozureCL
(defmethod make-load-form ((set charset) &optional environment)
  (make-load-form-saving-slots set :environment environment))

(declaim (inline mix))
(defun mix (code hash)
  "Given a character code CODE and a hash code HASH, computes and
returns the \"next\" hash code.  See comments below."
  (declare #.*standard-optimize-settings*)
  ;; mixing the CHAR-CODE back in at each step makes sure that if two
  ;; characters collide (their hashes end up pointing in the same
  ;; storage vector index) on one round, they should (hopefully!) not
  ;; collide on the next
  (sxhash (logand most-positive-fixnum (+ code hash))))

(declaim (inline compute-index))
(defun compute-index (hash vector)
  "Computes and returns the index into the vector VECTOR corresponding
to the hash code HASH."
  (declare #.*standard-optimize-settings*)
  (1+ (mod hash (1- (length vector)))))

(defun in-charset-p (char set)
  "Checks whether the character CHAR is in the charset SET."
  (declare #.*standard-optimize-settings*)
  (declare (character char) (charset set))
  (let ((vector (charset-vector set))
        (depth (charset-depth set))
        (code (char-code char)))
    (declare (fixnum depth))
    ;; as long as the set remains reasonably small, we use non-linear
    ;; hashing - the first hash of any character is its CHAR-CODE, and
    ;; subsequent hashes are computed by MIX above
    (cond ((or
            ;; depth 0 is special - each char maps only to its code,
            ;; nothing else
            (zerop depth)
            ;; index 0 is special - only #\Null maps to it, no matter
            ;; what the depth is
            (zerop code))
           (eq char (char vector code)))
          (t
           ;; otherwise hash starts out as the character code, but
           ;; maps to indexes 1-N
           (let ((hash code))
             (tagbody
              :retry
              (let* ((index (compute-index hash vector))
                     (x (char vector index)))
                (cond ((eq x (code-char 0))
                       ;; empty, no need to probe further
                       (return-from in-charset-p nil))
                      ((eq x char)
                       ;; got it
                       (return-from in-charset-p t))
                      ((zerop (decf depth))
                       ;; max probe depth reached, nothing found
                       (return-from in-charset-p nil))
                      (t
                       ;; nothing yet, try next place
                       (setf hash (mix code hash))
                       (go :retry))))))))))

(defun add-to-charset (char set)
  "Adds the character CHAR to the charset SET, extending SET if
necessary.  Returns CHAR."
  (declare #.*standard-optimize-settings*)
  (or (%add-to-charset char set t)
      (%add-to-charset/expand char set)
      (error "Oops, this should not happen..."))
  char)

(defun %add-to-charset (char set count)
  "Tries to add the character CHAR to the charset SET without
extending it.  Returns NIL if this fails.  Counts CHAR as new
if COUNT is true and it is added to SET."
  (declare #.*standard-optimize-settings*)
  (declare (character char) (charset set))
  (let ((vector (charset-vector set))
        (depth (charset-depth set))
        (code (char-code char)))
    (declare (fixnum depth))
    ;; see comments in IN-CHARSET-P for algorithm
    (cond ((or (zerop depth) (zerop code))
           (unless (eq char (char vector code))
             (setf (char vector code) char)
             (when count
               (incf (charset-count set))))
           char)
          (t
           (let ((hash code))
             (tagbody
              :retry
              (let* ((index (compute-index hash vector))
                     (x (char vector index)))
                (cond ((eq x (code-char 0))
                       (setf (char vector index) char)
                       (when count
                         (incf (charset-count set)))
                       (return-from %add-to-charset char))
                      ((eq x char)
                       (return-from %add-to-charset char))
                      ((zerop (decf depth))
                       ;; need to expand the table
                       (return-from %add-to-charset nil))
                      (t
                       (setf hash (mix code hash))
                       (go :retry))))))))))

(defun %add-to-charset/expand (char set)
  "Extends the charset SET and then adds the character CHAR to it."
  (declare #.*standard-optimize-settings*)
  (declare (character char) (charset set))
  (let* ((old-vector (charset-vector set))
         (new-size (* 2 (length old-vector))))
    (tagbody
     :retry
     ;; when the table grows large (currently over 1/3 of
     ;; CHAR-CODE-LIMIT), we dispense with hashing and just allocate a
     ;; storage vector with space for all characters, so that each
     ;; character always uses only the CHAR-CODE
     (multiple-value-bind (new-depth new-vector)
         (if (>= new-size #.(truncate char-code-limit 3))
           (values 0 (make-char-vector char-code-limit))
           (values +probe-depth+ (make-char-vector new-size)))
       (setf (charset-depth set) new-depth
             (charset-vector set) new-vector)
       (flet ((try-add (x)
                ;; don't count - old characters are already accounted
                ;; for, and might count the new one multiple times as
                ;; well
                (unless (%add-to-charset x set nil)
                  (assert (not (zerop new-depth)))
                  (setf new-size (* 2 new-size))
                  (go :retry))))
         (try-add char)
         (dotimes (i (length old-vector))
           (let ((x (char old-vector i)))
             (if (eq x (code-char 0))
               (when (zerop i)
                 (try-add x))
               (unless (zerop i)
                 (try-add x))))))))
    ;; added and expanded, /now/ count the new character.
    (incf (charset-count set))
    t))

(defun map-charset (function charset)
  "Calls FUNCTION with all characters in SET.  Returns NIL."
  (declare #.*standard-optimize-settings*)
  (declare (function function))
  (let* ((n (charset-count charset))
         (vector (charset-vector charset))
         (size (length vector)))
    ;; see comments in IN-CHARSET-P for algorithm
    (when (eq (code-char 0) (char vector 0))
      (funcall function (code-char 0))
      (decf n))
    (loop for i from 1 below size
          for char = (char vector i)
          unless (eq (code-char 0) char) do
          (funcall function char)
          ;; this early termination test should be worth it when
          ;; mapping across depth 0 charsets.
          (when (zerop (decf n))
            (return-from map-charset nil))))
  nil)

(defun create-charset-from-test-function (test-function start end)
  "Creates and returns a charset representing all characters with
character codes between START and END which satisfy TEST-FUNCTION."
  (declare #.*standard-optimize-settings*)
  (loop with charset = (make-charset)
        for code from start below end
        for char = (code-char code)
        when (and char (funcall test-function char))
        do (add-to-charset char charset)
        finally (return charset)))
