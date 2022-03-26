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

(def-suite editing-tests :description "The test suite for
DREI-EDITING related tests." :in drei-tests)

(in-suite editing-tests)

(defmacro with-buffer-action-test-functions (&body body)
  ;; Anamorphic.
  `(labels ((test-buffer-action-with-stickto (action offset mark-stickto initial-contents
                                                     end-contents kill-ring-end-contents)
              (with-buffer (buffer :initial-contents initial-contents)
                (with-view (view :buffer buffer)
                  ;; If your test surpasses this max size, you're
                  ;; probably doing something semi-insane.
                  (let* ((kill-ring (make-instance 'kill-ring :max-size 20))
                         (*kill-ring* kill-ring))
                    (let ((mark (clone-mark (point buffer) mark-stickto)))
                      (setf (offset mark) offset)
                      (funcall action view mark)
                      (is (string= end-contents
                                   (buffer-contents buffer)))
                      ;; `kill-ring-end-contents' is a list of what should
                      ;; be at the top of the kill ring now. Assert that
                      ;; it is.
                      (handler-case (mapcar #'(lambda (killed-string expected)
                                                (is (string= expected killed-string)))
                                            (loop repeat (length kill-ring-end-contents)
                                               collecting (coerce (kill-ring-yank kill-ring nil) 'string)
                                               do (rotate-yank-position kill-ring 1))
                                            kill-ring-end-contents)
                        (empty-kill-ring ()
                          (fail "Kill ring did not contain enough values to satisfy ~A" kill-ring-end-contents))))))))
            (test-buffer-action (action offset initial-contents end-contents kill-ring-end-contents)
              (test-buffer-action-with-stickto action offset :left initial-contents end-contents kill-ring-end-contents)
              (test-buffer-action-with-stickto action offset :right initial-contents end-contents kill-ring-end-contents)))
     ,@body))

(defmacro deletion/killing-test (unit
                                 ((fromstart-contents1 (&rest fromstart-contents1-killed))
                                  (fromstart-contents2 (&rest fromstart-contents2-killed)))
                                 ((fromend-contents1 (&rest fromend-contents1-killed))
                                  (fromend-contents2 (&rest fromend-contents2-killed)))
                                 (offset count
                                         (forward-contents (&rest forward-contents-killed))
                                         (backward-contents (&rest backward-contents-killed)))
                                 initial-contents)
  ;; Easy to get the ungodly amount of arguments wrong, so insert some
  ;; type-checks...
  (check-type unit symbol)
  (check-type fromstart-contents1 string)
  (check-type fromstart-contents2 string)
  (check-type fromend-contents1 string)
  (check-type fromend-contents2 string)
  (check-type offset integer)
  (check-type count integer)
  (check-type forward-contents string)
  (check-type backward-contents string)
  (check-type initial-contents string)
  (let ((forward-delete (intern (format nil "FORWARD-DELETE-~A" unit)))
        (backward-delete (intern (format nil "BACKWARD-DELETE-~A" unit)))
        (forward-kill (intern (format nil "FORWARD-KILL-~A" unit)))
        (backward-kill (intern (format nil "BACKWARD-KILL-~A" unit)))
        (end (length initial-contents)))
    (flet ((make-deletion-test-function-case
               (operation motion-count initial-offset after-contents kill-ring-end-contents)
             `(test-buffer-action #'(lambda (view mark)
                                      (,operation mark (syntax view) ,motion-count nil))
                                  ,initial-offset ,initial-contents
                                  ,after-contents
                                  ',kill-ring-end-contents)))
      `(progn 
         (test ,forward-delete
           (with-drei-environment (:initial-contents ,initial-contents)
             (with-buffer-action-test-functions
               ,(make-deletion-test-function-case forward-delete 1 0 fromstart-contents1 nil)
               ,(make-deletion-test-function-case forward-delete 2 0 fromstart-contents2 nil)
               ,(make-deletion-test-function-case forward-delete count offset forward-contents nil))))
         (test ,backward-delete
           (with-drei-environment (:initial-contents ,initial-contents)
             (with-buffer-action-test-functions
               ,(make-deletion-test-function-case backward-delete 1 end fromend-contents1 nil)
               ,(make-deletion-test-function-case backward-delete 2 end fromend-contents2 nil)
               ,(make-deletion-test-function-case backward-delete count offset backward-contents nil))))
         (test ,forward-kill
           (with-drei-environment (:initial-contents ,initial-contents)
             (with-buffer-action-test-functions
               ,(make-deletion-test-function-case forward-kill 1 0 fromstart-contents1 fromstart-contents1-killed)
               ,(make-deletion-test-function-case forward-kill 2 0 fromstart-contents2 fromstart-contents2-killed)
               ,(make-deletion-test-function-case forward-kill count offset forward-contents forward-contents-killed))))
         (test ,backward-kill
           (with-drei-environment (:initial-contents ,initial-contents)
             (with-buffer-action-test-functions
               ,(make-deletion-test-function-case backward-kill 1 end fromend-contents1 fromend-contents1-killed)
               ,(make-deletion-test-function-case backward-kill 2 end fromend-contents2 fromend-contents2-killed)
               ,(make-deletion-test-function-case backward-kill count offset backward-contents backward-contents-killed))))))))

(deletion/killing-test line
                       (("It has multiple lines.
The semantics of the kill-line function
are not very intuitive.

Not to the user, at least, but to the programmer, they are
good." ("This is a string.
"))
                        ("The semantics of the kill-line function
are not very intuitive.

Not to the user, at least, but to the programmer, they are
good." ("This is a string.
It has multiple lines.
")))
                       (("This is a string.
It has multiple lines.
The semantics of the kill-line function
are not very intuitive.

Not t" ("o the user, at least, but to the programmer, they are
good."))
                        ("This is a string.
It has multiple lines.
The semantics of the kill-line function
are not very intuitive.
" ("
Not to the user, at least, but to the programmer, they are
good.")))
                       (50 2 ("This is a string.
It has multiple lines.
The seman
Not to the user, at least, but to the programmer, they are
good." ("tics of the kill-line function
are not very intuitive.
"))
                           ("This is atics of the kill-line function
are not very intuitive.

Not to the user, at least, but to the programmer, they are
good." (" string.
It has multiple lines.
The seman")))
                       "This is a string.
It has multiple lines.
The semantics of the kill-line function
are not very intuitive.

Not to the user, at least, but to the programmer, they are
good.")

(deletion/killing-test word
                       (("! This is a sentence with words." (" Word"))
                        (" is a sentence with words." (" Word! This")))
                       ((" Word! This is a sentence with " ("words."))
                        (" Word! This is a sentence " ("with words.")))
                       (20 3 (" Word! This is a sen." ("tence with words"))
                           (" Word! This tence with words." ("is a sen")))
                       " Word! This is a sentence with words.")

(deletion/killing-test page
                       (("
B
C
D
E
F" ("A"))
                        ("
C
D
E
F" ("A
B")))
                       (("A
B
C
D
E
" ("F"))
                        ("A
B
C
D
" ("E
F")))
                       (5 2 ("A
B

E
F" ("C
D"))
                          ("C
D
E
F" ("A
B
")))
                       "A
B
C
D
E
F")

(deletion/killing-test paragraph
                       (("

Paragraphs are seperated by double newlines.

That really just looks like a single blank line, but it must not
contain space characters.

If this rule is not followed, that is, as always, a bug.

And it should be fixed.

This is the last paragraph." ("I am testing paragraphs."))
                        ("

That really just looks like a single blank line, but it must not
contain space characters.

If this rule is not followed, that is, as always, a bug.

And it should be fixed.

This is the last paragraph." ("I am testing paragraphs.

Paragraphs are seperated by double newlines."))) 
                       (("I am testing paragraphs.

Paragraphs are seperated by double newlines.

That really just looks like a single blank line, but it must not
contain space characters.

If this rule is not followed, that is, as always, a bug.

And it should be fixed.

" ("This is the last paragraph."))
                        ("I am testing paragraphs.

Paragraphs are seperated by double newlines.

That really just looks like a single blank line, but it must not
contain space characters.

If this rule is not followed, that is, as always, a bug.

" ("And it should be fixed.

This is the last paragraph.")))
                       (100 2 ("I am testing paragraphs.

Paragraphs are seperated by double newlines.

That really just looks like 

This is the last paragraph." ("a single blank line, but it must not
contain space characters.

If this rule is not followed, that is, as always, a bug.

And it should be fixed."))
                            ("I am testing paragraphs.

a single blank line, but it must not
contain space characters.

If this rule is not followed, that is, as always, a bug.

And it should be fixed.

This is the last paragraph." ("Paragraphs are seperated by double newlines.

That really just looks like ")))
                       "I am testing paragraphs.

Paragraphs are seperated by double newlines.

That really just looks like a single blank line, but it must not
contain space characters.

If this rule is not followed, that is, as always, a bug.

And it should be fixed.

This is the last paragraph.")

(defmacro transposition-test (unit (at-beginning-contents)
                              (at-end-contents &optional no-signal)
                              (&rest offset-contents-tests)
                              initial-contents)
  (check-type unit symbol)
  (check-type at-beginning-contents string)
  (check-type at-end-contents string)
  (check-type initial-contents string)
  (let ((transpose (intern (format nil "TRANSPOSE-~AS" unit)))
        (end (length initial-contents)))
    (flet ((make-transposition-test-function-case
               (operation initial-offset after-contents kill-ring-end-contents)
             `(test-buffer-action #'(lambda (view mark)
                                      (,operation mark (syntax view)))
                                  ,initial-offset ,initial-contents
                                  ,after-contents
                                  ',kill-ring-end-contents))
           (make-transposition-test-function-case-with-stickto
               (operation initial-offset stickto after-contents kill-ring-end-contents)
             `(test-buffer-action-with-stickto
               #'(lambda (view mark)
                   (,operation mark (syntax view)))
               ,initial-offset ,stickto ,initial-contents
               ,after-contents
               ',kill-ring-end-contents)))
      `(progn 
         (test ,transpose
           (with-buffer-action-test-functions
             ,(make-transposition-test-function-case transpose 0 at-beginning-contents nil)
             ;; Transposing at the end of the buffer is an error
             ,(if no-signal
                  `(progn
                     ,(make-transposition-test-function-case-with-stickto
                       transpose end :left at-end-contents nil)
                     ,(make-transposition-test-function-case-with-stickto
                       transpose end :right at-end-contents nil))
                  `(progn
                     (signals motion-limit-error
                       ,(make-transposition-test-function-case-with-stickto
                         transpose end :left at-end-contents nil))
                     (signals motion-limit-error
                       ,(make-transposition-test-function-case-with-stickto
                         transpose end :right at-end-contents nil))))
             ,@(loop for (offset after-contents) in offset-contents-tests
                  collecting (make-transposition-test-function-case
                              transpose offset after-contents nil))))))))

(transposition-test line
                    ("Second line.
First line.
Third line.")
                    ("First line.
Third line.
Second line." t)
                    ()
                    "First line.
Second line.
Third line.")

(transposition-test word
                    ("words Many, great words!")
                    ("")
                    ((8 "Many great, words words!")
                     (10 "Many great, words words!"))
                    "Many words, great words!")

(transposition-test page
                    ("B
A
C
D
E
F")
                    ("")
                    ((5 "A
C
B
D
E
F"))
                    "A
B
C
D
E
F")

(transposition-test paragraph
                    ("B

A

C

D

E

F")
                    ("")
                    ()
                    "A

B

C

D

E

F")
