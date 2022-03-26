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

(def-suite core-tests :description "The test suite for
DREI-CORE related tests." :in drei-tests)

(in-suite core-tests)

(test possibly-fill-line
  (with-drei-environment ()
    (possibly-fill-line)
    (is (string= (buffer-contents) "")))
  (with-drei-environment
      (:initial-contents "Very long line, this should be filled, if auto-fill is on.")

    (setf (auto-fill-column (current-view)) 200
          (auto-fill-mode (current-view)) nil
          (offset (point)) (size (current-buffer)))
    (possibly-fill-line)
    (is (string= (buffer-contents)
                 "Very long line, this should be filled, if auto-fill is on."))
    
    (setf (auto-fill-mode (current-view)) t)
    (possibly-fill-line)
    (is (string= (buffer-contents)
                 "Very long line, this should be filled, if auto-fill is on."))
    
    (setf (auto-fill-column (current-view)) 20)
    (possibly-fill-line)
    (is (string= (buffer-contents)
                 "Very long line,
this should be
filled, if auto-fill
is on."))))

(test back-to-indentation
  (with-drei-environment
      (:initial-contents #.(format nil "  ~A Foobar!" #\Tab))
    (end-of-buffer (point))
    (back-to-indentation (point) (current-syntax))
    (is (= (offset (point)) 4))))

(test insert-character
  ;; Test:
  ;; - Overwriting
  ;; - Auto-filling
  ;; - Standard insertion
  (with-drei-environment ()
    (setf (auto-fill-mode (current-view)) nil
          (overwrite-mode (current-view)) nil)
    (insert-character #\a)
    (is (string= (buffer-contents) "a"))
    (insert-character #\b)
    (is (string= (buffer-contents) "ab"))
    (backward-object (point) 2)
    (insert-character #\t)
    (is (string= (buffer-contents) "tab"))
    (setf (overwrite-mode (current-view)) t)
    (insert-character #\i)
    (insert-character #\p)
    (is (string= (buffer-contents) "tip"))
    ;; TODO: Also test dynamic abbreviations?
    ))

(test delete-horizontal-space
  (with-drei-environment (:initial-contents "     foo")
    (setf (offset (point)) 3)
    (delete-horizontal-space (point) (current-syntax))
    (is (string= (buffer-contents) "foo"))
    (insert-sequence (point) "     ")
    (setf (offset (point)) 3)
    (delete-horizontal-space (point) (current-syntax) t)
    (is (string= (buffer-contents) "  foo"))
    (delete-horizontal-space (point) (current-syntax))
    (is (string= (buffer-contents) "foo"))
    (delete-horizontal-space (point) (current-syntax))
    (is (string= (buffer-contents) "foo"))))

(test indent-current-line
  (with-drei-environment (:initial-contents "Foo bar baz
  Quux")
    (indent-current-line (current-view) (point))
    (is (string= (buffer-contents)
                 "Foo bar baz
  Quux"))
    (setf (offset (point)) 12)
    (indent-current-line (current-view) (point))
    (is (string= (buffer-contents)
                 "Foo bar baz
Quux"))))

(test insert-pair
  (with-drei-environment ()
    (insert-pair (mark) (current-syntax))
    (buffer-is "()")
    (beginning-of-buffer (point))
    (insert-pair (point) (current-syntax) 0 #\[ #\])
    (buffer-is "[] ()")))

(test goto-position
  (with-drei-environment (:initial-contents "foobarbaz")
    (goto-position (point) 5)
    (is (= (offset (point)) 5))))

(test goto-line
  (with-drei-environment (:initial-contents "First line
Second line
Third line")
    (goto-line (point) 1)
    (is (= (line-number (point)) 0))
    (is (= (offset (point)) 0))
    (goto-line (point) 2)
    (is (= (line-number (point)) 1))
    (is (= (offset (point)) 11))
    (goto-line (point) 3)
    (is (= (line-number (point)) 2))
    (is (= (offset (point)) 23))
    (goto-line (point) 4)
    (is (= (line-number (point)) 2))
    (is (= (offset (point)) 23))))

(test replace-one-string
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (replace-one-string (point) 17 "foo bar" nil)
    (buffer-is "foo bar"))
  (with-drei-environment (:initial-contents "drei climacs drei")
    (replace-one-string (point) 17 "foo bar" t)
    (buffer-is "foo bar"))
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (replace-one-string (point) 17 "foo bar" t)
    (buffer-is "Foo Bar"))
  (with-drei-environment (:initial-contents "DREI CLIMACS DREI")
    (replace-one-string (point) 17 "foo bar" t)
    (buffer-is "FOO BAR")))

(test downcase-word
  (with-drei-environment ()
    (downcase-word (point) (current-syntax) 1)
    (is (string= (buffer-contents) "")))
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (downcase-word (point) (current-syntax) 1)
    (buffer-is "drei Climacs Drei")
    (downcase-word (point) (current-syntax) 1)
    (buffer-is "drei climacs Drei")
    (downcase-word (point) (current-syntax) 1)
    (buffer-is "drei climacs drei"))
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (downcase-word (point) (current-syntax) 0)
    (buffer-is "Drei Climacs Drei"))
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (downcase-word (point) (current-syntax) 2)
    (buffer-is "drei climacs Drei"))
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (downcase-word (point) (current-syntax) 3)
    (buffer-is "drei climacs drei"))
  (with-drei-environment (:initial-contents "CLI MA CS CLIMACS")
    (downcase-word (point) (current-syntax) 3)
    (is (buffer-is "cli ma cs CLIMACS"))
    (is (= 9 (offset (point))))))

(test upcase-word
  (with-drei-environment ()
    (upcase-word (point) (current-syntax) 1)
    (is (string= (buffer-contents) "")))
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (upcase-word (point) (current-syntax) 1)
    (buffer-is "DREI Climacs Drei")
    (upcase-word (point) (current-syntax) 1)
    (buffer-is "DREI CLIMACS Drei")
    (upcase-word (point) (current-syntax) 1)
    (buffer-is "DREI CLIMACS DREI"))
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (upcase-word (point) (current-syntax) 0)
    (buffer-is "Drei Climacs Drei"))
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (upcase-word (point) (current-syntax) 2)
    (buffer-is "DREI CLIMACS Drei"))
  (with-drei-environment (:initial-contents "Drei Climacs Drei")
    (upcase-word (point) (current-syntax) 3)
    (buffer-is "DREI CLIMACS DREI"))
  (with-drei-environment (:initial-contents "cli ma cs climacs")
    (let ((m (clone-mark (point) :right)))
      (setf (offset m) 0)
      (upcase-word m (current-syntax) 3)
      (is (string= (buffer-contents)
                   "CLI MA CS climacs"))
      (is (= (offset m) 9)))))

(test capitalize-word
  (with-drei-environment ()
    (capitalize-word (point) (current-syntax) 1)
    (is (string= (buffer-contents) "")))
  (with-drei-environment (:initial-contents "drei climacs drei")
    (capitalize-word (point) (current-syntax) 1)
    (buffer-is "Drei climacs drei")
    (capitalize-word (point) (current-syntax) 1)
    (buffer-is "Drei Climacs drei")
    (capitalize-word (point) (current-syntax) 1)
    (buffer-is "Drei Climacs Drei"))
  (with-drei-environment (:initial-contents "drei climacs drei")
    (capitalize-word (point) (current-syntax) 0)
    (buffer-is "drei climacs drei"))
  (with-drei-environment (:initial-contents "drei climacs drei")
    (capitalize-word (point) (current-syntax) 2)
    (buffer-is "Drei Climacs drei"))
  (with-drei-environment (:initial-contents "drei climacs drei")
    (capitalize-word (point) (current-syntax) 3)
    (buffer-is "Drei Climacs Drei"))
  (with-drei-environment ( :initial-contents "cli ma cs climacs")
    (let ((m (clone-mark (point) :right)))
      (setf (offset m) 0)
      (capitalize-word m (current-syntax) 3)
      (is (string= (buffer-contents)
                   "Cli Ma Cs climacs"))
      (is (= (offset m) 9)))))

(test indent-region
  ;; FIXME: Sadly, we can't test this function, because it requires a
  ;; CLIM pane.
  )

(test fill-line
  (flet ((fill-it (fill-column)
           (fill-line (point)
                      (lambda (mark)
                        (proper-line-indentation (current-view) mark))
                      fill-column
                      (tab-space-count (current-view))
                      (current-syntax))))
    (with-drei-environment (:initial-contents "climacs  climacs  climacs  climacs")
      (let ((m (clone-mark (point) :right)))
        (setf (offset m) 25)
        (fill-line m #'(lambda (m) (declare (ignore m)) 8) 10 8 (current-syntax))
        (is (= (offset m) 25))
        (buffer-is "climacs
	climacs
	climacs  climacs")))
    (with-drei-environment (:initial-contents "climacs  climacs  climacs  climacs")
      (let ((m (clone-mark (point) :right)))
        (setf (offset m) 25)
        (fill-line m #'(lambda (m) (declare (ignore m)) 8) 10 8 (current-syntax) nil)
        (is (= (offset m) 27))
        (buffer-is "climacs 
	climacs 
	climacs  climacs")))
    (with-drei-environment (:initial-contents #.(format nil "climacs~Aclimacs~Aclimacs~Aclimacs"
                                                     #\Tab #\Tab #\Tab))
      (let ((m (clone-mark (point) :left)))
        (setf (offset m) 25)
        (fill-line m #'(lambda (m) (declare (ignore m)) 8) 10 8 (current-syntax))
        (is (= (offset m) 27))
        (buffer-is "climacs
	climacs
	climacs	climacs")))
    (with-drei-environment (:initial-contents #.(format nil "climacs~Aclimacs~Aclimacs~Aclimacs"
                                                     #\Tab #\Tab #\Tab))
      (let ((m (clone-mark (point) :left)))
        (setf (offset m) 25)
        (fill-line m #'(lambda (m) (declare (ignore m)) 8) 10 8 (current-syntax))
        (is (= (offset m) 27))
        (buffer-is "climacs
	climacs
	climacs	climacs")))
    (with-drei-environment (:initial-contents "c l i m a c s")
      (let ((m (clone-mark (point) :right)))
        (setf (offset m) 1)
        (fill-line m #'(lambda (m) (declare (ignore m)) 8) 0 8 (current-syntax))
        (is (= (offset m) 1))
        (buffer-is "c l i m a c s")))
    (with-drei-environment (:initial-contents "c l i m a c s")
      (let ((m (clone-mark (point) :right)))
        (setf (offset m) 1)
        (fill-line m #'(lambda (m) (declare (ignore m)) 8) 0 8 (current-syntax) nil)
        (is (= (offset m) 1))
        (buffer-is "c l i m a c s")))
    (with-drei-environment ()
      (fill-it 80)
      (buffer-is ""))
    (with-drei-environment
        (:initial-contents "Very long line, this should certainly be filled, if everything works")
      (end-of-buffer (point))
      (fill-it 200)
      (buffer-is "Very long line, this should certainly be filled, if everything works")
      (fill-it 20)
      (buffer-is "Very long line,
this should
certainly be filled,
if everything works"))))

(test fill-region
  (flet ((fill-it (fill-column)
           (fill-region (point) (mark) 
                        (lambda (mark)
                          (proper-line-indentation (current-view) mark))
                        fill-column
                        (tab-space-count (current-view))
                        (current-syntax))))
    (with-drei-environment
        (:initial-contents "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

What, you thought I would write something this long myself? Not a chance. Though this line is growing by a fair bit too, I better test it as well.")
      (end-of-line (mark))
      (fill-it 80)
      (buffer-is "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.

What, you thought I would write something this long myself? Not a chance. Though this line is growing by a fair bit too, I better test it as well.")
      (end-of-buffer (mark))
      (forward-paragraph (point) (current-syntax) 2 nil)
      (backward-paragraph (point) (current-syntax) 1)
      (fill-it 80)
      (buffer-is "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.

What, you thought I would write something this long myself? Not a chance.
Though this line is growing by a fair bit too, I better test it as well."))))

(test indent-line
  (dolist (stick-to '(:left :right))
    (with-drei-environment ()
      (buffer-is ""))
    (with-drei-environment (:initial-contents "I am to be indented")
      (indent-line (clone-mark (point) stick-to) 11 nil)
      (buffer-is "           I am to be indented"))
    (with-drei-environment (:initial-contents "I am to be indented")
      (indent-line (clone-mark (point) stick-to) 11 2)
      (buffer-is #. (format nil "~A~A~A~A~A I am to be indented"
                            #\Tab #\Tab #\Tab #\Tab #\Tab)))))

(test delete-indentation
  (with-drei-environment (:initial-contents "")
    (delete-indentation (current-syntax) (point))
    (buffer-is ""))
  (with-drei-environment (:initial-contents "Foo")
    (delete-indentation (current-syntax) (point))
    (buffer-is "Foo"))
  (with-drei-environment (:initial-contents " Foo")
    (delete-indentation (current-syntax) (point))
    (buffer-is "Foo"))
  (with-drei-environment (:initial-contents "   	Foo  ")
    (delete-indentation (current-syntax) (point))
    (buffer-is "Foo  "))
  (with-drei-environment (:initial-contents "   Foo  
  Bar
 Baz")
    (forward-line (point) (current-syntax))
    (delete-indentation (current-syntax) (point))
    (buffer-is "   Foo  
Bar
 Baz"))
  (with-drei-environment (:initial-contents "
foo bar")
    (let ((start (clone-mark (point (current-view))))
          (end (clone-mark (point (current-view))))
          (orig-contents (buffer-contents)))
      (beginning-of-buffer start)
      (end-of-buffer end)
      (do-buffer-region-lines (line start end)
        (delete-indentation (current-syntax) line))
      (buffer-is orig-contents))))

(test join-line
  (with-drei-environment (:initial-contents "
        climacs   ")
    (let ((m (clone-mark (point) :left)))
      (setf (offset m) 3)
      (join-line (current-syntax) m)
      (is (= (offset m) 0))
      (buffer-is "climacs   ")))
  (with-drei-environment (:initial-contents "
      climacs   ")
    (let ((m (clone-mark (point) :right)))
      (setf (offset m) 7)
      (join-line (current-syntax) m)
      (is (= (offset m) 0))
      (buffer-is "climacs   ")))
  (with-drei-environment (:initial-contents "   climacs   ")
    (let ((m (clone-mark (point) :left)))
      (setf (offset m) 7)
      (join-line (current-syntax) m)
      (is (= (offset m) 0))
      (buffer-is "   climacs   ")))
  (with-drei-environment (:initial-contents "climacs
   climacs   ")
    (let ((m (clone-mark (point) :right)))
      (setf (offset m) 12)
      (join-line (current-syntax) m)
      (is (= (offset m) 8))
      (buffer-is "climacs climacs   ")))
  (with-drei-environment (:initial-contents "

   climacs   ")
    (let ((m (clone-mark (point) :right)))
      (setf (offset m) 12)
      (join-line (current-syntax) m)
      (is (= (offset m) 0))
      (buffer-is "climacs   "))))

(test set-syntax
  (dolist (syntax-designator `("Lisp" drei-lisp-syntax::lisp-syntax
                                      ,(find-class 'drei-lisp-syntax::lisp-syntax)))
    (with-drei-environment ()
      (let ((old-syntax (current-syntax)))
       (set-syntax (current-view) syntax-designator)
       (is (not (eq old-syntax (current-syntax))))
       (is (typep (current-syntax) 'drei-lisp-syntax::lisp-syntax))))))

(test with-narrowed-buffer
  (with-drei-environment (:initial-contents "foo bar baz quux")
    (setf (offset (point)) 1
          (offset (mark)) (1- (size (current-buffer))))
    (let ((mark1 (clone-mark (point) :left))
          (mark2 (clone-mark (mark) :right)))
      (forward-object mark1)
      (backward-object mark2)
      (dolist (low (list 2 mark1))
        (dolist (high (list (- (size (current-buffer)) 2) mark2))
          (with-narrowed-buffer ((drei-instance) low high t)
            (is (= (offset (point)) 2))
            (is (= (offset (mark)) (- (size (current-buffer)) 2)))))))))
