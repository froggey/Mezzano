;;; -*- Mode: Lisp; Package: DREI-ABBREV -*-

;;;  (c) copyright 2004 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004 by
;;;           Elliott Johnson (ejohnson@fasl.info)

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

;;; Abbrevs are expanded by a call to the generic function
;;; expand-abbrev.  That function takes a word to be expanded and an
;;; instance of the class abbrev-expander and returns either NIL
;;; (meaning there was no expansion for this word) or another string
;;; which is the expansion of the word.  
;;;
;;; We define a particular sublcass of abbrev-expander which just
;;; contains a dictionary (an alist) of <word,expansion> pairs and
;;; which does case-mangling on the expansion according to the case of
;;; the word.  Client code would typically create other subclasses of
;;; abbrev-expander that can do more sophisticated abbrev expansion.

(in-package :drei-abbrev)

;;; the protocol class for all abbrev expanders. 
(defclass abbrev-expander () ()
  (:documentation "The protocol class for all abbreviation expanders"))

(defgeneric expand-abbrev (word abbrev-expander)
  (:documentation "Given a word and an abbrev expander, return the 
expanded abbrev, or NIL if no expansion exists"))

(defclass dictionary-abbrev-expander (abbrev-expander)
  ((dictionary :initform '() :accessor dictionary
	       :documentation "A dictionary of abbreviations."))
  (:documentation "A protocol class specified for dictionary abbreviation expanders."))

(defgeneric add-abbrev (word expansion dictionary-abbrev-expander)
  (:documentation "Add an abbrev expansion to a dictionary abbrev expander"))

(defmethod add-abbrev (word expansion (expander dictionary-abbrev-expander))
  (push (cons word expansion) (dictionary expander)))

(defun string-upper-case-p (string)
  "A predicate testing if each character of a string is uppercase."
  (every #'upper-case-p string))

(defmethod expand-abbrev (word (expander dictionary-abbrev-expander))
  "Expands an abbrevated word by attempting to assocate it with a member of
an abbreviation dictionary.  If such an association is found, an uppercase,
expanded version of the abbrevation is returned."
  (let ((expansion (cdr (assoc word (dictionary expander) :test #'string-equal))))
    (when expansion
      (cond ((string-upper-case-p word) (string-upcase expansion))
	    ((upper-case-p (aref word 0)) (string-capitalize expansion))
	    (t expansion)))))

(defun possibly-expand-abbrev (mark)
  "Replaces a bit of abbreviated text with its fully expanded counterpart."
  (let ((buffer (buffer mark)))
    (when (and (not (beginning-of-buffer-p mark))
	       (constituentp (object-before mark)))
      (let ((offset1 (offset mark))
	    (offset2 (offset mark)))
	(loop until (zerop offset1)
	      while (constituentp (buffer-object buffer (1- offset1)))
	      do (decf offset1))
	(let ((expansion (expand-abbrev (coerce (buffer-sequence buffer offset1 offset2)
						'string)
					(abbrev-expander (implementation buffer)))))
	  (when expansion
	    (delete-buffer-range buffer offset1 (- offset2 offset1))
	    (insert-buffer-sequence buffer offset1 expansion)))))))

(defclass abbrev-mixin ()
  ((expander :initform (make-instance 'dictionary-abbrev-expander)
	     :initarg :expander :accessor abbrev-expander))
  (:documentation "A mixin class which adds abbreviation expansion facilities to
a buffer via the accessor \"abbrev-expander\""))
