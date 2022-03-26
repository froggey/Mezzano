;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FLEXI-STREAMS; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/flexi-streams/util.lisp,v 1.24 2008/05/25 21:26:12 edi Exp $

;;; Copyright (c) 2005-2008, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :flexi-streams)

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(lw:with-unique-names lw:when-let)))

#-:lispworks
(defmacro when-let ((var form) &body body)
  "Evaluates FORM and binds VAR to the result, then executes BODY
if VAR has a true value."
  `(let ((,var ,form))
     (when ,var ,@body)))

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

#+:lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'with-rebinding)
          (macro-function 'lw:rebinding)))

#-:lispworks
(defmacro with-rebinding (bindings &body body)
  "WITH-REBINDING ( { var | (var prefix) }* ) form*

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

(defun normalize-external-format-name (name)
  "Converts NAME \(a symbol) to a `canonical' name for an
external format, e.g. :LATIN1 will be converted to :ISO-8859-1.
Also checks if there is an external format with that name and
signals an error otherwise."
  (let ((real-name (cdr (find name flex::+name-map+
                              :test (lambda (item pair)
                                      (or (string-equal item (cdr pair))
                                          (string-equal item (car pair))))))))
    (unless real-name
      (error 'external-format-error
             :format-control "~S is not known to be a name for an external format."
             :format-arguments (list name)))
    real-name))

(defun ascii-name-p (name)
  "Checks whether NAME is the keyword :ASCII."
  (eq name :us-ascii))

(defun koi8-r-name-p (name)
  "Checks whether NAME is the keyword :KOI8-R."
  (eq name :koi8-r))

(defun code-page-name-p (name)
  "Checks whether NAME is the keyword :CODE-PAGE."
  (eq name :code-page))

(defun iso-8859-name-p (name)
  "Checks whether NAME \(a keyword) names one of the known
ISO-8859 encodings."
  (find name +iso-8859-tables+ :key #'car))

(defun known-code-page-id-p (id)
  "Checks whether ID \(a number) denotes one of the known Windows
code pages."
  (and (find id +code-page-tables+ :key #'car)
       id))

#+:lispworks
(defun sans (plist &rest keys)
  "Returns PLIST with keyword arguments from KEYS removed."
  (sys::remove-properties plist keys))

#-:lispworks
(defun sans (plist &rest keys)
  "Returns PLIST with keyword arguments from KEYS removed."
  ;; stolen from Usenet posting <3247672165664225@naggum.no> by Erik
  ;; Naggum
  (let ((sans ()))
    (loop
      (let ((tail (nth-value 2 (get-properties plist keys))))
        ;; this is how it ends
        (unless tail
          (return (nreconc sans plist)))
        ;; copy all the unmatched keys
        (loop until (eq plist tail) do
              (push (pop plist) sans)
              (push (pop plist) sans))
        ;; skip the matched key
        (setq plist (cddr plist))))))

#+:lispworks
(defmacro with-accessors (slot-entries instance &body body)
  "For LispWorks, we prefer SLOT-VALUE over accessors for better
performance."
  ;; note that we assume that the variables have the same names as the
  ;; slots
  `(with-slots ,(mapcar #'car slot-entries)
       ,instance
     ,@body))

(defun make-octet-buffer (&optional (size +buffer-size+))
  "Creates and returns a fresh buffer \(a specialized array) of size
+BUFFER-SIZE+ to hold octets."
  (declare #.*standard-optimize-settings*)
  (make-array size :element-type 'octet))

(defun type-equal (type1 type2)
  "Whether TYPE1 and TYPE2 denote the same type."
  (declare #.*standard-optimize-settings*)
  (and (subtypep type1 type2)
       (subtypep type2 type1)))

(defun maybe-rewind (stream octets)
  "Tries to `rewind' the \(binary) stream STREAM by OCTETS octets.
Returns T if it succeeds, otherwise NIL."
  (when-let (position (file-position stream))
    (if (file-position stream (- position octets)) t nil)))

(defmacro logand* (x y)
  "Solely for optimization purposes.  Some Lisps need it, some don't."
  `(the fixnum (logand ,x ,y)))

(defmacro logior* (x y)
  "Solely for optimization purposes.  Some Lisps need it, some don't."
  `(the fixnum (logior ,x ,y)))

(defmacro ash* (integer count)
  "Solely for optimization purposes.  Some Lisps need it, some don't."
  `(the fixnum (ash ,integer ,count)))
