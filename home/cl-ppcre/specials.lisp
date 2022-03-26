;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/specials.lisp,v 1.43 2009/10/28 07:36:15 edi Exp $

;;; globally declared special variables

;;; Copyright (c) 2002-2009, Dr. Edmund Weitz. All rights reserved.

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

;;; special variables used to effect declarations

(defvar *standard-optimize-settings*
  '(optimize
    speed
    (space 0)
    (debug 1)
    (compilation-speed 0))
  "The standard optimize settings used by most declaration expressions.")

(defvar *special-optimize-settings*
  '(optimize speed space)
  "Special optimize settings used only by a few declaration expressions.")

;;; special variables used by the lexer/parser combo

(defvar *extended-mode-p* nil
  "Whether the parser will start in extended mode.")
(declaim (boolean *extended-mode-p*))

;;; special variables used by the SCAN function and the matchers

(defvar *regex-char-code-limit* char-code-limit
  "The upper exclusive bound on the char-codes of characters which can
occur in character classes.  Change this value BEFORE creating
scanners if you don't need the \(full) Unicode support of
implementations like AllegroCL, CLISP, LispWorks, or SBCL.")
(declaim (fixnum *regex-char-code-limit*))
  
(defvar *string* (make-sequence #+:lispworks 'lw:simple-text-string
                                #-:lispworks 'simple-string
                                0)
  "The string which is currently scanned by SCAN.
Will always be coerced to a SIMPLE-STRING.")
#+:lispworks
(declaim (lw:simple-text-string *string*))
#-:lispworks
(declaim (simple-string *string*))

(defvar *start-pos* 0
  "Where to start scanning within *STRING*.")
(declaim (fixnum *start-pos*))

(defvar *real-start-pos* nil
  "The real start of *STRING*. This is for repeated scans and is only used internally.")
(declaim (type (or null fixnum) *real-start-pos*))

(defvar *end-pos* 0
  "Where to stop scanning within *STRING*.")
(declaim (fixnum *end-pos*))

(defvar *reg-starts* (make-array 0)
  "An array which holds the start positions
of the current register candidates.")
(declaim (simple-vector *reg-starts*))
  
(defvar *regs-maybe-start* (make-array 0)
  "An array which holds the next start positions
of the current register candidates.")
(declaim (simple-vector *regs-maybe-start*))

(defvar *reg-ends* (make-array 0)
  "An array which holds the end positions
of the current register candidates.")
(declaim (simple-vector *reg-ends*))

(defvar *end-string-pos* nil
  "Start of the next possible end-string candidate.")

(defvar *rep-num* 0
  "Counts the number of \"complicated\" repetitions while the matchers
are built.")
(declaim (fixnum *rep-num*))

(defvar *zero-length-num* 0
  "Counts the number of repetitions the inner regexes of which may
have zero-length while the matchers are built.")
(declaim (fixnum *zero-length-num*))

(defvar *repeat-counters* (make-array 0
                                      :initial-element 0
                                      :element-type 'fixnum)
  "An array to keep track of how often
repetitive patterns have been tested already.")
(declaim (type (array fixnum (*)) *repeat-counters*))

(defvar *last-pos-stores* (make-array 0)
  "An array to keep track of the last positions
where we saw repetitive patterns.
Only used for patterns which might have zero length.")
(declaim (simple-vector *last-pos-stores*))

(defvar *use-bmh-matchers* nil
  "Whether the scanners created by CREATE-SCANNER should use the \(fast
but large) Boyer-Moore-Horspool matchers.")

(defvar *optimize-char-classes* nil
  "Whether character classes should be compiled into look-ups into
O\(1) data structures.  This is usually fast but will be costly in
terms of scanner creation time and might be costly in terms of size if
*REGEX-CHAR-CODE-LIMIT* is high.  This value will be used as the :KIND
keyword argument to CREATE-OPTIMIZED-TEST-FUNCTION - see there for the
possible non-NIL values.")

(defvar *property-resolver* nil
  "Should be NIL or a designator for a function which accepts strings
and returns unary character test functions or NIL.  This 'resolver' is
intended to handle `character properties' like \\p{IsAlpha}.  If
*PROPERTY-RESOLVER* is NIL, then the parser will simply treat \\p and
\\P as #\\p and #\\P as in older versions of CL-PPCRE.")

(defvar *allow-quoting* nil
  "Whether the parser should support Perl's \\Q and \\E.")

(defvar *allow-named-registers* nil
  "Whether the parser should support AllegroCL's named registers
\(?<name>\"<regex>\") and back-reference \\k<name> syntax.")

(pushnew :cl-ppcre *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-ppcre/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-ppcre
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
               
