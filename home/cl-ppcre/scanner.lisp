;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/scanner.lisp,v 1.36 2009/09/17 19:17:31 edi Exp $

;;; Here the scanner for the actual regex as well as utility scanners
;;; for the constant start and end strings are created.

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

(defmacro bmh-matcher-aux (&key case-insensitive-p)
  "Auxiliary macro used by CREATE-BMH-MATCHER."
  (let ((char-compare (if case-insensitive-p 'char-equal 'char=)))
    `(lambda (start-pos)
       (declare (fixnum start-pos))
       (if (or (minusp start-pos)
               (> (the fixnum (+ start-pos m)) *end-pos*))
         nil
         (loop named bmh-matcher
               for k of-type fixnum = (+ start-pos m -1)
               then (+ k (max 1 (aref skip (char-code (schar *string* k)))))
               while (< k *end-pos*)
               do (loop for j of-type fixnum downfrom (1- m)
                        for i of-type fixnum downfrom k
                        while (and (>= j 0)
                                   (,char-compare (schar *string* i)
                                                  (schar pattern j)))
                        finally (if (minusp j)
                                  (return-from bmh-matcher (1+ i)))))))))

(defun create-bmh-matcher (pattern case-insensitive-p)
  "Returns a Boyer-Moore-Horspool matcher which searches the (special)
simple-string *STRING* for the first occurence of the substring
PATTERN.  The search starts at the position START-POS within *STRING*
and stops before *END-POS* is reached.  Depending on the second
argument the search is case-insensitive or not.  If the special
variable *USE-BMH-MATCHERS* is NIL, use the standard SEARCH function
instead.  \(BMH matchers are faster but need much more space.)"
  (declare #.*standard-optimize-settings*)
  ;; see <http://www-igm.univ-mlv.fr/~lecroq/string/node18.html> for
  ;; details
  (unless *use-bmh-matchers*
    (let ((test (if case-insensitive-p #'char-equal #'char=)))
      (return-from create-bmh-matcher
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (and (not (minusp start-pos))
               (search pattern
                       *string*
                       :start2 start-pos
                       :end2 *end-pos*
                       :test test))))))
  (let* ((m (length pattern))
	 (skip (make-array *regex-char-code-limit*
                           :element-type 'fixnum
                           :initial-element m)))
    (declare (fixnum m))
    (loop for k of-type fixnum below m
          if case-insensitive-p
          do (setf (aref skip (char-code (char-upcase (schar pattern k)))) (- m k 1)
                   (aref skip (char-code (char-downcase (schar pattern k)))) (- m k 1))
	  else
          do (setf (aref skip (char-code (schar pattern k))) (- m k 1)))
    (if case-insensitive-p
      (bmh-matcher-aux :case-insensitive-p t)
      (bmh-matcher-aux))))

(defmacro char-searcher-aux (&key case-insensitive-p)
  "Auxiliary macro used by CREATE-CHAR-SEARCHER."
  (let ((char-compare (if case-insensitive-p 'char-equal 'char=)))
    `(lambda (start-pos)
      (declare (fixnum start-pos))
      (and (not (minusp start-pos))
           (loop for i of-type fixnum from start-pos below *end-pos*
                 thereis (and (,char-compare (schar *string* i) chr) i))))))

(defun create-char-searcher (chr case-insensitive-p)
  "Returns a function which searches the (special) simple-string
*STRING* for the first occurence of the character CHR. The search
starts at the position START-POS within *STRING* and stops before
*END-POS* is reached.  Depending on the second argument the search is
case-insensitive or not."
  (declare #.*standard-optimize-settings*)
  (if case-insensitive-p
    (char-searcher-aux :case-insensitive-p t)
    (char-searcher-aux)))

(declaim (inline newline-skipper))
(defun newline-skipper (start-pos)
  "Finds the next occurence of a character in *STRING* which is behind
a #\Newline."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum start-pos))
  ;; we can start with (1- START-POS) without testing for (PLUSP
  ;; START-POS) because we know we'll never call NEWLINE-SKIPPER on
  ;; the first iteration
  (loop for i of-type fixnum from (1- start-pos) below *end-pos*
        thereis (and (char= (schar *string* i)
                            #\Newline)
                     (1+ i))))

(defmacro insert-advance-fn (advance-fn)
  "Creates the actual closure returned by CREATE-SCANNER-AUX by
replacing '(ADVANCE-FN-DEFINITION) with a suitable definition for
ADVANCE-FN.  This is a utility macro used by CREATE-SCANNER-AUX."
  (subst
   advance-fn '(advance-fn-definition)
   '(lambda (string start end)
     (block scan
       ;; initialize a couple of special variables used by the
       ;; matchers (see file specials.lisp)
       (let* ((*string* string)
              (*start-pos* start)
              (*end-pos* end)
              ;; we will search forward for END-STRING if this value
              ;; isn't at least as big as POS (see ADVANCE-FN), so it
              ;; is safe to start to the left of *START-POS*; note
              ;; that this value will _never_ be decremented - this
              ;; is crucial to the scanning process
              (*end-string-pos* (1- *start-pos*))
              ;; the next five will shadow the variables defined by
              ;; DEFPARAMETER; at this point, we don't know if we'll
              ;; actually use them, though
              (*repeat-counters* *repeat-counters*)
              (*last-pos-stores* *last-pos-stores*)
              (*reg-starts* *reg-starts*)
              (*regs-maybe-start* *regs-maybe-start*)
              (*reg-ends* *reg-ends*)
              ;; we might be able to optimize the scanning process by
              ;; (virtually) shifting *START-POS* to the right
              (scan-start-pos *start-pos*)
              (starts-with-str (if start-string-test
                                 (str starts-with)
                                 nil))
              ;; we don't need to try further than MAX-END-POS
              (max-end-pos (- *end-pos* min-len)))
         (declare (fixnum scan-start-pos)
                  (function match-fn))
         ;; definition of ADVANCE-FN will be inserted here by macrology
         (labels ((advance-fn-definition))
           (declare (inline advance-fn))
           (when (plusp rep-num)
             ;; we have at least one REPETITION which needs to count
             ;; the number of repetitions
             (setq *repeat-counters* (make-array rep-num
                                                 :initial-element 0
                                                 :element-type 'fixnum)))
           (when (plusp zero-length-num)
             ;; we have at least one REPETITION which needs to watch
             ;; out for zero-length repetitions
             (setq *last-pos-stores* (make-array zero-length-num
                                                 :initial-element nil)))
           (when (plusp reg-num)
             ;; we have registers in our regular expression
             (setq *reg-starts* (make-array reg-num :initial-element nil)
                   *regs-maybe-start* (make-array reg-num :initial-element nil)
                   *reg-ends* (make-array reg-num :initial-element nil)))
           (when end-anchored-p
             ;; the regular expression has a constant end string which
             ;; is anchored at the very end of the target string
             ;; (perhaps modulo a #\Newline)
             (let ((end-test-pos (- *end-pos* (the fixnum end-string-len))))
               (declare (fixnum end-test-pos)
                        (function end-string-test))
               (unless (setq *end-string-pos* (funcall end-string-test
                                                       end-test-pos))
                 (when (and (= 1 (the fixnum end-anchored-p))
                            (> *end-pos* scan-start-pos)
                            (char= #\Newline (schar *string* (1- *end-pos*))))
                   ;; if we didn't find an end string candidate from
                   ;; END-TEST-POS and if a #\Newline at the end is
                   ;; allowed we try it again from one position to the
                   ;; left
                   (setq *end-string-pos* (funcall end-string-test
                                                   (1- end-test-pos))))))
             (unless (and *end-string-pos*
                          (<= *start-pos* *end-string-pos*))
               ;; no end string candidate found, so give up
               (return-from scan nil))
             (when end-string-offset
               ;; if the offset of the constant end string from the
               ;; left of the regular expression is known we can start
               ;; scanning further to the right; this is similar to
               ;; what we might do in ADVANCE-FN
               (setq scan-start-pos (max scan-start-pos
                                         (- (the fixnum *end-string-pos*)
                                            (the fixnum end-string-offset))))))
             (cond
               (start-anchored-p
                 ;; we're anchored at the start of the target string,
                 ;; so no need to try again after first failure
                 (when (or (/= *start-pos* scan-start-pos)
                           (< max-end-pos *start-pos*))
                   ;; if END-STRING-OFFSET has proven that we don't
                   ;; need to bother to scan from *START-POS* or if the
                   ;; minimal length of the regular expression is
                   ;; longer than the target string we give up
                   (return-from scan nil))
                 (when starts-with-str
                   (locally
                     (declare (fixnum starts-with-len))
                     (cond ((and (case-insensitive-p starts-with)
                                 (not (*string*-equal starts-with-str
                                                      *start-pos*
                                                      (+ *start-pos*
                                                         starts-with-len)
                                                      0 starts-with-len)))
                             ;; the regular expression has a
                             ;; case-insensitive constant start string
                             ;; and we didn't find it
                             (return-from scan nil))
                           ((and (not (case-insensitive-p starts-with))
                                 (not (*string*= starts-with-str
                                            *start-pos*
                                            (+ *start-pos* starts-with-len)
                                            0 starts-with-len)))
                             ;; the regular expression has a
                             ;; case-sensitive constant start string
                             ;; and we didn't find it
                             (return-from scan nil))
                           (t nil))))
                 (when (and end-string-test
                            (not end-anchored-p))
                   ;; the regular expression has a constant end string
                   ;; which isn't anchored so we didn't check for it
                   ;; already
                   (block end-string-loop
                     ;; we temporarily use *END-STRING-POS* as our
                     ;; starting position to look for end string
                     ;; candidates
                     (setq *end-string-pos* *start-pos*)
                     (loop
                       (unless (setq *end-string-pos*
                                       (funcall (the function end-string-test)
                                                *end-string-pos*))
                         ;; no end string candidate found, so give up
                         (return-from scan nil))
                       (unless end-string-offset
                         ;; end string doesn't have an offset so we
                         ;; can start scanning now
                         (return-from end-string-loop))
                       (let ((maybe-start-pos (- (the fixnum *end-string-pos*)
                                                 (the fixnum end-string-offset))))
                         (cond ((= maybe-start-pos *start-pos*)
                                 ;; offset of end string into regular
                                 ;; expression matches start anchor -
                                 ;; fine...
                                 (return-from end-string-loop))
                               ((and (< maybe-start-pos *start-pos*)
                                     (< (+ *end-string-pos* end-string-len) *end-pos*))
                                 ;; no match but maybe we find another
                                 ;; one to the right - try again
                                 (incf *end-string-pos*))
                               (t
                                 ;; otherwise give up
                                 (return-from scan nil)))))))
                 ;; if we got here we scan exactly once
                 (let ((next-pos (funcall match-fn *start-pos*)))
                   (when next-pos
                     (values (if next-pos *start-pos* nil)
                             next-pos
                             *reg-starts*
                             *reg-ends*))))
               (t
                 (loop for pos = (if starts-with-everything
                                   ;; don't jump to the next
                                   ;; #\Newline on the first
                                   ;; iteration
                                   scan-start-pos
                                   (advance-fn scan-start-pos))
                           then (advance-fn pos)
                       ;; give up if the regular expression can't fit
                       ;; into the rest of the target string
                       while (and pos
                                  (<= (the fixnum pos) max-end-pos))
                       do (let ((next-pos (funcall match-fn pos)))
                            (when next-pos
                              (return-from scan (values pos
                                                        next-pos
                                                        *reg-starts*
                                                        *reg-ends*)))
                            ;; not yet found, increment POS
                            #-cormanlisp (incf (the fixnum pos))
                            #+cormanlisp (incf pos)))))))))
    :test #'equalp))

(defun create-scanner-aux (match-fn
                           min-len
                           start-anchored-p
                           starts-with
                           start-string-test
                           end-anchored-p
                           end-string-test
                           end-string-len
                           end-string-offset
                           rep-num
                           zero-length-num
                           reg-num)
  "Auxiliary function to create and return a scanner \(which is
actually a closure).  Used by CREATE-SCANNER."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum min-len zero-length-num rep-num reg-num))
  (let ((starts-with-len (if (typep starts-with 'str)
                           (len starts-with)))
        (starts-with-everything (typep starts-with 'everything)))
    (cond
      ;; this COND statement dispatches on the different versions we
      ;; have for ADVANCE-FN and creates different closures for each;
      ;; note that you see only the bodies of ADVANCE-FN below - the
      ;; actual scanner is defined in INSERT-ADVANCE-FN above; (we
      ;; could have done this with closures instead of macrology but
      ;; would have consed a lot more)
      ((and start-string-test end-string-test end-string-offset)
        ;; we know that the regular expression has constant start and
        ;; end strings and we know the end string's offset (from the
        ;; left)
        (insert-advance-fn
          (advance-fn (pos)
            (declare (fixnum end-string-offset starts-with-len)
                     (function start-string-test end-string-test))
            (loop
              (unless (setq pos (funcall start-string-test pos))
                ;; give up completely if we can't find a start string
                ;; candidate
                (return-from scan nil))
              (locally
                ;; from here we know that POS is a FIXNUM
                (declare (fixnum pos))
                (when (= pos (- (the fixnum *end-string-pos*) end-string-offset))
                  ;; if we already found an end string candidate the
                  ;; position of which matches the start string
                  ;; candidate we're done
                  (return-from advance-fn pos))
                (let ((try-pos (+ pos starts-with-len)))
                  ;; otherwise try (again) to find an end string
                  ;; candidate which starts behind the start string
                  ;; candidate
                  (loop
                    (unless (setq *end-string-pos*
                                    (funcall end-string-test try-pos))
                      ;; no end string candidate found, so give up
                      (return-from scan nil))
                    ;; NEW-POS is where we should start scanning
                    ;; according to the end string candidate
                    (let ((new-pos (- (the fixnum *end-string-pos*)
                                      end-string-offset)))
                      (declare (fixnum new-pos *end-string-pos*))
                      (cond ((= new-pos pos)
                              ;; if POS and NEW-POS are equal then the
                              ;; two candidates agree so we're fine
                              (return-from advance-fn pos))
                            ((> new-pos pos)
                              ;; if NEW-POS is further to the right we
                              ;; advance POS and try again, i.e. we go
                              ;; back to the start of the outer LOOP
                              (setq pos new-pos)
                              ;; this means "return from inner LOOP"
                              (return))
                            (t
                              ;; otherwise NEW-POS is smaller than POS,
                              ;; so we have to redo the inner LOOP to
                              ;; find another end string candidate
                              ;; further to the right
                              (setq try-pos (1+ *end-string-pos*))))))))))))
      ((and starts-with-everything end-string-test end-string-offset)
        ;; we know that the regular expression starts with ".*" (which
        ;; is not in single-line-mode, see CREATE-SCANNER-AUX) and ends
        ;; with a constant end string and we know the end string's
        ;; offset (from the left)
        (insert-advance-fn
          (advance-fn (pos)
            (declare (fixnum end-string-offset)
                     (function end-string-test))
            (loop
              (unless (setq pos (newline-skipper pos))
                ;; if we can't find a #\Newline we give up immediately
                (return-from scan nil))
              (locally
                ;; from here we know that POS is a FIXNUM
                (declare (fixnum pos))
                (when (= pos (- (the fixnum *end-string-pos*) end-string-offset))
                  ;; if we already found an end string candidate the
                  ;; position of which matches the place behind the
                  ;; #\Newline we're done
                  (return-from advance-fn pos))
                (let ((try-pos pos))
                  ;; otherwise try (again) to find an end string
                  ;; candidate which starts behind the #\Newline
                  (loop
                    (unless (setq *end-string-pos*
                                    (funcall end-string-test try-pos))
                      ;; no end string candidate found, so we give up
                      (return-from scan nil))
                    ;; NEW-POS is where we should start scanning
                    ;; according to the end string candidate
                    (let ((new-pos (- (the fixnum *end-string-pos*)
                                      end-string-offset)))
                      (declare (fixnum new-pos *end-string-pos*))
                      (cond ((= new-pos pos)
                              ;; if POS and NEW-POS are equal then the
                              ;; the end string candidate agrees with
                              ;; the #\Newline so we're fine
                              (return-from advance-fn pos))
                            ((> new-pos pos)
                              ;; if NEW-POS is further to the right we
                              ;; advance POS and try again, i.e. we go
                              ;; back to the start of the outer LOOP
                              (setq pos new-pos)
                              ;; this means "return from inner LOOP"
                              (return))
                            (t
                              ;; otherwise NEW-POS is smaller than POS,
                              ;; so we have to redo the inner LOOP to
                              ;; find another end string candidate
                              ;; further to the right
                              (setq try-pos (1+ *end-string-pos*))))))))))))
      ((and start-string-test end-string-test)
        ;; we know that the regular expression has constant start and
        ;; end strings; similar to the first case but we only need to
        ;; check for the end string, it doesn't provide enough
        ;; information to advance POS
        (insert-advance-fn
          (advance-fn (pos)
            (declare (function start-string-test end-string-test))
            (unless (setq pos (funcall start-string-test pos))
              (return-from scan nil))
            (if (<= (the fixnum pos)
                    (the fixnum *end-string-pos*))
              (return-from advance-fn pos))
            (unless (setq *end-string-pos* (funcall end-string-test pos))
              (return-from scan nil))
            pos)))
      ((and starts-with-everything end-string-test)
        ;; we know that the regular expression starts with ".*" (which
        ;; is not in single-line-mode, see CREATE-SCANNER-AUX) and ends
        ;; with a constant end string; similar to the second case but we
        ;; only need to check for the end string, it doesn't provide
        ;; enough information to advance POS
        (insert-advance-fn
          (advance-fn (pos)
            (declare (function end-string-test))
            (unless (setq pos (newline-skipper pos))
              (return-from scan nil))
            (if (<= (the fixnum pos)
                    (the fixnum *end-string-pos*))
              (return-from advance-fn pos))
            (unless (setq *end-string-pos* (funcall end-string-test pos))
              (return-from scan nil))
            pos)))
      (start-string-test
        ;; just check for constant start string candidate
        (insert-advance-fn
          (advance-fn (pos)
            (declare (function start-string-test))
            (unless (setq pos (funcall start-string-test pos))
              (return-from scan nil))
            pos)))
      (starts-with-everything
        ;; just advance POS with NEWLINE-SKIPPER
        (insert-advance-fn
          (advance-fn (pos)
            (unless (setq pos (newline-skipper pos))
              (return-from scan nil))
            pos)))
      (end-string-test
        ;; just check for the next end string candidate if POS has
        ;; advanced beyond the last one
        (insert-advance-fn
          (advance-fn (pos)
            (declare (function end-string-test))
            (if (<= (the fixnum pos)
                    (the fixnum *end-string-pos*))
              (return-from advance-fn pos))
            (unless (setq *end-string-pos* (funcall end-string-test pos))
              (return-from scan nil))
            pos)))
      (t
        ;; not enough optimization information about the regular
        ;; expression to optimize so we just return POS
        (insert-advance-fn
          (advance-fn (pos)
            pos))))))
