;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-ppcre/repetition-closures.lisp,v 1.34 2009/09/17 19:17:31 edi Exp $

;;; This is actually a part of closures.lisp which we put into a
;;; separate file because it is rather complex. We only deal with
;;; REPETITIONs here. Note that this part of the code contains some
;;; rather crazy micro-optimizations which were introduced to be as
;;; competitive with Perl as possible in tight loops.

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

(defmacro incf-after (place &optional (delta 1) &environment env)
  "Utility macro inspired by C's \"place++\", i.e. first return the
value of PLACE and afterwards increment it by DELTA."
  (with-unique-names (%temp)
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
        (get-setf-expansion place env)
      `(let* (,@(mapcar #'list vars vals)
              (,%temp ,reader-form)
              (,(car store-vars) (+ ,%temp ,delta)))
        ,writer-form
        ,%temp))))

;; code for greedy repetitions with minimum zero

(defmacro greedy-constant-length-closure (check-curr-pos)
  "This is the template for simple greedy repetitions (where simple
means that the minimum number of repetitions is zero, that the inner
regex to be checked is of fixed length LEN, and that it doesn't
contain registers, i.e. there's no need for backtracking).
CHECK-CURR-POS is a form which checks whether the inner regex of the
repetition matches at CURR-POS."
  `(if maximum
    (lambda (start-pos)
      (declare (fixnum start-pos maximum))
      ;; because we know LEN we know in advance where to stop at the
      ;; latest; we also take into consideration MIN-REST, i.e. the
      ;; minimal length of the part behind the repetition
      (let ((target-end-pos (min (1+ (- *end-pos* len min-rest))
                                 ;; don't go further than MAXIMUM
                                 ;; repetitions, of course
                                 (+ start-pos
                                    (the fixnum (* len maximum)))))
            (curr-pos start-pos))
        (declare (fixnum target-end-pos curr-pos))
        (block greedy-constant-length-matcher
          ;; we use an ugly TAGBODY construct because this might be a
          ;; tight loop and this version is a bit faster than our LOOP
          ;; version (at least in CMUCL)
          (tagbody
            forward-loop
            ;; first go forward as far as possible, i.e. while
            ;; the inner regex matches
            (when (>= curr-pos target-end-pos)
              (go backward-loop))
            (when ,check-curr-pos
              (incf curr-pos len)
              (go forward-loop))
            backward-loop
            ;; now go back LEN steps each until we're able to match
            ;; the rest of the regex
            (when (< curr-pos start-pos)
              (return-from greedy-constant-length-matcher nil))
            (let ((result (funcall next-fn curr-pos)))
              (when result
                (return-from greedy-constant-length-matcher result)))
            (decf curr-pos len)
            (go backward-loop)))))
    ;; basically the same code; it's just a bit easier because we're
    ;; not bounded by MAXIMUM
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (let ((target-end-pos (1+ (- *end-pos* len min-rest)))
            (curr-pos start-pos))
        (declare (fixnum target-end-pos curr-pos))
        (block greedy-constant-length-matcher
          (tagbody
            forward-loop
            (when (>= curr-pos target-end-pos)
              (go backward-loop))
            (when ,check-curr-pos
              (incf curr-pos len)
              (go forward-loop))
            backward-loop
            (when (< curr-pos start-pos)
              (return-from greedy-constant-length-matcher nil))
            (let ((result (funcall next-fn curr-pos)))
              (when result
                (return-from greedy-constant-length-matcher result)))
            (decf curr-pos len)
            (go backward-loop)))))))

(defun create-greedy-everything-matcher (maximum min-rest next-fn)
  "Creates a closure which just matches as far ahead as possible,
i.e. a closure for a dot in single-line mode."
  (declare #.*standard-optimize-settings*)
  (declare (fixnum min-rest) (function next-fn))
  (if maximum
    (lambda (start-pos)
      (declare (fixnum start-pos maximum))
      ;; because we know LEN we know in advance where to stop at the
      ;; latest; we also take into consideration MIN-REST, i.e. the
      ;; minimal length of the part behind the repetition
      (let ((target-end-pos (min (+ start-pos maximum)
                                 (- *end-pos* min-rest))))
        (declare (fixnum target-end-pos))
        ;; start from the highest possible position and go backward
        ;; until we're able to match the rest of the regex
        (loop for curr-pos of-type fixnum from target-end-pos downto start-pos
              thereis (funcall next-fn curr-pos))))
    ;; basically the same code; it's just a bit easier because we're
    ;; not bounded by MAXIMUM
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (let ((target-end-pos (- *end-pos* min-rest)))
        (declare (fixnum target-end-pos))
        (loop for curr-pos of-type fixnum from target-end-pos downto start-pos
              thereis (funcall next-fn curr-pos))))))

(defgeneric create-greedy-constant-length-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION is of fixed length and doesn't contain registers."))

(defmethod create-greedy-constant-length-matcher ((repetition repetition)
                                                  next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((len (len repetition))
        (maximum (maximum repetition))
        (regex (regex repetition))
        (min-rest (min-rest repetition)))
    (declare (fixnum len min-rest)
             (function next-fn))
    (cond ((zerop len)
            ;; inner regex has zero-length, so we can discard it
            ;; completely
            next-fn)
          (t
            ;; now first try to optimize for a couple of common cases
            (typecase regex
              (str
                (let ((str (str regex)))
                  (if (= 1 len)
                    ;; a single character
                    (let ((chr (schar str 0)))
                      (if (case-insensitive-p regex)
                        (greedy-constant-length-closure
                         (char-equal chr (schar *string* curr-pos)))
                        (greedy-constant-length-closure
                         (char= chr (schar *string* curr-pos)))))
                    ;; a string
                    (if (case-insensitive-p regex)
                      (greedy-constant-length-closure
                       (*string*-equal str curr-pos (+ curr-pos len) 0 len))
                      (greedy-constant-length-closure
                       (*string*= str curr-pos (+ curr-pos len) 0 len))))))
              (char-class
                ;; a character class
                (insert-char-class-tester (regex (schar *string* curr-pos))
                  (greedy-constant-length-closure
                   (char-class-test))))
              (everything
                ;; an EVERYTHING object, i.e. a dot
                (if (single-line-p regex)
                  (create-greedy-everything-matcher maximum min-rest next-fn)
                  (greedy-constant-length-closure
                   (char/= #\Newline (schar *string* curr-pos)))))
              (t
                ;; the general case - we build an inner matcher which
                ;; just checks for immediate success, i.e. NEXT-FN is
                ;; #'IDENTITY
                (let ((inner-matcher (create-matcher-aux regex #'identity)))
                  (declare (function inner-matcher))
                  (greedy-constant-length-closure
                   (funcall inner-matcher curr-pos)))))))))

(defgeneric create-greedy-no-zero-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION can never match a zero-length string \(or instead the
maximal number of repetitions is 1)."))

(defmethod create-greedy-no-zero-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((maximum (maximum repetition))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after GREEDY-AUX is defined
        repeat-matcher)
    (declare (function next-fn))
    (cond
      ((eql maximum 1)
        ;; this is essentially like the next case but with a known
        ;; MAXIMUM of 1 we can get away without a counter; note that
        ;; we always arrive here if CONVERT optimizes <regex>* to
        ;; (?:<regex'>*<regex>)?
        (setq repeat-matcher
                (create-matcher-aux (regex repetition) next-fn))
        (lambda (start-pos)
          (declare (function repeat-matcher))
          (or (funcall repeat-matcher start-pos)
              (funcall next-fn start-pos))))
      (maximum
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track whether we've reached MAXIMUM
        ;; repetitions
        (let ((rep-num (incf-after *rep-num*)))
          (flet ((greedy-aux (start-pos)
                   (declare (fixnum start-pos maximum rep-num)
                            (function repeat-matcher))
                   ;; the actual matcher which first tries to match the
                   ;; inner regex of REPETITION (if we haven't done so
                   ;; too often) and on failure calls NEXT-FN
                   (or (and (< (aref *repeat-counters* rep-num) maximum)
                            (incf (aref *repeat-counters* rep-num))
                            ;; note that REPEAT-MATCHER will call
                            ;; GREEDY-AUX again recursively
                            (prog1
                              (funcall repeat-matcher start-pos)
                              (decf (aref *repeat-counters* rep-num))))
                       (funcall next-fn start-pos))))
            ;; create a closure to match the inner regex and to
            ;; implement backtracking via GREEDY-AUX
            (setq repeat-matcher
                    (create-matcher-aux (regex repetition) #'greedy-aux))
            ;; the closure we return is just a thin wrapper around
            ;; GREEDY-AUX to initialize the repetition counter
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (setf (aref *repeat-counters* rep-num) 0)
              (greedy-aux start-pos)))))
      (t
        ;; easier code because we're not bounded by MAXIMUM, but
        ;; basically the same
        (flet ((greedy-aux (start-pos)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (or (funcall repeat-matcher start-pos)
                     (funcall next-fn start-pos))))
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'greedy-aux))
          #'greedy-aux)))))

(defgeneric create-greedy-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is greedy and the minimal number of
repetitions is zero."))

(defmethod create-greedy-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((maximum (maximum repetition))
        ;; we make a reservation for our slot in *LAST-POS-STORES* because
        ;; we have to watch out for endless loops as the inner regex might
        ;; match zero-length strings
        (zero-length-num (incf-after *zero-length-num*))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after GREEDY-AUX is defined
        repeat-matcher)
    (declare (fixnum zero-length-num)
             (function next-fn))
    (cond
      (maximum
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track whether we've reached MAXIMUM
        ;; repetitions
        (let ((rep-num (incf-after *rep-num*)))
          (flet ((greedy-aux (start-pos)
                   ;; the actual matcher which first tries to match the
                   ;; inner regex of REPETITION (if we haven't done so
                   ;; too often) and on failure calls NEXT-FN
                   (declare (fixnum start-pos maximum rep-num)
                            (function repeat-matcher))
                   (let ((old-last-pos
                           (svref *last-pos-stores* zero-length-num)))
                     (when (and old-last-pos
                                (= (the fixnum old-last-pos) start-pos))
                       ;; stop immediately if we've been here before,
                       ;; i.e. if the last attempt matched a zero-length
                       ;; string
                       (return-from greedy-aux (funcall next-fn start-pos)))
                     ;; otherwise remember this position for the next
                     ;; repetition
                     (setf (svref *last-pos-stores* zero-length-num) start-pos)
                     (or (and (< (aref *repeat-counters* rep-num) maximum)
                              (incf (aref *repeat-counters* rep-num))
                              ;; note that REPEAT-MATCHER will call
                              ;; GREEDY-AUX again recursively
                              (prog1
                                (funcall repeat-matcher start-pos)
                                (decf (aref *repeat-counters* rep-num))
                                (setf (svref *last-pos-stores* zero-length-num)
                                        old-last-pos)))
                         (funcall next-fn start-pos)))))
            ;; create a closure to match the inner regex and to
            ;; implement backtracking via GREEDY-AUX
            (setq repeat-matcher
                    (create-matcher-aux (regex repetition) #'greedy-aux))
            ;; the closure we return is just a thin wrapper around
            ;; GREEDY-AUX to initialize the repetition counter and our
            ;; slot in *LAST-POS-STORES*
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (setf (aref *repeat-counters* rep-num) 0
                    (svref *last-pos-stores* zero-length-num) nil)
              (greedy-aux start-pos)))))
      (t
        ;; easier code because we're not bounded by MAXIMUM, but
        ;; basically the same
        (flet ((greedy-aux (start-pos)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (let ((old-last-pos
                         (svref *last-pos-stores* zero-length-num)))
                   (when (and old-last-pos
                              (= (the fixnum old-last-pos) start-pos))
                     (return-from greedy-aux (funcall next-fn start-pos)))
                   (setf (svref *last-pos-stores* zero-length-num) start-pos)
                   (or (prog1
                         (funcall repeat-matcher start-pos)
                         (setf (svref *last-pos-stores* zero-length-num) old-last-pos))
                       (funcall next-fn start-pos)))))
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'greedy-aux))
          (lambda (start-pos)
            (declare (fixnum start-pos))
            (setf (svref *last-pos-stores* zero-length-num) nil)
            (greedy-aux start-pos)))))))
  
;; code for non-greedy repetitions with minimum zero

(defmacro non-greedy-constant-length-closure (check-curr-pos)
  "This is the template for simple non-greedy repetitions \(where
simple means that the minimum number of repetitions is zero, that the
inner regex to be checked is of fixed length LEN, and that it doesn't
contain registers, i.e. there's no need for backtracking).
CHECK-CURR-POS is a form which checks whether the inner regex of the
repetition matches at CURR-POS."
  `(if maximum
    (lambda (start-pos)
      (declare (fixnum start-pos maximum))
      ;; because we know LEN we know in advance where to stop at the
      ;; latest; we also take into consideration MIN-REST, i.e. the
      ;; minimal length of the part behind the repetition
      (let ((target-end-pos (min (1+ (- *end-pos* len min-rest))
                                 (+ start-pos
                                    (the fixnum (* len maximum))))))
        ;; move forward by LEN and always try NEXT-FN first, then
        ;; CHECK-CUR-POS
        (loop for curr-pos of-type fixnum from start-pos
                                          below target-end-pos
                                          by len
              thereis (funcall next-fn curr-pos)
              while ,check-curr-pos
              finally (return (funcall next-fn curr-pos)))))
  ;; basically the same code; it's just a bit easier because we're
  ;; not bounded by MAXIMUM
  (lambda (start-pos)
    (declare (fixnum start-pos))
    (let ((target-end-pos (1+ (- *end-pos* len min-rest))))
      (loop for curr-pos of-type fixnum from start-pos
                                        below target-end-pos
                                        by len
            thereis (funcall next-fn curr-pos)
            while ,check-curr-pos
            finally (return (funcall next-fn curr-pos)))))))

(defgeneric create-non-greedy-constant-length-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is non-greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION is of fixed length and doesn't contain registers."))

(defmethod create-non-greedy-constant-length-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((len (len repetition))
        (maximum (maximum repetition))
        (regex (regex repetition))
        (min-rest (min-rest repetition)))
    (declare (fixnum len min-rest)
             (function next-fn))
    (cond ((zerop len)
            ;; inner regex has zero-length, so we can discard it
            ;; completely
            next-fn)
          (t
            ;; now first try to optimize for a couple of common cases
            (typecase regex
              (str
                (let ((str (str regex)))
                  (if (= 1 len)
                    ;; a single character
                    (let ((chr (schar str 0)))
                      (if (case-insensitive-p regex)
                        (non-greedy-constant-length-closure
                         (char-equal chr (schar *string* curr-pos)))
                        (non-greedy-constant-length-closure
                         (char= chr (schar *string* curr-pos)))))
                    ;; a string
                    (if (case-insensitive-p regex)
                      (non-greedy-constant-length-closure
                       (*string*-equal str curr-pos (+ curr-pos len) 0 len))
                      (non-greedy-constant-length-closure
                       (*string*= str curr-pos (+ curr-pos len) 0 len))))))
              (char-class
                ;; a character class
                (insert-char-class-tester (regex (schar *string* curr-pos))
                  (non-greedy-constant-length-closure
                   (char-class-test))))
              (everything
                (if (single-line-p regex)
                  ;; a dot which really can match everything; we rely
                  ;; on the compiler to optimize this away
                  (non-greedy-constant-length-closure
                   t)
                  ;; a dot which has to watch out for #\Newline
                  (non-greedy-constant-length-closure
                   (char/= #\Newline (schar *string* curr-pos)))))
              (t
                ;; the general case - we build an inner matcher which
                ;; just checks for immediate success, i.e. NEXT-FN is
                ;; #'IDENTITY
                (let ((inner-matcher (create-matcher-aux regex #'identity)))
                  (declare (function inner-matcher))
                  (non-greedy-constant-length-closure
                   (funcall inner-matcher curr-pos)))))))))

(defgeneric create-non-greedy-no-zero-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is non-greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION can never match a zero-length string \(or instead the
maximal number of repetitions is 1)."))

(defmethod create-non-greedy-no-zero-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((maximum (maximum repetition))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after NON-GREEDY-AUX is defined
        repeat-matcher)
    (declare (function next-fn))
    (cond
      ((eql maximum 1)
        ;; this is essentially like the next case but with a known
        ;; MAXIMUM of 1 we can get away without a counter
        (setq repeat-matcher
                (create-matcher-aux (regex repetition) next-fn))
        (lambda (start-pos)
          (declare (function repeat-matcher))
          (or (funcall next-fn start-pos)
              (funcall repeat-matcher start-pos))))
      (maximum
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track whether we've reached MAXIMUM
        ;; repetitions
        (let ((rep-num (incf-after *rep-num*)))
          (flet ((non-greedy-aux (start-pos)
                   ;; the actual matcher which first calls NEXT-FN and
                   ;; on failure tries to match the inner regex of
                   ;; REPETITION (if we haven't done so too often)
                   (declare (fixnum start-pos maximum rep-num)
                            (function repeat-matcher))
                   (or (funcall next-fn start-pos)
                       (and (< (aref *repeat-counters* rep-num) maximum)
                            (incf (aref *repeat-counters* rep-num))
                            ;; note that REPEAT-MATCHER will call
                            ;; NON-GREEDY-AUX again recursively
                            (prog1
                              (funcall repeat-matcher start-pos)
                              (decf (aref *repeat-counters* rep-num)))))))
            ;; create a closure to match the inner regex and to
            ;; implement backtracking via NON-GREEDY-AUX
            (setq repeat-matcher
                    (create-matcher-aux (regex repetition) #'non-greedy-aux))
            ;; the closure we return is just a thin wrapper around
            ;; NON-GREEDY-AUX to initialize the repetition counter
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (setf (aref *repeat-counters* rep-num) 0)
              (non-greedy-aux start-pos)))))
      (t
        ;; easier code because we're not bounded by MAXIMUM, but
        ;; basically the same
        (flet ((non-greedy-aux (start-pos)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (or (funcall next-fn start-pos)
                     (funcall repeat-matcher start-pos))))
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'non-greedy-aux))
          #'non-greedy-aux)))))
  
(defgeneric create-non-greedy-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is non-greedy and the minimal number of
repetitions is zero."))

(defmethod create-non-greedy-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  ;; we make a reservation for our slot in *LAST-POS-STORES* because
  ;; we have to watch out for endless loops as the inner regex might
  ;; match zero-length strings
  (let ((zero-length-num (incf-after *zero-length-num*))
        (maximum (maximum repetition))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after NON-GREEDY-AUX is defined
        repeat-matcher)
    (declare (fixnum zero-length-num)
             (function next-fn))
    (cond
      (maximum
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track whether we've reached MAXIMUM
        ;; repetitions
        (let ((rep-num (incf-after *rep-num*)))
          (flet ((non-greedy-aux (start-pos)
                   ;; the actual matcher which first calls NEXT-FN and
                   ;; on failure tries to match the inner regex of
                   ;; REPETITION (if we haven't done so too often)
                   (declare (fixnum start-pos maximum rep-num)
                            (function repeat-matcher))
                   (let ((old-last-pos
                           (svref *last-pos-stores* zero-length-num)))
                     (when (and old-last-pos
                                (= (the fixnum old-last-pos) start-pos))
                       ;; stop immediately if we've been here before,
                       ;; i.e. if the last attempt matched a zero-length
                       ;; string
                       (return-from non-greedy-aux (funcall next-fn start-pos)))
                     ;; otherwise remember this position for the next
                     ;; repetition
                     (setf (svref *last-pos-stores* zero-length-num) start-pos)
                     (or (funcall next-fn start-pos)
                         (and (< (aref *repeat-counters* rep-num) maximum)
                              (incf (aref *repeat-counters* rep-num))
                              ;; note that REPEAT-MATCHER will call
                              ;; NON-GREEDY-AUX again recursively
                              (prog1
                                (funcall repeat-matcher start-pos)
                                (decf (aref *repeat-counters* rep-num))
                                (setf (svref *last-pos-stores* zero-length-num)
                                        old-last-pos)))))))
            ;; create a closure to match the inner regex and to
            ;; implement backtracking via NON-GREEDY-AUX
            (setq repeat-matcher
                    (create-matcher-aux (regex repetition) #'non-greedy-aux))
            ;; the closure we return is just a thin wrapper around
            ;; NON-GREEDY-AUX to initialize the repetition counter and our
            ;; slot in *LAST-POS-STORES*
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (setf (aref *repeat-counters* rep-num) 0
                    (svref *last-pos-stores* zero-length-num) nil)
              (non-greedy-aux start-pos)))))
      (t
        ;; easier code because we're not bounded by MAXIMUM, but
        ;; basically the same
        (flet ((non-greedy-aux (start-pos)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (let ((old-last-pos
                         (svref *last-pos-stores* zero-length-num)))
                   (when (and old-last-pos
                              (= (the fixnum old-last-pos) start-pos))
                     (return-from non-greedy-aux (funcall next-fn start-pos)))
                   (setf (svref *last-pos-stores* zero-length-num) start-pos)
                   (or (funcall next-fn start-pos)
                       (prog1
                         (funcall repeat-matcher start-pos)
                         (setf (svref *last-pos-stores* zero-length-num)
                                 old-last-pos))))))
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'non-greedy-aux))
          (lambda (start-pos)
            (declare (fixnum start-pos))
            (setf (svref *last-pos-stores* zero-length-num) nil)
            (non-greedy-aux start-pos)))))))
  
;; code for constant repetitions, i.e. those with a fixed number of repetitions
                      
(defmacro constant-repetition-constant-length-closure (check-curr-pos)
  "This is the template for simple constant repetitions (where simple
means that the inner regex to be checked is of fixed length LEN, and
that it doesn't contain registers, i.e. there's no need for
backtracking) and where constant means that MINIMUM is equal to
MAXIMUM.  CHECK-CURR-POS is a form which checks whether the inner
regex of the repetition matches at CURR-POS."
  `(lambda (start-pos)
    (declare (fixnum start-pos))
      (let ((target-end-pos (+ start-pos
                               (the fixnum (* len repetitions)))))
        (declare (fixnum target-end-pos))
        ;; first check if we won't go beyond the end of the string
        (and (>= *end-pos* target-end-pos)
             ;; then loop through all repetitions step by step
             (loop for curr-pos of-type fixnum from start-pos
                                               below target-end-pos
                                               by len
                   always ,check-curr-pos)
             ;; finally call NEXT-FN if we made it that far
             (funcall next-fn target-end-pos)))))

(defgeneric create-constant-repetition-constant-length-matcher
    (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION has a constant number of repetitions.
It is furthermore assumed that the inner regex of REPETITION is of
fixed length and doesn't contain registers."))

(defmethod create-constant-repetition-constant-length-matcher
       ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((len (len repetition))
        (repetitions (minimum repetition))
        (regex (regex repetition)))
    (declare (fixnum len repetitions)
             (function next-fn))
    (if (zerop len)
      ;; if the length is zero it suffices to try once
      (create-matcher-aux regex next-fn)
      ;; otherwise try to optimize for a couple of common cases
      (typecase regex
        (str
          (let ((str (str regex)))
            (if (= 1 len)
              ;; a single character
              (let ((chr (schar str 0)))
                (if (case-insensitive-p regex)
                  (constant-repetition-constant-length-closure
                   (and (char-equal chr (schar *string* curr-pos))
                        (1+ curr-pos)))
                  (constant-repetition-constant-length-closure
                   (and (char= chr (schar *string* curr-pos))
                        (1+ curr-pos)))))
              ;; a string
              (if (case-insensitive-p regex)
                (constant-repetition-constant-length-closure
                 (let ((next-pos (+ curr-pos len)))
                   (declare (fixnum next-pos))
                   (and (*string*-equal str curr-pos next-pos 0 len)
                        next-pos)))
                (constant-repetition-constant-length-closure
                 (let ((next-pos (+ curr-pos len)))
                   (declare (fixnum next-pos))
                   (and (*string*= str curr-pos next-pos 0 len)
                        next-pos)))))))
        (char-class
          ;; a character class
          (insert-char-class-tester (regex (schar *string* curr-pos))
            (constant-repetition-constant-length-closure
             (and (char-class-test)
                  (1+ curr-pos)))))
        (everything
          (if (single-line-p regex)
            ;; a dot which really matches everything - we just have to
            ;; advance the index into *STRING* accordingly and check
            ;; if we didn't go past the end
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (let ((next-pos (+ start-pos repetitions)))
                (declare (fixnum next-pos))
                (and (<= next-pos *end-pos*)
                     (funcall next-fn next-pos))))
            ;; a dot which is not in single-line-mode - make sure we
            ;; don't match #\Newline
            (constant-repetition-constant-length-closure
             (and (char/= #\Newline (schar *string* curr-pos))
                  (1+ curr-pos)))))
        (t
          ;; the general case - we build an inner matcher which just
          ;; checks for immediate success, i.e. NEXT-FN is #'IDENTITY
          (let ((inner-matcher (create-matcher-aux regex #'identity)))
            (declare (function inner-matcher))
            (constant-repetition-constant-length-closure
             (funcall inner-matcher curr-pos))))))))
  
(defgeneric create-constant-repetition-matcher (repetition next-fn)
  (declare #.*standard-optimize-settings*)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION has a constant number of repetitions."))

(defmethod create-constant-repetition-matcher ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (let ((repetitions (minimum repetition))
        ;; we make a reservation for our slot in *REPEAT-COUNTERS*
        ;; because we need to keep track of the number of repetitions
        (rep-num (incf-after *rep-num*))
        ;; REPEAT-MATCHER is part of the closure's environment but it
        ;; can only be defined after NON-GREEDY-AUX is defined
        repeat-matcher)
    (declare (fixnum repetitions rep-num)
             (function next-fn))
    (if (zerop (min-len repetition))
      ;; we make a reservation for our slot in *LAST-POS-STORES*
      ;; because we have to watch out for needless loops as the inner
      ;; regex might match zero-length strings
      (let ((zero-length-num (incf-after *zero-length-num*)))
        (declare (fixnum zero-length-num))
        (flet ((constant-aux (start-pos)
                 ;; the actual matcher which first calls NEXT-FN and
                 ;; on failure tries to match the inner regex of
                 ;; REPETITION (if we haven't done so too often)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (let ((old-last-pos
                         (svref *last-pos-stores* zero-length-num)))
                   (when (and old-last-pos
                              (= (the fixnum old-last-pos) start-pos))
                     ;; if we've been here before we matched a
                     ;; zero-length string the last time, so we can
                     ;; just carry on because we will definitely be
                     ;; able to do this again often enough
                     (return-from constant-aux (funcall next-fn start-pos)))
                   ;; otherwise remember this position for the next
                   ;; repetition
                   (setf (svref *last-pos-stores* zero-length-num) start-pos)
                   (cond ((< (aref *repeat-counters* rep-num) repetitions)
                           ;; not enough repetitions yet, try it again
                           (incf (aref *repeat-counters* rep-num))
                           ;; note that REPEAT-MATCHER will call
                           ;; CONSTANT-AUX again recursively
                           (prog1
                             (funcall repeat-matcher start-pos)
                             (decf (aref *repeat-counters* rep-num))
                             (setf (svref *last-pos-stores* zero-length-num)
                                     old-last-pos)))
                         (t
                           ;; we're done - call NEXT-FN
                           (funcall next-fn start-pos))))))
          ;; create a closure to match the inner regex and to
          ;; implement backtracking via CONSTANT-AUX
          (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'constant-aux))
          ;; the closure we return is just a thin wrapper around
          ;; CONSTANT-AUX to initialize the repetition counter
          (lambda (start-pos)
            (declare (fixnum start-pos))
            (setf (aref *repeat-counters* rep-num) 0
                  (aref *last-pos-stores* zero-length-num) nil)
            (constant-aux start-pos))))
      ;; easier code because we don't have to care about zero-length
      ;; matches but basically the same
      (flet ((constant-aux (start-pos)
               (declare (fixnum start-pos)
                        (function repeat-matcher))
               (cond ((< (aref *repeat-counters* rep-num) repetitions)
                       (incf (aref *repeat-counters* rep-num))
                       (prog1
                         (funcall repeat-matcher start-pos)
                         (decf (aref *repeat-counters* rep-num))))
                     (t (funcall next-fn start-pos)))))
        (setq repeat-matcher
                (create-matcher-aux (regex repetition) #'constant-aux))
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (setf (aref *repeat-counters* rep-num) 0)
          (constant-aux start-pos))))))
  
;; the actual CREATE-MATCHER-AUX method for REPETITION objects which
;; utilizes all the functions and macros defined above

(defmethod create-matcher-aux ((repetition repetition) next-fn)
  (declare #.*standard-optimize-settings*)
  (with-slots (minimum maximum len min-len greedyp contains-register-p)
      repetition
    (cond ((and maximum
                (zerop maximum))
           ;; this should have been optimized away by CONVERT but just
           ;; in case...
           (error "Got REPETITION with MAXIMUM 0 \(should not happen)"))
          ((and maximum
                (= minimum maximum 1))
           ;; this should have been optimized away by CONVERT but just
           ;; in case...
           (error "Got REPETITION with MAXIMUM 1 and MINIMUM 1 \(should not happen)"))
          ((and (eql minimum maximum)
                len
                (not contains-register-p))
           (create-constant-repetition-constant-length-matcher repetition next-fn))
          ((eql minimum maximum)
           (create-constant-repetition-matcher repetition next-fn))
          ((and greedyp
                len
                (not contains-register-p))
           (create-greedy-constant-length-matcher repetition next-fn))
          ((and greedyp
                (or (plusp min-len)
                    (eql maximum 1)))
           (create-greedy-no-zero-matcher repetition next-fn))
          (greedyp
           (create-greedy-matcher repetition next-fn))
          ((and len
                (plusp len)
                (not contains-register-p))
           (create-non-greedy-constant-length-matcher repetition next-fn))
          ((or (plusp min-len)
               (eql maximum 1))
           (create-non-greedy-no-zero-matcher repetition next-fn))
          (t
           (create-non-greedy-matcher repetition next-fn)))))
