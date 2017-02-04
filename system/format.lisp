;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(cl:defpackage :sys.format
  (:use :cl #:sys.int))

(in-package :sys.format)

(defstruct directive
  character
  at-sign
  colon
  parameters)

;; Only tracks parameters for the start directive.
(defstruct block-directive
  character
  start-at-sign
  start-colon
  end-at-sign
  end-colon
  parameters
  inner)

(defparameter *block-directives*
  '((#\( #\))
    (#\[ #\])
    (#\< #\>)
    (#\{ #\})))

(defun whitespace[1]p (c)
  (or (eql c #\Newline)
      (eql c #\Space)
      (eql c #\Rubout)
      (eql c #\Page)
      (eql c #\Tab)
      (eql c #\Backspace)))

(defun parse-format-directive (control-string offset)
  (let ((prefix-parameters nil)
        (at-sign-modifier nil)
        (colon-modifier nil)
        (current-prefix nil))
    ;; Read prefix parameters
    (do () (nil)
      (case (char-upcase (char control-string offset))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\-)
         (when current-prefix (error "Invalid format control string ~S." control-string))
         ;; Eat digits until non-digit
         (let ((negative nil))
           (setf current-prefix 0)
           (case (char control-string offset)
             (#\+ (incf offset))
             (#\- (incf offset)
                  (setf negative t)))
           (when (not (digit-char-p (char control-string offset)))
             ;; The sign is optional, the digits are not.
             (error "Invalid format control string ~S." control-string))
           (do () ((not (digit-char-p (char control-string offset))))
             (setf current-prefix (+ (* current-prefix 10)
                                     (digit-char-p (char control-string offset))))
             (incf offset))
           (when negative
             (setf current-prefix (- current-prefix)))))
        (#\#
         (when current-prefix (error "Invalid format control string ~S." control-string))
         (incf offset)
         (setf current-prefix :sharp-sign))
        (#\V
         (when current-prefix (error "Invalid format control string ~S." control-string))
         (incf offset)
         (setf current-prefix :v))
        (#\'
         (when current-prefix (error "Invalid format control string ~S." control-string))
         (incf offset)
         (setf current-prefix (char control-string offset))
         (incf offset))
        (#\,
         (incf offset)
         (push current-prefix prefix-parameters)
         (setf current-prefix nil))
        (t (return))))
    (when current-prefix
      (push current-prefix prefix-parameters))
    (setf prefix-parameters (nreverse prefix-parameters))
    ;; Munch all colons and at-signs
    (do () (nil)
      (case (char control-string offset)
        (#\@ (setf at-sign-modifier t)
             (incf offset))
        (#\: (setf colon-modifier t)
             (incf offset))
        (t (return))))
    (case (char control-string offset)
      (#\Newline
       ;; Newline must be handled specially, as it advances through the control string.
       (unless colon-modifier
         ;; Eat trailing whitespace[1].
         (do () ((not (whitespace[1]p (char control-string (1+ offset)))))
           (incf offset)))
       (values offset #\Newline at-sign-modifier colon-modifier prefix-parameters))
      (#\/
       ;; This also advances through the control string.
       (let ((package-name "COMMON-LISP-USER")
             (symbol-name (make-array 20 :element-type 'character :adjustable t :fill-pointer 0))
             (allow-internal nil))
         (do ((ch (char control-string (incf offset))
                  (char control-string (incf offset))))
             ((eql ch #\/))
           (cond ((and (eql ch #\:)
                       (zerop (length symbol-name)))
                  (setf allow-internal t))
                 ((eql ch #\:)
                  (setf package-name symbol-name
                        symbol-name (make-array 20 :element-type 'character :adjustable t :fill-pointer 0)))
                 (t (vector-push-extend (char-upcase ch) symbol-name))))
         (push (list symbol-name package-name allow-internal) prefix-parameters)
         (values offset #\/ at-sign-modifier colon-modifier prefix-parameters)))
      (t (values offset (char-upcase (char control-string offset)) at-sign-modifier colon-modifier prefix-parameters)))))

(defun parse-format-control-substring (control-string start end-char)
  (do ((offset start (1+ offset))
       (accumulated-string)
       (result '()))
      ((>= offset (length control-string))
       (when end-char
         (error "No terminating ~~~C directive." end-char))
       (when accumulated-string
         (push accumulated-string result)
         (setf accumulated-string nil))
       (values offset (reverse result)))
    (flet ((append-character (c)
             "Append C to the accumulated-string."
             (when (not accumulated-string)
               (setf accumulated-string (make-array 50 :element-type 'character :adjustable t :fill-pointer 0)))
             (vector-push-extend c accumulated-string)))
      (cond ((eql #\~ (char control-string offset))
             (multiple-value-bind (new-offset character at-sign colon parameters)
                 (parse-format-directive control-string (1+ offset))
               (setf offset new-offset)
               ;; Flush accumulated output.
               (when accumulated-string
                 (push accumulated-string result)
                 (setf accumulated-string nil))
               (cond ((find character *block-directives* :key #'first)
                      (multiple-value-bind (new-offset inner end-at-sign end-colon)
                          (parse-format-control-substring control-string (1+ offset)
                                                          (second (find character *block-directives* :key #'first)))
                        (push (make-block-directive :character character
                                                    :start-at-sign at-sign
                                                    :start-colon colon
                                                    :end-at-sign end-at-sign
                                                    :end-colon end-colon
                                                    :parameters parameters
                                                    :inner inner)
                              result)
                        (setf offset new-offset)))
                     ((eql character end-char)
                      (when parameters
                        (error "~~~C does not take parameters." character))
                      (when (eql character end-char)
                        (return (values offset (reverse result) at-sign colon))))
                     ((find character *block-directives* :key #'second)
                      (error "Unexpected directive ~S in format control-string ~S!"
                             character control-string))
                     (t (push (make-directive :character character
                                              :at-sign at-sign
                                              :colon colon
                                              :parameters parameters)
                              result)))))
               (t (append-character (char control-string offset)))))))


(defun parse-format-control (control-string)
  (nth-value 1 (parse-format-control-substring control-string 0 nil)))

(defparameter *format-interpreters* '())

(defun format-interpreter (character)
  (check-type character character)
  (getf *format-interpreters* character))

(defun (setf format-interpreter) (value character)
  (check-type character character)
  (setf (getf *format-interpreters* character) value))

(defmacro define-format-interpreter (character (at-sign colon &rest parameter-lambda-list) &body body)
  (let ((arguments (gensym "Args"))
        (at-sign-sym (or at-sign (gensym "At-Sign")))
        (colon-sym (or colon (gensym "Colon"))))
    `(setf (format-interpreter ',character)
           (lambda (,arguments ,at-sign-sym ,colon-sym ,@parameter-lambda-list)
             (declare (sys.int::lambda-name (format-interpreter ,character)))
             (block nil
               ,@(when (not at-sign)
                       (list `(when ,at-sign-sym
                                (error "~~~C does not take the at-sign modifier." ',character))))
               ,@(when (not colon)
                       (list `(when ,colon-sym
                                (error "~~~C does not take the colon modifier." ',character))))
               (flet ((consume-argument ()
                        (when (endp ,arguments)
                          (error "No more format arguments."))
                        (pop ,arguments))
                      (remaining-arguments ()
                        ,arguments))
                 ,@body
                 ,arguments))))))

(defun format-integer (stream n base params at-sign colon)
  (let ((mincol (first params))
        (padchar (or (second params) #\Space))
        (commachar (or (third params) #\,))
        (comma-interval (or (fourth params) 3)))
    (unless (integerp n)
      (return-from format-integer
        (let ((*print-base* base))
          (write n :stream stream :escape nil :readably nil))))
    (check-type padchar character)
    (check-type commachar character)
    (check-type comma-interval integer)
    (when (cddddr params)
      (error "Expected 0 to 4 parameters."))
    (if (or mincol colon)
        ;; Fancy formatting.
        (let ((buffer (make-array 8
                                  :element-type 'character
                                  :adjustable t
                                  :fill-pointer 0))
              (negative nil))
          (when (minusp n)
            (setf negative t
                  n (- n)))
          (unless mincol (setf mincol 0))
          ;; Write the number backwards into the buffer, no commas or padding yet.
          (if (= n 0)
              (vector-push-extend #\0 buffer)
              (do () ((= n 0))
                (multiple-value-bind (quot rem)
                    (truncate n base)
                  (vector-push-extend (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" rem) buffer)
                  (setf n quot))))
          ;; TODO: count commas as well
          (dotimes (i (- mincol (+ (length buffer) (if (or negative at-sign) 1 0))))
            (write-char padchar stream))
          (cond
            (negative
             (write-char #\- stream))
            (at-sign
             (write-char #\+ stream)))
          (if colon
              (dotimes (i (length buffer))
                (when (and (not (zerop i))
                           (zerop (rem (- (length buffer) i) comma-interval)))
                  (write-char commachar stream))
                (write-char (char buffer (- (length buffer) i 1)) stream))
              (dotimes (i (length buffer))
                (write-char (char buffer (- (length buffer) i 1)) stream))))
        (progn
          (when (and at-sign (not (minusp n)))
            (write-char #\+ stream))
          (write n :stream stream :escape nil :radix nil :base base :readably nil)))))

(defvar *cardinal-names-1*
  '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
    "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))
(defvar *cardinal-names-10*
  '("zero" "ten" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))
(defvar *ordinal-names-1*
  '("zeroth" "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"
    "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth" "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))
(defvar *ordinal-names-10*
  '("zeroth" "tenth" "twentieth" "thirtieth" "fortieth" "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))
(defvar *radix-powers*
  '((1000000 "million" "millionth") (1000 "thousand" "thousandth") (100 "hundred" "hundredth")))

(defun print-cardinal (integer stream)
  (when (minusp integer)
    (write-string "negative " stream)
    (setf integer (- integer)))
  (cond ((< integer 20)
         (write-string (elt *cardinal-names-1* integer) stream))
        ((< integer 100)
         (multiple-value-bind (quot rem)
             (truncate integer 10)
           (write-string (elt *cardinal-names-10* quot) stream)
           (unless (zerop rem)
             (write-char #\- stream)
             (print-cardinal rem stream))))
        (t (loop for (power cardinal ordinal) in *radix-powers*
              when (>= integer power) do
                (multiple-value-bind (quot rem)
                    (truncate integer power)
                  (print-cardinal quot stream)
                  (write-char #\Space stream)
                  (write-string cardinal stream)
                  (unless (zerop rem)
                    (write-char #\Space stream)
                    (print-cardinal rem stream)))
                (return)
              finally (error "Number ~:D too large to be printed as a cardinal number." integer)))))

(defun print-ordinal (integer stream)
  (when (minusp integer)
    (write-string "negative " stream)
    (setf integer (- integer)))
  (cond ((< integer 20)
         (write-string (elt *ordinal-names-1* integer) stream))
        ((< integer 100)
         (multiple-value-bind (quot rem)
             (truncate integer 10)
           (cond ((zerop rem)
                  (write-string (elt *ordinal-names-10* quot) stream))
                 (t (write-string (elt *cardinal-names-10* quot) stream)
                    (write-char #\- stream)
                    (print-ordinal rem stream)))))
        (t (loop for (power cardinal ordinal) in *radix-powers*
              when (>= integer power) do
                (multiple-value-bind (quot rem)
                    (truncate integer power)
                  (print-cardinal quot stream)
                  (write-char #\Space stream)
                  (cond ((zerop rem)
                         (write-string ordinal stream))
                        (t (write-string cardinal stream)
                           (write-char #\Space stream)
                           (print-ordinal rem stream))))
                (return)
              finally (error "Number ~:D too large to be printed as an ordinal number." integer)))))

;;;; 22.3.1 FORMAT Basic Output.

(defun format-character (stream c at-sign colon)
  (check-type c character)
  (cond ((and at-sign (not colon))
         (write c :stream stream :escape t))
        (colon
         (if (and (graphic-char-p c) (not (eql #\Space c)))
             (write-char c stream)
             (write-string (char-name c) stream))
         ;; TODO: colon & at-sign.
         ;; Describes how to type the character if it requires
         ;; unusual shift keys to type.
         (when at-sign))
        (t (write-char c stream))))

(define-format-interpreter #\C (at-sign colon)
  (format-character *standard-output* (consume-argument) at-sign colon))

(define-format-interpreter #\% (at-sign colon &optional n)
  (check-type n (or integer null))
  (dotimes (i (or n 1))
    (terpri)))

(define-format-interpreter #\& (at-sign colon &optional n)
  (check-type n (or integer null))
  (when (or (null n) (plusp n))
    (fresh-line)
    (dotimes (i (1- (or n 1)))
      (terpri))))

(define-format-interpreter #\| (at-sign colon &optional n)
  (check-type n (or integer null))
  (dotimes (i (or n 1))
    (write-char #\Page)))

(define-format-interpreter #\~ (at-sign colon &optional n)
  (check-type n (or integer null))
  (dotimes (i (or n 1))
    (write-char #\~)))

;;;; 22.3.2 FORMAT Radix Control.

(defun format-radix (stream arg params at-sign colon)
  (cond
    (params
     (let ((base (or (first params) 10)))
       (check-type base integer)
       (format-integer stream arg
                       base (rest params)
                       at-sign colon)))
    (at-sign
     (error "TODO: Roman numerals."))
    (colon
     (print-ordinal arg stream))
    (t
     (print-cardinal arg stream))))

(define-format-interpreter #\R (at-sign colon &rest params)
  (format-radix *standard-output*
                (consume-argument)
                params at-sign colon))

(define-format-interpreter #\D (at-sign colon &rest params)
  (format-integer *standard-output* (consume-argument)
                  10 params at-sign colon))

(define-format-interpreter #\B (at-sign colon &rest params)
  (format-integer *standard-output* (consume-argument)
                  2 params at-sign colon))

(define-format-interpreter #\O (at-sign colon &rest params)
  (format-integer *standard-output* (consume-argument)
                  8 params at-sign colon))

(define-format-interpreter #\X (at-sign colon &rest params)
  (format-integer *standard-output* (consume-argument)
                  16 params at-sign colon))

;;;; 22.3.3 FORMAT Floating-Point Printers.
;;; TODO: F, E, G, $

(define-format-interpreter #\$ (at-sign colon &optional (d 2) (n 1) (w 0) (padchar #\Space))
  (let ((arg (consume-argument)))
    (when (realp arg)
      (setf arg (float 0.0s0)))
    (format t "~D" arg)))

;;;; 22.3.4 FORMAT Printer Operations.

(define-format-interpreter #\A (nil colon &optional mincol colinc minpad padchar)
  (let ((arg (consume-argument)))
    (if (and (null arg) colon)
        (write-string "()")
        (write arg :escape nil :readably nil))))

(define-format-interpreter #\S (nil colon &optional mincol colinc minpad padchar)
  (let ((arg (consume-argument)))
    (if (and (null arg) colon)
        (write-string "()")
        (write arg :escape t))))

(define-format-interpreter #\W (at-sign colon)
  (cond
    ((and at-sign colon)
     (write (consume-argument) :pretty t :level nil :length nil))
    (at-sign
     (write (consume-argument) :level nil :length nil))
    (colon
     (write (consume-argument) :pretty t))
    (t (write (consume-argument)))))

;;;; 22.3.5 FORMAT Pretty Printer Operations.

(define-format-interpreter #\_ (at-sign colon)
  (cond
    ((and at-sign colon)
     (pprint-newline :mandatory))
    (at-sign
     (pprint-newline :miser))
    (colon
     (pprint-newline :fill))
    (t (pprint-newline :linear))))

;;; TODO
(defun format-justification (args inner at-sign colon end-at-sign params)
  (interpret-format-control inner args))

(define-format-interpreter #\I (nil colon &optional count)
  (check-type count (or integer null))
  (pprint-indent (if colon
                     :current
                     :block)
                 (or count 1)))

(define-format-interpreter #\/ (at-sign colon function &rest params)
  (destructuring-bind (symbol-name package-name allow-internal)
      function
    (apply (intern symbol-name package-name)
           *standard-output*
           (consume-argument)
           colon at-sign
           params)))

;;;; 22.3.6 FORMAT Layout Control.

(define-format-interpreter #\T (at-sign colon &optional colnum colinc)
  (setf colnum (or colnum 1)
        colinc (or colinc 1))
  (cond (colon
         (pprint-tab (if at-sign :section-relative :section)
                     colnum colinc))
        (at-sign
         (dotimes (i colnum)
           (write-char #\Space))
         (let ((current (sys.gray:stream-line-column *standard-output*)))
           (when current
             (dotimes (i (- colinc (rem current colinc)))
               (write-char #\Space)))))
        (t (let ((current (sys.gray:stream-line-column *standard-output*)))
             (cond ((not current)
                    (write-string "  "))
                   ((< current colnum)
                    (dotimes (i (- colnum current))
                      (write-char #\Space)))
                   ((not (zerop colinc))
                    (dotimes (i (- colinc (rem (- current colnum) colinc)))
                      (write-char #\Space))))))))

;;; TODO!
(defun format-logical-block (args inner at-sign colon end-at-sign params)
  (interpret-format-control inner args))

;;;; 22.3.7 FORMAT Control-Flow Operations.

;;; TODO.
(define-format-interpreter #\* (at-sign colon &rest params))

(defun format-iteration (args inner at-sign colon end-at-sign end-colon params)
  (when (rest params)
    (error "~~{ expects at most one parameter."))
  (when end-at-sign
    (error "~~> does not take the at-sign modifier."))
  (let ((n (first params))
        (list (cond (at-sign
                     args)
                    (t (when (endp args)
                         (error "No more format arguments."))
                       (pop args)))))
    (check-type n (or null integer))
    (catch 'escape-upwards
      (loop
         (when (and (not end-colon) (endp list)) (return))
         (setf end-colon nil)
         (if colon
             (interpret-format-control inner
                                       (pop list))
             (setf list (interpret-format-control inner
                                                  list)))))
    (if at-sign
        list
        args)))

(defun decode-conditional-clauses (control-list)
  "Split CONTROL-LIST into a list of clauses at ~; directives."
  (let ((result '())
        (current '())
        saw-else)
    (dolist (element control-list)
      (cond ((and (directive-p element)
                  (eql (directive-character element) #\;))
             (when saw-else
               (error "Additional clauses after else clause."))
             (when (directive-colon element)
               (setf saw-else t))
             (when (directive-at-sign element)
               (error "~; in [] does not take the at-sign modifier."))
             (when (directive-parameters element)
               (error "~; in [] expects no parameters."))
             (push (reverse current) result)
             (setf current '()))
            (t (push element current))))
    (cond (saw-else
           (values (reverse result) (reverse current) saw-else))
          (t (push (reverse current) result)
             (values (reverse result) nil saw-else)))))

(defun format-conditional (args inner at-sign colon end-at-sign end-colon params)
  (when (or end-at-sign end-colon)
    (error "~~] does not take the at-sign or colon modifiers."))
  (multiple-value-bind (clauses default defaultp)
      (decode-conditional-clauses inner)
    (cond ((and at-sign colon)
           (error "At-sign and colon modifiers are mutually exclusive in ~~["))
          (at-sign ; Test argument. If true, rewind 1 and execute the one-clause consequent.
           (when params (error "~~@[ expects no parameters."))
           (when defaultp
             (error "Default clause with ~~:[."))
           (unless (eql (length clauses) 1)
             (error "~~@[ takes exactly one claus."))
           (when (endp args)
             (error "No more format arguments."))
           (cond ((first args)
                  (interpret-format-control (first clauses)
                                            args))
                 (t (pop args) args)))
          (colon ; Select first clause if argument is false, second if true.
           (when params (error "~~:[ expects no parameters."))
           (when defaultp
             (error "Default clause with ~~:[."))
           (unless (eql (length clauses) 2)
             (error "~~:[ takes exactly two clauses."))
           (when (endp args)
             (error "No more format arguments."))
           (interpret-format-control (if (pop args)
                                         (second clauses)
                                         (first clauses))
                                     args))
           (t (when (rest params)
                (error "~~[ expects at most one parameter."))
              (when (and (null (first params)) (endp args))
                (error "No more format arguments."))
              (let ((arg (or (first params) (pop args))))
                (check-type arg integer)
                (cond ((or (< arg 0)
                           (>= arg (length clauses)))
                       (if defaultp
                           (interpret-format-control default
                                                     args)
                           args))
                      (t (interpret-format-control (nth arg clauses)
                                                   args))))))))

(define-format-interpreter #\? (at-sign nil)
  (let ((control (parse-format-control (consume-argument))))
    (cond (at-sign
           (return (interpret-format-control control (remaining-arguments))))
          (t (interpret-format-control control (consume-argument))))))

;;;; 22.3.8 FORMAT Miscellaneous Operations.

(defun format-case-correcting (args inner at-sign colon end-at-sign end-colon params)
  (when params (error "~~( Expects no parameters."))
  (when (or end-at-sign end-colon)
    (error "~~) does not take the at-sign or colon modifiers."))
  (let ((*standard-output* (sys.int::make-case-correcting-stream
                            *standard-output*
                            (cond ((and colon at-sign)
                                   :upcase)
                                  (colon
                                   :titlecase)
                                  (at-sign
                                   :sentencecase)
                                  (t :downcase)))))
    (interpret-format-control inner args)))

(define-format-interpreter #\P (at-sign colon)
  (let ((arg (if colon
                 ;; FIXME: Should back up by one.
                 (first (remaining-arguments))
                 (consume-argument))))
    (if (and (numberp arg)
             (= arg 1))
        (if at-sign
            (write-string "y"))
        (if at-sign
            (write-string "ies")
            (write-string "s")))))

;;;; 22.3.9 FORMAT Miscellaneous Pseudo-Operations.

;; TODO!
(define-format-interpreter #\^ (at-sign colon &rest params)
  (when (not (remaining-arguments))
    (throw 'escape-upwards nil)))

(define-format-interpreter #\Newline (at-sign colon)
  (when at-sign
    (write-char #\Newline)))

;; FIXME, remove this when #\< is implemented.
(define-format-interpreter #\; (at-sign colon &rest params))

(defun format-justification-or-logical-block (args inner at-sign colon end-at-sign end-colon params)
  (if end-colon
      (format-logical-block args inner at-sign colon end-at-sign params)
      (format-justification args inner at-sign colon end-at-sign params)))

(defun interpret-format-control (control args)
  (flet ((compute-parameters (params)
           (mapcar (lambda (p)
                     (cond ((eql p :v)
                            (when (endp args)
                              (error "No more arguments for V parameter."))
                            (pop args))
                           ((eql p :sharp-sign)
                            (length args))
                           (t p)))
                   params)))
    (dolist (element control)
      (etypecase element
        (string (write-string element))
        (block-directive
         (setf args (funcall (ecase (block-directive-character element)
                               (#\( #'format-case-correcting)
                               (#\[ #'format-conditional)
                               (#\< #'format-justification-or-logical-block)
                               (#\{ #'format-iteration))
                             args
                             (block-directive-inner element)
                             (block-directive-start-at-sign element)
                             (block-directive-start-colon element)
                             (block-directive-end-at-sign element)
                             (block-directive-end-colon element)
                             (compute-parameters (block-directive-parameters element)))))
        (directive
         (let ((fn (format-interpreter (directive-character element))))
           (when (not fn)
             (error "Unknown format directive ~S!" (directive-character element)))
           (setf args (apply fn args
                             (directive-at-sign element)
                             (directive-colon element)
                             (compute-parameters (directive-parameters element)))))))))
  args)

(defun format (destination control-string &rest arguments)
  (flet ((do-format (stream)
           (etypecase control-string
             (string
              (let ((*standard-output* stream))
                (interpret-format-control (parse-format-control control-string)
                                          arguments)))
             (function
              (apply control-string stream arguments)))
           nil))
    (cond
      ((eql destination 'nil)
       (with-output-to-string (stream)
         (do-format stream)))
      ((and (stringp destination)
            (array-has-fill-pointer-p destination))
       (do-format (make-instance 'sys.int::string-output-stream
                                 :element-type 'character
                                 :string destination)))
      ((eql destination 't)
       (do-format *standard-output*))
      ((streamp destination)
       (do-format destination))
      (t (error 'type-error
                :expected-type '(or
                                 (member nil t)
                                 stream
                                 (and string (not simple-string)))
                :datum destination)))))

(defun formatter-1 (stream control-string arguments)
  (let ((*standard-output* stream))
    ;; Call I-F-C directly instead of FORMAT so the remaining arguments
    ;; are returned.
    (interpret-format-control (parse-format-control control-string)
                              arguments)))

(defmacro formatter (control-string)
  (let ((stream (gensym "STREAM"))
        (arguments (gensym "ARGUMENTS")))
    `(lambda (,stream &rest ,arguments)
       (formatter-1 ,stream ',control-string ,arguments))))
