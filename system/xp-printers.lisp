(in-package :mezzano.xp)

(defun fn-call (xp list)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list))

;Although idiosyncratic, I have found this very useful to avoid large
;indentations when printing out code.

(defun alternative-fn-call (xp list)
  (if (> (length (symbol-name (car list))) 12)
      (funcall (formatter "~:<~1I~@{~W~^ ~_~}~:>") xp list)
      (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list)))

(defun bind-list (xp list &rest args)
    (declare (ignore args))
  (if (do ((i 50 (1- i))
           (ls list (cdr ls))) ((null ls) t)
        (when (or (not (consp ls)) (not (symbolp (car ls))) (minusp i))
          (return nil)))
      (pprint-fill xp list)
      (funcall (formatter "~:<~@{~:/pprint-fill/~^ ~_~}~:>") xp list)))

(defun block-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~^~W~^ ~@_~W~^~@{ ~_~W~^~}~:>") xp list))

(defun defun-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~W~^ ~@_~W~^ ~@_~:/pprint-fill/~^~@{ ~_~W~^~}~:>")
           xp list))

(defun print-fancy-fn-call (xp list template)
  (let ((i 0) (in-first-section T))
    (pprint-logical-block+ (xp list "(" ")" nil T nil)
      (write+ (pprint-pop) xp)
      (pprint-indent+ :current 1 xp)
      (loop
        (pprint-exit-if-list-exhausted)
        (write-char++ #\space xp)
        (when (eq i (car template))
          (pprint-indent+ :block (cadr template) xp)
          (setf template (cddr template))
          (setf in-first-section nil))
        (pprint-newline (cond ((and (zerop i) in-first-section) :miser)
                              (in-first-section :fill)
                              (T :linear))
                        xp)
        (write+ (pprint-pop) xp)
        (incf i)))))

(defun maybelab (xp item &rest args)
    (declare (ignore args) (special need-newline indentation))
  (when need-newline (pprint-newline+ :mandatory xp))
  (cond ((and item (symbolp item))
         (write+ item xp)
         (setf need-newline nil))
        (T (pprint-tab+ :section indentation 0 xp)
           (write+ item xp)
           (setf need-newline T))))

(defun function-call-p (x)
  (and (consp x) (symbolp (car x)) (fboundp (car x))))

;THE FOLLOWING STUFF SETS UP THE DEFAULT *PRINT-PPRINT-DISPATCH*

;This is an attempt to specify a correct format for every form in the CL book
;that does not just get printed out like an ordinary function call
;(i.e., most special forms and many macros).  This of course does not
;cover anything new you define.

(defun let-print (xp obj)
  (funcall (formatter "~:<~1I~W~^ ~@_~/mezzano.xp::bind-list/~^~@{ ~_~W~^~}~:>") xp obj))

(defun cond-print (xp obj)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~:/pprint-linear/~^ ~_~}~:>") xp obj))

(defun dmm-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun defsetf-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun do-print (xp obj)
  (funcall
 (formatter "~:<~W~^ ~:I~@_~/mezzano.xp::bind-list/~^ ~_~:/pprint-linear/ ~1I~^~@{ ~_~W~^~}~:>")
           xp obj))

(defun flet-print (xp obj)
  (funcall (formatter "~:<~1I~W~^ ~@_~:<~@{~/mezzano.xp::block-like/~^ ~_~}~:>~^~@{ ~_~W~^~}~:>")
           xp obj))

(defun function-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (formatter "#'~W") xp (cadr list))
      (fn-call xp list)))

(defun mvb-print (xp list)
  (print-fancy-fn-call xp list '(1 3 2 1)))

(defun prog-print (xp list)
  (let ((need-newline T) (indentation (1+ (length (symbol-name (car list))))))
    (declare (special need-newline indentation))
    (funcall (formatter "~:<~W~^ ~:/pprint-fill/~^ ~@{~/mezzano.xp::maybelab/~^ ~}~:>")
             xp list)))

(defun setf-print (xp obj)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~:_~W~^ ~_~}~:>") xp obj))

(defun quote-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (formatter "'~W") xp (cadr list))
      (pprint-fill xp list)))

(defun tagbody-print (xp list)
  (let ((need-newline (and (consp (cdr list))
                           (symbolp (cadr list)) (cadr list)))
        (indentation (1+ (length (symbol-name (car list))))))
    (declare (special need-newline indentation))
    (funcall (formatter "~:<~W~^ ~@{~/mezzano.xp::maybelab/~^ ~}~:>") xp list)))

(defun up-print (xp list)
  (print-fancy-fn-call xp list '(0 3 1 1)))

;here is some simple stuff for printing LOOP

;The challange here is that we have to effectively parse the clauses of the
;loop in order to know how to print things.  Also you want to do this in a
;purely incremental way so that all of the abbreviation things work, and
;you wont blow up on circular lists or the like.  (More aesthic output could
;be produced by really parsing the clauses into nested lists before printing them.)

;The following program assumes the following simplified grammar of the loop
;clauses that explains how to print them.  Note that it does not bare much
;resemblence to the right parsing grammar, however, it produces half decent
;output.  The way to make the output better is to make the grammar more
;detailed.
;
;loop == (LOOP {clause}*)      ;one clause on each line.
;clause == block | linear | cond | finally
;block == block-head {expr}*   ;as many exprs as possible on each line.
;linear == linear-head {expr}* ;one expr on each line.
;finally == FINALLY [DO | DOING | RETURN] {expr}* ;one expr on each line.
;cond == cond-head [expr]
;          clause
;          {AND clause}*       ;one AND on each line.
;        [ELSE
;          clause
;          {AND clause}*]      ;one AND on each line.
;        [END]
;block-head == FOR | AS | WITH | AND
;              | REPEAT | NAMED | WHILE | UNTIL | ALWAYS | NEVER | THEREIS | RETURN
;              | COLLECT | COLLECTING | APPEND | APPENDING | NCONC | NCONCING | COUNT
;              | COUNTING | SUM | SUMMING | MAXIMIZE | MAXIMIZING | MINIMIZE | MINIMIZING
;linear-head == DO | DOING | INITIALLY
;var-head == FOR | AS | WITH
;cond-head == IF | WHEN | UNLESS
;expr == <anything that is not a head symbol>

;Note all the string comparisons below are required to support some
;existing implementations of LOOP.

(defun token-type (token &aux string)
  (cond ((not (symbolp token)) :expr)
        ((string= (setf string (string token)) "FINALLY") :finally)
        ((member string '("IF" "WHEN" "UNLESS") :test #'string=) :cond-head)
        ((member string '("DO" "DOING" "INITIALLY") :test #'string=) :linear-head)
        ((member string '("FOR" "AS" "WITH" "AND" "END" "ELSE"
                          "REPEAT" "NAMED" "WHILE" "UNTIL" "ALWAYS" "NEVER"
                          "THEREIS" "RETURN" "COLLECT" "COLLECTING" "APPEND"
                          "APPENDING" "NCONC" "NCONCING" "COUNT" "COUNTING"
                          "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING"
                          "MINIMIZE" "MINIMIZING")
                 :test #'string=)
         :block-head)
        (T :expr)))

(defun pretty-loop (xp loop)
  (if (not (and (consp (cdr loop)) (symbolp (cadr loop)))) ; old-style loop
      (fn-call xp loop)
      (pprint-logical-block (xp loop :prefix "(" :suffix ")")
        (let (token type)
          (labels ((next-token ()
                     (pprint-exit-if-list-exhausted)
                     (setf token (pprint-pop))
                     (setf type (token-type token)))
                   (print-clause (xp)
                     (case type
                       (:linear-head (print-exprs xp nil :mandatory))
                       (:cond-head (print-cond xp))
                       (:finally (print-exprs xp T :mandatory))
                       (otherwise (print-exprs xp nil :fill))))
                   (print-exprs (xp skip-first-non-expr newline-type)
                     (let ((first token))
                       (next-token)     ;so always happens no matter what
                       (pprint-logical-block (xp nil)
                         (write first :stream xp)
                         (when (and skip-first-non-expr (not (eq type :expr)))
                           (write-char #\space xp)
                           (write token :stream xp)
                           (next-token))
                         (when (eq type :expr)
                           (write-char #\space xp)
                           (pprint-indent :current 0 xp)
                           (loop (write token :stream xp)
                                 (next-token)
                                 (when (not (eq type :expr)) (return nil))
                                 (write-char #\space xp)
                                 (pprint-newline newline-type xp))))))
                   (print-cond (xp)
                     (let ((first token))
                       (next-token)     ;so always happens no matter what
                       (pprint-logical-block (xp nil)
                         (write first :stream xp)
                         (when (eq type :expr)
                           (write-char #\space xp)
                           (write token :stream xp)
                           (next-token))
                         (write-char #\space xp)
                         (pprint-indent :block 2 xp)
                         (pprint-newline :linear xp)
                         (print-clause xp)
                         (print-and-list xp)
                         (when (and (symbolp token)
                                    (string= (string token) "ELSE"))
                           (print-else-or-end xp)
                           (write-char #\space xp)
                           (pprint-newline :linear xp)
                           (print-clause xp)
                           (print-and-list xp))
                         (when (and (symbolp token)
                                    (string= (string token) "END"))
                           (print-else-or-end xp)))))
                   (print-and-list (xp)
                     (loop (when (not (and (symbolp token)
                                           (string= (string token) "AND")))
                                 (return nil))
                           (write-char #\space xp)
                           (pprint-newline :mandatory xp)
                           (write token :stream xp)
                           (next-token)
                           (write-char #\space xp)
                           (print-clause xp)))
                   (print-else-or-end (xp)
                     (write-char #\space xp)
                     (pprint-indent :block 0 xp)
                     (pprint-newline :linear xp)
                     (write token :stream xp)
                     (next-token)
                     (pprint-indent :block 2 xp)))
            (pprint-exit-if-list-exhausted)
            (write (pprint-pop) :stream xp)
            (next-token)
            (write-char #\space xp)
            (pprint-indent :current 0 xp)
            (loop (print-clause xp)
                  (write-char #\space xp)
                  (pprint-newline :linear xp)))))))

;Backquote is a big problem we MUST do all this reconsing of structure in
;order to get a list that will trigger the right formatting functions to
;operate on it.  On the other side of the coin, we must use a non-list structure
;for the little backquote printing markers to ensure that they will always
;print out the way we want no matter what the code printers say.
;  Note that since it is sometimes possible to write the same
;backquote form in several ways, this might not necessarily print out a
;form in exactly the way you wrote it.  For example '`(a .,b) and '`(a ,@b)
;both print out as `'(a .,b), because the backquote reader produces the
;same code in both cases.

(defvar *bq-list* #+:lucid 'lucid-runtime-support:bq-list
                  #+:symbolics 'si:xr-bq-list)
(defvar *bq-list** #+:lucid 'lucid-runtime-support:bq-list*
                   #+:symbolics 'si:xr-bq-list*)
(defvar *bq-cons* #+:lucid 'lucid-runtime-support:bq-cons
                  #+:symbolics 'si:xr-bq-cons)
(defvar *bq-append* #+:lucid 'lucid-runtime-support:bq-append
                    #+:symbolics 'si:xr-bq-append)
(defvar *bq-nconc* #+:lucid 'lucid-runtime-support:bq-nconc
                   #+:symbolics 'si:xr-bq-nconc)

(defun bq-print (xp obj)
  (funcall (formatter "`~W") xp (bqtify obj)))

(defvar *bq-vector* #+:lucid 'lucid-runtime-support:bq-nconc
                   #+:symbolics (list nil)) ;turned off
(defvar *bq-list-to-vector* #+:lucid 'lucid-runtime-support:bq-nconc
                            #+:symbolics (list nil)) ;turned off

(defun bq-vector-print (xp obj)
  (funcall (formatter "`#~W") xp (car (bqtify obj))))

(defclass bq-struct ()
  ((code :initarg :code :initform nil :accessor bq-struct-code)
   (data :initarg :data :initform nil :accessor bq-struct-data)))

(defun make-bq-struct (&rest args)
  (apply 'make-instance 'bq-struct args))

(defun bq-struct-p (object)
  (typep object 'bq-struct))

(defun bq-struct-print (xp obj)
  ;; We must print out the string as a string, in order to prevent
  ;; circularity testing
  (let ((code (bq-struct-code obj)))
    (declare (type simple-string code))
    (write-string++ code xp 0 (length code))
    (write+ (bq-struct-data obj) xp)))

;Convert the backquote form to a list resembling what the user typed in,
;with calls to printers for ",", ",@", etc.

(defun bqtify (exp)
  (cond ((or (numberp exp) (eq exp t) (null exp) (stringp exp)) exp)
        ((symbolp exp) (make-bq-struct :code "," :data exp))
        ((bq-struct-p exp)
         (make-bq-struct :code "," :data exp))
        ((atom exp) exp)
        ((eq (car exp) 'quote) (cadr exp))
        ((eq (car exp) *bq-list*)
         (mapcar 'bqtify (cdr exp)))
        ((eq (car exp) *bq-cons*)
         (cons (bqtify (cadr exp)) (bqtify-inline (cddr exp) nil)))
        ((eq (car exp) *bq-list**)
         (nconc (mapcar 'bqtify (butlast (cdr exp)))
                (bqtify-inline (last exp) nil)))
        ((eq (car exp) *bq-append*)
         (mapcon (lambda (x) (bqtify-inline x t)) (cdr exp)))
        ((eq (car exp) *bq-nconc*)
         (mapcon (lambda (x) (bqtify-inline x nil)) (cdr exp)))
        ((eq (car exp) *bq-vector*)
         (list (mapcar 'bqtify (cdr exp))))
        ((eq (car exp) *bq-list-to-vector*)
         (mapcar 'bqtify (cdr exp)))
        (t (make-bq-struct :code "," :data exp))))

;Convert a thing in a bq-form which is being expanded into the list, not
;just being made an element.  The argument is the list whose car is the
;form, and the value is stuff to be appended into the resulting code list.

(defun bqtify-inline (loc copy-p)
  (cond ((atom (cdr loc))
         (let ((tem (bqtify (car loc))))
           (cond ((and (bq-struct-p tem) (equal (bq-struct-code tem) ","))
                  (list (make-bq-struct :code ".," :data (car loc))))
                 (t tem))))
        ((and (listp (car loc))
              (eq (caar loc) 'quote)
              (listp (cadar loc)))
         (cadar loc))
        (t (list (make-bq-struct :code (cond (copy-p ",@") (T ",."))
                                 :data (car loc))))))

(setf *IPD* (make-pprint-dispatch))

(set-pprint-dispatch+ '(satisfies function-call-p) 'fn-call '(-5) *IPD*)
(set-pprint-dispatch+ 'cons 'pprint-fill '(-10) *IPD*)

#+(or :lucid :symbolics)(eval-when (eval load)
(set-pprint-dispatch+ 'bq-struct 'bq-struct-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-cons*)) 'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-list*)) 'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-list**)) 'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-append*)) 'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-nconc*)) 'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-vector*)) 'bq-vector-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-list-to-vector*)) 'bq-vector-print '(0) *IPD*) )

(set-pprint-dispatch+ '(cons (member defstruct)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member block)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member case)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member catch)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member ccase)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member compiler-let)) 'let-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member cond)) 'cond-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member ctypecase)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defconstant)) 'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member define-setf-method)) 'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defmacro)) 'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member define-modify-macro)) 'dmm-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defparameter)) 'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defsetf)) 'defsetf-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member define-compiler-macro)) 'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defstruct)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member deftype)) 'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defun)) 'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defvar)) 'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member do)) 'do-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member do*)) 'do-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member do-all-symbols)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member do-external-symbols)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member do-symbols)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member dolist)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member dotimes)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member ecase)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member etypecase)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member eval-when)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member flet)) 'flet-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member function)) 'function-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member labels)) 'flet-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member lambda)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member let)) 'let-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member let*)) 'let-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member locally)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member loop)) 'pretty-loop '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member macrolet)) 'flet-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member multiple-value-bind)) 'mvb-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member multiple-value-setf)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member prog)) 'prog-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member prog*)) 'prog-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member progv)) 'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member psetf)) 'setf-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member psetf)) 'setf-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member quote)) 'quote-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member return-from)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member setf)) 'setf-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member setf)) 'setf-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member tagbody)) 'tagbody-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member throw)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member typecase)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member unless)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member unwind-protect)) 'up-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member when)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member with-input-from-string)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member with-open-file)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member with-open-stream)) 'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member with-output-to-string)) 'block-like '(0) *IPD*)

(defmethod describe-object ((table pprint-dispatch-table) xp)
  (let ((stuff (copy-list (others table))))
    (maphash (lambda (key val) (declare (ignore key))
                     (push val stuff))
             (conses-with-cars table))
    (maphash (lambda (key val) (declare (ignore key))
                     (push val stuff))
             (structures table))
    (setf stuff (sort stuff 'priority-> :key (lambda (x) (car (full-spec x)))))
    (pprint-logical-block (xp stuff)
      (format xp "~A is a pprint dispatch table containing ~A entries: "
              table (length stuff))
      (loop (pprint-exit-if-list-exhausted)
            (let ((entry (pprint-pop)))
              (format xp (formatter "~{~_P=~4D ~W~} F=~W ")
                      (full-spec entry) (fn entry)))))))

;so only happens first time is loaded.
(when (member *print-pprint-dispatch* '(nil T))
  (setf *print-pprint-dispatch* (copy-pprint-dispatch nil)))
