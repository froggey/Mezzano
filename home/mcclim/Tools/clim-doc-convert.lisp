;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: clim-doc-convert; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Convert the CLIM-Spec TeX sources to (sane) HTML
;;;   Created: 2001-01-23
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;       $Id: clim-doc-convert.lisp,v 1.1 2001/07/23 01:51:27 gilbert Exp $
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann

;;; $Log: $
;;;

(declaim (optimize (safety 3)))

;;; Notes

;; Macro processing in this program is quite erratic. 

;;; TODOs

;; - we need to listen for ligatures like ``, '', or ---.

;; - the structure of sections must be different to the page tree. 
;; That is \part should insert a new node in the page tree, but not
;; modify any section counters. This is made harder by the
;; requirement, that file names reflect CLIM section numbers.

;; - \footnote

;; Nested begin/end itemize? That is looking at \item alone is not
;; enough. Hmm??

;; Unicode:

;; I'll use my own and very crude way to embed unicode characters into
;; ASCII strings. We use "^Uhhhh" to denote the character #xhhhh.

;; e.g. "2^U03C0" is 2pi.

(defpackage :clim-doc-convert
  (:use :common-lisp)
  (:export
   #:convert-file
   #:run))

(in-package :clim-doc-convert)

;;(defparameter *clim-doc-pathname-defaults* '#p"home:try/clim/*.tex")
;;(defparameter *input-file* (merge-pathnames "test" *clim-doc-pathname-defaults*))

;;;; User tweakable Parameters:

(defparameter *clim-doc-pathname-defaults*
  '#p"home:work/McCLIM/Documentation/Specification/*.tex")

(defparameter *input-file*
  (merge-pathnames "clim" *clim-doc-pathname-defaults*))

(defparameter *output-directory*
    "/tmp/clim-spec/")

(defparameter *max-level* 2)

(defparameter *ascii-translate-p* t
  "When dumping HTML, whether to translate characters above &#126, by *ASCII-TRANSLATION*.")

(defparameter *tmp-dir* '#p"/tmp/*.$$$")

(defparameter *html-dpi* 100 ; X screens more often have 100dpi
  "Resolution of HTML files in dots per inch.")

;;;; Some unicode characters

(defparameter *ascii-translation*
  (make-hash-table :test #'eql)
  "A hash table which maps Unicode character indices to possible ASCII renderings.
   E.g. #x21D0 might turn out as \"<==\"")

(defmacro defchar (name code ascii-translation)
  `(progn
     (defconstant ,name ,code)
     (setf (gethash ,code *ascii-translation*)
       ,ascii-translation)))

(defchar c/leftwards-double-arrow      #x21D0   "<==")
(defchar c/rightwards-double-arrow     #x21D2   "==>")
(defchar c/greek-small-letter-pi       #x03C0   "pi")
(defchar c/less-than-or-equal-to       #x2264   "<=")
(defchar c/greater-than-or-equal-to    #x2265   ">=")
(defchar c/prime                       #x2032   "'")
(defchar c/left-double-quotation-mark  #x201C   "\"")
(defchar c/right-double-quotation-mark #x201D   "\"")
(defchar c/minus-sign                  #x2212   "-")
(defchar c/em-dash                     #x2014   "--")

(defconstant +nbsp+ (code-char 160))

(defparameter *ligature-table* 
    (make-hash-table :test #'eql))

(defmacro define-ligature (c1 c2 expansion)
  (let ((g1 (gensym)) (g2 (gensym)) (g3 (gensym)))
    `(progn
       (let ((,g1 ,c1) (,g2 ,c2))
         (let ((,g3 (+ (* #x10000 (if (characterp ,g1) (char-code ,g1) ,g1))
                       (if (characterp ,g2) (char-code ,g2) ,g2))))
           (setf (gethash ,g3 *ligature-table*)
             ,expansion))))))

(define-ligature #\` #\`           c/left-double-quotation-mark)
(define-ligature #\' #\'           c/right-double-quotation-mark)
(define-ligature #\- #\-           c/minus-sign)
(define-ligature c/minus-sign #\-  c/em-dash)

(defun lookup-ligature (c1 c2)
  (gethash (+ (* #x10000 (if (characterp c1) (char-code c1) c1))
              (if (characterp c2) (char-code c2) c2))
           *ligature-table*))

(defconstant +ucs-escape+ (code-char 21))

(defun emit-char (char sink &optional (writer #'write-char))
  (when (characterp char)
    (setf char (char-code char)))
  (cond ((and (<= 32 char 126)
              (char/= (code-char char) +ucs-escape+))
         (funcall writer (code-char char) sink))
        (t
         (map nil (lambda (c) (funcall writer c sink))
              (format nil "~A~4,'0X" +ucs-escape+ char)))) )
         

;;;;
;;;; Utilities
;;;;

(defparameter *bags* nil)

(defmacro with-bag ((var) &body body)
  (let ((g (gensym)))
    `(let ((,g (or (pop *bags*)
                   (make-array 10 
                               :element-type '#.(array-element-type "")
                               :fill-pointer 0
                               :adjustable t))))
       (setf (fill-pointer ,g) 0)
       (multiple-value-prog1
           (let ((,var ,g))
             ,@body)
         (push ,g *bags*)))))

(defun curry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append args more))))

(defun white-space-p (x)
  (or (char= x #\space) 
      (char= x #\newline)
      (char= x #\page)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; TeX Parsing
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun normal-char-p (ch)
  "Is 'ch' a character, which has no special meaning to TeX?"
  (when (characterp ch)
    (locally
        (declare (type character ch))
      (or (char<= #\a ch #\z)
          (char<= #\A ch #\Z)
          (char<= #\0 ch #\9)
          (find ch ",.*'`()+;:=/@^?\"<>")
          (char= ch #\|)                ;???
          (char= ch #\_)                ;???
          (char= ch #\space)
          (char= ch #\page)
          (char= ch #\-)
          (<= 160 (char-code ch) 255)))))

(defun slurp-one (input)
  (let ((ch (read-char input nil :eof)))
    (cond ((eq ch :eof) :eof)
          ((normal-char-p ch)
           ch)
          ((char= ch #\\)
           (slurp-backslash input))
          ((char= ch #\newline)
           (slurp-newline input))
          ((char= ch #\%)
           (slurp-% input)
           (values))
          ((char= ch #\{)
           (slurp-group input))
          ((char= ch #\[)
           (slurp-bracket input))
          ((char= ch #\$)
           `(:math ,(with-output-to-string (bag)
                      (do ((c (read-char input) (read-char input)))
                          ((char= c #\$))
                        (write-char c bag)))))
          ((char= ch #\~) +nbsp+)
          ((char= ch #\&) '(:macro "&"))
          ((char= ch #\#)
           #+:IGNORE
           `(:splice
             ,@(lookup-macro-arg
                (parse-integer
                 (with-output-to-string (bag)
                   (do ((c (read-char input) (read-char input)))
                       ((not (digit-char-p c))
                        (unread-char c input)
                        (skip-white-space input))
                     (write-char c bag))))))
           #-:IGNORE
           ch)
          (t
           (error "Unknown char: ~S" ch)) )))

(defvar *macro-env*)

(defun lookup-macro-arg (n)
  (cdr (or (assoc n *macro-env*)
           (error "Macro Argument ~D is not defined."))))

(defun slurp-generic-group (end-marker input)
  (with-bag (bag)
    (let ((res nil))
      (do ()
          ((equal end-marker (peek-char nil input nil :eof))
           (unless (eq :eof end-marker)
             (read-char input))         ;consume the end marker
           (when (> (length bag) 0)
             (push (copy-seq bag) res))
           (reverse res))
        (labels ((stuff (x)
                   (cond ((and (consp x) (eq (car x) :splice))
                          (mapc #'stuff (cdr x)))
                         ((characterp x)
                          (vector-push-extend x bag))
                         ((not (null x))
                          (when (> (length bag) 0)
                            (push (copy-seq bag) res)
                            (setf (fill-pointer bag) 0))
                          (push x res))) ))
          (stuff (slurp-one input)))))))

(defun slurp-group (input)
  `(:GROUP ,@(slurp-generic-group #\} input)))

(defun slurp-bracket (input)
  ;; gross over-simplification
  `(:GROUP ,@(slurp-generic-group #\] input)))

(defun slurp-math (input)
  `(:MATH ,@(slurp-generic-group #\$ input)))

(defun slurp-stream (input)
  (slurp-generic-group :eof input))

(defun ident-char-p (ch)
  (and (characterp ch)
       (or (char<= #\a ch #\z)
           (char<= #\A ch #\Z)
           (char<= #\0 ch #\9))))

(defun slurp-backslash (input)
  (let ((c0 (peek-char nil input nil nil)))
    (cond ((ident-char-p c0)
           (let ((name
                  (with-output-to-string (bag)
                    (do ((c (read-char input nil :eof) (read-char input nil :eof)))
                        ((not (ident-char-p c))
                         (unless (eq c :eof) (unread-char c input)))
                      (write-char c bag)))))
             (skip-white-space input)
             (cond ((string= name "verb")
                    (let ((delim (read-char input)))
                      `(:VERB ,(with-output-to-string (bag)
                                 (do ((c (read-char input) (read-char input)))
                                     ((char= c delim))
                                   (write-char c bag))))))
                   ((string= name "begin")
                    (slurp-begin input))
                   ((string= name "def")
                    (slurp-def input)
                    nil)
                   (t
                    `(:MACRO ,name)) )))
          ((char= c0 #\\) (read-char input) '(:MACRO "\\"))
          ((char= c0 #\@) (read-char input) '(:MACRO "@"))
          ((char= c0 #\=) (read-char input) '(:MACRO "="))
          ((char= c0 #\>) (read-char input) '(:MACRO ">"))
          ((char= c0 #\space) (read-char input) '(:MACRO " "))
          ((char= c0 #\') (read-char input) '(:MACRO "'"))
          ((char= c0 #\[) (read-char input) '(:MACRO "["))
          ((char= c0 #\]) (read-char input) '(:MACRO "]"))
          ((find c0 "%#_{}&") (read-char input) c0)
          (t
           (error "Unknown \\-char: ~S." c0)))))

(defun skip-white-space (input)
  (do () ((not (white-space-p (peek-char nil input nil :eof)))) (read-char input)))

(defun slurp-token (input)
  (let ((c (read-char input)))
    (cond ((char= c #\\)
           (values :macro
                   (with-output-to-string (bag)
                     (do ((c (read-char input nil :eof) (read-char input nil :eof)))
                         ((not (ident-char-p c))
                          (unless (eq c :eof) (unread-char c input))
                          (skip-white-space input))
                       (write-char c bag)))))
          ((char= c #\#)
           (values :macro-arg 
                   (parse-integer
                    (with-output-to-string (bag)
                      (do ((c (read-char input) (read-char input)))
                          ((not (digit-char-p c))
                           (unread-char c input)
                           (skip-white-space input))
                        (write-char c bag))))))
          ((find c "^_")
           (values :special c))
          ((char= c #\{)
           (values :group
                   (let ((nesting 1))
                     (with-output-to-string (bag)
                       (loop
                         (setq c (read-char input))
                         (cond ((char= c #\}) (decf nesting))
                               ((char= c #\{) (incf nesting)))
                         (when (zerop nesting)
                           (return))
                         (write-char c bag))))))
          (t
           (values :char c)) )))

(defun slurp-def (input)
  (multiple-value-bind (cat name) (slurp-token input)
    (unless (eq cat :macro)
      (error "Expected macro-token after \\def -- got ~S ~S." cat name))
    (let ((arglist nil))
      (do ((q (multiple-value-list (slurp-token input))
              (multiple-value-list (slurp-token input))))
          ((not (eq (car q) :macro-arg))
           (unless (eq (car q) :group)
             (error "Expected group after \\def\\~A ..." name))
           (do-/def name (reverse arglist) (cadr q)))
        (push q arglist))) ))

(defun do-/def (name arglist body)
  (print `(:def ,name ,arglist ,body))
  #+:IGNORE
  (setf (gethash name *macro-table*)
        (lambda (whole)
          (let ((*macro-env* nil))
            (dolist (k arglist)
              (let (val)
                (multiple-value-setq (val whole) (fetch-group whole))
                (push (cons (second k) val)
                      *macro-env*)))
            (values
             `(:splice ,@(process-list (slurp-stream (make-string-input-stream body))))
             whole))))
  nil)

(defun slurp-begin (input)
  (let ((name
         (do ((x (slurp-one input) (slurp-one input)))
             ((and (consp x) (eq (car x) :group))
              ;; xxx
              (cadr x))
           (unless (or (null x)
                       (and (stringp x)
                            (every #'white-space-p x)))
             (error "Expected group, got ~S." x)) )))
    (cond ((string= name "verbatim")
           (let ((body
                  (with-bag (bag)
                    (let ((end-marker "\\end{verbatim}"))
                      (do ((c (read-char input) (read-char input)))
                          ((and (>= (length bag) (length end-marker))
                                (string= bag end-marker :start1 (- (length bag) (length end-marker))))
                           (subseq bag 0 (- (length bag) (length end-marker))))
                        (vector-push-extend c bag))))))
             `(:splice (:macro "begin") (:group "verbatim")
                       ,body
                       (:macro "end") (:group "verbatim"))))
          (t
           `(:splice (:macro "begin") (:group ,name))) )))

(defun slurp-newline (input)
  (cond ((equal #\newline (peek-char nil input nil nil))
         '++para++)
        (t
         #\newline
         #\space)))

(defun slurp-% (input)
  (do ((c (read-char input nil :eof) (read-char input nil :eof)))
      ((or (eq c :eof) (char= c #\newline))
       nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; TeX Macros
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Defining Macros

(defparameter *macro-table*
    (make-hash-table :test #'equal)
  "A hash table, which maps from macro names as strings to macro functions.
   (what a macro functions gets and what it should return is described
   in DEFINE-MACRO).")

(defmacro define-macro (name (whole) &body body) "
  Defines a new LaTeX macro called 'name', which should be a
  string. When the macro is applied, 'body' is executed, while
  'whole' is bound to a list of all remaining tokens. 'body' should
  return two values: the expansion and the new remaining tokens. A
  macro is expected to expand to HTML. When a macro wants to return
  more than (or less than) one HTML node, it may return a cons,
  whose car is :SPLICE and whose cdr is the list of HTML nodes. The
  caller is responsible to resolve this splicing."
  ;;
  (unless (stringp name)
    (error "Use strings to name a LaTeX macro -- got ~S instead." name))
  `(progn
     (setf (gethash ,name *macro-table*)
           #'(lambda (,whole) ,@body))
     ',name) )

(defmacro define-simple-macro (name args &body body) 
  "Defines a simple macro. 'args' is the list of arguments, the
   macro accepts. These macros return one value: the expansion; splicing
   as described in DEFINE-MARCO is available.

   The list of arguments can contain '&STAR-FLAG variable' to indicate that
   a TeX #\* flag is accepted directly after the macro name. If the star is
   present 'variable' then set to 'true'."
  ;;
  (multiple-value-bind (args star-variable)
      (strip-lambda-parameter '&STAR-FLAG args)
    (let* ((whole (make-symbol "WHOLE"))
           (parse-and-bind-args
            (make-bind-args whole args
                            `(values
                              (progn ,@body)
                              ,whole) )))
      `(define-macro ,name (,whole)
         ,(if star-variable
              `(multiple-value-bind (,star-variable ,whole)
                   (fetch-star-flag ,whole)
                 ,parse-and-bind-args)
            parse-and-bind-args)) )))

(eval-when (compile eval load)
  (defun strip-lambda-parameter (name lambda-list)
    "Helper function for DEFINE-SIMPLE-MACRO."
    (labels ((lop (x)
                  (cond ((null x) (values nil nil))
                        ((eq (car x) name)
                         (values (cddr x) (cadr x)))
                        (t (multiple-value-bind (rest var)
                               (lop (cdr x))
                             (values (cons (car x) rest) var))))))
      (lop lambda-list)))
        
  (defun make-bind-args (whole args body)
    "Helper function for DEFINE-SIMPLE-MACRO."
    (cond ((null args) body)
          (t
           `(let (,(car args))
              (multiple-value-setq (,(car args) ,whole) (fetch-group ,whole))
              ,(make-bind-args whole (cdr args) body))))) )

;;; Helper functions for fetching macro arguments

(defun fetch-group (list)
  (cond ((and (stringp (car list))
              (every #'white-space-p (car list)))
         (fetch-group (cdr list)))
        ((and (consp (car list))
              (eq (caar list) :group))
         (values (cdar list)
                 (cdr list)))
        ;; xxx
        ((or (and (stringp (car list)))
             (and (consp (car list))
                  (eq (caar list) :macro)))
         (values (car list) (cdr list)))
        ;;
        (t
         (error "Expected group -- ~S" (subseq list 0 10)))))

(defun fetch-star-flag (token-list)
  (cond ((and (stringp (car token-list))
              (>= (length (car token-list)) 1)
              (char= (char (car token-list) 0) #\*))
         (values
          ;; star flag
          t
          ;; rest tokens
          (if (> (length (car token-list)) 1)
              (cons (subseq (car token-list) 1) (cdr token-list))
            (cdr token-list))))
        (t
         (values nil token-list))))

;;; Macro Processing

(defun process-list (x)
  "Macro processes a list of tokens, returns a list of HTML nodes."
  (cond ((null x) nil)
        ((stringp (car x))
         (cons (process-string (car x))
               (process-list (cdr x))))
        ((and (consp (car x))
              (eq (caar x) :macro))
         (multiple-value-bind (x rest) (process-macro (second (car x)) (cdr x))
           (cond ((and (consp x) (eq (car x) :splice))
                  (append (cdr x) (process-list rest)))
                 (t
                  (cons x (process-list rest))))))
        ((and (consp (car x))
              (eq (caar x) :group))
         (append (process-list (cdar x))
                 (process-list (cdr x)))
         #+NIL
         (cons `(:GROUP ,@(process-list (cdar x)))
               (process-list (cdr x))))
        (t
         (cons (car x)
               (process-list (cdr x))))))

(defun process-macro (name rest)
  (let ((fn (gethash name *macro-table*)))
    (cond ((not fn)
           (cerror "Retry to find Macro"
                   "Macro ~S has no definition." name)
           (process-macro name rest)
           #+NIL
           (progn
             (pushnew name *unknown-macros* :test #'string=)
             (values nil rest)))
          (t
           (funcall fn rest)))))

(defun map-over-string (fun str)
  (assert (typep str 'simple-string))
  (locally
      (declare (type simple-string str)
               (optimize (speed 3) (safety 1)))
    (let ((n (length str)))
      (declare (type fixnum n))
      (do ((i 0 (the fixnum (+ i 1))))
          ((>= i n))
        (declare (type fixnum i))
        (let ((c (schar str i)))
          (cond ((char= c +ucs-escape+)
                 (let ((c (parse-integer str :start (+ i 1) :end (+ i 5) :junk-allowed nil :radix 16)))
                   (incf i 4)
                   (funcall fun c)))
                (t
                 (funcall fun c))))))))

(defun process-string (string)
  ;; Here we care for ligatures
  (let ((c0 nil))
    (with-bag (bag)
      (map-over-string (lambda (c1)
                         (let ((x (and c0 (lookup-ligature c0 c1))))
                           (if x
                               (setf c0 x)
                             (progn
                               (and c0 (emit-char c0 bag #'vector-push-extend))
                               (setf c0 c1)))))
                       string)
      (and c0 (emit-char c0 bag #'vector-push-extend))
      (copy-seq bag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; HTML and Post-Processing
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-node-attributes ((&rest atts) node &body body)
  (let ((node-var (gensym)))
    `(let ((,node-var ,node))
       (let (,@(mapcar (lambda (att)
                         `(,att (node-attr ,node-var ,(intern (symbol-name att) :keyword)))) 
                       atts))
         ,@body))))

(defun node-children (node)
  (and (consp node)
       (do ((q (cdr node) (cddr q)))
           ((not (keywordp (car q)))
            q))))

(defun node-attributes (node)
  (let ((atts nil))
    (and (consp node)
         (do ((q (cdr node) (cddr q)))
             ((not (keywordp (car q)))
              (reverse atts))
           (push (car q) atts)
           (push (cadr q) atts)))))

(defun node-attr (node attr &optional default)
  (and (consp node)
       (getf (cdr node) attr default)))

(defun node-gi (node)
  (if (consp node) (car node) :pcdata))

(defun special-node-p (tag node)
  (and (eq :SPECIAL (node-gi node))
       (eq tag (node-attr node :tag))))

(defun stringify-lhtml (x)
  (cond ((stringp x) x)
        ((reduce (lambda (x y) (concatenate 'string x y))
                 (mapcar #'stringify-lhtml (node-children x))))))

;;; Macros emit HTML nodes. However, the language they can emit is
;;; augmented by the following constructs:

;;; (:SPECIAL :TAG <name> [..<other-atts>..] . <children>)

;;; (:SPECIAL :TAG :LABEL-1-NAME
;;;           :TITLE <title>)
;;;
;;; (:SPECIAL :TAG :LABEL-1
;;;           :NAME <name>)
;;;
;;; (:SPECIAL :TAG :LABEL-2
;;;           :NAME <name>
;;;           :TITLE <title>)
;;;
;;; (:SPECIAL :TAG :REF
;;;           :NAME <name>
;;;          [:TITLE <title>]
;;;          [:OPTIONALP <boolean>]
;;;    {<child>}* )
;;;
;;; (:SECTION :LEVEL <integer>
;;;           :TITLE <title>
;;;           :NUMBER <section-number-vector>
;;;    {<child>}*)

;;; Just before dumping, we have pure HTML, but the :PAGE node:

;;; (:PAGE :FILE <filename>
;;;        :TITLE <pagetitle>   ;list of nodes
;;;        :PREV <p>  \  automatically
;;;        :NEXT <n>  |  inserted by
;;;        :UP   <u>  /  COLLECT-LABELS
;;;    {<child>}*)

;;; Each such PAGE node corresponds to a physical HTML page (file). 
;;; These nest and all the navigational links are inserted by the
;;; dumper.

;;;;
;;;; Link Post Processing
;;;;

(defstruct label-definition name file title)

(defun collect-labels (node)
  ;; Cares for :LABEL-1, :LABEL-2 and :REF nodes.
  ;; Also inserts :PREV, :NEXT, and :UP attributes to :PAGE nodes, for
  ;; navigation.
  ;;
  (let ((hash (make-hash-table :test #'equal))
        (current-label-title nil)
        (references)
        (?name (make-symbol "?NAME"))
        (?title (make-symbol "?TITLE"))
        (prev-page nil))
    ;; Phase I: collect label definitions
    (labels ((walk-node-children (node current-file)
                                 (list (append (list (node-gi node))
                                               (node-attributes node)
                                               (mapcan (lambda (x) (walk x current-file))
                                                       (node-children node)))))
             (walk (node current-file)
                   (cond ((special-node-p :label-1 node)
                          (with-node-attributes (name) node
                            (setf (gethash name hash)
                                  (make-label-definition :name name
                                                         :file current-file
                                                         :title current-label-title))
                            (list `(:A :name ,name))))
                         ((special-node-p :label-1-name node)
                          (with-node-attributes (title) node
                            (setf current-label-title title))
                          nil)
                         ((special-node-p :label-2 node)
                          (with-node-attributes (name title) node
                            (setf (gethash name hash)
                                  (make-label-definition :name name
                                                         :file current-file
                                                         :title title))
                            (list `(:A :name ,name
                                       ,@(mapcan (lambda (x) (walk x current-file)) (node-children node))))))
                         ((special-node-p :ref node)
                          (let ((new-node (list* :A :HREF ?name
                                                 :target "_top"
                                                 (if (node-children node)
                                                     (mapcan #'(lambda (x) (walk x current-file))
                                                             (node-children node))
                                                   ?title))))
                            (push (cons (node-attributes node) new-node)
                                  references)
                            (list new-node)))
                         ;;
                         ((eq (node-gi node) :page)
                          (with-node-attributes (file) node
                            (let ((nnode (list* (node-gi node)
                                                :prev (and prev-page (node-attr prev-page :file))
                                                :next nil
                                                :up current-file
                                                (node-attributes node))))
                              (when prev-page
                                (setf (getf (cdr prev-page) :next)
                                      file))
                              (setf prev-page nnode)
                              (nconc nnode (mapcan (lambda (x) (walk x file))
                                                   (node-children node)))
                              (list nnode))))
                         ;;
                         ((consp node)
                          (walk-node-children node current-file))
                         (t
                          (list node)))))
      (prog1
          (walk node nil)
        ;; Phase II: Evil destruction!
        (let ((undefined nil))
          (dolist (ref references)
            (let* ((atts (car ref))
                   (node (cdr ref))
                   (name (getf atts :name)))
              (let ((def (gethash name hash)))
                (cond ((null def)
                       (unless (member name undefined :test #'equal)
                         (warn "Label ~S not defined." name)
                         (push name undefined))
                       (let ((c (node-children (subst (list "???") ?title node))))
                         (setf (car node) :SPLICE
                               (cdr node) c)))
                      (t
                       (let ((nnode
                              (subst (label-definition-title def) ?title
                                     (subst (format nil "~A#~A"
                                                    (label-definition-file def)
                                                    (label-definition-name def))
                                            ?name
                                            node))))
                         (setf (car node) (car nnode)
                               (cdr node) (cdr nnode))))))))) ))))

;;;;
;;;; Dumping
;;;;

(defparameter *no-close-tag-elements*
  '(:BR :P)
  "List of elements, which must not have an close tag.")

(defparameter *play-with-frames-p* nil
  "Whether you want to play with HTML frames, better leave this off.")

(defun dump (x sink)
  "Dumps a single HTML node to the output stream 'sink'.
   :PAGE nodes are handled here."
  (cond ((stringp x) (dump-string x sink))
        ((eq (node-gi x) :special)
         (warn "Unhandled special node: ~S." x)
         nil)
        ((eq x '++para++)
         (princ "<P>" sink))            ;xxx
        ((eq x :newline)
         (terpri sink))
        ((and (consp x) (eq (car x) :splice))
         (dolist (k (cdr x)) (dump k sink)))
        ((and (consp x) (eq (car x) :math))
         (princ (translate-math (cdr x)) sink))
        ;;
        ((and (consp x) (eq (car x) :page))
         (dump-page x sink))
        ;;
        ((consp x)
         (princ "<" sink)
         (princ (car x) sink)
         (do ((q (cdr x) (cddr q)))
             ((not (keywordp (car q)))
              (terpri sink)
              (princ ">" sink)
              (dolist (k q)
                (dump k sink))
              (unless (member (car x) *no-close-tag-elements*)
                (format sink "</~A~%>" (car x)) ))
           (princ " " sink)
           (princ (car q) sink)
           (princ "=\"" sink)
           (dump-string (princ-to-string (cadr q)) sink)
           (princ "\"" sink))) ))

(defun dump-string (str sink)
  (declare (optimize (safety 3)))
  (map-over-string (lambda (c &aux tr)
                     (when (characterp c) (setq c (char-code c)))
                     (cond ((= c #.(char-code #\&)) (princ "&amp;" sink))
                           ((= c #.(char-code #\<)) (princ "&lt;" sink))
                           ((= c #.(char-code #\>)) (princ "&gt;" sink))
                           ((= c #.(char-code +nbsp+)) (princ "&nbsp;" sink))
                           ((<= 32 c 126)
                            (write-char (code-char c) sink))
                           ((and *ascii-translate-p*
                                 (setq tr (gethash c *ascii-translation*)))
                            (dump-string tr sink))
                           (t
                            (format sink "&#~D;" c))))
                   str))

(defun dump-page (x sink)
  (with-node-attributes (title file prev next up) x
    (with-open-file (sink (merge-pathnames file *output-directory*)
                          :direction :output
                          :if-exists :new-version)
      (cond (*play-with-frames-p*
             (let ((nfile (format nil "n~A" file))
                   (cfile (format nil "c~A" file)))
               (dump `(:html
                       (:title ,@title)
                       (:frameset :rows "40, 1*"
                                  (:frame :src ,nfile
                                          :noresize :noresize
                                          :scrolling :no
                                          :marginheight 0
                                          :frameborder 0)
                                  (:frame :src ,cfile
                                          :noresize :noresize
                                          :frameborder 0
                                          :scrolling :auto)))
                     sink)
               (with-open-file (sink (merge-pathnames nfile *output-directory*)
                                     :direction :output
                                     :if-exists :new-version)
                 (dump `(:html
                         (:body
                          ,(build-navbar prev next up)))
                       sink))
               (with-open-file (sink (merge-pathnames cfile *output-directory*)
                                     :direction :output
                                     :if-exists :new-version)
                 (dump `(:html
                         (:head
                          (:link :rel :stylesheet
                                 :type "text/css"
                                 :href "clim.css"))
                         (:body
                          :bgcolor "#ffffff"
                          :text    "#000000"
                          ,@(node-children x)))
                       sink)) ))
            (t
             (dump `(:html
                     (:head
                      (:title ,@title)
                      (:link :rel :stylesheet
                             :type "text/css"
                             :href "clim.css"))
                     (:body
                      ,(build-navbar prev next up)
                      ,@(node-children x)))
                   sink))))
    ;;
    (dump `(:UL (:LI (:A :HREF ,(namestring file)
                         :TARGET "_top"
                         ,@title)))
          sink)))

(defun build-navbar (prev next up)
  `(:div
    (:table :width "100%"
            (:tr
             (:td :align :left
                  (,@(if prev
                         (list :A :HREF (namestring prev) :TARGET "_top")
                       (list :span))
                     "Prev"))
             (:td :align :center
                  (,@(if up
                         (list :A :HREF (namestring up) :TARGET "_top")
                       (list :span))
                     "Up"))
             (:td :align :right
                  (,@(if next
                         (list :A :HREF (namestring next) :TARGET "_top")
                       (list :span))
                     "Next"))))
    (:br)
    (:br)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Converting EPSI files to PNG files
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-integer-list (string &key (start 0) (end (length string)))
  (multiple-value-bind (value start) (parse-integer string :start start :end end :junk-allowed t)
    (cond ((null value)
           nil)
          (t
           (cons value (parse-integer-list string :start start :end end))))))

(defun epsi-bounding-box (filename)
  (with-open-file (input filename :direction :input)
    (let ((q "%%BoundingBox: "))
      (do ((x (read-line input nil :eof) (read-line input nil :eof)))
          ((eq x :eof)
           (error "No eps bounding box line file ~S." filename))
        (when (eql 0 (search q x))
          (let ((bb (parse-integer-list x :start (length q))))
            (unless (= (length bb) 4)
              (error "Strange eps bounding box line: ~S." x))
            (return (values-list bb))))))))

(defmacro with-temponary-file ((var) &body body)
  `(invoke-with-temponary-file #'(lambda (,var) ,@body)))

(defun invoke-with-temponary-file (cont)
  (loop
    (let ((fn (merge-pathnames (make-pathname 
                                :name (write-to-string (random (expt 36 10)) :base 36))
                               *tmp-dir*)))
      (unwind-protect
          (when (with-open-file (f fn 
                                 :direction :io 
                                 :if-exists nil
                                 :if-does-not-exist :create)
                  f)
            (funcall cont fn)
            (return))
        (ignore-errors (delete-file fn))))))

(defun convert-epsi-to-png (filename out-filename 
                            &key (scale (/ *html-dpi* 72))    
                                 (border 5))
  "Converts an epsi file to a png file."
  (with-temponary-file (tmpfile)
    (with-temponary-file (tmpfile2)
      (multiple-value-bind (llx lly urx ury) (epsi-bounding-box filename)
        (unless (zerop
                 (shell (format nil "epsffit -s ~D ~D ~D ~D ~A ~A" 
                                border border 
                                (+ border (- urx llx))
                                (+ border (- ury lly))
                                (escape-string-for-shell (namestring filename))
                                (escape-string-for-shell (namestring tmpfile)))))
          (error "Executing failed."))
        (unless (zerop
                 (shell (format nil "gs -q -sDEVICE=pnmraw -sOutputFile=~A -dNOPAUSE -dBATCH ~
                                     -r~D -g~Dx~D ~
                                     ~A"
                                (escape-string-for-shell (namestring tmpfile2))
                                (ceiling (* scale 72))
                                (ceiling (* scale (+ (* 2 border) (- urx llx))))
                                (ceiling (* scale (+ (* 2 border) (- ury lly))))
                                (escape-string-for-shell (namestring tmpfile)) )))
          (error "Executing of ghostscript failed."))
        (unless (zerop
                 (shell (format nil "pnmtopng ~A > ~A"
                                (escape-string-for-shell (namestring tmpfile2))
                                (escape-string-for-shell (namestring out-filename)))))
          (error "Executing of pnmtopng failed.")) ))))

#+:CMU
(defun shell (command)
  (ext:process-exit-code
   (ext:run-program "/bin/sh" (list "-c" command) :wait t :input nil :output nil)))

(defun ensure-png-file (eps-file png-file)
  (when (> (file-write-date eps-file)
           (or (file-write-date png-file) 0))
    (format T "~&;; Converting ~A" eps-file)
    (convert-epsi-to-png eps-file png-file)))

(defun convert-espi (eps-file)
  (let ((eps-file (merge-pathnames eps-file *clim-doc-pathname-defaults*)))
    (let ((png-file (make-pathname :name (pathname-name eps-file)
                                   :type "png")))
      (ensure-png-file eps-file (merge-pathnames png-file *output-directory*))
      png-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common LaTeX and TeX macros
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Font Styles

(define-macro "sl" (rest) `(:span :class :sl ,@(process-list rest)))
(define-macro "tt" (rest) `(:tt ,@(process-list rest)))
(define-macro "it" (rest) `(:i  ,@(process-list rest)))
(define-macro "bf" (rest) `(:b  ,@(process-list rest)))
(define-macro "huge" (rest) `(:font :size "+2" ,@(process-list rest)))

;;;; Labels

(define-simple-macro "label" (name)
  (let ((name (chunks-as-string name)))
    `(:SPECIAL :TAG :LABEL-1 :NAME ,name)))

(define-simple-macro "ref" (name)
  (let ((name (chunks-as-string name)))
    `(:SPECIAL :TAG :REF :NAME ,name)))

;;;; Title Page Hacks

(defvar *title*)
(defvar *authors*)

(define-simple-macro "title" (x)
  (setf *title* x)
  nil)

(define-simple-macro "author" (x)
  (setf *authors* x)
  nil)

(define-simple-macro "maketitle" ()
  `(:div
    (:center (:H1 (:font :size 7 ,@(process-list *title*))))
    (:br)
    (:center ,@(process-list *authors*))))

;;;; Sections

;;; Note

;; We go around some hacks here to not reset the \section counter,
;; when we see a \part.

(defvar *section-counter*)

;; This is another hack to have appendixes counted by letters, but
;; keeping the usual section numbering.

(defvar *appendix-part*)
(defvar *appendix-0-section*)

(defun dosection (level whole)
  (multiple-value-bind (title whole) (fetch-group whole)
    (unless (zerop level)               ;was \part or \appendix
      (loop for i from (1+ level) below (1- (length *section-counter*)) do
        (setf (aref *section-counter* i) 0)))
    (incf (aref *section-counter* level))
    (let ((sec-num (coerce (subseq *section-counter* 0 (1+ level)) 'list)))
      ;; gross hack
      (let ((q (process-list whole)))
        (let ((i (or (position-if #'(lambda (x)
                                      (and (consp x) (eq (car x) :section)
                                           (<= (getf (cdr x) :level)
                                               level)))
                                  q)
                     (length q))))
          (let* ((title (process-list title))
                 (body (cons `(,(convert-level-to-Hn level)
                               ,(human-readable-section-number sec-num)
                               " "
                               ,@title)
                             (subseq q 0 i)))
                 (tail (subseq q i)))
            `(:SPLICE
              (:SPECIAL :TAG :LABEL-1-NAME
                        :TITLE ,(remove-if (lambda (x) (special-node-p :label-1 x)) title))
              (:SECTION 
               :LEVEL ,level
               :TITLE ,title
               :NUMBER ,sec-num
               ,@body)
              ,@tail))) ))))

(defun human-readable-section-number (s)
  ;; Do not include the very first element, which represents the \part
  ;; number.
  (if (and (eql (elt s 0) *appendix-part*) (>= (length s) 2))
      (format nil "~{~D~#[~:;.~]~}"
              (cons
               (code-char (+ (char-code #\@)
                             (- (cadr s) *appendix-0-section*)))
               (cddr s)))
    (format nil "~{~D~#[~:;.~]~}" (cdr s))))

(define-macro "part" (whole)          (dosection 0 whole))

(define-macro "appendix" (whole)
  (progn (setf *appendix-part* (elt *section-counter* 0))
         (setf *appendix-0-section* (elt *section-counter* 1))
         `(:splice ,@(process-list whole))))

(define-macro "chapter" (whole)       (dosection 1 whole))
(define-macro "section" (whole)       (dosection 2 whole))
(define-macro "subsection" (whole)    (dosection 3 whole))
(define-macro "subsubsection" (whole) (dosection 4 whole))

;;;; Environments

(define-macro "begin" (whole)
  (multiple-value-bind (name whole) (fetch-group whole)
    (assert (and (= (length name) 1) (stringp (first name))))
    (setq name (car name))
    (let ((nesting 1)
          (body nil))
      (do ((q whole (cdr q)))
          (nil)
        (cond ((null q)
               (error "Unexpected EOF while seeking \\end{~A}."
                      name)))
        (when (macro-p (car q) "begin")
          (incf nesting))
        (when (macro-p (car q) "end")
          (decf nesting))
        (when (zerop nesting)
          (unless (equal (cadr q) `(:group ,name))
            (error "Bad \end ~S for \begin{~A}" (cadr q) name))
          (setf whole (cddr q))
          (return))
        (push (car q) body) )
      (values (doenvironment name (reverse body))
              whole))))

(defun macro-p (x name)
  (and (consp x)
       (eq (first x) :macro)
       (string= (second x) name)))

(defun doenvironment (name body)
  (cond ((string= name "tabular")      (do-tabular body))
        ((string= name "figure")       (do-figure body))
        ((string= name "description")  (do-itemize :DL :DD :DT body))
        ((string= name "itemize")      (do-itemize :UL :LI :B body))
        ((string= name "enumerate")    (do-itemize :OL :LI :B body))
        ((string= name "verbatim")     (do-verbatim body))
        ((string= name "document")     (do-document body))
        (t
         ;;(warn "Unknown environment: ~S." name)
         (pushnew name *unknown-environments* :test #'string=)
         "") ))

(defun do-document (body)
  ;; xxx simple
  `(:splice ,@(process-list body)))

(define-macro "item" (whole)
  (cond ((group-p (car whole))
         (multiple-value-bind (text whole) (fetch-group whole)
           (values `(:+++item+++ ,(process-list text))
                   whole)))
        (t
         (values
          `(:+++item+++ nil)
          whole) )))

(defun do-itemize (tag-list tag-text tag-item body)
  (let ((body (process-list body))
        text res)
    (loop
      (setq body (skip-white-off-list body))
      (when (null body)
        (return))
      (cond ((and (consp (car body)) (eq (caar body) :+++item+++))
             (when text (push `(,tag-text ,@(reverse text)) res))
             (when (second (car body))
               (push `(,tag-item ,@(second (car body))) res))
             (setf text nil)
             (pop body))
            (t
             (push (pop body) text))))
    (when text (push `(,tag-text ,@(reverse text)) res))
    `(,tag-list ,@(reverse res)) ))

(defun skip-white-off-list (q)
  (cond ((null q) nil)
        ((white-chunk-p (car q)) (skip-white-off-list (cdr q)))
        (t q)))

(defun white-chunk-p (x)
  (or (and (stringp x)
           (every #'white-space-p x))
      (eql x '++para++)))

(defun group-p (x)
  (and (consp x) (eq (car x) :group)))

(defun do-verbatim (body)
  `(:PRE ,@(process-list body)))

(defun split-at-macro (q name)
  (cond ((null q) nil)
        ((macro-p (car q) name)
         (cons nil (split-at-macro (cdr q) name)))
        (t
         (let ((rest (split-at-macro (cdr q) name)))
           (cons (cons (car q) (car rest))
                 (cdr rest)))) ))

(defun do-tabular (body)
  ;; half-baked attempt at tables.
  (multiple-value-bind (colspec body) (fetch-group body)
    colspec
    `(:TABLE
      ,@(mapcar (lambda (row)
                  `(:TR ,@(mapcar (lambda (col)
                                    (setf col (remove-if (lambda (x) (macro-p x "hline")) col))
                                    (setf col (remove :newline col))
                                    `(:TD ,@(process-list col))
                                    )
                                  (split-at-macro row "&"))))         
                (split-at-macro body "\\")))))

;;;; What is needed for figures

(define-simple-macro "centerline" (contents)
  `(:CENTER ,@(process-list contents)))

(define-simple-macro "caption" (contents)
  `(:+++CAPTION+++ ,@(process-list contents)))

(defun do-figure (body)
  (setq body (process-list body))
  ;; fetch a possible caption
  (let (caption)
    (setq caption (find-if (lambda (x) (and (consp x) (eq (car x) :+++CAPTION+++))) body)
          body    (remove-if (lambda (x) (and (consp x) (eq (car x) :+++CAPTION+++))) body))
    `(:table :align :center
             :border 1
             (:tr
              (:td
               ,@body))
             ,@(if caption
                   (list `(:tr (:td :align :center ,@(cdr caption))))
                 nil))))

;; "\epsfig{file=region-normalization.epsi}"

(define-simple-macro "epsfig" (spec)
  ;; I do not know the precise syntax to emulate.
  (assert (and (= (length spec) 1) (stringp (car spec))))
  (setq spec (car spec))
  (cond ((= 0 (search "file=" spec))
         (let ((filename (subseq spec (length "file="))))
           (let ((png-file (convert-espi filename)))
             `(:IMG :SRC ,(namestring png-file)))))
        (t
         (error "Do not know how to handle \\epsfig{~A}." spec)) ))

;;;; \input

(define-simple-macro "input" (x)
  (let ((fn (merge-pathnames (pathname (stringify x)) *clim-doc-pathname-defaults*)))
    (cond ((probe-file fn)
           (with-open-file (input fn :direction :input)
             `(:splice ,@(process-list (slurp-stream input)))))
          (t
           (warn "File ~S not found, or is unreadable." fn)))))

;;;; The Index

(defvar *index-entries*)

(defstruct index-entry
  name
  title)

(defun add-index-entry (name title)
  (pushnew (make-index-entry :name name
                             :title title)
           *index-entries*
           :test #'equalp))

(define-simple-macro "printindex" ()
  `(:div
    ,@(let ((entries (sort
                      (copy-list *index-entries*)
                      #'string<
                      :key (lambda (e)
                             (reduce (lambda (x y) (concatenate 'string x y))
                                     (mapcar #'stringify-lhtml (index-entry-title e)))))))
        (mapcan (lambda (e)
                  (list `(:special :tag :ref
                                   :title ,(index-entry-title e)
                                   :name ,(index-entry-name e))
                        `(:br)))
                entries))))

;;;; Some dummies

(defmacro defignore (name arglist)
  `(define-simple-macro ,name (&star-flag f ,@arglist)
     (warn "Ignored \\~A~A --~{ ~S~}" ,name (if f "*" "") (list ,@arglist))
     nil))

(defignore "documentclass" (x))
(defignore "pagestyle" (x))
(defignore "makeatletter" ())
(defignore "makeatother" ())
(defignore "renewenvironment" (x y z))

(define-simple-macro "renewenvironment" (x y z)
  (warn "Ignored \\renewenvironment -- ~S ~S ~S." x y z)
  nil)

;;;; Math

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; CLIM Specific Macros
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note, some of these should be handled by a TeX macro processor,
;; especially the canned phrases, no need to concentrate special
;; knowledge here.

(define-simple-macro "cl" (x)        `(:SPAN :CLASS "cl" ,@(process-list x)))
(define-simple-macro "arg" (x)       `(:SPAN :CLASS "arg" ,@(process-list x)))

(define-simple-macro "optional" ()   `(:splice (:TT "&optional") " "))
(define-simple-macro "rest" ()       `(:splice (:TT "&rest") " "))
(define-simple-macro "key" ()        `(:splice (:TT "&key") " "))
(define-simple-macro "allow" ()      `(:splice (:TT "&allow-other-keys") " "))
(define-simple-macro "body" ()       `(:splice (:TT "&body") " "))

;;; Characters
(define-simple-macro "&" ()
  "&")

(define-simple-macro "'" (x)
  (cond ((equal x '("e")) "é")
        (t
         (warn "Unknown acent: ~S." x))))

;;; Miscellaneous

(define-simple-macro "comment" (text)
  "")

(define-simple-macro "keepout" (text)
  "")

(define-simple-macro "Issue" (who what)
  (doissue "Major Issue" who what))

(define-simple-macro "issue" (who what)
  (doissue "Minor Issue" who what))

(defun doissue (kind who what)
  `(:div :class "issue"
         (:p
          (:B ,kind) 
          ": "
          (:I ,@(process-list what))
          ,(process-string " --- ")
          ,@(process-list who))))

;;;
;;; Canned phrases.
;;;

(define-simple-macro "IfYouWantClass" (a1 a2 a3)
  `(:splice
    ,@(process-list 
       `("If you want to create a new class that behaves "
         "like " ,@a1 " " ,@a2 ", it should be a subclass of "
         (:macro "cl") (:group ,@a3) "."
         " All instantiable subclasses of "
         (:macro "cl") (:group ,@a3) " must obey the " ,@a2 " protocol."))) )

(define-simple-macro "AbstractClass" ()
  "This class is an abstract class, intended only to be subclassed, not instantiated.")

(define-simple-macro "Mutable" () 
  "Members of this class are mutable.")

(define-simple-macro "Immutable" () 
  "Members of this class are immutable.")

(define-simple-macro "UncapturedInputs" () 
  "This function does not capture any of its mutable inputs.")

(define-simple-macro "MayCaptureInputs" () 
  "This function is permitted to capture its mutable inputs; 
   the consequences of modifying those objects are unspecified.")

(define-simple-macro "FreshOutputs" () 
  "This function returns fresh objects that may be modified.")

(define-simple-macro "ReadOnly" () 
  "This function returns objects that reveal CLIM's internal state;
   do not modify those objects.")

(define-simple-macro "ReadWrite" ()
  "This function returns objects that reveal CLIM's internal state; 
   these objects may be modified.")

(define-simple-macro "NotInRelease2" ()
  `(:splice
    ,@(process-list
       `((:group (:macro "sl") "This is not fully supported in Release 2.")))) )

;;;
;;; Other macros present in document body
;;;

(define-simple-macro "DrawingOptions" () 
  "ink clipping-region transformation ")

(define-simple-macro "PointOptions" ()
  "line-style line-thickness line-unit")

(define-simple-macro "LineCapOptions" ()
  "line-style line-thickness line-unit line-dashes line-cap-shape")

(define-simple-macro "LineJointOptions" ()
  "line-style line-thickness line-unit line-dashes line-joint-shape")

(define-simple-macro "LineJointCapOptions" ()
  "line-style line-thickness line-unit line-dashes line-joint-shape line-cap-shape")

(define-simple-macro "TextOptions" ()
  "text-style text-family text-face text-size")

(define-simple-macro "DefineFrameCommand" ()
  `(:splice
    ,@(process-list '((:macro "cl") (:group "define-" (:group (:macro "it") "frame") "-command")))))

(define-simple-macro "Gadget" (x)
  `(:splice
    ,@(process-list `((:macro "subsection")
                      (:group "The ")
                      (:group (:macro "tt") ,@x)
                      " Gadget"))))

;;;
;;; Definitions
;;;

(defun dodef (space? whole args? kind)
  (declare (ignore space?))
  (let (name args body)
    (multiple-value-setq (name whole) (fetch-group whole))
    (when args?
      (multiple-value-setq (args whole) (fetch-group whole)))
    (do ()
        ((or (null whole)
             (and (consp (car whole))
                  (eq (caar whole) :macro)
                  (def-stopper? (second (car whole)))))
         )
      (push (pop whole) body))
    (setf body (reverse body))
    (values
     (progn
       `(:div :class "def"
              ,(let ((id (mungle-def-name name kind))
                     (title (make-def-title name kind)))
                 (add-index-entry id title)
                 `(:special :tag :label-2
                            :name ,id
                            :title ,title))
              (:table
               :cellspacing 0
               :cellpadding 0
               (:tr
                (:td :nowrap :nowrap
                     :align :left
                     :valign :baseline
                     :width "0%"
                     (:B ,@(process-list `((:MACRO "cl") (:GROUP ,@name))))
                     ,(string +nbsp+)
                     )
                (:td :align :left :valign :baseline :width "100%"
                     (:I
                      ,@(process-list args)))
                (:td :align :right :valign :baseline :width "0%"
                     ,(string +nbsp+)
                     "[" (:I ,(substitute +nbsp+ #\space kind)) "]")))
              (:div :class "defbody"
                    ""
                    ,@(process-list body)) ))
     whole)))

(defun def-stopper? (x)
  (or (and (>= (length x) 3)
           (string-equal x "def" :end1 3))
      (member x
              '("paragraph" "appendix" "part" "chapter" "section" "subsection" "subsubsection"
                "callback" "Callback")
              :test #'equal)))

(defun mungle-def-name (name kind)
  ;; kind is a plain string
  ;; name is a list of tokens
  (format nil "DF_~A_~A"
          (chunks-as-string name)
          (remove-if #'white-space-p kind)))

(defun make-def-title (name kind)
  (process-list (list '(:macro "cl")
                      `(:group ,@name)
                      " "
                      kind)))

(define-macro "Defmacro" (whole)         (dodef t   whole t   "Macro"))
(define-macro "Defun" (whole)            (dodef t   whole t   "Function"))
(define-macro "Defgeneric" (whole)       (dodef t   whole t   "Generic Function"))
(define-macro "Defmethod" (whole)        (dodef t   whole t   "Method"))
(define-macro "Defaftermethod" (whole)   (dodef t   whole t   ":After Method"))
(define-macro "Defaroundmethod" (whole)  (dodef t   whole t   ":Around Method"))
(define-macro "Defvar" (whole)           (dodef t   whole nil "Variable"))
(define-macro "Defconst" (whole)         (dodef t   whole nil "Constant"))
(define-macro "Defprotoclass" (whole)    (dodef t   whole nil "Protocol Class"))
(define-macro "Defpredicate" (whole)     (dodef t   whole t   "Protocol Predicate"))
(define-macro "Defclass" (whole)         (dodef t   whole nil "Class"))
(define-macro "Definitarg" (whole)       (dodef t   whole nil "Initarg"))
(define-macro "Deftype" (whole)          (dodef t   whole nil "Type"))
(define-macro "Defoption" (whole)        (dodef t   whole nil "Option"))
(define-macro "Defptype" (whole)         (dodef t   whole t   "Presentation Type"))
(define-macro "DefptypeAbbrev" (whole)   (dodef t   whole t   "Presentation Type Abbreviation"))
(define-macro "Defcondition" (whole)     (dodef t   whole nil "Condition"))
(define-macro "Deferror" (whole)         (dodef t   whole nil "Error Condition"))
(define-macro "Defrestart" (whole)       (dodef t   whole nil "Restart"))

(define-macro "defmacro" (whole)         (dodef nil whole t   "Macro"))
(define-macro "defun" (whole)            (dodef nil whole t   "Function"))
(define-macro "defgeneric" (whole)       (dodef nil whole t   "Generic Function"))
(define-macro "defmethod" (whole)        (dodef nil whole t   "Method"))
(define-macro "defvar" (whole)           (dodef nil whole nil "Variable"))
(define-macro "defconst" (whole)         (dodef nil whole nil "Constant"))
(define-macro "defprotoclass" (whole)    (dodef nil whole nil "Protocol Class"))
(define-macro "defpredicate" (whole)     (dodef nil whole t   "Protocol Predicate"))
(define-macro "defclass" (whole)         (dodef nil whole nil "Class"))
(define-macro "definitarg" (whole)       (dodef nil whole nil "Initarg"))
(define-macro "defoption" (whole)        (dodef nil whole nil "Option"))
                                       
(define-macro "Defcommandtable" (whole)  (dodef t   whole nil "Command Table"))
(define-macro "Defframe" (whole)         (dodef t   whole nil "Application Frame"))

(define-macro "Defgadget" (whole)        (dodef t   whole nil "Abstract Gadget"))
(define-macro "defgadget" (whole)        (dodef nil whole nil "Abstract Gadget"))

(define-macro "Defspane" (whole)         (dodef t   whole nil "Service Pane"))
(define-macro "defspane" (whole)         (dodef nil whole nil "Service Pane"))
(define-macro "Deflpane" (whole)         (dodef t   whole nil "Layout Pane"))
(define-macro "deflpane" (whole)         (dodef nil whole nil "Layout Pane"))

(define-macro "Callback" (whole)         (dodef t   whole t   "Callback Generic Function"))
(define-macro "callback" (whole)         (dodef t   whole t   "Callback Generic Function"))

(define-macro "Defdp" (whole) (dodef t whole t "Server Path"))
(define-macro "Defpresmeth" (whole) (dodef t whole t "Presentation Method"))

;; \def\NoChars{
;; \def\OneChar{
;; \def\TwoChars{
;; \def\ThreeChars{

;;; Misc

(define-simple-macro "GlossaryEntry" (a b c)
  `(:splice
    ,@(process-list
       `((:macro "item")
         (:group ,@a)
         (:group (:macro "it") ,@b)
         " "
         ,@c))))

(define-simple-macro "paragraph" (x)
  ;; XXX Is this a CLIM or a LaTeX macro?
  `(:H4 ,@(process-list x)))

;; Hard to do:
(define-simple-macro "[" () "[")
(define-simple-macro "]" () "]")

(define-simple-macro " " () " ");?? what is this?

(defun stringify (x)
  (cond ((stringp x) x)
        ((and (consp x) (= (length x) 1) (stringp (car x)))
         (car x))
        (t
         (error "Cannot 'stringify' ~S." x))))

(define-simple-macro "term" (x)
  `(:SPECIAL :TAG :REF
             :NAME ,(concatenate 'string "CONCEPT_" (chunks-as-string x))
             :OPTIONALP t
             :CLASS "term"
             (:SPAN :CLASS "term" ,@(process-list x))))

(define-simple-macro "concept" (x)
  (let ((id (concatenate 'string "CONCEPT_" (chunks-as-string x)))
        (title (process-list x)))
    (add-index-entry id title)
    `(:SPECIAL :TAG :LABEL-2
               :NAME ,id
               :TITLE ,title
               :CLASS "concept"
               (:SPAN :CLASS "concept" ,@(process-list x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Mess
;;;;

(defparameter *math-table*
    (make-hash-table :test #'equal))

(defparameter *unknown-math* nil)

(defun translate-math (x)
  (cond ((and (= (length x) 1)
              (stringp (first x))
              (every #'(lambda (c) 
                         (or (alpha-char-p c)
                             (digit-char-p c)
                             (find c "(), +-=")))
                     (first x)))
         (with-output-to-string (bag)
           (map nil (lambda (c)
                      (if (alpha-char-p c)
                          (format bag "<I>~A</I>" c)
                        (write-char c bag)))
                (first x))))
        (t
         (or (gethash x *math-table*)
             (progn
               (warn "+++++++ Unknown math: ~S." x)
               (pushnew x *unknown-math* :test #'equal)
               ""))) ))

(defun (setf translate-math) (v x)
  (setf (gethash x *math-table*) v)
  (setf *unknown-math* (delete x *unknown-math* :test #'equal))
  v)

(defparameter *unknown-macros* nil)
(defparameter *unknown-environments* nil)

(progn
  (setf (translate-math '("x")) "<I>x</I>")
  (setf (translate-math '("y")) "<I>y</I>")
  (setf (translate-math '("d")) "<I>d</I>")
  (setf (translate-math '("p1")) "<I>p1</I>")
  (setf (translate-math '("p2")) "<I>p2</I>")
  (setf (translate-math '("(x,y)")) "(<I>x</I>,<I>y</I>)")
  (setf (translate-math '("x_c")) "<I>x</I><SUB>c</SUB>")
  (setf (translate-math '("y_c")) "<I>y</I><SUB>c</SUB>")
  (setf (translate-math '("dx_1")) "<I>dx</I><SUB>1</SUB>")
  (setf (translate-math '("dy_1")) "<I>dy</I><SUB>1</SUB>")
  (setf (translate-math '("dx_2")) "<I>dx</I><SUB>2</SUB>")
  (setf (translate-math '("dy_2")) "<I>dy</I><SUB>2</SUB>")
  (setf (translate-math '("x_c + dx_1 + dx_2")) "<I>x</I><SUB>c</SUB> + <I>dx</I><SUB>1</SUB> + <I>dx</I><SUB>2</SUB>")
  (setf (translate-math '("y_c + dy_1 + dy_2")) "<I>y</I><SUB>c</SUB> + <I>dy</I><SUB>1</SUB> + <I>dy</I><SUB>2</SUB>")
  (setf (translate-math '("x_c + dx_1 - dx_2")) "<I>x</I><SUB>c</SUB> + <I>dx</I><SUB>1</SUB> - <I>dx</I><SUB>2</SUB>")
  (setf (translate-math '("y_c + dy_1 - dy_2")) "<I>y</I><SUB>c</SUB> + <I>dy</I><SUB>1</SUB> - <I>dy</I><SUB>2</SUB>")
  (setf (translate-math '("x_c - dx_1 - dx_2")) "<I>x</I><SUB>c</SUB> - <I>dx</I><SUB>1</SUB> - <I>dx</I><SUB>2</SUB>")
  (setf (translate-math '("y_c - dy_1 - dy_2")) "<I>y</I><SUB>c</SUB> - <I>dy</I><SUB>1</SUB> - <I>dy</I><SUB>2</SUB>")
  (setf (translate-math '("x_c - dx_1 + dx_2")) "<I>x</I><SUB>c</SUB> - <I>dx</I><SUB>1</SUB> + <I>dx</I><SUB>2</SUB>")
  (setf (translate-math '("y_c - dy_1 + dy_2")) "<I>y</I><SUB>c</SUB> - <I>dy</I><SUB>1</SUB> + <I>dy</I><SUB>2</SUB>")
  (setf (translate-math '("dx_2 = dy_1 = 0")) "<I>dx</I><SUB>2</SUB> = <I>dy</I><SUB>1</SUB> = 0")
  (setf (translate-math '("dx_1 = dy_2 = 0")) "<I>dx</I><SUB>1</SUB> = <I>dy</I><SUB>2</SUB> = 0")
  (setf (translate-math '("0")) "0")
  (setf (translate-math '("2" (:MACRO "pi"))) "2&#960;")
  (setf (translate-math '("(r_1,g_1,b_1,o_1)")) "(<I>r</I><SUB>1</SUB>,&nbsp;<I>g</I><SUB>1</SUB>,&nbsp;<I>b</I><SUB>1</SUB>,&nbsp;<I>o</I><SUB>1</SUB>)")
  (setf (translate-math '("(r_2,g_2,b_2,o_2)")) "(<I>r</I><SUB>2</SUB>,&nbsp;<I>g</I><SUB>2</SUB>,&nbsp;<I>b</I><SUB>2</SUB>,&nbsp;<I>o</I><SUB>2</SUB>)")
  (setf (translate-math '("(r_3,g_3,b_3,o_3)")) "(<I>r</I><SUB>3</SUB>,&nbsp;<I>g</I><SUB>3</SUB>,&nbsp;<I>b</I><SUB>3</SUB>,&nbsp;<I>o</I><SUB>3</SUB>)")
  (setf (translate-math '("1/72")) "1/72")
  (setf (translate-math '("2\\pi")) "2&#960;")
  (setf (translate-math '("IP < FP")) "IP < FP")
  (setf (translate-math '("SP \\leq IP \\leq FP")) "SP <= IP <= FP")
  (setf (translate-math '("\\Rightarrow")) "->")
  (setf (translate-math '("\\arg{n}-1")) "<I>n</I>-1")
  (setf (translate-math '("\\sqrt{3}")) "sqrt(3)")
  (setf (translate-math '("b_3")) "<I>b</I><SUB>1</SUB>")
  (setf (translate-math '("g_3")) "<I>g</I><SUB>3</SUB>")
  (setf (translate-math '("o_1")) "<I>o</I><SUB>1</SUB>")
  (setf (translate-math '("o_2 = 1")) "<I>o</I><SUB>2</SUB>&nbsp;=&nbsp;1")
  (setf (translate-math '("o_2")) "<I>o</I><SUB>2</SUB>")
  (setf (translate-math '("o_3 = 1")) "<I>o</I><SUB>3</SUB>&nbsp;=&nbsp;1")
  (setf (translate-math '("o_3")) "<I>o</I><SUB>3</SUB>")
  (setf (translate-math '("r_3")) "<I>r</I><SUB>3</SUB>")
  (setf (translate-math '("x^\\prime")) "<I>x</I>'")
  (setf (translate-math '("y^\\prime")) "<I>y</I>'")
  (setf (translate-math '("{\\cal F}")) "<I><B>F</B></I>")
  (setf (translate-math '("{\\cal F}\\colon (r_1,g_1,b_1,o_1,r_2,g_2,b_2,o_2)\\rightarrow (r_3,g_3,b_3,o_3)"))
        "<I><B>F</B></I>:&nbsp;(<I>r</I><SUB>1</SUB>,<I>g</I><SUB>1</SUB>,<I>b</I><SUB>1</SUB>,<I>o</I><SUB>1</SUB>,<I>r</I><SUB>2</SUB>,<I>g</I><SUB>2</SUB>,<I>b</I><SUB>2</SUB>,<I>o</I><SUB>2</SUB>)&nbsp;->&nbsp;(<I>r</I><SUB>3</SUB>,<I>g</I><SUB>3</SUB>,<I>b</I><SUB>3</SUB>,<I>o</I><SUB>3</SUB>)")
  )

(defun chunks-as-string (x)
  (unless (and (= (length x) 1)
               (stringp (first x)))
    (error "Expected a plain string here, got ~S." x))
  (remove-if #'white-space-p (first x)))

;; Math

;; This is a very simple attempt to support math.
;; We support some macros, like \pi
;; We support sub- and super-scripting.
;; Letters are rendered in italic, digits in roman.
;; Additionally subscripts and superscripts are always rendered in roman.
;; Resulting HTML is not beautified and we see operators as letters.

(defun m-slurp-token (input)
  (multiple-value-bind (cat cont) (slurp-token input)
    (cond ((eq cat :macro)
           (cond ((string= cont "pi") 
                  (values :char "pi"))
                 ((string= cont "leq") 
                  (values :char "<="))
                 ((string= cont "Rightarrow") 
                  (values :char "==>"))
                 ;;;
                 ((string= cont "pi") 
                  (values :char (format nil "&#~D;" c/greek-small-letter-pi)))
                 ((string= cont "leq") 
                  (values :char (format nil " &#~D; " c/less-than-or-equal-to)))
                 ((string= cont "Rightarrow") 
                  (values :char (format nil " &#~D; " c/rightwards-double-arrow)))
                 (t
                  (error "Undefined math-macro: ~S." cont))))
          (t
           (values cat cont)) )))

(defun math-letter-p (ch)
  (and (characterp ch)
       (alpha-char-p ch)))

(defun math-non-letter-p (ch)
  "Math non-letters, which could be copied verbatim."
  (or (and (characterp ch)
           (or (digit-char-p ch)
               (find ch " +-/<>=(),")))
      (stringp ch)))                    ;xxx

(defun m (input &optional (letter-font :I))
  (multiple-value-bind (cat cont) (m-slurp-token input)
    (ecase cat 
      (:eof nil)
      (:special
       (ecase cont
         ((#\^ #\_ )
          (let ((tag (ecase cont (#\^ :sup) (#\_ :sub))))
            (multiple-value-bind (cat2 cont2) (m-slurp-token input)
              (ecase cat2
                (:group 
                 `(,tag ,@(ms (make-string-input-stream cont2) nil)))
                (:char
                 `(,tag ,(string cont2)))))))))
      (:char
       (cond ((math-letter-p cont)
              (if letter-font `(,letter-font ,(string cont)) (string cont)))
             ((math-non-letter-p cont)
              (string cont))
             (t
              (error "Foo! ~S." cont)))) )))

(defun ms (input &optional (letter-font :I))
  (cond ((eq (peek-char nil input nil :eof) :eof)
         nil)
        (t
         (cons (m input letter-font)
               (ms input)))))
;;

(defun escape-string-for-shell (str)
  (with-output-to-string (bag)
    (loop for c across str do
          (cond ((or (alpha-char-p c) (digit-char-p c)
                     (find c ".,:-_/%^"))
                 (write-char c bag))
                (t
                 (write-char #\\ bag)
                 (write-char c bag))))))

(defun string* (x)
  (cond ((pathnamep x) (namestring (truename x)))
        (t (string x))))

(defun unix-grep (rx &rest files)
  #+EXCL
  (excl:shell (format nil "grep ~A~{ ~A~}"
                      (escape-string-for-shell rx)
                      (mapcar #'escape-string-for-shell (mapcar #'string* files)))))

;;;;

(defun dump-foo (nodes)
  `(:page
    :title ("CLIM II Specification")
    :file "index.html"
    ,@(dump-list nodes 0)))

(defun dump-list (nodes level)
  (let ((res nil)
        (toc nil))
    (dolist (x nodes)
      (cond ((consp x)
             (case (node-gi x)
               (:section
                (with-node-attributes (title number) x
                  ;; (format T "~&;; ~v<~> ~S ~A" (* 2 level) number title)
                  (let ((fn (make-pathname :name (section-number-to-filename number)
                                           :type "html")))
                    (let ((sub (dump-list (node-children x) (+ level 1) )))
                      (cond ((>= level *max-level*)
                             (dolist (k sub) (push k res)))
                            (t
                             (push `(:page
                                     :title ,title
                                     :file  ,fn
                                     ,@sub)
                                   res) ))))))
               (t
                (push x res))))
            (t
             (push x res))))
    (setq toc (reverse toc))
    `(,@(and toc (list
                  `(:UL
                    ,@(mapcar (lambda (x)
                                `(:LI ,x))
                              toc))))
               
      ,@(reverse res)) ))

(defun section-number-to-filename (sec-num)
  (format nil "section-~{~D.~}" sec-num)
  (format nil "s~{~2,'0D~}" sec-num))

(defun convert-level-to-Hn (level)
  (ecase level
    (0 :H1) (1 :H2) (2 :H3) (3 :H4) (4 :H5) (5 :H6)))

;;;;;;;

;;; ------------------------------------------------------------------------------------------
;;;  Testing the Document's Integrity
;;;

;; We want to compare the PostScript output word by word with the HTML
;; output. For that we use pdftotext on clim.pdf and Netscape Save as
;; Ascii on a special HTML version (everything in one file).

;; In the second step we try to find a canonic representation, that is
;; we compress white space to exactly one #\Newline.

;; The we use diff to find differences. Minor differences like
;; hyphenation are then sorted out later.

;; More intelligent would be to forbid TeX hyphenation.
;; This can be achieved by:
;;
;;    \hyphenpenalty=100000
;;    \exhyphenpenalty=100000
;;

(defun canon-white-space (input output)
  (do ((c0 #\null c1)
       (c1 (read-char input nil :eof)
           (read-char input nil :eof)))
      ((eql c1 :eof))
    (cond ((white-space-p c1)
           (cond ((white-space-p c0)
                  nil)
                 (t 
                  (princ #\newline output))))
          (t
           (princ c1 output)) )))

(defun bar ()
  (let ((in "/home/gilbert/work/McCLIM/Spec/src/clim.text")
        (out "/tmp/orig"))
    (with-open-file (in in)
      (with-open-file (out out :direction :output :if-exists :new-version)
        (canon-white-space in out))))
  (let ((in "/tmp/clim-spec/clim.text")
        (out "/tmp/my"))
    (with-open-file (in in)
      (with-open-file (out out :direction :output :if-exists :new-version)
        (canon-white-space in out)))))

(defignore "rm" ())

(defignore "parindent" (x))
(defignore "parskip" (x))
(defignore "renewcommand" (x y))
(defignore "newcommand" (x y z))
(defignore "pagebreak" ())
(defignore "tableofcontents" ())
(defignore "setcounter" (x y))
(defignore "usepackage" (x))

(defignore "date" (x))
(defignore "markright" (x))
(defignore "makeindex" ())

(defignore "hrule" ())
(defignore "footnote" (x))

(defignore "setlength" (x y))
(defignore "multicolumn" (x y z))
(defignore "newpage" ())

(defignore "include" (x))
(defignore "newif" (x y))
(defignore "addtolength" (x y))

(defignore "vspace" (x))
(defignore "vskip" (x))
  
(defignore "par" ())

(define-simple-macro "\\" (&star-flag f)
  `(:br))

;;;;;

;;;

(defun convert-file (&optional (fn *input-file*))
  (let ((*input-file* fn)
        (*clim-doc-pathname-defaults* fn)
        (*section-counter*  (vector 0 0 0 0 0 0 0 0))
        (*appendix-part*    nil)
        (*appendix-0-section* nil)
        (*index-entries* nil))
    (setf *unknown-macros* nil)
    (setf *unknown-environments* nil)
    (let (qq)
      (progn
        (setq qq (dump-foo (process-list
                            (with-open-file (input *input-file*)
                              (slurp-stream input)))))
        (setq qq (car (collect-labels qq)))
        (dump qq nil) ))
    (format T "~&;; Unknown macros:~{~%;;    ~A~}" *unknown-macros*)
    (format T "~&;; Unknown environments:~{~%;;    ~A~}" *unknown-environments*)
    (values) ))

(defparameter *McCLIM-base-dir*
  (let ((lp *load-pathname*))
    (make-pathname 
     :host (pathname-host lp)
     :device (pathname-device lp)
     :directory (let ((i (position "McCLIM" (pathname-directory lp)
                                   :test #'string-equal)))
                  (if i
                      (subseq (pathname-directory lp) 0 (+ 1 i))
                    (progn
                      (warn "Where is the McCLIM base in ~S." lp)
                      nil)))
     :name nil
     :type nil)))

(defun run ()
  (convert-file (make-pathname
                 :directory (append (pathname-directory *McCLIM-base-dir*)
                                    (list "Spec" "src"))
                 :name "clim"
                 :type "tex"
                 :defaults *McCLIM-base-dir*)))

;;;;
