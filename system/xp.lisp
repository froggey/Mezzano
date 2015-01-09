;-*-syntax:COMMON-LISP;Package:(XP :use "COMMON-LISP" :colon-mode :external)-*-

(in-package "XP" :use '("LISP"))

;This is the November, 26 1991 version of
;Richard C. Waters' XP pretty printer.

;The standard version of this program is available by anonymous FTP
;from MERL.COM in the files /pub/xp/xp*.  If you have gotten the file
;from somewhere else, or copied the files a long time ago, you might
;consider copying them from MERL.COM now to obtain the latest version.

;------------------------------------------------------------------------

;Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------

;The functions in this file are documented in Chapter 27 of Common Lisp:
;the Language Second Edition, Guy L. Steele Jr, Digital press, 1990,
;and in even greater detail in
;  MIT/AIM-1102a, July 1989.  
;This report can be obtained by writing to

;              Publications
;	       MIT AI Laboratory
;	       545 Tech. Sq.
;	       Cambridge MA 02139

;This file attempts to be as compatible with pure Common Lisp as possible.
;It has been tested on the following Common Lisps to date.
;  Symbolics CL versions 7 and 8 (does not work in version 6).
;  LUCID CL version 3.0.2 on a sun.
;  Allegro CL version 1.2.1 on a Macintosh.
;  LispWorks CL version 2.1.
;  CMU CL.

;The companion file "XPTEST.LISP" contains a set of 600+ tests.  You should
;run these tests after the first time you compile this file on a new system.

;The companion file "XPDOC.TXT" contains brief documentation.

(provide "XP")

(shadow '(write print prin1 princ pprint format write-to-string princ-to-string
	  prin1-to-string write-line write-string write-char terpri fresh-line
	  defstruct finish-output force-output clear-output))

(export '(formatter copy-pprint-dispatch pprint-dispatch
	  set-pprint-dispatch pprint-fill pprint-linear pprint-tabular
	  pprint-logical-block pprint-pop pprint-exit-if-list-exhausted
	  pprint-newline pprint-indent pprint-tab 
	  *print-pprint-dispatch* *print-right-margin* *default-right-margin*
	  *print-miser-width* *print-lines*
	  *last-abbreviated-printing*
	  #+symbolics pp))

(defvar *xp-printing-functions*
	'(write print prin1 princ pprint format write-to-string princ-to-string
	  prin1-to-string write-line write-string write-char terpri fresh-line
	  defstruct finish-output force-output clear-output)
  "printing functions redefined by xp.")

;must do the following in common lisps not supporting *print-shared*

(defvar *print-shared* nil)
(export '(*print-shared*))

(defvar *print-pprint-dispatch* t ;see initialization at end of file.
  "controls pretty printing of output")
(defvar *print-right-margin* nil
  "+#/nil the right margin for pretty printing")
(defvar *print-miser-width* 40.
  "+#/nil miser format starts when there is less than this width left")
(defvar *print-lines* nil
  "+#/nil truncates printing after # lines")
(defvar *default-right-margin* 70.
  "controls default line length; must be a non-negative integer")
(defvar *last-abbreviated-printing*
	#'(lambda (&optional stream) (declare (ignore stream)) nil)
  "funcalling this redoes the last xp printing that was abbreviated.")

(defvar *ipd* nil ;see initialization at end of file.
  "initial print dispatch table.")
(defvar *current-level* 0
  "current depth in logical blocks.")
(defvar *current-length* 0
  "current position in logical block.")
(defvar *abbreviation-happened* nil
  "t if current thing being printed has been abbreviated.")
(defvar *result* nil "used to pass back a value")

;default (bad) definitions for the non-portable functions

#-(or :symbolics :lucid :franz-inc :cmu)(eval-when (eval load compile)
(defun structure-type-p (x) (and (symbolp x) (get x 'structure-printer)))
(defun output-width     (&optional (s *standard-output*)) (declare (ignore s)) nil)
(defun output-position  (&optional (s *standard-output*)) (declare (ignore s)) nil) )


#+:symbolics(eval-when (eval load compile)
(defun structure-type-p (x) (and (symbolp x) (get x 'si:defstruct-description)))
(defun output-width     (&optional (s *standard-output*))
  (si:send s :send-if-handles :size-in-characters))
(defun output-position  (&optional (s *standard-output*))
  (si:send s :send-if-handles :read-cursorpos :character)) )


;XP is being considered for inclusion in Lucid Common Lisp.
;The prime contact there is Eric Benson "eb@lucid.com".

#+:lucid(eval-when (eval load compile)
(defun structure-type-p (x) (subtypep x 'structure))
(defun output-width     (&optional (s *standard-output*)) (declare (ignore s)) nil)
(defun output-position  (&optional (s *standard-output*)) (declare (ignore s)) nil) )


;XP is being included in CMU's Common Lisp.
;The prime contact there is Bill Chiles "chiles@cs.cmu.edu"
;and/or Blain Burks "mbb@cs.cmu.edu".

#+:cmu(eval-when (eval load compile)
(defun structure-type-p (x) (and (symbolp x) (get x 'lisp::%structure-definition)))
(defun output-width     (&optional (s *standard-output*)) (declare (ignore s)) nil)
(defun output-position  (&optional (s *standard-output*)) (lisp::charpos s)) )


;Definitions for FRANZ Common Lisp. (Only verified for the version 1.3
;(5/31/87) currently running on suns at MIT.)

#+:franz-inc(eval-when (eval load compile)
(defun structure-type-p (x) (and (symbolp x) (get x 'structure-printer)))
(defun output-width     (&optional (s *standard-output*)) (declare (ignore s)) nil)
(defun output-position  (&optional (s *standard-output*)) (excl::charpos s)) )


; Joachim Laubsch <laubsch%hpljl@hplabs.hp.com> is the contact at HP Labs.
; He reports that HP COMMON LISP II Development Environment Rev A.02.15 11/10/88
; requires the following patch due to a bug in REPLACE:

#+(and lucid hp (not patched))
(eval-when (eval load)
  (system::defadvice (replace patch)
      (SEQUENCE1 SEQUENCE2 &KEY (START1 0) END1 (START2 0) END2)
    (declare (fixnum START1 START2))
    (let ((copy-length (min (- (or end1 (length SEQUENCE1)) START1)
			    (- (or end2 (length SEQUENCE2)) START2))))
      (declare (fixnum copy-length))
      (if (= 0 copy-length)
	  SEQUENCE1
	(system::apply-advice-continue SEQUENCE1 SEQUENCE2
				       :START1 START1
				       :END1 (+ START1 copy-length)
				       :START2 START2
				       :END2 (+ START2 copy-length)
				       ()))
	
      )))

#+symbolics
(defvar original-trace-print nil)

(defun install (&key (package *package*) (macro nil) (shadow T) (remove nil))
  (when (not (packagep package)) (setq package (find-package package)))
  (when (not remove)
    (when macro
      (set-dispatch-macro-character #\# #\" #'format-string-reader))
    (when (not (eq package (find-package "XP")))
      (use-package "XP" package)
      (when shadow (shadowing-import *xp-printing-functions* package)))
  #+symbolics 
    (progn
      (when (null original-trace-print)
	(setq original-trace-print (symbol-function 'si:trace-print)))
      (setf (symbol-function 'si:trace-print) (symbol-function 'trace-print))
      (zl:setq-standard-value scl:*print-pretty-printer* 'pretty-printer)))
  (when (and remove (member (find-package "XP") (package-use-list package)))
    (unuse-package "XP" package)
    (dolist (sym (intersection *xp-printing-functions*
			       (package-shadowing-symbols package)))
      (unintern sym package))
  #+symbolics
    (progn
      (when original-trace-print
	(setf (symbol-function 'si:trace-print) original-trace-print))
      (zl:setq-standard-value scl:*print-pretty-printer* 'gprint:print-object)))
  T)


(defvar *locating-circularities* nil
  "Integer if making a first pass over things to identify circularities.
   Integer used as counter for #n= syntax.")
(defvar *parents* nil "used when *print-shared* is nil")

(defvar *circularity-hash-table* nil
  "Contains hash table used for locating circularities, or a stack.")
;When an entry is first made it is zero.
;If a duplicate is found, a positive integer tag is assigned.
;After the first time the object is printed out, the tag is negated.

(defvar *free-circularity-hash-tables* nil
  "free list of circularity hash tables") ; never bound

(defun get-circularity-hash-table ()
  (let ((table (pop *free-circularity-hash-tables*)))
    (if table table (make-hash-table :test 'eq))))

;If you call this, then the table gets efficiently recycled.

(defun free-circularity-hash-table (table)
  (clrhash table)
  (pushnew table *free-circularity-hash-tables*))

;                       ---- DISPATCHING ----

(lisp:defstruct (pprint-dispatch (:conc-name nil) (:copier nil))
  (conses-with-cars (make-hash-table :test #'eq) :type hash-table)
  (structures (make-hash-table :test #'eq) :type hash-table)
  (others nil :type list))

;The list and the hash-tables contain entries of the
;following form.  When stored in the hash tables, the test entry is 
;the number of entries in the OTHERS list that have a higher priority.

(lisp:defstruct (entry (:conc-name nil))
  (test nil)        ;predicate function or count of higher priority others.
  (fn nil)          ;pprint function
  (full-spec nil))  ;list of priority and type specifier

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (when (null table) (setq table *IPD*))
  (let* ((new-conses-with-cars
           (make-hash-table :test #'eq
	     :size (max (hash-table-count (conses-with-cars table)) 32)))
	 (new-structures
	   (make-hash-table :test #'eq
	     :size (max (hash-table-count (structures table)) 32))))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-conses-with-cars) (copy-entry value)))
	     (conses-with-cars table))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-structures) (copy-entry value)))
	     (structures table))
    (make-pprint-dispatch
      :conses-with-cars new-conses-with-cars
      :structures new-structures
      :others (copy-list (others table)))))

(defun set-pprint-dispatch (type-specifier function
			    &optional (priority 0) (table *print-pprint-dispatch*))
  (when (or (not (numberp priority)) (complexp priority))
    (error "invalid PRIORITY argument ~A to SET-PPRINT-DISPATCH" priority))
  (set-pprint-dispatch+ type-specifier function priority table))

(defun set-pprint-dispatch+ (type-specifier function priority table)
  (let* ((category (specifier-category type-specifier))
	 (pred
	   (if (not (eq category 'other)) nil
	       (let ((pred (specifier-fn type-specifier)))
		 (if (and (consp (caddr pred))
			  (symbolp (caaddr pred))
			  (equal (cdaddr pred) '(x)))
		     (symbol-function (caaddr pred))
		     (compile nil pred)))))
	 (entry (if function (make-entry :test pred
					 :fn function
					 :full-spec (list priority type-specifier)))))
    (case category
      (cons-with-car
	(cond ((null entry) (remhash (cadadr type-specifier) (conses-with-cars table)))
	      (T (setf (test entry)
		       (count-if #'(lambda (e)
				     (priority-> (car (full-spec e)) priority))
				 (others table)))
		 (setf (gethash (cadadr type-specifier) (conses-with-cars table)) entry))))
      (structure-type
	(cond ((null entry) (remhash type-specifier (structures table)))
	      (T (setf (test entry)
		       (count-if #'(lambda (e)
				     (priority-> (car (full-spec e)) priority))
				 (others table)))
		 (setf (gethash type-specifier (structures table)) entry))))
      (T ;other
	 (let ((old (car (member type-specifier (others table) :test #'equal
				 :key #'(lambda (e) (cadr (full-spec e)))))))
	   (when old
	     (setf (others table) (delete old (others table)))
	     (adjust-counts table (car (full-spec old)) -1)))
	 (when entry
	   (let ((others (cons nil (others table))))
	      (do ((l others (cdr l)))
		  ((null (cdr l)) (rplacd l (list entry)))
		(when (priority-> priority (car (full-spec (cadr l))))
		  (rplacd l (cons entry (cdr l)))
		  (return nil)))
	      (setf (others table) (cdr others)))
	   (adjust-counts table priority 1)))))
  nil)

(defun priority-> (x y)
  (if (consp x)
      (if (consp y) (> (car x) (car y)) nil)
      (if (consp y) T (> x y))))


(defun adjust-counts (table priority delta)
  (maphash #'(lambda (key value)
	         (declare (ignore key))
	       (if (priority-> priority (car (full-spec value)))
		   (incf (test value) delta)))
	   (conses-with-cars table))
  (maphash #'(lambda (key value)
	         (declare (ignore key))
	       (if (priority-> priority (car (full-spec value)))
		   (incf (test value) delta)))
	   (structures table)))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (when (null table) (setq table *IPD*))  
  (let ((fn (get-printer object table)))
    (values (or fn #'non-pretty-print) (not (null fn)))))

(defun get-printer (object table)
  (let* ((entry (if (consp object)
		    (gethash (car object) (conses-with-cars table))
		    (gethash (type-of object) (structures table)))))
    (if (not entry)
	(setq entry (find object (others table) :test #'fits))
	(do ((i (test entry) (1- i))
	     (l (others table) (cdr l)))
	    ((zerop i))
	  (when (fits object (car l)) (setq entry (car l)) (return nil))))
    (when entry (fn entry))))

(defun fits (obj entry) (funcall (test entry) obj))

(defun specifier-category (spec)
  (cond ((and (consp spec)
	      (eq (car spec) 'cons)
	      (consp (cdr spec))
	      (null (cddr spec))
	      (consp (cadr spec))
	      (eq (caadr spec) 'member)
	      (consp (cdadr spec))
	      (null (cddadr spec)))
	 'cons-with-car)
	((and (symbolp spec) (structure-type-p spec)) 'structure-type)
	(T 'other)))

(defvar *preds-for-specs*
  '((T always-true) (cons consp) (simple-atom simple-atom-p) (other otherp)
    (null null) (symbol symbolp) (atom atom) (cons consp)
    (list listp) (number numberp) (integer integerp)
    (rational rationalp) (float floatp) (complex complexp)
    (character characterp) (string stringp) (bit-vector bit-vector-p)
    (vector vectorp) (simple-vector simple-vector-p) 
    (simple-string simple-string-p) (simple-bit-vector simple-bit-vector-p)
    (array arrayp) (package packagep) (function functionp)
    (compiled-function compiled-function-p) (common commonp)))

(defun always-true (x) (declare (ignore x)) T)

(defun specifier-fn (spec)
  `(lambda (x) ,(convert-body spec)))

(defun convert-body (spec)
  (cond ((atom spec)
	 (let ((pred (cadr (assoc spec *preds-for-specs*))))
	   (if pred `(,pred x) `(typep x ',spec))))
	((member (car spec) '(and or not))
	 (cons (car spec) (mapcar #'convert-body (cdr spec))))
	((eq (car spec) 'member)
	 `(member x ',(copy-list (cdr spec))))
	((eq (car spec) 'cons)
	 `(and (consp x)
	       ,@(if (cdr spec) `((let ((x (car x)))
				    ,(convert-body (cadr spec)))))
	       ,@(if (cddr spec) `((let ((x (cdr x)))
				     ,(convert-body (caddr spec)))))))
	((eq (car spec) 'satisfies)
	 `(funcall (function ,(cadr spec)) x))
	(T `(typep x ',(copy-tree spec)))))

;               ---- XP STRUCTURES, AND THE INTERNAL ALGORITHM ----

(eval-when (eval load compile) ;not used at run time.
  (defvar block-stack-entry-size 1)
  (defvar prefix-stack-entry-size 5)
  (defvar queue-entry-size 7)
  (defvar buffer-entry-size 1)
  (defvar prefix-entry-size 1)
  (defvar suffix-entry-size 1))

(eval-when (eval load compile) ;used at run time
  (defvar block-stack-min-size #.(* 35. block-stack-entry-size))
  (defvar prefix-stack-min-size #.(* 30. prefix-stack-entry-size))
  (defvar queue-min-size #.(* 75. queue-entry-size))
  (defvar buffer-min-size 256.)
  (defvar prefix-min-size 256.)
  (defvar suffix-min-size 256.)) 

(lisp:defstruct (xp-structure (:conc-name nil) (:print-function describe-xp))
  (BASE-STREAM nil) ;;The stream io eventually goes to.
  LINEL ;;The line length to use for formatting.
  LINE-LIMIT ;;If non-NIL the max number of lines to print.
  LINE-NO ;;number of next line to be printed.
  CHAR-MODE ;;NIL :UP :DOWN :CAP0 :CAP1 :CAPW
  CHAR-MODE-COUNTER ;depth of nesting of ~(...~)
  DEPTH-IN-BLOCKS
   ;;Number of logical blocks at QRIGHT that are started but not ended.
  (BLOCK-STACK (make-array #.block-stack-min-size)) BLOCK-STACK-PTR
   ;;This stack is pushed and popped in accordance with the way blocks are 
   ;;nested at the moment they are entered into the queue.  It contains the 
   ;;following block specific value.
   ;;SECTION-START total position where the section (see AIM-1102)
   ;;that is rightmost in the queue started.
  (BUFFER (make-array #.buffer-min-size :element-type
		      #-symbolics 'string-char #+symbolics 'character))
   CHARPOS BUFFER-PTR BUFFER-OFFSET
   ;;This is a vector of characters (eg a string) that builds up the
   ;;line images that will be printed out.  BUFFER-PTR is the
   ;;buffer position where the next character should be inserted in
   ;;the string.  CHARPOS is the output character position of the
   ;;first character in the buffer (non-zero only if a partial line
   ;;has been output).  BUFFER-OFFSET is used in computing total lengths.
   ;;It is changed to reflect all shifting and insertion of prefixes so that
   ;;total length computes things as they would be if they were 
   ;;all on one line.  Positions are kept three different ways
   ;; Buffer position (eg BUFFER-PTR)
   ;; Line position (eg (+ BUFFER-PTR CHARPOS)).  Indentations are stored in this form.
   ;; Total position if all on one line (eg (+ BUFFER-PTR BUFFER-OFFSET))
   ;;  Positions are stored in this form.
  (QUEUE (make-array #.queue-min-size)) QLEFT QRIGHT
   ;;This holds a queue of action descriptors.  QLEFT and QRIGHT
   ;;point to the next entry to dequeue and the last entry enqueued
   ;;respectively.  The queue is empty when
   ;;(> QLEFT QRIGHT).  The queue entries have several parts:
   ;;QTYPE one of :NEWLINE/:IND/:START-BLOCK/:END-BLOCK
   ;;QKIND :LINEAR/:MISER/:FILL/:MANDATORY or :UNCONDITIONAL/:FRESH
   ;; or :BLOCK/:CURRENT
   ;;QPOS total position corresponding to this entry
   ;;QDEPTH depth in blocks of this entry.
   ;;QEND offset to entry marking end of section this entry starts. (NIL until known.)
   ;; Only :start-block and non-literal :newline entries can start sections.
   ;;QOFFSET offset to :END-BLOCK for :START-BLOCK (NIL until known).
   ;;QARG for :IND indentation delta
   ;;     for :START-BLOCK suffix in the block if any.
   ;;                      or if per-line-prefix then cons of suffix and
   ;;                      per-line-prefix.
   ;;     for :END-BLOCK suffix for the block if any.
  (PREFIX (make-array #.buffer-min-size :element-type
		      #-symbolics 'string-char #+symbolics 'character))
   ;;this stores the prefix that should be used at the start of the line
  (PREFIX-STACK (make-array #.prefix-stack-min-size)) PREFIX-STACK-PTR
   ;;This stack is pushed and popped in accordance with the way blocks 
   ;;are nested at the moment things are taken off the queue and printed.
   ;;It contains the following block specific values.
   ;;PREFIX-PTR current length of PREFIX.
   ;;SUFFIX-PTR current length of pending suffix
   ;;NON-BLANK-PREFIX-PTR current length of non-blank prefix.
   ;;INITIAL-PREFIX-PTR prefix-ptr at the start of this block.
   ;;SECTION-START-LINE line-no value at last non-literal break at this level.
  (SUFFIX (make-array #.buffer-min-size :element-type
		      #-symbolics 'string-char #+symbolics 'character)))
   ;;this stores the suffixes that have to be printed to close of the current
   ;;open blocks.  For convenient in popping, the whole suffix
   ;;is stored in reverse order.
 

(defmacro LP<-BP (xp &optional (ptr nil))
  (if (null ptr) (setq ptr `(buffer-ptr ,xp)))
  `(+ ,ptr (charpos ,xp)))
(defmacro TP<-BP (xp)
  `(+ (buffer-ptr ,xp) (buffer-offset ,xp)))
(defmacro BP<-LP (xp ptr)
  `(- ,ptr (charpos ,xp)))
(defmacro BP<-TP (xp ptr)
  `(- ,ptr (buffer-offset ,xp)))
;This does not tell you the line position you were at when the TP
;was set, unless there have been no newlines or indentation output 
;between ptr and the current output point.
(defmacro LP<-TP (xp ptr)
  `(LP<-BP ,xp (BP<-TP ,xp ,ptr)))

;We don't use adjustable vectors or any of that, because we seldom have
;to actually extend and non-adjustable vectors are a lot faster in
;many Common Lisps.

(defmacro check-size (xp vect ptr)
  (let* ((min-size
	   (symbol-value
	     (intern (concatenate 'string (string vect) "-MIN-SIZE")
		     (find-package "XP"))))
	 (entry-size
	   (symbol-value
	     (intern (concatenate 'string (string vect) "-ENTRY-SIZE")
		     (find-package "XP")))))
    `(when (and (> ,ptr ,(- min-size entry-size)) ;seldom happens
		(> ,ptr (- (length (,vect ,xp)) ,entry-size)))
       (let* ((old (,vect ,xp))
	      (new (make-array (+ ,ptr ,(if (= entry-size 1) 50
					    (* 10 entry-size)))
			       :element-type (array-element-type old))))
	 (replace new old)
	 (setf (,vect ,xp) new)))))

(defmacro section-start (xp) `(aref (block-stack ,xp) (block-stack-ptr ,xp)))

(defun push-block-stack (xp)
  (incf (block-stack-ptr xp) #.block-stack-entry-size)
  (check-size xp block-stack (block-stack-ptr xp)))

(defun pop-block-stack (xp)
  (decf (block-stack-ptr xp) #.block-stack-entry-size))

(defmacro prefix-ptr (xp)
  `(aref (prefix-stack ,xp) (prefix-stack-ptr ,xp)))
(defmacro suffix-ptr (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 1)))
(defmacro non-blank-prefix-ptr (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 2)))
(defmacro initial-prefix-ptr (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 3)))
(defmacro section-start-line (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 4)))

(defun push-prefix-stack (xp)
  (let ((old-prefix 0) (old-suffix 0) (old-non-blank 0))
    (when (not (minusp (prefix-stack-ptr xp)))
      (setq old-prefix (prefix-ptr xp)
	    old-suffix (suffix-ptr xp)
	    old-non-blank (non-blank-prefix-ptr xp)))
    (incf (prefix-stack-ptr xp) #.prefix-stack-entry-size)
    (check-size xp prefix-stack (prefix-stack-ptr xp))
    (setf (prefix-ptr xp) old-prefix)
    (setf (suffix-ptr xp) old-suffix)
    (setf (non-blank-prefix-ptr xp) old-non-blank)))

(defun pop-prefix-stack (xp)
  (decf (prefix-stack-ptr xp) #.prefix-stack-entry-size))

(defmacro Qtype   (xp index) `(aref (queue ,xp) ,index))
(defmacro Qkind   (xp index) `(aref (queue ,xp) (1+ ,index)))
(defmacro Qpos    (xp index) `(aref (queue ,xp) (+ ,index 2)))
(defmacro Qdepth  (xp index) `(aref (queue ,xp) (+ ,index 3)))
(defmacro Qend    (xp index) `(aref (queue ,xp) (+ ,index 4)))
(defmacro Qoffset (xp index) `(aref (queue ,xp) (+ ,index 5)))
(defmacro Qarg    (xp index) `(aref (queue ,xp) (+ ,index 6)))

;we shift the queue over rather than using a circular queue because
;that works out to be a lot faster in practice.  Note, short printout
;does not ever cause a shift, and even in long printout, the queue is
;shifted left for free every time it happens to empty out.

(defun enqueue (xp type kind &optional arg)
  (incf (Qright xp) #.queue-entry-size)
  (when (> (Qright xp) #.(- queue-min-size queue-entry-size))
    (replace (queue xp) (queue xp) :start2 (Qleft xp) :end2 (Qright xp))
    (setf (Qright xp) (- (Qright xp) (Qleft xp)))
    (setf (Qleft xp) 0))
  (check-size xp queue (Qright xp))
  (setf (Qtype xp (Qright xp)) type)
  (setf (Qkind xp (Qright xp)) kind)
  (setf (Qpos xp (Qright xp)) (TP<-BP xp))
  (setf (Qdepth xp (Qright xp)) (depth-in-blocks xp))
  (setf (Qend xp (Qright xp)) nil)
  (setf (Qoffset xp (Qright xp)) nil)
  (setf (Qarg xp (Qright xp)) arg))

(defmacro Qnext (index) `(+ ,index #.queue-entry-size))

(defvar *describe-xp-streams-fully* nil "Set to T to see more info.")

(defun describe-xp (xp s depth) 
    (declare (ignore depth))
  (lisp:format s "#<XP stream ")
  (if (not (base-stream xp))
      (lisp:format s "not currently in use")
      (lisp:format s "outputting to ~S" (base-stream xp)))
  (when (base-stream xp)
    (lisp:format s "~&buffer= ~S" (subseq (buffer xp) 0 (max (buffer-ptr xp) 0)))
    (when (not *describe-xp-streams-fully*) (lisp:princ " ..." s))
    (when *describe-xp-streams-fully*
      (lisp:format s "~&   pos   _123456789_123456789_123456789_123456789")
      (lisp:format s "~&depth-in-blocks= ~D linel= ~D line-no= ~D line-limit= ~D"
		   (depth-in-blocks xp) (linel xp) (line-no xp) (line-limit xp))
      (when (or (char-mode xp) (not (zerop (char-mode-counter xp))))
	(lisp:format s "~&char-mode= ~S char-mode-counter= ~D"
		     (char-mode xp) (char-mode-counter xp)))
      (unless (minusp (block-stack-ptr xp))
	(lisp:format s "~&section-start")
	(do ((save (block-stack-ptr xp)))
	    ((minusp (block-stack-ptr xp)) (setf (block-stack-ptr xp) save))
	  (lisp:format s " ~D" (section-start xp))
	  (pop-block-stack xp)))
      (lisp:format s "~&linel= ~D charpos= ~D buffer-ptr= ~D buffer-offset= ~D"
		   (linel xp) (charpos xp) (buffer-ptr xp) (buffer-offset xp))
      (unless (minusp (prefix-stack-ptr xp))
	(lisp:format s "~&prefix= ~S"
		     (subseq (prefix xp) 0 (max (prefix-ptr xp) 0)))
	(lisp:format s "~&suffix= ~S"
		     (subseq (suffix xp) 0 (max (suffix-ptr xp) 0))))
      (unless (> (Qleft xp) (Qright xp))
	(lisp:format s "~&ptr type         kind           pos depth end offset arg")
	(do ((p (Qleft xp) (Qnext p))) ((> p (Qright xp)))
	  (lisp:format s "~&~4A~13A~15A~4A~6A~4A~7A~A"
	    (/ (- p (Qleft xp)) #.queue-entry-size)
	    (Qtype xp p)
	    (if (member (Qtype xp p) '(:newline :ind)) (Qkind xp p) "")
	    (BP<-TP xp (Qpos xp p))
	    (Qdepth xp p)
	    (if (not (member (Qtype xp p) '(:newline :start-block))) ""
		(and (Qend xp p)
		     (/ (- (+ p (Qend xp p)) (Qleft xp)) #.queue-entry-size)))
	    (if (not (eq (Qtype xp p) :start-block)) ""
		(and (Qoffset xp p)
		     (/ (- (+ p (Qoffset xp p)) (Qleft xp)) #.queue-entry-size)))
	    (if (not (member (Qtype xp p) '(:ind :start-block :end-block))) ""
		(Qarg xp p)))))
      (unless (minusp (prefix-stack-ptr xp))
	(lisp:format s "~&initial-prefix-ptr prefix-ptr suffix-ptr non-blank start-line")
	(do ((save (prefix-stack-ptr xp)))
	    ((minusp (prefix-stack-ptr xp)) (setf (prefix-stack-ptr xp) save))
	  (lisp:format s "~& ~19A~11A~11A~10A~A"
		       (initial-prefix-ptr xp) (prefix-ptr xp) (suffix-ptr xp)
		       (non-blank-prefix-ptr xp) (section-start-line xp))
	  (pop-prefix-stack xp)))))
    (lisp:princ ">" s)
    (values))

;This maintains a list of XP structures.  We save them
;so that we don't have to create new ones all of the time.
;We have separate objects so that many can be in use at once.

;(Note should really be doing some locking here, but CL does not have the
;primitives for it.  There is a tiny probability here that two different
;processes could end up trying to use the same xp-stream)

(defvar *free-xps* nil "free list of XP stream objects") ; never bound

(defun get-pretty-print-stream (stream)
  (let ((xp (pop *free-xps*)))
    (initialize-xp (if xp xp (make-xp-structure)) stream)))

;If you call this, the xp-stream gets efficiently recycled.

(defun free-pretty-print-stream (xp)
  (setf (base-stream xp) nil)
  (pushnew xp *free-xps*))

;This is called to initialize things when you start pretty printing.

(defun initialize-xp (xp stream)
  (setf (base-stream xp) stream)
  (setf (linel xp) (max 0 (cond (*print-right-margin*)
				((output-width stream))
				(T *default-right-margin*))))
  (setf (line-limit xp) *print-lines*)
  (setf (line-no xp) 1)
  (setf (char-mode xp) nil)
  (setf (char-mode-counter xp) 0)
  (setf (depth-in-blocks xp) 0)
  (setf (block-stack-ptr xp) 0)
  (setf (charpos xp) (cond ((output-position stream)) (T 0)))
  (setf (section-start xp) 0)
  (setf (buffer-ptr xp) 0)
  (setf (buffer-offset xp) (charpos xp))
  (setf (Qleft xp) 0)
  (setf (Qright xp) #.(- queue-entry-size))
  (setf (prefix-stack-ptr xp) #.(- prefix-stack-entry-size))
  xp)

;The char-mode stuff is a bit tricky.
;one can be in one of the following modes:
;NIL no changes to characters output.
;:UP CHAR-UPCASE used.
;:DOWN CHAR-DOWNCASE used.
;:CAP0 capitalize next alphanumeric letter then switch to :DOWN.
;:CAP1 capitalize next alphanumeric letter then switch to :CAPW
;:CAPW downcase letters.  When a word break letter found, switch to :CAP1.
;It is possible for ~(~) to be nested in a format string, but note that
;each mode specifies what should happen to every letter.  Therefore, inner
;nested modes never have any effect.  You can just ignore them.

(defun push-char-mode (xp new-mode)
  (if (zerop (char-mode-counter xp))
      (setf (char-mode xp) new-mode))
  (incf (char-mode-counter xp)))

(defun pop-char-mode (xp)
  (decf (char-mode-counter xp))
  (if (zerop (char-mode-counter xp))
      (setf (char-mode xp) nil)))

;Assumes is only called when char-mode is non-nil
(defun handle-char-mode (xp char)
  (case (char-mode xp)
    (:CAP0 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :DOWN) (char-upcase char))))
    (:CAP1 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :CAPW) (char-upcase char))))
    (:CAPW (cond ((alphanumericp char) (char-downcase char))
		 (T (setf (char-mode xp) :CAP1) char)))
    (:UP (char-upcase char))
    (T (char-downcase char)))) ;:DOWN

;All characters output are passed through the handler above.  However, it must
;be noted that on-each-line prefixes are only processed in the context of the
;first place they appear.  They stay the same later no matter what.  Also
;non-literal newlines do not count as word breaks.


;This handles the basic outputting of characters.  note + suffix means that
;the stream is known to be an XP stream, all inputs are mandatory, and no
;error checking has to be done.  Suffix ++ additionally means that the
;output is guaranteed not to contain a newline char.

(defun write-char+ (char xp)
  (if (eql char #\newline) (pprint-newline+ :unconditional xp)
      (write-char++ char xp)))

(defun write-string+ (string xp start end)
  (let ((sub-end nil) next-newline)
    (loop (setq next-newline
		(position #\newline string :test #'char= :start start :end end))
	  (setq sub-end (if next-newline next-newline end))
	  (write-string++ string xp start sub-end)
	  (when (null next-newline) (return nil))
	  (pprint-newline+ :unconditional xp)
	  (setq start (1+ sub-end)))))

;note this checks (> BUFFER-PTR LINEL) instead of (> (LP<-BP) LINEL)
;this is important so that when things are longer than a line they
;end up getting printed in chunks of size LINEL.

(defun write-char++ (char xp)
  (when (> (buffer-ptr xp) (linel xp))
    (force-some-output xp))
  (let ((new-buffer-end (1+ (buffer-ptr xp))))
    (check-size xp buffer new-buffer-end)
    (if (char-mode xp) (setq char (handle-char-mode xp char)))
    (setf (char (buffer xp) (buffer-ptr xp)) char)    
    (setf (buffer-ptr xp) new-buffer-end)))

(defun force-some-output (xp)
  (attempt-to-output xp nil nil)
  (when (> (buffer-ptr xp) (linel xp)) ;only if printing off end of line
    (attempt-to-output xp T T)))

(defun write-string++ (string xp start end)
  (when (> (buffer-ptr xp) (linel xp))
    (force-some-output xp))
  (write-string+++ string xp start end))

;never forces output; therefore safe to call from within output-line.

(defun write-string+++ (string xp start end) 
  (let ((new-buffer-end (+ (buffer-ptr xp) (- end start))))
    (check-size xp buffer new-buffer-end)
    (do ((buffer (buffer xp))
	 (i (buffer-ptr xp) (1+ i))
	 (j start (1+ j)))
	((= j end))
      (let ((char (char string j)))
	(if (char-mode xp) (setq char (handle-char-mode xp char)))
	(setf (char buffer i) char)))
    (setf (buffer-ptr xp) new-buffer-end)))

(defun pprint-tab+ (kind colnum colinc xp)
  (let ((indented? nil) (relative? nil))
    (case kind
      (:section (setq indented? T))
      (:line-relative (setq relative? T))
      (:section-relative (setq indented? T relative? T)))
    (let* ((current
	     (if (not indented?) (LP<-BP xp)
		 (- (TP<-BP xp) (section-start xp))))
	   (new
	     (if (zerop colinc)
		 (if relative? (+ current colnum) (max colnum current))
		 (cond (relative?
			(* colinc (floor (+ current colnum colinc -1) colinc)))
		       ((> colnum current) colnum)
		       (T (+ colnum
			     (* colinc
				(floor (+ current (- colnum) colinc) colinc)))))))
	   (length (- new current)))
      (when (plusp length)
	(if (char-mode xp) (handle-char-mode xp #\space))
	(let ((end (+ (buffer-ptr xp) length)))
	  (check-size xp buffer end)
	  (fill (buffer xp) #\space :start (buffer-ptr xp) :end end)
	  (setf (buffer-ptr xp) end))))))

;note following is smallest number >= x that is a multiple of colinc
;  (* colinc (floor (+ x (1- colinc)) colinc))

(defun pprint-newline+ (kind xp)
  (enqueue xp :newline kind)
  (do ((ptr (Qleft xp) (Qnext ptr)))    ;find sections we are ending
      ((not (< ptr (Qright xp))))	;all but last
    (when (and (null (Qend xp ptr))
	       (not (> (depth-in-blocks xp) (Qdepth xp ptr)))
	       (member (Qtype xp ptr) '(:newline :start-block)))
      (setf (Qend xp ptr) (- (Qright xp) ptr))))
  (setf (section-start xp) (TP<-BP xp))
  (when (and (member kind '(:fresh :unconditional)) (char-mode xp))
    (handle-char-mode xp #\newline))
  (when (member kind '(:fresh :unconditional :mandatory))
    (attempt-to-output xp T nil)))

(defun start-block (xp prefix-string on-each-line? suffix-string)
  (when prefix-string (write-string++ prefix-string xp 0 (length prefix-string)))
  (if (and (char-mode xp) on-each-line?)
      (setq prefix-string
	    (subseq (buffer xp) (- (buffer-ptr xp) (length prefix-string))
		    (buffer-ptr xp))))
  (push-block-stack xp)
  (enqueue xp :start-block nil
	   (if on-each-line? (cons suffix-string prefix-string) suffix-string))
  (incf (depth-in-blocks xp))	      ;must be after enqueue
  (setf (section-start xp) (TP<-BP xp)))

(defun end-block (xp suffix)
  (unless (eq *abbreviation-happened* '*print-lines*)
    (when suffix (write-string+ suffix xp 0 (length suffix)))
    (decf (depth-in-blocks xp))
    (enqueue xp :end-block nil suffix)
    (do ((ptr (Qleft xp) (Qnext ptr))) ;looking for start of block we are ending
	((not (< ptr (Qright xp))))    ;all but last
      (when (and (= (depth-in-blocks xp) (Qdepth xp ptr))
		 (eq (Qtype xp ptr) :start-block)
		 (null (Qoffset xp ptr)))
	(setf (Qoffset xp ptr) (- (Qright xp) ptr))
	(return nil)))	;can only be 1
    (pop-block-stack xp)))

(defun pprint-indent+ (kind n xp)
  (enqueue xp :ind kind n))

; The next function scans the queue looking for things it can do.
;it keeps outputting things until the queue is empty, or it finds
;a place where it cannot make a decision yet.

(defmacro maybe-too-large (xp Qentry)
  `(let ((limit (linel ,xp)))
     (when (eql (line-limit ,xp) (line-no ,xp)) ;prevents suffix overflow
       (decf limit 2) ;3 for " .." minus 1 for space (heuristic)
       (when (not (minusp (prefix-stack-ptr ,xp)))
	 (decf limit (suffix-ptr ,xp))))
     (cond ((Qend ,xp ,Qentry)
	    (> (LP<-TP ,xp (Qpos ,xp (+ ,Qentry (Qend ,xp ,Qentry)))) limit))
	   ((or force-newlines? (> (LP<-BP ,xp) limit)) T)
	   (T (return nil)))))	;wait until later to decide.

(defmacro misering? (xp)
  `(and *print-miser-width*
	(<= (- (linel ,xp) (initial-prefix-ptr ,xp)) *print-miser-width*)))

;If flush-out? is T and force-newlines? is NIL then the buffer,
;prefix-stack, and queue will be in an inconsistent state after the call.
;You better not call it this way except as the last act of outputting.

(defun attempt-to-output (xp force-newlines? flush-out?)
  (do () ((> (Qleft xp) (Qright xp))
	  (setf (Qleft xp) 0)
	  (setf (Qright xp) #.(- queue-entry-size))) ;saves shifting
    (case (Qtype xp (Qleft xp))
      (:ind
       (unless (misering? xp)
	 (set-indentation-prefix xp
	   (case (Qkind xp (Qleft xp))
	     (:block (+ (initial-prefix-ptr xp) (Qarg xp (Qleft xp))))
	     (T ; :current
	       (+ (LP<-TP xp (Qpos xp (Qleft xp)))
		  (Qarg xp (Qleft xp)))))))
       (setf (Qleft xp) (Qnext (Qleft xp))))
      (:start-block
       (cond ((maybe-too-large xp (Qleft xp))
	      (push-prefix-stack xp)
	      (setf (initial-prefix-ptr xp) (prefix-ptr xp))
	      (set-indentation-prefix xp (LP<-TP xp (Qpos xp (Qleft xp))))
	      (let ((arg (Qarg xp (Qleft xp))))
		(when (consp arg) (set-prefix xp (cdr arg)))
		(setf (initial-prefix-ptr xp) (prefix-ptr xp))
		(cond ((not (listp arg)) (set-suffix xp arg))
		      ((car arg) (set-suffix xp (car arg)))))
	      (setf (section-start-line xp) (line-no xp)))
	     (T (incf (Qleft xp) (Qoffset xp (Qleft xp)))))
       (setf (Qleft xp) (Qnext (Qleft xp))))
      (:end-block (pop-prefix-stack xp) (setf (Qleft xp) (Qnext (Qleft xp))))
      (T ; :newline
       (when (case (Qkind xp (Qleft xp))
	       (:fresh (not (zerop (LP<-BP xp))))
	       (:miser (misering? xp))
	       (:fill (or (misering? xp)
			  (> (line-no xp) (section-start-line xp))
			  (maybe-too-large xp (Qleft xp))))
	       (T T)) ;(:linear :unconditional :mandatory) 
	 (output-line xp (Qleft xp))
	 (setup-for-next-line xp (Qleft xp)))
       (setf (Qleft xp) (Qnext (Qleft xp))))))
  (when flush-out? (flush xp)))

;this can only be called last!

(defun flush (xp)
  (unless *locating-circularities*
    (lisp:write-string
       (buffer xp) (base-stream xp) :end (buffer-ptr xp)))
  (incf (buffer-offset xp) (buffer-ptr xp))
  (incf (charpos xp) (buffer-ptr xp))
  (setf (buffer-ptr xp) 0))

;This prints out a line of stuff.

(defun output-line (xp Qentry)
  (let* ((out-point (BP<-TP xp (Qpos xp Qentry)))
	 (last-non-blank (position #\space (buffer xp) :test-not #'char=
				   :from-end T :end out-point))
	 (end (cond ((member (Qkind xp Qentry) '(:fresh :unconditional)) out-point)
		    (last-non-blank (1+ last-non-blank))
		    (T 0)))
	 (line-limit-exit (and (line-limit xp) (not (> (line-limit xp) (line-no xp))))))
    (when line-limit-exit
      (setf (buffer-ptr xp) end)          ;truncate pending output.
      (write-string+++ " .." xp 0 3)
      (reverse-string-in-place (suffix xp) 0 (suffix-ptr xp))
      (write-string+++ (suffix xp) xp 0 (suffix-ptr xp))
      (setf (Qleft xp) (Qnext (Qright xp)))
      (setq *abbreviation-happened* '*print-lines*)
      (throw 'line-limit-abbreviation-exit T))
    (incf (line-no xp))
    (unless *locating-circularities*
      (lisp:write-line
          (buffer xp) (base-stream xp) :end end))))

(defun setup-for-next-line (xp Qentry)
  (let* ((out-point (BP<-TP xp (Qpos xp Qentry)))
	 (prefix-end
	   (cond ((member (Qkind xp Qentry) '(:unconditional :fresh))
		  (non-blank-prefix-ptr xp))
		 (T (prefix-ptr xp))))
	 (change (- prefix-end out-point)))
    (setf (charpos xp) 0)
    (when (plusp change)                  ;almost never happens
      (check-size xp buffer (+ (buffer-ptr xp) change)))
    (replace (buffer xp) (buffer xp) :start1 prefix-end
	     :start2 out-point :end2 (buffer-ptr xp))
    (replace (buffer xp) (prefix xp) :end2 prefix-end)
    (incf (buffer-ptr xp) change)
    (decf (buffer-offset xp) change)
    (when (not (member (Qkind xp Qentry) '(:unconditional :fresh)))
      (setf (section-start-line xp) (line-no xp)))))

(defun set-indentation-prefix (xp new-position)
  (let ((new-ind (max (non-blank-prefix-ptr xp) new-position)))
    (setf (prefix-ptr xp) (initial-prefix-ptr xp))
    (check-size xp prefix new-ind)
    (when (> new-ind (prefix-ptr xp))
      (fill (prefix xp) #\space :start (prefix-ptr xp) :end new-ind))
    (setf (prefix-ptr xp) new-ind)))

(defun set-prefix (xp prefix-string)
  (replace (prefix xp) prefix-string
	   :start1 (- (prefix-ptr xp) (length prefix-string)))
  (setf (non-blank-prefix-ptr xp) (prefix-ptr xp)))

(defun set-suffix (xp suffix-string)
  (let* ((end (length suffix-string))
	 (new-end (+ (suffix-ptr xp) end)))
    (check-size xp suffix new-end)
    (do ((i (1- new-end) (1- i)) (j 0 (1+ j))) ((= j end))
      (setf (char (suffix xp) i) (char suffix-string j)))
    (setf (suffix-ptr xp) new-end)))

(defun reverse-string-in-place (string start end)
  (do ((i start (1+ i)) (j (1- end) (1- j))) ((not (< i j)) string)
    (let ((c (char string i)))
      (setf (char string i) (char string j))
      (setf (char string j) c))))

;		   ---- BASIC INTERFACE FUNCTIONS ----

;The internal functions in this file, and the (formatter "...") expansions
;use the '+' forms of these functions directly (which is faster) because,
;they do not need error checking of fancy stream coercion.  The '++' forms
;additionally assume the thing being output does not contain a newline.

(defun write (object &rest pairs &key (stream *standard-output*)
	      (escape *print-escape*) (radix *print-radix*)
	      (base *print-base*) (circle *print-circle*)
	      (pretty *print-pretty*) (level *print-level*)
	      (length *print-length*) (case *print-case*)
	      (gensym *print-gensym*) (array *print-array*)
	      (pprint-dispatch *print-pprint-dispatch*)
	      (right-margin *print-right-margin*)
	      (lines *print-lines*) (miser-width *print-miser-width*))
  (setq stream (decode-stream-arg stream))
  (let ((*print-pprint-dispatch* pprint-dispatch) (*print-right-margin* right-margin)
	(*print-lines* lines) (*print-miser-width* miser-width))
    (cond ((or (xp-structure-p stream) pretty)
	   (let ((*print-escape* escape) (*print-radix* radix)
		 (*print-base* base) (*print-circle* circle)
		 (*print-pretty* pretty) (*print-level* level)
		 (*print-length* length) (*print-case* case)
		 (*print-gensym* gensym) (*print-array* array))
	     (basic-write object stream)))
	  (T (remf pairs :dispatch) (remf pairs :right-margin)
	     (remf pairs :lines) (remf pairs :miser-width)
	     (apply #'lisp:write object pairs))))
  object)

(defun basic-write (object stream)
  (cond ((xp-structure-p stream) (write+ object stream))
	(*print-pretty* (maybe-initiate-xp-printing
			  #'(lambda (s o) (write+ o s)) stream object))
	(T (lisp:write object :stream stream))))

(defun maybe-initiate-xp-printing (fn stream &rest args)
  (if (xp-structure-p stream) (apply fn stream args)
      (let ((*abbreviation-happened* nil)
	    (*locating-circularities* (if *print-circle* 0 nil))
	    (*circularity-hash-table*
	      (if *print-circle* (get-circularity-hash-table) nil))
	    (*parents* (when (not *print-shared*) (list nil)))
	    (*result* nil))
	(xp-print fn (decode-stream-arg stream) args)
	(if *circularity-hash-table*
	    (free-circularity-hash-table *circularity-hash-table*))
	(when *abbreviation-happened* 
	  (setq *last-abbreviated-printing*
		(eval
		  `(function
		     (lambda (&optional (stream ',stream))
		       (let ((*package* ',*package*))
			 (apply #'maybe-initiate-xp-printing
				',fn stream
				',(copy-list args))))))))
	*result*)))

#-symbolics
(defun xp-print (fn stream args)
  (setq *result* (do-xp-printing fn stream args))
  (when *locating-circularities*
    (setq *locating-circularities* nil)
    (setq *abbreviation-happened* nil)
    (setq *parents* nil)
    (setq *result* (do-xp-printing fn stream args))))

(defun decode-stream-arg (stream)
  (cond ((eq stream T) *terminal-io*)
	((null stream) *standard-output*)
	(T stream)))

(defun do-xp-printing (fn stream args)
  (let ((xp (get-pretty-print-stream stream))
	(*current-level* 0)
	(result nil))
    (catch 'line-limit-abbreviation-exit
      (start-block xp nil nil nil)
      (setq result (apply fn xp args))
      (end-block xp nil))
    (when (and *locating-circularities*
	       (zerop *locating-circularities*)	;No circularities.
	       (= (line-no xp) 1)	     	;Didn't suppress line.
	       (zerop (buffer-offset xp)))	;Didn't suppress partial line.
      (setq *locating-circularities* nil))	;print what you have got.
    (when (catch 'line-limit-abbreviation-exit
	    (attempt-to-output xp nil T) nil)
      (attempt-to-output xp T T))
    (free-pretty-print-stream xp)
    result))

(defun write+ (object xp)
  (let ((*parents* *parents*))
    (unless (and *circularity-hash-table*
		(eq (circularity-process xp object nil) :subsequent))
      (when (and *circularity-hash-table* (consp object))
	;;avoid possible double check in handle-logical-block.
	(setq object (cons (car object) (cdr object))))
      (let ((printer (if *print-pretty* (get-printer object *print-pprint-dispatch*) nil))
	    type)
	(cond (printer (funcall printer xp object))
	      ((maybe-print-fast xp object))
	      ((and *print-pretty*
		    (symbolp (setq type (type-of object)))
		    (setq printer (get type 'structure-printer))
		    (not (eq printer :none)))
	       (funcall printer xp object))
	      ((and *print-pretty* *print-array* (arrayp object)
		    (not (stringp object)) (not (bit-vector-p object))
		    (not (structure-type-p (type-of object))))
	       (pretty-array xp object))
	      (T (let ((stuff
			 (with-output-to-string (s)
			   (non-pretty-print object s))))
		   (write-string+ stuff xp 0 (length stuff)))))))))

(defun non-pretty-print (object s)
  (lisp:write object
	      :level (if *print-level*
			 (- *print-level* *current-level*))
	      :pretty nil
	      :stream s))

;It is vital that this function be called EXACTLY once for each occurrence of 
;  each thing in something being printed.
;Returns nil if printing should just continue on.
;  Either it is not a duplicate, or we are in the first pass and do not know.
;returns :FIRST if object is first occurrence of a DUPLICATE.
;  (This can only be returned on a second pass.)
;  After an initial code (printed by this routine on the second pass)
;  printing should continue on for the object.
;returns :SUBSEQUENT if second or later occurrence.
;  Printing is all taken care of by this routine.

;Note many (maybe most) lisp implementations have characters and small numbers
;represented in a single word so that the are always eq when they are equal and the
;reader takes care of properly sharing them (just as it does with symbols).
;Therefore, we do not want circularity processing applied to them.  However,
;some kinds of numbers (e.g., bignums) undoubtedly are complex structures that
;the reader does not share.  However, they cannot have circular pointers in them
;and it is therefore probably a waste to do circularity checking on them.  In
;any case, it is not clear that it easy to tell exactly what kinds of numbers a
;given implementation of CL is going to have the reader automatically share.

(defun circularity-process (xp object interior-cdr?)
  (unless (or (numberp object)
	      (characterp object)
	      (and (symbolp object)	;Reader takes care of sharing.
		   (or (null *print-gensym*) (symbol-package object)))) 
    (let ((id (gethash object *circularity-hash-table*)))
      (if *locating-circularities*
	  (cond ((null id)	;never seen before
		 (when *parents* (push object *parents*))
		 (setf (gethash object *circularity-hash-table*) 0)
		 nil)
		((zerop id) ;possible second occurrence
		 (cond ((or (null *parents*) (member object *parents*))
			(setf (gethash object *circularity-hash-table*)
			      (incf *locating-circularities*))
			:subsequent)
		       (T nil)))
		(T :subsequent));third or later occurrence
	  (cond ((or (null id)	;never seen before (note ~@* etc. conses)
		     (zerop id));no duplicates
		 nil)
		((plusp id)
		 (cond (interior-cdr?
			(decf *current-level*)
			(write-string++ ". #" xp 0 3))
		       (T (write-char++ #\# xp)))
		 (print-fixnum xp id)
		 (write-char++ #\= xp)
		 (setf (gethash object *circularity-hash-table*) (- id))
		 :first)
		(T (if interior-cdr? (write-string++ ". #" xp 0 3)
		       (write-char++ #\# xp))
		   (print-fixnum xp (- id))
		   (write-char++ #\# xp)
		   :subsequent))))))

;This prints a few very common, simple atoms very fast.
;Pragmatically, this turns out to be an enormous savings over going to the
;standard printer all the time.  There would be diminishing returns from making
;this work with more things, but might be worth it.

(defun maybe-print-fast (xp object)
  (cond ((stringp object)
	 (cond ((null *print-escape*) (write-string+ object xp 0 (length object)) T)
	       ((every #'(lambda (c) (not (or (char= c #\") (char= c #\\))))
		       object)
		(write-char++ #\" xp)
		(write-string+ object xp 0 (length object))
		(write-char++ #\" xp) T)))
	((typep object 'fixnum)
	 (when (and (null *print-radix*) (= *print-base* 10.))
	   (when (minusp object)
	     (write-char++ #\- xp)
	     (setq object (- object)))
	   (print-fixnum xp object) T))
	((symbolp object)
	 (let ((s (symbol-name object))
	       (p (symbol-package object))
	       (is-key (keywordp object))
	       (mode (case *print-case*
		       (:downcase :down)
		       (:capitalize :cap1)
		       (T nil)))) ;note no-escapes-needed requires all caps
	   (cond ((and (or is-key (eq p *package*)
			   (and *package* ;can be NIL on symbolics
				(eq object (find-symbol s))))
		       (no-escapes-needed s))
		  (when (and is-key *print-escape*)
		    (write-char++ #\: xp))
		  (if mode (push-char-mode xp mode))
		  (write-string++ s xp 0 (length s))
		  (if mode (pop-char-mode xp)) T))))))

(defun print-fixnum (xp fixnum)
  (multiple-value-bind (digits d)
      (truncate fixnum 10)
    (unless (zerop digits)
      (print-fixnum xp digits))
    (write-char++ (code-char (+ #.(char-code #\0) d)) xp)))

;just wants to succeed fast in a lot of common cases.
;assumes no funny readtable junk for the characters shown.

(defun no-escapes-needed (s)
  (let ((n (length s)))
    (and (not (zerop n))
	 (let ((c (schar s 0)))
	   (or (and (alpha-char-p c) (upper-case-p c)) (find c "*<>")))
	 (do ((i 1 (1+ i))) ((= i n) T)
	   (let ((c (schar s i)))
	     (if (not (or (digit-char-p c)
			  (and (alpha-char-p c) (upper-case-p c))
			  (find c "*+<>-")))
		 (return nil)))))))


(defun print (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (terpri stream)
  (let ((*print-escape* T))
    (basic-write object stream))
  (write-char #\space stream)
  object)

(defun prin1 (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (let ((*print-escape* T))
    (basic-write object stream))
  object)

(defun princ (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (let ((*print-escape* nil))
    (basic-write object stream))
  object)

(defun pprint (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (terpri stream)
  (let ((*print-escape* T) (*print-pretty* T))
    (basic-write object stream))
  (values))

(defun write-to-string (object &rest pairs &key &allow-other-keys)
  (with-output-to-string (s)
    (apply #'write object :stream s pairs)))

(defun prin1-to-string (object)
  (with-output-to-string (stream)
    (let ((*print-escape* T))
      (basic-write object stream))))

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (let ((*print-escape* nil))
      (basic-write object stream))))

;Any format string that is converted to a function is always printed
;via an XP stream (See formatter).

(defun format (stream string-or-fn &rest args)
  (cond ((stringp stream)
	 (lisp:format stream "~A"
		      (with-output-to-string (stream)
			(apply #'format stream string-or-fn args)))
	 nil)
	((null stream)
	 (with-output-to-string (stream)
	   (apply #'format stream string-or-fn args)))
	(T (if (eq stream T) (setq stream *standard-output*))
	   (when (stringp string-or-fn)
	     (setq string-or-fn (process-format-string string-or-fn nil)))
	   (cond ((not (stringp string-or-fn))
		  (apply string-or-fn stream args))
		 ((xp-structure-p stream)
		  (apply #'using-format stream string-or-fn args))
		 (T (apply #'lisp:format stream string-or-fn args)))
	   nil)))

(defvar *format-string-cache* T)

(defun process-format-string (string-or-fn force-fn?)
  (cond ((not (stringp string-or-fn)) string-or-fn) ;called from ~? too.
	((not *format-string-cache*)
	 (maybe-compile-format-string string-or-fn force-fn?))
	(T (when (not (hash-table-p *format-string-cache*))
	     (setq *format-string-cache* (make-hash-table :test #'eq)))
	   (let ((value (gethash string-or-fn *format-string-cache*)))
	     (when (or (not value) (and force-fn? (stringp value)))
	       (setq value (maybe-compile-format-string string-or-fn force-fn?))
	       (setf (gethash string-or-fn *format-string-cache*) value))
	     value))))

(defun write-char (char &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (write-char+ char stream) 
      (lisp:write-char char stream))
  char)

(defun write-string (string &optional (stream *standard-output*)
		     &key (start 0) (end (length string)))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (write-string+ string stream start end)
      (lisp:write-string string stream :start start :end end))
  string)

(defun write-line (string &optional (stream *standard-output*)
		   &key (start 0) (end (length string)))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (progn (write-string+ string stream start end)
	     (pprint-newline+ :unconditional stream))
      (lisp:write-line string stream :start start :end end))
  string)

(defun terpri (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (pprint-newline+ :unconditional stream)
      (lisp:terpri stream))
  nil)

;This has to violate the XP data abstraction and fool with internal
;stuff, in order to find out the right info to return as the result.

(defun fresh-line (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (cond ((xp-structure-p stream)
	 (attempt-to-output stream T T) ;ok because we want newline
	 (when (not (zerop (LP<-BP stream)))
	   (pprint-newline+ :fresh stream)
	   T))
	(T (lisp:fresh-line stream))))

;Each of these causes the stream to be pessimistic and insert
;newlines wherever it might have to, when forcing the partial output
;out.  This is so that things will be in a consistent state if
;output continues to the stream later.

(defun finish-output (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (when (xp-structure-p stream)
    (attempt-to-output stream T T)
    (setq stream (base-stream stream)))
  (lisp:finish-output stream)
  nil)

(defun force-output (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (when (xp-structure-p stream)
    (attempt-to-output stream T T)
    (setq stream (base-stream stream)))
  (lisp:force-output stream)
  nil)

(defun clear-output (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (when (xp-structure-p stream)
    (let ((*locating-circularities* 0)) ;hack to prevent visible output
      (attempt-to-output stream T T)
      (setq stream (base-stream stream))))
  (lisp:clear-output stream)
  nil)

;note we are assuming that if a structure is defined using xp::defstruct,
;then its print-function (if any) will be defined using xp::print etc.

(defmacro defstruct (name &body body)
  (let* ((struct-name (if (consp name) (car name) name))
	 (printer (cadr (safe-assoc :print-function name)))
	 (xp-print-fn
	   (intern (concatenate 'string
		     "PRINT-" (string (package-name
					(symbol-package struct-name)))
		     ":" (string struct-name))
		   (find-package "XP"))))
    (cond (printer
	   `(eval-when (eval load compile)
	      (lisp:defstruct ,name ,@ body)
	      (defun ,xp-print-fn (xp obj)
		(funcall (function ,printer) obj xp *current-level*))
	      (setf (get ',struct-name 'structure-printer) #',xp-print-fn)
	      ',(if (consp name) (car name) name)))
	  ((and (not (safe-assoc :type name))
		(not (safe-assoc :include name)))
	   (let* ((conc-name-spec (safe-assoc :conc-name name))
		  (conc-name (cond ((null conc-name-spec)
				    (concatenate 'string (string struct-name) "-"))
				   ((null (cadr conc-name-spec)) "")
				   (T (string (cadr conc-name-spec)))))
		  (slots (mapcar #'(lambda (x) (if (consp x) (car x) x)) body)))
	     `(eval-when (eval load compile)
		(lisp:defstruct ,name ,@ body)
		(defun ,xp-print-fn (xp obj)
		  (funcall (formatter "~@<#S(~;~W ~:I~@_~@{:~A ~W~^ ~:_~}~;)~:>") xp
			   ',struct-name
			   ,@(mapcan #'(lambda (slot)
					 `(,(string slot)
					    (,(intern (concatenate 'string
					                 conc-name (string slot)))
					      obj)))
				     slots)))
		(setf (get ',struct-name 'structure-printer) #',xp-print-fn)
		',(if (consp name) (car name) name))))
	  (T `(eval-when (eval load compile)
		(setf (get ',struct-name 'structure-printer) :none)
		(lisp:defstruct ,name ,@ body))))))

(defun safe-assoc (item list)
  (do ((l list (cdr l))) ((not (consp l)) nil)
    (if (and (consp (car l)) (eq (caar l) item)) (return (car l)))))

;           ---- FUNCTIONAL INTERFACE TO DYNAMIC FORMATTING ----

;The internal functions in this file, and the (formatter "...") expansions
;use the '+' forms of these functions directly (which is faster) because,
;they do not need error checking or fancy stream coercion.  The '++' forms
;additionally assume the thing being output does not contain a newline.

(defmacro pprint-logical-block ((stream-symbol list
				 &key (prefix nil) (per-line-prefix nil)
				      (suffix ""))
				&body body)
  (cond ((eq stream-symbol nil) (setq stream-symbol '*standard-output*))
	((eq stream-symbol T) (setq stream-symbol '*terminal-io*)))
  (when (not (symbolp stream-symbol))
    (warn "STREAM-SYMBOL arg ~S to PPRINT-LOGICAL-BLOCK is not a bindable symbol"
	  stream-symbol)
    (setq stream-symbol '*standard-output*))
  (when (and prefix per-line-prefix)
    (warn "prefix ~S and per-line-prefix ~S cannot both be specified ~
           in PPRINT-LOGICAL-BLOCK")
    (setq per-line-prefix nil))
  `(maybe-initiate-xp-printing
     #'(lambda (,stream-symbol)
	 (let ((+l ,list)
	       (+p ,(or prefix per-line-prefix ""))
	       (+s ,suffix))
	   (pprint-logical-block+
	     (,stream-symbol +l +p +s ,(not (null per-line-prefix)) T nil)
	     ,@ body nil)))
     (decode-stream-arg ,stream-symbol)))

;Assumes var and args must be variables.  Other arguments must be literals or variables.

(defmacro pprint-logical-block+ ((var args prefix suffix per-line? circle-check? atsign?)
				 &body body)
   (when (and circle-check? atsign?)
     (setq circle-check? 'not-first-p))
  `(let ((*current-level* (1+ *current-level*))
	 (*current-length* -1)
	 (*parents* *parents*)
	 ,@(if (and circle-check? atsign?) `((not-first-p (plusp *current-length*)))))
     (unless (check-block-abbreviation ,var ,args ,circle-check?)
       (block logical-block
	 (start-block ,var ,prefix ,per-line? ,suffix)
	 (unwind-protect
	   (macrolet ((pprint-pop () `(pprint-pop+ ,',args ,',var))
		      (pprint-exit-if-list-exhausted ()
			`(if (null ,',args) (return-from logical-block nil))))
	     ,@ body)
	   (end-block ,var ,suffix))))))

(defun pprint-newline (kind &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (when (not (member kind '(:linear :miser :fill :mandatory)))
    (error "Invalid KIND argument ~A to PPRINT-NEWLINE" kind))
  (when (xp-structure-p stream)
    (pprint-newline+ kind stream))
  nil)

(defun pprint-indent (relative-to n &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (when (not (member relative-to '(:block :current)))
    (error "Invalid KIND argument ~A to PPRINT-INDENT" relative-to))
  (when (xp-structure-p stream)
    (pprint-indent+ relative-to n stream))
  nil)

(defun pprint-tab (kind colnum colinc &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (when (not (member kind '(:line :section :line-relative :section-relative)))
    (error "Invalid KIND argument ~A to PPRINT-TAB" kind))
  (when (xp-structure-p stream)
    (pprint-tab+ kind colnum colinc stream))
  nil)

;                        ---- COMPILED FORMAT ----

;Note that compiled format strings always print through xp streams even if
;they don't have any xp directives in them.  As a result, the compiled code
;can depend on the fact that the stream being operated on is an xp
;stream not an ordinary one.

 (eval-when (eval load compile)

(proclaim '(special *string* *used-args* *used-outer-args* *used-initial*
		    *get-arg-carefully* *inner-end* *outer-end* *at-top*))

(defvar *fn-table* (make-hash-table) "used to access fns for commands")

;Each of these functions expect to get called with two arguments
;start and end.  Start points to the first character after the ~
;marking the command.  End points to the first character after the
;command.  This includes the matching end command for paired commands.

(defmacro def-format-handler (char args &body body)
  (let ((name (intern (lisp:format nil "FORMAT-~A" char) (find-package "XP"))))
    `(eval-when (eval load compile)
       (defun ,name ,args ,@ body)
       (setf (gethash (char-upcase ,char) *fn-table*) (function ,name))
       (setf (gethash (char-downcase ,char) *fn-table*) (function ,name)))))

;Definitions of the forms used in the code created by PARSE.
;Note these functions assume the stream is in the var XP and is an xp stream,

; INITIAL holds the initial value of ARGS (for ~@*).
;Initial is always bound to (args) if it is bound at all.
;Note this uses args, but only when actually binding

(defun initial () (setq *used-initial* T) 'init)

(defmacro bind-initial (&body code)
  `(let* ((*used-initial* nil)
	  (body (progn ,@ code)))
     (if *used-initial* (make-binding 'init (args) body) body)))

; ARGS holds the current argument list
;The val bound to args must always be computed (to use it up) even if args is not used.

(defun args () (setq *used-args* T) 'args)

(defmacro bind-args (doit? val &body code)
  (if (eq doit? T)
      `(let* ((val ,val)
	      (*used-args* nil)
	      (body (progn ,@ code)))
	 (if *used-args* (make-binding 'args val body) (cons val body)))
      `(flet ((code () ,@ code))
	 (if (not ,doit?) (code) ;important bindings not done if not doit?
	     (let* ((val ,val)
		    (*used-args* nil)
		    (body (code)))
	       (if *used-args* (make-binding 'args val body) (cons val body)))))))

(defun outer-args () (setq *used-outer-args* T) 'outer-args)

(defmacro bind-outer-args (&body code)
  `(let* ((*used-outer-args* nil)
	  (body (progn ,@ code)))
     (if *used-outer-args* (make-binding 'outer-args (args) body) body)))

(defmacro maybe-bind (doit? var val &body code)
  `(let ((body (progn ,@ code)))
     (if ,doit? (make-binding ,var ,val body) body)))

(defun make-binding (var value body)
  `((let ((,var ,value)) ,@ body)))

(defun num-args () `(length ,(args)))

(defun get-arg ()
  (if *get-arg-carefully*
      (if *at-top* `(pprint-pop+top ,(args) xp) `(pprint-pop+ ,(args) xp))
      `(pop ,(args))))

(defmacro pprint-pop+ (args xp)
  `(if (pprint-pop-check+ ,args ,xp)
       (return-from logical-block nil)
       (pop ,args)))

(defun pprint-pop-check+ (args xp)
  (incf *current-length*)
  (cond ((not (listp args))  ;must be first so supersedes length abbrev
	 (write-string++ ". " xp 0 2)
	 (write+ args xp)
	 T)
	((and *print-length* ;must supersede circle check
	      (not (< *current-length* *print-length*)))
	 (write-string++ "..." xp 0 3)
	 (setq *abbreviation-happened* T)
	 T)
	((and *circularity-hash-table* (not (zerop *current-length*)))
	 (case (circularity-process xp args T)
	   (:first ;; note must inhibit rechecking of circularity for args.
		   (write+ (cons (car args) (cdr args)) xp) T)
	   (:subsequent T)
	   (T nil)))))

(defmacro pprint-pop+top (args xp)
  `(if (pprint-pop-check+top ,args ,xp)
       (return-from logical-block nil)
       (pop ,args)))

(defun pprint-pop-check+top (args xp)
  (incf *current-length*)
  (cond ((not (listp args))  ;must be first so supersedes length abbrev
	 (write-string++ ". " xp 0 2)
	 (write+ args xp)
	 T)
	((and *print-length* ;must supersede circle check
	      (not (< *current-length* *print-length*)))
	 (write-string++ "..." xp 0 3)
	 (setq *abbreviation-happened* T)
	 T)))

(defun literal (start end)
  (let ((sub-end nil) next-newline (result nil))
    (loop (setq next-newline
		(position #\newline *string* :start start :end end))
	  (setq sub-end (if next-newline next-newline end))
	  (when (< start sub-end)
	    (push (if (= start (1- sub-end))
		      `(write-char++ ,(aref *string* start) xp)
		      `(write-string++ ,(subseq *string* start sub-end) xp
				    ,0 ,(- sub-end start)))
		  result))
	  (when (null next-newline) (return nil))
	  (push `(pprint-newline+ :unconditional xp) result)
	  (setq start (1+ sub-end)))
    (if (null (cdr result)) (car result) (cons 'progn (nreverse result)))))

;This is available for putting on #".

(proclaim '(special *default-package*))

(defun format-string-reader (stream sub-char arg)
    (declare (ignore arg))
  (unread-char sub-char stream)
  `(function
    (lambda (s &rest args)
      (formatter-in-package ,(read stream) ,(package-name *package*)))))

(defmacro formatter-in-package (string reader-package)
  (formatter-fn string reader-package))

(defmacro formatter (string)
  `(function
    (lambda (s &rest args)
      (formatter-in-package ,string "USER"))))

(defun formatter-fn (*string* *default-package*)
  (or (catch :format-compilation-error
	`(apply (function maybe-initiate-xp-printing)
	        (function
		 (lambda (xp &rest args)
		  ,@(bind-initial
		     `((block top
			 ,@(let ((*get-arg-carefully* nil)
				 (*at-top* t)
				 (*inner-end* 'top)
				 (*outer-end* 'top))
			     (compile-format 0 (length *string*))))))
		  (if ,(args) (copy-list ,(args))))) ;needed by symbolics.
	        s args))
      `(apply #'format s *string* args)))



;The business with the catch above allows many (formatter "...") errors to be
;reported in a file without stopping the compilation of the file.

(defun maybe-compile-format-string (string force-fn?)
  (if (not (or force-fn? (fancy-directives-p string))) string
      (eval `(formatter ,string))))

;COMPILE-FORMAT gets called to turn a bit of format control string into code.

(defvar *testing-errors* nil "Used only when testing XP")

(defun err (id msg i)
  (if *testing-errors* (throw :testing-errors (list id i)))
  (warn "XP: cannot compile format string ~%~A~%~S~%~V@T|"
	msg *string* (1+ i))
  (throw :format-compilation-error nil))

(defun position-in (set start)
  (position-if #'(lambda (c) (find c set)) *string* :start start))

(defun position-not-in (set start)
  (position-if-not #'(lambda (c) (find c set)) *string* :start start))

(defun next-directive1 (start end)
  (let ((i (position #\~ *string* :start start :end end)) j)
    (when i
      (setq j (params-end (1+ i)))
      (when (char= (aref *string* j) #\/)
	(setq j (position #\/ *string* :start (1+ j) :end end))
 	(when (null j)
	  (err 3 "Matching / missing" (position #\/ *string* :start start)))))
    (values i j)))

(defun params-end (start) ;start points just after ~
  (let ((j start) (end (length *string*)))
    (loop
      (setq j (position-not-in "+-0123456789,Vv#:@" j))
      (when (null j) (err 1 "missing directive" (1- start)))
      (when (not (eq (aref *string* j) #\')) (return j))
      (incf j)
      (if (= j end) (err 2 "No character after '" (1- j)))
      (incf j))))

;Only called after correct parse is known.

(defun directive-start (end) ;end points at characters after params
  (loop
    (setq end (position #\~ *string* :end end :from-end T))
    (when (or (zerop end) (not (eq (aref *string* (1- end)) #\')))
      (return end))
    (decf end)))

(defun next-directive (start end)
  (let (i j ii k count c close
	(pairs '((#\( . #\)) (#\[ . #\]) (#\< . #\>) (#\{ . #\}))))
    (multiple-value-setq (i j) (next-directive1 start end))
    (when i
      (setq c (aref *string* j))
      (setq close (cdr (assoc c pairs)))
      (when close
	(setq k j count 0)
	(loop
	  (multiple-value-setq (ii k) (next-directive1 k end))
	  (when (null ii) (err 4 "No matching close directive" j))
	  (when (eql (aref *string* k) c) (incf count))
	  (when (eql (aref *string* k) close) (decf count)
	    (when (minusp count) (setq j k) (return nil))))))
    (values c i j)))

;breaks things up at ~; directives.

(defun chunk-up (start end)
  (let ((positions (list start)) (spot start))
    (loop
      (multiple-value-bind (c i j) (next-directive spot end)
	(declare (ignore i))
	(when (null c) (return (nreverse (cons end positions))))
	(when (eql c #\;) (push (1+ j) positions))
	(setq spot j)))))

(defun fancy-directives-p (*string*)
  (let (i (j 0) (end (length *string*)) c)
    (loop
      (multiple-value-setq (i j) (next-directive1 j end))	
      (when (not i) (return nil))
      (setq c (aref *string* j))
      (when (or (find c "_Ii/Ww") (and (find c ">Tt") (colonp j)))
	(return T)))))

(defun num-args-in-args (start &optional (err nil))
  (let ((n 0) (i (1- start)) c)
    (loop
      (setq i (position-not-in "+-0123456789," (1+ i)))
      (setq c (aref *string* i))
      (cond ((or (char= c #\V) (char= c #\v)) (incf n))
	    ((char= c #\#) 
	     (when err
	       (err 21 "# not allowed in ~~<...~~> by (formatter \"...\")" start))
	     (return nil))
	    ((char= c #\') (incf i))
	    (T (return n))))))

(defun compile-format (start end)
  (let ((result nil))
    (prog (c i j fn)
     L(multiple-value-setq (c i j) (next-directive start end))
      (when (if (null c) (< start end) (< start i))
	(push (literal start (if i i end)) result))
      (when (null c) (return (nreverse result)))
      (when (char= c #\newline)
	(multiple-value-bind (colon atsign)
	    (parse-params (1+ i) nil :nocolonatsign T)
	  (when atsign (push `(pprint-newline+ :unconditional xp) result))
	  (incf j)
	  (when (not colon)
	    (setq j (position-if-not
		      #'(lambda (c)
			  (or (char= c #\tab) (char= c #\space)))
		      *string* :start j :end end))
	    (when (null j) (setq j end)))
	  (setq start j)
	  (go L)))
      (setq fn (gethash c *fn-table*))
      (when (null fn) (err 5 "Unknown format directive" j))
      (incf j)
      (push (funcall fn (1+ i) j) result)
      (setq start j)
      (go L))))

;This gets called with start pointing to the character after the ~ that
;starts a command.  Defaults, is a list of default values for the
;parameters.  Max is the maximum number of parameters allowed.  Nocolon,
;noatsign, nocolonatsign can be used to specify what colon atsign
;combinations are permitted. Parse params returns three values, colon?,
;atsign? and a list of code chunks that correspond to the parameters
;specified.

(defun parse-params (start defaults &key (max (length defaults))
		     (nocolon nil) (noatsign nil) (nocolonatsign nil))
  (let ((colon nil) (atsign nil) (params nil) (i start) j c)
    (loop
      (setq c (aref *string* i))
      (cond ((or (char= c #\V) (char= c #\v)) (push (get-arg) params) (incf i))
	    ((char= c #\#) (push (num-args) params) (incf i))
	    ((char= c #\') (incf i) (push (aref *string* i) params) (incf i))
	    ((char= c #\,) (push nil params))
	    (T (setq j (position-not-in "+-0123456789" i))
	       (if (= i j) (return nil))
	       (push (parse-integer *string* :start i :end j :radix 10.) params)
	       (setq i j)))
      (if (char= (aref *string* i) #\,) (incf i) (return nil)))
    (setq params (nreverse params))
    (do ((ps params (cdr ps))
	 (ds defaults (cdr ds))
	 (nps nil))
	((null ds) (setq params (nreconc nps ps)))
      (push (cond ((or (null ps) (null (car ps))) (car ds))
		  ((not (consp (car ps))) (car ps))
		  (T `(cond (,(car ps)) (T ,(car ds)))))
	    nps))
    (if (and max (< max (length params))) (err 6 "Too many parameters" i))
    (loop
      (setq c (aref *string* i))
      (cond ((char= c #\:)
	     (if colon (err 7 "Two colons specified" i))
	     (setq colon T))
	    ((char= c #\@)
	     (if atsign (err 8 "Two atsigns specified" i))
	     (setq atsign T))
	    (T (return nil)))
      (incf i))
    (if (and colon nocolon) (err 9 "Colon not permitted" i))
    (if (and atsign noatsign) (err 10 "Atsign not permitted" i))
    (if (and colon atsign nocolonatsign)
	(err 11 "Colon and atsign together not permitted" i))
    (values colon atsign params)))

;Both these only called if correct parse already known.

(defun colonp (j) ;j points to directive name
  (or (eql (aref *string* (1- j)) #\:)
      (and (eql (aref *string* (1- j)) #\@)
	   (eql (aref *string* (- j 2)) #\:))))

(defun atsignp (j) ;j points to directive name
  (or (eql (aref *string* (1- j)) #\@)
      (and (eql (aref *string* (1- j)) #\:)
	   (eql (aref *string* (- j 2)) #\@))))

(def-format-handler #\/ (start end)
  (multiple-value-bind (colon atsign params) (parse-params start nil :max nil)
    (let* ((whole-name-start (1+ (params-end start)))
	   (colon-pos (position #\: *string* :start whole-name-start :end (1- end)))
	   (pkg (find-package
		  (if colon-pos
		      (string-upcase (subseq *string* whole-name-start colon-pos))
		      *default-package*)))
	   (name-start (cond ((null colon-pos) whole-name-start)
			     ((and (< colon-pos (1- end))
				   (char= #\: (aref *string* (1+ colon-pos))))
			      (+ colon-pos 2))
			     (T (1+ colon-pos))))
	   (fn (intern (string-upcase (subseq *string* name-start (1- end))) pkg)))
      (if (not (find-if #'consp params))
	  `(funcall (symbol-function ',fn) xp ,(get-arg) ,colon ,atsign ,@ params)
	  (let ((vars (mapcar #'(lambda (arg)
				  (declare (ignore arg))
				  (gentemp))
			      params)))
	    `(let ,(mapcar #'list vars params)
	       (funcall (symbol-function ',fn) xp ,(get-arg) ,colon ,atsign ,@ vars)))))))

(def-format-handler #\A (start end)
  (if (not (= end (1+ start))) (simple-directive start end)
      `(let ((*print-escape* nil))
	 (write+ ,(get-arg) XP))))

(def-format-handler #\S (start end)
  (if (not (= end (1+ start))) (simple-directive start end)
      `(let ((*print-escape* T))
	 (write+ ,(get-arg) XP))))

;The basic Format directives "DBOXRCFEG$".  The key thing about all of
;these directives is that they just get a single arg and print a chunk of
;stuff.  Further they are complex enough that I just call the standard
;definition of FORMAT to get the work done.  What should really be being
;called is the internal routine that FORMAT uses to do the corresponding
;work.  However, this cannot be done in a portable way.

(def-format-handler #\D (start end) (simple-directive start end))
(def-format-handler #\B (start end) (simple-directive start end))
(def-format-handler #\O (start end) (simple-directive start end))
(def-format-handler #\X (start end) (simple-directive start end))
(def-format-handler #\R (start end) (simple-directive start end))
(def-format-handler #\C (start end) (simple-directive start end))
(def-format-handler #\F (start end) (simple-directive start end))
(def-format-handler #\E (start end) (simple-directive start end))
(def-format-handler #\G (start end) (simple-directive start end))
(def-format-handler #\$ (start end) (simple-directive start end))

(defun simple-directive (start end)
  (let ((n (num-args-in-args start)))
    (if n `(using-format xp ,(subseq *string* (1- start) end)
			 ,@ (copy-tree (make-list (1+ n) :initial-element (get-arg))))
	(multiple-value-bind (colon atsign params)
	    (parse-params start nil :max 8)
	  (let* ((arg-str (subseq "v,v,v,v,v,v,v,v" 0
				  (max 0 (1- (* 2 (length params))))))
		 (str (concatenate 'string "~"
				   arg-str
				   (if colon ":" "")
				   (if atsign "@" "")
				   (subseq *string* (1- end) end))))
	    `(using-format xp ,str ,@ params ,(get-arg)))))))

(defun using-format (xp string &rest args)
  (let ((result (apply #'lisp:format nil string args)))
    (write-string+ result xp 0 (length result))))

;Format directives that get open coded "P%&~|T*?^"

(def-format-handler #\P (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
  (let ((arg (if colon `(car (backup-in-list 1 ,(initial) ,(args))) (get-arg))))
    (if atsign
	`(if (not (eql ,arg 1)) (write-string++ "ies" xp 0 3) (write-char++ #\y xp))
	`(if (not (eql ,arg 1)) (write-char++ #\s XP))))))

(def-format-handler #\% (start end) (declare (ignore end))
  (multiple-newlines start :unconditional))

(def-format-handler #\& (start end) (declare (ignore end))
  (multiple-newlines start :fresh))

(defun multiple-newlines (start kind)
  (multiple-value-bind (colon atsign params) 
      (parse-params start '(1) :nocolon T :noatsign T)
      (declare (ignore colon atsign))
    (if (eql (car params) 1) `(pprint-newline+ ,kind xp)
	`(multiple-newlines1 xp ,kind ,(car params)))))

(defun multiple-newlines1 (xp kind num)
  (do ((n num (1- n))) ((not (plusp n)))
    (pprint-newline+ kind xp)
    (setq kind :unconditional)))

(def-format-handler #\| (start end) (declare (ignore end))
  (multiple-chars start #.(aref (lisp:format nil "~|") 0)))

(def-format-handler #\~ (start end) (declare (ignore end))
  (multiple-chars start #\~))

(defun multiple-chars (start char)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(1) :nocolon t :noatsign t)
      (declare (ignore colon atsign))
    (if (eql (car params) 1) `(write-char++ ,char xp)
	`(multiple-chars1 xp ,(car params) ,char))))

(defun multiple-chars1 (xp num char)
  (do ((n num (1- n))) ((not (plusp n)))
    (write-char++ char xp)))

(def-format-handler #\T (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params) (parse-params start '(1 1))
    `(pprint-tab+ ,(if colon (if atsign :section-relative :section)
		             (if atsign :line-relative :line))
		  ,(pop params) ,(pop params) xp)))

(def-format-handler #\* (start end) (declare (ignore end))
  (if (atsignp (params-end start))
      (multiple-value-bind (colon atsign params)
	  (parse-params start '(0) :nocolon t)
	  (declare (ignore colon atsign))
	`(setq args (backup-to ,(car params) ,(initial) ,(args))))
      (multiple-value-bind (colon atsign params)
	  (parse-params start '(1))
	  (declare (ignore atsign))
	`(setq args
	       ,(if colon `(backup-in-list ,(car params) ,(initial) ,(args))
		    `(nthcdr ,(car params) ,(args)))))))

;fancy stuff here, so will not get spurious indications of circularity.

(defun backup-in-list (num list some-tail)
  (backup-to (- (tail-pos list some-tail) num) list some-tail))

(defun backup-to (num list some-tail)
  (if (not *circularity-hash-table*) (nthcdr num list)
      (multiple-value-bind (pos share) (tail-pos list some-tail)
	  (declare (ignore pos))
	(if (not (< num share)) (nthcdr num list)
	    (do ((L (nthcdr num list) (cdr L))
		 (n (- share num) (1- n))
		 (R nil (cons (car L) R)))
		((zerop n) (nreconc R L)))))))

;because of backup-to, a prefix of some-tail may have been copied (in which
;case it cannot share anything with list), but there is a cons in some-tail
;that is in list.  This can be used to determine the position of some-tail
;relative to list.  However, we have to be careful, because they both could
;be cdr recursive.

(defun tail-pos (list some-tail)
  (block outer
    (do ((n 0 (1+ n))
	 (L list (cdr L)))
	(nil)
      (do ((m n (1- m))
	   (ST some-tail (cdr ST)))
	  (nil)
	(if (minusp m) (return nil))
	(if (eq ST L) (return-from outer (values m n)))))))

(def-format-handler #\? (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil :nocolon t)
      (declare (ignore colon))
    (if (not atsign) `(apply #'format xp ,(get-arg) ,(get-arg))
	`(let ((fn (process-format-string ,(get-arg) T)))
	   (setq args (apply fn xp ,(args)))))))

(def-format-handler #\^ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start nil :max 3 :noatsign t)
      (declare (ignore atsign))
    `(if ,(cond ((null params) `(null ,(if colon `(cdr ,(outer-args)) (args))))
		(t `(do-complex-^-test ,@ params)))
	 (return-from ,(if colon *outer-end* *inner-end*) nil))))

(defun do-complex-^-test (a1 &optional (a2 nil) (a3 nil))
  (cond (a3 (and (<= a1 a2) (<= a2 a3)))
	(a2 (= a1 a2))
	(t (= 0 a1))))

;delimited pairs of format directives. "(){}[]<>;"

(def-format-handler #\[ (start end)
  (multiple-value-bind (colon atsign params)
      (parse-params start nil :max 1 :nocolonatsign T)
    (setq start (1+ (params-end start)))
    (let* ((chunks (chunk-up start end))
	   (innards (do ((ns chunks (cdr ns))
			 (ms (cdr chunks) (cdr ms))
			 (result nil))
			((null ms) (return (nreverse result)))
		      (push (compile-format (car ns) (directive-start (car ms)))
			    result))))
      (cond (colon (when (not (= (length innards) 2))
		     (err 13 "Wrong number of clauses in ~~:[...~~]" (1- start)))
		   `(cond ((null ,(get-arg)) ,@ (car innards))
			  (T ,@ (cadr innards))))
	    (atsign (when (not (= (length innards) 1))
		      (err 14 "Too many clauses in ~~@[...~~]" (1- start)))
		    `(cond ((car args) ,@ (car innards)) (T ,(get-arg))))
	    (T (let* ((j -1) (len (- (length chunks) 2))
		      (else? (colonp (1- (nth len chunks)))))
		 `(case ,(if params (car params) (get-arg))
		    ,@(mapcar #'(lambda (unit)
				  (incf j)
				  `(,(if (and else? (= j len)) T j) ,@ unit))
			      innards))))))))

(def-format-handler #\( (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end start)))
    (setq end (directive-start end))
    `(progn (push-char-mode xp ,(cond ((and colon atsign) :UP)
				      (colon :CAP1)
				      (atsign :CAP0)
				      (T :DOWN)))
	    ,@(compile-format start end)
	    (pop-char-mode xp))))

(def-format-handler #\; (start end) (declare (ignore start))
  (err 15 "~~; appears out of context" (1- end)))
(def-format-handler #\] (start end) (declare (ignore start))
  (err 16 "Unmatched closing directive" (1- end)))
(def-format-handler #\) (start end) (declare (ignore start))
  (err 17 "Unmatched closing directive" (1- end)))
(def-format-handler #\> (start end) (declare (ignore start))
  (err 18 "Unmatched closing directive" (1- end)))
(def-format-handler #\} (start end) (declare (ignore start))
  (err 19 "Unmatched closing directive" (1- end)))

(def-format-handler #\{ (start end)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(-1) :max 1)
    (let* ((force-once (colonp (1- end)))
	   (n (car params))
	   (bounded (not (eql n -1))))
      (setq start (1+ (params-end start)))
      (setq end (directive-start end))
      (car (maybe-bind bounded 'N n ;must be outermost if is V or #
	     (maybe-bind (not (> end start)) 'FN  ;must be second
			 `(process-format-string ,(get-arg) T)
	       (bind-args (not atsign) (get-arg)
		 `((prog () ,@(if force-once '((go S)))
		       L (if (null ,(args)) (return nil))
		       ,@(if force-once '(S))
			 ,@(if bounded '((if (= N 0) (return nil) (decf N))))
			 ,@(bind-outer-args
			     (bind-args colon (get-arg)
			       (bind-initial
				 (let ((*get-arg-carefully*
					 (and *get-arg-carefully* atsign))
				       (*at-top* (and *at-top* atsign))
				       (*outer-end* nil)
				       (*inner-end* nil))
				   (if (not colon)
				       (if (not (> end start))
					   `((setq args (apply FN xp ,(args))))
					   (compile-format start end))
				       (let ((*inner-end* 'inner))
					 `((block inner
					     ,@(if (not (> end start))
						   `((setq args (apply FN xp ,(args))))
						   (compile-format start end))))))))))
			 (go L))))))))))

(def-format-handler #\< (start end)
  (if (colonp (1- end))
      (handle-logical-block start end)
      (handle-standard-< start end)))

(defun handle-standard-< (start end)
  (let ((n (num-args-in-directive start end)))
    `(using-format xp ,(subseq *string* (1- start) end)
		   ,@ (copy-tree (make-list n :initial-element (get-arg))))))

(defun num-args-in-directive (start end)
  (let ((n 0) c i j)
    (incf n (num-args-in-args start T))
    (multiple-value-setq (j i) (next-directive1 start end))
    (loop 
      (multiple-value-setq (c i j) (next-directive j end))
      (when (null c) (return n))
      (cond ((eql c #\;)
	     (if (colonp j)
		 (err 22 "~~:; not supported in ~~<...~~> by (formatter \"...\")." j)))
	    ((find c "*[^<_IiWw{Tt")
	     (err 23 "~~<...~~> too complicated to be supported by (formatter \"...\")." j))
	    ((eql c #\() (incf n (num-args-in-directive (1+ i) j)))
	    ((find c "%&\|~") (incf n (num-args-in-args (1+ i) T)))
	    ((eql c #\?)
	     (when (atsignp j)
	       (err 23 "~~<...~~> too complicated to be supported by (formatter \"...\")." j))
	     (incf n 2))
	    ((find c "AaSsDdBbOoXxRrCcFfEeGg$Pp")
	     (incf n (1+ (num-args-in-args (1+ i) T))))))))

;The pretty-printing directives. "_IW<:>"

(def-format-handler #\_ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    `(pprint-newline+ ,(cond ((and colon atsign) :mandatory)
			     (colon :fill)
			     (atsign :miser)
			     (T :linear)) XP)))

(def-format-handler #\I (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(0) :noatsign T)
      (declare (ignore atsign))
    `(pprint-indent+ ,(if colon :current :block) ,(car params) XP)))

(def-format-handler #\W (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (cond ((not (or colon atsign)) `(write+ ,(get-arg) XP))
	  (T `(let (,@(if colon '((*print-pretty* T)))
		    ,@(if atsign '((*print-level* nil) (*print-length* nil))))
		(write+ ,(get-arg) XP))))))

(defun handle-logical-block (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end start)))
    (let* ((chunks (chunk-up start end))
	   (on-each-line?
	     (and (cddr chunks) (atsignp (1- (cadr chunks)))))
	   (prefix
	     (cond ((cddr chunks) (pop chunks)
		    (subseq *string* start (directive-start (car chunks))))
		   (colon "(")))
	   (suffix
	     (cond ((cddr chunks)
		    (subseq *string* (cadr chunks)
			    (directive-start (caddr chunks))))
		   (colon ")"))))
      (when (cdddr chunks) (err 24 "Too many subclauses in ~~<...~~:>" (1- start)))
      (when (and prefix (or (find #\~ prefix) (find #\newline prefix)))
	(err 25 "Prefix in ~~<...~~:> must be a literal string without newline" start))
      (when (and suffix (or (find #\~ suffix) (find #\newline suffix)))
	(err 26 "Suffix in ~~<...~~:> must be a literal string without newline"
	     (cadr chunks)))
      (car (bind-args T (if atsign `(prog1 ,(args) (setq ,(args) nil)) (get-arg))
	     (bind-initial
	       `((pprint-logical-block+ (xp ,(args) ,prefix ,suffix ,on-each-line?
					    ,(not (and *at-top* atsign)) ,atsign)
		   ,@(fill-transform (atsignp (1- end))
		       (let ((*get-arg-carefully* T)
			     (*at-top* (and *at-top* atsign))
			     (*inner-end* 'logical-block)
			     (*outer-end* 'logical-block))
			 (compile-format (car chunks)
					 (directive-start (cadr chunks)))))))))))))

(defun check-block-abbreviation (xp args circle-check?)
  (cond ((not (listp args)) (write+ args xp) T)
	((and *print-level* (> *current-level* *print-level*))
	 (write-char++ #\# XP) (setq *abbreviation-happened* T) T)
	((and *circularity-hash-table* circle-check?
	      (eq (circularity-process xp args nil) :subsequent)) T)
	(T nil)))

(defun fill-transform (doit? body)
  (if (not doit?) body
      (mapcan #'(lambda (form)
		  (cond ((eq (car form) 'write-string++)
			 (fill-transform-literal (cadr form)))
			((eq (car form) 'write-char++)
			 (fill-transform-char (cadr form)))
			(T (list form))))
	      body)))

(defun fill-transform-char (char)
  (if (or (char= char #\space) (char= char #\tab))
      (list `(write-char++ ,char xp) '(pprint-newline+ :fill xp))
      `((write-char++ ,char xp))))

(defun fill-transform-literal (string)
  (flet ((white-space (c) (or (char= c #\space) (char= c #\tab))))
    (do ((index 0 end) (result) (end nil nil)) (nil)
      (let ((white (position-if #'white-space string :start index)))
	(when white
	  (setq end (position-if-not #'white-space string :start (1+ white))))
	(when (null end)
	  (setq end (length string)))
	(push `(write-string++ ,(subseq string index end) xp ,0 ,(- end index))
	      result)
	(if white (push '(pprint-newline+ :fill xp) result))
	(if (null white) (return (nreverse result)))))))

 ) ;end of eval when for all (formatter "...") stuff.

;                ---- PRETTY PRINTING FORMATS ----

(defun pretty-array (xp array)
  (cond ((vectorp array) (pretty-vector xp array))
	((zerop (array-rank array))
	 (write-string++ "#0A " xp 0 4)
	 (write+ (aref array) xp))
	(T (pretty-non-vector xp array))))

(defun pretty-vector (xp v)
  (pprint-logical-block (xp nil :prefix "#(" :suffix ")")
    (let ((end (length v)) (i 0))
      (when (plusp end)
	(loop (pprint-pop)
	      (write+ (aref v i) xp)
	      (if (= (incf i) end) (return nil))
	      (write-char++ #\space xp)
	      (pprint-newline+ :fill xp))))))

(proclaim '(special *prefix*))

(defun pretty-non-vector (xp array)
  (let* ((bottom (1- (array-rank array)))
	 (indices (make-list (1+ bottom) :initial-element 0))
	 (dims (array-dimensions array))
	 (*prefix* (lisp:format nil "#~DA(" (1+ bottom))))
    (labels ((pretty-slice (slice)
	       (pprint-logical-block (xp nil :prefix *prefix* :suffix ")")
		 (let ((end (nth slice dims))
		       (spot (nthcdr slice indices))
		       (i 0)
		       (*prefix* "("))
		   (when (plusp end)
		     (loop (pprint-pop)
			   (setf (car spot) i)
			   (if (= slice bottom)
			       (write+ (apply #'aref array indices) xp)
			       (pretty-slice (1+ slice)))
			   (if (= (incf i) end) (return nil))
			   (write-char++ #\space xp)
			   (pprint-newline+ (if (= slice bottom) :fill :linear) xp)))))))
      (pretty-slice 0))))

;Must use pprint-logical-block (no +) in the following three, because they are
;exported functions.

(defun pprint-linear (s list &optional (colon? T) atsign?)
     (declare (ignore atsign?))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-newline+ :linear s))))

(defun pprint-fill (s list &optional (colon? T) atsign?)
    (declare (ignore atsign?))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-newline+ :fill s))))

(defun pprint-tabular (s list &optional (colon? T) atsign? (tabsize nil))
  (declare (ignore atsign?))
  (when (null tabsize) (setq tabsize 16))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-tab+ :section-relative 0 tabsize s)
	  (pprint-newline+ :fill s))))

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
      (funcall (formatter "~:<~@{~:/xp:pprint-fill/~^ ~_~}~:>") xp list)))

(defun block-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~^~W~^ ~@_~W~^~@{ ~_~W~^~}~:>") xp list))

(defun defun-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~W~^ ~@_~W~^ ~@_~:/xp:pprint-fill/~^~@{ ~_~W~^~}~:>")
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
	  (setq template (cddr template))
	  (setq in-first-section nil))
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
	 (setq need-newline nil))
	(T (pprint-tab+ :section indentation 0 xp)
	   (write+ item xp)
	   (setq need-newline T))))

(defun function-call-p (x)
  (and (consp x) (symbolp (car x)) (fboundp (car x))))


;THE FOLLOWING STUFF SETS UP THE DEFAULT *PRINT-PPRINT-DISPATCH*
 
;This is an attempt to specify a correct format for every form in the CL book
;that does not just get printed out like an ordinary function call 
;(i.e., most special forms and many macros).  This of course does not 
;cover anything new you define.

(defun let-print (xp obj)
  (funcall (formatter "~:<~1I~W~^ ~@_~/xp:bind-list/~^~@{ ~_~W~^~}~:>") xp obj))

(defun cond-print (xp obj)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~:/xp:pprint-linear/~^ ~_~}~:>") xp obj))

(defun dmm-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun defsetf-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun do-print (xp obj)
  (funcall 
 (formatter "~:<~W~^ ~:I~@_~/xp:bind-list/~^ ~_~:/xp:pprint-linear/ ~1I~^~@{ ~_~W~^~}~:>")
           xp obj))


(defun flet-print (xp obj)
  (funcall (formatter "~:<~1I~W~^ ~@_~:<~@{~/xp:block-like/~^ ~_~}~:>~^~@{ ~_~W~^~}~:>")
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
    (funcall (formatter "~:<~W~^ ~:/xp:pprint-fill/~^ ~@{~/xp:maybelab/~^ ~}~:>")
	     xp list)))

(defun setq-print (xp obj)
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
    (funcall (formatter "~:<~W~^ ~@{~/xp:maybelab/~^ ~}~:>") xp list)))

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
;	   {AND clause}*       ;one AND on each line.
;        [ELSE
;          clause
;	   {AND clause}*]      ;one AND on each line.
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
	((string= (setq string (string token)) "FINALLY") :finally)
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
		     (setq token (pprint-pop))
		     (setq type (token-type token)))
		   (print-clause (xp)
		     (case type
		       (:linear-head (print-exprs xp nil :mandatory))
		       (:cond-head (print-cond xp))
		       (:finally (print-exprs xp T :mandatory))
		       (otherwise (print-exprs xp nil :fill))))
		   (print-exprs (xp skip-first-non-expr newline-type)
		     (let ((first token))
		       (next-token)	;so always happens no matter what
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
		       (next-token)	;so always happens no matter what
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
  (funcall (xp:formatter "`#~W") xp (car (bqtify obj))))

(lisp:defstruct bq-struct code data)

(defun bq-struct-print (xp obj)
  ;; We must print out the string as a string, in order to prevent
  ;; circularity testing
  (let ((code (bq-struct-code obj)))
    (declare (simple-string code))
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
	 (mapcon #'(lambda (x) (bqtify-inline x t)) (cdr exp)))
	((eq (car exp) *bq-nconc*)
	 (mapcon #'(lambda (x) (bqtify-inline x nil)) (cdr exp)))
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

(setq *IPD* (make-pprint-dispatch))

(set-pprint-dispatch+ '(satisfies function-call-p) #'fn-call '(-5) *IPD*)
(set-pprint-dispatch+ 'cons #'pprint-fill '(-10) *IPD*)

#+(or :lucid :symbolics)(eval-when (eval load)
(set-pprint-dispatch+ 'bq-struct #'bq-struct-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-cons*)) #'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-list*)) #'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-list**)) #'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-append*)) #'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-nconc*)) #'bq-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-vector*)) #'bq-vector-print '(0) *IPD*)
(set-pprint-dispatch+ `(cons (member ,*bq-list-to-vector*)) #'bq-vector-print '(0) *IPD*) )

(set-pprint-dispatch+ '(cons (member defstruct)) #'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member block)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member case)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member catch)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member ccase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member compiler-let)) #'let-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member cond)) #'cond-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member ctypecase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defconstant)) #'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member define-setf-method)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defmacro)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member define-modify-macro)) #'dmm-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member defparameter)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defsetf)) #'defsetf-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member define-setf-method)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member lisp:defstruct)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member deftype)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defun)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member defvar)) #'defun-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member do)) #'do-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member do*)) #'do-print '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member do-all-symbols)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member do-external-symbols)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member do-symbols)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member dolist)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member dotimes)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member ecase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member etypecase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member eval-when)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member flet)) #'flet-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member function)) #'function-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member labels)) #'flet-print '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member lambda)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member let)) #'let-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member let*)) #'let-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member locally)) #'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member loop)) #'pretty-loop '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member macrolet)) #'flet-print '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member multiple-value-bind)) #'mvb-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member multiple-value-setq)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member prog)) #'prog-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member prog*)) #'prog-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member progv)) #'defun-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member psetf)) #'setq-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member psetq)) #'setq-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member quote)) #'quote-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member return-from)) #'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member setf)) #'setq-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member setq)) #'setq-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member tagbody)) #'tagbody-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member throw)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member typecase)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member unless)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member unwind-protect)) #'up-print '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member when)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member with-input-from-string)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member with-open-file)) #'block-like '(0) *IPD*)
(set-pprint-dispatch+ '(cons (member with-open-stream)) #'block-like '(0) *IPD*) 
(set-pprint-dispatch+ '(cons (member with-output-to-string)) #'block-like '(0) *IPD*) 

(defun pprint-dispatch-print (xp table)
  (let ((stuff (copy-list (others table))))
    (maphash #'(lambda (key val) (declare (ignore key))
		       (push val stuff))
	     (conses-with-cars table))
    (maphash #'(lambda (key val) (declare (ignore key))
		       (push val stuff))
	     (structures table))
    (setq stuff (sort stuff #'priority-> :key #'(lambda (x) (car (full-spec x)))))
    (pprint-logical-block (xp stuff :prefix "#<" :suffix ">")
      (format xp (formatter "pprint dispatch table containing ~A entries: ")
	      (length stuff))
      (loop (pprint-exit-if-list-exhausted)
	    (let ((entry (pprint-pop)))
	      (format xp (formatter "~{~_P=~4D ~W~} F=~W ")
		      (full-spec entry) (fn entry)))))))

(setf (get 'pprint-dispatch 'structure-printer) #'pprint-dispatch-print)

(set-pprint-dispatch+ 'pprint-dispatch #'pprint-dispatch-print '(0) *IPD*) 

;       ---- THINGS THAT ONLY WORK ON SYMBOLICS MACHINES ----

;The following are a number of special things that work on the
;Symbolics Lisp Machine only.  Similar things may be possible on
;other systems.

#+symbolics
(eval-when (eval load)

;must be careful to avoid an infinite loop, because XP calls write,
;which obeys scl:*print-pretty-printer*.

(defvar *allow-errors* nil)

(defun default-system-printing (object template stream)
    (declare (ignore template))
  (si:print-object object *current-level* *print-escape* stream))

(defun pretty-printer (object template stream)
    (declare (ignore template))
  (let ((scl:*print-pretty-printer* #'default-system-printing))
    (setq stream (decode-stream-arg stream))
    (if (or *allow-errors* (and (streamp stream) (not (scl:send stream :interactive))))
	(let ((*allow-errors* nil)) (format stream (formatter "~w") object))
	(unless (scl:ignore-errors (format stream (formatter "~w") object) t)
	  (let ((*print-pretty* nil))
	    (lisp:princ "#<Cannot pretty print " stream)
	    (unless (scl:ignore-errors (lisp:write object :stream stream) t)
	      (lisp:princ "#<Cannot print value of " stream)
	      (lisp:princ (sys:%p-pointer object) stream)
	      (lisp:princ ">" stream)))
	  (lisp:princ ">" stream))))
  object)

(defun xp-print (fn stream args &optional (recursive nil))
  (unless (and (not recursive)
	       (si:send-if-handles stream :xp fn stream args))
    (setq *result* (do-xp-printing fn stream args))
    (when *locating-circularities*
      (setq *locating-circularities* nil)
      (setq *abbreviation-happened* nil)
      (setq *parents* nil)
      (setq *result* (do-xp-printing fn stream args)))))

(defun xp-sensitivity-print (fn stream args)
  (let* ((object (cond ((and args (null (cdr args))) (car args))
		       ((copy-list args))))) ;in case is in stack
    (si:with-sensitivity-flag (object stream)
      (xp-print fn stream args T))
    T))
dw::
(defmethod (:xp dynamic-window) (fn stream args)
  (xp::xp-sensitivity-print fn stream args))
zwei::
(defmethod (:xp presentation-recording-interval-stream) (fn stream args)
    (xp::xp-sensitivity-print fn stream args))
zwei::
(defmethod (:xp mini-ie-stream) mini-ie-forward-output-message)

;didn't bother with (:xp filling-stream) see (:gprint filling-stream).

;see the method (:gprint dynamic-window) and the file gprint for
;info about how to really make everything mousable.

dw::
(define-presentation-translator print-without-abbreviation ;modelled on describe-form
  (expression form
   :gesture :print-without-abbreviation 
   :documentation "Pretty print in full"
   :priority 3.0)
   (object)
  `(xp::pp1 ',object))

(setf (dw::mouse-char-for-gesture :print-without-abbreviation) #\mouse-m)

(defun xp::pp1 (x)
  (pp x)
  (setq *** ** ** * * x)
  (values))

(defun pp1-print (xp obj)
  (setf (line-limit xp) 1)
  (funcall (formatter "~W") xp (cons 'pp (cdr obj))))

(set-pprint-dispatch+ '(cons (member pp1)) #'pp1-print '(0) *IPD*)

#|
;The following seems to cause problems in Genera 8.0.1 and does not
;seem all that worth saving, since we now have mousability.

(defun re-pretty-print-in-full (ignore)
  (zl:send tv:selected-window :refresh-rubout-handler)
  (let ((*print-lines* nil)
	(*print-length* nil)
	(*print-level* nil))
    (lisp:fresh-line tv:selected-window)
    (funcall (scl:symbol-value-in-stack-group '*last-abbreviated-printing*
	       (zl:send (zl:send tv:selected-window :process) :stack-group))
	     tv:selected-window))
  (when (and (zl:send tv:selected-window :interactor-p)
	     (not (and (typep tv:selected-window 'tv:basic-typeout-window)
		       (zl:send tv:selected-window :incomplete-p))))
    (lisp:terpri tv:selected-window)
    (if (stringp si:*cp-prompt*) (lisp:princ si:*cp-prompt* tv:selected-window)
	(funcall si:*cp-prompt* tv:selected-window nil))
    (zl:send tv:selected-window :refresh-rubout-handler)))

;The above can be installed by doing
;(tv:add-function-key #\resume 're-pretty-print-in-full
;		      "print without length, line, or level abbreviation")
|#

;The following allows XP to utilize the information maintained by the ZWEI 
;editor about how various forms should be indented.

(defun fancy-fn-call (xp list)
  (let ((template (gethash (car list) zwei:*lisp-indentation-offset-hash-table*)))
    (when (not (consp template))
      (setq template
	    (if (string-equal (car list) "def" :end1 3 :end2 3)
		zwei:*lisp-defun-indentation*)))
    (if template (print-fancy-fn-call xp list template) (fn-call xp list))))

(set-pprint-dispatch+ '(satisfies function-call-p) #'fancy-fn-call '(-5) *IPD*)

;inverter for #"..."

(defun formatter-print (xp list)
  (if (and (consp (cdr list)) (stringp (cadr list)) (null (cddr list))
	   (eq (get-dispatch-macro-character #\# #\") #'format-string-reader))
      (funcall (formatter "#~W") xp (cadr list))
      (xp:pprint-fill xp list)))

(set-pprint-dispatch+ '(cons (member formatter)) #'formatter-print '(0) *IPD*)

;This ZL function needs extra help to print right.

(defun do-named-print (xp obj)
  (funcall (formatter "~:<~W~^ ~:I~@_~W~^ ~_~/xp:bind-list/~^ ~_~
                       ~:/xp:pprint-linear/~^~1I~@{~_~W~^~}~:>") xp obj))

(set-pprint-dispatch+ '(cons (member zl:do-named)) #'do-named-print '(0) *IPD*)

;;; This is a revamped trace printing function utilizing xp to the full.
;;; because of the weak way XP supports mousability at the moment,
;;; we cannot get the args on one line when they will fit yet.

(defconstant trace-leader "|   |   |   |   |   |   |   |   |   |   |   |   | ")

(defvar *trace-print-lines* 1 "controles *print-lines* when trace prints")

(defun trace-print (depth direction function print-args-flag extras-1 extras-2)
  (declare (special si:arglist si:values))
  (let ((ind (min 50 (* 2 (1- si:trace-level))))
	(args (and (eq direction 'si:enter) print-args-flag si:arglist))
	(vals (and print-args-flag si:values))
	(*print-lines* *trace-print-lines*))
    (flet ((trace-line (control-string &rest args)
	     (terpri si:trace-output)
	     (write-string trace-leader si:trace-output :end ind)
	     (apply #'format si:trace-output control-string args)))
      (trace-line "~D ~A ~S " depth direction function)
      (if (and args (null (cdr args))) (prin1 (car args) si:trace-output)
	  (dolist (a args) (trace-line "  ~S" a)))
      (if (and vals (null (cdr vals))) (prin1 (car vals) si:trace-output)
	  (dolist (v vals) (trace-line "  ~S" v)))
      (dolist (e extras-1) (trace-line "  \\\\ ~S" (eval e)))
      (dolist (e extras-2) (trace-line "  // ~S" (eval e))))))

(defun pp (&optional (object nil objectp) (stream *standard-output*))
  (let ((*print-pretty* T)
	(*print-lines* nil)
	(*print-length* nil)
	(*print-level* nil))
    (lisp:fresh-line stream)
    (if objectp (write object :stream stream)
	(funcall *last-abbreviated-printing* stream))
    (values)))
)

;so only happens first time is loaded.
(when (eq *print-pprint-dispatch* T)
  (setq *print-pprint-dispatch* (copy-pprint-dispatch nil)))

;changes since last documentation.
;~/fn/ only refers to global function values, not lexical.

;------------------------------------------------------------------------

;Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------
