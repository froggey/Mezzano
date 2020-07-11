(in-package :mezzano.xp)

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
;              MIT AI Laboratory
;              545 Tech. Sq.
;              Cambridge MA 02139

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
        (lambda (&optional stream) (declare (ignore stream)) nil)
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

(defun structure-type-p (x)
  (and (symbolp x)
       (typep (find-class x nil) 'structure-class)))
(defun output-width (&optional (s *standard-output*))
  (mezzano.gray:stream-line-length s))
(defun output-position (&optional (s *standard-output*))
  (mezzano.gray:stream-line-column s))

(defvar *circularity-hash-table* nil
  "Contains hash table used for locating circularities")
;When an entry is first made it is zero.
;If a duplicate is found, a positive integer tag is assigned.
;After the first time the object is printed out, the tag is negated.

(defun get-circularity-hash-table ()
  (make-hash-table :test 'eq))

;                       ---- DISPATCHING ----

;; FIXME: This needs locking, not just the hash-tables but also for others.
(defclass pprint-dispatch-table ()
  ((conses-with-cars :initarg :conses-with-cars :accessor conses-with-cars)
   (structures :initarg :structures :accessor structures)
   (others :initarg :others :accessor others))
  (:default-initargs
   :conses-with-cars (make-hash-table :test #'eq :synchronized t)
   :structures (make-hash-table :test #'eq :synchronized t)
   :others nil))

(defun make-pprint-dispatch (&rest args)
  (apply 'make-instance 'pprint-dispatch-table args))

;The list and the hash-tables contain entries of the
;following form.  When stored in the hash tables, the test entry is
;the number of entries in the OTHERS list that have a higher priority.

(defclass entry ()
  ((test :initarg :test :accessor test)        ;predicate function or count of higher priority others.
   (fn :initarg :fn :accessor fn)          ;pprint function
   (full-spec :initarg :full-spec :accessor full-spec))  ;list of priority and type specifier
  (:default-initargs
   :test nil :fn nil :full-spec nil))

(defun make-entry (&rest args)
  (apply 'make-instance 'entry args))

(defun copy-entry (entry)
  (make-instance 'entry
                 :test (test entry)
                 :fn (fn entry)
                 :full-spec (full-spec entry)))

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (when (null table) (setf table *IPD*))
  (let* ((new-conses-with-cars (make-hash-table
                                :test #'eq
                                :synchronized t))
         (new-structures (make-hash-table
                          :test #'eq
                          :synchronized t)))
    (maphash (lambda (key value)
                 (setf (gethash key new-conses-with-cars) (copy-entry value)))
             (conses-with-cars table))
    (maphash (lambda (key value)
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
                       (count-if (lambda (e)
                                     (priority-> (car (full-spec e)) priority))
                                 (others table)))
                 (setf (gethash (cadadr type-specifier) (conses-with-cars table)) entry))))
      (structure-type
        (cond ((null entry) (remhash type-specifier (structures table)))
              (T (setf (test entry)
                       (count-if (lambda (e)
                                     (priority-> (car (full-spec e)) priority))
                                 (others table)))
                 (setf (gethash type-specifier (structures table)) entry))))
      (T ;other
         (let ((old (car (member type-specifier (others table) :test #'equal
                                 :key (lambda (e) (cadr (full-spec e)))))))
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
  (maphash (lambda (key value)
                 (declare (ignore key))
               (if (priority-> priority (car (full-spec value)))
                   (incf (test value) delta)))
           (conses-with-cars table))
  (maphash (lambda (key value)
                 (declare (ignore key))
               (if (priority-> priority (car (full-spec value)))
                   (incf (test value) delta)))
           (structures table)))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (when (null table) (setf table *IPD*))
  (let ((fn (get-printer object table)))
    (values (or fn #'non-pretty-print) (not (null fn)))))

(defun get-printer (object table)
  (when (not (typep table 'pprint-dispatch-table))
    (return-from get-printer nil))
  (let* ((entry (if (consp object)
                    (gethash (car object) (conses-with-cars table))
                    (gethash (type-of object) (structures table)))))
    (if (not entry)
        (setf entry (find object (others table) :test #'fits))
        (do ((i (test entry) (1- i))
             (l (others table) (cdr l)))
            ((zerop i))
          (when (fits object (car l)) (setf entry (car l)) (return nil))))
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

(defconstant +block-stack-entry-size+ 1)
(defconstant +prefix-stack-entry-size+ 5)
(defconstant +queue-entry-size+ 7)
(defconstant +buffer-entry-size+ 1)
(defconstant +prefix-entry-size+ 1)
(defconstant +suffix-entry-size+ 1)

(defconstant +block-stack-min-size+ (* 35 +block-stack-entry-size+))
(defconstant +prefix-stack-min-size+ (* 30 +prefix-stack-entry-size+))
(defconstant +queue-min-size+ (* 75 +queue-entry-size+))
(defconstant +buffer-min-size+ 256)
(defconstant +prefix-min-size+ 256)
(defconstant +suffix-min-size+ 256)

(defclass xp-structure (mezzano.gray:fundamental-character-output-stream)
  ;;The stream IO eventually goes to.
  ((base-stream :initarg :base-stream :accessor base-stream)
   ;;The line length to use for formatting.
   (linel :accessor linel)
   ;;If non-NIL the max number of lines to print.
   (line-limit :accessor line-limit)
   ;;number of next line to be printed.
   (line-no :accessor line-no)
   ;;NIL :UP :DOWN :CAP0 :CAP1 :CAPW
   (char-mode :accessor char-mode)
   ;;depth of nesting of ~(...~)
   (char-mode-counter :accessor char-mode-counter)
   ;;Number of logical blocks at QRIGHT that are started but not ended.
   (depth-in-blocks :accessor depth-in-blocks)

   ;;This stack is pushed and popped in accordance with the way blocks are
   ;;nested at the moment they are entered into the queue.  It contains the
   ;;following block specific value.
   ;;SECTION-START total position where the section (see AIM-1102)
   ;;that is rightmost in the queue started.
   (block-stack :accessor block-stack)
   (block-stack-ptr :accessor block-stack-ptr)

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
   (buffer :accessor buffer)
   (charpos :accessor charpos)
   (buffer-ptr :accessor buffer-ptr)
   (buffer-offset :accessor buffer-offset)

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
   (queue :accessor queue)
   (qleft :accessor qleft)
   (qright :accessor qright)

   ;;this stores the prefix that should be used at the start of the line
   (prefix :accessor prefix)

   ;;This stack is pushed and popped in accordance with the way blocks
   ;;are nested at the moment things are taken off the queue and printed.
   ;;It contains the following block specific values.
   ;;PREFIX-PTR current length of PREFIX.
   ;;SUFFIX-PTR current length of pending suffix
   ;;NON-BLANK-PREFIX-PTR current length of non-blank prefix.
   ;;INITIAL-PREFIX-PTR prefix-ptr at the start of this block.
   ;;SECTION-START-LINE line-no value at last non-literal break at this level.
   (prefix-stack :accessor prefix-stack)
   (prefix-stack-ptr :accessor prefix-stack-ptr)

   ;;this stores the suffixes that have to be printed to close of the current
   ;;open blocks.  For convenient in popping, the whole suffix
   ;;is stored in reverse order.
   (suffix :accessor suffix)

   ;; Integer if making a first pass over things to identify circularities.
   ;; Integer used as counter for #n= syntax.
   (locating-circularities :accessor locating-circularities :initarg :locating-circularities)))

(defmethod print-object ((object xp-structure) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if (not (base-stream object))
        (cl:format stream "not currently in use")
        (cl:format stream "outputting to ~S" (base-stream object)))))

(defun xp-structure-p (object)
  (typep object 'xp-structure))

(defmacro LP<-BP (xp &optional (ptr nil))
  (if (null ptr) (setf ptr `(buffer-ptr ,xp)))
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
          (intern (concatenate 'string "+" (string vect) "-MIN-SIZE+")
                  :mezzano.xp))
         (entry-size
          (intern (concatenate 'string "+" (string vect) "-ENTRY-SIZE+")
                  :mezzano.xp)))
    `(when (and (> ,ptr (- ,min-size ,entry-size)) ;seldom happens
                (> ,ptr (- (length (,vect ,xp)) ,entry-size)))
       (let* ((old (,vect ,xp))
              (new (make-array (+ ,ptr (if (= ,entry-size 1)
                                           50
                                           (* 10 ,entry-size)))
                               :element-type (array-element-type old))))
         (replace new old)
         (setf (,vect ,xp) new)))))

(defmacro section-start (xp) `(aref (block-stack ,xp) (block-stack-ptr ,xp)))

(defun push-block-stack (xp)
  (incf (block-stack-ptr xp) +block-stack-entry-size+)
  (check-size xp block-stack (block-stack-ptr xp)))

(defun pop-block-stack (xp)
  (decf (block-stack-ptr xp) +block-stack-entry-size+))

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
      (setf old-prefix (prefix-ptr xp)
            old-suffix (suffix-ptr xp)
            old-non-blank (non-blank-prefix-ptr xp)))
    (incf (prefix-stack-ptr xp) +prefix-stack-entry-size+)
    (check-size xp prefix-stack (prefix-stack-ptr xp))
    (setf (prefix-ptr xp) old-prefix)
    (setf (suffix-ptr xp) old-suffix)
    (setf (non-blank-prefix-ptr xp) old-non-blank)))

(defun pop-prefix-stack (xp)
  (decf (prefix-stack-ptr xp) +prefix-stack-entry-size+))

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
  (incf (Qright xp) +queue-entry-size+)
  (when (> (Qright xp) (- +queue-min-size+ +queue-entry-size+))
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

(defmacro Qnext (index) `(+ ,index +queue-entry-size+))

(defmethod describe-object ((xp xp-structure) s)
  (cl:format s "~S is an XP stream, " xp)
  (if (not (base-stream xp))
      (cl:format s "not currently in use")
      (cl:format s "outputting to ~S" (base-stream xp)))
  (when (base-stream xp)
    (cl:format s "~&buffer= ~S" (subseq (buffer xp) 0 (max (buffer-ptr xp) 0)))
    (cl:format s "~&   pos   _123456789_123456789_123456789_123456789")
    (cl:format s "~&depth-in-blocks= ~D linel= ~D line-no= ~D line-limit= ~D"
               (depth-in-blocks xp) (linel xp) (line-no xp) (line-limit xp))
    (when (or (char-mode xp) (not (zerop (char-mode-counter xp))))
      (cl:format s "~&char-mode= ~S char-mode-counter= ~D"
                 (char-mode xp) (char-mode-counter xp)))
    (unless (minusp (block-stack-ptr xp))
      (cl:format s "~&section-start")
      (do ((save (block-stack-ptr xp)))
          ((minusp (block-stack-ptr xp)) (setf (block-stack-ptr xp) save))
        (cl:format s " ~D" (section-start xp))
        (pop-block-stack xp)))
    (cl:format s "~&linel= ~D charpos= ~D buffer-ptr= ~D buffer-offset= ~D"
               (linel xp) (charpos xp) (buffer-ptr xp) (buffer-offset xp))
    (unless (minusp (prefix-stack-ptr xp))
      (cl:format s "~&prefix= ~S"
                 (subseq (prefix xp) 0 (max (prefix-ptr xp) 0)))
      (cl:format s "~&suffix= ~S"
                 (subseq (suffix xp) 0 (max (suffix-ptr xp) 0))))
    (unless (> (Qleft xp) (Qright xp))
      (cl:format s "~&ptr type         kind           pos depth end offset arg")
      (do ((p (Qleft xp) (Qnext p))) ((> p (Qright xp)))
        (cl:format s "~&~4A~13A~15A~4A~6A~4A~7A~A"
                   (/ (- p (Qleft xp)) +queue-entry-size+)
                   (Qtype xp p)
                   (if (member (Qtype xp p) '(:newline :ind)) (Qkind xp p) "")
                   (BP<-TP xp (Qpos xp p))
                   (Qdepth xp p)
                   (if (not (member (Qtype xp p) '(:newline :start-block))) ""
                       (and (Qend xp p)
                            (/ (- (+ p (Qend xp p)) (Qleft xp)) +queue-entry-size+)))
                   (if (not (eq (Qtype xp p) :start-block)) ""
                       (and (Qoffset xp p)
                            (/ (- (+ p (Qoffset xp p)) (Qleft xp)) +queue-entry-size+)))
                   (if (not (member (Qtype xp p) '(:ind :start-block :end-block))) ""
                       (Qarg xp p)))))
    (unless (minusp (prefix-stack-ptr xp))
      (cl:format s "~&initial-prefix-ptr prefix-ptr suffix-ptr non-blank start-line")
      (do ((save (prefix-stack-ptr xp)))
          ((minusp (prefix-stack-ptr xp)) (setf (prefix-stack-ptr xp) save))
        (cl:format s "~& ~19A~11A~11A~10A~A"
                   (initial-prefix-ptr xp) (prefix-ptr xp) (suffix-ptr xp)
                   (non-blank-prefix-ptr xp) (section-start-line xp))
        (pop-prefix-stack xp)))))

(defmethod initialize-instance :after ((xp xp-structure) &key)
  (setf (linel xp) (max 0 (cond (*print-right-margin*)
                                ((output-width (base-stream xp)))
                                (T *default-right-margin*))))
  (setf (line-limit xp) *print-lines*)
  (setf (line-no xp) 1)
  (setf (char-mode xp) nil)
  (setf (char-mode-counter xp) 0)
  (setf (depth-in-blocks xp) 0)
  (setf (block-stack xp) (make-array +block-stack-min-size+))
  (setf (block-stack-ptr xp) 0)
  (setf (buffer xp) (make-array +buffer-min-size+ :element-type 'character))
  ;; FIXME: If the output position can't be determined, then this will
  ;; incorrectly assume that the stream is at the start of the line, which
  ;; may be incorrect.
  ;; Printing a backtrace on the cold string will demonstrate this, as the
  ;; backtrace function calls fresh-line before the frame and does not print
  ;; a trailing newline. As XP believes that the stream is at the start of the
  ;; line every frame, newlines separating the frames never get printed and
  ;; the frames end up bunched together on one line.
  (setf (charpos xp) (cond ((output-position (base-stream xp)))
                           (T 0)))
  (setf (section-start xp) 0)
  (setf (buffer-ptr xp) 0)
  (setf (buffer-offset xp) (charpos xp))
  (setf (queue xp) (make-array +queue-min-size+))
  (setf (Qleft xp) 0)
  (setf (Qright xp) (- +queue-entry-size+))
  (setf (prefix xp) (make-array +buffer-min-size+ :element-type 'character))
  (setf (prefix-stack xp) (make-array +prefix-stack-min-size+))
  (setf (prefix-stack-ptr xp) (- +prefix-stack-entry-size+))
  (setf (suffix xp) (make-array +buffer-min-size+ :element-type 'character)))

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
    (loop (setf next-newline
                (position #\newline string :test #'char= :start start :end end))
          (setf sub-end (if next-newline next-newline end))
          (write-string++ string xp start sub-end)
          (when (null next-newline) (return nil))
          (pprint-newline+ :unconditional xp)
          (setf start (1+ sub-end)))))

;note this checks (> BUFFER-PTR LINEL) instead of (> (LP<-BP) LINEL)
;this is important so that when things are longer than a line they
;end up getting printed in chunks of size LINEL.

(defun write-char++ (char xp)
  (when (> (buffer-ptr xp) (linel xp))
    (force-some-output xp))
  (let ((new-buffer-end (1+ (buffer-ptr xp))))
    (check-size xp buffer new-buffer-end)
    (if (char-mode xp) (setf char (handle-char-mode xp char)))
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
        (if (char-mode xp) (setf char (handle-char-mode xp char)))
        (setf (char buffer i) char)))
    (setf (buffer-ptr xp) new-buffer-end)))

(defun pprint-tab+ (kind colnum colinc xp)
  (let ((indented? nil) (relative? nil))
    (case kind
      (:section (setf indented? T))
      (:line-relative (setf relative? T))
      (:section-relative (setf indented? T relative? T)))
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
      ((not (< ptr (Qright xp))))       ;all but last
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
      (setf prefix-string
            (subseq (buffer xp) (- (buffer-ptr xp) (length prefix-string))
                    (buffer-ptr xp))))
  (push-block-stack xp)
  (enqueue xp :start-block nil
           (if on-each-line? (cons suffix-string prefix-string) suffix-string))
  (incf (depth-in-blocks xp))         ;must be after enqueue
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
        (return nil)))  ;can only be 1
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
           (T (return nil)))))  ;wait until later to decide.

(defmacro misering? (xp)
  `(and *print-miser-width*
        (<= (- (linel ,xp) (initial-prefix-ptr ,xp)) *print-miser-width*)))

;If flush-out? is T and force-newlines? is NIL then the buffer,
;prefix-stack, and queue will be in an inconsistent state after the call.
;You better not call it this way except as the last act of outputting.

(defun attempt-to-output (xp force-newlines? flush-out?)
  (do () ((> (Qleft xp) (Qright xp))
          (setf (Qleft xp) 0)
          (setf (Qright xp) (- +queue-entry-size+))) ;saves shifting
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
  (unless (locating-circularities xp)
    (write-string
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
      (setf *abbreviation-happened* '*print-lines*)
      (throw 'line-limit-abbreviation-exit T))
    (incf (line-no xp))
    (unless (locating-circularities xp)
      (write-line
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

;                  ---- BASIC INTERFACE FUNCTIONS ----

;The internal functions in this file, and the (formatter "...") expansions
;use the '+' forms of these functions directly (which is faster) because,
;they do not need error checking of fancy stream coercion.  The '++' forms
;additionally assume the thing being output does not contain a newline.

(defun mezzano.internals::write-pretty (object stream)
  (cond ((xp-structure-p stream) (write+ object stream))
        (*print-pretty* (maybe-initiate-xp-printing
                          (lambda (s o) (write+ o s)) stream object))
        (T (write object :stream stream))))

(defun maybe-initiate-xp-printing (fn stream &rest args)
  (if (xp-structure-p stream)
      (apply fn stream args)
      (let ((*abbreviation-happened* nil)
            (*circularity-hash-table*
              (if *print-circle* (get-circularity-hash-table) nil))
            (*result* nil))
        (xp-print fn (decode-stream-arg stream) args)
        (when *abbreviation-happened*
          (setf *last-abbreviated-printing*
                (let ((current-package *package*)
                      (copied-args (copy-list args)))
                  (lambda (&optional (stream stream))
                    (let ((*package* current-package))
                      (apply #'maybe-initiate-xp-printing
                             fn stream copied-args))))))
        *result*)))

(defun xp-print (fn stream args)
  (when *print-circle*
    ;; First pass to locate circularities.
    (setf *result* (do-xp-printing fn stream args 0))
    (setf *abbreviation-happened* nil))
  (setf *result* (do-xp-printing fn stream args nil)))

(defun decode-stream-arg (stream)
  (cond ((eq stream T) *terminal-io*)
        ((null stream) *standard-output*)
        (T stream)))

(defun do-xp-printing (fn stream args locating-circularities)
  (let ((xp (make-instance 'xp-structure
                           :base-stream stream
                           :locating-circularities (if locating-circularities
                                                       0
                                                       nil)))
        (*current-level* 0)
        (result nil))
    (catch 'line-limit-abbreviation-exit
      (start-block xp nil nil nil)
      (setf result (apply fn xp args))
      (end-block xp nil))
    #++ ; disabled, think this is a performance thing...
    (when (and (locating-circularities xp)
               (zerop (locating-circularities xp)) ;No circularities.
               (= (line-no xp) 1)               ;Didn't suppress line.
               (zerop (buffer-offset xp)))      ;Didn't suppress partial line.
      (setf (locating-circularities xp) nil))      ;print what you have got.
    (when (catch 'line-limit-abbreviation-exit
            (attempt-to-output xp nil T) nil)
      (attempt-to-output xp T T))
    result))

(defun write+ (object xp)
  (unless (and *circularity-hash-table*
               (eq (circularity-process xp object nil) :subsequent))
    (when (and *circularity-hash-table* (consp object))
      ;;avoid possible double check in handle-logical-block.
      (setf object (cons (car object) (cdr object))))
    (let ((printer (if *print-pretty* (get-printer object *print-pprint-dispatch*) nil))
          #+(or)
          type)
      (cond (printer (funcall printer xp object))
            ((maybe-print-fast xp object))
            #+(or)
            ((and *print-pretty*
                  (symbolp (setf type (type-of object)))
                  (setf printer (get type 'structure-printer))
                  (not (eq printer :none)))
             (funcall printer xp object))
            ((and *print-pretty* *print-array* (arrayp object)
                  (not (stringp object)) (not (bit-vector-p object))
                  (not (structure-type-p (type-of object))))
             (pretty-array xp object))
            (T
             (non-pretty-print object xp))))))

(defun non-pretty-print (object s)
  (let ((*print-level* (if *print-level*
                           (- *print-level* *current-level*)))
        ;; The original behaviour was to rebind *print-pretty* to nil,
        ;; but that seemed to be so that the non-pretty-printer would be
        ;; invoked. Now it is invoked directly and we we don't actually
        ;; want to disable pretty printing, so leave *print-pretty* alone.
        #++ (*print-pretty* nil))
    (mezzano.internals::write-object object s)))

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
              (and (symbolp object)     ;Reader takes care of sharing.
                   (or (null *print-gensym*) (symbol-package object))))
    (let ((id (gethash object *circularity-hash-table*)))
      (if (locating-circularities xp)
          (cond ((null id)      ;never seen before
                 (setf (gethash object *circularity-hash-table*) 0)
                 nil)
                ((zerop id) ;possible second occurrence
                 (setf (gethash object *circularity-hash-table*)
                       (incf (locating-circularities xp)))
                 :subsequent)
                (T :subsequent));third or later occurrence
          (cond ((or (null id)  ;never seen before (note ~@* etc. conses)
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
               ((every (lambda (c) (not (or (char= c #\") (char= c #\\))))
                       object)
                (write-char++ #\" xp)
                (write-string+ object xp 0 (length object))
                (write-char++ #\" xp) T)))
        ((typep object 'fixnum)
         (when (and (null *print-radix*) (= *print-base* 10.))
           (when (minusp object)
             (write-char++ #\- xp)
             (setf object (- object)))
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
    (write-char++ (code-char (+ (char-code #\0) d)) xp)))

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

(defmethod mezzano.gray:stream-write-char ((stream xp-structure) char)
  (write-char+ char stream)
  char)

(defmethod mezzano.gray:stream-write-string ((stream xp-structure) string &optional (start 0) end)
  (write-string+ string stream start (or end (length string)))
  string)

(defmethod mezzano.gray:stream-terpri ((stream xp-structure))
  (pprint-newline+ :unconditional stream)
  nil)

;This has to violate the XP data abstraction and fool with internal
;stuff, in order to find out the right info to return as the result.

(defmethod mezzano.gray:stream-fresh-line ((stream xp-structure))
  (attempt-to-output stream T T) ;ok because we want newline
  (when (not (zerop (LP<-BP stream)))
    (pprint-newline+ :fresh stream)
    T))

;Each of these causes the stream to be pessimistic and insert
;newlines wherever it might have to, when forcing the partial output
;out.  This is so that things will be in a consistent state if
;output continues to the stream later.

(defmethod mezzano.gray:stream-finish-output ((stream xp-structure))
  (attempt-to-output stream T T)
  (finish-output (base-stream stream))
  nil)

(defmethod mezzano.gray:stream-force-output ((stream xp-structure))
  (attempt-to-output stream T T)
  (force-output (base-stream stream))
  nil)

(defmethod mezzano.gray:stream-clear-output ((stream xp-structure))
  (let ((old-l-c (locating-circularities stream)))
    (unwind-protect
         (progn (setf (locating-circularities stream) 0) ;hack to prevent visible output
                (attempt-to-output stream T T)
                (clear-output (base-stream stream)))
      (setf (locating-circularities stream) old-l-c)))
  nil)

;note we are assuming that if a structure is defined using xp::defstruct,
;then its print-function (if any) will be defined using xp::print etc.

#+(or)(defmacro defstruct (name &body body)
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
              (cl:defstruct ,name ,@ body)
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
                  (slots (mapcar (lambda (x) (if (consp x) (car x) x)) body)))
             `(eval-when (eval load compile)
                (cl:defstruct ,name ,@ body)
                (defun ,xp-print-fn (xp obj)
                  (funcall (formatter "~@<#S(~;~W ~:I~@_~@{:~A ~W~^ ~:_~}~;)~:>") xp
                           ',struct-name
                           ,@(mapcan (lambda (slot)
                                         `(,(string slot)
                                            (,(intern (concatenate 'string
                                                         conc-name (string slot)))
                                              obj)))
                                     slots)))
                (setf (get ',struct-name 'structure-printer) #',xp-print-fn)
                ',(if (consp name) (car name) name))))
          (T `(eval-when (eval load compile)
                (setf (get ',struct-name 'structure-printer) :none)
                (cl:defstruct ,name ,@ body))))))

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
  (cond ((eq stream-symbol nil) (setf stream-symbol '*standard-output*))
        ((eq stream-symbol T) (setf stream-symbol '*terminal-io*)))
  (when (not (symbolp stream-symbol))
    (warn "STREAM-SYMBOL arg ~S to PPRINT-LOGICAL-BLOCK is not a bindable symbol"
          stream-symbol)
    (setf stream-symbol '*standard-output*))
  (when (and prefix per-line-prefix)
    (warn "prefix ~S and per-line-prefix ~S cannot both be specified ~
           in PPRINT-LOGICAL-BLOCK"
          prefix per-line-prefix)
    (setf per-line-prefix nil))
  `(maybe-initiate-xp-printing
     (lambda (,stream-symbol)
         (let ((+l ,list)
               (+p ,(or prefix per-line-prefix ""))
               (+s ,suffix))
           (pprint-logical-block+
             (,stream-symbol +l +p +s ,(not (null per-line-prefix)) T nil)
             ,@ body nil)))
     (decode-stream-arg ,stream-symbol)))

;Assumes var and args must be variables.  Other arguments must be literals or variables.

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
         (setf *abbreviation-happened* T)
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
         (setf *abbreviation-happened* T)
         T)))

(defmacro pprint-logical-block+ ((var args prefix suffix per-line? circle-check? atsign?)
                                 &body body)
   (when (and circle-check? atsign?)
     (setf circle-check? 'not-first-p))
  `(let ((*current-level* (1+ *current-level*))
         (*current-length* -1)
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
  (setf stream (decode-stream-arg stream))
  (check-type kind (member :linear :miser :fill :mandatory))
  (when (xp-structure-p stream)
    (pprint-newline+ kind stream))
  nil)

(defun pprint-indent (relative-to n &optional (stream *standard-output*))
  (setf stream (decode-stream-arg stream))
  (check-type relative-to (member :block :current))
  (when (xp-structure-p stream)
    (pprint-indent+ relative-to n stream))
  nil)

(defun pprint-tab (kind colnum colinc &optional (stream *standard-output*))
  (setf stream (decode-stream-arg stream))
  (check-type kind (member :line :section :line-relative :section-relative))
  (when (xp-structure-p stream)
    (pprint-tab+ kind colnum colinc stream))
  nil)

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

(defun pretty-non-vector (xp array)
  (let* ((bottom (1- (array-rank array)))
         (indices (make-list (1+ bottom) :initial-element 0))
         (dims (array-dimensions array)))
    (labels ((pretty-slice (slice prefix)
               (pprint-logical-block (xp nil :prefix prefix :suffix ")")
                 (let ((end (nth slice dims))
                       (spot (nthcdr slice indices))
                       (i 0))
                   (when (plusp end)
                     (loop (pprint-pop)
                           (setf (car spot) i)
                           (if (= slice bottom)
                               (write+ (apply #'aref array indices) xp)
                               (pretty-slice (1+ slice) "("))
                           (if (= (incf i) end) (return nil))
                           (write-char++ #\space xp)
                           (pprint-newline+ (if (= slice bottom) :fill :linear) xp)))))))
      (pretty-slice 0 (format nil "#~DA(" (1+ bottom))))))

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
  (when (null tabsize) (setf tabsize 16))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
                                :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
          (pprint-exit-if-list-exhausted)
          (write-char++ #\space s)
          (pprint-tab+ :section-relative 0 tabsize s)
          (pprint-newline+ :fill s))))

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
