;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(declaim (special * ** ***
                  + ++ +++
                  / // ///
                  -))

(defun repl ()
  (let ((* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil)
        (+ nil) (++ nil) (+++ nil)
        (- nil))
    (loop
       (with-simple-restart (abort "Return to READ-EVAL-PRINT loop.")
         (fresh-line)
         (format t "~A> " (package-shortest-name *package*))
         (let ((form (read)))
           (fresh-line)
           (let ((result (multiple-value-list (let ((- form))
                                                (eval form)))))
             (setf *** **
                   ** *
                   * (first result)
                   /// //
                   // /
                   / result
                   +++ ++
                   ++ +
                   + form)
             (when result
               (dolist (v result)
                 (fresh-line)
                 (write v)))))))))

(defun hexdump-range (start end &optional stream)
  (when (< start end)
    (write-sequence (make-array (- end start)
                                :element-type '(unsigned-byte 8)
                                :memory (+ #x8000000000 start))
                    stream)))

(defgeneric documentation (x doc-type))
(defgeneric (setf documentation) (new-value x doc-type))

(defmethod documentation (x doc-type) nil)
(defmethod (setf documentation) (new-value x doc-type) new-value)

;; (%cpuid leaf ecx) -> eax ebx ecx edx
(sys.int::define-lap-function %cpuid ()
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.+n-fixnum-bits+)
  (sys.lap-x86:mov64 :rcx :r9)
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+)
  (sys.lap-x86:cpuid)
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 +n-fixnum-bits+))))
  (sys.lap-x86:lea64 :r9 ((:rbx #.(ash 1 +n-fixnum-bits+))))
  (sys.lap-x86:lea64 :r10 ((:rcx #.(ash 1 +n-fixnum-bits+))))
  (sys.lap-x86:lea64 :r11 ((:rdx #.(ash 1 +n-fixnum-bits+))))
  (sys.lap-x86:mov32 :ecx #.(ash 4 +n-fixnum-bits+))
  (sys.lap-x86:ret))

(defun cpuid (leaf &optional (rcx 0))
  (check-type leaf (unsigned-byte 32))
  (check-type rcx (unsigned-byte 32))
  (%cpuid leaf rcx))

(defun decode-cpuid-vendor (vendor-1 vendor-2 vendor-3)
  (let ((vendor (make-string (* 4 3))))
    (setf (char vendor 0) (code-char (ldb (byte 8 0) vendor-1))
          (char vendor 1) (code-char (ldb (byte 8 8) vendor-1))
          (char vendor 2) (code-char (ldb (byte 8 16) vendor-1))
          (char vendor 3) (code-char (ldb (byte 8 24) vendor-1))
          (char vendor 4) (code-char (ldb (byte 8 0) vendor-2))
          (char vendor 5) (code-char (ldb (byte 8 8) vendor-2))
          (char vendor 6) (code-char (ldb (byte 8 16) vendor-2))
          (char vendor 7) (code-char (ldb (byte 8 24) vendor-2))
          (char vendor 8) (code-char (ldb (byte 8 0) vendor-3))
          (char vendor 9) (code-char (ldb (byte 8 8) vendor-3))
          (char vendor 10) (code-char (ldb (byte 8 16) vendor-3))
          (char vendor 11) (code-char (ldb (byte 8 24) vendor-3)))
    vendor))

(defun make-string (size &key initial-element (element-type 'character))
  (if initial-element
      (make-array size :element-type element-type :initial-element initial-element)
      (make-array size :element-type element-type)))

(defvar *load-verbose* nil)
(defvar *load-print* nil)

(defun load-lisp-source (stream)
  (let ((*readtable* *readtable*)
        (*package* *package*)
        (*load-truename* stream)
        (*load-pathname* stream)
        (eof (cons nil nil)))
    (loop (let ((form (read stream nil eof)))
            (when (eql form eof) (return))
            (when *load-print* (format t ";; Loading ~S~%" form))
            (eval form)))
    t))

(defun load-from-stream (stream)
  (when *load-verbose*
    (format t ";;; Loading from ~S~%" stream))
  (if (subtypep (stream-element-type stream) 'character)
      (load-lisp-source stream)
      (mini-load-llf stream)))

(defvar *load-pathname* nil)
(defvar *load-truename* nil)

(defun load (filespec &key
             (verbose *load-verbose*)
             (print *load-print*)
             (if-does-not-exist t)
             (external-format :default))
  (let ((*load-verbose* verbose)
        (*load-print* print))
    (cond ((streamp filespec)
           (let* ((*load-pathname* (pathname filespec))
                  (*load-truename* (pathname filespec)))
             (load-from-stream filespec)))
          (t (let* ((path (merge-pathnames filespec))
                    (*load-pathname* (pathname path))
                    (*load-truename* (pathname path)))
               (with-open-file (stream filespec
                                       :if-does-not-exist (if if-does-not-exist
                                                              :error
                                                              nil)
                                       :element-type (if (string-equal (pathname-type path) "LLF")
                                                         '(unsigned-byte 8)
                                                         'character)
                                       :external-format (if (string-equal (pathname-type path) "LLF")
                                                            :default
                                                            external-format))
                 (when stream
                   (load-from-stream stream))))))))

(defmacro multiple-value-setq (vars form)
  (dolist (v vars)
    (check-type v symbol))
  `(values (setf (values ,@vars) ,form)))

(defun assemble-lap (code &optional name debug-info wired)
  (multiple-value-bind (mc constants fixups symbols gc-data)
      (sys.lap-x86:assemble code
        :base-address 16
        :initial-symbols '((nil . :fixup)
                           (t . :fixup)
                           (:unbound-value . :fixup)
                           (:unbound-tls-slot . :fixup)
                           (:undefined-function . :fixup)
                           (:closure-trampoline . :fixup))
        :info (list name debug-info))
    (declare (ignore fixups symbols))
    (make-function-with-fixups sys.int::+object-tag-function+ mc fixups constants gc-data wired)))

(defun lisp-implementation-type ()
  "Mezzano")

(defun lisp-implementation-version ()
  "devel")

;; (short-name long-name)
(defvar *site-info* nil)

(defun short-site-name () (first *site-info*))
(defun long-site-name () (second *site-info*))

;; (instance)
(defvar *machine-info* nil)
(defun machine-instance () (first *machine-info*))
(defun machine-type () "x86-64")
(defun machine-version ()
  (multiple-value-bind (cpuid-max vendor-1 vendor-3 vendor-2)
      (cpuid 0)
    (declare (ignore cpuid-max))
    (decode-cpuid-vendor vendor-1 vendor-2 vendor-3)))

;;; Mezzano uses no supporting software.
(defun software-type () nil)
(defun software-version () nil)

(unless (boundp 'lambda-list-keywords)
  (defconstant lambda-list-keywords '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

(defconstant array-rank-limit 8) ; ###
(defconstant array-dimension-limit (ash 1 48))
(defconstant array-total-size-limit (ash 1 48))

(defconstant char-code-limit #x110000)

(defconstant call-arguments-limit 500)
(defconstant lambda-parameters-limit 500)
(defconstant multiple-values-limit (+ (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+) 5))

(defconstant most-negative-short-float (%integer-as-single-float #xFF7FFFFF))
(defconstant most-negative-single-float (%integer-as-single-float #xFF7FFFFF))
(defconstant most-negative-double-float (%integer-as-single-float #xFF7FFFFF))
(defconstant most-negative-long-float (%integer-as-single-float #xFF7FFFFF))
(defconstant most-positive-short-float (%integer-as-single-float #x7F7FFFFF))
(defconstant most-positive-single-float (%integer-as-single-float #x7F7FFFFF))
(defconstant most-positive-double-float (%integer-as-single-float #x7F7FFFFF))
(defconstant most-positive-long-float (%integer-as-single-float #x7F7FFFFF))

(defun string-left-trim (character-bag string)
  (setf string (string string))
  (let ((n-from-left (dotimes (i (length string)
                               ;; All the characters must be trimmed!
                               (return-from string-left-trim ""))
                       (when (not (find (char string i) character-bag :test #'char=))
                         (return i)))))
    (if (zerop n-from-left)
        string
        (subseq string n-from-left))))

(defvar *modules* '())
(defvar *require-hooks* '())

(defun provide (module-name)
  (pushnew (string module-name) *modules*
           :test #'string=)
  (values))

(defun require (module-name &optional pathname-list)
  (unless (member (string module-name) *modules*
                  :test #'string=)
    (if pathname-list
        (if (listp pathname-list)
            (dolist (pathname pathname-list)
              (load pathname))
            (load pathname-list))
        (dolist (hook *require-hooks*
                 (error "Unable to REQUIRE module ~A." module-name))
          (when (funcall hook module-name)
            (return)))))
  (values))

(defmethod print-object ((c cell-error) s)
  (print-unreadable-object (c s :type t)
    (write (cell-error-name c) :stream s)))

(defmethod print-object ((c simple-condition) s)
  (print-unreadable-object (c s :type t)
    (apply #'format s
           (simple-condition-format-control c)
           (simple-condition-format-arguments c))))

(defmethod print-object ((object package) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~:(~S~)" (package-shortest-name object))))

(defmethod print-object ((o structure-definition) stream)
  (print-unreadable-object (o stream :identity t :type t)
    (write (structure-name o) :stream stream)))

(defun pprint-indent (relative-to n &optional stream)
  (check-type relative-to (member :block :current))
  (check-type n real)
  nil)

(defun pprint-newline (kind &optional stream)
  (check-type kind (member :linear :fill :miser :mandatory))
  nil)

(defun pprint-tab (kind colnum colinc &optional stream)
  (check-type kind (member :line :section :line-relative :section-relative))
  (check-type colnum (integer 0))
  (check-type colinc (integer 0))
  nil)

(defmacro pprint-logical-block ((stream-symbol
                                 object
                                 &key
                                 (prefix nil prefix-p)
                                 (per-line-prefix nil per-line-prefix-p)
                                 (suffix ""))
                                &body body)
  (check-type stream-symbol symbol)
  (assert (not (and prefix-p per-line-prefix-p)) ()
          ":PREFIX and :PER-LINE-PREFIX are mutually exclusive.")
  (let ((obj-sym (gensym "object"))
        (block-name (gensym "pprint-logical-block"))
        (pop-count (gensym "pop-count"))
        (pprint-pop-fn (gensym "pprint-pop")))
    (case stream-symbol
      ((t) (setf stream-symbol '*terminal-io*))
      ((nil) (setf stream-symbol '*standard-output*)))
    `(%pprint-logical-block ,stream-symbol ,object
                            ;; FIXME: when prefix is supplied, make sure it
                            ;; does not evaluate to NIL before passing in.
                            ,(cond (prefix-p prefix)
                                   ((not per-line-prefix-p) "")
                                   (t nil))
                            ;; Same here.
                            ,per-line-prefix
                            ,suffix
                            ;; FIXME: Declares & stuff.
                            (lambda (,stream-symbol ,obj-sym &aux (,pop-count 0))
                              (declare (ignorable ,obj-sym))
                              (block ,block-name
                                (flet ((,pprint-pop-fn ()
                                         (cond ((or (not (listp ,obj-sym))
                                                    (circlep ,obj-sym))
                                                (write-string ". " ,stream-symbol)
                                                (write ,obj-sym :stream ,stream-symbol)
                                                (return-from ,block-name))
                                               ((and *print-length* (>= ,pop-count *print-length*))
                                                (write-string "..." ,stream-symbol)
                                                (return-from ,block-name))
                                               (t (incf ,pop-count)
                                                  (pop ,obj-sym)))))
                                  (macrolet ((pprint-pop ()
                                               (list ',pprint-pop-fn))
                                             (pprint-exit-if-list-exhausted ()
                                               `(when (null ,',obj-sym)
                                                  (return-from ,',block-name))))
                                    ,@body)))))))

(defun %pprint-logical-block (base-stream object prefix per-line-prefix suffix fn)
  (declare (ignore per-line-prefix))
  (when prefix (write-string prefix base-stream))
  (funcall fn base-stream object)
  (when suffix (write-string suffix base-stream))
  nil)

(defun circlep (object)
  (declare (ignore object))
  nil)

(defstruct (random-state
             (:constructor %make-random-state (bits)))
  bits)

(defvar *random-state* (%make-random-state 0))

(defun make-random-state (&optional state)
  (case state
    ((t) (%make-random-state 0))
    ((nil) (copy-random-state *random-state*))
    (otherwise (copy-random-state state))))

(defun random (limit &optional (random-state *random-state*))
  (rem (incf (random-state-bits random-state)) limit))

(defun set (symbol value)
  (setf (symbol-value symbol) value))

(defun remprop (symbol indicator)
  (remf (symbol-plist symbol) indicator))

(defun map-apropos (fn string package)
  (cond (package
         (do-symbols (sym package)
           (when (search string (symbol-name sym) :test #'string-equal)
             (funcall fn sym))))
        (t
         (do-all-symbols (sym)
           (when (search string (symbol-name sym) :test #'string-equal)
             (funcall fn sym))))))

(defun apropos (string &optional package)
  (map-apropos (lambda (sym)
                 (let ((info '()))
                   (when (boundp sym) (push "bound" info))
                   (when (fboundp sym) (push "fbound" info))
                   (if info
                       (format t "~S ~A~%" sym info)
                       (format t "~S~%" sym))))
               string package))

(defun apropos-list (string &optional package)
  (let ((syms '()))
    (map-apropos (lambda (sym)
                   (pushnew sym syms))
                 string package)
    syms))

;;;; MERGE, from SBCL.

;;; Destructively merge LIST-1 with LIST-2 (given that they're already
;;; sorted w.r.t. PRED-FUN on KEY-FUN, giving output sorted the same
;;; way). In the resulting list, elements of LIST-1 are guaranteed to
;;; come before equal elements of LIST-2.
;;;
;;; Enqueues the values in the right order in HEAD's cdr, and returns
;;; the merged list.
(defun merge-lists* (head list1 list2 test key &aux (tail head))
  (declare (type cons head list1 list2)
           (type function test key)
           (optimize speed))
  (let ((key1 (funcall key (car list1)))
        (key2 (funcall key (car list2))))
    (macrolet ((merge-one (l1 k1 l2)
                 `(progn
                    (setf (cdr tail) ,l1
                          tail       ,l1)
                    (let ((rest (cdr ,l1)))
                      (cond (rest
                             (setf ,l1 rest
                                   ,k1 (funcall key (first rest))))
                            (t
                             (setf (cdr ,l1) ,l2)
                             (return (cdr head))))))))
      (loop
       (if (funcall test key2           ; this way, equivalent
                         key1)          ; values are first popped
           (merge-one list2 key2 list1) ; from list1
           (merge-one list1 key1 list2))))))

;;; Convenience wrapper for CL:MERGE
(declaim (inline merge-lists))
(defun merge-lists (list1 list2 test key)
  (cond ((null list1)
         list2)
        ((null list2)
         list1)
        (t
         (let ((head (cons nil nil)))
           (declare (dynamic-extent head))
           (merge-lists* head list1 list2 test key)))))

(defmacro funcall2-using-key (pred key one two)
  `(if ,key
       (funcall ,pred (funcall ,key ,one)
                (funcall ,key  ,two))
       (funcall ,pred ,one ,two)))

;;; MERGE-VECTORS returns a new vector which contains an interleaving
;;; of the elements of VECTOR-1 and VECTOR-2. Elements from VECTOR-2
;;; are chosen only if they are strictly less than elements of
;;; VECTOR-1, (PRED ELT-2 ELT-1), as specified in the manual.
(defmacro merge-vectors (vector-1 length-1 vector-2 length-2
                               result-vector pred key access)
  (let ((result-i (gensym))
        (i (gensym))
        (j (gensym)))
    `(let* ((,result-i 0)
            (,i 0)
            (,j 0))
       (declare (fixnum ,result-i ,i ,j))
       (loop
        (cond ((= ,i ,length-1)
               (loop (if (= ,j ,length-2) (return))
                     (setf (,access ,result-vector ,result-i)
                           (,access ,vector-2 ,j))
                     (incf ,result-i)
                     (incf ,j))
               (return ,result-vector))
              ((= ,j ,length-2)
               (loop (if (= ,i ,length-1) (return))
                     (setf (,access ,result-vector ,result-i)
                           (,access ,vector-1 ,i))
                     (incf ,result-i)
                     (incf ,i))
               (return ,result-vector))
              ((funcall2-using-key ,pred ,key
                                   (,access ,vector-2 ,j) (,access ,vector-1 ,i))
               (setf (,access ,result-vector ,result-i)
                     (,access ,vector-2 ,j))
               (incf ,j))
              (t (setf (,access ,result-vector ,result-i)
                       (,access ,vector-1 ,i))
                 (incf ,i)))
        (incf ,result-i)))))

(defun make-sequence (result-type size &key (initial-element nil initial-element-p))
  (cond ((subtypep result-type 'list)
         (loop for i below size collect initial-element))
        ((subtypep result-type 'vector)
         (if initial-element-p
             (make-array size
                         :element-type (or (coerce-vector-element-type result-type) 't)
                         :initial-element initial-element)
             (make-array size
                         :element-type (or (coerce-vector-element-type result-type) 't))))
        (t (error "Type ~S is not a recognized sequence type." result-type))))

(defun merge (result-type sequence1 sequence2 predicate &key key)
  "Merge the sequences SEQUENCE1 and SEQUENCE2 destructively into a
   sequence of type RESULT-TYPE using PREDICATE to order the elements."
  ;; FIXME: This implementation is remarkably inefficient in various
  ;; ways. In decreasing order of estimated user astonishment, I note:
  ;; full calls to SPECIFIER-TYPE at runtime; copying input vectors
  ;; to lists before doing MERGE-LISTS -- WHN 2003-01-05
  (cond
    ((subtypep result-type 'list)
     ;; the VECTOR clause, below, goes through MAKE-SEQUENCE, so
     ;; benefits from the error checking there. Short of
     ;; reimplementing everything, we can't do the same for the LIST
     ;; case, so do relevant length checking here:
     (let ((s1 (coerce sequence1 'list))
           (s2 (coerce sequence2 'list))
           (pred-fun predicate)
           (key-fun (or key
                        #'identity)))
       (when (subtypep 'list result-type) ; result-type = 'list
         (return-from merge (merge-lists s1 s2 pred-fun key-fun)))
       (when (and (subtypep 'null result-type)
                  (subtypep result-type 'null))
         (if (and (null s1) (null s2))
             (return-from merge 'nil)
             ;; FIXME: This will break on circular lists (as,
             ;; indeed, will the whole MERGE function).
             (error "MERGE result type has too few elements.")))
       (error "Result type ~S looks a bit like 'LIST, but is too complicated!" result-type)))
    ((subtypep result-type 'vector)
     (let* ((vector-1 (coerce sequence1 'vector))
            (vector-2 (coerce sequence2 'vector))
            (length-1 (length vector-1))
            (length-2 (length vector-2))
            (result (make-sequence result-type (+ length-1 length-2))))
       (declare (vector vector-1 vector-2)
                (fixnum length-1 length-2))
       (if (and (simple-vector-p result)
                (simple-vector-p vector-1)
                (simple-vector-p vector-2))
           (merge-vectors vector-1 length-1 vector-2 length-2
                          result predicate key svref)
           (merge-vectors vector-1 length-1 vector-2 length-2
                          result predicate key aref))))
    (t (error "Unknown MERGE result-type ~S." result-type))))

(defmethod print-object ((object mezzano.supervisor::nic) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~:(~A~) ~/sys.net::format-mac-address/"
            (type-of (mezzano.supervisor::nic-device object))
            (mezzano.supervisor:nic-mac object))))

(defmethod print-object ((object mezzano.supervisor::disk) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "Disk")
    (when (typep (mezzano.supervisor::disk-device object) 'mezzano.supervisor::partition)
      (format stream " Partition on ~S" (mezzano.supervisor::partition-disk (mezzano.supervisor::disk-device object))))))

(defun copy-pprint-dispatch (&optional table))
(defun set-pprint-dispatch (type-specifier function &optional (priority 0) table))
