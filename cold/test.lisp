(in-package #:sys.int)

(declaim (special *cold-toplevel-forms*
                  *package-system*
                  *additional-cold-toplevel-forms*
                  *initial-obarray*
                  *initial-keyword-obarray*
                  *initial-setf-obarray*
                  *initial-structure-obarray*
                  *kboot-tag-list*
                  *multiboot-info*)
         (special *terminal-io*
                  *standard-output*
                  *standard-input*
                  *debug-io*
                  *cold-stream-screen*
                  *keyboard-shifted*))

(defun write-char (character &optional stream)
  (cold-write-char character stream))

(defun start-line-p (stream)
  (cold-start-line-p stream))

(defun read-char (&optional stream (eof-error-p t) eof-value recursive-p)
  (cold-read-char stream))

(defun unread-char (character &optional stream)
  (cold-unread-char character stream))

(defun peek-char (&optional peek-type s (eof-error-p t) eof-value recursive-p)
  (cond ((eql peek-type nil)
         (let ((ch (cold-read-char s)))
           (cold-unread-char ch s)
           ch))
        ((eql peek-type t)
         (do ((ch (cold-read-char s)
                  (cold-read-char s)))
             ((not (whitespace[2]p ch))
              (cold-unread-char ch s)
              ch)))
        ((characterp peek-type)
         (error "TODO: character peek."))
        (t (error "Bad peek type ~S." peek-type))))

(defun read-line (&optional (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (do ((result (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
       (c (read-char input-stream eof-error-p nil recursive-p)
          (read-char input-stream eof-error-p nil recursive-p)))
      ((or (null c)
           (eql c #\Newline))
       (if (and (null c) (eql (length result) 0))
           (values eof-value t)
           (values result (null c))))
    (vector-push-extend c result)))

(defun yes-or-no-p (&optional control &rest arguments)
  (declare (dynamic-extent arguments))
  (when control
    (write-char #\Newline)
    (apply 'format t control arguments)
    (write-char #\Space))
  (format t "(Yes or No) ")
  (loop
     (let ((line (read-line)))
       (when (string-equal line "yes")
         (return t))
       (when (string-equal line "no")
         (return nil)))
     (write-char #\Newline)
     (format t "Please respond with \"yes\" or \"no\". ")))

(defvar *cold-stream*)
(defun streamp (object)
  (eql object *cold-stream*))

(defun backtrace (&optional limit)
  (do ((i 0 (1+ i))
       (fp (read-frame-pointer)
           (memref-unsigned-byte-64 fp 0)))
      ((or (and limit (> i limit))
           (= fp 0)))
    (write-char #\Newline)
    (write-integer fp 16)
    (write-char #\Space)
    (let* ((fn (memref-t fp -2))
           (name (when (functionp fn) (function-name fn))))
      (write-integer (lisp-object-address fn) 16)
      (when name
        (write-char #\Space)
        (write name)))))

(defun raise-overflow (&optional lhs rhs what)
  (write-char #\!)
  (write-char #\%)
  (write what)
  (write-char #\Space)
  (write lhs)
  (write-char #\Space)
  (write rhs)
  (backtrace)
  (loop (%hlt)))

(defun error (datum &rest arguments)
  (write-char #\!)
  (write datum)
  (write-char #\Space)
  (write arguments)
  (backtrace)
  (loop (%hlt)))

(defun pathnamep (x) nil)
(defun pathnames-equal (x y) nil)

;; NOTE: incomplete.
(defun equal (x y)
  (cond
    ((eql x y))
    ((stringp x)
     (and (stringp y)
          (string= x y)))
    ((consp x)
     (and (consp y)
	  (equal (car x) (car y))
	  (equal (cdr x) (cdr y))))
    ((and (pathnamep x) (pathnamep y))
     (pathnames-equal x y))))

(defun equalp (x y)
  (typecase x
    (character (and (characterp y)
                    (char-equal x y)))
    (number (and (numberp y)
                 (= x y)))
    (cons (and (consp y)
               (equalp (car x) (car y))
               (equalp (cdr x) (cdr y))))
    (vector (and (vectorp y)
                 (eql (length x) (length y))
                 (dotimes (i (length x) t)
                   (when (not (equalp (aref x i) (aref y i)))
                     (return nil)))))
    (array (and (arrayp y)
                (equalp (array-dimensions x) (array-dimensions y))
                (dotimes (i (array-total-size x) t)
                  (when (not (equalp (row-major-aref x i) (row-major-aref y i)))
                    (return nil)))))
    ;; TODO: structures and hash-tables.
    (t (eq x y))))

(defun %with-stream-editor (stream recursive-p function)
  (funcall function))

(defun find-package-or-die (name)
  t)
(defun find-package (name)
  t)
(defun keywordp (object)
  (find object *initial-keyword-obarray*))
(defun find-symbol (name &optional package)
  (let ((pkg (if (string= package "KEYWORD")
                 *initial-keyword-obarray*
                 *initial-obarray*)))
    (dotimes (i (length pkg) (values nil nil))
      (when (string= name (symbol-name (aref pkg i)))
        (return (values (aref pkg i) :internal))))))
(defun cold-intern (name &optional package)
  (let ((pkg (if (string= package "KEYWORD")
                 *initial-keyword-obarray*
                 *initial-obarray*)))
    (dotimes (i (length pkg)
              (let ((sym (make-symbol (string name))))
                (setf (symbol-package sym) (if (string= package "KEYWORD") :keyword t))
                (when (string= package "KEYWORD")
                  (setf (symbol-value sym) sym
                        (symbol-mode sym) :constant))
                (vector-push-extend sym pkg)
                (values sym nil)))
      (when (string= name (symbol-name (aref pkg i)))
        (return (values (aref pkg i) :internal))))))
(defun package-name (package)
  (ecase package
    ((t) "SYSTEM")
    ((:keyword) "KEYWORD")))

(defun %defmacro (name function)
  (funcall #'(setf macro-function) function name))

(defun %compiler-defun (name source-lambda)
  (let ((sym (function-symbol name)))
    (when (or (get sym 'inline-mode)
              (get sym 'inline-form))
      (setf (get sym 'inline-form) source-lambda)))
  nil)

(defun %defun (name lambda)
  (setf (fdefinition name) lambda)
  name)

;; TODO: Symbol macros, macroexpand-hook.
(defun macroexpand-1 (form &optional env)
  (if (consp form)
      (let ((mf (macro-function (car form) env)))
	(if mf
	    (values (funcall mf form env) t)
	    (values form nil)))
      (values form nil)))

(defun macroexpand (form &optional env)
  (let ((did-expand nil))
    (do () (nil)
       (multiple-value-bind (expansion expanded-p)
           (macroexpand-1 form env)
         (if expanded-p
             (setf form expansion
                   did-expand t)
             (return (values form did-expand)))))))

(defun %defstruct (structure-type)
  (setf (get (structure-name structure-type) 'structure-type) structure-type))

(defun function-symbol (name)
  "Convert a function name to a symbol."
  (cond ((symbolp name) name)
	((and (consp name)
	      (= (list-length name) 2)
	      (eql (first name) 'setf)
	      (symbolp (second name)))
	 (let ((sym (get (second name) 'setf-symbol)))
	   (unless sym
	     (setf sym (make-symbol (symbol-name (second name)))
                   (get sym 'setf-symbol-backlink) (second name)
		   (get (second name) 'setf-symbol) sym))
	   sym))
	(t (error "Invalid function name ~S." name))))

(defun fdefinition (name)
  (symbol-function (function-symbol name)))

(defun (setf fdefinition) (value name)
  (setf (symbol-function (function-symbol name)) value))

(defun compiler-macro-function (name &optional environment)
  (get (function-symbol name) 'compiler-macro-function))

(defun (setf compiler-macro-function) (value name &optional environment)
  (setf (get (function-symbol name) 'compiler-macro-function) value))

(defun simplify-string (string)
  (if (simple-string-p string)
      string
      (make-array (length string)
                  :element-type (if (every 'sys.int::base-char-p string)
                                    'base-char
                                    'character)
                  :initial-contents string)))

(declaim (inline identity))
(defun identity (thing)
  thing)

(declaim (inline complement))
(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(defun set-difference (list-1 list-2)
  (let ((result '()))
    (dolist (e list-1)
      (when (not (member e list-2))
	(setq result (cons e result))))
    result))

(defun union (list-1 list-2)
  (let ((result (copy-list list-1)))
    (dolist (e list-2)
      (when (not (member e list-1))
	(setq result (cons e result))))
    result))

(defun intersection (list-1 list-2)
  (when list-1
    (if (member (first list-1) list-2)
        (cons (first list-1) (intersection (rest list-1) list-2))
        (intersection (rest list-1) list-2))))

(declaim (special * ** ***))

(defun repl ()
  (let ((* nil) (** nil) (*** nil))
    (loop
       (fresh-line)
       (write-char #\>)
       (let ((form (read)))
         (fresh-line)
         (let ((result (multiple-value-list (eval form))))
           (setf *** **
                 ** *
                 * (first result))
           (when result
             (dolist (v result)
               (fresh-line)
               (write v))))))))

(defvar *early-initialize-hook* '())
(defvar *initialize-hook* '())

(defun add-hook (hook function)
  (unless (boundp hook)
    (setf (symbol-value hook) '()))
  (pushnew function (symbol-value hook)))

(defun set-gc-light ())
(defun clear-gc-light ())

(defun emergency-halt (message)
  (%cli)
  (dotimes (i (%simple-array-length message))
    (let ((code (logand (char-code (schar message i)) #xFF)))
      (setf (io-port/8 #xE9) code)
      (setf (io-port/8 #x3F8) code)
      (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 i)
            (logior code #x7000))))
  (loop (%hlt)))

(defun gc-trace (object direction prefix)
  (setf (io-port/8 #xE9) (char-code direction))
  (setf (io-port/8 #xE9) (logand (char-code prefix) #xFF))
  (let ((pointer (%pointer-field object))
        (tag (%tag-field object)))
    (dotimes (i 15)
      (setf (io-port/8 #xE9) (hexify (logand (ash pointer (* -4 (- 14 i))) #b1111))))
    (setf (io-port/8 #xE9) (hexify tag))
    (setf (io-port/8 #xE9) #x0A)))

(defun mumble (message)
  (mumble-string message)
  (setf (io-port/8 #xE9) #x0A))

;;; Used while the GC is copying, so no lookup tables.
(defun hexify (nibble)
  (cond ((<= 0 nibble 9)
         (+ nibble (char-code #\0)))
        (t (+ (- nibble 10) (char-code #\A)))))

(defun mumble-string (message)
  (dotimes (i (%simple-array-length message))
    (let ((code (logand (char-code (schar message i)) #xFF)))
      (setf (io-port/8 #xE9) code)
      (setf (io-port/8 #x3F8) code)
      (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 i)
            (logior code #x7000)))))
(defun mumble-hex (number)
  (dotimes (i 16)
    (setf (io-port/8 #xE9) (hexify (logand (ash number (* -4 (- 15 i))) #b1111)))))

(defun make-case-correcting-stream (stream case)
  stream)

(defun describe-symbol (object stream)
  (format stream "~S is a symbol, with address ~X~%" object (lisp-object-address object))
  (if (symbol-package object)
      (format stream "  ~A is in the ~A package~%" object (package-name (symbol-package object))))
  (if (boundp object)
      (format stream "  ~A has the value ~S~%" object (symbol-value object)))
  (if (fboundp object)
      (format stream "  ~A is the function ~S~%" object (symbol-function object)))
  (if (symbol-plist object)
      (format stream "  ~A has the plist ~S~%" object (symbol-plist object)))
  (when (symbol-mode object)
    (format stream "  ~A is declared ~A~%" object (symbol-mode object)))
  (when (symbol-tls-slot object)
    (format stream "  ~A uses the TLS slot ~D~%" (symbol-tls-slot object))))

(defun describe-character (object stream)
  (format stream "~S is a ~S~%" object (type-of object)))

(defun describe-float (object stream)
  (format stream "~D is a single-precision floating-point number.~%" object))

(defun describe-function (object stream)
  (multiple-value-bind (lambda-expression closure-p name)
      (function-lambda-expression object)
    (format stream "~S is a ~A" object (if closure-p "closure" "function"))
    (when name
      (format stream " named ~S" name))
    (format stream "~%")))

(defun describe-array (object stream)
  (format stream "~S is a~A array of ~S, with address ~X~%" object
          (if (typep object 'simple-array) " simple" "n")
          (array-element-type object)
          (lisp-object-address object))
  (format stream "  It is ~D elements long.~%" (array-total-size object))
  (if (array-has-fill-pointer-p object)
      (format stream "  It has a fill-pointer of ~S.~%" (fill-pointer object)))
  (format stream "  It has dimensions ~S.~%" (array-dimensions object)))

(defun describe-bignum (object stream)
  (format stream "~S is a bignum, with address ~X~%"
          object (lisp-object-address object)))

(defun describe-structure (object stream)
  (format stream "~S is a structure, with address ~X~%"
          object (lisp-object-address object)))

(defun describe-stack-group (object stream)
  (format stream "~S is a stack-group, with address ~X~%"
          object (lisp-object-address object)))

(defun describe (object &optional (stream *standard-output*))
  (case stream
    ((nil) (setf stream *standard-output*))
    ((t) (setf stream *terminal-io*)))
  (case (logand (lisp-object-address object) 15)
    (#b0000 (format stream "~D is an even fixnum.~%" object))
    ;; TODO: Identify proper/dotted/circular lists.
    (#b0001 (format stream "~S is a list, with address ~X~%" object (lisp-object-address object)))
    (#b0010 (describe-symbol object stream))
    (#b0011 (describe-array object stream)) ; simple array.
    ;; #b0100
    ;; #b0101
    ;; #b0110
    (#b0111 (cond ((structure-object-p object)
                   (describe-structure object stream))
                  ((stack-group-p object)
                   (describe-stack-group object stream))
                  ((std-instance-p object)
                   (describe-object object stream))
                  ((bignump object)
                   (describe-bignum object stream))
                  (t (describe-array object stream))))
    (#b1000 (format stream "~D is an odd fixnum.~%" object))
    ;; #b1001
    (#b1010 (describe-character object stream))
    (#b1011 (describe-float object stream))
    (#b1100 (if (funcallable-std-instance-p object)
                (describe-object object stream)
                (describe-function object stream)))
    ;; #b1101
    (#b1110 (format stream "This is an unbound value marker.~%"))
    (#b1111 (format stream "This is a GC forwarding pointer, pointing to address ~X~%" (logand (lisp-object-address object)
                                                                                               (lognot #xF))))
    (t (format stream "~S is an unknown/invalid object, with address ~X~%" object (lisp-object-address object))))
  (values))

(declaim (special *features* *macroexpand-hook*))

(defun multiboot-module-count ()
  (if (and *multiboot-info*
           (logtest (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 0) +multiboot-flag-modules+))
      (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 5)
      0))

(defun multiboot-module-info ()
  (if (and *multiboot-info*
           (logtest (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 0) +multiboot-flag-modules+))
      (make-array (* (multiboot-module-count) 4)
                  :element-type '(unsigned-byte 32)
                  :memory (+ #x8000000000 (memref-unsigned-byte-32 (+ *multiboot-info* #x8000000000) 6)))
      (make-array 0 :element-type '(unsigned-byte 32))))

(defparameter *maximum-multiboot-command-line-length* 100)

(defun multiboot-module (id)
  (assert (< id (multiboot-module-count)))
  (let* ((info (multiboot-module-info))
         (base (aref info (+ (* id 4) 0)))
         (end (aref info (+ (* id 4) 1)))
         (size (- end base))
         (command-line (+ #x8000000000 (aref info (+ (* id 4) 2))))
         (cmdl-len (dotimes (i *maximum-multiboot-command-line-length*
                             *maximum-multiboot-command-line-length*)
                     (when (eql (memref-unsigned-byte-8 command-line i) 0)
                       (return i)))))
    (values (make-array size
                        :element-type '(unsigned-byte 8)
                        :memory (+ #x8000000000 base))
            (make-array cmdl-len
                        :element-type 'base-char
                        :memory command-line))))

(defun multiboot-module-data (id)
  (multiple-value-bind (data command-line)
      (multiboot-module id)
    (declare (ignore command-line))
    data))

(defun multiboot-module-command-line (id)
  (multiple-value-bind (data command-line)
      (multiboot-module id)
    (declare (ignore data))
    command-line))

(defun mini-multiboot-module-stream (id)
  (mini-vector-stream (multiboot-module-data id)))

(defun mini-vector-stream (vector)
  (cons vector 0))

(defun %read-byte (stream)
  (prog1 (aref (car stream) (cdr stream))
    (incf (cdr stream))))

(defun %read-sequence (seq stream)
  (replace seq (car stream) :start2 (cdr stream) :end2 (+ (cdr stream) (length seq)))
  (incf (cdr stream) (length seq)))

(defun %defconstant (name value &optional docstring)
  (proclaim `(special ,name))
  (setf (symbol-value name) value)
  (proclaim `(constant ,name))
  name)

(defun enter-debugger (condition)
  (write-char #\!)
  (write condition)
  (backtrace)
  (loop (%hlt)))

(defun invoke-debugger (condition)
  (write-char #\!)
  (write condition)
  (backtrace)
  (loop (%hlt)))

(defun round-up (n boundary)
  (if (zerop (rem n boundary))
      n
      (+ n boundary (- (rem n boundary)))))

(defun ub16ref/be (vector index)
  (logior (ash (aref vector index) 8)
	  (aref vector (1+ index))))
(defun (setf ub16ref/be) (value vector index)
  (setf (aref vector index) (ash value -8)
	(aref vector (1+ index)) (logand value #xFF))
  value)

(defun ub16ref/le (vector index)
  (logior (aref vector index)
	  (ash (aref vector (1+ index)) 8)))
(defun (setf ub16ref/le) (value vector index)
  (setf (aref vector index) (logand value #xFF)
	(aref vector (1+ index)) (ash value -8))
  value)

(defun ub32ref/be (vector index)
  (logior (ash (aref vector index) 24)
	  (ash (aref vector (+ index 1)) 16)
	  (ash (aref vector (+ index 2)) 8)
	  (aref vector (+ index 3))))
(defun (setf ub32ref/be) (value vector index)
  (setf (aref vector index) (ash value -24)
	(aref vector (+ index 1)) (logand (ash value -16) #xFF)
	(aref vector (+ index 2)) (logand (ash value -8) #xFF)
	(aref vector (+ index 3)) (logand value #xFF))
  value)

(defun ub32ref/le (vector index)
  (logior (aref vector index)
	  (ash (aref vector (+ index 1)) 8)
	  (ash (aref vector (+ index 2)) 16)
	  (ash (aref vector (+ index 3)) 24)))
(defun (setf ub32ref/le) (value vector index)
  (setf (aref vector index) (logand value #xFF)
	(aref vector (+ index 1)) (logand (ash value -8) #xFF)
	(aref vector (+ index 2)) (logand (ash value -16) #xFF)
	(aref vector (+ index 3)) (ash value -24))
  value)

(defun ub64ref/le (vector index)
  (logior (aref vector index)
	  (ash (aref vector (+ index 1)) 8)
	  (ash (aref vector (+ index 2)) 16)
	  (ash (aref vector (+ index 3)) 24)
	  (ash (aref vector (+ index 3)) 32)
	  (ash (aref vector (+ index 3)) 40)
	  (ash (aref vector (+ index 3)) 48)
	  (ash (aref vector (+ index 3)) 56)))
(defun (setf ub64ref/le) (value vector index)
  (setf (aref vector index) (ldb (byte 8 0) value)
	(aref vector (+ index 1)) (ldb (byte 8 8) value)
	(aref vector (+ index 2)) (ldb (byte 8 16) value)
	(aref vector (+ index 3)) (ldb (byte 8 24) value)
	(aref vector (+ index 4)) (ldb (byte 8 32) value)
	(aref vector (+ index 5)) (ldb (byte 8 40) value)
	(aref vector (+ index 6)) (ldb (byte 8 48) value)
	(aref vector (+ index 7)) (ldb (byte 8 56) value))
  value)

(defun format-uuid (stream argument &optional colon-p at-sign-p)
  (check-type argument (unsigned-byte 128))
  (format stream "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~12,'0X"
          (ldb (byte 32 96) argument)
          (ldb (byte 16 80) argument)
          (ldb (byte 16 64) argument)
          (ldb (byte 16 48) argument)
          (ldb (byte 48 0) argument)))

(defun format-ipv4-address (stream argument &optional colon-p at-sign-p)
  (check-type argument (unsigned-byte 32))
  (format stream "~D.~D.~D.~D"
          (ldb (byte 8 24) argument)
          (ldb (byte 8 16) argument)
          (ldb (byte 8 8) argument)
          (ldb (byte 8 0) argument)))

(defun format-ipv6-address (stream argument &optional colon-p at-sign-p)
  (check-type argument (unsigned-byte 128))
  (dotimes (i 8)
    (unless (zerop i) (write-char #\: stream))
    (let ((group (ldb (byte 16 (* (- 7 i) 16)) argument)))
      (write group :base 16 :stream stream))))

(defun format-mac-address (stream argument &optional colon-p at-sign-p)
  (check-type argument (unsigned-byte 64))
  (dotimes (i 8)
    (unless (zerop i) (write-char #\: stream))
    (format stream "~2,'0X" (ldb (byte 8 (* (- 7 i) 8)) argument))))

(defun display-kboot-tag-list ()
  (when *kboot-tag-list*
    (labels ((p/8 (addr) (memref-unsigned-byte-8 (+ #x8000000000 addr) 0))
             (p/16 (addr) (memref-unsigned-byte-16 (+ #x8000000000 addr) 0))
             (p/32 (addr) (memref-unsigned-byte-32 (+ #x8000000000 addr) 0))
             (p/64 (addr) (memref-unsigned-byte-64 (+ #x8000000000 addr) 0))
             (p/uuid (addr)
               (let ((uuid (make-array 64 :element-type 'base-char)))
                 (dotimes (i 64)
                   (setf (aref uuid i) (code-char (p/8 (+ addr i)))))
                 (subseq uuid 0 (position (code-char 0) uuid))))
             (p/be (addr len)
               "Read a big-endian value from memory."
               (let ((value 0))
                 (dotimes (i len)
                   (setf value (logior (ash value 8)
                                       (p/8 (+ addr i)))))
                 value))
             (p/ipv4 (addr) (p/be addr 4))
             (p/ipv6 (addr) (p/be addr 16)))
      (format t "Loaded by KBoot. Tag list at #x~8,'0X~%" *kboot-tag-list*)
      (let ((addr *kboot-tag-list*)
            ;; For sanity checking.
            (max-addr (+ *kboot-tag-list* 1024))
            (last-type -1)
            (saw-memory nil)
            (saw-vmem nil)
            (saw-e820 nil))
        (loop (when (>= addr max-addr)
                (format t "Went past tag list max address.~%")
                (return))
           (let ((type (p/32 (+ addr 0)))
                 (size (p/32 (+ addr 4))))
             (unless (member type '(#.+kboot-tag-memory+ #.+kboot-tag-vmem+ #.+kboot-tag-e820+))
               (format t "Tag ~A (~D), ~:D bytes.~%"
                       (if (> type (length *kboot-tag-names*))
                           "Unknown"
                           (aref *kboot-tag-names* type))
                       type size))
             (when (and (eql addr *kboot-tag-list*)
                        (not (eql type +kboot-tag-core+)))
               (format t "CORE tag not first in the list?~%"))
             (case type
               (#.+kboot-tag-none+ (return))
               (#.+kboot-tag-core+
                (unless (eql addr *kboot-tag-list*)
                  (format t "CORE tag not first in the list?~%"))
                (format t "     tags_phys: ~8,'0X~%" (p/64 (+ addr 8)))
                (format t "     tags_size: ~:D bytes~%" (p/32 (+ addr 16)))
                (format t "   kernel_phys: ~8,'0X~%" (p/64 (+ addr 24)))
                (format t "    stack_base: ~8,'0X~%" (p/64 (+ addr 32)))
                (format t "    stack_phys: ~8,'0X~%" (p/64 (+ addr 40)))
                (format t "    stack_size: ~:D bytes~%" (p/32 (+ addr 48)))
                (setf max-addr (+ *kboot-tag-list* (p/32 (+ addr 16)))))
               #+nil(#.+kboot-tag-option+)
               (#.+kboot-tag-memory+
                (unless (eql last-type +kboot-tag-memory+)
                  (when saw-memory
                    (format t "MEMORY tags are non-contigious.~%"))
                  (setf saw-memory t)
                  (format t "MEMORY map:~%"))
                (let ((start (p/64 (+ addr 8)))
                      (length (p/64 (+ addr 16)))
                      (type (p/8 (+ addr 24))))
                  (format t "  ~16,'0X-~16,'0X  ~A~%"
                          start (+ start length)
                          (case type
                            (#.+kboot-memory-free+ "Free")
                            (#.+kboot-memory-allocated+ "Allocated")
                            (#.+kboot-memory-reclaimable+ "Reclaimable")
                            (t (format nil "Unknown (~D)" type))))))
               (#.+kboot-tag-vmem+
                (unless (eql last-type +kboot-tag-vmem+)
                  (when saw-vmem
                    (format t "VMEM tags are non-contigious.~%"))
                  (setf saw-vmem t)
                  (format t "VMEM map:~%"))
                (let ((start (p/64 (+ addr 8)))
                      (size (p/64 (+ addr 16)))
                      (phys (p/64 (+ addr 24))))
                  (format t "  ~16,'0X-~16,'0X -> ~8,'0X~%"
                          start (+ start size) phys)))
               (#.+kboot-tag-pagetables+
                (format t "          pml4: ~8,'0X~%" (p/64 (+ addr 8)))
                (format t "       mapping: ~8,'0X~%" (p/64 (+ addr 16))))
               (#.+kboot-tag-module+
                (format t "          addr: ~8,'0X~%" (p/64 (+ addr 8)))
                (format t "          size: ~:D bytes~%" (p/32 (+ addr 16))))
               (#.+kboot-tag-video+
                (case (p/32 (+ addr 8))
                  (#.+kboot-video-vga+
                   (format t "  VGA text mode.~%")
                   (format t "          cols: ~:D~%" (p/8 (+ addr 16)))
                   (format t "          rows: ~:D~%" (p/8 (+ addr 17)))
                   (format t "             x: ~:D~%" (p/8 (+ addr 18)))
                   (format t "             y: ~:D~%" (p/8 (+ addr 19)))
                   (format t "      mem_phys: ~8,'0X~%" (p/64 (+ addr 24)))
                   (format t "      mem_virt: ~8,'0X~%" (p/64 (+ addr 32)))
                   (format t "      mem_size: ~:D bytes~%" (p/32 (+ addr 40))))
                  (#.+kboot-video-lfb+
                   (let ((flags (p/32 (+ addr 16))))
                     (format t " LFB ~A mode.~%"
                             (cond
                               ((logtest flags +kboot-lfb-indexed+)
                                "indexed colour")
                               ((logtest flags +kboot-lfb-rgb+)
                                "direct colour")
                               (t "unknown")))
                     (format t "         flags: ~8,'0B" flags)
                     (when (logtest flags +kboot-lfb-rgb+)
                       (format t " KBOOT_LFB_RGB"))
                     (when (logtest flags +kboot-lfb-indexed+)
                       (format t " KBOOT_LFB_INDEXED"))
                     (format t "~%")
                     (format t "         width: ~:D~%" (p/32 (+ addr 20)))
                     (format t "        height: ~:D~%" (p/32 (+ addr 24)))
                     (format t "           bpp: ~:D~%" (p/8 (+ addr 28)))
                     (format t "         pitch: ~:D~%" (p/32 (+ addr 32)))
                     (format t "       fb_phys: ~8,'0X~%" (p/64 (+ addr 40)))
                     (format t "       fb_virt: ~8,'0X~%" (p/64 (+ addr 48)))
                     (format t "       fb_size: ~:D bytes~%" (p/32 (+ addr 56)))
                     (when (logtest flags +kboot-lfb-rgb+)
                       (format t "      red_size: ~:D bits~%" (p/8 (+ addr 60)))
                       (format t "       red_pos: ~:D~%" (p/8 (+ addr 61)))
                       (format t "    green_size: ~:D bits~%" (p/8 (+ addr 62)))
                       (format t "     green_pos: ~:D~%" (p/8 (+ addr 63)))
                       (format t "     blue_size: ~:D bits~%" (p/8 (+ addr 64)))
                       (format t "      blue_pos: ~:D~%" (p/8 (+ addr 65))))
                     (when (logtest flags +kboot-lfb-indexed+)
                       (format t "  palette_size: ~:D~%" (p/16 (+ addr 66))))))))
               (#.+kboot-tag-bootdev+
                (format t "        method: ~A~%"
                        (case (p/32 (+ addr 8))
                          (#.+kboot-bootdev-none+ "None")
                          (#.+kboot-bootdev-disk+ "Disk")
                          (#.+kboot-bootdev-network+ "Network")
                          (t (format nil "Unknown (~D)" (p/32 (+ addr 8))))))
                (case (p/32 (+ addr 8))
                  (#.+kboot-bootdev-disk+
                   (format t "         flags: ~8,'0B~%" (p/32 (+ addr 12)))
                   (format t "          uuid: ~S~%" (p/uuid (+ addr 16)))
                   (format t "        device: ~2,'0X~%" (p/8 (+ addr 80)))
                   (format t "     partition: ~2,'0X~%" (p/8 (+ addr 81)))
                   (format t " sub_partition: ~2,'0X~%" (p/8 (+ addr 82))))
                  (#.+kboot-bootdev-network+
                   (let ((flags (p/32 (+ addr 12))))
                     (format t "         flags: ~8,'0B" flags)
                     (when (logtest flags +kboot-net-ipv6+)
                       (format t " KBOOT_NET_IPv6"))
                     (format t "~%")
                     (if (logtest flags +kboot-net-ipv6+)
                         (format t "     server_ip: ~/sys.int::format-ipv6-address/~%"
                                 (p/ipv6 (+ addr 16)))
                         (format t "     server_ip: ~/sys.int::format-ipv4-address/~%"
                                 (p/ipv4 (+ addr 16))))
                     (format t "   server_port: ~D~%" (p/16 (+ addr 32)))
                     (if (logtest flags +kboot-net-ipv6+)
                         (format t "    gateway_ip: ~/sys.int::format-ipv6-address/~%"
                                 (p/ipv6 (+ addr 34)))
                         (format t "    gateway_ip: ~/sys.int::format-ipv4-address/~%"
                                 (p/ipv4 (+ addr 34))))
                     (if (logtest flags +kboot-net-ipv6+)
                         (format t "     client_ip: ~/sys.int::format-ipv6-address/~%"
                                 (p/ipv6 (+ addr 50)))
                         (format t "     client_ip: ~/sys.int::format-ipv4-address/~%"
                                 (p/ipv4 (+ addr 50))))
                     (format t "    client_mac: ~/sys.int::format-mac-address/~%"
                             (p/be (+ addr 66) 8))))))
               (#.+kboot-tag-e820+
                (unless (eql last-type +kboot-tag-e820+)
                  (when saw-e820
                    (format t "E820 tags are non-contigious.~%"))
                  (setf saw-e820 t)
                  (format t "E820 map:~%"))
                (let ((start (p/64 (+ addr 8)))
                      (size (p/64 (+ addr 16)))
                      (type (p/32 (+ addr 24)))
                      (attr (p/32 (+ addr 28))))
                  (format t "  ~16,'0X-~16,'0X ~D ~D~%"
                          start (+ start size) type attr))))
             (setf last-type type)
             (incf addr (round-up size 8))))))))

(defun load-modules ()
  (when *multiboot-info*
    (dotimes (i (multiboot-module-count))
      (format t "Loading module ~S.~%" (multiboot-module-command-line i))
      (mini-load-llf (mini-multiboot-module-stream i))))
  (when *kboot-tag-list*
    (flet ((p/8 (addr) (memref-unsigned-byte-8 (+ #x8000000000 addr) 0))
           (p/16 (addr) (memref-unsigned-byte-16 (+ #x8000000000 addr) 0))
           (p/32 (addr) (memref-unsigned-byte-32 (+ #x8000000000 addr) 0))
           (p/64 (addr) (memref-unsigned-byte-64 (+ #x8000000000 addr) 0)))
      (let ((addr *kboot-tag-list*)
            ;; For sanity checking.
            (max-addr (+ *kboot-tag-list* 1024)))
      (loop (when (>= addr max-addr) (return))
         (let ((type (p/32 (+ addr 0)))
               (size (p/32 (+ addr 4))))
           (when (and (eql addr *kboot-tag-list*)
                      (not (eql type +kboot-tag-core+)))
             (format t "CORE tag not first in the list?~%")
             (return))
           (case type
             (#.+kboot-tag-none+ (return))
             (#.+kboot-tag-core+
              (unless (eql addr *kboot-tag-list*)
                (format t "CORE tag not first in the list?~%")
                (return))
              (setf max-addr (+ *kboot-tag-list* (p/32 (+ addr 16)))))
             (#.+kboot-tag-module+
              (let* ((address (p/64 (+ addr 8)))
                     (size (p/32 (+ addr 16)))
                     (array (make-array size
                                        :element-type '(unsigned-byte 8)
                                        :memory (+ #x8000000000 address)))
                     (stream (mini-vector-stream array)))
                (format t "Loading KBoot module at ~X~%" address)
                (mini-load-llf stream))))
           (incf addr (round-up size 8))))))))

(defun initialize-lisp ()
  (setf *next-symbol-tls-slot* 12
        *array-types* #(t
                        base-char
                        character
                        bit
                        (unsigned-byte 2)
                        (unsigned-byte 4)
                        (unsigned-byte 8)
                        (unsigned-byte 16)
                        (unsigned-byte 32)
                        (unsigned-byte 64)
                        (signed-byte 1)
                        (signed-byte 2)
                        (signed-byte 4)
                        (signed-byte 8)
                        (signed-byte 16)
                        (signed-byte 32)
                        (signed-byte 64)
                        single-float
                        double-float
                        long-float
                        xmm-vector
                        (complex single-float)
                        (complex double-float)
                        (complex long-float))
        ;; Ugh!
        (memref-unsigned-byte-64 *small-static-area* 0) (- (* 4 1024 1024) 2)
        (memref-unsigned-byte-64 *small-static-area* 1) #b100
        *package* nil
        *cold-stream* (make-cold-stream)
        *terminal-io* *cold-stream*
        *standard-output* *cold-stream*
        *standard-input* *cold-stream*
        *debug-io* *cold-stream*
        *cold-stream-screen* nil
        *keyboard-shifted* nil
        *early-initialize-hook* '()
        *initialize-hook* '())
  (setf *print-base* 10.
        *print-escape* t
        *print-readably* nil
        *print-safe* t)
  (setf *features* '(:unicode :little-endian :x86-64 :lisp-os :ieee-floating-point :ansi-cl :common-lisp)
        *macroexpand-hook* 'funcall
        most-positive-fixnum #.(- (expt 2 60) 1)
        most-negative-fixnum #.(- (expt 2 60)))
  (setf (symbol-function 'intern) #'cold-intern)
  ;; Initialize defstruct and patch up all the structure types.
  (bootstrap-defstruct)
  (dotimes (i (length *initial-structure-obarray*))
    (setf (%struct-slot (svref *initial-structure-obarray* i) 0) *structure-type-type*))
  (write-line "Cold image coming up...")
  ;; Hook SETF symbols up.
  (dotimes (i (length *initial-setf-obarray*))
    (let* ((sym (svref *initial-setf-obarray* i))
           (other (find (symbol-name sym) *initial-obarray* :test 'string= :key 'symbol-name)))
      (assert other)
      (setf (get sym 'setf-symbol-backlink) other
            (get other 'setf-symbol) sym)))
  (dotimes (i (length *cold-toplevel-forms*))
    (eval (svref *cold-toplevel-forms* i)))
  (setf *initial-obarray* (make-array (length *initial-obarray*)
                                      :fill-pointer t :adjustable t
                                      :initial-contents *initial-obarray*))
  (setf *initial-keyword-obarray* (make-array (length *initial-keyword-obarray*)
                                              :fill-pointer t :adjustable t
                                              :initial-contents *initial-keyword-obarray*))
  (dotimes (i (length *initial-keyword-obarray*))
    (setf (symbol-package (aref *initial-keyword-obarray* i)) :keyword
          (symbol-mode (aref *initial-keyword-obarray* i)) :constant))
  (dolist (sym '(nil t most-positive-fixnum most-negative-fixnum))
    (setf (symbol-mode sym) :constant))
  ;; Pull in the real package system.
  ;; If anything goes wrong before init-package-sys finishes then things
  ;; break in terrible ways.
  (dotimes (i (length *package-system*))
    (eval (svref *package-system* i)))
  (initialize-package-system)
  (let ((*package* *package*))
    (dotimes (i (length *additional-cold-toplevel-forms*))
      (eval (svref *additional-cold-toplevel-forms* i))))
  (gc)
  (setf (fdefinition 'initialize-lisp) #'reinitialize-lisp)
  (reinitialize-lisp))

(defun reinitialize-lisp ()
  (init-isa-pic)
  (cold-stream-init)
  (gc-init-system-memory)
  (mapc 'funcall *early-initialize-hook*)
  (%sti)
  (pci-device-scan)
  (write-line "Hello, world.")
  (load-modules)
  (mapc 'funcall *initialize-hook*)
  (terpri)
  (write-char #\*)
  (write-char #\O)
  (write-char #\K)
  (terpri)
  (display-kboot-tag-list)
  (repl)
  (loop (%hlt)))
