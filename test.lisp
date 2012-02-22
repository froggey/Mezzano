(setf (symbol-function 'sys.int::raise-undefined-function)
      (lambda (invoked-through)
	(let ((str (if (symbolp invoked-through)
		       (symbol-name invoked-through)
		       "Undefined function")))
	  (dotimes (i (sys.int::%simple-array-length str))
	    (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 i)
		  (logior (char-code (schar str i)) #x0F00))))
	(loop)))

(defun endp (list)
  (cond ((null list) t)
        ((consp list) nil)
        (t (error 'type-error
                  :datum list
                  :expected-type 'list))))

(defun (setf system:symbol-mode) (value symbol)
  (let ((flags (sys.int::%symbol-flags symbol))
        (bits (ecase value
                ((nil) 0)
                ((:special) 1)
                ((:constant) 2)
                ((:symbol-macro) 3))))
    (setf (sys.int::%symbol-flags symbol)
          (logior (logand flags -4) bits))
    value))

(defun proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (special (dolist (var (rest declaration-specifier))
               (setf (system:symbol-mode var) :special)))))

(defvar *screen-offset* 0)

(defvar *gb-keymap-low*
  #(nil #\Esc #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\- #\= #\Backspace
    #\Tab #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p #\[ #\] #\Newline
    :control #\a #\s #\d #\f #\g #\h #\j #\k #\l #\; #\' #\`
    :shift #\# #\z #\x #\c #\v #\b #\n #\m #\, #\. #\/ :shift nil
    :meta #\Space :capslock nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil #\\
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil))
(defvar *gb-keymap-high*
  #(nil #\Esc #\! #\" #\£ #\$ #\% #\^ #\& #\* #\( #\) #\_ #\+ #\Backspace
    #\Tab #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\{ #\} #\Newline
    :control #\A #\S #\D #\F #\G #\H #\J #\K #\L #\: #\@ #\¬
    :shift #\~ #\Z #\X #\C #\V #\B #\N #\M #\< #\> #\? :shift nil
    :meta #\Space :capslock nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil #\|
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil))

(defvar *keyboard-shifted* nil)

(defun write-char (c &optional stream)
  (setf (system:io-port/8 #xE9) (logand (char-code c) #xFF))
  (cond ((eql c #\Newline)
         (incf *screen-offset* (- 80 (rem *screen-offset* 80))))
        (t (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 *screen-offset*)
                 (logior (char-code c) #x0F00))
           (incf *screen-offset*)))
  (when (>= *screen-offset* (* 80 25))
    (setf *screen-offset* 0))
  c)

(defun terpri (&optional stream)
  (write-char #\Newline))

(defun fresh-line (&optional stream)
  (unless (zerop (rem *screen-offset* 80))
    (terpri)))

(defun write-to-the-screen (str)
  (dotimes (i (sys.int::%simple-array-length str))
    (write-char (schar str i))))

(write-to-the-screen "Hello, World!")

(defun sys.int::raise-undefined-function (invoked-through)
  (error 'undefined-function :name symbol))

(defun sys.int::raise-unbound-error (symbol)
  (error 'unbound-variable :name symbol))

(defun poll-keyboard ()
  (loop (let ((cmd (system:io-port/8 #x64)))
          (when (= (logand cmd 1) 1)
            ;; Byte ready.
            (return (system:io-port/8 #x60))))))

(defun read-keyboard-char ()
  (loop
     (let* ((scancode (poll-keyboard))
            (key (#+ignore aref svref (if *keyboard-shifted*
                           *gb-keymap-high*
                           *gb-keymap-low*)
                       (logand scancode #x7F))))
       (cond ((= (logand scancode #x80) 0)
              ;; Key press.
              (cond ((eql key :shift)
                     (setf *keyboard-shifted* t))
                    ((characterp key)
                     (write-char key)
                     (return key))))
             (t ;; Key release.
              (case key
                (:shift (setf *keyboard-shifted* nil))))))))

(defvar *unread-char* nil)

(defun read-char (&optional stream eof-error-p eof-value recursive-p)
  (cond (*unread-char*
         (prog1 *unread-char*
           (setf *unread-char* nil)))
        (t (read-keyboard-char))))

(defun unread-char (character &optional stream)
  (when *unread-char*
    (error "Multiple unread-char!"))
  (setf *unread-char* character)
  nil)

(defun peek-char (&optional peek-type stream eof-error-p eof-value recursive-p)
  (cond ((eql peek-type nil)
         (let ((ch (read-char)))
           (unread-char ch)
           ch))
        ((eql peek-type t)
         (do ((ch (read-char) (read-char)))
             ((not (sys.int::whitespace[2]p ch))
              (unread-char ch)
              ch)))
        ((characterp peek-type)
         (error "TODO: character peek."))
        (t (error "Bad peek type ~S." peek-type))))

(setf *standard-input* nil
      *standard-output* nil)

(defun write-unsigned-integer (x base)
  (unless (= x 0)
    (write-unsigned-integer (truncate x base) base)
    (write-char (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                       (rem x base)))))

(defun write-integer (x &optional (base 10))
  (cond ((= x 0)
         (write-char #\0))
        ((< x 0)
         (write-char #\-)
         (write-unsigned-integer (- 0 x) base))
        (t (write-unsigned-integer x base))))

(defun backtrace ()
  (do ((fp (sys.int::read-frame-pointer)
           (sys.int::memref-unsigned-byte-64 fp 0)))
      ((= fp 0))
    (write-char #\Newline)
    (write-integer fp 16)
    (write-char #\Space)
    (write-integer (sys.int::memref-unsigned-byte-64 fp -2) 16)))

(defun integerp (object)
  (system:fixnump object))

(defun numberp (object)
  (integerp object))

(defvar *bump-pointer* #x8001000000)

(defun cons (car cdr)
  (let* ((address *bump-pointer*)
         (val (sys.int::%%assemble-value address 1)))
    (setf (car val) car
          (cdr val) cdr)
    (incf *bump-pointer* 16)
    val))

(defun sys.int::allocate-std-instance (class slots)
  (let* ((address *bump-pointer*)
         (val (sys.int::%%assemble-value address #b0100)))
    (setf (sys.int::std-instance-class val) class
          (sys.int::std-instance-slots val) slots)
    (incf *bump-pointer* 16)
    val))

(defun list (&rest args)
  args)

(defun plusp (number)
  (> number 0))

(defun minusp (number)
  (< number 0))

(defun zerop (number)
  (= number 0))

(defun expt (base power)
  (let ((accum 1))
    (dotimes (i power accum)
      (setf accum (* accum base)))))

(defun 1- (number)
  (- number 1))

(defun 1+ (number)
  (+ number 1))

;;; (type tag size-in-bits 16-byte-aligned-p)
(defvar *array-info*
  '((bit 3 1 nil)
    ((unsigned-byte 2) 4 2 nil)
    ((unsigned-byte 4) 5 4 nil)
    ((unsigned-byte 8) 6 8 nil)
    ((unsigned-byte 16) 7 16 nil)
    ((unsigned-byte 32) 8 32 nil)
    ((unsigned-byte 64) 9 64 nil)
    ((signed-byte 1) 10 1 nil)
    ((signed-byte 2) 11 2 nil)
    ((signed-byte 4) 12 4 nil)
    ((signed-byte 8) 13 8 nil)
    ((signed-byte 16) 14 16 nil)
    ((signed-byte 32) 15 32 nil)
    ((signed-byte 64) 16 64 nil)
    (base-char 1 8 nil)
    (character 2 32 nil)
    (single-float 17 32 t)
    (double-float 18 64 t)
    (long-float 19 128 t)
    (sys.int::xmm-vector 20 128 t)
    ((complex single-float) 21 64 t)
    ((complex double-float) 22 128 t)
    ((complex long-float) 23 256 t)
    (t 0 64 nil)))

(defun sys.int::%allocate-and-clear-array (length real-element-type)
  (let* ((info (assoc real-element-type *array-info* :test 'equal))
         (total-size (+ 64 ; header word.
                        (if (fourth info) 64 0) ; padding for alignment.
                        (* length (third info))))
         (address *bump-pointer*))
    ;; Align on a 16-byte boundary.
    (unless (zerop (rem total-size 128))
      (incf total-size (- 128 (rem total-size 128))))
    ;; Clear memory.
    (dotimes (i (truncate total-size 64))
      (setf (sys.int::memref-unsigned-byte-64 address i) 0))
    ;; Set header word.
    (setf (sys.int::memref-unsigned-byte-64 address 0)
          (logior (ash length 8)
                  (ash (second info) 1)))
    ;; Advance pointer.
    (incf *bump-pointer* (truncate total-size 8))
    ;; Return value.
    (sys.int::%%assemble-value address #b0111)))

(defun sys.int::%allocate-and-fill-array (length real-element-type initial-element)
  (let ((array (sys.int::%allocate-and-clear-array length real-element-type)))
    (dotimes (i length)
      (setf (aref array i) initial-element))
    array))

(defun sys.int::make-simple-vector (length)
  "Allocate a SIMPLE-VECTOR with LENGTH elements.
Equivalent to (make-array length). Used by the compiler to
allocate environment frames."
  (let* ((total-size (1+ length))
         (address *bump-pointer*))
    ;; Align on a 16-byte boundary.
    (unless (zerop (logand total-size 1))
      (incf total-size))
    ;; Clear memory.
    (dotimes (i total-size)
      (setf (sys.int::memref-unsigned-byte-64 address i) 0))
    ;; Set header word.
    (setf (sys.int::memref-unsigned-byte-64 address 0)
          (ash length 8))
    ;; Advance pointer.
    (incf *bump-pointer* (* total-size 8))
    ;; Return value.
    (sys.int::%%assemble-value address #b0111)))

(defun sys.int::make-closure (function environment)
  "Allocate a closure object."
  (check-type function function)
  (let ((address *bump-pointer*))
    (incf *bump-pointer* (* 6 8))
    ;; Initialize and clear constant slots.
    ;; Function tag, flags and MC size.
    (setf (sys.int::memref-unsigned-byte-32 address 0) #x00020001
          ;; Constant pool size and slot count.
          (sys.int::memref-unsigned-byte-32 address 1) #x00000002
          (sys.int::memref-unsigned-byte-32 address 2) #x00000000
          ;; The code.
          ;; mov64 :rbx (:rip 21)/pool[1]
          (sys.int::memref-unsigned-byte-32 address 3) #x151D8B48
          ;; jmp (:rip 7)/pool[0]
          (sys.int::memref-unsigned-byte-32 address 4) #xFF000000
          (sys.int::memref-unsigned-byte-32 address 5) #x00000725
          (sys.int::memref-unsigned-byte-32 address 6) #xCCCCCC00
          (sys.int::memref-unsigned-byte-32 address 7) #xCCCCCCCC
          ;; Constant pool.
          (sys.int::memref-unsigned-byte-32 address 8) 0
          (sys.int::memref-unsigned-byte-32 address 9) 0
          (sys.int::memref-unsigned-byte-32 address 10) 0
          (sys.int::memref-unsigned-byte-32 address 11) 0)
    (let ((value (sys.int::%%assemble-value address #b1100)))
      ;; Initialize constant pool
      (setf (sys.int::memref-t address 4) function
            (sys.int::memref-t address 5) environment)
      value)))

(defun sys.int::%make-array-header (dimensions fill-pointer info storage)
  (let* ((address *bump-pointer*)
         (val (sys.int::%%assemble-value address #b0011)))
    (setf (sys.int::%array-header-dimensions val) dimensions
          (sys.int::%array-header-fill-pointer val) fill-pointer
          (sys.int::%array-header-info val) info
          (sys.int::%array-header-storage val) storage)
    (incf *bump-pointer* 32)
    val))

(defun sys.int::%make-character (code &optional bits)
  (check-type code (or (integer 0 #xD7FF)
                       (integer #xE000 #x0010FFFF))
              "a unicode code-point")
  (check-type bits (or null (integer 0 15)))
  (sys.int::%%assemble-value (ash (logior code (ash (or bits 0) 21)) 4) #b1010))

(defun char-code (character)
  (check-type character character)
  (logand (ash (sys.int::lisp-object-address character) -4) #x1FFFFF))

(defun system:char-bits (character)
  (check-type character character)
  (logand (ash (ash (sys.int::lisp-object-address character) -4) -21) 15))

(defun char-upcase (char)
  (let ((code (char-code char)))
    (if (<= #x61 code #x7A)
        (sys.int::%make-character (logand code (lognot #x20))
                                  (system:char-bits char))
        char)))

;;; FIXME: some parts must run with the GC off.
(defun sys.int::%simple-array-aref (array index)
  (ecase (sys.int::%simple-array-type array)
    (0 ;; simple-vector
     (svref array index))
    ((1 2) ;; simple-base-string or simple-string
     (schar array index))))
(defun (setf sys.int::%simple-array-aref) (value array index)
  (ecase (sys.int::%simple-array-type array)
    (0 ;; simple-vector
     (setf (svref array index) value))
    ((1 2) ;; simple-base-string or simple-string
     (setf (schar array index) value))))

(defparameter *array-types*
  #(t
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
    sys.int::xmm-vector
    (complex single-float)
    (complex double-float)
    (complex long-float)))

(defun sys.int::%simple-array-element-type (array)
  (svref *array-types* (sys.int::%simple-array-type array)))

(defun simple-string-p (object)
  (when (sys.int::%simple-array-p object)
    (let ((tag (sys.int::%simple-array-type object)))
      (or (eql tag 1) (eql tag 2)))))

(defun char= (character &rest more-characters)
  (declare (dynamic-extent more-characters))
  (check-type character character)
  (dolist (c more-characters t)
    (check-type c character)
    (when (not (eql character c))
      (return nil))))

(defun base-char-p (character)
  (check-type character character)
  (and (zerop (system:char-bits character))
       (< (char-code character) 256)))

(defun /= (number &rest more-numbers)
  "Returns true if no two numbers are the same in value; otherwise, returns false."
  (declare (dynamic-extent more-numbers))
  (check-type number number)
  (do ((lhs number (car n))
       (n more-numbers (cdr n)))
      ((endp n) t)
    (dolist (rhs n)
      (check-type rhs number)
      (when (= lhs rhs)
	(return-from /= nil)))))

(defun sys.int::simplify-string (string)
  (if (simple-string-p string)
      string
      (make-array (length string)
                  :element-type (if (every 'base-char-p string)
                                    'base-char
                                    'character)
                  :initial-contents string)))

(defun make-symbol (name)
  (check-type name string)
  (prog1 (sys.int::%make-symbol *bump-pointer* (sys.int::simplify-string name))
    (incf *bump-pointer* (* 8 6))))

(defun sys.int::raise-type-error (datum expected-type)
  (error 'type-error :datum datum :expected-type expected-type))

(defun eval (form)
  (typecase form
    (symbol (symbol-value form))
    (cons (case (first form)
            ((function) (symbol-function (second form)))
            ((quote) (second form))
            ((setq) (setf (symbol-value (second form)) (eval (third form))))
            (t (apply (first form) (mapcar 'eval (rest form))))))
    (t form)))

(defun apply (function arg &rest more-args)
  (declare (dynamic-extent more-args))
  (cond (more-args
         ;; Convert (... (final-list ...)) to (... final-list...)
         (do* ((arg-list (cons arg more-args))
               (i arg-list (cdr i)))
              ((null (cddr i))
               (setf (cdr i) (cadr i))
               (apply function arg-list))))
        (t (apply function arg))))

(defun write-string (string)
  (dotimes (i (length string))
    (write-char (char string i))))

(defun write (object &rest blah)
  (typecase object
    (integer (write-integer object))
    (cons
     (write-char #\()
     (write (car object))
     (do ((i (cdr object) (cdr i)))
         ((atom i)
          (when i
            (write-string " . ")
            (write i))
          (write-char #\)))
       (write-char #\Space)
       (write (car i))))
    (symbol
     (when (keywordp object)
       (write-char #\:))
     (write-string (symbol-name object)))
    (string
     (write-char #\")
     (dotimes (i (length object))
       (let ((c (char object i)))
         (case c
           (#\\ (write-char #\\) (write-char #\\))
           (#\" (write-char #\\) (write-char #\"))
           (t (write-char c)))))
     (write-char #\"))
    (character
     (write-char #\#)
     (write-char #\\)
     (write-char object))
    ((satisfies sys.int::structure-object-p)
     (write-char #\#)
     (write-char #\<)
     (write (sys.int::structure-name (sys.int::%struct-slot object 0)))
     (write-char #\Space)
     (write-integer (sys.int::lisp-object-address object) 16)
     (write-char #\>))
    (simple-condition
     (write-char #\#)
     (write-char #\<)
     (write (class-name (class-of object)))
     (write-char #\Space)
     (write (simple-condition-format-control object))
     (write-char #\Space)
     (write-integer (sys.int::lisp-object-address object) 16)
     (write-char #\>))
    (t (write-char #\#)
       (write-char #\<)
       (write-string "Unknown-object ")
       (write-integer (sys.int::lisp-object-address object) 16)
       (write-char #\>)))
  object)

(defun fboundp (name)
  (sys.int::%fboundp (sys.int::function-symbol name)))

(defun fmakunbound (name)
  (sys.int::%fmakunbound (sys.int::function-symbol name))
  name)

(defparameter *debugger-depth* 0)
(defvar *debug-io* nil)

(defun sys.int::enter-debugger (condition)
  (let* ((*standard-input* *debug-io*)
	 (*standard-output* *debug-io*)
	 (debug-level *debugger-depth*)
	 (*debugger-depth* (1+ *debugger-depth*))
	 (restarts (compute-restarts))
	 (restart-count (length restarts)))
    (fresh-line)
    (write condition :escape nil :readably nil)
    (write-char #\Space)
    (write-char #\()
    (write (class-name (class-of condition)))
    (write-char #\))
    (fresh-line)
    (show-restarts restarts)
    (loop
       (fresh-line)
       (write-string "Pick a restart# ")
       (let ((form (read)))
         (when (and (integerp form) (>= form 0) (< form restart-count))
           (invoke-restart-interactively (nth (- restart-count form 1) restarts)))))))

(defun show-restarts (restarts)
  (let ((restart-count (length restarts)))
    (write-string "Available restarts:")(terpri)
    (do ((i 0 (1+ i))
	 (r restarts (cdr r)))
	((null r))
      (write i)
      (write-char #\Space)
      (write (restart-name (car r)))
      (terpri))))

(defun sys.int::%invalid-argument-error (&rest args)
  (fresh-line)
  (write-string "Invalid arguments to function")
  (fresh-line)
  (backtrace)
  (loop))

(defun copy-list (list)
  (when list
    (cons (car list) (copy-list (cdr list)))))

(setf *package* (find-package "CL-USER"))
(loop
   (with-simple-restart (continue "Carry on chaps.")
     (fresh-line)
     (write-char #\>)
     (let ((form (read)))
       (fresh-line)
       (let ((result (eval form)))
         (fresh-line)
         (write result)))))
