(in-package #:sys.int)

(declaim (special *cold-toplevel-forms*
                  *initial-obarray*
                  *initial-keyword-obarray*
                  *initial-setf-obarray*
                  *initial-structure-obarray*)
         (special *terminal-io*
                  *standard-output*
                  *standard-input*))

(defun write-char (character &optional stream)
  (setf (io-port/8 #xE9) (logand (char-code character) #xFF)))

(defun raise-undefined-function (invoked-through &rest args)
  (declare (ignore args))
  (write-char #\!)
  (write-char #\#)
  (when (symbolp invoked-through)
    (write-string (symbol-name invoked-through)))
  (loop (%hlt)))

(defun raise-unbound-error (symbol)
  (write-char #\!)
  (write-char #\*)
  (write-string (symbol-name symbol))
  (loop (%hlt)))

(defun raise-type-error (datum expected-type)
  (write-char #\!)
  (write-char #\$)
  (write expected-type)
  (write-char #\Space)
  (write datum)
  (loop (%hlt)))

(defun length (sequence)
  (etypecase sequence
    (list (or (list-length sequence)
	      (error 'simple-type-error
		     :expected-type 'sequence
		     :datum sequence
		     :format-control "List ~S is circular."
		     :format-arguments (list sequence))))
    (vector (if (array-has-fill-pointer-p sequence)
		(fill-pointer sequence)
		(array-dimension sequence 0)))))

(defun endp (list)
  (cond ((null list) t)
        ((consp list) nil)
        (t (error 'type-error
                  :datum list
                  :expected-type 'list))))

(defun char (string index)
  (schar string index))

(defun vectorp (object)
  (or (and (%array-header-p object)
	   (fixnump (%array-header-dimensions object)))
      (%simple-array-p object)))

(defun array-has-fill-pointer-p (array)
  (check-type array array)
  (when (%array-header-p array)
    (%array-header-fill-pointer array)))

(defun fill-pointer (vector)
  (check-vector-has-fill-pointer vector)
  (%array-header-fill-pointer vector))

(defun (setf fill-pointer) (new-value vector)
  (check-type new-value (integer 0))
  ;; Not sure if this should allow adding a fill-pointer to a vector
  ;; that doesn't already have one.
  (check-vector-has-fill-pointer vector)
  (unless (<= 0 new-value (%array-header-dimensions vector))
    (error "New fill-pointer ~S exceeds vector bounds. Should be non-negative and <=~S."
	   new-value (%array-header-dimensions vector)))
  (setf (%array-header-fill-pointer vector) new-value))

(defun arrayp (object)
  (or (%array-header-p object)
      (%simple-array-p object)))

(defun array-element-type (array)
  (check-type array array)
  (if (%array-header-p array)
      (cond ((or (null (%array-header-dimensions array))
		 (integerp (%array-header-storage array)))
	     ;; 0D and memory arrays store the type in the info slot.
	     (%array-header-info array))
	    ((%array-header-info array)
	     ;; Displaced arrays inherit the type of the array they displace on.
	     (array-element-type (%array-header-storage array)))
	    (t ;; Normal arrays use the type of their storage simple array.
	     (%simple-array-element-type (%array-header-storage array))))
      (%simple-array-element-type array)))

(defun array-rank (array)
  (check-type array array)
  (cond ((%array-header-p array)
	 (if (fixnump (%array-header-dimensions array))
	     1
	     (list-length (%array-header-dimensions array))))
	(t 1)))

(defun array-dimensions (array)
  (check-type array array)
  (cond ((%array-header-p array)
	 (if (fixnump (%array-header-dimensions array))
	     (list (%array-header-dimensions array))
	     (%array-header-dimensions array)))
	(t (list (array-dimension array 0)))))

(defun array-dimension (array axis-number)
  (check-type array array)
  (check-type axis-number (integer 0) "a non-negative integer")
  (cond ((%array-header-p array)
	 (let* ((dims (%array-header-dimensions array))
		(rank (if (fixnump dims)
			  1
			  (list-length dims))))
	   (when (>= axis-number rank)
	     (error "Axis ~S exceeds array rank ~S." axis-number rank))
	   (if (fixnump dims)
	       dims
	       (nth axis-number dims))))
	(t (unless (zerop axis-number)
	     (error "Axis ~S exceeds array rank 1." axis-number))
	   (%simple-array-length array))))

(defun %row-major-aref (array index)
  "ROW-MAJOR-AREF with no bounds check."
  (if (%array-header-p array)
      (cond ((null (%array-header-dimensions array))
	     ;; 0D array, value is stored in the storage slot.
	     (%array-header-storage array))
	    ((null (%array-header-info array))
	     ;; Normal array, must be backed by a simple array.
	     (%simple-array-aref (%array-header-storage array) index))
	    ((fixnump (%array-header-info array))
	     ;; Displaced array.
	     (row-major-aref (%array-header-storage array) (+ index (%array-header-info array))))
	    (t ;; Direct memory access array.
	     (%memory-aref (%array-header-info array) (%array-header-storage array) index)))
      (%simple-array-aref array index)))

(defun aref-1 (array index)
  (unless (= (array-rank array) 1)
    (error "Invalid number of indices to array ~S." array))
  (when (>= index (array-dimension array 0))
    (error "Index ~S out of bounds. Must be 0 <= n < ~D~%"
           index (array-dimension array 0)))
  (%row-major-aref array index))

(defun elt (sequence index)
  (check-type sequence sequence)
  (if (listp sequence)
      ;; TODO: more error checking.
      (nth index sequence)
      (aref sequence index)))

(defun stringp (object)
  (or (simple-string-p object)
      (and (vectorp object)
           (let ((element-type (array-element-type object)))
             (or (eql element-type 'nil)
                 (eql element-type 'base-char)
                 (eql element-type 'character))))))

(defun string (x)
  (etypecase x
    (string x)
    (character (make-array 1 :element-type 'character :initial-element x))
    (symbol (symbol-name x))))

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
    "Returns true if STRING1 and STRING2 are the same length and contain the
same characters in the corresponding positions; otherwise it returns false."
  (compare-sequence 'char= (string string1) (string string2)
		    :start1 start1 :end1 end1
		    :start2 start2 :end2 end2))

(defun char= (c &rest args)
  (dolist (a args t)
    (when (not (eql a c))
      (return nil))))

(defun find-package-or-die (name)
  nil)
(defun find-package (name)
  nil)
(defun keywordp (object)
  (find object *initial-keyword-obarray*))
(defun find-symbol (name package)
  (let ((pkg (if (string= package "KEYWORD")
                 *initial-keyword-obarray*
                 *initial-obarray*)))
    (dotimes (i (length pkg) (values nil nil))
      (when (string= name (symbol-name (svref pkg i)))
        (return (values (svref pkg i) :internal))))))

;;; TODO: This requires a considerably more flexible mechanism.
;;; 12 is where the TLS slots in a stack group start.
(defparameter *next-symbol-tls-slot* 12)
(defconstant +maximum-tls-slot+ 512)
(defun %allocate-tls-slot (symbol)
  (when (>= *next-symbol-tls-slot* +maximum-tls-slot+)
    (error "Critial error! TLS slots exhausted!"))
  (let ((slot *next-symbol-tls-slot*))
    (incf *next-symbol-tls-slot*)
    (setf (ldb (byte 16 8) (%symbol-flags symbol)) slot)
    slot))

(defun proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (special (dolist (var (rest declaration-specifier))
               (setf (system:symbol-mode var) :special)))
    (constant (dolist (var (rest declaration-specifier))
                (setf (system:symbol-mode var) :constant)))))

(defun system:symbol-mode (symbol)
  (svref #(nil :special :constant :symbol-macro)
         (ldb (byte 2 0) (%symbol-flags symbol))))

(defun (setf system:symbol-mode) (value symbol)
  (setf (ldb (byte 2 0) (%symbol-flags symbol))
        (ecase value
          ((nil) +symbol-mode-nil+)
          ((:special) +symbol-mode-special+)
          ((:constant) +symbol-mode-constant+)
          ((:symbol-macro) +symbol-mode-symbol-macro+)))
  value)

(defun %defmacro (name function)
  (funcall #'(setf macro-function) function name))

(defun macro-function (symbol &optional env)
  (dolist (e env
           (get symbol '%macro-function))
    (when (eql (first e) :macros)
      (let ((fn (assoc symbol (rest e))))
        (when fn (return (cdr fn)))))))

(defun (setf macro-function) (value symbol &optional env)
  (when env
    (error "TODO: (Setf Macro-function) in environment."))
  (setf (symbol-function symbol) (lambda (&rest r)
                                   (declare (ignore r))
                                   (error 'undefined-function :name symbol))
        (get symbol '%macro-function) value))

(defun %defstruct (structure-type)
  (setf (get (structure-name structure-type) 'structure-type) structure-type))

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
        *package* nil
        *terminal-io* nil
        *standard-output* nil
        *standard-input* nil)
  ;; Initialize defstruct and patch up all the structure types.
  (bootstrap-defstruct)
  (dotimes (i (length *initial-structure-obarray*))
    (setf (%struct-slot (svref *initial-structure-obarray* i) 0) *structure-type-type*))
  (write-line "Hello, world.")
  (setf *print-base* 10.
        *print-escape* t
        *print-readably* nil)
  (dotimes (i (length *cold-toplevel-forms*))
    (funcall (svref *cold-toplevel-forms* i)))
  (write *initial-obarray*)
  (terpri)
  (write-char #\*)
  (write-char #\O)
  (write-char #\K)
  (loop (%hlt)))
