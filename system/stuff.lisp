;;;; A bunch of functions with no proper home.

(in-package :mezzano.internals)

(defconstant call-arguments-limit 500)
(defconstant lambda-parameters-limit 500)
(defconstant multiple-values-limit (+ 5 mezzano.supervisor::+thread-mv-slots-size+))

(unless (boundp 'lambda-list-keywords)
  (defconstant lambda-list-keywords '(&allow-other-keys &aux &body &environment &key &optional &rest &whole &closure &count)))

(declaim (inline null not))

(defun null (object)
  (if object
      'nil
      't))

(defun not (object)
  (if object
      'nil
      't))

(defun equal (x y)
  (cond
    ((eql x y))
    ((stringp x)
     (and (stringp y)
          (string= x y)))
    ((bit-vector-p x)
     (and (bit-vector-p y)
          (eql (length x) (length y))
          (dotimes (i (length x) t)
            (when (not (eql (bit x i) (bit y i)))
              (return nil)))))
    ((consp x)
     (loop
        (when (not (consp y))
          (return nil))
        (when (not (equal (car x) (car y)))
          (return nil))
        (setf x (cdr x)
              y (cdr y))
        (when (not (consp x))
          (return (equal x y)))))
    ((and (pathnamep x) (pathnamep y))
     (pathnames-equal x y))))

(define-compiler-macro equal (&whole whole x y)
  (declare (notinline typep)) ; ### Bootstrap hack.
  (when (or (not (or (symbolp x) (listp x)))
            (and (listp x)
                 (= (list-length x) 2)
                 (eql (first x) 'quote)))
    (rotatef x y))
  (cond
    ((or (not (or (symbolp y) (listp y)))
         (and (listp y)
              (= (list-length y) 2)
              (eql (first y) 'quote)))
     (let ((constant (if (not (or (symbolp y) (listp y)))
                         ;; Self-evaluating form.
                         y
                         ;; Quoted form.
                         (second y))))
       (typecase constant
         (symbol `(eq ,x ',constant))
         ((or number character)
          `(eql ,x ',constant))
         (cons
          (when (null (dotted-list-length constant))
            ;; Give up when faced with a circular list.
            (return-from equal whole))
          (let ((sym (gensym)))
            `(let ((,sym ,x))
               (and (consp ,sym)
                    (equal (car ,sym) ',(car constant))
                    (equal (cdr ,sym) ',(cdr constant))))))
         (string
          (let ((sym (gensym)))
            `(let ((,sym ,x))
               (and (stringp ,sym)
                    (string= ,sym ',constant)))))
         (bit-vector
          (let ((sym (gensym)))
            `(let ((,sym ,x))
               (and (bit-vector-p ,sym)
                    (eql (length ,sym) ,(length constant))
                    (every 'eql ,sym ',constant)))))
         (pathname
          (let ((sym (gensym)))
            `(let ((,sym ,x))
               (and (pathnamep ,sym)
                    (pathnames-equal ,sym ',constant)))))
         (t `(eq ,x ',constant)))))
    (t whole)))

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
    (hash-table
     (and (hash-table-p y)
          (eql (hash-table-count x)
               (hash-table-count y))
          (eql (hash-table-test x)
               (hash-table-test y))
          (block nil
            (maphash (lambda (k v)
                       (multiple-value-bind (other-v presentp)
                           (gethash k y)
                         (when (or (not presentp)
                                   (not (equalp v other-v)))
                           (return nil))))
                     x)
            t)))
    (structure-object
     (and (eq (class-of x) (class-of y))
          (loop
             with ty = (class-of x)
             for slot in (mezzano.clos:class-slots ty)
             for slot-name = (mezzano.clos:slot-definition-name slot)
             for fixed-vector = (mezzano.clos:structure-slot-definition-fixed-vector slot)
             do
               (if fixed-vector
                   (dotimes (i fixed-vector)
                     (when (not (equalp (%struct-vector-slot x ty slot-name i)
                                        (%struct-vector-slot y ty slot-name i)))
                       (return nil)))
                   (when (not (equalp (%struct-slot x ty slot-name)
                                      (%struct-slot y ty slot-name)))
                     (return nil)))
             finally
               (return t))))
    (t (equal x y))))

(defun macroexpand-1 (form &optional env)
  (declare (notinline typep)) ; ### Bootstrap hack.
  (cond ((symbolp form)
         (let ((var (mezzano.compiler::lookup-variable-in-environment form env)))
           (cond ((typep var 'mezzano.compiler::symbol-macro)
                  (values (mezzano.compiler::symbol-macro-expansion var) t))
                 (t
                  (values form nil)))))
        ((consp form)
         (let ((fn (macro-function (first form) env)))
           (if fn
               (values (funcall *macroexpand-hook* fn form env) t)
               (values form nil))))
        (t (values form nil))))

(defun macroexpand (form &optional env)
  (let ((did-expand nil))
    (do () (nil)
       (multiple-value-bind (expansion expanded-p)
           (macroexpand-1 form env)
         (if expanded-p
             (setf form expansion
                   did-expand t)
             (return (values form did-expand)))))))

(declaim (inline identity))
(defun identity (thing)
  thing)

(declaim (inline complement))
(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(defun set (symbol value)
  (setf (symbol-value symbol) value))

(defun remprop (symbol indicator)
  (remf (symbol-plist symbol) indicator))

(defun bsearch (item vector &key (start 0) end (stride 1) (key 'identity))
  "Locate ITEM using a binary search through VECTOR."
  ;; IMIN/IMAX are inclusive indicies.
  (do ((imin start)
       (imax (1- (truncate (or end (length vector)) stride))))
      ((< imax imin)
       nil)
    (let* ((imid (truncate (+ imin imax) 2))
           (elt (funcall key (aref vector (* imid stride)))))
      (cond ((< elt item) (setf imin (1+ imid)))
            ((> elt item) (setf imax (1- imid)))
            (t (return (* imid stride)))))))

;;;
;;; PRNG WELL512 based on the public domain algorithm by Chris Lomont
;;; as published in
;;; http://lomont.org/Math/Papers/2008/Lomont_PRNG_2008.pdf
;;;

(defstruct (random-state
             (:constructor %make-random-state (index bits)))
  index
  bits)

(defvar *random-state* (make-random-state t))

(defun make-random-state (&optional state)
  (case state
    ((t)
     (let ((state (%make-random-state 0 (make-array
                                         16
                                         :element-type '(unsigned-byte 32)
                                         :initial-element (get-universal-time)))))
       (dotimes (i 64)
         (%random state))
       state))
    ((nil)
     (%make-random-state (random-state-index *random-state*)
                         (copy-seq (random-state-bits *random-state*))))
    (otherwise
     (check-type state random-state)
     (%make-random-state (random-state-index state)
                         (copy-seq (random-state-bits state))))))

;;; Generate 32-bit random number
(defun %random (random-state)
  (let* ((idx (random-state-index random-state))
         (bits (random-state-bits random-state))
         (a (aref bits idx))
         (c (aref bits (ldb (byte 4 0) (+ idx 13))))
         (b (logxor a c (dpb a (byte 16 16) 0) (dpb c (byte 17 15) 0)))
         (d))
    (setf c (logxor c (ash c -11)))
    (setf a (setf (aref bits idx) (logxor b c)))
    (setf d  (logxor a (logand (dpb a (byte 27 5) 0) #xDA442D24)))
    (setf idx (ldb (byte 4 0) (+ idx 15)))
    (setf (random-state-index random-state) idx)
    (setf a (aref bits idx))
    (setf (aref bits idx) (logxor a b d
                                  (dpb a (byte 30 2) 0)
                                  (dpb b (byte 14 18) 0)
                                  (dpb c (byte 4 28) 0)))))

(defun random (limit &optional (random-state *random-state*))
  (unless (or (and (integerp limit) (> limit 0))
              (and (realp limit) (> limit 0.0)))
    (error 'type-error :expected-type '(or positive-fixnum positive-real)
                :datum limit))
  (let* ((r (%random random-state))
         (rd (/ (float r 1.0d0) (+ 1.0d0 (float #xFFFFFFFF 1.0d0)))))
    (etypecase limit
      ;; Using high bits of %random even though it takes more work
      ;; using floating point intermediaries
      (integer (values (truncate (* limit rd))))
      ;; 32-bits is too big for single precision so try again if rd
      ;; rounds up to 1.0f0
      (single-float
       (let ((rs (float rd 1.0f0)))
         (if (= rs 1.0f0)
             (random limit random-state)
             (* limit rs))))
      (double-float (* limit rd))
      ;; 32-bits is too big for half precision so try again if rd
      ;; rounds up to 1.0f0
      (short-float
       (let ((rs (float rd 1.0s0)))
         (if (= rs 1.0s0)
             (random limit random-state)
             (* limit rs)))))))

;;; Heap grovelling

(defun all-generic-functions ()
  (declare (notinline typep find-class))
  (remove (mezzano.clos:class-prototype (find-class 'standard-generic-function))
          (get-all-objects (lambda (object) (typep object 'standard-generic-function)))))

(defun get-all-objects (filter-function)
  (let ((objects (make-array 10000 :fill-pointer 0)))
    (loop
       (setf (fill-pointer objects) 0)
       ;; Don't include :CONS as it's just full of CONSes
       (dolist (area '(:general :pinned :wired :wired-function :function))
         (walk-area area
                    (lambda (object address size)
                      (declare (ignore address size))
                      (when (and (not (%object-of-type-p object +object-tag-freelist-entry+))
                                 (funcall filter-function object))
                        (vector-push object objects)))))
       (when (not (eql (fill-pointer objects) (array-dimension objects 0)))
         (return))
       (adjust-array objects (* (array-dimension objects 0) 2)))
    objects))

(defun get-all-object-references (object)
  "Find all objects that refer to OBJECT."
  (let* ((objects (make-array 10000 :fill-pointer 0))
         (helper (locally
                     ;; Hide this from the compiler.
                     ;; The closure needs to be created here, not brought
                     ;; forwards into the lambda passed to walk-all-areas.
                     (declare (notinline identity))
                   (identity
                    (lambda (container inner-object index)
                      (declare (ignore index))
                      (when (eql inner-object object)
                        (vector-push container objects)))))))
    (loop
       (setf (fill-pointer objects) 0)
       (walk-all-areas
        (lambda (object address size)
          (declare (ignore address size))
          (walk-object-references object helper)))
       (when (not (eql (fill-pointer objects) (array-dimension objects 0)))
         (return))
       (adjust-array objects (* (array-dimension objects 0) 2)))
    objects))

(defun contestable-lock-p (object)
  (or (mezzano.supervisor:mutex-p object)
      (mezzano.supervisor:rw-lock-p object)))

(defun lock-contested-count (lock)
  (if (mezzano.supervisor:mutex-p lock)
      (mezzano.supervisor:mutex-contested-count lock)
      (+ (mezzano.supervisor:rw-lock-read-contested-count lock)
         (mezzano.supervisor:rw-lock-write-contested-count lock))))

(defun print-contested-mutexes (&key (threshold 0))
  (let ((mutexes (sort
                  (remove-if
                   (lambda (m)
                     (<= (lock-contested-count m) threshold))
                   (get-all-objects #'contestable-lock-p))
                  #'>
                  :key #'lock-contested-count)))
    (loop
       for m across mutexes
       do (format t "~S: ~:D times.~%" m (lock-contested-count m)))))

;;; Memory grovelling

(defun hexdump (start length &key memory width endian)
  (check-type width (member nil 8 16 32 64))
  (check-type endian (member nil :little :big))
  (setf width (or width 8))
  (setf endian (or endian :little))
  (cond ((eql memory :virtual)
         (setf memory #'sys.int::memref-unsigned-byte-8))
        ((member memory '(nil :physical))
         (setf memory #'mezzano.supervisor::physical-memref-unsigned-byte-8)))
  (loop
     for line from start below (+ start length) by 16
     do
       (format t "~8,'0X |" line)
       (ecase width
         (8
          (loop
             for offset below 16
             do
               (when (eql offset 8)
                 (write-char #\Space))
               (format t " ~2,'0X" (funcall memory (+ line offset)))))
         (16
          (loop
             for offset below 16 by 2
             do
               (when (eql offset 8)
                 (write-char #\Space))
               (let ((b0 (funcall memory (+ line offset)))
                     (b1 (funcall memory (+ line offset 1))))
                 (when (eql endian :big)
                   (rotatef b0 b1))
                 (format t " ~2,'0X~2,'0X" b1 b0))))
         (32
          (loop
             for offset below 16 by 4
             do
               (when (eql offset 8)
                 (write-char #\Space))
               (let ((b0 (funcall memory (+ line offset)))
                     (b1 (funcall memory (+ line offset 1)))
                     (b2 (funcall memory (+ line offset 2)))
                     (b3 (funcall memory (+ line offset 3))))
                 (when (eql endian :big)
                   (psetq b0 b3
                          b1 b2
                          b2 b1
                          b3 b0))
                 (format t " ~2,'0X~2,'0X~2,'0X~2,'0X" b3 b2 b1 b0))))
         (64
          (loop
             for offset below 16 by 8
             do
               (when (eql offset 8)
                 (write-char #\Space))
               (let ((b0 (funcall memory (+ line offset)))
                     (b1 (funcall memory (+ line offset 1)))
                     (b2 (funcall memory (+ line offset 2)))
                     (b3 (funcall memory (+ line offset 3)))
                     (b4 (funcall memory (+ line offset 4)))
                     (b5 (funcall memory (+ line offset 5)))
                     (b6 (funcall memory (+ line offset 6)))
                     (b7 (funcall memory (+ line offset 7))))
                 (when (eql endian :big)
                   (psetq b0 b7
                          b1 b6
                          b2 b5
                          b3 b4
                          b4 b3
                          b5 b2
                          b6 b1
                          b7 b0))
                 (format t " ~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X"
                         b7 b6 b5 b4 b3 b2 b1 b0)))))
       (format t " | ")
       (loop
          for offset below 16
          do
            (when (eql offset 8)
              (write-char #\Space))
            (let ((ch (funcall memory (+ line offset))))
              (if (<= #x20 ch #x7E)
                  (write-char (code-char ch))
                  (write-char #\Snowman))))
       (terpri))
  (values))
