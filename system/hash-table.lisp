;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defvar *hash-table-unbound-value* (list "unbound hash-table entry"))
(defvar *hash-table-tombstone* (list "hash-table tombstone"))

(defstruct (hash-table
             (:constructor %make-hash-table))
  (test 'eql :type (member eq eql equal equalp) :read-only t)
  (hash-function nil :read-only t)
  (count 0)
  (used 0)
  rehash-size
  rehash-threshold
  (storage (error "No storage provided.") :type simple-vector)
  (storage-epoch *gc-epoch*)
  (lock (mezzano.supervisor:make-mutex "Hash-table lock")))

(defun hash-table-size (hash-table)
  (ash (%object-header-data (hash-table-storage hash-table)) -1))

(defun hash-table-key-at (hash-table index)
  (svref (hash-table-storage hash-table) (* index 2)))

(defun (setf hash-table-key-at) (value hash-table index)
  (setf (svref (hash-table-storage hash-table) (* index 2)) value))

(defun hash-table-value-at (hash-table index)
  (svref (hash-table-storage hash-table) (1+ (* index 2))))

(defun (setf hash-table-value-at) (value hash-table index)
  (setf (svref (hash-table-storage hash-table) (1+ (* index 2))) value))

;; Hmmm. Improves the check-type forms, but shouldn't this be
;; done automatically by defstruct?
(deftype hash-table ()
  `(satisfies hash-table-p))

(defun hash-table-test-hash-function (test)
  (ecase test
    ((eq) 'eq-hash)
    ((eql) 'eql-hash)
    ((equal) 'sxhash)
    ((equalp) 'equalp-hash)))

(defun make-hash-table (&key (test 'eql) (size 101) (rehash-size 2.5) (rehash-threshold 0.5))
  ;; Canonicalize and check the test function
  (cond ((eql test #'eq) (setf test 'eq))
        ((eql test #'eql) (setf test 'eql))
        ((eql test #'equal) (setf test 'equal))
        ((eql test #'equalp) (setf test 'equalp)))
  (check-type test (member eq eql equal equalp))
  (check-type size (integer 0) "a non-negative integer")
  (check-type rehash-size (or (integer 1 *) (float (1.0) *)))
  (check-type rehash-threshold (real 0 1))
  (%make-hash-table :test test
                    :hash-function (hash-table-test-hash-function test)
                    :rehash-size rehash-size
                    :rehash-threshold rehash-threshold
                    :storage (make-array (* size 2) :initial-element *hash-table-unbound-value*)))

(defun gethash (key hash-table &optional default)
  (check-type hash-table hash-table)
  (mezzano.supervisor:with-mutex ((hash-table-lock hash-table))
    (let ((slot (find-hash-table-slot key hash-table)))
      (if slot
          (values (hash-table-value-at hash-table slot) t)
          (values default nil)))))

(defun (setf gethash) (value key hash-table &optional default)
  (declare (ignore default))
  (check-type hash-table hash-table)
  (mezzano.supervisor:with-mutex ((hash-table-lock hash-table))
    (multiple-value-bind (slot free-slot)
        (find-hash-table-slot key hash-table)
      (cond
        (slot
         ;; Replacing an existing entry
         (setf (hash-table-value-at hash-table slot) value))
        ;; Adding a new entry.
        ((or (and (eq (hash-table-key-at hash-table free-slot) *hash-table-unbound-value*)
                  (= (1+ (hash-table-used hash-table)) (hash-table-size hash-table)))
             (>= (/ (float (hash-table-count hash-table)) (float (hash-table-size hash-table)))
                 (hash-table-rehash-threshold hash-table)))
         ;; There must always be at least one unbound slot in the hash table.
         (hash-table-rehash hash-table t)
         (multiple-value-bind (slot free-slot)
             (find-hash-table-slot key hash-table)
           (declare (ignore slot))
           (when (and (eq (hash-table-key-at hash-table free-slot) *hash-table-unbound-value*)
                      (= (1+ (hash-table-used hash-table)) (hash-table-size hash-table)))
             ;; Can't happen. Resizing the hash-table adds new slots.
             (error "Impossible!"))
           (unless (eql (hash-table-key-at hash-table free-slot) *hash-table-tombstone*)
             (incf (hash-table-used hash-table)))
           (incf (hash-table-count hash-table))
           (setf (hash-table-key-at hash-table free-slot) key
                 (hash-table-value-at hash-table free-slot) value)))
        ;; No rehash/resize needed. Insert directly.
        (t (unless (eql (hash-table-key-at hash-table free-slot) *hash-table-tombstone*)
             (incf (hash-table-used hash-table)))
           (incf (hash-table-count hash-table))
           (setf (hash-table-key-at hash-table free-slot) key
                 (hash-table-value-at hash-table free-slot) value))))))

(defun remhash (key hash-table)
  (check-type hash-table hash-table)
  (mezzano.supervisor:with-mutex ((hash-table-lock hash-table))
    (let ((slot (find-hash-table-slot key hash-table)))
      (when slot
        ;; Entry exists.
        (setf (hash-table-key-at hash-table slot) *hash-table-tombstone*
              (hash-table-value-at hash-table slot) *hash-table-tombstone*)
        (decf (hash-table-count hash-table))
        t))))

(defun clrhash (hash-table)
  (check-type hash-table hash-table)
  (mezzano.supervisor:with-mutex ((hash-table-lock hash-table))
    (setf (hash-table-count hash-table) 0
          (hash-table-used hash-table) 0
          (hash-table-storage hash-table) (make-array (length (hash-table-storage hash-table))
                                                      :initial-element *hash-table-unbound-value*))
    hash-table))

(defun find-hash-table-slot-1 (key hash-table)
  (do* ((free-slot nil)
        (hash (funcall (hash-table-hash-function hash-table) key))
        (size (hash-table-size hash-table))
        (test (hash-table-test hash-table))
        (storage (hash-table-storage hash-table))
        (unbound-marker *hash-table-unbound-value*)
        (tombstone *hash-table-tombstone*)
        ;; This hash implementation is inspired by the Python dict implementation.
        (slot (logand hash #xffffffff) (logand #xffffffff (+ (* slot 5) perturb 1)))
        (perturb hash (ash perturb -5)))
       (nil)
    (let* ((offset (rem slot size))
           (slot-key (svref storage (ash offset 1)))) ; hash-table-key-at
      (when (and (null free-slot) (or (eq slot-key unbound-marker)
                                      (eq slot-key tombstone)))
        (setf free-slot offset))
      (when (eq slot-key unbound-marker)
        ;; Unbound value marks the end of this run.
        (return (values nil free-slot)))
      (when (funcall test key slot-key)
        (return (values offset free-slot))))))

(defun find-hash-table-slot (key hash-table)
  "Locate the slot matching KEY. Returns NIL if KEY is not in HASH-TABLE.
The second return value is the first available/empty slot in the hash-table for KEY.
Requires at least one completely unbound slot to terminate."
  (loop
     (let ((epoch *gc-epoch*))
       (multiple-value-bind (offset free-slot)
           (find-hash-table-slot-1 key hash-table)
         (when (and (eql epoch *gc-epoch*)
                    (eql (hash-table-storage-epoch hash-table) *gc-epoch*))
           (return (values offset free-slot)))
         (hash-table-rehash hash-table nil)))))

(defun hash-table-rehash (hash-table resize-p)
  "Resize and rehash HASH-TABLE so that there are no tombstones the usage
is below the rehash-threshold."
  (let ((new-size (if resize-p
                      (if (floatp (hash-table-rehash-size hash-table))
                          (ceiling (* (hash-table-size hash-table) (hash-table-rehash-size hash-table)))
                          (+ (hash-table-size hash-table) (hash-table-rehash-size hash-table)))
                      (hash-table-size hash-table)))
        (old-size (hash-table-size hash-table))
        (old-storage (hash-table-storage hash-table)))
    (setf (hash-table-storage hash-table) (make-array (* new-size 2)
                                                      :initial-element *hash-table-unbound-value*)
          (hash-table-count hash-table) 0
          (hash-table-used hash-table) 0
          (hash-table-storage-epoch hash-table) *gc-epoch*)
    (dotimes (i old-size hash-table)
      (let ((key (svref old-storage (* i 2)))
            (value (svref old-storage (1+ (* i 2)))))
        (unless (or (eq key *hash-table-unbound-value*)
                    (eq value *hash-table-tombstone*))
          (multiple-value-bind (slot free-slot)
              (find-hash-table-slot-1 key hash-table)
            (when slot
              (error "Duplicate key ~S in hash-table?" key))
            (incf (hash-table-used hash-table))
            (incf (hash-table-count hash-table))
            (setf (hash-table-key-at hash-table free-slot) key
                  (hash-table-value-at hash-table free-slot) value)))))))

(defun make-hash-table-iterator (hash-table)
  (declare (type hash-table hash-table))
  (cons hash-table 0))

(defun hash-table-iterator-next (iterator)
  (let ((ht (car iterator)))
    (do () ((>= (cdr iterator) (hash-table-size ht)))
      (let ((key (hash-table-key-at ht (cdr iterator)))
            (value (hash-table-value-at ht (cdr iterator))))
        ;; Increment the key until a non-unbound/-tombstone key is found.
        (incf (cdr iterator))
        (unless (or (eq key *hash-table-unbound-value*)
                    (eq key *hash-table-tombstone*))
          (return (values t key value)))))))

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (let ((sym (gensym (symbol-name name))))
    `(let ((,sym (make-hash-table-iterator ,hash-table)))
       (macrolet ((,name () '(hash-table-iterator-next ,sym)))
         ,@body))))

(defun maphash (function hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (do () (nil)
      (multiple-value-bind (more key value) (next-entry)
        (unless more (return nil))
        (funcall function key value)))))

(defun eq-hash (object)
  (lisp-object-address object))

(defun hash-bignum (bignum)
  (let ((n (logxor (%n-bignum-fragments bignum)
                   #xb89a91d9)))
    (dotimes (i (%n-bignum-fragments bignum))
      (setf n (logxor n (%bignum-fragment bignum i))))
    n))

(defun eql-hash (object)
  ;; Future note: double-floats and other float types will need to be
  ;; special-cased here as well.
  (cond ((bignump object)
         (hash-bignum object))
        ((ratiop object)
         (logxor (eql-hash (numerator object))
                 (eql-hash (denominator object))))
        ((complexp object)
         (logxor (eql-hash (realpart object))
                 (eql-hash (imagpart object))))
        ((double-float-p object)
         (%double-float-as-integer object))
        (t
         ;; Fixnums, single-floats, and characters are immediate objects and
         ;; can be safely hashed by their "address".
         (eq-hash object))))

(defun sxhash-1 (object depth)
  (if (zerop depth)
      #x12345678
      (typecase object
        (bit-vector 0) ; TODO. could copy the bitvector, then munge it into a bignum. nasty.
        (cons (logxor (sxhash-1 (car object) (1- depth))
                      (sxhash-1 (cdr object) (1- depth))))
        (pathname (sxhash-1 (namestring object) (1- depth)))
        (string
         ;; djb2 string hash
         ;; We use 25-bit characters (unicode+bucky bits), instead of 8-bit chars.
         ;; I'm unsure how that'll change the behaviour of the hash function
         (let ((hash 5381))
           (dotimes (i (length object) hash)
             (setf hash (logand #xFFFFFFFF (+ (logand #xFFFFFFFF (* hash 33))
                                              (char-int (char object i))))))))
        (symbol
         (sxhash-1 (string object) depth))
        ;; EQL-HASH also works for characters and numbers.
        (t (eql-hash object)))))

(defun sxhash (object)
  (sxhash-1 object 10))

(defun equalp-hash (object &optional (depth 10))
  ;; this is woefully incomplete...
  (if (zerop depth)
      #x12345678
      (typecase object
        (cons (logxor (equalp-hash (car object) (1- depth))
                      (equalp-hash (cdr object) (1- depth))))
        ;;(pathname ...)
        (character (char-int (char-upcase object)))
        (string
         ;; djb2 string hash
         ;; We use 25-bit characters (unicode+bucky bits), instead of 8-bit chars.
         ;; I'm unsure how that'll change the behaviour of the hash function
         (let ((hash 5381))
           (dotimes (i (length object) hash)
             (setf hash (logand #xFFFFFFFF (+ (logand #xFFFFFFFF (* hash 33))
                                              (char-int (char-upcase (char object i)))))))))
        (symbol (eql-hash object))
        (t 0))))
