;;;; Hash tables

(in-package :mezzano.internals)

(defglobal *hash-table-unbound-value*)
(defglobal *hash-table-tombstone*)

(defun align-up-to-power-of-two (integer)
  "Align INTEGER up to the next power of two."
  (if (zerop integer)
      0
      (ash 1 (integer-length (1- integer)))))

(defstruct (hash-table
             (:constructor %make-hash-table))
  (test 'eql :type (member eq eql equal equalp) :read-only t)
  (test-function (error "Unreachable, missing test-function") :type function :read-only t)
  (hash-function (error "Unreachable, missing hash-function") :type function :read-only t)
  (gethash-fn nil :type (or null function))
  (puthash-fn nil :type (or null function))
  (cashash-fn nil :type (or null function))
  (remhash-fn nil :type (or null function))
  (%count 0)
  (used 0)
  (rehash-size 1 :type (or (integer 1 *) (float (1.0) *)))
  (rehash-threshold 0 :type (real 0 1))
  (storage (error "No storage provided.") :type simple-vector)
  (storage-epoch *gc-epoch*)
  (synchronized nil :read-only t)
  lock
  ;; If true, then all keys are stable over GCs and no rehashing is required
  ;; when the GC epoch changes.
  (gc-invariant t)
  (weakness nil :read-only t :type (member nil :key :value :key-or-value :key-and-value)))

(declaim (inline %make-hash-table))

(defun object-hash-gc-invariant-under-test (object test)
  "Returns true if an object's hash will stay the same after a garbage collection cycle."
  (ecase test
    (eq
     ;; All immediate & pinned objects are invariant.
     (or (immediatep object)
         (eql (ldb +address-tag+ (lisp-object-address object))
              +address-tag-pinned+)))
    (eql
     ;; As with EQ, plus numbers and symbols.
     (or (immediatep object)
         (eql (ldb +address-tag+ (lisp-object-address object))
              +address-tag-pinned+)
         (numberp object)
         (symbolp object)))
    (equal
     (labels ((frob (object depth)
                (when (zerop depth)
                  ;; Hard limit on recursion limit for conses, to avoid more complicated
                  ;; circularity checks.
                  ;; TODO: Do the normal fast/slow circularity check and chase
                  ;; all the way down the cdr.
                  (return-from frob nil))
                (or (immediatep object)
                    (and (eql (ldb +address-tag+ (lisp-object-address object))
                              +address-tag-pinned+)
                         ;; Pathnames are never pinned and conses are checked below.
                         (not (consp object)))
                    (numberp object)
                    (stringp object)
                    (symbolp object)
                    (bit-vector-p object)
                    (pathnamep object)
                    (and (consp object)
                         (frob (car object) (1- depth))
                         (frob (cdr object) (1- depth))))))
       (frob object 10)))
    (equalp
     ;; Don't allow arbitrary pinned heap objects. EQUALP gets really hairy.
     (or (immediatep object)
         (numberp object)
         (symbolp object)
         (stringp object)
         (bit-vector-p object)
         (pathnamep object)))))

(defun hash-table-size (hash-table)
  (let ((storage (hash-table-storage hash-table)))
    (if (hash-table-weakness hash-table)
        (weak-hash-table-size storage)
        (strong-hash-table-size storage))))

(defun hash-table-count (hash-table)
  (hash-table-%count hash-table))

(declaim (inline strong-hash-table-size))
(defun strong-hash-table-size (storage)
  (the fixnum (ash (the fixnum (%object-header-data storage)) -1)))

(defun weak-hash-table-size (storage)
  (%object-header-data storage))

;; For strong hash-tables only.
(declaim (inline hash-table-key-at (setf hash-table-key-at)
                 hash-table-value-at (setf hash-table-value-at)))
(defun hash-table-key-at (hash-table index)
  (aref (hash-table-storage hash-table) (the fixnum (ash index 1))))
(defun (setf hash-table-key-at) (value hash-table index)
  (setf (aref (hash-table-storage hash-table) (the fixnum (ash index 1))) value))
(defun hash-table-value-at (hash-table index)
  (aref (hash-table-storage hash-table) (the fixnum (1+ (the fixnum (ash index 1))))))
(defun (setf hash-table-value-at) (value hash-table index)
  (setf (aref (hash-table-storage hash-table) (the fixnum (1+ (the fixnum (ash index 1))))) value))

(defun weak-hash-table-entry-at (hash-table slot)
  (aref (hash-table-storage hash-table) slot))

;; Value should be either the unbound value, tombstone, or a weak pointer.
(defun (setf weak-hash-table-entry-at) (value hash-table slot)
  (setf (aref (hash-table-storage hash-table) slot) value))

(defun set-full-weak-hash-table-entry (hash-table slot key value)
  (setf (weak-hash-table-entry-at hash-table slot)
        (make-weak-pointer key :value value :weakness (hash-table-weakness hash-table))))

(defun hash-table-test-hash-function (test)
  (ecase test
    ((eq) #'eq-hash)
    ((eql) #'eql-hash)
    ((equal) #'sxhash)
    ((equalp) #'equalp-hash)))

(defun hash-table-enforce-gc-invariant-keys (hash-table)
  (eql (hash-table-gc-invariant hash-table) :mandatory))

(defmacro with-hash-table-lock ((hash-table) &body body)
  (let ((sym (gensym)))
    `(let ((,sym ,hash-table))
       (flet ((doit ()
                (block nil
                  ,@body)))
         (declare (dynamic-extent #'doit))
         (if (hash-table-synchronized ,sym)
             (mezzano.supervisor:without-footholds ()
               (mezzano.supervisor:with-mutex ((hash-table-lock ,sym))
                 (doit)))
             (doit))))))

;;; Unsynchronized hash-tables are safe for use with concurrent readers
;;; as long as they only contains gc-invariant keys.
(defun make-hash-table (&key (test 'eql) (size 128) (rehash-size 2.5) (rehash-threshold 0.5) synchronized enforce-gc-invariant-keys weakness)
  ;; Canonicalize and check the test function
  (cond ((eql test #'eq) (setf test 'eq))
        ((eql test #'eql) (setf test 'eql))
        ((eql test #'equal) (setf test 'equal))
        ((eql test #'equalp) (setf test 'equalp)))
  (check-type test (member eq eql equal equalp))
  (check-type size (integer 0) "a non-negative integer")
  (check-type rehash-size (or (integer 1 *) (float (1.0) *)))
  (check-type rehash-threshold (real 0 1))
  (check-type weakness (member nil :key :value :key-or-value :key-and-value))
  (setf size (align-up-to-power-of-two size))
  (let ((ht (%make-hash-table :test test
                              :test-function (%coerce-to-callable test)
                              :hash-function (hash-table-test-hash-function test)
                              :rehash-size rehash-size
                              :rehash-threshold rehash-threshold
                              :storage (make-array (if weakness size (* size 2)) :initial-element *hash-table-unbound-value*)
                              :synchronized (if synchronized t nil)
                              :gc-invariant (if enforce-gc-invariant-keys
                                                :mandatory
                                                t)
                              :weakness weakness)))
    (when synchronized
      (setf (hash-table-lock ht) (mezzano.supervisor:make-mutex ht)))
    (multiple-value-bind (fast-gethash fast-puthash fast-cashash fast-remhash)
        (if weakness
            (ecase test
              ((eq) (values #'weak-gethash-standard-eq #'weak-puthash-standard-eq #'weak-cashash-standard-eq #'weak-remhash-standard-eq))
              ((eql) (values #'weak-gethash-standard-eql #'weak-puthash-standard-eql #'weak-cashash-standard-eql #'weak-remhash-standard-eql))
              ((equal) (values #'weak-gethash-standard-equal #'weak-puthash-standard-equal #'weak-cashash-standard-equal #'weak-remhash-standard-equal))
              ((equalp) (values #'weak-gethash-standard-equalp #'weak-puthash-standard-equalp #'weak-cashash-standard-equalp #'weak-remhash-standard-equalp)))
            (ecase test
              ((eq) (values #'gethash-standard-eq #'puthash-standard-eq #'cashash-standard-eq #'remhash-standard-eq))
              ((eql) (values #'gethash-standard-eql #'puthash-standard-eql #'cashash-standard-eql #'remhash-standard-eql))
              ((equal) (values #'gethash-standard-equal #'puthash-standard-equal #'cashash-standard-equal #'remhash-standard-equal))
              ((equalp) (values #'gethash-standard-equalp #'puthash-standard-equalp #'cashash-standard-equalp #'remhash-standard-equalp))))
      (setf (hash-table-gethash-fn ht) (if synchronized
                                           (lambda (key hash-table default)
                                             (declare (lambda-name fast-gethash-with-lock))
                                             (with-hash-table-lock (hash-table)
                                               (funcall fast-gethash key hash-table default)))
                                           fast-gethash)
            (hash-table-puthash-fn ht) (if synchronized
                                           (lambda (value key hash-table)
                                             (declare (lambda-name fast-puthash-with-lock))
                                             (with-hash-table-lock (hash-table)
                                               (funcall fast-puthash value key hash-table)))
                                           fast-puthash)
            (hash-table-cashash-fn ht) (if synchronized
                                           (lambda (old-value new-value key hash-table default)
                                             (declare (lambda-name fast-cashash-with-lock))
                                             (with-hash-table-lock (hash-table)
                                               (funcall fast-cashash old-value new-value key hash-table default)))
                                           fast-cashash)
            (hash-table-remhash-fn ht) (if synchronized
                                           (lambda (key hash-table)
                                             (declare (lambda-name fast-remhash-with-lock))
                                             (with-hash-table-lock (hash-table)
                                               (funcall fast-remhash key hash-table)))
                                           fast-remhash)))
    ht))

(defun hash-table-update-gc-invariant (hash-table key)
  (when (and (hash-table-gc-invariant hash-table)
             (not (object-hash-gc-invariant-under-test key (hash-table-test hash-table))))
    (when (eql (hash-table-gc-invariant hash-table) :mandatory)
      (error "Hash-table key ~S is not gc-invariant under hash-table test ~S"
             key (hash-table-test hash-table)))
    ;; Hash table is no longer invariant.
    (setf (hash-table-gc-invariant hash-table) nil)))

(defun gethash (key hash-table &optional default)
  (check-type hash-table hash-table)
  (let ((fast-fn (hash-table-gethash-fn hash-table)))
    (declare (optimize speed (safety 0)))
    (funcall (the function fast-fn) key hash-table default)))

(defun (setf gethash) (value key hash-table &optional default)
  (declare (ignore default))
  (check-type hash-table hash-table)
  (let ((fast-fn (hash-table-puthash-fn hash-table)))
    (declare (optimize speed (safety 0)))
    (funcall (the function fast-fn) value key hash-table))
  value)

(defun (cas gethash) (old-value new-value key hash-table &optional default)
  (check-type hash-table hash-table)
  (let ((fast-fn (hash-table-cashash-fn hash-table)))
    (declare (optimize speed (safety 0)))
    (funcall (the function fast-fn) old-value new-value key hash-table default)))

(defun remhash (key hash-table)
  (check-type hash-table hash-table)
  (let ((fast-fn (hash-table-remhash-fn hash-table)))
    (declare (optimize speed (safety 0)))
    (funcall (the function fast-fn) key hash-table)))

(defun clrhash (hash-table)
  (check-type hash-table hash-table)
  (with-hash-table-lock (hash-table)
    (setf (hash-table-%count hash-table) 0
          (hash-table-used hash-table) 0
          ;; Make sure to preserve :MANDATORY.
          (hash-table-gc-invariant hash-table) (or (hash-table-gc-invariant hash-table) t)
          (hash-table-storage hash-table) (make-array (length (hash-table-storage hash-table))
                                                      :initial-element *hash-table-unbound-value*))
    hash-table))

;; Since strong & weak hash tables have different internal storage representations,
;; this function is needed to paper over the differences in access methods.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun probe-hash-table-slot (weakp storage offset unbound-marker tombstone free-slot test-fn key)
    (if weakp
        `(let* ((offset ,offset)
                (slot-entry (aref ,storage offset)))
           (multiple-value-bind (slot-key livep)
               (if (or (eq slot-entry ,unbound-marker)
                       (eq slot-entry ,tombstone))
                   (values nil nil)
                   (weak-pointer-key slot-entry))
             (when (and (null ,free-slot)
                        (not livep))
               (setf ,free-slot offset))
             (when (eq slot-entry ,unbound-marker)
               ;; Unbound value marks the end of this run.
               (return (values nil ,free-slot)))
             ;; Dead entries and tombstones report NIL as their key. They
             ;; must not match a live NIL key being looked up.
             (when (and livep (,test-fn ,key slot-key))
               (return (values offset ,free-slot)))))
        `(let* ((offset ,offset)
                (slot-key (aref ,storage (the fixnum (ash offset 1))))) ; hash-table-key-at
           (declare (type fixnum offset))
           (when (and (null ,free-slot)
                      (or (eq slot-key ,unbound-marker)
                          (eq slot-key ,tombstone)))
             (setf ,free-slot offset))
           (when (eq slot-key ,unbound-marker)
             ;; Unbound value marks the end of this run.
             (return (values nil ,free-slot)))
           (when (,test-fn ,key slot-key)
             (return (values offset ,free-slot)))))))

(defmacro define-optimized-hash-table-functions-internal
    (name test-fn hash-fn weakp)
  (let* ((weak-name (if weakp "WEAK-" ""))
         (gethash (intern (format nil "~AGETHASH-~A" weak-name name) (symbol-package name)))
         (puthash (intern (format nil "~APUTHASH-~A" weak-name name) (symbol-package name)))
         (cashash (intern (format nil "~ACASHASH-~A" weak-name name) (symbol-package name)))
         (remhash (intern (format nil "~AREMHASH-~A" weak-name name) (symbol-package name)))
         (find-hash-table-slot (intern (format nil "%~AFIND-HASH-TABLE-SLOT-~A" weak-name name) (symbol-package name)))
         (find-hash-table-slot-1 (intern (format nil "%~AFIND-HASH-TABLE-SLOT-1-~A" weak-name name) (symbol-package name)))
         (hash-table-rehash (intern (format nil "%~AHASH-TABLE-REHASH-~A" weak-name name) (symbol-package name))))
    `(progn
       ;; gethash
       (defun ,gethash (key hash-table default)
         (declare (optimize speed (safety 0))
                  (type hash-table hash-table))
         (let ((slot (,find-hash-table-slot key hash-table)))
           (if slot
               (locally
                   (declare (type fixnum slot))
                 ,(if weakp
                      `(multiple-value-bind (value livep)
                           (weak-pointer-value (weak-hash-table-entry-at hash-table slot))
                         (if livep
                             (values value t)
                             (values default nil)))
                      `(values (hash-table-value-at hash-table slot) t)))
               (values default nil))))
       ;; puthash
       (defun ,puthash (value key hash-table)
         (multiple-value-bind (slot free-slot)
             (,find-hash-table-slot key hash-table)
           ;; Finding the slot can cause a rehash, which can reset the invariant state.
           ;; So update the invariant state *after* finding it, to avoid the update getting lost.
           ;; This can occur if this is the first key being put in an a hash table that
           ;; was created in an older GC epoch.
           (hash-table-update-gc-invariant hash-table key)
           (cond
             (slot
              ;; Replacing an existing entry
              ,(if weakp
                   `(set-full-weak-hash-table-entry hash-table slot key value)
                   `(setf (hash-table-value-at hash-table slot) value)))
             ;; Adding a new entry.
             ((or (and (eq ,(if weakp
                                `(weak-hash-table-entry-at hash-table free-slot)
                                `(hash-table-key-at hash-table free-slot))
                           *hash-table-unbound-value*)
                       (= (1+ (hash-table-used hash-table)) (hash-table-size hash-table)))
                  (>= (/ (float (hash-table-%count hash-table)) (float (hash-table-size hash-table)))
                      (hash-table-rehash-threshold hash-table)))
              ;; There must always be at least one unbound slot in the hash table.
              (,hash-table-rehash hash-table t)
              ;; Since we haven't inserted yet, rehash would have lost the invariantness of
              ;; the key we're inserting. Do it again to make sure.
              (hash-table-update-gc-invariant hash-table key)
              (multiple-value-bind (slot free-slot)
                  (,find-hash-table-slot key hash-table)
                (declare (ignore slot))
                (when (and (eq ,(if weakp
                                    `(weak-hash-table-entry-at hash-table free-slot)
                                    `(hash-table-key-at hash-table free-slot))
                               *hash-table-unbound-value*)
                           (= (1+ (hash-table-used hash-table)) (hash-table-size hash-table)))
                  ;; Can't happen. Resizing the hash-table adds new slots.
                  (error "Impossible!"))
                (unless (eq ,(if weakp
                                 `(weak-hash-table-entry-at hash-table free-slot)
                                 `(hash-table-key-at hash-table free-slot))
                            *hash-table-tombstone*)
                  (incf (hash-table-used hash-table)))
                (incf (hash-table-%count hash-table))
                ,(if weakp
                     `(set-full-weak-hash-table-entry hash-table free-slot key value)
                     `(setf (hash-table-key-at hash-table free-slot) key
                            (hash-table-value-at hash-table free-slot) value))))
             ;; No rehash/resize needed. Insert directly.
             (t (unless (eq ,(if weakp
                                 `(weak-hash-table-entry-at hash-table free-slot)
                                 `(hash-table-key-at hash-table free-slot))
                            *hash-table-tombstone*)
                  (incf (hash-table-used hash-table)))
                (incf (hash-table-%count hash-table))
                ,(if weakp
                     `(set-full-weak-hash-table-entry hash-table free-slot key value)
                     `(setf (hash-table-key-at hash-table free-slot) key
                            (hash-table-value-at hash-table free-slot) value))))))
       (defun ,cashash (old-value new-value key hash-table default)
         (multiple-value-bind (slot free-slot)
             (,find-hash-table-slot key hash-table)
           (hash-table-update-gc-invariant hash-table key)
           (cond
             (slot
              ;; Replacing an existing entry
              ,(if weakp
                   `(multiple-value-bind (current livep)
                        (weak-pointer-value (weak-hash-table-entry-at hash-table slot))
                      (when (not livep)
                        (setf current default))
                      (when (eq current old-value)
                        (set-full-weak-hash-table-entry hash-table slot key new-value))
                      current)
                   `(let ((current (hash-table-value-at hash-table slot)))
                      (when (eq current old-value)
                        (setf (hash-table-value-at hash-table slot) new-value))
                      current)))
             ;; Adding a new entry.
             ((or (and (eq (,(if weakp
                                 `weak-hash-table-entry-at
                                 `hash-table-key-at)
                            hash-table free-slot)
                           *hash-table-unbound-value*)
                       (= (1+ (hash-table-used hash-table)) (hash-table-size hash-table)))
                  (>= (/ (float (hash-table-%count hash-table)) (float (hash-table-size hash-table)))
                      (hash-table-rehash-threshold hash-table)))
              (when (not (eq old-value default))
                (return-from ,cashash default))
              ;; There must always be at least one unbound slot in the hash table.
              (,hash-table-rehash hash-table t)
              ;; Since we haven't inserted yet, rehash would have lost the invariantness of
              ;; the key we're inserting. Do it again to make sure.
              (hash-table-update-gc-invariant hash-table key)
              (multiple-value-bind (slot free-slot)
                  (,find-hash-table-slot key hash-table)
                (declare (ignore slot))
                (when (and (eq (,(if weakp
                                     `weak-hash-table-entry-at
                                     `hash-table-key-at)
                                hash-table free-slot)
                               *hash-table-unbound-value*)
                           (= (1+ (hash-table-used hash-table)) (hash-table-size hash-table)))
                  ;; Can't happen. Resizing the hash-table adds new slots.
                  (error "Impossible!"))
                (unless (eq (,(if weakp
                                  `weak-hash-table-entry-at
                                  `hash-table-key-at)
                             hash-table free-slot)
                            *hash-table-tombstone*)
                  (incf (hash-table-used hash-table)))
                (incf (hash-table-%count hash-table))
                ,(if weakp
                     `(set-full-weak-hash-table-entry hash-table free-slot key new-value)
                     `(setf (hash-table-key-at hash-table free-slot) key
                            (hash-table-value-at hash-table free-slot) new-value))
                default))
             ;; No rehash/resize needed. Insert directly.
             (t
              (when (not (eq old-value default))
                (return-from ,cashash default))
              (unless (eq (,(if weakp
                                'weak-hash-table-entry-at
                                'hash-table-key-at)
                           hash-table free-slot)
                          *hash-table-tombstone*)
                (incf (hash-table-used hash-table)))
              (incf (hash-table-%count hash-table))
              ,(if weakp
                   `(set-full-weak-hash-table-entry hash-table free-slot key new-value)
                   `(setf (hash-table-key-at hash-table free-slot) key
                          (hash-table-value-at hash-table free-slot) new-value))
              default))))
       (defun ,remhash (key hash-table)
         (let ((slot (,find-hash-table-slot key hash-table)))
           (when slot
             ;; Entry exists.
             ,(if weakp
                  '(setf (weak-hash-table-entry-at hash-table slot) *hash-table-tombstone*)
                  '(setf (hash-table-key-at hash-table slot) *hash-table-tombstone*
                         (hash-table-value-at hash-table slot) *hash-table-tombstone*))
             (decf (hash-table-%count hash-table))
             t)))
       ;; internal lookup function, performs the actual hash-lookup without
       ;; accounting for the current gc-epoch.
       (defun ,find-hash-table-slot-1 (key hash-table)
         (declare (optimize speed (safety 0) (debug 1))
                  (type hash-table hash-table))
         (do* ((free-slot nil)
               (hash (logand #xffffffff (,hash-fn key)))
               (storage (hash-table-storage hash-table))
               (size (,(if weakp 'weak-hash-table-size 'strong-hash-table-size)
                      storage))
               (mask (the fixnum (1- size))) ; size is always a power-of-2
               (unbound-marker *hash-table-unbound-value*)
               (tombstone *hash-table-tombstone*)
               ;; This hash implementation is inspired by the Python dict implementation.
               (slot (logand hash #xffffffff) (logand #xffffffff (the fixnum (+ (the fixnum (+ (the fixnum (* slot 5)) perturb)) 1))))
               (perturb hash (the fixnum (ash perturb -5))))
              (nil)
           (declare (type fixnum hash size slot perturb)
                    (type simple-vector storage))
           ,(probe-hash-table-slot weakp 'storage '(logand slot mask) 'unbound-marker 'tombstone 'free-slot test-fn 'key)))
       (defun ,find-hash-table-slot (key hash-table)
         "Locate the slot matching KEY. Returns NIL if KEY is not in HASH-TABLE.
The second return value is the first available/empty slot in the hash-table for KEY.
Requires at least one completely unbound slot to terminate."
         (declare (optimize speed (safety 0))
                  (type hash-table hash-table))
         (if (hash-table-gc-invariant hash-table)
             (,find-hash-table-slot-1 key hash-table)
             (loop
               (let ((epoch *gc-epoch*))
                 (multiple-value-bind (offset free-slot)
                     (,find-hash-table-slot-1 key hash-table)
                   (when (and (eql epoch *gc-epoch*)
                              (eql (hash-table-storage-epoch hash-table) *gc-epoch*))
                     (return (values offset free-slot)))
                   (,hash-table-rehash hash-table nil))))))
       (defun ,hash-table-rehash (hash-table resize-p)
         "Resize and rehash HASH-TABLE so that there are no tombstones the usage
is below the rehash-threshold."
         (let* ((old-storage (hash-table-storage hash-table))
                (old-size (,(if weakp
                                'weak-hash-table-size
                                'strong-hash-table-size)
                           old-storage))
                (new-size (align-up-to-power-of-two
                           (if resize-p
                               (if (floatp (hash-table-rehash-size hash-table))
                                   (ceiling (* old-size (hash-table-rehash-size hash-table)))
                                   (+ old-size (hash-table-rehash-size hash-table)))
                               old-size))))
           (setf (hash-table-storage hash-table) (make-array ,(if weakp
                                                                  'new-size
                                                                  '(* new-size 2))
                                                             :initial-element *hash-table-unbound-value*)
                 (hash-table-%count hash-table) 0
                 (hash-table-used hash-table) 0
                 ;; Make sure to preserve :MANDATORY.
                 (hash-table-gc-invariant hash-table) (or (hash-table-gc-invariant hash-table) t)
                 (hash-table-storage-epoch hash-table) *gc-epoch*)
           (dotimes (i old-size hash-table)
             ,(if weakp
                  `(let ((entry (aref old-storage i)))
                     (unless (or (eq entry *hash-table-unbound-value*)
                                 (eq entry *hash-table-tombstone*))
                       (multiple-value-bind (key livep)
                           (weak-pointer-key entry)
                         (when livep
                           (when (and (hash-table-gc-invariant hash-table)
                                      (not (object-hash-gc-invariant-under-test key (hash-table-test hash-table))))
                             ;; Hash table is no longer invariant.
                             (setf (hash-table-gc-invariant hash-table) nil))
                           (multiple-value-bind (slot free-slot)
                               (,find-hash-table-slot-1 key hash-table)
                             (when slot
                               (error "Duplicate key ~S in hash-table?" key))
                             (incf (hash-table-used hash-table))
                             (incf (hash-table-%count hash-table))
                             (setf (weak-hash-table-entry-at hash-table free-slot) entry))))))
                  `(let ((key (aref old-storage (* i 2)))
                         (value (aref old-storage (1+ (* i 2)))))
                     (unless (or (eq key *hash-table-unbound-value*)
                                 (eq key *hash-table-tombstone*))
                       (when (and (hash-table-gc-invariant hash-table)
                                  (not (object-hash-gc-invariant-under-test key (hash-table-test hash-table))))
                         ;; Hash table is no longer invariant.
                         (setf (hash-table-gc-invariant hash-table) nil))
                       (multiple-value-bind (slot free-slot)
                           (,find-hash-table-slot-1 key hash-table)
                         (when slot
                           (error "Duplicate key ~S in hash-table?" key))
                         (incf (hash-table-used hash-table))
                         (incf (hash-table-%count hash-table))
                         (setf (hash-table-key-at hash-table free-slot) key
                               (hash-table-value-at hash-table free-slot) value)))))))))))

(defmacro define-optimized-hash-table-functions (name test-fn hash-fn)
  `(progn
     (define-optimized-hash-table-functions-internal ,name ,test-fn ,hash-fn nil)
     (define-optimized-hash-table-functions-internal ,name ,test-fn ,hash-fn t)
     ',name))

(define-optimized-hash-table-functions standard-eq eq eq-hash)
(define-optimized-hash-table-functions standard-eql eql eql-hash)
(define-optimized-hash-table-functions standard-equal equal sxhash)
(define-optimized-hash-table-functions standard-equalp equalp equalp-hash)

(defstruct (hash-table-iterator
             (:constructor %make-hash-table-iterator))
  storage
  index
  weakp)

(defun make-hash-table-iterator (hash-table)
  (check-type hash-table hash-table)
  ;; Take the current table to prevent read-triggered rehashes from
  ;; shuffling the table under us and invalidating the current index.
  (%make-hash-table-iterator
   :storage (hash-table-storage hash-table)
   :index 0
   :weakp (hash-table-weakness hash-table)))

(defun hash-table-iterator-next (iterator)
  (let* ((ht (hash-table-iterator-storage iterator))
         (size (length ht)))
    (if (hash-table-iterator-weakp iterator)
        (do () ((>= (hash-table-iterator-index iterator) size))
          (let* ((index (hash-table-iterator-index iterator))
                 (entry (aref ht index)))
            ;; Increment the key until a non-unbound/-tombstone key is found.
            (incf (hash-table-iterator-index iterator))
            (unless (or (eq entry *hash-table-unbound-value*)
                        (eq entry *hash-table-tombstone*))
              (multiple-value-bind (key value livep)
                  (weak-pointer-pair entry)
                (when livep
                  (return (values t key value)))))))
        (do () ((>= (hash-table-iterator-index iterator) size))
          (let* ((index (hash-table-iterator-index iterator))
                 (key (aref ht index))
                 (value (aref ht (1+ index))))
            ;; Increment the key until a non-unbound/-tombstone key is found.
            (incf (hash-table-iterator-index iterator) 2)
            (unless (or (eq key *hash-table-unbound-value*)
                        (eq key *hash-table-tombstone*))
              (return (values t key value))))))))

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

(declaim (inline eq-hash))
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
        ((typep object 'ratio)
         (logxor (eql-hash (numerator object))
                 (eql-hash (denominator object))))
        ((complexp object)
         (logxor (eql-hash (realpart object))
                 (eql-hash (imagpart object))))
        ((double-float-p object)
         (%double-float-as-integer object))
        ((short-float-p object)
         (%short-float-as-integer object))
        ((symbolp object)
         (symbol-hash object))
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
        (string
         (hash-string object))
        (symbol
         (symbol-hash object))
        (t
         ;; ### Bootstrap hack. Don't call typep with 'pathname.
         (if (pathnamep object)
             (hash-pathname object depth)
             ;; EQL-HASH also works for characters and numbers.
             (eql-hash object))))))

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
