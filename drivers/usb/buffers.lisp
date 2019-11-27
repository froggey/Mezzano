;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

;;======================================================================
;;
;; This file provides a buffer management service for USB drivers. It
;; provides (1-dimensional) arrays with the proper length and type,
;; but are backed by wired memory provided by the HCD. The reason the
;; HCD provides the wired memory is because only the HCD knows
;; alignment requirements of the host controller.
;;
;; The implmenetation uses internal array manipulation routines to
;; force the array to have the right type (unsigned-byte 8, 16 or 32)
;; and the right length. This allows the allocation of wired memory to
;; be trivial - allocate a block to an array and never free it, while
;; providing exactly the array configuration required by the
;; driver. Manipulating the array header may be a really bad idea.
;;
;; Internal to this file, buf refers to a buffer structure and array
;; refers to a lisp array. Externally, buffer refers to a lisp array.
;;
;;======================================================================

(in-package :mezzano.driver.usb.buffers)

(defvar *buf-pool-lock* (sup:make-mutex "Global Buffer Pool Lock"))
(defvar *buf-pools* NIL)

;; ### Do these need to be synchronized?
;; It looks like they are accessed by multiple threads.
(defvar *array->buf* (make-hash-table :synchronized t))
(defvar *phys-addr->buf* (make-hash-table :synchronized t))

(defstruct (buffer (:print-function buffer-print))
  state
  phys-addr
  num-bytes
  buf-pool
  array)

(defstruct (buf-pool (:print-function buf-pool-print))
  (name  "Unnamed" :read-only T)
  (hcd   NIL       :read-only T)
  (sizes '(1024)   :read-only T) ;; max number of bytes in buffers
  lock
  free-lists                     ;; corresponding buffer free list for each size
  num-buffers)

(defun buffer-print (buffer stream level)
  (declare (ignore level))
  (format stream "<buffer phys-addr: ~16,'0X num-bytes: ~D pool: ~A>"
          (buffer-phys-addr buffer)
          (buffer-num-bytes buffer)
          (buf-pool-name (buffer-buf-pool buffer))))

(defun buf-pool-print (buf-pool stream level)
  (declare (ignore level))
  (sup:with-mutex ((buf-pool-lock buf-pool))
    (let ((total (buf-pool-num-buffers buf-pool))
          (free 0))
      (dolist (free-list (buf-pool-free-lists buf-pool))
        (incf free (length free-list)))
      (format stream
              "<buffer pool: ~A total: ~D, free: ~D, allocated: ~D ("
              (buf-pool-name buf-pool)
              total free (- total free))
      (loop for size in (buf-pool-sizes buf-pool)
         for free-list in (buf-pool-free-lists buf-pool)
         do (format stream " ~D:~D" size (length free-list)))
      (format stream ")>"))))

(defun search-free-list (buf-pool type num-elements num-bytes)
  (sup:with-mutex ((buf-pool-lock buf-pool))
    (let ((free-lists (buf-pool-free-lists buf-pool)))
      (dolist (size (buf-pool-sizes buf-pool))
        (when (and (<= num-bytes size) (car free-lists))
          (let* ((buf (pop (car free-lists)))
                 (array (buffer-array buf)))
            (setf (buffer-state buf) :in-use
                  (sys.int::%complex-array-dimension array 0) num-elements
                  (sys.int::%complex-array-info array) type)
            (return-from search-free-list array)))
        (setf free-lists (cdr free-lists))))))

(defun alloc-memory (buf-pool num-bytes)
  (loop for size in (buf-pool-sizes buf-pool)
     when (<= num-bytes size) do
       (return-from alloc-memory
         (values (get-buffer-memory (buf-pool-hcd buf-pool) size) size)))
  (error "Buffer size, ~D, too big. Max size ~D"
         (car (last (buf-pool-sizes buf-pool)))))

(defun create-buffer (buf-pool type num-elements num-bytes)
  (multiple-value-bind (phys-addr num-bytes)
      (alloc-memory buf-pool num-bytes)
    (let* ((array (make-array num-elements
                              :element-type type
                              :physical-memory phys-addr))
           (buf (make-buffer :state :in-use
                             :phys-addr phys-addr
                             :num-bytes num-bytes
                             :buf-pool buf-pool
                             :array array
                             )))
      (sup:with-mutex ((buf-pool-lock buf-pool))
        (incf (buf-pool-num-buffers buf-pool))
        (sup:with-mutex (*buf-pool-lock*)
          (setf (gethash array *array->buf*) buf
                (gethash phys-addr *phys-addr->buf*) buf)))
      array)))

;;======================================================================
;; Public APIs
;;======================================================================

(defgeneric get-buffer-memory (hcd num-bytes))

(defun create-buffer-pool (hcd name sizes)
  (let ((buf-pool (make-buf-pool
                   :name name
                   :lock (sup:make-mutex "Buffer Pool Lock")
                   :hcd hcd
                   :num-buffers 0
                   :sizes sizes
                   :free-lists (make-list (length sizes)))))
    (sup:with-mutex (*buf-pool-lock*)
      (push buf-pool *buf-pools*))
    buf-pool))

(defun delete-buffer-pool (buf-pool)
  (maphash #'(lambda (key value)
               (when (eq (buffer-buf-pool value) buf-pool)
                 (remhash key *array->buf*)))
           *array->buf*)

  (maphash #'(lambda (key value)
               (when (eq (buffer-buf-pool value) buf-pool)
                 (remhash key *phys-addr->buf*)))
           *phys-addr->buf*)

  (sup:with-mutex (*buf-pool-lock*)
    (setf *buf-pools* (delete buf-pool *buf-pools*)))
  (values))

(defun alloc-buffer/8 (buf-pool num-elements)
  (or (search-free-list buf-pool
                        sys.int::+object-tag-array-unsigned-byte-8+
                        num-elements
                        num-elements)
      (create-buffer buf-pool
                     '(unsigned-byte 8)
                     num-elements
                     num-elements)))

(defun alloc-buffer/16 (buf-pool num-elements)
  (or (search-free-list buf-pool
                        sys.int::+object-tag-array-unsigned-byte-16+
                        num-elements
                        (* 2 num-elements))
      (create-buffer buf-pool
                     '(unsigned-byte 16)
                     num-elements
                     (* 2 num-elements))))

(defun alloc-buffer/32 (buf-pool num-elements)
  (or (search-free-list buf-pool
                        sys.int::+object-tag-array-unsigned-byte-32+
                        num-elements
                        (* 4 num-elements))
      (create-buffer buf-pool
                     '(unsigned-byte 32)
                     num-elements
                     (* 4 num-elements))))

(defun free-buffer (array)
  (let ((buf (gethash array *array->buf*)))
    (cond ((null buf)
           (error "Buffer assoicated with ~A not found" array))
          ((eq (buffer-state buf) :free)
           (error "Buffer associated with ~A already free" array))
          (T
           (let ((buf-pool (buffer-buf-pool buf)))
             (sup:with-mutex ((buf-pool-lock buf-pool))
               (let ((num-bytes (buffer-num-bytes buf))
                     (free-lists (buf-pool-free-lists buf-pool)))
                 (dolist (size (buf-pool-sizes buf-pool))
                   (when (= num-bytes size)
                     (push buf (car free-lists))
                     (setf (buffer-state buf) :free)
                     (return-from free-buffer (values)))
                   (setf free-lists (cdr free-lists)))))
             (error "Invalid buffer size ~D" (buffer-num-bytes buf)))))))

(defun array->phys-addr (array)
  (let ((buf (gethash array *array->buf*)))
    (if buf
        (buffer-phys-addr buf)
        (error "Array ~A not a buffer array" array))))

(defun phys-addr->array (phys-addr)
  (let ((buf (gethash phys-addr *phys-addr->buf*)))
    (if buf
        (buffer-array buf)
        (error "Physical address ~16,'0X not a buffer address" phys-addr))))

(defconstant +symbol->alloc-routine-plist+
  '(/8 alloc-buffer/8
    :unsigned-byte-8  alloc-buffer/8
    :byte-8  alloc-buffer/8
    /16  alloc-buffer/16
    :unsigned-byte-16 alloc-buffer/16
    :byte-16 alloc-buffer/16
    /32 alloc-buffer/32
    :unsigned-byte-32 alloc-buffer/32
    :byte-32 alloc-buffer/32))

(defmacro with-buffers ((pool bufs) &body body)
  (destructuring-bind (name element-size element-count)
      ;; if bufs is a list of lists, pop the first list off,
      ;; if not, return the list and set bufs to nil
      (if (listp (car bufs)) (pop bufs)
          (prog1
              bufs
            (setf bufs NIL)))
    (let ((alloc (getf +symbol->alloc-routine-plist+ element-size)))
      (when (null alloc)
        (error "with-buffers: undefined element-size ~A" element-size))
      `(let ((,name))
         (unwind-protect
              (progn
                (setf ,name  (,alloc ,pool ,element-count))
              ,@(if (null bufs)
                    body
                    `((with-buffers (,pool ,bufs) ,@body))))
           (when ,name
             (free-buffer ,name)))))))
