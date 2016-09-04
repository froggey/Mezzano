;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.lap.arm64)

(defparameter *instruction-assemblers* (make-hash-table))
(defvar *function-reference-resolver*)

(defvar *fixup-target*)
(defvar *constant-pool*)
(defvar *machine-code*)
(defvar *base-address*)
(defvar *symbol-table*)
(defvar *missing-symbols*)
(defvar *fixups*)
(defvar *gc-data*)
(defvar *literal-pool*)

(defun current-address ()
  (+ *base-address* (length *machine-code*)))

(defun assemble (code-list &key (base-address 0) (initial-symbols '()) info)
  (let ((*base-address* base-address)
        (*symbol-table* (make-hash-table))
        (*constant-pool* (make-array (+ (length info) 16)
                                     :fill-pointer (length info)
                                     :adjustable t))
        (*machine-code* (make-array 128
                                    :element-type '(unsigned-byte 8)
                                    :fill-pointer 0
                                    :adjustable t))
        (*missing-symbols* '())
        (*fixups* '())
        (*gc-data* '())
        (*literal-pool* (make-array 128
                                    :element-type '(unsigned-byte 64)
                                    :fill-pointer 0
                                    :adjustable t)))
    (setf (subseq *constant-pool* 0 (length info)) info)
    (loop
       for (symbol . value) in initial-symbols
       do (setf (gethash symbol *symbol-table*) value))
    ;; Pass 1: dry run, compute symbol addresses & literal pool layout.
    (dolist (instruction code-list)
      (etypecase instruction
        (symbol
         (when (gethash instruction *symbol-table*)
           (cerror "Replace the existing symbol." "Duplicate symbol ~S." instruction))
         (setf (gethash instruction *symbol-table*) (current-address)))
        (cons
         (let ((handler (gethash (first instruction) *instruction-assemblers*)))
           (cond (handler
                  (funcall handler instruction))
                 (t
                  (cerror "Skip it." "Unrecognized instruction ~S." (first instruction))))))))
    ;; Align & append the literal pool.
    (values *machine-code*
            *constant-pool*
            *fixups*
            (let ((alist '()))
              (maphash (lambda (k v)
                         (push (cons k v) alist))
                       *symbol-table*)
              alist)
            (make-array 0 :element-type '(unsigned-byte 8)))))
