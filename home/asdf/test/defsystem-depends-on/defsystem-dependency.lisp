(in-package :asdf-test)
(defvar *dd* 0)
(incf *dd*)
(format t "Loaded defsystem-dependency.lisp ~d time~(~:*~p~)~%" *dd*)
(setf (find-class 'asdf::my-cl-source-file) (find-class 'cl-source-file))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cdd* 0))

(eval-when (:compile-toplevel :execute)
  (incf *cdd*))

;; Modify the current *readtable* -- it's very rude, but it's still supported,
;; and some bugs in previous versions of ASDF (e.g. 3.3.0, fixed in 3.3.0.1)
;; failed to restore syntax tables inside with-standard-io-syntax, causing it to fail.
(defun sharp-b (stream chr arg)
  (declare (ignore chr arg))
  (let ((arg (read stream nil nil t)))
    (if *read-suppress*
        nil
        (if (stringp arg)
            (map '(vector (unsigned-byte 8)) 'char-code arg)
            (error "#b takes a string argument: ~s" arg)))))

(set-dispatch-macro-character #\# #\b #'sharp-b)
