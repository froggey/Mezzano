;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Ugly printer. Provide the pprint functions & macros, and do the bare
;;; minimum required.

(in-package :sys.int)

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

(defun copy-pprint-dispatch (&optional table))
(defun set-pprint-dispatch (type-specifier function &optional (priority 0) table))
