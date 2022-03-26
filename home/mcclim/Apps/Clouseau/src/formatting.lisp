;;;; Copyright (C) 2018, 2019, 2020 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; Utilities

(defun call-with-preserved-cursor-x (thunk stream)
  (let ((old-x (stream-cursor-position stream)))
    (prog1
        (funcall thunk stream)
      (setf (stream-cursor-position stream)
            (values old-x (nth-value 1 (stream-cursor-position stream)))))))

(defmacro with-preserved-cursor-x ((stream) &body body)
  (check-type stream symbol)
  `(call-with-preserved-cursor-x (lambda (,stream) ,@body) ,stream))

(defun call-with-preserved-cursor-y (thunk stream)
  (let ((old-y (nth-value 1 (stream-cursor-position stream))))
    (prog1
        (funcall thunk stream)
      (setf (stream-cursor-position stream)
            (values (stream-cursor-position stream) old-y)))))

(defmacro with-preserved-cursor-y ((stream) &body body)
  (check-type stream symbol)
  `(call-with-preserved-cursor-y (lambda (,stream) ,@body) ,stream))

(defun call-with-superscript (thunk stream)
  (let ((exponent-offset (* 0.25 (nth-value 1 (text-size stream "0")))))
    (clim:stream-increment-cursor-position stream 0 exponent-offset)
    (flet ((superscript (thunk)
             (with-preserved-cursor-y (stream)
               (clim:stream-increment-cursor-position stream 0 (- exponent-offset))
               (with-text-size (stream :smaller)
                 (funcall thunk stream)))))
      (funcall thunk #'superscript))))

(defmacro with-superscript ((stream superscript-var) &body body)
  (check-type stream symbol)
  (with-unique-names (superscript)
    `(call-with-superscript
      (lambda (,superscript)
        (macrolet ((,superscript-var (&body body)
                     `(funcall ,',superscript (lambda (,',stream) ,@body))))
          ,@body))
      ,stream)))

;;; Styles

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *styles*
    (alist-hash-table
     `((:header                      . (                                                    :text-face :bold))
       (:changable                   . (:ink ,+dark-violet+))
       (:slot-like                   . (:ink ,+dark-orange+                                                    :text-size :small))
       (:slot-like/same-size         . (:ink ,+dark-orange+))
       (:inactive                    . (:ink ,+dark-gray+))
       (:unbound                     . (:ink ,+dark-gray+                                   :text-face :italic))
       (:note                        . (                                                    :text-face :italic :text-size :small))
       (:error                       . (:ink ,+dark-red+                                    :text-face :italic))

       (:identity                    . (:ink ,+dark-slate-blue+                                                :text-size :smaller))

       (:package-note                . (:ink ,+dark-gray+                                                      :text-size :smaller))

       (:float-sign                  . (:ink ,(make-contrasting-inks 8 0)))
       (:float-significand           . (:ink ,(make-contrasting-inks 8 1)))
       (:float-radix                 . (:ink ,(make-contrasting-inks 8 3)))
       (:float-exponent              . (:ink ,(make-contrasting-inks 8 4)))

       (:complex-realpart            . (:ink ,(make-contrasting-inks 8 0)))
       (:complex-imagpart            . (:ink ,(make-contrasting-inks 8 1)))
       (:complex-magnitude           . (:ink ,(make-contrasting-inks 8 3)))
       (:complex-phase               . (:ink ,(make-contrasting-inks 8 4)))

       (:hash-table-count            . (:ink ,(make-contrasting-inks 8 4)))
       (:hash-table-size             . ())
       (:hash-table-rehash-size      . (:ink ,(make-contrasting-inks 8 5)))
       (:hash-table-rehash-threshold . (:ink ,(make-contrasting-inks 8 6)))

       (:disassembly                 . (                                  :text-family :fix)))))

  (defun style-drawing-options (style)
    (gethash style *styles*)))

(defun call-with-style (thunk stream style)
  (if (null style)
      (funcall thunk stream)
      (apply #'invoke-with-drawing-options stream thunk
             (etypecase style
               (list   style)
               (symbol (style-drawing-options style))))))

(defmacro with-style ((stream style) &body body)
  (check-type stream symbol)
  (when (and (constantp style)
             (not (nth-value 1 (style-drawing-options (eval style)))))
    (warn "~@<~S is not a know style.~@:>" style))
  `(call-with-style (lambda (,stream) (declare (ignorable ,stream)) ,@body)
                    ,stream ,style))

(defun call-with-section (body-thunk title-thunk stream)
  (with-preserved-cursor-x (stream)
    (with-style (stream :header)
      (funcall title-thunk stream))
    (fresh-line stream)
    (indenting-output (stream "    ")
      (funcall body-thunk stream))))

(defmacro with-section ((stream) title &body body)
  (check-type stream symbol)
  `(call-with-section
    (lambda (,stream) ,@body)
    (lambda (,stream)
      ,(typecase title
         (string `(write-string ,title ,stream))
         (t      title)))
    ,stream))

(defun call-with-placeholder-if-empty
    (test-thunks empty-thunks non-empty-thunk stream)
  (or (some (lambda (test-thunk empty-thunk)
              (when (funcall test-thunk)
                (with-style (stream :unbound)
                  (funcall empty-thunk stream))
                t))
            test-thunks empty-thunks)
      (funcall non-empty-thunk stream)))

(defmacro with-placeholder-if-empty ((stream) &body clauses)
  "Print normal output or a placeholder to STREAM according to CLAUSES.

Each clause in CLAUSES is of the form (TEST . BODY) where TEST is a
single form which tests the applicability of the clause for all but
the final clause and TEST must be T for the final clause. BODY can
either be a list of forms suitable for an implicit PROGN or a string.

The body of the first clause the TEST of which evaluates to true or
the body of the final clause will be executed to produce output on
STREAM. Output produced by clauses other than the final one will use
the :UNBOUND style."
  (check-type stream symbol)
  (loop :for ((test . body) . rest) :on clauses
        :for body-thunk = `(lambda (,stream)
                             ,@(typecase body
                                 ((cons string)
                                  `((format ,stream ,(first body))))
                                 (t
                                  body)))
        :when (and (not rest) (not (eq test t)))
          :do (error "~@<Final clause must be of the form (T ~
                      . BODY).~@:>")
        :when rest
          :collect `(lambda () ,test) :into test-thunks
          :and :collect body-thunk :into empty-thunks
        :finally (return `(call-with-placeholder-if-empty
                           (list ,@test-thunks) (list ,@empty-thunks)
                           ,body-thunk ,stream))))

;;; Tables

(defmacro formatting-header ((stream) &body columns)
  (check-type stream symbol)
  `(with-style (,stream :header)
     (formatting-row (,stream)
       ,@(map 'list (lambda (column)
                      `(formatting-cell (,stream)
                         ,(typecase column
                            (string `(write-string ,column ,stream))
                            (t      column))))
              columns))))

;;; Badges

(defparameter *badge-text-style*
  (make-text-style nil :roman :smaller))

(defun call-with-output-as-badge (thunk stream)
  (with-preserved-cursor-y (stream)
    (surrounding-output-with-border (stream :shape      :rounded
                                            :background +light-blue+
                                            :radius     2
                                            :padding    2)
      (with-drawing-options (stream :text-style *badge-text-style*)
        (with-end-of-line-action (stream :allow)
          (funcall thunk stream))))))

(defmacro with-output-as-badge ((stream) &body body)
  (check-type stream symbol)
  `(call-with-output-as-badge (lambda (,stream) ,@body) ,stream))

(defun badge (stream format-control &rest format-arguments)
  (with-output-as-badge (stream)
    (apply #'format stream format-control format-arguments)))

;;; Object border

(defun color-for-bar (depth)
  (let ((limit (contrasting-inks-limit nil)))
    (make-contrasting-inks limit (mod depth limit))))

(defun call-with-object-border (thunk stream depth)
  (if (plusp depth)
      (surrounding-output-with-border
          (stream :padding        2
                  :shape          :rectangle
                  :line-thickness 2
                  :line-dashes    '(1 1)
                  :ink            (color-for-bar (1- depth)))
        (funcall thunk stream))
      (funcall thunk stream)))

(defmacro with-object-border ((stream depth) &body body)
  `(call-with-object-border (lambda (,stream) ,@body) ,stream ,depth))

;;; Object identity

(defun print-object-identity (object stream)
  (let ((string (with-output-to-string (stream)
                  (print-unreadable-object (object stream :identity t)))))
    (with-style (stream :identity)
      (format stream "@~A" (string-trim "#<>{} " string)))))

;;; Strings

(defun print-string-compactly (string stream &key (delimitersp *print-escape*)
                                                  (line-limit 3))
  (when delimitersp
    (write-char #\" stream))
  (loop :for line-count :from 0 :below line-limit
        :for start = 0 :then (1+ end)
        :for end = (position #\Newline string :start start)
        :while end
        :do (write-string string stream :start start :end end)
            (with-style (stream :inactive) (write-char #\¶ stream))
        :finally (cond (end
                        (with-style (stream :inactive)
                          (write-string ".." stream)))
                       (t
                        (write-string string stream :start start :end end))))
  (when delimitersp
    (write-char #\" stream))
  string)

;;; Symbols

(defun print-symbol-in-context (symbol context-package stream)
  (write-string (symbol-name symbol) stream)
  (let ((symbol-package (symbol-package symbol)))
    (unless (eq context-package symbol-package)
      (write-char #\Space stream)
      (with-style (stream :package-note)
        (write-string (or (package-name symbol-package) "unnamed") stream)))))

;;; Safety

(defvar *safe-pprint-dispatch*
  (let ((dispatch (with-standard-io-syntax
                    (copy-pprint-dispatch *print-pprint-dispatch*))))
    (set-pprint-dispatch 'string (lambda (stream object)
                                   (print-string-compactly object stream))
                         1 dispatch)
    dispatch))

(defun call-with-safe-and-terse-printing (thunk)
  (let ((*print-circle*          t)
        (*print-length*          3)
        (*print-level*           3)
        (*print-lines*           3)
        (*print-pprint-dispatch* *safe-pprint-dispatch*))
    (funcall thunk)))

(defmacro with-safe-and-terse-printing ((stream) &body body)
  (check-type stream symbol)
  `(call-with-safe-and-terse-printing (lambda () ,@body)))

(defun call-with-error-handling (thunk stream message)
  (handler-bind
      ((error (lambda (condition)
                (when (typep condition *handle-errors*)
                  (with-style (stream :error)
                    (write-string message stream)
                    (fresh-line stream)
                    (ignore-errors (princ condition stream)))
                  (return-from call-with-error-handling)))))
    (funcall thunk)))

(defmacro with-error-handling ((stream message) &body body)
  `(call-with-error-handling (lambda () ,@body) ,stream ,message))

(defmacro with-print-error-handling ((stream) &body body)
  `(with-error-handling (,stream "error printing object") ,@body))
