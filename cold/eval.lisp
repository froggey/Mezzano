;;;; Simple EVAL for use in cold images.

(in-package #:sys.int)

(defun eval-cons (form)
  (case (first form)
    ((if) (if (eval (second form))
              (eval (third form))
              (eval (fourth form))))
    ((quote) (second form))
    (t (multiple-value-bind (expansion expanded-p)
           (macroexpand form)
         (if expanded-p
             (eval expansion)
             (apply (first form) (mapcar 'eval (rest form))))))))

(defun eval (form)
  (typecase form
    (cons (eval-cons form))
    (symbol (symbol-value form))
    (t form)))
