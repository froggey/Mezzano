;;;; Simple EVAL for use in cold images.

(in-package #:sys.int)

(defun eval-cons (form)
  (case (first form)
    ((if) (if (eval (second form))
              (eval (third form))
              (eval (fourth form))))
    ((function) (fdefinition (second form)))
    ((quote) (second form))
    ((progn) (do ((f (rest form) (cdr f)))
                 ((null (cdr f))
                  (eval (car f)))
               (eval (car f))))
    ((setq) (do ((f (rest form) (cddr f)))
                ((null (cddr f))
                 (setf (symbol-value (car f)) (eval (cadr f))))
              (setf (symbol-value (car f)) (eval (cadr f)))))
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
