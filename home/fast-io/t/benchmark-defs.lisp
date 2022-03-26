(in-package :fast-io.test)

(declaim (inline now))
(defun now ()
  (coerce (/ (get-internal-real-time)
             internal-time-units-per-second)
          'double-float))

(defmacro bench ((&optional (times 1)) &body body)
  (with-gensyms (results t1 t2 i)
    (declare (ignorable results t2))
    (once-only (times)
      `(let (,t1
             #+-(,results (make-array ,times :element-type 'double-float)))
         (declare (ignorable ,t1))
         (time
          (dotimes (,i ,times)
            #+-
            (setf ,t1 (now))
            ,@body
            #+-
            (let ((,t2 (now)))
              (setf (aref ,results ,i) (- ,t2 ,t1)))))
         #+-
         (format t "Tot: ~F   |  Min: ~F Max: ~F~%Avg: ~F Med: ~F Var: ~F Std: ~F"
                 (reduce #'+ ,results)
                 (reduce #'min ,results)
                 (reduce #'max ,results)
                 (mean ,results)
                 (median ,results)
                 (variance ,results)
                 (standard-deviation ,results))))))

