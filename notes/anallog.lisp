(in-package #:cl-user)
(use-package '#:iterate)

(defun load-log (path)
  (iter
    (with head = (list 'first nil '()))
    (with tree = (list head))
    (for line in-file path using 'read-line)
    (let ((action (char line 0))
          (mode (char line 1))
          (address (subseq line 2)#+nil(parse-integer line :start 2 :radix 16)))
      (case mode
        (#\d (setf mode "dynamic"))
        (#\s (setf mode "static")))
      (case action
        (#\> ;; Enter object.
         (let ((new (list address nil '())))
           (push new (third (first tree)))
           (push new tree)))
        (#\~ ;; New address.
         (setf (second (first tree)) address))
        (#\< ;; Leave object.
         (let ((self (pop tree)))
           (setf (third self) (nreverse (third self)))))))
    (finally (dolist (x tree)
               (setf (third x) (nreverse (third x))))
             (return head))))

(defun dottify-log (log file)
  (with-open-file (s file :direction :output :if-exists :supersede)
    (labels ((crunch (node)
               (dolist (x (third node))
                 (format s "  ~S -> ~S;~%" (first node) (first x)))
               (mapc #'crunch (third node))))
      (format s "digraph arse {~%")
      (crunch log)
      (format s "}~%"))))
