(in-package :med)


(defun buffer-current-package (buffer)
  "From point, search backwards for a top-level IN-PACKAGE form.
If no such form is found, then return the CL-USER package."
  ;; TODO: create a cache for this
  (let ((point (current-buffer *editor*))
        (temporary-package (make-package (gensym))))
    (import 'in-package temporary-package)
    (export 'in-package temporary-package)
    (unwind-protect
         (or (ignore-errors
               (save-excursion (buffer)
                  (with-mark (point (buffer-point buffer))
                    (move-beginning-of-buffer buffer)
                    (let* ((str (buffer-string buffer (buffer-point buffer) point))
                           (pos (search (format nil "~A~A" #\( "in-package ") str :from-end t)))
                      (when (and pos (or (= 0 pos)
                                         (char= (char str (1- pos)) #\Newline)))
                        (let ((form (let ((*package* temporary-package)
                                          (*read-eval* nil))
                                      (ignore-errors
                                        (read-from-string (subseq str pos))))))
                          (when (and (listp form)
                                     (eql (first form) 'in-package)
                                     (= (list-length form) 2))
                            (return-from buffer-current-package (find-package (second form))))))))))
             (find-package :cl-user))
      (delete-package temporary-package))))

(defun eval-top-level-form-command ()
  (let ((buffer (current-buffer *editor*)))
    (save-excursion (buffer)
      (beginning-of-top-level-form buffer)
      (mark-to-point buffer (buffer-mark buffer))
      (move-sexp buffer 1)
      (let ((str (buffer-string buffer
                                (buffer-point buffer)
                                (buffer-mark buffer)))
            (package (buffer-current-package buffer)))
;        (format t "Read ~S in package ~S~%" str package)
        (let ((form (let ((*package* package))
                      (read-from-string str))))
          (save-buffer-command) ;; FIXME: for now, since we're a bit unstable
          (format t "Evaluated ~S~%" (cadr form))
          (eval form))))))

(defun beginning-of-top-level-form-command ()
  (beginning-of-top-level-form (current-buffer *editor*)))

(defun eval-expression-command ()
  (format t "~A~%" (eval (read-from-string (read-from-minibuffer "Eval: ")))))

(defun eval-last-sexp-command ()
   (let* ((buffer (current-buffer *editor*)))
     (with-mark (point (buffer-point buffer))
       (save-excursion (buffer)
         (move-sexp buffer -1)
         (let ((string (buffer-string buffer point (buffer-point buffer))))
           (print (eval (read-from-string string))))))))

