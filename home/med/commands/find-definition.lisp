(in-package :med)

(defvar *mark-stack* ())

(defun find-definition (function-symbol)
  (let* ((buffer (current-buffer *editor*))
         (loc (mezzano.debug:function-source-location
               (or (and (symbolp function-symbol)
                        (macro-function function-symbol))
                   (fdefinition function-symbol))))
         (file (mezzano.debug:source-location-file loc))
         (form (mezzano.debug:source-location-top-level-form-number loc)))
      (cond ((and file form)
             (format t "~A ~A ~A ~A~%" buffer *package* file form)
             (let ((buffer (find-file file)))
               (move-beginning-of-buffer buffer)
               (move-sexp buffer (1+ form))
               (move-sexp buffer -1)))
            (t (format t "Cannot find definition for function ~A" function-symbol)))))

(defun find-definition-command ()
  (find-definition (read-from-string (symbol-at-point (current-buffer *editor*)))))
