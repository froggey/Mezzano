(in-package :mcclim-harfbuzz)

(defmacro with-buffer ((sym) &body body)
  (alexandria:with-gensyms (buffer)
    `(let ((,buffer (hb-buffer-create)))
       (unwind-protect
            (let ((,sym ,buffer))
              ,@body)
         (hb-buffer-destroy ,buffer)))))

(defun buffer-add-string (buffer string)
  (cffi:with-foreign-strings (((string-buf byte-length) string :encoding :utf-8))
    (let ((string-length (1- byte-length)))
      (hb-buffer-add-utf8 buffer string-buf string-length 0 string-length))))
