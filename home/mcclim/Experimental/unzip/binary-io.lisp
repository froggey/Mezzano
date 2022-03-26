(in-package :unzip)

(defun read-unsigned-byte/le (size input &optional (eof-error-p t) eof-value)
  (loop
      for i from 0 below (ceiling size 8)
      sum (let ((b (read-byte input eof-error-p nil)))
            (if b
                (ash b (* 8 i))
              (return eof-value)))))

(defun read-unsigned-byte (size input &optional (eof-error-p t) eof-value)
  (loop
      for i from (1- (ceiling size 8)) downto 0
      sum (let ((b (read-byte input eof-error-p nil)))
            (print i)
            (if b
                (ash b (* 8 i))
              (return eof-value)))))

(defun write-unsigned-byte/le (size byte output)
  (loop
      for i from 0 below (ceiling size 8)
      do (write-byte (logand #xFF (ash byte (* -8 i))) output) ))

(defun write-unsigned-byte (size byte output)
  (loop
      for i from (1- (ceiling size 8)) downto 0
      do (write-byte (logand #xFF (ash byte (* -8 i))) output)))


;;;

(defun read-zero-terminated-string (input)
  (with-output-to-string (bag)
    (do ((c (read-byte input) (read-byte input)))
        ((zerop c))
      (write-char (code-char c) bag))))

