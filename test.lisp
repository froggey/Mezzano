(setf (symbol-function 'sys.int::raise-undefined-function)
      (lambda (invoked-through)
	(let ((str (if (symbolp invoked-through)
		       (symbol-name invoked-through)
		       "Undefined function")))
	  (dotimes (i (sys.int::%simple-array-length str))
	    (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 i)
		  (logior (char-code (schar str i)) #x0F00))))
	(loop)))

(defun endp (list)
  (cond ((null list) t)
        ((consp list) nil)
        (t (error 'type-error
                  :datum list
                  :expected-type 'list))))

(defun (setf system:symbol-mode) (value symbol)
  (let ((flags (sys.int::%symbol-flags symbol))
        (bits (ecase value
                ((nil) 0)
                ((:special) 1)
                ((:constant) 2)
                ((:symbol-macro) 3))))
    (setf (sys.int::%symbol-flags symbol)
          (logior (logand flags -4) bits))
    value))

(defun proclaim (declaration-specifier)
  (case (first declaration-specifier)
    (special (dolist (var (rest declaration-specifier))
               (setf (system:symbol-mode var) :special)))))

(defvar *screen-offset* 0)

(defvar *gb-keymap-low*
  #(nil #\Esc #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\- #\= #\Backspace
    #\Tab #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p #\[ #\] #\Newline
    :control #\a #\s #\d #\f #\g #\h #\j #\k #\l #\; #\' #\`
    :shift #\# #\z #\x #\c #\v #\b #\n #\m #\, #\. #\/ :shift nil
    :meta #\Space :capslock nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil #\\
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil))
(defvar *gb-keymap-high*
  #(nil #\Esc #\! #\" #\£ #\$ #\% #\^ #\& #\* #\( #\) #\_ #\+ #\Backspace
    #\Tab #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\{ #\} #\Newline
    :control #\A #\S #\D #\F #\G #\H #\J #\K #\L #\: #\@ #\¬
    :shift #\~ #\Z #\X #\C #\V #\B #\N #\M #\< #\> #\? :shift nil
    :meta #\Space :capslock nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil #\|
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil))

(defvar *keyboard-shifted* nil)

(defun write-char (c &optional stream)
  (cond ((eql c #\Newline)
         (incf *screen-offset* (- 80 (rem *screen-offset* 80))))
        (t (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 *screen-offset*)
                 (logior (char-code c) #x0F00))
           (incf *screen-offset*)))
  (when (>= *screen-offset* (* 80 25))
    (setf *screen-offset* 0))
  c)

(defun write-to-the-screen (str)
  (dotimes (i (sys.int::%simple-array-length str))
    (write-char (schar str i))))

(write-to-the-screen "Hello, World!")

(defun sys.int::raise-undefined-function (invoked-through)
  (write-to-the-screen "Undefined function: ")
  (if (symbolp invoked-through)
      (write-to-the-screen (symbol-name invoked-through))
      (write-to-the-screen "#<function>"))
  (backtrace)
  (loop))

(defun sys.int::raise-unbound-error (symbol)
  (write-to-the-screen "Unbound symbol: ")
  (write-to-the-screen (symbol-name symbol))
  (backtrace)
  (loop))

(defun poll-keyboard ()
  (loop (let ((cmd (system:io-port/8 #x64)))
          (when (= (logand cmd 1) 1)
            ;; Byte ready.
            (return (system:io-port/8 #x60))))))

(defun read-keyboard-char ()
  (loop
     (let* ((scancode (poll-keyboard))
            (key (#+ignore aref svref (if *keyboard-shifted*
                           *gb-keymap-high*
                           *gb-keymap-low*)
                       (logand scancode #x7F))))
       (cond ((= (logand scancode #x80) 0)
              ;; Key press.
              (cond ((eql key :shift)
                     (setf *keyboard-shifted* t))
                    ((characterp key)
                     (return key))))
             (t ;; Key release.
              (case key
                (:shift (setf *keyboard-shifted* nil))))))))

(defun read-char (&optional stream eof-error-p eof-value recursive-p)
  (read-keyboard-char))

(setf *standard-input* nil
      *standard-output* nil)

(defun write-unsigned-integer (x base)
  (unless (= x 0)
    (write-unsigned-integer (truncate x base) base)
    (write-char (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                       (rem x base)))))

(defun write-integer (x &optional (base 10))
  (cond ((= x 0)
         (write-char #\0))
        ((< x 0)
         (write-char #\-)
         (write-unsigned-integer (- 0 x) base))
        (t (write-unsigned-integer x base))))

(defun backtrace ()
  (do ((fp (sys.int::read-frame-pointer)
           (sys.int::memref-unsigned-byte-64 fp 0)))
      ((= fp 0))
    (write-char #\Newline)
    (write-integer fp 16)
    (write-char #\Space)
    (write-integer (sys.int::memref-unsigned-byte-64 fp -2) 16)))

(defun error (what &optional a b c d)
  (write-to-the-screen "Error: ")
  (write-to-the-screen (if (symbolp what)
                           (symbol-name what)
                           what))
  (backtrace)
  (loop))

(defun indirect-funcall (l)
  (funcall l))
(defun indirect-funcall* (l a)
  (funcall l a))

(system:io-port/8 #xe9)

(indirect-funcall* #'(lambda (a &optional (b 4321))
                      (indirect-funcall #'(lambda ()
                                            (write-integer a)
                                            (write-char #\Space)
                                            (write-integer b))))
                  1234)

(system:io-port/8 #xe9)

(loop (write (read)))
