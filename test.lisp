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

(defun null (x)
  (null x))

(defun not (x)
  (not x))

(defun car (x)
  (car x))

(defun cdr (x)
  (cdr x))

(defun eq (x y)
  (eq x y))

(defun eql (x y)
  (eq x y))

(defun <  (x y) (< x y))
(defun <= (x y) (<= x y))
(defun >  (x y) (> x y))
(defun >= (x y) (>= x y))
(defun =  (x y) (= x y))

(defun + (x y) (+ x y))
(defun symbol-plist (symbol) (symbol-plist symbol))
(defun boundp (symbol) (boundp symbol))

(defun (setf system::symbol-mode) (value symbol)
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
               (setf (system::symbol-mode var) :special)))))

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

(defun write-to-the-screen (str)
  (dotimes (i (sys.int::%simple-array-length str))
    (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 *screen-offset*)
	  (logior (char-code (schar str i)) #x0F00))
    (setf *screen-offset* (1+ *screen-offset*))))

(write-to-the-screen "Hello, World!")

(defun sys.int::raise-undefined-function (invoked-through)
  (write-to-the-screen "Undefined function: ")
  (if (symbolp invoked-through)
      (write-to-the-screen (symbol-name invoked-through))
      (write-to-the-screen "#<function>"))
  (loop))
(defun sys.int::raise-unbound-error (symbol)
  (write-to-the-screen "Unbound symbol: ")
  (write-to-the-screen (symbol-name symbol))
  (loop))

(loop)
