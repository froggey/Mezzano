(in-package #:sys.int)

(defstruct cold-stream)
(defvar *screen-offset* 0)

(setf *terminal-io* (make-cold-stream))

(defun cold-write-char (c stream)
  (setf (system:io-port/8 #xE9) (logand (char-code c) #xFF))
  (cond ((eql c #\Newline)
         (incf *screen-offset* (- 80 (rem *screen-offset* 80))))
        (t (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 *screen-offset*)
                 (logior (char-code c) #x0F00))
           (incf *screen-offset*)))
  (when (>= *screen-offset* (* 80 25))
    (setf *screen-offset* 0))
  c)

(defun cold-start-line-p (stream)
  (zerop (rem *screen-offset* 80)))

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

(defun poll-keyboard ()
  (loop (let ((cmd (system:io-port/8 #x64)))
          (when (= (logand cmd 1) 1)
            ;; Byte ready.
            (return (system:io-port/8 #x60))))))

(defun read-keyboard-char ()
  (loop
     (let* ((scancode (poll-keyboard))
            (key (svref (if *keyboard-shifted*
                           *gb-keymap-high*
                           *gb-keymap-low*)
                       (logand scancode #x7F))))
       (cond ((= (logand scancode #x80) 0)
              ;; Key press.
              (cond ((eql key :shift)
                     (setf *keyboard-shifted* t))
                    ((characterp key)
                     (write-char key)
                     (return key))
                    ((null key)
                     (write-string "Unknown keycode #x")
                     (sys.int::write-integer scancode 16)
                     (write-char #\/)
                     (sys.int::write-integer scancode))))
             (t ;; Key release.
              (case key
                (:shift (setf *keyboard-shifted* nil))))))))

(defvar *unread-char* nil)

(defun cold-read-char (stream)
  (cond (*unread-char*
         (prog1 *unread-char*
           (setf *unread-char* nil)))
        (t (read-keyboard-char))))

(defun cold-unread-char (character stream)
  (when *unread-char*
    (error "Multiple unread-char!"))
  (setf *unread-char* character))

(defun cold-listen (stream)
  (or *unread-char* (= (logand (system:io-port/8 #x64) 1) 1)))

