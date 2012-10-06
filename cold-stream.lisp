(in-package #:sys.int)

(defstruct cold-stream)
(defvar *screen-offset* 0)

(setf *terminal-io* (make-cold-stream))

(add-hook '*early-initialize-hook*
          #'(lambda ()
              (setf *terminal-io* (make-cold-stream))
              (setf *screen-offset* 0)
              (setf *keyboard-shifted* nil)))

(defun cold-write-char (c stream)
  (setf (system:io-port/8 #xE9) (logand (char-code c) #xFF))
  (cond ((eql c #\Newline)
         (incf *screen-offset* (- 80 (rem *screen-offset* 80))))
        (t (setf (sys.int::memref-unsigned-byte-16 #x80000B8000 *screen-offset*)
                 (logior (logand (char-code c) #xFF) #x0F00))
           (incf *screen-offset*)))
  (when (>= *screen-offset* (* 80 25))
    (setf *screen-offset* 0))
  c)

(defun cold-start-line-p (stream)
  (zerop (rem *screen-offset* 80)))

;; FIXME: use the proper character names for the special keys
(defvar *gb-keymap-low*
  #(nil #\Esc #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\- #\= #\Backspace
    #\Tab #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p #\[ #\] #\Newline
    :left-control #\a #\s #\d #\f #\g #\h #\j #\k #\l #\; #\' #\`
    :left-shift #\# #\z #\x #\c #\v #\b #\n #\m #\, #\. #\/ :right-shift #\u001040FC ; KP-Multiply
    ;; - - - F1 F2 F3 F4 F5
    :left-meta #\Space :capslock #\u00104001 #\u00104002 #\u00104003 #\u00104004 #\u00104005
    ;; F6 F7 F8 F9 F10 - -
    #\u00104006 #\u00104007 #\u00104008 #\u00104009 #\u0010400A nil nil
    ;; KP-7 KP-8 KP-9 KP-Minus
    #\u001040F7 #\u001040F8 #\u001040F9 #\u001040FD
    ;; KP-4 KP-5 KP-6 KP-Plus
    #\u001040F4 #\u001040F5 #\u001040F6 #\u001040FE
    ;; KP-1 KP-2 KP-3 KP-0 KP-Period - - - F11 F12 - - - - - - -
    #\u001040F1 #\u001040F2 #\u001040F3 #\u001040F0 #\u001040FA nil nil #\\ #\u0010400B #\u0010400C nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))
(defvar *gb-keymap-high*
  #(nil #\Esc #\! #\" #\£ #\$ #\% #\^ #\& #\* #\( #\) #\_ #\+ #\Backspace
    #\Tab #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\{ #\} #\Newline
    :left-control #\A #\S #\D #\F #\G #\H #\J #\K #\L #\: #\@ #\¬
    :left-shift #\~ #\Z #\X #\C #\V #\B #\N #\M #\< #\> #\? :right-shift #\u001040FC
    :left-meta #\Space :capslock #\u00104001 #\u00104002 #\u00104003 #\u00104004 #\u00104005
    #\u00104006 #\u00104007 #\u00104008 #\u00104009 #\u0010400A nil nil
    ;; KP-7 KP-8 KP-9 KP-Minus
    #\u001040F7 #\u001040F8 #\u001040F9 #\u001040FD
    ;; KP-4 KP-5 KP-6 KP-Plus
    #\u001040F4 #\u001040F5 #\u001040F6 #\u001040FE
    ;; KP-1 KP-2 KP-3 KP-0 KP-Period - - - F11 F12 - - - - - - -
    #\u001040F1 #\u001040F2 #\u001040F3 #\u001040F0 #\u001040FA nil nil #\| #\u0010400B #\u0010400C nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

(defvar *keyboard-shifted* nil)

(defun poll-keyboard ()
  (unwind-protect
       (progn #+nil(setf (isa-pic-irq-mask +ps/2-key-irq+) t)
              (loop (let ((cmd (system:io-port/8 #x64)))
                      (when (= (logand cmd 1) 1)
                        ;; Byte ready.
                        (return (system:io-port/8 #x60))))))
    #+nil(setf (isa-pic-irq-mask +ps/2-key-irq+) nil)))

(defun read-keyboard-char ()
  (loop
     (let* ((scancode (poll-keyboard))
            (key (svref (if *keyboard-shifted*
                           *gb-keymap-high*
                           *gb-keymap-low*)
                       (logand scancode #x7F))))
       (cond ((= (logand scancode #x80) 0)
              ;; Key press.
              (cond ((member key '(:shift :left-shift :right-shift))
                     (setf *keyboard-shifted* t))
                    ((characterp key)
                     (return key))
                    ((null key)
                     (write-string "Unknown keycode #x")
                     (sys.int::write-integer scancode 16)
                     (write-char #\/)
                     (sys.int::write-integer scancode))))
             (t ;; Key release.
              (case key
                ((:shift :left-shift :right-shift) (setf *keyboard-shifted* nil))))))))

(defvar *unread-char* nil)

(defun cold-read-char (stream)
  (cond (*unread-char*
         (prog1 *unread-char*
           (setf *unread-char* nil)))
        (t (let ((c (read-keyboard-char)))
             (cold-write-char c nil)
             c))))

(defun cold-unread-char (character stream)
  (when *unread-char*
    (error "Multiple unread-char!"))
  (setf *unread-char* character))

(defun cold-listen (stream)
  (or *unread-char* (= (logand (system:io-port/8 #x64) 1) 1)))
