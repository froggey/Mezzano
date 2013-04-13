(in-package #:sys.int)

(defstruct cold-stream)
(defvar *screen-offset* (cons 0 0))

(defvar *cold-stream-screen*)

(defun cold-stream-init ()
  (setf *terminal-io* (make-cold-stream))
  (setf *screen-offset* (cons 0 0))
  (setf *cold-stream-screen* (list :vga #x80000B8000 80 25))
  (when *kboot-tag-list*
    (flet ((p/8 (addr) (memref-unsigned-byte-8 (+ #x8000000000 addr) 0))
           (p/16 (addr) (memref-unsigned-byte-16 (+ #x8000000000 addr) 0))
           (p/32 (addr) (memref-unsigned-byte-32 (+ #x8000000000 addr) 0))
           (p/64 (addr) (memref-unsigned-byte-64 (+ #x8000000000 addr) 0)))
      (let ((addr *kboot-tag-list*)
            ;; For sanity checking.
            (max-addr (+ *kboot-tag-list* 1024)))
        (loop (when (>= addr max-addr) (return))
           (let ((type (p/32 (+ addr 0)))
                 (size (p/32 (+ addr 4))))
             (when (and (eql addr *kboot-tag-list*)
                        (not (eql type +kboot-tag-core+)))
               (format t "CORE tag not first in the list?~%")
               (return))
             (case type
               (#.+kboot-tag-none+ (return))
               (#.+kboot-tag-core+
                (unless (eql addr *kboot-tag-list*)
                  (format t "CORE tag not first in the list?~%")
                  (return))
                (setf max-addr (+ *kboot-tag-list* (p/32 (+ addr 16)))))
               (#.+kboot-tag-video+
                (case (p/32 (+ addr 8))
                  (#.+kboot-video-vga+
                   (setf *cold-stream-screen* (list :vga
                                                    (+ #x8000000000 (p/64 (+ addr 24))) ; address
                                                    (p/8 (+ addr 16)) ; cols
                                                    (p/8 (+ addr 17))) ; rows
                         *screen-offset* (cons (p/8 (+ addr 18)) (p/8 (+ addr 19)))))
                  (#.+kboot-video-lfb+
                   (cond
                     ((and (logtest (p/32 (+ addr 16)) +kboot-lfb-rgb+) ; flags
                           ;; Currently xRGB 8888 supported...
                           (eql (p/8 (+ addr 28)) 32) ; bpp
                           (eql (p/8 (+ addr 60)) 8) ; red_size
                           (eql (p/8 (+ addr 62)) 8) ; green_size
                           (eql (p/8 (+ addr 64)) 8) ; blue_size
                           (eql (p/8 (+ addr 61)) 16) ; red_pos
                           (eql (p/8 (+ addr 63)) 8) ; green_pos
                           (eql (p/8 (+ addr 65)) 0) ; blue_pos
                           (eql (p/32 (+ addr 32)) (* (p/32 (+ addr 20)) 4))) ; pitch == width*4
                      (setf *cold-stream-screen* (list :framebuffer
                                                       (make-array (list (p/32 (+ addr 24)) (p/32 (+ addr 20)))
                                                                   :element-type '(unsigned-byte 32)
                                                                   :memory (+ #x8000000000 (p/64 (+ addr 40)))))))
                     (t (setf *cold-stream-screen* nil))
                     )))))
               (incf addr (round-up size 8)))))))
  (setf *keyboard-shifted* nil))
#+nil(add-hook '*early-initialize-hook* 'cold-stream-init)

(defun cold-write-char (c stream)
  (setf (system:io-port/8 #xE9) (logand (char-code c) #xFF))
  (cond ((eql (first *cold-stream-screen*) :vga)
         (cond ((eql c #\Newline)
                (setf (car *screen-offset*) (third *cold-stream-screen*)))
               (t (setf (sys.int::memref-unsigned-byte-16 (second *cold-stream-screen*)
                                                          (+ (car *screen-offset*)
                                                             (* (cdr *screen-offset*)
                                                                (third *cold-stream-screen*))))
                        (logior (logand (char-code c) #xFF) #x0F00))
                  (incf (car *screen-offset*))))
         (when (>= (car *screen-offset*) (third *cold-stream-screen*))
           (setf (car *screen-offset*) 0)
           (incf (cdr *screen-offset*)))
         (when (>= (cdr *screen-offset*) (fourth *cold-stream-screen*))
           (setf (cdr *screen-offset*) 0)))
        ((eql (first *cold-stream-screen*) :framebuffer)
         (let ((fb (second *cold-stream-screen*))
               (*cold-stream-screen* nil)
               (x (car *screen-offset*))
               (y (cdr *screen-offset*)))
           (cond ((eql c #\Newline)
                  ;; Clear the next line.
                  (setf (car *screen-offset*) 0
                        y (if (> (+ y 16 16) (array-dimension fb 0))
                              0
                              (+ y 16))
                        (cdr *screen-offset*) y)
                  (%bitset 16 (array-dimension fb 1) #xFF000000 fb y 0))
                 (t (let ((width (unifont-glyph-width c)))
                      (when (> (+ x width) (array-dimension fb 1))
                        ;; Advance to the next line.
                        ;; Maybe should clear the end of the current line?
                        (setf x 0
                              y (if (> (+ y 16 16) (array-dimension fb 0))
                                    0
                                    (+ y 16))
                              (cdr *screen-offset*) y)
                        (%bitset 16 (array-dimension fb 1) #xFF000000 fb y 0))
                      (render-char-at c fb x y)
                      (incf x width)
                      (setf (car *screen-offset*) x))))))
        ((eql (first *cold-stream-screen*) :serial)
         (setf (system:io-port/8 (second *cold-stream-screen*)) (logand (char-code c) #xFF)))
        ((functionp (first *cold-stream-screen*))
         (funcall (first *cold-stream-screen*) c)))
 c)

(defun cold-start-line-p (stream)
  (or (null *screen-offset*) (zerop (car *screen-offset*))))

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

(defconstant +ps/2-key-irq+ 1)
(defconstant +ps/2-aux-irq+ 12)

(defun poll-keyboard ()
  (unwind-protect
       (progn (setf (isa-pic-irq-mask +ps/2-key-irq+) t)
              (loop (let ((cmd (system:io-port/8 #x64)))
                      (when (= (logand cmd 1) 1)
                        ;; Byte ready.
                        (return (system:io-port/8 #x60))))))
    (setf (isa-pic-irq-mask +ps/2-key-irq+) nil)))

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

(defun cold-clear-input (stream)
  nil)
