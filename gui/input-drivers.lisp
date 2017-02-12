;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.gui.input-drivers)

(defvar *keyboard-forwarder* nil)
(defvar *mouse-forwarder* nil)

(defconstant +extended-scan-code+ #xE0)

(defvar *extended-key-alist*
  '((#x5B #\Left-Super)
    (#x1D #\Right-Control)
    (#x5C #\Right-Super)
    (#x38 #\Right-Meta)
    (#x5D #\Menu)
    (#x52 #\Insert)
    (#x47 #\Home)
    (#x49 #\Page-Up)
    (#x53 #\Delete)
    (#x4F #\End)
    (#x51 #\Page-Down)
    (#x48 #\Up-Arrow)
    (#x4B #\Left-Arrow)
    (#x50 #\Down-Arrow)
    (#x4D #\Right-Arrow)
    (#x35 #\KP-Divide)
    (#x1C #\KP-Enter)))

(defvar *translation-table*
  #(nil #\Esc #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\- #\= #\Backspace
    #\Tab #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\[ #\] #\Newline
    #\Left-Control #\A #\S #\D #\F #\G #\H #\J #\K #\L #\; #\' #\`
    #\Left-Shift #\# #\Z #\X #\C #\V #\B #\N #\M #\, #\. #\/ #\Right-Shift #\KP-Multiply
    #\Left-Meta #\Space #\Caps-Lock #\F1 #\F2 #\F3 #\F4 #\F5
    #\F6 #\F7 #\F8 #\F9 #\F10 nil nil
    #\KP-7 #\KP-8 #\KP-9 #\KP-Minus
    #\KP-4 #\KP-5 #\KP-6 #\KP-Plus
    #\KP-1 #\KP-2 #\KP-3 #\KP-0 #\KP-Period nil nil #\\ #\F11 #\F12 nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
  "An array, converting from non-extended scancodes to HID keys.")

(defun keyboard-forwarder-thread ()
  ;; Read bytes from the keyboard and translate them into HID events for the input manager.
  (loop
     (sys.int::log-and-ignore-errors
       (let ((byte (mezzano.supervisor:ps/2-key-read)))
         (cond
           ((eql byte +extended-scan-code+)
            ;; Reading extended scan code.
            (setf byte (mezzano.supervisor:ps/2-key-read))
            (let ((extended-key (assoc (logand byte #x7F) *extended-key-alist*)))
              (cond (extended-key
                     ;; Got a recognized extended key, submit it.
                     (mezzano.gui.compositor:submit-key (second extended-key) (logtest byte #x80)))
                    (t (format *error-output* "Ignoring unknown extended scancode ~2,'0X~%" byte)))))
           (t (let ((key (aref *translation-table* (logand byte #x7F))))
                (cond (key
                       ;; Got a regular key, submit it.
                       (mezzano.gui.compositor:submit-key key (logtest byte #x80)))
                      (t (format *error-output* "Ignoring unknown scancode ~2,'0X~%" byte))))))))))

(defun mouse-forwarder-thread ()
  ;; Read bytes from the mouse and turn them into HID events.
  (loop
     (sys.int::log-and-ignore-errors
       (let ((byte-1 (mezzano.supervisor:ps/2-aux-read)))
         ;; Check sync bit.
         (when (logtest byte-1 #b00001000)
           (let ((byte-2 (mezzano.supervisor:ps/2-aux-read))
                 (byte-3 (mezzano.supervisor:ps/2-aux-read)))
             (mezzano.gui.compositor:submit-mouse
              (logand byte-1 #b111) ; Buttons 1 to 3.
              (logior byte-2 (if (logtest byte-1 #b00010000) -256 0)) ; x-motion
              (- (logior byte-3 (if (logtest byte-1 #b00100000) -256 0)))))))))) ; y-motion

(when (not *keyboard-forwarder*)
  (setf *keyboard-forwarder* (mezzano.supervisor:make-thread 'keyboard-forwarder-thread
                                                             :name "Keyboard Forwarder")))

(when (not *mouse-forwarder*)
  (setf *mouse-forwarder* (mezzano.supervisor:make-thread 'mouse-forwarder-thread
                                                          :name "Mouse Forwarder")))
