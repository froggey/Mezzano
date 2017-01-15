;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.gui.input-drivers)

(defconstant +evdev-type-syn+ #x00)
(defconstant +evdev-type-key+ #x01)
(defconstant +evdev-type-rel+ #x02)
(defconstant +evdev-type-abs+ #x03)
(defconstant +evdev-type-msc+ #x04)
(defconstant +evdev-type-sw+  #x05)
(defconstant +evdev-type-led+ #x11)
(defconstant +evdev-type-snd+ #x12)
(defconstant +evdev-type-rep+ #x14)
(defconstant +evdev-type-ff+  #x15)
(defconstant +evdev-type-pwr+ #x16)
(defconstant +evdev-type-ff-status+ #x17)

(defvar *virtio-input-forwarders* (make-hash-table))

(defparameter *evdev-key-to-hid-character*
  #(nil #\Esc #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\- #\= #\Backspace
    #\Tab #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\[ #\] #\Newline
    #\Left-Control #\A #\S #\D #\F #\G #\H #\J #\K #\L #\; #\' #\`
    #\Left-Shift #\\ #\Z #\X #\C #\V #\B #\N #\M #\, #\. #\/ #\Right-Shift #\KP-Multiply
    #\Left-Meta #\Space #\Caps-Lock
    #\F1 #\F2 #\F3 #\F4 #\F5 #\F6 #\F7 #\F8 #\F9 #\F10
    ;; Num lock
    nil
    ;; Scroll lock
    nil
    #\KP-7 #\KP-8 #\KP-9 #\KP-Minus
    #\KP-4 #\KP-5 #\KP-6 #\KP-Plus
    #\KP-1 #\KP-2 #\KP-3
    #\KP-0 #\KP-Period
    nil ; 84 not assigned
    nil ; 85 ZENKAKUHANKAKU
    nil ; 86 102ND
    #\F11 ; 87 F11
    #\F12 ; 88 F12
    nil ; 89 RO
    nil ; 90 KATAKANA
    nil ; 91 HIRAGANA
    nil ; 92 HENKAN
    nil ; 93 KATAKANAHIRAGANA
    nil ; 94 MUHENKAN
    nil ; 95 KPJPCOMMA
    #\KP-Enter ; 96 KPENTER
    nil ; 97 RIGHTCTRL
    #\KP-Divide ; 98 KPSLASH
    nil ; 99 SYSRQ
    #\Right-Meta ; 100 RIGHTALT
    nil ; 101 LINEFEED
    #\Home ; 102 HOME
    #\Up-Arrow ; 103 UP
    #\Page-Up ; 104 PAGEUP
    #\Left-Arrow ; 105 LEFT
    #\Right-Arrow ; 106 RIGHT
    #\End ; 107 END
    #\Down-Arrow ; 108 DOWN
    #\Page-Down ; 109 PAGEDOWN
    #\Insert ; 110 INSERT
    #\Delete ; 111 DELETE
    nil ; 112 MACRO
    nil ; 113 MUTE
    nil ; 114 VOLUMEDOWN
    nil ; 115 VOLUMEUP
    nil ; 116 POWER
    nil ; 117 KPEQUAL
    nil ; 118 KPPLUSMINUS
    nil ; 119 PAUSE
    nil ; 120 SCALE
))

(defun translate-evdev-key-code (code)
  (cond ((< code (length *evdev-key-to-hid-character*))
         (aref *evdev-key-to-hid-character* code))))

(defun virtio-input-thread (device)
  (let ((mouse-button-state 0)
        (mouse-rel-x 0)
        (mouse-rel-y 0)
        (mouse-state-changed nil))
    (loop
       (sys.int::log-and-ignore-errors
        (multiple-value-bind (type code value)
            (mezzano.supervisor:read-virtio-input-device device)
          (case type
            (#.+evdev-type-syn+
             (when mouse-state-changed
               ;; Submit changes to compositor.
               (mezzano.gui.compositor:submit-mouse
                mouse-button-state
                mouse-rel-x
                mouse-rel-y)
               ;; Reset X/Y motion, but leave button state intact.
               (setf mouse-state-changed nil
                     mouse-rel-x 0
                     mouse-rel-y 0)))
            (#.+evdev-type-key+
             (let ((translated (translate-evdev-key-code code)))
               (cond (translated
                      (mezzano.gui.compositor:submit-key translated
                                                         (zerop value)))
                     ((<= #x110 code #x112)
                      ;; Mouse button change.
                      (setf mouse-state-changed t)
                      (cond ((not (zerop value))
                             ;; Press.
                             (setf mouse-button-state (logior mouse-button-state
                                                              (ash 1 (- code #x110)))))
                            (t
                             ;; Release

                             (setf mouse-button-state (logand mouse-button-state
                                                              (lognot (ash 1 (- code #x110)))))))))))
            (#.+evdev-type-rel+
             (let ((signed-value (if (logbitp 31 value)
                                     (logior (ash -1 32) value)
                                     value)))
               (case code
                 (0 ; rel-x
                  (setf mouse-state-changed t)
                  (setf mouse-rel-x signed-value))
                 (1 ; rel-y
                  (setf mouse-state-changed t)
                  (setf mouse-rel-y signed-value)))))))))))

(defun detect-virtio-input-devices ()
  (dolist (dev mezzano.supervisor:*virtio-input-devices*)
    (when (not (gethash dev *virtio-input-forwarders*))
      (format t "Created input forwarder for ~A~%"
              (with-output-to-string (s)
                (print-unreadable-object (dev s :type t :identity t))))
      (setf (gethash dev *virtio-input-forwarders*)
            (mezzano.supervisor:make-thread (lambda ()
                                              (virtio-input-thread dev))
                                            :name (format nil "Virtio-Input Forwarder for ~A"
                                                          (with-output-to-string (s)
                                                            (print-unreadable-object (dev s :type t :identity t)))))))))

(mezzano.supervisor:add-boot-hook 'detect-virtio-input-devices)
(detect-virtio-input-devices)
