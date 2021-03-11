;;;; Copyright (c) 2019, 2020, 2021 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

(in-package :mezzano.driver.usb.hid)

;;======================================================================
;; Page info
;;
;; Map from HID keyboard key values to characters. This is one "page"
;; of the HID pages described in the document USB HID Usage Tables
;; version 1.12 10/28/2004
;;
;; This is the Keyboard/Keypad Page (page 0x07). It is actually has
;; 64KB entries, however, only the first 256 are defined. So, we use a
;; 256 entry array.
;;
;; The Mezzano version only includes characters that are mapped in
;; gui/keymaps.lisp, all other entries are keywords. When a USB
;; keyboard generates a value that maps to a keyword entry, that value
;; is dropped (not passed on to the compositor code).
;;
;; The non-Mezzano version is used for testing with SBCL which doesn't
;; allow arbitrarily defined characters and therefore only supports a
;; subset of the characters supported by Mezzano.
;;
;; This is a read-only table, it is only written here during
;; initialized.
;;======================================================================

(defvar hid-keymap
  #+mezzano
  #(:no-key :roll-over-error :post-fail-error :undefined-error
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
    #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
    #\U #\V #\W #\X #\Y #\Z
    #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
    #\Newline #\Esc #\Backspace #\Tab #\Space
    #\- #\= #\[ #\] #\\ #\# #\; #\' #\` #\, #\. #\/
    #\Caps-Lock
    #\F1 #\F2 #\F3 #\F4 #\F5 #\F6 #\F7 #\F8 #\F9 #\F10 #\F11 #\F12
    #\Print-Screen #\Scroll-Lock #\Pause #\Insert #\Home
    #\Page-Up #\Delete #\End #\Page-Down
    #\Right-Arrow #\Left-Arrow #\Down-Arrow #\Up-Arrow
    :KP-num-lock #\KP-Divide #\KP-Multiply #\KP-Minus #\KP-Plus #\KP-Enter
    #\KP-1 #\KP-2 #\KP-3 #\KP-4 #\KP-5 #\KP-6 #\KP-7 #\KP-8 #\KP-9 #\KP-0
    #\KP-Period
    #\\ :application :power :KP-equal
    :F13 :F4 :F15 :F16 :F17 :F18 :F19 :F20 :F21 :F22 :F23 :F24
    :Execute :Help #\Menu :Select :Stop :Again :Undo
    :Cut :Copy :Paste :Find :Mute :Volume-Up :Volume-Down
    :Locking-Caps-Lock :Locking-Num-Lock :Locking-Scroll-Lock
    :KP-Comma :KP-Equal
    :Int-1 :Int-2 :Int-3 :Int-4 :Int-5 :Int-6 :Int-7 :Int-8 :Int-9
    :Lang-1 :Lang-2 :Lang-3 :Lang-4 :Lang-5 :Lang-6 :Lang-7 :Lang-8 :Lang-9
    :Erase :Attention :Cancel :Clear :Prior :Return :Separator :Out :Oper
    :Clear/Again :CrSel/Props :ExSel
    :reserved-A5 :reserved-A6 :reserved-A7
    :reserved-A8 :reserved-A9 :reserved-AA :reserved-AB
    :reserved-AC :reserved-AD :reserved-AE :reserved-AF
    :KP-00 :KP-000
    :Thousands-Separator :Decimal-Separator
    :Currency-Unit :Currency-Sub-Unit
    :KP-Left-Paren :KP-Right-Paren :KP-Left-Brace :KP-Right-Brace
    :KP-Tab :KP-Backspace
    :KP-A :KP-B :KP-C :KP-D :KP-E :KP-F
    :KP-XOR :KP-^ :KP-% :KP-< :KP-> :KP-& :KP-&& :KP-Bar :KP-Bar-Bar
    :KP-Colon :KP-Pound :KP-Space :KP-@ :KP-!
    :KP-Mem-Store :KP-Mem-Recall :KP-Mem-Clear
    :KP-Mem-Add :KP-Mem-Minus :KP-Mem-Multiply :KP-Mem-Divide
    :KP-Plus-Minus :KP-Clear :KP-Clear-Entry
    :KP-Binary :KP-Octal :KP-Decimal :KP-Hexadecimal
    :reserved-DE :reserved-DF
    :Left-Control :Left-Shift :Left-Alt :Left-GUI
    :Right-Control :Right-Shift :Right-Alt :Right-GUI
    :reserved-E8 :reserved-E9 :reserved-EA :reserved-EB
    :reserved-EC :reserved-ED :reserved-EE :reserved-EF
    :reserved-F0 :reserved-F1 :reserved-F2 :reserved-F3
    :reserved-F4 :reserved-F5 :reserved-F6 :reserved-F7
    :reserved-F8 :reserved-F9 :reserved-FA :reserved-FB
    :reserved-FC :reserved-FD :reserved-FE :reserved-FF)
  #-mezzano
  #(:no-key :roll-over-error :post-fail-error :undefined-error
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
    #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
    #\U #\V #\W #\X #\Y #\Z
    #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
    #\Newline #\Esc #\Backspace #\Tab #\Space
    #\- #\= #\[ #\] #\\ #\# #\; #\' #\` #\, #\. #\/
    :caps-lock
    :F1 :F2 :F3 :F4 :F5 :F6 :F7 :F8 :F9 :F10 :F11 :F12
    :Print-Screen :Scroll-Lock :Pause :Insert :Home
    :Page-Up :Delete :End :Page-Down
    :Right-Arrow :Left-Arrow :Down-Arrow :Up-Arrow
    :KP-num-lock :KP-Divide :KP-Multiply :KP-Minus :KP-Plus :KP-Enter
    :KP-1 :KP-2 :KP-3 :KP-4 :KP-5 :KP-6 :KP-7 :KP-8 :KP-9 :KP-0
    :KP-Period
    #\\ :application :power :KP-equal
    :F13 :F4 :F15 :F16 :F17 :F18 :F19 :F20 :F21 :F22 :F23 :F24
    :Execute :Help :Menu :Select :Stop :Again :Undo
    :Cut :Copy :Paste :Find :Mute :Volume-Up :Volume-Down
    :Locking-Caps-Lock :Locking-Num-Lock :Locking-Scroll-Lock
    :KP-Comma :KP-Equal
    :Int-1 :Int-2 :Int-3 :Int-4 :Int-5 :Int-6 :Int-7 :Int-8 :Int-9
    :Lang-1 :Lang-2 :Lang-3 :Lang-4 :Lang-5 :Lang-6 :Lang-7 :Lang-8 :Lang-9
    :Erase :Attention :Cancel :Clear :Prior :Return :Separator :Out :Oper
    :Clear/Again :CrSel/Props :ExSel
    :reserved-A5 :reserved-A6 :reserved-A7
    :reserved-A8 :reserved-A9 :reserved-AA :reserved-AB
    :reserved-AC :reserved-AD :reserved-AE :reserved-AF
    :KP-00 :KP-000
    :Thousands-Separator :Decimal-Separator
    :Currency-Unit :Currency-Sub-Unit
    :KP-Left-Paren :KP-Right-Paren :KP-Left-Brace :KP-Right-Brace
    :KP-Tab :KP-Backspace
    :KP-A :KP-B :KP-C :KP-D :KP-E :KP-F
    :KP-XOR :KP-^ :KP-% :KP-< :KP-> :KP-& :KP-&& :KP-Bar :KP-Bar-Bar
    :KP-Colon :KP-Pound :KP-Space :KP-@ :KP-!
    :KP-Mem-Store :KP-Mem-Recall :KP-Mem-Clear
    :KP-Mem-Add :KP-Mem-Minus :KP-Mem-Multiply :KP-Mem-Divide
    :KP-Plus-Minus :KP-Clear :KP-Clear-Entry
    :KP-Binary :KP-Octal :KP-Decimal :KP-Hexadecimal
    :reserved-DE :reserved-DF
    :Left-Control :Left-Shift :Left-Alt :Left-GUI
    :Right-Control :Right-Shift :Right-Alt :Right-GUI
    :reserved-E8 :reserved-E9 :reserved-EA :reserved-EB
    :reserved-EC :reserved-ED :reserved-EE :reserved-EF
    :reserved-F0 :reserved-F1 :reserved-F2 :reserved-F3
    :reserved-F4 :reserved-F5 :reserved-F6 :reserved-F7
    :reserved-F8 :reserved-F9 :reserved-FA :reserved-FB
    :reserved-FC :reserved-FD :reserved-FE :reserved-FF))

;; submit key function name, the compositor package may not defined
;; when USB drivers are loaded, the symbol is generated when a hid
;; keyboard is probed.
(defvar *submit-key* nil)

;;======================================================================
;; HID Keyboard interrupt handler
;;======================================================================
(defvar *keyboard-ints* NIL) ;; for debug

(defstruct (hid-keyboard-endpt (:include hid-endpt) (:conc-name hid-endpt-))
  pressed-keys
  pressed-modifiers)

(defun keyboard-int-callback (driver endpoint-num status length buf)
  (unwind-protect
       (cond ((eq status :success)
              (let ((endpoint (aref (hid-driver-endpoints driver) endpoint-num)))
                (funcall (hid-endpt-function endpoint) endpoint length buf)))
             (T
              (format sys.int::*cold-stream*
                      "Interrupt error ~A on endpoint number ~D~%"
                      status endpoint-num)
              (with-trace-level (1)
                (format sys.int::*cold-stream*
                        "length: ~D~%buf: ~S~%" length buf))))
    (with-trace-level (7)
      (push (format nil "~D: ~A" length buf) *keyboard-ints*))
    (free-buffer buf)))

;;======================================================================
;; Code which generates function to process interrupt buffer data
;;======================================================================
(defvar *modifier-buttons*
  ;; Modifier buttons can either be :button-e[0-7] or :<keyword>
  ;; depending on how the HID descriptor is defined, so we have to
  ;; check for both cases.
  '(:button-e0 :button-e1 :button-e2 :button-e3
    :button-e4 :button-e5 :button-e6 :button-e7
    :left-control :left-shift :left-alt :left-gui
    :right-control :right-shift :right-alt :right-gui))

(defvar *modifier->char*
  ;; Modifier buttons can either be :button-e[0-7] or :<keyword>
  ;; depending on how the HID descriptor is defined, so we have to
  ;; translate both cases.
  #+mezzano
  '((:button-e0 . #\Left-Control)
    (:button-e1 . #\Left-Shift)
    (:button-e2 . #\Left-Meta)    ;; USB: alt
    (:button-e3 . #\Left-Super)   ;; USB: gui
    (:button-e4 . #\Right-Control)
    (:button-e5 . #\Right-Shift)
    (:button-e6 . #\Right-Meta)   ;; USB: alt
    (:button-e7 . #\Right-Super)  ;; USB: gui
    (:left-control  . #\Left-Control)
    (:left-shift    . #\Left-Shift)
    (:left-alt      . #\Left-Meta)
    (:left-meta     . #\Left-Super)
    (:right-control . #\Right-Control)
    (:right-shift   . #\Right-Shift)
    (:right-alt     . #\Right-Meta)
    (:right-gui     . #\Right-Super)
  #-mezzano
  '((:button-e0 . :left-control)
    (:button-e1 . :left-shift)
    (:button-e2 . :left-meta)    ;; USB: alt
    (:button-e3 . :left-super)   ;; USB: gui
    (:button-e4 . :right-control)
    (:button-e5 . :right-shift)
    (:button-e6 . :right-meta)    ;; USB: alt
    (:button-e7 . :right-super) ;; USB: gui
    (:left-control  . :left-control)
    (:left-shift    . :left-shift)
    (:left-alt      . :left-meta)
    (:left-gui      . :left-super)
    (:right-control . :right-control)
    (:right-shift   . :right-shift)
    (:right-alt     . :right-meta)
    (:right-gui     . :right-super))))

(defun get-scattered-modifier-bits (fields buf offset)
  ;; Handle general case where modifier bits are in multiple buffer
  ;; bytes by gathering the bits into a single byte. This case is not
  ;; expected to be used, because all of the hid keyboard examples
  ;; seen so far have the modifier bits in a single byte. However,
  ;; this code is provided for completeness.
  `(logior
    ,@(loop
         for key in *modifier-buttons*
         for field = (cdr (assoc key fields))
         with bit-pos = 0
         when field collect
           `(ash (aref ,buf (+ ,offset ,(getf field :byte-offset)))
                 ,(- bit-pos (getf field :bit-offset)))
         when field do
           (incf bit-pos))))

(defun get-modifier-bits (fields buf offset)
  (let ((byte-offset NIL)
        (single-offset-p T))
    (loop
       for key in *modifier-buttons*
       for field-byte-offset = (getf (cdr (assoc key fields)) :byte-offset)
       do
         (cond ((null byte-offset)
                (setf byte-offset field-byte-offset))
               ((and field-byte-offset
                     (/= field-byte-offset byte-offset))
                (setf single-offset-p NIL))))
    (cond ((null byte-offset)
           (format #+mezzano sys.int::*cold-stream* #-mezzano t
                   "HID keyboard probe failed because ~
                   no modifier buttons found~%")
           (throw :probe-failed :failed))
          (single-offset-p
           ;; this is the common case - all of the modifiers in a single byte
           `(aref ,buf (+ ,offset ,byte-offset)))
          (T
           (get-scattered-modifier-bits fields buf offset)))))

(defun modifier-release-code (fields modifiers old-modifiers)
  (let ((%released-modifiers (gensym "RELEASED-MODIFIERS-")))
    `(let ((,%released-modifiers (logandc2 ,old-modifiers ,modifiers)))
       (when (/= ,%released-modifiers 0)
         ,@(loop
              for (button . key) in *modifier->char*
              for field = (cdr (assoc button fields))
              when field collect
                `(when (logbitp ,(getf field :bit-offset) ,%released-modifiers)
                   (funcall *submit-key* ,key T)))))))

(defun modifier-press-code (fields modifiers old-modifiers)
  (let ((%pressed-modifiers (gensym "PRESSED-MODIFIERS-")))
    `(let ((,%pressed-modifiers (logandc2 ,modifiers ,old-modifiers)))
       (when (/= ,%pressed-modifiers 0)
         ,@(loop
              for (button . key) in *modifier->char*
              for field = (cdr (assoc button fields))
              when field collect
                `(when (logbitp ,(getf field :bit-offset) ,%pressed-modifiers)
                   (funcall *submit-key* ,key NIL)))))))

(defun key-release-code (cur-keys old-keys)
  (let ((%key (gensym "KEY-")))
    `(dolist (,%key (set-difference ,old-keys ,cur-keys))
       (funcall *submit-key* ,%key T))))

(defun key-press-code (cur-keys old-keys)
  (let ((%key (gensym "KEY-")))
    `(dolist (,%key (set-difference ,cur-keys ,old-keys))
       (funcall *submit-key* ,%key NIL))))

(defun generate-keyboard-code (report)
  (let* ((fields (caddr report))
         (array-field (cdr (assoc :array-01 fields))))
    (when (null array-field)
      (format #+mezzano sys.int::*cold-stream* #-mezzano t
              "HID keyboard probe failed because ~
               no character array field found~%")
      (throw :probe-failed :failed))
    `(,(car report)
       (let ((cur-keys (loop
                          repeat ,(getf array-field :count)
                          for idx = (+ offset ,(getf array-field :byte-offset))
                          then (1+ idx)
                          for key = (aref buf idx)
                          when (/= key 0) collect (aref hid-keymap key)))
             (cur-modifiers ,(get-modifier-bits fields 'buf 'offset)))
         ,@(with-trace-level (5)
             '((push (list old-keys old-modifiers cur-keys cur-modifiers)
                logs)))
         ,(key-release-code 'cur-keys 'old-keys)
         ,(modifier-release-code fields 'cur-modifiers 'old-modifiers)
         ,(modifier-press-code fields 'cur-modifiers 'old-modifiers)
         ,(key-press-code 'cur-keys 'old-keys)
         (setf old-keys cur-keys
               old-modifiers cur-modifiers)
         (incf offset ,(/ (cadddr report) 8))))))

(defun generate-keyboard-case-clauses (reports)
  (let* ((keyboard-report-p NIL)
         (clauses
          (loop
             for report in reports
             collect
               (cond ((eq (cadr report) :keyboard)
                      (setf keyboard-report-p T)
                      (generate-keyboard-code report))
                     (T
                      `(incf offset ,(/ (cadddr report) 8)))))))
    (when (not keyboard-report-p)
      (format #+mezzano sys.int::*cold-stream* #-mezzano t
              "HID keyboard probe failed because ~
               report descritor contained no keyboard reports~%")
      (throw :probe-failed :failed))
    clauses))

(defun generate-keyboard-buf-function (reports)
  (let ((case-clauses (generate-keyboard-case-clauses reports)))
    `(lambda (endpoint length buf)
       (loop
          with offset = 0
          with old-keys = (hid-endpt-pressed-keys endpoint)
          with old-modifiers = (hid-endpt-pressed-modifiers endpoint)
          ,@(with-trace-level (5) '(with logs = nil))
          when (>= offset length) do
            (progn
              ,@(with-trace-level (5)
                  '((loop
                       for log in (reverse logs)
                       do
                         (format sys.int::*cold-stream* "~A ~A - ~A ~A~%"
                          (car log) (cadr log) (caddr log) (cadddr log)))))
              (setf (hid-endpt-pressed-keys endpoint) old-keys
                    (hid-endpt-pressed-modifiers endpoint) old-modifiers)
              (return))
          do
            ,@(if (= (length case-clauses) 1)
                  (cdar case-clauses)
                  `((case (aref buf offset)
                      ,@case-clauses)))))))

(defun generate-keyboard-buf-code (descriptors)
  (let ((reports (mapcar
                  #'(lambda (collection)
                      (multiple-value-list (convert-collection
                                            collection 0 0 0)))
                  descriptors)))
    (with-trace-level (1)
      (format *trace-stream* "reports:~%~S~%~%" reports))
    (let ((func (generate-keyboard-buf-function reports)))
      (with-trace-level (1)
        (format *trace-stream* "function:~%~A~%~%" func))
      (compile nil func))))

(defun probe-hid-keyboard (usbd device configs descriptors)
  (setf *submit-key* (intern "SUBMIT-KEY" :mezzano.gui.compositor))
  (let ((endpoint (make-hid-keyboard-endpt :type :keyboard)))
    (setf (hid-endpt-parse-state endpoint) descriptors
          (hid-endpt-function endpoint) (generate-keyboard-buf-code descriptors)
          (hid-endpt-pressed-keys endpoint) NIL
          (hid-endpt-pressed-modifiers endpoint) 0)
    (let ((endpt-desc (pop configs)))
      (when (or (null endpt-desc)
                (/= (aref endpt-desc +ed-type+) +desc-type-endpoint+))
        (sup:debug-print-line "HID probe failed because "
                              "found descriptor type "
                              (aref endpt-desc +ed-type+)
                              " instead of endpoint descriptor.")
        (throw :probe-failed :failed))
      (let ((max-packet (logior(aref endpt-desc +ed-max-packet+)
                               (ash (aref endpt-desc (1+ +ed-max-packet+)) 8))))
        ;; The buffer size is computed to be the largest buffer <= 64
        ;; bytes that contains an integer number of endpoint
        ;; packets. Being a multiple of endpoint packets seems to be a
        ;; requirement to avoid Data Overruns. This will be a problem
        ;; if a keyboard report runs over the end of the buffer.
        (setf (hid-endpt-buf-size endpoint)
              (* (floor 56 max-packet) max-packet))
        (with-trace-level (1)
          (format *trace-stream* "buffer size: ~D~%"
                  (hid-endpt-buf-size endpoint))))
      (let* ((driver (make-hid-driver
                      :usbd usbd
                      :device device
                      :endpoints (make-array 32 :initial-element NIL)))
             (endpt-num
              (parse-endpt-descriptor
               usbd driver endpoint device endpt-desc 'keyboard-int-callback)))
        (setf (aref (hid-driver-endpoints driver) endpt-num) endpoint)
        (values configs driver)))))

(register-hid-device-function :keyboard 'probe-hid-keyboard)
