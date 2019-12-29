;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

;;======================================================================
;;
;; HID (Human Interface Device) Class Driver
;;
;; This file is made up of three broad sections:
;;     1. The HID device drivers
;;     2. Code that generates a function that decodes interrupt
;;        buffers (input reports) using the data structure generated
;;        by section 3.
;;     3. Code that creates a data structure for use by section 2
;;        above by parsing the raw report description.
;;
;;======================================================================

(defpackage :mezzano.driver.usb.hid
  (:use :cl :mezzano.driver.usb)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sys.int :mezzano.internals)))

(in-package :mezzano.driver.usb.hid)

(defvar *trace-stream* t #+nil sys.int::*cold-stream*)
(defvar *trace* 0)

(defmacro with-trace-level ((trace-level) &body body)
  `(when (>= *trace* ,trace-level)
     ,@body))

;; submit mouse function name, compositor package not defined when
;; drivers are loaded, generate the symbol at first hid mouse probe

(defstruct hid-endpt
  type                     ; :mouse or :keyboard
  buf-size                 ; interrupt buffer size (bytes)
  parse-state              ; for debug
  function                 ; function that decodes interrupt bufs
  )

(defstruct hid-driver
  usbd
  device
  endpoints
  num-devices                           ; number of active devices
  )

(defun parse-endpt-descriptor
    (usbd driver endpoint device endpt-desc callback)
  ;; get buffer size from report in interface
  ;; get packet size from endpt-desc
  (let* ((address (aref endpt-desc +ed-address+))
         (endpt-in (ldb-test +ed-direction-field+ address))
         (endpt-num (ldb +ed-endpt-num-field+ address)))

    (ecase (aref endpt-desc +ed-attributes+)
      (#.+ed-attr-interrupt+
       (create-interrupt-endpt usbd
                               device
                               driver
                               endpt-num
                               3        ;; sort of depends on interval
                               (hid-endpt-buf-size endpoint)
                               callback
                               (aref endpt-desc +ed-interval+)))
      )
    endpt-num))

(defmethod delete-device ((driver hid-driver) device)
  ;; Device has disconnected - clean up any resources allocated here
  ;; Nothing to do here?
  )

;;======================================================================
;; HID Mouse Driver
;;======================================================================

(defvar *submit-mouse* nil)

(defun probe-hid-mouse (usbd device iface-desc configs)
  (when (/= (aref iface-desc +id-num-endpoints+) 1)
    (sup:debug-print-line "HID Probe failed because "
                          "mouse interface descriptor has "
                          (aref iface-desc +id-num-endpoints+)
                          " endpoints. Only exactly 1 supported.")
    (throw :probe-failed nil))

  (when (not *submit-mouse*)
    (setf *submit-mouse* (intern "SUBMIT-MOUSE" :mezzano.gui.compositor)))

  (let ((iface-num (aref iface-desc +id-number+))
        (hid-desc (pop configs))
        (endpoint (make-hid-endpt :type :mouse)))
    (when (or (null hid-desc)
              (/= (aref hid-desc +hd-type+) +desc-type-hid+))
      (sup:debug-print-line
       "HID Probe failed because "
       "mouse interface descriptor not followed by HID descriptor.")
      (throw :probe-failed nil))

    (let ((type (aref hid-desc +hd-descriptor-type+))
          (size (get-unsigned-word/16 hid-desc +hd-descriptor-length+)))
      (when (/= type +desc-type-report+)
        (sup:debug-print-line "HID probe failed because descriptor type "
                              type
                              " not report, the only type supported.")
        (throw :probe-failed nil))

      (let ((state (parse-report-descriptor usbd device iface-num size)))
        (multiple-value-bind (buf-size function) (generate-mouse-buf-code state)
          (setf (hid-endpt-buf-size endpoint) buf-size
                (hid-endpt-parse-state endpoint) state
                (hid-endpt-function endpoint) function))))

    (let ((endpt-desc (pop configs)))
      (when (or (null endpt-desc)
                (/= (aref endpt-desc +ed-type+) +desc-type-endpoint+))
        (sup:debug-print-line "HID probe failed because "
                              "found descriptor type "
                              (aref endpt-desc +ed-type+)
                              " instead of endpoint descriptor.")
        (throw :probe-failed nil))
      (let* ((driver (make-hid-driver
                      :usbd usbd
                      :device device
                      :endpoints (make-array 32 :initial-element NIL)))
             (endpt-num
              (parse-endpt-descriptor
               usbd driver endpoint device endpt-desc 'mouse-int-callback)))
        (setf (aref (hid-driver-endpoints driver) endpt-num) endpoint)
        (values configs driver)))))

(define-usb-class-driver "HID Mouse" 'probe-hid-mouse
  '((#.+id-class-hid+ #.+hid-subclass-none+ #.+id-protocol-mouse+)
    (#.+id-class-hid+ #.+hid-subclass-boot+ #.+id-protocol-mouse+)))

;;======================================================================
;;
;; At some point these variables (*threshold* and *multiplier*) and
;; the function (adjust-motion) should be settable from a user
;; configuration GUI so that users can customize their pointer's
;; operation. (Of course the (declaim (inline ...)) will have to be
;; removed. Aug. 11, 2019
;;
;;======================================================================

(defvar *threshold* 50)
(defvar *multiplier* 3)

(declaim (inline adjust-motion))

(defun adjust-motion (motion)
  (cond ((>= motion *threshold*)
         (- (* *multiplier* motion) (* (1- *multiplier*) *threshold*)))
        ((>= motion (- *threshold*))
         motion)
        (T
         (+ (* *multiplier* motion) (* (1- *multiplier*) *threshold*)))))

(defun mouse-event (buttons x-motion y-motion wheel-motion)
  (funcall *submit-mouse*
           (logior buttons
                   (if (> wheel-motion 0)
                       #b00001000
                       0)
                   (if (< wheel-motion 0)
                       #b00010000
                       0))
           (adjust-motion x-motion)
           (adjust-motion y-motion))
  (when (/= wheel-motion 0)
    ;; button up event for buttons 4 and 5
    (funcall *submit-mouse* buttons 0 0)))

(defvar *mouse-ints* NIL) ;; for debug

(defun mouse-int-callback (driver endpoint-num status length buf)
  (unwind-protect
       (cond ((eq status :success)
              (let ((endpoint (aref (hid-driver-endpoints driver) endpoint-num)))
                (funcall (hid-endpt-function endpoint) length buf)))
             (T
              (format sys.int::*cold-stream*
                      "Interrupt error ~A on endpoint number ~D~%"
                      status endpoint-num)))
    (with-trace-level (7)
      (push (format nil "~D: ~A" length buf) *mouse-ints*))
    (free-buffer buf)))

;;======================================================================
;; HID Keyboard Driver
;;======================================================================

(defun probe-hid-keyboard (usbd driver device iface-desc configs)
  (when (/= (aref iface-desc +id-num-endpoints+) 1)
    (sup:debug-print-line "HID Probe failed because "
                          "keyboard interface descriptor has "
                          (aref iface-desc +id-num-endpoints+)
                          " endpoints. Only exactly 1 supported.")
    (throw :probe-failed nil))

  (let ((iface-num (aref iface-desc +id-number+))
        (hid-desc (pop configs))
        (endpoint (make-hid-endpt :type :keyboard)))

    (when (or (null hid-desc)
              (/= (aref hid-desc +hd-type+) +desc-type-hid+))
      (sup:debug-print-line
       "HID Probe failed because "
       "keyboard interface descriptor not followed by HID descriptor.")
      (throw :probe-failed nil))

    (let ((type (aref hid-desc +hd-descriptor-type+))
          (size (get-unsigned-word/16 hid-desc +hd-descriptor-length+)))
      (when (/= type +desc-type-report+)
        (sup:debug-print-line "HID probe failed because descriptor type "
                              type
                              " not report, the only type supported.")
        (throw :probe-failed nil))

      (let ((state (parse-report-descriptor
                    usbd driver device iface-num size)))
        (multiple-value-bind (buf-size function)
            (generate-keyboard-buf-code state)
          (setf (hid-endpt-buf-size endpoint) buf-size
              (hid-endpt-parse-state endpoint) state
              (hid-endpt-function endpoint) function))))

    (let ((endpt-desc (pop configs)))
      (when (or (null endpt-desc)
                (/= (aref endpt-desc +ed-type+) +desc-type-endpoint+))
        (sup:debug-print-line "HID probe failed because "
                              "found descriptor type "
                              (aref endpt-desc +ed-type+)
                              " instead of endpoint descriptor.")
        (throw :probe-failed nil))
      ;; TODO write keyboard-int-callback
      #+nil
      (let ((endpt-num (parse-endpt-descriptor usbd
                                               endpoint
                                               device
                                               endpt-desc
                                               'keyboard-int-callback)))
        (setf (aref (hid-driver-endpoints driver) endpt-num) endpoint))))
  configs)

#+nil
(define-usb-class-driver "HID Keyboard" 'probe-hid-keyboard
  '((#.+id-class-hid+ #.+hid-subclass-none+ #.+id-protocol-keyboard+)
    (#.+id-class-hid+ #.+hid-subclass-boot+ #.+id-protocol-keyboard+)))


;;======================================================================
;; Structures used to parse HID Report descriptor and to generate code
;; that decodes interrupt buffers (input reports).
;; ======================================================================

(defstruct report-field
  name
  byte-offset
  bit-offset
  num-bits
  count
  minimum
  maximum
  values)

(defun find-report-field (report name)
  (dolist (field report)
    (when (eql (report-field-name field) name)
      (return field))))

(defstruct report-info
  byte-offset
  bit-offset
  format)

(defstruct parse-info
  input              ; report-info for input report
  output             ; report-info for output report
  feature            ; report-info for feature report
  input-reports
  output-reports
  feature-reports
  state                                 ; plist
  )

(defun parse-state-value (state indicator)
  (getf (parse-info-state state) indicator))

(defun (setf parse-state-value) (value state indicator)
  (setf (getf (parse-info-state state) indicator) value))

(defun increment-position (report-info num-bits)
  (multiple-value-bind (bytes bit-pos)
      (truncate (+ (report-info-bit-offset report-info) num-bits) 8)
    (incf (report-info-byte-offset report-info) bytes)
    (setf (report-info-bit-offset report-info) bit-pos)))

(defun get-buffer-size (report-info)
  (if (= (report-info-bit-offset report-info) 0)
      (report-info-byte-offset report-info)
      (1+ (report-info-byte-offset report-info))))

;;======================================================================
;;
;; Generate a function which parses the mouse interrupt buffer and
;; returns the buttons state, x-motion and y-motion
;;
;;======================================================================

(defun generate-array-index (offset offset-var)
  (if offset-var
      `(+ ,offset-var ,offset)
      offset))

(defun generate-single-button (bit-offset button offset-var)
  (let ((index (generate-array-index
                (report-field-byte-offset button)
                offset-var)))
    (cond ((null button) 0)
          ((or (/= (report-field-num-bits button) 1)
               (/= (report-field-count button) 1))
           (error "Multi-bit button unsuported size/count: ~D/~D"
                  (report-field-num-bits button)
                  (report-field-count button)))
          ((= (report-field-bit-offset button) bit-offset)
           `(logand (aref buf ,index)
                    ,(ash 1 bit-offset)))
          (T
           `(ash (logand (aref buf ,index)
                         ,(ash 1 (report-field-bit-offset button)))
                 ,(- bit-offset (report-field-bit-offset button)))))))

(defun generate-button-code (report offset-var)
  ;; generate code to extract up to the first three buttons
  (let ((button1 (find-report-field report :button1))
        (button2 (find-report-field report :button2))
        (button3 (find-report-field report :button3)))
    (when (or (and button1 (not(= (report-field-num-bits button1)
                                  (report-field-count button1)
                                  1)))
              (and button2 (not(= (report-field-num-bits button2)
                                  (report-field-count button2)
                                  1)))
              (and button3 (not(= (report-field-num-bits button3)
                                  (report-field-count button3)
                                  1))))
      (error "Multi-bit button unsupported"))
    (cond ((and button1 button2 button3  ;; 3 button mouse
                ;; all in the same byte
                (= (report-field-byte-offset button1)
                   (report-field-byte-offset button2)
                   (report-field-byte-offset button3))
                ;; bits are sequential
                (= (+ (report-field-bit-offset button1) 2)
                   (+ (report-field-bit-offset button2) 1)
                   (report-field-bit-offset button3)))
           ;; optimum cases - 3 sequental bits
           (let ((index (generate-array-index
                         (report-field-byte-offset button1)
                         offset-var)))
             (if (= (report-field-bit-offset button1) 0)
                 `(logand (aref buf ,index) #x07)
                 `(ldb (byte 3 ,(report-field-bit-offset  button1))
                       (aref buf ,index)))))

          ((and button1 button2 (null button3)  ;; 2 button mouse
                ;; all in the same byte
                (= (report-field-byte-offset button1)
                   (report-field-byte-offset button2))
                ;; all single bit
                (= (caddr button1) (caddr button2) 1)
                ;; bits are sequential
                (= (+ (report-field-bit-offset button1) 1)
                   (report-field-bit-offset button2)))
           ;; optimum cases - 2 sequential bits
           (let ((index (generate-array-index
                         (report-field-byte-offset button1)
                         offset-var)))
             (if (= (report-field-bit-offset button1) 0)
                 `(logand (aref buf ,index) #x03)
                 `(ldb (byte 2 ,(report-field-bit-offset  button1))
                       (aref buf ,index)))))

          ((and button1 (null button2) (null button3)  ;; 1 button mouse
                ;; single bit
                (= (caddr button1) 1))
           (let ((index (generate-array-index
                         (report-field-byte-offset button1)
                         offset-var)))
             ;; optimum cases
             (if (= (report-field-bit-offset button1) 0)
                 `(logand (aref buf ,index) #x01)
                 `(ldb (byte 1 ,(report-field-bit-offset  button1))
                       (aref buf ,index)))))

          (T    ;; treat each button individually
           `(logior ,(generate-single-button 0 button1 offset-var)
                    ,(generate-single-button 1 button2 offset-var)
                    ,(generate-single-button 2 button3 offset-var))))))

(defun generate-get-bits-code (byte-offset bit-offset bits count offset-var)
  ;; TODO need to handle count
  (when (/= count 1)
    (sup:debug-print-line "HID Probe failed because "
                          "count is " count " not equal to 1.")
    (throw :probe-failed nil))
  (let ((index (generate-array-index byte-offset offset-var))
        (1+index (generate-array-index (1+ byte-offset) offset-var))
        (2+index (generate-array-index (+ byte-offset 2) offset-var)))
    ;; Generate code to extract a bit field from the buffer
    (cond ((and (= bit-offset 0) (= bits 8))
           `(aref buf ,index))
          ((and (= bit-offset 0) (< bits 8))
           `(logand (aref buf ,index) ,(1- (expt 2 bits))))
          ((<= (+ bit-offset bits) 8)
           `(ldb (byte ,bits ,bit-offset) (aref buf ,index)))
          ((and (= bit-offset 0) (= bits 16))
           `(dpb (aref buf ,1+index) (byte 8 8) (aref buf ,index)))
          ((and (= bit-offset 0) (< bits 16))
           `(dpb (ldb (byte ,(- bits 8) 0) (aref buf ,1+index))
                 (byte ,(- bits 8) 8)
                 (aref buf ,index)))
          ((<= (+ bit-offset bits) 16)
           `(dpb (aref buf ,1+index)
                 (byte ,(- bits (- 8 bit-offset)) ,(- 8 bit-offset))
                 (ldb (byte ,(- 8 bit-offset) ,bit-offset)
                      (aref buf ,index))))
          ((<= bits 16)
           `(dpb (aref buf ,2+index)
                 (byte ,(- bits (+ 8 (- 8 bit-offset))) ,(+ 8 (- 8 bit-offset)))
                 (dpb (aref buf ,1+index)
                      (byte 8 ,(- 8 bit-offset))
                      (ldb (byte ,(- 8 bit-offset) ,bit-offset)
                           (aref buf ,index)))))
          (T
           (sup:debug-print-line "HID Probe failed because "
                                 "field with size "
                                 bits
                                 " not supported.")
           (throw :probe-failed nil)))))

(defun signed-field-p (bits min max)
  ;; if msb of min is 1 and msb of max is 0, assume field is signed.
  (and (logbitp (1- bits) min) (not (logbitp (1- bits) max))))

(defun generate-field-value-code
    (report field-name offset-var &optional (required-p T))
  ;; generate code to return the value of a field
  (let ((field-format (find-report-field report field-name)))
    (cond ((null field-format)
           (when required-p
             (format sys.int::*cold-stream*
                     "HID Probe failed because field ~A undefined in report."
                     field-name)
             (throw :probe-failed nil))
           0)
          (T
           (let* ((byte-offset (report-field-byte-offset field-format))
                  (bit-offset (report-field-bit-offset field-format))
                  (num-bits (report-field-num-bits field-format))
                  (count (report-field-count field-format))
                  (min (report-field-minimum field-format))
                  (max (report-field-maximum field-format))
                  (value (generate-get-bits-code
                          byte-offset bit-offset num-bits count offset-var))
                  (sym (gensym "X-")))
             (cond ((signed-field-p num-bits min max)
                    `(let ((,sym ,value))
                       (if (logbitp ,(1- num-bits) ,sym)
                           (- (1+ (logxor ,(1- (expt 2 num-bits)) ,sym)))
                           ,sym)))
                   (T value)))))))

(defun input-reports-buf-size (input-reports)
  (let ((sum 0))
    (dolist (report-plist input-reports)
      (incf sum (getf report-plist :buf-size)))
    ;; buffer should be large enough to receive all of the reports at
    ;; once but might as well allow up to 64 bytes if the sum is less
    ;; than that. A larger buffer shouldn't hurt ... August 9, 2019
    (max sum 64)))

(defun generate-mouse-case-code (input-reports offset-var)
  (let ((results NIL)     ; list of "case" entries
        (mouse-case NIL))
    (dolist (report-plist input-reports)
      (let ((type (second (getf report-plist :report)))
            (report-id (first (getf report-plist :report)))
            (buf-size (getf report-plist :buf-size))
            (fields (getf report-plist :fields)))
        (cond ((eq type :mouse)
               (when mouse-case
                 (sup:debug-print-line "HID probe failed because "
                                       "there are multiple mouse reports")
                 (throw :probe-failed nil))
               (setf mouse-case
                     `(,report-id
                       (mouse-event
                        ,(generate-button-code fields offset-var)
                        ,(generate-field-value-code fields :x offset-var)
                        ,(generate-field-value-code fields :y offset-var)
                        ,(generate-field-value-code
                          fields :wheel offset-var nil))
                       (incf ,offset-var ,buf-size))))
              (T
               (push `(,report-id
                       (incf ,offset-var ,buf-size)) results)))))
    (when (null mouse-case)
      (sup:debug-print-line "HID probe failed because "
                            "there is no mouse report")
      (throw :probe-failed nil))
    ;; make mouse case the first case
    (cons mouse-case results)))

(defun generate-mouse-buf-code (state)
  ;; Use the parsed report descriptor to generate a function that
  ;; extracts the buttons state, x motion and y motion for a mouse
  ;; from a buffer. The function returns (values button-state x-motion
  ;; y-motion).
  (cond ((report-info-format (parse-info-input state))
         ;; simple case - single report with NO report ID

         (let* ((fields (report-info-format (parse-info-input state)))
                (buf-size (get-buffer-size (parse-info-input state)))
                (result `(lambda (length buf)
                           (declare (ignore length))
                           (mouse-event
                            ,(generate-button-code fields nil)
                            ,(generate-field-value-code fields :x nil)
                            ,(generate-field-value-code fields :y nil)
                            ,(generate-field-value-code
                              fields :wheel nil nil)))))
           (with-trace-level (3)
             (format *trace-stream* "~A~%" result))
           (values buf-size (compile nil result))))
        ((= (length (parse-info-input-reports state)) 1)
         ;; next most simple case - single report with report ID
         (let* ((report-plist (car (parse-info-input-reports state)))
                (type (second (getf report-plist :report)))
                (buf-size (getf report-plist :buf-size))
                (fields (getf report-plist :fields)))

           (when (not (eq type :mouse))
             (format
              sys.int::*cold-stream*
              "HID Probe failed because report type is ~A instead of :mouse."
              type)
             (throw :probe-failed nil))

           (let ((result `(function (lambda (length buf)
                           (declare (ignore length))
                            (mouse-event
                             ,(generate-button-code fields nil)
                             ,(generate-field-value-code fields :x nil)
                             ,(generate-field-value-code fields :y nil)
                             ,(generate-field-value-code
                               fields :wheel nil nil))))))
             (with-trace-level (3)
               (format *trace-stream* "~A~%" result))
             (values buf-size (eval result)))))
        (T ;; complex case - multiple reports with report IDs
         (let* ((input-reports (parse-info-input-reports state))
                (buf-size (input-reports-buf-size input-reports))
                (result `(function (lambda (length buf)
                           (do ((offset 0))
                               ((>= offset length))
                             (case (aref buf offset)
                               ,@(generate-mouse-case-code input-reports
                                                           'offset)
                               ))))))
           (with-trace-level (3)
             (format *trace-stream* "~A~%" result))
           (values buf-size (eval result))))))

(defun generate-keyboard-buf-code (state)
  ;; TODO create generate-keyboard-buf-code
  (values nil nil))

;;======================================================================
;; Code that parses HID reports
;;======================================================================

;;======================================================================
;; Pages info - implement sparse array as hash table - may change later
;;======================================================================

;; This looks like it might be read by multiple threads, but only written
;; here during initialization and only invariant keys are used.
;; It is safe to leave it unsynchronized with invariant keys.
(defvar *pages* (make-hash-table :test 'equal :enforce-gc-invariant-keys t))

(setf
 (gethash (list 1 #x01) *pages*) :pointer
 (gethash (list 1 #x02) *pages*) :mouse
 (gethash (list 1 #x03) *pages*) :desktop-reserved
 (gethash (list 1 #x04) *pages*) :joystick
 (gethash (list 1 #x05) *pages*) :game-pad
 (gethash (list 1 #x06) *pages*) :keyboard
 (gethash (list 1 #x07) *pages*) :keypad
 (gethash (list 1 #x08) *pages*) :multi-axis-controller
 (gethash (list 1 #x09) *pages*) :tablet-pc

 (gethash (list 1 #x30) *pages*) :x
 (gethash (list 1 #x31) *pages*) :y
 (gethash (list 1 #x32) *pages*) :z
 (gethash (list 1 #x38) *pages*) :wheel
 (gethash (list 1 #x3C) *pages*) :motion-wakeup

 (gethash (list 1 #x48) *pages*) :resolution-multiplier

 (gethash (list 1 #x80) *pages*) :system-control
 (gethash (list 1 #x81) *pages*) :system-power-down
 (gethash (list 1 #x82) *pages*) :system-sleep
 (gethash (list 1 #x83) *pages*) :system-wake-up

 (gethash (list 7 #xE0) *pages*) :left-control
 (gethash (list 7 #xE1) *pages*) :left-shift
 (gethash (list 7 #xE2) *pages*) :left-alt
 (gethash (list 7 #xE3) *pages*) :left-gui
 (gethash (list 7 #xE4) *pages*) :right-control
 (gethash (list 7 #xE5) *pages*) :right-shift
 (gethash (list 7 #xE6) *pages*) :right-alt
 (gethash (list 7 #xE7) *pages*) :right-gui

 (gethash (list 8 #x01) *pages*) :num-lock
 (gethash (list 8 #x02) *pages*) :caps-lock
 (gethash (list 8 #x03) *pages*) :scroll-lock
 (gethash (list 8 #x04) *pages*) :compose
 (gethash (list 8 #x05) *pages*) :kana

 (gethash (list 8 #x3C) *pages*) :usage-mode-indicator
 (gethash (list 8 #x3D) *pages*) :indicator-on
 (gethash (list 8 #x3F) *pages*) :indicator-slow-blink
 (gethash (list 8 #x40) *pages*) :indicator-fast-blink
 (gethash (list 8 #x41) *pages*) :indicator-off
 (gethash (list 8 #x4B) *pages*) :led-generic-indicator

 (gethash (list 9 #x01) *pages*) :button1
 (gethash (list 9 #x02) *pages*) :button2
 (gethash (list 9 #x03) *pages*) :button3
 (gethash (list 9 #x04) *pages*) :button4
 (gethash (list 9 #x05) *pages*) :button5
 (gethash (list 9 #x06) *pages*) :button6
 (gethash (list 9 #x07) *pages*) :button7
 (gethash (list 9 #x08) *pages*) :button8
 (gethash (list 9 #x09) *pages*) :button9
 (gethash (list 9 #x0A) *pages*) :button10
 (gethash (list 9 #x0B) *pages*) :button11
 (gethash (list 9 #x0C) *pages*) :button12
 (gethash (list 9 #x0D) *pages*) :button13
 (gethash (list 9 #x0E) *pages*) :button14
 (gethash (list 9 #x0F) *pages*) :button15
 (gethash (list 9 #x10) *pages*) :button16

 (gethash (list 12 #x001) *pages*) :consumer-control
 (gethash (list 12 #x238) *pages*) :ac-pan
 )

(defun get-page-entry (page item)
  (cond ((>= page #xFF00)
         ;; Vendor specific
         (format nil "V~4,'0X-~4,'0X" page item))
        (T
         (let ((result (gethash (list page item) *pages*)))
           (with-trace-level (9)
             (when (null result)
               (format *trace-stream* "~%Undefined ~D #x~2,'0X~%~%" page item)))
           result))))

;;======================================================================
;;======================================================================

(defun parse-item (state buf offset)
  (let ((header (aref buf offset)))
    (if (= header #b11111110)
        ;; parse long item
        ()
        ;; parse short item
        (let* ((bsize (ldb (byte 2 0) header))
               (size (if (= bsize 3) 4 bsize))
               (type (nth (ldb (byte 2 2) header)
                          '(:main :global :local :reserved)))
               (tag (ldb (byte 4 4) header))
               (data 0))
          (dotimes (i size)
            (setf data (dpb (aref buf (+ offset 1 i))
                            (byte 8 (* 8 i))
                            data)))
          (with-trace-level (5)
            (print-report-item
             *trace-stream* state buf offset header size type data))
          (values type tag data (+ offset size 1))))))

(defun parse-fields (state report-info data)
  (cond ((logbitp 0 data)
         ;; constant field - want to ignore
         (let ((num-bits (* (parse-state-value state :report-size)
                            (parse-state-value state :report-count))))
           (push
            (make-report-field
             :name :constant
             :byte-offset (report-info-byte-offset report-info)
             :bit-offset (report-info-bit-offset report-info)
             :num-bits num-bits
             :count 1
             :minimum NIL
             :maximum NIL
             :values NIL)
            (report-info-format report-info))
           (increment-position report-info num-bits)))
        ((logbitp 1 data)
         ;; variable
         (loop
            with usage-count = (or (parse-state-value state :usage-count)
                                   (parse-state-value state :report-count))
            for idx from
              (1- usage-count) downto 0
            with num-bits = (parse-state-value state :report-size)
            with symbols = (parse-state-value state :usage)
            do (push
                (make-report-field
                 :name (nth idx symbols)
                 :byte-offset (report-info-byte-offset report-info)
                 :bit-offset (report-info-bit-offset report-info)
                 :num-bits num-bits
                 :count 1
                 :minimum (parse-state-value state :logical-minimum)
                 :maximum (parse-state-value state :logical-maximum)
                 :values NIL)
                (report-info-format report-info))
              (increment-position report-info num-bits)
            finally
              (when (and (parse-state-value state :usage-count)
                         (/= (parse-state-value state :usage-count)
                             (parse-state-value state :report-count)))
                (increment-position report-info
                                    (- (parse-state-value state :report-count)
                                       (parse-state-value state :usage-count))))

              (setf (parse-state-value state :usage)
                    (nthcdr usage-count symbols)
                    (parse-state-value state :usage-count)
                    NIL)))
        ((not (logbitp 1 data))
         ;; Array number of used entries = max - min + 1
         (let* ((num-bits (parse-state-value state :report-size))
                (count (parse-state-value state :report-count))
                (num-entries
                 (1+ (- (parse-state-value state :logical-maximum)
                        (parse-state-value state :logical-minimum))))
                (symbols (last (reverse (parse-state-value state :usage))
                               num-entries)))
           (push
            (make-report-field
             :name (nth num-entries (parse-state-value state :usage))
             :byte-offset (report-info-byte-offset report-info)
             :bit-offset (report-info-bit-offset report-info)
             :num-bits num-bits
             :count count
             :minimum (parse-state-value state :logical-minimum)
             :maximum (parse-state-value state :logical-maximum)
             :values symbols)
            (report-info-format report-info))
           (setf (parse-state-value state :usage)
                 (nthcdr (1+ num-entries)
                         (parse-state-value state :usage))
                 (parse-state-value state :usage-count)
                 NIL)
           (increment-position report-info (* num-bits count))))
        ))

(defun parse-main (state tag data)
  (case tag
    (#x08 ; Input
     (parse-fields state (parse-info-input state) data))
    (#x09 ; Output
     (parse-fields state (parse-info-output state) data))
    (#x0A ; Collection
     (case data
       (#x00
        (push :physical (parse-state-value state :collection)))
       (#x01
        (push :application (parse-state-value state :collection)))
       ))
    (#x0B ; Feature
     (parse-fields state (parse-info-feature state) data))
    (#x0C ;; End Collection
     (pop (parse-state-value state :collection))
     (pop (parse-state-value state :usage)))
    ))

(defun end-of-report-id (state)
  (let ((report-info (parse-info-input state)))
    (when (report-info-format report-info)
      ;; It was an input report
      (push (list :report (parse-state-value state :report-id)
                  :buf-size (get-buffer-size report-info)
                  :fields (report-info-format report-info))
            (parse-info-input-reports state))))
  (setf (parse-info-input state)
        (make-report-info :byte-offset 0
                          :bit-offset 0
                          :format nil))

  (let ((report-info(parse-info-output state)))
    (when (report-info-format report-info)
      ;; It was an output report
      (push (list :report (parse-state-value state :report-id)
                  :buf-size (get-buffer-size report-info)
                  :fields (report-info-format report-info))
            (parse-info-output-reports state))))
  (setf (parse-info-output state)
        (make-report-info :byte-offset 0
                          :bit-offset 0
                          :format nil))

  (let ((report-info (parse-info-feature state)))
    (when (report-info-format report-info)
      ;; It was an feature report
      (push (list :report (parse-state-value state :report-id)
                  :buf-size (get-buffer-size report-info)
                  :fields (report-info-format report-info))
            (parse-info-feature-reports state))))
  (setf (parse-info-feature state)
        (make-report-info :byte-offset 0
                          :bit-offset 0
                          :format nil)))

(defun parse-global (state tag data)
  (case tag
    (#x00 ; usage page
     (setf (parse-state-value state :page) data))
    (#x01
     (setf (parse-state-value state :logical-minimum) data))
    (#x02
     (setf (parse-state-value state :logical-maximum) data))
    (#x07
     (setf (parse-state-value state :report-size) data))
    (#x08
     (when (parse-state-value state :report-id)
       ;; start of new report - end of previous report
       (end-of-report-id state))
     (setf (parse-state-value state :report-id)
           (list data (car (parse-state-value state :usage)))
           (report-info-byte-offset (parse-info-input state)) 1
           (report-info-byte-offset (parse-info-output state)) 1
           (report-info-byte-offset (parse-info-feature state)) 1))
    (#x09
     (setf (parse-state-value state :report-count) data))
    ))

(defun parse-local (state tag data)
  (case tag
    (#x00 ; usage
     (push (get-page-entry (parse-state-value state :page) data)
           (parse-state-value state :usage)))
    (#x01
     (let ((max (parse-state-value state :usage-maximum)))
       (cond (max
              (remf  (parse-info-state state) :usage-maximum)
              (loop for idx from data to max
                 do (push
                     (get-page-entry (parse-state-value state :page) idx)
                     (parse-state-value state :usage)))
              (setf (parse-state-value state :usage-count)
                    (1+ (- max data))))
             (T
              (setf (parse-state-value state :usage-minimum) data)))))
    (#x02
     (let ((min (parse-state-value state :usage-minimum)))
       (cond (min
              (remf  (parse-info-state state) :usage-minimum)
              (loop for idx from min to data
                 do (push
                     (get-page-entry (parse-state-value state :page) idx)
                     (parse-state-value state :usage)))
              (setf (parse-state-value state :usage-count)
                    (1+ (- data min))))
             (T
              (setf (parse-state-value state :usage-maximum) data)))))
    ))

(defun parse-report-finish (state)
  (when (parse-state-value state :report-id)
    ;;end of report descriptor - end of previous report
    (end-of-report-id state))

  (with-trace-level (6)
    (format *trace-stream* "~A~%~%" state)))

(defun %parse-report-descriptor (buf size)
  ;; Returns number of bytes and list of report fields for input,
  ;; output and features.
  (do ((offset 0)
       (state (make-parse-info :input (make-report-info :byte-offset 0
                                                        :bit-offset 0
                                                        :format nil)
                               :output (make-report-info :byte-offset 0
                                                         :bit-offset 0
                                                         :format nil)
                               :feature (make-report-info :byte-offset 0
                                                          :bit-offset 0
                                                          :format nil)
                               :input-reports nil
                               :output-reports nil
                               :feature-reports nil
                               :state NIL)))
      ((>= offset size) (parse-report-finish state) state)
    (multiple-value-bind (type tag data new-offset)
        (parse-item state buf offset)
      (setf offset new-offset)
      (case type
        (:main
         (parse-main state tag data))
        (:global
         (parse-global state tag data))
        (:local
         (parse-local state tag data))
        )
      (with-trace-level (6)
        (format *trace-stream* "~A~%~%" state))
      )))

(defun parse-report-descriptor (usbd device iface-num size)
  (sup:debug-print-line "iface num " iface-num)
  (with-buffers ((buf-pool usbd) (report-buf /8 size))
    (let ((num-bytes (control-receive-data usbd
                        device
                        (encode-request-type  +rt-dir-device-to-host+
                                              +rt-type-standard+
                                              +rt-rec-interface+)
                        +dev-req-get-descriptor+
                        (dpb +desc-type-report+
                             (byte 8 8)
                             0)
                        iface-num
                        size
                        report-buf)))
      (when (/= num-bytes size)
        ;; Unable to get report descriptor
        (sup:debug-print-line "HID Probe failed because "
                              "unable to get report descriptor, only got "
                            num-bytes
                            " bytes instead of "
                            size
                            ".")
        (throw :probe-failed nil)))

    (with-trace-level (3)
      (format sys.int::*cold-stream* "report descriptor: ~%")
      (print-buffer sys.int::*cold-stream* report-buf :indent "    "))
    (%parse-report-descriptor report-buf size)))
