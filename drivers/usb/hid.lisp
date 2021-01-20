;;;; Copyright (c) 2019, 2020, 2021 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

;;======================================================================
;;
;; HID (Human Interface Device) Class Driver
;;
;;  The HID class driver searchs for a HID driver for each interface
;;  the HID device provides. It does this by reading and parsing the
;;  report descriptors to see if there are any HID drivers registered
;;  for the reports.
;;
;;======================================================================

(defpackage :mezzano.driver.usb.hid
  (:use :cl :mezzano.driver.usb)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sys.int :mezzano.internals)))

(in-package :mezzano.driver.usb.hid)

(defvar *trace-stream* sys.int::*cold-stream*)
(defvar *trace* 0)

(defmacro with-trace-level ((trace-level) &body body)
  `(when (>= *trace* ,trace-level)
     ,@body))

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
  (let* ((endpt-num (ldb +ed-endpt-num-field+ (aref endpt-desc +ed-address+))))
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
;; Human Interface Device (HID) report parser
;;
;; This code parses USB HID report descriptors which describe the HID
;; including the type of device and a description of the messages the
;; device sends and receives. The report descriptor is defined in
;; setion 6.2.2 of USB Device Class Definition for Human Interface
;; Devices version 1.11 6/27/2001
;; ======================================================================

;;======================================================================
;; Pages info
;;
;; Implement a sparse 2^32 entry array as hash table, this
;; implmentation may change later without affecting the parser.
;;
;; The table values come from the document USB HID Usage Tables
;; version 1.12 10/28/2004
;;
;; This is a read-only table, it is only written here during
;; initialized here and the keys are a list of two integers between 0
;; and #x3FFF (which combined would be the index into the 2^32 entry
;; array). Even though this table may be accessed by multiple threads,
;; because it is read-only and the keys are invariant, it is safe to
;; leave it unsynchronized.
;;======================================================================
(defvar *pages* (make-hash-table
                 :test 'equal
                 #+mezzano :enforce-gc-invariant-keys #+mezzano t))

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
 (gethash (list 12 #x0B0) *pages*) :play
 (gethash (list 12 #x0B1) *pages*) :pause
 (gethash (list 12 #x0B2) *pages*) :record
 (gethash (list 12 #x0B3) *pages*) :fast-forward
 (gethash (list 12 #x0B4) *pages*) :rewind
 (gethash (list 12 #x0B5) *pages*) :scan-next-track
 (gethash (list 12 #x0B6) *pages*) :scan-previous-track
 (gethash (list 12 #x0B7) *pages*) :stop
 (gethash (list 12 #x0B8) *pages*) :eject
 (gethash (list 12 #x0CD) *pages*) :pause-play
 (gethash (list 12 #x0E2) *pages*) :mute
 (gethash (list 12 #x0E9) *pages*) :volume-increment
 (gethash (list 12 #x0EA) *pages*) :volume-decrement
 (gethash (list 12 #x183) *pages*) :app-launch-ctl-config
 (gethash (list 12 #x18A) *pages*) :app-launch-email-reader
 (gethash (list 12 #x192) *pages*) :app-launch-calculator
 (gethash (list 12 #x194) *pages*) :app-launch-machine-browser
 (gethash (list 12 #x221) *pages*) :app-ctl-search
 (gethash (list 12 #x222) *pages*) :app-ctl-go-to
 (gethash (list 12 #x223) *pages*) :app-ctl-home
 (gethash (list 12 #x224) *pages*) :app-ctl-back
 (gethash (list 12 #x225) *pages*) :app-ctl-forward
 (gethash (list 12 #x226) *pages*) :app-ctl-stop
 (gethash (list 12 #x227) *pages*) :app-ctl-refresh
 (gethash (list 12 #x22A) *pages*) :app-ctl-bookmarks
 (gethash (list 12 #x238) *pages*) :app-ctl-pan
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
;; Structures used to parse HID Report descriptor used by HID drivers
;; to decode interrupt buffers (input reports).
;; ======================================================================

(defstruct parse-fields
  (report-count nil)
  (report-size nil)
  (logical-minimum nil)
  (logical-maximum nil)
  (usage-minimum nil)
  (usage-maximum nil)
  (usage-page nil)
  (usage nil))

(defstruct parse-info
  (global-fields (make-parse-fields))
  (local-fields (make-parse-fields))
  collection
  results)

(defun reset-parse-fields (fields)
  (setf (parse-fields-report-count fields) nil
        (parse-fields-report-size fields) nil
        (parse-fields-logical-minimum fields) nil
        (parse-fields-logical-maximum fields) nil
        (parse-fields-usage-minimum fields) nil
        (parse-fields-usage-maximum fields) nil
        (parse-fields-usage-page fields) nil
        (parse-fields-usage fields) nil))

;; Define accessors of the form parse-info-<field name> which gives
;; priority to the local value over the global value
(macrolet
    ((def-parse-info-field (&rest field-names)
       (let ((results '(progn)))
         (dolist (field-name field-names (reverse results))
           (let ((info-name (intern (concatenate 'string
                                                 "PARSE-INFO-"
                                                 (symbol-name field-name))))
                 (accessor-name (intern (concatenate 'string
                                                     "PARSE-FIELDS-"
                                                     (symbol-name field-name)))))
             (push `(defun ,info-name (info)
                      (or (,accessor-name (parse-info-local-fields info))
                          (,accessor-name (parse-info-global-fields info))))
                   results))))))
  (def-parse-info-field report-count report-size logical-minimum logical-maximum
                        usage-minimum usage-maximum usage-page usage))

;;======================================================================
;; Start of parser code
;; ======================================================================

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

(defun parse-main-data (tag data)
  (let ((result nil))
    (push (if (logbitp 8 data) :buffered-bytes :bit-field) result)
    (when (or (= tag #x09) (= tag #x0B))
      (push (if (logbitp 7 data) :volatile :non-volatile) result))
    (when (logbitp 6 data)
      (push :null-state result))
    (when (not (logbitp 5 data))
      (push :preferred-state result))
    (when (logbitp 4 data)
      (push :non-linear result))
    (when (logbitp 3 data)
      (push :wrap result))
    (push (if (logbitp 2 data) :relative :absolute) result)
    (push (if (logbitp 1 data) :variable :array) result)
    (push (if (logbitp 0 data) :constant :data) result)
    result))

(defun parse-main (state tag data)
  (case tag
    ((#x08 #x09 #x0B) ; Input, Output or Feature
     (nconc (car (parse-info-collection state))
            (list (case tag (#x08 :input) (#x09 :output) (#x0B :feature))
                  (list :type (parse-main-data tag data)
                        :count (parse-info-report-count state)
                        :size (parse-info-report-size state)
                        :logical-minimum (parse-info-logical-minimum state)
                        :logical-maximum (parse-info-logical-maximum state)
                        :usage-minimum (parse-info-usage-minimum state)
                        :usage-maximum (parse-info-usage-maximum state)
                        :usage (reverse (parse-info-usage state)))))
     (reset-parse-fields (parse-info-local-fields state)))
    (#x0A ; Collection
     (push (list (case data
                   (#x00 :physical)
                   (#x01 :application)
                   (#x02 :logical)
                   (#x03 :report)
                   (#x04 :named-array)
                   (#x05 :usage-switch)
                   (#x06 :usage-modifier))
                 (car (parse-info-usage state)))
           (parse-info-collection state))
     (setf (parse-fields-usage (parse-info-local-fields state)) nil))
    (#x0C ; End Collection
     (let ((collection (pop (parse-info-collection state))))
       (cond ((null (parse-info-collection state))
              (push collection (parse-info-results state))
              (setf (parse-info-collection state) nil)
              (reset-parse-fields (parse-info-local-fields state))
              (reset-parse-fields (parse-info-global-fields state)))
             (T
              (nconc (car (parse-info-collection state))
                     (list :collection collection))))))))

(defun parse-global (state tag data)
  (let ((fields (parse-info-global-fields state)))
    (case tag
      (#x00
       (setf (parse-fields-usage-page fields) data))
      (#x01
       (setf (parse-fields-logical-minimum fields) data))
      (#x02
       (setf (parse-fields-logical-maximum fields) data))
      (#x07
       (setf (parse-fields-report-size fields) data))
      (#x08
       (setf (getf (car (parse-info-collection state)) :report-id) data))
      (#x09
       (setf (parse-fields-report-count fields) data)))))

(defun parse-local (state tag data)
  (let ((fields (parse-info-local-fields state)))
    (case tag
      (#x00
       (push (get-page-entry (parse-info-usage-page state) data)
             (parse-fields-usage fields)))
      (#x01
       (setf (parse-fields-usage-minimum fields) data))
      (#x02
       (setf (parse-fields-usage-maximum fields) data)))))

(defun %parse-report-descriptor (buf size)
  ;; Returns data structure defining report fields
  (loop
     with offset = 0
     with state = (make-parse-info)
     when (>= offset size) do
       (let ((result (nreverse (parse-info-results state))))
         (with-trace-level (1)
           (format *trace-stream* "report-descriptor:~%~S~%~%" result))
         (return result))
     do
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
        (throw :probe-failed :failed)))

    (with-trace-level (3)
      (format sys.int::*cold-stream* "report descriptor: ~%")
      (print-buffer sys.int::*cold-stream* report-buf :indent "    "))
    (%parse-report-descriptor report-buf size)))

;;======================================================================
;;======================================================================

(defvar *hid-probe-fcns* NIL)

(defun register-hid-device-function (device-type function)
  (setf (getf *hid-probe-fcns* device-type) function))

;;======================================================================
;;
;; Probe HID device, parse the report descriptor, and call appropriate
;; HID device driver based on the application type.
;;
;;======================================================================

(defun probe-hid-device (usbd device iface-desc configs)
  (when (/= (aref iface-desc +id-num-endpoints+) 1)
    (sup:debug-print-line "HID Probe failed because "
                          "interface descriptor has "
                          (aref iface-desc +id-num-endpoints+)
                          " endpoints. Only exactly 1 supported.")
    (throw :probe-failed :failed))

  (let ((iface-num (aref iface-desc +id-number+))
        (hid-desc (pop configs)))
    (when (or (null hid-desc)
              (/= (aref hid-desc +hd-type+) +desc-type-hid+))
      (sup:debug-print-line
       "HID Probe failed because "
       "interface descriptor not followed by HID descriptor.")
      (throw :probe-failed :failed))
    (let ((type (aref hid-desc +hd-descriptor-type+))
          (size (get-unsigned-word/16 hid-desc +hd-descriptor-length+)))
      (when (/= type +desc-type-report+)
        (sup:debug-print-line "HID probe failed because descriptor type "
                              type
                              " not report, the only type supported.")
        (throw :probe-failed :failed))
      (let* ((descriptors (parse-report-descriptor usbd device iface-num size)))
        (loop
           for descriptor in descriptors
           for probe-fcn = (getf *hid-probe-fcns* (getf descriptor :application))
           when probe-fcn do
             (multiple-value-bind (%configs driver)
                 (funcall probe-fcn usbd device configs descriptors)
               (when driver
                 (return-from probe-hid-device (values %configs driver)))))))
    (values configs NIL)))

(define-usb-class-driver "HID" 'probe-hid-device
  '((#.+id-class-hid+ NIL NIL)))
