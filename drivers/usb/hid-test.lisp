(in-package :mezzano.driver.usb.hid)

(defun test-parse ()
  ;; Logitech TrackMan Wheel
  (let ((buf #(#x05 #x01 #x09 #x02 #xA1 #x01 #x05 #x09
               #x19 #x01 #x29 #x03 #x15 #x00 #x25 #x01
               #x95 #x03 #x75 #x01 #x81 #x02 #x95 #x01
               #x75 #x05 #x81 #x03 #x05 #x01 #x09 #x01
               #xA1 #x00 #x09 #x30 #x09 #x31 #x15 #x81
               #x25 #x7F #x75 #x08 #x95 #x02 #x81 #x06
               #xC0 #x09 #x38 #x95 #x01 #x81 #x06 #x09
               #x3C #x15 #x00 #x25 #x01 #x75 #x01 #x95
               #x01 #xB1 #x22 #x95 #x07 #xB1 #x01 #x05
               #x08 #x09 #x4B #x15 #x00 #x25 #x03 #x95
               #x01 #x75 #x02 #x09 #x3C #xA1 #x02 #x09
               #x41 #x09 #x3D #x09 #x3F #x09 #x40 #x91
               #x00 #xC0 #x75 #x06 #x91 #x01 #xC0)))
    (%parse-report-descriptor buf (length buf))))

(defun test-parse2 ()
  ;; Logitech Unifying Receivers - mouse
  (let ((buf #(#x05 #x01 #x09 #x02 #xA1 #x01 #x85 #x02
               #x09 #x01 #xA1 #x00 #x05 #x09 #x19 #x01
               #x29 #x10 #x15 #x00 #x25 #x01 #x95 #x10
               #x75 #x01 #x81 #x02 #x05 #x01 #x16 #x01
               #xF8 #x26 #xFF #x07 #x75 #x0C #x95 #x02
               #x09 #x30 #x09 #x31 #x81 #x06 #x15 #x81
               #x25 #x7F #x75 #x08 #x95 #x01 #x09 #x38
               #x81 #x06 #x05 #x0C #x0A #x38 #x02 #x95
               #x01 #x81 #x06 #xC0 #xC0)))
    (%parse-report-descriptor buf (length buf))))

(defun test-parse3 ()
  ;; Logitech Unifying Receivers - keyboard
  (let ((buf #(#x05 #x01 #x09 #x06 #xA1 #x01 #x85 #x01
	       #x95 #x08 #x75 #x01 #x15 #x00 #x25 #x01
	       #x05 #x07 #x19 #xE0 #x29 #xE7 #x81 #x02
	       #x95 #x06 #x75 #x08 #x15 #x00 #x26 #xFF
               #x00 #x05 #x07 #x19 #x00 #x2A #xFF #x00
	       #x81 #x00 #x85 #x0e #x05 #x08 #x95 #x05
	       #x75 #x01 #x15 #x00 #x25 #x01 #x19 #x01
	       #x29 #x05 #x91 #x02 #x95 #x01 #x75 #x03
	       #x91 #x01 #xC0)))
    (%parse-report-descriptor buf (length buf))))

(defun test-parse4 ()
  ;; Actual Logitech Unifying Receiver  - mouse
  (let ((buf #(#x05 #x01 #x09 #x02 #xA1 #x01 #x85 #x02
               #x09 #x01 #xA1 #x00 #x05 #x09 #x19 #x01
               #x29 #x10 #x15 #x00 #x25 #x01 #x95 #x10
               #x75 #x01 #x81 #x02 #x05 #x01 #x16 #x01
               #xF8 #x26 #xFF #x07 #x75 #x0C #x95 #x02
               #x09 #x30 #x09 #x31 #x81 #x06 #x15 #x81
               #x25 #x7F #x75 #x08 #x95 #x01 #x09 #x38
               #x81 #x06 #x05 #x0C #x0A #x38 #x02 #x95
               #x01 #x81 #x06 #xC0 #xC0 #x05 #x0C #x09
               #x01 #xA1 #x01 #x85 #x03 #x75 #x10 #x95
               #x02 #x15 #x01 #x26 #x8C #x02 #x19 #x01
               #x2A #x8C #x02 #x81 #x00 #xC0 #x05 #x01
               #x09 #x80 #xA1 #x01 #x85 #x04 #x75 #x02
               #x95 #x01 #x15 #x01 #x25 #x03 #x09 #x82
               #x09 #x81 #x09 #x83 #x81 #x60 #x75 #x06
               #x81 #x03 #xC0 #x06 #xBC #xFF #x09 #x88
               #xA1 #x01 #x85 #x08 #x19 #x01 #x29 #xFF
               #x15 #x01 #x26 #xFF #x00 #x75 #x08 #x95
               #x01 #x81 #x00 #xC0)))
    (%parse-report-descriptor buf (length buf))))

(defun test-parse5 ()
  ;; Actual PixArt HP USB Optical Mouse - defines 3 buttons with count = 8
  (let ((buf #(#x05 #x01 #x09 #x02 #xA1 #x01 #x09 #x01
               #xA1 #x00 #x05 #x09 #x19 #x01 #x29 #x03
               #x15 #x00 #x25 #x01 #x95 #x08 #x75 #x01
               #x81 #x02 #x05 #x01 #x09 #x30 #x09 #x31
               #x09 #x38 #x15 #x81 #x25 #x7F #x75 #x08
               #x95 #x03 #x81 #x06 #xC0 #xC0)))
    (%parse-report-descriptor buf (length buf))))

(defun test-mouse-buf ()
  (let ((report (list (make-report-field :name :button1
                                         :byte-offset 0
                                         :bit-offset 0
                                         :num-bits 1
                                         :count 1
                                         :minimum 0
                                         :maximum 1
                                         :values nil)
                      (make-report-field :name :button2
                                         :byte-offset 0
                                         :bit-offset 1
                                         :num-bits 1
                                         :count 1
                                         :minimum 0
                                         :maximum 1
                                         :values nil)
                      (make-report-field :name :button3
                                         :byte-offset 0
                                         :bit-offset 2
                                         :num-bits 1
                                         :count 1
                                         :minimum 0
                                         :maximum 1
                                         :values nil)
                      (make-report-field :name :constant
                                         :byte-offset 0
                                         :bit-offset 3
                                         :num-bits 5
                                         :count 1
                                         :minimum nil
                                         :maximum nil
                                         :values nil)
                      (make-report-field :name :x
                                         :byte-offset 1
                                         :bit-offset 0
                                         :num-bits 8
                                         :count 1
                                         :minimum #x81
                                         :maximum #x7F
                                         :values nil)
                      (make-report-field :name :y
                                         :byte-offset 2
                                         :bit-offset 0
                                         :num-bits 8
                                         :count 1
                                         :minimum #x81
                                         :maximum #x7F
                                         :values nil)
                      (make-report-field :name :wheel
                                         :byte-offset 3
                                         :bit-offset 0
                                         :num-bits 8
                                         :count 1
                                         :minimum #x81
                                         :maximum #x7F
                                         :values nil))))
    (generate-mouse-buf-code report)))

(defun test-mouse-buf2()
  (multiple-value-bind (buf-size report) (test-parse2)
    (declare (ignore buf-size))
    (generate-mouse-buf-code report)))
