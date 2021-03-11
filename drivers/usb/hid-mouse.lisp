;;;; Copyright (c) 2019, 2020, 2021 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

(in-package :mezzano.driver.usb.hid)

;;======================================================================
;; Mouse event interrupt handler
;; ======================================================================
(defvar *mouse-ints* NIL) ;; for debug

(defun mouse-int-callback (driver endpoint-num status length buf)
  (unwind-protect
       (cond ((eq status :success)
              (let ((endpoint (aref (hid-driver-endpoints driver) endpoint-num)))
                (funcall (hid-endpt-function endpoint) length buf)))
             (T
              (format sys.int::*cold-stream*
                      "Interrupt error ~A on endpoint number ~D~%"
                      status endpoint-num)
              (with-trace-level (1)
                (format sys.int::*cold-stream*
                        "length: ~D~%buf: ~S~%" length buf))))
    (with-trace-level (7)
      (push (format nil "~D: ~A" length buf) *mouse-ints*))
    (free-buffer buf)))

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

;; submit mouse function name, compositor package not defined when
;; drivers are loaded, generate the symbol at first hid mouse probe
(defvar *submit-mouse* nil)
(defvar *submit-mouse-normalized* nil)

;;======================================================================
;; relative-mouse-event and normalized-mouse-event are optimized for
;; time over space/code commonality. These functions are called from
;; the function generated below based on the mouse report descriptor.
;; ======================================================================
(defun relative-mouse-event (buttons x-motion y-motion wheel-motion)
  (with-trace-level (7)
    (format #+mezzano sys.int::*cold-stream* #-mezzano t
            "relative mouse event: ~D ~D ~D ~D~%"
            buttons x-motion y-motion wheel-motion))
  (cond ((= wheel-motion 0)
         (funcall *submit-mouse* buttons
                  (adjust-motion x-motion) (adjust-motion y-motion)))
        (T
         (if (> wheel-motion 0)
             (funcall *submit-mouse* (logior buttons #b00001000)
                      (adjust-motion x-motion) (adjust-motion y-motion))
             (funcall *submit-mouse* (logior buttons #b00010000)
                      (adjust-motion x-motion) (adjust-motion y-motion)))
         ;; button up event for buttons 4 and 5
         (funcall *submit-mouse* buttons 0 0))))

(defun normalized-mouse-event (buttons x-motion y-motion wheel-motion)
  (with-trace-level (7)
    (format #+mezzano sys.int::*cold-stream* #-mezzano t
            "normalized mouse event: ~D ~D ~D ~D~%"
            buttons x-motion y-motion wheel-motion))
  (cond ((= wheel-motion 0)
         (funcall *submit-mouse-normalized* x-motion y-motion :buttons buttons))
        (T
         ;; The qemu tablet report includes a wheel field, so include
         ;; this code, but tablets with multitouch probably don't use
         ;; a wheel field, and so multitouch is not supported by this
         ;; HID mouse driver
         (if (> wheel-motion 0)
             (funcall *submit-mouse-normalized* x-motion y-motion
                      :buttons (logior buttons #b00001000))
             (funcall *submit-mouse-normalized* x-motion y-motion
                      :buttons (logior buttons #b00010000)))
         (funcall *submit-mouse-normalized* x-motion y-motion
                  :buttons buttons))))

;;======================================================================
;; Use the parsed report descriptor to generate a function that
;; extracts the buttons state, x motion, y motion and wheel motion
;; from a report buffer and calls either realtive-mouse-event or
;; absolute-mouse-event with those values.
;; ======================================================================

;;======================================================================
;; Code to convert parsed report descriptor to list of field
;; descriptions
;; ======================================================================

(defun convert-input-field (field bit-offset button-number)
  (let ((count (getf field :count))
        (size (getf field :size))
        (type (getf field :type))
        (%button-number 0))
    (values
     (cond ((member :constant type)
            ()
            #+nil ;; Are ignore fields needed? The generated code
                  ;; shouldn't depend on them.
            (let ((num-bits (* count size)))
              `((:ignore :number-bits ,num-bits
                         :byte-offset ,(ash bit-offset -3)
                         :bit-offset ,(logand bit-offset #x07)))))
           ((= size 1)
            ;; assume 1-bit fields are buttons
            (let ((usage-count (length (getf field :usage))))
              (cond ((= usage-count count)
                     (loop
                        repeat count
                        for %bit-offset = bit-offset then
                          (incf %bit-offset)
                        for usages = (getf field :usage) then (cdr usages)
                        for usage = (car usages)
                        collect
                          `(,usage :number-bits 1
                                   :type :variable
                                   :byte-offset ,(ash %bit-offset -3)
                                   :bit-offset ,(logand %bit-offset #x07))))
                    (T
                     (loop
                        for button-num from
                          (+ (or (getf field :usage-minimum) 1)
                             button-number) to
                          (+ (or (getf field :usage-maximum) count)
                             button-number)
                        for %bit-offset = bit-offset then
                          (incf %bit-offset)
                        do
                          (incf %button-number)
                        collect
                          `(,(intern (format nil "BUTTON~D" button-num)
                                     :keyword)
                             :number-bits 1
                             :type :variable
                             :byte-offset ,(ash %bit-offset -3)
                             :bit-offset ,(logand %bit-offset #x07)))))))
           ((member :array type)
            ;; assume array fields are buttons
            (let ((usage-count (length (getf field :usage))))
              (cond ((= usage-count count)
                     (loop
                        repeat count
                        for %bit-offset = bit-offset then
                          (incf %bit-offset size)
                        for usages = (getf field :usage) then (cdr usages)
                        for usage = (car usages)
                        collect
                          `(,usage :number-bits ,size
                                   :type :array
                                   :byte-offset ,(ash %bit-offset -3)
                                   :bit-offset ,(logand %bit-offset #x07))))
                    (T
                     (loop
                        for button-num from (1+ button-number) to
                          (+ count button-number)
                        for %bit-offset = bit-offset then
                          (incf %bit-offset size)
                        do
                          (incf %button-number)
                        collect
                          `(,(intern (format nil "BUTTON~D" button-num)
                                     :keyword)
                             :number-bits ,size
                             :type :array
                             :byte-offset ,(ash %bit-offset -3)
                             :bit-offset ,(logand %bit-offset #x07)
                             ,@(if (getf field :usage)
                                   (list :usage (getf field :usage))
                                   (list :min (getf field :logical-minimum)
                                         :max (getf field :logical-maximum)))))))))
           (T
            (loop
               repeat count
               for usages = (getf field :usage) then (cdr usages)
               for usage = (car usages)
               for %bit-offset = bit-offset then (incf %bit-offset size)
               collect `(,usage
                         :number-bits ,size
                         :type :variable
                         :min ,(getf field :logical-minimum)
                         :max ,(getf field :logical-maximum)
                         :relative ,(null (member :absolute type))
                         :byte-offset ,(ash %bit-offset -3)
                         :bit-offset ,(logand %bit-offset #x07)))))
     (incf bit-offset (* count size))
     (+ button-number %button-number))))

(defun convert-collection (collection bit-offset button-number)
  (loop
     for (key value) on collection by #'cddr
     with application-id = nil
     with report-id = nil
     with fields = (list nil)
     do
       (case key
         (:application
          (setf application-id value))
         (:report-id
          (when (/= bit-offset 0)
            #+mezzano
            (sup:debug-print-line
             "HID mouse probe failed because report id came after first field")
            ;; TODO - print error message
            (throw :probe-failed :failed))
          (setf report-id value
                bit-offset 8))
         (:input
          (multiple-value-bind (field %bit-offset %button-number)
              (convert-input-field value bit-offset button-number)
            (setf bit-offset %bit-offset)
            (setf button-number %button-number)
            (nconc fields field)))
         (:feature
          (let ((num-bits (* (getf value :count) (getf value :size))))
            #+nil ;; Are ignore fields needed? The generated code
                  ;; shouldn't depend on them
            (nconc fields `((:ignore :number-bits ,num-bits
                                     :byte-offset ,(ash bit-offset -3)
                                     :bit-offset ,(logand bit-offset #x07))))
            (incf bit-offset num-bits)))
         (:collection
          (multiple-value-bind (%report-id %application-id %fields %bit-offset)
              (convert-collection value bit-offset button-number)
            (when %report-id
              (setf report-id %report-id))
            (when %application-id
              (setf application-id %application-id))
            (nconc fields %fields)
            (setf bit-offset %bit-offset))))
     finally
       (return (values report-id application-id (cdr fields) bit-offset))))

;;======================================================================
;; Code to convert list(s) of field descriptions to a function that
;; extracts the required info and calls either relative-mouse-event or
;; absolute-mouse-event
;; ======================================================================

(defun fields-absolute-p (fields)
  (let* ((field-x (cdr (assoc :x fields)))
         (field-x-relative (getf field-x :relative))
         (field-y (cdr (assoc :y fields)))
         (field-y-relative (getf field-y :relative)))
    (cond ((and field-x-relative field-y-relative)
           ;; common case - both x and y relative
           NIL)
          ((null field-x)
           #+mezzano
           (sup:debug-print-line "HID mouse probe failed because no :X field")
           (throw :probe-failed :failed))
          ((null field-y)
           #+mezzano
           (sup:debug-print-line "HID mouse probe failed because no :Y field")
           (throw :probe-failed :failed))
          ((and (null field-x-relative) (null field-y-relative))
           ;; both fields absolute
           T)
          (T
           (format #+mezzano sys.int::*cold-stream* #-mezzano t
                   "HID mouse probe failed because :X relative ~A does not ~
                    match :Y relative ~A~%"
                   field-x-relative field-y-relative)
           (throw :probe-failed :failed)))))

(defun generate-button-code (fields offset-var)
  (labels ((buf-offset (offset)
             (if offset-var `(+ ,offset ,offset-var) offset))
           (optimum-case (button mask)
             (if (= (getf button :bit-offset) 0)
                 `(logand
                   (aref buf ,(buf-offset (getf button :byte-offset)))
                   ,mask)
                 `(logand
                   (ash (aref buf ,(buf-offset (getf button :byte-offset)))
                        ,(- (getf button :bit-offset)))
                   ,mask)))
           (get-button (button offset)
             (when button
               `((logand (ash (aref buf ,(buf-offset (getf button :byte-offset)))
                              ,(- offset (getf button :bit-offset)))
                         ,(ash 1 offset))))))
    (let ((button1 (cdr (assoc :button1 fields)))
          (button2 (cdr (assoc :button2 fields)))
          (button3 (cdr (assoc :button3 fields))))
      (when (or (and button1 (not (eq (getf button1 :number-bits) 1)))
                (and button2 (not (eq (getf button1 :number-bits) 1)))
                (and button3 (not (eq (getf button1 :number-bits) 1))))
        #+mezzano
        (sup:debug-print-line
         "HID mouse probe failed, multibit buttons not supported")
        (throw :probe-failed :failed))
      (cond (;; three button mouse
             (and button1 button2 button3
                  ;; all in the same byte
                  (= (getf button1 :byte-offset)
                     (getf button2 :byte-offset)
                     (getf button3 :byte-offset))
                  ;; bits bits are sequential
                  (= (+ (getf button1 :bit-offset) 2)
                     (+ (getf button2 :bit-offset) 1)
                     (getf button3 :bit-offset)))
             ;; optimum case - 3 sequentail bits
                  (optimum-case button1 #x07))
            (;; two button mouse
             (and button1 button2 (null button3)
                  ;; all in the same byte
                  (= (getf button1 :byte-offset)
                     (getf button2 :byte-offset))
                  ;; bits bits are sequential
                  (= (+ (getf button1 :bit-offset) 1)
                     (getf button2 :bit-offset)))
             ;; optimum case - 2 sequentail bits
             (optimum-case button1 #x03))
            (;; one button mouse
             (and button1 (null button2) (null button3))
             (optimum-case button1 #x01))
            (;; treat each button separately
             T
             `(logior ,@(get-button button1 0)
                      ,@(get-button button2 1)
                      ,@(get-button button3 2)))))))

(defun generate-field-value-code (fields name offset-var required-p)
  (labels ((buf-offset (offset)
             (if offset-var `(+ ,offset ,offset-var) offset))
           (get-partial-byte (byte-offset bit-offset size)
             `(logand (ash (aref buf ,(buf-offset byte-offset)) ,(- bit-offset))
                      ,(1- (ash 1 size))))
           (accessor-code (fields)
             (let ((byte-offset (getf fields :byte-offset))
                   (bit-offset (getf fields :bit-offset))
                   (size (getf fields :number-bits)))
               (when (not (and byte-offset bit-offset size))
                 (format #+mezzano sys.int::*cold-stream* #-mezzano t
                         "HID mouse probe failed because ~
                        a required field is undefined: ~A ~A ~A.~%"
                         byte-offset bit-offset size)
                 (throw :probe-failed :failed))
               (cond ((and (= bit-offset 0) (= size 8))
                      ;; just use the array value
                      `(aref buf ,(buf-offset byte-offset)))
                     ((and (= bit-offset 0) (< size 8))
                      ;; just mask off value
                      `(logand (aref buf ,(buf-offset byte-offset))
                                     ,(1- (ash 1 size))))
                     ((<= (+ bit-offset size) 8)
                      ;; Shift and mask off value
                      (get-partial-byte byte-offset bit-offset size))
                     ((and (= bit-offset 0) (= size 16))
                      ;; combine two full bytes
                      `(logior
                        (aref buf ,(buf-offset byte-offset))
                        (ash (aref buf ,(buf-offset (1+ byte-offset))) 8)))
                     ((and (= bit-offset 0) (< size 16))
                      `(logior
                        (aref buf ,(buf-offset byte-offset))
                        (ash (logand (aref buf ,(buf-offset (1+ byte-offset)))
                                     ,(1- (ash 1 (- size 8))))
                             8)))
                     ((<= (+ bit-offset size) 16)
                      ;; combine two partial bytes
                      (let* ((first-bits (- 8 bit-offset))
                             (second-bits (- size first-bits)))
                        `(logior
                          ,(get-partial-byte byte-offset bit-offset first-bits)
                          (ash ,(if (= second-bits 8)
                                    `(aref buf ,(buf-offset (1+ byte-offset)))
                                    `(logand
                                      (aref buf ,(buf-offset (1+ byte-offset)))
                                      ,(1- (ash 1 second-bits))))
                               ,first-bits))))
                     ((<= size 16)
                      ;; combine two paritial bytes and one full byte
                      (let* ((first-bits (- 8 bit-offset))
                             (last-shift (+ first-bits 8))
                             (last-bits (- size last-shift)))
                        `(logior
                          ,(get-partial-byte byte-offset bit-offset first-bits)
                          (ash (aref buf ,(buf-offset (1+ byte-offset)))
                               ,first-bits)
                          (ash (logand (aref buf ,(buf-offset (+ 2 byte-offset)))
                                       ,(1- (ash 1 last-bits)))
                               ,last-shift))))
                     (T
                      (format #+mezzano sys.int::*cold-stream* #-mezzano t
                              "HID mouse probe failed because ~
                             field ~A with size ~D bits not supported."
                              name size)
                      (throw :probe-failed :failed))))))
    (let ((fields (cdr (assoc name fields))))
      (cond ((null fields)
             (when required-p
               (format #+mezzano sys.int::*cold-stream* #-mezzano t
                       "HID mouse probe failed because field ~S is undefined.~%"
                       name)
               (throw :probe-failed :failed))
             0)
            (T
             (let ((field-value (accessor-code fields))
                   (bits (getf fields :number-bits))
                   (min (getf fields :min))
                   (max (getf fields :max))
                   (sym (gensym "X-")))
               ;; if msb of min is 1 and msb of max is 0, assume field
               ;; is signed.
               (if (and (logbitp (1- bits) min) (not (logbitp (1- bits) max)))
                   `(let ((,sym ,field-value))
                      (if (logbitp ,(1- bits) ,sym)
                          (- (1+ (logxor ,(1- (ash 1 bits)) ,sym)))
                          ,sym))
                   ;; usigned - assume that means absolute, so normalize
                   `(/ ,field-value ,max))))))))

(defun generate-mouse-case-clauses (reports)
  (let* ((num-mouse-reports 0)
         (clauses
          (loop
             for report in reports
             for fields = (caddr report)
             collect
               `(,(car report)
                  ,@(when (eq (cadr report) :mouse)
                      (incf num-mouse-reports)
                      `((,(if (fields-absolute-p fields)
                              'normalized-mouse-event
                              'relative-mouse-event)
                          ,(generate-button-code fields 'offset)
                          ,(generate-field-value-code fields :x 'offset T)
                          ,(generate-field-value-code fields :y 'offset T)
                          ,(generate-field-value-code fields :wheel 'offset NIL))))
                  (incf offset ,(/ (cadddr report) 8))))))
    (when (/= num-mouse-reports 1)
      (format #+mezzano sys.int::*cold-stream* #-mezzano t
              "HID mouse probe failed because ~
               report descritor contained ~D mouse reports~%"
              num-mouse-reports)
      (throw :probe-failed :failed))
    clauses))

(defun generate-mouse-buf-function (reports)
  (let ((case-clauses (generate-mouse-case-clauses reports)))
    `(lambda (length buf)
       (loop
          with offset = 0
          when (>= offset length) do
            (return)
          do
            ,@(if (= (length case-clauses) 1)
                  (cdar case-clauses)
                  `((case (aref buf offset)
                      ,@case-clauses)))))))

(defun compute-buf-size (reports)
  (let ((size (/ (loop for report in reports sum (cadddr report)) 8)))
    (if (> size 64)
        size
        (* (1+ (floor 64 size)) size))))

(defun generate-mouse-buf-code (descriptors)
  ;; Use the parsed report descriptor(s) to generate a function that
  ;; extracts the buttons state, x motion, y motion and wheel motion
  ;; from a report buffer and calls relative-mouse-event or
  ;; normalized-mouse-event with those values.
  (let ((reports (mapcar
                  #'(lambda (collection)
                      (multiple-value-list (convert-collection
                                            collection 0 0 0)))
                  descriptors)))
    (with-trace-level (1)
      (format *trace-stream* "reports:~%~S~%~%" reports))
    (let ((func (generate-mouse-buf-function reports)))
      (with-trace-level (1)
        (format *trace-stream* "function:~%~A~%~%" func))
      (compile nil func))))

;;======================================================================
;; HID Mouse Driver
;;======================================================================
(defun probe-hid-mouse (usbd device configs descriptors)
  (setf *submit-mouse*
        (intern "SUBMIT-MOUSE" :mezzano.gui.compositor)
        *submit-mouse-normalized*
        (intern "SUBMIT-MOUSE-NORMALIZED" :mezzano.gui.compositor))
  (let ((endpoint (make-hid-endpt :type :mouse)))
    (setf (hid-endpt-parse-state endpoint) descriptors
          (hid-endpt-function endpoint) (generate-mouse-buf-code descriptors))
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
        ;; if a mouse report runs over the end of the buffer.
        (setf (hid-endpt-buf-size endpoint)
              (* (floor 64 max-packet) max-packet))
        (with-trace-level (1)
          (format *trace-stream* "buffer size: ~D~%"
                  (hid-endpt-buf-size endpoint))))
      (let* ((driver (make-hid-driver
                      :usbd usbd
                      :device device
                      :endpoints (make-array 32 :initial-element NIL)))
             (endpt-num
              (parse-endpt-descriptor
               usbd driver endpoint device endpt-desc 'mouse-int-callback)))
        (setf (aref (hid-driver-endpoints driver) endpt-num) endpoint)
        (values configs driver)))))

(register-hid-device-function :mouse 'probe-hid-mouse)
