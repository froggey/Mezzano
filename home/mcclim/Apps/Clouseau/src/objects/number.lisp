;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; Utilities

(defun prime-factors (integer)
  (declare (optimize speed (debug 1) (safety 1))
           (type (unsigned-byte 32) integer))
  (let ((factors '()))
    (labels ((add-factor (factor)
               (declare (type (unsigned-byte 32) factor))
               (let ((first (first factors)))
                 (if (or (null first) (/= factor (the (unsigned-byte 32)
                                                      (car first))))
                     (push (cons factor 1) factors)
                     (incf (the (unsigned-byte 32) (cdr first))))))
             (try (n d upper-bound)
               (declare (type (unsigned-byte 32) n d upper-bound))
               (if (> d upper-bound)
                   (add-factor n)
                   (multiple-value-bind (quotient remainder) (truncate n d)
                     (cond ((zerop remainder)
                            (add-factor d)
                            (rec quotient))
                           (t
                            (try n (if (evenp d) (1+ d) (+ d 2)) upper-bound))))))
             (rec (n)
               (declare (type (unsigned-byte 32) n))
               (when (>= n 2)
                 (try n 2 (isqrt n)))))
      (rec integer))
    factors))

;;; Object states

(defclass inspected-integer (inspected-object)
  ((%prime-factors-p :initarg  :prime-factors-p
                     :accessor prime-factors-p
                     :initform nil)))

(defmethod object-state-class ((object integer) (place t))
  'inspected-integer)

(defmethod make-object-state ((object integer) (place t))
  (make-instance (object-state-class object place)
                 :place           place
                 :prime-factors-p (< (abs object) (ash 1 32))))

;;; Object inspection methods

(defmethod inspect-object-using-state ((object number)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (let ((class (class-of object))
        (type  (type-of object)))
    (inspect-class-as-name class stream)
    (when (and (not (consp type))
               (not (eql type (class-name class))))
      (write-char #\space stream)
      (with-style (stream :note)
        (princ type stream))))
  (write-char #\Space stream)
  (call-next-method))

(defmethod inspect-object-using-state :after ((object float)
                                              (state  inspected-object)
                                              (style  (eql :badges))
                                              (stream t))
  #+sbcl (let ((has-value-p t))
           (macrolet ((maybe-special-float (predicate label)
                        `(when (,predicate object)
                           (setf has-value-p nil)
                           (write-char #\Space stream)
                           (badge stream ,label))))
             (maybe-special-float sb-ext:float-infinity-p     "infinity")
             (maybe-special-float sb-ext:float-denormalized-p "denormalized")
             (maybe-special-float sb-ext:float-nan-p          "nan")
             (maybe-special-float sb-ext:float-trapping-nan-p "trapping-nan"))))

(defmethod inspect-object-using-state :after ((object number)
                                              (state  inspected-object)
                                              (style  (eql :badges))
                                              (stream t))
  (macrolet
      ((limits ()
         (labels ((constant (constant-name)
                    `((= object ,constant-name)
                      (write-char #\Space stream)
                      (badge stream ,(string-downcase
                                      (symbol-name constant-name)))))
                  (type (type)
                    (let ((lower (let ((*package* (find-package "CL")))
                                   (symbolicate '#:most-negative- type)))
                          (upper (let ((*package* (find-package "CL")))
                                   (symbolicate '#:most-positive- type))))
                      `(,(constant lower)
                        ,(constant upper)))))
           `(cond
              ,@(mappend #'type '(fixnum
                                  single-float
                                  double-float))
              ,(constant 'pi)))))
    (limits)))

(defmethod inspect-object-using-state ((object integer)
                                       (state  inspected-integer)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    ;; Value in different bases.
    (macrolet ((value-in-base (label format-control)
                 `(progn
                    (with-style (stream :slot-like)
                      (formatting-cell (stream) (write-string ,label stream))
                      (formatting-cell (stream) (declare (ignore stream))))
                    (formatting-cell (stream)
                      (format stream ,format-control object)))))
      (formatting-row (stream)
        (value-in-base "Decimal value"     "~:D")
        (value-in-base "Hexadecimal value" "#x~X"))
      (formatting-row (stream)
        (value-in-base "Octal value"       "#o~O")
        (value-in-base "Binary value"      "#b~B")))

    ;; Bit statistics
    (formatting-row (stream)
      (format-place-cells stream object 'reader-place 'integer-length
                          :label "Length (bits)")
      (format-place-cells stream object 'reader-place 'logcount
                          :label "Hamming weight"))

    ;; Prime factors.
    (unless (<= -3 object 3)
      (formatting-row (stream)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Factors" stream))
          (formatting-cell (stream) (declare (ignore stream))))
        (formatting-cell (stream)
          (with-placeholder-if-empty (stream)
            ((not (prime-factors-p state))
             "Cowardly refusing to compute prime factors")
            (t
             ;; Print prime factors with exponents. Avoid offsetting
             ;; the whole line if all exponents are 1.
             (let ((factors (prime-factors (abs object))))
               (flet ((print-factors (&optional sup)
                        (when (minusp object)
                          (write-string "-1 × " stream))
                        (loop for ((factor . exponent) . rest) on factors
                              do (princ factor stream)
                                 (unless (= exponent 1)
                                   (funcall sup (curry #'princ exponent)))
                                 (when rest (write-string " × " stream)))))
                 (if (find 1 factors :test #'/= :key #'cdr)
                     (call-with-superscript #'print-factors stream)
                     (print-factors)))))))))))

(defmethod inspect-object-using-state ((object ratio)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (formatting-table (stream)
    (formatting-row (stream)
      (format-place-cells stream object 'reader-place 'numerator
                          :label "Numerator")
      (format-place-cells stream object 'reader-place 'denominator
                          :label "Denominator"))
    (format-place-row stream object 'pseudo-place (float object 1.0d0)
                      :label "Float")))

(defmethod inspect-object-using-state ((object float)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (let ((radix       (float-radix object))
        (has-value-p #+sbcl (not (or (sb-ext:float-infinity-p object)
                                     (sb-ext:float-denormalized-p object)
                                     (sb-ext:float-nan-p object)
                                     (sb-ext:float-trapping-nan-p object)))
                     #-sbcl t))
    (multiple-value-bind (significand exponent sign)
        (when has-value-p (decode-float object))
      (with-preserved-cursor-x (stream)
        (formatting-table (stream)
          (formatting-row (stream)
            (format-place-cells stream object 'pseudo-place radix
                                :label        "Radix"
                                :object-style (if has-value-p :float-radix nil))
            (format-place-cells stream object 'reader-place 'float-precision
                                :label "Precision")
            (format-place-cells stream object 'reader-place 'float-digits
                                :label "Digits"))

          (when has-value-p
            (formatting-row (stream)
              (format-place-cells stream object 'pseudo-place sign
                                  :label "Sign" :object-style :float-sign)
              (format-place-cells stream object 'pseudo-place significand
                                  :label        "Significand"
                                  :object-style :float-significand)
              (format-place-cells stream object 'pseudo-place exponent
                                  :label        "Exponent"
                                  :object-style :float-exponent)))))
      ;; Value
      (when has-value-p
        (with-superscript (stream sup)
          (format stream "~A = " object)
          (with-style (stream :float-sign)
            (format stream "~F" sign))
          (write-string " × " stream)
          (with-style (stream :float-significand)
            (format stream "~F" significand))
          (write-string " × " stream)
          (with-style (stream :float-radix)
            (format stream "~D" radix))
          (sup (with-style (stream :float-exponent)
                 (format stream "~D" exponent))))))))

(defmethod inspect-object-using-state ((object complex)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (format-place-cells stream object 'pseudo-place (type-of (realpart object))
                            :label "Part type"))

      (formatting-row (stream)
        (format-place-cells stream object 'reader-place 'realpart
                            :label        "Real part"
                            :object-style :complex-realpart)

        (format-place-cells stream object 'reader-place 'imagpart
                            :label        "Imaginary part"
                            :object-style :complex-imagpart))

      (formatting-row (stream)
        (format-place-cells stream object 'reader-place 'abs
                            :label        "Magnitude"
                            :object-style :complex-magnitude)

        (format-place-cells stream object 'reader-place 'phase
                            :label "Phase" :object-style :complex-phase))))

  ;; Graphical representation.
  (let* ((real          (realpart object))
         (imag          (imagpart object))
         (phase         (phase object))
         (magnitude     (abs object))
         (unit-circle-p (<= .7 magnitude 1.3))
         (radius        (max magnitude (if unit-circle-p 1 0)))
         (scale         (if (plusp radius)
                            (/ radius)
                            1))
         (size          40))
    (with-room-for-graphics (stream)
      (with-scaling (stream size size)
        ;; Center
        (with-drawing-options (stream :ink +dark-gray+ :line-dashes '(4 4))
          (when unit-circle-p
            (draw-circle* stream 0 0 scale :filled nil))
          (draw-line* stream -1 0 1 0)
          (draw-line* stream 0 -1 0 1))
        (with-scaling (stream scale scale)
          (draw-line* stream 0 imag real imag :ink (make-contrasting-inks 8 0))
          (draw-line* stream real 0 real imag :ink (make-contrasting-inks 8 1))

          (draw-line* stream 0 0 (* magnitude (cos phase)) (* magnitude (sin phase))
                      :ink (make-contrasting-inks 8 3))

          (multiple-value-bind (start end)
              (if (plusp phase)
                  (values (- (* 2 pi) phase) 0)
                  (values 0                  (- phase)))
            (draw-circle* stream 0 0 (* 0.5 magnitude)
                          :filled nil :start-angle start :end-angle end
                          :ink (make-contrasting-inks 8 4))))))))

;;; No circularity tracking for numbers.
(defmethod note-object-occurrence ((object       number)
                                   (state        inspected-object)
                                   (presentation t)
                                   (stream       t)))
