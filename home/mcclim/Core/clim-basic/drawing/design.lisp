;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2014 by
;;;      Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 1998,2002 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;;; Some Notes

;;; The design design has a pitfall:
;;;
;;; As drawing is specified, a design of class opacity carries no color
;;; and thus borrows its color from  +foreground-ink+. So that
;;;
;;; (make-opacity v) == (compose-in +foreground-ink+ (make-opacity v))
;;
;;; This implies, that an opacity is not neccessary uniform, it depends
;;; on the selected foreground ink.
;;;
;;; They halfway fix this by specifing that the mask argument of
;;; compose-in and compose-out is thought as to be drawn with
;;; +foreground-ink+ = +background-ink+ = +black+.
;;;
;;; But Is (make-opacity 1) really the same as +foreground-ink+?
;;;     Or: Is
;;;         (compose-in D +foreground-ink+) different from
;;;         (compose-in D (make-opacity 1))?
;;;
;;; If the above equation is true, we get a funny algebra:
;;;
;;; (make-opacity 0) = +transparent-ink+ = +nowhere+
;;; (make-opacity 1) = +foreground-ink+ = +everywhere+
;;;
;;; --GB

;;; I agree with this interpretation. -Hefner

;;; It might be handy to have the equivalent of parent-relative
;;; backgrounds. We can specify new indirect inks:
;;;
;;; +parent-background+
;;; +parent-relative-background+
;;; +parent-foreground+
;;; +parent-relative-foreground+
;;;
;;; The relative one would have the same "absolute" origin as the
;;; relevant inks of the parent.
;;;
;;; When they are evaluated, they look at the parent's
;;; foreground/background ink. Though the relative variants are
;;; expensive, when you want to scroll them ...
;;;
;;;
;;; Further we really should specify some form of indirekt ink
;;; protocol.
;;;
;;; --GB

;;;; Design Protocol
;;;
;;; DRAW-DESIGN already is all you need for a design protocol.
;;;
;;; --GB
;;;
;;;

;;;
;;;   EFFECTIVE-TRANSFORMED-DESIGN design                             [function]
;;;
;;;      Returns a transformed design with all transformations collapset into a
;;;      single transformation and a design being the source pattern. If
;;;      resulting transformation is an identity returns source pattern
;;;      itself. If function is called with a pattern which is not transformed
;;;      that pattern is returned.
;;;
;;;
;;;   DESIGN-INK design x y                                           [method]
;;;
;;;      Returns ink at position X, Y. If DESIGN is uniform then it is
;;;      returned. If DESIGN is not defined for the specified coordinates (i.e
;;;      for array pattern they are out of bounds), +TRANSPARENT-INK+ is
;;;      returned.
;;;
;;;
;;;   COLOR-RGBA design                                               [method]
;;;
;;;      Like COLOR-RGB but works also on UNIFORM-COMPOSITUM and OPACITY.
;;;      Returns four values: red, green, blue and opacity. Each is a float
;;;      between 0.0 and 1.0.
;;;
;;;
;;;   INDIRECT-INK                                                    [class]
;;;   INDIRECT-INK-P ink                                              [predicate]
;;;   INDIRECT-INK-INK ink                                            [function]
;;;
;;;      Indirect inks are underspecified in the standard. Class is exported for
;;;      specialization. Predicate INDIRECT-INK-P is defined. Initargs:
;;;
;;;      SYMBOL is dynamically read with SYMBOL-VALUE when function
;;;       INDIRECT-INK-INK is called. It should be bound to an ink. This is
;;;       internal interface. There is no guarantee for infinite recursion
;;;       protection, so if dynamic variable is bound to ink referring results
;;;       are not specified.
;;;
;;;      DEFAULT ink used if symbol is not bound.

(in-package :clim-internals)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defgeneric color-rgb (color))
(defgeneric color-rgba (color))
(defgeneric design-ink (design x y))

(defmethod print-object ((color color) stream)
  (print-unreadable-object (color stream :identity nil :type t)
    (multiple-value-call #'format stream "~,4F ~,4F ~,4F" (color-rgb color))))

;;; standard-color

(defclass standard-color (color)
  ((red   :initarg :red
          :initform 0
          :type (real 0 1))
   (green :initarg :green
          :initform 0
          :type (real 0 1))
   (blue  :initarg :blue
          :initform 0
          :type (real 0 1))))

(defmethod opacity-value ((ink standard-color))
  1.0)

(defmethod color-rgb ((color standard-color))
  (with-slots (red green blue) color
    (values red green blue)))

(defmethod color-rgba ((color standard-color))
  (with-slots (red green blue) color
    (values red green blue 1.0)))

(defmethod design-ink ((color standard-color) x y)
  (declare (ignore x y))
  color)

(defclass named-color (standard-color)
  ((name :initarg :name
	 :initform "Unnamed color") ))

(defmethod print-object ((color named-color) stream)
  (with-slots (name) color
    (print-unreadable-object (color stream :type t :identity nil)
      (format stream "~S" name))))

(defmethod make-load-form ((color named-color) &optional env)
  (declare (ignore env))
  (with-slots (name red green blue) color
    `(make-named-color ',name ,red ,green ,blue)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *color-hash-table*
    (trivial-garbage:make-weak-hash-table
     :weakness :value
     :weakness-matters nil)))

(defun compute-color-key (red green blue)
  (+ (ash (round (* 255 red)) 16)
     (ash (round (* 255 green)) 8)
     (round (* 255 blue))))

(defun make-rgb-color (red green blue)
  (let ((key (compute-color-key red green blue)))
    (declare (type fixnum key))
    (or (gethash key *color-hash-table*)
	(setf (gethash key *color-hash-table*)
	      (make-instance 'named-color :red red :green green :blue blue)))))

(defun make-gray-color (intensity)
  (make-rgb-color intensity intensity intensity))

(defun make-named-color (name red green blue)
  (let* ((key (compute-color-key red green blue))
	 (entry (gethash key *color-hash-table*)))
    (declare (type fixnum key))
    (cond (entry
	   (when (string-equal (slot-value entry 'name) "Unnamed color")
	     (setf (slot-value entry 'name) name))
	   entry)
	  (t (setf (gethash key *color-hash-table*)
		   (make-instance 'named-color :name name :red red :green green :blue blue))))))
) ; eval-when

;;;
;;; Below is a literal translation from Dylan's DUIM source code,
;;; which was itself probably literal translation from some Lisp code.
;;;

(defconstant +ihs-rgb-c1+ (sqrt (coerce 1/6 'double-float)))
(defconstant +ihs-rgb-c2+ (sqrt (coerce 1/2 'double-float)))
(defconstant +ihs-rgb-c3+ (sqrt (coerce 1/3 'double-float)))

(defun ihs-to-rgb (intensity hue saturation)
  (let* ((hh (- (* (mod (- hue 1/2) 1) 2 pi) pi))
         (c3 (cos saturation))
         (s3 (sin saturation))
         (cos-hh (cos hh))
         (sin-hh (sin hh))
         (x (* +ihs-rgb-c1+ s3 cos-hh intensity))
         (y (* +ihs-rgb-c2+ s3 sin-hh intensity))
         (z (* +ihs-rgb-c3+ c3 intensity)))
    (declare (type (real #.(- pi) #.pi) hh))
    (values (max 0 (min 1 (+ x x z)))
            (max 0 (min 1 (+ y z (- x))))
            (max 0 (min 1 (- z x y))))))

(defun rgb-to-ihs (red green blue)
  (let* ((x (* +ihs-rgb-c1+ (- (* 2 red) blue green)))
         (y (* +ihs-rgb-c2+ (- green blue)))
         (z (* +ihs-rgb-c3+ (+ red green blue)))
         (q (+ (* x x) (* y y)))
         (intensity (sqrt (+ q (* z z))))) ;sqrt(r^2 + g^2 + b^2)
    (if (zerop q)
        ;; A totally unsaturated color
        (values intensity 0 0)
      (let* ((hue (mod (/ (atan y x) (* 2 pi)) 1))
             (f1 (/ z intensity))
             (f2 (sqrt (- 1 (* f1 f1))))
             (saturation (atan f2 f1)))
        (values intensity hue saturation)))))

(defgeneric color-ihs (color))

(defmethod color-ihs ((color color))
  (multiple-value-call #'rgb-to-ihs (color-rgb color)))

(defun make-ihs-color (i h s)
  (multiple-value-call #'make-rgb-color (ihs-to-rgb i h s)))

;;;
;;; 13.3.2 Contrasting Colors
;;;

(defconstant +contrasting-colors+
  (vector (make-named-color "Contrasting1" 0.0        0.44705883  0.7411765)
          (make-named-color "Contrasting2" 0.8509804  0.3254902   0.09803922)
          (make-named-color "Contrasting3" 0.92941177 0.69411767  0.1254902)
          (make-named-color "Contrasting4" 0.49411765 0.18431373  0.5568628)
          (make-named-color "Contrasting5" 0.46666667 0.6745098   0.1882353)
          (make-named-color "Contrasting6" 0.3019608  0.74509805  0.93333334)
          (make-named-color "Contrasting7" 0.63529414 0.078431375 0.18431373)
          (make-named-color "Contrasting8" 0.53529414 0.378431375 0.38431373)))

(defmethod contrasting-inks-limit (port)
  (length +contrasting-colors+))

(defun make-contrasting-inks (n &optional k)
  (let ((contrasting-colors +contrasting-colors+))
    (unless (<= 1 n (length contrasting-colors))
      (error "The argument N = ~D is out of range [1, ~D]"
             n (length contrasting-colors)))
    (unless (or (null k) (<= 0 k (1- n)))
      (error "The argument K = ~D is out of range [0, ~D]" k (1- n)))
    (if (null k)
        (subseq contrasting-colors 0 n)
        (aref contrasting-colors k))))

;;; The two default colors

(defconstant +white+ (make-named-color "white" 1.0000 1.0000 1.0000))
(defconstant +black+ (make-named-color "black" 0.0000 0.0000 0.0000))

;;;;
;;;; 13.6 Indirect Inks
;;;;

(defclass indirect-ink (design)
  ((dynamic-variable-symbol :initarg :symbol  :initform (error "required"))
   (default-ink             :initarg :default :initform (error "required"))))

(defun indirect-ink-p (design)
  (typep design 'indirect-ink))

(defun indirect-ink-ink (design)
  (check-type design indirect-ink)
  (with-slots (dynamic-variable-symbol default-ink) design
    (if (boundp dynamic-variable-symbol)
        (symbol-value dynamic-variable-symbol)
        default-ink)))

(defmethod opacity-value ((ink indirect-ink))
  (let ((ink (indirect-ink-ink ink)))
    (opacity-value ink)))

(defmethod color-rgb ((ink indirect-ink))
  (let ((ink (indirect-ink-ink ink)))
    (color-rgb ink)))

(defmethod color-rgba ((ink indirect-ink))
  (let ((ink (indirect-ink-ink ink)))
    (color-rgba ink)))

(defmethod design-ink ((ink indirect-ink) x y)
  (let ((ink (indirect-ink-ink ink)))
    (design-ink ink x y)))

(defclass %foreground-ink (indirect-ink everywhere-mixin) ())

(defvar *foreground-ink*)
(defvar +foreground-ink+
  (make-instance '%foreground-ink :symbol '*foreground-ink* :default +black+))

(defvar *background-ink*)
(defvar +background-ink+
  (make-instance 'indirect-ink :symbol '*background-ink* :default +white+))

;;;;
;;;; 13.4 Opacity
;;;;

(defmethod print-object ((object opacity) stream)
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~S" (opacity-value object))))

;; Note: Though tempting, opacity is not a uniform-design!

(defclass standard-opacity (opacity)
  ((value :initarg :value
          :type (real 0 1)
          :reader opacity-value)))

(defmethod design-ink ((opacity standard-opacity) x y)
  (declare (ignore x y))
  opacity)

(defclass %transparent-ink (standard-opacity nowhere-mixin)
  ()
  (:default-initargs :value 0))

(defvar +transparent-ink+
  (make-instance '%transparent-ink :value 0))

(defmethod opacity-value ((region everywhere-mixin))
  (declare (ignore region))
  1.0)

(defmethod opacity-value ((region nowhere-mixin))
  (declare (ignore region))
  0.0)

(defun make-opacity (value)
  (setf value (clamp value 0 1))        ;defensive programming
  (cond ((= value 0) +transparent-ink+)
        ((= value 1) +everywhere+)      ; used to say +foreground-ink+
        (t
         (make-instance 'standard-opacity :value value))))

(defmethod color-rgba ((color opacity))
  (multiple-value-call #'values
    (color-rgb (indirect-ink-ink +foreground-ink+))
    (opacity-value color)))

;;; Regions
(defmethod design-ink ((design region) x y)
  (if (region-contains-position-p design x y)
      (design-ink +foreground-ink+ x y)
      +transparent-ink+))

;;;;
;;;; 13.7 Flipping Ink
;;;;

(defclass standard-flipping-ink (design)
  ((design1 :initarg :design1
            :type design)
   (design2 :initarg :design2
            :type design)))

(defmethod design-ink ((flipping-ink standard-flipping-ink) x y)
  (declare (ignore x y))
  flipping-ink)

(defvar +flipping-ink+ (make-instance 'standard-flipping-ink
                         :design1 +foreground-ink+
                         :design2 +background-ink+))

(defmethod print-object ((ink (eql +flipping-ink+)) stream)
  (format stream "#.~S" '+flipping-ink+))

(defmethod print-object ((flipper standard-flipping-ink) stream)
  (with-slots (design1 design2) flipper
    (print-unreadable-object (flipper stream :identity nil :type t)
      (format stream "~S ~S" design1 design2))))

(defgeneric make-flipping-ink (design1 design2))

(defmethod make-flipping-ink ((design1 design) (design2 design))
  (make-instance 'standard-flipping-ink :design1 design1 :design2 design2))

(defmethod make-flipping-ink ((design1 (eql +foreground-ink+))
                              (design2 (eql +background-ink+)))
  +flipping-ink+)

(defmethod make-flipping-ink ((design1 (eql +background-ink+))
                              (design2 (eql +foreground-ink+)))
  +flipping-ink+)

;;;;
;;;; 14 General Designs
;;;;

(declaim (inline color-blend-function))
(defun color-blend-function (r1 g1 b1 o1  r2 g2 b2 o2)
  (let* ((o3 (+ o1 (* (- 1 o1) o2)))
         (r3 (/ (+ (* r1 o1) (* (- 1 o1) o2 r2)) o3))
         (g3 (/ (+ (* g1 o1) (* (- 1 o1) o2 g2)) o3))
         (b3 (/ (+ (* b1 o1) (* (- 1 o1) o2 b2)) o3)))
    (values
     r3 g3 b3 o3)))

(defgeneric compose-over (design1 design2))
(defgeneric compose-in (ink mask))
(defgeneric compose-out (ink mask))

;;;
;;; For patterns look in pattern.lisp

;;;

(defclass transformed-design (design)
  ((transformation
    :initarg :transformation
    :reader transformed-design-transformation)
   (design
    :initarg :design
    :reader transformed-design-design)))

;;; This may be cached in a transformed-design slot. -- jd 2018-09-24
(defun effective-transformed-design (design &aux source-design)
  "Merges all transformations along the way and returns a shallow, transformed
desgin. If design is not transformed (or effective transformation is an
identity-transformation) then source design is returned."
  (check-type design design)
  (labels ((effective-transformation (p)
             (let* ((design* (transformed-design-design p))
                    (transformation (transformed-design-transformation p)))
               (typecase design*
                 (transformed-design
                  (compose-transformations transformation
                                           (effective-transformation design*)))
                 (otherwise
                  (setf source-design design*)
                  transformation)))))
    (typecase design
      (transformed-design
       (if (identity-transformation-p (transformed-design-transformation design))
           (effective-transformed-design (transformed-design-design design))
           (make-instance (type-of design)
                          ;; Argument order matters: EFFECTIVE-TRANSFORMATION
                          ;; sets SOURCE-DESIGN.
                          :transformation (effective-transformation design)
                          :design source-design)))
      (otherwise design))))

(defmethod transform-region :around (transformation (design design))
  (if (or (identity-transformation-p transformation)
          (typep design '(or color opacity uniform-compositum standard-flipping-ink indirect-ink)))
      design
      (call-next-method)))

(defmethod transform-region (transformation (design design))
  (let ((old-transformation (transformed-design-transformation design)))
    (if (and (translation-transformation-p transformation)
             (translation-transformation-p old-transformation))
        (make-instance 'transformed-design
                       :design (transformed-design-design design)
                       :transformation (compose-transformations old-transformation transformation))
        (make-instance 'transformed-design :design design :transformation transformation))))

(defmethod transformed-design-transformation ((design design)) +identity-transformation+)
(defmethod transformed-design-design ((design design)) design)

;;;

(defclass masked-compositum (design)
  ((ink  :initarg :ink  :reader compositum-ink)
   (mask :initarg :mask :reader compositum-mask)))

(defmethod print-object ((object masked-compositum) stream)
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~S ~S ~S ~S"
            :ink  (compositum-ink object)
            :mask (compositum-mask object))))

(defclass in-compositum (masked-compositum) ())

(defmethod compose-in ((ink design) (mask design))
  (make-instance 'in-compositum :ink ink :mask mask))

(defclass out-compositum (masked-compositum) ())

(defmethod compose-out ((ink design) (mask design))
  (make-instance 'out-compositum :ink ink :mask mask))

(defclass over-compositum (design)
  ((foreground :initarg :foreground :reader compositum-foreground)
   (background :initarg :background :reader compositum-background)))

(defmethod compose-over ((foreground design) (background design))
  (make-instance 'over-compositum
    :foreground foreground
    :background background))

;;; Inefficient fallback method.
;;; FIXME we forward-reference %rgba-value.
(defmethod design-ink ((ink over-compositum) x y)
  (let ((fg (design-ink (compositum-foreground ink) x y))
        (bg (design-ink (compositum-background ink) x y)))
    (multiple-value-bind (r g b o)
        (multiple-value-call #'color-blend-function
          (color-rgba fg)
          (color-rgba bg))
      (make-uniform-compositum (make-rgb-color r g b) o)) ))

(defclass uniform-compositum (in-compositum)
  ;; we use this class to represent rgbo values
  ())

(defmethod color-rgba ((color uniform-compositum))
  (with-slots (ink mask) color
    (multiple-value-call #'values (color-rgb ink) (opacity-value mask))))

(defmethod design-ink ((compositum uniform-compositum) x y)
  (declare (ignore x y))
  compositum)

;;;
;;; color
;;; opacity
;;; indirect-ink
;;; in-compositum
;;; over-compositum
;;; out-compositum
;;; uniform-compositum
;;;


;;;; -----------------------------------------------------------------
;;;;
;;;; COMPOSE-IN
;;;;

(defun make-uniform-compositum (ink opacity-value)
  (cond ((= opacity-value 0)
         +transparent-ink+)
        ((= opacity-value 1)
         ink)
        (t
         (make-instance 'uniform-compositum
           :ink ink
           :mask (make-opacity opacity-value)))))
;;; COLOR

(defmethod compose-in ((ink design) (mask color))
  (declare (ignorable ink))
  ink)

;;; OPACITY

(defmethod compose-in ((ink opacity) (mask opacity))
  (make-opacity (* (opacity-value ink) (opacity-value mask))))

(defmethod compose-in ((ink color) (mask opacity))
  (make-uniform-compositum ink (opacity-value mask)))

;;; UNIFORM-COMPOSITUM

(defmethod compose-in ((ink uniform-compositum) (mask uniform-compositum))
  (make-uniform-compositum (compositum-ink ink)
                           (* (opacity-value (compositum-mask ink))
                              (opacity-value (compositum-mask mask)))))

(defmethod compose-in ((ink uniform-compositum) (mask opacity))
  (make-uniform-compositum (compositum-ink ink)
                           (* (opacity-value (compositum-mask ink))
                              (opacity-value mask))))

(defmethod compose-in ((ink opacity) (mask uniform-compositum))
  (make-opacity (* (opacity-value mask)
                   (opacity-value (compositum-mask mask)))))

(defmethod compose-in ((ink color) (mask uniform-compositum))
  (make-uniform-compositum ink (opacity-value mask)))

(defmethod compose-in ((design design) (mask everywhere-mixin))
  (declare (ignore mask))
  design)

(defmethod compose-in ((design design) (mask nowhere-mixin))
  (declare (ignore design mask))
  +nowhere+)

;;; IN-COMPOSITUM

;;; Since compose-in is associative, we can write it this way:
(defmethod compose-in ((ink in-compositum) (mask design))
  (compose-in (compositum-ink ink)
              (compose-in (compositum-mask ink)
                          mask)))

#+nyi
(defmethod compose-in ((ink opacity) (mask in-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink color) (mask in-compositum))
  (declare (ignorable ink mask))
  )


;;; OUT-COMPOSITUM

#+nyi
(defmethod compose-in ((ink out-compositum) (mask out-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink out-compositum) (mask in-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink out-compositum) (mask uniform-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink out-compositum) (mask opacity))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink uniform-compositum) (mask out-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink opacity) (mask out-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink color) (mask out-compositum))
  (declare (ignorable ink mask))
  )


;;; OVER-COMPOSITUM

#+nyi
(defmethod compose-in ((ink over-compositum) (mask over-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink over-compositum) (mask out-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink over-compositum) (mask in-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink over-compositum) (mask uniform-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink over-compositum) (mask opacity))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink out-compositum) (mask over-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink uniform-compositum) (mask over-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink opacity) (mask over-compositum))
  (declare (ignorable ink mask))
  )

#+nyi
(defmethod compose-in ((ink color) (mask over-compositum))
  (declare (ignorable ink mask))
  )

;;;; -----------------------------------------------------------------
;;;;
;;;;  Compose-Out
;;;;

(defmethod compose-out ((design design) (mask everywhere-mixin))
  (declare (ignore design mask))
  +nowhere+)

(defmethod compose-out ((design design) (mask nowhere-mixin))
  (declare (ignore mask))
  design)

(defmethod compose-out ((design design) (mask color))
  (declare (ignore design mask))
  +nowhere+)

(defmethod compose-out ((design design) (mask uniform-compositum))
  (compose-in design (make-opacity (- 1.0 (compositum-mask (opacity-value mask))))))

(defmethod compose-out ((design design) (mask standard-opacity))
  (compose-in design (make-opacity (- 1.0 (opacity-value mask)))))

;;;; -----------------------------------------------------------------
;;;;
;;;;  Compose-Over
;;;;

;;; COLOR

(defmethod compose-over ((foreground color) (background design))
  (declare (ignorable background))
  foreground)

;;; OPACITY

(defmethod compose-over ((foreground opacity) (background opacity))
  (make-opacity
   (+ (opacity-value foreground)
      (* (- 1 (opacity-value foreground)) (opacity-value background)))))

(defmethod compose-over ((foreground opacity) (background color))
  (make-instance 'over-compositum
    :foreground foreground
    :background background))

;;; UNIFORM-COMPOSITUM

(defmethod compose-over ((foreground uniform-compositum) (background uniform-compositum))
  (multiple-value-bind (r g b o)
      (multiple-value-call #'color-blend-function
        (color-rgb (compositum-ink foreground))
        (opacity-value (compositum-mask foreground))
        (color-rgb (compositum-ink background))
        (opacity-value (compositum-mask background)))
    (make-uniform-compositum
     (make-rgb-color r g b)
     o)))

(defmethod compose-over ((foreground uniform-compositum) (background opacity))
  (make-instance 'over-compositum
    :foreground foreground
    :background background))

(defmethod compose-over ((foreground uniform-compositum) (background color))
  (multiple-value-bind (r g b o)
      (multiple-value-call #'color-blend-function
        (color-rgb (compositum-ink foreground))
        (opacity-value (compositum-mask foreground))
        (color-rgb background)
        1)
    (make-uniform-compositum
     (make-rgb-color r g b)
     o)))

(defmethod compose-over ((foreground opacity) (background uniform-compositum))
  (multiple-value-bind (r g b o)
      (multiple-value-call #'color-blend-function
        (color-rgb foreground)
        (color-rgb (compositum-ink background))
        (opacity-value (compositum-mask background)))
    (make-uniform-compositum
     (make-rgb-color r g b)
     o)))

;;; IN-COMPOSITUM

#+nyi
(defmethod compose-over ((foreground in-compositum) (background in-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground in-compositum) (background uniform-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground in-compositum) (background opacity))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground in-compositum) (background color))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground uniform-compositum) (background in-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground opacity) (background in-compositum))
  (declare (ignorable foreground background))
  )

;;; OUT-COMPOSITUM

#+nyi
(defmethod compose-over ((foreground out-compositum) (background out-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground out-compositum) (background in-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground out-compositum) (background uniform-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground out-compositum) (background opacity))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground out-compositum) (background color))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground in-compositum) (background out-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground uniform-compositum) (background out-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground color) (background out-compositum))
  (declare (ignorable foreground background))
  )

;;; OVER-COMPOSITUM

#+nyi
(defmethod compose-over ((foreground over-compositum) (background over-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground over-compositum) (background out-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground over-compositum) (background in-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground over-compositum) (background uniform-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground over-compositum) (background opacity))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground over-compositum) (background color))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground out-compositum) (background over-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground in-compositum) (background over-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground uniform-compositum) (background over-compositum))
  (declare (ignorable foreground background))
  )

#+nyi
(defmethod compose-over ((foreground opacity) (background over-compositum))
  (declare (ignorable foreground background))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Comparison of designs.

(defgeneric design-equalp (design1 design2))

(defmethod design-equalp :around ((design1 t) (design2 t))
  (or (eql design1 design2)
      (call-next-method)))

(defmethod design-equalp ((design1 t) (design2 t))
  nil)

(defmethod design-equalp ((design1 standard-color) (design2 standard-color))
  (multiple-value-bind (r1 g1 b1)
      (color-rgb design1)
    (multiple-value-bind (r2 g2 b2)
	(color-rgb design2)
      (and (= r1 r2)
	   (= g1 g2)
	   (= b1 b2)))))

;;; Color utilities

(defgeneric highlight-shade (ink)
  (:documentation
  "Produce an alternate shade of the given ink for the purpose of highlighting.
   Typically the ink will be brightened, but very light inks may be darkened."))

(defmethod highlight-shade (ink) ink)

(defmethod highlight-shade ((ink (eql +background-ink+)))
  +foreground-ink+)

(defmethod highlight-shade ((ink (eql +foreground-ink+)))
  +background-ink+)

(defmethod highlight-shade ((ink standard-color))
  (let ((brighten-factor 0.5)
        (darken-factor 0.15))
  (multiple-value-bind (r g b) (color-rgb ink)
    (multiple-value-bind (blend-ink factor)
        (if (> (- 3.0 r g b) 0.2)
            (values +white+ brighten-factor)
            (values +black+ darken-factor))
      (compose-over (compose-in blend-ink (make-opacity factor))
                    ink)))))
