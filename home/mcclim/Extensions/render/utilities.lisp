(in-package :mcclim-render-internals)

(defconstant +alpha-pos+ 24)
(defconstant +red-pos+ 16)
(defconstant +green-pos+ 8)
(defconstant +blue-pos+ 0)

(defmacro ldb-a (argb)
  `(ldb (byte 8 +alpha-pos+) ,argb))

(defmacro ldb-r (argb)
  `(ldb (byte 8 +red-pos+) ,argb))

(defmacro ldb-g (argb)
  `(ldb (byte 8 +green-pos+) ,argb))

(defmacro ldb-b (argb)
  `(ldb (byte 8 +blue-pos+) ,argb))

(defmacro do-regions (((src-j dest-j y1s y1d y2)
                       (src-i dest-i x1s x1d x2)
                       &key backward) &body body)
  "Region mapping macro. Iterates over two regions synchronously. When BACKWARD
is T iteration starts from the bottom-right corner, otherwise from the
top-left. Useful when we iterate over the same array and mutate its state."
  (check-type backward (member t nil))
  (if (null backward)
      `(loop
          for ,src-j fixnum from ,y1s
          for ,dest-j fixnum from ,y1d to ,y2
          do (loop
                for ,src-i fixnum from ,x1s
                for ,dest-i fixnum from ,x1d to ,x2
                do (progn ,@body)))
      `(loop
          for ,src-j fixnum from (+ ,y1s (- ,y2 ,y1d)) downto ,y1s
          for ,dest-j fixnum from ,y2 downto ,y1d
          do (loop
                for ,src-i fixnum from (+ ,x1s (- ,x2 ,x1d)) downto ,x1s
                for ,dest-i fixnum from ,x2 downto ,x1d
                do (progn ,@body)))))

(defmacro do-region-pixels ((&rest clauses) &body body)
  (let ((outer-clauses '())
        (inner-clauses '()))
    (flet ((do-clause (clause)
             (climi::with-current-source-form (clause)
               (destructuring-bind (width pixel-and-coordinates
                                    &key (x1 0) x2 (x-step 1)
                                         (y1 0) y2 (y-step 1))
                   clause
                 (destructuring-bind (index-var &optional (x-var (gensym "X"))
                                                          (y-var (gensym "Y")))
                     (alexandria:ensure-list pixel-and-coordinates)
                   (alexandria:with-unique-names (width-var base)
                     (when index-var
                       (push `(with ,width-var :of-type (unsigned-byte 20) = ,width)
                             outer-clauses))
                     (push `(for ,y-var :of-type (or (eql -1) (unsigned-byte 20))
                                 from ,y1
                                 ,@(when y2 `(to ,y2))
                                 by ,y-step)
                           outer-clauses)
                     (when index-var
                       (push `(with ,base :of-type alexandria:array-index
                                    = (* ,y-var ,width-var))
                             inner-clauses))
                     (push `(for ,x-var :of-type (or (eql -1) alexandria:array-index)
                                 from ,x1
                                 ,@(when x2 `(to ,x2))
                                 by ,x-step)
                           inner-clauses)
                     (when index-var
                       (push `(for ,index-var :of-type (or (eql -1) alexandria:array-index)
                                   from (+ ,base ,x1) by ,x-step)
                             inner-clauses))))))))
      (map nil #'do-clause clauses)
      `(loop ,@(reduce #'append (reverse outer-clauses))
             do (loop ,@(reduce #'append (reverse inner-clauses))
                      do (progn ,@body))))))

(declaim (inline %check-coords))

;;; Returns T for valid arguments, NIL for malformed width/height and signals an
;;; error if coordinates go out of arrays bounds.
(defun %check-coords (src-array dst-array sx sy dx dy width height)
  (when (or (<= width 0) (<= height 0))
    (warn "mcclim-render operation called with width = ~s and height = ~s; doing nothing."
          width height)
    (return-from %check-coords nil))
  (unless (and (array-in-bounds-p src-array (1- (+ sy height)) (1- (+ sx width)))
               (array-in-bounds-p dst-array (1- (+ dy height)) (1- (+ dx width))))
    (error "mcclim-render operation: some coordinates are out of image bounds:~@
             src array ~s, P1 ~s, P2 ~s,~@
             dst array ~s, P1 ~s, P2 ~s."
           (nreverse (array-dimensions src-array)) (cons sx sy) (cons (+ sx width) (+ sy height))
           (nreverse (array-dimensions dst-array)) (cons dx dy) (cons (+ dx width) (+ dy height))))
  T)

;;;
;;; color functions
;;;

(deftype octet ()
  '(unsigned-byte 8))

(defmacro let-rgba (((r g b a) elt) &body body)
  (alexandria:once-only (elt)
    `(let (,@(when a
               `((,a (ldb-a ,elt))))
           (,r (ldb-r ,elt))
           (,g (ldb-g ,elt))
           (,b (ldb-b ,elt)))
       ,@body)))

(declaim (inline color-value->octet)
         (ftype (function (real) octet) color-value->octet))
(defun color-value->octet (v)
  (round (* 255 v)))

(declaim (inline color-octet->value)
         (ftype (function (octet) real) color-octet->value))
(defun color-octet->value (o)
  (/ o 255))

(declaim (inline color-octet-xor)
         (ftype (function (octet octet) octet) color-octet-xor))
(defun color-octet-xor (d1 d2)
  (logxor d1 d2))

(declaim (type (simple-array (integer -255 255) (#.(* 256 (+ 256 256)))) +octet-mult-table+))
(alexandria:define-constant +octet-mult-table+
    (loop :with result = '()
          :for a :from 0 :to 255
          :do (loop :for b :from -256 :to 255
                    :do (push (truncate (* a (+ b (logxor #x1 (ldb (byte 1 8) b)))) 256)
                              result))
          :finally (return (coerce (reverse result)
                                   `(simple-array (integer -255 255) (,(* 256 (+ 256 256)))))))
  :test 'equalp)
(declaim (inline octet-mult)
         (ftype (function (octet (integer -255 255)) (integer -255 255)) octet-mult))
(defun octet-mult (a b)
  (declare (optimize speed)
           (type octet a)
           (type (integer -255 255) b))
  #+no (truncate (* a (+ b (logxor #x1 (ldb (byte 1 8) b)))) 256)
  (aref +octet-mult-table+ (the fixnum (+ (the fixnum (+ (the fixnum (ash a 9)) b)) 256))))

;;; blend functions

(declaim (inline %lerp)
         (ftype (function (octet octet octet) octet) %lerp))
(defun %lerp (p q a)
  (declare (optimize speed)
           (type octet p q a))
  (logand #xFF (+ p (octet-mult a (the fixnum (- q p))))))

(declaim (inline %prelerp)
         (ftype (function (octet octet octet) octet) %prelerp))
(defun %prelerp (p q a)
  (logand #xFF (- (the fixnum (+ p q)) (octet-mult a p))))

(declaim (type (simple-array (integer 0 65535) (#. (* 256 256))) +byte-blend-value-table+))
(alexandria:define-constant +byte-blend-value-table+
    (loop :with result = '()
          :for value :from 0 :to 255
          :do (loop :for gamma :from 0 :to 255
                    :do (push (if (<= gamma 1)
                                  (* 255 value)
                                  (truncate (* 255 value) gamma))
                              result))
          :finally (return (coerce (reverse result)
                                   `(simple-array (integer 0 65535) (,(* 256 256))))))
  :test 'equalp)

(declaim (inline %byte-blend-value)
         (ftype (function (octet octet octet octet) octet) %byte-blend-value))
(defun %byte-blend-value (fg bg a.fg a.bg)
  (declare (optimize speed))
  (let ((gamma (%prelerp a.fg a.bg a.bg))
        (value (%lerp (octet-mult bg a.bg) fg a.fg)))
    #+no (if (<= gamma 1) ; TODO values are not octets
        (* 255 value)
        (truncate (* 255 value) gamma))
    (aref +byte-blend-value-table+ (+ (ash value 8) gamma))))

(declaim (inline %byte-blend-value2)
         (ftype (function (octet octet octet octet octet) octet) %byte-blend-value2))
(defun %byte-blend-value2 (fg bg a.fg a.bg gamma)
  (declare (optimize speed)
           (type octet fg bg a.fg a.bg gamma))
  (let ((value (%lerp (octet-mult bg a.bg) fg a.fg)))
    (declare (type octet value))
    (if (<= gamma 1)			; TODO values are not octets
        (the fixnum (* 255 value))
        (the fixnum (truncate (the fixnum (* 255 value)) gamma)))
    #+no (aref +byte-blend-value-table+ (+ (ash value 8) gamma))))

(declaim (inline octet-blend-function octet-blend-function*)
         (ftype (function #1=(octet octet octet octet octet octet octet octet)
                          (values octet octet octet octet))
                octet-blend-function)
         (ftype (function #1# argb-pixel)
                octet-blend-function*))
(defun octet-blend-function (r.fg g.fg b.fg a.fg r.bg g.bg b.bg a.bg)
  (let ((gamma (%prelerp a.fg a.bg a.bg)))
    (values (%byte-blend-value2 r.fg r.bg a.fg a.bg gamma)
            (%byte-blend-value2 g.fg g.bg a.fg a.bg gamma)
            (%byte-blend-value2 b.fg b.bg a.fg a.bg gamma)
            gamma)))

(defun octet-blend-function* (r.fg g.fg b.fg a.fg r.bg g.bg b.bg a.bg)
  (declare (optimize speed))
  (let ((gamma (%prelerp a.fg a.bg a.bg)))
    (logior (ash gamma                                          24)
            (ash (%byte-blend-value2 r.fg r.bg a.fg a.bg gamma) 16)
            (ash (%byte-blend-value2 g.fg g.bg a.fg a.bg gamma) 8)
            (ash (%byte-blend-value2 b.fg b.bg a.fg a.bg gamma) 0))))

(declaim (inline octet-rgba-blend-function)
         (ftype (function (octet octet octet octet octet octet octet octet)
                          (values octet octet octet octet))
                octet-rgba-blend-function))
(defun octet-rgba-blend-function (r.fg g.fg b.fg a.fg r.bg g.bg b.bg a.bg)
  (octet-blend-function r.fg g.fg b.fg a.fg r.bg g.bg b.bg a.bg))

(declaim (inline octet-rgb-blend-function)
         (ftype (function (octet octet octet octet octet octet octet)
                          (values octet octet octet))
                octet-rgb-blend-function))
(defun octet-rgb-blend-function (r.fg g.fg b.fg a.fg r.bg g.bg b.bg)
  (values (%byte-blend-value r.fg r.bg a.fg 255)
          (%byte-blend-value g.fg g.bg a.fg 255)
          (%byte-blend-value b.fg b.bg a.fg 255)))

(declaim (inline octet-gray-blend-function)
         (ftype (function (octet octet octet) octet)
                octet-gray-blend-function))
(defun octet-gray-blend-function (g.fg a.fg g.bg)
  (%byte-blend-value g.fg g.bg a.fg 255))

(declaim (inline octet-alpha-blend-function)
         (ftype (function (octet octet) octet)
                octet-alpha-blend-function))
(defun octet-alpha-blend-function (a.fg a.bg)
  (%prelerp a.fg a.bg a.bg))

;;; conversion

(defgeneric color->octets (color)
  (:method ((color standard-color))
    (multiple-value-bind (r g b) (climi::color-rgb color)
      (values (color-value->octet r)
              (color-value->octet g)
              (color-value->octet b)))))

;;; rgba->
(declaim (inline rgba->rgb)
         (ftype (function (octet octet octet octet)
                          (values octet octet octet))
                rgba->rgb))
(defun rgba->rgb (red green blue alpha)
  (declare (ignore alpha))
  (values red green blue))

(declaim (inline rgba->gray)
         (ftype (function (octet octet octet octet) octet) rgba->gray))
(defun rgba->gray (red green blue alpha)
  (declare (ignore alpha))
  (values (round (+ red green blue) 3)))

(declaim (inline rgba->gray-alpha)
         (ftype (function (octet octet octet octet) (values octet octet))
                rgba->gray-alpha))
(defun rgba->gray-alpha (red green blue alpha)
  (values (round (+ red green blue) 3) alpha))

(declaim (inline rgba->alpha)
         (ftype (function (octet octet octet octet) octet) rgba->alpha))
(defun rgba->alpha (red green blue alpha)
  (declare (ignore red green blue))
  alpha)

;;; rgb->
(declaim (inline rgb->rgba)
         (ftype (function (octet octet octet)
                          (values octet octet octet octet))
                rgb->rgba))
(defun rgb->rgba (red green blue)
  (values red green blue 255))

(declaim (inline rgb->gray)
         (ftype (function (octet octet octet)
                          octet)
                rgb->gray))
(defun rgb->gray (red green blue)
  (values (round (+ red green blue) 3)))

(declaim (inline rgb->alpha)
         (ftype (function (octet octet octet)
                          octet)
                rgb->alpha))
(defun rgb->alpha (red green blue)
   (rgb->gray red blue green))

;;; gray->
(declaim (inline gray->rgba)
         (ftype (function (octet) (values octet octet octet octet))
                gray->rgba))
(defun gray->rgba (gray)
  (values gray gray gray 255))

(declaim (inline gray->rgb)
         (ftype (function (octet) (values octet octet octet)) gray->rgb))
(defun gray->rgb (gray)
  (values gray gray gray))

(declaim (inline gray->alpha)
         (ftype (function (octet) octet) gray->alpha))
(defun gray->alpha (gray)
  gray)

(declaim (inline %rgba->vals %vals->rgba))
(defun %vals->rgba (r g b &optional (a #xff))
  (declare (type octet r g b a)
           (optimize (speed 3) (safety 0)))
  (the argb-pixel
       (logior (the argb-pixel (ash a +alpha-pos+))
               (the argb-pixel (ash r +red-pos+))
               (the argb-pixel (ash g +green-pos+))
               (the argb-pixel (ash b +blue-pos+)))))

(defun %rgba->vals (rgba)
  (declare (type (unsigned-byte 32) rgba)
           (optimize (speed 3) (safety 0)))
  (values (the octet (ldb-r rgba))
          (the octet (ldb-g rgba))
          (the octet (ldb-b rgba))
          (the octet (ldb-a rgba))))
