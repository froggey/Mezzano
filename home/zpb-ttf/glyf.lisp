;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; Loading data from the 'glyf' table.
;;;
;;; $Id: glyf.lisp,v 1.13 2006/03/23 22:22:01 xach Exp $

(in-package #:zpb-ttf)

(defclass control-point ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (on-curve-p :initarg :on-curve-p :reader on-curve-p)))

(defun make-control-point (x y on-curve-p)
  (make-instance 'control-point
                 :x x
                 :y y
                 :on-curve-p on-curve-p))

(defmethod print-object ((control-point control-point) stream)
  (print-unreadable-object (control-point stream :type t)
    (format stream "~D,~D~:[~;*~]"
            (x control-point) (y control-point) (on-curve-p control-point))))

(defmacro do-contour-segments* ((p1 p2) contour &body body)
  (let ((length (gensym))
        (i (gensym))
        (stack (gensym))
        (next (gensym))
        (next-point (gensym "NEXT-POINT"))
        (midpoint (gensym "MIDPOINT"))
        (contour* (gensym))
        (loop (gensym "LOOP"))
        (body-tag (gensym "BODY"))
        (mid p1)
        (end p2))
    `(let* ((,i 1)
            (,contour* ,contour)
            (,length (length ,contour*))
            ,stack ,next ,mid ,end)
       (unless (zerop ,length)
         (flet ((,next-point ()
                  (when (< ,i ,length)
                    (prog1 (aref ,contour* ,i) (incf ,i))))
                (,midpoint (p0 p1)
                  (make-control-point (/ (+ (x p0) (x p1)) 2)
                                      (/ (+ (y p0) (y p1)) 2)
                                      t)))
           (tagbody
              ,loop
              (setf ,mid nil
                    ,next (,next-point))
              (unless ,next
                (setf ,mid ,stack
                      ,end (aref ,contour* 0))
                (go ,body-tag))
              (if (on-curve-p ,next)
                  (setf ,end ,next
                        ,mid ,stack
                        ,stack nil)
                  (cond (,stack
                         (setf ,mid ,stack
                               ,end (,midpoint ,stack ,next)
                               ,stack ,next))
                        (t
                         (setf ,stack ,next)
                         (go ,loop))))
              ,body-tag
              ,@body
              (when ,next
                (go ,loop))))))))
                               
(defmacro do-contour-segments ((p0 p1 p2) contour &body body)
    "A contour is made up of segments. A segment may be a straight line
or a curve. For each segment, bind the P0 and P2 variables to the
start and end points of the segment. If the segment is a curve, set P1
to the control point of the curve, otherwise set P1 to NIL."
    ;; This macro started out life as a function and was converted.
    (let ((start p0)
          (contour* (gensym "CONTOUR")))
      `(let ((,contour* ,contour))
         (when (plusp (length ,contour*))
           (let ((,start (aref ,contour* 0)))
             (do-contour-segments* (,p1 ,p2)
                 ,contour*
               (progn ,@body)
               (setf ,start ,p2)))))))

(defun explicit-contour-points (contour)
  (let ((new-contour (make-array (length contour)
                                 :adjustable t
                                 :fill-pointer 0)))
    (when (plusp (length contour))
      (vector-push-extend (aref contour 0) new-contour))
    (do-contour-segments* (p1 p2)
        contour
      (when p1
        (vector-push-extend p1 new-contour))
      (vector-push-extend p2 new-contour))
    new-contour))


;;; Locating a glyph's contours and bounding box in the font loader's
;;; stream, and loading them

(defparameter *empty-contours*
  (make-array 0 :element-type '(signed-byte 16)))

(defparameter *empty-bounding-box*
  (make-array 4
              :initial-element 0
              :element-type '(signed-byte 16)))

(defun empty-bounding-box ()
  (copy-seq *empty-bounding-box*))

(defun empty-contours ()
  (copy-seq *empty-contours*))

(defun dump-compound-flags (flags)
  (format t "XXX flags=~16,'0B~%" flags)
  (let ((meanings '((0 . ARG_1_AND_2_ARE_WORDS)
                       (1 . ARGS_ARE_XY_VALUES)
                       (2 . ROUND_XY_TO_GRID)
                       (3 . WE_HAVE_A_SCALE)
                       (4 . OBSOLETE)
                       (5 . MORE_COMPONENTS)
                       (6 . WE_HAVE_AN_X_AND_Y_SCALE)
                       (7 . WE_HAVE_A_TWO_BY_TWO)
                       (8 . WE_HAVE_INSTRUCTIONS)
                       (9 . USE_MY_METRICS)
                       (10 . OVERLAP_COMPOUND))))
       (loop for ((bit . meaning)) on meanings
             do (when (logbitp bit flags)
                  (format t "...~A~%" meaning)))))

(defun transform-option-count (flags)
  (let ((scale-p 3)
        (xy-scale-p 6)
        (2*2-scale-p 7))
    (cond ((logbitp scale-p flags) 1)
          ((logbitp xy-scale-p flags) 2)
          ((logbitp 2*2-scale-p flags) 4)
          (t 0))))

(defun make-transformer (a b c d e f)
  "Given the elements of the transformation matrix specified by A, B,
C, D, E, and F, return a function of two arguments that returns the
arguments transformed as multiple values.
Ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6glyf.html"
  (let ((m (max (abs a) (abs b)))
        (n (max (abs c) (abs d))))
    (when (<= (abs (- (abs a) (abs b))) 33/65536)
      (setf m (* m 2)))
    (when (<= (abs (- (abs c) (abs d))) 33/65536)
      (setf n (* n 2)))
    (lambda (x y)
      (values (* m (+ (* (/ a m) x)
                      (* (/ c m) y)
                      e))
              (* n (+ (* (/ b n) x)
                      (* (/ d n) y)
                      f))))))

(defun transform-contours (fn contours)
  "Call FN with the X and Y coordinates of each point of each contour
in the vector CONTOURS. FN should return two values, which are used to
update the X and Y values of each point."
  (loop for contour across contours do
        (loop for p across contour do
              (setf (values (x p) (y p))
                    (funcall fn (x p) (y p))))))

(defun merge-contours (contours-list)
  (let* ((total-contours (loop for contours in contours-list
                               summing (length contours)))
         (merged (make-array total-contours))
         (i 0))
    (dolist (contours contours-list merged)
      (loop for contour across contours do
            (setf (aref merged i) contour)
            (incf i)))))

(defun read-compound-contours (loader)
  (let ((contours-list '())
        (stream (input-stream loader)))
    (loop
     (let ((flags (read-uint16 stream))
           (font-index (read-uint16 stream)))
       (let ((position (file-position stream))
             (contours (read-contours-at-index font-index loader)))
         (push contours contours-list)
         (file-position stream position)
         (let ((args-words-p (logbitp 0 flags))
               (args-xy-values-p (logbitp 1 flags))
               (more-components-p (logbitp 5 flags))
               arg1 arg2)
           (cond ((and args-words-p args-xy-values-p)
                  (setf arg1 (read-int16 stream)
                        arg2 (read-int16 stream)))
                 (args-words-p
                  (setf arg1 (read-uint16 stream)
                        arg2 (read-uint16 stream))
                  (error "Compound glyphs relative to indexes not yet supported"))
                 (args-xy-values-p
                  (setf arg1 (read-int8 stream)
                        arg2 (read-int8 stream)))
                 (t
                  (setf arg1 (read-uint8 stream)
                        arg2 (read-uint8 stream))
                  (error "Compound glyphs relative to indexes not yet supported")))
           ;; Transform according to the transformation matrix
           (let ((a 1.0) (b 0.0) (c 0.0) (d 1.0)
                 (e arg1) (f arg2))
             (ecase (transform-option-count flags)
               (0)
               (1
                (setf a (setf d (read-fixed2.14 stream))))
               (2
                (setf a (read-fixed2.14 stream)
                      d (read-fixed2.14 stream)))
               (4
                (setf a (read-fixed2.14 stream)
                      b (read-fixed2.14 stream)
                      c (read-fixed2.14 stream)
                      d (read-fixed2.14 stream))))
             (let ((transform-fn (make-transformer a b c d e f)))
               (transform-contours transform-fn contours)))
           (unless more-components-p
             (return (merge-contours contours-list)))))))))
     
(defun read-points-vector (stream flags count axis)
  (let ((points (make-array count :fill-pointer 0))
        (short-index (if (eql axis :x) 1 2))
        (same-index (if (eql axis :x) 4 5)))
    (flet ((save-point (point)
             (vector-push point points)))
      (loop for flag across flags
            for short-p = (logbitp short-index flag)
            for same-p = (logbitp same-index flag)
            do (cond (short-p
                      (let ((new-point (read-uint8 stream)))
                        (save-point (if same-p new-point (- new-point)))))
                     (t
                      (if same-p
                          (save-point 0)
                          (save-point (read-int16 stream)))))))
    points))

(defun read-simple-contours (contour-count stream)
  "With the stream positioned immediately after the glyph bounding
box, read the contours data from STREAM and return it as a vector."
  (let ((contour-endpoint-indexes (make-array contour-count)))
    (loop for i below contour-count
          for endpoint-index = (read-uint16 stream)
          do (setf (svref contour-endpoint-indexes i) endpoint-index))
    ;; instructions
    (let ((n-points (1+ (svref contour-endpoint-indexes
                               (1- contour-count))))
          (instruction-length (read-uint16 stream)))
      (loop for i below instruction-length
            do (read-byte stream))
      ;; read the flags
      (let ((flags (make-array n-points)))
        (loop with i = 0
              while (< i n-points) do
              (let ((flag-byte (read-uint8 stream)))
                (setf (svref flags i) flag-byte)
                (incf i)
                (when (logbitp 3 flag-byte)
                  (let ((n-repeats (read-uint8 stream)))
                    (loop repeat n-repeats do
                          (setf (svref flags i) flag-byte)
                          (incf i))))))
        (let ((x-points (read-points-vector stream flags n-points :x ))
              (y-points (read-points-vector stream flags n-points :y))
              (control-points (make-array n-points :fill-pointer 0))
              (contours (make-array contour-count)))
          (loop for x-point across x-points
                for y-point across y-points
                for flag across flags
                for x = x-point then (+ x x-point)
                for y = y-point then (+ y y-point)
                do
                (vector-push-extend (make-control-point x y
                                                        (logbitp 0 flag))
                                    control-points))
          (loop for start = 0 then (1+ end)
                for end across contour-endpoint-indexes
                for i from 0
                do (setf (svref contours i)
                         (subseq control-points start (1+ end))))
          contours)))))

(defun read-contours-at-index (index loader)
  "Read the contours at glyph index INDEX, discarding bounding box
information."
  (let ((stream (input-stream loader)))
    (file-position stream (+ (table-position "glyf" loader)
                             (glyph-location index loader)))
    (let ((contour-count (read-int16 stream))
          (xmin (read-int16 stream))
          (ymin (read-int16 stream))
          (xmax (read-int16 stream))
          (ymax (read-int16 stream)))
      (declare (ignore xmin ymin xmax ymax))
      (if (= contour-count -1)
          (read-compound-contours loader)
          (read-simple-contours contour-count stream)))))
