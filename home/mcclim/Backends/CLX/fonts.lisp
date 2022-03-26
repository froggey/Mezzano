;;;; -*- Mode: Lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; Package: SYSTEM -*-
;;;; vim: set filetype=lisp tabstop=8 shiftwidth=2 expandtab:

;;;;
;;;;  fonts.lisp  -- Font access abstraction for CLX backend
;;;;
;;;;  Copyright (c) 2016, Daniel Kochmański
;;;;
;;;;    see toplevel file 'copyright' for the copyright details.
;;;;

(in-package #:clim-clx)

(defconstant *families/names*
  '(:fix         "adobe-courier"
    :serif       "adobe-times"
    :sans-serif  "adobe-helvetica"))

(defparameter *families/faces*
  '(;; "adobe-courier"
    ((:fix :roman)                 . "medium-r")
    ((:fix :bold)                  . "bold-r")
    ((:fix :italic)                . "medium-o")
    ((:fix (:bold :italic))        . "bold-o")
    ((:fix (:italic :bold))        . "bold-o")
    ;; "adome-times"
    ((:serif :roman)               . "medium-r")
    ((:serif :bold)                . "bold-r")
    ((:serif :italic)              . "medium-i")
    ((:serif (:bold :italic))      . "bold-i")
    ((:serif (:italic :bold))      . "boid-i")
    ;; "adobe-helvetica"
    ((:sans-serif :roman)          . "medium-r")
    ((:sans-serif :bold)           . "bold-r")
    ((:sans-serif :italic)         . "medium-o")
    ((:sans-serif (:bold :italic)) . "bold-o")
    ((:sans-serif (:italic :bold)) . "bold-o")))

(defun open-font (display font-name)
  (let ((fonts (xlib:list-font-names display font-name :max-fonts 1)))
    (when fonts
      (xlib:open-font display (first fonts)))))

(defmethod text-style-mapping ((port clx-port) (text-style text-style) &optional character-set
                               &aux (text-style (climb:parse-text-style* text-style)))
  (declare (ignore character-set))
  (labels
      ((find-and-make-xlib-face (display family face size)
         (let* ((family-name (if (stringp family)
                                 family
                                 (getf *families/names* family)))
                (face-name (if (stringp face)
                               face
                               (alexandria:assoc-value *families/faces* (list family face)
                                                       :test #'equal))))
           (flet ((try (encoding)
                    (open-font display
                               (format nil "-~a-~a-*-*-~d-*-*-*-*-*-~a"
                                       family-name face-name size encoding))))
             ;; XXX: this part is a bit problematic - we either list
             ;; all fonts with any possible encoding (what leads to
             ;; the situation, when our font can't render a simple
             ;; string "abcd") or we end with only a partial list of
             ;; fonts. since we have mcclim-ttf extension which
             ;; handles unicode characters well, this mechanism of
             ;; getting fonts is deprecated and there is no big harm.
             (or (try "iso8859-1")
                 (progn
                   (setf family :sans-serif)
                   (try "iso8859-1"))
                 (progn
                   (setf family :fix)
                   (xlib:open-font display "fixed"))))))
       (find-font ()
         (multiple-value-bind (family face size)
             (text-style-components text-style)
           (setf size (max 2 size))
           (let ((display (clim-clx::clx-port-display port)))
             (find-and-make-xlib-face display family face size)))))
    (find-font)))



(defmethod climb:font-ascent ((font xlib:font))
  (xlib:font-ascent font))

(defmethod climb:font-descent ((font xlib:font))
  (xlib:font-descent font))

(defmethod climb:font-character-width ((font xlib:font) char)
  (xlib:char-width font (char-code char)))

(defmethod climb:font-text-extents ((font xlib:font) string &key start end align-x align-y direction)
  (declare (ignore align-x align-y direction))
  (multiple-value-bind (width ascent descent
                        left-bearing right-bearing overall-ascent overall-descent
                        overall-direction next-start)
      (xlib:text-extents font string :start start :end end :translate #'translate)
    (declare (ignore next-start overall-direction))
    (let ((height (+ overall-ascent overall-descent)))
     (values left-bearing (- ascent) right-bearing descent
             left-bearing overall-ascent width height overall-ascent overall-descent 0
             width 0))))


;;; Font listing implementation

(defclass clx-font-family (clim-extensions:font-family)
  ((all-faces :initform nil
              :accessor all-faces
              :reader clim-extensions:font-family-all-faces)
   (raw-name :initarg :raw-name
             :reader clx-font-family-raw-name)))

(defclass clx-font-face (clim-extensions:font-face)
  ((all-sizes :initform nil
              :accessor all-sizes
              :reader clim-extensions:font-face-all-sizes)
   (raw-name :initarg :raw-name
             :reader clx-font-face-raw-name)))

(defmethod clim-extensions:port-all-font-families ((port clx-basic-port) &key invalidate-cache)
  (when (or (null (clim-clx::font-families port)) invalidate-cache)
    (setf (font-families port) (reload-font-table port)))
  (font-families port))

(defun split-font-name (name)
  (loop
     repeat 12
     for next = (position #\- name :start 0)
     :then (position #\- name :start (1+ next))
     and prev = nil then next
     while next
     when prev
     collect (subseq name (1+ prev) next)))

;;; XXX: xlib can acquire very large set of fonts from xserver, but
;;; most of them doesn't have a pixelsize or doesn't handle even basic
;;; encodings. To make the result as clean as possible we load only
;;; fonts which we know that can render correctly basic text. For more
;;; fancy needs we have ttf fonts which work well with unicode. xorg
;;; fonts are deprecated.
(defun reload-font-table (port)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (font (xlib:list-font-names (clx-port-display port) "*-iso8859-1"))
      (destructuring-bind
            (&optional foundry family weight slant setwidth style pixelsize
                       ;;pointsize xresolution yresolution
                       ;;spacing averagewidth registry encoding
                       &rest ignore)
          (split-font-name font)
        (declare (ignore setwidth style ignore))
        (setf pixelsize (and pixelsize (parse-integer pixelsize)))
        (unless (or (null foundry)
                    (null pixelsize)
                    (zerop pixelsize))
          (let* ((family-name (format nil "~A ~A" foundry family))
                 (family-name* (format nil "~A-~A" foundry family))
                 (family-instance
                  (or (gethash family-name table)
                      (setf (gethash family-name table)
                            (make-instance 'clx-font-family
                                           :port port
                                           :name family-name
                                           :raw-name family-name*))))
                 (face-name (format nil "~A ~A" weight slant))
                 (face-name* (format nil "~A-~A" weight slant))
                 (face-instance
                  (find face-name (all-faces family-instance)
                        :key #'clim-extensions:font-face-name
                        :test #'equal)))
            (unless face-instance
              (setf face-instance
                    (make-instance 'clx-font-face
                                   :family family-instance
                                   :name face-name
                                   :raw-name face-name*))
              (push face-instance (all-faces family-instance)))
            (pushnew pixelsize (all-sizes face-instance))))))
    (sort (loop
             for family being each hash-value in table
             do
               (setf (all-faces family)
                     (sort (all-faces family)
                           #'string<
                           :key #'clim-extensions:font-face-name))
               (dolist (face (all-faces family))
                 (setf (all-sizes face)
                       #- (or) (sort (all-sizes face) #'<)))
             collect family)
          #'string<
          :key #'clim-extensions:font-family-name)))

(defmethod clim-extensions:font-face-text-style
    ((face clx-font-face) &optional size)
  (make-text-style
   (clx-font-family-raw-name
    (clim-extensions:font-face-family face))
   (clx-font-face-raw-name face)
   size))
