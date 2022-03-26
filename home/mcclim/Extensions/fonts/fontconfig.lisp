;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: MCCLIM-FREETYPE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: TrueType font detection
;;;   Created: 2003-05-25 16:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2008 by Andy Hefner
;;;  (c) copyright 2016 by Daniel Kochmański
;;;
;;;    See toplevel file 'Copyright' for the copyright details.
;;;

;;; This file contains our attempts to configure TTF paths and map
;;; them to the predefined text styles. First we check if some default
;;; paths can be used for that purpose, otherwise we shell out to
;;; `fc-match'.

(in-package :mcclim-truetype)


;;; fallback (path may be set in a restart by the user)
(defparameter *truetype-font-path*
  (find-if #'probe-file
           #+mezzano
           '(#p"LOCAL:>Fonts>")
           #-mezzano
           '(#p"/usr/share/fonts/truetype/ttf-dejavu/"
             #p"/usr/share/fonts/truetype/dejavu/"
             #p"/usr/share/fonts/dejavu/"
             #p"/usr/share/fonts/truetype/"
             #p"/usr/share/fonts/TTF/"
             #p"/usr/share/fonts/dejavu/"
             #p"/usr/share/fonts/"
             #p"/usr/local/share/fonts/dejavu/"
             #p"/usr/X11R6/lib/X11/fonts/TTF/"
             #p"/usr/X11R7/lib/X11/fonts/TTF/"
             #p"/opt/X11/share/fonts/TTF/"
             #p"/opt/X11/share/fonts/"
             #p"~/.guix-profile/share/fonts/truetype/"
             #p"/Library/Fonts/"
             #-mezzano #p"C:/Windows/Fonts/")))

;;; Here are mappings for the DejaVu family of fonts, which are a
;;; derivative of Vera with improved unicode coverage.
;;;
;;; Paths are relative so we are able to rely on value of a special
;;; variable *truetype-font-path*, so if it is changed in
;;; `invoke-with-truetype-path-restart' it will be used.
(defvar *families/faces*
  '(((:fix :roman) . "DejaVuSansMono.ttf")
    ((:fix :italic) . "DejaVuSansMono-Oblique.ttf")
    ((:fix (:bold :italic)) . "DejaVuSansMono-BoldOblique.ttf")
    ((:fix (:italic :bold)) . "DejaVuSansMono-BoldOblique.ttf")
    ((:fix :bold) . "DejaVuSansMono-Bold.ttf")
    ((:serif :roman) . "DejaVuSerif.ttf")
    ((:serif :italic) . "DejaVuSerif-Italic.ttf")
    ((:serif (:bold :italic)) . "DejaVuSerif-BoldOblique.ttf")
    ((:serif (:italic :bold)) . "DejaVuSerif-BoldOblique.ttf")
    ((:serif :bold) . "DejaVuSerif-Bold.ttf")
    ((:sans-serif :roman) . "DejaVuSans.ttf")
    ((:sans-serif :italic) . "DejaVuSans-Oblique.ttf")
    ((:sans-serif (:bold :italic)) . "DejaVuSans-BoldOblique.ttf")
    ((:sans-serif (:italic :bold)) . "DejaVuSans-BoldOblique.ttf")
    ((:sans-serif :bold) . "DejaVuSans-Bold.ttf")))

(defun invoke-with-truetype-path-restart (continuation)
  (restart-case (funcall continuation)
    (change-font-path (new-path)
      :report (lambda (stream) (format stream "Retry with alternate truetype font path"))
      :interactive (lambda ()
                     (format *query-io* "Enter new value: ")
                     (list (read-line)))
      (setf *truetype-font-path* new-path)
      (invoke-with-truetype-path-restart continuation))))


;;; predefined paths (registers all found ttf fonts)

(defun default-font/family-map ()
  (when (null *truetype-font-path*)
    (return-from default-font/family-map nil))
  (flet ((try-ttf (name)
           ;; probe for files existance - if they do not exist our
           ;; mapping is futile and we must try `fc-match'.
           (if-let ((path (probe-file
                           (merge-pathnames name *truetype-font-path*))))
             path
             (progn
               (warn "~s doesn't exist" (merge-pathnames name *truetype-font-path*))
               (return-from default-font/family-map)))))
    `(((:fix :roman) .                 ,(try-ttf "DejaVuSansMono.ttf" ))
      ((:fix :italic) .                ,(try-ttf "DejaVuSansMono-Oblique.ttf"))
      ((:fix (:bold :italic)) .        ,(try-ttf "DejaVuSansMono-BoldOblique.ttf"))
      ((:fix (:italic :bold)) .        ,(try-ttf "DejaVuSansMono-BoldOblique.ttf"))
      ((:fix :bold) .                  ,(try-ttf "DejaVuSansMono-Bold.ttf"))
      ((:serif :roman) .               ,(try-ttf "DejaVuSerif.ttf"))
      ((:serif :italic) .              ,(try-ttf "DejaVuSerif-Italic.ttf"))
      ((:serif (:bold :italic)) .      ,(try-ttf "DejaVuSerif-BoldItalic.ttf"))
      ((:serif (:italic :bold)) .      ,(try-ttf "DejaVuSerif-BoldItalic.ttf"))
      ((:serif :bold) .                ,(try-ttf "DejaVuSerif-Bold.ttf"))
      ((:sans-serif :roman) .          ,(try-ttf "DejaVuSans.ttf"))
      ((:sans-serif :italic) .         ,(try-ttf "DejaVuSans-Oblique.ttf"))
      ((:sans-serif (:bold :italic)) . ,(try-ttf "DejaVuSans-BoldOblique.ttf"))
      ((:sans-serif (:italic :bold)) . ,(try-ttf "DejaVuSans-BoldOblique.ttf"))
      ((:sans-serif :bold) .           ,(try-ttf "DejaVuSans-Bold.ttf")))))


;;; `fc-match' implementation

(defparameter *family-names*
  '((:serif      . "Serif")
    (:sans-serif . "Sans")
    (:fix        . "Mono")))

(defparameter *fontconfig-faces*
  '((:roman . "")
    (:bold  . "bold")
    (:italic . "oblique")
    ((:bold :italic) . "bold:oblique")))

(defun parse-fontconfig-output (s)
  (let* ((match-string (concatenate 'string (string #\Tab) "file:"))
         (matching-line
          (loop for l = (read-line s nil nil)
                while l
                if (= (mismatch l match-string) (length match-string))
                   do (return l)))
         (filename (when matching-line
                     (probe-file
                      (subseq matching-line
                              (1+ (position #\" matching-line :from-end nil :test #'char=))
                              (position #\" matching-line :from-end t   :test #'char=))))))
    (when filename
      (parse-namestring filename))))

(defun warn-about-unset-font-path ()
  (cerror "Proceed"
          "~%~%NOTE:~%~
* McCLIM was unable to configure itself automatically using
  fontconfig. Therefore you must configure it manually.~%"))

(defun find-fontconfig-font (font-fc-name)
  (multiple-value-bind (output errors code)
      (uiop:run-program (list "fc-match" "-v" font-fc-name)
			:output :string :input nil :error-output nil
			:force-shell t :ignore-error-status t)
    (declare (ignore errors))
    (if (not (zerop code))
	(warn "~&fc-match failed with code ~D.~%" code)
	(with-input-from-string (stream output)
	  (parse-fontconfig-output stream)))))

(defun fontconfig-name (family face)
  (format nil "~A:~A" family face))

(defun build-font/family-map (&optional (families *family-names*))
  (loop for family in families nconcing
    (loop for face in *fontconfig-faces*
          as filename = (find-fontconfig-font (fontconfig-name (cdr family) (cdr face)))
          when (null filename) do (return-from build-font/family-map nil)
          collect
          (cons (list (car family) (car face)) filename))))


;;; configure fonts

(defun autoconfigure-fonts ()
  (invoke-with-truetype-path-restart
   (lambda ()
     (check-type *truetype-font-path* pathname)
     (if-let ((map (or (support-map-p (default-font/family-map))
                       (support-map-p (build-font/family-map)))))
       (setf *families/faces* map)
       (warn-about-unset-font-path)))))

(defun support-map-p (font-map)
  (handler-case
      (when (every #'(lambda (font)
                       (zpb-ttf:with-font-loader (ignored (cdr font)) t))
                   font-map)
        font-map)
    (zpb-ttf::bad-magic () nil)))
