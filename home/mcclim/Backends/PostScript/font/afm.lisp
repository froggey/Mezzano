;;; -*- Mode: Lisp; Package: CLIM-POSTSCRIPT -*-

;;;  (c) copyright 2002 by
;;;           Alexey Dejneka (adejneka@comail.ru)

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

;;; TODO:
;;;
;;; - It is ugly. Rewrite.
;;;
;;; - Kerning, ligatures.
;;; - Full AFM/AMFM/ACFM support.

(in-package :clim-postscript-font)

(defun space-char-p (char)
  (member char '(#\Space #\Tab)))

(defvar *stream*)
(defvar *line*)
(defvar *line-length*)
(defvar *position*)

(defun skip-whitespaces ()
  (let ((pos (position-if-not #'space-char-p *line* :start *position*)))
    (if pos
        (setq *position* pos)
        (setq *position* *line-length*))))

(defun assert-whitespace ()
  (or (= *line-length* *position*)
      (space-char-p (aref *line* *position*))))

(defun end-of-line-p ()
  (skip-whitespaces)
  (= *line-length* *position*))

(defun take-line ()
  (loop
     (setq *line* (read-line *stream*))
     (setq *line-length* (length *line*))
     (setq *position* 0)
     (skip-whitespaces)
     (unless (= *position* *line-length*)
       (return-from take-line))))

(defun take-name ()
  (skip-whitespaces)
  (let ((start *position*)
        (end (or (position-if #'space-char-p *line* :start *position*)
                 *line-length*)))
    (setq *position* end)
    (subseq *line* start end)))

(defun take-integer ()
  (skip-whitespaces)
  (let ((start *position*)
        (end (or (position-if #'space-char-p *line* :start *position*)
                 *line-length*)))
    (setq *position* end)
    (parse-integer *line* :start start :end end)))

(defun take-number ()
  (skip-whitespaces)
  ;; FIXME
  (let ((string (take-name)))
    (assert (every #'(lambda (char)
                       (or (digit-char-p char)
                           (member char '(#\- #\.))))
                   string))
    (let ((number (read-from-string string)))
      (assert (numberp number))
      number)))

(defun take-string ()
  (skip-whitespaces)
  (prog1
      (subseq *line* *position* *line-length*)
    (setq *position* *line-length*)))

(defun take-delimiter ()
  (assert (string= (take-name) ";")))

(defun skip-until-delimiter ()
  (let ((pos (position #\; *line* :start *position*)))
    (assert pos)
    (setq *position* pos)))

;;;
(defparameter *font-metrics-keywords*
  '(("EndFontMetrics" . :end-font-metrics)
    ("FontName" . :font-name)
    ("Ascender" . :ascender)
    ("Descender" . :descender)
    ("ItalicAngle" . :italic-angle)
    ("StartCharMetrics" . :start-char-metrics)))

(defparameter *char-metrics-keywords*
  '(("C" . :c)
    ("WX" . :wx)
    ("N" . :n)
    ("B" . :b)
    ("EndCharMetrics" . :end-char-metrics)))

(defun take-keyword (table)
  (let ((name (take-name)))
    (cdr (assoc name table :test #'equal))))

(defun read-char-metrics ()
  (loop for code = nil
     and name = nil
     and width = nil
     and ascent = nil
     and descent = nil
     and xmin = nil
     and xmax = nil
     do (take-line)
       (loop for keyword = (and (not (end-of-line-p))
				(take-keyword *char-metrics-keywords*))
	  while keyword
	  do (ecase keyword
	       ((nil) (skip-until-delimiter))
	       (:end-char-metrics
		(return-from read-char-metrics metrics))
	       (:c (setq code (take-number)))
	       (:wx (setq width (take-number)))
	       (:n (setq name (take-name)))
	       (:b (setq xmin (take-number))
		   (setq descent (- (take-number)))
		   (setq xmax (take-number))
		   (setq ascent (take-number))))
	    (take-delimiter))
     collect (list code name width ascent descent xmin xmax) into metrics))

(defun read-afm-stream (stream)
  (let (name ascent descent
        italic-angle
        char-infos)
    (let ((*stream* stream))
      (loop
         (take-line)
         (let ((keyword (take-keyword *font-metrics-keywords*)))
           (ecase keyword
             ((nil))
             (:end-font-metrics (return))
             (:font-name (setq name (take-string))) ; FIXME: repeated definition
             (:ascender (setq ascent (take-number))) ; FIXME: extra tokens
             (:descender (setq descent (- (take-number))))
             (:italic-angle (setq italic-angle (take-number)))
             (:start-char-metrics (setq char-infos (read-char-metrics)))))))
    (values name ascent descent italic-angle char-infos)))

;;;;

;;; FIXME: This function should be given a right name, exported from
;;; CLIM-POSTSCRIPT and documented in the manual.
(defun load-afm-file (afm-filename)
  "Loads font information from the specified AFM file."
  (multiple-value-call #'define-font-metrics
    (with-open-file (stream afm-filename)
      (read-afm-stream stream))))
