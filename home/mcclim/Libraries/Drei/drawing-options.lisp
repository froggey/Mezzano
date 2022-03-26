;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2008 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; A bunch of predefined drawing options, styles and faces to make
;;; syntax highlighting rules more elegant.

(in-package :drei)

;;; Some general styles.

(defvar +roman-face+ (make-face :style (make-text-style nil :roman nil))
  "A face specifying a roman style, but with unspecified family
and size.")

(defvar +italic-face+ (make-face :style (make-text-style nil :italic nil))
  "A face specifying an italic style, but with unspecified
family and size.")

(defvar +bold-face+ (make-face :style (make-text-style nil :bold nil))
  "A face specifying a boldface style, but with unspecified
family and size.")

(defvar +bold-italic-face+ (make-face :style (make-text-style nil :bold nil))
  "A face specifying an italic boldface style, but with
unspecified family and size.")

;;; ...and their drawing options.

(defvar +roman-face-drawing-options+ (make-drawing-options :face +roman-face+)
  "Options used for drawing with a roman face.")

(defvar +italic-face-drawing-options+ (make-drawing-options :face +italic-face+)
  "Options used for drawing with an italic face.")

(defvar +bold-face-drawing-options+ (make-drawing-options :face +bold-face+)
  "Options used for drawing with boldface.")

(defvar +bold-italic-face-drawing-options+ (make-drawing-options :face +bold-italic-face+)
  "Options used for drawing with italic boldface.")

;;; Some drawing options for specific syntactical elements,
;;; approximately like GNU Emacs. These are not constants, as users
;;; may want to change them to fit their colour scheme. Of course,
;;; syntax highlighting rules are free to ignore these, but I think
;;; the default rules should at least use these.

(defvar *keyword-drawing-options*
  (make-drawing-options :face (make-face :ink +red3+))
  "The drawing options used for drawing the syntactical
equivalent of keyword symbols. In Lisp, this is used for keyword
symbols.")

(defvar *special-operator-drawing-options*
  (make-drawing-options :face (make-face :ink +steel-blue+
                                         :style (make-text-style nil :bold nil)))
  "The drawing options used for drawing the syntactical
equivalent of special operators. In Lisp, this is used for macros
and special operators, in most other languages, it should
probably be used for language keywords.")

(defvar *special-variable-drawing-options*
  (make-drawing-options :face (make-face :ink +darkgoldenrod4+))
  "The drawing options used for drawing variables that are
somehow special. In Lisp, this is used for globally bound
non-constant variables with dynamic scope. In other language, it
should probably be used for global variables or similar.")

(defvar *string-drawing-options*
  (make-drawing-options :face (make-face :ink +green4+))
  "The drawing options used for syntax-highlighting strings.")

(defvar *comment-drawing-options*
  (make-drawing-options :face (make-face :ink +maroon+
                                         :style (make-text-style nil :bold nil)))
  "The drawing options used for drawing comments in source
code.")

(defvar *error-drawing-options*
  (make-drawing-options :face (make-face :ink +red+))
  "The drawing options used for drawing syntax errors.")
