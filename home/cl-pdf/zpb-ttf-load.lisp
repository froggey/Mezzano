;;; cl-pdf copyright 2002-2013 Marc Battyani and contributors see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.com
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;; Use the zpb-ttf library to load Unicode TrueType fonts into cl-pdf
;;; Copyright (c) 2006 Peter Heslin, All Rights Reserved

(in-package #:pdf)

;; Missing: weight, char-width, version, mapping-scheme, esc-char,
;; character-set, vvector, fixed-v-p, cap-height, x-height.

;; Value of descender is different from that obtained via ufm file.
;; For leading should we use line-gap or the formula used for afm files?

;; Usage: (load-ttf-font "times.ttf")

(defun read-ttf-metrics (file)
  (zpb-ttf:with-font-loader (loader file)
    (let* ((font-metrics (make-instance 'ttu-font-metrics))
           (characters (characters font-metrics))
           (kernings (kernings font-metrics))
           (units (/ 1000 (zpb-ttf:units/em loader)))
           (italic-angle (* units (zpb-ttf:italic-angle loader)))
           (italic-sin (sin (/ (* pi italic-angle) -180)))
           (font-bbox  (map 'vector (lambda (x) (* 0.001 units x))
                            (zpb-ttf:bounding-box loader)))
           (min-code #xfffe)
           (max-code 0)
           (void-char (make-instance 'char-metrics :code -1 :name "VoidChar" :index 0 
                                     :width 0 :bbox #(0 0 0 0) :spacing 0))
           encoding-vector pdf-widths)

      (setf (gethash "VoidCharacter" characters) void-char)

      (dotimes (gid (zpb-ttf:glyph-count loader))
        (let ((g (zpb-ttf:index-glyph gid loader)))
          (when g
            (let* ((code (or (zpb-ttf:code-point g) -1))
                   (name (zpb-ttf:postscript-name g))
                   (width (* 0.001 units (zpb-ttf:advance-width g)))
                   (char-bbox (zpb-ttf:bounding-box g))
                   (bbox (if char-bbox
                             (map 'vector (lambda (x) (* units 0.001 x)) char-bbox)
                             font-bbox))
                   (urx (aref bbox 2))
                   (stroke-width (if bbox (if (zerop urx) width (* 0.001 urx)) 0))
                   (char-metrics
                    (make-instance 'char-metrics :code code :name name
                                   :index gid :width  width
                                   :bbox bbox :spacing (- width stroke-width)
                                   :left-italic-correction (if bbox (* italic-sin (aref bbox 3)) 0)
                                   :right-italic-correction (if bbox (* italic-sin (aref bbox 1)) 0))))
              (when name
                (setf (gethash name characters) char-metrics))

              ;; Code taken from read-ufm-file
              (when (and (<= 0 code #xfffe))
                (when (> code max-code) (setf max-code code))
                (when (< code min-code) (setf min-code code))
                (setf (aref (c2g font-metrics) (* 2 code))
                      (code-char (ldb (byte 8 8) gid))
                      (aref (c2g font-metrics) (+ (* 2 code) 1))
                      (code-char (ldb (byte 8 0) gid)))
                (vector-push-extend code (cid-widths font-metrics))
                (vector-push-extend (vector (round (* 1000 (width char-metrics))))
                                    (cid-widths font-metrics)))))))

      (flet ((register-kern-pair (name1 name2 dx dy)
               (let* ((char1 (gethash name1 characters))
                      (char2 (when char1 (gethash name2 characters))))
                 (when char2
                   (setf (gethash (cons char1 char2) kernings) (cons (* 0.001 dx) (* 0.001 dy)))))))
        (dolist (kern (zpb-ttf:all-kerning-pairs loader))
          (register-kern-pair (zpb-ttf:postscript-name (first kern))
                              (zpb-ttf:postscript-name (second kern))
                              (third kern) 0)))

      (setf (font-name font-metrics) (zpb-ttf:postscript-name loader)
            (full-name font-metrics) (zpb-ttf:full-name loader)
            (family-name font-metrics) (zpb-ttf:family-name loader)
            (underline-position font-metrics) (* 0.001 units (zpb-ttf:underline-position loader))
            (underline-thickness font-metrics) (* units 0.001 (zpb-ttf:underline-thickness loader))
            (italic-angle font-metrics) italic-angle
            (italic-sin font-metrics) italic-sin
            (fixed-pitch-p font-metrics) (zpb-ttf:fixed-pitch-p loader)
            (font-bbox font-metrics) font-bbox
            (notice font-metrics) (zpb-ttf:name-entry-value :copyright-notice loader)
            (ascender font-metrics) (* 0.001 units (zpb-ttf:ascender loader))
            (descender font-metrics) (* 0.001 (zpb-ttf:descender loader))
;            (leading font-metrics) (- 1 (descender font-metrics))
            (leading font-metrics) (zpb-ttf:line-gap loader)
            (encoding-scheme font-metrics) :unicode-encoding
            (characters font-metrics) characters
            (kernings font-metrics) kernings)

      ;; From read-ufm-file
      (setf encoding-vector (make-array (1+ max-code) :initial-element void-char)
            pdf-widths (make-array (1+ max-code) :initial-element 0))
      (iter (for (name char-metrics) in-hashtable (characters font-metrics))
            (for code = (code char-metrics))
            (when (<= min-code code max-code)
              (setf (aref encoding-vector code) char-metrics
                    (aref pdf-widths code) (round (* 1000 (width char-metrics))))))
      (setf (min-code font-metrics) min-code
            (max-code font-metrics) max-code
            (encoding-vector font-metrics) encoding-vector
            (pdf-widths font-metrics) pdf-widths
            (encoding-scheme font-metrics) :unicode-encoding
            (gethash (string-downcase (font-name font-metrics)) *font-metrics*) font-metrics)
      font-metrics)))

(defun load-ttf-font (ttf-file)
  (let ((ttffm (read-ttf-metrics ttf-file)))
    (with-open-file (in ttf-file :direction :input :element-type '(unsigned-byte 8))
      (setf (length1 ttffm)
            (file-length in)
            (binary-data ttffm)
            (make-array (length1 ttffm) :element-type '(unsigned-byte 8) :initial-element 0))
      (read-sequence (binary-data ttffm) in))
    ttffm))
