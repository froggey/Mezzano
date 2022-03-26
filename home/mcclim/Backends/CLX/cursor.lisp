(in-package :clim-clx)

(defvar *clx-cursor-mapping*  
  '(;; These are taken from the Franz CLIM User's Guide
    (:busy 150)
    (:button 60)
    (:default 68)
    (:horizontal-scroll 108)
    (:horizontal-thumb 108)
    (:lower-left 12)
    (:lower-right 14)
    (:move 52)
    (:position 130)
    (:prompt 152)
    (:scroll-down 106)
    (:scroll-left 110)
    (:scroll-right 112)
    (:scroll-up 114)
    (:upper-left 134)
    (:upper-right 136)
    (:vertical-scroll 116)
    (:vertical-thumb 116)
    ;; The following are not in the Franz docs, but might be useful.
    (:i-beam 152)
    (:vertical-pointer 22)
    (:pencil 86)
    (:rotate 50)    
    (:choose 60)))

(defun make-cursor-table (port)
  (declare (optimize (safety 3) (debug 3) (speed 0) (space 0)))
  (let ((font (xlib:open-font (clx-port-display port) "cursor")))
    
    (loop for (symbol code) in *clx-cursor-mapping*
          do (setf (gethash symbol (clx-port-cursor-table port))
                   (xlib:create-glyph-cursor :foreground (xlib:make-color :red 0.0 :green 0.0 :blue 0.0)
                                             :background (xlib:make-color :red 1.0 :green 1.0 :blue 1.0)
                                             :source-font font
                                             :source-char code
                                             :mask-font font
                                             :mask-char (1+ code))))
    (xlib:close-font font)))
