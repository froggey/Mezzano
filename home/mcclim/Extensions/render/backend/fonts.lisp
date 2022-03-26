(in-package :mcclim-render-internals)

(defclass render-truetype-font (mcclim-truetype::cached-truetype-font)
  ())

(defmethod font-generate-glyph ((font render-truetype-font) code
                                &optional (tr +identity-transformation+))
  (let ((character (code-char (ldb (byte #.(ceiling (log char-code-limit 2)) 0) code)))
        (next-character (code-char (ldb (byte #.(ceiling (log char-code-limit 2))
                                              #.(ceiling (log char-code-limit 2)))
                                        code))))
    (multiple-value-bind (arr left top width height dx dy udx udy)
        (glyph-pixarray font character next-character
                        (compose-transformations #1=(make-scaling-transformation 1.0 -1.0)
                                                 (compose-transformations tr #1#)))
      (let ((right (+ left (1- (array-dimension arr 1))))
            (bottom (- top (1- (array-dimension arr 0)))))
        (glyph-info code arr width height left right top bottom dx dy udx udy)))))
