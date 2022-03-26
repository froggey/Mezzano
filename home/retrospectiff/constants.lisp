
(cl:in-package :retrospectiff.constants)

(cl:defconstant +image-width-tag+ 256)
(cl:defconstant +image-length-tag+ 257)
(cl:defconstant +bits-per-sample-tag+ 258)
(cl:defconstant +compression-tag+ 259)
(cl:defconstant +photometric-interpretation-tag+ 262)
(cl:defconstant +strip-offsets-tag+ 273)
(cl:defconstant +samples-per-pixel-tag+ 277)
(cl:defconstant +rows-per-strip-tag+ 278)
(cl:defconstant +rows-per-strip-tag+ 278)
(cl:defconstant +strip-byte-counts-tag+ 279)
(cl:defconstant +x-resolution-tag+ 282)
(cl:defconstant +y-resolution-tag+ 283)
(cl:defconstant +planar-configuration-tag+ 284)
(cl:defconstant +resolution-unit-tag+ 296)
(cl:defconstant +predictor-tag+ 317)
(cl:defconstant +color-map-tag+ 320)
(cl:defconstant +jpeg-tables+ 347)

(cl:defconstant +planar-configuration-chunky+ 1)
(cl:defconstant +planar-configuration-planar+ 2)

(cl:defconstant +photometric-interpretation-white-is-zero+ 0)
(cl:defconstant +photometric-interpretation-black-is-zero+ 1)
(cl:defconstant +photometric-interpretation-rgb+ 2)
(cl:defconstant +photometric-interpretation-palette-color+ 3)

(cl:defconstant +horizontal-differencing+ 2)

(cl:defconstant +no-compression+ 1)
(cl:defconstant +lzw-compression+ 5)
(cl:defconstant +old-style-jpeg-compression+ 6)
(cl:defconstant +jpeg-compression+ 7)
(cl:defconstant +deflate-compression+ 8)
(cl:defconstant +packbits-compression+ #x8005)


(cl:defconstant +field-type-byte+ 1)
(cl:defconstant +field-type-ascii+ 2)
(cl:defconstant +field-type-short+ 3)
(cl:defconstant +field-type-long+ 4)
(cl:defconstant +field-type-rational+ 5)
(cl:defconstant +field-type-sbyte+ 6)
(cl:defconstant +field-type-undefined+ 7)
(cl:defconstant +field-type-sshort+ 8)
(cl:defconstant +field-type-slong+ 9)
(cl:defconstant +field-type-srational+ 10)
(cl:defconstant +field-type-float+ 11)
(cl:defconstant +field-type-double+ 12)

(cl:defconstant +exif-tag+ 34665)
(cl:defconstant +icc-profile-tag+ 34675)

;; NOTE: to get the symbols in constants.lisp, do 
;;
;; (cl:do-symbols (sym :retrospectiff.constants)
;;   (cl:format t "~&#:~A" (string-downcase (symbol-name sym))))
