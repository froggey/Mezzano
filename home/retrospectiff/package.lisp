
(cl:defpackage #:retrospectiff.constants
  (:export #:+field-type-slong+
           #:+x-resolution-tag+
           #:+field-type-sshort+
           #:+field-type-undefined+
           #:+photometric-interpretation-tag+
           #:+photometric-interpretation-palette-color+
           #:+field-type-long+
           #:+lzw-compression+
           #:+samples-per-pixel-tag+
           #:+packbits-compression+
           #:+planar-configuration-chunky+
           #:+field-type-sbyte+
           #:+field-type-srational+
           #:+resolution-unit-tag+
           #:+no-compression+
           #:+planar-configuration-tag+
           #:+image-width-tag+
           #:+bits-per-sample-tag+
           #:+field-type-float+
           #:+predictor-tag+
           #:+field-type-rational+
           #:+deflate-compression+
           #:+planar-configuration-planar+
           #:+image-length-tag+
           #:+horizontal-differencing+
           #:+field-type-short+
           #:+field-type-double+
           #:+strip-byte-counts-tag+
           #:+field-type-ascii+
           #:+strip-offsets-tag+
           #:+photometric-interpretation-white-is-zero+
           #:+icc-profile-tag+
           #:+photometric-interpretation-rgb+
           #:+old-style-jpeg-compression+
           #:+photometric-interpretation-black-is-zero+
           #:+color-map-tag+
           #:+y-resolution-tag+
           #:+jpeg-tables+
           #:+rows-per-strip-tag+
           #:+exif-tag+
           #:+jpeg-compression+
           #:+compression-tag+
           #:+field-type-byte+))

(cl:defpackage #:retrospectiff.globals
  (:use #:cl)
  (:export #:*byte-order*
           #:*tiff-file-offset*))

(cl:defpackage #:retrospectiff.binary-types
  (:use #:cl
        #:com.gigamonkeys.binary-data
        #:com.gigamonkeys.binary-data.common-datatypes
        #:retrospectiff.constants
        #:retrospectiff.globals)
  (:import-from #:flexi-streams #:make-in-memory-input-stream)
  (:shadow #:rational #:numerator #:denominator)
  (:export #:tiff-fields

           #:ifd
           
           #:ifd-entry
           #:tag
           #:data

           #:entries
           #:entry-count
           #:entry-bytes
           
           #:byte-ifd-entry
           #:ascii-ifd-entry
           #:short-ifd-entry
           #:long-ifd-entry
           #:rational-ifd-entry

           #:rational
           #:numerator
           #:denominator

           #:ifd-list
           #:ifd-offset

           #:convert-to-signed-integer
           #:convert-to-unsigned-integer

           #:read-bytes
           #:read-value

           #:write-value))

(cl:defpackage #:retrospectiff.util
  (:use #:cl)
  (:export #:string-contents-of-stream
           #:string-contents-of-file
           #:vector-contents-of-stream
           #:vector-contents-of-file
           #:ensure-array-size-and-set-fill-pointer
           #:remove-keyword-args))

(cl:defpackage #:retrospectiff.bit-array
  (:use #:cl)
  (:export #:set-bits
           #:get-bits))

(cl:defpackage #:retrospectiff.compression
  (:use #:cl
        #:retrospectiff.constants
        #:retrospectiff.bit-array
        #:retrospectiff.binary-types
        #:retrospectiff.util)
  (:shadowing-import-from #:retrospectiff.binary-types
                          #:rational
                          #:numerator
                          #:denominator)
  (:export #:find-compression-decoder
           #:image-info
           #:jpeg-image-info))

(cl:defpackage #:retrospectiff.image
  (:use #:cl)
  (:export #:tiff-image
           #:tiff-image-length
           #:tiff-image-width
           #:tiff-image-samples-per-pixel
           #:tiff-image-bits-per-sample
           #:tiff-image-data
           #:tiff-image-byte-order
           #:tiff-image-color-map
           #:tiff-image-min-is-white))

(cl:defpackage #:retrospectiff.ifd
  (:use #:cl
        #:retrospectiff.constants
        #:retrospectiff.globals
        #:retrospectiff.binary-types
        #:retrospectiff.image)
  (:shadowing-import-from #:retrospectiff.binary-types
                           #:rational
                           #:numerator
                           #:denominator)
  (:export #:make-tiff-fields
           #:get-ifd-value
           #:get-ifd-values))

(cl:defpackage #:retrospectiff
  (:use #:cl
        #:retrospectiff.constants
        #:retrospectiff.globals
        #:retrospectiff.binary-types
        #:retrospectiff.compression
        #:retrospectiff.ifd
        #:retrospectiff.image
        #:retrospectiff.util)
  (:nicknames #:tiff)
  (:shadowing-import-from #:retrospectiff.binary-types
                          #:rational
                          #:numerator
                          #:denominator)
  (:export #:read-tiff-stream
           #:read-tiff-file

           #:write-tiff-stream
           #:write-tiff-file

           #:tiff-image
           #:tiff-image-length
           #:tiff-image-width
           #:tiff-image-samples-per-pixel
           #:tiff-image-bits-per-sample
           #:tiff-image-data
           #:tiff-image-byte-order
           #:tiff-image-color-map
           #:tiff-image-min-is-white))

(cl:defpackage #:retrospectiff2
  (:use #:cl
        #:opticl-core
        #:retrospectiff.constants
        #:retrospectiff.globals
        #:retrospectiff.binary-types
        #:retrospectiff.compression
        #:retrospectiff.ifd
        #:retrospectiff.image
        #:retrospectiff.util)
  (:nicknames #:tiff2)
  (:shadowing-import-from #:retrospectiff.binary-types
                          #:rational
                          #:numerator
                          #:denominator)
  (:export #:read-tiff-stream
           #:read-tiff-file

           #:write-tiff-stream
           #:write-tiff-file

           #:tiff-image
           #:tiff-image-length
           #:tiff-image-width
           #:tiff-image-samples-per-pixel
           #:tiff-image-bits-per-sample
           #:tiff-image-data
           #:tiff-image-byte-order
           #:tiff-image-color-map
           #:tiff-image-min-is-white))


