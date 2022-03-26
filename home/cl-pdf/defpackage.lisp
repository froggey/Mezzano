;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:cl-user)

(defpackage :pdf
  (:nicknames :cl-pdf)
  (:use :common-lisp :iterate)
  (:export
   #:*compress-streams* #:*zlib-search-paths* #:*zlib-loaded* #:load-zlib
   #:*max-number-of-pages* #:max-number-of-pages-reached
   #:*afm-files-directories* #:*default-encoding*
   #:*char-single-byte-codes* #:*default-charset*
   #:*letter-portrait-page-bounds* #:*letter-landscape-page-bounds*
   #:*a4-portrait-page-bounds* #:*a4-landscape-page-bounds*
   #:*default-page-bounds*
   #:bounds #:*load-images-lazily*
   #:load-fonts #:get-font #:font #:clear-font-cache #:name #:encoding #:font-metrics #:add-font-to-page
   #:font-name #:full-name #:family-name #:weight #:font-bbox #:*version* #:notice #:encoding-scheme
   #:characters #:code #:width #:bbox #:kernings #:get-font-descender
   #:get-char #:get-char-width #:get-char-size
   #:get-kerning #:hyphen-char #:mapping-scheme
   #:get-char-italic-correction #:get-font-italic-correction
   #:esc-char #:character-set #:base-font-p #:vvector #:fixed-v-p #:cap-height #:x-height
   #:ascender #:descender #:char-metrics #:char-width #:font-metrics #:read-afm-file
   #:*document* #:*page* #:with-document #:with-page #:image #:add-images-to-page
   #:write-document #:set-font #:in-text-mode #:move-text #:draw-text #:show-text #:show-char
   #:show-text-on-next-line #:draw-text-on-next-line
   #:show-spaced-strings #:draw-spaced-strings
   #:set-text-rendering-mode #:set-char-spacing #:set-text-x-scale #:set-text-leading
   #:set-text-rise #:move-to-next-line #:set-text-matrix #:draw-and-adjust-string
   #:with-saved-state #:set-transform-matrix #:translate #:rotate #:scale #:skew #:rotate* #:skew*
   #:set-line-width #:set-line-cap #:set-line-join #:set-dash-pattern
   #:set-miter-limit #:move-to #:line-to #:bezier-to #:bezier2-to #:bezier3-to
   #:arc #:pie #:circle #:ellipse #:rectangle #:polyline #:regular-polygon #:star
   #:close-path #:basic-rect #:stroke #:close-and-stroke #:fill-path #:close-and-fill #:even-odd-fill
   #:fill-and-stroke #:even-odd-fill-and-stroke #:close-fill-and-stroke
   #:close-even-odd-fill-and-stroke #:end-path-no-op #:clip-path #:even-odd-clip-path
   #:set-gray-stroke #:set-gray-fill #:set-rgb-stroke #:set-rgb-fill #:set-cymk-stroke
   #:set-color-stroke #:set-color-fill
   #:set-transparency #:set-fill-transparency #:set-stroke-transparency
   #:set-cymk-fill #:paint-image #:draw-image #:get-named-reference #:register-page-reference
   #:add-link #:add-URI-link #:add-external-link #:read-jpeg-file #:make-jpeg-image
   #:make-image #:width #:height
   #:enter-outline-level #:close-outline-level #:with-outline-level
   #:draw-centered-text #:draw-left-text #:draw-right-text
   #:vertical-value-axis #:horizontal-histo-axis #:legend #:histogram #:draw-object
   #:pie-chart #:plot-xy
   #:load-t1-font #:load-ttu-font #:load-ttf-font #:encoding #:get-encoding
   #:draw-bar-code128 #:*page-number*
   #:test-template #:make-template-from-page #:add-templates-to-page #:draw-template
   #:image-file-parse-error #:initialize! #:confirm-afm-files-directories))
