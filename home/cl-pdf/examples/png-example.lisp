;;;
;;; PNG imaging example
;;;

(defvar *test-png-pathname* (when *load-pathname*
                               (merge-pathnames #P"get_adobe_reader.png" *load-pathname*)))

(defun ex-png (&optional (file #P"/tmp/ex-png.pdf"))
  (pdf:with-document ()
    (let ((png-image (pdf::make-image *test-png-pathname*))
          (pal-opaq  (pdf::make-image(merge-pathnames "ys-pal-opaq" *test-png-pathname*)))
          (gray-opaq (pdf::make-image(merge-pathnames "ys-gray-opaq" *test-png-pathname*)))
          (mono-opaq (pdf::make-image(merge-pathnames "ys-mono-opaq" *test-png-pathname*)))
          (pal-tran  (pdf::make-image(merge-pathnames "ys-pal-tran" *test-png-pathname*)))
          (gray-tran (pdf::make-image(merge-pathnames "ys-gray-tran" *test-png-pathname*)))
          (mono-tran (pdf::make-image(merge-pathnames "ys-mono-tran" *test-png-pathname*)))
	  (helvetica (pdf:get-font "Helvetica")))
      (pdf:with-page ()
        (pdf:add-images-to-page png-image
                                pal-opaq gray-opaq mono-opaq
                                pal-tran gray-tran mono-tran)
        (pdf:with-outline-level ("Page 1" (pdf:register-page-reference))
          (pdf:with-saved-state 
            (pdf:set-rgb-fill 1 1 0)
            (pdf:basic-rect 20 580 450 100)
            (pdf:fill-path))
          (pdf:in-text-mode
            (pdf:set-font helvetica 36.0)
            (pdf:move-text 100 800)
            (pdf:draw-text "PNG Images Test")
   
            (pdf:set-font helvetica 12.0)
            (pdf:move-text -10 -30)
            (pdf:draw-text "Palette-256              Gray                     Mono       - OPAQUE")
            (pdf:move-text 30 -20)
            (pdf:set-text-leading 20)
            (pdf:show-text-on-next-line "ABCDIFGHIJKLMNOPQRSTUVWXYZ")
            (pdf:show-text-on-next-line "abcdifghijklmnopqrstuvwxyz")

            (pdf:move-text -30 -40)
            (pdf:draw-text "Palette-256              Gray                      Mono     - TRANSPARENT")
            (pdf:move-text 30 -20)
            (pdf:set-text-leading 20)
            (pdf:show-text-on-next-line "ABCDIFGHIJKLMNOPQRSTUVWXYZ")
            (pdf:show-text-on-next-line "abcdifghijklmnopqrstuvwxyz")
          )
          (pdf:draw-image pal-opaq   30 700 125 66 0 t)
          (pdf:draw-image gray-opaq 180 700 100 66 0 t)
          (pdf:draw-image mono-opaq 300 700  75 66 0 t)
          (pdf:add-URI-link 30 700 125 66 "http://www.ystok.ru")

          (pdf:draw-image pal-tran   30 600 125 66 0 t)
          (pdf:draw-image gray-tran 180 600 100 66 0 t)
          (pdf:draw-image mono-tran 300 600  75 66 0 t)
          (pdf:add-URI-link 30 600 125 66 "http://www.ystok.ru")

          (pdf:draw-image png-image 30 10 88 31 0 t))))
    (pdf:write-document file)))

;(ex-png (current-pathname "ex-png.pdf"))
