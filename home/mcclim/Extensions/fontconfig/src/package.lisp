(defpackage :mcclim-fontconfig
  (:use :cl)
  (:documentation "CFFI interface to Fontconfig")
  (:export #:get-version
           #:init-fontconfig
           #:init-reinitialize
           #:init-bring-up-to-date
           #:match-font
           #:font-list
           #:list-font-dirs
           #:app-font-add-dir
           #:app-font-add-file
           #:charset-contains-char-p
           #:font-render-prepare
           #:font-render-prepare-match
           #:query-freetype))
