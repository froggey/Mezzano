(cl:in-package #:asdf-user)

#| dummy system to make Quicklisp happy |#
(defsystem #:mcclim-fonts
  :depends-on (#:clim-basic)
  :components ((:file "common")))

(defsystem #:mcclim-fonts/truetype
    :depends-on (#:clim-basic #:zpb-ttf #:cl-vectors #:cl-paths-ttf #:cl-aa #:alexandria)
    :components ((:static-file "README.md")
                 (:file "truetype-package")
                 (:file "fontconfig" :depends-on ("truetype-package"))
                 (:file "mcclim-native-ttf" :depends-on ("truetype-package"))))

(defmethod perform :after ((o load-op)
                           (s (eql (find-system :mcclim-fonts/truetype))))
  (uiop:symbol-call :mcclim-truetype :autoconfigure-fonts))

(defsystem #:mcclim-fonts/clx-truetype
  :depends-on (#:mcclim-fonts/truetype #:mcclim-clx)
  :components ((:file "xrender-fonts")))

(defsystem #:mcclim-fonts/clx-freetype
  :depends-on (#:mcclim-fonts #:mcclim-clx #:cl-freetype2 #:mcclim-fontconfig #:mcclim-harfbuzz)
  :components ((:file "freetype")))
