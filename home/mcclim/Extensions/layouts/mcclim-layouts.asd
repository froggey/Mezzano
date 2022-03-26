
#| dummy system to make Quicklisp happy |#
(defsystem #:mcclim-layouts)

(defsystem #:mcclim-layouts/tab
  :depends-on (#:clim)
  :components ((:file "tab-layout")))
