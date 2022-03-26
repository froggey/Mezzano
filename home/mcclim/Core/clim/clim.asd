
(defsystem #:clim
  :depends-on (#:clim-core #:drei-mcclim)
  :components ((:file "input-editing-drei")
               (:file "text-editor-gadget")))
