
(cl:defpackage :mcclim-truetype
  (:use :climi :clim :clim-lisp)
  (:import-from :alexandria
                :ensure-gethash
                :when-let
                :if-let)
  (:export :*truetype-font-path*
           :*family-names*
           :*fontconfig-faces*
           :truetype-device-font-name 
           :fontconfig-font-name
           :make-truetype-device-font-name 
           :make-fontconfig-font-name))
