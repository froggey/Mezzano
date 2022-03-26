#+lispworks
(lw:defsystem "CL-PDF"
  (:default-pathname "")
  :members ("defpackage"
	    "zlib"
	    "pdf"
	    "pdf-base"
	    "pdf-geom"
	    "font"
	    "text"
	    "chart"
	    )
  :rules
  ((:in-order-to :compile :all (:requires (:load :previous)))
   (:in-order-to :load :all (:requires (:load :previous))))
  )


#+allegro
(excl:defsystem :cl-pdf
    ()
  (:serial
   "defpackage"
   "zlib"
   "pdf"
   "pdf-base"
   "pdf-geom"
   "font"
   "text"
   "chart"
   ))

#+CMU
(mk:defsystem :cl-pdf
    :components
  ((:file "defpackage")
   (:file "zlib")
   (:file "pdf")
   (:file "pdf-base")
   (:file "pdf-geom")
   (:file "font")
   (:file "text")
   (:file "chart")
   ))
