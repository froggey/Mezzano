
(ql:quickload :mcclim)
(ql:quickload :mcclim-clx-fb)

(setf clim:*default-server-path*  (list :clx-fb :host ""
					:protocol :unix
					:display-id 0))


(ql:quickload :clim-examples)
;;(CLIM-DEMO:DEMODEMO)

#|
(clim:destroy-port (clim:find-port))
(maphash #'(lambda (key val)
		      (when 
			  (typep key 'mcclim-render::image-sheet-mixin)
			(mcclim-render::save-sheet-image-to-file key "/tmp/aa.png" nil)(format t "~A ~A~%" key val)))
		  (slot-value (clim:find-port) 'climi::sheet->mirror))

|#
