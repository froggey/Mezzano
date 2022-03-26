;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for details of the BSD style license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(defun example1 (&optional (file #P"/tmp/ex1.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let ((helvetica (pdf:get-font "Helvetica")))
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 1"))
	  (pdf:translate 230 500)
	  (loop repeat 150
	 for i = 0.67 then (* i 1.045)
	 do (pdf:in-text-mode
	     (pdf:set-font helvetica i)
	     (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
	     (pdf:move-text (* i 3) 0)
	     (pdf:show-text "cl-typesetting"))
	   (pdf:rotate 13)))))
    (pdf:write-document file)))

;; for the TrueType Example, you need to load the font first:
;; (read the unicode-readme for more info)
#+nil
(pdf:load-ttu-font #P"/tmp/times.ufm" #P"/tmp/times.ttf")

(defun example1-ttu (&optional (file #P"/tmp/ex1-ttu.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let ((helvetica (pdf:get-font "TimesNewRomanPSMT"))) ; The windows times font
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 1 with Unicode"))
	  (pdf:translate 230 500)
	  (loop repeat 150
	 for i = 0.67 then (* i 1.05)
	 do (pdf:in-text-mode
	     (pdf:set-font helvetica i)
	     (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
	     (pdf:move-text (* i 3) 0)
	     (pdf:show-text (format nil "Lisp lives! ~cx.~cy.x " (code-char 955)(code-char 955))))
	   (pdf:rotate 13)))))
    (pdf:write-document file)))

(defun example2 (&optional (file #P"/tmp/ex2.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let ((helvetica (pdf:get-font "Helvetica")))
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 2"))
	  (pdf:move-to (+ 10 (random 500))(+ 10 (random 400)))
	  (pdf:set-gray-fill 0.5)
	  (dotimes (i 50)
	    (pdf:line-to (+ 50 (random 500)) (+ 50 (random 400))))
	  (pdf:close-even-odd-fill-and-stroke)
	  (pdf:move-to (+ 50 (random 500))(+ 400 (random 400)))
	  (pdf:set-rgb-fill 0.5 0.5 0.8)
	  (pdf:set-rgb-stroke 0.9 0.5 0.1)
	  (dotimes (i 50)
	    (pdf:bezier2-to (+ 50 (random 500)) (+ 400 (random 400))
			    (+ 50 (random 500)) (+ 400 (random 400))))
	  (pdf:close-even-odd-fill-and-stroke))))
    (pdf:write-document file)))

(defun gen-image-bits ()
  (with-output-to-string (s)
     (loop for x from -10 to 10 by 1/10
	   do (loop for y from -10 to 10 by 1/10
		    do (format s "~2,'0x~2,'0x~2,'0x"
			  (round (+ 200 (* 55 (sin x))))
			  (round (+ 200 (* 55 (cos y))))
			  (round (+ 200 (* 55 (sin (+ x y))))))))))

(defun example3 (&optional (file #P"/tmp/ex3.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let* ((helvetica (pdf:get-font "Helvetica"))
	       (image (make-instance 'pdf:image
				     :bits (gen-image-bits)
				     :width 201 :height 201)))
	  (pdf:draw-bar-code128 "30A0033111436" 20 100)
	  (pdf:add-images-to-page image)
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 3"))
	  (pdf:with-saved-state
	      (pdf:translate 102 550)
	    (pdf:rotate 20)
	    (pdf:scale 200 125)
	    (pdf:paint-image image))
	  (pdf:with-saved-state
	      (pdf:translate 100 540)
	    (pdf:rotate -70)
	    (pdf:scale 300 200)
	    (pdf:paint-image image)))))
    (pdf:write-document file)))


;; logo

(defparameter *fractal-ratio* 0.8)
(defconstant +sin60+ (sin (/ pi 3)))
(defconstant +cos60+ (cos (/ pi 3)))
(defconstant +tg30+ (tan (/ pi 6)))
(defconstant +tg60-tg30+ (- (tan (/ pi 3))(tan (/ pi 6))))

(defun %fractal (x y dx dy level)
  (if (zerop level)
    (let ((dx/2 (* dx 0.5))
	  (dy/2 (* dy 0.5)))
      (pdf:move-to (- x dx/2) (- y dy/2))
      (pdf:line-to x (+ y dy/2))
      (pdf:line-to (+ x dx/2) (- y dy/2))
      (pdf:close-fill-and-stroke))
    (let* ((delta (- 1 *fractal-ratio*))
	   (delta05 (* 0.5 delta))
	   (ratio2 (- 1 delta05))
	   (deltax (* dx 0.25 (+ 1 (* 0.5 +sin60+ (- 1 ratio2)))))
	   (deltay (* dy 0.25 (+ 1 delta05)))
	   (dyf2 (* dy 0.5 (+ 1 delta05 )))
	   (dxf2 (* dx 0.5 (+ 1 delta05 ))))
      (decf level)
      (setf dx (* dx 0.5))
      (setf dy (* dy 0.5))
      (%down-fractal x (- y (* 1 dy)(* dx +tg30+ -1)(* 0.125 +tg60-tg30+ dxf2)) dxf2 dyf2 level)
      (%fractal x      (+ y (* dy 0.5)) (* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      (%fractal (+ x deltax)(- y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      (%fractal (- x deltax)(- y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      )))

(defun %down-fractal (x y dx dy level)
  (setf level 0)
  (if (zerop level)
    (let ((dx/2 (* dx 0.5))
	  (dy/2 (* dy 0.5)))
      (pdf:move-to (- x dx/2) (+ y dy/2))
      (pdf:line-to x (- y dy/2))
      (pdf:line-to (+ x dx/2)(+ y dy/2))
      (pdf:close-fill-and-stroke))
    (let* ((delta (- 1 *fractal-ratio*))
	   (delta05 (* 0.5 delta))
	   (ratio2 (- 1 delta05))
	   (deltax (* dx 0.25 (+ 1 (* 0.5 +sin60+ (- 1 ratio2)))))
	   (deltay (* dy 0.25 (+ 1 delta05)))
	   (dyf2 (* dy 0.5 (+ 1 delta05 )))
	   (dxf2 (* dx 0.5 (+ 1 delta05 ))))
      (decf level)
      (setf dx (* dx 0.5))
      (setf dy (* dy 0.5))
      (%fractal x (+ y (* 1 dy)(* dx +tg30+ -1)(* 0.125 +tg60-tg30+ dxf2)) dxf2 dyf2 level)
      (%down-fractal x      (- y (* dy 0.5)) (* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      (%down-fractal (+ x deltax)(+ y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      (%down-fractal (- x deltax)(+ y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      )
    ))

(defun fractal (x y l level)
  (let ((dx l)
	(dy (* l +sin60+)))
  (%fractal x y dx dy level)))

;the logo
(defun example4 (&optional (file #P"/tmp/ex4.pdf"))
  (pdf:with-document ()
    (loop for i from 1 to 7
	  do (pdf:with-page ()
	       (pdf:with-outline-level ((format nil "Page ~d" i)(pdf:register-page-reference))
		 (let* ((helvetica (pdf:get-font "Helvetica")))
		   (pdf:in-text-mode
		    (pdf:set-font helvetica 36.0)
		    (pdf:move-text 100 800)
		    (pdf:draw-text (format nil "cl-pdf: Example 4    page ~d" i)))
		   (pdf:set-rgb-stroke 1.0 1.0 1.0)
		   (pdf:set-rgb-fill 0.4 0.4 0.9)
		   (pdf:set-line-width 0.2)
		   (fractal 298 530 600 i)))))
    (pdf:write-document file)))

(defvar *dx* #(1 0 -1 0))
(defvar *dy* #(0 1 0 -1))

;make-maze
(defun example5 (&key (nx 100) (ny 150) (size 5) (file #P"/tmp/ex5.pdf"))
  (let ((x-stack '())
	(y-stack '())
	(visited (make-array (list nx ny) :initial-element nil))
	(v-walls (make-array (list nx ny) :initial-element t))
	(h-walls (make-array (list nx ny) :initial-element t))
	(x (random nx))
	(y (random ny))
	next-x next-y)
    (flet ((find-cell ()
	       (let ((tested (vector nil nil nil nil))
		     (nb-tested 0))
		 (loop while (< nb-tested 4)
		       for test = (random 4)
		       unless (svref tested test)
		       do (incf nb-tested)
		       (setf (svref tested test) t)
		       (setf next-x (+ x (svref *dx* test)))
		       (setf next-y (+ y (svref *dy* test)))
		       (when (and (>= next-x 0)(< next-x nx)(>= next-y 0)(< next-y ny)
				  (not (aref visited next-x next-y)))
			 (return-from find-cell t)))
		 nil)))
      (setf (aref visited x y) t)
      (loop with nb-visited = 1 and total-cells = (* nx ny)
	    while (< nb-visited total-cells)
	    do (if (find-cell)
		 (progn (push x x-stack)(push y y-stack)
			(if (/= next-x x)
			  (setf (aref h-walls (min x next-x) y) nil)
			  (setf (aref v-walls x (min y next-y)) nil))
			(setf x next-x y next-y)
			(setf (aref visited x y) t)
			(incf nb-visited))
		 (progn (setf x (pop x-stack) y (pop y-stack))))))
    (pdf:with-document ()
      (pdf:with-page ()
	(pdf:with-outline-level ("Example" (pdf:register-page-reference))
	  (pdf:translate (* 0.5 (- 595 (* nx size)))(* 0.5 (- 841 (* ny size))))
	  (setf (aref h-walls (1- nx) (random ny)) nil)
	  (pdf:move-to 0 0)
	  (pdf:line-to (*  nx size) 0)
	  (pdf:move-to 0 size)
	  (pdf:line-to 0 (* ny size))
	  (loop for x from 0 below nx
		for x0 = 0 then x1
		for x1 from size by size
		do (loop for y from 0 below ny
			 for y0 = 0 then y1
			 for y1 from size by size
			 do
			 (when (aref h-walls x y)
			   (pdf:move-to x1 y0)
			   (pdf:line-to x1 y1))
			 (when (aref v-walls x y)
			   (pdf:move-to x0 y1)
			   (pdf:line-to x1 y1)))
		(pdf:stroke))))
      (pdf:write-document file))))


(defun example6 (&optional (file #P"/tmp/ex6.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let ((helvetica (pdf:get-font "Helvetica")))
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 6"))
	  (pdf:set-rgb-stroke 0.1 0.1 0.1)
	  (pdf:set-rgb-fill 0.8 0.8 0.8)
	  (let ((x 50) (y 600))
	    (dotimes (i 2)
	      (pdf:rectangle x y 500 140 :radius 10)
	      (pdf:close-fill-and-stroke)
	      (setf y (- y 180))))
	  (pdf:translate 50 670)
	  (let ((x 50) (y 0))
	    (loop repeat 4
	      for i = 8 then (* i 1.05)
	      do
	      (pdf:set-rgb-fill (* 0.1 i) (* 0.01 i) (* 0.02 i))
	      (pdf:circle x y (* 4 i))
	      (pdf:close-fill-and-stroke)
	      (pdf:ellipse (+ x 250) y (* 5 i) (* 4 i))
	      (pdf:close-fill-and-stroke)
	      (setf x (+ x 50))))
	  (pdf:translate 0 -180)
	  (pdf:regular-polygon 150 0 50 7 :fillet-radius 8)
	  (pdf:close-fill-and-stroke)
	  (pdf:star 350 0 50 30 6 :fillet-radius 5)
	  (pdf:close-fill-and-stroke)

	  (pdf:set-rgb-fill 0.8 0.6 0.2)
	  (pdf:regular-polygon 150 0 30 5 :fillet-radius 4)
	  (pdf:close-fill-and-stroke)
	  (pdf:star 350 0 40 20 4 :fillet-radius 6)
	  (pdf:close-fill-and-stroke)

	  (pdf:set-rgb-fill 0.4 0.8 0.7)
	  (pdf:regular-polygon 150 0 15 3 :fillet-radius 3)
	  (pdf:close-fill-and-stroke)
	  (pdf:star 350 0 35 10 12 :fillet-radius 1)
	  (pdf:close-fill-and-stroke)
	  (pdf:set-line-width 0.5)
	  (loop for r from 2 to 100 by 2
		for start = (* pi 0.001 (random 2000))
		for length = (* pi 0.001 (random 2000))
		do (pdf:set-rgb-stroke (* 0.01 (random 100))(* 0.01 (random 100))(* 0.01 (random 100)))
		(pdf:arc 250 -230 r start length)
		(pdf:stroke)))))
    (pdf:write-document file)))

(defvar *test-jpeg-file-path* (when *load-pathname*
			   (merge-pathnames #P"banner.jpg" *load-pathname*)))

(unless *test-jpeg-file-path*
  (error "please set the *test-jpeg-file-path* variable to the banner.jpg file location"))

(defvar *test-jpeg* *test-jpeg-file-path*)

(defun example7 (&optional (file #P"/tmp/ex7.pdf"))
  (pdf:with-document ()
    (let ((jpg-image (pdf:make-jpeg-image *test-jpeg*))
	  (helvetica (pdf:get-font "Helvetica")))
      (pdf:with-outline-level ("Contents" "page 1")
	(pdf:with-page ()
	  (pdf:register-page-reference "page 1")
	  (pdf:with-outline-level ("Page 1" "page 1")
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 36.0)
	     (pdf:move-text 100 800)
	     (pdf:draw-text "cl-pdf: Example 7"))
	    (pdf:set-rgb-stroke 0.1 0.1 0.1)
	    (pdf:set-rgb-fill 0.6 0.6 0.8)
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 13.0)
	     (pdf:move-text 10 700)
	     (pdf:draw-text "Test for bookmarks, JPEG support, internal links, URI links and basic charts"))
	    (pdf:add-images-to-page jpg-image)
	    (pdf:draw-image jpg-image 10 10 239 50 0 t)
	    (pdf:add-URI-link 10 10 239 50 "http://www.fractalconcept.com/asp/html/cl-pdf.html" :border #(1 1 1))
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 10.0)
	     (pdf:move-text 500 10)
	     (pdf:draw-text "goto page 2"))
	    (pdf:add-link 495 8 80 14 "page 2")
	    (pdf:draw-object (make-instance 'pdf:histogram :x 200 :y 450 :width 300 :height 200
					    :label-names '("Winter" "Spring" "Summer" "Autumn")
					    :labels&colors '(("Serie 1" (1.0 0.0 0.0))
							     ("Serie 2" (0.0 1.0 0.0)))
					    :series '((42 46 48 42)(40 38 51 46))
					    :background-color '(0.9 0.9 0.9)
					    :stacked-series nil ;;; try also with t
					    :x-axis-options ()
					    :y-axis-options ()
					    :legend-options ()))
	    (pdf:draw-object (make-instance 'pdf:pie-chart :x 200 :y 100 :width 200 :height 200
					    :serie '(12 23 65 33)
					    :labels&colors '(("Winter" (1.0 0.0 0.0))
							     ("Spring" (0.0 1.0 0.0))
							     ("Summer" (0.0 0.0 1.0))
							     ("Autumn" (0.0 1.0 1.0)))))))
	(pdf:with-page ()
	  (pdf:register-page-reference "page 2")
	  (pdf:with-outline-level ("Page 2" "page 2")
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 36.0)
	     (pdf:move-text 100 800)
	     (pdf:draw-text "Page 2"))
	    (pdf:add-images-to-page jpg-image)
	    (pdf:draw-image jpg-image 10 10 239 50 0 t)
	    (pdf:add-URI-link 10 10 239 50 "http://www.fractalconcept.com/asp/html/cl-pdf.html" :border #(1 1 1))
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 10.0)
	     (pdf:move-text 500 10)
	     (pdf:draw-text "goto page 1"))
	    (pdf:add-link 495 8 80 14 "page 1")
	    (pdf:draw-object
	     (make-instance 'pdf:plot-xy :x 100 :y 400 :width 400 :height 200
			    :labels&colors '(("Data 1" (1.0 0.0 0.0))
					     ("Data 2" (0.0 1.0 0.0))
					     ("Data 3" (0.0 0.0 1.0)))
			    :series '(((1 40) (3 38) (5 31) (7 36))
				      ((2 53) (2.5 42) (3.7 46) (6 48))
				      ((1.3 12) (1.6 18) (2 16) (3 27)))
			    :background-color '(0.9 0.9 0.9)
			    :x-axis-options ()
			    :y-axis-options '(:min-value 0)
			    :legend-options ()))))))
    (pdf:write-document file)))

; Von Koch fractal (brute force ;-))

(defun vk-fractal (l level)
  (pdf:with-saved-state
      (if (zerop level)
          (progn
            (pdf:move-to 0 0)
            (pdf:line-to l 0)
            (pdf:stroke))
          (loop with l3 = (/ l 3.0) and l-1 = (1- level)
                for angle in '(nil 60 -120 60)
                do (when angle (pdf:rotate angle))
                   (vk-fractal l3 l-1))))
  (pdf:translate l 0))


(defun example8 (&optional (file #P"/tmp/ex8.pdf"))
  (pdf:with-document ()
    (loop for i from 0 to 6
	  do (pdf:with-page ()
	       (pdf:with-outline-level ((format nil "Page ~d" i)(pdf:register-page-reference))
		 (let* ((helvetica (pdf:get-font "Helvetica" :win-ansi-encoding)))
		   (pdf:draw-centered-text 297 800
					   (format nil "Koch's flake (Level ~d, ~d segments, perimeter ~,1f mm)"
						   i (* 3 (expt 4 i))(/ (* 180 (* 3 (expt 4 i)))(expt 3 i)))
				      helvetica 18.0)
                   (pdf:translate 42 530)
		   (pdf:set-line-width 0.1)
		   (vk-fractal 510 i)(pdf:rotate -120)(vk-fractal 510 i)(pdf:rotate -120)(vk-fractal 510 i)))))
    (pdf:write-document file)))

;;; A Mandelbrot set from Yannick Gingras

(defun hsv->rgb (h s v)
  "channels are in range [0..1]"
  (if (eql 0 s)
      (list v v v)
      (let* ((i (floor (* h 6)))
             (f (- (* h 6) i))
             (p (* v (- 1 s)))
             (q (* v (- 1 (* s f))))
             (t_ (* v (- 1 (* s (- 1 f)))))
             (hint (mod i 6)))
        (case hint
          (0 (list v t_ p))
          (1 (list q v p))
          (2 (list p v t_))
          (3 (list p q v))
          (4 (list t_ p v))
          (5 (list v p q))))))

(defun make-color-map (nb-col
		       start-rad
		       &optional
		       stop-rad
		       (sat .85)
		       (tilt-angle 0)
		       (nb-loop 1)
		       (clockwise t))
  ;; borowed from Poly-pen --YGingras
  (let* ((stop-rad (if stop-rad stop-rad start-rad))
         (angle-inc (* (if clockwise 1.0 -1.0) nb-loop))
         (val-inc (- stop-rad start-rad)))
    (coerce (loop for k from 0 to (1- nb-col) collect
		  (let ((i (/ k (1- nb-col))))
		    (mapcar #'(lambda (x) (round x 1/255))
			    (hsv->rgb (mod (+ tilt-angle (* i angle-inc)) 1)
				      sat
				      (+ start-rad (* i val-inc))))))
            'vector)))


(defun gen-mandelbrot-bits (w h)
  ;; Inside a Moth by Lahvak, for other interesting regions see
  ;;     http://fract.ygingras.net/top

  ;; TODO:AA
  (declare (optimize speed (debug 0) (safety 0) (space 0))
	   (type fixnum w h))
  (let* ((nb-cols 30000)
	 (nb-iter (expt 2 11)) ;; crank this if you have a fast box
	 (center #c(-0.7714390420105d0 0.1264514778485d0))
	 (zoom (* (expt (/ h 320) 2) 268436766))
	 (inc (/ h (* 150000d0 zoom)))
	 (cols (make-color-map nb-cols 1 0.2 0.9 0.24 0.21 t))
	 (c #c(0d0 0d0))
	 (z #c(0d0 0d0))
	 (region nil))
    (declare (type double-float inc))
    (dotimes (i h)
      (dotimes (j w)
	(setf c (complex (+ (realpart center)
			    (* inc (+ (the fixnum j) (/ w -2d0))))
			 (+ (imagpart center)
			    (* inc (+ (the fixnum i) (/ h -2d0)))))
	      z #c(0d0 0d0))
	;; standard Mandelbrot Set formula
	(push (dotimes (n nb-iter 0)
		(setf z (+ (* z z) c))
		(when (< 2 (abs z))
                  (return (- nb-iter
                             ;; sub-iter smoothing
                             (- n (log (log (abs (+ (* z z) c)) 10) 2))))))
	      region)))
    (with-output-to-string (s)
      (let ((max (reduce #'max region)))
	(dolist (x (nreverse region))
	  (destructuring-bind (r g b)
	      (if (zerop x)
		  '(0 0 0)
		  ;; pallette stretching
		  (elt cols (floor (expt (/ x max) (/ nb-iter 256))
				   (/ 1 (1- nb-cols)))))
	    (format s "~2,'0x~2,'0x~2,'0x" r g b)))))))

;;; Example 9 is a Mandelbrot set from Yannick Gingras
;;; Takes a long long time...

(defun example9 (&optional (file #P"/tmp/ex9.pdf"))
  "draw a nice region of the Mandelbrot Set"
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let* ((w 600)
	       (h 750)
	       (helvetica (pdf:get-font "Helvetica"))
	       (image (make-instance 'pdf:image
				     :bits (gen-mandelbrot-bits w h)
				     :width w :height h)))
	  (pdf:add-images-to-page image)
	  (pdf:in-text-mode
	    (pdf:set-font helvetica 36.0)
	    (pdf:move-text 100 800)
	    (pdf:draw-text "cl-pdf: Example 9"))
	  (pdf:with-saved-state
	      (pdf:translate 0 0)
	    (pdf:scale (/ w 2) (/ h 2))
	    (pdf:paint-image image)))))
    (pdf:write-document file)))

;;; Example 10: trnasparency by Eric Marsden

(defun example10 (&optional (file #p"/tmp/ex10.pdf"))
  (pdf:with-document ()
    (dolist (bm '(:normal :multiple :screen :overlay :darken :lighten :ColorDodge :ColorBurn
                  :HardLight :SoftLight :difference :exclusion :saturation :color :luminosity))
      (let ((helvetica (pdf:get-font "Helvetica")))
        (pdf:with-page ()
          (pdf:with-outline-level ((format nil "Blend mode ~A" bm) (pdf:register-page-reference))
            (pdf:in-text-mode
              (pdf:set-font helvetica 14.0)
              (pdf:move-text 15 820)
              (pdf:draw-text (format nil "PDF transparency with ~A blend-mode" bm)))
            (dotimes (y 10)
              (pdf:in-text-mode
                (pdf:set-font helvetica 10.0)
                (pdf:set-rgb-fill 0 0 0)
                (pdf:move-text 10 (+ 45 (* y 80)))
                (pdf:draw-text (format nil "Alpha = ~,1F" (/ y 9.0))))
              (pdf:set-rgb-fill 1 0.6 0.2)
              (pdf:rectangle 70 (+ 20 (* y 80)) 500 30)
              (pdf:fill-path))
            (pdf:translate 100 50)
            (dotimes (y 10)
              (dotimes (x 10)
                (apply #'pdf:set-rgb-fill (hsv->rgb (/ x 9.1) 1 1))
                (pdf:set-transparency (/ y 9.0) bm)
                (pdf:circle (* x 50) (* y 80) 30)
                (pdf:fill-path)))))))
    (pdf:write-document file)))

;;; Example 11: lazy image loading
(defun example11 (&optional (file #p"/tmp/ex11.pdf"))
  (let ((pdf:*load-images-lazily* t))
    (pdf:with-document ()
      (loop for f in (directory (merge-pathnames "*.png"))
	 do (pdf:with-page ()
	      (let ((jpg (pdf:make-image f)))
		(setf (pdf:bounds pdf:*page*) (vector 0 0 (pdf:width jpg) (pdf:height jpg)))
		(pdf:add-images-to-page jpg)
		(pdf:draw-image jpg 0 0 (pdf:width jpg) (pdf:height jpg) 0 t))))
      (pdf:write-document file))))