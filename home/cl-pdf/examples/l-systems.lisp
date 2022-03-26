

;; This an adaptation to cl-pdf of the postscript Lindenmayer System generator
;; code posted by Frank Buss in comp.lang.lisp.
;;
;; The original post is here:
;; http://groups.google.de/group/comp.lang.lisp/browse_thread/thread/05631fa93379bca8/248b67466ca2aa7d
;; 
;; Frank Buss wrote:
;; 
;; I've implemented a Lindenmayer System generator, with postscript output.
;; A L-System is a fractal, which can be used to describe plants. The
;; program implements the commands described here:
;; 
;; http://www.biologie.uni-hamburg.de/b-online/e28_3/lsys.html
;; 
;; Some example outputs (converted to PDF, because it is shorter)
;; 
;; http://www.frank-buss.de/tmp/bush.pdf
;; http://www.frank-buss.de/tmp/snow.pdf
;; http://www.frank-buss.de/tmp/dragon.pdf
;; 
;; The code is below. But it is too slow. I think it is the function
;; "starts-with" and the string handling, perhaps it could be faster with
;; lists, because then the substitute step could be implemented faster.
;; 
;; How can I profile the program? I know the "time"-macro, but would be
;; nice to have an utility, which writes for every function or every line
;; the time after program termination.
;; 
;; And it would be nice to test new rules interactive, without the need to
;; produce a postscript file, first. Is this possible with LTK or some other
;; GUI interface?
;; 
;; Another idea: Enhancing it to 3D output and color, like this program:
;; http://home.wanadoo.nl/laurens.lapre/lparser.htm

(defun starts-with (string search start)
  "Returns true, if the string 'search' is at 'start' in 'string'."
  (let ((string-len (length string))
        (search-len (length search)))
    (if (> (+ start search-len) string-len)
        nil
        (equal (subseq string start (+ start search-len)) search))))

(defun search-longest (rules string start)
  "Returns the successor and predecessor length, if a predecessor
   is found in 'string' at 'start'. Returns the longest match."
  (let ((found-successor nil)
        (found-predecessor-length 0))
    (loop for (predecessor . successor) in rules do
          (when (starts-with string predecessor start)
            (let ((predecessor-length (length predecessor)))
              (when (>= predecessor-length found-predecessor-length)
                (setq found-predecessor-length predecessor-length)
                (setq found-successor successor)))))
    (values found-successor found-predecessor-length)))

(defun apply-rules (rules axiom)
  "Returns the next iteration, starting with the string in 'axiom'."
  (let ((result ""))
    (loop for i from 0 to (1- (length axiom)) do
          (multiple-value-bind (successor predecessor-length)
              (search-longest rules axiom i)
            (if successor
                (progn
                  (setq result (concatenate 'string result successor))
                  (setq i (1- (+ i predecessor-length))))
                (progn
                  (setq result (concatenate 'string result (subseq axiom i (1+ i))))))))
    result))

(defun l-system (rules axiom depth)
  "Returns 'depth' iterations, starting with 'axiom' and applying the 'rules'."
  (let ((result axiom))
    (loop repeat depth do
          (setq result (apply-rules rules result)))
    result))

(defun forward (point len angle)
  "Returns a new point by starting from 'point' and adding the polar coordinates 'len' and 'angles'."
  (let ((x (car point))
        (y (cdr point))
        (rad (* (/ pi 180.0) angle)))
    (cons (+ x (* (sin rad) len)) (+ y (* (cos rad) len)))))

(defun do-l-system (commands len angle fun)
  "Calls 'fun x0 y0 x1 y1' for every command in the 'commands' string."
  (let ((point-stack '())
        (angle-stack '())
        (current-point '(0e0 . 0e0))
        (current-angle 0e0))
    (loop for i from 0 to (1- (length commands)) do
          (let ((command (elt commands i)))
            (cond ((eq command #\f)
                   (setq current-point (forward current-point len current-angle)))
                  ((eq command #\F)
                   (let ((next-point (forward current-point len current-angle)))
                     (funcall fun (car current-point) (cdr current-point) (car next-point) (cdr next-point))
                     (setq current-point next-point)))
                  ((eq command #\+)
                   (setq current-angle (+ current-angle angle)))
                  ((eq command #\-)
                   (setq current-angle (- current-angle angle)))
                  ((eq command #\[)
                   (push current-point point-stack)
                   (push current-angle angle-stack))
                  ((eq command #\])
                   (setq current-point (pop point-stack))
                   (setq current-angle (pop angle-stack))))))))

(defun pdf-l-system (rules axiom length angle depth &key (x0 0)(y0 0) (file #P"/tmp/l-system.pdf"))
  "Calculates and prints a Lindenmayer System as pdf."
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:translate (+ x0 287) (+ y0 400))
      (pdf:set-line-width 0)
      (do-l-system (l-system rules axiom depth) length angle
                   (lambda (x0 y0 x1 y1)
                     (pdf:move-to x0 y0)
                     (pdf:line-to x1 y1)))
      (pdf:stroke))
    (pdf:write-document file)))
    
;; this one takes longer but adjusts the scaling and centering so that it fits on the page
(defun pdf-l-system-centered (rules axiom angle depth &key (file #P"/tmp/l-system.pdf"))
  "Calculates and prints a Lindenmayer System as pdf."
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:set-line-width 0)
      (let ((commands (l-system rules axiom depth))
            (min-x 1e30)
            (min-y 1e30)
            (max-x -1e30)
            (max-y -1e30))
        (do-l-system commands 1 angle
                     (lambda (x0 y0 x1 y1)
                       (when (< x0 min-x) (setq min-x x0))
                       (when (< y0 min-y) (setq min-y y0))
                       (when (< x1 min-x) (setq min-x x1))
                       (when (< y1 min-y) (setq min-y y1))
                       (when (> x0 max-x) (setq max-x x0))
                       (when (> y0 max-y) (setq max-y y0))
                       (when (> x1 max-x) (setq max-x x1))
                       (when (> y1 max-y) (setq max-y y1))))
        (let* ((length (/ 500.0 (max (- max-y min-y) (- max-x min-x)))) 
               (dx (* 0.5 (- 595 (* (+ max-x min-x) length))))
               (dy (* 0.5 (- 841 (* (+ max-y min-y) length)))))
          (pdf:translate dx dy)
          (do-l-system (l-system rules axiom depth) length angle
                       (lambda (x0 y0 x1 y1)
                         (pdf:move-to x0 y0)
                         (pdf:line-to x1 y1)))
          (pdf:stroke))))
    (pdf:write-document file)))
    
;;; dragon, needs some minutes to calculate with depth 16
; (pdf-l-system-centered '(("FL"."FL+FR+") ("FR"."-FL-FR")) "FL" 90 16)

;;; snowflake
; (pdf-l-system-centered '(("F"."F+F--F+F")) "F--F--F" 60 7)

;;; bush
; (pdf-l-system-centered '(("F"."FF-[-F+F+F]+[+F-F-F]")) "+F" 23 5)

