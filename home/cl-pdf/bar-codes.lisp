;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;; code 128 barcode from Lars Rustemeier

(in-package #:pdf)

;;
;; Utils
;;

(defun group (string n)
  (when (zerop n) (error "zero length"))
  (loop with length = (length string)
        for i from 0 below length by n
        collect (subseq string i (min (+ i n) length))))

;;
;; Logic Layer
;;

;; Format: 
;; [Value] [Code A] [Code B] [Code C] [Pattern (B S B S B S)]
(defparameter *table* 
  '((0   #\space   #\space   "00"      2 -1 2 -2 2 -2)
    (1   #\!       #\!       "01"      2 -2 2 -1 2 -2)
    (2   #\"       #\"       "02"      2 -2 2 -2 2 -1)
    (3   #\#       #\#       "03"      1 -2 1 -2 2 -3)
    (4   #\$       #\$       "04"      1 -2 1 -3 2 -2)
    (5   #\%       #\%       "05"      1 -3 1 -2 2 -2)
    (6   #\&       #\&       "06"      1 -2 2 -2 1 -3)
    (7   #\'       #\'       "07"      1 -2 2 -3 1 -2)
    (8   #\(       #\(       "08"      1 -3 2 -2 1 -2)
    (9   #\)       #\)       "09"      2 -2 1 -2 1 -3)
    (10  #\*       #\*       "10"      2 -2 1 -3 1 -2)
    (11  #\+       #\+       "11"      2 -3 1 -2 1 -2)
    (12  #\,       #\,       "12"      1 -1 2 -2 3 -2)
    (13  #\-       #\-       "13"      1 -2 2 -1 3 -2)
    (14  #\.       #\.       "14"      1 -2 2 -2 3 -1)
    (15  #\/       #\/       "15"      1 -1 3 -2 2 -2)
    (16  #\0       #\0       "16"      1 -2 3 -1 2 -2)
    (17  #\1       #\1       "17"      1 -2 3 -2 2 -1)
    (18  #\2       #\2       "18"      2 -2 3 -2 1 -1)
    (19  #\3       #\3       "19"      2 -2 1 -1 3 -2)
    (20  #\4       #\4       "20"      2 -2 1 -2 3 -1)
    (21  #\5       #\5       "21"      2 -1 3 -2 1 -2)
    (22  #\6       #\6       "22"      2 -2 3 -1 1 -2)
    (23  #\7       #\7       "23"      3 -1 2 -1 3 -1)
    (24  #\8       #\8       "24"      3 -1 1 -2 2 -2)
    (25  #\9       #\9       "25"      3 -2 1 -1 2 -2)
    (26  #\:       #\:       "26"      3 -2 1 -2 2 -1)
    (27  #\;       #\;       "27"      3 -1 2 -2 1 -2)
    (28  #\<       #\<       "28"      3 -2 2 -1 1 -2)
    (29  #\=       #\=       "29"      3 -2 2 -2 1 -1)
    (30  #\>       #\>       "30"      2 -1 2 -1 2 -3)
    (31  #\?       #\?       "31"      2 -1 2 -3 2 -1)
    (32  #\@       #\@       "32"      2 -3 2 -1 2 -1)
    (33  #\A       #\A       "33"      1 -1 1 -3 2 -3)
    (34  #\B       #\B       "34"      1 -3 1 -1 2 -3)
    (35  #\C       #\C       "35"      1 -3 1 -3 2 -1)
    (36  #\D       #\D       "36"      1 -1 2 -3 1 -3)
    (37  #\E       #\E       "37"      1 -3 2 -1 1 -3)
    (38  #\F       #\F       "38"      1 -3 2 -3 1 -1)
    (39  #\G       #\G       "39"      2 -1 1 -3 1 -3)
    (40  #\H       #\H       "40"      2 -3 1 -1 1 -3)
    (41  #\I       #\I       "41"      2 -3 1 -3 1 -1)
    (42  #\J       #\J       "42"      1 -1 2 -1 3 -3)
    (43  #\K       #\K       "43"      1 -1 2 -3 3 -1)
    (44  #\L       #\L       "44"      1 -3 2 -1 3 -1)
    (45  #\M       #\M       "45"      1 -1 3 -1 2 -3)
    (46  #\N       #\N       "46"      1 -1 3 -3 2 -1)
    (47  #\O       #\O       "47"      1 -3 3 -1 2 -1)
    (48  #\P       #\P       "48"      3 -1 3 -1 2 -1)
    (49  #\Q       #\Q       "49"      2 -1 1 -3 3 -1)
    (50  #\R       #\R       "50"      2 -3 1 -1 3 -1)
    (51  #\S       #\S       "51"      2 -1 3 -1 1 -3)
    (52  #\T       #\T       "52"      2 -1 3 -3 1 -1)
    (53  #\U       #\U       "53"      2 -1 3 -1 3 -1)
    (54  #\V       #\V       "54"      3 -1 1 -1 2 -3)
    (55  #\W       #\W       "55"      3 -1 1 -3 2 -1)
    (56  #\X       #\X       "56"      3 -3 1 -1 2 -1)
    (57  #\Y       #\Y       "57"      3 -1 2 -1 1 -3)
    (58  #\Z       #\Z       "58"      3 -1 2 -3 1 -1)
    (59  #\[       #\[       "59"      3 -3 2 -1 1 -1)
    (60  #\\       #\\       "60"      3 -1 4 -1 1 -1)
    (61  #\]       #\]       "61"      2 -2 1 -4 1 -1)
    (62  #\^       #\^       "62"      4 -3 1 -1 1 -1)
    (63  #\_       #\_       "63"      1 -1 1 -2 2 -4)
    (64  :NUL      #\'       "64"      1 -1 1 -4 2 -2)
    (65  :SOH      #\a       "65"      1 -2 1 -1 2 -4)
    (66  :STX      #\b       "66"      1 -2 1 -4 2 -1)
    (67  :ETX      #\c       "67"      1 -4 1 -1 2 -2)
    (68  :EOT      #\d       "68"      1 -4 1 -2 2 -1)
    (69  :ENQ      #\e       "69"      1 -1 2 -2 1 -4)
    (70  :ACK      #\f       "70"      1 -1 2 -4 1 -2)
    (71  :BEL      #\g       "71"      1 -2 2 -1 1 -4)
    (72  :BS       #\h       "72"      1 -2 2 -4 1 -1)
    (73  :HT       #\i       "73"      1 -4 2 -1 1 -2)
    (74  :LF       #\j       "74"      1 -4 2 -2 1 -1)
    (75  :VT       #\k       "75"      2 -4 1 -2 1 -1)
    (76  :FF       #\l       "76"      2 -2 1 -1 1 -4)
    (77  :CR       #\m       "77"      4 -1 3 -1 1 -1)
    (78  :SO       #\n       "78"      2 -4 1 -1 1 -2)
    (79  :SI       #\o       "79"      1 -3 4 -1 1 -1)
    (80  :DLE      #\p       "80"      1 -1 1 -2 4 -2)
    (81  :DC1      #\q       "81"      1 -2 1 -1 4 -2)
    (82  :DC2      #\r       "82"      1 -2 1 -2 4 -1)
    (83  :DC3      #\s       "83"      1 -1 4 -2 1 -2)
    (84  :DC4      #\t       "84"      1 -2 4 -1 1 -2)
    (85  :NAK      #\u       "85"      1 -2 4 -2 1 -1)
    (86  :SYN      #\v       "86"      4 -1 1 -2 1 -2)
    (87  :ETB      #\w       "87"      4 -2 1 -1 1 -2)
    (88  :CAN      #\x       "88"      4 -2 1 -2 1 -1)
    (89  :EM       #\y       "89"      2 -1 2 -1 4 -1)
    (90  :SUB      #\z       "90"      2 -1 4 -1 2 -1)
    (91  :ESC      #\{       "91"      4 -1 2 -1 2 -1)
    (92  :FS       #\|       "92"      1 -1 1 -1 4 -3)
    (93  :GS       #\}       "93"      1 -1 1 -3 4 -1)
    (94  :RS       #\~       "94"      1 -3 1 -1 4 -1)
    (95  :US       :DEL      "95"      1 -1 4 -1 1 -3)
    (96  :FNC-3    :FNC-3    "96"      1 -1 4 -3 1 -1)
    (97  :FNC-2    :FNC-2    "97"      4 -1 1 -1 1 -3)
    (98  :SHIFT    :SHIFT    "98"      4 -1 1 -3 1 -1)
    (99  :CODE-C   :CODE-C   "99"      1 -1 3 -1 4 -1)
    (100 :CODE-B   :FNC-4    :CODE-B   1 -1 4 -1 3 -1)
    (101 :FNC-4    :CODE-A   :CODE-A   3 -1 1 -1 4 -1)
    (102 :FNC-1    :FNC-1    :FNC-1    4 -1 1 -1 3 -1)
    (103 :START-A  :START-A  :START-A  2 -1 1 -4 1 -2)
    (104 :START-B  :START-B  :START-B  2 -1 1 -2 1 -4)
    (105 :START-C  :START-C  :START-C  2 -1 1 -2 3 -2)
    (106 :STOP     :STOP     :STOP     2 -3 3 -1 1 -1 2)))

(defconstant +magic-modulo-number+ 103)

(defun build-char-ht (lst hpred key-selector val-selector)
  (let ((ht (make-hash-table :test hpred)))
    (dolist (e lst)
      (setf (gethash (funcall key-selector e) ht) (funcall val-selector e)))
    ht))

(defparameter *table-h* (build-char-ht *table* #'eql #'first #'cdr))
(defparameter *table-a* (build-char-ht *table* #'eql #'second #'(lambda (e) (cons (first e) (cddddr e)))))
(defparameter *table-b* (build-char-ht *table* #'eql #'third #'(lambda (e) (cons (first e) (cddddr e)))))
(defparameter *table-c* (build-char-ht *table* #'equal #'fourth #'(lambda (e) (cons (first e) (cddddr e)))))
  

;;
;; Calulate checksum
;;

(defun code128-checksum (chars)
  (mod (do ((vals (cdr chars) (cdr vals))
	       (factor 1 (+ factor 1))
	       (chksum (first chars) (+ chksum (* (car vals) factor))))
	      ((null vals)
	       chksum))
	  +magic-modulo-number+))

;;
;; Generic
;;

(defun code128-n-raw (string start s-table getter)
  (let ((chars-and-bars (cons (gethash start s-table)
			      (map 'list #'(lambda (s) (gethash s s-table)) string))))
    (let* ((chars (mapcar #'car chars-and-bars))
	   (bars (mapcar #'cdr chars-and-bars))
	   (chksum (code128-checksum chars))
	   (chk-char-bar (cdr (gethash (funcall getter (gethash chksum *table-h*)) s-table)))
	   (stop (cdr (gethash :STOP s-table))))
      (append bars (list chk-char-bar stop)))))

;;
;; Chars and control codes
;;
(defun code128-a (text)
  (code128-n-raw text :START-A *table-a* #'first))

;;
;; Full printable ascii
;;
(defun code128-b (text)
  (code128-n-raw text :START-B *table-b* #'second))

;;
;; Compact digits
;;
(defun code128-c (text)
  (code128-n-raw (group text 2) :START-C *table-c* #'third))

;;
;; Calculate width of barcode in units
;;	

(defun unit-width (lst)
  ;;(reduce + 0 (map (lambda (e) (reduce + 0 e)) (map second lst))))
  (reduce #'+ (mapcar #'(lambda (e) (reduce #'+ (mapcar #'abs e))) lst)))

;;
;; Drawing (Presentation Layer)
;;

(defun draw-bar-segment (line-height line-width black)
  (with-saved-state
   (when (plusp black)
     (rectangle 0 0 (* black line-width) (- line-height))
     (fill-path)))
  (translate (abs (* black line-width)) 0))

(defun draw-bars (bars line-height line-width &optional (start-stop-factor 0))
  (let ((l (length bars)))
    (do* ((bars bars (cdr bars))
	  (bar (car bars) (car bars))
	  (i 0 (1+ i)))
	((null bars)
	 nil)
      (dolist (segment bar)
	(let ((line-height (if (or (= i 0) (= i (- l 1)))
			       (* line-height (+ 1 start-stop-factor))
			     line-height)))
	  (draw-bar-segment line-height line-width segment))))))
    
(defun draw-chars (string line-height line-width font font-size segs-per-char)
  (with-saved-state
   (translate (* 11 line-width) (- (+ line-height font-size)))
   (set-font font font-size)
   (loop for char across string do
	 (with-saved-state
	     (in-text-mode
	      (show-char char)))
	 (translate (* segs-per-char line-width) 0))))

;;
;; Autoselect's mode based on content
;; 
;; One could spend a lot of time on making really smart (Switching
;; between alphabets on the fly etc). I fear that for more advanced
;; uses it is better to let the user specify the token list
;; manually. Or build utils on top of this file's functionality. There
;; simply are too many border cases for this to make me feel right
;; about a totally automatic approach.

(defun draw-bar-code128 (string x y &key (font (get-font)) (font-size 5) (start-stop-factor 0.3) (height 100) (width 400) (show-string t) (segs-per-char 11 segs-per-char-p))
    (let ((dispatch #'code128-b))
      (when (and (evenp (length string))(every #'digit-char-p string))
	(unless segs-per-char-p
          (setf segs-per-char 5.5))
	(setf dispatch #'code128-c))
      (with-saved-state
	  (translate x y)
	(set-line-width 0)
	(let* ((bars (funcall dispatch string))
	       (unit-w (unit-width bars))
	       (line-width (/ width unit-w))
	       (line-height height))
	  (with-saved-state
	      (draw-bars bars line-height line-width start-stop-factor))
	  (when show-string
	    (with-saved-state
		(draw-chars string line-height line-width font font-size segs-per-char)))))))

;;
;; Pres test code
;;

#+nil
(defun tester (str &optional (to-file "/tmp/foobar.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ((format nil "BarCodes Sample") (pdf:register-page-reference))
	  (pdf:with-saved-state
	   (pdf:translate -150 -400)
	   (pdf:rotate 90)
	   (pdf:translate 500 -300)
	   (pdf:rotate 5)	   
	   (draw-bar-code128 str 0 0 :height 300 :width 600 :start-stop-factor 0.2 :font-size 40 :show-string t))))
  (pdf:write-document to-file)))


#+nil
(tester "CL:PDF Marc B" #P"/home/largo/char-b.pdf")
#+nil
(tester "012345" #P"/home/largo/char-c.pdf")
