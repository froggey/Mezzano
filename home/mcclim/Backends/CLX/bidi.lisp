;;; Port of Kensanata's bidi code for Emacs:
;;; https://github.com/kensanata/emacs-bidi
;;; The original author has given permission to re-license this code under LGPL2.1+
;;; https://mailman.common-lisp.net/pipermail/mcclim-devel/2018-October/002102.html

(defpackage :mcclim-bidi
  (:use :cl)
  (:export #:directions))

(in-package :mcclim-bidi)

(defun bidi-apply-replacement (list old new)
  "Replace all instances of OLD with NEW in LIST.
LIST will be modified in place."
  (let ((lst (member old list)))
    (loop
      while lst
      do (progn
           (setf (car lst) new)
           (setq lst (member old (cdr lst))))))
  list)

(defun bidi-apply-w1 (sor types)
  "Apply UAX#9 rule W1.
SOR is the start-of-level-run bidi type.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Examine each non-spacing mark (NSM) in the level run, and change the
type of the NSM to the type of the previous character. If the NSM is at
the start of the level run, it will get the type of sor."
  ;; Skip this if no NSM types are found.
  (if (not (member 'cl-unicode-names::nsm types))
      types
      (let ((previous-type sor)
	    (lst types))
        (loop
          while lst
	  do (progn
               (if (eq (car lst) 'cl-unicode-names::nsm)
                   (setf (car lst) previous-type)
                   (setq previous-type (car lst)))
	       (setq lst (cdr lst))))
        types)))

(defun bidi-apply-w2 (sor types)
  "Apply UAX#9 rule W2.
SOR is the start-of-level-run bidi type.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Search backwards from each instance of a European number until the first
strong type (R, L, AL, or sor) is found.  If an AL is found, change the type of
the European number to Arabic number."
  (if (null (member 'cl-unicode-names::en types))
      types
      (let ((previous-type sor)
	    (lst types)
	    type)
        (loop
          while lst
	  do (progn
               (setq type (car lst))
	       (if (eq type 'cl-unicode-names::en)
	           (when (eq previous-type 'cl-unicode-names::al)
	             (setf (car lst) 'cl-unicode-names::al))
	           (when (or (eq type 'cl-unicode-names::r)
		             (eq type 'cl-unicode-names::l)
		             (eq type 'cl-unicode-names::al))
	             (setq previous-type type)))
	       (setq lst (cdr lst))))
        types)))

(defun bidi-apply-w3 (types)
  "Apply UAX#9 rule W3.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Change all ALs to R."
  (bidi-apply-replacement types 'cl-unicode-names::al 'cl-unicode-names::r))

(defun bidi-apply-w4 (types)
  "Apply UAX#9 rule W4.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

A single European separator between two European numbers changes to a
European number. A single common separator between two numbers of the
same type changes to that type."
  ;; I don't want to use string-match, here, because while the
  ;; categories are characters, they may have special meanings in
  ;; regexps, so I'd have to escape all special characters and then call
  ;; string-match.  That seems like too much overhead.
  (let ((lst (member 'cl-unicode-names::en types)))
    (loop
      while lst
      do (progn
           (when (and (or (eq (nth 1 lst) 'cl-unicode-names::cs)
	                  (eq (nth 1 lst) 'cl-unicode-names::es))
	              (eq (nth 2 lst) 'cl-unicode-names::en))
             (setq lst (cdr lst))
             (setf (car lst) 'cl-unicode-names::en)
             (setq lst (cdr lst)))
           (setq lst (member 'cl-unicode-names::en (cdr lst))))))
  (let ((lst (member 'cl-unicode-names::an types)))
    (loop
      while lst
      do (progn
           (when (and (eq (nth 1 lst) 'cl-unicode-names::cs)
		      (eq (nth 2 lst) 'cl-unicode-names::an))
	     (setq lst (cdr lst))
	     (setf (car lst) 'cl-unicode-names::an)
	     (setq lst (cdr lst)))
           (setq lst (member 'cl-unicode-names::en (cdr lst))))))
  types)

(defun bidi-apply-w5-sub (types)
  "Change all ET following EN to EN.
Called by `bidi-apply-w5', once for the real list, once for the reversed list."
  (let ((lst (member 'cl-unicode-names::en types)))
    (loop
      while lst
      do (progn
           (setq lst (cdr lst))
           (loop
             while (and lst
		        (eq (car lst) 'cl-unicode-names::et))
	     do (progn
                  (setf (car lst) 'cl-unicode-names::en)
	          (setq lst (cdr lst))))
           (setq lst (member 'cl-unicode-names::en lst)))))
  types)

(defun bidi-apply-w5 (types)
  "Apply UAX#9 rule W5.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

A sequence of European terminators adjacent to European numbers changes
to all European numbers."
  ;; We use the following trick, here.  First we search for any ET
  ;; following EN, then we reverse the list, and repeat.
  (if (not (member 'cl-unicode-names::et types))
      types
      (progn
        (setq types (bidi-apply-w5-sub types))
        (if (not (member 'cl-unicode-names::et types))
	    types
            (nreverse (bidi-apply-w5-sub (nreverse types)))))))

(defun bidi-apply-w6 (types)
  "Apply UAX#9 rule W6.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Separators and terminators change to Other Neutral."
  ;; Difficult decision: Is it faster to search the list three times
  ;; using memq or is it faster to step through the list on our own?
  ;; Better to reuse code.  And it looks good.
  (let* ((cs-result (bidi-apply-replacement types 'cl-unicode-names::cs 'cl-unicode-names::on))
         (es-result (bidi-apply-replacement cs-result 'cl-unicode-names::es 'cl-unicode-names::on)))
   (bidi-apply-replacement es-result 'cl-unicode-names::et 'cl-unicode-names::on)))

(defun bidi-apply-w7 (sor types)
  "Apply UAX#9 rule W7.
SOR is the start-of-level-run bidi type.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

Search backwards from each instance of a European number until the first
strong type (R, L, or sor) is found. If an L is found, then change the
type of the European number to L."
  ;; Looks very similar to `bidi-apply-w7'.  Does it make sense to
  ;; factor this out?  I don't think so.  That would be "method
  ;; stealing".
  (if (null (member 'cl-unicode-names::en types))
      types
      (let ((previous-type sor)
	    (lst types)
	    type)
        (progn
          (loop
            while lst
            do (progn
                 (setq type (car lst))
                 (if (eq type 'cl-unicode-names::en)
	             (when (eq previous-type 'cl-unicode-names::l)
	               (setf (car lst) 'cl-unicode-names::l))
	             (when (or (eq type 'cl-unicode-names::r)
		               (eq type 'cl-unicode-names::l))
	               (setq previous-type type)))
                 (setq lst (cdr lst))))
          types))))

(defun bidi-apply-conversion (start-lst end-lst new)
  "Transform elements from START-LST to END-LST to NEW.
START-LST is a list, and END-LST is a sublist, a tail, in START-LST.
Every car in START-LST will then be changed to NEW until END-LST is
reached.  END-LST remains unmodified.  START-LST is modified in place."
  ;; A potential error check has been left out.  start-lst ought never
  ;; to be nil.
  (let ((start start-lst))
    (loop
      while (and start-lst
		 (not (eq start-lst end-lst)))
      do (progn
           (setf (car start-lst) new)
           (setq start-lst (cdr start-lst))))
    start))

(defun bidi-apply-n1 (sor eor types)
  "Apply UAX#9 rule N1.
SOR is the start-of-level-run bidi type.
EOR is the end-of-level-run bidi type.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

A sequence of neutrals takes the direction of the surrounding strong
text if the text on both sides has the same direction. European and
Arabic numbers are treated as though they were R. Start-of-level-run
\(sor) and end-of-level-run \(eor) are used at level run boundaries."
  ;; The idea is this: We step through the list.  start-lst points to
  ;; the last strong type we encountered.  This marks the start of a
  ;; "stretch of neutrals".  lst points to the current position in the
  ;; list.  The elements between start-lst and lst may be switched.
  (let ((start-lst types)
	(lst types)
	(start-type sor)
	(type (car types)))
    (loop
      while lst
      do (progn
           (cond ((eq type 'cl-unicode-names::l) ; potential end or start of L stretch
	          (if (eq start-type 'cl-unicode-names::l)
		      (bidi-apply-conversion start-lst lst 'cl-unicode-names::l)
	              (setq start-type 'cl-unicode-names::l))
	          (setq start-lst (cdr lst)))
	         ((or (eq type 'cl-unicode-names::r) ; potential end or start of R stretch
		      (eq type 'cl-unicode-names::en)
		      (eq type 'cl-unicode-names::an))
	          (if (eq start-type 'cl-unicode-names::r)
		      (bidi-apply-conversion start-lst lst 'cl-unicode-names::r)
	              (setq start-type 'cl-unicode-names::r))
	          (setq start-lst (cdr lst)))
	         ;; is the following clause ever needed?
	         ((not (or (eq type 'cl-unicode-names::b) ; definitely not part of a stretch
		           (eq type 'cl-unicode-names::s)
		           (eq type 'cl-unicode-names::ws)
		           (eq type 'cl-unicode-names::on)))
	          (setq start-type nil
		        start-lst nil)))
           (setq lst (cdr lst)
	         type (car lst))))
    ;; final stretch
    (when (and start-lst
	       (eq start-type eor))
      (bidi-apply-conversion start-lst nil start-type)))
  types)

(defun bidi-apply-n2 (e types)
  "Apply UAX#9 rule N2.
TYPES is a list of bidi types to operate on.
E is the embedding level to use.
TYPES will be modified in place.

Any remaining neutrals take the embedding direction."
  (let* ((result-b (bidi-apply-replacement types 'cl-unicode-names::b e))
         (result-s (bidi-apply-replacement result-b 'cl-unicode-names::s e))
         (result-ws (bidi-apply-replacement result-s 'cl-unicode-names::ws e)))
    (bidi-apply-replacement result-ws 'cl-unicode-names::on e)))


(defun get-category (char)
  (nth-value 1 (cl-unicode:bidi-class char)))

(defun bidi-get-types (string)
  (map 'list #'get-category string))

(defun bidi-resolve-weak-types (types &optional r2l-context)
  "Resolve weak bidi types in TYPES, a list of bidi categories.
TYPES is a list of bidi types to operate on.
TYPES will be modified in place.

This uses rules W1 to W7 and rules N1 and N2 from UAX#9.

Since this function doesn't take care of explicit codes, all types are
considered to be at the same level.  If you decomposed a string into
stretches of differing embedding levels, you can call this function for
every such stretch.

The resulting list will have the same number of elements and all weak
types will be replaced by strong types.  If the optional argument
R2L-CONTEXT is given, the context will be right-to-left.  If R2L-CONTEXT
is nil, a left-to-right context will be assumed.

In UAX#9 parlance: Each paragraph has a text ordering type E.  If
R2L-CONTEXT is given, E is set to R, otherwise E is set to L.
Furthermore, we set the start-of-level-run type SOR and the
end-of-level-run type EOR to E.  E, SOR, and EOR are used for rules W1,
W2, W7, N1, and N2.

For more information in the various rules, see the functions:
 BIDI-APPLY-W1
 BIDI-APPLY-W2
 BIDI-APPLY-W3
 BIDI-APPLY-W4
 BIDI-APPLY-W5
 BIDI-APPLY-W6
 BIDI-APPLY-W7
 BIDI-APPLY-N1
 BIDI-APPLY-N2"
  (let* ((e (if r2l-context 'cl-unicode-names::r 'cl-unicode-names::l))
	 (sor e)
	 (eor e))
    (let* ((l0 (bidi-apply-w1 sor types))
           (l1 (bidi-apply-w2 sor l0))
           (l2 (bidi-apply-w3 l1))
           (l3 (bidi-apply-w4 l2))
           (l4 (bidi-apply-w5 l3))
           (l5 (bidi-apply-w6 l4))
           (l6 (bidi-apply-w7 sor l5))
           (l7 (bidi-apply-n1 sor eor l6)))
      (bidi-apply-n2 e l7))))

(defun bidi-resolve-implicit-levels (types &optional levels)
  "Resolve the implicit levels in TYPES.
TYPES is a list of bidi types to operate on.
LEVELS is an optional list of explicit levels.  If it is provided,
the list should have the same length as TYPES.
LEVELS will be modified in place, if it is provided.

This uses rules I1 and I2 from UAX#9.

For all characters with an even (left-to-right) embedding direction,
those of type R go up one level and those of type AN or EN go up two
levels.  For all characters with an odd (right-to-left) embedding
direction, those of type L, EN or AN go up one level."
  (setq levels (or levels (make-list (length types) :initial-element 0)))
  (let ((result levels))
    (loop
      while types
      do (let ((type (car types))
	        (level (car levels)))
	   (if (evenp level)
	       ;; I1: even (left-to-right) embedding direction
	       (cond ((eq type 'cl-unicode-names::r)
		      (setf (car levels) (1+ level)))
		     ((or (eq type 'cl-unicode-names::an)
		          (eq type 'cl-unicode-names::en))
		      (setf (car levels) (+ 2 level))))
	       ;; I2: odd (right-to-left) embedding direction
	       (when (or (eq type 'cl-unicode-names::l)
		         (eq type 'cl-unicode-names::en)
		         (eq type 'cl-unicode-names::an))
	         (setf (car levels) (1+ level))))
	   (setq types (cdr types))
	   (setq levels (cdr levels))))
    result))

(defun directions (string r2l-context)
  (let ((dirdata (bidi-resolve-implicit-levels (bidi-resolve-weak-types (bidi-get-types string) r2l-context))))
    (assert (alexandria:length= string dirdata))
    (labels ((number-to-type (n)
               (ecase n
                 (0 :ltr)
                 (1 :rtl))))
     (let ((result nil))
       (loop
         with start = 0
         with i = 0
         for prev = nil then dir
         for dir in dirdata
         if (and prev (not (eql prev dir)))
           do (progn
                (push (cons (number-to-type prev) (subseq string start i)) result)
                (setq start i)
                (setq prev dir))
         do (incf i)
         finally (when (> i start)
                   (push (cons (number-to-type prev) (subseq string start i)) result)))
       result))))
