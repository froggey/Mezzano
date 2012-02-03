;;;; String functions. These replace most of the genesis string functions.

(in-package "SYSTEM.INTERNALS")

(defun string (x)
  (etypecase x
    (string x)
    (character (make-array 1 :element-type 'character :initial-element x))
    (symbol (symbol-name x))))

(defun stringp (object)
  (or (simple-string-p object)
      (and (vectorp object)
	   (subtypep (array-element-type object) 'character))))

(declaim (inline compare-sequence))
(defun compare-sequence (predicate seq1 seq2 &key (start1 0) end1 (start2 0) end2)
  (unless end1 (setf end1 (length seq1)))
  (when (or (< start1 0) (> end1 (length seq1)))
    (error "Invalid bounding index (~S ~S) for ~S." start1 end1 seq1))
  (unless end2 (setf end2 (length seq2)))
  (when (or (< start2 0) (> end2 (length seq2)))
    (error "Invalid bounding index (~S ~S) for ~S." start2 end2 seq2))
  (when (= (- end1 start1) (- end2 start2))
    (dotimes (i (- end1 start1) t)
      (unless (funcall predicate (elt seq1 (+ start1 i)) (elt seq2 (+ start2 i)))
	(return nil)))))

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
    "Returns true if STRING1 and STRING2 are the same length and contain the
same characters in the corresponding positions; otherwise it returns false."
  (compare-sequence 'char= (string string1) (string string2)
		    :start1 start1 :end1 end1
		    :start2 start2 :end2 end2))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Returns true if STRING1 and STRING2 are the same length and contain the
same characters in the corresponding positions; otherwise it returns false."
  (compare-sequence 'char-equal (string string1) (string string2)
		    :start1 start1 :end1 end1
		    :start2 start2 :end2 end2))

;;; FIXME: should be in character.lisp (not written yet).
(defun digit-char-p (char &optional (radix 10))
  "Tests whether CHAR is a digit in the specified RADIX.
If it is, then its weight is returned as an integer; otherwise, nil is returned."
  (check-type char character)
  (check-type radix (integer 2 36) "a radix")
  (do ((weight 0 (1+ weight)))
      ((>= weight radix))
    (when (char= (char-upcase char) (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" weight))
      (return weight))))