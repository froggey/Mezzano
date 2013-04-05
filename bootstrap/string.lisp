;;;; String functions. These replace most of the genesis string functions.

(in-package #:sys.int)

(defun string (x)
  (etypecase x
    (string x)
    (character (make-array 1 :element-type 'character :initial-element x))
    (symbol (symbol-name x))))

(defun stringp (object)
  (or (simple-string-p object)
      (and (vectorp object)
           (let ((element-type (array-element-type object)))
             (or (eql element-type 'nil)
                 (eql element-type 'base-char)
                 (eql element-type 'character))))))

(macrolet ((def (name comparator docstring)
             `(defun ,name (string1 string2 &key (start1 0) end1 (start2 0) end2)
                ,docstring
                (setf string1 (string string1))
                (setf string2 (string string2))
                (unless end1 (setf end1 (length string1)))
                (unless end2 (setf end2 (length string2)))
                (when (or (< start1 0) (> end1 (length string1)))
                  (error "Invalid bounding index (~S ~S) for ~S." start1 end1 string1))
                (when (or (< start2 0) (> end2 (length string2)))
                  (error "Invalid bounding index (~S ~S) for ~S." start2 end2 string2))
                (when (eql (- end1 start1) (- end2 start2))
                  (dotimes (i (- end1 start1) t)
                    (unless (,comparator (char string1 (+ start1 i))
                                         (char string2 (+ start2 i)))
                      (return nil)))))))
  (def string= char= "Returns true if STRING1 and STRING2 are the same length and contain the
same characters in the corresponding positions; otherwise it returns false.")
  (def string-equal char-equal "Returns true if STRING1 and STRING2 are the same length and contain the
same characters in the corresponding positions; otherwise it returns false."))

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

(macrolet ((def (name modifier copy &optional documentation)
             `(defun ,name (string &key (start 0) end)
                ,@(when documentation (list documentation))
                (declare (type string string))
                ,(if copy
                     `(setf string (string string))
                     `(check-type string string))
                (let ((new-string ,(if copy
                                       '(make-array (length string)
                                         :element-type (array-element-type string)
                                         :initial-contents string)
                                       'string)))
                  (unless end (setf end (length new-string)))
                  (dotimes (i (- end start))
                    (setf (char new-string (+ start i)) (,modifier (char new-string (+ start i)))))
                  new-string))))
  (def string-upcase char-upcase t)
  (def string-downcase char-downcase t)
  (def nstring-upcase char-upcase nil)
  (def nstring-downcase char-downcase nil))
