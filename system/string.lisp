;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; String functions. These replace most of the genesis string functions.

(in-package :sys.int)

(defun string (x)
  (etypecase x
    (string x)
    (character (make-array 1 :element-type 'character :initial-element x))
    (symbol (symbol-name x))))

(defun stringp (object)
  (and (character-array-p object)
       (eql (array-rank object) 1)))

(defun simple-string-p (object)
  (and (simple-character-array-p object)
       (eql (array-rank object) 1)))

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

(macrolet ((def (sensitive-name insensitive-name char-comparator numeric-comparator)
             `(progn
                (defun ,sensitive-name (string1 string2 &key (start1 0) end1 (start2 0) end2)
                  (setf string1 (string string1))
                  (setf string2 (string string2))
                  (unless end1 (setf end1 (length string1)))
                  (unless end2 (setf end2 (length string2)))
                  (dotimes (i (min (- end1 start1)
                                   (- end2 start2))
                            (if (,numeric-comparator (- end1 start1) (- end2 start2))
                                end1
                                nil))
                    ;; Compare prefix.
                    (unless (char= (char string1 (+ start1 i))
                                   (char string2 (+ start2 i)))
                      (return (if (,char-comparator (char string1 (+ start1 i))
                                                    (char string2 (+ start2 i)))
                                  (+ start1 i)
                                  nil)))))
                (defun ,insensitive-name (string1 string2 &key (start1 0) end1 (start2 0) end2)
                  (,sensitive-name (string-downcase string1) (string-downcase string2)
                                   :start1 start1
                                   :end1 end1
                                   :start2 start2
                                   :end2 end2)))))
  (def string< string-lessp char< <)
  (def string> string-greaterp char> >)
  (def string<= string-not-greaterp char<= <=)
  (def string>= string-not-lessp char>= >=)
  (def string/= string-not-equal char/= /=))

(defun string-trim (character-bag string)
  (let* ((string (string string))
         (left-position (position-if-not (lambda (x) (find x character-bag)) string))
         (right-position (position-if-not (lambda (x) (find x character-bag)) string :from-end t)))
    (cond ((and left-position right-position)
           (subseq string left-position (1+ right-position)))
          (t ""))))

(defun string-left-trim (character-bag string)
  (let* ((string (string string))
         (left-position (position-if-not (lambda (x) (find x character-bag)) string)))
    (cond (left-position
           (subseq string left-position))
          (t ""))))

(defun string-right-trim (character-bag string)
  (let* ((string (string string))
         (right-position (position-if-not (lambda (x) (find x character-bag)) string :from-end t)))
    (cond (right-position
           (subseq string 0 (1+ right-position)))
          (t ""))))

(defun make-string (size &key initial-element (element-type 'character))
  (if initial-element
      (make-array size :element-type element-type :initial-element initial-element)
      (make-array size :element-type element-type)))
