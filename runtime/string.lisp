;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

;; Hardcoded string accessor, the support stuff for arrays doesn't function at this point.
(defun char (string index)
  (cond ((sys.int::character-array-p string)
         (let ((data (sys.int::%complex-array-storage string)))
           (assert (and (<= 0 index)
                        (< index (sys.int::%object-header-data data)))
                   (string index))
           (sys.int::%%assemble-value
            (ash (ecase (sys.int::%object-tag data)
                   (#.sys.int::+object-tag-array-unsigned-byte-8+
                    (sys.int::%object-ref-unsigned-byte-8 data index))
                   (#.sys.int::+object-tag-array-unsigned-byte-16+
                    (sys.int::%object-ref-unsigned-byte-16 data index))
                   (#.sys.int::+object-tag-array-unsigned-byte-32+
                    (sys.int::%object-ref-unsigned-byte-32 data index))
                   (t 0))
                 4)
            sys.int::+tag-character+)))
        (t
         ;; Possibly a displaced string.
         (check-type string string)
         (aref string index))))

(defun char-code (character)
  (check-type character character)
  (logand (ash (sys.int::lisp-object-address character) -4) #x1FFFFF))

(defun copy-string-in-area (string &optional area)
  (cond ((sys.int::character-array-p string)
         (let* ((data (sys.int::%complex-array-storage string))
                (len (or (sys.int::%complex-array-fill-pointer string)
                         (sys.int::%object-header-data data)))
                (tag (sys.int::%object-tag data))
                (elt-size (ecase tag
                            (#.sys.int::+object-tag-array-unsigned-byte-8+
                             1)
                            (#.sys.int::+object-tag-array-unsigned-byte-16+
                             2)
                            (#.sys.int::+object-tag-array-unsigned-byte-32+
                             4)))
                (new-data (mezzano.runtime::%allocate-object
                           tag
                           len
                           (ceiling (* len elt-size) 8)
                           area))
                (new-header (mezzano.runtime::%allocate-object
                             sys.int::+object-tag-simple-string+
                             1
                             (+ 3 1)
                             area)))
           (setf (sys.int::%complex-array-storage new-header) new-data
                 (sys.int::%complex-array-fill-pointer new-header) nil
                 (sys.int::%complex-array-info new-header) nil
                 (sys.int::%complex-array-dimension new-header 0) len)
           (dotimes (i len)
             (let ((val (ecase tag
                          (#.sys.int::+object-tag-array-unsigned-byte-8+
                           (sys.int::%object-ref-unsigned-byte-8 data i))
                          (#.sys.int::+object-tag-array-unsigned-byte-16+
                           (sys.int::%object-ref-unsigned-byte-16 data i))
                          (#.sys.int::+object-tag-array-unsigned-byte-32+
                           (sys.int::%object-ref-unsigned-byte-32 data i)))))
               (ecase tag
                 (#.sys.int::+object-tag-array-unsigned-byte-8+
                  (setf (sys.int::%object-ref-unsigned-byte-8 new-data i) val))
                 (#.sys.int::+object-tag-array-unsigned-byte-16+
                  (setf (sys.int::%object-ref-unsigned-byte-16 new-data i) val))
                 (#.sys.int::+object-tag-array-unsigned-byte-32+
                  (setf (sys.int::%object-ref-unsigned-byte-32 new-data i) val)))))
           new-header))
        (t
         (error "TODO: copy non-character-array strings"))))
