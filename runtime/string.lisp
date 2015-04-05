;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

;; Hardcoded string accessor, the support stuff for arrays doesn't function at this point.
(defun char (string index)
  (assert (sys.int::character-array-p string) (string))
  (let ((data (sys.int::%complex-array-storage string)))
    (assert (and (<= 0 index)
                 (< index (sys.int::%object-header-data data)))
            (string index))
    (sys.int::%%assemble-value
     (ash (case (sys.int::%object-tag data)
            (#.sys.int::+object-tag-array-unsigned-byte-8+
             (sys.int::%object-ref-unsigned-byte-8 data index))
            (#.sys.int::+object-tag-array-unsigned-byte-16+
             (sys.int::%object-ref-unsigned-byte-16 data index))
            (#.sys.int::+object-tag-array-unsigned-byte-32+
             (sys.int::%object-ref-unsigned-byte-32 data index))
            (t 0))
          4)
     sys.int::+tag-character+)))

(defun char-code (character)
  (check-type character character)
  (logand (ash (sys.int::lisp-object-address character) -4) #x1FFFFF))
