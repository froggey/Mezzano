;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Support code with no specific home.

(in-package :mezzano.supervisor)

;; fixme: multiple-evaluation of PLACE.
(defmacro push-wired (item place)
  "Like PUSH, but the CONS is allocated in the wired area."
  `(setf ,place (sys.int::cons-in-area ,item ,place :wired)))

(defun string-length (string)
  "Return the length of STRING. For use when calling LENGTH is not safe."
  (assert (sys.int::character-array-p string))
  (or (sys.int::%complex-array-fill-pointer string)
      (sys.int::%complex-array-dimension string 0)))

(defun align-up (value power-of-two)
  "Align VALUE up to the nearest multiple of POWER-OF-TWO."
  (logand (+ value (1- power-of-two)) (lognot (1- power-of-two))))

(defun align-down (value power-of-two)
  "Align VALUE down to the nearest multiple of POWER-OF-TWO."
  (logand value (lognot (1- power-of-two))))

(defun bsearch (item vector &key (start 0) end (stride 1))
  "Locate ITEM using a binary search through VECTOR."
  ;; IMIN/IMAX are inclusive indicies.
  (do ((imin start)
       (imax (1- (truncate (or end (sys.int::simple-vector-length vector)) stride))))
      ((< imax imin)
       nil)
    (let* ((imid (truncate (+ imin imax) 2))
           (elt (svref vector (* imid stride))))
      (cond ((< elt item) (setf imin (1+ imid)))
            ((> elt item) (setf imax (1- imid)))
            (t (return (* imid stride)))))))
