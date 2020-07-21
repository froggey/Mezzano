;;;; Support code with no specific home.

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

(defmacro define-doubly-linked-list-helpers (name element-next element-prev list-head list-tail)
  `(progn
     (defmacro ,(intern (format nil "DO-~A" name)) ((element list &optional result-form) &body body)
       `(do ((,element (,',list-head ,list) (,',element-next ,element)))
            ((null ,element)
             ,result-form)
          ,@body))
     (defun ,(intern (format nil "~A-EMPTY-P" name)) (list)
       (null (,list-head list)))
     (defun ,(intern (format nil "~A-LINKED-P" name)) (element)
       (not (eql (,element-next element) :unlinked)))
     (defun ,(intern (format nil "~A-PUSH-FRONT" name)) (element list)
       (setf (,element-prev element) nil)
       (cond ((,list-head list)
              ;; List not empty
              (setf (,element-next element) (,list-head list))
              (setf (,element-prev (,list-head list)) element
                    (,list-head list) element))
             (t
              ;; List empty
              (setf (,element-next element) nil)
              (setf (,list-head list) element
                    (,list-tail list) element)))
       element)
     (defun ,(intern (format nil "~A-PUSH-BACK" name)) (element list)
       (setf (,element-next element) nil)
       (cond ((,list-tail list)
              ;; List not empty
              (setf (,element-prev element) (,list-tail list))
              (setf (,element-next (,list-tail list)) element
                    (,list-tail list) element))
             (t
              ;; List empty
              (setf (,element-prev element) nil)
              (setf (,list-head list) element
                    (,list-tail list) element)))
       element)
     (defun ,(intern (format nil "~A-INSERT-BEFORE" name)) (element existing list)
       (cond ((eql (,list-head list) existing)
              (setf (,list-head list) element))
             (t
              (setf (,element-next (,element-prev existing)) element)))
       (setf (,element-prev element) (,element-prev existing)
             (,element-next element) existing)
       (setf (,element-prev existing) element)
       element)
     (defun ,(intern (format nil "~A-INSERT-AFTER" name)) (element existing list)
       (cond ((eql (,list-tail list) existing)
              (setf (,list-tail list) element))
             (t
              (setf (,element-prev (,element-next existing)) element)))
       (setf (,element-next element) (,element-next existing)
             (,element-prev element) existing)
       (setf (,element-next existing) element)
       element)
     (defun ,(intern (format nil "~A-POP-FRONT" name)) (list)
       (let ((element (,list-head list)))
         (when element
           (cond ((eql (,list-tail list) element)
                  ;; head=tail, this is the last element.
                  (setf (,list-head list) nil
                        (,list-tail list) nil))
                 (t
                  ;; Pop stuff.
                  (setf (,element-prev (,element-next element)) nil
                        (,list-head list) (,element-next element))))
           (setf (,element-next element) :unlinked
                 (,element-prev element) :unlinked))
         element))
     (defun ,(intern (format nil "~A-POP-BACK" name)) (list)
       (let ((element (,list-tail list)))
         (when element
           (cond ((eql (,list-head list) element)
                  ;; head=tail, this is the last element.
                  (setf (,list-tail list) nil
                        (,list-head list) nil))
                 (t
                  ;; Pop stuff.
                  (setf (,element-next (,element-prev element)) nil
                        (,list-tail list) (,element-prev element))))
           (setf (,element-next element) :unlinked
                 (,element-prev element) :unlinked))
         element))
     (defun ,(intern (format nil "~A-REMOVE" name)) (element list)
      (cond ((and (eql (,list-head list) element)
                  (eql (,list-tail list) element))
             ;; Only element in the list
             (setf (,list-head list) nil
                   (,list-tail list) nil))
            ((eql (,list-head list) element)
             ;; More than one element, at head.
             (setf (,element-prev (,element-next element)) nil)
             (setf (,list-head list) (,element-next element)))
            ((eql (,list-tail list) element)
             ;; More than one element, at tail.
             (setf (,element-next (,element-prev element)) nil)
             (setf (,list-tail list) (,element-prev element)))
            (t
             ;; Somewhere in the middle of the run queue.
             (setf (,element-next (,element-prev element)) (,element-next element)
                   (,element-prev (,element-next element)) (,element-prev element))))
      (setf (,element-next element) :unlinked
            (,element-prev element) :unlinked)
      element)))

(defmacro with-device-access ((device-boot-id inaccessible-form) &body body)
  `(with-snapshot-inhibited ()
     (cond ((not (eql ,device-boot-id *boot-id*))
            ,inaccessible-form)
           (t ,@body))))
