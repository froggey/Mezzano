;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(declaim (special *pci-ids*))

(defun bsearch (item vector &key (stride 1) (key 'identity))
  "Locate ITEM using a binary search through VECTOR."
  ;; IMIN/IMAX are inclusive indicies.
  (do ((imin 0)
       (imax (1- (truncate (length vector) stride))))
      ((< imax imin)
       nil)
    (let* ((imid (truncate (+ imin imax) 2))
           (elt (funcall key (aref vector (* imid stride)))))
      (cond ((< elt item) (setf imin (1+ imid)))
            ((> elt item) (setf imax (1- imid)))
            (t (return (* imid stride)))))))

(defun pci-find-vendor (id &optional (ids *pci-ids*))
  (let ((position (bsearch id ids :stride 3)))
    (when position
      (values (aref ids (+ position 1))
              (aref ids (+ position 2))))))

(defun pci-find-device (vid did &optional (ids *pci-ids*))
  (multiple-value-bind (vname devices)
      (pci-find-vendor vid ids)
    (when (and vname devices)
      (let ((position (bsearch did devices :stride 3)))
        (when position
          (values vname
                  (aref devices (+ position 1))
                  (aref devices (+ position 2))))))))

(defun pci-find-subsystem (vid did svid sdid &optional (ids *pci-ids*))
  (multiple-value-bind (vname dname subsystems)
      (pci-find-device vid did ids)
    (when (and vname subsystems)
      (let ((position (bsearch (logior (ash svid 16) sdid) subsystems :stride 2)))
        (if position
            (values vname dname (aref subsystems (1+ position)))
            (values vname dname nil))))))
