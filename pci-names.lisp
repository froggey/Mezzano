;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(declaim (special *pci-ids*))

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
