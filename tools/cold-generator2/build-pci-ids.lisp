;;;; Load pci.ids from pciids.sourceforge.net and generate
;;;; a reasonably efficient representation.

;;;; BUILD-PCI-IDS generates:
;;;; pci-ids: #({vendor-id vendor-name devices})
;;;; devices: #({device-id device-name subsystems})
;;;; subsystems: #({subsystem-id subsystem-name})
;;;; vendor-id: (unsigned-byte 16)
;;;; device-id: (unsigned-byte 16)
;;;; vendor-name: string
;;;; device-name: string
;;;; subsystem-name: string
;;;; subsystem-id: (unsigned-byte 32)
;;;; The subsystem-id is constructed by shifting the subsystem vendor id
;;;; left by 16 bits and ORing it with the subsystem device id.
;;;; All arrays are sorted by id, allowing fast lookup using a binary search.

(defpackage :mezzano.cold-generator.build-pci-ids
  (:export :build-pci-ids)
  (:use :cl :iter))

(in-package :mezzano.cold-generator.build-pci-ids)

(defvar *vendor-regex* (cl-ppcre:create-scanner "^([0123456789ABCDEFabcdef]{4})\\s+(.*)$"))
(defvar *device-regex* (cl-ppcre:create-scanner "^\\t([0123456789ABCDEFabcdef]{4})\\s+(.*)$"))
(defvar *subsystem-regex* (cl-ppcre:create-scanner "^\\t\\t([0123456789ABCDEFabcdef]{4})\\s+([0123456789ABCDEFabcdef]{4})\\s+(.*)$"))

(defun read-pci-id-data (pathspec)
  "Read in the pci.ids file and return sorted lists of vendors, devices, and subdevices."
  (flet ((hex (s) (parse-integer s :radix 16)))
    (iter (for line in-file pathspec using 'read-line)
          (with vendor-id)
          (with vendor-name)
          (with device-id)
          (with device-name)
          (with devices)
          (with subsystems)
          (cl-ppcre:register-groups-bind ((#'hex id) name)
              (*vendor-regex* line)
            (when vendor-id
              ;; Finish the previous device, if any.
              (when device-id
                (push (list device-id
                            device-name
                            (sort subsystems '< :key 'first))
                      devices)
                (setf device-id nil))
              ;; Finish the previous vendor.
              (collect (list vendor-id
                             vendor-name
                             (sort devices '< :key 'first))))
            (setf vendor-id id
                  vendor-name name
                  device-id nil
                  devices '())
            #+nil(format t "Vendor ~4,'0X: ~S~%" id name))
          (cl-ppcre:register-groups-bind ((#'hex id) name)
              (*device-regex* line)
            (when device-id
              ;; Finish the previous device.
              (push (list device-id
                          device-name
                          (sort subsystems '< :key 'first))
                    devices))
            (setf device-id id
                  device-name name
                  subsystems '())
            #+nil(format t "  Device ~4,'0X: ~S~%" id name))
          (cl-ppcre:register-groups-bind ((#'hex vid) (#'hex did) name)
              (*subsystem-regex* line)
            ;; Subsystems store the Vendor and Device IDs as a combined integer, to
            ;; make searching easier.
            (push (list (logior (ash vid 16) did)
                        name)
                  subsystems)
            #+nil(format t "    Subsystem ~8,'0X: ~S~%" (logior (ash vid 16) did) name)))))

(defun compactify-pci-subsystems (subsystem-list)
  (when subsystem-list
    (let ((subsystems (make-array (* (length subsystem-list) 2)))
          (position 0))
      (iter (for (combined-id name) in subsystem-list)
            (setf (aref subsystems (+ position 0)) combined-id
                  (aref subsystems (+ position 1)) name)
            (incf position 2))
      subsystems)))

(defun compactify-pci-device-data (device-list)
  (when device-list
    (let ((devices (make-array (* (length device-list) 3)))
          (position 0))
      (iter (for (id name subsystems) in device-list)
            (setf (aref devices (+ position 0)) id
                  (aref devices (+ position 1)) name
                  (aref devices (+ position 2)) (compactify-pci-subsystems subsystems))
            (incf position 3))
      devices)))

(defun compactify-pci-data (data)
  (let ((vendor-data (make-array (* (length data) 3)))
        (position 0))
    (iter (for (id name devices) in data)
          (setf (aref vendor-data (+ position 0)) id)
          (setf (aref vendor-data (+ position 1)) name)
          (setf (aref vendor-data (+ position 2)) (compactify-pci-device-data devices))
          (incf position 3))
    vendor-data))

(defun build-pci-ids (pathspec)
  (compactify-pci-data (read-pci-id-data pathspec)))

(defun bsearch (item vector &optional (stride 1))
  "Locate ITEM using a binary search through VECTOR."
  (do ((imin 0)
       (imax (length vector)))
      ((< imax imin) nil)
    (let* ((imid (* (truncate (truncate (+ imin imax) stride) 2) stride))
           (elt (aref vector imid)))
      (format t "~D ~D ~D   ~D~%" imin imid imax elt)
      (cond ((< elt item) (setf imin (1+ imid)))
            ((> elt item) (setf imax (1- imid)))
            (t (return imid))))))

(defun find-vendor (id ids)
  (let ((position (bsearch id ids 3)))
    (when position
      (values (aref ids (+ position 1))
              (aref ids (+ position 2))))))

(defun find-device (vid did ids)
  (multiple-value-bind (vname devices)
      (find-vendor vid ids)
    (when (and vname devices)
      (let ((position (bsearch did devices 3)))
        (when position
          (values vname
                  (aref devices (+ position 1))
                  (aref devices (+ position 2))))))))

(defun find-subsystem (vid did svid sdid ids)
  (multiple-value-bind (vname dname subsystems)
      (find-device vid did ids)
    (when (and vname subsystems)
      (let ((position (bsearch (logior (ash svid 16) sdid) subsystems 2)))
        (if position
            (values vname dname (aref subsystems (1+ position)))
            (values vname dname nil))))))
