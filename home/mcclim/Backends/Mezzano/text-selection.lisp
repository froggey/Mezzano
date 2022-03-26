(in-package #:clim-mezzano)

;; (defmethod clim-backend:bind-selection
;;     ((port mezzano-port) (sheet basic-sheet) &optional time)
;;   (declare (ignore time)))
;;
;; (defmethod clim-backend:release-selection ((port mezzano-port) &optional time)
;;   (declare (ignore time)))
;;
;; (defmethod request-selection :around ((port mezzano-port) requestor time)
;;   (declare (ignore requestor time)))
;;
;; (defmethod clim-backend:selection-owner ((port mezzano-port))
;;   (clim-standard::port-selection-owner port))
;;
;; (defmethod (setf clim-backend:selection-owner) (owner (port mezzano-port))
;;   (setf (clim-standard::port-selection-owner port) owner))
;;
;; (defmethod clim-backend::selection-requester ((port mezzano-port))
;;   (clim-standard::port-selection-requester port))
;;
;; (defmethod (setf clim-backend::selection-requester)
;;     (requester (port mezzano-port))
;;   (setf (clim-standard::port-selection-requester port) requester))
