(in-package :mcclim-render-internals)

;;;
;;; Port
;;;

(defclass render-port-mixin (basic-port)
  ((all-font-families :initform nil :accessor all-font-families)))

;;; change geometry

(defmethod port-set-mirror-region :after ((port render-port-mixin) mirror region)
  (let ((sheet (port-lookup-sheet port mirror)))
    (%set-image-region (sheet-mirror sheet) region)))

(defmethod port-set-mirror-transformation :after ((port render-port-mixin) mirror transformation)
  (declare (ignore port mirror transformation))
  nil)

;;; realize/destroy mirrors
(defmethod realize-mirror ((port render-port-mixin) (sheet image-sheet-mixin)))

;;;
;;; Fonts
;;;

(defmethod text-style-mapping ((port render-port-mixin) (text-style standard-text-style)
                               &optional character-set
                               &aux (text-style (climb:parse-text-style* text-style)))
  (declare (ignore character-set))
  (labels
      ((find-and-make-truetype-font (family face size)
         (let* ((font-path-maybe-relative
                 (cdr (assoc (list family face) *families/faces*
                             :test #'equal)))
                (font-path
                 (and font-path-maybe-relative
                      (case (car (pathname-directory
                                  font-path-maybe-relative))
                        (:absolute font-path-maybe-relative)
                        (otherwise (merge-pathnames
                                    font-path-maybe-relative
                                    (or *truetype-font-path* "")))))))
           (if (and font-path (probe-file font-path))
               (make-truetype-font port font-path size)
               ;; We could error here, but we want to fallback to
               ;; fonts provided by CLX server. Its better to have
               ;; ugly fonts than none at all.
               (error 'missing-font
                      :filename font-path
                      :text-style text-style))))
       (find-font ()
         (multiple-value-call #'find-and-make-truetype-font
           (clim:text-style-components text-style))))
    (or (find-truetype-font port text-style)
        (invoke-with-truetype-path-restart #'find-font))))

(defmethod clim-internals::text-style-size ((gs-text-style cons))
  (caddr gs-text-style))

(defmethod clim-extensions:port-all-font-families :around
    ((port render-port-mixin) &key invalidate-cache)
  (declare (ignore invalidate-cache))
  (register-all-ttf-fonts port)
  (append (call-next-method) (all-font-families port)))

(let ((font-loader-cache (make-hash-table :test #'equal))
      (font-families     (make-hash-table :test #'equal))
      (font-faces        (make-hash-table :test #'equal))
      (font-cache        (make-hash-table :test #'equal))
      (text-style-cache  (make-hash-table :test #'equal)))
  (defun make-truetype-font (port filename size)
    (clim-sys:with-lock-held (*zpb-font-lock*)
      (let* ((loader (ensure-gethash filename font-loader-cache
                                     (zpb-ttf:open-font-loader filename)))
             (family-name (zpb-ttf:family-name loader))
             (family (ensure-gethash family-name font-families
                                     (make-instance 'mcclim-truetype::truetype-font-family
                                                    :port port
                                                    :name (zpb-ttf:family-name loader))))
             (face-name (zpb-ttf:subfamily-name loader))
             (font-face (ensure-gethash
                         (list family-name face-name) font-faces
                         (make-instance 'truetype-face
                                        :family family
                                        :name (zpb-ttf:subfamily-name loader)
                                        :loader loader)))
             (font (ensure-gethash
                    (list loader size) font-cache
                    (make-instance 'render-truetype-font
                                   :face font-face
                                   :size size))))
        (pushnew family (all-font-families port))
        (ensure-gethash
         (list port (make-text-style family-name face-name size))
         text-style-cache
         font))))
  (defun find-truetype-font (port text-style)
    (gethash (list port text-style) text-style-cache)))

(defun register-all-ttf-fonts (port &optional (dir *truetype-font-path*))
  (when *truetype-font-path*
    (dolist (path (directory (merge-pathnames "*.ttf" dir)))
      ;; make-truetype-font make fail if zpb can't load the particular
      ;; file - in that case it signals an error and no font is
      ;; created. In that case we just skip that file- hence IGNORE-ERRORS.
      (ignore-errors
        (map () #'(lambda (size)
                    (make-truetype-font port path size))
             '(8 10 12 14 18 24 48 72))))))
