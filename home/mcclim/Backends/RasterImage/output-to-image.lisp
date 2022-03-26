(in-package :mcclim-raster-image)

;;;
;;; with-output-to ...
;;;

(defmacro with-output-to-raster-image-stream ((stream-var stream format &rest options)
                                              &body body)
  (alexandria:with-gensyms (cont exit-fn enter-fn)
    `(flet ((,cont (,stream-var)
              ,@body)
            (,exit-fn (sheet stream)
              (declare (ignorable stream))
              (climi::write-bitmap-file
               (image-mirror-image (sheet-mirror sheet))
               ,stream :format ,format))
            (,enter-fn (sheet stream)
              (declare (ignore sheet stream))
              nil))
       (declare (dynamic-extent #',cont))
       (invoke-with-output-to-raster-image
        #',cont #',enter-fn #',exit-fn :rgb-image ,format ,@options))))

(defun extract-format (file)
  (alexandria:make-keyword (string-upcase (pathname-type (pathname file)))))

(defmacro with-output-to-raster-image-file ((stream-var file &rest options)
                                            &body body)
  (alexandria:with-gensyms (format cont exit-fn enter-fn)
    `(let ((,format (extract-format ,file)))
       (flet ((,cont (,stream-var)
                ,@body)
              (,exit-fn (sheet stream)
                (declare (ignore stream))
                (climi::write-bitmap-file
                 (image-mirror-image (sheet-mirror sheet))
                 ,file :format ,format))
              (,enter-fn (sheet stream)
                (declare (ignore sheet stream))
                nil))
         (declare (dynamic-extent #',cont))
         (invoke-with-output-to-raster-image
          #',cont #',enter-fn #',exit-fn :rgb-image ,format ,@options)))))

(defmacro with-output-to-image ((stream-var image &rest options)
                                &body body)
  (alexandria:with-gensyms (cont exit-fn enter-fn)
    (alexandria:once-only (image)
      `(flet ((,cont (,stream-var)
                ,@body)
              (,exit-fn (sheet stream)
                (declare (ignore stream))
                (image-mirror-image (sheet-mirror sheet)))
              (,enter-fn (sheet stream)
                (declare (ignore stream))
                (when ,image
                  (setf (image-mirror-image sheet) ,image))))
         (declare (dynamic-extent #',cont))
         (invoke-with-output-to-raster-image
          #',cont #',enter-fn #',exit-fn :rgb-image :rgb-image ,@options)))))

(defmacro with-output-to-rgba-pattern ((stream-var &rest options)
                                       &body body)
  `(with-output-to-image (,stream-var nil ,@options)
     ,@body))

(defmacro with-output-to-image-pattern ((stream-var &rest options)
                                        &body body)
  `(with-output-to-image (,stream-var nil ,@options)
     ,@body))

(defun invoke-with-output-to-raster-image (continuation enter-fn exit-fn server format
                                           &key (width :compute) (height :compute)
                                                (border-width 0) (recording-p t))
  (with-port (port server :width 1 :height 1)
    (let ((top-level-sheet (make-raster-top-level-sheet port format))
          (vbox (make-instance 'vbox-pane :port port))
          (border-pane (make-instance 'climi::border-pane
                                      :port port
                                      :border-width border-width))
          (stream (make-raster-image-stream port)))
      (sheet-adopt-child border-pane stream)
      (sheet-adopt-child vbox border-pane)
      (sheet-adopt-child top-level-sheet vbox)
      (realize-mirror port top-level-sheet)
      (funcall enter-fn top-level-sheet stream)

      ;; When WIDTH or HEIGHT is :COMPUTE, render into an output
      ;; record and use its dimensions to change the space
      ;; requirements of STREAM. When WIDTH is :COMPUTE, do this two
      ;; times: one time to determine the required width and a second
      ;; time to determine the resulting height when using the
      ;; computed width.
      (if (or (eq width :compute) (eq height :compute))
          (flet ((try ()
                   (let ((record (with-output-to-output-record (stream)
                                   (funcall continuation stream))))
                     ;; FIXME Enlarging the space requirements a bit
                     ;; is needed to prevent things from getting
                     ;; clipped.
                     (change-space-requirements
                      stream
                      :width (if (eq width :compute)
                                 (+ (bounding-rectangle-width record) 2)
                                 width)
                      :height (if (eq height :compute)
                                  (+ (bounding-rectangle-height record) 2)
                                  height)))))
            ;; Ensure STREAM's preferred width is set to something
            ;; reasonable, then call CONTINUATION and update STREAM's
            ;; space requirements.
            (change-space-requirements
             stream :width (if (eq width :compute) 1000 width))
            (try)
            (when (eq width :compute)
              (try)))
          (change-space-requirements stream :width width :height height))

      (with-output-recording-options (stream :record recording-p :draw t)
        (funcall continuation stream)
        (medium-finish-output (sheet-medium stream)))
      (values (funcall exit-fn top-level-sheet stream)
              (when recording-p
                (stream-output-history stream))))))
