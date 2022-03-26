(in-package :clim-demo)

(defparameter *render-image-tests* (make-hash-table :test 'equal))

(defparameter *render-image-width* 510)
(defparameter *render-image-height* 700)
(defparameter *render-image-border-width* 5)

(defparameter *testing-image-directory* (uiop/pathname:merge-pathnames* "Examples/images/" (asdf:system-source-directory (asdf:find-system :mcclim))))
(defparameter *testing-image-files* '("RGBXPLORER8.png"
                                      "White_Balance_RGB.png"
                                      "MicroGrayTest.png"))

(defparameter *testing-image-rgb-file* "RGBXPLORER8.png")
(defparameter *testing-image-bn1-file* "White_Balance_RGB.png")
(defparameter *testing-image-bn2-file* "MicroGrayTest.png")

(defstruct render-image-test name description drawer)

(defmacro define-render-image-test (name arglist description &body body)
  (check-type name string)
  (check-type description string)
  `(setf (gethash ,name *render-image-tests*)
         (make-render-image-test :name ,name
                                 :description ,description
                                 :drawer (lambda ,arglist ,@body))))

(define-application-frame render-image-tests ()
  ((signal-condition-p :initform nil)
   (current-selection :initform nil))
  (:menu-bar nil)
  (:panes
   (output :application-pane
           :min-width *render-image-width*
           :min-height *render-image-height*
           :display-time nil
           :display-function #'render-image-display-output
           :end-of-line-action :wrap
           :end-of-page-action :wrap)
   (description :application-pane)
   (selector :list-pane
             :mode :exclusive
             :name-key #'render-image-test-name
             :items (sort (loop for x being the hash-values of *render-image-tests*
                                collect x) #'string< :key #'render-image-test-name)
             :value-changed-callback #'render-image-update-selection)
   (condition-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback 'render-image-update-condition-option)
      (clim:radio-box-current-selection "message")
      "break")))
  (:layouts
   (default
       (spacing (:thickness 3)
         (horizontally ()
           (vertically ()
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (scrolling (:scroll-bar :vertical :height *render-image-height*) selector)))
             (labelling (:label "Condition")
               condition-option))
           (vertically ()
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (horizontally ()
                   (labelling (:label "Stream")
                     (climi::bordering (:border-width *render-image-border-width*)
                       output)))))
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (scrolling (:scroll-bar :vertical :height 200) description)))))))))

(defun render-image-update-condition-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (signal-condition-p) clim:*application-frame*
    (setf signal-condition-p
	  (string= (clim:gadget-label selected-gadget) "break"))))

(defun render-image-update-selection (pane item)
  (declare (ignore pane))
  (with-slots (current-selection) clim:*application-frame*
    (setf current-selection item))
  (window-clear (get-frame-pane *application-frame* 'description))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'output) :force-p t))

(defun render-image-display-output (frame pane)
  (declare (ignore pane))
  (let ((output (get-frame-pane frame 'output))
        (item (slot-value frame 'current-selection)))
    (let ((description (get-frame-pane *application-frame* 'description)))
      (when item
        (with-text-style (description (make-text-style :sans-serif :roman :normal))
          (write-string (render-image-test-description item) description)
          (terpri description))
        (if (slot-value *application-frame* 'signal-condition-p)
            (clim:with-drawing-options (output :clipping-region
                                               (clim:make-rectangle* 0 0 *render-image-width* *render-image-height*))
              (clim:draw-rectangle* output 0 0 *render-image-width* *render-image-height* :filled t
                                    :ink clim:+grey90+)
              (funcall (render-image-test-drawer item) output))
            (handler-case
                (clim:with-drawing-options (output :clipping-region
                                                   (clim:make-rectangle* 0 0 *render-image-width* *render-image-height*))
                  (clim:draw-rectangle* output 0 0 *render-image-width* *render-image-height* :filled t
                                        :ink clim:+grey90+)
                  (funcall (render-image-test-drawer item) output))
              (condition (condition)
                (clim:with-drawing-options (description :ink +red+)
                  (format description "Error:~a~%" condition)))))))))


(defun run-render-image-tests ()
  (run-frame-top-level
   (make-application-frame
    'render-image-tests)))

(defun render-image-test-make-rgba-image (w h color)
  (let* ((image (make-instance 'climi::%rgba-pattern
                               :array (make-array (list h w)
                                                  :element-type '(unsigned-byte 32)
                                                  :initial-element #xFFFFFF00)))
         (pixels (climi::pattern-array image)))
    (dotimes (x w)
      (dotimes (y h)
        (setf (aref pixels y x) (climi::%rgba-value color))))
    image))

(defun render-image-test-01-2d (stream)
  (flet ((draw-rect (color w h)
           (let ((image (render-image-test-make-rgba-image 90 70 color)))
             (clim-render:draw-image* stream image w h))))
    (draw-rect #xFFFFFFFF 10 10)
    (draw-rect #x000000FF 110 10)
    (draw-rect #x0000F0FF 10 100)
    (draw-rect #x00F000FF 110 100)
    (draw-rect #xF00000FF 210 100)
    (draw-rect #x800080FF 10 200)
    (draw-rect #x808000FF 110 200)))

(defun render-image-test-02 (stream)
  (flet ((draw-rect (color w h)
           (let ((image (render-image-test-make-rgba-image 90 70 color)))
             (clim-render:draw-image* stream image w h))))
    (draw-rect +white+ 10 10)
    (draw-rect +black+ 110 10)
    (draw-rect +red+ 10 100)
    (draw-rect +green+ 110 100)
    (draw-rect +blue+ 210 100)
    (draw-rect +purple+ 10 200)
    (draw-rect +olive-drab+ 110 200)))

(defun render-image-test-03 (stream h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (clim-render:draw-image* stream image 10 h))))

(defun render-image-test-04 (stream)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (clim-render:draw-image* stream image 10 10))))

(defun render-image-test-05 (stream transformation)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (clim-render:draw-image* stream image 0 0 :transformation transformation))))

(defun render-image-test-06 (stream transformation clipping-region)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          clipping-region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (transform-region transformation clipping-region)
        (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
      (clim-render:draw-image* stream image 0 0
                              :transformation transformation
                              :clipping-region clipping-region))))

(defun render-image-test-08 (stream h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (draw-design stream image :x 10 :y h))))

(defun render-image-test-09 (stream transformation)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (draw-design stream image :x 0 :y 0 :transformation transformation))))

(defun render-image-test-10 (stream transformation clipping-region)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          clipping-region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (transform-region transformation clipping-region)
        (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
      (draw-design stream image :x 0 :y 0
                   :transformation transformation
                   :clipping-region clipping-region))))

(defun render-image-test-16 (stream design cx cy cw ch w h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (make-pattern-from-bitmap-file path)))
      (clim-render:fill-image image design :x cx :y cy :width cw :height ch)
      (clim-render:draw-image* stream image w h))))

(define-render-image-test "2d - 01) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (render-image-test-01-2d stream))

(define-render-image-test "2d - 02) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (render-image-test-02 stream))

(define-render-image-test "2d - 03) read rgb" (stream)
    ""
  (render-image-test-03 stream 10))


(define-render-image-test "2d - 04) read gray" (stream)
    ""
  (render-image-test-04 stream))

(define-render-image-test "2d - 05) translate" (stream)
    ""
  (render-image-test-05 stream (clim:make-translation-transformation 10 10)))

(define-render-image-test "2d - 06) clipping" (stream)
    ""
  (render-image-test-06 stream
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250)))

(define-render-image-test "2d - 07) with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (render-image-test-03 stream 0)))

(define-render-image-test "2d - 08) design draw" (stream)
    ""
  (render-image-test-08 stream 10))

(define-render-image-test "2d - 09) design translate" (stream)
    ""
  (render-image-test-09 stream (clim:make-translation-transformation 10 10)))

(define-render-image-test "2d - 10) design clipping" (stream)
    ""
  (render-image-test-10 stream
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250)))

(define-render-image-test "2d - 11) design with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (render-image-test-08 stream 0)))

(define-render-image-test "2d - 16) fill color" (stream)
    ""
  (render-image-test-16 stream
                        +red+
                        100 100 100 150
                        10 10)
  (render-image-test-16 stream
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))

;;;
;;; indipendent
;;;

(define-render-image-test "zz - 01) output record :draw nil" (stream)
    ""
  (clim:with-output-to-output-record (stream)
    (with-output-recording-options (stream :record t :draw nil)
      (render-image-test-08 stream 10)
      (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t))))

(define-render-image-test "zz - 01) output record moving" (stream)
    ""
  (let ((record
         (clim:with-output-to-output-record (stream)
           (with-output-recording-options (stream :record t :draw t)
             (render-image-test-08 stream 10)
             (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))
    (setf (clim:output-record-position record) (values 10 310))
    (replay record stream)))
