(in-package :pngload)

(defvar *decode-data* nil)
(defvar *flatten* nil)

(defun get-image-bytes ()
  (with-slots (width height interlace-method) *png-object*
    (ecase interlace-method
      (:null
       (+ height (* height (get-scanline-bytes width))))
      (:adam7
       (loop :for (width height) :in (calculate-sub-image-dimensions)
             :sum (* height (1+ (get-scanline-bytes width))))))))

(defun get-image-raw-channels ()
  (with-slots (color-type) *png-object*
    (ecase color-type
      ((:truecolour :indexed-colour) 3)
      (:truecolour-alpha 4)
      (:greyscale-alpha 2)
      (:greyscale 1))))

(defun get-image-channels ()
  (with-slots (color-type transparency) *png-object*
    (let ((channels (get-image-raw-channels)))
      (when transparency
        (assert (member color-type '(:truecolour :indexed-colour :greyscale)))
        (incf channels))
      channels)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (alexandria:define-constant +filter-type-none+ 0)
  (alexandria:define-constant +filter-type-sub+ 1)
  (alexandria:define-constant +filter-type-up+ 2)
  (alexandria:define-constant +filter-type-average+ 3)
  (alexandria:define-constant +filter-type-paeth+ 4))

(defun allocate-image-data ()
  (with-slots (width height color-type bit-depth transparency) *png-object*
    (let* ((channels (get-image-channels))
           (args (list (if *flatten*
                           (* width height channels)
                           `(,height ,width ,@(when (> channels 1) (list channels))))
                       :element-type (ecase bit-depth
                                       ((1 2 4 8) 'ub8)
                                       (16 'ub16)))))
      (when *use-static-vector* (assert *flatten*))
      (if *use-static-vector*
          (apply #'static-vectors:make-static-vector args)
          (apply #'make-array args)))))

(declaim (inline unfilter-sub))
(defun unfilter-sub (x data start pixel-bytes)
  (declare (ub8a1d data)
           (ub8 pixel-bytes)
           (ub32 x)
           (fixnum start)
           (optimize speed))
  (if (>= x pixel-bytes)
      (aref data (+ start (- x pixel-bytes)))
      0))

(declaim (inline unfilter-up))
(defun unfilter-up (x y data start-up)
  (declare (ub8a1d data)
           (ub32 x y)
           (fixnum start-up)
           (optimize speed))
  (if (zerop y)
      0
      (aref data (+ x start-up))))

(declaim (inline unfilter-average))
(defun unfilter-average (x y data start start-up pixel-bytes)
  (declare (ub8a1d data)
           (ub32 x y)
           (fixnum start start-up)
           (ub8 pixel-bytes)
           (optimize speed))
  (let ((a (unfilter-sub x data start pixel-bytes))
        (b (unfilter-up x y data start-up)))
    (declare (ub8 a b))
    (floor (+ a b) 2)))

(declaim (inline unfilter-paeth))
(defun unfilter-paeth (x y data start-left start-up pixel-bytes)
  (declare (ub8a1d data)
           (ub32 x y)
           (fixnum start-left start-up)
           (ub8 pixel-bytes)
           (optimize speed))
  (let* ((a (unfilter-sub x data start-left pixel-bytes))
         (b (unfilter-up x y data start-up))
         (c (if (plusp y)
                (unfilter-sub x data start-up pixel-bytes)
                0))
         (p (- (+ a b) c))
         (pa (abs (- p a)))
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c))))

(declaim (inline unfilter-byte))
(defun unfilter-byte (filter x y data start start-up pixel-bytes)
  (ecase filter
    (#.+filter-type-none+ 0)
    (#.+filter-type-sub+ (unfilter-sub x data start pixel-bytes))
    (#.+filter-type-up+ (unfilter-up x y data start-up))
    (#.+filter-type-average+
     (unfilter-average x y data start start-up pixel-bytes))
    (#.+filter-type-paeth+
     (unfilter-paeth x y data start start-up pixel-bytes))))

(defun unfilter (data width height start)
  (declare (ub32 width height)
           (fixnum start)
           (ub8a1d data))
  (loop :with pixel-bytes = (get-pixel-bytes)
        :with scanline-bytes fixnum = (get-scanline-bytes width)
        :with row-bytes = (1+ scanline-bytes)
        :for y :below height
        :for in-start :from start :by row-bytes
        :for left-start :from start :by scanline-bytes
        :for up-start :from (- start scanline-bytes) :by scanline-bytes
        :for filter = (aref data in-start)
        :do (loop :for xs fixnum :from (1+ in-start)
                  :for xo fixnum :from left-start
                  :for x fixnum :below scanline-bytes
                  :for sample = (aref data xs)
                  :for out = (unfilter-byte filter x y data left-start up-start pixel-bytes)
                  :do (setf (aref data xo)
                            (ldb (byte 8 0) (+ sample out))))))

(defmacro maybe-flatten (dims bit-depth)
  (let ((nd-fn-sym (intern (format nil "COPY/~dD/~d" dims bit-depth)))
        (1d-fn-sym (intern (format nil "COPY/1D/~d" bit-depth)))
        (copy-fn-sym (intern (format nil "COPY/~d" bit-depth)))
        (copy-flip-fn-sym (intern (format nil "COPY/~d/FLIP" bit-depth)))
        (nd-type-sym (intern (format nil "UB~dA~dD" bit-depth dims)))
        (1d-type-sym (intern (format nil "UB~dA1D" bit-depth))))
    `(flet ((,nd-fn-sym ()
              (declare (,nd-type-sym data))
              (if *flip-y*
                  (,copy-flip-fn-sym)
                  (,copy-fn-sym)))
            (,1d-fn-sym ()
              (declare (,1d-type-sym data))
              (if *flip-y*
                  (,copy-flip-fn-sym)
                  (,copy-fn-sym))))
       (if *flatten*
           (,1d-fn-sym)
           (,nd-fn-sym)))))

(defmacro copy/8 ()
  `(loop :for d :below (array-total-size data)
         :for s :below (array-total-size image-data)
         :do (locally (declare (optimize speed (safety 0)))
               (setf (row-major-aref data d)
                     (aref image-data s)))))

(defmacro copy/8/flip ()
  `(with-slots (width height bit-depth) *png-object*
     (let* ((channels (get-image-raw-channels))
            (stride (* channels width))
            (ssize (array-total-size image-data))
            (dsize (array-total-size data)))
       (declare (fixnum ssize dsize)
                (type (unsigned-byte 34) stride))
       (loop :for dy :below height
             :for sy :downfrom (1- height)
             :for d1 := (* dy stride)
             :for s1 := (* sy stride)
             :do (assert (<= 0 (+ d1 stride) dsize))
                 (assert (<= 0 (+ s1 stride) ssize))
                 (locally (declare (optimize speed))
                   (loop :for s fixnum :from s1 :below ssize
                         :for d fixnum :from d1 :below dsize
                         :repeat stride
                         :do (locally (declare (optimize speed (safety 0)))
                               (setf (row-major-aref data d)
                                     (aref image-data s)))))))))

(defmacro copy/16 ()
  `(progn (assert (zerop (mod (array-total-size image-data) 2)))
          (loop :for d :below (array-total-size data)
                :for s :below (array-total-size image-data) :by 2
                :do (locally (declare (optimize speed (safety 0)))
                      (setf (row-major-aref data d)
                            (dpb (aref image-data s) (byte 8 8)
                                 (aref image-data (1+ s))))))))

(defmacro copy/16/flip ()
  `(with-slots (width height bit-depth) *png-object*
     (let* ((channels (get-image-raw-channels))
            (stride (* channels width))
            (ssize (array-total-size image-data))
            (dsize (array-total-size data)))
       (declare (fixnum ssize dsize)
                (type (unsigned-byte 34) stride))
       (loop :for dy :below height
             :for sy :downfrom (1- height)
             :for d1 := (* dy stride)
             :for s1 := (* sy stride 2)
             :do (assert (<= 0 (+ d1 stride) dsize))
                 (assert (<= 0 (+ s1 stride stride) ssize))
                 (locally (declare (optimize speed))
                   (loop :for s fixnum :from s1 :below ssize :by 2
                         :for d fixnum :from d1 :below dsize
                         :repeat stride
                         :do (locally (declare (optimize speed (safety 0)))
                               (setf (row-major-aref data d)
                                     (dpb (aref image-data s) (byte 8 8)
                                          (aref image-data (1+ s)))))))))))

(defun copy/pal/8 (image-data)
  (with-slots (data palette transparency) *png-object*
    (macrolet ((copy ()
                 `(loop :with c = (get-image-channels)
                        :for d :below (array-total-size data) :by c
                        :for s :across image-data
                        :do  (setf (row-major-aref data (+ d 0))
                                   (aref palette s 0)
                                   (row-major-aref data (+ d 1))
                                   (aref palette s 1)
                                   (row-major-aref data (+ d 2))
                                   (aref palette s 2))
                             (when transparency
                               (setf (row-major-aref data (+ d 3))
                                     (if (array-in-bounds-p transparency s)
                                         (aref transparency s)
                                         255))))))
      (if *flatten*
          (locally (declare (ub8a1d data)) (copy))
          (locally (declare (ub8a3d data)) (copy))))))

(defun copy/pal/sub (image-data)
  (with-slots (width height bit-depth palette transparency data) *png-object*
    (loop :with scanline-bytes = (get-scanline-bytes width)
          :with pixels-per-byte = (/ 8 bit-depth)
          :with channels = (get-image-channels)
          :with dstride = (* width channels)
          :for y :below height
          :for yb = (* y scanline-bytes)
          :do (flet (((setf data) (v y x c)
                       (setf (row-major-aref data (+ (* y dstride) (* x channels) c)) v)))
                (loop :for x :below width
                      :do (multiple-value-bind (b p) (floor x pixels-per-byte)
                            (let ((i (ldb (byte bit-depth (- 8 (* p bit-depth) bit-depth))
                                          (aref image-data (+ yb b)))))
                              (setf (data y x 0)
                                    (aref palette i 0)
                                    (data y x 1)
                                    (aref palette i 1)
                                    (data y x 2)
                                    (aref palette i 2))
                              (when transparency
                                (setf (data y x 3)
                                      (if (array-in-bounds-p transparency i)
                                          (aref transparency i)
                                          255))))))))))

(defun copy/2d/sub (image-data)
  (with-slots (width bit-depth data) *png-object*
    (declare (ub8a data))
    (loop :with s = 0
          :with x = 0
          :with bx = 0
          :with p = 0
          :with b = 0
          :with scanline-bytes = (get-scanline-bytes width)
          :with ssize = (array-total-size image-data)
          :for d :below (array-total-size data)
          :while (< (+ s bx) ssize)
          :when (zerop p)
            :do (setf b (aref image-data (+ s bx)))
          :do (setf (row-major-aref data d)
                    (ldb (byte bit-depth (- 8 p bit-depth)) b))
              (incf p bit-depth)
              (incf x)
              (cond
                ((>= x width)
                 (setf x 0
                       bx 0
                       p 0)
                 (incf s scanline-bytes))
                ((>= p 8)
                 (setf p 0)
                 (incf bx 1))))))

(defmacro trns (opaque)
  `(loop :with c = (get-image-channels)
         :with key = (etypecase transparency
                       (ub16 (make-array 1 :element-type 'ub16 :initial-element transparency))
                       (ub16a1d transparency))
         :for s :from (- (* width height (1- c)) (1- c)) :downto 0 :by (1- c)
         :for d :from (- (array-total-size data) c) :downto 0 :by c
         :do (loop :for i :below (1- c)
                   :for k :across key
                   :for v = (row-major-aref data (+ s i))
                   :do (setf (row-major-aref data (+ d i)) v)
                   :count (= v k) :into matches
                   :collect (list v k matches) :into foo
                   :finally (setf (row-major-aref data (+ d (1- c)))
                                  (if (= matches (1- c))
                                      0
                                      ,opaque)))))

(defun flip (image)
  (let ((w (width *png-object*))
        (h (height *png-object*))
        (c (get-image-channels)))
    (let ((stride (* w c))
          (end (array-total-size image)))
      (assert (plusp stride))
      (macrolet ((f (&key (opt t))
                   `(loop :for y1 :below (floor h 2)
                          :for y2 :downfrom (1- h) :above 0
                          :do (loop :for x1 :from (* y1 stride) :below end
                                    :for x2 :from (* y2 stride) :below end
                                    :repeat stride
                                    :do (,@(if opt
                                               '(locally (declare (optimize speed (safety 0))))
                                               '(progn))
                                         (rotatef (row-major-aref image x1)
                                                  (row-major-aref image x2)))))))
        (typecase image
          (ub8a3d (f))
          (ub8a2d (f))
          (ub8a1d (f))
          (ub16a3d (f))
          (ub16a2d (f))
          (ub16a1d (f))
          (t (f :opt nil)))))))

(defun maybe-flip (data)
  (when *flip-y*
    (flip data)))

(defun decode ()
  (let ((image-data (data *png-object*)))
    (declare (ub8a1d image-data))
    (with-slots (width height bit-depth interlace-method color-type transparency data) *png-object*
      (if (eq interlace-method :null)
          (unfilter image-data width height 0)
          (setf image-data (deinterlace-adam7 image-data)))
      (assert (and (typep bit-depth 'ub8)
                   (member bit-depth '(1 2 4 8 16))))
      (setf data (allocate-image-data))
      (let ((data data))
        (ecase color-type
          ((:truecolour :truecolour-alpha :greyscale-alpha)
           (ecase bit-depth
             (8 (maybe-flatten 3 8))
             (16 (maybe-flatten 3 16)))
           (when transparency
             (ecase bit-depth
               (8 (trns #xff))
               (16 (trns #xffff)))))
          (:greyscale
           (if transparency
               (ecase bit-depth
                 (8 (maybe-flatten 3 8) (trns #xff))
                 (16 (maybe-flatten 3 16) (trns #xffff))
                 ((1 2 4)
                  (copy/2d/sub image-data)
                  (trns #xff)
                  (maybe-flip data)))
               (ecase bit-depth
                 (8 (maybe-flatten 2 8))
                 (16 (maybe-flatten 2 16))
                 ((1 2 4) (copy/2d/sub image-data) (maybe-flip data)))))
          (:indexed-colour
           (ecase bit-depth
             (8 (copy/pal/8 image-data))
             ((1 2 4) (copy/pal/sub image-data)))
           (maybe-flip data)))))
    *png-object*))
