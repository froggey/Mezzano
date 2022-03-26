(in-package :cl-user)

(defpackage #:pngload.test
  (:use #:cl
        #:pngload)
  (:export #:test-images
           #:test-read-times))

(in-package :pngload.test)

(defvar *failed* nil)
(defparameter *ref* :png-read)
(defparameter *break-on-failure* nil)

(defun get-path ()
  (uiop:ensure-directory-pathname
   (asdf:system-relative-pathname :pngload "test")))

(defun get-image-name (file)
  (namestring
   (make-pathname
    :defaults
    (pathname-name file)
    :type (pathname-type file))))

(defun load-ref-image/png-read (file)
  (let* ((png (png-read:read-png-file file))
         (tr (png-read:transparency png)))
    (when tr
      (let* ((old (png-read:image-data png))
             (dims (array-dimensions old))
             (w (first dims))
             (h (second dims))
             (c (third dims))
             (new (make-array (list w h (1+ (or c 1)))
                              :element-type (if (< (png-read:bit-depth png) 8)
                                                '(unsigned-byte 8)
                                                (array-element-type old)))))
        (flet ((old (x y i)
                 (cond
                   ((and c (< i c))
                    (aref old x y i))
                   ((and (not c) (= i 0))
                    (aref old x y)))))
          (loop :for x :below (first dims)
                :do (loop :for y :below (second dims)
                          :do (loop :for i :below (1+ (or c 1))
                                    :do (setf (aref new x y i)
                                              (or (old x y i)
                                                  (aref tr x y)))))))
        (setf (png-read:image-data png) new)))
    (opticl:transpose-image (png-read:image-data png))))

(defun load-ref-image (file)
  (ecase *ref*
    (:opticl (opticl:read-image-file file))
    (:png-read (load-ref-image/png-read file))))

(defun test-images* (&key flip flatten)
  (let ((*failed*)
        (files (uiop:directory-files (get-path))))
    (flet ((test-image (file)
             (multiple-value-bind (image error)
                 (load-file file :flip-y flip :flatten flatten)
               (let ((ref (load-ref-image file)))
                 (when flip
                   (setf ref (opticl:vertical-flip-image ref)))
                 (when flatten
                   (setf ref (make-array (array-total-size ref)
                                         :element-type (array-element-type ref)
                                         :displaced-to ref)))
                 (unless (and image (equalp (data image) ref))
                   (when *break-on-failure*
                     (break "~s failed~@[: ~s~] ~s" (get-image-name file)
                            error
                            (and image (pngload:color-type image))
                            image ref))
                   (push (get-image-name file) *failed*))))))
      (map nil #'test-image files)
      (format t "~&~%Testing against ~s" *ref*)
      (when (or flip flatten)
        (format t " (options: flip ~s, flatten ~s)~%" flip flatten))
      (format t "~&Passed (~d)" (- (length files) (length *failed*)))
      (format t "~&Failed (~d)~@[: ~s~]" (length *failed*) *failed*)
      (list (- (length files) (length *failed*))
            (length *failed*)))))

(defun test-images (&key (ref :png-read))
  (let* ((*ref* ref)
         (r (list (test-images*)
                  (test-images* :flip t)
                  (test-images* :flatten t)
                  (test-images* :flip t :flatten t))))
    (format t "~%Total: Passed: ~s / Failed: ~s~%"
            (reduce '+ (mapcar #'first r))
            (reduce '+ (mapcar #'second r)))))

(defun test-read-time (library-name func file count)
  (let ((start (local-time:now)))
    (dotimes (i count)
      (funcall func file))
    (format t "~A: ~,3fs~%"
            library-name
            (local-time:timestamp-difference (local-time:now) start))))

(defun test-read-times (file &key (count 1))
  (test-read-time "pngload" #'load-file file count)
  (test-read-time "png-read" #'png-read:read-png-file file count)
  (test-read-time "opticl" #'opticl:read-image-file file count))
