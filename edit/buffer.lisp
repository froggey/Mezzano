(in-package #:edit)

(defclass buffer ()
  ((name :initarg :name :reader buffer-name)
   (first-line :initarg :first-line :accessor buffer-first-line)
   (last-line :initarg :last-line :accessor buffer-last-line)
   (point :initarg :point :accessor buffer-point)
   (keymap :initarg :keymap :accessor buffer-keymap)
   (locals :initarg :locals :reader buffer-locals))
  (:default-initargs :keymap (make-hash-table)
    :locals (make-hash-table)))

(defclass line ()
  ((contents :initarg :contents)
   (next :initarg :next)
   (prev :initarg :prev))
  (:default-initargs
   :contents (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
    :next nil
    :prev nil))

(defvar *buffer* nil "The current buffer.")
(defvar *all-buffers* (make-hash-table :test 'equal))

(defmethod initialize-instance :after ((instance buffer) &key &allow-other-keys)
  (let ((line (make-instance 'line)))
    (check-type (buffer-name instance) string)
    ;; Uniquify the buffer name.
    (when (gethash (buffer-name instance) *all-buffers*)
      (let ((original-name (buffer-name instance))
            (i 2))
        (do ()
            ((not (gethash (buffer-name instance) *all-buffers*)))
          (setf (slot-value instance 'name) (format nil "~A<~D>" original-name i)))))
    (setf (gethash (buffer-name instance) *all-buffers*) instance)
    (setf (slot-value instance 'first-line) line
	  (slot-value instance 'last-line) line
	  (slot-value instance 'point) (cons line 0))))

(defun buffer-local (name)
  (gethash name (buffer-locals *buffer*)))

(defun (setf buffer-local) (value name)
  (setf (gethash name (buffer-locals *buffer*)) value))

(defun current-buffer ()
  *buffer*)

(defun set-buffer (buffer)
  (check-type buffer buffer)
  (setf *buffer* buffer))

(defun get-buffer (buffer-or-name &optional (errorp t))
  (etypecase buffer-or-name
    (buffer buffer-or-name)
    (string (or (gethash buffer-or-name *all-buffers*)
                (when errorp (error 'simple-editor-error
                                    :format-control "No buffer named ~S."
                                    :format-arguments (list buffer-or-name)))))))

(defun get-buffer-create (buffer-or-name)
  (or (get-buffer buffer-or-name nil)
      (make-instance 'buffer :name buffer-or-name)))

(defun point-line ()
  (car (buffer-point *buffer*)))
(defun (setf point-line) (value)
  (setf (car (buffer-point *buffer*)) value))
(defun point-character ()
  (cdr (buffer-point *buffer*)))
(defun (setf point-character) (value)
  (setf (cdr (buffer-point *buffer*)) value))

(defun show-buffer ()
  (do ((line (buffer-first-line *buffer*) (slot-value line 'next)))
      ((null line))
    (show-line line)))

(defun show-line (line)
  (let ((contents (slot-value line 'contents)))
    (cond ((eql (point-line) line)
	   (dotimes (i (length contents))
	     (when (eql (point-character) i)
	       (write-char #\|))
	     (write-char (char contents i)))
	   (when (eql (point-character) (length contents))
	     (write-char #\|))
	   (terpri))
	  (t (write contents :escape nil :pretty t)
	     (terpri)))))

(defun forward-char (&optional (n 1))
  (cond ((minusp n)
	 (backward-char (- n)))
	((zerop n))
	(t (do ((contents (slot-value (point-line) 'contents)
			  (slot-value (point-line) 'contents)))
	       ((<= n (- (length contents) (point-character)))
		(incf (point-character) n))
	     (let ((next (slot-value (point-line) 'next)))
	       (decf n (1+ (- (length contents) (point-character))))
	       (when (null next)
		 (setf (point-character) (length contents))
		  (error 'simple-editor-error
			 :format-control "End of buffer."
			 :format-arguments '()))
		 (setf (point-line) next
		       (point-character) 0))))))

(defun backward-char (&optional (n 1))
  (cond ((minusp n)
	 (forward-char (- n)))
	((zerop n))
	(t (do ()
	       ((<= n (point-character))
		(decf (point-character) n))
	     (let ((prev (slot-value (point-line) 'prev)))
	       (decf n (1+ (point-character)))
	       (when (null prev)
		 (setf (point-character) 0)
		 (error 'simple-editor-error
			:format-control "Beginning of buffer."
			:format-arguments '()))
	       (setf (point-line) prev
		     (point-character) (length (slot-value prev 'contents))))))))

(defun forward-line (&optional (n 1))
  (cond ((plusp n)
	 (dotimes (i n)
           (let ((next (slot-value (point-line) 'next)))
             (when (null next)
               (error 'simple-editor-error
                      :format-control "End of buffer."
                      :format-arguments '()))
             (setf (point-line) next
                   (point-character) (min (length (slot-value next 'contents)) (point-character))))))
	((minusp n)
	 (dotimes (i (- n))
           (let ((prev (slot-value (point-line) 'prev)))
	     (when (null prev)
	       (error 'simple-editor-error
		      :format-control "Beginning of buffer."
		      :format-arguments '()))
	     (setf (point-line) prev
		   (point-character) (min (length (slot-value prev 'contents)) (point-character))))))))

(defun next-line (&optional (n 1))
  (forward-line n))

(defun previous-line (&optional (n 1))
  (forward-line (- n)))

(defun move-beginning-of-line ()
  (setf (point-character) 0))

(defun move-end-of-line ()
  (setf (point-character) (length (slot-value (point-line) 'contents))))

(defun beginning-of-buffer ()
  (setf (point-line) (buffer-first-line *buffer*))
  (move-beginning-of-line))

(defun end-of-buffer ()
  (setf (point-line) (buffer-last-line *buffer*))
  (move-end-of-line))

(defun newline ()
  (insert-char #\Newline))

(defun insert (string)
  (dotimes (i (length string))
    (insert-char (char string i))))

(defun insert-char (char)
  (cond ((eql char #\Newline)
         (let ((contents (slot-value (point-line) 'contents))
               (next (slot-value (point-line) 'next)))
	   (let* ((new-text (subseq contents (point-character)))
		  (new-contents (make-array (length new-text)
					    :element-type 'character
					    :initial-contents new-text
					    :fill-pointer t
					    :adjustable t))
		  (new-line (make-instance 'line
					   :contents new-contents
					   :next next
					   :prev (point-line))))
	     (setf (fill-pointer contents) (point-character))
	     (if next
		 (setf (slot-value next 'prev) new-line)
		 (setf (buffer-last-line *buffer*) new-line))
	     (setf (slot-value (point-line) 'next) new-line)
	     (setf (buffer-point *buffer*) (cons new-line 0)))))
	(t (let ((contents (slot-value (point-line) 'contents)))
	     (cond ((eql (point-character) (length contents))
		    (vector-push-extend char contents))
		   (t ;; Extend the buffer & shuffle characters.
		    (vector-push-extend #\! contents)
		    (dotimes (i (- (length contents) (point-character) 1))
		      (setf (char contents (- (length contents) (1+ i)))
			    (char contents (1- (- (length contents) (1+ i))))))
		    (setf (char contents (point-character)) char)))
	     (incf (point-character))))))

(defun delete-char (&optional (n 1))
  (dotimes (i n)
    (cond ((eql (length (slot-value (point-line) 'contents)) (point-character))
           ;; Deleting end of line, merge the next line.
           (when (null (slot-value (point-line) 'next))
             (error 'simple-editor-error
                    :format-control "End of buffer."
                    :format-arguments '()))
           (let ((new-text (concatenate 'string (slot-value (point-line) 'contents) (slot-value (slot-value (point-line) 'next) 'contents)))
                 (next-next (slot-value (slot-value (point-line) 'next) 'next)))
             (setf (slot-value (point-line) 'contents) (make-array (length new-text)
                                                                   :element-type 'character
                                                                   :initial-contents new-text
                                                                   :fill-pointer t
                                                                   :adjustable t)
                   (slot-value (point-line) 'next) next-next)
             (if next-next
                 (setf (slot-value next-next 'prev) (point-line))
                 (setf (buffer-last-line *buffer*) (point-line)))))
          ((eql (length (slot-value (point-line) 'contents)) (1+ (point-character)))
           ;; Deleting the last character on the line.
           (decf (fill-pointer (slot-value (point-line) 'contents))))
          (t ;; Delete character, shuffle other characters.
           (dotimes (i (- (length (slot-value (point-line) 'contents)) (point-character) 1))
             (setf (char (slot-value (point-line) 'contents) (+ (point-character) i))
                   (char (slot-value (point-line) 'contents) (+ (point-character) i 1))))
           (decf (fill-pointer (slot-value (point-line) 'contents)))))))

(defun backward-delete-char (&optional (n 1))
  (dotimes (i n)
    (backward-char)
    (delete-char)))

(defun clear-buffer ()
  (let ((line (make-instance 'line)))
    (setf (buffer-first-line *buffer*) line
	  (buffer-last-line *buffer*) line
          (point-line) line
          (point-character) 0)))
