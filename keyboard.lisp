(in-package #:sys.int)

(defstruct (ps/2-fifo (:area :static))
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (buffer (make-array 500 :element-type '(unsigned-byte 8) :area :static)
          :type (simple-array (unsigned-byte 8) (*))))

(defconstant +ps/2-data-port+ #x60)
(defconstant +ps/2-control-port+ #x64)
(defconstant +ps/2-key-irq+ 1)
(defconstant +ps/2-aux-irq+ 12)

(sys.intc:define-interrupt-handler ps/2-interrupt (fifo &aux data x)
  (setf data (io-port/8 +ps/2-data-port+))
  (setf x (1+ (ps/2-fifo-tail fifo)))
  (when (>= x (length (ps/2-fifo-buffer fifo)))
    (setf x 0))
  ;; When next reaches head, the buffer is full.
  (unless (= x (ps/2-fifo-head fifo))
    (setf (aref (ps/2-fifo-buffer fifo) (ps/2-fifo-tail fifo)) data
          (ps/2-fifo-tail fifo) x))
  't)

(defvar *ps/2-key-fifo*)
(defvar *ps/2-aux-fifo*)

(defun ps/2-read-fifo (fifo)
  (loop
     (when (not (eql (ps/2-fifo-head fifo)
                     (ps/2-fifo-tail fifo)))
       (return (prog1 (aref (ps/2-fifo-buffer fifo) (ps/2-fifo-head fifo))
                 (incf (ps/2-fifo-head fifo))
                 (when (>= (ps/2-fifo-head fifo) (length (ps/2-fifo-buffer fifo)))
                   (setf (ps/2-fifo-head fifo) 0)))))
     (process-wait "Keyboard input"
                   (lambda ()
                     (not (eql (ps/2-fifo-head fifo) (ps/2-fifo-tail fifo)))))))

(defclass ps/2-keyboard-stream (stream-object) ())

(defconstant +extended-scan-code+ #xE0)

;; (extended-scancode normal-key [shifted-key])
(defvar *extended-key-alist*
  '((#x5B :left-super)
    (#x1D :right-control)
    (#x5C :right-super)
    (#x38 :right-meta)
    (#x5D #\u0010401B) ; menu
    (#x52 #\u00104010) ; insert
    (#x47 #\u00104012) ; home
    (#x49 #\u00104014) ; page up
    (#x53 #\u00104011) ; delete
    (#x4F #\u00104013) ; end
    (#x51 #\u00104015) ; page down
    (#x48 #\u00104018) ; up arrow
    (#x4B #\u00104016) ; left arrow
    (#x50 #\u00104019) ; down arrow
    (#x4D #\u00104017) ; right arrow
    (#x35 #\u001040FB) ; KP divide
    (#x1C #\u001040FF) ; KP enter
    ))

(defvar *ps/2-keyboard-extended-key* nil)
(defvar *ps/2-keyboard-shifted* nil)
(defvar *ps/2-keyboard-ctrled* nil)
(defvar *ps/2-keyboard-metaed* nil)
(defvar *ps/2-keyboard-supered* nil)
(defvar *ps/2-keyboard-hypered* nil)
(defun ps/2-read-char (fifo)
  (loop
     (let* ((scancode (ps/2-read-fifo fifo))
            (key (svref (if *ps/2-keyboard-shifted*
                           *gb-keymap-high*
                           *gb-keymap-low*)
                       (logand scancode #x7F))))
       (when *ps/2-keyboard-extended-key*
         (setf *ps/2-keyboard-extended-key* nil)
         (let ((extended-key (assoc (logand scancode #x7F) *extended-key-alist*)))
           (cond ((null extended-key)
                  (setf key nil))
                 ((and *ps/2-keyboard-shifted* (third extended-key))
                  (setf key (third extended-key)))
                 (t (setf key (second extended-key))))))
       (cond ((= scancode +extended-scan-code+)
              (setf *ps/2-keyboard-extended-key* t))
             ((= (logand scancode #x80) 0)
              ;; Key press.
              (cond ((member key '(:shift :left-shift :right-shift))
                     (setf *ps/2-keyboard-shifted* t))
                    ((member key '(:control :left-control :right-control))
                     (setf *ps/2-keyboard-ctrled* t))
                    ((member key '(:meta :left-meta :right-meta))
                     (setf *ps/2-keyboard-metaed* t))
                    ((member key '(:super :left-super :right-super))
                     (setf *ps/2-keyboard-supered* t))
                    ((member key '(:hyper :left-hyper :right-hyper))
                     (setf *ps/2-keyboard-hypered* t))
                    ((characterp key)
                     (when *ps/2-keyboard-ctrled*
                       (setf (char-bit key :control) t))
                     (when *ps/2-keyboard-metaed*
                       (setf (char-bit key :meta) t))
                     (when *ps/2-keyboard-supered*
                       (setf (char-bit key :super) t))
                     (when *ps/2-keyboard-hypered*
                       (setf (char-bit key :hyper) t))
                     (return key))
                    ((null key)
                     (write-string "Unknown keycode #x")
                     (sys.int::write-integer scancode 16)
                     (write-char #\/)
                     (sys.int::write-integer scancode))))
             (t ;; Key release.
              (case key
                ((:shift :left-shift :right-shift) (setf *ps/2-keyboard-shifted* nil))
                ((:control :left-control :right-control) (setf *ps/2-keyboard-ctrled* nil))
                ((:meta :left-meta :right-meta) (setf *ps/2-keyboard-metaed* nil))
                ((:super :left-super :right-super) (setf *ps/2-keyboard-supered* nil))
                ((:hyper :left-hyper :right-hyper) (setf *ps/2-keyboard-hypered* nil))))))))

(defmethod stream-read-char ((stream ps/2-keyboard-stream))
  (ps/2-read-char *ps/2-key-fifo*))

(defun keyboard-listen (fifo)
  (loop (when (eql (ps/2-fifo-head fifo)
                   (ps/2-fifo-tail fifo))
          (return nil))
     (let* ((scancode (aref (ps/2-fifo-buffer fifo) (ps/2-fifo-head fifo)))
            (key (svref (if *ps/2-keyboard-shifted*
                            *gb-keymap-high*
                            *gb-keymap-low*)
                        (logand scancode #x7F))))
       (when *ps/2-keyboard-extended-key*
         (setf *ps/2-keyboard-extended-key* nil)
         (let ((extended-key (assoc (logand scancode #x7F) *extended-key-alist*)))
           (cond ((null extended-key)
                  (setf key nil))
                 ((and *ps/2-keyboard-shifted* (third extended-key))
                  (setf key (third extended-key)))
                 (t (setf key (second extended-key))))))
       (cond ((= scancode +extended-scan-code+)
              (setf *ps/2-keyboard-extended-key* t))
             ((logtest scancode #x80)
              ;; Key release.
              (case key
                ((:shift :left-shift :right-shift) (setf *ps/2-keyboard-shifted* nil))
                ((:control :left-control :right-control) (setf *ps/2-keyboard-ctrled* nil))
                ((:meta :left-meta :right-meta) (setf *ps/2-keyboard-metaed* nil))
                ((:super :left-super :right-super) (setf *ps/2-keyboard-supered* nil))
                ((:hyper :left-hyper :right-hyper) (setf *ps/2-keyboard-hypered* nil))))
             (t ;; Key press.
              (cond ((member key '(:shift :left-shift :right-shift))
                     (setf *ps/2-keyboard-shifted* t))
                    ((member key '(:control :left-control :right-control))
                     (setf *ps/2-keyboard-ctrled* t))
                    ((member key '(:meta :left-meta :right-meta))
                     (setf *ps/2-keyboard-metaed* t))
                    ((member key '(:super :left-super :right-super))
                     (setf *ps/2-keyboard-supered* t))
                    ((member key '(:hyper :left-hyper :right-hyper))
                     (setf *ps/2-keyboard-hypered* t))
                    ((characterp key)
                     (return t)))))
       (incf (ps/2-fifo-head fifo))
       (when (>= (ps/2-fifo-head fifo) (length (ps/2-fifo-buffer fifo)))
         (setf (ps/2-fifo-head fifo) 0)))))
(defmethod stream-listen ((stream ps/2-keyboard-stream))
  (keyboard-listen *ps/2-key-fifo*))

(defun init-ps/2 ()
  (setf *ps/2-key-fifo* (make-ps/2-fifo)
        (isa-pic-interrupt-handler +ps/2-key-irq+) (sys.intc:make-interrupt-handler 'ps/2-interrupt *ps/2-key-fifo*)
        (isa-pic-irq-mask +ps/2-key-irq+) nil
        *ps/2-aux-fifo* (make-ps/2-fifo)
        (isa-pic-interrupt-handler +ps/2-aux-irq+) (sys.intc:make-interrupt-handler 'ps/2-interrupt *ps/2-aux-fifo*)
        (isa-pic-irq-mask +ps/2-aux-irq+) nil
        *ps/2-keyboard-shifted* nil))

(add-hook '*initialize-hook* 'init-ps/2)
