(in-package #:sys.int)

(defstruct ps/2-fifo
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (buffer (make-array 500 :element-type '(unsigned-byte 8))
          :type (simple-array (unsigned-byte 8) (*))))

(defconstant +ps/2-data-port+ #x60)
(defconstant +ps/2-control-port+ #x61)
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
  (loop (unwind-protect
             (progn
               (%cli)
               (when (not (eql (ps/2-fifo-head fifo)
                               (ps/2-fifo-tail fifo)))
                 (return (prog1 (aref (ps/2-fifo-buffer fifo) (ps/2-fifo-head fifo))
                           (incf (ps/2-fifo-head fifo))
                           (when (>= (ps/2-fifo-head fifo) (length (ps/2-fifo-buffer fifo)))
                             (setf (ps/2-fifo-head fifo) 0))))))
          (%sti))
     (%hlt)))

(defclass ps/2-keyboard-stream (stream-object) ())

(defvar *ps/2-keyboard-shifted* nil)
(defun ps/2-read-char (fifo)
  (loop
     (let* ((scancode (ps/2-read-fifo fifo))
            (key (svref (if *ps/2-keyboard-shifted*
                           *gb-keymap-high*
                           *gb-keymap-low*)
                       (logand scancode #x7F))))
       (cond ((= (logand scancode #x80) 0)
              ;; Key press.
              (cond ((eql key :shift)
                     (setf *ps/2-keyboard-shifted* t))
                    ((characterp key)
                     (return key))
                    ((null key)
                     (write-string "Unknown keycode #x")
                     (sys.int::write-integer scancode 16)
                     (write-char #\/)
                     (sys.int::write-integer scancode))))
             (t ;; Key release.
              (case key
                (:shift (setf *ps/2-keyboard-shifted* nil))))))))

(defmethod stream-read-char ((stream ps/2-keyboard-stream))
  (ps/2-read-char *ps/2-key-fifo*))

(defmethod stream-listen ((stream ps/2-keyboard-stream))
  (not (eql (ps/2-fifo-head *ps/2-key-fifo*)
            (ps/2-fifo-tail *ps/2-key-fifo*))))

(defun init-ps/2 ()
  (setf *ps/2-key-fifo* (make-ps/2-fifo)
        (isa-pic-interrupt-handler +ps/2-key-irq+) (sys.intc:make-interrupt-handler 'ps/2-interrupt *ps/2-key-fifo*)
        (isa-pic-irq-mask +ps/2-key-irq+) nil
        *ps/2-aux-fifo* (make-ps/2-fifo)
        (isa-pic-interrupt-handler +ps/2-aux-irq+) (sys.intc:make-interrupt-handler 'ps/2-interrupt *ps/2-aux-fifo*)
        (isa-pic-irq-mask +ps/2-aux-irq+) nil
        *ps/2-keyboard-shifted* nil))

(add-hook '*initialize-hook* 'init-ps/2)
