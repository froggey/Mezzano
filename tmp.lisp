(in-package :sys.int)

#+nil(load "xterm.llf")
#+nil(load "../lispos-home/source/asdf.llf")
#+nil(require :ssh)

(defun set-trace ()
  (setf ssh::*ssh-output-trace* (make-array 1000 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
(defun save-trace (name)
  (with-open-file (s name :direction :output :element-type '(unsigned-byte 8))
    (write-sequence ssh::*ssh-output-trace* s)))

(in-package :ssh)
(setf (gethash (name-char "C-F1") sys.graphics::*global-keybindings*)
      (lambda () (create-ssh-client '(192 168 1 16) :port 1984)))
