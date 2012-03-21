(in-package #:sys.int)

;;; Initialize the ISA PIC.
(setf (io-port/8 #x20) #x11
      (io-port/8 #xA0) #x11
      (io-port/8 #x21) #x30
      (io-port/8 #xA1) #x38
      (io-port/8 #x21) #x04
      (io-port/8 #xA1) #x02
      (io-port/8 #x21) #x01
      (io-port/8 #xA1) #x01
      ;; Mask all IRQs except for the cascade IRQ (2).
      (io-port/8 #x21) #b11111011
      (io-port/8 #xA1) #b11111111)

(defun sys.int::simplify-string (string)
  (if (simple-string-p string)
      string
      (make-array (length string)
                  :element-type (if (every 'sys.int::base-char-p string)
                                    'base-char
                                    'character)
                  :initial-contents string)))

(when (probe-bochs-vbe)
  (setf *framebuffer* (make-array '(600 800)
                                  :element-type '(unsigned-byte 32)
                                  :memory (+ #x8000000000 *bochs-vbe-framebuffer-address*))))
(write-string "Hello, World!")

(setf *package* (find-package "CL-USER"))
(defun repl ()
  (loop
     (with-simple-restart (abort "Return to READ-EVAL-PRINT loop.")
       (fresh-line)
       (write-char #\>)
       (let ((form (read)))
         (fresh-line)
         (let ((result (multiple-value-list (eval form))))
           (if result
               (dolist (v result)
                 (fresh-line)
                 (write v))
               (progn
                 (fresh-line)
                 (write-string "; No values."))))))))

(repl)
