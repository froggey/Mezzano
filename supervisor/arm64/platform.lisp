;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defun initialize-early-platform ()
  (initialize-gic #x08000000 #x08010000))

;; TODO: Parse the FDT instead of hard-coding.
(defun initialize-platform ()
  (initialize-platform-time 30)
  ;; This is correct for qemu's cortex-a53 virt machine.
  (dotimes (i 32)
    (virtio-mmio-register (+ #x0A000000 (* i #x200))
                          (+ 48 i))))
