;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defun initialize-early-platform ()
  (initialize-gic #x08000000 #x08010000))

(defun initialize-platform ()
  ;; TODO: Parse the FDT instead of hard-coding.
  ;; This is correct for qemu's cortex-a53 virt machine.
  (dotimes (i 32)
    (virtio-mmio-register (+ #x0A000000 (* i #x200))
                          (+ 48 i))))
