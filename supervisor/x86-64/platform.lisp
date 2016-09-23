;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defun initialize-early-platform ()
  (initialize-interrupts)
  (initialize-i8259))

(defun initialize-platform ()
  (initialize-acpi)
  (initialize-time)
  (initialize-ps/2)
  (initialize-pci))
