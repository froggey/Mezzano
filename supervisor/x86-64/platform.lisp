;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defun initialize-platform-early-console (boot-information-page)
  (declare (ignore boot-information-page))
  ;; TODO: This (along with the other serial settings) should be provided by the bootloader.
  (let ((serial-port-io-base #x3F8))
    (initialize-debug-serial serial-port-io-base 4 38400)))

(defun initialize-early-platform ()
  (initialize-interrupts)
  (initialize-i8259))

(defun initialize-platform ()
  (initialize-platform-time)
  (initialize-ps/2)
  (initialize-pci)
  (when (not (boot-option +boot-option-no-detect+))
    (pci-detect)))
