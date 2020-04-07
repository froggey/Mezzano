;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.disk
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:ext :mezzano.extensions))
  (:export
   ;; APIs
   :block-device-sector-size
   :block-device-n-sectors
   :block-device-read
   :block-device-write
   :block-device-flush
   :block-device-read-sector
   :block-device-write-sector
   :parse-partition-table
   :all-block-devices
   :register-block-device
   :unregister-block-device
   :probe-block-device
   :find-local-block-device

   ;; disk mixin
   :disk-mixin
   :disk-writable-p
   :disk-n-sectors
   :disk-sector-size

   ;; disk partition table mixin
   :disk-pt-mixin
   :pt-type
   :disk-id
   :pt-first-lba
   :pt-last-lba

   ;; disk partition mixin
   :disk-partition-mixin
   :dp-disk
   :dp-partition-num
   :dp-partition-type
   :dp-start-lba
   :dp-size))
