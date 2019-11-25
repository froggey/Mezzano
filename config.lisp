;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.internals)

;; This is the local IP address of the system running the file-server.
(defparameter *file-server-host-ip* '(192 168 0 123))

;; The full path to the Mezzano source tree.
;; This must be a string and it must end with '/'.
(defparameter *mezzano-source-path* "/Full/path/to/Mezzano/")

;; The full path to the Mezzano home directory, containing the libraries.
;; This must be a string and it must end with '/'.
(defparameter *home-directory-path* "/Full/path/to/home/directory/")
