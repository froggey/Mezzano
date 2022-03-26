;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(defpackage #:skippy-system
  (:use :cl #:asdf))

(in-package #:skippy-system)

(defsystem #:skippy
  :version "1.3.12"
  :author "Zachary Beane <xach@xach.com>"
  :description "Read and write GIF files"
  :license "BSD"
  :components ((:file "package")
               (:file "conditions"
                      :depends-on ("package"))
               (:file "types"
                      :depends-on ("package"))
               (:file "bitstream"
                      :depends-on ("package"
                                   "types"))
               (:file "lzw"
                      :depends-on ("package"
                                   "conditions"
                                   "bitstream"))
               (:file "color-table"
                      :depends-on ("package"
                                   "conditions"))
               (:file "canvas"
                      :depends-on ("package"
                                   "color-table"))
               (:file "data-stream"
                      :depends-on ("package"))
               (:file "image"
                      :depends-on ("data-stream"
                                   "color-table"
                                   "canvas"))
               (:file "gif89a"
                      :depends-on ("package"
                                   "conditions"
                                   "types"
                                   "lzw"
                                   "data-stream"
                                   "image"))
               (:file "load-gif"
                      :depends-on ("package"
                                   "conditions"
                                   "types"
                                   "lzw"
                                   "data-stream"
                                   "image"
                                   "gif89a"))))
                      

