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
;;; Interface functions for creating, initializing, and closing a
;;; FONT-LOADER object.
;;;
;;; $Id: font-loader-interface.lisp,v 1.6 2006/03/23 22:20:35 xach Exp $

(in-package #:zpb-ttf)

(defun arrange-finalization (object stream)
  (flet ((quietly-close (&optional object)
           (declare (ignore object))
           (ignore-errors (close stream))))
    #+sbcl
    (sb-ext:finalize object #'quietly-close)
    #+cmucl
    (ext:finalize object #'quietly-close)
    #+clisp
    (ext:finalize object #'quietly-close)
    #+allegro
    (excl:schedule-finalization object #'quietly-close)))
    

;;;
;;; FIXME: move most/all of this stuff into initialize-instance
;;;

(defun open-font-loader-from-stream (input-stream)
  (let ((magic (read-uint32 input-stream)))
    (when (/= magic #x00010000 #x74727565)
      (error 'bad-magic
             :location "font header"
             :expected-values (list #x00010000 #x74727565)
             :actual-value magic))
    (let* ((table-count (read-uint16 input-stream))
           (font-loader (make-instance 'font-loader
                                       :input-stream input-stream
                                       :table-count table-count)))
      ;; skip the unused stuff:
      ;; searchRange, entrySelector, rangeShift
      (read-uint16 input-stream)
      (read-uint16 input-stream)
      (read-uint16 input-stream)
      (loop repeat table-count
            for tag = (read-uint32 input-stream)
            for checksum = (read-uint32 input-stream)
            for offset = (read-uint32 input-stream)
            for size = (read-uint32 input-stream)
            do (setf (gethash tag (tables font-loader))
                     (make-instance 'table-info
                                    :offset offset
                                    :name (number->tag tag)
                                    :size size)))
      (load-maxp-info font-loader)
      (load-head-info font-loader)
      (load-kern-info font-loader)
      (load-loca-info font-loader)
      (load-name-info font-loader)
      (load-cmap-info font-loader)
      (load-post-info font-loader)
      (load-hhea-info font-loader)
      (load-hmtx-info font-loader)
      (setf (glyph-cache font-loader)
            (make-array (glyph-count font-loader) :initial-element nil))
      font-loader)))

(defun open-font-loader-from-file (thing)
  (let ((stream (open thing
                      :direction :input
                      :element-type '(unsigned-byte 8))))
    (let ((font-loader (open-font-loader-from-stream stream)))
      (arrange-finalization font-loader stream)
      font-loader)))

(defun open-font-loader (thing)
  (typecase thing
    (font-loader
     (unless (open-stream-p (input-stream thing))
       (setf (input-stream thing) (open (input-stream thing))))
     thing)
    (stream
     (if (open-stream-p thing)
         (open-font-loader-from-stream thing)
         (error "~A is not an open stream" thing)))
    (t
     (open-font-loader-from-file thing))))

(defun close-font-loader (loader)
  (close (input-stream loader)))

(defmacro with-font-loader ((loader file) &body body)
  `(let (,loader)
    (unwind-protect
         (progn
           (setf ,loader (open-font-loader ,file))
           ,@body)
      (when ,loader
        (close-font-loader ,loader)))))

