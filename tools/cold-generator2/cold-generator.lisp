;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.cold-generator
  (:use :cl)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:load #:mezzano.cold-generator.load)
                    (#:eval #:mezzano.cold-generator.eval)
                    (#:ser #:mezzano.cold-generator.serialize)
                    (#:write #:mezzano.cold-generator.write)))

(in-package :mezzano.cold-generator)

(defun build-directory (environment)
  (merge-pathnames (make-pathname :directory `(:relative ,(format nil "~(build-~A~)" (env:environment-target environment))))))

(defun create-thread (environment name &key stack-size (initial-state :runnable))
  (setf name (env:make-array environment (length name)
                             :element-type 'character
                             :initial-contents name
                             :area :wired))
  (let* ((stack (env:make-stack environment stack-size))
         (thread (env:make-structure
                  environment 'mezzano.supervisor:thread
                  :name name
                  :state initial-state
                  :stack stack)))
    (setf (env:structure-slot-value environment thread 'mezzano.supervisor::self) thread)
    thread))

(defun load-compiled-file (environment filespec &key eval wired)
  (let ((forms (load:load-compiled-file environment filespec :wired wired)))
    (cond (eval
           (eval:eval-toplevel-list environment forms))
          (t
           forms))))

;; Ugh.
(defun load-compiler-builtins (environment)
  (let ((llf-path (merge-pathnames "%%compiler-builtins.llf"
                                   (build-directory environment))))
    (ensure-directories-exist llf-path)
    (sys.c::save-compiler-builtins llf-path (env:environment-target environment))
    (load-compiled-file environment llf-path :eval t :wired t)))

(defun load-source-file (environment file &key eval wired)
  (load-compiled-file
   environment
   (let ((sys.c::*target-architecture* (env:environment-target environment)))
     (cold-generator::maybe-compile-file file))
   :eval eval
   :wired wired))

(defun filter-files-by-architecture (files architecture)
  (loop
     for filespec in files
     when (or (not (consp filespec))
              (member architecture (rest filespec)))
     collect (if (consp filespec)
                 (first filespec)
                 filespec)))

(defun load-source-files (environment files &key eval wired)
  (loop
     for file in (filter-files-by-architecture files (env:environment-target environment))
     append (load-source-file environment
                              file
                              :eval eval
                              :wired wired)))

(defun save-cold-files (environment)
  (setf (env:cross-symbol-value environment 'sys.int::*cold-toplevel-forms*)
        (concatenate
         'vector
         (load-compiler-builtins environment)
         (load-source-files environment cold-generator::*supervisor-source-files* :eval t :wired t)
         (load-source-files environment cold-generator::*source-files* :eval t))))

(defun save-package-system (environment)
  (setf (env:cross-symbol-value environment 'sys.int::*package-system*)
        (coerce
         (load-source-file environment "system/packages.lisp")
         'vector)))

(defun save-warm-files (environment)
  ;; Bake the compiled files directly into the image.
  (loop
     with result = (env:make-array environment 0 :adjustable t :fill-pointer 0 :area :pinned)
     for file in (filter-files-by-architecture cold-generator::*warm-source-files* (env:environment-target environment))
     for compiled-file = (let ((sys.c::*target-architecture* (env:environment-target environment))
                               ;; HACK! Force use of the new compiler building the SIMD/float functions.
                               (sys.c::*use-new-compiler* (if (member file '("runtime/simd.lisp"
                                                                             "runtime/float-x86-64.lisp"))
                                                              t
                                                              sys.c::*use-new-compiler*)))
                           (cold-generator::maybe-compile-file file))
     do
       (format t ";; Warm loading ~A.~%" compiled-file)
       (with-open-file (stream compiled-file :element-type '(unsigned-byte 8))
         (let ((vec (env:make-array environment (file-length stream) :element-type '(unsigned-byte 8) :area :pinned)))
           (read-sequence vec stream)
           (vector-push-extend (env:cons-in-area (pathname-name compiled-file) vec environment :pinned) result)))
     finally
       (setf (env:cross-symbol-value environment 'sys.int::*warm-llf-files*) result)))

(defun save-debug-8x8-font (environment)
  (format t ";; Saving 8x8 debug font.~%")
  (let* ((font-data (with-open-file (s cold-generator::*8x8-debug-font*)
                      (read s)))
         (font-array (env:make-array environment 128 :initial-element nil :area :wired)))
    (assert (eql (array-dimension font-data 0) 128))
    (dotimes (i 128)
      (let ((array (env:make-array environment (* 8 8) :element-type '(unsigned-byte 32) :area :wired)))
        (setf (aref font-array i) array)
        (dotimes (y 8)
          (let ((line (aref font-data i y)))
            (dotimes (x 8)
              (setf (aref array (+ (* y 8) x)) (if (logbitp x line)
                                                   #xFF000000
                                                   #xFFFFFFFF)))))))
    (setf (env:cross-symbol-value environment 'sys.int::*debug-8x8-font*) font-array)))

(defun save-unifont-data (environment)
  (format t ";; Saving Unifont.~%")
  (multiple-value-bind (tree data)
      (with-open-file (s cold-generator::*unifont*)
        (build-unicode:generate-unifont-table s))
    (env:set-object-graph-area environment tree :pinned)
    (env:set-object-graph-area environment data :pinned)
    (setf (env:cross-symbol-value environment 'sys.int::*unifont-bmp*) tree)
    (setf (env:cross-symbol-value environment 'sys.int::*unifont-bmp-data*) data)))

(defun save-unicode (environment)
  (format t ";; Saving Unicode data.~%")
  (multiple-value-bind (planes name-store encoding-table name-trie)
      (build-unicode:generate-unicode-data-tables (build-unicode:read-unicode-data cold-generator::*unicode-data*))
    (env:set-object-graph-area environment planes :pinned)
    (env:set-object-graph-area environment name-store :pinned)
    (env:set-object-graph-area environment encoding-table :pinned)
    (env:set-object-graph-area environment name-trie :pinned)
    (setf (env:cross-symbol-value environment 'sys.int::*unicode-info*) planes)
    (setf (env:cross-symbol-value environment 'sys.int::*unicode-name-store*) name-store)
    (setf (env:cross-symbol-value environment 'sys.int::*unicode-encoding-table*) encoding-table)
    (setf (env:cross-symbol-value environment 'sys.int::*unicode-name-trie*) name-trie)))

(defun save-pci-ids (environment)
  (format t ";; Saving PCI IDs.~%")
  (let ((pci-ids (build-pci-ids:build-pci-ids cold-generator::*pci-ids*)))
    (env:set-object-graph-area environment pci-ids :wired)
    (setf (env:cross-symbol-value environment 'sys.int::*pci-ids*) pci-ids)))

(defun save-git-rev (environment)
  (let ((git-rev (cold-generator::git-revision)))
    (setf (env:cross-symbol-value environment 'sys.int::*git-revision*) git-rev)))

(defgeneric configure-system-for-target (environment target))

(defun save-tables (environment)
  (let ((fref-table (env:make-array environment 0 :adjustable t :fill-pointer 0)))
    (env:do-all-environment-frefs (fref environment)
      (when (not (symbolp (env:function-reference-name fref)))
        (vector-push-extend fref fref-table)))
    (setf (env:cross-symbol-value environment 'sys.int::*initial-fref-obarray*) fref-table))
  (let ((struct-table (env:make-array environment 0 :adjustable t :fill-pointer 0)))
    (env:do-all-environment-structs (sdef environment)
      (vector-push-extend sdef struct-table))
    (setf (env:cross-symbol-value environment 'sys.int::*initial-structure-obarray*) struct-table))
  ;; Do this last, no symbols can be added after it.
  (let ((symbol-table (env:make-array environment 0 :adjustable t :fill-pointer 0)))
    ;; Prod symbol to make sure it gets included.
    (setf (env:cross-symbol-value environment 'sys.int::*initial-obarray*) nil)
    (env:do-all-environment-symbols (symbol environment)
      (let ((package (env:cross-symbol-package environment symbol)))
        (check-type package keyword)
        (vector-push-extend symbol symbol-table)))
    (setf (env:cross-symbol-value environment 'sys.int::*initial-obarray*) symbol-table)))

(defun configure-system (environment)
  (save-cold-files environment)
  (save-package-system environment)
  (setf (env:cross-symbol-value environment 'sys.int::*additional-cold-toplevel-forms*) #())
  (save-warm-files environment)
  (save-debug-8x8-font environment)
  (save-unifont-data environment)
  (save-unicode environment)
  (save-pci-ids environment)
  (save-git-rev environment)
  (setf (env:cross-symbol-value environment 'sys.int::*supervisor-log-buffer*)
                    (env:make-array environment (* 1024 1024)
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0
                                    :area :wired))
  (setf (env:cross-symbol-value environment 'sys.int::*exception-stack*)
        (env:make-stack environment (* 128 1024)))
  (setf (env:cross-symbol-value environment 'sys.int::*irq-stack*)
        (env:make-stack environment (* 128 1024)))
  (setf (env:cross-symbol-value environment 'sys.int::*bsp-wired-stack*)
        (env:make-stack environment (* 128 1024)))
  (setf (env:cross-symbol-value environment 'sys.int::*bsp-info-vector*)
        (env:make-array environment (1- (/ (* 2 #x1000) 8))
                        :element-type '(unsigned-byte 64)
                        :initial-element 0
                        :area :wired))
  ;; Initial thread will have the initial stack field set by serialize.
  (setf (env:cross-symbol-value environment 'sys.int::*initial-thread*)
        (create-thread environment "Initial thread"
                       :stack-size (* 128 1024)
                       :initial-state :active))
  (setf (env:cross-symbol-value environment 'sys.int::*bsp-idle-thread*)
        (create-thread environment "BSP idle thread"
                       :stack-size (* 128 1024)
                       :initial-state :runnable))
  (setf (env:cross-symbol-value environment 'sys.int::*snapshot-thread*)
        (create-thread environment "Snapshot thread"
                       :stack-size (* 1024 1024)
                       :initial-state :sleeping))
  (setf (env:cross-symbol-value environment 'sys.int::*pager-thread*)
        (create-thread environment "Pager thread"
                       :stack-size (* 1024 1024)
                       :initial-state :sleeping))
  (setf (env:cross-symbol-value environment 'sys.int::*disk-io-thread*)
        (create-thread environment "Disk IO thread"
                       :stack-size (* 1024 1024)
                       :initial-state :sleeping))
  (env:add-special environment nil nil)
  (env:add-special environment t t)
  (env:add-special environment :unbound-value (env:make-structure environment 'mezzano.runtime::unbound-value))
  (configure-system-for-target environment (env:environment-target environment))
  (setf (env:cross-symbol-value environment
                            'sys.int::*structure-type-type*)
        (env:find-structure-definition
         environment
         (env:translate-symbol environment 'sys.int::structure-definition)))
  (setf (env:cross-symbol-value environment
                            'sys.int::*structure-slot-type*)
        (env:find-structure-definition
         environment
         (env:translate-symbol environment 'sys.int::structure-slot-definition)))
  ;; Generate tables containing all packaged symbols, non-symbol frefs, and structure definitions.
  ;; Once this is complete further objects should not be added as these tables will not be
  ;; updated.
  (save-tables environment)
  (values))

(defun make-image (image-name &key header-path image-size map-file (architecture :x86-64) uuid)
  (let ((environment (env:make-standard-environment :target architecture)))
    (configure-system environment)
    (format t ";; Serializing image. This may take a while...")
    (finish-output)
    (let ((image (ser:serialize-image environment)))
      (format t " done.~%")
      (let ((map-file-path (or map-file
                               (merge-pathnames (make-pathname :type "map" :defaults image-name)
                                                (build-directory environment)))))
        (format t ";; Writing map file to ~S.~%" map-file-path)
        (ser:write-map-file map-file-path image))
      (let ((image-header (make-instance 'write:image-header
                                         :uuid (cond ((stringp uuid) (cold-generator::parse-uuid uuid))
                                                     ((not uuid) (cold-generator::generate-uuid))
                                                     (uuid))
                                         :entry-fref (ser:serialize-object
                                                      (env:function-reference
                                                       environment
                                                       (env:translate-symbol environment 'sys.int::bootloader-entry-point))
                                                      image environment)
                                         :initial-thread (ser:serialize-object
                                                          (env:cross-symbol-value environment 'sys.int::*initial-thread*)
                                                          image environment)
                                         :nil (ser:serialize-object nil image environment)
                                         :architecture architecture)))
        (format t ";; Writing image with UUID ~/cold-generator::format-uuid/.~%"
                (write:image-header-uuid image-header))
        (format t ";; Nil at ~X~%" (write:image-header-nil image-header))
        (format t ";; Entry-Fref at ~X~%" (write:image-header-entry-fref image-header))
        (format t ";; Initial-Thread at ~X~%" (write:image-header-initial-thread image-header))
        (cond ((streamp image-name)
               (write:write-image image image-name image-header
                                 :disk-header-path header-path
                                 :image-size image-size)
               (truename image-name))
              (t
               (with-open-file (stream (merge-pathnames (make-pathname :type "image" :defaults image-name)
                                                        (build-directory environment))
                                       :direction :output
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede)
                 (write:write-image image stream image-header
                                   :disk-header-path header-path
                                   :image-size image-size)
                 (truename stream))))))))
