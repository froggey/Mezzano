;;;; The cold image generator.
;;;;
;;;; Cross-compiles source files and produces a cold image.

(defpackage :mezzano.cold-generator
  (:use :cl)
  (:nicknames #:cold-generator) ; For backwards compatibility
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:load #:mezzano.cold-generator.load)
                    (#:eval #:mezzano.cold-generator.eval)
                    (#:ser #:mezzano.cold-generator.serialize)
                    (#:write #:mezzano.cold-generator.write)
                    (#:util #:mezzano.cold-generator.util)
                    (#:clos #:mezzano.cold-generator.clos)
                    (#:uuid #:mezzano.uuid)
                    (#:sys.int #:mezzano.internals))
  (:export #:make-image
           #:set-up-cross-compiler))

(in-package :mezzano.cold-generator)

(defparameter *supervisor-source-files*
  '("supervisor/entry.lisp"
    ("supervisor/x86-64/cpu.lisp" :x86-64)
    ;;("supervisor/arm64/cpu.lisp" :arm64) fixme
    "supervisor/interrupts.lisp"
    ("supervisor/x86-64/interrupts.lisp" :x86-64)
    ;;("supervisor/arm64/interrupts.lisp" :arm64)
    ;;("supervisor/arm64/gic.lisp" :arm64)
    "supervisor/debug.lisp"
    "supervisor/serial.lisp"
    ;;("supervisor/uart.lisp" :arm64)
    "supervisor/disk.lisp"
    "supervisor/partition.lisp"
    "supervisor/thread.lisp"
    ("supervisor/x86-64/thread.lisp" :x86-64)
    ;;("supervisor/arm64/thread.lisp" :arm64)
    "supervisor/sync.lisp"
    "supervisor/physical.lisp"
    "supervisor/snapshot.lisp"
    ("supervisor/x86-64/snapshot.lisp" :x86-64)
    ;;("supervisor/arm64/snapshot.lisp" :arm64)
    "supervisor/store.lisp"
    "supervisor/pager.lisp"
    ("supervisor/x86-64/pager.lisp" :x86-64)
    ;;("supervisor/arm64/pager.lisp" :arm64)
    "supervisor/time.lisp"
    ("supervisor/x86-64/time.lisp" :x86-64)
    ;;("supervisor/arm64/time.lisp" :arm64)
    "supervisor/ps2.lisp"
    "supervisor/video.lisp"
    "supervisor/pci.lisp"
    "supervisor/cdrom.lisp"
    "supervisor/ata.lisp"
    "supervisor/ahci.lisp"
    "supervisor/virtio.lisp"
    "supervisor/virtio-pci.lisp"
    "supervisor/virtio-mmio.lisp"
    "supervisor/virtio-block.lisp"
    "supervisor/virtio-input.lisp"
    "supervisor/virtio-gpu.lisp"
    "supervisor/virtualbox.lisp"
    "supervisor/profiler.lisp"
    "supervisor/support.lisp"
    "supervisor/acpi.lisp"
    "supervisor/efi.lisp"
    ("supervisor/x86-64/platform.lisp" :x86-64)
    ;;("supervisor/arm64/platform.lisp" :arm64)
    "runtime/runtime.lisp"
    ("runtime/runtime-x86-64.lisp" :x86-64)
    ;;("runtime/runtime-arm64.lisp" :arm64)
    "system/data-types.lisp"
    "runtime/allocate.lisp"
    "runtime/cons.lisp"
    "runtime/numbers.lisp"
    ("runtime/float-x86-64.lisp" :x86-64)
    ;;("runtime/float-arm64.lisp" :arm64)
    "runtime/string.lisp"
    "runtime/array.lisp"
    "runtime/struct.lisp"
    "runtime/symbol.lisp"
    "runtime/function.lisp"
    "runtime/instance.lisp"
    "supervisor/fdt.lisp"
    "supervisor/dma-buffer.lisp"))

(defparameter *source-files*
  '("system/cold-start.lisp"
    "system/defstruct.lisp"
    "system/cons.lisp"
    "system/sequence.lisp"
    "system/runtime-array.lisp"
    "system/array.lisp"
    "system/printer.lisp"
    "system/stuff.lisp"
    "system/runtime-support.lisp"
    "system/type.lisp"
    "system/setf.lisp"
    "system/cas.lisp"
    "system/string.lisp"
    "system/hash-table.lisp"
    "system/numbers/runtime-numbers.lisp"
    "system/numbers/logical.lisp"
    "system/numbers/transcendental.lisp"
    "system/numbers/ratio.lisp"
    "system/numbers/complex.lisp"
    "system/numbers/bignum.lisp"
    "system/numbers/float.lisp"
    ("system/numbers/bignum-x86-64.lisp" :x86-64)
    ;;("system/numbers/bignum-arm64.lisp" :arm64)
    "system/numbers/numbers.lisp"
    "system/gc.lisp"
    "system/weak-objects.lisp"
    "system/room.lisp"
    "system/reader.lisp"
    "system/character.lisp"
    "system/backquote.lisp"
    "system/format.lisp"
    "system/defmacro.lisp"
    "system/basic-macros.lisp"
    "system/parse.lisp"
    "system/load.lisp"
    "system/time.lisp"
    "system/delimited-continuations.lisp"
    ("system/delimited-continuations-x86-64.lisp" :x86-64)
    "system/clos/boot.lisp"
))

(defparameter *warm-source-files*
  '("system/clos/macros.lisp"
    "system/clos/fast-class-hash-table.lisp"
    "system/clos/single-dispatch-emf-table.lisp"
    "system/clos/multiple-dispatch-emf-table.lisp"
    "system/restarts.lisp"
    "system/clos/closette.lisp"
    "system/clos/struct.lisp"
    "system/clos/method-combination.lisp"
    "system/weak-objects-printers.lisp"
    "system/sync.lisp"
    "system/thread-pool.lisp"
    "system/describe.lisp"
    "system/runtime-misc.lisp"
    "system/condition.lisp"
    "system/error.lisp"
    "system/coerce.lisp"
    "system/debug.lisp"
    "system/dispatch.lisp"
    "system/full-eval.lisp"
    "system/fast-eval.lisp"
    "system/eval.lisp"
    "system/gray-streams.lisp"
    "system/external-format.lisp"
    "system/standard-streams.lisp"
    "system/stream.lisp"
    "system/ansi-loop.lisp"
    "compiler/package.lisp"
    "system/environment.lisp"
    "compiler/compiler.lisp"
    "compiler/lap.lisp"
    "compiler/lap-x86.lisp"
    "compiler/lap-arm64.lisp"
    "compiler/environment.lisp"
    "compiler/global-environment.lisp"
    "compiler/ast.lisp"
    "compiler/ast-generator.lisp"
    "compiler/pass1.lisp"
    "compiler/constprop.lisp"
    "compiler/simplify.lisp"
    "compiler/lift.lisp"
    "compiler/inline.lisp"
    "compiler/kill-temps.lisp"
    "compiler/keyword-arguments.lisp"
    "compiler/simplify-arguments.lisp"
    "compiler/dynamic-extent.lisp"
    "compiler/lower-environment.lisp"
    "compiler/lower-special-bindings.lisp"
    "compiler/value-aware-lowering.lisp"
    "compiler/simplify-control-flow.lisp"
    "compiler/blexit.lisp"
    "compiler/transforms.lisp"
    "compiler/type-check.lisp"
    "compiler/backend/backend.lisp"
    "compiler/backend/instructions.lisp"
    "compiler/backend/cfg.lisp"
    "compiler/backend/analysis.lisp"
    "compiler/backend/dominance.lisp"
    "compiler/backend/convert-ast.lisp"
    "compiler/backend/multiple-values.lisp"
    "compiler/backend/ssa.lisp"
    "compiler/backend/passes.lisp"
    "compiler/backend/debug.lisp"
    "compiler/backend/register-allocation.lisp"
    "compiler/backend/canon.lisp"
    "compiler/backend/x86-64/x86-64.lisp"
    "compiler/backend/x86-64/target.lisp"
    "compiler/backend/x86-64/codegen.lisp"
    "compiler/backend/x86-64/builtin.lisp"
    "compiler/backend/x86-64/misc.lisp"
    "compiler/backend/x86-64/object.lisp"
    "compiler/backend/x86-64/cons.lisp"
    "compiler/backend/x86-64/memory.lisp"
    "compiler/backend/x86-64/number.lisp"
    "compiler/backend/x86-64/simd.lisp"
    "compiler/backend/arm64/arm64.lisp"
    "compiler/backend/arm64/target.lisp"
    "compiler/backend/arm64/codegen.lisp"
    "compiler/backend/arm64/builtin.lisp"
    "compiler/backend/arm64/misc.lisp"
    "compiler/backend/arm64/object.lisp"
    "compiler/backend/arm64/number.lisp"
    ("runtime/simd.lisp" :x86-64)
    "system/numbers/nibbles.lisp"
    "system/file-compiler.lisp"
    "system/clos/constructor.lisp"
    "system/xp-package.lisp"
    "system/xp.lisp"
    "system/xp-format.lisp"
    "system/xp-printers.lisp"
    "system/profiler.lisp"
    "drivers/network-card.lisp"
    "drivers/virtio-net.lisp"
    ("drivers/rtl8168.lisp" :x86-64)
    "net/package.lisp"
    "net/network.lisp"
    "net/ethernet.lisp"
    "net/arp.lisp"
    "net/ip.lisp"
    "net/udp.lisp"
    "net/tcp.lisp"
    "net/dns.lisp"
    "net/dhcp.lisp"
    "net/network-setup.lisp"
    "system/uuid.lisp"
    "file/fs.lisp"
    "file/remote.lisp"
    ;; Non-supervisor disk code
    "disk/crc32.lisp"
    "disk/guid.lisp"
    "disk/disk.lisp"
    "disk/partition.lisp"
    ;; FAT file system
    "file/cache.lisp"
    "file/fat32.lisp"
    "config.lisp"
    "ipl.lisp"))

(defparameter *8x8-debug-font* "tools/font8x8.lisp-expr")
(defparameter *unifont* "tools/unifont-5.1.20080820.hex")
(defparameter *unicode-data* "tools/UnicodeData.txt")
(defparameter *pci-ids* "tools/pci.ids")

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
                  :%name name
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

(defun maybe-compile-file (path environment &key force package)
  (let ((llf-path (merge-pathnames (make-pathname :type "llf" :defaults path)
                                   (build-directory environment))))
    (ensure-directories-exist llf-path)
    (with-open-file (s llf-path
                       :element-type '(unsigned-byte 8)
                       :if-does-not-exist nil)
      (when s
        (handler-case (load:validate-llf-header s (env:environment-target environment))
          (load:invalid-llf (c)
            (format t "Rebuilding ~A: ~A~%" llf-path c)
            (delete-file s)))))
    (when (or force
              (not (probe-file llf-path))
              (<= (file-write-date llf-path) (file-write-date path)))
      (format t "~A is out of date will be recompiled.~%" llf-path)
      (let ((mezzano.compiler::*target-architecture* (env:environment-target environment))
            (cross-cl:*features* (list* (env:environment-target environment) cross-cl:*features*)))
        (mezzano.compiler::cross-compile-file path :output-file llf-path :package package)))
    llf-path))

;; Ugh.
(defun load-compiler-builtins (environment)
  (let ((llf-path (merge-pathnames "%%compiler-builtins.llf"
                                   (build-directory environment))))
    (ensure-directories-exist llf-path)
    (mezzano.compiler::save-compiler-builtins llf-path (env:environment-target environment))
    (load-compiled-file environment llf-path :eval t :wired t)))

(defun load-source-file (environment file &key eval wired force-recompile package)
  (load-compiled-file
   environment
   (maybe-compile-file file environment :force force-recompile :package package)
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
         (load-source-files environment *supervisor-source-files* :eval t :wired t)
         (load-source-files environment *source-files* :eval t))))

(defun save-package-system (environment)
  (setf (env:cross-symbol-value environment 'sys.int::*package-system*)
        (coerce
         (load-source-file environment "system/packages.lisp")
         'vector)))

(defun save-warm-files (environment)
  ;; Bake the compiled files directly into the image.
  (loop
     with result = (env:make-array environment 0 :adjustable t :fill-pointer 0 :area :pinned)
     for file in (filter-files-by-architecture *warm-source-files* (env:environment-target environment))
     for compiled-file = (maybe-compile-file file environment)
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
  (let* ((font-data (with-open-file (s *8x8-debug-font*)
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
      (with-open-file (s *unifont*)
        (mezzano.cold-generator.build-unicode:generate-unifont-table s))
    (env:set-object-graph-area environment tree :pinned)
    (env:set-object-graph-area environment data :pinned)
    (setf (env:cross-symbol-value environment 'sys.int::*unifont-bmp*) tree)
    (setf (env:cross-symbol-value environment 'sys.int::*unifont-bmp-data*) data)))

(defun save-unicode (environment)
  (format t ";; Saving Unicode data.~%")
  (multiple-value-bind (planes name-store encoding-table name-trie)
      (mezzano.cold-generator.build-unicode:generate-unicode-data-tables
       (mezzano.cold-generator.build-unicode:read-unicode-data *unicode-data*))
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
  (let ((pci-ids (mezzano.cold-generator.build-pci-ids:build-pci-ids *pci-ids*)))
    (env:set-object-graph-area environment pci-ids :wired)
    (setf (env:cross-symbol-value environment 'sys.int::*pci-ids*) pci-ids)))

(defun save-git-rev (environment)
  (let ((git-rev (util:git-revision)))
    (setf (env:cross-symbol-value environment 'sys.int::*git-revision*) git-rev)))

(defgeneric configure-system-for-target (environment target))

(defun save-tables (environment)
  (let ((fref-table (env:make-array environment 0 :adjustable t :fill-pointer 0)))
    (env:do-all-environment-frefs (fref environment)
      (when (not (symbolp (env:function-reference-name fref)))
        (vector-push-extend fref fref-table)))
    (setf (env:cross-symbol-value environment 'sys.int::*initial-fref-obarray*) fref-table))
  (let ((doclist '()))
    (env:do-all-environment-frefs (fref environment)
      (when (env:function-reference-documentation fref)
        (push (list (env:function-reference-name fref) (env:function-reference-documentation fref)) doclist)))
    (setf (env:cross-symbol-value environment 'sys.int::*initial-function-docstrings*) doclist))
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
  (setf (env:cross-symbol-value environment 'sys.int::*page-fault-stack*)
        (env:make-stack environment (* 128 1024)))
  (setf (env:cross-symbol-value environment 'sys.int::*bsp-wired-stack*)
        (env:make-stack environment (* 128 1024)))
  (setf (env:cross-symbol-value environment 'mezzano.supervisor::*bsp-cpu*)
        (env:make-structure environment 'mezzano.supervisor::cpu))
  (env:add-special environment
                   :symbol-binding-cache-sentinel
                   (env:symbol-global-value-cell
                    environment
                    (env:make-symbol environment "SYMBOL-BINDING-CACHE-SENTINEL")))
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
  (env:add-special environment :unbound-value (env:make-structure environment 'mezzano.runtime::unbound-value :tag :unbound-symbol))
  (env:add-special environment :layout-instance-header
                   (env:make-instance-header
                    environment
                    (env:find-structure-definition
                     environment
                     (env:translate-symbol environment 'sys.int::layout))))
  (configure-system-for-target environment (env:environment-target environment))
  (clos:configure-clos environment #'load-source-file)
  (values))

(defun finalize-system (environment)
  ;; Generate tables containing all packaged symbols, non-symbol frefs,
  ;; and structure definitions. Once this is complete further objects
  ;; should not be added as these tables will not be updated.
  (save-tables environment)
  (values))

(defun make-image (image-name &key header-path image-size map-file (architecture :x86-64) uuid)
  (let ((environment (env:make-standard-environment :target architecture)))
    (configure-system environment)
    (finalize-system environment)
    (format t ";; Serializing image. This may take a while...")
    (finish-output)
    (let ((image (ser:serialize-image environment)))
      (format t " done.~%")
      (let ((map-file-path (or map-file
                               (merge-pathnames (make-pathname :type "map" :defaults image-name)
                                                (build-directory environment)))))
        (format t ";; Writing map file to ~S.~%" map-file-path)
        (ser:write-map-file map-file-path image))
      (let ((symtabl-file-path (or map-file
                               (merge-pathnames (make-pathname :type "symbol-table" :defaults image-name)
                                                (build-directory environment)))))
        (format t ";; Writing symbol table to ~S.~%" symtabl-file-path)
        (ser:write-symbol-table symtabl-file-path image))
      (let ((image-header (make-instance 'write:image-header
                                         :uuid (cond ((stringp uuid) (uuid:string->uuid uuid))
                                                     ((not uuid) (uuid:generate-uuid :result-type 'vector))
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
        (format t ";; Writing image with UUID ~/mezzano.uuid:format-uuid/.~%"
                (write:image-header-uuid image-header))
        (format t ";; Nil at ~X~%" (write:image-header-nil image-header))
        (format t ";; Entry-Fref at ~X (~X)~%"
                (write:image-header-entry-fref image-header)
                (+ (write:image-header-entry-fref image-header) 23))
        (format t ";; Initial-Thread at ~X~%" (write:image-header-initial-thread image-header))
        (format t ";; Initial stack pointer is ~X~%" (ser:image-initial-stack-pointer image))
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

(defparameter *cross-source-files*
  '("system/basic-macros.lisp"
    "system/defmacro.lisp"
    "system/backquote.lisp"
    "system/setf.lisp"
    "system/cas.lisp"
    "runtime/instance.lisp"
    "system/defstruct.lisp"
    "system/condition.lisp"
    "system/type.lisp"
    "system/error.lisp"
    "system/restarts.lisp"
    "system/runtime-array.lisp"
    "system/array.lisp"
    "system/sequence.lisp"
    "system/hash-table.lisp"
    "system/packages.lisp"
    "system/weak-objects.lisp"
    "system/gray-streams.lisp"
    "system/stream.lisp"
    "system/reader.lisp"
    "system/printer.lisp"
    "system/numbers/numbers.lisp"
    "system/numbers/logical.lisp"
    "system/character.lisp"
    "system/clos/macros.lisp"
    "system/clos/closette.lisp"
    "system/data-types.lisp"
    "system/gc.lisp"
    "system/cold-start.lisp"
    "system/cons.lisp"
    "system/numbers/runtime-numbers.lisp"
    "system/numbers/ratio.lisp"
    "supervisor/time.lisp"
    "supervisor/thread.lisp"
    "supervisor/interrupts.lisp"
    "supervisor/entry.lisp"
    "supervisor/physical.lisp"
    "supervisor/x86-64/cpu.lisp"
    ;;"supervisor/arm64/cpu.lisp" fixme
    "supervisor/support.lisp"
    "runtime/struct.lisp"
    "runtime/array.lisp"
    "runtime/symbol.lisp"
    "system/stuff.lisp"
    "system/sync.lisp"
    "system/numbers/nibbles.lisp"
)
  "These files are loaded into the compiler environment so other source
files will be compiled correctly.")

(defun set-up-cross-compiler ()
  (with-compilation-unit ()
    (flet ((load-files (file-list)
             (dolist (f file-list)
               (cond ((consp f)
                      (mezzano.compiler::load-for-cross-compiler (first f)))
                     (t
                      (mezzano.compiler::load-for-cross-compiler f))))))
      (load-files *cross-source-files*)
      (load-files *supervisor-source-files*)
      (load-files *source-files*)
      (load-files *warm-source-files*))))
