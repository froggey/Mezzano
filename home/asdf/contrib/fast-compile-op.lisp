;; fast-compile-op: compile just one file in the current process,
;; loading all dependencies as source.
;; How to use it:
;; (1) your build system compiles all your system's dependencies as bundle.
;; (2) For each component in the system, invoke a process that
;;   (a) loads all the transitive dependencies as bundles
;;   (b) use register-preloaded-system
;;       to tell ASDF the direct dependencies are loaded
;;   (c) use fast-compile-op on each of the components in the system
;; (3) when they are all done, combine the fasls in a bundle.

#+sbcl (setf sb-ext:*evaluator-mode* :interpret)

(defclass fast-compile-op (basic-compile-op selfward-operation)
  ((selfward-operation :initform 'prepare-source-op)))

(defmethod action-description ((o fast-compile-op) (c component))
  (declare (ignorable o))
  (format nil (compatfmt "~@<fast compiling ~3i~_~A~@:>") c))

(defmethod perform ((o fast-compile-op) (c component))
  (let (#+sbcl (sb-ext:*evaluator-mode* :compile))
    (perform (make-operation 'compile-op) c)))

(defmethod output-files ((o fast-compile-op) (c component))
  (output-files (make-operation 'compile-op) c))

(defmethod input-files ((o fast-compile-op) (c component))
  (input-files (make-operation 'compile-op) c))

