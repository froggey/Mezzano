;;;; FDT (Flattened Device Tree) parser

(in-package :mezzano.supervisor)

(defconstant +fdt-header-magic-offset+                0)
(defconstant +fdt-header-total-size-offset+           4)
(defconstant +fdt-header-structure-offset+            8)
(defconstant +fdt-header-strings-offset+             12)
(defconstant +fdt-header-mem-rsvmap-offset+          16)
(defconstant +fdt-header-version-offset+             20)
(defconstant +fdt-header-last-compat-version-offset+ 24)
(defconstant +fdt-header-boot-cpuid-offset+          28)
(defconstant +fdt-header-strings-size-offset+        32)
(defconstant +fdt-header-structure-size-offset+      36)

(defconstant +fdt-magic+ #xD00DFEED)
(defconstant +fdt-begin-node+ 1)
(defconstant +fdt-end-node+ 2)
(defconstant +fdt-prop+ 3)
(defconstant +fdt-nop+ 4)

(sys.int::defglobal *fdt*)
(sys.int::defglobal *fdt-trace*)

(defun fdt-log (&rest things)
  (declare (dynamic-extent things))
  (when *fdt-trace*
    (debug-print-line-1 things)))

(defun fdt-check-header (fdt-address)
  (let ((magic (memref-ub32/be (+ fdt-address +fdt-header-magic-offset+)))
        (version (memref-ub32/be (+ fdt-address +fdt-header-version-offset+)))
        (compat-version (memref-ub32/be (+ fdt-address +fdt-header-last-compat-version-offset+))))
    (and (eql magic +fdt-magic+)
         (>= version 17)
         (<= compat-version 17))))

(defun initialize-fdt (boot-information-page)
  (setf *fdt-trace* nil)
  (let ((fdt-address (sys.int::memref-unsigned-byte-64
                      (+ boot-information-page +boot-information-fdt-address+))))
    (when (or (zerop fdt-address)
              (not (fdt-check-header (convert-to-pmap-address fdt-address))))
      (setf *fdt* nil)
      (return-from initialize-fdt nil))
    (setf *fdt* (convert-to-pmap-address fdt-address))
    (debug-print-line "Boot via Devicetree. FDT at " fdt-address)))

(defmacro do-fdt-child-nodes ((child-node node &optional result-form) &body body)
  (let ((node-sym (gensym "NODE"))
        (loop-head (gensym "LOOP-HEAD")))
    `(let ((,node-sym (fdt-first-child-node ,node)))
       (block nil
         (tagbody
            ,loop-head
            (when (not ,node-sym)
              (return (let ((,child-node nil)) ,child-node (progn ,result-form))))
            (let ((,child-node ,node-sym))
              ,@body)
            (setf ,node-sym (fdt-next-sibling-node ,node-sym))
            (go ,loop-head))))))

(defun fdt-present-p ()
  "Returns true if the bootloader provided an FDT."
  *fdt*)

(defun fdt-root ()
  "Return the root node of the FDT."
  (when (not *fdt*)
    (panic "No FDT provided"))
  (+ *fdt* (memref-ub32/be (+ *fdt* +fdt-header-structure-offset+))))

(defun fdt-node-contents (node)
  (let ((name-len (fdt-node-name-len node)))
    (+ node 4 (align-up (1+ name-len) 4))))

(defun fdt-node-name-len (node)
  (loop
     for i from 0
     when (zerop (sys.int::memref-unsigned-byte-8 (+ node 4 i)))
     do (return i)))

(defun fdt-first-child-node (node)
  "Return the first child node of NODE, or NIL if there are no children"
  ;; Skip over the start node's name & properties.
  (fdt-log "search for first child node of " (- node *fdt*))
  (let ((offset (fdt-node-contents node)))
    (loop
         (case (memref-ub32/be offset)
           (#.+fdt-begin-node+
            ;; Reached the first child-node
            (fdt-log "found at " (- offset *fdt*))
            (return-from fdt-first-child-node offset))
           (#.+fdt-end-node+
            ;; There were no child-nodes.
            (fdt-log "no child nodes")
            (return-from fdt-first-child-node nil))
           (#.+fdt-prop+
            (let ((len (fdt-property-length offset)))
              (incf offset (+ 12 (align-up len 4)))))
           (#.+fdt-nop+
            (incf offset 4))
           (t
            (panic "Corrupt FDT, got token " (memref-ub32/be offset) " at offset " (- offset *fdt*)))))))

(defun fdt-next-sibling-node (node)
  "Return the next sibling node of NODE, or NIL if there is no sibling"
  (let ((depth 0))
    (fdt-log "look for next sibling for node " (- node *fdt*))
    ;; Walk nodes until FDT_END_NODE that reduces depth to 0.
    (loop
       (case (memref-ub32/be node)
         (#.+fdt-begin-node+
          (incf depth)
          (setf node (fdt-node-contents node)))
         (#.+fdt-end-node+
          (decf depth)
          (incf node 4)
          (when (eql depth 0)
            (return)))
         (#.+fdt-prop+
          (let ((len (fdt-property-length node)))
            (incf node (+ 12 (align-up len 4)))))
         (#.+fdt-nop+
          (incf node 4))
         (t
          (panic "Corrupt FDT, got token " (memref-ub32/be node) " at offset " (- node *fdt*)))))
    ;; Skip any FDT_NOP nodes.
    (loop
       (case (memref-ub32/be node)
         (#.+fdt-begin-node+
          (fdt-log "found at " (- node *fdt*))
          (return node))
         (#.+fdt-end-node+
          ;; No next sibling.
          (fdt-log "no next sibling")
          (return nil))
         (#.+fdt-nop+
          (incf node 4))
         (t
          (panic "Corrupt FDT, got token " (memref-ub32/be node) " at offset " (- node *fdt*)))))))

(defun fdt-get-named-child-node (parent name)
  (do-fdt-child-nodes (node parent nil)
    (when (fdt-string-compare (+ node 4) name)
      (return node))))

(defun fdt-get-named-child-node-internal (parent name name-len)
  (do-fdt-child-nodes (node parent nil)
    (when (fdt-string-compare-memory (+ node 4) name name-len)
      (return node))))

(defun fdt-property-length (prop)
  "Return the length in bytes of the property"
  (memref-ub32/be (+ prop 4)))

(defun fdt-property-data (prop)
  "Return a pointer to the property's data"
  ;; Skip the token, length, and name fields.
  (+ prop 12))

(defun fdt-property-name (prop)
  "Return a pointer to the property's zero-terminated name"
  (+ *fdt*
     (memref-ub32/be (+ *fdt* +fdt-header-strings-offset+))
     (memref-ub32/be (+ prop 8))))

(defun fdt-first-property (node)
  "Return the first property in NODE."
  (let ((offset (fdt-node-contents node)))
    (loop
       (case (memref-ub32/be offset)
         ((#.+fdt-begin-node+ #.+fdt-end-node+)
          ;; Reached end of properties.
          (fdt-log "fdt node " (- node *fdt*) " has no properties")
          (return nil))
         (#.+fdt-prop+
          (fdt-log "fdt node " (- node *fdt*) " first property at " (- offset *fdt*))
          (return offset))
         (#.+fdt-nop+
          (incf offset 4))
         (t
          (panic "Corrupt FDT, got token " (memref-ub32/be node) " at offset " (- node *fdt*)))))))

(defun fdt-next-property (prop)
  "Return the next property, or NIL if prop is the last property."
  (fdt-log "looking for prop " (- prop *fdt*) " next property  " (memref-ub32/be (+ prop 0)) " " (memref-ub32/be (+ prop 4)) " " (memref-ub32/be (+ prop 8)))
  (incf prop (+ 12 (align-up (fdt-property-length prop) 4)))
  (loop
     (fdt-log "examine " (- prop *fdt*) " " (memref-ub32/be prop))
     (case (memref-ub32/be prop)
       ((#.+fdt-begin-node+ #.+fdt-end-node+)
        ;; Reached end of properties.
        (return nil))
       (#.+fdt-prop+
        (return prop))
       (#.+fdt-nop+
        (incf prop 4))
       (t
        (panic "Corrupt FDT, got token " (memref-ub32/be prop) " at offset " (- prop *fdt*))))))

(defun fdt-string-compare (fdt-string lisp-string)
  (and (loop
          for i below (string-length lisp-string)
          for ch = (char lisp-string i)
          when (not (eql (char-code ch) (sys.int::memref-unsigned-byte-8 fdt-string i)))
          do (return nil)
          finally (return t))
       (eql (sys.int::memref-unsigned-byte-8 fdt-string (string-length lisp-string)) 0)))

(defun fdt-string-compare-memory (fdt-string memory-string memory-string-length)
  (and (loop
          for i below memory-string-length
          when (not (eql (sys.int::memref-unsigned-byte-8 memory-string i)
                         (sys.int::memref-unsigned-byte-8 fdt-string i)))
          do (return nil)
          finally (return t))
       (eql (sys.int::memref-unsigned-byte-8 fdt-string memory-string-length) 0)))

(defun fdt-get-property (node name)
  "Return the property named NAME in NODE, or nil if there is no such property"
  (let ((prop (fdt-first-property node)))
    (loop
       (when (not prop)
         (return nil))
       (when (fdt-string-compare (fdt-property-name prop) name)
         (return prop))
       (setf prop (fdt-next-property prop)))))

(defun fdt-string-list-contains (string-list-address string-list-len string)
  (let ((remaining-len string-list-len)
        (str-len (string-length string)))
    (loop
       (when (< remaining-len str-len)
         (return nil))
       (when (fdt-string-compare string-list-address string)
         (return t))
       (loop
          (when (eql (sys.int::memref-unsigned-byte-8 string-list-address) 0)
            (incf string-list-address)
            (decf remaining-len)
            (return))
          (incf string-list-address)
          (decf remaining-len)))))

(defun fdt-compatible-p (node compatible)
  "Test if NODE's compatible property contains COMPATIBLE."
  (let ((prop (fdt-get-property node "compatible")))
    (cond (prop
           (when *fdt-trace*
             (debug-write "Check node ")
             (debug-write node)
             (debug-write " compatible with ")
             (debug-write compatible)
             (debug-write " compat:")
             (dotimes (i (fdt-property-length prop))
               (debug-write-char
                (sys.int::%%make-character
                 (sys.int::memref-unsigned-byte-8 (fdt-property-data prop) i))))
             (debug-print-line))
           (fdt-string-list-contains (fdt-property-data prop) (fdt-property-length prop) compatible))
          (t nil))))

(defun fdt-read-u32 (prop &optional (index 0))
  "Read a u32 property."
  (memref-ub32/be (+ (fdt-property-data prop) (* index 4))))

(defun fdt-read-integer (prop length &optional (index 0))
  (let ((result 0))
    (dotimes (i length)
      (setf result (logior (ash result 32)
                           (fdt-read-u32 prop (+ index i)))))
    result))

(defun fdt-resolve-alias (name name-len)
  (let* ((aliases-node (fdt-get-named-child-node (fdt-root) "aliases"))
         (prop (if aliases-node
                   (fdt-first-property aliases-node)
                   nil)))
    (loop
       (when (not prop)
         (return nil))
       (when (fdt-string-compare-memory (fdt-property-name prop) name name-len)
         (debug-print-line "Resolved alias " name " to " prop)
         (return prop))
       (setf prop (fdt-next-property prop)))))

(defun fdt-resolve-prop-path (prop)
  "Prop contains a path, possibly terminated by ':'. Return the node associated with it."
  (let ((path-len (loop
                     for i below (fdt-property-length prop)
                     when (eql (sys.int::memref-unsigned-byte-8 (fdt-property-data prop) i) (char-code #\:))
                     do (return i)
                     finally (return (1- (fdt-property-length prop))))))
    (when (not (eql (sys.int::memref-unsigned-byte-8 (fdt-property-data prop) 0) (char-code #\/)))
      ;; This is an alias. Search the aliases node for a matching property.
      (setf prop (fdt-resolve-alias (fdt-property-data prop) path-len))
      (when (not prop)
        (debug-print-line "failed to resolved path property alias")
        (return-from fdt-resolve-prop-path nil))
      (setf path-len (fdt-property-length prop)))
    (let ((current (fdt-root))
          (path-element-start 1)) ; skip leading '/'
      (debug-write "resolving path: ")
      (dotimes (i path-len)
        (debug-write-char
         (sys.int::%%make-character
          (sys.int::memref-unsigned-byte-8 (fdt-property-data prop) i))))
      (debug-print-line)
      (loop
         (let ((name-len (loop
                            for len from 0
                            for elt = (sys.int::memref-unsigned-byte-8 (+ (fdt-property-data prop) path-element-start) len)
                            when (or (eql elt (char-code #\/))
                                     (zerop elt))
                            do (return len))))
           (debug-write "resolving element: ")
           (dotimes (i name-len)
             (debug-write-char
              (sys.int::%%make-character
               (sys.int::memref-unsigned-byte-8 (+ (fdt-property-data prop) path-element-start) i))))
           (debug-print-line)
           (setf current (fdt-get-named-child-node-internal current
                                                            (+ (fdt-property-data prop) path-element-start)
                                                            name-len))
           (when (not current)
             (debug-print-line "failed to resolved path property")
             (return nil))
           (incf path-element-start name-len)
           (incf path-element-start)
           (when (>= path-element-start (1- (fdt-property-length prop)))
             (debug-print-line "Resolved path property " prop " to node " current)
             (return current)))))))

(defun fdt-address-cells (node)
  "Read the #address-cells property for node, returning the default if not present."
  (let ((prop (fdt-get-property node "#address-cells")))
    (if prop
        (fdt-read-u32 prop)
        2)))

(defun fdt-size-cells (node)
  "Read the #size-cells property for node, returning the default if not present."
  (let ((prop (fdt-get-property node "#size-cells")))
    (if prop
        (fdt-read-u32 prop)
        1)))
