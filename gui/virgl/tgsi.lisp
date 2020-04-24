;;;; TGSI shader assembler.

(in-package :mezzano.gui.virgl.tgsi)

(defun check-swizzle (swizzle)
  ;; This checks both swizzles and writemasks.
  (when (not swizzle)
    ;; This is a writemask that writes nothing.
    (return-from check-swizzle))
  (let ((name (symbol-name swizzle)))
    ;; Swizzles are 4 elements long, anything shorter is a writemask.
    (cond ((< (length name) 4)
           ;; Writemasks must have their elements in order.
           ;; :XYZ, not :XZY
           (loop
              with order = '(#\X #\Y #\Z #\W)
              for ch across name
              do
                (loop
                   (when (not order)
                     (error "Bad writemask ~S" swizzle))
                   (when (eql ch (pop order))
                     (return)))))
          (t
           (assert (eql (length name) 4))
           ;; Swizzles consist of XYZW in any order and duplication.
           (loop
              for ch across name
              when (not (member ch '(#\X #\Y #\Z #\W)))
              do (error "Bad swizzle ~S" swizzle))))))

(defun convert-opcode-name (opcode)
  (let ((name (symbol-name opcode)))
    (cond ((find #\- name)
           (substitute #\_ #\- name))
          (t name))))

(defun assemble (processor source)
  (let* ((total-size 2) ; header token + processor token
         (saw-label nil)
         (text (with-output-to-string (text)
                 (write-line (ecase processor
                               (:vertex "VERT")
                               (:fragment "FRAG"))
                             text)
                 (dolist (stmt source)
                   (etypecase stmt
                     ((unsigned-byte 32) ; label
                      ;; Can't have multiple labels one after the other.
                      (when saw-label
                        (error "Unexpected label ~D after label ~D" stmt saw-label))
                      (format text "~D: " stmt)
                      (incf total-size)
                      (setf saw-label stmt))
                     ((cons (eql dcl))
                      ;; Declaration.
                      (when saw-label
                        (error "Unexpected label ~D before declaration ~S" saw-label stmt))
                      (destructuring-bind ((file index &optional (end-index index)) &rest things)
                          (rest stmt)
                        (check-type file (member :in :out :const :temp :samp))
                        (check-type index (unsigned-byte 32))
                        (check-type end-index (unsigned-byte 32))
                        (incf total-size 2) ; Declaration + range (for index)
                        (cond ((eql index end-index)
                               (format text "DCL ~A[~D]" file index))
                              (t
                               (format text "DCL ~A[~D..~D]" file index end-index)))
                        ;; Things are dimensions, semantic, interpolation & array info.
                        ;; TODO: Check this more exactly.
                        (dolist (thing things)
                          (incf total-size) ; Each thing requires an extra token.
                          (check-type thing keyword)
                          (format text ", ~A" thing))
                        (terpri text)))
                     ((cons (eql imm))
                      ;; Immediate.
                      (when saw-label
                        (error "Unexpected label ~D before immediate ~S" saw-label stmt))
                      ;; TODO: Immediates can be numbered too: "IMM[42] FLT32 ..."
                      (destructuring-bind (type (x y z w))
                          ;; TODO: Support uint32, int32, and flt64
                          ;; flt32, uint32, int32 all have 4 elements. flt64 has 2.
                          (rest stmt)
                        (assert (eql type :flt32))
                        (check-type x single-float)
                        (check-type y single-float)
                        (check-type z single-float)
                        (check-type w single-float)
                        (incf total-size 5) ; immediate token + 4 data elements.
                        (format text "IMM FLT32 {~A, ~A, ~A, ~A}~%" x y z w)))
                     (cons
                      ;; An instruction.
                      (setf saw-label nil)
                      (format text "~A" (convert-opcode-name (first stmt)))
                      (incf total-size)
                      (let ((first-operand-p t)
                            (texture nil))
                        (when (eql (first stmt) 'tex)
                          (assert (eql (length stmt) 5))
                          (setf texture (or (fifth stmt) :tex-missing-texture))
                          (setf stmt (butlast stmt)))
                        (dolist (operand (rest stmt))
                          (destructuring-bind (file index &optional (swizzle :xyzw))
                              operand
                            (check-type file (member :imm :in :out :const :temp :samp))
                            (check-type index (unsigned-byte 32))
                            (check-swizzle swizzle)
                            (cond (first-operand-p
                                   (setf first-operand-p nil)
                                   (write-string " " text))
                                  (t
                                   (write-string ", " text)))
                            (format text "~A[~D]" file index)
                            (when (not (eql swizzle :xyzw))
                              (format text ".~A" swizzle))
                            (incf total-size)))
                        (when texture
                          (check-type texture (member :1d :2d :3d))
                          (incf total-size)
                          (format text ", ~A" texture)))
                      (terpri text))))
                 (when saw-label
                   (error "Trailing label ~D at end of source" saw-label)))))
    (values text total-size)))
