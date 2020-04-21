;;;; TGSI shader assembler.

(in-package :mezzano.gui.virgl.tgsi)

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
                      (destructuring-bind ((file index) &rest things)
                          (rest stmt)
                        (check-type file (member :in :out))
                        (check-type index (unsigned-byte 8))
                        (incf total-size 2) ; Declaration + range (for index)
                        (format text "DCL ~A[~D]" file index)
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
                      (format text "~A" (first stmt))
                      (incf total-size)
                      (let ((first-operand-p t))
                        (dolist (operand (rest stmt))
                          (destructuring-bind (file index)
                              operand
                            (check-type file (member :imm :in :out))
                            (check-type index (unsigned-byte 32))
                            (cond (first-operand-p
                                   (setf first-operand-p nil)
                                   (write-string " " text))
                                  (t
                                   (write-string ", " text)))
                            (format text "~A[~D]" file index)
                            (incf total-size))))
                      (terpri text))))
                 (when saw-label
                   (error "Trailing label ~D at end of source" saw-label)))))
    (values text total-size)))
