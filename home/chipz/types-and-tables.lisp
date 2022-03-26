(in-package :chipz)

(deftype index () '(mod #.array-dimension-limit))

(deftype simple-octet-vector (&optional length)
  (let ((length (or length '*)))
    `(simple-array (unsigned-byte 8) (,length))))

(deftype deflate-code-length () '(integer 0 #.+max-code-length+))
(deftype deflate-code () '(unsigned-byte #.+max-code-length+))
(deftype deflate-code-value () '(integer 0 (#.+max-codes+)))

(defparameter *distance-code-extra-bits*
  ;; codes 30 and 31 will never actually appear, but we represent them
  ;; for completeness' sake
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 0 0))
(defparameter *distance-code-base-distances*
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
      1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))

(declaim (inline n-length-extra-bits n-distance-extra-bits length-base distance-base))
(defun n-length-extra-bits (value)
  (aref +length-code-extra-bits+ value))

(defun n-distance-extra-bits (distance-code)
  (svref *distance-code-extra-bits* distance-code))

(defun length-base (value)
  (aref +length-code-base-lengths+ value))

(defun distance-base (distance-code)
  (svref *distance-code-base-distances* distance-code))

(defparameter *code-length-code-order*
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defstruct (code-range-descriptor
             (:conc-name code-)
             (:constructor make-crd (n-bits start-value end-value)))
  (n-bits 0 :type deflate-code-length)
  (start-value 0 :type deflate-code-value)
  (end-value 0 :type deflate-code-value))

(defstruct (huffman-decode-table
             (:conc-name hdt-)
             (:constructor make-hdt (counts offsets symbols bits)))
  ;; FIXME: look into combining these two into one array for speed.
  (counts (error "required parameter")
          :type (simple-array (unsigned-byte 16) (#.+max-code-length+))
          :read-only t)
  (offsets (error "required parameter") :type (simple-array (unsigned-byte 16) (#.(1+ +max-code-length+)))
           :read-only t)
  (symbols nil :read-only t :type (simple-array fixnum (*)))
  (bits nil :read-only t))
) ; EVAL-WHEN


;;; decode table construction

(defun construct-huffman-decode-table (code-lengths &optional n-syms)
  (let* ((n-syms (or n-syms (length code-lengths)))
         (min-code-length +max-code-length+)
         (max-code-length 0)
         (counts (make-array +max-code-length+ :initial-element 0
                            :element-type '(unsigned-byte 16)))
         (offsets (make-array (1+ +max-code-length+) :initial-element 0
                             :element-type '(unsigned-byte 16)))
         (symbols (make-array n-syms :initial-element 0 :element-type 'fixnum)))
    (declare (type (simple-array (unsigned-byte 16) (*)) counts)
             (type (simple-array fixnum (*)) symbols))
    (dotimes (i n-syms)
      (let ((c (aref code-lengths i)))
        (setf min-code-length (min min-code-length c))
        (setf max-code-length (max max-code-length c))
        (incf (aref counts c))))
    ;; generate offsets
    (loop for i from 1 below +deflate-max-bits+
          do (setf (aref offsets (1+ i)) (+ (aref offsets i) (aref counts i))))
    (dotimes (i n-syms (make-hdt counts offsets symbols max-code-length))
      (let ((l (aref code-lengths i)))
        (unless (zerop l)
          (setf (aref symbols (aref offsets l)) i)
          (incf (aref offsets l)))))))


;;; decoders for fixed compression blocks

(defparameter *fixed-block-code-lengths*
  (map 'list #'make-crd
       '(8   9   7   8)                 ; lengths
       '(0   144 256 280)               ; start values
       '(143 255 279 287)))             ; end values

(defparameter *fixed-block-distance-lengths*
  (list (make-crd 5 0 29)))

(defun code-n-values (c)
  (1+ (- (code-end-value c) (code-start-value c))))

(defun compute-huffman-decode-structure (code-descriptors)
  (let* ((n-syms (loop for cd in code-descriptors
                       sum (code-n-values cd)))
         (code-lengths (make-array n-syms :element-type '(unsigned-byte 16))))
    (dolist (cd code-descriptors)
      (fill code-lengths (code-n-bits cd)
            :start (code-start-value cd) :end (1+ (code-end-value cd))))
    (construct-huffman-decode-table code-lengths)))

(defparameter *fixed-literal/length-table*
  (compute-huffman-decode-structure *fixed-block-code-lengths*))
(defparameter *fixed-distance-table*
  (compute-huffman-decode-structure *fixed-block-distance-lengths*))

(defmacro probably-the-fixnum (form)
  #+sbcl
  `(sb-ext:truly-the fixnum ,form)
  #-sbcl
  form)

;;; I want to make this work, but it drastically slows the code down in
;;; sbcl.  Part of this is due to bad code generation (jump to jump to
;;; jump, yuck).
#+nil
(defun decode-value (table state)
  (declare (type huffman-decode-table table))
  (declare (type inflate-state state))
  (declare (optimize (speed 3)))
  (do ((bits (inflate-state-bits state))
       (n-bits (inflate-state-n-bits state))
       (counts (hdt-counts table))
       (len 1)
       (first 0)
       (code 0))
      (nil nil)
    (declare (type (unsigned-byte 32) bits))
    (declare (type (integer 0 32) n-bits))
    (declare (type (and fixnum (integer 0 *)) first code))
    (do ()
        ((zerop n-bits)
         (when (= (inflate-state-input-index state)
                  (inflate-state-input-end state))
           (throw 'inflate-done nil))
         (setf bits (aref (inflate-state-input state)
                          (inflate-state-input-index state)))
         (setf (inflate-state-input-index state)
               (sb-ext:truly-the fixnum (1+ (inflate-state-input-index state))))
         (setf n-bits 8))
      ;; We would normally do this with READ-BITS, but DECODE-VALUE
      ;; is a hotspot in profiles along with this would-be call to
      ;; READ-BITS, so we inline it all here.
      (setf code (logior code (logand bits 1))
            bits (ash bits -1))
      (decf n-bits)
      (let ((count (aref counts len)))
        (when (< (- code count) first)
          (setf (inflate-state-bits state) bits)
          (setf (inflate-state-n-bits state) n-bits)
          (return-from decode-value (aref (hdt-symbols table)
                                          #+sbcl
                                          (sb-ext:truly-the fixnum
                                                            (+ (aref (hdt-offsets table) (1- len))
                                                                  (- code first)))
                                          #-sbcl (+ (aref (hdt-offsets table) (1- len))
                                                                  (- code first)))))
        (setf first
              #+sbcl (sb-ext:truly-the fixnum (+ first count))
              #-sbcl (+ first count)
              first
              #+sbcl (sb-ext:truly-the fixnum (ash first 1))
              #-sbcl (ash first 1)
              code
              #+sbcl (sb-ext:truly-the fixnum (ash code 1))
              #-sbcl (ash code 1)
              len (1+ len))))))
