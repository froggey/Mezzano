;;;; Low-level support functions for strings and characters.

(in-package :mezzano.runtime)

(defun wiredp (object)
  (< (sys.int::lisp-object-address object) #x8000000000))

;; Hardcoded string accessor, the support stuff for arrays doesn't function at this point.
(defun char (string index)
  (check-type index integer)
  (cond ((sys.int::character-array-p string)
         (let ((data (sys.int::%complex-array-storage string)))
           (assert (and (<= 0 index)
                        (< index (sys.int::%object-header-data data)))
                   (string index))
           (sys.int::%%assemble-value
            (logior
             (ash (ecase (sys.int::%object-tag data)
                    (#.sys.int::+object-tag-array-unsigned-byte-8+
                     (sys.int::%object-ref-unsigned-byte-8 data index))
                    (#.sys.int::+object-tag-array-unsigned-byte-16+
                     (sys.int::%object-ref-unsigned-byte-16 data index))
                    (#.sys.int::+object-tag-array-unsigned-byte-32+
                     (sys.int::%object-ref-unsigned-byte-32 data index)))
                  (+ (byte-position sys.int::+immediate-tag+)
                     (byte-size sys.int::+immediate-tag+)))
             (dpb sys.int::+immediate-tag-character+
                  sys.int::+immediate-tag+
                  0))
            sys.int::+tag-immediate+)))
        (t
         ;; Possibly a displaced string.
         (check-type string string)
         (aref string index))))

(defun resize-string-storage (old-storage new-tag)
  (let* ((old-tag (sys.int::%object-tag old-storage))
         (elt-size (ecase new-tag
                     ((nil) old-tag)
                     (#.sys.int::+object-tag-array-unsigned-byte-8+
                      1)
                     (#.sys.int::+object-tag-array-unsigned-byte-16+
                      2)
                     (#.sys.int::+object-tag-array-unsigned-byte-32+
                      4)))
         (len (sys.int::%object-header-data old-storage))
         (new-storage (mezzano.runtime::%allocate-object
                       new-tag
                       len
                       (ceiling (* len elt-size) 8)
                       (if (wiredp old-storage) :wired nil))))
    (dotimes (i len)
      (let ((val (ecase old-tag
                   (#.sys.int::+object-tag-array-unsigned-byte-8+
                    (sys.int::%object-ref-unsigned-byte-8 old-storage i))
                   (#.sys.int::+object-tag-array-unsigned-byte-16+
                    (sys.int::%object-ref-unsigned-byte-16 old-storage i))
                   (#.sys.int::+object-tag-array-unsigned-byte-32+
                    (sys.int::%object-ref-unsigned-byte-32 old-storage i)))))
        (ecase new-tag
          (#.sys.int::+object-tag-array-unsigned-byte-8+
           (setf (sys.int::%object-ref-unsigned-byte-8 new-storage i) val))
          (#.sys.int::+object-tag-array-unsigned-byte-16+
           (setf (sys.int::%object-ref-unsigned-byte-16 new-storage i) val))
          (#.sys.int::+object-tag-array-unsigned-byte-32+
           (setf (sys.int::%object-ref-unsigned-byte-32 new-storage i) val)))))
    new-storage))

(defun ensure-string-wide-enough (character string)
  (let* ((int-value (char-int character))
         (min-len (cond ((<= int-value #xFF)
                         sys.int::+object-tag-array-unsigned-byte-8+)
                        ((<= int-value #xFFFF)
                         sys.int::+object-tag-array-unsigned-byte-16+)
                        (t
                         sys.int::+object-tag-array-unsigned-byte-32+)))
         (backing-type (sys.int::%object-tag (sys.int::%complex-array-storage string))))
    (when (< backing-type min-len)
      ;; Promote the storage array to fit the character.
      (setf (sys.int::%complex-array-storage string)
            (resize-string-storage (sys.int::%complex-array-storage string)
                                   min-len)))))

(defun (setf char) (value string index)
  (check-type index integer)
  (check-type value character)
  (cond ((sys.int::character-array-p string)
         (ensure-string-wide-enough value string)
         (let ((int-value (char-int value))
               (data (sys.int::%complex-array-storage string)))
           (assert (and (<= 0 index)
                        (< index (sys.int::%object-header-data data)))
                   (string index))
           (ecase (sys.int::%object-tag data)
             (#.sys.int::+object-tag-array-unsigned-byte-8+
              (setf (sys.int::%object-ref-unsigned-byte-8 data index) int-value))
             (#.sys.int::+object-tag-array-unsigned-byte-16+
              (setf (sys.int::%object-ref-unsigned-byte-16 data index) int-value))
             (#.sys.int::+object-tag-array-unsigned-byte-32+
              (setf (sys.int::%object-ref-unsigned-byte-32 data index) int-value)))
         value))
        (t
         ;; Possibly a displaced string.
         (check-type string string)
         (setf (aref string index) value))))

(defun schar (string index)
  (check-type string string)
  (char string index))

(defun (setf schar) (value string index)
  (check-type string string)
  (setf (char string index) value))

(defun copy-string-in-area (string &optional area)
  (cond ((sys.int::character-array-p string)
         (let* ((data (sys.int::%complex-array-storage string))
                (len (or (sys.int::%complex-array-fill-pointer string)
                         (sys.int::%object-header-data data)))
                (tag (sys.int::%object-tag data))
                (elt-size (ecase tag
                            (#.sys.int::+object-tag-array-unsigned-byte-8+
                             1)
                            (#.sys.int::+object-tag-array-unsigned-byte-16+
                             2)
                            (#.sys.int::+object-tag-array-unsigned-byte-32+
                             4)))
                (new-data (mezzano.runtime::%allocate-object
                           tag
                           len
                           (ceiling (* len elt-size) 8)
                           area))
                (new-header (mezzano.runtime::%allocate-object
                             sys.int::+object-tag-simple-string+
                             1
                             (+ 3 1)
                             area)))
           (setf (sys.int::%complex-array-storage new-header) new-data
                 (sys.int::%complex-array-fill-pointer new-header) nil
                 (sys.int::%complex-array-info new-header) nil
                 (sys.int::%complex-array-dimension new-header 0) len)
           (dotimes (i len)
             (let ((val (ecase tag
                          (#.sys.int::+object-tag-array-unsigned-byte-8+
                           (sys.int::%object-ref-unsigned-byte-8 data i))
                          (#.sys.int::+object-tag-array-unsigned-byte-16+
                           (sys.int::%object-ref-unsigned-byte-16 data i))
                          (#.sys.int::+object-tag-array-unsigned-byte-32+
                           (sys.int::%object-ref-unsigned-byte-32 data i)))))
               (ecase tag
                 (#.sys.int::+object-tag-array-unsigned-byte-8+
                  (setf (sys.int::%object-ref-unsigned-byte-8 new-data i) val))
                 (#.sys.int::+object-tag-array-unsigned-byte-16+
                  (setf (sys.int::%object-ref-unsigned-byte-16 new-data i) val))
                 (#.sys.int::+object-tag-array-unsigned-byte-32+
                  (setf (sys.int::%object-ref-unsigned-byte-32 new-data i) val)))))
           new-header))
        (t
         (error "TODO: copy non-character-array strings"))))

(defun make-wired-string (len &key fullwidth)
  (let* ((tag (if fullwidth
                  #.sys.int::+object-tag-array-unsigned-byte-32+
                  #.sys.int::+object-tag-array-unsigned-byte-8+))
         (elt-size (if fullwidth
                       4
                       1))
         (data (mezzano.runtime::%allocate-object
                tag
                len
                (ceiling (* len elt-size) 8)
                :wired))
         (header (mezzano.runtime::%allocate-object
                  sys.int::+object-tag-simple-string+
                  1
                  (+ 3 1)
                  :wired)))
    (setf (sys.int::%complex-array-storage header) data
          (sys.int::%complex-array-fill-pointer header) nil
          (sys.int::%complex-array-info header) nil
          (sys.int::%complex-array-dimension header 0) len)
    header))

(declaim (inline sys.int::%%make-character))
(defun sys.int::%%make-character (code &optional bits)
  (sys.int::%%assemble-value
   (if bits
       (logior (dpb code sys.int::+char-code+ 0)
               (dpb bits sys.int::+char-bits+ 0))
       (dpb code sys.int::+char-code+ 0))
   (dpb sys.int::+immediate-tag-character+
        sys.int::+immediate-tag+
        sys.int::+tag-immediate+)))

(defun sys.int::%make-character (code &optional bits)
  (check-type code (integer 0 #x0010FFFF)
              "a unicode code-point")
  (check-type bits (or null (integer 0 15)))
  (if (or (<= #xD800 code #xDFFF) ; UTF-16 surrogates.
          ;; Noncharacters.
          (<= #xFDD0 code #xFDEF)
          ;; The final two code points in each plane are noncharacters.
          (eql (logand code #xFFFE) #xFFFE))
      nil
      (sys.int::%%make-character code bits)))

(defun char-code (character)
  (check-type character character)
  (ldb sys.int::+char-code+ (sys.int::lisp-object-address character)))

(defun char-int (character)
  (check-type character character)
  ;; Strip tag & immediate tag.
  (ash (sys.int::lisp-object-address character)
       (- (+ (byte-position sys.int::+immediate-tag+)
             (byte-size sys.int::+immediate-tag+)))))

(defun code-char (code)
  (sys.int::%make-character code))
