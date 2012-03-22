(in-package #:sys.int)

(defun integerp (object)
  (system:fixnump object))

(defun realp (object)
  (integerp object))

(defun numberp (object)
  (realp object))

(defun expt (base power)
  (let ((accum 1))
    (dotimes (i power accum)
      (setf accum (* accum base)))))

(defstruct (byte (:constructor byte (size position)))
  (size 0 :type (integer 0) :read-only-p t)
  (position 0 :type (integer 0) :read-only-p t))

(defun ldb (bytespec integer)
  (logand (ash integer (- (byte-position bytespec)))
          (1- (ash 1 (byte-size bytespec)))))

(defun dpb (newbyte bytespec integer)
  (let ((mask (1- (ash 1 (byte-size bytespec)))))
    (logior (ash (logand newbyte mask) (byte-position bytespec))
            (logand integer (lognot (ash mask (byte-position bytespec)))))))

(define-compiler-macro ldb (&whole whole bytespec integer)
  (cond ((and (listp bytespec)
              (= (length bytespec) 3)
              (eql (first bytespec) 'byte)
              (integerp (second bytespec))
              (not (minusp (second bytespec)))
              (integerp (third bytespec))
              (not (minusp (third bytespec))))
         `(logand (ash ,integer ,(- (third bytespec)))
                  ,(1- (ash 1 (second bytespec)))))
        (t whole)))

(define-compiler-macro dpb (&whole whole newbyte bytespec integer)
  (cond ((and (listp bytespec)
              (= (length bytespec) 3)
              (eql (first bytespec) 'byte)
              (integerp (second bytespec))
              (not (minusp (second bytespec)))
              (integerp (third bytespec))
              (not (minusp (third bytespec))))
         ;; Maintain correct order of evaluation for NEWBYTE and INTEGER.
         (let ((mask (1- (ash 1 (second bytespec)))))
           `(logior (ash (logand ,newbyte ,mask) ,(third bytespec))
                    (logand ,integer ,(lognot (ash mask (third bytespec)))))))
        (t whole)))
