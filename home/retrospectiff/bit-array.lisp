
(in-package :retrospectiff.bit-array)

;;; we need a way to represent variable bit-length "characters" for
;;; LZW compression.  we can either use an array of (unsigned-byte 8)
;;; and do the mapping between bits ourselves, using, presumably, ldb
;;; and dpb, or we can use an array of bits. Let's play around with
;;; both ideas and see which one looks like it's going to be cleaner.

;;; The TIFF spec says that LZW data is stored with the
;;; most-significant bits first, also known as FillOrder == 1:
;;;
;;; "Pixel 0 of a row is stored in the high-order bit of byte 0, pixel
;;; 1 is stored in the next-highest bit, ..., pixel 7 is stored in the
;;; low-order bit of byte 0, pixel 8 is stored in the high-order bit
;;; of byte 1, and so on."
;;;
;;;  _7_ _6_ _5_ _4_ _3_ _2_ _1_ _0_
;;; |_x_|_x_|_x_|_x_|_x_|_x_|_x_|_x_| byte 0
;;; |_x_|_x_|___|___|___|___|___|___| byte 1
;;; |___|___|___|___|___|___|___|___| byte 2
;;; ...

(defun rem8 (x)
  (- x (ash (ash x -3) 3)))


;; we want to loop over the appropriate bytes in array and set
;; with the appropriate values.
;;
;; this is rather tricky to calculate the offsets as we need to
;; figure out the bit indices both for array and value.
;; 
;; the key values are:
;;
;; 1. the size (in bytes) to be copied at each step (the same for
;; the src and the dest).
;;
;; 2. the starting bit in the particular '(unsigned-byte 8) being
;; copied into.
;;
;; 3. the starting bit in the src value that we are copying from.
(defun set-bits (array bit-start bit-end value)
  (let ((bit-length (- bit-end bit-start))
        (byte-start (ash bit-start -3))
        (byte-end (ash (1- bit-end) -3)))
    (loop for i from byte-start to byte-end
       for k from (- byte-end byte-start) downto 0
       with src-pos = 0
       do (let (size
                dest-pos)
            (cond ((= i byte-start)
                   (setf size (min bit-length (1+ (rem8 (1- (- 8 bit-start)))))
                         src-pos (- bit-length size)
                         dest-pos (max 0 (- 8 bit-end))))
                  ((= i byte-end)
                   (setf size (1+ (rem8 (1- bit-end)))
                         dest-pos (- 8 size))
                   (decf src-pos size))
                  (t
                   (setf size 8
                         dest-pos 0)
                   (decf src-pos 8)))
            (setf (aref array i)
                  (dpb (ldb (byte size src-pos) value)
                       (byte size dest-pos)
                       (aref array i)))))))

(defun get-bits (array bit-start bit-end)
  (let ((bit-length (- bit-end bit-start))
        (byte-start (ash bit-start -3))
        (byte-end (ash (1- bit-end) -3))
        (dest 0))
    (loop for i from byte-start to byte-end
       for k from (- byte-end byte-start) downto 0
       with int-pos = 0
       do (let (size
                array-byte-pos)
            (cond ((= i byte-start)
                   (setf size (min bit-length (1+ (rem8 (1- (- 8 bit-start)))))
                         int-pos (- bit-length size)
                         array-byte-pos (max 0 (- 8 bit-end))))
                  ((= i byte-end)
                   (setf size (1+ (rem8 (1- bit-end)))
                         array-byte-pos (- 8 size))
                   (decf int-pos size))
                  (t
                   (setf size 8
                         array-byte-pos 0)
                   (decf int-pos 8)))
            (setf dest
                  (dpb (ldb 
                        (byte size array-byte-pos)
                        (aref array i))
                       (byte size int-pos)
                       dest))))
    dest))
