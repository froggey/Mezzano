;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-jpn.lisp --- Japanese encodings.
;;;

(in-package #:babel-encodings)

;;;; helper functions
(defvar *eucjp-to-ucs-hash* (make-hash-table))
(defvar *ucs-to-eucjp-hash* (make-hash-table))
(defvar *cp932-to-ucs-hash* (make-hash-table))
(defvar *ucs-to-cp932-hash* (make-hash-table))

(dolist (i `((,*cp932-only*
              ,*cp932-to-ucs-hash*
              ,*ucs-to-cp932-hash*)
             (,*eucjp-only*
              ,*eucjp-to-ucs-hash*
              ,*ucs-to-eucjp-hash*)
             (,*eucjp*
              ,*eucjp-to-ucs-hash*
              ,*ucs-to-eucjp-hash*)))
  (dolist (j (first i))
    (setf (gethash (car j) (second i)) (cadr j))
    (setf (gethash (cadr j) (third i)) (car j))))

(flet ((euc-cp932 (x)
         (let ((high (ash x -16))
               (mid (logand (ash x -8) 255))
               (low (logand x 255)))
           (cond ((not (zerop high))
                  nil)
                 ((= mid #x8e)
                  (logand x 255))
                 ((zerop mid)
                  x)
                 ((decf mid #xa1)
                  (decf low #x80)
                  (incf low (if (zerop (logand mid 1)) #x1f #x7e))
                  (incf low (if (<= #x7f low #x9d) 1 0))
                  (setq mid (ash mid -1))
                  (incf mid (if (<= mid #x1e) #x81 #xc1))
                  (+ (ash mid 8) low))))))
  (dolist (i *eucjp*)
    (let ((cp932 (euc-cp932 (first i))))
      (when cp932
        (setf (gethash cp932 *cp932-to-ucs-hash*) (second i))
        (setf (gethash (second i) *ucs-to-cp932-hash*) cp932)))))

;ascii
(loop for i from #x00 to #x7f do
      (setf (gethash i *cp932-to-ucs-hash*) i)
      (setf (gethash i *eucjp-to-ucs-hash*) i)
      (setf (gethash i *ucs-to-eucjp-hash*) i)
      (setf (gethash i *ucs-to-cp932-hash*) i))

;half-width katakana
(loop for i from #xa1 to #xdf do
      (setf (gethash i *cp932-to-ucs-hash*) (+ #xff61 #x-a1 i))
      (setf (gethash (+ #xff61 #x-a1 i) *ucs-to-cp932-hash*) i)
      (setf (gethash (+ #x8e00 i) *eucjp-to-ucs-hash*) (+ #xff61 #x-a1 i))
      (setf (gethash (+ #xff61 #x-a1 i) *ucs-to-eucjp-hash*) (+ #x8e00 i)))

;; This is quoted from https://support.microsoft.com/en-us/kb/170559/en-us
(let ((kb170559 "0x8790   -> U+2252   -> 0x81e0   Approximately Equal To Or The Image Of
0x8791   -> U+2261   -> 0x81df   Identical To
0x8792   -> U+222b   -> 0x81e7   Integral
0x8795   -> U+221a   -> 0x81e3   Square Root
0x8796   -> U+22a5   -> 0x81db   Up Tack
0x8797   -> U+2220   -> 0x81da   Angle
0x879a   -> U+2235   -> 0x81e6   Because
0x879b   -> U+2229   -> 0x81bf   Intersection
0x879c   -> U+222a   -> 0x81be   Union
0xed40   -> U+7e8a   -> 0xfa5c   CJK Unified Ideograph
0xed41   -> U+891c   -> 0xfa5d   CJK Unified Ideograph
0xed42   -> U+9348   -> 0xfa5e   CJK Unified Ideograph
0xed43   -> U+9288   -> 0xfa5f   CJK Unified Ideograph
0xed44   -> U+84dc   -> 0xfa60   CJK Unified Ideograph
0xed45   -> U+4fc9   -> 0xfa61   CJK Unified Ideograph
0xed46   -> U+70bb   -> 0xfa62   CJK Unified Ideograph
0xed47   -> U+6631   -> 0xfa63   CJK Unified Ideograph
0xed48   -> U+68c8   -> 0xfa64   CJK Unified Ideograph
0xed49   -> U+92f9   -> 0xfa65   CJK Unified Ideograph
0xed4a   -> U+66fb   -> 0xfa66   CJK Unified Ideograph
0xed4b   -> U+5f45   -> 0xfa67   CJK Unified Ideograph
0xed4c   -> U+4e28   -> 0xfa68   CJK Unified Ideograph
0xed4d   -> U+4ee1   -> 0xfa69   CJK Unified Ideograph
0xed4e   -> U+4efc   -> 0xfa6a   CJK Unified Ideograph
0xed4f   -> U+4f00   -> 0xfa6b   CJK Unified Ideograph
0xed50   -> U+4f03   -> 0xfa6c   CJK Unified Ideograph
0xed51   -> U+4f39   -> 0xfa6d   CJK Unified Ideograph
0xed52   -> U+4f56   -> 0xfa6e   CJK Unified Ideograph
0xed53   -> U+4f92   -> 0xfa6f   CJK Unified Ideograph
0xed54   -> U+4f8a   -> 0xfa70   CJK Unified Ideograph
0xed55   -> U+4f9a   -> 0xfa71   CJK Unified Ideograph
0xed56   -> U+4f94   -> 0xfa72   CJK Unified Ideograph
0xed57   -> U+4fcd   -> 0xfa73   CJK Unified Ideograph
0xed58   -> U+5040   -> 0xfa74   CJK Unified Ideograph
0xed59   -> U+5022   -> 0xfa75   CJK Unified Ideograph
0xed5a   -> U+4fff   -> 0xfa76   CJK Unified Ideograph
0xed5b   -> U+501e   -> 0xfa77   CJK Unified Ideograph
0xed5c   -> U+5046   -> 0xfa78   CJK Unified Ideograph
0xed5d   -> U+5070   -> 0xfa79   CJK Unified Ideograph
0xed5e   -> U+5042   -> 0xfa7a   CJK Unified Ideograph
0xed5f   -> U+5094   -> 0xfa7b   CJK Unified Ideograph
0xed60   -> U+50f4   -> 0xfa7c   CJK Unified Ideograph
0xed61   -> U+50d8   -> 0xfa7d   CJK Unified Ideograph
0xed62   -> U+514a   -> 0xfa7e   CJK Unified Ideograph
0xed63   -> U+5164   -> 0xfa80   CJK Unified Ideograph
0xed64   -> U+519d   -> 0xfa81   CJK Unified Ideograph
0xed65   -> U+51be   -> 0xfa82   CJK Unified Ideograph
0xed66   -> U+51ec   -> 0xfa83   CJK Unified Ideograph
0xed67   -> U+5215   -> 0xfa84   CJK Unified Ideograph
0xed68   -> U+529c   -> 0xfa85   CJK Unified Ideograph
0xed69   -> U+52a6   -> 0xfa86   CJK Unified Ideograph
0xed6a   -> U+52c0   -> 0xfa87   CJK Unified Ideograph
0xed6b   -> U+52db   -> 0xfa88   CJK Unified Ideograph
0xed6c   -> U+5300   -> 0xfa89   CJK Unified Ideograph
0xed6d   -> U+5307   -> 0xfa8a   CJK Unified Ideograph
0xed6e   -> U+5324   -> 0xfa8b   CJK Unified Ideograph
0xed6f   -> U+5372   -> 0xfa8c   CJK Unified Ideograph
0xed70   -> U+5393   -> 0xfa8d   CJK Unified Ideograph
0xed71   -> U+53b2   -> 0xfa8e   CJK Unified Ideograph
0xed72   -> U+53dd   -> 0xfa8f   CJK Unified Ideograph
0xed73   -> U+fa0e   -> 0xfa90   CJK compatibility Ideograph
0xed74   -> U+549c   -> 0xfa91   CJK Unified Ideograph
0xed75   -> U+548a   -> 0xfa92   CJK Unified Ideograph
0xed76   -> U+54a9   -> 0xfa93   CJK Unified Ideograph
0xed77   -> U+54ff   -> 0xfa94   CJK Unified Ideograph
0xed78   -> U+5586   -> 0xfa95   CJK Unified Ideograph
0xed79   -> U+5759   -> 0xfa96   CJK Unified Ideograph
0xed7a   -> U+5765   -> 0xfa97   CJK Unified Ideograph
0xed7b   -> U+57ac   -> 0xfa98   CJK Unified Ideograph
0xed7c   -> U+57c8   -> 0xfa99   CJK Unified Ideograph
0xed7d   -> U+57c7   -> 0xfa9a   CJK Unified Ideograph
0xed7e   -> U+fa0f   -> 0xfa9b   CJK compatibility Ideograph
0xed80   -> U+fa10   -> 0xfa9c   CJK compatibility Ideograph
0xed81   -> U+589e   -> 0xfa9d   CJK Unified Ideograph
0xed82   -> U+58b2   -> 0xfa9e   CJK Unified Ideograph
0xed83   -> U+590b   -> 0xfa9f   CJK Unified Ideograph
0xed84   -> U+5953   -> 0xfaa0   CJK Unified Ideograph
0xed85   -> U+595b   -> 0xfaa1   CJK Unified Ideograph
0xed86   -> U+595d   -> 0xfaa2   CJK Unified Ideograph
0xed87   -> U+5963   -> 0xfaa3   CJK Unified Ideograph
0xed88   -> U+59a4   -> 0xfaa4   CJK Unified Ideograph
0xed89   -> U+59ba   -> 0xfaa5   CJK Unified Ideograph
0xed8a   -> U+5b56   -> 0xfaa6   CJK Unified Ideograph
0xed8b   -> U+5bc0   -> 0xfaa7   CJK Unified Ideograph
0xed8c   -> U+752f   -> 0xfaa8   CJK Unified Ideograph
0xed8d   -> U+5bd8   -> 0xfaa9   CJK Unified Ideograph
0xed8e   -> U+5bec   -> 0xfaaa   CJK Unified Ideograph
0xed8f   -> U+5c1e   -> 0xfaab   CJK Unified Ideograph
0xed90   -> U+5ca6   -> 0xfaac   CJK Unified Ideograph
0xed91   -> U+5cba   -> 0xfaad   CJK Unified Ideograph
0xed92   -> U+5cf5   -> 0xfaae   CJK Unified Ideograph
0xed93   -> U+5d27   -> 0xfaaf   CJK Unified Ideograph
0xed94   -> U+5d53   -> 0xfab0   CJK Unified Ideograph
0xed95   -> U+fa11   -> 0xfab1   CJK compatibility Ideograph
0xed96   -> U+5d42   -> 0xfab2   CJK Unified Ideograph
0xed97   -> U+5d6d   -> 0xfab3   CJK Unified Ideograph
0xed98   -> U+5db8   -> 0xfab4   CJK Unified Ideograph
0xed99   -> U+5db9   -> 0xfab5   CJK Unified Ideograph
0xed9a   -> U+5dd0   -> 0xfab6   CJK Unified Ideograph
0xed9b   -> U+5f21   -> 0xfab7   CJK Unified Ideograph
0xed9c   -> U+5f34   -> 0xfab8   CJK Unified Ideograph
0xed9d   -> U+5f67   -> 0xfab9   CJK Unified Ideograph
0xed9e   -> U+5fb7   -> 0xfaba   CJK Unified Ideograph
0xed9f   -> U+5fde   -> 0xfabb   CJK Unified Ideograph
0xeda0   -> U+605d   -> 0xfabc   CJK Unified Ideograph
0xeda1   -> U+6085   -> 0xfabd   CJK Unified Ideograph
0xeda2   -> U+608a   -> 0xfabe   CJK Unified Ideograph
0xeda3   -> U+60de   -> 0xfabf   CJK Unified Ideograph
0xeda4   -> U+60d5   -> 0xfac0   CJK Unified Ideograph
0xeda5   -> U+6120   -> 0xfac1   CJK Unified Ideograph
0xeda6   -> U+60f2   -> 0xfac2   CJK Unified Ideograph
0xeda7   -> U+6111   -> 0xfac3   CJK Unified Ideograph
0xeda8   -> U+6137   -> 0xfac4   CJK Unified Ideograph
0xeda9   -> U+6130   -> 0xfac5   CJK Unified Ideograph
0xedaa   -> U+6198   -> 0xfac6   CJK Unified Ideograph
0xedab   -> U+6213   -> 0xfac7   CJK Unified Ideograph
0xedac   -> U+62a6   -> 0xfac8   CJK Unified Ideograph
0xedad   -> U+63f5   -> 0xfac9   CJK Unified Ideograph
0xedae   -> U+6460   -> 0xfaca   CJK Unified Ideograph
0xedaf   -> U+649d   -> 0xfacb   CJK Unified Ideograph
0xedb0   -> U+64ce   -> 0xfacc   CJK Unified Ideograph
0xedb1   -> U+654e   -> 0xfacd   CJK Unified Ideograph
0xedb2   -> U+6600   -> 0xface   CJK Unified Ideograph
0xedb3   -> U+6615   -> 0xfacf   CJK Unified Ideograph
0xedb4   -> U+663b   -> 0xfad0   CJK Unified Ideograph
0xedb5   -> U+6609   -> 0xfad1   CJK Unified Ideograph
0xedb6   -> U+662e   -> 0xfad2   CJK Unified Ideograph
0xedb7   -> U+661e   -> 0xfad3   CJK Unified Ideograph
0xedb8   -> U+6624   -> 0xfad4   CJK Unified Ideograph
0xedb9   -> U+6665   -> 0xfad5   CJK Unified Ideograph
0xedba   -> U+6657   -> 0xfad6   CJK Unified Ideograph
0xedbb   -> U+6659   -> 0xfad7   CJK Unified Ideograph
0xedbc   -> U+fa12   -> 0xfad8   CJK compatibility Ideograph
0xedbd   -> U+6673   -> 0xfad9   CJK Unified Ideograph
0xedbe   -> U+6699   -> 0xfada   CJK Unified Ideograph
0xedbf   -> U+66a0   -> 0xfadb   CJK Unified Ideograph
0xedc0   -> U+66b2   -> 0xfadc   CJK Unified Ideograph
0xedc1   -> U+66bf   -> 0xfadd   CJK Unified Ideograph
0xedc2   -> U+66fa   -> 0xfade   CJK Unified Ideograph
0xedc3   -> U+670e   -> 0xfadf   CJK Unified Ideograph
0xedc4   -> U+f929   -> 0xfae0   CJK compatibility Ideograph
0xedc5   -> U+6766   -> 0xfae1   CJK Unified Ideograph
0xedc6   -> U+67bb   -> 0xfae2   CJK Unified Ideograph
0xedc7   -> U+6852   -> 0xfae3   CJK Unified Ideograph
0xedc8   -> U+67c0   -> 0xfae4   CJK Unified Ideograph
0xedc9   -> U+6801   -> 0xfae5   CJK Unified Ideograph
0xedca   -> U+6844   -> 0xfae6   CJK Unified Ideograph
0xedcb   -> U+68cf   -> 0xfae7   CJK Unified Ideograph
0xedcc   -> U+fa13   -> 0xfae8   CJK compatibility Ideograph
0xedcd   -> U+6968   -> 0xfae9   CJK Unified Ideograph
0xedce   -> U+fa14   -> 0xfaea   CJK compatibility Ideograph
0xedcf   -> U+6998   -> 0xfaeb   CJK Unified Ideograph
0xedd0   -> U+69e2   -> 0xfaec   CJK Unified Ideograph
0xedd1   -> U+6a30   -> 0xfaed   CJK Unified Ideograph
0xedd2   -> U+6a6b   -> 0xfaee   CJK Unified Ideograph
0xedd3   -> U+6a46   -> 0xfaef   CJK Unified Ideograph
0xedd4   -> U+6a73   -> 0xfaf0   CJK Unified Ideograph
0xedd5   -> U+6a7e   -> 0xfaf1   CJK Unified Ideograph
0xedd6   -> U+6ae2   -> 0xfaf2   CJK Unified Ideograph
0xedd7   -> U+6ae4   -> 0xfaf3   CJK Unified Ideograph
0xedd8   -> U+6bd6   -> 0xfaf4   CJK Unified Ideograph
0xedd9   -> U+6c3f   -> 0xfaf5   CJK Unified Ideograph
0xedda   -> U+6c5c   -> 0xfaf6   CJK Unified Ideograph
0xeddb   -> U+6c86   -> 0xfaf7   CJK Unified Ideograph
0xeddc   -> U+6c6f   -> 0xfaf8   CJK Unified Ideograph
0xeddd   -> U+6cda   -> 0xfaf9   CJK Unified Ideograph
0xedde   -> U+6d04   -> 0xfafa   CJK Unified Ideograph
0xeddf   -> U+6d87   -> 0xfafb   CJK Unified Ideograph
0xede0   -> U+6d6f   -> 0xfafc   CJK Unified Ideograph
0xede1   -> U+6d96   -> 0xfb40   CJK Unified Ideograph
0xede2   -> U+6dac   -> 0xfb41   CJK Unified Ideograph
0xede3   -> U+6dcf   -> 0xfb42   CJK Unified Ideograph
0xede4   -> U+6df8   -> 0xfb43   CJK Unified Ideograph
0xede5   -> U+6df2   -> 0xfb44   CJK Unified Ideograph
0xede6   -> U+6dfc   -> 0xfb45   CJK Unified Ideograph
0xede7   -> U+6e39   -> 0xfb46   CJK Unified Ideograph
0xede8   -> U+6e5c   -> 0xfb47   CJK Unified Ideograph
0xede9   -> U+6e27   -> 0xfb48   CJK Unified Ideograph
0xedea   -> U+6e3c   -> 0xfb49   CJK Unified Ideograph
0xedeb   -> U+6ebf   -> 0xfb4a   CJK Unified Ideograph
0xedec   -> U+6f88   -> 0xfb4b   CJK Unified Ideograph
0xeded   -> U+6fb5   -> 0xfb4c   CJK Unified Ideograph
0xedee   -> U+6ff5   -> 0xfb4d   CJK Unified Ideograph
0xedef   -> U+7005   -> 0xfb4e   CJK Unified Ideograph
0xedf0   -> U+7007   -> 0xfb4f   CJK Unified Ideograph
0xedf1   -> U+7028   -> 0xfb50   CJK Unified Ideograph
0xedf2   -> U+7085   -> 0xfb51   CJK Unified Ideograph
0xedf3   -> U+70ab   -> 0xfb52   CJK Unified Ideograph
0xedf4   -> U+710f   -> 0xfb53   CJK Unified Ideograph
0xedf5   -> U+7104   -> 0xfb54   CJK Unified Ideograph
0xedf6   -> U+715c   -> 0xfb55   CJK Unified Ideograph
0xedf7   -> U+7146   -> 0xfb56   CJK Unified Ideograph
0xedf8   -> U+7147   -> 0xfb57   CJK Unified Ideograph
0xedf9   -> U+fa15   -> 0xfb58   CJK compatibility Ideograph
0xedfa   -> U+71c1   -> 0xfb59   CJK Unified Ideograph
0xedfb   -> U+71fe   -> 0xfb5a   CJK Unified Ideograph
0xedfc   -> U+72b1   -> 0xfb5b   CJK Unified Ideograph
0xee40   -> U+72be   -> 0xfb5c   CJK Unified Ideograph
0xee41   -> U+7324   -> 0xfb5d   CJK Unified Ideograph
0xee42   -> U+fa16   -> 0xfb5e   CJK compatibility Ideograph
0xee43   -> U+7377   -> 0xfb5f   CJK Unified Ideograph
0xee44   -> U+73bd   -> 0xfb60   CJK Unified Ideograph
0xee45   -> U+73c9   -> 0xfb61   CJK Unified Ideograph
0xee46   -> U+73d6   -> 0xfb62   CJK Unified Ideograph
0xee47   -> U+73e3   -> 0xfb63   CJK Unified Ideograph
0xee48   -> U+73d2   -> 0xfb64   CJK Unified Ideograph
0xee49   -> U+7407   -> 0xfb65   CJK Unified Ideograph
0xee4a   -> U+73f5   -> 0xfb66   CJK Unified Ideograph
0xee4b   -> U+7426   -> 0xfb67   CJK Unified Ideograph
0xee4c   -> U+742a   -> 0xfb68   CJK Unified Ideograph
0xee4d   -> U+7429   -> 0xfb69   CJK Unified Ideograph
0xee4e   -> U+742e   -> 0xfb6a   CJK Unified Ideograph
0xee4f   -> U+7462   -> 0xfb6b   CJK Unified Ideograph
0xee50   -> U+7489   -> 0xfb6c   CJK Unified Ideograph
0xee51   -> U+749f   -> 0xfb6d   CJK Unified Ideograph
0xee52   -> U+7501   -> 0xfb6e   CJK Unified Ideograph
0xee53   -> U+756f   -> 0xfb6f   CJK Unified Ideograph
0xee54   -> U+7682   -> 0xfb70   CJK Unified Ideograph
0xee55   -> U+769c   -> 0xfb71   CJK Unified Ideograph
0xee56   -> U+769e   -> 0xfb72   CJK Unified Ideograph
0xee57   -> U+769b   -> 0xfb73   CJK Unified Ideograph
0xee58   -> U+76a6   -> 0xfb74   CJK Unified Ideograph
0xee59   -> U+fa17   -> 0xfb75   CJK compatibility Ideograph
0xee5a   -> U+7746   -> 0xfb76   CJK Unified Ideograph
0xee5b   -> U+52af   -> 0xfb77   CJK Unified Ideograph
0xee5c   -> U+7821   -> 0xfb78   CJK Unified Ideograph
0xee5d   -> U+784e   -> 0xfb79   CJK Unified Ideograph
0xee5e   -> U+7864   -> 0xfb7a   CJK Unified Ideograph
0xee5f   -> U+787a   -> 0xfb7b   CJK Unified Ideograph
0xee60   -> U+7930   -> 0xfb7c   CJK Unified Ideograph
0xee61   -> U+fa18   -> 0xfb7d   CJK compatibility Ideograph
0xee62   -> U+fa19   -> 0xfb7e   CJK compatibility Ideograph
0xee63   -> U+fa1a   -> 0xfb80   CJK compatibility Ideograph
0xee64   -> U+7994   -> 0xfb81   CJK Unified Ideograph
0xee65   -> U+fa1b   -> 0xfb82   CJK compatibility Ideograph
0xee66   -> U+799b   -> 0xfb83   CJK Unified Ideograph
0xee67   -> U+7ad1   -> 0xfb84   CJK Unified Ideograph
0xee68   -> U+7ae7   -> 0xfb85   CJK Unified Ideograph
0xee69   -> U+fa1c   -> 0xfb86   CJK compatibility Ideograph
0xee6a   -> U+7aeb   -> 0xfb87   CJK Unified Ideograph
0xee6b   -> U+7b9e   -> 0xfb88   CJK Unified Ideograph
0xee6c   -> U+fa1d   -> 0xfb89   CJK compatibility Ideograph
0xee6d   -> U+7d48   -> 0xfb8a   CJK Unified Ideograph
0xee6e   -> U+7d5c   -> 0xfb8b   CJK Unified Ideograph
0xee6f   -> U+7db7   -> 0xfb8c   CJK Unified Ideograph
0xee70   -> U+7da0   -> 0xfb8d   CJK Unified Ideograph
0xee71   -> U+7dd6   -> 0xfb8e   CJK Unified Ideograph
0xee72   -> U+7e52   -> 0xfb8f   CJK Unified Ideograph
0xee73   -> U+7f47   -> 0xfb90   CJK Unified Ideograph
0xee74   -> U+7fa1   -> 0xfb91   CJK Unified Ideograph
0xee75   -> U+fa1e   -> 0xfb92   CJK compatibility Ideograph
0xee76   -> U+8301   -> 0xfb93   CJK Unified Ideograph
0xee77   -> U+8362   -> 0xfb94   CJK Unified Ideograph
0xee78   -> U+837f   -> 0xfb95   CJK Unified Ideograph
0xee79   -> U+83c7   -> 0xfb96   CJK Unified Ideograph
0xee7a   -> U+83f6   -> 0xfb97   CJK Unified Ideograph
0xee7b   -> U+8448   -> 0xfb98   CJK Unified Ideograph
0xee7c   -> U+84b4   -> 0xfb99   CJK Unified Ideograph
0xee7d   -> U+8553   -> 0xfb9a   CJK Unified Ideograph
0xee7e   -> U+8559   -> 0xfb9b   CJK Unified Ideograph
0xee80   -> U+856b   -> 0xfb9c   CJK Unified Ideograph
0xee81   -> U+fa1f   -> 0xfb9d   CJK compatibility Ideograph
0xee82   -> U+85b0   -> 0xfb9e   CJK Unified Ideograph
0xee83   -> U+fa20   -> 0xfb9f   CJK compatibility Ideograph
0xee84   -> U+fa21   -> 0xfba0   CJK compatibility Ideograph
0xee85   -> U+8807   -> 0xfba1   CJK Unified Ideograph
0xee86   -> U+88f5   -> 0xfba2   CJK Unified Ideograph
0xee87   -> U+8a12   -> 0xfba3   CJK Unified Ideograph
0xee88   -> U+8a37   -> 0xfba4   CJK Unified Ideograph
0xee89   -> U+8a79   -> 0xfba5   CJK Unified Ideograph
0xee8a   -> U+8aa7   -> 0xfba6   CJK Unified Ideograph
0xee8b   -> U+8abe   -> 0xfba7   CJK Unified Ideograph
0xee8c   -> U+8adf   -> 0xfba8   CJK Unified Ideograph
0xee8d   -> U+fa22   -> 0xfba9   CJK compatibility Ideograph
0xee8e   -> U+8af6   -> 0xfbaa   CJK Unified Ideograph
0xee8f   -> U+8b53   -> 0xfbab   CJK Unified Ideograph
0xee90   -> U+8b7f   -> 0xfbac   CJK Unified Ideograph
0xee91   -> U+8cf0   -> 0xfbad   CJK Unified Ideograph
0xee92   -> U+8cf4   -> 0xfbae   CJK Unified Ideograph
0xee93   -> U+8d12   -> 0xfbaf   CJK Unified Ideograph
0xee94   -> U+8d76   -> 0xfbb0   CJK Unified Ideograph
0xee95   -> U+fa23   -> 0xfbb1   CJK compatibility Ideograph
0xee96   -> U+8ecf   -> 0xfbb2   CJK Unified Ideograph
0xee97   -> U+fa24   -> 0xfbb3   CJK compatibility Ideograph
0xee98   -> U+fa25   -> 0xfbb4   CJK compatibility Ideograph
0xee99   -> U+9067   -> 0xfbb5   CJK Unified Ideograph
0xee9a   -> U+90de   -> 0xfbb6   CJK Unified Ideograph
0xee9b   -> U+fa26   -> 0xfbb7   CJK compatibility Ideograph
0xee9c   -> U+9115   -> 0xfbb8   CJK Unified Ideograph
0xee9d   -> U+9127   -> 0xfbb9   CJK Unified Ideograph
0xee9e   -> U+91da   -> 0xfbba   CJK Unified Ideograph
0xee9f   -> U+91d7   -> 0xfbbb   CJK Unified Ideograph
0xeea0   -> U+91de   -> 0xfbbc   CJK Unified Ideograph
0xeea1   -> U+91ed   -> 0xfbbd   CJK Unified Ideograph
0xeea2   -> U+91ee   -> 0xfbbe   CJK Unified Ideograph
0xeea3   -> U+91e4   -> 0xfbbf   CJK Unified Ideograph
0xeea4   -> U+91e5   -> 0xfbc0   CJK Unified Ideograph
0xeea5   -> U+9206   -> 0xfbc1   CJK Unified Ideograph
0xeea6   -> U+9210   -> 0xfbc2   CJK Unified Ideograph
0xeea7   -> U+920a   -> 0xfbc3   CJK Unified Ideograph
0xeea8   -> U+923a   -> 0xfbc4   CJK Unified Ideograph
0xeea9   -> U+9240   -> 0xfbc5   CJK Unified Ideograph
0xeeaa   -> U+923c   -> 0xfbc6   CJK Unified Ideograph
0xeeab   -> U+924e   -> 0xfbc7   CJK Unified Ideograph
0xeeac   -> U+9259   -> 0xfbc8   CJK Unified Ideograph
0xeead   -> U+9251   -> 0xfbc9   CJK Unified Ideograph
0xeeae   -> U+9239   -> 0xfbca   CJK Unified Ideograph
0xeeaf   -> U+9267   -> 0xfbcb   CJK Unified Ideograph
0xeeb0   -> U+92a7   -> 0xfbcc   CJK Unified Ideograph
0xeeb1   -> U+9277   -> 0xfbcd   CJK Unified Ideograph
0xeeb2   -> U+9278   -> 0xfbce   CJK Unified Ideograph
0xeeb3   -> U+92e7   -> 0xfbcf   CJK Unified Ideograph
0xeeb4   -> U+92d7   -> 0xfbd0   CJK Unified Ideograph
0xeeb5   -> U+92d9   -> 0xfbd1   CJK Unified Ideograph
0xeeb6   -> U+92d0   -> 0xfbd2   CJK Unified Ideograph
0xeeb7   -> U+fa27   -> 0xfbd3   CJK compatibility Ideograph
0xeeb8   -> U+92d5   -> 0xfbd4   CJK Unified Ideograph
0xeeb9   -> U+92e0   -> 0xfbd5   CJK Unified Ideograph
0xeeba   -> U+92d3   -> 0xfbd6   CJK Unified Ideograph
0xeebb   -> U+9325   -> 0xfbd7   CJK Unified Ideograph
0xeebc   -> U+9321   -> 0xfbd8   CJK Unified Ideograph
0xeebd   -> U+92fb   -> 0xfbd9   CJK Unified Ideograph
0xeebe   -> U+fa28   -> 0xfbda   CJK compatibility Ideograph
0xeebf   -> U+931e   -> 0xfbdb   CJK Unified Ideograph
0xeec0   -> U+92ff   -> 0xfbdc   CJK Unified Ideograph
0xeec1   -> U+931d   -> 0xfbdd   CJK Unified Ideograph
0xeec2   -> U+9302   -> 0xfbde   CJK Unified Ideograph
0xeec3   -> U+9370   -> 0xfbdf   CJK Unified Ideograph
0xeec4   -> U+9357   -> 0xfbe0   CJK Unified Ideograph
0xeec5   -> U+93a4   -> 0xfbe1   CJK Unified Ideograph
0xeec6   -> U+93c6   -> 0xfbe2   CJK Unified Ideograph
0xeec7   -> U+93de   -> 0xfbe3   CJK Unified Ideograph
0xeec8   -> U+93f8   -> 0xfbe4   CJK Unified Ideograph
0xeec9   -> U+9431   -> 0xfbe5   CJK Unified Ideograph
0xeeca   -> U+9445   -> 0xfbe6   CJK Unified Ideograph
0xeecb   -> U+9448   -> 0xfbe7   CJK Unified Ideograph
0xeecc   -> U+9592   -> 0xfbe8   CJK Unified Ideograph
0xeecd   -> U+f9dc   -> 0xfbe9   CJK compatibility Ideograph
0xeece   -> U+fa29   -> 0xfbea   CJK compatibility Ideograph
0xeecf   -> U+969d   -> 0xfbeb   CJK Unified Ideograph
0xeed0   -> U+96af   -> 0xfbec   CJK Unified Ideograph
0xeed1   -> U+9733   -> 0xfbed   CJK Unified Ideograph
0xeed2   -> U+973b   -> 0xfbee   CJK Unified Ideograph
0xeed3   -> U+9743   -> 0xfbef   CJK Unified Ideograph
0xeed4   -> U+974d   -> 0xfbf0   CJK Unified Ideograph
0xeed5   -> U+974f   -> 0xfbf1   CJK Unified Ideograph
0xeed6   -> U+9751   -> 0xfbf2   CJK Unified Ideograph
0xeed7   -> U+9755   -> 0xfbf3   CJK Unified Ideograph
0xeed8   -> U+9857   -> 0xfbf4   CJK Unified Ideograph
0xeed9   -> U+9865   -> 0xfbf5   CJK Unified Ideograph
0xeeda   -> U+fa2a   -> 0xfbf6   CJK compatibility Ideograph
0xeedb   -> U+fa2b   -> 0xfbf7   CJK compatibility Ideograph
0xeedc   -> U+9927   -> 0xfbf8   CJK Unified Ideograph
0xeedd   -> U+fa2c   -> 0xfbf9   CJK compatibility Ideograph
0xeede   -> U+999e   -> 0xfbfa   CJK Unified Ideograph
0xeedf   -> U+9a4e   -> 0xfbfb   CJK Unified Ideograph
0xeee0   -> U+9ad9   -> 0xfbfc   CJK Unified Ideograph
0xeee1   -> U+9adc   -> 0xfc40   CJK Unified Ideograph
0xeee2   -> U+9b75   -> 0xfc41   CJK Unified Ideograph
0xeee3   -> U+9b72   -> 0xfc42   CJK Unified Ideograph
0xeee4   -> U+9b8f   -> 0xfc43   CJK Unified Ideograph
0xeee5   -> U+9bb1   -> 0xfc44   CJK Unified Ideograph
0xeee6   -> U+9bbb   -> 0xfc45   CJK Unified Ideograph
0xeee7   -> U+9c00   -> 0xfc46   CJK Unified Ideograph
0xeee8   -> U+9d70   -> 0xfc47   CJK Unified Ideograph
0xeee9   -> U+9d6b   -> 0xfc48   CJK Unified Ideograph
0xeeea   -> U+fa2d   -> 0xfc49   CJK compatibility Ideograph
0xeeeb   -> U+9e19   -> 0xfc4a   CJK Unified Ideograph
0xeeec   -> U+9ed1   -> 0xfc4b   CJK Unified Ideograph
0xeeef   -> U+2170   -> 0xfa40   Small Roman Numeral One
0xeef0   -> U+2171   -> 0xfa41   Small Roman Numeral Two
0xeef1   -> U+2172   -> 0xfa42   Small Roman Numeral Three
0xeef2   -> U+2173   -> 0xfa43   Small Roman Numeral Four
0xeef3   -> U+2174   -> 0xfa44   Small Roman Numeral Five
0xeef4   -> U+2175   -> 0xfa45   Small Roman Numeral Six
0xeef5   -> U+2176   -> 0xfa46   Small Roman Numeral Seven
0xeef6   -> U+2177   -> 0xfa47   Small Roman Numeral Eight
0xeef7   -> U+2178   -> 0xfa48   Small Roman Numeral Nine
0xeef8   -> U+2179   -> 0xfa49   Small Roman Numeral Ten
0xeef9   -> U+ffe2   -> 0x81ca   Fullwidth Not Sign
0xeefa   -> U+ffe4   -> 0xfa55   Fullwidth Broken Bar
0xeefb   -> U+ff07   -> 0xfa56   Fullwidth Apostrophe
0xeefc   -> U+ff02   -> 0xfa57   Fullwidth Quotation Mark
0xfa4a   -> U+2160   -> 0x8754   Roman Numeral One
0xfa4b   -> U+2161   -> 0x8755   Roman Numeral Two
0xfa4c   -> U+2162   -> 0x8756   Roman Numeral Three
0xfa4d   -> U+2163   -> 0x8757   Roman Numeral Four
0xfa4e   -> U+2164   -> 0x8758   Roman Numeral Five
0xfa4f   -> U+2165   -> 0x8759   Roman Numeral Six
0xfa50   -> U+2166   -> 0x875a   Roman Numeral Seven
0xfa51   -> U+2167   -> 0x875b   Roman Numeral Eight
0xfa52   -> U+2168   -> 0x875c   Roman Numeral Nine
0xfa53   -> U+2169   -> 0x875d   Roman Numeral Ten
0xfa54   -> U+ffe2   -> 0x81ca   Fullwidth Not Sign
0xfa58   -> U+3231   -> 0x878a   Parenthesized Ideograph Stock
0xfa59   -> U+2116   -> 0x8782   Numero Sign
0xfa5a   -> U+2121   -> 0x8784   Telephone Sign
0xfa5b   -> U+2235   -> 0x81e6   Because"))
  (with-input-from-string (s kb170559)
    (loop for line = (read-line s nil) until (null line)
          do (let ((ucs (parse-integer (subseq line 14 18) :radix 16))
                   (cp932 (parse-integer (subseq line 26 30) :radix 16)))
               (setf (gethash ucs *ucs-to-cp932-hash*) cp932)))))

(defun eucjp-to-ucs (code)
  (values (gethash code *eucjp-to-ucs-hash*)))

(defun ucs-to-eucjp (code)
  (values (gethash code *ucs-to-eucjp-hash*)))

(defun cp932-to-ucs (code)
  (values (gethash code *cp932-to-ucs-hash*)))

(defun ucs-to-cp932 (code)
  (values (gethash code *ucs-to-cp932-hash*)))

;;;; EUC-JP

(define-character-encoding :eucjp
    "An 8-bit, variable-length character encoding in which
character code points in the range #x00-#x7f can be encoded in a
single octet; characters with larger code values can be encoded
in 2 to 3 bytes."
  :max-units-per-char 3
  :literal-char-code-limit #x80)


(define-octet-counter :eucjp (getter type)
  `(named-lambda eucjp-octet-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with noctets fixnum = 0
           for i fixnum from start below end
           for code of-type code-point = (,getter seq i)
           do (let* ((c (ucs-to-eucjp code))
                     (new (+ (cond ((< #xffff c) 3)
                                   ((< #xff c) 2)
                                   (t 1))
                             noctets)))
                (if (and (plusp max) (> new max))
                    (loop-finish)
                    (setq noctets new)))
           finally (return (values noctets i)))))

(define-code-point-counter :eucjp (getter type)
  `(named-lambda eucjp-code-point-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with nchars fixnum = 0
           with i fixnum = start
           while (< i end) do
             (let* ((octet (,getter seq i))
                    (next-i (+ i (cond ((= #x8f octet) 3)
                                       ((or (< #xa0 octet #xff)
                                            (= #x8e octet)) 2)
                                       (t 1)))))
               (declare (type ub8 octet) (fixnum next-i))
               (cond ((> next-i end)
                      ;; Should we add restarts to this error, we'll have
                      ;; to figure out a way to communicate with the
                      ;; decoder since we probably want to do something
                      ;; about it right here when we have a chance to
                      ;; change the count or something.  (Like an
                      ;; alternative replacement character or perhaps the
                      ;; existence of this error so that the decoder
                      ;; doesn't have to check for it on every iteration
                      ;; like we do.)
                      ;;
                      ;; FIXME: The data for this error is not right.
                      (decoding-error (vector octet) :eucjp seq i
                                      nil 'end-of-input-in-character)
                      (return (values (1+ nchars) end)))
                     (t
                      (setq nchars (1+ nchars)
                            i next-i)
                      (when (and (plusp max) (= nchars max))
                        (return (values nchars i))))))
           finally (progn (assert (= i end))
                     (return (values nchars i))))))

(define-encoder :eucjp (getter src-type setter dest-type)
  `(named-lambda eucjp-encoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop with di fixnum = d-start
           for i fixnum from start below end
           for code of-type code-point = (,getter src i)
           for eucjp of-type code-point
             = (ucs-to-eucjp code) do
               (macrolet ((set-octet (offset value)
                            `(,',setter ,value dest (the fixnum (+ di ,offset)))))
                 (cond
                   ;; 1 octet
                   ((< eucjp #x100)
                    (set-octet 0 eucjp)
                    (incf di))
                   ;; 2 octets
                   ((< eucjp #x10000)
                    (set-octet 0 (f-logand #xff (f-ash eucjp -8)))
                    (set-octet 1 (logand eucjp #xff))
                    (incf di 2))
                   ;; 3 octets
                   (t
                    (set-octet 0 (f-logand #xff (f-ash eucjp -16)))
                    (set-octet 1 (f-logand #xff (f-ash eucjp -8)))
                    (set-octet 2 (logand eucjp #xff))
                    (incf di 3))
                   ))
           finally (return (the fixnum (- di d-start))))))


(define-decoder :eucjp (getter src-type setter dest-type)
  `(named-lambda eucjp-decoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((u2 0))
       (declare (type ub8 u2))
       (loop for di fixnum from d-start
             for i fixnum from start below end
             for u1 of-type ub8 = (,getter src i) do
               ;; Note: CONSUME-OCTET doesn't check if I is being
               ;; incremented past END.  We're assuming that END has
               ;; been calculated with the CODE-POINT-POINTER above that
               ;; checks this.
               (macrolet
                   ((consume-octet ()
                      `(let ((next-i (incf i)))
                         (if (= next-i end)
                             ;; FIXME: data for this error is incomplete.
                             ;; and signalling this error twice
                             (return-from setter-block
                               (decoding-error nil :eucjp src i +repl+
                                               'end-of-input-in-character))
                             (,',getter src next-i))))
                    (handle-error (n &optional (c 'character-decoding-error))
                      `(decoding-error
                        (vector ,@(subseq '(u1 u2) 0 n))
                        :eucjp src (1+ (- i ,n)) +repl+ ',c))
                    (handle-error-if-icb (var n)
                      `(when (not (< #x7f ,var #xc0))
                         (decf i)
                         (return-from setter-block
                           (handle-error ,n invalid-utf8-continuation-byte)))))
                 (,setter
                  (block setter-block
                    (cond
                      ;; 3 octets
                      ((= u1 #x8f)
                       (setq u2 (consume-octet))
                       (eucjp-to-ucs (logior #x8f0000
                                             (f-ash u2 8)
                                             (consume-octet))))
                      ;; 2 octets
                      ((or (= u1 #x8e)
                           (< #xa0 u1 #xff))
                       (eucjp-to-ucs (logior (f-ash u1 8)
                                             (consume-octet))))
                      ;; 1 octet
                      (t
                       (eucjp-to-ucs u1))))
                  dest di))
         finally (return (the fixnum (- di d-start)))))))

;;;; CP932

(define-character-encoding :cp932
    "An 8-bit, variable-length character encoding in which
character code points in the range #x00-#x7f can be encoded in a
single octet; characters with larger code values can be encoded
in 2 bytes."
  :max-units-per-char 2
  :literal-char-code-limit #x80)


(define-octet-counter :cp932 (getter type)
  `(named-lambda cp932-octet-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with noctets fixnum = 0
           for i fixnum from start below end
           for code of-type code-point = (,getter seq i)
           do (let* ((c (ucs-to-cp932 code))
                     (new (+ (cond ((< #xff c) 2)
                                   (t 1))
                             noctets)))
                (if (and (plusp max) (> new max))
                    (loop-finish)
                    (setq noctets new)))
           finally (return (values noctets i)))))

(define-code-point-counter :cp932 (getter type)
  `(named-lambda cp932-code-point-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with nchars fixnum = 0
           with i fixnum = start
           while (< i end) do
             (let* ((octet (,getter seq i))
                    (next-i (+ i (cond ((or (<= #x81 octet #x9f)
                                            (<= #xe0 octet #xfc))
                                        2)
                                       (t 1)))))
               (declare (type ub8 octet) (fixnum next-i))
               (cond ((> next-i end)
                      ;; Should we add restarts to this error, we'll have
                      ;; to figure out a way to communicate with the
                      ;; decoder since we probably want to do something
                      ;; about it right here when we have a chance to
                      ;; change the count or something.  (Like an
                      ;; alternative replacement character or perhaps the
                      ;; existence of this error so that the decoder
                      ;; doesn't have to check for it on every iteration
                      ;; like we do.)
                      ;;
                      ;; FIXME: The data for this error is not right.
                      (decoding-error (vector octet) :cp932 seq i
                                      nil 'end-of-input-in-character)
                      (return (values (1+ nchars) end)))
                     (t
                      (setq nchars (1+ nchars)
                            i next-i)
                      (when (and (plusp max) (= nchars max))
                        (return (values nchars i))))))
           finally (progn (assert (= i end))
                     (return (values nchars i))))))

(define-encoder :cp932 (getter src-type setter dest-type)
  `(named-lambda cp932-encoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop with di fixnum = d-start
           for i fixnum from start below end
           for code of-type code-point = (,getter src i)
           for cp932 of-type code-point
             = (ucs-to-cp932 code) do
               (macrolet ((set-octet (offset value)
                            `(,',setter ,value dest (the fixnum (+ di ,offset)))))
                 (cond
                   ;; 1 octet
                   ((< cp932 #x100)
                    (set-octet 0 cp932)
                    (incf di))
                   ;; 2 octets
                   ((< cp932 #x10000)
                    (set-octet 0 (f-logand #xff (f-ash cp932 -8)))
                    (set-octet 1 (logand cp932 #xff))
                    (incf di 2))
                   ;; 3 octets
                   (t
                    (set-octet 0 (f-logand #xff (f-ash cp932 -16)))
                    (set-octet 1 (f-logand #xff (f-ash cp932 -8)))
                    (set-octet 2 (logand cp932 #xff))
                    (incf di 3))
                   ))
           finally (return (the fixnum (- di d-start))))))


(define-decoder :cp932 (getter src-type setter dest-type)
  `(named-lambda cp932-decoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((u2 0))
       (declare (type ub8 u2))
       (loop for di fixnum from d-start
             for i fixnum from start below end
             for u1 of-type ub8 = (,getter src i) do
               ;; Note: CONSUME-OCTET doesn't check if I is being
               ;; incremented past END.  We're assuming that END has
               ;; been calculated with the CODE-POINT-POINTER above that
               ;; checks this.
               (macrolet
                   ((consume-octet ()
                      `(let ((next-i (incf i)))
                         (if (= next-i end)
                             ;; FIXME: data for this error is incomplete.
                             ;; and signalling this error twice
                             (return-from setter-block
                               (decoding-error nil :cp932 src i +repl+
                                               'end-of-input-in-character))
                             (,',getter src next-i))))
                    (handle-error (n &optional (c 'character-decoding-error))
                      `(decoding-error
                        (vector ,@(subseq '(u1 u2) 0 n))
                        :cp932 src (1+ (- i ,n)) +repl+ ',c))
                    (handle-error-if-icb (var n)
                      `(when (not (< #x7f ,var #xc0))
                         (decf i)
                         (return-from setter-block
                           (handle-error ,n invalid-utf8-continuation-byte)))))
                 (,setter
                  (block setter-block
                    (cond
                      ;; 2 octets
                      ((or (<= #x81 u1 #x9f)
                           (<= #xe0 u1 #xfc))
                       (setq u2 (consume-octet))
                       (cp932-to-ucs (logior (f-ash u1 8)
                                             u2)))
                      ;; 1 octet
                      (t
                       (cp932-to-ucs u1))))
                  dest di))
         finally (return (the fixnum (- di d-start)))))))
