;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1251.lisp --- Implementation of the CP1251 character encoding.
;;;
;;; Copyright (C) 2009, Andrey Moskvitin
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:babel-encodings)

(define-character-encoding :koi8-ru
    "An 8-bit, fixed-width character Russian encoding."
  :literal-char-code-limit #x80)

(define-constant +koi8-ru-to-unicode+
    #(#x2500 #x2502 #x250C #x2510 #x2514 #x2518 #x251C #x2524
      #x252C #x2534 #x253C #x2580 #x2584 #x2588 #x258C #x2590
      #x2591 #x2592 #x2593 #x2320 #x25A0 #x2219 #x221A #x2248
      #x2264 #x2265 #x00A0 #x2321 #x00B0 #x00B2 #x00B7 #x00F7
      #x2550 #x2551 #x2552 #x0451 #x0454 #x2554 #x0456 #x0457
      #x2557 #x2558 #x2559 #x255A #x255B #x0491 #x045E #x255E
      #x255F #x2560 #x2561 #x0401 #x0404 #x2563 #x0406 #x0407
      #x2566 #x2567 #x2568 #x2569 #x256A #x0490 #x040E #x00A9
      #x044E #x0430 #x0431 #x0446 #x0434 #x0435 #x0444 #x0433
      #x0445 #x0438 #x0439 #x043A #x043B #x043C #x043D #x043E
      #x043F #x044F #x0440 #x0441 #x0442 #x0443 #x0436 #x0432
      #x044C #x044B #x0437 #x0448 #x044D #x0449 #x0447 #x044A
      #x042E #x0410 #x0411 #x0426 #x0414 #x0415 #x0424 #x0413
      #x0425 #x0418 #x0419 #x041A #x041B #x041C #x041D #x041E
      #x041F #x042F #x0420 #x0421 #x0422 #x0423 #x0416 #x0412
      #x042C #x042B #x0417 #x0428 #x042D #x0429 #x0427 #x042A)
      :test #'equalp)

(define-unibyte-decoder :koi8-ru (octet)
  (if (< octet #x80)
      octet
      (svref +koi8-ru-to-unicode+ (the ub8 (- octet #x80)))))

(define-constant +unicode-04->koi8-ru+
    #(#x7f #x79 #x78 #x7c #x60 #x71 #x41 #x42 #x57 #x47 #x44 #x45 #x56 #x5a
      #x49 #x4a #x4b #x4c #x4d #x4e #x4f #x50 #x52 #x53 #x54 #x55 #x46 #x48
      #x43 #x5e #x5b #x5d #x5f #x59 #x58 #x5c #x40 #x51 nil #x23 nil nil #x24
      nil #x26 #x27 nil nil nil nil nil nil #x2e nil nil nil nil nil nil nil
      nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      nil nil nil nil nil nil #x3d #x2d)
  :test 'equalp)

(define-unibyte-encoder :koi8-ru (code)
  (or (and (< code #x80) code)
      (let ((hi (ldb (byte 8 8) code))
            (lo (ldb (byte 8 0) code)))
        (case hi
          (#x4
           (case lo
             (#x1 #xb3)
             (#x4 #xb4)
             (#x6 #xb6)
             (#x7 #xb7)
             (#xe #xbe)
             (#x10 #xe1)
             (#x11 #xe2)
             (#x12 #xf7)
             (#x13 #xe7)
             (#x14 #xe4)
             (#x15 #xe5)
             (#x16 #xf6)
             (#x17 #xfa)
             (#x18 #xe9)
             (#x19 #xea)
             (#x1a #xeb)
             (#x1b #xec)
             (#x1c #xed)
             (#x1d #xee)
             (#x1e #xef)
             (#x1f #xf0)
             (#x20 #xf2)
             (#x21 #xf3)
             (#x22 #xf4)
             (#x23 #xf5)
             (#x24 #xe6)
             (#x25 #xe8)
             (#x26 #xe3)
             (#x27 #xfe)
             (#x28 #xfb)
             (#x29 #xfd)
             (#x2a #xff)
             (#x2b #xf9)
             (#x2c #xf8)
             (#x2d #xfc)
             (#x2e #xe0)
             (#x2f #xf1)
             (#x30 #xc1)
             (#x31 #xc2)
             (#x32 #xd7)
             (#x33 #xc7)
             (#x34 #xc4)
             (#x35 #xc5)
             (#x36 #xd6)
             (#x37 #xda)
             (#x38 #xc9)
             (#x39 #xca)
             (#x3a #xcb)
             (#x3b #xcc)
             (#x3c #xcd)
             (#x3d #xce)
             (#x3e #xcf)
             (#x3f #xd0)
             (#x40 #xd2)
             (#x41 #xd3)
             (#x42 #xd4)
             (#x43 #xd5)
             (#x44 #xc6)
             (#x45 #xc8)
             (#x46 #xc3)
             (#x47 #xde)
             (#x48 #xdb)
             (#x49 #xdd)
             (#x4a #xdf)
             (#x4b #xd9)
             (#x4c #xd8)
             (#x4d #xdc)
             (#x4e #xc0)
             (#x4f #xd1)
             (#x51 #xa3)
             (#x54 #xa4)
             (#x56 #xa6)
             (#x57 #xa7)
             (#x5e #xae)
             (#x90 #xbd)
             (#x91 #xad)))
          (#x0
           (case lo
             (#xa0 #x9a)
             (#xa9 #xbf)
             (#xb0 #x9c)
             (#xb2 #x9d)
             (#xb7 #x9e)
             (#xf7 #x9f)))
          (#x22 (case lo (#x19 #x95) (#x1a #x96) (#x48 #x97) (#x64 #x98) (#x65 #x99)))
          (#x23 (case lo (#x20 #x93) (#x21 #x9b)))
          (#x25
           (case lo
             (#x0 #x80)
             (#x2 #x81)
             (#xc #x82)
             (#x10 #x83)
             (#x14 #x84)
             (#x18 #x85)
             (#x1c #x86)
             (#x24 #x87)
             (#x2c #x88)
             (#x34 #x89)
             (#x3c #x8a)
             (#x50 #xa0)
             (#x51 #xa1)
             (#x52 #xa2)
             (#x54 #xa5)
             (#x57 #xa8)
             (#x58 #xa9)
             (#x59 #xaa)
             (#x5a #xab)
             (#x5b #xac)
             (#x5e #xaf)
             (#x5f #xb0)
             (#x60 #xb1)
             (#x61 #xb2)
             (#x63 #xb5)
             (#x66 #xb8)
             (#x67 #xb9)
             (#x68 #xba)
             (#x69 #xbb)
             (#x6a #xbc)
             (#x80 #x8b)
             (#x84 #x8c)
             (#x88 #x8d)
             (#x8c #x8e)
             (#x90 #x8f)
             (#x91 #x90)
             (#x92 #x91)
             (#x93 #x92)
             (#xa0 #x94)))))
      (handle-error)))

(define-character-encoding :koi8-r
    "An 8-bit, fixed-width character Russian encoding."
  :literal-char-code-limit #x80)

(define-constant +koi8-r-to-unicode+
    #(#x2500 #x2502 #x250C #x2510 #x2514 #x2518 #x251C #x2524
      #x252C #x2534 #x253C #x2580 #x2584 #x2588 #x258C #x2590
      #x2591 #x2592 #x2593 #x2320 #x25A0 #x2219 #x221A #x2248
      #x2264 #x2265 #x00A0 #x2321 #x00B0 #x00B2 #x00B7 #x00F7
      #x2550 #x2551 #x2552 #x0451 #x2553 #x2554 #x2555 #x2556
      #x2557 #x2558 #x2559 #x255A #x255B #x255C #x255D #x255E
      #x255F #x2560 #x2561 #x0401 #x2562 #x2563 #x2564 #x2565
      #x2566 #x2567 #x2568 #x2569 #x256A #x256B #x256C #x00A9
      #x044E #x0430 #x0431 #x0446 #x0434 #x0435 #x0444 #x0433
      #x0445 #x0438 #x0439 #x043A #x043B #x043C #x043D #x043E
      #x043F #x044F #x0440 #x0441 #x0442 #x0443 #x0436 #x0432
      #x044C #x044B #x0437 #x0448 #x044D #x0449 #x0447 #x044A
      #x042E #x0410 #x0411 #x0426 #x0414 #x0415 #x0424 #x0413
      #x0425 #x0418 #x0419 #x041A #x041B #x041C #x041D #x041E
      #x041F #x042F #x0420 #x0421 #x0422 #x0423 #x0416 #x0412
      #x042C #x042B #x0417 #x0428 #x042D #x0429 #x0427 #x042A)
  :test #'equalp)

(define-unibyte-decoder :koi8-r (octet)
  (if (< octet #x80)
      octet
      (svref +koi8-r-to-unicode+ (the ub8 (- octet #x80)))))

(define-constant +unicode-x04->koi8-r+
    #(nil #x33 nil nil nil nil nil nil nil nil nil nil nil nil nil nil #x61
      #x62 #x77 #x67 #x64 #x65 #x76 #x7a #x69 #x6a #x6b #x6c #x6d #x6e #x6f
      #x70 #x72 #x73 #x74 #x75 #x66 #x68 #x63 #x7e #x7b #x7d #x7f #x79 #x78
      #x7c #x60 #x71 #x41 #x42 #x57 #x47 #x44 #x45 #x56 #x5a #x49 #x4a #x4b
      #x4c #x4d #x4e #x4f #x50 #x52 #x53 #x54 #x55 #x46 #x48 #x43 #x5e #x5b
      #x5d #x5f #x59 #x58 #x5c #x40 #x51 nil #x23)
  :test 'equalp)

(define-constant +unicode-x25->koi8-r+
    #(#x0 nil #x1 nil nil nil nil nil nil nil nil nil #x2 nil nil nil #x3 nil
      nil nil #x4 nil nil nil #x5 nil nil nil #x6 nil nil nil nil nil nil nil
      #x7 nil nil nil nil nil nil nil #x8 nil nil nil nil nil nil nil #x9 nil
      nil nil nil nil nil nil #xa nil nil nil nil nil nil nil nil nil nil nil
      nil nil nil nil nil nil nil nil #x20 #x21 #x22 #x24 #x25 #x26 #x27 #x28
      #x29 #x2a #x2b #x2c #x2d #x2e #x2f #x30 #x31 #x32 #x34 #x35 #x36 #x37
      #x38 #x39 #x3a #x3b #x3c #x3d #x3e nil nil nil nil nil nil nil nil nil
      nil nil nil nil nil nil nil nil nil nil #xb nil nil nil #xc nil nil nil
      #xd nil nil nil #xe nil nil nil #xf #x10 #x11 #x12 nil nil nil nil nil
      nil nil nil nil nil nil nil #x14)
    :test 'equalp)

(define-unibyte-encoder :koi8-r (code)
  (or (and (< code #x80) code)
      (let ((hi (ldb (byte 8 8) code))
            (lo (ldb (byte 8 0) code)))
        (case hi
          (#x4
           (case lo
             (#x1 #xb3)
             (#x10 #xe1)
             (#x11 #xe2)
             (#x12 #xf7)
             (#x13 #xe7)
             (#x14 #xe4)
             (#x15 #xe5)
             (#x16 #xf6)
             (#x17 #xfa)
             (#x18 #xe9)
             (#x19 #xea)
             (#x1a #xeb)
             (#x1b #xec)
             (#x1c #xed)
             (#x1d #xee)
             (#x1e #xef)
             (#x1f #xf0)
             (#x20 #xf2)
             (#x21 #xf3)
             (#x22 #xf4)
             (#x23 #xf5)
             (#x24 #xe6)
             (#x25 #xe8)
             (#x26 #xe3)
             (#x27 #xfe)
             (#x28 #xfb)
             (#x29 #xfd)
             (#x2a #xff)
             (#x2b #xf9)
             (#x2c #xf8)
             (#x2d #xfc)
             (#x2e #xe0)
             (#x2f #xf1)
             (#x30 #xc1)
             (#x31 #xc2)
             (#x32 #xd7)
             (#x33 #xc7)
             (#x34 #xc4)
             (#x35 #xc5)
             (#x36 #xd6)
             (#x37 #xda)
             (#x38 #xc9)
             (#x39 #xca)
             (#x3a #xcb)
             (#x3b #xcc)
             (#x3c #xcd)
             (#x3d #xce)
             (#x3e #xcf)
             (#x3f #xd0)
             (#x40 #xd2)
             (#x41 #xd3)
             (#x42 #xd4)
             (#x43 #xd5)
             (#x44 #xc6)
             (#x45 #xc8)
             (#x46 #xc3)
             (#x47 #xde)
             (#x48 #xdb)
             (#x49 #xdd)
             (#x4a #xdf)
             (#x4b #xd9)
             (#x4c #xd8)
             (#x4d #xdc)
             (#x4e #xc0)
             (#x4f #xd1)
             (#x51 #xa3)))
          (#x0
           (case lo
             (#xa0 #x9a)
             (#xa9 #xbf)
             (#xb0 #x9c)
             (#xb2 #x9d)
             (#xb7 #x9e)
             (#xf7 #x9f)))
          (#x22 (case lo (#x19 #x95) (#x1a #x96) (#x48 #x97) (#x64 #x98) (#x65 #x99)))
          (#x23 (case lo (#x20 #x93) (#x21 #x9b)))
          (#x25
           (case lo
             (#x0 #x80)
             (#x2 #x81)
             (#xc #x82)
             (#x10 #x83)
             (#x14 #x84)
             (#x18 #x85)
             (#x1c #x86)
             (#x24 #x87)
             (#x2c #x88)
             (#x34 #x89)
             (#x3c #x8a)
             (#x50 #xa0)
             (#x51 #xa1)
             (#x52 #xa2)
             (#x53 #xa4)
             (#x54 #xa5)
             (#x55 #xa6)
             (#x56 #xa7)
             (#x57 #xa8)
             (#x58 #xa9)
             (#x59 #xaa)
             (#x5a #xab)
             (#x5b #xac)
             (#x5c #xad)
             (#x5d #xae)
             (#x5e #xaf)
             (#x5f #xb0)
             (#x60 #xb1)
             (#x61 #xb2)
             (#x62 #xb4)
             (#x63 #xb5)
             (#x64 #xb6)
             (#x65 #xb7)
             (#x66 #xb8)
             (#x67 #xb9)
             (#x68 #xba)
             (#x69 #xbb)
             (#x6a #xbc)
             (#x6b #xbd)
             (#x6c #xbe)
             (#x80 #x8b)
             (#x84 #x8c)
             (#x88 #x8d)
             (#x8c #x8e)
             (#x90 #x8f)
             (#x91 #x90)
             (#x92 #x91)
             (#x93 #x92)
             (#xa0 #x94)))))
      (handle-error)))

(define-character-encoding :koi8-u
    "An 8-bit, fixed-width character Ukranian encoding."
  :literal-char-code-limit #x80)

(define-constant +koi8-u-to-unicode+
    #(#x2500 #x2502 #x250C #x2510 #x2514 #x2518 #x251C #x2524
      #x252C #x2534 #x253C #x2580 #x2584 #x2588 #x258C #x2590
      #x2591 #x2592 #x2593 #x2320 #x25A0 #x2219 #x221A #x2248
      #x2264 #x2265 #x00A0 #x2321 #x00B0 #x00B2 #x00B7 #x00F7
      #x2550 #x2551 #x2552 #x0451 #x0454 #x2554 #x0456 #x0457
      #x2557 #x2558 #x2559 #x255A #x255B #x0491 #x255D #x255E
      #x255F #x2560 #x2561 #x0401 #x0404 #x2563 #x0406 #x0407
      #x2566 #x2567 #x2568 #x2569 #x256A #x0490 #x256C #x00A9
      #x044E #x0430 #x0431 #x0446 #x0434 #x0435 #x0444 #x0433
      #x0445 #x0438 #x0439 #x043A #x043B #x043C #x043D #x043E
      #x043F #x044F #x0440 #x0441 #x0442 #x0443 #x0436 #x0432
      #x044C #x044B #x0437 #x0448 #x044D #x0449 #x0447 #x044A
      #x042E #x0410 #x0411 #x0426 #x0414 #x0415 #x0424 #x0413
      #x0425 #x0418 #x0419 #x041A #x041B #x041C #x041D #x041E
      #x041F #x042F #x0420 #x0421 #x0422 #x0423 #x0416 #x0412
      #x042C #x042B #x0417 #x0428 #x042D #x0429 #x0427 #x042A )
  :test #'equalp)

(define-unibyte-decoder :koi8-u (octet)
  (if (< octet #x80)
      octet
      (svref +koi8-u-to-unicode+ (the ub8 (- octet #x80)))))

(define-constant +unicode-x04->koi8-u+
    #(nil #x33 nil nil #x34 nil #x36 #x37 nil nil nil nil nil nil nil nil #x61
      #x62 #x77 #x67 #x64 #x65 #x76 #x7a #x69 #x6a #x6b #x6c #x6d #x6e #x6f
      #x70 #x72 #x73 #x74 #x75 #x66 #x68 #x63 #x7e #x7b #x7d #x7f #x79 #x78
      #x7c #x60 #x71 #x41 #x42 #x57 #x47 #x44 #x45 #x56 #x5a #x49 #x4a #x4b
      #x4c #x4d #x4e #x4f #x50 #x52 #x53 #x54 #x55 #x46 #x48 #x43 #x5e #x5b
      #x5d #x5f #x59 #x58 #x5c #x40 #x51 nil #x23 nil nil #x24 nil #x26 #x27
      nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      nil nil #x3d #x2d)
    :test 'equalp)

(define-constant +unicode-x25->koi8-u+
    #(#x0 nil #x1 nil nil nil nil nil nil nil nil nil #x2 nil nil nil #x3 nil
      nil nil #x4 nil nil nil #x5 nil nil nil #x6 nil nil nil nil nil nil nil
      #x7 nil nil nil nil nil nil nil #x8 nil nil nil nil nil nil nil #x9 nil
      nil nil nil nil nil nil #xa nil nil nil nil nil nil nil nil nil nil nil
      nil nil nil nil nil nil nil nil #x20 #x21 #x22 nil #x25 nil nil #x28
      #x29 #x2a #x2b #x2c nil #x2e #x2f #x30 #x31 #x32 nil #x35 nil nil #x38
      #x39 #x3a #x3b #x3c nil #x3e nil nil nil nil nil nil nil nil nil nil nil
      nil nil nil nil nil nil nil nil #xb nil nil nil #xc nil nil nil #xd nil
      nil nil #xe nil nil nil #xf #x10 #x11 #x12 nil nil nil nil nil nil nil
      nil nil nil nil nil #x14)
  :test 'equalp)

(define-unibyte-encoder :koi8-u (code)
  (or (and (< code #x80) code)
      (let ((hi (ldb (byte 8 8) code))
            (lo (ldb (byte 8 0) code)))
        (case hi
          (#x4
           (case lo
             (#x1 #xb3)
             (#x4 #xb4)
             (#x6 #xb6)
             (#x7 #xb7)
             (#x10 #xe1)
             (#x11 #xe2)
             (#x12 #xf7)
             (#x13 #xe7)
             (#x14 #xe4)
             (#x15 #xe5)
             (#x16 #xf6)
             (#x17 #xfa)
             (#x18 #xe9)
             (#x19 #xea)
             (#x1a #xeb)
             (#x1b #xec)
             (#x1c #xed)
             (#x1d #xee)
             (#x1e #xef)
             (#x1f #xf0)
             (#x20 #xf2)
             (#x21 #xf3)
             (#x22 #xf4)
             (#x23 #xf5)
             (#x24 #xe6)
             (#x25 #xe8)
             (#x26 #xe3)
             (#x27 #xfe)
             (#x28 #xfb)
             (#x29 #xfd)
             (#x2a #xff)
             (#x2b #xf9)
             (#x2c #xf8)
             (#x2d #xfc)
             (#x2e #xe0)
             (#x2f #xf1)
             (#x30 #xc1)
             (#x31 #xc2)
             (#x32 #xd7)
             (#x33 #xc7)
             (#x34 #xc4)
             (#x35 #xc5)
             (#x36 #xd6)
             (#x37 #xda)
             (#x38 #xc9)
             (#x39 #xca)
             (#x3a #xcb)
             (#x3b #xcc)
             (#x3c #xcd)
             (#x3d #xce)
             (#x3e #xcf)
             (#x3f #xd0)
             (#x40 #xd2)
             (#x41 #xd3)
             (#x42 #xd4)
             (#x43 #xd5)
             (#x44 #xc6)
             (#x45 #xc8)
             (#x46 #xc3)
             (#x47 #xde)
             (#x48 #xdb)
             (#x49 #xdd)
             (#x4a #xdf)
             (#x4b #xd9)
             (#x4c #xd8)
             (#x4d #xdc)
             (#x4e #xc0)
             (#x4f #xd1)
             (#x51 #xa3)
             (#x54 #xa4)
             (#x56 #xa6)
             (#x57 #xa7)
             (#x90 #xbd)
             (#x91 #xad)))
          (#x0
           (case lo
             (#xa0 #x9a)
             (#xa9 #xbf)
             (#xb0 #x9c)
             (#xb2 #x9d)
             (#xb7 #x9e)
             (#xf7 #x9f)))
          (#x22 (case lo (#x19 #x95) (#x1a #x96) (#x48 #x97) (#x64 #x98) (#x65 #x99)))
          (#x23 (case lo (#x20 #x93) (#x21 #x9b)))
          (#x25
           (case lo
             (#x0 #x80)
             (#x2 #x81)
             (#xc #x82)
             (#x10 #x83)
             (#x14 #x84)
             (#x18 #x85)
             (#x1c #x86)
             (#x24 #x87)
             (#x2c #x88)
             (#x34 #x89)
             (#x3c #x8a)
             (#x50 #xa0)
             (#x51 #xa1)
             (#x52 #xa2)
             (#x54 #xa5)
             (#x57 #xa8)
             (#x58 #xa9)
             (#x59 #xaa)
             (#x5a #xab)
             (#x5b #xac)
             (#x5d #xae)
             (#x5e #xaf)
             (#x5f #xb0)
             (#x60 #xb1)
             (#x61 #xb2)
             (#x63 #xb5)
             (#x66 #xb8)
             (#x67 #xb9)
             (#x68 #xba)
             (#x69 #xbb)
             (#x6a #xbc)
             (#x6c #xbe)
             (#x80 #x8b)
             (#x84 #x8c)
             (#x88 #x8d)
             (#x8c #x8e)
             (#x90 #x8f)
             (#x91 #x90)
             (#x92 #x91)
             (#x93 #x92)
             (#xa0 #x94)))))
      (handle-error)))
