(in-package :bitio)

(defparameter *octet-bit-reverse-table*
  (make-array 256
              :element-type
              '(unsigned-byte 8)
              :initial-contents
              '(#x00 #x80 #x40 #xC0 #x20 #xA0 #x60 #xE0 #x10 #x90
                #x50 #xD0 #x30 #xB0 #x70 #xF0 #x08 #x88 #x48 #xC8
                #x28 #xA8 #x68 #xE8 #x18 #x98 #x58 #xD8 #x38 #xB8
                #x78 #xF8 #x04 #x84 #x44 #xC4 #x24 #xA4 #x64 #xE4
                #x14 #x94 #x54 #xD4 #x34 #xB4 #x74 #xF4 #x0C #x8C
                #x4C #xCC #x2C #xAC #x6C #xEC #x1C #x9C #x5C #xDC
                #x3C #xBC #x7C #xFC #x02 #x82 #x42 #xC2 #x22 #xA2
                #x62 #xE2 #x12 #x92 #x52 #xD2 #x32 #xB2 #x72 #xF2
                #x0A #x8A #x4A #xCA #x2A #xAA #x6A #xEA #x1A #x9A
                #x5A #xDA #x3A #xBA #x7A #xFA #x06 #x86 #x46 #xC6
                #x26 #xA6 #x66 #xE6 #x16 #x96 #x56 #xD6 #x36 #xB6
                #x76 #xF6 #x0E #x8E #x4E #xCE #x2E #xAE #x6E #xEE
                #x1E #x9E #x5E #xDE #x3E #xBE #x7E #xFE #x01 #x81
                #x41 #xC1 #x21 #xA1 #x61 #xE1 #x11 #x91 #x51 #xD1
                #x31 #xB1 #x71 #xF1 #x09 #x89 #x49 #xC9 #x29 #xA9
                #x69 #xE9 #x19 #x99 #x59 #xD9 #x39 #xB9 #x79 #xF9
                #x05 #x85 #x45 #xC5 #x25 #xA5 #x65 #xE5 #x15 #x95
                #x55 #xD5 #x35 #xB5 #x75 #xF5 #x0D #x8D #x4D #xCD
                #x2D #xAD #x6D #xED #x1D #x9D #x5D #xDD #x3D #xBD
                #x7D #xFD #x03 #x83 #x43 #xC3 #x23 #xA3 #x63 #xE3
                #x13 #x93 #x53 #xD3 #x33 #xB3 #x73 #xF3 #x0B #x8B
                #x4B #xCB #x2B #xAB #x6B #xEB #x1B #x9B #x5B #xDB
                #x3B #xBB #x7B #xFB #x07 #x87 #x47 #xC7 #x27 #xA7
                #x67 #xE7 #x17 #x97 #x57 #xD7 #x37 #xB7 #x77 #xF7
                #x0F #x8F #x4F #xCF #x2F #xAF #x6F #xEF #x1F #x9F
                #x5F #xDF #x3F #xBF #x7F #xFF)))

;; A fast means to reverse the bits of an octet
(defun octet-reverse (octet)
  (aref *octet-bit-reverse-table* octet))


;; Reverse all bits in a canonical integer up to the bit-max
;; This is kinda slow, but generic.
(defun integer-reverse (num num-bits)
  (let ((value 0))
    (loop :for i :below num-bits :do
      (setf (ldb (byte 1 (- (1- num-bits) i)) value)
            (ldb (byte 1 i) num)))
    value))
