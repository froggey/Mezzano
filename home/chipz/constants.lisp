(in-package :chipz)

(defmacro define-constant (name value)
  `(unless (boundp ',name)
     (defconstant ,name ,value)))


;;;; DEFLATE constants.

;;; block types
(define-constant +block-no-compress+ 0)
(define-constant +block-fixed-codes+ 1)
(define-constant +block-dynamic-codes+ 2)
(define-constant +block-invalid+ 3)

(define-constant +max-code-length+ 16)
(define-constant +max-codes+ 288)
(define-constant +max-n-code-lengths+ 19)
(define-constant +deflate-max-bits+ 15)

(define-constant +length-code-extra-bits+
  (coerce #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0)
          '(vector (unsigned-byte 16))))

(define-constant +length-code-base-lengths+
  (coerce #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27
            31 35 43 51 59 67 83 99 115 131 163 195 227 258)
          '(vector (unsigned-byte 16))))


;;;; BZIP constants.

(defconstant +bz-header-b+ #x42)
(defconstant +bz-header-z+ #x5a)
(defconstant +bz-header-h+ #x68)
(defconstant +bz-header-0+ #x30)
(defconstant +100k+ (expt 10 5))

(defconstant +mtfa-size+ 4096)
(defconstant +mtfl-size+ 16)
(defconstant +bz-max-alpha-size+ 258)
(defconstant +bz-max-code-len+ 23)
(defconstant +bz-runa+ 0)
(defconstant +bz-runb+ 1)
(defconstant +bz-n-groups+ 6)
(defconstant +bz-g-size+ 50)
(defconstant +bz-n-iters+ 4)
(defconstant +bz-max-selectors+ (+ 2 (/ (* 9 +100k+) +bz-g-size+)))


;;; miscellaneous

;;; for DECOMPRESS.
(defconstant +default-buffer-size+ 8192)

;;; CRC32
(declaim (type (simple-array (unsigned-byte 32) (256)) +crc32-table+ +bzip2-crc32-table+))
(define-constant +crc32-table+
  (coerce '(#x00000000 #x77073096 #xEE0E612C #x990951BA #x076DC419 #x706AF48F
            #xE963A535 #x9E6495A3 #x0EDB8832 #x79DCB8A4 #xE0D5E91E #x97D2D988
            #x09B64C2B #x7EB17CBD #xE7B82D07 #x90BF1D91 #x1DB71064 #x6AB020F2
            #xF3B97148 #x84BE41DE #x1ADAD47D #x6DDDE4EB #xF4D4B551 #x83D385C7
            #x136C9856 #x646BA8C0 #xFD62F97A #x8A65C9EC #x14015C4F #x63066CD9
            #xFA0F3D63 #x8D080DF5 #x3B6E20C8 #x4C69105E #xD56041E4 #xA2677172
            #x3C03E4D1 #x4B04D447 #xD20D85FD #xA50AB56B #x35B5A8FA #x42B2986C
            #xDBBBC9D6 #xACBCF940 #x32D86CE3 #x45DF5C75 #xDCD60DCF #xABD13D59
            #x26D930AC #x51DE003A #xC8D75180 #xBFD06116 #x21B4F4B5 #x56B3C423
            #xCFBA9599 #xB8BDA50F #x2802B89E #x5F058808 #xC60CD9B2 #xB10BE924
            #x2F6F7C87 #x58684C11 #xC1611DAB #xB6662D3D #x76DC4190 #x01DB7106
            #x98D220BC #xEFD5102A #x71B18589 #x06B6B51F #x9FBFE4A5 #xE8B8D433
            #x7807C9A2 #x0F00F934 #x9609A88E #xE10E9818 #x7F6A0DBB #x086D3D2D
            #x91646C97 #xE6635C01 #x6B6B51F4 #x1C6C6162 #x856530D8 #xF262004E
            #x6C0695ED #x1B01A57B #x8208F4C1 #xF50FC457 #x65B0D9C6 #x12B7E950
            #x8BBEB8EA #xFCB9887C #x62DD1DDF #x15DA2D49 #x8CD37CF3 #xFBD44C65
            #x4DB26158 #x3AB551CE #xA3BC0074 #xD4BB30E2 #x4ADFA541 #x3DD895D7
            #xA4D1C46D #xD3D6F4FB #x4369E96A #x346ED9FC #xAD678846 #xDA60B8D0
            #x44042D73 #x33031DE5 #xAA0A4C5F #xDD0D7CC9 #x5005713C #x270241AA
            #xBE0B1010 #xC90C2086 #x5768B525 #x206F85B3 #xB966D409 #xCE61E49F
            #x5EDEF90E #x29D9C998 #xB0D09822 #xC7D7A8B4 #x59B33D17 #x2EB40D81
            #xB7BD5C3B #xC0BA6CAD #xEDB88320 #x9ABFB3B6 #x03B6E20C #x74B1D29A
            #xEAD54739 #x9DD277AF #x04DB2615 #x73DC1683 #xE3630B12 #x94643B84
            #x0D6D6A3E #x7A6A5AA8 #xE40ECF0B #x9309FF9D #x0A00AE27 #x7D079EB1
            #xF00F9344 #x8708A3D2 #x1E01F268 #x6906C2FE #xF762575D #x806567CB
            #x196C3671 #x6E6B06E7 #xFED41B76 #x89D32BE0 #x10DA7A5A #x67DD4ACC
            #xF9B9DF6F #x8EBEEFF9 #x17B7BE43 #x60B08ED5 #xD6D6A3E8 #xA1D1937E
            #x38D8C2C4 #x4FDFF252 #xD1BB67F1 #xA6BC5767 #x3FB506DD #x48B2364B
            #xD80D2BDA #xAF0A1B4C #x36034AF6 #x41047A60 #xDF60EFC3 #xA867DF55
            #x316E8EEF #x4669BE79 #xCB61B38C #xBC66831A #x256FD2A0 #x5268E236
            #xCC0C7795 #xBB0B4703 #x220216B9 #x5505262F #xC5BA3BBE #xB2BD0B28
            #x2BB45A92 #x5CB36A04 #xC2D7FFA7 #xB5D0CF31 #x2CD99E8B #x5BDEAE1D
            #x9B64C2B0 #xEC63F226 #x756AA39C #x026D930A #x9C0906A9 #xEB0E363F
            #x72076785 #x05005713 #x95BF4A82 #xE2B87A14 #x7BB12BAE #x0CB61B38
            #x92D28E9B #xE5D5BE0D #x7CDCEFB7 #x0BDBDF21 #x86D3D2D4 #xF1D4E242
            #x68DDB3F8 #x1FDA836E #x81BE16CD #xF6B9265B #x6FB077E1 #x18B74777
            #x88085AE6 #xFF0F6A70 #x66063BCA #x11010B5C #x8F659EFF #xF862AE69
            #x616BFFD3 #x166CCF45 #xA00AE278 #xD70DD2EE #x4E048354 #x3903B3C2
            #xA7672661 #xD06016F7 #x4969474D #x3E6E77DB #xAED16A4A #xD9D65ADC
            #x40DF0B66 #x37D83BF0 #xA9BCAE53 #xDEBB9EC5 #x47B2CF7F #x30B5FFE9
            #xBDBDF21C #xCABAC28A #x53B39330 #x24B4A3A6 #xBAD03605 #xCDD70693
            #x54DE5729 #x23D967BF #xB3667A2E #xC4614AB8 #x5D681B02 #x2A6F2B94
            #xB40BBE37 #xC30C8EA1 #x5A05DF1B #x2D02EF8D)
          '(vector (unsigned-byte 32))))

(define-constant +bzip2-crc32-table+
    (coerce '(#x00000000 #x04c11db7 #x09823b6e #x0d4326d9
              #x130476dc #x17c56b6b #x1a864db2 #x1e475005
              #x2608edb8 #x22c9f00f #x2f8ad6d6 #x2b4bcb61
              #x350c9b64 #x31cd86d3 #x3c8ea00a #x384fbdbd
              #x4c11db70 #x48d0c6c7 #x4593e01e #x4152fda9
              #x5f15adac #x5bd4b01b #x569796c2 #x52568b75
              #x6a1936c8 #x6ed82b7f #x639b0da6 #x675a1011
              #x791d4014 #x7ddc5da3 #x709f7b7a #x745e66cd
              #x9823b6e0 #x9ce2ab57 #x91a18d8e #x95609039
              #x8b27c03c #x8fe6dd8b #x82a5fb52 #x8664e6e5
              #xbe2b5b58 #xbaea46ef #xb7a96036 #xb3687d81
              #xad2f2d84 #xa9ee3033 #xa4ad16ea #xa06c0b5d
              #xd4326d90 #xd0f37027 #xddb056fe #xd9714b49
              #xc7361b4c #xc3f706fb #xceb42022 #xca753d95
              #xf23a8028 #xf6fb9d9f #xfbb8bb46 #xff79a6f1
              #xe13ef6f4 #xe5ffeb43 #xe8bccd9a #xec7dd02d
              #x34867077 #x30476dc0 #x3d044b19 #x39c556ae
              #x278206ab #x23431b1c #x2e003dc5 #x2ac12072
              #x128e9dcf #x164f8078 #x1b0ca6a1 #x1fcdbb16
              #x018aeb13 #x054bf6a4 #x0808d07d #x0cc9cdca
              #x7897ab07 #x7c56b6b0 #x71159069 #x75d48dde
              #x6b93dddb #x6f52c06c #x6211e6b5 #x66d0fb02
              #x5e9f46bf #x5a5e5b08 #x571d7dd1 #x53dc6066
              #x4d9b3063 #x495a2dd4 #x44190b0d #x40d816ba
              #xaca5c697 #xa864db20 #xa527fdf9 #xa1e6e04e
              #xbfa1b04b #xbb60adfc #xb6238b25 #xb2e29692
              #x8aad2b2f #x8e6c3698 #x832f1041 #x87ee0df6
              #x99a95df3 #x9d684044 #x902b669d #x94ea7b2a
              #xe0b41de7 #xe4750050 #xe9362689 #xedf73b3e
              #xf3b06b3b #xf771768c #xfa325055 #xfef34de2
              #xc6bcf05f #xc27dede8 #xcf3ecb31 #xcbffd686
              #xd5b88683 #xd1799b34 #xdc3abded #xd8fba05a
              #x690ce0ee #x6dcdfd59 #x608edb80 #x644fc637
              #x7a089632 #x7ec98b85 #x738aad5c #x774bb0eb
              #x4f040d56 #x4bc510e1 #x46863638 #x42472b8f
              #x5c007b8a #x58c1663d #x558240e4 #x51435d53
              #x251d3b9e #x21dc2629 #x2c9f00f0 #x285e1d47
              #x36194d42 #x32d850f5 #x3f9b762c #x3b5a6b9b
              #x0315d626 #x07d4cb91 #x0a97ed48 #x0e56f0ff
              #x1011a0fa #x14d0bd4d #x19939b94 #x1d528623
              #xf12f560e #xf5ee4bb9 #xf8ad6d60 #xfc6c70d7
              #xe22b20d2 #xe6ea3d65 #xeba91bbc #xef68060b
              #xd727bbb6 #xd3e6a601 #xdea580d8 #xda649d6f
              #xc423cd6a #xc0e2d0dd #xcda1f604 #xc960ebb3
              #xbd3e8d7e #xb9ff90c9 #xb4bcb610 #xb07daba7
              #xae3afba2 #xaafbe615 #xa7b8c0cc #xa379dd7b
              #x9b3660c6 #x9ff77d71 #x92b45ba8 #x9675461f
              #x8832161a #x8cf30bad #x81b02d74 #x857130c3
              #x5d8a9099 #x594b8d2e #x5408abf7 #x50c9b640
              #x4e8ee645 #x4a4ffbf2 #x470cdd2b #x43cdc09c
              #x7b827d21 #x7f436096 #x7200464f #x76c15bf8
              #x68860bfd #x6c47164a #x61043093 #x65c52d24
              #x119b4be9 #x155a565e #x18197087 #x1cd86d30
              #x029f3d35 #x065e2082 #x0b1d065b #x0fdc1bec
              #x3793a651 #x3352bbe6 #x3e119d3f #x3ad08088
              #x2497d08d #x2056cd3a #x2d15ebe3 #x29d4f654
              #xc5a92679 #xc1683bce #xcc2b1d17 #xc8ea00a0
              #xd6ad50a5 #xd26c4d12 #xdf2f6bcb #xdbee767c
              #xe3a1cbc1 #xe760d676 #xea23f0af #xeee2ed18
              #xf0a5bd1d #xf464a0aa #xf9278673 #xfde69bc4
              #x89b8fd09 #x8d79e0be #x803ac667 #x84fbdbd0
              #x9abc8bd5 #x9e7d9662 #x933eb0bb #x97ffad0c
              #xafb010b1 #xab710d06 #xa6322bdf #xa2f33668
              #xbcb4666d #xb8757bda #xb5365d03 #xb1f740b4)
            '(vector (unsigned-byte 32))))

;;; Adler32, smallest prime < 65536
(defconstant adler32-modulo 65521)
