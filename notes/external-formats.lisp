(in-package #:sys.int)

(defmacro define-encoding (name (&rest all-keys &key alias replacement inherit encoder decoder) &body mappings)
  `(%define-encoding ',name ',mappings ,@all-keys))

(defvar *character-encodings* (make-hash-table))
(defvar *default-external-format* :utf-8)

(defun %define-encoding (name mappings &key alias replacement inherit encoder decoder)
  (when (not (listp alias))
    (setf alias (list alias)))
  (let ((encoding (make-encoding :name name
                                 :mappings mappings
                                 :alias alias
                                 :replacement (or replacement (code-char #xFFFD))
                                 :inherit inherit
                                 :encoder encoder
                                 :decoder decoder)))
    (setf (gethash name *character-encodings*) encoding)
    (dolist (a alias)
      (setf (gethash a *character-encodings*) encoding)))
  name)

(defun get-encoding (name &optional (errorp t))
  (when (eql name :default)
    (setf name *default-external-format*))
  (or (gethash name *character-encodings*)
      (when errorp
        (error "No such encoding ~S." name))))

(defun encode-character (encoding character stream)
  (let ((mapping (encoding-mappings encoding))
        (replacement (encoding-replacement encoding)))
    (when (or (not (zerop (char-bits character)))
      (setf character replacement))
        (assert  (character))
    (assert (and (<= 0 code #x1FFFFF)
                 (not (<= #xD800 code #xDFFF)))
            (character))
    (dolist (i mapping (let ((encoder (encoding-encoder encoding)))
                         (adsfasdf)))
      (let ((

(define-encoding :us-ascii (:alias :ascii :replacement #\?)
  (#x00 #x0000 128))

(define-encoding :iso-8859-1 (:alias (:latin1 :CP819) :replacement #\?)
  (#x00 #x0000 256))

;;; This encoding treats the control characters as control characters.
(define-encoding :cp-437-control (:inherit :us-ascii)
  (#x80 #x00C7)
  (#x81 #x00FC)
  (#x82 #x00E9)
  (#x83 #x00E2)
  (#x84 #x00E4)
  (#x85 #x00E0)
  (#x86 #x00E5)
  (#x87 #x00E7)
  (#x88 #x00EA)
  (#x89 #x00EB)
  (#x8A #x00E8)
  (#x8B #x00EF)
  (#x8C #x00EE)
  (#x8D #x00EC)
  (#x8E #x00C4)
  (#x8F #x00C5)
  (#x90 #x00C9)
  (#x91 #x00E6)
  (#x92 #x00C6)
  (#x93 #x00F4)
  (#x94 #x00F6)
  (#x95 #x00F2)
  (#x96 #x00FB)
  (#x97 #x00F9)
  (#x98 #x00FF)
  (#x99 #x00D6)
  (#x9A #x00DC)
  (#x9B #x00A2)
  (#x9C #x00A3)
  (#x9D #x00A5)
  (#x9E #x20A7)
  (#x9F #x0192)
  (#xA0 #x00E1)
  (#xA1 #x00ED)
  (#xA2 #x00F3)
  (#xA3 #x00FA)
  (#xA4 #x00F1)
  (#xA5 #x00D1)
  (#xA6 #x00AA)
  (#xA7 #x00BA)
  (#xA8 #x00BF)
  (#xA9 #x2310)
  (#xAA #x00AC)
  (#xAB #x00BD)
  (#xAC #x00BC)
  (#xAD #x00A1)
  (#xAE #x00AB)
  (#xAF #x00BB)
  (#xB0 #x2591)
  (#xB1 #x2592)
  (#xB2 #x2593)
  (#xB3 #x2502)
  (#xB4 #x2524)
  (#xB5 #x2561)
  (#xB6 #x2562)
  (#xB7 #x2556)
  (#xB8 #x2555)
  (#xB9 #x2563)
  (#xBA #x2551)
  (#xBB #x2557)
  (#xBC #x255D)
  (#xBD #x255C)
  (#xBE #x255B)
  (#xBF #x2510)
  (#xC0 #x2514)
  (#xC1 #x2534)
  (#xC2 #x252C)
  (#xC3 #x251C)
  (#xC4 #x2500)
  (#xC5 #x253C)
  (#xC6 #x255E)
  (#xC7 #x255F)
  (#xC8 #x255A)
  (#xC9 #x2554)
  (#xCA #x2569)
  (#xCB #x2566)
  (#xCC #x2560)
  (#xCD #x2550)
  (#xCE #x256C)
  (#xCF #x2567)
  (#xD0 #x2568)
  (#xD1 #x2564)
  (#xD2 #x2565)
  (#xD3 #x2559)
  (#xD4 #x2558)
  (#xD5 #x2552)
  (#xD6 #x2553)
  (#xD7 #x256B)
  (#xD8 #x256A)
  (#xD9 #x2518)
  (#xDA #x250C)
  (#xDB #x2588)
  (#xDC #x2584)
  (#xDD #x258C)
  (#xDE #x2590)
  (#xDF #x2580)
  (#xE0 #x03B1)
  (#xE1 #x00DF)
  (#xE2 #x0393)
  (#xE3 #x03C0)
  (#xE4 #x03A3)
  (#xE5 #x03C3)
  (#xE6 #x00B5)
  (#xE7 #x03C4)
  (#xE8 #x03A6)
  (#xE9 #x0398)
  (#xEA #x03A9)
  (#xEB #x03B4)
  (#xEC #x221E)
  (#xED #x03C6)
  (#xEE #x03B5)
  (#xEF #x2229)
  (#xF0 #x2261)
  (#xF1 #x00B1)
  (#xF2 #x2265)
  (#xF3 #x2264)
  (#xF4 #x2320)
  (#xF5 #x2321)
  (#xF6 #x00F7)
  (#xF7 #x2248)
  (#xF8 #x00B0)
  (#xF9 #x2219)
  (#xFA #x00B7)
  (#xFB #x221A)
  (#xFC #x207F)
  (#xFD #x00B2)
  (#xFE #x25A0)
  (#xFF #x00A0))

;;; This one treats control characters as graphical characters.
(define-encoding :cp-437 (:inherit :cp-437-control)
  (#x01 #x263A)
  (#x02 #x263B)
  (#x03 #x2665)
  (#x04 #x2666)
  (#x05 #x2663)
  (#x06 #x2660)
  (#x07 #x2022)
  (#x08 #x25D8)
  (#x09 #x25CB)
  (#x0A #x25D9)
  (#x0B #x2642)
  (#x0C #x2640)
  (#x0D #x266A)
  (#x0E #x266B)
  (#x0F #x263C)
  (#x10 #x25BA)
  (#x11 #x25C4)
  (#x12 #x2195)
  (#x13 #x203C)
  (#x14 #x00B6)
  (#x15 #x00A7)
  (#x16 #x25AC)
  (#x17 #x21A8)
  (#x18 #x2191)
  (#x19 #x2193)
  (#x1A #x2192)
  (#x1B #x2190)
  (#x1C #x221F)
  (#x1D #x2194)
  (#x1E #x25B2)
  (#x1F #x25BC)
  (#x7F #x2302)
  (:suppress #x0001 31)
  (:suppress #x007F))

(define-encoding :symbolics (:replacement #\?)
  (#x00 #x00B7) ;MIDDLE DOT
  (#x01 #x2193) ;DOWNWARDS ARROW
  (#x02 #x03B1) ;GREEK SMALL LETTER ALPHA
  (#x03 #x03B2) ;GREEK SMALL LETTER BETA
  (#x04 #x2227) ;LOGICAL AND
  (#x05 #x00AC) ;NOT SIGN
  (#x06 #x03B5) ;GREEK SMALL LETTER EPSILON
  (#x07 #x03C0) ;GREEK SMALL LETTER PI
  (#x08 #x03BB) ;GREEK SMALL LETTER LAMBDA
  (#x09 #x03B3) ;GREEK SMALL LETTER GAMMA
  (#x0A #x03B4) ;GREEK SMALL LETTER DELTA
  (#x0B #x2191) ;UPWARDS ARROW
  (#x0C #x00B1) ;PLUS-MINUS SIGN
  (#x0D #x2295) ;CIRCLED PLUS
  (#x0E #x221E) ;INFINITY
  (#x0F #x2202) ;PARTIAL DIFFERENTIAL
  (#x10 #x2282) ;SUBSET OF
  (#x11 #x2283) ;SUPERSET OF
  (#x12 #x2229) ;INTERSECTION
  (#x13 #x222A) ;UNION
  (#x14 #x2200) ;FOR ALL
  (#x15 #x2203) ;THERE EXISTS
  (#x16 #x2297) ;CIRCLED TIMES
  (#x17 #x2194) ;LEFT RIGHT ARROW
  (#x18 #x2190) ;LEFTWARDS ARROW
  (#x19 #x2192) ;RIGHTWARDS ARROW
  (#x1A #x2260) ;NOT EQUAL TO
  (#x1B #x25CA) ;LOZENGE
  (#x1C #x2264) ;LESS-THAN OR EQUAL TO
  (#x1D #x2265) ;GREATER-THAN OR EQUAL TO
  (#x1E #x2261) ;IDENTICAL TO
  (#x1F #x2228) ;LOGICAL OR
  (#x20 #x0020 95)
  (#x7F #x222B) ;INTEGRAL
  (#x80 #x0000) ; Null
  (#x87 #x007F) ; Rubout
  (#x88 #x0008) ; Back-Space
  (#x89 #x0009) ; Tab
  (#x8D #x000A) ; Return/Newline
  (#x9F #x001B)) ; Escape
;; Missing special characters (octal):
;; 201 Suspend
;; 202 Clear-Input
;; 203 Reserved
;; 204 Function
;; 205 Macro
;; 206 Help
;; 212 Line
;; 213 Refresh
;; 214 Page
;; 216 Quote
;; 217 Hold-Output
;; 220 Stop-Output
;; 221 Abort
;; 222 Resume
;; 223 Status
;; 224 End
;; 225 Square
;; 226 Circle
;; 227 Triangle
;; 230 Roman-IV
;; 231 Hand-Up
;; 232 Scroll
;; 233 Hand-Left
;; 234 Hand-Right
;; 235 Select
;; 236 Network
;; 240 Complete
;; 241 Symbol-Help

(defun encode-utf-8 (character stream)
  (let ((code (char-code character)))
    (assert (zerop (char-bits character)) (character))
    (assert (and (<= 0 code #x1FFFFF)
                 (not (<= #xD800 code #xDFFF)))
            (character))
    (cond ((<= code #x7F)
           (write-byte code stream)
           t)
          ((<= #x80 code #x7FF)
           (write-byte (logior (ash (logand code #x7C0) -6) #xC0) stream)
           (write-byte (logior (logand code #x3F) #x80) stream)
           t)
          ((or (<= #x800 code #xD7FF)
               (<= #xE000 code #xFFFF))
           (write-byte (logior (ash (logand code #xF000) -12) #xE0) stream)
           (write-byte (logior (ash (logand code #xFC0) -6) #x80) stream)
           (write-byte (logior (logand code #x3F) #x80) stream)
           t)
          ((<= #x10000 code #x10FFFF)
           (write-byte (logior (ash (logand code #x1C0000) -18) #xE0) stream)
           (write-byte (logior (ash (logand code #x3F000) -12) #x80) stream)
           (write-byte (logior (ash (logand code #xFC0) -6) #x80) stream)
           (write-byte (logior (logand code #x3F) #x80) stream)
           t)
          (t nil))))

(defun utf-8-sequence-length (byte)
  (cond
    ((eql (logand byte #x80) #x00)
     (values 1 byte))
    ((eql (logand byte #xE0) #xC0)
     (values 2 (logand byte #x1F)))
    ((eql (logand byte #xF0) #xE0)
     (values 3 (logand byte #x0F)))
    ((eql (logand byte #xF8) #xF0)
     (values 4 (logand byte #x07)))
    (t (values nil nil))))

(defun decode-utf-8 (stream)
  (multiple-value-bind (length value)
      (utf-8-sequence-length (read-byte stream))
    (when length
      ;; Read remaining bytes. They must all be continuation bytes.
      (dotimes (i (1- length))
        (let ((byte (read-byte stream)))
          (unless (eql (logand byte #xC0) #x80)
            (error "Invalid UTF-8 continuation byte ~S." byte))
          (setf value (logior (ash value 6) (logand byte #x3F)))))
      ;; FIXME: Detect overlong forms.
      (when (and (<= value #x1FFFFF)
                 (not (<= #xD800 code #xDFFF)))
        (code-char value)))))

(define-encoding :utf-8 (:alias :utf8 :encoder 'encode-utf-8 :decoder 'decode-utf-8)
  ;; The first 128 codepoints have a 1:1 representation.
  (#x00 #x0000 128))
