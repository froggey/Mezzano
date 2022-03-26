;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

(defvar *default-encoding* :win-ansi-encoding)

(defvar *encodings* (make-hash-table :test #'equal))

(defun get-encoding (encoding-designator)
  (gethash encoding-designator *encodings*))

;;; Abstract encoding

(defclass encoding ()
 ((name :accessor name :initform nil :initarg :name)
  (keyword-name :accessor keyword-name :initform nil :initarg :keyword-name)
  (standard-encoding :accessor standard-encoding :initform nil :initarg :standard-encoding)))

(defmethod initialize-instance :after ((encoding encoding) &key &allow-other-keys)
  (unless (name encoding)
    (error "You must provide a name when you create an encoding."))
  (unless (keyword-name encoding)
    (setf (keyword-name encoding)(intern (string-upcase (name encoding)) :keyword)))
  (setf (gethash encoding *encodings*) encoding
	(gethash (name encoding) *encodings*) encoding
	(gethash (keyword-name encoding) *encodings*) encoding))

(defmethod print-object ((self encoding) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~a" (name self))))

;;; Single-byte encoding

(defclass single-byte-encoding (encoding)
 ((char-names :accessor char-names :initform nil :initarg :char-names)
  (char-codes :accessor char-codes :initform (make-hash-table :test #'equal))))

(defmethod initialize-instance :after ((encoding single-byte-encoding) &key &allow-other-keys)
  (unless (char-names encoding)
    (error "You must provide code-to-char-name array (256) when you create a single-byte encoding."))
  (loop with char-codes = (char-codes encoding)
	for char-name across (char-names encoding)
	for code from 0
	when char-name do (setf (gethash char-name char-codes) code)))

(defmethod make-dictionary ((encoding encoding) &key &allow-other-keys)
  (make-instance 'dictionary
                 :dict-values
                 `(("/Type" . "/Encoding")
                   ("/Differences" . ,(compute-encoding-differences encoding nil)))))

;; Charset is an atom or "extended charset" - an alist storing (char . code) pairs
(defgeneric charset (encoding))

(defmethod charset ((encoding single-byte-encoding))
  (declare (ignorable encoding))
  *char-single-byte-codes*)

(defun char-external-code (char charset)
  (cond ((null charset)
         (char-code char))
        ((atom charset)
         #+lispworks (ef:char-external-code char charset)
         #+allegro   (aref (excl:string-to-octets (coerce `(,char) 'string)
                                                  :external-format charset)
                           0)
         #+sbcl      (aref (sb-ext:string-to-octets (coerce `(,char) 'string)
                                                    :external-format charset)
                           0)
         #-(or lispworks allegro sbcl)  (char-code char))
        ((cdr (assoc char charset)))		; map to single-byte if possible
        (t (char-code char))))

;;; Built-in encoding instances

(defparameter *standard-encoding*
  (make-instance 'single-byte-encoding
                 :name "StandardEncoding"  :keyword-name :standard-encoding
		 :standard-encoding t :char-names #(
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil "space"
"exclam" "quotedbl" "numbersign" "dollar" "percent" "ampersand"
"quoteright" "parenleft" "parenright" "asterisk" "plus" "comma"
"hyphen" "period" "slash" "zero" "one" "two" "three" "four" "five"
"six" "seven" "eight" "nine" "colon" "semicolon" "less" "equal"
"greater" "question" "at" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"
"L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
"bracketleft" "backslash" "bracketright" "asciicircum" "underscore"
"quoteleft" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
"p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "braceleft" "bar"
"braceright" "asciitilde" nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil "exclamdown" "cent" "sterling" "fraction" "yen"
"florin" "section" "currency" "quotesingle" "quotedblleft"
"guillemotleft" "guilsinglleft" "guilsinglright" "fi" "fl" nil "endash"
"dagger" "daggerdbl" "periodcentered" nil "paragraph" "bullet"
"quotesinglbase" "quotedblbase" "quotedblright" "guillemotright"
"ellipsis" "perthousand" nil "questiondown" nil "grave" "acute"
"circumflex" "tilde" "macron" "breve" "dotaccent" "dieresis" nil "ring"
"cedilla" nil "hungarumlaut" "ogonek" "caron" "emdash" nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil "AE" nil "ordfeminine"
nil nil nil nil "Lslash" "Oslash" "OE" "ordmasculine" nil nil nil nil
nil "ae" nil nil nil "dotlessi" nil nil "lslash" "oslash" "oe"
"germandbls" nil nil nil nil )))

(defparameter *mac-roman-encoding*
  (make-instance 'single-byte-encoding
                 :name "MacRomanEncoding"  :keyword-name :mac-roman-encoding
		 :standard-encoding t  :char-names #(
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil "space"
"exclam" "quotedbl" "numbersign" "dollar" "percent" "ampersand"
"quotesingle" "parenleft" "parenright" "asterisk" "plus" "comma"
"hyphen" "period" "slash" "zero" "one" "two" "three" "four" "five"
"six" "seven" "eight" "nine" "colon" "semicolon" "less" "equal"
"greater" "question" "at" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"
"L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
"bracketleft" "backslash" "bracketright" "asciicircum" "underscore"
"grave" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
"q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "braceleft" "bar" "braceright"
"asciitilde" nil "Adieresis" "Aring" "Ccedilla" "Eacute" "Ntilde"
"Odieresis" "Udieresis" "aacute" "agrave" "acircumflex" "adieresis"
"atilde" "aring" "ccedilla" "eacute" "egrave" "ecircumflex" "edieresis"
"iacute" "igrave" "icircumflex" "idieresis" "ntilde" "oacute" "ograve"
"ocircumflex" "odieresis" "otilde" "uacute" "ugrave" "ucircumflex"
"udieresis" "dagger" "degree" "cent" "sterling" "section" "bullet"
"paragraph" "germandbls" "registered" "copyright" "trademark" "acute"
"dieresis" nil "AE" "Oslash" nil "plusminus" nil nil "yen" "mu" nil nil
nil nil nil "ordfeminine" "ordmasculine" nil "ae" "oslash"
"questiondown" "exclamdown" "logicalnot" nil "florin" nil nil
"guillemotleft" "guillemotright" "ellipsis" "space" "Agrave" "Atilde"
"Otilde" "OE" "oe" "endash" "emdash" "quotedblleft" "quotedblright"
"quoteleft" "quoteright" "divide" nil "ydieresis" "Ydieresis"
"fraction" "currency" "guilsinglleft" "guilsinglright" "fi" "fl"
"daggerdbl" "periodcentered" "quotesinglbase" "quotedblbase"
"perthousand" "Acircumflex" "Ecircumflex" "Aacute" "Edieresis" "Egrave"
"Iacute" "Icircumflex" "Idieresis" "Igrave" "Oacute" "Ocircumflex" nil
"Ograve" "Uacute" "Ucircumflex" "Ugrave" "dotlessi" "circumflex"
"tilde" "macron" "breve" "dotaccent" "ring" "cedilla" "hungarumlaut"
"ogonek" "caron" )))

(defparameter *win-ansi-encoding*
  (make-instance 'single-byte-encoding
                 :name "WinAnsiEncoding"  :keyword-name :win-ansi-encoding
		 :standard-encoding t  :char-names #(
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil "space"
"exclam" "quotedbl" "numbersign" "dollar" "percent" "ampersand"
"quotesingle" "parenleft" "parenright" "asterisk" "plus" "comma"
"hyphen" "period" "slash" "zero" "one" "two" "three" "four" "five"
"six" "seven" "eight" "nine" "colon" "semicolon" "less" "equal"
"greater" "question" "at" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"
"L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
"bracketleft" "backslash" "bracketright" "asciicircum" "underscore"
"grave" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
"q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "braceleft" "bar" "braceright"
"asciitilde" "bullet" "Euro" "bullet" "quotesinglbase" "florin"
"quotedblbase" "ellipsis" "dagger" "daggerdbl" "circumflex"
"perthousand" "Scaron" "guilsinglleft" "OE" "bullet" "Zcaron" "bullet"
"bullet" "quoteleft" "quoteright" "quotedblleft" "quotedblright"
"bullet" "endash" "emdash" "tilde" "trademark" "scaron"
"guilsinglright" "oe" "bullet" "zcaron" "Ydieresis" "space"
"exclamdown" "cent" "sterling" "currency" "yen" "brokenbar" "section"
"dieresis" "copyright" "ordfeminine" "guillemotleft" "logicalnot"
"hyphen" "registered" "macron" "degree" "plusminus" "twosuperior"
"threesuperior" "acute" "mu" "paragraph" "periodcentered" "cedilla"
"onesuperior" "ordmasculine" "guillemotright" "onequarter" "onehalf"
"threequarters" "questiondown" "Agrave" "Aacute" "Acircumflex" "Atilde"
"Adieresis" "Aring" "AE" "Ccedilla" "Egrave" "Eacute" "Ecircumflex"
"Edieresis" "Igrave" "Iacute" "Icircumflex" "Idieresis" "Eth" "Ntilde"
"Ograve" "Oacute" "Ocircumflex" "Otilde" "Odieresis" "multiply"
"Oslash" "Ugrave" "Uacute" "Ucircumflex" "Udieresis" "Yacute" "Thorn"
"germandbls" "agrave" "aacute" "acircumflex" "atilde" "adieresis"
"aring" "ae" "ccedilla" "egrave" "eacute" "ecircumflex" "edieresis"
"igrave" "iacute" "icircumflex" "idieresis" "eth" "ntilde" "ograve"
"oacute" "ocircumflex" "otilde" "odieresis" "divide" "oslash" "ugrave"
"uacute" "ucircumflex" "udieresis" "yacute" "thorn" "ydieresis" )))

(defparameter *pdf-doc-encoding*
  (make-instance 'single-byte-encoding
                 :name "PDFDocEncoding"  :keyword-name :pdf-doc-encoding
		 :standard-encoding t  :char-names #(
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil "breve" "caron" "circumflex" "dotaccent"
"hungarumlaut" "ogonek" "ring" "tilde" "space" "exclam" "quotedbl"
"numbersign" "dollar" "percent" "ampersand" "quotesingle" "parenleft"
"parenright" "asterisk" "plus" "comma" "hyphen" "period" "slash" "zero"
"one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "colon"
"semicolon" "less" "equal" "greater" "question" "at" "A" "B" "C" "D"
"E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V"
"W" "X" "Y" "Z" "bracketleft" "backslash" "bracketright" "asciicircum"
"underscore" "grave" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l"
"m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "braceleft"
"bar" "braceright" "asciitilde" nil "bullet" "dagger" "daggerdbl"
"ellipsis" "emdash" "endash" "florin" "fraction" "guilsinglleft"
"guilsinglright" "minus" "perthousand" "quotedblbase" "quotedblleft"
"quotedblright" "quoteleft" "quoteright" "quotesinglbase" "trademark"
"fi" "fl" "Lslash" "OE" "Scaron" "Ydieresis" "Zcaron" "dotlessi"
"lslash" "oe" "scaron" "zcaron" nil "Euro" "exclamdown" "cent"
"sterling" "currency" "yen" "brokenbar" "section" "dieresis"
"copyright" "ordfeminine" "guillemotleft" "logicalnot" nil "registered"
"macron" "degree" "plusminus" "twosuperior" "threesuperior" "acute"
"mu" "paragraph" "periodcentered" "cedilla" "onesuperior"
"ordmasculine" "guillemotright" "onequarter" "onehalf" "threequarters"
"questiondown" "Agrave" "Aacute" "Acircumflex" "Atilde" "Adieresis"
"Aring" "AE" "Ccedilla" "Egrave" "Eacute" "Ecircumflex" "Edieresis"
"Igrave" "Iacute" "Icircumflex" "Idieresis" "Eth" "Ntilde" "Ograve"
"Oacute" "Ocircumflex" "Otilde" "Odieresis" "multiply" "Oslash"
"Ugrave" "Uacute" "Ucircumflex" "Udieresis" "Yacute" "Thorn"
"germandbls" "agrave" "aacute" "acircumflex" "atilde" "adieresis"
"aring" "ae" "ccedilla" "egrave" "eacute" "ecircumflex" "edieresis"
"igrave" "iacute" "icircumflex" "idieresis" "eth" "ntilde" "ograve"
"oacute" "ocircumflex" "otilde" "odieresis" "divide" "oslash" "ugrave"
"uacute" "ucircumflex" "udieresis" "yacute" "thorn" "ydieresis" )))

(defparameter *mac-expert-encoding*
  (make-instance 'single-byte-encoding
                 :name "MacExpertEncoding"  :keyword-name :mac-expert-encoding
		 :standard-encoding t  :char-names #(
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil "space"
"exclamsmall" "Hungarumlautsmall" "centoldstyle" "dollaroldstyle"
"dollarsuperior" "ampersandsmall" "Acutesmall" "parenleftsuperior"
"parenrightsuperior" "twodotenleader" "onedotenleader" "comma" "hyphen"
"period" "fraction" "zerooldstyle" "oneoldstyle" "twooldstyle"
"threeoldstyle" "fouroldstyle" "fiveoldstyle" "sixoldstyle"
"sevenoldstyle" "eightoldstyle" "nineoldstyle" "colon" "semicolon" nil
"threequartersemdash" nil "questionsmall" nil nil nil nil "Ethsmall"
nil nil "onequarter" "onehalf" "threequarters" "oneeighth"
"threeeighths" "fiveeighths" "seveneighths" "onethird" "twothirds" nil
nil nil nil nil nil "ff" "fi" "fl" "ffi" "ffl" "parenleftinferior" nil
"parenrightinferior" "Circumflexsmall" "hypheninferior" "Gravesmall"
"Asmall" "Bsmall" "Csmall" "Dsmall" "Esmall" "Fsmall" "Gsmall" "Hsmall"
"Ismall" "Jsmall" "Ksmall" "Lsmall" "Msmall" "Nsmall" "Osmall" "Psmall"
"Qsmall" "Rsmall" "Ssmall" "Tsmall" "Usmall" "Vsmall" "Wsmall" "Xsmall"
"Ysmall" "Zsmall" "colonmonetary" "onefitted" "rupiah" "Tildesmall" nil
nil "asuperior" "centsuperior" nil nil nil nil "Aacutesmall"
"Agravesmall" "Acircumflexsmall" "Adieresissmall" "Atildesmall"
"Aringsmall" "Ccedillasmall" "Eacutesmall" "Egravesmall"
"Ecircumflexsmall" "Edieresissmall" "Iacutesmall" "Igravesmall"
"Icircumflexsmall" "Idieresissmall" "Ntildesmall" "Oacutesmall"
"Ogravesmall" "Ocircumflexsmall" "Odieresissmall" "Otildesmall"
"Uacutesmall" "Ugravesmall" "Ucircumflexsmall" "Udieresissmall" nil
"eightsuperior" "fourinferior" "threeinferior" "sixinferior"
"eightinferior" "seveninferior" "Scaronsmall" nil "centinferior"
"twoinferior" nil "Dieresissmall" nil "Caronsmall" "osuperior"
"fiveinferior" nil "commainferior" "periodinferior" "Yacutesmall" nil
"dollarinferior" nil nil "Thornsmall" nil "nineinferior" "zeroinferior"
"Zcaronsmall" "AEsmall" "Oslashsmall" "questiondownsmall" "oneinferior"
"Lslashsmall" nil nil nil nil nil nil "Cedillasmall" nil nil nil nil
nil "OEsmall" "figuredash" "hyphensuperior" nil nil nil nil
"exclamdownsmall" nil "Ydieresissmall" nil "onesuperior" "twosuperior"
"threesuperior" "foursuperior" "fivesuperior" "sixsuperior"
"sevensuperior" "ninesuperior" "zerosuperior" nil "esuperior"
"rsuperior" "tsuperior" nil nil "isuperior" "ssuperior" "dsuperior" nil
nil nil nil nil "lsuperior" "Ogoneksmall" "Brevesmall" "Macronsmall"
"bsuperior" "nsuperior" "msuperior" "commasuperior" "periodsuperior"
"Dotaccentsmall" "Ringsmall" nil nil nil nil )))

(defparameter *symbol-encoding*
  (make-instance 'single-byte-encoding
                 :name "SymbolEncoding"  :keyword-name :symbol-encoding
		 :standard-encoding t  :char-names #(
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil "space"
"exclam" "universal" "numbersign" "existential" "percent" "ampersand"
"suchthat" "parenleft" "parenright" "asteriskmath" "plus" "comma"
"minus" "period" "slash" "zero" "one" "two" "three" "four" "five" "six"
"seven" "eight" "nine" "colon" "semicolon" "less" "equal" "greater"
"question" "congruent" "Alpha" "Beta" "Chi" "Delta" "Epsilon" "Phi"
"Gamma" "Eta" "Iota" "theta1" "Kappa" "Lambda" "Mu" "Nu" "Omicron" "Pi"
"Theta" "Rho" "Sigma" "Tau" "Upsilon" "sigma1" "Omega" "Xi" "Psi"
"Zeta" "bracketleft" "therefore" "bracketright" "perpendicular"
"underscore" "radicalex" "alpha" "beta" "chi" "delta" "epsilon" "phi"
"gamma" "eta" "iota" "phi1" "kappa" "lambda" "mu" "nu" "omicron" "pi"
"theta" "rho" "sigma" "tau" "upsilon" "omega1" "omega" "xi" "psi"
"zeta" "braceleft" "bar" "braceright" "similar" nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil "Euro" "Upsilon1" "minute"
"lessequal" "fraction" "infinity" "florin" "club" "diamond" "heart"
"spade" "arrowboth" "arrowleft" "arrowup" "arrowright" "arrowdown"
"degree" "plusminus" "second" "greaterequal" "multiply" "proportional"
"partialdiff" "bullet" "divide" "notequal" "equivalence" "approxequal"
"ellipsis" "arrowvertex" "arrowhorizex" "carriagereturn" "aleph"
"Ifraktur" "Rfraktur" "weierstrass" "circlemultiply" "circleplus"
"emptyset" "intersection" "union" "propersuperset" "reflexsuperset"
"notsubset" "propersubset" "reflexsubset" "element" "notelement"
"angle" "gradient" "registerserif" "copyrightserif" "trademarkserif"
"product" "radical" "dotmath" "logicalnot" "logicaland" "logicalor"
"arrowdblboth" "arrowdblleft" "arrowdblup" "arrowdblright"
"arrowdbldown" "lozenge" "angleleft" "registersans" "copyrightsans"
"trademarksans" "summation" "parenlefttp" "parenleftex" "parenleftbt"
"bracketlefttp" "bracketleftex" "bracketleftbt" "bracelefttp"
"braceleftmid" "braceleftbt" "braceex" nil "angleright" "integral"
"integraltp" "integralex" "integralbt" "parenrighttp" "parenrightex"
"parenrightbt" "bracketrighttp" "bracketrightex" "bracketrightbt"
"bracerighttp" "bracerightmid" "bracerightbt" nil )))

(defvar *zapf-dingbats-encoding*
  (make-instance 'single-byte-encoding
                 :name "ZapfDingbatsEncoding"  :keyword-name :zapf-dingbats-encoding
		 :standard-encoding t  :char-names #(
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil "space" "a1"
"a2" "a202" "a3" "a4" "a5" "a119" "a118" "a117" "a11" "a12" "a13" "a14"
"a15" "a16" "a105" "a17" "a18" "a19" "a20" "a21" "a22" "a23" "a24"
"a25" "a26" "a27" "a28" "a6" "a7" "a8" "a9" "a10" "a29" "a30" "a31"
"a32" "a33" "a34" "a35" "a36" "a37" "a38" "a39" "a40" "a41" "a42" "a43"
"a44" "a45" "a46" "a47" "a48" "a49" "a50" "a51" "a52" "a53" "a54" "a55"
"a56" "a57" "a58" "a59" "a60" "a61" "a62" "a63" "a64" "a65" "a66" "a67"
"a68" "a69" "a70" "a71" "a72" "a73" "a74" "a203" "a75" "a204" "a76"
"a77" "a78" "a79" "a81" "a82" "a83" "a84" "a97" "a98" "a99" "a100" nil
"a89" "a90" "a93" "a94" "a91" "a92" "a205" "a85" "a206" "a86" "a87"
"a88" "a95" "a96" nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil "a101" "a102" "a103" "a104" "a106" "a107"
"a108" "a112" "a111" "a110" "a109" "a120" "a121" "a122" "a123" "a124"
"a125" "a126" "a127" "a128" "a129" "a130" "a131" "a132" "a133" "a134"
"a135" "a136" "a137" "a138" "a139" "a140" "a141" "a142" "a143" "a144"
"a145" "a146" "a147" "a148" "a149" "a150" "a151" "a152" "a153" "a154"
"a155" "a156" "a157" "a158" "a159" "a160" "a161" "a163" "a164" "a196"
"a165" "a192" "a166" "a167" "a168" "a169" "a170" "a171" "a172" "a173"
"a162" "a174" "a175" "a176" "a177" "a178" "a179" "a193" "a180" "a199"
"a181" "a200" "a182" nil "a201" "a183" "a184" "a197" "a185" "a194"
"a198" "a186" "a195" "a187" "a188" "a189" "a190" "a191" nil )))


(defun compute-encoding-differences (encoding &optional (from *standard-encoding*))
  (let ((differences (make-array 20 :fill-pointer 0 :adjustable t))
	(range-started nil))
    (if from
	(flet ((start-range (code)
		 (when (or (and code (not range-started))(and (not code) range-started))
		   (setf range-started code)
		   (when code (vector-push-extend code differences)))))
	  (loop ;with start-code = nil
		for standard-char-name across (char-names from)
		for char-name across (char-names encoding)
		for code from 0
		do
		(cond
		  ((and (not char-name) standard-char-name)
		   (start-range code) (vector-push-extend ".notdef" differences))
		  ((and char-name (not (equal char-name standard-char-name)))
		   (start-range code)
		   (vector-push-extend (add-/ char-name) differences))
		  (t (start-range nil)))))
	(full-encoding-differences encoding))
	differences))

;;; Just put all...
(defun full-encoding-differences (encoding)
  (let ((differences (make-array 20 :fill-pointer 0 :adjustable t)))
    (vector-push-extend 0 differences)
    (loop for char-name across (char-names encoding)
	  for code from 0
	  do (if char-name
		 (vector-push-extend (concatenate 'string "/" char-name) differences)
		 (vector-push-extend "/.notdef" differences)))
    differences))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom encoding

(defclass custom-encoding (single-byte-encoding)
 ((base-encoding :initarg :base-encoding :reader base-encoding :initform nil)
  ;; Implementation-dependent value that specifeis a mapping
  ;;  from Unicode character codes (Lisp characters)
  ;;  to one-byte character codes belonging [0-255] range.
  ;; Q: Store char-to-code hash-table?
  (charset :accessor charset :initarg :charset :initform nil)))

(defmethod initialize-instance :after ((encoding custom-encoding) &key &allow-other-keys)
  (with-slots (base-encoding) encoding
    (when (and base-encoding
               (or (stringp base-encoding) (symbolp base-encoding)))
      (setf base-encoding (gethash base-encoding *encodings* base-encoding)))))


(defmethod make-dictionary ((encoding custom-encoding) &key &allow-other-keys)
  (with-slots (base-encoding) encoding
    (make-instance 'dictionary :dict-values
      `(("/Type" . "/Encoding")
        ,@(when base-encoding
            `(("/BaseEncoding" . ,(add-/ (name base-encoding)))
              ("/Differences" . ,(compute-encoding-differences encoding
                                                               base-encoding))))))))

;;; CAUTION:
;;;  Basing on :win-ansi-encoding fails for embedded Type1 fonts!
;;;  For some installed fonts that are not embedded, :win-ansi-encoding gets better
;;;  results, so it should be specified explicitly for get-font.
;;;  It seems that get-font should not have any default for the encoding parameter.

(defparameter *win-1251-encoding*
  (make-instance 'custom-encoding
                 :name "Win1251Encoding"  :keyword-name :win-1251-encoding
                 :base-encoding :standard-encoding	;:win-ansi-encoding doesn't work!
                 :charset #+lispworks 1251		; passed to ef:char-external-code
		          #+allegro :1251
                          #+sbcl :windows-1251
                          #-(or lispworks allegro sbcl) nil		; <- customize your lisp here
                 :char-names #(
	nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
	"space" "exclam" "quotedbl" "numbersign" "dollar" "percent" "ampersand"
	"quotesingle" "parenleft" "parenright" "asterisk" "plus" "comma" "hyphen"
	"period" "slash" "zero" "one" "two" "three" "four" "five" "six" "seven"
	"eight" "nine" "colon" "semicolon" "less" "equal" "greater" "question" "at"
	"A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
	"T" "U" "V" "W" "X" "Y" "Z" "bracketleft" "backslash" "bracketright"
        "asciicircum" "underscore" "grave"
        "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
        "t" "u" "v" "w" "x" "y" "z" "braceleft" "bar" "braceright" "asciitilde"
        nil "Djecyrillic" "Gjecyrillic" "quotesinglbase" "gjecyrillic" "quotedblbase"
        "ellipsis" "dagger" "daggerdbl" "Euro" "perthousand" "Ljecyrillic"
        "guilsinglleft" "Njecyrillic" "Kjecyrillic" "Tshecyrillic" "Dzhecyrillic"
        "djecyrillic" "quoteleft" "quoteright" "quotedblleft" "quotedblright" "bullet"
        "endash" "emdash" NIL "trademark" "ljecyrillic" "guilsinglright" "njecyrillic"
        "kjecyrillic" "tshecyrillic" "dzhecyrillic" "space" "Ushortcyrillic"
        "ushortcyrillic" "Jecyrillic" "currency" "Gheupturncyrillic" "brokenbar"
        "section" "Iocyrillic" "copyright" "Ecyrillic" "guillemotleft" "logicalnot"
        "hyphen" "registered" "Yicyrillic" "degree" "plusminus" "Icyrillic" "icyrillic"
        "gheupturncyrillic" "mu" "paragraph" "periodcentered" "iocyrillic" "numero"
        "ecyrillic" "guillemotright" "jecyrillic" "Dzecyrillic" "dzecyrillic" "yicyrillic"
        "Acyrillic" "Becyrillic" "Vecyrillic" "Gecyrillic" "Decyrillic" "Iecyrillic"
        "Zhecyrillic" "Zecyrillic" "Iicyrillic" "Iishortcyrillic" "Kacyrillic"
        "Elcyrillic" "Emcyrillic" "Encyrillic" "Ocyrillic" "Pecyrillic" "Ercyrillic"
        "Escyrillic" "Tecyrillic" "Ucyrillic" "Efcyrillic" "Khacyrillic" "Tsecyrillic"
        "Checyrillic" "Shacyrillic" "Shchacyrillic" "Hardsigncyrillic" "Yericyrillic"
        "Softsigncyrillic" "Ereversedcyrillic" "IUcyrillic" "IAcyrillic"
        "acyrillic" "becyrillic" "vecyrillic" "gecyrillic" "decyrillic" "iecyrillic"
        "zhecyrillic" "zecyrillic" "iicyrillic" "iishortcyrillic" "kacyrillic"
        "elcyrillic" "emcyrillic" "encyrillic" "ocyrillic" "pecyrillic" "ercyrillic"
        "escyrillic" "tecyrillic" "ucyrillic" "efcyrillic" "khacyrillic" "tsecyrillic"
        "checyrillic" "shacyrillic" "shchacyrillic" "hardsigncyrillic" "yericyrillic"
        "softsigncyrillic" "ereversedcyrillic" "iucyrillic" "iacyrillic")))


(defparameter *latin-2-encoding*
  (make-instance 'pdf::custom-encoding
		 :name "Latin2Encoding"
		 :keyword-name :latin-2-encoding
		 :base-encoding :standard-encoding
		 :charset :latin-2
		 :char-names #( nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil
"space" "exclam" "quotedbl" "numbersign" "dollar" "percent"
"ampersand" "quoteright" "parenleft" "parenright" "asterisk"
"plus" "comma" "minus" "period" "slash" "zero" "one" "two"
"three" "four" "five" "six" "seven" "eight" "nine" "colon"
"semicolon" "less" "equal" "greater" "question" "at" "A" "B"
"C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O"
"P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "bracketleft" "backslash"
"bracketright" "asciicircum" "underscore" "quoteleft" "a" "b" "c"
"d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q"
"r" "s" "t" "u" "v" "w" "x" "y" "z" "braceleft" "bar" "braceright" "asciitilde"
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil "dotlessi" "grave"
"acute" "circumflex" "tilde" "macron" "cent" "ydieresis" "dieresis" nil "ring"
"cedilla" nil "hungarumlaut" "twosuperior" "periodcentered" "nbspace" "Aogonek"
"breve" "Lslash" "currency" "Lcaron" "Sacute" "section" "dieresis" "Scaron" "Scedilla"
"Tcaron" "Zacute" "hyphen" "Zcaron" "Zdotaccent" "degree" "aogonek" "ogonek" "lslash"
"acute" "lcaron" "sacute" "caron" "cedilla" "scaron" "scedilla" "tcaron" "zacute" "dblacute"
"zcaron" "zdotaccent" "Racute" "Aacute" "Acircumflex" "Atilde" "Adieresis" "Lacute"
"Cacute" "Ccedilla" "Ccaron" "Eacute" "Eogonek" "Edieresis" "Ecaron" "Iacute" "Icircumflex"
"Dcaron" "Dbar" "Nacute" "Ncaron" "Oacute" "Ocircumflex" "Odblacute" "Odieresis"
"multiply" "Rcaron" "Uring" "Uacute" "Udblacute" "Udieresis" "Yacute" "Tcedilla" "germandbls"
"racute" "aacute" "acircumflex" "atilde" "adieresis" "lacute" "cacute" "ccedilla" "ccaron"
"eacute" "eogonek" "edieresis" "ecaron" "iacute" "icircumflex" "dcaron" "dbar" "nacute"
"ncaron" "oacute" "ocircumflex" "odblacute" "odieresis" "divide" "rcaron" "uring" "uacute"
"udblacute" "udieresis" "yacute" "tcedilla" "dotaccent")))


(defparameter *win-1250-encoding*
  (make-instance 'pdf::custom-encoding
		 :name "Win1250Encoding"
		 :keyword-name :win-1250-encoding
		 :base-encoding :standard-encoding
		 :charset #+sbcl :windows-1250
                          #-sbcl :1250
		 :char-names #(
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil "space"
"exclam" "quotedbl" "numbersign" "dollar" "percent" "ampersand"
"quotesingle" "parenleft" "parenright" "asterisk" "plus" "comma"
"hyphen" "period" "slash" "zero" "one" "two" "three" "four"
"five" "six" "seven" "eight" "nine" "colon" "semicolon" "less"
"equal" "greater" "question" "at" "A" "B" "C" "D" "E" "F"
"G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
"U" "V" "W" "X" "Y" "Z" "bracketleft" "backslash"
"bracketright" "asciicircum" "underscore" "grave" "a" "b" "c"
"d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q"
"r" "s" "t" "u" "v" "w" "x" "y" "z" "braceleft" "bar"
"braceright" "asciitilde" nil nil nil "quotesinglbase" nil
"quotedblbase" "ellipsis" "dagger" "daggerdbl" nil "perthousand"
"Scaron" "guilsinglleft" "Sacute" "Tcaron" "Zcaron" "Zacute" nil
"quoteleft" "quoteright" "quotedblleft" "quotedblright" "bullet"
"endash" "emdash" nil "trademark" "scaron" "guilsinglright"
"sacute" "tcaron" "zcaron" "zacute" "space" "caron" "breve"
"Lslash" "currency" "Aogonek" "brokenbar" "section" "dieresis"
"copyright" "Scommaaccent" "guillemotleft" "logicalnot" "hyphen"
"registered" "Zdotaccent" "degree" "plusminus" "ogonek" "lslash"
"acute" "mu" "paragraph" "periodcentered" "cedilla" "aogonek"
"scommaaccent" "guillemotright" "Lcaron" "hungarumlaut" "lcaron"
"zdotaccent" "Racute" "Aacute" "Acircumflex" "Abreve"
"Adieresis" "Lacute" "Cacute" "Ccedilla" "Ccaron" "Eacute"
"Eogonek" "Edieresis" "Ecaron" "Iacute" "Icircumflex" "Dcaron"
"Dcroat" "Nacute" "Ncaron" "Oacute" "Ocircumflex"
"Ohungarumlaut" "Odieresis" "multiply" "Rcaron" "Uring" "Uacute"
"Uhungarumlaut" "Udieresis" "Yacute" "Tcommaaccent" "germandbls"
"racute" "aacute" "acircumflex" "abreve" "adieresis" "lacute"
"cacute" "ccedilla" "ccaron" "eacute" "eogonek" "edieresis"
"ecaron" "iacute" "icircumflex" "dcaron" "dcroat" "nacute"
"ncaron" "oacute" "ocircumflex" "ohungarumlaut" "odieresis"
"divide" "rcaron" "uring" "uacute" "uhungarumlaut" "udieresis"
"yacute" "tcommaaccent" "dotaccent"
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Double-byte encodings

(defclass unicode-encoding (encoding) ()
 (:default-initargs
  :name "UnicodeEncoding"
  :keyword-name :unicode-encoding
  :standard-encoding t))

(defparameter *unicode-encoding* (make-instance 'unicode-encoding))
