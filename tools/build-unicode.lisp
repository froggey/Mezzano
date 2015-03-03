;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :build-unicode
  (:export :decode-glyph :generate-unifont-table
	   :read-unicode-data :generate-unicode-data-tables)
  (:use :cl))

(in-package :build-unicode)

(defun decode-glyph (glyph)
  "Generate a simple-bit-vector from GLYPH."
  ;; Each character represents 4 bits, there are 16 lines per glyph.
  (let* ((width (/ (* (length glyph) 4) 16))
         (vec (make-array (* width 16) :element-type 'bit))
         (chars-per-row (/ width 4)))
    (dotimes (y 16)
      (let ((row (parse-integer glyph
                                :start (* y chars-per-row)
                                :end (* (1+ y) chars-per-row)
                                :radix 16)))
        (dotimes (x width)
          (setf (bit vec (+ x (* y width))) (ldb (byte 1 (- width 1 x)) row)))))
    vec))

(defun generate-unifont-table (unifont)
  "Generate a radix tree of glyphs from a stream of unifont entries."
  (do ((unifont-table (make-array 256 :initial-element nil))
       (unifont-data (make-array (* (expt 2 16) (* 16 16))
                                 :element-type 'bit
                                 :fill-pointer 0))
       (line (read-line unifont nil) (read-line unifont nil)))
      ((null line)
       (values unifont-table
               (make-array (length unifont-data)
                           :element-type 'bit
                           :initial-contents unifont-data)))
    (with-simple-restart (continue "Skip this entry.")
      (let* ((codepoint (parse-integer line :end 4 :radix 16))
	     (row (ldb (byte 8 8) codepoint))
	     (cell (ldb (byte 8 0) codepoint))
	     (glyph (subseq line 5)))
	;; Strip out non-printing glyphs, the private use area and surrogate forms.
	(when (not (or (string= "00542A542A542A542A542A542A542A00" glyph)
		       (<= #xD800 codepoint #xF8FF)))
          (unless (aref unifont-table row)
            (setf (aref unifont-table row) (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0)))
	  (let ((decoded-glyph (decode-glyph glyph))
                (current (length unifont-data))
                (row-vec (aref unifont-table row)))
            (dotimes (i (length decoded-glyph))
              (vector-push (bit decoded-glyph i) unifont-data))
	    (setf (aref row-vec cell) (logior #x80000000
                                              (ecase (length glyph)
                                                (32 0)
                                                (64 #x40000000))
                                              current))))))))

(defun explode-line (line seperator)
  "Break a line of text apart into a list using SEPERATOR as
the seperator character."
  (let ((start 0)
	(list nil))
    (dotimes (i (length line))
      (when (eql (char line i) seperator)
	(push (subseq line start i) list)
	(setf start (1+ i))))
    (nreverse list)))

(defun read-unicode-data (pathspec)
  (with-open-file (s pathspec)
    (do* ((line (read-line s nil) (read-line s nil))
	  (data (cons nil nil))
	  (tail data))
	 ((null line) (cdr data))
      (setf (cdr tail) (cons (explode-line line #\;) nil)
	    tail (cdr tail)))))

(defun build-unicode-symbol-table (data)
  (let ((symbol-table (make-hash-table :test 'equal)))
    (dolist (line data)
      ;; Break names apart at word boundaries
      ;; "TIBETAN MARK TSA -PHRU" => "TIBETAN " "MARK " "TSA " "-" "PHRU"
      (let ((start 0)
	    (name (second line)))
	(if (eql (char name 0) #\<)
	    (incf (gethash name symbol-table 0))
	    (dotimes (i (length name) (incf (gethash (subseq name start (length name)) symbol-table 0)))
	      (when (or (eql (char name i) #\Space)
			(eql (char name i) #\-))
		(incf (gethash (subseq name start (1+ i)) symbol-table 0))
		(setf start (1+ i)))))))
    symbol-table))

;;; Unicode character names use a simple compression scheme
;;; where the most common symbols are encoded in one byte
;;; or an escape byte followed by another byte.
;;; The escape byte is 0.
;;; [A-Z0-9- ] are encoded as (1+ (position ch *unicode-direct-name-codes*))
;;; Also used for the name trie, so space must be last.
(defparameter *unicode-direct-name-codes* "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789- ")

(defun build-unicode-shorthand-table (symbols)
  "Create the encoding table from a hash-table of symbols and freqencies."
  (let ((frequency-table (make-array (hash-table-count symbols) :fill-pointer 0))
	(encoding-table nil))
    (maphash (lambda (k v)
	       ;; Exclude short symbols and infrequently used symbols.
	       (when (and (> v 10) (> (length k) 2))
		 (vector-push k frequency-table)))
	     symbols)
    (sort frequency-table (lambda (a b)
			    (> (gethash a symbols 0)
			       (gethash b symbols 0))))
    (dotimes (i (min (- 256 (length *unicode-direct-name-codes*) 1)
		     (length frequency-table)))
      (push (cons (+ i 1 (length *unicode-direct-name-codes*))
		  (aref frequency-table i))
	    encoding-table))
    (dotimes (i (min 256 (- (length frequency-table) (- 256 (length *unicode-direct-name-codes*) 1))))
      (push (cons (list i) (aref frequency-table (+ i (- 256 (length *unicode-direct-name-codes*) 1))))
	    encoding-table))
    (nreverse encoding-table)))

(defun encode-unicode-name (name encoding-table)
  (let ((start 0)
	(encoded (make-array (length name) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (flet ((add-symbol (start end)
	     (let ((encoding (find-if (lambda (symbol)
					(string= symbol name :start2 start :end2 end))
				      encoding-table
				      :key #'cdr)))
	       ;; Attempt to match the symbol in the encoding table.
	       (if encoding
		   (if (consp (car encoding))
		       (progn (vector-push 0 encoded)
			      (vector-push (caar encoding) encoded))
		       (vector-push (car encoding) encoded))
		   ;; No match, just push each character in the symbol.
		   (dotimes (i (- end start))
		     (vector-push (1+ (position (char name (+ start i))
						*unicode-direct-name-codes*))
				  encoded))))))
      (dotimes (i (length name) (add-symbol start (length name)))
	;; Break names apart the same way BUILD-UNICODE-SYMBOL-TABLE does.
	(when (or (eql (char name i) #\Space)
		  (eql (char name i) #\-))
	  (add-symbol start (1+ i))
	  (setf start (1+ i)))))
    encoded))

(defun decode-unicode-name (encoded encoding-table &key (start 0) end)
  (unless end (setf end (length encoded)))
  (let ((name (make-array 8 :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (dotimes (i (- end start))
      (let ((short (if (eql (aref encoded (+ i start)) 0)
		       (find (aref encoded (+ (incf i) start)) encoding-table
			     :key (lambda (x) (when (consp (car x)) (caar x))))
		       (find (aref encoded (+ i start)) encoding-table :key #'car))))
	(if short
	    (dotimes (j (length (cdr short)))
	      (vector-push-extend (char (cdr short) j) name))
	    (vector-push-extend (aref *unicode-direct-name-codes* (1- (aref encoded (+ i start)))) name))))
    name))

(defun add-name-to-trie (name value trie)
  ;; Remove spaces & medial hyphens from name and convert to a simple-base-string.
  (let ((new-name (make-array (length name) :element-type 'base-string :fill-pointer 0))
	(prev-was-letter t))
    (dotimes (i (length name))
      (let ((ch (char name i)))
	(cond ((eql ch #\-)
	       (unless (and prev-was-letter (not (eql (char name (1+ i)) #\Space)))
		 (vector-push #\- new-name)))
	      ((eql ch #\Space)
	       (setf prev-was-letter nil))
	      (t (vector-push ch new-name)
		 (setf prev-was-letter t)))))
    (setf name (coerce new-name 'simple-base-string)))
  (dotimes (i (length name) (progn (when (aref trie 0)
				     (error "U~4,'0X ~A conflicts with ~4,'0X in unicode trie."
					    value name (aref trie 0)))
				   (setf (aref trie 0) value)))
    (labels ((position-of (char)
	       (1+ (position char *unicode-direct-name-codes*)))
	     (insert (ofs)
	       (unless (aref trie ofs)
		 (setf (aref trie ofs) (make-array (+ 1 37) :initial-element nil)))
	       (setf trie (aref trie ofs))))
      (insert (position-of (char name i))))))

(defun pack-name-trie (trie)
  (let ((packed (make-array (+ 1 37)
			    :element-type '(unsigned-byte 32)
			    :initial-element 0
			    :adjustable t
			    :fill-pointer 0))
	(scan-start-point 0))
    (labels ((write-node-at (ofs node)
	       (setf (aref packed ofs) (if (aref node 0)
					   (logior #xFC000000 (aref node 0))
					   #xF8000000))
	       ;; Install placeholder markers
	       (dotimes (i (1- (length *unicode-direct-name-codes*)))
		 (when (aref node (1+ i))
		   (setf (aref packed (+ ofs 1 i)) (ash (1+ i) 26))))
	       ;; Compute actual offsets
	       (dotimes (i (1- (length *unicode-direct-name-codes*)))
		 (when (aref node (1+ i))
		   (setf (aref packed (+ ofs 1 i)) (logior (ash (1+ i) 26)
							   (pack-one (aref node (1+ i)))))))
	       ofs)
	     (pack-one (node)
	       (do ((i scan-start-point (1+ i))
		    (seen-blank nil))
		   ((>= i (length packed))
		    ;; No matching point, extend the array!
		    (incf (fill-pointer packed) (+ 1 37))
		    (write-node-at i node))
		 ;; Maintain a minimum amount of space in the array.
		 (when (>= (+ (fill-pointer packed) (+ 1 37)) (array-dimension packed 0))
		   (adjust-array packed (* (array-dimension packed 0) 2)
				 :initial-element 0))
		 (when (eql (aref packed i) 0)
		   (when (not seen-blank)
		     ;; Cache the first blank tile.
		     (setf scan-start-point i
			   seen-blank t))
		   (dotimes (j (1- (length *unicode-direct-name-codes*))
			     (progn
			       (setf (fill-pointer packed) (max (fill-pointer packed)
								(+ i 1 37)))
			       (return-from pack-one (write-node-at i node))))
		     (when (aref node (1+ j))
		       (when (not (eql (aref packed (+ i 1 j)) 0))
			 (return))))))))
      (pack-one trie)
      ;; Turn it into a simple array.
      (make-array (length packed)
                  :element-type '(unsigned-byte 32)
                  :initial-contents packed))))

(defun generate-unicode-data-tables (unicode-data)
  (let* ((planes (make-array 17 :initial-element nil))
	 (name-store (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
	 (encoding-table (build-unicode-shorthand-table (build-unicode-symbol-table unicode-data)))
	 (name-trie (make-array (+ 1 37) :initial-element nil)))
    (dolist (line unicode-data)
      (with-simple-restart (continue "Skip this entry.")
	;; Skip <control> characters and the regions
	(unless (eql #\< (char (nth 1 line) 0))
	  (let* ((codepoint (parse-integer (nth 0 line) :radix 16))
		 (plane (ldb (byte 5 16) codepoint))
		 (row (ldb (byte 8 8) codepoint))
		 (cell (ldb (byte 8 0) codepoint))
		 (name (nth 1 line))
		 (encoded-name (encode-unicode-name name encoding-table))
		 (category (nth 2 line))
		 (uppercase (parse-integer (nth 12 line) :radix 16 :junk-allowed t))
		 (lowercase (parse-integer (nth 13 line) :radix 16 :junk-allowed t))
		 (case (cond ((and (string= category "Lu") lowercase)
			       (logior lowercase (ash 1 21)))
			     ((and (string= category "Ll") uppercase)
			      (logior uppercase (ash 2 21)))
			     (t 0))))
	    ;; Insert this codepoint into the information table.
	    (unless (aref planes plane)
	      (setf (aref planes plane) (make-array 256 :initial-element nil)))
	    (unless (aref (aref planes plane) row)
	      (setf (aref (aref planes plane) row) (make-array 256
							       :element-type '(unsigned-byte 64)
							       :initial-element 0)))
	    (setf (aref (aref (aref planes plane) row) cell)
		  ;; 20 bits for the offset, 6 for the length,
		  ;; 21 bits for the othercase character code.
		  ;; 2 bits for the category. 0 = no case, 1 = lowercase, 2 = uppercase.
		  (logior (length name-store)
			  (ash (length encoded-name) 20)
			  (ash case 26)))
	    ;; Append the name to the name-store
	    (dotimes (i (length encoded-name))
	      (vector-push-extend (aref encoded-name i) name-store))
	    ;; U+1180 HANGUL JUNGSEONG O-E conflicts with U+116C HANGUL JUNGSEONG OE
	    (unless (eql #x1180 codepoint)
	      (add-name-to-trie name codepoint name-trie))))))
    (values planes
            ;; Pack the name-store.
            (make-array (length name-store)
                        :element-type '(unsigned-byte 8)
                        :initial-contents name-store)
            encoding-table
            (pack-name-trie name-trie))))
